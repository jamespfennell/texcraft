use super::*;
use std::collections::HashSet;

pub fn compile(
    instructions: &[lang::Instruction],
    entry_points: &HashMap<char, usize>,
) -> Result<(CompiledProgram, Vec<CompilationWarning>), CompilationError> {
    let (pair_to_instruction, warnings) = build_pair_to_instruction_map(instructions, entry_points);
    let pair_to_replacement = calculate_replacements(instructions, pair_to_instruction)?;
    let program = lower_and_optimize(pair_to_replacement);
    Ok((program, warnings))
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord)]
struct Node(char, char);

struct OngoingCalculation {
    // Node this calculation is for.
    node: Node,
    // Part of the ligature result that has been finalized and won't change.
    finalized: Vec<(char, FixWord)>,
    // Characters that are still pending replacement. The next step is to apply the ligature
    // rule for (pending[0], pending[1]).
    pending: (char, char, Option<char>),
}

impl OngoingCalculation {
    fn child(&self) -> Node {
        Node(self.pending.0, self.pending.1)
    }
}

fn build_pair_to_instruction_map(
    instructions: &[lang::Instruction],
    entry_points: &HashMap<char, usize>,
) -> (HashMap<(char, char), usize>, Vec<CompilationWarning>) {
    let mut result = HashMap::<(char, char), usize>::new();
    let mut reachable_instructions = HashSet::<usize>::with_capacity(instructions.len());
    let mut warnings = vec![];
    for (&left, &entry_point) in entry_points {
        let mut next_instruction = Some(entry_point);
        while let Some(next) = next_instruction {
            let instruction = match instructions.get(next) {
                None => {
                    warnings.push(CompilationWarning::InvalidNextInstruction);
                    break;
                }
                Some(instruction) => instruction,
            };
            reachable_instructions.insert(next);
            next_instruction = instruction
                .next_instruction
                .map(|increment| next + 1 + (increment as usize));

            let right = instruction.right_char;
            if let Some(_old_instruction) = result.insert((left, right), next) {
                warnings.push(CompilationWarning::DuplicateRule);
            }
        }
    }
    if reachable_instructions.len() < instructions.len() {
        for (i, _) in instructions.iter().enumerate() {
            if !reachable_instructions.contains(&i) {
                warnings.push(CompilationWarning::OrphanRule);
            }
        }
    }
    (result, warnings)
}

struct Replacement(Vec<(char, FixWord)>, char);

fn calculate_replacements(
    instructions: &[lang::Instruction],
    pair_to_instruction: HashMap<(char, char), usize>,
) -> Result<HashMap<(char, char), Replacement>, CompilationError> {
    let mut result: HashMap<(char, char), Replacement> = Default::default();
    let mut actionable: Vec<OngoingCalculation> = vec![];
    let mut node_to_parents: HashMap<Node, Vec<OngoingCalculation>> = Default::default();
    for (&(left, right), &index) in &pair_to_instruction {
        let (pre_cursor, cursor, post_cursor): (TC, char, TC) = match instructions[index].operation
        {
            lang::Operation::Kern(kern) => {
                result.insert((left, right), Replacement(vec![(left, kern)], right));
                continue;
            }
            lang::Operation::Ligature {
                char_to_insert,
                post_lig_operation,
            } => match post_lig_operation {
                lang::PostLigOperation::RetainBothMoveNowhere => {
                    (TC::None, left, TC::Two(char_to_insert, right))
                }
                lang::PostLigOperation::RetainBothMoveToInserted => {
                    (TC::One(left), char_to_insert, TC::One(right))
                }
                lang::PostLigOperation::RetainBothMoveToRight => {
                    (TC::Two(left, char_to_insert), right, TC::None)
                }
                lang::PostLigOperation::RetainRightMoveToInserted => {
                    (TC::None, char_to_insert, TC::One(right))
                }
                lang::PostLigOperation::RetainRightMoveToRight => {
                    (TC::One(char_to_insert), right, TC::None)
                }
                lang::PostLigOperation::RetainLeftMoveNowhere => {
                    (TC::None, left, TC::One(char_to_insert))
                }
                lang::PostLigOperation::RetainLeftMoveToInserted => {
                    (TC::One(left), char_to_insert, TC::None)
                }
                lang::PostLigOperation::RetainNeitherMoveToInserted => {
                    (TC::None, char_to_insert, TC::None)
                }
            },
        };
        let node = Node(left, right);
        let pre_cursor = pre_cursor.map(|c| (c, FixWord::ZERO)).collect();
        match post_cursor {
            TC::None => {
                result.insert((node.0, node.1), Replacement(pre_cursor, cursor));
            }
            TC::One(c2) => {
                actionable.push(OngoingCalculation {
                    node,
                    finalized: pre_cursor,
                    pending: (cursor, c2, None),
                });
                node_to_parents.insert(node, vec![]);
            }
            TC::Two(c2, c3) => {
                actionable.push(OngoingCalculation {
                    node,
                    finalized: pre_cursor,
                    pending: (cursor, c2, Some(c3)),
                });
                node_to_parents.insert(node, vec![]);
            }
        };
    }

    while let Some(mut calc) = actionable.pop() {
        let child = Node(calc.pending.0, calc.pending.1); // todo calc.child() ?
        if let Some(blocking) = node_to_parents.get_mut(&child) {
            blocking.push(calc);
            continue;
        }
        let last = match result.get(&(child.0, child.1)) {
            // todo node.tuple?
            None => {
                calc.finalized.push((child.0, FixWord::ZERO));
                child.1
            }
            Some(Replacement(replacement, last)) => {
                calc.finalized.extend(replacement);
                *last
            }
        };
        match calc.pending.2 {
            None => {
                if let Some(blocking) = node_to_parents.remove(&calc.node) {
                    actionable.extend(blocking);
                }
                result.insert(
                    (calc.node.0, calc.node.1),
                    Replacement(calc.finalized, last),
                );
            }
            Some(other) => {
                calc.pending = (last, other, None);
                actionable.push(calc);
            }
        }
    }

    // Next we check for infinite loops. If there is one, the node_to_parents
    // map will be non-empty and contain all the nodes that couldn't be calculated.
    let node_to_calc: HashMap<Node, OngoingCalculation> = node_to_parents
        .into_values()
        .flatten()
        .map(|calc| (calc.node, calc))
        .collect();
    if let Some(&smallest_node) = node_to_calc.keys().min() {
        let infinite_loop = {
            let mut seen = HashSet::<Node>::new();
            let mut next_node = smallest_node;
            while seen.insert(next_node) {
                next_node = node_to_calc[&next_node].child();
            }
            let mut infinite_loop: Vec<Node> = vec![];
            while Some(next_node) != infinite_loop.first().copied() {
                infinite_loop.push(next_node);
                next_node = node_to_calc[&next_node].child();
            }
            infinite_loop
        };
        let starting_pair = (infinite_loop[0].0, infinite_loop[0].1);
        let mut starting_replacement = format!["{}{}", starting_pair.0, starting_pair.1];
        let mut cursor_position = 0_usize;
        let mut steps = Vec::<InfiniteLoopStep>::new();
        for node in infinite_loop {
            // TODO: the bug here for /LIG/ nodes is that the loop may caused by the second dep of /LIG/
            // and so we've lost information on how we got from the start to here.
            let calc = &node_to_calc[&node];
            let mut new_starting_replacement = starting_replacement[0..cursor_position].to_string();
            for (finalized, _) in &calc.finalized {
                new_starting_replacement.push(*finalized);
            }
            let new_cursor_position = new_starting_replacement.len();
            new_starting_replacement.push(calc.pending.0);
            new_starting_replacement.push(calc.pending.1);
            if let Some(c) = calc.pending.2 {
                new_starting_replacement.push(c);
            }
            new_starting_replacement.push_str(&starting_replacement[cursor_position + 2..]);

            starting_replacement = new_starting_replacement;
            cursor_position = new_cursor_position;

            steps.push(InfiniteLoopStep {
                post_replacement: starting_replacement.clone(),
                post_cursor_position: cursor_position,
                instruction_index: pair_to_instruction[&(node.0, node.1)],
            });
        }
        return Err(CompilationError {
            starting_pair,
            infinite_loop: steps,
        });
    }

    Ok(result)
}

enum TC {
    None,
    One(char),
    Two(char, char),
}

impl Iterator for TC {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let a = match *self {
            TC::None => (TC::None, None),
            TC::One(a) => (TC::None, Some(a)),
            TC::Two(a, b) => (TC::One(b), Some(a)),
        };
        *self = a.0;
        a.1
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let n = match self {
            TC::None => 0,
            TC::One(_) => 1,
            TC::Two(_, _) => 2,
        };
        (n, Some(n))
    }
}

fn lower_and_optimize(pair_to_replacement: HashMap<(char, char), Replacement>) -> CompiledProgram {
    let mut intermediate: HashMap<char, Vec<(char, RawReplacement)>> = Default::default();
    let mut middle_chars: Vec<(char, FixWord)> = Default::default();

    for (node, Replacement(middle, last)) in pair_to_replacement {
        let (left_char_operation, middle_extension) = match middle.first().copied() {
            None => (LeftCharOperation::Delete, &middle[..]),
            Some((first, kern)) => {
                if first == node.0 {
                    (
                        if kern == FixWord::ZERO {
                            LeftCharOperation::Retain
                        } else {
                            LeftCharOperation::AppendKern(kern)
                        },
                        &middle[1..],
                    )
                } else {
                    (LeftCharOperation::Delete, &middle[..])
                }
            }
        };
        let start: u16 = middle_chars.len().try_into().unwrap();
        middle_chars.extend(middle_extension);
        let end: u16 = middle_chars.len().try_into().unwrap();
        intermediate.entry(node.0).or_default().push((
            node.1,
            RawReplacement {
                left_char_operation,
                middle_char_bounds: start..end,
                last_char: last,
            },
        ));
    }

    let mut left_to_pairs: HashMap<char, (u16, u16)> = Default::default();
    let mut pairs: Vec<(char, RawReplacement)> = Default::default();

    for (left, replacements) in intermediate {
        let start: u16 = pairs.len().try_into().unwrap();
        pairs.extend(replacements);
        let end: u16 = pairs.len().try_into().unwrap();
        left_to_pairs.insert(left, (start, end));
    }
    CompiledProgram {
        left_to_pairs,
        pairs,
        middle_chars,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lang::PostLigOperation::*;

    pub fn new_lig(
        next_instruction: Option<u8>,
        right_char: char,
        char_to_insert: char,
        post_lig_operation: lang::PostLigOperation,
    ) -> lang::Instruction {
        lang::Instruction {
            next_instruction,
            right_char,
            operation: lang::Operation::Ligature {
                char_to_insert,
                post_lig_operation,
            },
        }
    }

    fn run_success_test(
        instructions: Vec<lang::Instruction>,
        entry_points: Vec<(char, usize)>,
        want: Vec<(char, char, Vec<(char, FixWord)>)>,
    ) {
        let entry_points: HashMap<char, usize> = entry_points.into_iter().collect();
        let want: HashMap<(char, char), Vec<(char, FixWord)>> =
            want.into_iter().map(|t| ((t.0, t.1), t.2)).collect();
        let compiled_program = compile(&instructions, &entry_points).unwrap().0;

        let mut got: HashMap<(char, char), Vec<(char, FixWord)>> = Default::default();
        for pair in compiled_program.all_pairs_having_replacement() {
            let replacement: Vec<(char, FixWord)> = compiled_program
                .get_replacement_iter(pair.0, pair.1)
                .collect();
            got.insert(pair, replacement);
        }

        assert_eq!(got, want);
    }

    macro_rules! success_tests {
        ( $( ($name: ident, $instructions: expr, $entry_points: expr, $want: expr, ), )+ ) => {
            $(
                #[test]
                fn $name() {
                    let instructions = $instructions;
                    let entry_points = $entry_points;
                    let want = $want;
                    run_success_test(instructions, entry_points, want);
                }
            )+
        };
    }

    success_tests!(
        (empty_program, vec![], vec![], vec![],),
        (
            kern,
            vec![lang::Instruction::new_kern(None, 'V', FixWord::UNITY)],
            vec![('A', 0)],
            vec![('A', 'V', vec![('A', FixWord::UNITY), ('V', FixWord::ZERO)]),],
        ),
        (
            same_kern_for_multiple_left_characters,
            vec![lang::Instruction::new_kern(None, 'V', FixWord::UNITY)],
            vec![('A', 0), ('B', 0)],
            vec![
                ('A', 'V', vec![('A', FixWord::UNITY), ('V', FixWord::ZERO)]),
                ('B', 'V', vec![('B', FixWord::UNITY), ('V', FixWord::ZERO)]),
            ],
        ),
        (
            duplicate_kern,
            vec![
                lang::Instruction::new_kern(Some(0), 'V', FixWord::UNITY * 2),
                lang::Instruction::new_kern(None, 'V', FixWord::UNITY * 3),
            ],
            vec![('A', 0)],
            vec![(
                'A',
                'V',
                vec![('A', FixWord::UNITY * 3), ('V', FixWord::ZERO)]
            ),],
        ),
        (
            kern_instructions_with_relationship,
            vec![
                lang::Instruction {
                    next_instruction: Some(0),
                    right_char: 'V',
                    operation: lang::Operation::Kern(FixWord::UNITY * 2)
                },
                lang::Instruction {
                    next_instruction: None,
                    right_char: 'W',
                    operation: lang::Operation::Kern(FixWord::UNITY * 3)
                },
                lang::Instruction {
                    next_instruction: None,
                    right_char: 'X',
                    operation: lang::Operation::Kern(FixWord::UNITY * 4)
                },
            ],
            vec![('A', 0), ('B', 1), ('C', 2)],
            vec![
                (
                    'A',
                    'V',
                    vec![('A', FixWord::UNITY * 2), ('V', FixWord::ZERO)]
                ),
                (
                    'A',
                    'W',
                    vec![('A', FixWord::UNITY * 3), ('W', FixWord::ZERO)]
                ),
                (
                    'B',
                    'W',
                    vec![('B', FixWord::UNITY * 3), ('W', FixWord::ZERO)]
                ),
                (
                    'C',
                    'X',
                    vec![('C', FixWord::UNITY * 4), ('X', FixWord::ZERO)]
                ),
            ],
        ),
        (
            single_lig_1,
            vec![lang::Instruction::new_lig(
                None,
                'B',
                'Z',
                RetainNeitherMoveToInserted
            )],
            vec![('A', 0)],
            vec![('A', 'B', vec![('Z', FixWord::ZERO)])],
        ),
        (
            single_lig_2,
            vec![
                lang::Instruction::new_lig(Some(0), 'B', 'Z', RetainLeftMoveToInserted),
                lang::Instruction::new_kern(None, 'Z', FixWord::UNITY),
            ],
            vec![('A', 0)],
            vec![
                ('A', 'B', vec![('A', FixWord::ZERO), ('Z', FixWord::ZERO)]),
                ('A', 'Z', vec![('A', FixWord::UNITY), ('Z', FixWord::ZERO)]),
            ],
        ),
        (
            retain_left_move_nowhere_1,
            vec![
                lang::Instruction::new_lig(Some(0), 'B', 'Z', RetainLeftMoveNowhere),
                lang::Instruction::new_kern(None, 'Z', FixWord::UNITY),
            ],
            vec![('A', 0)],
            vec![
                ('A', 'B', vec![('A', FixWord::UNITY), ('Z', FixWord::ZERO)]),
                ('A', 'Z', vec![('A', FixWord::UNITY), ('Z', FixWord::ZERO)]),
            ],
        ),
        (
            retain_left_move_nowhere_2,
            vec![new_lig(None, 'B', 'Z', RetainLeftMoveNowhere),],
            vec![('A', 0)],
            vec![('A', 'B', vec![('A', FixWord::ZERO), ('Z', FixWord::ZERO)]),],
        ),
        (
            single_lig_4,
            vec![
                lang::Instruction::new_lig(None, 'B', 'Z', RetainRightMoveToInserted),
                lang::Instruction::new_kern(None, 'B', FixWord::UNITY),
            ],
            vec![('A', 0), ('Z', 1)],
            vec![
                ('A', 'B', vec![('Z', FixWord::UNITY), ('B', FixWord::ZERO)]),
                ('Z', 'B', vec![('Z', FixWord::UNITY), ('B', FixWord::ZERO)]),
            ],
        ),
        (
            single_lig_5,
            vec![
                lang::Instruction::new_lig(None, 'B', 'Z', RetainRightMoveToRight),
                lang::Instruction::new_kern(None, 'B', FixWord::UNITY),
            ],
            vec![('A', 0), ('Z', 1)],
            vec![
                ('A', 'B', vec![('Z', FixWord::ZERO), ('B', FixWord::ZERO)]),
                ('Z', 'B', vec![('Z', FixWord::UNITY), ('B', FixWord::ZERO)]),
            ],
        ),
        (
            retain_both_move_nowhere_1,
            vec![
                lang::Instruction::new_lig(Some(0), 'B', 'Z', RetainBothMoveNowhere,),
                lang::Instruction::new_kern(None, 'Z', FixWord::UNITY * 2),
                lang::Instruction::new_kern(None, 'B', FixWord::UNITY * 3),
            ],
            vec![('A', 0), ('Z', 2)],
            vec![
                (
                    'A',
                    'B',
                    vec![
                        ('A', FixWord::UNITY * 2),
                        ('Z', FixWord::UNITY * 3),
                        ('B', FixWord::ZERO)
                    ]
                ),
                (
                    'A',
                    'Z',
                    vec![('A', FixWord::UNITY * 2), ('Z', FixWord::ZERO)]
                ),
                (
                    'Z',
                    'B',
                    vec![('Z', FixWord::UNITY * 3), ('B', FixWord::ZERO)]
                ),
            ],
        ),
        (
            retain_both_move_nowhere_2,
            vec![lang::Instruction::new_lig(
                None,
                'B',
                'Z',
                RetainBothMoveNowhere
            ),],
            vec![('A', 0)],
            vec![(
                'A',
                'B',
                vec![
                    ('A', FixWord::ZERO),
                    ('Z', FixWord::ZERO),
                    ('B', FixWord::ZERO)
                ]
            ),],
        ),
        (
            retain_both_move_nowhere_3,
            vec![
                new_lig(None, 'B', 'Z', RetainBothMoveNowhere),
                new_lig(None, 'B', 'Y', RetainRightMoveToRight),
            ],
            vec![('A', 0), ('Z', 1)],
            vec![
                (
                    'A',
                    'B',
                    vec![
                        ('A', FixWord::ZERO),
                        ('Y', FixWord::ZERO),
                        ('B', FixWord::ZERO)
                    ]
                ),
                ('Z', 'B', vec![('Y', FixWord::ZERO), ('B', FixWord::ZERO),]),
            ],
        ),
        (
            retain_both_move_nowhere_4,
            vec![
                new_lig(Some(0), 'B', 'Z', RetainBothMoveNowhere),
                new_lig(None, 'Z', 'Y', RetainBothMoveToRight),
            ],
            vec![('A', 0)],
            vec![
                (
                    'A',
                    'B',
                    vec![
                        ('A', FixWord::ZERO),
                        ('Y', FixWord::ZERO),
                        ('Z', FixWord::ZERO),
                        ('B', FixWord::ZERO)
                    ]
                ),
                (
                    'A',
                    'Z',
                    vec![
                        ('A', FixWord::ZERO),
                        ('Y', FixWord::ZERO),
                        ('Z', FixWord::ZERO)
                    ]
                ),
            ],
        ),
        (
            retain_both_move_to_inserted_1,
            vec![
                lang::Instruction::new_lig(Some(0), 'B', 'Z', RetainBothMoveToInserted,),
                lang::Instruction::new_kern(None, 'Z', FixWord::UNITY * 2),
                lang::Instruction::new_kern(None, 'B', FixWord::UNITY * 3),
            ],
            vec![('A', 0), ('Z', 2)],
            vec![
                (
                    'A',
                    'B',
                    vec![
                        ('A', FixWord::ZERO),
                        ('Z', FixWord::UNITY * 3),
                        ('B', FixWord::ZERO)
                    ]
                ),
                (
                    'A',
                    'Z',
                    vec![('A', FixWord::UNITY * 2), ('Z', FixWord::ZERO)]
                ),
                (
                    'Z',
                    'B',
                    vec![('Z', FixWord::UNITY * 3), ('B', FixWord::ZERO)]
                ),
            ],
        ),
        (
            retain_both_move_to_inserted_2,
            vec![
                lang::Instruction::new_lig(None, 'B', 'Z', RetainBothMoveToInserted),
                lang::Instruction::new_lig(None, 'B', 'Y', RetainRightMoveToInserted),
            ],
            vec![('A', 0), ('Z', 1)],
            vec![
                (
                    'A',
                    'B',
                    vec![
                        ('A', FixWord::ZERO),
                        ('Y', FixWord::ZERO),
                        ('B', FixWord::ZERO)
                    ]
                ),
                (
                    'Z',
                    'B',
                    vec![('Y', FixWord::ZERO * 3), ('B', FixWord::ZERO)]
                ),
            ],
        ),
        (
            retain_both_move_to_inserted_3,
            vec![lang::Instruction::new_lig(
                None,
                'B',
                'Z',
                RetainBothMoveToInserted
            ),],
            vec![('A', 0)],
            vec![(
                'A',
                'B',
                vec![
                    ('A', FixWord::ZERO),
                    ('Z', FixWord::ZERO),
                    ('B', FixWord::ZERO)
                ]
            ),],
        ),
        (
            retain_both_move_to_inserted_4,
            vec![
                lang::Instruction::new_lig(None, 'B', 'Z', RetainBothMoveToInserted,),
                lang::Instruction::new_lig(None, 'B', 'Y', RetainRightMoveToRight),
            ],
            vec![('A', 0), ('Z', 1)],
            vec![
                (
                    'A',
                    'B',
                    vec![
                        ('A', FixWord::ZERO),
                        ('Y', FixWord::ZERO),
                        ('B', FixWord::ZERO)
                    ]
                ),
                (
                    'Z',
                    'B',
                    vec![('Y', FixWord::ZERO * 3), ('B', FixWord::ZERO)]
                ),
            ],
        ),
        (
            retain_both_move_to_right_1,
            vec![
                lang::Instruction::new_lig(Some(0), 'B', 'Z', RetainBothMoveToRight,),
                lang::Instruction::new_kern(None, 'Z', FixWord::UNITY * 2),
                lang::Instruction::new_kern(None, 'B', FixWord::UNITY * 3),
            ],
            vec![('A', 0), ('Z', 2)],
            vec![
                (
                    'A',
                    'B',
                    vec![
                        ('A', FixWord::ZERO),
                        ('Z', FixWord::ZERO),
                        ('B', FixWord::ZERO)
                    ]
                ),
                (
                    'A',
                    'Z',
                    vec![('A', FixWord::UNITY * 2), ('Z', FixWord::ZERO)]
                ),
                (
                    'Z',
                    'B',
                    vec![('Z', FixWord::UNITY * 3), ('B', FixWord::ZERO)]
                ),
            ],
        ),
    );

    fn run_infinite_loop_test(
        instructions: Vec<lang::Instruction>,
        entry_points: Vec<(char, usize)>,
        want_err: CompilationError,
    ) {
        let entry_points: HashMap<char, usize> = entry_points.into_iter().collect();
        let got_err = compile(&instructions, &entry_points).unwrap_err();
        assert_eq!(got_err, want_err);
    }

    macro_rules! infinite_loop_tests {
        ( $( ($name: ident, $instructions: expr, $entry_points: expr, $want_err: expr, ), )+ ) => {
            $(
                #[test]
                fn $name() {
                    let instructions = $instructions;
                    let entry_points = $entry_points;
                    let want_err = $want_err;
                    run_infinite_loop_test(instructions, entry_points, want_err);
                }
            )+
        };
    }

    infinite_loop_tests!(
        (
            single_command,
            vec![new_lig(None, 'B', 'A', RetainBothMoveToInserted)],
            vec![('A', 0)],
            CompilationError {
                starting_pair: ('A', 'B'),
                infinite_loop: vec![InfiniteLoopStep {
                    instruction_index: 0,
                    post_replacement: "AAB".into(),
                    post_cursor_position: 1,
                }]
            },
        ),
        (
            two_commands,
            vec![
                new_lig(None, 'B', 'C', RetainRightMoveToInserted),
                new_lig(None, 'B', 'A', RetainRightMoveToInserted),
            ],
            vec![('A', 0), ('C', 1)],
            CompilationError {
                starting_pair: ('A', 'B'),
                infinite_loop: vec![
                    InfiniteLoopStep {
                        instruction_index: 0,
                        post_replacement: "CB".into(),
                        post_cursor_position: 0,
                    },
                    InfiniteLoopStep {
                        instruction_index: 1,
                        post_replacement: "AB".into(),
                        post_cursor_position: 0,
                    }
                ]
            },
        ),
        /* TODO: fix the bug and enable this test
        (
            three_commands,
            vec![
                new_lig(Some(1), 'B', 'C', RetainBothMoveNowhere), // AB -> ACB
                new_lig(None, 'C', 'D', RetainRightMoveToInserted), // ACB -> DCB
                new_lig(None, 'C', 'A', RetainLeftMoveToInserted), // DCB -> DAB
            ],
            vec![('A', 0), ('D', 2)],
            CompilationError {
                starting_pair: ('A', 'B'),
                infinite_loop: vec![
                    InfiniteLoopStep {
                        instruction_index: 0,
                        post_replacement: "ACB".into(),
                        post_cursor_position: 0,
                    },
                    InfiniteLoopStep {
                        instruction_index: 1,
                        post_replacement: "DCB".into(),
                        post_cursor_position: 0,
                    },
                    InfiniteLoopStep {
                        instruction_index: 2,
                        post_replacement: "DAB".into(),
                        post_cursor_position: 1,
                    }
                ]
            },
        ),
        */
        (
            infinite_append,
            vec![
                new_lig(None, 'B', 'C', RetainBothMoveToInserted),
                new_lig(None, 'B', 'A', RetainBothMoveToInserted),
            ],
            vec![('A', 0), ('C', 1)],
            CompilationError {
                starting_pair: ('A', 'B'),
                infinite_loop: vec![
                    InfiniteLoopStep {
                        instruction_index: 0,
                        post_replacement: "ACB".into(),
                        post_cursor_position: 1,
                    },
                    InfiniteLoopStep {
                        instruction_index: 1,
                        post_replacement: "ACAB".into(),
                        post_cursor_position: 2,
                    }
                ]
            },
        ),
        (
            path_leading_to_loop,
            vec![
                new_lig(None, 'B', 'C', RetainBothMoveToInserted),
                new_lig(None, 'B', 'C', RetainBothMoveToInserted),
            ],
            vec![('A', 0), ('C', 1)],
            CompilationError {
                starting_pair: ('C', 'B'),
                infinite_loop: vec![InfiniteLoopStep {
                    instruction_index: 1,
                    post_replacement: "CCB".into(),
                    post_cursor_position: 1,
                }]
            },
        ),
    );
}
