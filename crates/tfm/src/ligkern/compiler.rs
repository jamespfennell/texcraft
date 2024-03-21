use super::*;
use std::collections::HashSet;

pub fn compile(
    program: &lang::Program,
    kerns: &[Number],
    entry_points: &HashMap<Char, u16>,
) -> Result<CompiledProgram, InfiniteLoopError> {
    let pair_to_instruction = build_pair_to_instruction_map(program, entry_points);
    let pair_to_replacement =
        calculate_replacements(&program.instructions, kerns, pair_to_instruction)?;
    let program = lower_and_optimize(pair_to_replacement);
    Ok(program)
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord)]
struct Node(LeftChar, Char);

struct OngoingCalculation {
    // Node this calculation is for.
    node: Node,
    // Part of the ligature result that has been finalized and won't change.
    finalized: ReplacementBuilder,
    // Characters that are still pending replacement. The next step is to apply the ligature
    // rule for (pending[0], pending[1]).
    pending: (LeftChar, Char, Option<Char>),
}

impl OngoingCalculation {
    fn child(&self) -> Node {
        Node(self.pending.0, self.pending.1)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, PartialOrd, Ord)]
enum LeftChar {
    Char(Char),
    BoundaryChar,
}

impl LeftChar {
    fn char_or(&self) -> Option<Char> {
        match self {
            LeftChar::Char(c) => Some(*c),
            LeftChar::BoundaryChar => None,
        }
    }
}

impl From<Char> for LeftChar {
    fn from(value: Char) -> Self {
        LeftChar::Char(value)
    }
}

impl TryFrom<LeftChar> for Char {
    type Error = ();

    fn try_from(value: LeftChar) -> Result<Self, Self::Error> {
        match value {
            LeftChar::Char(c) => Ok(c),
            LeftChar::BoundaryChar => Err(()),
        }
    }
}

fn build_pair_to_instruction_map(
    program: &lang::Program,
    entry_points: &HashMap<Char, u16>,
) -> HashMap<(LeftChar, Char), usize> {
    let mut result = HashMap::<(LeftChar, Char), usize>::new();
    let all_entry_points = entry_points
        .iter()
        .map(|(c, e)| (LeftChar::Char(*c), *e))
        .chain(
            program
                .left_boundary_char_entrypoint
                .iter()
                .map(|e| (LeftChar::BoundaryChar, *e)),
        );
    for (left, entry_point) in all_entry_points {
        let mut next_instruction = Some(entry_point as usize);
        while let Some(next) = next_instruction {
            let instruction = match program.instructions.get(next) {
                None => {
                    // Invalid next instruction
                    break;
                }
                Some(instruction) => instruction,
            };
            next_instruction = instruction
                .next_instruction
                .map(|increment| next + 1 + (increment as usize));

            // We only insert the element if it doesn't already exist.
            result.entry((left, instruction.right_char)).or_insert(next);
        }
    }
    result
}

// TODO: replace the first LeftChar with a bool indicating whether the
// first character is deleted. Anytime we need the first char, get it from
// the context. I think in this way we can remove all of the expect() calls
// on boundary chars.
struct Replacement(LeftChar, Vec<(Number, Char)>);

enum ReplacementBuilder {
    Empty,
    NonEmpty {
        replacement: Replacement,
        last_kern: Number,
    },
}

impl ReplacementBuilder {
    fn new_single_char(c1: LeftChar) -> Self {
        ReplacementBuilder::NonEmpty {
            replacement: Replacement(c1, vec![]),
            last_kern: Number::ZERO,
        }
    }
    fn new_double_char(c1: LeftChar, c2: Char) -> Self {
        ReplacementBuilder::NonEmpty {
            replacement: Replacement(c1, vec![(Number::ZERO, c2)]),
            last_kern: Number::ZERO,
        }
    }
    fn finish(self, last_char: Char) -> Replacement {
        match self {
            ReplacementBuilder::Empty => Replacement(last_char.into(), vec![]),
            ReplacementBuilder::NonEmpty {
                mut replacement,
                last_kern,
            } => {
                replacement.1.push((last_kern, last_char));
                replacement
            }
        }
    }
    fn push(&mut self, c: LeftChar, kern: Number) {
        match self {
            ReplacementBuilder::Empty => {
                *self = ReplacementBuilder::NonEmpty {
                    replacement: Replacement(c, vec![]),
                    last_kern: kern,
                }
            }
            ReplacementBuilder::NonEmpty {
                replacement,
                last_kern,
            } => {
                replacement.1.push((
                    *last_kern,
                    c.try_into()
                        .expect("boundary char can't appear in the middle of a replacement"),
                ));
                *last_kern = kern;
            }
        }
    }

    fn extend(&mut self, replacement: &Replacement) -> LeftChar {
        let mut c: LeftChar = replacement.0;
        for (kern, next_c) in replacement.1.iter().copied() {
            self.push(c, kern);
            c = next_c.into();
        }
        c
    }

    fn chars(&self) -> Option<Chars> {
        match self {
            ReplacementBuilder::Empty => None,
            ReplacementBuilder::NonEmpty {
                replacement,
                last_kern: _,
            } => Some(Chars(
                replacement.0,
                replacement.1.iter().map(|(_, c)| *c).collect(),
            )),
        }
    }
}

fn calculate_replacements(
    instructions: &[lang::Instruction],
    kerns: &[Number],
    pair_to_instruction: HashMap<(LeftChar, Char), usize>,
) -> Result<HashMap<(LeftChar, Char), Replacement>, InfiniteLoopError> {
    let mut result: HashMap<(LeftChar, Char), Replacement> = Default::default();
    let mut actionable: Vec<OngoingCalculation> = vec![];
    let mut node_to_parents: HashMap<Node, Vec<OngoingCalculation>> = Default::default();
    for (&(left, right), &index) in &pair_to_instruction {
        let operation = instructions[index].operation;
        let operation = match operation {
            lang::Operation::EntrypointRedirect(u, _) => {
                // This reimplements the phantom ligature bug in tftopl.
                // TODO: in tfmtools don't reimplement these bugs.
                let [op_byte, remainder] = u.to_be_bytes();
                lang::Operation::lig_kern_operation_from_bytes(op_byte, remainder)
            }
            operation => operation,
        };
        let (pre_cursor, cursor, post_cursor): (ReplacementBuilder, LeftChar, TC) = match operation
        {
            lang::Operation::Kern(kern) => {
                result.insert((left, right), Replacement(left, vec![(kern, right)]));
                continue;
            }
            lang::Operation::KernAtIndex(index) => {
                result.insert(
                    (left, right),
                    Replacement(
                        left,
                        vec![(
                            kerns.get(index as usize).copied().unwrap_or_default(),
                            right,
                        )],
                    ),
                );
                continue;
            }
            lang::Operation::EntrypointRedirect(_, _) => {
                continue;
            }
            lang::Operation::Ligature {
                char_to_insert,
                post_lig_operation,
                post_lig_tag_invalid: _,
            } => match post_lig_operation {
                lang::PostLigOperation::RetainBothMoveNowhere => (
                    ReplacementBuilder::Empty,
                    left,
                    TC::Two(char_to_insert, right),
                ),
                lang::PostLigOperation::RetainBothMoveToInserted => (
                    ReplacementBuilder::new_single_char(left),
                    char_to_insert.into(),
                    TC::One(right),
                ),
                lang::PostLigOperation::RetainBothMoveToRight => (
                    ReplacementBuilder::new_double_char(left, char_to_insert),
                    right.into(),
                    TC::None { cursor: right },
                ),
                lang::PostLigOperation::RetainRightMoveToInserted => (
                    ReplacementBuilder::Empty,
                    char_to_insert.into(),
                    TC::One(right),
                ),
                lang::PostLigOperation::RetainRightMoveToRight => (
                    ReplacementBuilder::new_single_char(char_to_insert.into()),
                    right.into(),
                    TC::None { cursor: right },
                ),
                lang::PostLigOperation::RetainLeftMoveNowhere => {
                    (ReplacementBuilder::Empty, left, TC::One(char_to_insert))
                }
                lang::PostLigOperation::RetainLeftMoveToInserted => (
                    ReplacementBuilder::new_single_char(left),
                    char_to_insert.into(),
                    TC::None {
                        cursor: char_to_insert,
                    },
                ),
                lang::PostLigOperation::RetainNeitherMoveToInserted => (
                    ReplacementBuilder::Empty,
                    char_to_insert.into(),
                    TC::None {
                        cursor: char_to_insert,
                    },
                ),
            },
        };
        let node = Node(left, right);
        match post_cursor {
            TC::None { cursor } => {
                result.insert((node.0, node.1), pre_cursor.finish(cursor));
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
        let child = Node(calc.pending.0, calc.pending.1);
        if let Some(blocking) = node_to_parents.get_mut(&child) {
            blocking.push(calc);
            continue;
        }
        let last = match result.get(&(child.0, child.1)) {
            None => {
                calc.finalized.push(child.0, Number::ZERO);
                child.1
            }
            Some(replacement) => calc
                .finalized
                .extend(replacement)
                .try_into()
                .expect("boundary char can't be in the middle of a replacement"),
        };
        match calc.pending.2 {
            None => {
                if let Some(blocking) = node_to_parents.remove(&calc.node) {
                    actionable.extend(blocking);
                }
                result.insert((calc.node.0, calc.node.1), calc.finalized.finish(last));
            }
            Some(other) => {
                calc.pending = (last.into(), other, None);
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
        let starting_pair = infinite_loop[0];
        let mut replacement: Option<Chars> = None;
        let mut cursor_position = 0_usize;
        let mut steps = Vec::<InfiniteLoopStep>::new();
        for node in infinite_loop {
            // TODO: the bug here for /LIG/ nodes is that the loop may caused by the second dep of /LIG/
            // and so we've lost information on how we got from the start to here.
            let calc = &node_to_calc[&node];

            let (head, tail): (Option<Chars>, Vec<Char>) = match replacement.take() {
                None => (None, vec![]),
                Some(mut replacement) => {
                    if cursor_position == 0 {
                        (None, vec![])
                    } else {
                        let tail: Vec<Char> = replacement
                            .1
                            .get(cursor_position + 1..)
                            .unwrap_or_default()
                            .into();
                        replacement.1.truncate(cursor_position - 1);
                        (Some(replacement), tail)
                    }
                }
            };
            let head = match calc.finalized.chars() {
                None => head,
                Some(middle) => match head {
                    None => Some(middle),
                    Some(mut head) => {
                        head.push(middle.0);
                        head.1.extend(middle.1);
                        Some(head)
                    }
                },
            };
            let new_cursor_position = match &head {
                None => 0,
                Some(head) => 1 + head.1.len(),
            };
            let mut head = match head {
                None => Chars(calc.pending.0, vec![]),
                Some(mut head) => {
                    head.push(calc.pending.0);
                    head
                }
            };
            head.1.push(calc.pending.1);
            if let Some(c) = calc.pending.2 {
                head.1.push(c);
            }
            head.1.extend(tail);

            let post_replacement = match head.0 {
                LeftChar::Char(c) => {
                    let mut v = vec![c];
                    v.extend(&head.1);
                    (false, v)
                }
                LeftChar::BoundaryChar => (true, head.1.clone()),
            };
            replacement = Some(head);
            cursor_position = new_cursor_position;

            steps.push(InfiniteLoopStep {
                post_replacement,
                post_cursor_position: cursor_position,
                instruction_index: pair_to_instruction[&(node.0, node.1)],
            });
        }
        return Err(InfiniteLoopError {
            starting_pair: (starting_pair.0.char_or(), starting_pair.1),
            infinite_loop: steps,
        });
    }

    Ok(result)
}

#[derive(Debug)]
struct Chars(LeftChar, Vec<Char>);

impl Chars {
    fn push(&mut self, c: LeftChar) {
        self.1.push(
            c.try_into()
                .expect("left boundary char cannot appear in the middle of a replacement"),
        );
    }
}

enum TC {
    None { cursor: Char },
    One(Char),
    Two(Char, Char),
}

fn lower_and_optimize(
    pair_to_replacement: HashMap<(LeftChar, Char), Replacement>,
) -> CompiledProgram {
    let mut intermediate: HashMap<LeftChar, Vec<(Char, RawReplacement)>> = Default::default();
    let mut middle_chars: Vec<(Char, Number)> = Default::default();

    for (node, Replacement(head, tail)) in pair_to_replacement {
        let start: u16 = middle_chars.len().try_into().unwrap();

        let (left_char_operation, last_char) = match tail.first().copied() {
            None => {
                let head: Char = head.try_into().expect("boundary chars cannot appear as replacements (in this case for the deleted left char");
                (LeftCharOperation::Delete, head)
            }
            Some((first_kern, mut last_char)) => {
                let left_char_operation = if node.0 == head {
                    if first_kern == Number::ZERO {
                        LeftCharOperation::Retain
                    } else {
                        LeftCharOperation::AppendKern(first_kern)
                    }
                } else {
                    let head: Char = head.try_into().expect("boundary chars cannot appear as replacements (in this case for the deleted left char");
                    middle_chars.push((head, first_kern));
                    LeftCharOperation::Delete
                };
                for (kern, c) in tail[1..].iter().copied() {
                    middle_chars.push((last_char, kern));
                    last_char = c;
                }
                (left_char_operation, last_char)
            }
        };

        let end: u16 = middle_chars.len().try_into().unwrap();
        intermediate.entry(node.0).or_default().push((
            node.1,
            RawReplacement {
                left_char_operation,
                middle_char_bounds: start..end,
                last_char,
            },
        ));
    }

    let mut left_to_pairs: HashMap<Char, (u16, u16)> = Default::default();
    let mut pairs: Vec<(Char, RawReplacement)> = Default::default();

    for (left, replacements) in intermediate {
        let start: u16 = pairs.len().try_into().unwrap();
        pairs.extend(replacements);
        let end: u16 = pairs.len().try_into().unwrap();
        match left {
            LeftChar::Char(left) => {
                left_to_pairs.insert(left, (start, end));
            }
            LeftChar::BoundaryChar => {
                // TODO
            }
        }
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

    fn new_kern(next_instruction: Option<u8>, right_char: char, kern: Number) -> lang::Instruction {
        lang::Instruction {
            next_instruction,
            right_char: right_char.try_into().unwrap(),
            operation: lang::Operation::Kern(kern),
        }
    }

    pub fn new_lig(
        next_instruction: Option<u8>,
        right_char: char,
        char_to_insert: char,
        post_lig_operation: lang::PostLigOperation,
    ) -> lang::Instruction {
        lang::Instruction {
            next_instruction,
            right_char: right_char.try_into().unwrap(),
            operation: lang::Operation::Ligature {
                char_to_insert: char_to_insert.try_into().unwrap(),
                post_lig_operation,
                post_lig_tag_invalid: false,
            },
        }
    }

    pub fn new_stop(
        next_instruction: Option<u8>,
        right_char: char,
        entrypoint_redirect: u16,
    ) -> lang::Instruction {
        lang::Instruction {
            next_instruction,
            right_char: right_char.try_into().unwrap(),
            operation: lang::Operation::EntrypointRedirect(entrypoint_redirect, true),
        }
    }

    fn run_success_test(
        instructions: Vec<lang::Instruction>,
        entry_points: Vec<(char, u16)>,
        want: Vec<(char, char, Vec<(char, Number)>)>,
    ) {
        let entry_points: HashMap<Char, u16> = entry_points
            .into_iter()
            .map(|(c, u)| (c.try_into().unwrap(), u))
            .collect();
        let want: HashMap<(Char, Char), Vec<(Char, Number)>> = want
            .into_iter()
            .map(|t| {
                (
                    (t.0.try_into().unwrap(), t.1.try_into().unwrap()),
                    t.2.into_iter()
                        .map(|(c, n)| (c.try_into().unwrap(), n))
                        .collect(),
                )
            })
            .collect();
        let program = lang::Program {
            instructions,
            ..Default::default()
        };
        let compiled_program = compile(&program, &vec![], &entry_points).unwrap();

        let mut got: HashMap<(Char, Char), Vec<(Char, Number)>> = Default::default();
        for pair in compiled_program.all_pairs_having_replacement() {
            let replacement: Vec<(Char, Number)> = compiled_program
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
            vec![new_kern(None, 'V', Number::UNITY)],
            vec![('A', 0)],
            vec![('A', 'V', vec![('A', Number::UNITY), ('V', Number::ZERO)]),],
        ),
        (
            same_kern_for_multiple_left_characters,
            vec![new_kern(None, 'V', Number::UNITY)],
            vec![('A', 0), ('B', 0)],
            vec![
                ('A', 'V', vec![('A', Number::UNITY), ('V', Number::ZERO)]),
                ('B', 'V', vec![('B', Number::UNITY), ('V', Number::ZERO)]),
            ],
        ),
        (
            duplicate_kern,
            vec![
                new_kern(Some(0), 'V', Number::UNITY * 2),
                new_kern(None, 'V', Number::UNITY * 3),
            ],
            vec![('A', 0)],
            vec![(
                'A',
                'V',
                vec![('A', Number::UNITY * 2), ('V', Number::ZERO)]
            ),],
        ),
        (
            kern_instructions_with_relationship,
            vec![
                new_kern(Some(0), 'V', Number::UNITY * 2),
                new_kern(None, 'W', Number::UNITY * 3),
                new_kern(None, 'X', Number::UNITY * 4),
            ],
            vec![('A', 0), ('B', 1), ('C', 2)],
            vec![
                (
                    'A',
                    'V',
                    vec![('A', Number::UNITY * 2), ('V', Number::ZERO)]
                ),
                (
                    'A',
                    'W',
                    vec![('A', Number::UNITY * 3), ('W', Number::ZERO)]
                ),
                (
                    'B',
                    'W',
                    vec![('B', Number::UNITY * 3), ('W', Number::ZERO)]
                ),
                (
                    'C',
                    'X',
                    vec![('C', Number::UNITY * 4), ('X', Number::ZERO)]
                ),
            ],
        ),
        (
            single_lig_1,
            vec![new_lig(None, 'B', 'Z', RetainNeitherMoveToInserted)],
            vec![('A', 0)],
            vec![('A', 'B', vec![('Z', Number::ZERO)])],
        ),
        (
            single_lig_2,
            vec![
                new_lig(Some(0), 'B', 'Z', RetainLeftMoveToInserted),
                new_kern(None, 'Z', Number::UNITY),
            ],
            vec![('A', 0)],
            vec![
                ('A', 'B', vec![('A', Number::ZERO), ('Z', Number::ZERO)]),
                ('A', 'Z', vec![('A', Number::UNITY), ('Z', Number::ZERO)]),
            ],
        ),
        (
            retain_left_move_nowhere_1,
            vec![
                new_lig(Some(0), 'B', 'Z', RetainLeftMoveNowhere),
                new_kern(None, 'Z', Number::UNITY),
            ],
            vec![('A', 0)],
            vec![
                ('A', 'B', vec![('A', Number::UNITY), ('Z', Number::ZERO)]),
                ('A', 'Z', vec![('A', Number::UNITY), ('Z', Number::ZERO)]),
            ],
        ),
        (
            retain_left_move_nowhere_2,
            vec![new_lig(None, 'B', 'Z', RetainLeftMoveNowhere),],
            vec![('A', 0)],
            vec![('A', 'B', vec![('A', Number::ZERO), ('Z', Number::ZERO)]),],
        ),
        (
            single_lig_4,
            vec![
                new_lig(None, 'B', 'Z', RetainRightMoveToInserted),
                new_kern(None, 'B', Number::UNITY),
            ],
            vec![('A', 0), ('Z', 1)],
            vec![
                ('A', 'B', vec![('Z', Number::UNITY), ('B', Number::ZERO)]),
                ('Z', 'B', vec![('Z', Number::UNITY), ('B', Number::ZERO)]),
            ],
        ),
        (
            single_lig_5,
            vec![
                new_lig(None, 'B', 'Z', RetainRightMoveToRight),
                new_kern(None, 'B', Number::UNITY),
            ],
            vec![('A', 0), ('Z', 1)],
            vec![
                ('A', 'B', vec![('Z', Number::ZERO), ('B', Number::ZERO)]),
                ('Z', 'B', vec![('Z', Number::UNITY), ('B', Number::ZERO)]),
            ],
        ),
        (
            retain_both_move_nowhere_1,
            vec![
                new_lig(Some(0), 'B', 'Z', RetainBothMoveNowhere,),
                new_kern(None, 'Z', Number::UNITY * 2),
                new_kern(None, 'B', Number::UNITY * 3),
            ],
            vec![('A', 0), ('Z', 2)],
            vec![
                (
                    'A',
                    'B',
                    vec![
                        ('A', Number::UNITY * 2),
                        ('Z', Number::UNITY * 3),
                        ('B', Number::ZERO)
                    ]
                ),
                (
                    'A',
                    'Z',
                    vec![('A', Number::UNITY * 2), ('Z', Number::ZERO)]
                ),
                (
                    'Z',
                    'B',
                    vec![('Z', Number::UNITY * 3), ('B', Number::ZERO)]
                ),
            ],
        ),
        (
            retain_both_move_nowhere_2,
            vec![new_lig(None, 'B', 'Z', RetainBothMoveNowhere),],
            vec![('A', 0)],
            vec![(
                'A',
                'B',
                vec![
                    ('A', Number::ZERO),
                    ('Z', Number::ZERO),
                    ('B', Number::ZERO)
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
                        ('A', Number::ZERO),
                        ('Y', Number::ZERO),
                        ('B', Number::ZERO)
                    ]
                ),
                ('Z', 'B', vec![('Y', Number::ZERO), ('B', Number::ZERO),]),
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
                        ('A', Number::ZERO),
                        ('Y', Number::ZERO),
                        ('Z', Number::ZERO),
                        ('B', Number::ZERO)
                    ]
                ),
                (
                    'A',
                    'Z',
                    vec![
                        ('A', Number::ZERO),
                        ('Y', Number::ZERO),
                        ('Z', Number::ZERO)
                    ]
                ),
            ],
        ),
        (
            retain_both_move_to_inserted_1,
            vec![
                new_lig(Some(0), 'B', 'Z', RetainBothMoveToInserted,),
                new_kern(None, 'Z', Number::UNITY * 2),
                new_kern(None, 'B', Number::UNITY * 3),
            ],
            vec![('A', 0), ('Z', 2)],
            vec![
                (
                    'A',
                    'B',
                    vec![
                        ('A', Number::ZERO),
                        ('Z', Number::UNITY * 3),
                        ('B', Number::ZERO)
                    ]
                ),
                (
                    'A',
                    'Z',
                    vec![('A', Number::UNITY * 2), ('Z', Number::ZERO)]
                ),
                (
                    'Z',
                    'B',
                    vec![('Z', Number::UNITY * 3), ('B', Number::ZERO)]
                ),
            ],
        ),
        (
            retain_both_move_to_inserted_2,
            vec![
                new_lig(None, 'B', 'Z', RetainBothMoveToInserted),
                new_lig(None, 'B', 'Y', RetainRightMoveToInserted),
            ],
            vec![('A', 0), ('Z', 1)],
            vec![
                (
                    'A',
                    'B',
                    vec![
                        ('A', Number::ZERO),
                        ('Y', Number::ZERO),
                        ('B', Number::ZERO)
                    ]
                ),
                ('Z', 'B', vec![('Y', Number::ZERO * 3), ('B', Number::ZERO)]),
            ],
        ),
        (
            retain_both_move_to_inserted_3,
            vec![new_lig(None, 'B', 'Z', RetainBothMoveToInserted),],
            vec![('A', 0)],
            vec![(
                'A',
                'B',
                vec![
                    ('A', Number::ZERO),
                    ('Z', Number::ZERO),
                    ('B', Number::ZERO)
                ]
            ),],
        ),
        (
            retain_both_move_to_inserted_4,
            vec![
                new_lig(None, 'B', 'Z', RetainBothMoveToInserted,),
                new_lig(None, 'B', 'Y', RetainRightMoveToRight),
            ],
            vec![('A', 0), ('Z', 1)],
            vec![
                (
                    'A',
                    'B',
                    vec![
                        ('A', Number::ZERO),
                        ('Y', Number::ZERO),
                        ('B', Number::ZERO)
                    ]
                ),
                ('Z', 'B', vec![('Y', Number::ZERO * 3), ('B', Number::ZERO)]),
            ],
        ),
        (
            retain_both_move_to_right_1,
            vec![
                new_lig(Some(0), 'B', 'Z', RetainBothMoveToRight,),
                new_kern(None, 'Z', Number::UNITY * 2),
                new_kern(None, 'B', Number::UNITY * 3),
            ],
            vec![('A', 0), ('Z', 2)],
            vec![
                (
                    'A',
                    'B',
                    vec![
                        ('A', Number::ZERO),
                        ('Z', Number::ZERO),
                        ('B', Number::ZERO)
                    ]
                ),
                (
                    'A',
                    'Z',
                    vec![('A', Number::UNITY * 2), ('Z', Number::ZERO)]
                ),
                (
                    'Z',
                    'B',
                    vec![('Z', Number::UNITY * 3), ('B', Number::ZERO)]
                ),
            ],
        ),
    );

    fn run_infinite_loop_test(
        instructions: Vec<lang::Instruction>,
        entry_points: Vec<(char, u16)>,
        want_err: InfiniteLoopError,
    ) {
        let program = lang::Program {
            instructions,
            ..Default::default()
        };
        let entry_points: HashMap<Char, u16> = entry_points
            .into_iter()
            .map(|(c, u)| (c.try_into().unwrap(), u))
            .collect();
        let got_err = compile(&program, &vec![], &entry_points).unwrap_err();
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
            InfiniteLoopError {
                starting_pair: starting_pair('A', 'B'),
                infinite_loop: vec![InfiniteLoopStep {
                    instruction_index: 0,
                    post_replacement: post_replacement("AAB"),
                    post_cursor_position: 1,
                }]
            },
        ),
        (
            duplicate_command_single_char,
            vec![
                new_lig(Some(0), 'A', 'A', RetainLeftMoveNowhere),
                new_stop(None, 'A', 225),
            ],
            vec![('A', 0)],
            InfiniteLoopError {
                starting_pair: starting_pair('A', 'A'),
                infinite_loop: vec![InfiniteLoopStep {
                    instruction_index: 0,
                    post_replacement: post_replacement("AA"),
                    post_cursor_position: 0,
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
            InfiniteLoopError {
                starting_pair: starting_pair('A', 'B'),
                infinite_loop: vec![
                    InfiniteLoopStep {
                        instruction_index: 0,
                        post_replacement: post_replacement("CB"),
                        post_cursor_position: 0,
                    },
                    InfiniteLoopStep {
                        instruction_index: 1,
                        post_replacement: post_replacement("AB"),
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
                starting_pair: starting_pair('A', 'B'),
                infinite_loop: vec![
                    InfiniteLoopStep {
                        instruction_index: 0,
                        post_replacement: post_replacement("ACB"),
                        post_cursor_position: 0,
                    },
                    InfiniteLoopStep {
                        instruction_index: 1,
                        post_replacement: post_replacement("DCB"),
                        post_cursor_position: 0,
                    },
                    InfiniteLoopStep {
                        instruction_index: 2,
                        post_replacement: post_replacement("DAB"),
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
            InfiniteLoopError {
                starting_pair: starting_pair('A', 'B'),
                infinite_loop: vec![
                    InfiniteLoopStep {
                        instruction_index: 0,
                        post_replacement: post_replacement("ACB"),
                        post_cursor_position: 1,
                    },
                    InfiniteLoopStep {
                        instruction_index: 1,
                        post_replacement: post_replacement("ACAB"),
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
            InfiniteLoopError {
                starting_pair: starting_pair('C', 'B'),
                infinite_loop: vec![InfiniteLoopStep {
                    instruction_index: 1,
                    post_replacement: post_replacement("CCB"),
                    post_cursor_position: 1,
                }]
            },
        ),
    );

    fn starting_pair(l: char, r: char) -> (Option<Char>, Char) {
        (Some(l.try_into().unwrap()), r.try_into().unwrap())
    }

    fn post_replacement(s: &str) -> (bool, Vec<Char>) {
        (false, s.chars().map(|c| c.try_into().unwrap()).collect())
    }
}
