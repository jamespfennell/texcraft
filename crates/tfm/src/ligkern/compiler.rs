use super::*;
use std::collections::HashSet;

pub fn compile(
    program: &lang::Program,
    kerns: &[FixWord],
    entry_points: &HashMap<Char, u16>,
) -> (CompiledProgram, Vec<InfiniteLoopError>) {
    let pair_to_instruction = build_pair_to_instruction_map(program, entry_points);
    let (pair_to_replacement, infinite_loop_errors) =
        calculate_replacements(program, kerns, pair_to_instruction);
    let program = lower_and_optimize(pair_to_replacement);
    (program, infinite_loop_errors)
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord)]
struct Node(LeftChar, Char);

struct OngoingCalculation {
    // Node this calculation is for.
    node: Node,
    // Part of the ligature result that has been finalized and won't change.
    finalized: Finalized,
    // Characters that are still pending replacement. The next step is to apply the ligature
    // rule for the node. After that, if the second element is not empty, the next step
    // is to apply the ligature rule for (tuple.0.1, tuple.1).
    pending: (Node, Option<Char>),
}

impl OngoingCalculation {
    fn child(&self) -> Node {
        self.pending.0
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
) -> HashMap<Node, usize> {
    let mut result = HashMap::<Node, usize>::new();
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
            result
                .entry(Node(left, instruction.right_char))
                .or_insert(next);
        }
    }
    result
}

// TODO: replace the first LeftChar with a bool indicating whether the
// first character is deleted. Anytime we need the first char, get it from
// the context. I think in this way we can remove all of the expect() calls
// on boundary chars.
struct Replacement(LeftChar, Vec<(FixWord, Char)>);

enum Finalized {
    Empty,
    NonEmpty {
        replacement: Replacement,
        last_kern: FixWord,
    },
}

impl Finalized {
    fn new_single_char(c1: LeftChar) -> Self {
        Finalized::NonEmpty {
            replacement: Replacement(c1, vec![]),
            last_kern: FixWord::ZERO,
        }
    }
    fn new_double_char(c1: LeftChar, c2: Char) -> Self {
        Finalized::NonEmpty {
            replacement: Replacement(c1, vec![(FixWord::ZERO, c2)]),
            last_kern: FixWord::ZERO,
        }
    }
    fn finish(self, last_char: Char) -> Replacement {
        match self {
            Finalized::Empty => Replacement(last_char.into(), vec![]),
            Finalized::NonEmpty {
                mut replacement,
                last_kern,
            } => {
                replacement.1.push((last_kern, last_char));
                replacement
            }
        }
    }
    fn push(&mut self, c: LeftChar, kern: FixWord) {
        match self {
            Finalized::Empty => {
                *self = Finalized::NonEmpty {
                    replacement: Replacement(c, vec![]),
                    last_kern: kern,
                }
            }
            Finalized::NonEmpty {
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
}

fn calculate_replacements(
    program: &lang::Program,
    kerns: &[FixWord],
    pair_to_instruction: HashMap<Node, usize>,
) -> (HashMap<Node, Replacement>, Vec<InfiniteLoopError>) {
    let mut result: HashMap<Node, Replacement> = Default::default();
    let mut actionable: Vec<OngoingCalculation> = vec![];
    let mut node_to_parents: HashMap<Node, Vec<OngoingCalculation>> = Default::default();
    for (&pair, &index) in &pair_to_instruction {
        let Node(left, right) = pair;
        let operation = program.instructions[index].operation;
        let operation = match operation {
            lang::Operation::EntrypointRedirect(u, _) => {
                // This reimplements the phantom ligature bug in tftopl.
                // TODO: in tfmtools don't reimplement these bugs.
                let [op_byte, remainder] = u.to_be_bytes();
                lang::Operation::lig_kern_operation_from_bytes(op_byte, remainder)
            }
            operation => operation,
        };
        enum Pending {
            None(Char),
            One(Node),
            Two(Node, Char),
        }
        let (finalized, pending): (Finalized, Pending) = match operation {
            lang::Operation::Kern(kern) => {
                result.insert(pair, Replacement(left, vec![(kern, right)]));
                continue;
            }
            lang::Operation::KernAtIndex(index) => {
                result.insert(
                    pair,
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
                    Finalized::Empty,
                    Pending::Two(Node(left, char_to_insert), right),
                ),
                lang::PostLigOperation::RetainBothMoveToInserted => (
                    Finalized::new_single_char(left),
                    Pending::One(Node(char_to_insert.into(), right)),
                ),
                lang::PostLigOperation::RetainBothMoveToRight => (
                    Finalized::new_double_char(left, char_to_insert),
                    Pending::None(right),
                ),
                lang::PostLigOperation::RetainRightMoveToInserted => (
                    Finalized::Empty,
                    Pending::One(Node(char_to_insert.into(), right)),
                ),
                lang::PostLigOperation::RetainRightMoveToRight => (
                    Finalized::new_single_char(char_to_insert.into()),
                    Pending::None(right),
                ),
                lang::PostLigOperation::RetainLeftMoveNowhere => {
                    (Finalized::Empty, Pending::One(Node(left, char_to_insert)))
                }
                lang::PostLigOperation::RetainLeftMoveToInserted => (
                    Finalized::new_single_char(left),
                    Pending::None(char_to_insert),
                ),
                lang::PostLigOperation::RetainNeitherMoveToInserted => {
                    (Finalized::Empty, Pending::None(char_to_insert))
                }
            },
        };
        match pending {
            Pending::None(cursor) => {
                result.insert(pair, finalized.finish(cursor));
            }
            Pending::One(node) => {
                actionable.push(OngoingCalculation {
                    node: pair,
                    finalized,
                    pending: (node, None),
                });
                node_to_parents.insert(pair, vec![]);
            }
            Pending::Two(node, c3) => {
                actionable.push(OngoingCalculation {
                    node: pair,
                    finalized,
                    pending: (node, Some(c3)),
                });
                node_to_parents.insert(pair, vec![]);
            }
        };
    }

    while let Some(mut calc) = actionable.pop() {
        let child = calc.child();
        if let Some(blocking) = node_to_parents.get_mut(&child) {
            blocking.push(calc);
            continue;
        }
        let last = match result.get(&child) {
            None => {
                calc.finalized.push(child.0, FixWord::ZERO);
                child.1
            }
            Some(replacement) => calc
                .finalized
                .extend(replacement)
                .try_into()
                .expect("boundary char can't be in the middle of a replacement"),
        };
        match calc.pending.1 {
            None => {
                if let Some(blocking) = node_to_parents.remove(&calc.node) {
                    actionable.extend(blocking);
                }
                result.insert(calc.node, calc.finalized.finish(last));
            }
            Some(other) => {
                calc.pending = (Node(last.into(), other), None);
                actionable.push(calc);
            }
        }
    }

    // Next we check for infinite loops. If there is one, the node_to_parents
    // map will be non-empty and contain all the nodes that couldn't be calculated.
    //
    // The main complication here is that there are multiple nodes we can report
    // as being the cause of the infinite loop. E.g., the loop could be
    // (A,R) -> (B,R) -> (C,R) -> (A,R), and we could report any of these 3 nodes.
    // What we want to do, though, is report the same node that Knuth does in tftopl
    // and pltotf. Thus the algorithm here replicates what Knuth does.
    //
    // Knuth iterates over all nodes in the following order: in lexigraphical order
    // for the left pair (with the boundary char last), and in the instruction order
    // for the right pair. I.e, if we have pairs (A,R) and (B,R) the pair whose
    // instruction comes first in the lig/kern program will come first.
    //
    // Then, given such a node, Knuth performs a path traversal following each
    // node's dependencies. The first node that is seen twice in the traversal is the
    // node that is considered to break the infinite loop. Knuth then essentially breaks
    // the loop and moves on. Note that Knuth's algorithm correctly handles cases
    // like (A,R) -> (B,R) -> (C,R) -> (B,R) - in such a case, the first node we see (A,R)
    // isn't actually what causes the loop.
    //
    // The process described in the last paragraph can happen multiple times if there
    // are multiple infinite loops. Knuth reports the node from the last infinite loop.
    // Note that we have E2E tests that cover these kinds of cases.
    let mut node_to_child: HashMap<Node, Node> = node_to_parents
        .into_values()
        .flatten()
        .map(|calc| (calc.node, calc.child()))
        .collect();
    let mut knuth_ordered_nodes: Vec<Node> = node_to_child.keys().copied().collect();
    knuth_ordered_nodes.sort_by(|lhs, rhs| {
        lhs.0
            .cmp(&rhs.0)
            .then(pair_to_instruction[lhs].cmp(&pair_to_instruction[rhs]))
    });

    let mut infinite_loop_errors: Vec<InfiniteLoopError> = vec![];
    for mut node in knuth_ordered_nodes {
        let mut seen = HashSet::<Node>::new();
        while let Some(child) = node_to_child.remove(&node) {
            seen.insert(node);
            node = child;
        }
        // As mentioned above, when Knuth finds an infinite loop he breaks it.
        // It's possible that the node that started this iteration is part of an infinite
        // loop that has already been broken. For example the lig/kern program could be:
        // - (A,R) -> (C,R) -> (C,R)
        // - (B,R) -> (C,R) -> (C,R)
        // In this case when considering (B,R) there is nothing to do because the (C,R)
        // loop has already been broken.
        //
        // We detect this case by keeping track of which nodes we've seen for the first
        // time in this traversal.
        // If we haven't seen it, the loop has already been broken. There is an E2E test
        // for this case.
        if seen.contains(&node) {
            infinite_loop_errors.push(InfiniteLoopError {
                starting_pair: (node.0.char_or(), node.1),
            });
        }
    }

    (result, infinite_loop_errors)
}

fn lower_and_optimize(pair_to_replacement: HashMap<Node, Replacement>) -> CompiledProgram {
    let mut intermediate: HashMap<LeftChar, Vec<(Char, RawReplacement)>> = Default::default();
    let mut middle_chars: Vec<(Char, FixWord)> = Default::default();

    for (node, Replacement(head, tail)) in pair_to_replacement {
        let start: u16 = middle_chars.len().try_into().unwrap();

        let (left_char_operation, last_char) = match tail.first().copied() {
            None => {
                let head: Char = head.try_into().expect("boundary chars cannot appear as replacements (in this case for the deleted left char");
                (LeftCharOperation::Delete, head)
            }
            Some((first_kern, mut last_char)) => {
                let left_char_operation = if node.0 == head {
                    if first_kern == FixWord::ZERO {
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

    let mut left_to_pairs: BTreeMap<Char, (u16, u16)> = Default::default();
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

    fn new_kern(
        next_instruction: Option<u8>,
        right_char: char,
        kern: FixWord,
    ) -> lang::Instruction {
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

    fn run_success_test(
        instructions: Vec<lang::Instruction>,
        entry_points: Vec<(char, u16)>,
        want: Vec<(char, char, Vec<(char, FixWord)>)>,
    ) {
        let entry_points: HashMap<Char, u16> = entry_points
            .into_iter()
            .map(|(c, u)| (c.try_into().unwrap(), u))
            .collect();
        let want: HashMap<(Char, Char), Vec<(Char, FixWord)>> = want
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
        let (compiled_program, infinite_loop_error_or) = compile(&program, &vec![], &entry_points);
        assert!(infinite_loop_error_or.is_empty(), "no infinite loop errors");

        let mut got: HashMap<(Char, Char), Vec<(Char, FixWord)>> = Default::default();
        for pair in compiled_program.all_pairs_having_replacement() {
            let replacement: Vec<(Char, FixWord)> = compiled_program
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
            vec![new_kern(None, 'V', FixWord::ONE)],
            vec![('A', 0)],
            vec![('A', 'V', vec![('A', FixWord::ONE), ('V', FixWord::ZERO)]),],
        ),
        (
            same_kern_for_multiple_left_characters,
            vec![new_kern(None, 'V', FixWord::ONE)],
            vec![('A', 0), ('B', 0)],
            vec![
                ('A', 'V', vec![('A', FixWord::ONE), ('V', FixWord::ZERO)]),
                ('B', 'V', vec![('B', FixWord::ONE), ('V', FixWord::ZERO)]),
            ],
        ),
        (
            duplicate_kern,
            vec![
                new_kern(Some(0), 'V', FixWord::ONE * 2),
                new_kern(None, 'V', FixWord::ONE * 3),
            ],
            vec![('A', 0)],
            vec![(
                'A',
                'V',
                vec![('A', FixWord::ONE * 2), ('V', FixWord::ZERO)]
            ),],
        ),
        (
            kern_instructions_with_relationship,
            vec![
                new_kern(Some(0), 'V', FixWord::ONE * 2),
                new_kern(None, 'W', FixWord::ONE * 3),
                new_kern(None, 'X', FixWord::ONE * 4),
            ],
            vec![('A', 0), ('B', 1), ('C', 2)],
            vec![
                (
                    'A',
                    'V',
                    vec![('A', FixWord::ONE * 2), ('V', FixWord::ZERO)]
                ),
                (
                    'A',
                    'W',
                    vec![('A', FixWord::ONE * 3), ('W', FixWord::ZERO)]
                ),
                (
                    'B',
                    'W',
                    vec![('B', FixWord::ONE * 3), ('W', FixWord::ZERO)]
                ),
                (
                    'C',
                    'X',
                    vec![('C', FixWord::ONE * 4), ('X', FixWord::ZERO)]
                ),
            ],
        ),
        (
            single_lig_1,
            vec![new_lig(None, 'B', 'Z', RetainNeitherMoveToInserted)],
            vec![('A', 0)],
            vec![('A', 'B', vec![('Z', FixWord::ZERO)])],
        ),
        (
            single_lig_2,
            vec![
                new_lig(Some(0), 'B', 'Z', RetainLeftMoveToInserted),
                new_kern(None, 'Z', FixWord::ONE),
            ],
            vec![('A', 0)],
            vec![
                ('A', 'B', vec![('A', FixWord::ZERO), ('Z', FixWord::ZERO)]),
                ('A', 'Z', vec![('A', FixWord::ONE), ('Z', FixWord::ZERO)]),
            ],
        ),
        (
            retain_left_move_nowhere_1,
            vec![
                new_lig(Some(0), 'B', 'Z', RetainLeftMoveNowhere),
                new_kern(None, 'Z', FixWord::ONE),
            ],
            vec![('A', 0)],
            vec![
                ('A', 'B', vec![('A', FixWord::ONE), ('Z', FixWord::ZERO)]),
                ('A', 'Z', vec![('A', FixWord::ONE), ('Z', FixWord::ZERO)]),
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
                new_lig(None, 'B', 'Z', RetainRightMoveToInserted),
                new_kern(None, 'B', FixWord::ONE),
            ],
            vec![('A', 0), ('Z', 1)],
            vec![
                ('A', 'B', vec![('Z', FixWord::ONE), ('B', FixWord::ZERO)]),
                ('Z', 'B', vec![('Z', FixWord::ONE), ('B', FixWord::ZERO)]),
            ],
        ),
        (
            single_lig_5,
            vec![
                new_lig(None, 'B', 'Z', RetainRightMoveToRight),
                new_kern(None, 'B', FixWord::ONE),
            ],
            vec![('A', 0), ('Z', 1)],
            vec![
                ('A', 'B', vec![('Z', FixWord::ZERO), ('B', FixWord::ZERO)]),
                ('Z', 'B', vec![('Z', FixWord::ONE), ('B', FixWord::ZERO)]),
            ],
        ),
        (
            retain_both_move_nowhere_1,
            vec![
                new_lig(Some(0), 'B', 'Z', RetainBothMoveNowhere,),
                new_kern(None, 'Z', FixWord::ONE * 2),
                new_kern(None, 'B', FixWord::ONE * 3),
            ],
            vec![('A', 0), ('Z', 2)],
            vec![
                (
                    'A',
                    'B',
                    vec![
                        ('A', FixWord::ONE * 2),
                        ('Z', FixWord::ONE * 3),
                        ('B', FixWord::ZERO)
                    ]
                ),
                (
                    'A',
                    'Z',
                    vec![('A', FixWord::ONE * 2), ('Z', FixWord::ZERO)]
                ),
                (
                    'Z',
                    'B',
                    vec![('Z', FixWord::ONE * 3), ('B', FixWord::ZERO)]
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
                new_lig(Some(0), 'B', 'Z', RetainBothMoveToInserted,),
                new_kern(None, 'Z', FixWord::ONE * 2),
                new_kern(None, 'B', FixWord::ONE * 3),
            ],
            vec![('A', 0), ('Z', 2)],
            vec![
                (
                    'A',
                    'B',
                    vec![
                        ('A', FixWord::ZERO),
                        ('Z', FixWord::ONE * 3),
                        ('B', FixWord::ZERO)
                    ]
                ),
                (
                    'A',
                    'Z',
                    vec![('A', FixWord::ONE * 2), ('Z', FixWord::ZERO)]
                ),
                (
                    'Z',
                    'B',
                    vec![('Z', FixWord::ONE * 3), ('B', FixWord::ZERO)]
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
            vec![new_lig(None, 'B', 'Z', RetainBothMoveToInserted),],
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
                new_lig(None, 'B', 'Z', RetainBothMoveToInserted,),
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
                new_lig(Some(0), 'B', 'Z', RetainBothMoveToRight,),
                new_kern(None, 'Z', FixWord::ONE * 2),
                new_kern(None, 'B', FixWord::ONE * 3),
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
                    vec![('A', FixWord::ONE * 2), ('Z', FixWord::ZERO)]
                ),
                (
                    'Z',
                    'B',
                    vec![('Z', FixWord::ONE * 3), ('B', FixWord::ZERO)]
                ),
            ],
        ),
    );
}
