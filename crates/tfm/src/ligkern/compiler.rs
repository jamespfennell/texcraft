use super::*;
use std::collections::HashSet;

pub fn compile(
    program: &lang::Program,
    kerns: &[FixWord],
    entry_points: &HashMap<Char, u16>,
) -> (CompiledProgram, Vec<InfiniteLoopError>) {
    let pair_to_instruction = build_node_to_program_start_map(program, entry_points);
    let (replacements, infinite_loop_errors) =
        calculate_replacements(program, kerns, pair_to_instruction);
    let program = CompiledProgram {
        replacements: replacements
            .into_iter()
            .filter_map(|(node, replacement)| {
                match node.0 {
                    LeftChar::Char(char) => Some(((char, node.1), replacement)),
                    LeftChar::BoundaryChar => None, // TODO
                }
            })
            .collect(),
    };
    println!(
        "{:?}",
        program
            .replacements
            .get(&('a'.try_into().unwrap(), 'c'.try_into().unwrap()))
    );
    (program, infinite_loop_errors)
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord)]
struct Node(LeftChar, Char);

struct OngoingCalculation {
    // Node this calculation is for.
    // TODO: rename root?
    node: Node,

    finalized: Vec<IntermediateOp>,
    // Characters that are still pending replacement. The next step is to apply the ligature
    // rule for the node. After that, if the second element is not empty, the next step
    // is to apply the ligature rule for (tuple.0.1, tuple.1).
    // pending_new: (LigOrChar, LigOrChar, Option<LigOrChar>),
    // Invariant: either 2 or 3 elements in size?
    // Lets make a special type for this?
    pending: Vec<C>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct C {
    pub c: Char,
    pub is_lig: bool,
    pub consumes_left: bool,
    pub consumes_right: bool,
}

impl C {
    fn char(c: Char, is_left_char: bool) -> Self {
        Self {
            c,
            is_lig: false,
            consumes_left: is_left_char,
            consumes_right: !is_left_char,
        }
    }
    fn merge(&self, left: &C, right: &C) -> Self {
        match (self.consumes_left, self.consumes_right) {
            (true, true) => C {
                c: self.c,
                // TODO: should we have the self.is_lig here?
                // I think it's correct: if any node in the provenance chain is a lig,
                // then the result should be a lig.
                // Test this by removing it and seeing if unit tests pass.
                is_lig: self.is_lig || left.is_lig || right.is_lig,
                consumes_left: left.consumes_left || right.consumes_left,
                consumes_right: left.consumes_right || right.consumes_right,
            },
            (true, false) => C {
                c: self.c,
                is_lig: self.is_lig || left.is_lig,
                consumes_left: left.consumes_left,
                consumes_right: left.consumes_right,
            },
            (false, true) => C {
                c: self.c,
                is_lig: self.is_lig || right.is_lig,
                consumes_left: right.consumes_left,
                consumes_right: right.consumes_right,
            },
            (false, false) => self.clone(),
        }
    }
}

impl OngoingCalculation {
    fn child(&self) -> Node {
        Node(self.pending[0].c.into(), self.pending[1].c.into())
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

impl std::fmt::Display for LeftChar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LeftChar::Char(char) => write!(f, "{char}"),
            LeftChar::BoundaryChar => Ok(()),
        }
    }
}

fn build_node_to_program_start_map(
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct Replacement(pub(crate) Vec<IntermediateOp>, pub C);

fn calculate_replacements(
    program: &lang::Program,
    kerns: &[FixWord],
    pair_to_instruction: HashMap<Node, usize>,
) -> (HashMap<Node, Replacement>, Vec<InfiniteLoopError>) {
    let mut new_result: HashMap<Node, Replacement> = Default::default();
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
        let (finalized, pending): (Vec<C>, Vec<C>) = match operation {
            lang::Operation::Kern(kern) => {
                new_result.insert(
                    pair,
                    Replacement(
                        vec![
                            IntermediateOp::C(C::char(left.try_into().unwrap(), true)),
                            IntermediateOp::Kern(kern),
                        ],
                        C::char(right, false),
                    ),
                );
                continue;
            }
            lang::Operation::KernAtIndex(index) => {
                let kern = kerns.get(index as usize).copied().unwrap_or_default();
                new_result.insert(
                    pair,
                    Replacement(
                        vec![
                            IntermediateOp::C(C::char(left.try_into().unwrap(), true)),
                            IntermediateOp::Kern(kern),
                        ],
                        C::char(right, false),
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
                    // finalized
                    vec![],
                    // pending
                    vec![
                        C::char(left.try_into().unwrap(), true),
                        C {
                            c: char_to_insert,
                            is_lig: true,
                            consumes_left: false,
                            consumes_right: false,
                        },
                        C::char(right.try_into().unwrap(), false),
                    ],
                ),
                lang::PostLigOperation::RetainBothMoveToInserted => (
                    // finalized
                    vec![C::char(left.try_into().unwrap(), true)],
                    // pending
                    vec![
                        C {
                            c: char_to_insert,
                            is_lig: true,
                            consumes_left: false,
                            consumes_right: false,
                        },
                        C::char(right.try_into().unwrap(), false),
                    ],
                ),
                lang::PostLigOperation::RetainBothMoveToRight => (
                    // finalized
                    vec![
                        C::char(left.try_into().unwrap(), true),
                        C {
                            c: char_to_insert,
                            is_lig: true,
                            consumes_left: false,
                            consumes_right: false,
                        },
                        C::char(right.try_into().unwrap(), false),
                    ],
                    // pending
                    vec![],
                ),
                lang::PostLigOperation::RetainRightMoveToInserted => (
                    // finalized
                    vec![],
                    // pending
                    vec![
                        C {
                            c: char_to_insert,
                            is_lig: true,
                            consumes_left: true,
                            consumes_right: false,
                        },
                        C::char(right.try_into().unwrap(), false),
                    ],
                ),
                lang::PostLigOperation::RetainRightMoveToRight => (
                    // finalized
                    vec![
                        C {
                            c: char_to_insert,
                            is_lig: true,
                            consumes_left: true,
                            consumes_right: false,
                        },
                        C::char(right.try_into().unwrap(), false),
                    ],
                    // pending
                    vec![],
                ),
                lang::PostLigOperation::RetainLeftMoveNowhere => (
                    // finalized
                    vec![],
                    // pending
                    vec![
                        C::char(left.try_into().unwrap(), true),
                        C {
                            c: char_to_insert,
                            is_lig: true,
                            consumes_left: false,
                            consumes_right: true,
                        },
                    ],
                ),
                lang::PostLigOperation::RetainLeftMoveToInserted => (
                    // finalized
                    vec![
                        C::char(left.try_into().unwrap(), true),
                        C {
                            c: char_to_insert,
                            is_lig: true,
                            consumes_left: false,
                            consumes_right: true,
                        },
                    ],
                    // pending
                    vec![],
                ),
                lang::PostLigOperation::RetainNeitherMoveToInserted => (
                    // finalized
                    vec![C {
                        c: char_to_insert,
                        is_lig: true,
                        consumes_left: true,
                        consumes_right: true,
                    }],
                    //pending
                    vec![],
                ),
            },
        };
        if !pending.is_empty() {
            let finalized = finalized
                .into_iter()
                .map(|c| IntermediateOp::C(c))
                .collect();
            actionable.push(OngoingCalculation {
                node: pair,
                finalized,
                pending,
            });
            node_to_parents.insert(pair, vec![]);
        } else {
            let mut finalized = finalized;
            let last = finalized.pop().unwrap();
            let finalized = finalized
                .into_iter()
                .map(|c| IntermediateOp::C(c))
                .collect();
            new_result.insert(pair, Replacement(finalized, last));
        }
    }

    while let Some(mut calc) = actionable.pop() {
        let child = calc.child();
        if let Some(blocking) = node_to_parents.get_mut(&child) {
            blocking.push(calc);
            continue;
        }

        let last_1 = match new_result.get(&child) {
            None => {
                // There is no lig/kern rule for this pair.
                let left = calc.pending[0].clone();
                let right = calc.pending[1].clone();
                calc.finalized.push(IntermediateOp::C(left));
                right
            }
            Some(replacement) => {
                println!("FINALIZING FOR node={:?}", calc.node);
                println!("FINALIZING FOR pending={:?}", calc.pending);
                println!("FINALIZING FOR replacement={:?}", replacement);
                // TODO: this is not good enough :( <- this is where the test is failing!
                // We may need to modify some ligature nodes
                // we need to iterate over the Replacement value and replace char or lig originals
                // with calc.new_pending[0] and calc.new_pending[1] - either the char
                // or the content of the ligature

                // maybe we need to track the provenance of nodes so that we
                // can convert the char nodes in the replacement into the potential lig
                // nodes in the child pair
                // e.g. child = ((Z, lig), (B, char))
                // replacement for ZB is (Z, char)<kern>(B, char)
                // what we actually need to do is: (z, lig)<kern>(B, char)
                // maybe C should have LeftChar variants RightChar?

                // we find out where the left and right chars map to
                // either using lig consumes_left/right
                // or a new provenance field for char
                // then we merge the left and right char into the replacement
                // merging means we overwrite the replacement with side-channel data in
                // the original

                // let left = calc.pending[0].clone();
                // let right = calc.pending[1].clone();
                let mut has_left = replacement.1.consumes_left;
                let mut has_right = replacement.1.consumes_right;
                for elem in &replacement.0 {
                    if let IntermediateOp::C(c) = elem {
                        assert!(!has_left || !c.consumes_left);
                        assert!(!has_right || !c.consumes_right);
                        has_left = has_left || c.consumes_left;
                        has_right = has_right || c.consumes_right;
                    }
                }
                assert!(has_left);
                assert!(has_right);

                let left = calc.pending[0].clone();
                let right = calc.pending[1].clone();

                for elem in &replacement.0 {
                    let elem = match elem {
                        IntermediateOp::Kern(_) => elem.clone(),
                        IntermediateOp::C(c) => IntermediateOp::C(c.merge(&left, &right)),
                    };
                    calc.finalized.push(elem);
                }
                // calc.finalized.extend_from_slice(&replacement.0);
                replacement.1.merge(&left, &right)
            }
        };
        match calc.pending.get(2) {
            None => {
                if let Some(blocking) = node_to_parents.remove(&calc.node) {
                    actionable.extend(blocking);
                }
                new_result.insert(
                    calc.node,
                    Replacement(
                        calc.finalized,
                        last_1,
                        /*
                        match last_1.o {
                            Some(s) => TerminalOp::Lig(last_1.c, s),
                            None => C::char(last_1.c),
                        },
                         */
                    ),
                );
            }
            Some(_) => {
                calc.pending = if calc.pending.len() <= 2 {
                    unreachable!("can't hit this");
                } else {
                    vec![calc.pending[1].clone(), calc.pending[2].clone()]
                };
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
    // Knuth iterates over all nodes in the following order: in lexicographical order
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
    (new_result, infinite_loop_errors)
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
        want_new: Vec<(char, char, Vec<IntermediateOp>, C)>,
    ) {
        let entry_points: HashMap<Char, u16> = entry_points
            .into_iter()
            .map(|(c, u)| (c.try_into().unwrap(), u))
            .collect();
        let want_new: HashMap<(Char, Char), Replacement> = want_new
            .into_iter()
            .map(|t| {
                (
                    (t.0.try_into().unwrap(), t.1.try_into().unwrap()),
                    Replacement(t.2, t.3),
                )
            })
            .collect();
        let program = lang::Program {
            instructions,
            ..Default::default()
        };
        let (compiled_program, infinite_loop_error_or) = compile(&program, &vec![], &entry_points);
        assert!(infinite_loop_error_or.is_empty(), "no infinite loop errors");

        let mut got_new: HashMap<(Char, Char), Replacement> = Default::default();
        for pair in compiled_program.all_pairs_having_ops() {
            let replacement = compiled_program
                .get_replacement_utf8(pair.0.into(), pair.1.into())
                .unwrap();
            got_new.insert(pair, replacement.clone());
        }

        assert_eq!(got_new, want_new);
    }

    macro_rules! success_tests {
        ( $( ($name: ident, $instructions: expr, $entry_points: expr, $want_new: expr, ), )+ ) => {
            $(
                #[test]
                fn $name() {
                    let instructions = $instructions;
                    let entry_points = $entry_points;
                    let want_new = $want_new;
                    run_success_test(instructions, entry_points, want_new);
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
            vec![(
                'A',
                'V',
                vec![
                    IntermediateOp::C(C::char(Char::A, true)),
                    IntermediateOp::Kern(FixWord::ONE)
                ],
                C::char(Char::V, false),
            )],
        ),
        (
            same_kern_for_multiple_left_characters,
            vec![new_kern(None, 'V', FixWord::ONE)],
            vec![('A', 0), ('B', 0)],
            vec![
                (
                    'A',
                    'V',
                    vec![
                        IntermediateOp::C(C::char(Char::A, true)),
                        IntermediateOp::Kern(FixWord::ONE)
                    ],
                    C::char(Char::V, false),
                ),
                (
                    'B',
                    'V',
                    vec![
                        IntermediateOp::C(C::char(Char::B, true)),
                        IntermediateOp::Kern(FixWord::ONE)
                    ],
                    C::char(Char::V, false),
                ),
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
                vec![
                    IntermediateOp::C(C::char(Char::A, true)),
                    IntermediateOp::Kern(FixWord::ONE * 2)
                ],
                C::char(Char::V, false),
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
                    vec![
                        IntermediateOp::C(C::char(Char::A, true)),
                        IntermediateOp::Kern(FixWord::ONE * 2)
                    ],
                    C::char(Char::V, false),
                ),
                (
                    'A',
                    'W',
                    vec![
                        IntermediateOp::C(C::char(Char::A, true)),
                        IntermediateOp::Kern(FixWord::ONE * 3)
                    ],
                    C::char(Char::W, false),
                ),
                (
                    'B',
                    'W',
                    vec![
                        IntermediateOp::C(C::char(Char::B, true)),
                        IntermediateOp::Kern(FixWord::ONE * 3)
                    ],
                    C::char(Char::W, false),
                ),
                (
                    'C',
                    'X',
                    vec![
                        IntermediateOp::C(C::char(Char::C, true)),
                        IntermediateOp::Kern(FixWord::ONE * 4)
                    ],
                    C::char(Char::X, false),
                ),
            ],
        ),
        (
            single_lig_1,
            vec![new_lig(None, 'B', 'Z', RetainNeitherMoveToInserted)],
            vec![('A', 0)],
            vec![
                (
                    'A',
                    'B',
                    vec![],
                    C {
                        c: Char::Z,
                        is_lig: true,
                        consumes_left: true,
                        consumes_right: true,
                    }
                ),
                //TerminalOp::Lig(Char::Z,),
            ],
        ),
        (
            single_lig_2,
            vec![
                new_lig(Some(0), 'B', 'Z', RetainLeftMoveToInserted),
                new_kern(None, 'Z', FixWord::ONE),
            ],
            vec![('A', 0)],
            vec![
                (
                    'A',
                    'B',
                    vec![IntermediateOp::C(C::char(Char::A, true)),],
                    C {
                        c: Char::Z,
                        is_lig: true,
                        consumes_left: false,
                        consumes_right: true,
                    }
                ),
                (
                    'A',
                    'Z',
                    vec![
                        IntermediateOp::C(C::char(Char::A, true)),
                        IntermediateOp::Kern(FixWord::ONE)
                    ],
                    C::char(Char::Z, false),
                ),
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
                (
                    'A',
                    'B',
                    vec![
                        IntermediateOp::C(C::char(Char::A, true)),
                        IntermediateOp::Kern(FixWord::ONE),
                    ],
                    C {
                        c: Char::Z,
                        is_lig: true,
                        consumes_left: false,
                        consumes_right: true,
                    }
                ),
                (
                    'A',
                    'Z',
                    vec![
                        IntermediateOp::C(C::char(Char::A, true)),
                        IntermediateOp::Kern(FixWord::ONE),
                    ],
                    C::char(Char::Z, false),
                ),
            ],
        ),
        (
            retain_left_move_nowhere_2,
            vec![new_lig(None, 'B', 'Z', RetainLeftMoveNowhere),],
            vec![('A', 0)],
            // vec![('A', 'B', vec![('A', FixWord::ZERO), ('Z', FixWord::ZERO)]),],
            vec![],
        ),
        (
            single_lig_4,
            vec![
                new_lig(None, 'B', 'Z', RetainRightMoveToInserted),
                new_kern(None, 'B', FixWord::ONE),
            ],
            vec![('A', 0), ('Z', 1)],
            vec![
                (
                    'A',
                    'B',
                    vec![
                        IntermediateOp::C(C {
                            c: Char::Z,
                            is_lig: true,
                            consumes_left: true,
                            consumes_right: false,
                        }),
                        IntermediateOp::Kern(FixWord::ONE),
                    ],
                    // C{},
                    C::char(Char::B, false),
                    //    vec![('Z', FixWord::ONE), ('B', FixWord::ZERO)]
                    //   left: {(Char(65), Char(66)): Replacement([LeftChar, Kern(FixWord(1048576))], C { c: Char(66), is_lig: false, consumes_left: false, consumes_right: false, o: None }),
                    //  right: {(Char(65), Char(66)): Replacement([C(C { c: Char(90), is_lig: true, consumes_left: true, consumes_right: false, o: Some("A") }), Kern(FixWord(1048576))], C { c: Char(66), is_lig: false, consumes_left: false, consumes_right: false, o: None }),
                    // (Char(90), Char(66)): Replacement([LeftChar, Kern(FixWord(1048576))], C { c: Char(66), is_lig: false, consumes_left: false, consumes_right: false, o: None })}
                    // (Char(90), Char(66)): Replacement([LeftChar, Kern(FixWord(1048576))], C { c: Char(66), is_lig: false, consumes_left: false, consumes_right: false, o: None })}
                ),
                (
                    'Z',
                    'B',
                    vec![
                        IntermediateOp::C(C::char(Char::Z, true)),
                        IntermediateOp::Kern(FixWord::ONE),
                    ],
                    C::char(Char::B, false),
                    // vec![('Z', FixWord::ONE), ('B', FixWord::ZERO)]
                ),
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
                (
                    'A',
                    'B',
                    vec![IntermediateOp::C(C {
                        c: Char::Z,
                        is_lig: true,
                        consumes_left: true,
                        consumes_right: false,
                    })],
                    C::char(Char::B, false),
                ),
                (
                    'Z',
                    'B',
                    vec![
                        IntermediateOp::C(C::char(Char::Z, true)),
                        IntermediateOp::Kern(FixWord::ONE),
                    ],
                    C::char(Char::B, false),
                ),
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
                        IntermediateOp::C(C::char(Char::A, true)),
                        IntermediateOp::Kern(FixWord::ONE * 2),
                        IntermediateOp::C(C {
                            c: Char::Z,
                            is_lig: true,
                            consumes_left: false,
                            consumes_right: false,
                        }),
                        IntermediateOp::Kern(FixWord::ONE * 3),
                    ],
                    C::char(Char::B, false),
                ),
                (
                    'A',
                    'Z',
                    vec![
                        IntermediateOp::C(C::char(Char::A, true)),
                        IntermediateOp::Kern(FixWord::ONE * 2),
                    ],
                    C::char(Char::Z, false),
                ),
                (
                    'Z',
                    'B',
                    vec![
                        IntermediateOp::C(C::char(Char::Z, true)),
                        IntermediateOp::Kern(FixWord::ONE * 3),
                    ],
                    C::char(Char::B, false),
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
                    IntermediateOp::C(C::char(Char::A, true)),
                    IntermediateOp::C(C {
                        c: Char::Z,
                        is_lig: true,
                        consumes_left: false,
                        consumes_right: false,
                    }),
                ],
                C::char(Char::B, false),
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
                        IntermediateOp::C(C::char(Char::A, true)),
                        IntermediateOp::C(C {
                            c: Char::Y,
                            is_lig: true,
                            consumes_left: false,
                            consumes_right: false,
                        }),
                    ],
                    C::char(Char::B, false),
                ),
                (
                    'Z',
                    'B',
                    vec![IntermediateOp::C(C {
                        c: Char::Y,
                        is_lig: true,
                        consumes_left: true,
                        consumes_right: false,
                    }),],
                    C::char(Char::B, false),
                ),
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
                        IntermediateOp::C(C::char(Char::A, true)),
                        IntermediateOp::C(C {
                            c: Char::Y,
                            is_lig: true,
                            consumes_left: false,
                            consumes_right: false,
                        }),
                        IntermediateOp::C(C {
                            c: Char::Z,
                            is_lig: true,
                            consumes_left: false,
                            consumes_right: false,
                        }),
                    ],
                    C::char(Char::B, false),
                ),
                (
                    'A',
                    'Z',
                    vec![
                        IntermediateOp::C(C::char(Char::A, true)),
                        IntermediateOp::C(C {
                            c: Char::Y,
                            is_lig: true,
                            consumes_left: false,
                            consumes_right: false,
                        }),
                    ],
                    C {
                        c: Char::Z,
                        is_lig: false,
                        consumes_left: false,
                        consumes_right: true,
                    }
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
                        IntermediateOp::C(C::char(Char::A, true)),
                        IntermediateOp::C(C {
                            c: Char::Z,
                            is_lig: true,
                            consumes_left: false,
                            consumes_right: false,
                        }),
                        IntermediateOp::Kern(FixWord::ONE * 3),
                    ],
                    C::char(Char::B, false),
                ),
                (
                    'A',
                    'Z',
                    vec![
                        IntermediateOp::C(C::char(Char::A, true)),
                        IntermediateOp::Kern(FixWord::ONE * 2),
                    ],
                    C::char(Char::Z, false),
                ),
                (
                    'Z',
                    'B',
                    vec![
                        IntermediateOp::C(C::char(Char::Z, true)),
                        IntermediateOp::Kern(FixWord::ONE * 3),
                    ],
                    C::char(Char::B, false),
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
                        IntermediateOp::C(C::char(Char::A, true)),
                        IntermediateOp::C(C {
                            c: Char::Y,
                            is_lig: true,
                            consumes_left: false,
                            consumes_right: false,
                        }),
                    ],
                    C::char(Char::B, false),
                ),
                (
                    'Z',
                    'B',
                    vec![IntermediateOp::C(C {
                        c: Char::Y,
                        is_lig: true,
                        consumes_left: true,
                        consumes_right: false,
                    }),],
                    C::char(Char::B, false),
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
                    IntermediateOp::C(C::char(Char::A, true)),
                    IntermediateOp::C(C {
                        c: Char::Z,
                        is_lig: true,
                        consumes_left: false,
                        consumes_right: false,
                    }),
                ],
                C::char(Char::B, false),
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
                        IntermediateOp::C(C::char(Char::A, true)),
                        IntermediateOp::C(C {
                            c: Char::Y,
                            is_lig: true,
                            consumes_left: false,
                            consumes_right: false,
                        }),
                    ],
                    C::char(Char::B, false),
                ),
                (
                    'Z',
                    'B',
                    vec![IntermediateOp::C(C {
                        c: Char::Y,
                        is_lig: true,
                        consumes_left: true,
                        consumes_right: false,
                    }),],
                    C::char(Char::B, false),
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
            /*
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
             */
            vec![],
        ),
    );
}
