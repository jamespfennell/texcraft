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

    finalized_new: Vec<IntermediateOp>,
    // Characters that are still pending replacement. The next step is to apply the ligature
    // rule for the node. After that, if the second element is not empty, the next step
    // is to apply the ligature rule for (tuple.0.1, tuple.1).
    // pending_new: (LigOrChar, LigOrChar, Option<LigOrChar>),
    // Invariant: either 2 or 3 elements in size?
    new_pending: Vec<C>,
}

#[derive(Clone, Debug)]
struct C {
    c: Char,
    o: Option<Rc<str>>,
}

impl OngoingCalculation {
    fn child(&self) -> Node {
        Node(self.new_pending[0].c.into(), self.new_pending[1].c.into())
    }
}

// TODO: destroy this or C
#[derive(Debug, Clone)]
enum LigOrChar {
    Lig(Char, Rc<str>),
    Char(Char),
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
pub struct Replacement(pub Vec<IntermediateOp>, pub TerminalOp);

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
        let (finalized_new, new_pending): (Vec<IntermediateOp>, Vec<C>) = match operation {
            lang::Operation::Kern(kern) => {
                new_result.insert(
                    pair,
                    Replacement(
                        vec![IntermediateOp::LeftChar, IntermediateOp::Kern(kern)],
                        TerminalOp::Char(right),
                    ),
                );
                continue;
            }
            lang::Operation::KernAtIndex(index) => {
                let kern = kerns.get(index as usize).copied().unwrap_or_default();
                new_result.insert(
                    pair,
                    Replacement(
                        vec![IntermediateOp::LeftChar, IntermediateOp::Kern(kern)],
                        TerminalOp::Char(right),
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
                    vec![],
                    vec![
                        C {
                            c: left.try_into().unwrap(),
                            o: None,
                        },
                        C {
                            c: char_to_insert,
                            o: Some("".into()),
                        },
                        C { c: right, o: None },
                    ],
                ),
                lang::PostLigOperation::RetainBothMoveToInserted => (
                    vec![IntermediateOp::LeftChar],
                    vec![
                        C {
                            c: char_to_insert,
                            o: Some("".into()),
                        },
                        C { c: right, o: None },
                    ],
                ),
                lang::PostLigOperation::RetainBothMoveToRight => (
                    vec![
                        IntermediateOp::LeftChar,
                        IntermediateOp::Lig(char_to_insert, "".into()),
                    ],
                    vec![],
                ),
                lang::PostLigOperation::RetainRightMoveToInserted => (
                    vec![],
                    vec![
                        C {
                            c: char_to_insert,
                            o: Some(format!("{left}").into()),
                        },
                        C { c: right, o: None },
                    ],
                ),
                lang::PostLigOperation::RetainRightMoveToRight => {
                    new_result.insert(
                        pair,
                        Replacement(
                            vec![IntermediateOp::Lig(
                                char_to_insert,
                                format!("{left}").into(),
                            )],
                            TerminalOp::Char(right),
                        ),
                    );
                    (
                        vec![IntermediateOp::Lig(
                            char_to_insert,
                            format!("{left}").into(),
                        )],
                        vec![],
                    )
                }
                lang::PostLigOperation::RetainLeftMoveNowhere => (
                    vec![],
                    vec![
                        C {
                            c: left.try_into().unwrap(),
                            o: None,
                        },
                        C {
                            c: char_to_insert,
                            o: Some(format!("{right}").into()),
                        },
                    ],
                ),
                lang::PostLigOperation::RetainLeftMoveToInserted => {
                    new_result.insert(
                        pair,
                        Replacement(
                            vec![IntermediateOp::LeftChar],
                            TerminalOp::Lig(char_to_insert, format!("{right}").into()),
                        ),
                    );
                    (vec![IntermediateOp::LeftChar], vec![])
                }
                lang::PostLigOperation::RetainNeitherMoveToInserted => {
                    new_result.insert(
                        pair,
                        Replacement(
                            vec![],
                            TerminalOp::Lig(char_to_insert, format!("{left}{right}").into()),
                        ),
                    );
                    (vec![], vec![])
                }
            },
        };
        if !new_pending.is_empty() {
            actionable.push(OngoingCalculation {
                node: pair,
                finalized_new,
                new_pending,
            });
            node_to_parents.insert(pair, vec![]);
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
                let left = calc.new_pending[0].clone();
                let right = calc.new_pending[1].clone();
                calc.finalized_new.push(match left.o {
                    Some(o) => IntermediateOp::Lig(left.c, o),
                    None => IntermediateOp::Char(left.c),
                });
                match right.o {
                    Some(o) => LigOrChar::Lig(right.c, o),
                    None => LigOrChar::Char(right.c),
                }
            }
            Some(replacement) => {
                println!("FINALIZING FOR node={:?}", calc.node);
                println!("FINALIZING FOR pending={:?}", calc.new_pending);
                // TODO: this is not good enough :( <- this is where the test is failing!
                // We may need to modify some ligature nodes
                // we need to iterate over the Replacement value and replace char or lig originals
                // with calc.new_pending[0] and calc.new_pending[1] - either the char
                // or the content of the ligature
                calc.finalized_new.extend_from_slice(&replacement.0);
                match &replacement.1 {
                    // TerminalOp::RightChar => LigOrChar::Char(child.1),
                    TerminalOp::Char(char) => LigOrChar::Char(*char),
                    TerminalOp::Lig(char, s) => LigOrChar::Lig(*char, s.clone()),
                }
            }
        };
        match calc.new_pending.get(2) {
            None => {
                if let Some(blocking) = node_to_parents.remove(&calc.node) {
                    actionable.extend(blocking);
                }
                new_result.insert(
                    calc.node,
                    Replacement(
                        calc.finalized_new,
                        match last_1 {
                            LigOrChar::Lig(char, s) => TerminalOp::Lig(char, s),
                            LigOrChar::Char(char) => TerminalOp::Char(char),
                        },
                    ),
                );
            }
            Some(_) => {
                calc.new_pending = if calc.new_pending.len() <= 2 {
                    unreachable!("can't hit this");
                } else {
                    vec![calc.new_pending[1].clone(), calc.new_pending[2].clone()]
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
        want_new: Vec<(char, char, Vec<IntermediateOp>, TerminalOp)>,
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
                vec![IntermediateOp::LeftChar, IntermediateOp::Kern(FixWord::ONE)],
                TerminalOp::Char(Char::V),
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
                    vec![IntermediateOp::LeftChar, IntermediateOp::Kern(FixWord::ONE)],
                    TerminalOp::Char(Char::V),
                ),
                (
                    'B',
                    'V',
                    vec![IntermediateOp::LeftChar, IntermediateOp::Kern(FixWord::ONE)],
                    TerminalOp::Char(Char::V),
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
                    IntermediateOp::LeftChar,
                    IntermediateOp::Kern(FixWord::ONE * 2)
                ],
                TerminalOp::Char(Char::V),
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
                        IntermediateOp::LeftChar,
                        IntermediateOp::Kern(FixWord::ONE * 2)
                    ],
                    TerminalOp::Char(Char::V),
                ),
                (
                    'A',
                    'W',
                    vec![
                        IntermediateOp::LeftChar,
                        IntermediateOp::Kern(FixWord::ONE * 3)
                    ],
                    TerminalOp::Char(Char::W),
                ),
                (
                    'B',
                    'W',
                    vec![
                        IntermediateOp::LeftChar,
                        IntermediateOp::Kern(FixWord::ONE * 3)
                    ],
                    TerminalOp::Char(Char::W),
                ),
                (
                    'C',
                    'X',
                    vec![
                        IntermediateOp::LeftChar,
                        IntermediateOp::Kern(FixWord::ONE * 4)
                    ],
                    TerminalOp::Char(Char::X),
                ),
            ],
        ),
        (
            single_lig_1,
            vec![new_lig(None, 'B', 'Z', RetainNeitherMoveToInserted)],
            vec![('A', 0)],
            vec![('A', 'B', vec![], TerminalOp::Lig(Char::Z, "AB".into())),],
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
                    vec![IntermediateOp::LeftChar,],
                    TerminalOp::Lig(Char::Z, "B".into()),
                ),
                (
                    'A',
                    'Z',
                    vec![IntermediateOp::LeftChar, IntermediateOp::Kern(FixWord::ONE)],
                    TerminalOp::Char(Char::Z),
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
                    vec![IntermediateOp::LeftChar, IntermediateOp::Kern(FixWord::ONE),],
                    TerminalOp::Lig(Char::Z, "B".into())
                ),
                (
                    'A',
                    'Z',
                    vec![IntermediateOp::LeftChar, IntermediateOp::Kern(FixWord::ONE),],
                    TerminalOp::Char(Char::Z),
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
            /*
            vec![
                ('A', 'B', vec![('Z', FixWord::ONE), ('B', FixWord::ZERO)]),
                ('Z', 'B', vec![('Z', FixWord::ONE), ('B', FixWord::ZERO)]),
            ],
             */
            vec![],
        ),
        (
            single_lig_5,
            vec![
                new_lig(None, 'B', 'Z', RetainRightMoveToRight),
                new_kern(None, 'B', FixWord::ONE),
            ],
            vec![('A', 0), ('Z', 1)],
            /*
            vec![
                ('A', 'B', vec![('Z', FixWord::ZERO), ('B', FixWord::ZERO)]),
                ('Z', 'B', vec![('Z', FixWord::ONE), ('B', FixWord::ZERO)]),
            ],
             */
            vec![],
        ),
        (
            retain_both_move_nowhere_1,
            vec![
                new_lig(Some(0), 'B', 'Z', RetainBothMoveNowhere,),
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
             */
            vec![],
        ),
        (
            retain_both_move_nowhere_2,
            vec![new_lig(None, 'B', 'Z', RetainBothMoveNowhere),],
            vec![('A', 0)],
            /*
            vec![(
                'A',
                'B',
                vec![
                    ('A', FixWord::ZERO),
                    ('Z', FixWord::ZERO),
                    ('B', FixWord::ZERO)
                ]
            ),],
             */
            vec![],
        ),
        (
            retain_both_move_nowhere_3,
            vec![
                new_lig(None, 'B', 'Z', RetainBothMoveNowhere),
                new_lig(None, 'B', 'Y', RetainRightMoveToRight),
            ],
            vec![('A', 0), ('Z', 1)],
            /*
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
             */
            vec![],
        ),
        (
            retain_both_move_nowhere_4,
            vec![
                new_lig(Some(0), 'B', 'Z', RetainBothMoveNowhere),
                new_lig(None, 'Z', 'Y', RetainBothMoveToRight),
            ],
            vec![('A', 0)],
            /*
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
             */
            vec![],
        ),
        (
            retain_both_move_to_inserted_1,
            vec![
                new_lig(Some(0), 'B', 'Z', RetainBothMoveToInserted,),
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
             */
            vec![],
        ),
        (
            retain_both_move_to_inserted_2,
            vec![
                new_lig(None, 'B', 'Z', RetainBothMoveToInserted),
                new_lig(None, 'B', 'Y', RetainRightMoveToInserted),
            ],
            vec![('A', 0), ('Z', 1)],
            /*
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
             */
            vec![],
        ),
        (
            retain_both_move_to_inserted_3,
            vec![new_lig(None, 'B', 'Z', RetainBothMoveToInserted),],
            vec![('A', 0)],
            /*
            vec![(
                'A',
                'B',
                vec![
                    ('A', FixWord::ZERO),
                    ('Z', FixWord::ZERO),
                    ('B', FixWord::ZERO)
                ]
            ),],
             */
            vec![],
        ),
        (
            retain_both_move_to_inserted_4,
            vec![
                new_lig(None, 'B', 'Z', RetainBothMoveToInserted,),
                new_lig(None, 'B', 'Y', RetainRightMoveToRight),
            ],
            vec![('A', 0), ('Z', 1)],
            /*
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
             */
            vec![],
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
