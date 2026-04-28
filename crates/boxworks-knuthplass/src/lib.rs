//! # Knuth-Plass line breaking algorithm.

use std::{collections::VecDeque, ops::AddAssign};

use boxworks::ds::{self, KernKind};
use common::{GlueOrder, Scaled};
pub mod debug;

#[derive(Default, Debug)]
pub struct LineBreaker {
    params: Params,
}

#[derive(Default, Debug)]
pub struct Params {
    pub line_penalty: i32,
    pub adj_demerits: i32,
    pub tracing_paragraphs: i32,
    pub hyphen_penalty: i32,
    pub ex_hyphen_penalty: i32,
    pub par_fill_skip: common::Glue,
    pub pre_tolerance: i32,
    pub tolerance: i32,
    pub emergency_stretch: i32,
    pub left_skip: common::Glue,
    pub right_skip: common::Glue,
}

#[derive(Copy, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Debug)]
struct Scaled64(i64);

impl std::ops::Sub for Scaled64 {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 - rhs.0)
    }
}

impl std::ops::Neg for Scaled64 {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self(-self.0)
    }
}

impl AddAssign<common::Scaled> for Scaled64 {
    fn add_assign(&mut self, rhs: common::Scaled) {
        self.0 += rhs.0 as i64
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct Diffs {
    width: Scaled64,
    shrinkability: Scaled64,
    stretchabilities: [Scaled64; 4],
}

impl Diffs {
    fn update_from_glue(&mut self, glue: &common::Glue) {
        self.width += glue.width;
        // TODO: emit warning if the shrink order is not normal like in TeX.2021.825
        self.shrinkability += glue.shrink;
        self.stretchabilities[glue.stretch_order as usize] += glue.stretch;
    }
    fn infinitely_stretchable(&self) -> bool {
        self.stretchabilities[GlueOrder::Fil as usize].0 != 0
            || self.stretchabilities[GlueOrder::Fill as usize].0 != 0
            || self.stretchabilities[GlueOrder::Filll as usize].0 != 0
    }
    fn finite_stretchability(&self) -> Scaled64 {
        self.stretchabilities[GlueOrder::Normal as usize]
    }
}

// TeX.2021.819
#[derive(Clone, Debug, PartialEq, Eq)]
struct ActiveNode {
    // Deltas between this active node and the preceding element in
    // the list of active nodes.
    diffs: Diffs,
    fitness_class: FitnessClass,
    hyphenated: bool,
    line_number: usize,
    line_class: LineClass,
    // Index of the passive node corresponding to this active node.
    node_index: usize,
    total_demerits: i32,
}

/// TeX.2021.817
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FitnessClass {
    VeryLoose = 0,
    Loose = 1,
    Decent = 2,
    Tight = 3,
}

struct PassiveNode {
    // Index of the element in the horizontal list
    _elem: usize,
    // Index of the passive node corresponding to the previous break
    // in the optimal path to this node.
    _previous_node_index: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum LineClass {
    Beginning,
    Numbered(usize),
    End,
}

impl LineBreaker {
    pub fn break_line<F>(
        &mut self,
        list: &mut Vec<ds::Horizontal>,
        font_width: &F,
        line_widths: &[common::Scaled],
        mut debug_logger: Option<&mut dyn debug::Logger>,
    ) where
        F: Fn(char, u32) -> Scaled,
    {
        self.params.par_fill_skip = common::Glue {
            width: common::Scaled::ZERO,
            stretch: common::Scaled::ONE,
            stretch_order: common::GlueOrder::Fil,
            shrink: common::Scaled::ZERO,
            shrink_order: Default::default(),
        };

        // TeX.2021.816
        if matches!(list.last(), Some(ds::Horizontal::Glue(_))) {
            list.pop();
        }
        list.push(ds::Horizontal::Penalty(ds::Penalty::INFINITE));
        list.push(ds::Horizontal::Glue(ds::Glue {
            kind: ds::GlueKind::Normal,
            value: self.params.par_fill_skip,
        }));

        if let Some(debug_logger) = debug_logger.as_deref_mut() {
            debug_logger.log_attempt(1);
        }
        self.break_line_attempt(list, font_width, line_widths, debug_logger);
    }

    pub fn break_line_attempt<F>(
        &mut self,
        list: &[ds::Horizontal],
        font_width: &F,
        line_widths: &[common::Scaled],
        mut debug_logger: Option<&mut dyn debug::Logger>,
    ) where
        F: Fn(char, u32) -> Scaled,
    {
        // TODO: for different runs this is different
        let threshold = 100;
        self.params.line_penalty = 10;
        self.params.adj_demerits = 10_000;

        // self.params.pre_tolerance as i64;

        let force_solution = false;

        let mut auto_breaking = true;
        let mut passive_nodes = vec![PassiveNode {
            _elem: 0,
            _previous_node_index: 0,
        }];

        let mut active_nodes = VecDeque::<ActiveNode>::from([
            // TeX.2021.864
            ActiveNode {
                diffs: {
                    // TeX.2021.827
                    let mut d: Diffs = Default::default();
                    d.update_from_glue(&self.params.left_skip);
                    d.update_from_glue(&self.params.right_skip);
                    d
                },
                fitness_class: FitnessClass::Decent,
                hyphenated: false,
                line_number: 0,
                line_class: LineClass::Beginning,
                node_index: 0,
                total_demerits: 0,
            },
        ]);
        let mut diffs: Diffs = Default::default();
        // This is the loop in TeX.2021.863
        for i in 0..=list.len() {
            let elem = list.get(i);
            use ds::Horizontal::*;
            let mut disc_width = Scaled::ZERO;
            // This switch is TeX.2021.866. In TeX, Knuth invokes `try_break` inline
            // at the relevant parts of the switch. We instead return the two arguments
            // to `try_break` from the match, and the rest of this function implements
            // the `try_break`. Switch cases for which `try_break` should not be invoked
            // contain a continue statement.
            let (mut penalty, hyphenated) = match elem {
                None => {
                    // TeX.202.873
                    // I'm guessing the last break is considered hyphenated so as to penalize
                    // the second-to-last line being hyphenated. A hyphen doesn't look nice
                    // in the bottom right of the paragraph.
                    (EJECT_PENALTY, true)
                }
                Some(elem) => match elem {
                    Char(ds::Char { char, font }) | Ligature(ds::Ligature { char, font, .. }) => {
                        // TeX.2021.867 has an optimization in which subsequent chars are read
                        // here. I'm not convinced it's worth it.
                        diffs.width += font_width(*char, *font);
                        continue;
                    }
                    HList(ds::HList { width, .. })
                    | VList(ds::VList { width, .. })
                    | Rule(ds::Rule { width, .. }) => {
                        diffs.width += *width;
                        continue;
                    }
                    Mark(_) | Insertion(_) | Adjust(_) => {
                        // do nothing
                        continue;
                    }
                    Discretionary(discretionary) => {
                        // TeX.2021.869
                        disc_width = discretionary
                            .pre_break
                            .iter()
                            .map(|e| e.width(font_width))
                            .sum();
                        // If the break occurs here, the pre_break items will be added
                        // before the break. Thus the actual width of the line is the current
                        // width, plus the width of the pre_break items. This modification
                        // to deltas.width is undone later in the function.
                        diffs.width += disc_width;
                        (
                            if discretionary.pre_break.is_empty() {
                                self.params.ex_hyphen_penalty
                            } else {
                                self.params.hyphen_penalty
                            },
                            true,
                        )
                        // Knuth includes the following optimization, which we omit.
                        // The discretionary node specifies that the following r
                        // elements of the horizontal list should be removed if
                        // a break occurs here. These elements must be one of the 6
                        // types allowed in discretionary lists. None of these elements
                        // can themselves be breakpoints. Thus, Knuth skips ahead
                        // by r elements in the horizontal list just updating the widths.
                        // It's unclear if this optimization is worth implementing...
                    }
                    Whatsit(_whatsit) => todo!(),
                    Math(math) => {
                        auto_breaking = *math == ds::Math::After;
                        if auto_breaking && matches!(list.get(i + 1), Some(Glue(_))) {
                            // List of allowable line breaks in TeXBook chapter 14 p96:
                            // (c) at a math-off that is immediately followed by glue.
                            (0, false)
                        } else {
                            continue;
                        }
                    }
                    Glue(glue) => {
                        // TeX.2021.868
                        if auto_breaking && i > 0 && list[i - 1].precedes_break() {
                            // List of allowable line breaks in TeXBook chapter 14 p96:
                            // (a) at glue, provided that this glue is immediately preceded by
                            // a non-discardable item, and that it is not part of a math formula
                            // (i.e., not between math-on and math-off). A break "at glue" occurs
                            // at the left edge of the glue space.
                            (0, false)
                        } else {
                            diffs.update_from_glue(&glue.value);
                            continue;
                        }
                    }
                    Kern(kern) => {
                        if kern.kind == KernKind::Explicit
                            && auto_breaking
                            && matches!(list.get(i + 1), Some(Glue(_)))
                        {
                            // List of allowable line breaks in TeXBook chapter 14 p96:
                            // (b) at a kern, provided that this kern is immediately followed by glue,
                            // and that it is not part of a math formula.
                            (0, false)
                        } else {
                            diffs.width += kern.width;
                            continue;
                        }
                    }
                    Penalty(penalty) => {
                        // List of allowable line breaks in TeXBook chapter 14 p96:
                        // (d) at a penalty (which might have been inserted automatically in a formula).
                        (penalty.0, false)
                    }
                },
            };

            // TeX.2021.831
            if penalty >= INFINITE_PENALTY {
                continue;
            }
            if penalty <= EJECT_PENALTY {
                penalty = EJECT_PENALTY
            }

            let mut n = active_nodes.len();
            while n > 0 {
                let (line_class, mut m) = get_next_line_class(&active_nodes, n);
                n -= m;

                // TODO: destroy this. Instead just use the line number to calculate the class.

                // The line width is calculated in TeX.2021.850. However in this implementation
                // of Knuth-Plass, the line width calculator is passed as a parameter.
                let (next_line_class, line_width) = match line_class {
                    // todo: if len(line_widths) = 1 then we should return End
                    LineClass::Beginning => {
                        if line_widths.len() == 1 {
                            (LineClass::End, *line_widths.first().unwrap())
                        } else {
                            (LineClass::Numbered(0), *line_widths.first().unwrap())
                        }
                    }
                    LineClass::Numbered(i) => match line_widths.get(i + 1) {
                        Some(line_width) => {
                            // if index i+1 is the last element, meaning it has i+2 elements
                            if line_widths.len() == i + 2 {
                                (LineClass::End, *line_width)
                            } else {
                                (LineClass::Numbered(i + 1), *line_width)
                            }
                        }
                        None => (LineClass::End, *line_widths.last().unwrap()),
                    },
                    LineClass::End => (LineClass::End, *line_widths.last().unwrap()),
                };

                // TeX.2021.833
                #[derive(Clone, Copy, Debug)]
                struct Candidate {
                    total_demerits: i32,
                    previous_node_index: usize,
                    line_number: usize,
                }
                // TeX.2021.834
                let mut candidates = [Candidate {
                    total_demerits: i32::MAX,
                    previous_node_index: 0,
                    line_number: 0,
                }; 4];
                let mut minimum_demerits = i32::MAX;

                while m > 0 {
                    m -= 1;
                    let active_node = active_nodes.pop_front().expect("active nodes to consider");
                    let line_diffs = Diffs {
                        width: diffs.width - active_node.diffs.width,
                        shrinkability: diffs.shrinkability - active_node.diffs.shrinkability,
                        stretchabilities: [
                            diffs.stretchabilities[0] - active_node.diffs.stretchabilities[0],
                            diffs.stretchabilities[1] - active_node.diffs.stretchabilities[1],
                            diffs.stretchabilities[2] - active_node.diffs.stretchabilities[2],
                            diffs.stretchabilities[3] - active_node.diffs.stretchabilities[3],
                        ],
                    };

                    // TeX.2021.851
                    let shortfall = Scaled64(line_width.0 as i64) - line_diffs.width;
                    let (badness, fitness_class) = if shortfall.0 > 0 {
                        // Stretching the line
                        // TeX.2021.852
                        if line_diffs.infinitely_stretchable() {
                            (0, FitnessClass::Decent)
                        } else {
                            let b = badness(shortfall, line_diffs.finite_stretchability());
                            (
                                b,
                                if b <= 12 {
                                    FitnessClass::Decent
                                } else if b <= 99 {
                                    FitnessClass::Loose
                                } else {
                                    FitnessClass::VeryLoose
                                },
                            )
                        }
                    } else {
                        // Shrinking the line
                        // TeX.2021.853
                        if -shortfall > line_diffs.shrinkability {
                            (INFINITE_BADNESS + 1, FitnessClass::Decent)
                        } else {
                            let b = badness(-shortfall, line_diffs.shrinkability);
                            (
                                b,
                                if b <= 12 {
                                    FitnessClass::Decent
                                } else {
                                    FitnessClass::Tight
                                },
                            )
                        }
                    };

                    let emergency_break = if badness == INFINITE_BADNESS + 1 || penalty >= 10000 {
                        // Forced break
                        // TeX.2021.854
                        // TODO: I think the new_active_nodes check is wrong, it might also need
                        // to incorporate non-deactivated nodes.
                        // TODO: I think this is all wrong.
                        let last_active_node = i == 0 && active_nodes.is_empty();
                        force_solution && minimum_demerits == i32::MAX && last_active_node
                    } else {
                        false
                    };

                    // Allowable break.
                    // Add a candidate
                    if badness <= threshold || emergency_break {
                        // TeX.2021.855
                        let demerits = self.demerits(
                            badness.try_into().unwrap_or(i32::MAX),
                            penalty,
                            active_node.fitness_class,
                            fitness_class,
                        );
                        let total_demerits = demerits + active_node.total_demerits;
                        // The logging here is implemented in TeX.2021.856
                        if let Some(debug_logger) = debug_logger.as_deref_mut() {
                            debug_logger.log_feasible_breakpoint(
                                list,
                                debug::FeasibleBreakpoint {
                                    elem_index: i,
                                    badness,
                                    penalty,
                                    demerits,
                                    previous_node_index: active_node.node_index,
                                },
                            );
                        }
                        let candidate = &mut candidates[fitness_class as usize];
                        if total_demerits <= candidate.total_demerits {
                            // TeX.2021.856
                            *candidate = Candidate {
                                total_demerits,
                                previous_node_index: active_node.node_index,
                                line_number: active_node.line_number + 1,
                            };
                        }
                        if total_demerits <= minimum_demerits {
                            minimum_demerits = total_demerits;
                        }
                    }
                    // TODO: Potentially deactivate the node
                    active_nodes.push_back(active_node);
                }

                // TeX.2021.835
                // TODO: if this is the last active node we must also calculated candidates
                if minimum_demerits < AWFUL_BAD {
                    // TeX.2021.836
                    if self.params.adj_demerits.abs() >= AWFUL_BAD - minimum_demerits {
                        minimum_demerits = AWFUL_BAD - 1;
                    } else {
                        minimum_demerits += self.params.adj_demerits.abs();
                    }
                    let mut diffs = diffs.clone();
                    // TeX.2021.837
                    if let Some(Glue(glue)) = elem {
                        diffs.update_from_glue(&glue.value);
                        // TODO finish this section
                    }
                    for fitness_class in [
                        FitnessClass::VeryLoose,
                        FitnessClass::Loose,
                        FitnessClass::Decent,
                        FitnessClass::Tight,
                    ] {
                        let candidate = candidates[fitness_class as usize];
                        if candidate.total_demerits > minimum_demerits {
                            continue;
                        }
                        // TeX.2021.845
                        let active_node = ActiveNode {
                            diffs: diffs.clone(),
                            fitness_class,
                            hyphenated,
                            line_number: candidate.line_number,
                            line_class: next_line_class,
                            node_index: passive_nodes.len(),
                            total_demerits: candidate.total_demerits,
                        };

                        // Logging here is TeX.2021.846
                        if let Some(debug_logger) = debug_logger.as_deref_mut() {
                            debug_logger.log_new_active_node(debug::NewActiveNode {
                                node_index: active_node.node_index,
                                line_number: active_node.line_number,
                                fitness_class: active_node.fitness_class as u8,
                                hyphenated: active_node.hyphenated,
                                total_demerits: active_node.total_demerits,
                                previous_node_index: candidate.previous_node_index,
                            });
                        }
                        active_nodes.push_back(active_node);
                        passive_nodes.push(PassiveNode {
                            _elem: i,
                            _previous_node_index: candidate.previous_node_index,
                        });
                    }
                }
            }

            // At the end we update the widths.
            let Some(elem) = elem else { break };
            match elem {
                Char(_) | HList(_) | VList(_) | Rule(_) | Mark(_) | Insertion(_) | Adjust(_)
                | Ligature(_) => {
                    // Unreachable because we've already handled these nodes in
                    // the earlier switch and skipped the rest of the loop.
                }
                Discretionary(_) => {
                    diffs.width += -disc_width;
                }
                Whatsit(_whatsit) => todo!(),
                Math(_) => {
                    // Math nodes have no width.
                }
                Glue(glue) => {
                    diffs.update_from_glue(&glue.value);
                }
                Kern(kern) => {
                    diffs.width += kern.width;
                }
                Penalty(_) => {
                    // Penalty nodes have no width.
                }
            }
        }

        // TODO:
        // Chose active nodes where i = list.len()-1 !
        // And then return the list of optimal breaks
    }

    /// TeX.2021.859
    fn demerits(
        &self,
        badness: i32,
        penalty: i32,
        previous_fitness_class: FitnessClass,
        this_fitness_class: FitnessClass,
    ) -> i32 {
        let mut d = self.params.line_penalty + badness;
        if d.abs() >= 10_000 {
            d = 10_000;
        }
        d = d * d;
        if penalty > 0 {
            d += penalty * penalty;
        } else if penalty > EJECT_PENALTY {
            d -= penalty * penalty;
        }
        // TODO: hyphenation adjustment.
        // \doublehyphendemerits and \finalhyphendemerits.
        if (previous_fitness_class as isize - this_fitness_class as isize).abs() > 1 {
            d += self.params.adj_demerits;
        }
        d
    }
}

// TeX.2021.833
const AWFUL_BAD: i32 = 0o7_777_777_777;
const INFINITE_BADNESS: i64 = 10000;
const INFINITE_PENALTY: i32 = 10000;
const EJECT_PENALTY: i32 = -10000;

/// TeX.2021.108
fn badness(shortfall: Scaled64, stretchability: Scaled64) -> i64 {
    let t = shortfall.0;
    let s = stretchability.0;
    if t == 0 {
        return 0;
    }
    if s <= 0 {
        return INFINITE_BADNESS;
    }
    let r = if t <= 7_230_584 {
        (t * 297) / s
    } else if s >= 1_663_497 {
        t / (s / 297)
    } else {
        t
    };
    if r > 1290 {
        INFINITE_BADNESS
    } else {
        (r * r * r + 0o400_000) / 0o1_000_000
    }
}

fn get_next_line_class(active_nodes: &VecDeque<ActiveNode>, max: usize) -> (LineClass, usize) {
    let line_class = active_nodes
        .front()
        .expect("still active nodes to be considered")
        .line_class;
    let mut m = 0;
    for (i, node) in active_nodes.iter().enumerate() {
        if i >= max {
            break;
        }
        if node.line_class != line_class {
            break;
        }
        m += 1;
    }
    (line_class, m)
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use std::{cell::RefCell, rc::Rc};

    use super::*;
    use boxworks::TextPreprocessor;
    use boxworks_text as bwt;
    #[test]
    fn wolf_hall_5in() {
        const A: &'static str = "
            He thinks, if you were born in Putney, you saw the river every day, and imagined it 
            widening out to the sea. Even if you had never seen the ocean you had a picture of 
            it in your head from what you had been told by foreign people who sometimes came 
            upriver. You knew that one day you would go out into a world of marble pavements 
            and peacocks, of hillsides buzzing with heat, the fragrance of crushed herbs rising 
            around you as you walked. You planned for what your journeys would bring you: 
            the touch of warm terracotta, the night sky of another climate, alien flowers, the 
            stone eyed gaze of other peoples saints. But if you were born in Aslockton, in flat 
            fields under a wide sky, you might just be able to imagine Cambridge: no farther.
        ";
        const LOG: &'static str = r"
            @firstpass
            He thinks, if you were born in Putney, you saw the river every day, and imagined 
            @ via @@0 b=1 p=0 d=121
            @@1: line 1.2 t=121 -> @@0
            it 
            @ via @@0 b=3 p=0 d=169
            @@2: line 1.2 t=169 -> @@0
            widening out to the sea. Even if you had never seen the ocean you had a picture
            
            @ via @@1 b=0 p=0 d=100
            @ via @@2 b=2 p=0 d=144
            @@3: line 2.2 t=221 -> @@1
            of 
            @ via @@1 b=35 p=0 d=2025
            @ via @@2 b=1 p=0 d=121
            @@4: line 2.2 t=290 -> @@2
            @@5: line 2.3 t=2146 -> @@1
            it 
            @ via @@2 b=35 p=0 d=2025
            @@6: line 2.3 t=2194 -> @@2
            in your head from what you had been told by foreign people who sometimes 
            @ via @@3 b=1 p=0 d=121
            @ via @@4 b=40 p=0 d=2500
            @ via @@5 b=40 p=0 d=12500
            @@7: line 3.1 t=2790 -> @@4
            @@8: line 3.2 t=342 -> @@3
            came 
            @ via @@4 b=12 p=0 d=484
            @ via @@5 b=12 p=0 d=484
            @ via @@6 b=0 p=0 d=100
            @@9: line 3.2 t=774 -> @@4
            upriver. You knew that one day you would go out into a world of marble 
            @ via @@7 b=16 p=0 d=676
            @ via @@8 b=16 p=0 d=676
            @@10: line 4.1 t=1018 -> @@8
            pavements 
            @ via @@9 b=12 p=0 d=484
            @@11: line 4.2 t=1258 -> @@9
            and peacocks, of hillsides buzzing with heat, the fragrance of crushed 
            @ via @@10 b=10 p=0 d=400
            @@12: line 5.2 t=1418 -> @@10
            herbs rising 
            @ via @@11 b=1 p=0 d=121
            @@13: line 5.2 t=1379 -> @@11
            around you as you walked. You planned for what your journeys would 
            @ via @@12 b=0 p=0 d=100
            @@14: line 6.2 t=1518 -> @@12
            bring you: 
            @ via @@13 b=1 p=0 d=121
            @@15: line 6.2 t=1500 -> @@13
            the 
            @ via @@13 b=77 p=0 d=7569
            @@16: line 6.3 t=8948 -> @@13
            touch of warm terracotta, the night sky of another climate, alien 
            @ via @@14 b=12 p=0 d=484
            @@17: line 7.2 t=2002 -> @@14
            flowers, the 
            @ via @@15 b=4 p=0 d=196
            @@18: line 7.2 t=1696 -> @@15
            stone 
            @ via @@16 b=0 p=0 d=100
            @@19: line 7.2 t=9048 -> @@16
            eyed gaze of other peoples saints. But if you were born in Aslockton, in 
            @ via @@18 b=47 p=0 d=3249
            @@20: line 8.1 t=4945 -> @@18
            flat 
            @ via @@18 b=0 p=0 d=100
            @@21: line 8.2 t=1796 -> @@18
            fields 
            @ via @@19 b=0 p=0 d=100
            @@22: line 8.2 t=9148 -> @@19
            under a wide sky, you might just be able to imagine Cambridge: no 
            @ via @@20 b=60 p=0 d=4900
            @@23: line 9.1 t=9845 -> @@20
            farther. 
            @\par via @@20 b=75 p=-10000 d=17225
            @\par via @@21 b=0 p=-10000 d=100
            @\par via @@22 b=0 p=-10000 d=100
            @\par via @@23 b=0 p=-10000 d=100
            @@24: line 9.2- t=1896 -> @@21
        ";
        run_test(TFM_CMR10, A, LOG);
    }

    const TFM_CMR10: &'static [u8] = include_bytes!("../../tfm/corpus/computer-modern/cmr10.tfm");

    fn run_test(tfm_bytes: &[u8], input: &str, want_log: &str) {
        let mut tfm_file = tfm::File::deserialize(tfm_bytes).0.unwrap();
        let lig_kern_program =
            tfm::ligkern::CompiledProgram::compile_from_tfm_file(&mut tfm_file).0;
        let mut tp: bwt::TextPreprocessorImpl = Default::default();
        tp.register_font(0, &tfm_file, lig_kern_program);
        tp.activate_font(0);
        let mut list = vec![];
        for word in input.split_ascii_whitespace() {
            tp.add_word(word.trim_matches(' '), &mut list);
            tp.add_space(&mut list);
        }

        let font_width = |c: char, _: u32| {
            tfm_file
                .width_utf8(c)
                .expect(&format!("char {c} not in font"))
        };
        let width = common::Scaled::parse_from_string("5in").unwrap();
        let mut line_breaker: LineBreaker = Default::default();

        let log: Rc<RefCell<String>> = Default::default();
        let mut logger = debug::TexLogger::new(log.clone());

        line_breaker.break_line(&mut list, &font_width, &[width], Some(&mut logger));

        let log = log.take();
        assert_eq!(normalize(want_log), normalize(&log));
    }

    fn normalize(s: &str) -> String {
        let v: Vec<&str> = s
            .split('\n')
            .map(|l| l.trim())
            .filter(|l| !l.is_empty())
            .collect();
        v.join("\n")
    }
}
