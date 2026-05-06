//! # Knuth-Plass line breaking algorithm.

use std::{collections::VecDeque, ops::AddAssign};

use boxworks::ds::{self, KernKind};
use common::{GlueOrder, Scaled};
pub mod debug;

pub struct LineBreaker<'a> {
    pub params: &'a Params,
    pub line_widths: &'a [common::Scaled],
    pub debug_logger: Option<&'a mut dyn debug::Logger>,
}

#[derive(Debug)]
pub struct Params {
    pub adj_demerits: i32,
    pub double_hyphen_demerits: i32,
    pub ex_hyphen_penalty: i32,
    pub final_hyphen_demerits: i32,
    pub hyphen_penalty: i32,
    pub left_skip: common::Glue,
    pub line_penalty: i32,
    pub looseness: i32,
    pub par_fill_skip: common::Glue,
    pub pre_tolerance: i32,
    pub right_skip: common::Glue,
    pub tolerance: i32,
}

impl Default for Params {
    fn default() -> Self {
        Self::plain_tex_defaults()
    }
}

impl Params {
    pub fn plain_tex_defaults() -> Self {
        Self {
            adj_demerits: 10000,
            double_hyphen_demerits: 10000,
            ex_hyphen_penalty: 50,
            final_hyphen_demerits: 5000,
            hyphen_penalty: 50,
            left_skip: common::Glue::ZERO,
            line_penalty: 10,
            looseness: 0,
            par_fill_skip: common::Glue {
                width: common::Scaled::ZERO,
                stretch: common::Scaled::ONE,
                stretch_order: common::GlueOrder::Fil,
                shrink: common::Scaled::ZERO,
                shrink_order: Default::default(),
            },
            pre_tolerance: 100,
            right_skip: common::Glue::ZERO,
            tolerance: 200,
        }
    }
}

#[derive(Copy, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Debug)]
struct Scaled64(i64);

impl std::ops::Add for Scaled64 {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

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

impl std::ops::SubAssign<common::Scaled> for Scaled64 {
    fn sub_assign(&mut self, rhs: common::Scaled) {
        self.0 -= rhs.0 as i64
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

impl<'a> boxworks::LineBreaker for LineBreaker<'a> {
    fn break_line<F: boxworks::FontRepo>(
        mut self,
        font_repo: &F,
        _v_list: &mut Vec<ds::Vertical>,
        h_list: &mut Vec<ds::Horizontal>,
    ) {
        // TeX.2021.816
        if matches!(h_list.last(), Some(ds::Horizontal::Glue(_))) {
            h_list.pop();
        }
        h_list.push(ds::Horizontal::Penalty(ds::Penalty::INFINITE));
        h_list.push(ds::Horizontal::Glue(ds::Glue {
            kind: ds::GlueKind::Normal,
            value: self.params.par_fill_skip,
        }));

        if let Some(debug_logger) = self.debug_logger.as_deref_mut() {
            debug_logger.log_attempt(1);
        }
        if self.break_line_attempt(h_list, font_repo, self.params.pre_tolerance) {
            return;
        }
        if let Some(debug_logger) = self.debug_logger.as_deref_mut() {
            debug_logger.log_attempt(2);
        }
        boxworks_hyphenate::hyphenate_list(h_list);
        if self.break_line_attempt(h_list, font_repo, self.params.tolerance) {
            return;
        }
        println!("need emergency attempt");
    }
}

impl<'a> LineBreaker<'a> {
    pub fn break_line_attempt<F: boxworks::FontRepo>(
        &mut self,
        list: &[ds::Horizontal],
        font_repo: &F,
        tolerance: i32,
    ) -> bool {
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
                        diffs.width += font_repo
                            .width(*char, *font)
                            .unwrap_or(common::Scaled::ZERO);
                        continue;
                    }
                    HBox(ds::HBox { width, .. })
                    | VBox(ds::VBox { width, .. })
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
                            .map(|e| e.width(font_repo))
                            .sum();
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
                            println!("adding kern width {}", kern.width);
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
                // TODO: For discretionaty nodes we need to adjust the width here?
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
                    // todo: if len(self.line_widths) = 1 then we should return End
                    LineClass::Beginning => {
                        if self.line_widths.len() == 1 {
                            (LineClass::End, *self.line_widths.first().unwrap())
                        } else {
                            (LineClass::Numbered(0), *self.line_widths.first().unwrap())
                        }
                    }
                    LineClass::Numbered(i) => match self.line_widths.get(i + 1) {
                        Some(line_width) => {
                            // if index i+1 is the last element, meaning it has i+2 elements
                            if self.line_widths.len() == i + 2 {
                                (LineClass::End, *line_width)
                            } else {
                                (LineClass::Numbered(i + 1), *line_width)
                            }
                        }
                        None => (LineClass::End, *self.line_widths.last().unwrap()),
                    },
                    LineClass::End => (LineClass::End, *self.line_widths.last().unwrap()),
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
                    let ideal_line_width = Scaled64(line_width.0 as i64);
                    let actual_line_width_no_stretching =
                        diffs.width - active_node.diffs.width + Scaled64(disc_width.0 as i64);
                    let line_diffs = Diffs {
                        width: ideal_line_width - actual_line_width_no_stretching,
                        shrinkability: diffs.shrinkability - active_node.diffs.shrinkability,
                        stretchabilities: [
                            diffs.stretchabilities[0] - active_node.diffs.stretchabilities[0],
                            diffs.stretchabilities[1] - active_node.diffs.stretchabilities[1],
                            diffs.stretchabilities[2] - active_node.diffs.stretchabilities[2],
                            diffs.stretchabilities[3] - active_node.diffs.stretchabilities[3],
                        ],
                    };

                    // TeX.2021.851
                    let shortfall = line_diffs.width;
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

                    // The conidition of the if statement is in TeX.2021.851
                    let deactivate = if badness > INFINITE_BADNESS || penalty == EJECT_PENALTY {
                        // TeX.2021.854
                        // TODO: finish TeX.2021.854 when we implement the final pass.
                        true
                    } else {
                        false
                    };

                    // Allowable break.
                    // Add a candidate
                    if badness <= tolerance {
                        // TeX.2021.855
                        let demerits = self.demerits(
                            badness,
                            penalty,
                            active_node.fitness_class,
                            fitness_class,
                            active_node.hyphenated && hyphenated,
                            active_node.hyphenated && elem.is_none(),
                        );
                        let total_demerits = demerits + active_node.total_demerits;
                        // The logging here is implemented in TeX.2021.856
                        if let Some(debug_logger) = self.debug_logger.as_deref_mut() {
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
                    if !deactivate {
                        active_nodes.push_back(active_node);
                    }
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
                    if let Some(elem) = elem {
                        // TeX.2021.837
                        match elem {
                            Discretionary(discretionary) => {
                                // TeX.2021.840
                                // TODO: also need to factor in the post_break adjustment.
                                let mut j = i + 1;
                                while j < i + 1 + discretionary.replace_count as usize {
                                    diffs.width += match &list[j] {
                                        Char(ds::Char { char, font })
                                        | Ligature(ds::Ligature { char, font, .. }) => font_repo
                                            .width(*char, *font)
                                            .unwrap_or(common::Scaled::ZERO),
                                        HBox(ds::HBox { width, .. })
                                        | VBox(ds::VBox { width, .. })
                                        | Rule(ds::Rule { width, .. })
                                        | Kern(ds::Kern { width, .. }) => *width,
                                        _ => {
                                            eprintln!(
                                                "invalid node {:?} in discretionary replacement list",
                                                &list[j]
                                            );
                                            common::Scaled::ZERO
                                        }
                                    };
                                    j += 1;
                                }
                            }
                            Math(_math) => {
                                // TODO when math node is fixed in boxworks crate.
                            }
                            Glue(glue) => {
                                diffs.update_from_glue(&glue.value);
                            }
                            Kern(kern) => {
                                if kern.kind == ds::KernKind::Explicit {
                                    diffs.width -= kern.width;
                                }
                            }
                            _ => {}
                        }
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
                        if let Some(debug_logger) = self.debug_logger.as_deref_mut() {
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
                Char(_) | HBox(_) | VBox(_) | Rule(_) | Mark(_) | Insertion(_) | Adjust(_)
                | Ligature(_) => {
                    // Unreachable because we've already handled these nodes in
                    // the earlier switch and skipped the rest of the loop.
                }
                Discretionary(_) => {
                    // diffs.width += -disc_width;
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

        !active_nodes.is_empty()
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
        consecutive_hyphens: bool,
        end_after_hyphen: bool,
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
        if end_after_hyphen {
            d += self.params.final_hyphen_demerits;
        } else if consecutive_hyphens {
            d += self.params.double_hyphen_demerits;
        }
        if (previous_fitness_class as isize - this_fitness_class as isize).abs() > 1 {
            d += self.params.adj_demerits;
        }
        d
    }
}

// TeX.2021.833
const AWFUL_BAD: i32 = 0o7_777_777_777;
const INFINITE_BADNESS: i32 = 10000;
const INFINITE_PENALTY: i32 = 10000;
const EJECT_PENALTY: i32 = -10000;

/// TeX.2021.108
fn badness(shortfall: Scaled64, stretchability: Scaled64) -> i32 {
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
        ((r * r * r + 0o400_000) / 0o1_000_000)
            .try_into()
            .unwrap_or(INFINITE_BADNESS)
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
    use super::*;
    use boxworks::TextPreprocessor;
    use boxworks_text as bwt;
    use pretty_assertions::assert_eq;
    use std::{cell::RefCell, rc::Rc};

    const WOLF_HALL_5IN_TYPESET: &'static str = "
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
    const WOLF_HALL_5IN_LOG: &'static str = include_str!("../testdata/wolf_hall_5in_log.txt");
    const WOLF_HALL_3IN_LOG: &'static str = include_str!("../testdata/wolf_hall_3in_log.txt");
    const TFM_CMR10: &'static [u8] = include_bytes!("../../tfm/corpus/computer-modern/cmr10.tfm");

    #[test]
    fn wolf_hall_5in() {
        run_log_test(TFM_CMR10, WOLF_HALL_5IN_TYPESET, WOLF_HALL_5IN_LOG, "5in");
    }

    #[test]
    fn wolf_hall_3in() {
        run_log_test(TFM_CMR10, WOLF_HALL_5IN_TYPESET, WOLF_HALL_3IN_LOG, "3in");
    }

    fn run_log_test(tfm_bytes: &[u8], input: &str, want_log: &str, width: &str) {
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

        let mut font_repo: bwt::TfmFontRepo = Default::default();
        font_repo.register_font(0, tfm_file);
        let width = common::Scaled::parse_from_string(width).unwrap();
        let params = Params::plain_tex_defaults();

        let log: Rc<RefCell<String>> = Default::default();
        let mut logger = debug::TexLogger::new(log.clone());

        let line_breaker = super::LineBreaker {
            params: &params,
            line_widths: &[width],
            debug_logger: Some(&mut logger),
        };
        let mut v_list = vec![];
        use boxworks::LineBreaker;
        line_breaker.break_line(&font_repo, &mut v_list, &mut list);

        let log = log.take();
        assert_eq!(normalize(want_log), normalize(&log));
    }

    fn normalize(s: &str) -> String {
        let v: Vec<&str> = s
            .split('\n')
            .map(|l| l.trim())
            .map(|l| l.strip_prefix(r"\customFont ").unwrap_or(l))
            .filter(|l| !l.is_empty())
            .collect();
        v.join("\n")
    }
}
