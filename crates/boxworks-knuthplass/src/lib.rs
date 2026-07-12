//! # Knuth-Plass line breaking algorithm.

use std::{collections::VecDeque, ops::AddAssign};

use boxworks::ds::{self, KernKind};
use common::{GlueOrder, Scaled};
pub mod debug;

pub struct LineBreaker<'a> {
    pub params: &'a Params,
    pub line_widths: &'a [Scaled],
    pub line_indents: &'a [Scaled],
    pub debug_logger: Option<&'a mut dyn debug::Logger>,
    pub hyphenator: &'a dyn boxworks::Hyphenator,
}

#[derive(Debug)]
pub struct Params {
    pub adj_demerits: i32,
    pub broken_penalty: i32,
    pub double_hyphen_demerits: i32,
    pub club_penalty: i32,
    pub ex_hyphen_penalty: i32,
    pub final_hyphen_demerits: i32,
    // TODO: this is actually different for each invocation. Make it so!
    pub final_widow_penalty: i32,
    pub hyphen_penalty: i32,
    pub inter_line_penalty: i32,
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
            broken_penalty: 100,
            double_hyphen_demerits: 10000,
            club_penalty: 150,
            ex_hyphen_penalty: 50,
            final_hyphen_demerits: 5000,
            final_widow_penalty: 150,
            hyphen_penalty: 50,
            inter_line_penalty: 0,
            left_skip: common::Glue::ZERO,
            line_penalty: 10,
            looseness: 0,
            par_fill_skip: common::Glue {
                width: Scaled::ZERO,
                stretch: Scaled::ONE,
                stretch_order: common::GlueOrder::Fil,
                shrink: Scaled::ZERO,
                shrink_order: Default::default(),
            },
            pre_tolerance: 100,
            right_skip: common::Glue::ZERO,
            tolerance: 200,
        }
    }

    /// Output the parameters in TeX format.
    pub fn tex(&self) -> String {
        let Params {
            adj_demerits,
            broken_penalty,
            double_hyphen_demerits,
            club_penalty,
            ex_hyphen_penalty,
            final_hyphen_demerits,
            final_widow_penalty: _,
            hyphen_penalty,
            inter_line_penalty,
            left_skip,
            line_penalty,
            looseness,
            par_fill_skip,
            pre_tolerance,
            right_skip,
            tolerance,
        } = self;
        format!(
            r"
            \adjdemerits={adj_demerits}
            \brokenpenalty={broken_penalty}
            \clubpenalty={club_penalty}
            \doublehyphendemerits={double_hyphen_demerits}
            \exhyphenpenalty={ex_hyphen_penalty}
            \finalhyphendemerits={final_hyphen_demerits}
            \hyphenpenalty={hyphen_penalty}
            \interlinepenalty={inter_line_penalty}
            \leftskip={left_skip}
            \linepenalty={line_penalty}
            \looseness={looseness}
            \parfillskip={par_fill_skip}
            \pretolerance={pre_tolerance}
            \rightskip={right_skip}
            \tolerance={tolerance}
        "
        )
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

impl AddAssign<Scaled> for Scaled64 {
    fn add_assign(&mut self, rhs: Scaled) {
        self.0 += rhs.0 as i64
    }
}

impl std::ops::SubAssign<Scaled> for Scaled64 {
    fn sub_assign(&mut self, rhs: Scaled) {
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

impl std::ops::Add for Diffs {
    type Output = Diffs;

    fn add(self, rhs: Self) -> Self::Output {
        Diffs {
            width: self.width + rhs.width,
            shrinkability: self.shrinkability + rhs.shrinkability,
            stretchabilities: [
                self.stretchabilities[0] + rhs.stretchabilities[0],
                self.stretchabilities[1] + rhs.stretchabilities[1],
                self.stretchabilities[2] + rhs.stretchabilities[2],
                self.stretchabilities[3] + rhs.stretchabilities[3],
            ],
        }
    }
}

impl std::ops::Sub for Diffs {
    type Output = Diffs;

    fn sub(self, rhs: Self) -> Self::Output {
        Diffs {
            width: self.width - rhs.width,
            shrinkability: self.shrinkability - rhs.shrinkability,
            stretchabilities: [
                self.stretchabilities[0] - rhs.stretchabilities[0],
                self.stretchabilities[1] - rhs.stretchabilities[1],
                self.stretchabilities[2] - rhs.stretchabilities[2],
                self.stretchabilities[3] - rhs.stretchabilities[3],
            ],
        }
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
    elem: usize,
    // Index of the passive node corresponding to the previous break
    // in the optimal path to this node.
    previous_node_index: usize,
}

impl<'a> boxworks::LineBreaker for LineBreaker<'a> {
    fn break_line<F: boxworks::FontRepo>(
        mut self,
        font_repo: &F,
        v_list: &mut Vec<ds::Vertical>,
        h_list: &mut Vec<ds::Horizontal>,
    ) {
        // This function is analagous to TeX.2021.815.

        // TeX.2021.816
        if matches!(h_list.last(), Some(ds::Horizontal::Glue(_))) {
            h_list.pop();
        }
        h_list.push(ds::Horizontal::Penalty(ds::Penalty::INFINITE));
        h_list.push(ds::Horizontal::Glue(ds::Glue {
            kind: ds::GlueKind::Normal,
            value: self.params.par_fill_skip,
        }));

        let break_points = self.break_line_all_attempts(font_repo, self.hyphenator, v_list, h_list);
        self.post_line_break(font_repo, v_list, h_list, &break_points);
    }
}

impl<'a> LineBreaker<'a> {
    fn post_line_break<F: boxworks::FontRepo>(
        &self,
        font_repo: &F,
        v_list: &mut Vec<ds::Vertical>,
        h_list: &[ds::Horizontal],
        break_points: &[usize],
    ) {
        let mut start_of_line = 0_usize;
        let mut disc_post_break_nodes: Option<Vec<ds::DiscretionaryElem>> = None;
        for (line_index, break_point) in break_points.iter().enumerate() {
            let mut inner_list: Vec<ds::Horizontal> = vec![];

            // TeX.2021.887
            if !self.params.left_skip.is_zero() {
                inner_list.push(
                    ds::Glue {
                        value: self.params.left_skip,
                        kind: ds::GlueKind::Normal,
                    }
                    .into(),
                );
            }

            // TeX.2021.884
            if let Some(disc_nodes) = disc_post_break_nodes.take() {
                for disc_node in disc_nodes {
                    inner_list.push(disc_node.into());
                }
            }

            inner_list.extend_from_slice(&h_list[start_of_line..*break_point]);
            start_of_line = *break_point + 1;

            // TeX.2021.881
            // This is the check that `q != null` in Knuth's TeX.
            // This logic does not run for the final breakpoint.
            if let Some(break_point_node) = h_list.get(*break_point).cloned() {
                use ds::Horizontal::*;
                match break_point_node {
                    Discretionary(discretionary) => {
                        // TeX.2021.882
                        // The empty discretionary survives in the list, funnily enough.
                        inner_list.push(Discretionary(Default::default()));
                        for pre_break_node in discretionary.pre_break {
                            inner_list.push(pre_break_node.into());
                        }
                        disc_post_break_nodes = Some(discretionary.post_break);
                        start_of_line += discretionary.replace_count as usize;
                    }
                    Math(math) => {
                        // TODO: set the width of Math to zero
                        inner_list.push(math.into());
                    }
                    Glue(_) => {
                        // Do nothing. In TeX there is an "optimization" in which the glue is
                        // modified to be \rightskip, but we don't do this. Instead it is inserted
                        // below.
                    }
                    Kern(mut kern) => {
                        kern.width = Scaled::ZERO;
                        inner_list.push(kern.into());
                    }
                    Penalty(penalty) => {
                        // Do nothing.
                        inner_list.push(penalty.into());
                    }
                    _ => {
                        unreachable!("node cannot appear as a breakpoint: {break_point_node:?}");
                    }
                }
            }

            // TeX.2021.886
            // Unlike \leftskip, there is no check if the glue here is zero.
            inner_list.push(
                ds::Glue {
                    value: self.params.right_skip,
                    kind: ds::GlueKind::Normal,
                }
                .into(),
            );

            // TeX.2021.889
            let width = self
                .line_widths
                .get(line_index)
                .unwrap_or(self.line_widths.last().expect("non-empty line widths"));
            let indent = self
                .line_indents
                .get(line_index)
                .copied()
                .unwrap_or(self.line_indents.last().copied().unwrap_or(Scaled::ZERO));
            let h_box = {
                let mut b = ds::HBox::pack(font_repo, inner_list, ds::PackWidth::Exact(*width));
                b.shift_amount = indent;
                b
            };

            // TeX.2021.888 and TeX.2021.679
            if !v_list.is_empty() {
                // TODO: make \baselineskip configurable
                // TODO: implement `\lineskiplimit` `\lineskip`.
                let mut baseline_skip = common::Glue {
                    width: common::Scaled::ONE * 12,
                    ..Default::default()
                };
                baseline_skip.width -= h_box.height;
                let mut j = v_list.len() - 1;
                // TODO: this is probably way too wrong. And shouldn't be managed
                // here: probably the v list should carry last_depth, like Knuth
                // does.
                let last_depth = loop {
                    let elem = &v_list[j];
                    use ds::Vertical::*;
                    match elem {
                        HBox(hbox) => break hbox.depth,
                        VBox(vbox) => break vbox.depth,
                        _ => {}
                    };
                    j = match j.checked_sub(1) {
                        None => break common::Scaled::ZERO,
                        Some(j) => j,
                    };
                };
                baseline_skip.width -= last_depth;
                v_list.push(
                    ds::Glue {
                        value: baseline_skip,
                        kind: Default::default(),
                    }
                    .into(),
                );
            }
            v_list.push(h_box.into());

            // TeX.2021.890
            // If this is not the last line, we consider penalties.
            if line_index + 1 != break_points.len() {
                let mut p = self.params.inter_line_penalty;
                if line_index == 0 {
                    p += self.params.club_penalty;
                }
                if line_index + 2 == break_points.len() {
                    p += self.params.final_widow_penalty;
                }
                if disc_post_break_nodes.is_some() {
                    p += self.params.broken_penalty;
                }
                if p != 0 {
                    v_list.push(ds::Penalty(p).into());
                }
            }
        }
    }
    pub fn break_line_all_attempts<F: boxworks::FontRepo>(
        &mut self,
        font_repo: &F,
        hyphenator: &dyn boxworks::Hyphenator,
        _v_list: &mut Vec<ds::Vertical>,
        h_list: &mut Vec<ds::Horizontal>,
    ) -> Vec<usize> {
        // We manually unroll the "loop" in TeX.2021.863.
        if let Some(debug_logger) = self.debug_logger.as_deref_mut() {
            debug_logger.log_attempt(1);
        }
        if let Some(v) =
            self.break_line_single_attempt(h_list, font_repo, self.params.pre_tolerance, false)
        {
            return v;
        }
        if let Some(debug_logger) = self.debug_logger.as_deref_mut() {
            debug_logger.log_attempt(2);
        }
        hyphenator.hyphenate(h_list);
        if let Some(v) =
            self.break_line_single_attempt(h_list, font_repo, self.params.tolerance, true)
        {
            return v;
        }
        // TODO: implement \emergencystretch and final pass
        panic!("need emergency attempt");
    }

    pub fn break_line_single_attempt<F: boxworks::FontRepo>(
        &mut self,
        list: &[ds::Horizontal],
        font_repo: &F,
        tolerance: i32,
        force_solution: bool,
    ) -> Option<Vec<usize>> {
        let mut auto_breaking = true;
        let mut passive_nodes = vec![PassiveNode {
            elem: 0,
            previous_node_index: 0,
        }];

        let background = {
            // TeX.2021.827
            let mut b: Diffs = Default::default();
            b.update_from_glue(&self.params.left_skip);
            b.update_from_glue(&self.params.right_skip);
            b
        };
        let mut active_nodes = VecDeque::<ActiveNode>::from([
            // TeX.2021.864
            ActiveNode {
                diffs: Default::default(),
                fitness_class: FitnessClass::Decent,
                hyphenated: false,
                line_number: 0,
                node_index: 0,
                total_demerits: 0,
            },
        ]);
        // TeX.2021.864
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
                    // This corresponds to the try_break call in TeX.202.873.
                    // I'm guessing the last break is considered hyphenated so as to penalize
                    // the second-to-last line being hyphenated. A hyphen doesn't look nice
                    // in the bottom right of the paragraph.
                    (EJECT_PENALTY, true)
                }
                Some(elem) => match elem {
                    Char(ds::Char { char, font }) | Ligature(ds::Ligature { char, font, .. }) => {
                        // TeX.2021.867 has an optimization in which subsequent chars are read
                        // here. I'm not convinced it's worth it.
                        diffs.width += font_repo.width(*char, *font).unwrap_or(Scaled::ZERO);
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
                // TODO: For discretionary nodes we need to adjust the width here?
                continue;
            }
            if penalty <= EJECT_PENALTY {
                penalty = EJECT_PENALTY
            }

            let mut n = active_nodes.len();
            while n > 0 {
                let mut m = self.num_nodes_for_next_class(&active_nodes, n);
                n -= m;

                // TeX.2021.833
                #[derive(Clone, Copy, Debug)]
                struct Candidate {
                    total_demerits: i32,
                    previous_node_index: usize,
                    line_number: usize,
                    artificial_demerits: bool,
                }
                // TeX.2021.834
                let mut candidates = [Candidate {
                    total_demerits: AWFUL_BAD,
                    previous_node_index: 0,
                    line_number: 0,
                    artificial_demerits: false,
                }; 4];
                let mut minimum_demerits = AWFUL_BAD;

                while m > 0 {
                    m -= 1;
                    let active_node = active_nodes.pop_front().expect("active nodes to consider");
                    // This is the key formula that essentially defines what we're doing with diffs.
                    let line_diffs = diffs.clone() - active_node.diffs.clone() + background.clone();
                    let line_width = self
                        .line_widths
                        // The index here is actually line_index = (line_number - 1)
                        // = (previous_line_number + 1 - 1) = previous_line_number.
                        .get(active_node.line_number)
                        .copied()
                        .unwrap_or(*self.line_widths.last().unwrap());

                    // TeX.2021.851
                    let shortfall = Scaled64(line_width.0 as i64)
                        - line_diffs.width
                        - Scaled64(disc_width.0 as i64);
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
                        let b = if -shortfall > line_diffs.shrinkability {
                            INFINITE_BADNESS + 1
                        } else {
                            badness(-shortfall, line_diffs.shrinkability)
                        };
                        (
                            b,
                            if b <= 12 {
                                FitnessClass::Decent
                            } else {
                                FitnessClass::Tight
                            },
                        )
                    };

                    // The condition of the if statement is in TeX.2021.851
                    let (deactivate, allowable_break, artificial_demerits) =
                        if badness > INFINITE_BADNESS || penalty == EJECT_PENALTY {
                            // TeX.2021.854
                            if force_solution
                                && minimum_demerits == AWFUL_BAD
                                && active_nodes.is_empty()
                            {
                                (true, true, true)
                            } else {
                                (true, badness <= tolerance, false)
                            }
                        } else {
                            (false, badness <= tolerance, false)
                        };

                    // Allowable break.
                    // Add a candidate
                    if allowable_break {
                        // TeX.2021.855
                        let demerits = if artificial_demerits {
                            0_i32
                        } else {
                            self.demerits(
                                badness,
                                penalty,
                                active_node.fitness_class,
                                fitness_class,
                                active_node.hyphenated && hyphenated,
                                active_node.hyphenated && elem.is_none(),
                            )
                        };
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
                                    artificial_demerits,
                                    previous_node_index: active_node.node_index,
                                },
                            );
                        }
                        let candidate = &mut candidates[fitness_class as usize];
                        if total_demerits <= candidate.total_demerits {
                            *candidate = Candidate {
                                total_demerits,
                                previous_node_index: active_node.node_index,
                                line_number: active_node.line_number + 1,
                                artificial_demerits,
                            };
                        }
                        if total_demerits <= minimum_demerits {
                            minimum_demerits = total_demerits;
                        }
                    }
                    if !deactivate {
                        // Our implementation of TeX.2021.860
                        active_nodes.push_back(active_node);
                    }
                }

                // TeX.2021.835
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
                                        | Ligature(ds::Ligature { char, font, .. }) => {
                                            font_repo.width(*char, *font).unwrap_or(Scaled::ZERO)
                                        }
                                        HBox(ds::HBox { width, .. })
                                        | VBox(ds::VBox { width, .. })
                                        | Rule(ds::Rule { width, .. })
                                        | Kern(ds::Kern { width, .. }) => *width,
                                        _ => {
                                            eprintln!(
                                                "invalid node {:?} in discretionary replacement list",
                                                &list[j]
                                            );
                                            Scaled::ZERO
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
                                artificial_demerits: candidate.artificial_demerits,
                                previous_node_index: candidate.previous_node_index,
                            });
                        }
                        active_nodes.push_back(active_node);
                        passive_nodes.push(PassiveNode {
                            elem: i,
                            previous_node_index: candidate.previous_node_index,
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

        // TeX.2021.874
        // The following line exits early if there are no active nodes and thus no solution.
        let mut best = active_nodes.front()?;
        for active_node in &active_nodes {
            if active_node.total_demerits < best.total_demerits {
                best = active_node;
            }
        }

        if self.params.looseness != 0 {
            // TeX.2021.875
            let looseness = self.params.looseness;
            let best_line_number: i32 = best.line_number.try_into().unwrap();
            let mut actual_looseness = 0;
            for active_node in &active_nodes {
                let line_number: i32 = active_node.line_number.try_into().unwrap();
                let line_diff = line_number - best_line_number;

                if (line_diff < actual_looseness && looseness <= line_diff)
                    || (line_diff > actual_looseness && looseness >= line_diff)
                {
                    best = active_node;
                    actual_looseness = line_diff;
                } else if line_diff == actual_looseness
                    && active_node.total_demerits < best.total_demerits
                {
                    best = active_node;
                }
            }
            // If we don't get the desired looseness and this is not the final pass,
            // we try again. This conditional is a negated version of the last conditional
            // in TeX.2021.873.
            if actual_looseness != looseness && !force_solution {
                return None;
            }
        }

        if let Some(debug_logger) = self.debug_logger.as_deref_mut() {
            debug_logger.log_selected_node(best.node_index);
        }

        // TeX.2021.878
        let mut v = vec![];
        let mut index = best.node_index;
        while index > 0 {
            let passive_node = &passive_nodes[index];
            v.push(passive_node.elem);
            index = passive_node.previous_node_index;
        }
        v.reverse();
        Some(v)
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

    /// Returns the number of active nodes at the head of the list that will all create
    /// new active nodes of the same line class.
    ///
    /// Note that the active nodes at the head grouped in this was may not all have the
    /// same line class themselves. For example,
    /// if the line widths are [5,4,3], then there are 3 line classes: line 1, line 2, and
    /// remaining lines. Active nodes in line 1 create active nodes in line 2. But active
    /// nodes in line 2 and remaining lines create active nodes in remaining lines. Thus in
    /// the grouping here, active nodes for line 2 and remaining nodes are returned
    /// together.
    ///
    /// This "subtlety" is covered by unit tests and was in fact discovered by a failing
    /// unit test.
    fn num_nodes_for_next_class(&self, active_nodes: &VecDeque<ActiveNode>, k: usize) -> usize {
        let first_active_node = active_nodes.front().expect("active nodes are non-empty");
        let prev_line_number = first_active_node.line_number;
        if self.params.looseness == 0 && prev_line_number + 2 >= self.line_widths.len() {
            // This covers the last class of active nodes whose widths are all the same.
            return k;
        }
        active_nodes
            .iter()
            .take(k)
            .take_while(|active_node| active_node.line_number == first_active_node.line_number)
            .count()
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

#[cfg(test)]
mod tests {
    use super::*;
    use boxworks::TextPreprocessor;
    use boxworks_text as bwt;
    use pretty_assertions::assert_eq;
    use std::{cell::RefCell, rc::Rc};

    const TFM_CMR10: &'static [u8] = include_bytes!("../../tfm/corpus/computer-modern/cmr10.tfm");

    macro_rules! tests {
        (
            $( (
                $name: ident,
                $input: expr,
                $widths: expr,
                $( text_params: boxworks_text::Params {
                    $( $text_param_name: ident: $text_param_value: expr, )+
                }, )?
                $( params: Params {
                    $( $param_name: ident: $param_value: expr, )+
                }, )?
                $( typeset: $want: expr, )?
                $( log: $want_log: expr, )?
            ), )+
        ) => {
            $(
                mod $name {
                    use super::*;
                    const INPUT: &'static str = include_str!(concat!("../testdata/", $input));
                    $(
                    #[test]
                    fn typeset() {
                        let input_file = concat!(env!("CARGO_MANIFEST_DIR"), "/testdata/", $want);
                        let want = include_str!(concat!("../testdata/", $want));
                        let widths = $widths;
                        run_test(
                            TFM_CMR10,
                            INPUT,
                            input_file,
                            want,
                            widths,
                            text_params(),
                            params(),
                        );
                    }
                    )?
                    $(
                    #[test]
                    fn log() {
                        let log_file = concat!(env!("CARGO_MANIFEST_DIR"), "/testdata/", $want_log);
                        let want_log = include_str!(concat!("../testdata/", $want_log));
                        let widths = $widths;
                        run_log_test(
                            TFM_CMR10,
                            INPUT,
                            log_file,
                            want_log,
                            widths,
                            text_params(),
                            params(),
                        );
                    }
                    )?

                    fn text_params() -> boxworks_text::Params {
                        boxworks_text::Params {
                            $( $(
                                $text_param_name: $text_param_value,
                            )+ )?
                            .. boxworks_text::Params::plain_tex_defaults()
                        }
                    }

                    fn params() -> Params {
                        Params {
                            $( $(
                                $param_name: $param_value,
                            )+ )?
                            .. Params::plain_tex_defaults()
                        }
                    }
                }
            )+
        };
    }

    tests!(
        (
            wolf_hall_5in,
            "wolf_hall_input.txt",
            &["5in"],
            typeset: "wolf_hall_5in_want.txt",
            log: "wolf_hall_5in_log.txt",
        ),
        (
            wolf_hall_3in,
            "wolf_hall_input.txt",
            &["3in"],
            typeset: "wolf_hall_3in_want.txt",
            log: "wolf_hall_3in_log.txt",
        ),
        (
            wolf_hall_2in,
            "wolf_hall_input.txt",
            &["2in"],
            typeset: "wolf_hall_2in_want.txt",
            log: "wolf_hall_2in_log.txt",
        ),
        (
            wolf_hall_variable_widths,
            "wolf_hall_input.txt",
            &["5in", "4in", "3in", "4in"],
            typeset: "wolf_hall_variable_widths_want.txt",
            log: "wolf_hall_variable_widths_log.txt",
        ),
        (
            farewell_to_arms_looseness_plus_1,
            "farewell_to_arms_input.txt",
            &["3in"],
            params: Params {
                looseness: 1,
            },
            typeset: "farewell_to_arms_looseness_plus_1_want.txt",
            // TODO: there is a real bug that is exposed by this log diff: at
            // breaks following a hyphenated word with a ligature after the
            // hyphen ("of-fi..."), our badness disagrees with TeX (b=40 vs
            // b=6) and a later node picks a different predecessor. Likely
            // the unimplemented post-break width adjustment for
            // discretionaries (the TeX.2021.840 TODO in break_line).
            // log: "farewell_to_arms_looseness_plus_1_log.txt",
        ),
        (
            farewell_to_arms_looseness_minus_1,
            "farewell_to_arms_input.txt",
            &["5in"],
            params: Params {
                looseness: -1,
            },
            typeset: "farewell_to_arms_looseness_minus_1_want.txt",
            log: "farewell_to_arms_looseness_minus_1_log.txt",
        ),
        (
            wolf_hall_ragged_right,
            "wolf_hall_input.txt",
            &["5in"],
            text_params: boxworks_text::Params {
                space_skip: common::Glue {
                    width: common::Scaled::parse_from_string("3.33298pt").unwrap(),
                    ..Default::default()
                },
                extra_space_skip: common::Glue {
                    width: common::Scaled::parse_from_string("5.0pt").unwrap(),
                    ..Default::default()
                },
            },
            params: Params {
                right_skip: common::Glue {
                    stretch: common::Scaled::parse_from_string("20.00003pt").unwrap(),
                    ..Default::default()
                },
            },
            typeset: "wolf_hall_ragged_right.txt",
            log: "wolf_hall_ragged_right_log.txt",
        ),
        (
            wolf_hall_ragged_right_margin,
            "wolf_hall_input.txt",
            &["5in"],
            text_params: boxworks_text::Params {
                space_skip: common::Glue {
                    width: common::Scaled::parse_from_string("3.33298pt").unwrap(),
                    ..Default::default()
                },
                extra_space_skip: common::Glue {
                    width: common::Scaled::parse_from_string("5.0pt").unwrap(),
                    ..Default::default()
                },
            },
            params: Params {
                right_skip: common::Glue {
                    width: common::Scaled::parse_from_string("20.0pt").unwrap(),
                    stretch: common::Scaled::parse_from_string("20.00003pt").unwrap(),
                    ..Default::default()
                },
            },
            typeset: "wolf_hall_ragged_right_margin.txt",
        ),
        (
            alice_paragraph_1_10in,
            "alice_paragraph_1.txt",
            &["10in"],
            typeset: "alice_paragraph_1_want.txt",
            log: "alice_paragraph_1_log.txt",
        ),
        (
            alice_paragraph_2_10in,
            "alice_paragraph_2.txt",
            &["10in"],
            typeset: "alice_paragraph_2_want.txt",
            log: "alice_paragraph_2_log.txt",
        ),
    );

    fn run(
        tfm_bytes: &[u8],
        input: &str,
        widths: &[&str],
        text_params: boxworks_text::Params,
        params: Params,
    ) -> (ds::VBox, String) {
        let mut tfm_file = tfm::File::deserialize(tfm_bytes).0.unwrap();
        let lig_kern_program =
            tfm::ligkern::CompiledProgram::compile_from_tfm_file(&mut tfm_file).0;
        let mut tp = bwt::TextPreprocessorImpl::new(text_params);
        tp.register_font(0, &tfm_file, lig_kern_program.clone());
        tp.activate_font(0);
        let mut list = vec![];
        for word in input.split_ascii_whitespace() {
            tp.add_word(word.trim_matches(' '), &mut list);
            tp.add_space(&mut list);
        }

        let mut font_repo: bwt::TfmFontRepo = Default::default();
        font_repo.register_font(0, tfm_file);
        let widths = parse_widths(widths);

        let log: Rc<RefCell<String>> = Default::default();
        let mut logger = debug::TexLogger::new(log.clone());

        let hyphenator = boxworks_hyphenate::Hyphenator::plain_tex_en_us(lig_kern_program);

        let line_breaker = super::LineBreaker {
            params: &params,
            line_widths: &widths,
            line_indents: &[],
            debug_logger: Some(&mut logger),
            hyphenator: &hyphenator,
        };
        let mut v_list = vec![];
        use boxworks::LineBreaker;
        line_breaker.break_line(&font_repo, &mut v_list, &mut list);

        let v_box = ds::VBox {
            list: v_list,
            ..Default::default()
        };
        (v_box, log.take())
    }

    fn run_test(
        tfm_bytes: &[u8],
        input: &str,
        input_file: &str,
        want: &str,
        widths: &[&str],
        text_params: boxworks_text::Params,
        params: Params,
    ) {
        if verify_with_tex() {
            let (vlist, _) = run_tex(tfm_bytes, input, widths, text_params, params);
            if std::env::var("TEXCRAFT_VERIFY_OVERWRITE").unwrap_or_default() == "true" {
                if !boxworks_testing::is_box_eq!(want, vlist.clone()) {
                    std::fs::write(input_file, format!["{vlist}"]).unwrap();
                }
            } else {
                boxworks_testing::assert_box_eq!(want, vlist);
            }
            return;
        }
        let (got, _) = run(tfm_bytes, input, widths, text_params, params);
        boxworks_testing::assert_box_eq!(want, got);
    }

    fn run_log_test(
        tfm_bytes: &[u8],
        input: &str,
        log_file: &str,
        want_log: &str,
        widths: &[&str],
        text_params: boxworks_text::Params,
        params: Params,
    ) {
        if verify_with_tex() {
            let (_, stdout) = run_tex(tfm_bytes, input, widths, text_params, params);
            let trace = extract_paragraph_trace(&stdout);
            let want = normalize(want_log);
            let got = normalize(&trace);
            if std::env::var("TEXCRAFT_VERIFY_OVERWRITE").unwrap_or_default() == "true" {
                if want != got {
                    std::fs::write(log_file, got).unwrap();
                }
            } else {
                assert_eq!(want, got);
            }
            return;
        }
        let (_, got_log) = run(tfm_bytes, input, widths, text_params, params);
        assert_eq!(normalize(want_log), normalize(&got_log));
    }

    fn parse_widths(widths: &[&str]) -> Vec<common::Scaled> {
        widths
            .iter()
            .map(|w| common::Scaled::parse_from_string(w).unwrap())
            .collect()
    }

    fn verify_with_tex() -> bool {
        std::env::var("TEXCRAFT_VERIFY").unwrap_or_default() == "tex"
    }

    /// A [`boxworks::tex::TexEngine`] that records the terminal output of the
    /// inner engine. The box-building templates set `\tracingonline=1`, so
    /// the `\tracingparagraphs` trace appears in this output.
    struct RecordingTexEngine {
        inner: Box<dyn boxworks::tex::TexEngine>,
        stdout: RefCell<String>,
    }

    impl boxworks::tex::TexEngine for RecordingTexEngine {
        fn run(
            &self,
            tex_source_code: &str,
            auxiliary_files: &std::collections::HashMap<std::path::PathBuf, Vec<u8>>,
        ) -> String {
            let output = self.inner.run(tex_source_code, auxiliary_files);
            *self.stdout.borrow_mut() = output.clone();
            output
        }
    }

    /// Runs TeX on the input and returns the resulting vertical list and the
    /// raw terminal output.
    fn run_tex(
        tfm_bytes: &[u8],
        input: &str,
        widths: &[&str],
        text_params: boxworks_text::Params,
        params: Params,
    ) -> (ds::VBox, String) {
        use std::collections::HashMap;
        use std::path::PathBuf;

        let mut auxiliary_files: HashMap<PathBuf, Vec<u8>> = Default::default();
        auxiliary_files.insert("customFont.tfm".into(), tfm_bytes.to_vec());
        let preamble = format![
            r"
            \tracingparagraphs=1

            % Boxworks does not yet append the \overfullrule rule to overfull
            % boxes (TeX.2021.666), so suppress it in TeX's output for now.
            \hfuzz=\maxdimen

            \font \customFont customFont

            \customFont
            {}
            {}
            ",
            text_params.tex(),
            params.tex(),
        ];

        // By default TeX wraps terminal output at 79 characters, which would
        // split long \tracingparagraphs lines. The testdata log files are
        // unwrapped, so raise the limit; the spawned tex process inherits it.
        std::env::set_var("max_print_line", "10000");

        let widths = parse_widths(widths);
        let engine = RecordingTexEngine {
            inner: boxworks::tex::new_tex_engine_binary("tex".to_string()).unwrap(),
            stdout: Default::default(),
        };
        let texts = vec![format![r"\looseness={} {}", params.looseness, input.trim()]];
        let (_, vlists) = boxworks::tex::build_vertical_lists(
            &engine,
            &auxiliary_files,
            &preamble,
            &widths,
            &mut texts.iter(),
        );
        let [vlist]: [ds::VBox; 1] = vlists.try_into().expect("one text in, one vlist out");
        (vlist, engine.stdout.into_inner())
    }

    /// Extracts the `\tracingparagraphs` trace from TeX's terminal output:
    /// everything from `@firstpass` (or `@secondpass`, if the first pass is
    /// skipped) up to the box display that follows the paragraph.
    fn extract_paragraph_trace(stdout: &str) -> String {
        let mut lines = vec![];
        let mut in_trace = false;
        for line in stdout.lines() {
            if !in_trace && (line.starts_with("@firstpass") || line.starts_with("@secondpass")) {
                in_trace = true;
            }
            if line.starts_with("Texcraft: begin") {
                break;
            }
            if in_trace {
                lines.push(line);
            }
        }
        lines.join("\n")
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
