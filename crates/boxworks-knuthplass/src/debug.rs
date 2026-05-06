use std::{cell::RefCell, rc::Rc};

use boxworks::ds;

pub struct FeasibleBreakpoint {
    pub elem_index: usize,
    pub badness: i32,
    pub penalty: i32,
    pub demerits: i32,
    pub previous_node_index: usize,
}

pub struct NewActiveNode {
    pub node_index: usize,
    pub line_number: usize,
    pub fitness_class: u8,
    pub hyphenated: bool,
    pub total_demerits: i32,
    pub previous_node_index: usize,
}

pub trait Logger {
    fn log_attempt(&mut self, attempt_number: u8);
    fn log_feasible_breakpoint(&mut self, list: &[ds::Horizontal], fb: FeasibleBreakpoint);
    fn log_new_active_node(&mut self, an: NewActiveNode);
}

pub struct TexLogger {
    writer: Rc<RefCell<dyn std::fmt::Write>>,
    next_elem_to_write: usize,
    pending_fbs: Vec<(FeasibleBreakpoint, String)>,
}

impl TexLogger {
    pub fn new(writer: Rc<RefCell<dyn std::fmt::Write>>) -> Self {
        Self {
            writer,
            next_elem_to_write: 0,
            pending_fbs: Default::default(),
        }
    }
}

impl Logger for TexLogger {
    fn log_attempt(&mut self, attempt_number: u8) {
        self.next_elem_to_write = 0;
        // TeX.2021.863
        let _ = writeln!(
            self.writer.borrow_mut(),
            "@{}",
            match attempt_number {
                1 => "firstpass",
                2 => "secondpass",
                _ => "emergencypass",
            }
        );
    }
    fn log_feasible_breakpoint(&mut self, list: &[ds::Horizontal], fb: FeasibleBreakpoint) {
        if self.next_elem_to_write <= fb.elem_index {
            let upper = if fb.elem_index >= list.len() {
                list.len() - 1
            } else {
                fb.elem_index
            };
            _ = boxworks::ds::short_display_hlist(
                &mut *self.writer.borrow_mut(),
                &list[self.next_elem_to_write..=upper],
            );
            _ = writeln!(&mut *self.writer.borrow_mut());
        }
        self.next_elem_to_write = fb.elem_index + 1;
        // Rather than writing the breakpoints now, we store them and write them out
        // when the active node is printed. We do this because the ordering of breakpoints
        // is different in our implementation versus TeXs. We want the logging to be the
        // same, we so need to print the breakpoints in the same order TeX does.
        // TeX.2021.856
        let line = format!(
            "@{} via @@{} b={} p={} d={}",
            match list.get(fb.elem_index) {
                None => r"\par",
                Some(elem) => {
                    use ds::Horizontal::*;
                    match elem {
                        Discretionary(_) => r"\discretionary",
                        _ => "",
                    }
                }
            },
            fb.previous_node_index,
            fb.badness,
            fb.penalty,
            fb.demerits,
        );
        self.pending_fbs.push((fb, line));
    }
    fn log_new_active_node(&mut self, an: NewActiveNode) {
        self.pending_fbs
            .sort_by(|a, b| a.0.previous_node_index.cmp(&b.0.previous_node_index));
        for (_, line) in &self.pending_fbs {
            _ = writeln!(self.writer.borrow_mut(), "{}", line);
        }
        self.pending_fbs.clear();
        // TeX.2021.846
        let _ = writeln!(
            self.writer.borrow_mut(),
            "@@{}: line {}.{}{} t={} -> @@{}",
            an.node_index,
            an.line_number,
            an.fitness_class,
            if an.hyphenated { "-" } else { "" },
            an.total_demerits,
            an.previous_node_index,
        );
    }
}
