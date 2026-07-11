use std::{cell::RefCell, rc::Rc};

use boxworks::ds;

pub struct FeasibleBreakpoint {
    pub elem_index: usize,
    pub badness: i32,
    pub penalty: i32,
    pub demerits: i32,
    pub artificial_demerits: bool,
    pub previous_node_index: usize,
}

pub struct NewActiveNode {
    pub node_index: usize,
    pub line_number: usize,
    pub fitness_class: u8,
    pub hyphenated: bool,
    pub total_demerits: i32,
    pub artificial_demerits: bool,
    pub previous_node_index: usize,
}

pub trait Logger {
    fn log_attempt(&mut self, attempt_number: u8);
    fn log_feasible_breakpoint(&mut self, list: &[ds::Horizontal], fb: FeasibleBreakpoint);
    fn log_new_active_node(&mut self, an: NewActiveNode);
    /// Called with the node index of the active node the algorithm chose
    /// (TeX.2021.874-877); the chosen breakpoints are the chain of
    /// `previous_node_index` links from this node. TeX's log has no
    /// equivalent line, hence the default no-op. This is used e.g. in knuthplass.dev
    /// to extract more information from the algorithm.
    fn log_selected_node(&mut self, _node_index: usize) {}
}

pub struct TexLogger {
    writer: Rc<RefCell<dyn std::fmt::Write>>,
    next_elem_to_write: usize,
}

impl TexLogger {
    pub fn new(writer: Rc<RefCell<dyn std::fmt::Write>>) -> Self {
        Self {
            writer,
            next_elem_to_write: 0,
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
        // TeX prints "*" for badness beyond infinite and for artificial
        // demerits (TeX.2021.856).
        let b = if fb.badness > crate::INFINITE_BADNESS {
            "*".into()
        } else {
            fb.badness.to_string()
        };
        let d = if fb.artificial_demerits {
            "*".into()
        } else {
            fb.demerits.to_string()
        };
        _ = writeln!(
            self.writer.borrow_mut(),
            "@{} via @@{} b={} p={} d={}",
            match list.get(fb.elem_index) {
                None => r"\par",
                Some(elem) => {
                    use ds::Horizontal::*;
                    match elem {
                        Discretionary(discretionary) => {
                            self.next_elem_to_write += discretionary.replace_count as usize;
                            r"\discretionary"
                        }
                        _ => "",
                    }
                }
            },
            fb.previous_node_index,
            b,
            fb.penalty,
            d,
        );
    }
    fn log_new_active_node(&mut self, an: NewActiveNode) {
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
