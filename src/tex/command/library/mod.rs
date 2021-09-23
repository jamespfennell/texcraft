//! Texcraft primitive library.
//!
//! This module contains implementations of TeX primtives for Texcraft.

use crate::tex::state::Base;
use crate::tex::token::catcode;

pub mod alloc;
pub mod catcodecmd;
pub mod conditional;
pub mod def;
pub mod letassignment;
#[macro_use]
pub mod registers;
pub mod execwhitespace;
pub mod texcraft;
pub mod the;
pub mod time;
pub mod variableops;

/// A state struct that is compatible with every primitive in the Texcraft commands library.
///
/// This is temporary state struct until we create states corresponding to actualy TeX engines.
pub struct WholeLibraryState {
    registers: registers::Component,
    time: time::Component,
    alloc: alloc::Component,
}

impl WholeLibraryState {
    pub fn new() -> Base<WholeLibraryState> {
        let mut s = Base::<WholeLibraryState>::new(
            catcode::tex_defaults(),
            WholeLibraryState {
                registers: registers::Component::new(32768),
                time: time::Component::new(),
                alloc: alloc::Component::new(),
            },
        );
        conditional::add_all_conditionals(&mut s);
        def::add_all_commands(&mut s);
        s.set_command("the", the::get_the());
        s.set_command("let", letassignment::get_let());
        s.set_command("count", registers::get_count());
        s.set_command("countdef", registers::get_countdef());
        s.set_command("catcode", catcodecmd::get_catcode());
        s.set_command("advance", variableops::get_advance());
        s.set_command("multiply", variableops::get_multiply());
        s.set_command("divide", variableops::get_divide());
        s.set_command("time", time::get_time());
        s.set_command("day", time::get_day());
        s.set_command("month", time::get_month());
        s.set_command("year", time::get_year());
        s.set_command("newint", alloc::get_newint());
        s.set_command("newarray", alloc::get_newarray());
        s
    }
}

implement_has_registers![WholeLibraryState, registers];

impl time::HasTime for WholeLibraryState {
    fn get_time(&self) -> &time::Component {
        &self.time
    }
    fn get_time_mut(&mut self) -> &mut time::Component {
        &mut self.time
    }
}

impl alloc::HasAlloc for WholeLibraryState {
    fn alloc(&self) -> &alloc::Component {
        &self.alloc
    }
    fn alloc_mut(&mut self) -> &mut alloc::Component {
        &mut self.alloc
    }
}
