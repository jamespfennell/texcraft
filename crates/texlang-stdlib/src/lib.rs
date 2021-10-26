//! Texlang standard library of primitives.
//!
//! This module contains implementations of TeX primitives for Texcraft.

extern crate texcraft_stdext;
extern crate texlang_core;

use texlang_core::state::Base;
use texlang_core::token::catcode;

pub mod alloc;
pub mod catcodecmd;
pub mod conditional;
pub mod def;
pub mod execwhitespace;
pub mod letassignment;
#[macro_use]
pub mod registers;
pub mod texcraft;
pub mod the;
pub mod time;
pub mod tracing;
pub mod variableops;

/// A state struct that is compatible with every primitive in the Texlang standard library.
pub struct StdLibState {
    registers: registers::Component<32768>,
    time: time::Component,
    alloc: alloc::Component,
}

impl StdLibState {
    pub fn new() -> Base<StdLibState> {
        let mut s = Base::<StdLibState>::new(
            catcode::tex_defaults(),
            StdLibState {
                registers: registers::Component::new(),
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
        s.set_command("tracingmacros", tracing::get_tracingmacros());
        s.set_command("newint", alloc::get_newint());
        s.set_command("newarray", alloc::get_newarray());
        s
    }
}

implement_has_registers![StdLibState, registers, 32768];

impl time::HasTime for StdLibState {
    fn get_time(&self) -> &time::Component {
        &self.time
    }
    fn get_time_mut(&mut self) -> &mut time::Component {
        &mut self.time
    }
}

impl alloc::HasAlloc for StdLibState {
    fn alloc(&self) -> &alloc::Component {
        &self.alloc
    }
    fn alloc_mut(&mut self) -> &mut alloc::Component {
        &mut self.alloc
    }
}
