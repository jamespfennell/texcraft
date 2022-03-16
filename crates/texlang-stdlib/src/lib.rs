//! Texlang standard library of primitives.
//!
//! This module contains implementations of TeX primitives for Texcraft.

extern crate texcraft_stdext;
extern crate texlang_core;

use texlang_core::runtime::implement_has_component;
use texlang_core::runtime::Env;
use texlang_core::runtime::HasExpansionState;
use texlang_core::token::catcode::CatCodeMap;

pub mod alloc;
pub mod catcodecmd;
pub mod conditional;
pub mod def;
pub mod script;
pub mod io;
pub mod letassignment;
pub mod prefix;
pub mod registers;
#[cfg(test)]
pub mod testutil;
pub mod texcraft;
pub mod the;
pub mod time;
pub mod tracing;
pub mod variableops;

/// A state struct that is compatible with every primitive in the Texlang standard library.
#[derive(Default)]
pub struct StdLibState {
    alloc: alloc::Component,
    exec: script::Component,
    registers: registers::Component<32768>,
    prefix: prefix::Component,
    time: time::Component,
    expansion_state: StdLibExpansionState,
}

#[derive(Default)]
pub struct StdLibExpansionState {
    conditional: conditional::Component,
}

impl StdLibState {
    pub fn new() -> Env<StdLibState> {
        let mut s =
            Env::<StdLibState>::new(CatCodeMap::new_with_tex_defaults(), Default::default());
        conditional::add_all_conditionals(&mut s);

        s.set_command("advance", variableops::get_advance());

        s.set_command("catcode", catcodecmd::get_catcode());
        s.set_command("count", registers::get_count());
        s.set_command("countdef", registers::get_countdef());

        s.set_command("day", time::get_day());
        s.set_command("def", def::get_def());
        s.set_command("divide", variableops::get_divide());

        s.set_command("gdef", def::get_gdef());
        s.set_command("global", prefix::get_global());

        s.set_command("input", io::input::get_input());

        s.set_command("let", letassignment::get_let());
        s.set_command("long", prefix::get_long());

        s.set_command("month", time::get_month());
        s.set_command("multiply", variableops::get_multiply());

        s.set_command("newint", alloc::get_newint());
        s.set_command("newarray", alloc::get_newarray());

        s.set_command("outer", prefix::get_outer());

        s.set_command("the", the::get_the());
        s.set_command("time", time::get_time());
        s.set_command("tracingmacros", tracing::get_tracingmacros());

        s.set_command("year", time::get_year());

        s
    }
}

impl HasExpansionState for StdLibState {
    type E = StdLibExpansionState;

    fn expansion_state_mut(&mut self) -> &mut Self::E {
        &mut self.expansion_state
    }
}

implement_has_component![
    StdLibState,
    (alloc::Component, alloc),
    (script::Component, exec),
    (registers::Component<32768>, registers),
    (prefix::Component, prefix),
    (time::Component, time),
];

implement_has_component![StdLibExpansionState, (conditional::Component, conditional),];
