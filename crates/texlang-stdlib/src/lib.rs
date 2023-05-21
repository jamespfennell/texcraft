//! Texlang standard library of primitives.
//!
//! This module contains implementations of TeX primitives for Texcraft.

extern crate texcraft_stdext;
extern crate texlang_core;

use std::collections::HashMap;

use texlang_core::traits::*;
use texlang_core::vm;
use texlang_core::vm::implement_has_component;

pub mod alias;
pub mod alloc;
pub mod catcode;
pub mod conditional;
pub mod def;
pub mod expansion;
pub mod io;
pub mod math;
pub mod prefix;
pub mod registers;
#[cfg(feature = "repl")]
pub mod repl;
pub mod script;
pub mod sleep;
pub mod testing;
pub mod texcraft;
pub mod the;
pub mod time;
pub mod tracingmacros;

/// A state struct that is compatible with every primitive in the Texlang standard library.
#[derive(Default)]
pub struct StdLibState {
    alloc: alloc::Component,
    catcode: catcode::Component,
    conditional: conditional::Component,
    prefix: prefix::Component,
    registers: registers::Component<32768>,
    script: script::Component,
    time: time::Component,
    tracing_macros: tracingmacros::Component,
}

impl TexlangState for StdLibState {
    #[inline]
    fn cat_code(&self, c: char) -> texlang_core::token::CatCode {
        catcode::cat_code(self, c)
    }

    #[inline]
    fn post_macro_expansion_hook(
        token: texlang_core::token::Token,
        input: &vm::ExpansionInput<Self>,
        tex_macro: &texlang_core::texmacro::Macro,
        arguments: &[&[texlang_core::token::Token]],
        reversed_expansion: &[texlang_core::token::Token],
    ) {
        tracingmacros::hook(token, input, tex_macro, arguments, reversed_expansion)
    }

    #[inline]
    fn expansion_override_hook(
        token: texlang_core::token::Token,
        input: &mut vm::ExpansionInput<Self>,
        tag: Option<texlang_core::command::Tag>,
    ) -> anyhow::Result<Option<texlang_core::token::Token>> {
        expansion::noexpand_hook(token, input, tag)
    }
}

impl StdLibState {
    pub fn all_initial_built_ins(
    ) -> HashMap<&'static str, texlang_core::command::BuiltIn<StdLibState>> {
        HashMap::from([
            ("advance", math::get_advance()),
            //
            ("catcode", catcode::get_catcode()),
            ("count", registers::get_count()),
            ("countdef", registers::get_countdef()),
            //
            ("day", time::get_day()),
            ("def", def::get_def()),
            ("divide", math::get_divide()),
            //
            ("else", conditional::get_else()),
            ("expandafter", expansion::get_expandafter_optimized()),
            //
            ("fi", conditional::get_fi()),
            //
            ("gdef", def::get_gdef()),
            ("global", prefix::get_global()),
            //
            ("ifcase", conditional::get_if_case()),
            ("iffalse", conditional::get_if_false()),
            ("ifnum", conditional::get_if_num()),
            ("ifodd", conditional::get_if_odd()),
            ("iftrue", conditional::get_if_true()),
            ("input", io::get_input()),
            //
            ("let", alias::get_let()),
            ("long", prefix::get_long()),
            //
            ("month", time::get_month()),
            ("multiply", math::get_multiply()),
            //
            ("newint", alloc::get_newint()),
            ("newarray", alloc::get_newarray()),
            ("noexpand", expansion::get_noexpand()),
            //
            ("or", conditional::get_or()),
            ("outer", prefix::get_outer()),
            //
            ("relax", expansion::get_relax()),
            //
            ("sleep", sleep::get_sleep()),
            //
            ("the", the::get_the()),
            ("time", time::get_time()),
            ("tracingmacros", tracingmacros::get_tracingmacros()),
            //
            ("year", time::get_year()),
        ])
    }

    pub fn new() -> vm::VM<StdLibState> {
        vm::VM::<StdLibState>::new(StdLibState::all_initial_built_ins(), Default::default())
    }
}

implement_has_component![
    StdLibState,
    (alloc::Component, alloc),
    (catcode::Component, catcode),
    (conditional::Component, conditional),
    (prefix::Component, prefix),
    (registers::Component<32768>, registers),
    (script::Component, script),
    (time::Component, time),
    (tracingmacros::Component, tracing_macros),
];

#[cfg(test)]
mod tests {
    use super::*;
    use testing::*;
    use texlang_core::command;

    type State = StdLibState;

    fn initial_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        StdLibState::all_initial_built_ins()
    }

    test_suite![expansion_equality_tests((
        overwrite_else,
        r"\def\else{}\ifodd 2 \else should be skipped \fi",
        r""
    ),),];
}
