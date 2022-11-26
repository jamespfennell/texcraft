//! Texlang standard library of primitives.
//!
//! This module contains implementations of TeX primitives for Texcraft.

extern crate texcraft_stdext;
extern crate texlang_core;

use std::collections::HashMap;

use texlang_core::runtime::implement_has_component;
use texlang_core::runtime::Env;
use texlang_core::token::catcode::CatCodeMap;

pub mod alloc;
pub mod catcodecmd;
pub mod conditional;
pub mod def;
pub mod io;
pub mod letassignment;
pub mod prefix;
pub mod registers;
#[cfg(feature = "repl")]
pub mod repl;
pub mod script;
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
    conditional: conditional::Component,
}

impl StdLibState {
    pub fn all_initial_built_ins(
    ) -> HashMap<&'static str, texlang_core::command::Command<StdLibState>> {
        HashMap::from([
            ("advance", variableops::get_advance()),
            //
            ("catcode", catcodecmd::get_catcode()),
            ("count", registers::get_count()),
            ("countdef", registers::get_countdef()),
            //
            ("day", time::get_day()),
            ("def", def::get_def()),
            ("divide", variableops::get_divide()),
            //
            ("else", conditional::get_else()),
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
            ("input", io::input::get_input()),
            //
            ("let", letassignment::get_let()),
            ("long", prefix::get_long()),
            //
            ("month", time::get_month()),
            ("multiply", variableops::get_multiply()),
            //
            ("newint", alloc::get_newint()),
            ("newarray", alloc::get_newarray()),
            //
            ("or", conditional::get_or()),
            ("outer", prefix::get_outer()),
            //
            ("the", the::get_the()),
            ("time", time::get_time()),
            ("tracingmacros", tracing::get_tracingmacros()),
            //
            ("year", time::get_year()),
        ])
    }

    pub fn new() -> Env<StdLibState> {
        Env::<StdLibState>::new(
            CatCodeMap::new_with_tex_defaults(),
            StdLibState::all_initial_built_ins(),
            Default::default(),
        )
    }
}

implement_has_component![
    StdLibState,
    (alloc::Component, alloc),
    (script::Component, exec),
    (registers::Component<32768>, registers),
    (prefix::Component, prefix),
    (time::Component, time),
    (conditional::Component, conditional),
];
