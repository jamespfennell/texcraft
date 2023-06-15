//! Texlang standard library of primitives.
//!
//! This module contains implementations of TeX primitives for Texcraft.

extern crate texcraft_stdext;
extern crate texlang_core;

use std::collections::HashMap;

use texlang_core::command;
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
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct StdLibState {
    alloc: alloc::Component,
    catcode: catcode::Component,
    conditional: conditional::Component,
    prefix: prefix::Component,
    registers: registers::Component<i32, 32768>,
    repl: repl::Component,
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
    ) -> command::Result<Option<texlang_core::token::Token>> {
        expansion::noexpand_hook(token, input, tag)
    }

    #[inline]
    fn variable_assignment_scope_hook(
        state: &mut Self,
    ) -> texcraft_stdext::collections::groupingmap::Scope {
        prefix::variable_assignment_scope_hook(state)
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
            ("globaldefs", prefix::get_globaldefs()),
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
            ("newInt", alloc::get_newint()),
            ("newInt_getter_provider_\u{0}", alloc::get_newint_getter_provider()),
            ("newIntArray", alloc::get_newintarray()),
            ("newIntArray_getter_provider_\u{0}", alloc::get_newintarray_getter_provider()),
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

    pub fn new() -> Box<vm::VM<StdLibState>> {
        vm::VM::<StdLibState>::new(StdLibState::all_initial_built_ins())
    }
}

implement_has_component![
    StdLibState,
    (alloc::Component, alloc),
    (catcode::Component, catcode),
    (conditional::Component, conditional),
    (prefix::Component, prefix),
    (registers::Component<i32, 32768>, registers),
    (repl::Component, repl),
    (script::Component, script),
    (time::Component, time),
    (tracingmacros::Component, tracing_macros),
];

pub struct ErrorCase {
    pub description: &'static str,
    pub source_code: &'static str,
}

impl ErrorCase {
    /// Returns a vector of TeX snippets that exercise all error paths in Texlang
    pub fn all_error_cases() -> Vec<ErrorCase> {
        let mut cases = vec![];
        for (description, source_code) in vec![
            ("end of input after \\global", r"\global"),
            ("can't be prefixed by \\global", r"\global \sleep"),
            ("can't be prefixed by \\global (character)", r"\global a"),
            ("can't be prefixed by \\long", r"\long \let \a = \def"),
            ("can't be prefixed by \\outer", r"\outer \let \a = \def"),
            ("bad rhs in assignment", r"\year = X"),
            ("invalid variable (undefined)", r"\advance \undefined by 4"),
            (
                "invalid variable (not a variable command)",
                r"\advance \def by 4",
            ),
            ("invalid variable (character token)", r"\advance a by 4"),
            ("invalid variable (eof)", r"\advance"),
            ("invalid relation", r"\ifnum 3 z 4"),
            ("malformed by keyword", r"\advance \year bg"),
            ("undefined control sequence", r"\elephant"),
            ("invalid character", "\u{7F}"),
            ("empty control sequence", r"\"),
            ("invalid end of group", r"}"),
            ("invalid start of number", r"\count X"),
            ("invalid start of number (eof)", r"\count"),
            ("invalid start of number (not a variable)", r"\count \def"),
            (
                "case negative number to positive (from constant)",
                r"\count -1",
            ),
            (
                "cast negative number to positive (from variable)",
                r"\count 0 = 1 \count - \count 0",
            ),
            (
                "read positive number from negative variable value",
                r"\count 0 = -1 \count \count 0",
            ),
            ("invalid character", r"\count `\def"),
            ("invalid character (eof)", r"\count `"),
            ("invalid octal digit", r"\count '9"),
            ("invalid octal digit (eof)", r"\count '"),
            ("invalid hexadecimal digit", "\\count \"Z"),
            ("invalid hexadecimal digit (eof)", "\\count \""),
            (
                "decimal number too big (radix)",
                r"\count 1000000000000000000000",
            ),
            (
                "decimal number too big (sum)",
                r"\count 18446744073709551617",
            ),
            ("octal number too big", r"\count '7777777777777777777777"),
            (
                "hexadecimal number too big",
                "\\count \"AAAAAAAAAAAAAAAAAAAAAA",
            ),
            ("number with letter catcode", r"\catcode `1 = 11 \count 1"),
            /*
            ("", r""),
            ("", r""),
            ("", r""),
            ("", r""),
            ("", r""),
            ("", r""),
            ("", r""),
            ("", r""),
            ("", r""),
            ("", r""),
            ("", r""),
            ("", r""),
            ("", r""),
            ("", r""), */
            ("category code out of bounds", r"\catcode 0 = 17"),
            ("invalid command target", r"\let a = \year"),
            ("invalid command target (eof)", r"\let"),
        ] {
            cases.push(ErrorCase {
                description,
                source_code,
            })
        }
        cases
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use testing::*;
    use texlang_core::command;

    type State = StdLibState;

    fn initial_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        StdLibState::all_initial_built_ins()
    }

    test_suite![
        expansion_equality_tests((
            overwrite_else,
            r"\def\else{}\ifodd 2 \else should be skipped \fi",
            r""
        )),
        serde_tests((serde_sanity_check, r"\def\HW{Hello World} ", r"\HW"),),
    ];

    #[test]
    fn all_error_cases() {
        let options = vec![
            TestOption::InitialCommands(StdLibState::all_initial_built_ins),
            TestOption::AllowUndefinedCommands(false),
        ];
        for case in ErrorCase::all_error_cases() {
            println!("CASE {}", case.description);
            run_failure_test::<StdLibState>(case.source_code, &options)
        }
    }
}
