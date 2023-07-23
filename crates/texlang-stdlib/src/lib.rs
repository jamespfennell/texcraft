//! # The Texlang standard library
//!
//! This module contains implementations of TeX primitives for Texlang.

extern crate texcraft_stdext;
extern crate texlang;

use std::collections::HashMap;

use texlang::command;
use texlang::token;
use texlang::traits::*;
use texlang::vm;
use texlang::vm::implement_has_component;

pub mod alias;
pub mod alloc;
pub mod catcode;
pub mod chardef;
pub mod conditional;
pub mod def;
pub mod endlinechar;
pub mod expansion;
pub mod input;
pub mod job;
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
    pub alloc: alloc::Component,
    pub catcode: catcode::Component,
    pub conditional: conditional::Component,
    pub end_line_char: endlinechar::Component,
    pub input: input::Component<16>,
    pub job: job::Component,
    pub prefix: prefix::Component,
    pub registers_i32: registers::Component<i32, 32768>,
    pub registers_token_list: registers::Component<Vec<token::Token>, 256>,
    pub repl: repl::Component,
    pub script: script::Component,
    pub time: time::Component,
    pub tracing_macros: tracingmacros::Component,
}

impl TexlangState for StdLibState {
    #[inline]
    fn cat_code(&self, c: char) -> texlang::token::CatCode {
        catcode::cat_code(self, c)
    }

    #[inline]
    fn end_line_char(&self) -> Option<char> {
        endlinechar::end_line_char(self)
    }

    #[inline]
    fn post_macro_expansion_hook(
        token: texlang::token::Token,
        input: &vm::ExpansionInput<Self>,
        tex_macro: &texlang::texmacro::Macro,
        arguments: &[&[texlang::token::Token]],
        reversed_expansion: &[texlang::token::Token],
    ) {
        tracingmacros::hook(token, input, tex_macro, arguments, reversed_expansion)
    }

    #[inline]
    fn expansion_override_hook(
        token: texlang::token::Token,
        input: &mut vm::ExpansionInput<Self>,
        tag: Option<texlang::command::Tag>,
    ) -> command::Result<Option<texlang::token::Token>> {
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
    pub fn all_initial_built_ins() -> HashMap<&'static str, texlang::command::BuiltIn<StdLibState>>
    {
        HashMap::from([
            ("advance", math::get_advance()),
            //
            ("catcode", catcode::get_catcode()),
            ("closein", input::get_closein()),
            ("chardef", chardef::get_chardef()),
            ("count", registers::get_count()),
            ("countdef", registers::get_countdef()),
            //
            ("day", time::get_day()),
            ("def", def::get_def()),
            ("divide", math::get_divide()),
            ("dumpFormat", job::get_dumpformat()),
            ("dumpValidate", job::get_dumpvalidate()),
            //
            ("else", conditional::get_else()),
            ("endinput", input::get_endinput()),
            ("endlinechar", endlinechar::get_endlinechar()),
            ("expandafter", expansion::get_expandafter_optimized()),
            //
            ("fi", conditional::get_fi()),
            //
            ("gdef", def::get_gdef()),
            ("global", prefix::get_global()),
            ("globaldefs", prefix::get_globaldefs()),
            //
            ("ifcase", conditional::get_ifcase()),
            ("ifeof", input::get_ifeof()),
            ("iffalse", conditional::get_iffalse()),
            ("ifnum", conditional::get_ifnum()),
            ("ifodd", conditional::get_ifodd()),
            ("iftrue", conditional::get_iftrue()),
            ("input", input::get_input()),
            //
            ("jobname", job::get_jobname()),
            //
            ("let", alias::get_let()),
            ("long", prefix::get_long()),
            //
            ("month", time::get_month()),
            ("multiply", math::get_multiply()),
            //
            ("newInt", alloc::get_newint()),
            (
                "newInt_getter_provider_\u{0}",
                alloc::get_newint_getter_provider(),
            ),
            ("newIntArray", alloc::get_newintarray()),
            (
                "newIntArray_getter_provider_\u{0}",
                alloc::get_newintarray_getter_provider(),
            ),
            ("noexpand", expansion::get_noexpand()),
            //
            ("or", conditional::get_or()),
            ("openin", input::get_openin()),
            ("outer", prefix::get_outer()),
            //
            ("read", input::get_read()),
            ("relax", expansion::get_relax()),
            //
            ("sleep", sleep::get_sleep()),
            //
            ("the", the::get_the()),
            ("time", time::get_time()),
            ("toks", registers::get_toks()),
            ("toksdef", registers::get_toksdef()),
            ("tracingmacros", tracingmacros::get_tracingmacros()),
            //
            ("year", time::get_year()),
        ])
    }

    /// Create a new VM that uses the standard library's state and all of its commands.
    pub fn new_vm() -> Box<vm::VM<StdLibState>> {
        vm::VM::<StdLibState>::new(StdLibState::all_initial_built_ins())
    }
}

implement_has_component![
    StdLibState,
    (alloc::Component, alloc),
    (catcode::Component, catcode),
    (conditional::Component, conditional),
    (endlinechar::Component, end_line_char),
    (input::Component<16>, input),
    (job::Component, job),
    (prefix::Component, prefix),
    (registers::Component<i32, 32768>, registers_i32),
    (
        registers::Component<Vec<token::Token>, 256>,
        registers_token_list
    ),
    (repl::Component, repl),
    (script::Component, script),
    (time::Component, time),
    (tracingmacros::Component, tracing_macros),
];

impl texlang_common::HasLogging for StdLibState {}
impl texlang_common::HasFileSystem for StdLibState {}
impl texlang_common::HasTerminalIn for StdLibState {}

/// A TeX snippet that exercises some error case in the standard library.
pub struct ErrorCase {
    pub description: &'static str,
    pub source_code: &'static str,
}

impl ErrorCase {
    /// Returns a vector of TeX snippets that exercise all error paths in Texlang.
    pub fn all_error_cases() -> Vec<ErrorCase> {
        let mut cases = vec![];
        for (description, source_code) in vec![
            (r"\toks starts with a letter token", r"\toks 0 = a"),
            (
                r"\toks starts with a non-variable command",
                r"\toks 0 = \def",
            ),
            (
                r"\toks starts is a variable command of the wrong type",
                r"\toks 0 = \count 0",
            ),
            (
                r"end of input while scanning token list",
                r"\toks 0 = {  no closing brace",
            ),
            (r"assign number from \toks", r"\count 0 = \toks 0"),
            (r"end of input right after \toks", r"\toks 0"),
            (r"\count is out of bounds (negative)", r"\count -200"),
            (r"\count is out of bounds (positive)", r"\count 2000000"),
            ("file does not exist", r"\input doesNotExist"),
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
    use texlang::command;

    type State = StdLibState;

    fn initial_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        StdLibState::all_initial_built_ins()
    }

    test_suite![
        expansion_equality_tests(
            (
                overwrite_else,
                r"\def\else{}\ifodd 2 \else should be skipped \fi",
                r""
            ),
            (
                math_and_active_char,
                r"\catcode`\A=13 \countdef A5 \countdef ~6 ~=7 A=8 \advance~byA \the~",
                r"15",
            ),
            /*
                        s.cat_code_map_mut().insert(
                '[' as u32,
                catcode::RawCatCode::Regular(catcode::CatCode::BeginGroup),
            );
            s.cat_code_map_mut().insert(
                ']' as u32,
                catcode::RawCatCode::Regular(catcode::CatCode::EndGroup),
            );
            s.cat_code_map_mut().insert(
                '!' as u32,
                catcode::RawCatCode::Regular(catcode::CatCode::texmacro::Parameter),
                 */
            (
                texbook_exercise_20_7,
                r"\catcode`\[=1 \catcode`\]=2 \catcode`\!=6 \def\!!1#2![{!#]#!!2}\! x{[y]][z}",
                r"\catcode`\[=1 \catcode`\]=2 \catcode`\!=6 {#]![y][z}",
            )
        ),
        serde_tests((serde_sanity, r"\def\HW{Hello World} ", r"\HW"),),
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
