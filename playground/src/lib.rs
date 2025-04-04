//! Texcraft Playground TeX engine

use std::collections::HashMap;

use wasm_bindgen::prelude::*;
use web_sys::console;

use texcraft_stdext::collections::groupingmap;
use texlang::prelude as txl;
use texlang::traits::*;
use texlang::*;
use texlang_stdlib::*;

#[wasm_bindgen(start)]
pub fn main_js() -> Result<(), JsValue> {
    #[cfg(debug_assertions)]
    console_error_panic_hook::set_once();

    console::log_1(&JsValue::from_str("Texcraft WASM initialized"));
    Ok(())
}

#[wasm_bindgen]
pub fn run(
    file_name: String,
    input: String,
    minutes_since_midnight: i32,
    day: i32,
    month: i32,
    year: i32,
) -> String {
    let mut vm = new_vm(minutes_since_midnight, day, month, year);
    vm.push_source(file_name, input).unwrap();
    match script::run_to_string(&mut vm) {
        Ok(s) => s,
        Err(err) => format!["{err}"],
    }
}

#[derive(Default)]
struct PlaygroundState {
    alloc: alloc::Component,
    codes_cat_code: codes::Component<types::CatCode>,
    codes_math_code: codes::Component<types::MathCode>,
    conditional: conditional::Component,
    end_line_char: endlinechar::Component,
    prefix: prefix::Component,
    registers_i32: registers::Component<i32, 32768>,
    registers_scaled: registers::Component<core::Scaled, 32768>,
    registers_glue: registers::Component<core::Glue, 32768>,
    registers_token_list: registers::Component<Vec<token::Token>, 256>,
    script: script::Component,
    time: time::Component,
}

impl TexlangState for PlaygroundState {
    fn cat_code(&self, c: char) -> texlang::types::CatCode {
        codes::cat_code(self, c)
    }

    fn expansion_override_hook(
        token: texlang::token::Token,
        input: &mut vm::ExpansionInput<Self>,
        tag: Option<texlang::command::Tag>,
    ) -> txl::Result<Option<texlang::token::Token>> {
        expansion::noexpand_hook(token, input, tag)
    }

    fn variable_assignment_scope_hook(state: &mut Self) -> groupingmap::Scope {
        prefix::variable_assignment_scope_hook(state)
    }
}

impl texlang_stdlib::the::TheCompatible for PlaygroundState {}

implement_has_component![PlaygroundState{
    alloc: alloc::Component,
    codes_cat_code: codes::Component<types::CatCode>,
    codes_math_code: codes::Component<types::MathCode>,
    conditional: conditional::Component,
    end_line_char: endlinechar::Component,
    prefix: prefix::Component,
    registers_i32: registers::Component<i32, 32768>,
    registers_scaled: registers::Component<core::Scaled, 32768>,
    registers_glue: registers::Component<core::Glue, 32768>,
    registers_token_list: registers::Component<Vec<token::Token>, 256>,
    script: script::Component,
    time: time::Component,
}];

impl HasDefaultBuiltInCommands for PlaygroundState {
    fn default_built_in_commands() -> HashMap<&'static str, command::BuiltIn<Self>> {
        HashMap::from([
            (
                "\\",
                command::Command::CharacterTokenAlias(token::Value::Other('\\')).into(),
            ),
            //
            ("advance", math::get_advance()),
            //
            ("catcode", codes::get_catcode()),
            ("chardef", chardef::get_chardef()),
            ("count", registers::get_count()),
            ("countdef", registers::get_countdef()),
            //
            ("day", time::get_day()),
            ("def", def::get_def()),
            ("dimen", registers::get_dimen()),
            ("divide", math::get_divide()),
            //
            ("else", conditional::get_else()),
            ("endinput", input::get_endinput()),
            ("endlinechar", endlinechar::get_endlinechar()),
            ("expandafter", expansion::get_expandafter_optimized()),
            //
            //
            ("fi", conditional::get_fi()),
            //
            ("gdef", def::get_gdef()),
            ("global", prefix::get_global()),
            ("globaldefs", prefix::get_globaldefs()),
            //
            ("ifcase", conditional::get_ifcase()),
            ("iffalse", conditional::get_iffalse()),
            ("ifnum", conditional::get_ifnum()),
            ("ifodd", conditional::get_ifodd()),
            ("iftrue", conditional::get_iftrue()),
            //
            ("let", alias::get_let()),
            ("long", prefix::get_long()),
            //
            ("mathchardef", mathchardef::get_mathchardef()),
            ("mathcode", codes::get_mathcode()),
            ("month", time::get_month()),
            ("multiply", math::get_multiply()),
            //
            ("newInt", alloc::get_newint()),
            ("newIntArray", alloc::get_newintarray()),
            ("newline", script::get_newline()),
            ("noexpand", expansion::get_noexpand()),
            //
            ("or", conditional::get_or()),
            ("outer", prefix::get_outer()),
            //
            ("par", script::get_par()),
            //
            ("relax", expansion::get_relax()),
            //
            ("skip", registers::get_skip()),
            //
            ("the", the::get_the()),
            ("toks", registers::get_toks()),
            ("toksdef", registers::get_toksdef()),
            ("time", time::get_time()),
            //
            ("year", time::get_year()),
        ])
    }
}

fn new_vm(minutes_since_midnight: i32, day: i32, month: i32, year: i32) -> vm::VM<PlaygroundState> {
    let mut vm = vm::VM::<PlaygroundState>::new();
    vm.state.time = time::Component::new_with_values(minutes_since_midnight, day, month, year);
    vm
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    #[test]
    fn built_in_commands_contain_all_std_lib_commands() {
        let playground_primitives: HashSet<&'static str> =
            PlaygroundState::default_built_in_commands()
                .into_keys()
                .collect();
        let std_lib_primitives: HashSet<&'static str> =
            texlang_stdlib::StdLibState::default_built_in_commands()
                .into_keys()
                .collect();
        let intentionally_missing: HashSet<&'static str> = vec![
            // Supporting error modes doesn't seem really useful for the Playground
            "batchmode",
            "errorstopmode",
            "nonstopmode",
            "scrollmode",
            // Playground doesn't have access to a filesystem (yet?)
            "closein",
            "openin",
            "ifeof",
            "input",
            "read",
            // Playground doesn't support (de)serialization
            "dump",
            "dumpFormat",
            "dumpValidate",
            "newInt_getter_provider_\u{0}",
            "newIntArray_getter_provider_\u{0}",
            //
            "jobname", // TODO: requires some setup
            //
            "sleep", // Using time functions in WASM is tedious, and \sleep isn't worth the hassle
            //
            "tracingmacros", // TODO: we should support this but we need to figure out capturing stderr
        ]
        .into_iter()
        .collect();

        let missing: HashSet<&'static str> = std_lib_primitives
            .difference(&playground_primitives)
            .map(|s| *s)
            .filter(|s| !intentionally_missing.contains(s))
            .collect();
        if !missing.is_empty() {
            panic!(
                "the playground is missing the following Texlang std lib primitives: {:?}",
                missing
            )
        }
    }
}
