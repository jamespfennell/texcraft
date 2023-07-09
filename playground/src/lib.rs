//! Texcraft Playground TeX engine

use std::collections::HashMap;

use wasm_bindgen::prelude::*;
use web_sys::console;

use texcraft_stdext::collections::groupingmap;
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
    match script::run(&mut vm) {
        Ok(tokens) => token::write_tokens(&tokens, vm.cs_name_interner()),
        Err(err) => format!["{err:?}"],
    }
}

#[derive(Default)]
struct PlaygroundState {
    alloc: alloc::Component,
    catcode: catcode::Component,
    conditional: conditional::Component,
    end_line_char: endlinechar::Component,
    prefix: prefix::Component,
    registers: registers::Component<i32, 256>,
    script: script::Component,
    time: time::Component,
}

impl TexlangState for PlaygroundState {
    fn cat_code(&self, c: char) -> texlang::token::CatCode {
        catcode::cat_code(self, c)
    }

    fn expansion_override_hook(
        token: texlang::token::Token,
        input: &mut vm::ExpansionInput<Self>,
        tag: Option<texlang::command::Tag>,
    ) -> command::Result<Option<texlang::token::Token>> {
        expansion::noexpand_hook(token, input, tag)
    }

    fn variable_assignment_scope_hook(state: &mut Self) -> groupingmap::Scope {
        prefix::variable_assignment_scope_hook(state)
    }
}

implement_has_component![
    PlaygroundState,
    (alloc::Component, alloc),
    (catcode::Component, catcode),
    (conditional::Component, conditional),
    (endlinechar::Component, end_line_char),
    (prefix::Component, prefix),
    (registers::Component<i32, 256>, registers),
    (script::Component, script),
    (time::Component, time),
];

fn initial_primitives() -> HashMap<&'static str, command::BuiltIn<PlaygroundState>> {
    HashMap::from([
        (
            "\\",
            command::Command::Character(token::Value::Other('\\')).into(),
        ),
        //
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
        ("the", the::get_the()),
        ("time", time::get_time()),
        //
        ("year", time::get_year()),
    ])
}

fn new_vm(
    minutes_since_midnight: i32,
    day: i32,
    month: i32,
    year: i32,
) -> Box<vm::VM<PlaygroundState>> {
    let mut vm = vm::VM::<PlaygroundState>::new(initial_primitives());
    vm.state.time = time::Component::new_with_values(minutes_since_midnight, day, month, year);
    vm
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;
    use texlang_stdlib;

    #[test]
    fn initial_primitives_contain_all_std_lib_primitives() {
        let playground_primitives: HashSet<&'static str> =
            initial_primitives().into_keys().collect();
        let std_lib_primitives: HashSet<&'static str> =
            texlang_stdlib::StdLibState::all_initial_built_ins()
                .into_keys()
                .collect();
        let intentionally_missing: HashSet<&'static str> = vec![
            "dump",         // Playground doesn't support serding
            "dumpFormat",   // Playground doesn't support serding
            "dumpValidate", // Playground doesn't support serding
            //
            "input", // Playground doesn't have access to a filesystem (yet?)
            //
            "jobname", // TODO: requires some setup
            //
            "newInt_getter_provider_\u{0}", // Playground doesn't support serding
            "newIntArray_getter_provider_\u{0}", // Playground doesn't support serding
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
