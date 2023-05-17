//! Texcraft Playground TeX engine

use std::collections::HashMap;

use wasm_bindgen::prelude::*;
use web_sys::console;

use texlang_core::token::catcode;
use texlang_core::*;
use texlang_stdlib::alloc;
use texlang_stdlib::catcodecmd;
use texlang_stdlib::conditional;
use texlang_stdlib::def;
use texlang_stdlib::letassignment;
use texlang_stdlib::prefix;
use texlang_stdlib::registers;
use texlang_stdlib::script;
use texlang_stdlib::the;
use texlang_stdlib::time;
use texlang_stdlib::variableops;

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
        Err(err) => format!["{err}"],
    }
}

struct PlaygroundState {
    alloc: alloc::Component,
    exec: script::Component,
    registers: registers::Component<256>,
    prefix: prefix::Component,
    time: time::Component,
    conditional: conditional::Component,
}

implement_has_component![
    PlaygroundState,
    (alloc::Component, alloc),
    (script::Component, exec),
    (registers::Component<256>, registers),
    (prefix::Component, prefix),
    (time::Component, time),
    (conditional::Component, conditional),
];

fn new_vm(minutes_since_midnight: i32, day: i32, month: i32, year: i32) -> vm::VM<PlaygroundState> {
    let initial_built_ins = HashMap::from([
        (
            "\\",
            command::Command::Character(token::Value::Other('\\')).into(),
        ),
        //
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
        //
        ("let", letassignment::get_let()),
        //
        ("month", time::get_month()),
        ("multiply", variableops::get_multiply()),
        //
        ("newarray", alloc::get_newarray()),
        ("newint", alloc::get_newint()),
        ("newline", script::get_newline()),
        //
        ("or", conditional::get_or()),
        //
        ("par", script::get_par()),
        //
        ("the", the::get_the()),
        ("time", time::get_time()),
        //
        ("year", time::get_year()),
    ]);
    vm::VM::<PlaygroundState>::new(
        catcode::CatCodeMap::new_with_tex_defaults(),
        initial_built_ins,
        PlaygroundState {
            alloc: Default::default(),
            exec: Default::default(),
            registers: Default::default(),
            prefix: Default::default(),
            time: time::Component::new_with_values(minutes_since_midnight, day, month, year),
            conditional: Default::default(),
        },
        Default::default(),
    )
}
