//! Texcraft Playground TeX engine

use wasm_bindgen::prelude::*;
use web_sys::console;

use texlang_core::prelude::*;
use texlang_core::runtime::{implement_has_component, HasExpansionState};
use texlang_core::token;
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
use texlang_stdlib::StdLibExpansionState;

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
    let mut env = init_state(minutes_since_midnight, day, month, year);
    env.push_source(file_name, input).unwrap();
    match script::run(&mut env, true) {
        Ok(tokens) => token::write_tokens(&tokens, env.cs_name_interner()),
        Err(err) => format!["{}", err],
    }
}

struct PlaygroundState {
    alloc: alloc::Component,
    exec: script::Component,
    registers: registers::Component<256>,
    prefix: prefix::Component,
    time: time::Component,
    expansion_state: StdLibExpansionState,
}

impl HasExpansionState for PlaygroundState {
    type E = StdLibExpansionState;

    fn expansion_state_mut(&mut self) -> &mut Self::E {
        &mut self.expansion_state
    }
}

implement_has_component![
    PlaygroundState,
    (alloc::Component, alloc),
    (script::Component, exec),
    (registers::Component<256>, registers),
    (prefix::Component, prefix),
    (time::Component, time),
];

fn init_state(
    minutes_since_midnight: i32,
    day: i32,
    month: i32,
    year: i32,
) -> runtime::Env<PlaygroundState> {
    let mut s = runtime::Env::<PlaygroundState>::new(
        CatCodeMap::new_with_tex_defaults(),
        PlaygroundState {
            alloc: Default::default(),
            exec: Default::default(),
            registers: Default::default(),
            prefix: Default::default(),
            time: time::Component::new_with_values(minutes_since_midnight, day, month, year),
            expansion_state: Default::default(),
        },
    );
    conditional::add_all_conditionals(&mut s);

    s.set_command("\\", command::Command::Character(Value::Other('\\')));

    s.set_command("advance", variableops::get_advance());

    s.set_command("catcode", catcodecmd::get_catcode());
    s.set_command("count", registers::get_count());
    s.set_command("countdef", registers::get_countdef());

    s.set_command("day", time::get_day());
    s.set_command("def", def::get_def());
    s.set_command("divide", variableops::get_divide());

    s.set_command("gdef", def::get_gdef());
    s.set_command("global", prefix::get_global());

    s.set_command("let", letassignment::get_let());

    s.set_command("month", time::get_month());
    s.set_command("multiply", variableops::get_multiply());

    s.set_command("newarray", alloc::get_newarray());
    s.set_command("newint", alloc::get_newint());
    s.set_command("newline", script::get_newline());

    s.set_command("par", script::get_par());

    s.set_command("the", the::get_the());
    s.set_command("time", time::get_time());

    s.set_command("year", time::get_year());

    s
}
