//! Texcraft Playground TeX engine

use wasm_bindgen::prelude::*;
use web_sys::console;

use texcraft::tex::command::library::catcodecmd;
use texcraft::tex::command::library::conditional;
use texcraft::tex::command::library::def;
use texcraft::tex::command::library::registers;
use texcraft::tex::command::library::the;
use texcraft::tex::command::library::execwhitespace;
use texcraft::tex::command::library::alloc;
use texcraft::tex::command::library::letassignment;
use texcraft::tex::command::library::time;
use texcraft::tex::command::library::variableops;
use texcraft::tex::driver;
use texcraft::tex::input;
use texcraft::tex::prelude::*;
use texcraft::tex::token;
use texcraft::tex::token::catcode;

#[wasm_bindgen(start)]
pub fn main_js() -> Result<(), JsValue> {
    #[cfg(debug_assertions)]
    console_error_panic_hook::set_once();

    console::log_1(&JsValue::from_str("Texcraft WASM initialized"));
    Ok(())
}

#[wasm_bindgen]
pub fn greet(input: String, minutes_since_midnight: i32, day: i32, month: i32, year: i32) -> String {
    let mut s = init_state(minutes_since_midnight, day, month, year);
    let mut input_unit = input::Unit::new();
    input_unit.push_new_str(input);

    match driver::exec(&mut s, &mut input_unit, true) {
        Ok(tokens) => token::write_tokens(&tokens),
        Err(err) => format!["{}", err],
    }
}

struct PlaygroundState {
    alloc: alloc::Component,
    time: time::Component,
}

impl alloc::HasAlloc for PlaygroundState {
    fn alloc(&self) -> &alloc::Component {
        &self.alloc
    }
    fn alloc_mut(&mut self) -> &mut alloc::Component {
        &mut self.alloc
    }
}

impl time::HasTime for PlaygroundState {
    fn get_time(&self) -> &time::Component {
        &self.time
    }
    fn get_time_mut(&mut self) -> &mut time::Component {
        &mut self.time
    }
}

fn init_state(minutes_since_midnight: i32, day: i32, month: i32, year: i32) -> Base<PlaygroundState> {
    let mut s = Base::<PlaygroundState>::new(
        catcode::tex_defaults(), 
        PlaygroundState{
            alloc: alloc::Component::new(),
            time: time::Component::new_with_values(minutes_since_midnight, day, month, year),
        },
    );
    conditional::add_all_conditionals(&mut s);
    def::add_all_commands(&mut s);
    s.set_command("the", the::get_the());
    //s.set_command("count", registers::get_count());
    //s.set_command("countdef", registers::get_countdef());
    s.set_command("newint", alloc::get_newint());
    s.set_command("newarray", alloc::get_newarray());
    s.set_command("catcode", catcodecmd::get_catcode());
    s.set_command("advance", variableops::get_advance());
    s.set_command("multiply", variableops::get_multiply());
    s.set_command("divide", variableops::get_divide());
    s.set_command("time", time::get_time());
    s.set_command("day", time::get_day());
    s.set_command("month", time::get_month());
    s.set_command("year", time::get_year());
    s.set_command("par", execwhitespace::get_par());
    s.set_command("let", letassignment::get_let());
    s.set_command("newline", execwhitespace::get_newline());
    s.set_command("\\", command::Command::Character('\\', CatCode::Other));
    s
}
