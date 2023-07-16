//! Primitive that pauses execution for a duration of time

use core::time;
use std::thread;
use texlang::traits::*;
use texlang::*;

pub const SLEEP_DOC: &str = "Sleep for a number of milliseconds";

/// Get the `\sleep` expansion primitive.
pub fn get_sleep<S: TexlangState>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(
        |_: token::Token, input: &mut vm::ExecutionInput<S>| -> command::Result<()> {
            let milliseconds = parse::Uint::<{ parse::Uint::MAX }>::parse(input)?.0;
            writeln![
                input.vm().terminal_out.borrow_mut(),
                "\\sleep: sleeping for {milliseconds}ms",
            ]
            .unwrap();
            thread::sleep(time::Duration::from_millis(milliseconds as u64));
            Ok(())
        },
    )
}
