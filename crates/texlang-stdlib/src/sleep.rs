//! Primitive that pauses execution for a duration of time

use core::time;
use std::thread;
use texlang_core::traits::*;
use texlang_core::*;

pub const SLEEP_DOC: &str = "Sleep for a number of milliseconds";

/// Get the `\sleep` expansion primitive.
pub fn get_sleep<S: TexlangState>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(
        |_: token::Token, input: &mut vm::ExecutionInput<S>| -> command::Result<()> {
            let milliseconds = usize::parse(input)?;
            writeln![
                input.vm().std_err.borrow_mut(),
                "\\sleep: sleeping for {milliseconds}ms",
            ]
            .unwrap();
            thread::sleep(time::Duration::from_millis(milliseconds as u64));
            Ok(())
        },
    )
}
