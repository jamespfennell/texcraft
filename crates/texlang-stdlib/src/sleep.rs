//! Primitive that pauses execution for a duration of time

use core::time;
use std::thread;
use texlang_core::parse;
use texlang_core::prelude::*;

pub const SLEEP_DOC: &str = "Sleep for a number of milliseconds";

/// Get the `\sleep` expansion primitive.
pub fn get_sleep<S>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(
        |_: Token, input: &mut vm::ExecutionInput<S>| -> anyhow::Result<()> {
            let milliseconds: u32 = parse::parse_number(input)?;
            thread::sleep(time::Duration::from_millis(milliseconds as u64));
            Ok(())
        },
    )
}
