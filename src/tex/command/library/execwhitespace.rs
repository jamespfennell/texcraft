/// Commands for printing whitespace in exec mode
use crate::tex::prelude::*;

fn newline_primitive_fn<S>(_: Token, input: &mut ExecutionInput<S>) -> anyhow::Result<()> {
    let newline_token = Token::new_space('\n');
    input.base_mut().exec_output.push(newline_token);
    input.base_mut().num_trailing_newlines += 1;
    Ok(())
}

/// Get the `\newline` command.
pub fn get_newline<S>() -> command::ExecutionPrimitive<S> {
    command::ExecutionPrimitive {
        call_fn: newline_primitive_fn,
        docs: "",
        id: None,
    }
}

fn par_primitive_fn<S>(_: Token, input: &mut ExecutionInput<S>) -> anyhow::Result<()> {
    if input.base().exec_output.is_empty() || input.base().num_trailing_newlines >= 2 {
        return Ok(());
    }
    let par_token = Token::new_space('\n');
    if input.base().num_trailing_newlines == 0 {
        input.base_mut().exec_output.push(par_token);
        input.base_mut().num_trailing_newlines += 1;
    }
    input.base_mut().exec_output.push(par_token);
    input.base_mut().num_trailing_newlines += 1;
    Ok(())
}

/// Get the `\par` command.
pub fn get_par<S>() -> command::ExecutionPrimitive<S> {
    command::ExecutionPrimitive {
        call_fn: par_primitive_fn,
        docs: "",
        id: None,
    }
}
