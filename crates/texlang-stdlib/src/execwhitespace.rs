/// Commands for printing whitespace in exec mode
use texlang_core::prelude::*;

#[derive(Default)]
pub struct Component {
    exec_output: Vec<Token>,
    num_trailing_newlines: usize,
}

/// Trait for states that contain a [time Component](Component).
pub trait HasExec {
    fn exec(&self) -> &Component;
    fn exec_mut(&mut self) -> &mut Component;
}

/// Get the `\newline` command.
pub fn get_newline<S: HasExec>() -> command::ExecutionFn<S> {
    newline_primitive_fn
}

fn newline_primitive_fn<S: HasExec>(
    t: Token,
    input: &mut runtime::ExecutionInput<S>,
) -> anyhow::Result<()> {
    let newline_token = Token::new_space('\n', t.traceback_id());
    input.state_mut().exec_mut().exec_output.push(newline_token);
    input.state_mut().exec_mut().num_trailing_newlines += 1;
    Ok(())
}

/// Get the `\par` command.
pub fn get_par<S: HasExec>() -> command::ExecutionFn<S> {
    par_primitive_fn
}

fn par_primitive_fn<S: HasExec>(
    t: Token,
    input: &mut runtime::ExecutionInput<S>,
) -> anyhow::Result<()> {
    if input.state().exec().exec_output.is_empty()
        || input.state().exec().num_trailing_newlines >= 2
    {
        return Ok(());
    }
    let par_token = Token::new_space('\n', t.traceback_id());
    if input.state().exec().num_trailing_newlines == 0 {
        input.state_mut().exec_mut().exec_output.push(par_token);
        input.state_mut().exec_mut().num_trailing_newlines += 1;
    }
    input.state_mut().exec_mut().exec_output.push(par_token);
    input.state_mut().exec_mut().num_trailing_newlines += 1;
    Ok(())
}

pub fn exec<S: HasExec>(
    execution_input: &mut runtime::ExecutionInput<S>,
    err_for_undefined_cs: bool,
) -> anyhow::Result<Vec<Token>> {
    let undefined_cs_handler = match err_for_undefined_cs {
        true => runtime::default_undefined_cs_handler,
        false => handle_character,
    };
    runtime::run(execution_input, handle_character, undefined_cs_handler)?;
    let mut result = Vec::new();
    std::mem::swap(
        &mut result,
        &mut execution_input.state_mut().exec_mut().exec_output,
    );
    Ok(result)
}

fn handle_character<S: HasExec>(
    mut token: Token,
    input: &mut runtime::ExecutionInput<S>,
) -> anyhow::Result<()> {
    if let Some('\n') = token.char() {
        token = Token::new_space(' ', token.traceback_id());
    }
    input.state_mut().exec_mut().exec_output.push(token);
    input.state_mut().exec_mut().num_trailing_newlines = 0;
    Ok(())
}
