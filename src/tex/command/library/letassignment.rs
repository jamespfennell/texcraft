//! `\let` assignments

use crate::tex::parse;
use crate::tex::prelude::*;

const LET_DOC: &str = "Assign a command or character to a control sequence";

/// Get the `\let` command.
pub fn get_let<S>() -> command::ExecutionPrimitive<S> {
    command::ExecutionPrimitive {
        call_fn: let_primitive_fn,
        docs: LET_DOC,
        id: None,
    }
}

fn let_primitive_fn<S>(let_token: Token, input: &mut ExecutionInput<S>) -> anyhow::Result<()> {
    let name =
        parse::parse_command_target("\\let assignment", let_token, input.unexpanded_stream())?;
    parse::parse_optional_equals(input.unexpanded_stream())?;
    let command =
        match input.unexpanded_stream().next()? {
            None => return Err(error::EndOfInputError::new(
                "unexpected end of input while reading the right hand side of a \\let assignment",
            )
            .cast()),
            Some(token) => match token.value {
                Character(c, cat_code) => command::Command::Character(c, cat_code),
                ControlSequence(_, ref name) => match input.base().get_command(name) {
                    None => return Err(error::new_undefined_cs_error(token, input.base())),
                    Some(cmd) => cmd.clone(),
                },
            },
        };
    input.base_mut().set_command(name, command);
    Ok(())
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::tex::command::library::def;
    use crate::tex::driver;
    use crate::tex::input;
    use crate::tex::token::catcode;

    struct State;
    fn new_state() -> State {
        State {}
    }

    fn setup_expansion_test(s: &mut Base<State>) {
        def::add_all_commands(s);
        s.set_command("let", get_let());
    }

    expansion_test![let_for_macro, r"\def\A{abc}\let\B\A\B", "abc"];
    expansion_test![let_for_macro_equals, r"\def\A{abc}\let\B=\A\B", "abc"];
}
