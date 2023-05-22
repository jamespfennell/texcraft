use crate::command;
use crate::error;
use crate::token;
use crate::traits::*;
use crate::variable;
use crate::vm;

/// Parses a variable.
pub fn parse_variable<S: TexlangState, I: AsMut<vm::ExpandedStream<S>>>(
    input: &mut I,
) -> anyhow::Result<variable::Variable<S>> {
    let input = input.as_mut();
    match input.next()? {
        None => Err(error::EndOfInputError::new(
            "Unexpected end of input while reading in a variable",
        )
        .cast()),
        Some(token) => match token.value() {
            token::Value::ControlSequence(name) => match input.commands_map().get_command(&name) {
                None => Err(error::TokenError::new(token, "Undefined control sequence").cast()),
                Some(command::Command::Variable(cmd)) => cmd.clone().resolve(token, input),
                Some(_) => Err(error::TokenError::new(
                    token,
                    "Expected variable command (register or parameter); found something else",
                )
                .cast()),
            },
            _ => Err(error::TokenError::new(
                token,
                "Unexpected character token while reading in a variable",
            )
            .cast()),
        },
    }
}

/// Parses an optional equal signs, and optional spaces on either side.
pub fn parse_optional_equals<T: vm::TokenStream>(input: &mut T) -> anyhow::Result<()> {
    while let Some(found_equals) = get_optional_element![
        input,
        token::Value::Other('=') => false,
        token::Value::Space(_) => true,
    ] {
        if found_equals {
            break;
        }
    }
    while get_optional_element![
        input,
        token::Value::Space(_) => (),
    ]
    .is_some()
    {}
    Ok(())
}
