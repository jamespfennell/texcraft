use crate::tex::prelude::*;
use crate::tex::variable;

/// Parses a variable.
pub fn parse_variable<S>(
    input: &mut command::ExpandedInput<S>,
) -> anyhow::Result<variable::Variable<S>> {
    match input.next()? {
        None => Err(error::EndOfInputError::new(
            "Unexpected end of input while reading in a variable",
        )
        .cast()),
        Some(token) => match token.value() {
            ControlSequence(name) => match input.base().get_command(&name) {
                None => Err(error::TokenError::new(token, "Undefined control sequence").cast()),
                Some(&command::Command::Variable(cmd)) => cmd.variable(&token, input),
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
pub fn parse_optional_equals<T: Stream>(input: &mut T) -> anyhow::Result<()> {
    while let Some(found_equals) = get_optional_element![
        input,
        Value::Other('=') => false,
        Value::Space(_) => true,
    ] {
        if found_equals {
            break;
        }
    }
    while get_optional_element![
        input,
        Value::Space(_) => (),
    ]
    .is_some()
    {}
    Ok(())
}
