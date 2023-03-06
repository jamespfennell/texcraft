use crate::prelude::*;
use crate::variable;
use crate::vm::RefVM;

/// Parses a variable.
pub fn parse_variable<S, I: AsMut<vm::ExpansionInput<S>>>(
    input: &mut I,
) -> anyhow::Result<variable::Variable<S>> {
    let input = input.as_mut();
    match input.next()? {
        None => Err(error::EndOfInputError::new(
            "Unexpected end of input while reading in a variable",
        )
        .cast()),
        Some(token) => match token.value() {
            ControlSequence(name) => match input.base().commands_map.get_fn(&name) {
                None => Err(error::TokenError::new(token, "Undefined control sequence").cast()),
                Some(command::Fn::Variable(cmd)) => cmd.clone().resolve(token, input),
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
pub fn parse_optional_equals<T: TokenStream>(input: &mut T) -> anyhow::Result<()> {
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
