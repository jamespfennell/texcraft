use crate::command;
use crate::error;
use crate::token;
use crate::traits::*;
use crate::variable;
use crate::vm;

impl<S: TexlangState> Parsable<S> for variable::Variable<S> {
    fn parse_impl(input: &mut vm::ExpandedStream<S>) -> anyhow::Result<Self> {
        match input.next()? {
            None => Err(error::EndOfInputError::new(
                "Unexpected end of input while reading in a variable",
            )
            .cast()),
            Some(token) => match token.value() {
                token::Value::ControlSequence(name) => match input.commands_map().get_command(&name)
                {
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
}

/// When parsed, this type consumes an optional equals from the token stream.
pub struct OptionalEquals;

impl<S: TexlangState> Parsable<S> for OptionalEquals {
    fn parse_impl(input: &mut vm::ExpandedStream<S>) -> anyhow::Result<Self> {
        parse_optional_equals(input)?;
        Ok(OptionalEquals {})
    }
}

/// When parsed, this type consumes an optional equals from the token stream without performing expansion.
pub struct OptionalEqualsUnexpanded;

impl<S: TexlangState> Parsable<S> for OptionalEqualsUnexpanded {
    fn parse_impl(input: &mut vm::ExpandedStream<S>) -> anyhow::Result<Self> {
        parse_optional_equals(input.unexpanded())?;
        Ok(OptionalEqualsUnexpanded {})
    }
}

fn parse_optional_equals<S: TexlangState, I: TokenStream<S = S>>(
    input: &mut I,
) -> anyhow::Result<()> {
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
