//! Logic for parsing elements of the TeX grammer from token streams.

#[macro_use]
mod helpers;

mod keyword;
mod number;
mod relation;
mod variable;

pub use keyword::parse_optional_by;
pub use number::parse_number;
pub use relation::parse_relation;
pub use relation::Relation;
pub use variable::parse_optional_equals;
pub use variable::parse_variable;

use crate::prelude::*;

use super::token::CsName;

pub fn parse_optional_space<S>(input: &mut runtime::ExpandedInput<S>) -> anyhow::Result<()> {
    get_optional_element![input, Value::Space(_) => (),];
    Ok(())
}

// TODO: take a &token instead
// TODO: just take execution input as input
pub fn parse_command_target<S: stream::Stream>(
    target_description: &str,
    token: Token,
    input: &mut S,
) -> anyhow::Result<CsName> {
    Ok(match input.next()? {
        None => {
            return Err(error::TokenError::new(
                token,
                format![
                    "unexpected end of input while reading the target of a {}",
                    target_description
                ],
            )
            .cast());
        }
        Some(token) => match token.value() {
            ControlSequence(name) => name,
            _ => {
                return Err(error::TokenError::new(
                    token,
                    format!["unexpected target of a {}", target_description],
                )
                .add_note(format![
                    "the target of a {} must be a control sequence",
                    target_description
                ])
                .cast());
            }
        },
    })
}

/// Parses balanced tokens from the stream.
///
/// Returns false if the input ended before balanced tokens completed.
pub fn parse_balanced_tokens<S: stream::Stream>(
    stream: &mut S,
    result: &mut Vec<Token>,
) -> anyhow::Result<bool> {
    let mut scope_depth = 0;
    while let Some(token) = stream.next()? {
        match token.value() {
            Value::BeginGroup(_) => {
                scope_depth += 1;
            }
            Value::EndGroup(_) => {
                if scope_depth == 0 {
                    return Ok(true);
                }
                scope_depth -= 1;
            }
            _ => (),
        }
        result.push(token);
    }
    Ok(false)
}
