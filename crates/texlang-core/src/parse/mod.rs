//! Logic for parsing elements of the TeX grammar from token streams.
//! 
//! This parsing module is based around the [Parsable] trait.
//! This trait is implemented by Rust types that correspond to elements of the TeX grammar.
//! The trait implementation provides a way to parse grammar elements out of the input stream.

#[macro_use]
mod helpers;

mod filelocation;
mod keyword;
mod number;
mod relation;
#[cfg(test)]
mod testing;
mod variable;

pub use filelocation::{parse_file_location, FileLocation};
pub use keyword::parse_optional_by;
pub use number::parse_catcode;
pub use number::parse_number;
pub use variable::parse_optional_equals;
pub use variable::parse_variable;

use crate::error;
use crate::token;
use crate::traits::*;
use crate::vm;

/// Implementations of this trait are elements of the TeX grammar than can be parsed from a stream of tokens.
pub trait Parsable: Sized {
    /// Parses a value from an input stream.
    ///
    /// This method just delegates to [Parsable::parse_impl].
    #[inline]
    fn parse<S, I>(input: &mut I) -> anyhow::Result<Self>
    where
        S: TexlangState,
        I: AsMut<vm::ExpandedStream<S>>,
    {
        Parsable::parse_impl(input.as_mut())
    }

    /// Parses a value from the [vm::ExpandedStream].
    fn parse_impl<S: TexlangState>(input: &mut vm::ExpandedStream<S>) -> anyhow::Result<Self>;
}

// TODO: just take execution input as input - this shouldn't be called with expanded input!!!!!!!
// TODO: destroy the target description?
pub fn parse_command_target<S: vm::TokenStream>(
    target_description: &str,
    token: token::Token,
    input: &mut S,
) -> anyhow::Result<token::CsName> {
    Ok(match input.next()? {
        None => {
            return Err(error::TokenError::new(
                token,
                format![
                    "unexpected end of input while reading the target of a {target_description}"
                ],
            )
            .cast());
        }
        Some(token) => match token.value() {
            token::Value::ControlSequence(name) => name,
            _ => {
                return Err(error::TokenError::new(
                    token,
                    format!["unexpected target of a {target_description}"],
                )
                .add_note(format![
                    "the target of a {target_description} must be a control sequence"
                ])
                .cast());
            }
        },
    })
}

/// Parses balanced tokens from the stream.
///
/// Returns false if the input ended before balanced tokens completed.
pub fn parse_balanced_tokens<S: vm::TokenStream>(
    stream: &mut S,
    result: &mut Vec<token::Token>,
) -> anyhow::Result<bool> {
    let mut scope_depth = 0;
    while let Some(token) = stream.next()? {
        match token.value() {
            token::Value::BeginGroup(_) => {
                scope_depth += 1;
            }
            token::Value::EndGroup(_) => {
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
