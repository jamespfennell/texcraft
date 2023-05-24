//! Logic for parsing elements of the TeX grammar from token streams.
//!
//! This parsing module is based around the [Parsable] trait, which is the most important type in the module.
//! This trait is implemented by Rust types that correspond to elements of the TeX grammar.
//! The trait implementation provides a way to parse grammar elements out of the input stream.
//!
//! The module contains implementations of [Parsable] for tuples where each element is parsable.
//! This allows expressions like `<integer><relation><integer>` to be parsed by one invocation
//!     of [Parsable::parse], in this case on the type `(i32, std::cmp::Ordering, i32)`.
//!
//! The second most important thing is the collection of custom Rust types like [OptionalBy] and
//!     [FileLocation] which correspond to Rust grammar elements.
//!
//! Finally this module contains some functions for special situation like parsing lists of tokens.

#[macro_use]
mod helpers;

mod filelocation;
mod keyword;
mod number;
mod relation;
#[cfg(test)]
mod testing;
mod variable;

pub use filelocation::FileLocation;
pub use keyword::OptionalBy;
pub use variable::OptionalEquals;
pub use variable::OptionalEqualsUnexpanded;

use crate::error;
use crate::token;
use crate::traits::*;
use crate::vm;

/// Implementations of this trait are elements of the TeX grammar than can be parsed from a stream of tokens.
pub trait Parsable<S: TexlangState>: Sized {
    /// Parses a value from an input stream.
    ///
    /// This method just delegates to [Parsable::parse_impl].
    #[inline]
    fn parse<I>(input: &mut I) -> anyhow::Result<Self>
    where
        I: AsMut<vm::ExpandedStream<S>>,
    {
        Parsable::parse_impl(input.as_mut())
    }

    /// Parses a value from the [vm::ExpandedStream].
    fn parse_impl(input: &mut vm::ExpandedStream<S>) -> anyhow::Result<Self>;
}

macro_rules! generate_tuple_impls {
    ( $first: ident ) => {};
    ( $first: ident, $( $name: ident ),+ ) => {
        generate_tuple_impls![ $( $name ),+];

        impl<S: TexlangState, $first : Parsable<S>, $( $name : Parsable<S> ),+> Parsable<S> for ($first, $( $name ),+) {
            fn parse_impl(input: &mut vm::ExpandedStream<S>) -> anyhow::Result<Self> {
                Ok(($first::parse(input)?, $( $name::parse(input)? ),+))
            }
        }
    };
}

generate_tuple_impls![T1, T2, T3, T4, T5];

/// The target of a command like `\let` or `\def`.
pub enum CommandTarget {
    ControlSequence(token::CsName),
}

impl<S: TexlangState> Parsable<S> for CommandTarget {
    fn parse_impl(input: &mut vm::ExpandedStream<S>) -> anyhow::Result<Self> {
        match input.unexpanded().next()? {
            None => Err(error::EndOfInputError::new(
                "unexpected end of input while reading a command target".to_string(),
            )
            .cast()),
            Some(token) => match token.value() {
                token::Value::ControlSequence(name) => Ok(CommandTarget::ControlSequence(name)),
                _ => Err(
                    error::TokenError::new(token, "unexpected command target".to_string())
                        .add_note("a command target must be a control sequence".to_string())
                        .cast(),
                ),
            },
        }
    }
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
