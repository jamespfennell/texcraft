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
pub use keyword::To;
pub use number::Uint;
pub use variable::ArithmeticVariable;
pub use variable::OptionalEquals;
pub use variable::OptionalEqualsUnexpanded;

use crate::prelude as txl;
use crate::traits::*;
use crate::*;

/// Implementations of this trait are elements of the TeX grammar than can be parsed from a stream of tokens.
pub trait Parsable<S: TexlangState>: Sized {
    /// Parses a value from an input stream.
    ///
    /// This method just delegates to [Parsable::parse_impl].
    #[inline]
    fn parse<I>(input: &mut I) -> txl::Result<Self>
    where
        I: AsMut<vm::ExpandedStream<S>>,
    {
        Parsable::parse_impl(input.as_mut())
    }

    /// Parses a value from the [vm::ExpandedStream].
    fn parse_impl(input: &mut vm::ExpandedStream<S>) -> txl::Result<Self>;
}

#[derive(Debug)]
pub struct Error {
    pub expected: String,
    pub got: Option<token::Token>,
    pub got_override: String,
    pub annotation_override: String,
    pub guidance: String,
    pub additional_notes: Vec<String>,
}

impl error::TexError for Error {
    fn kind(&self) -> error::Kind {
        match self.got {
            None => error::Kind::EndOfInput,
            Some(token) => error::Kind::Token(token),
        }
    }

    fn title(&self) -> String {
        let got = if self.got_override.is_empty() {
            match self.got {
                None => "the input ended".to_string(),
                Some(token) => match token.value() {
                    token::Value::Letter(c) => format!["found the letter {c}"],
                    token::Value::Other(c) => format!["found a non-letter character {c}"],
                    _ => match (token.char(), token.cat_code()) {
                        (Some(c), Some(code)) => {
                            format!["found a token with value {c} and category code {code}"]
                        }
                        _ => "found a control sequence".to_string(),
                    },
                },
            }
        } else {
            self.got_override.clone()
        };
        format!["expected {}, instead {}", self.expected, got]
    }

    fn notes(&self) -> Vec<error::display::Note> {
        vec![self.guidance.clone().into()]
    }

    fn source_annotation(&self) -> String {
        if !self.annotation_override.is_empty() {
            return self.annotation_override.clone();
        }
        error::TexError::default_source_annotation(self)
    }
}

impl Error {
    pub fn new<T: Into<String>, R: Into<String>>(
        expected: T,
        got: Option<token::Token>,
        guidance: R,
    ) -> Self {
        Error {
            expected: expected.into(),
            got,
            got_override: "".into(),
            annotation_override: "".into(),
            guidance: guidance.into(),
            additional_notes: vec![],
        }
    }

    pub fn with_got_override<T: Into<String>>(mut self, got_override: T) -> Self {
        self.got_override = got_override.into();
        self
    }

    pub fn with_annotation_override<T: Into<String>>(mut self, annotation_override: T) -> Self {
        self.annotation_override = annotation_override.into();
        self
    }
}

macro_rules! generate_tuple_impls {
    ( $first: ident ) => {};
    ( $first: ident, $( $name: ident ),+ ) => {
        generate_tuple_impls![ $( $name ),+];

        impl<S: TexlangState, $first : Parsable<S>, $( $name : Parsable<S> ),+> Parsable<S> for ($first, $( $name ),+) {
            fn parse_impl(input: &mut vm::ExpandedStream<S>) -> txl::Result<Self> {
                Ok(($first::parse(input)?, $( $name::parse(input)? ),+))
            }
        }
    };
}

generate_tuple_impls![T1, T2, T3, T4, T5];

impl<S: TexlangState> Parsable<S> for token::CommandRef {
    fn parse_impl(input: &mut vm::ExpandedStream<S>) -> txl::Result<Self> {
        while let Some(found_equals) = get_optional_element![
            input.unexpanded(),
            token::Value::Space(_) => true,
        ] {
            if found_equals {
                break;
            }
        }
        get_required_element![
            input.unexpanded(),
            "a control sequence or active character",
            "a command must be a control sequence or an active character",
            token::Value::CommandRef(command_ref) => command_ref,
        ]
    }
}

impl<S: TexlangState> Parsable<S> for Vec<token::Token> {
    fn parse_impl(input: &mut vm::ExpandedStream<S>) -> txl::Result<Self> {
        let mut result = input.checkout_token_buffer();
        let first_token_or = input.next()?;
        let got = match first_token_or {
            None => "the input ended",
            Some(first_token) => match first_token.value() {
                token::Value::CommandRef(command_ref) => {
                    match input.commands_map().get_command(&command_ref) {
                        Some(command::Command::Variable(cmd)) => {
                            if let crate::variable::ValueRef::TokenList(token_list) =
                                cmd.clone().value(first_token, input)?
                            {
                                result.extend(token_list.iter());
                                return Ok(result);
                            };
                            "a variable command of the wrong type (wanted a token list)"
                        }
                        Some(_) => "a command that is not a variable command",
                        None => "an undefined command",
                    }
                }
                token::Value::BeginGroup(_) => {
                    let found_end_group = finish_parsing_balanced_tokens(input, &mut result)?;
                    if found_end_group {
                        return Ok(result);
                    }
                    input.return_token_buffer(result);
                    return Err(input.vm().fatal_error(parse::Error::new(
                        "balanced braces",
                        None,
                        "",
                    )));
                }
                _ => "a non-command, non-opening brace token",
            },
        };
        input.return_token_buffer(result);
        Err(input.vm().fatal_error(
            parse::Error::new(
                "an opening brace or a variable of type token list",
                first_token_or,
                "",
            )
            .with_got_override(format!("got {got}"))
            .with_annotation_override(got),
        ))
    }
}

/// Parses balanced tokens from the stream.
///
/// This function assumes the the initial opening brace has ready been consumed.
/// It returns false if the input ends before balanced tokens completed.
///
/// This function is analogous to `scan_toks(true, true)` in Knuth's TeX.
pub fn finish_parsing_balanced_tokens<S: vm::TokenStream>(
    stream: &mut S,
    result: &mut Vec<token::Token>,
) -> txl::Result<bool> {
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

/// When parsed, this type consumes an arbitrary number of spaces from the unexpanded input stream
pub struct SpacesUnexpanded;

impl<S: TexlangState> Parsable<S> for SpacesUnexpanded {
    fn parse_impl(input: &mut vm::ExpandedStream<S>) -> txl::Result<Self> {
        let input = input.unexpanded();
        while let Some(token) = input.peek()? {
            match token.value() {
                token::Value::Space(_) => {
                    input.consume()?;
                    continue;
                }
                _ => break,
            }
        }
        Ok(SpacesUnexpanded {})
    }
}

impl<S: TexlangState> Parsable<S> for Option<char> {
    fn parse_impl(input: &mut vm::ExpandedStream<S>) -> txl::Result<Self> {
        let Some(token) = input.peek()? else {
            return Ok(None);
        };
        let c = match token.value() {
            token::Value::BeginGroup(_)
            | token::Value::EndGroup(_)
            | token::Value::MathShift(_)
            | token::Value::AlignmentTab(_)
            | token::Value::Parameter(_)
            | token::Value::Superscript(_)
            | token::Value::Subscript(_)
            | token::Value::Space(_) => return Ok(None),
            token::Value::Letter(c) => c,
            token::Value::Other(c) => c,
            token::Value::CommandRef(command_ref) => {
                match input.commands_map().get_command(&command_ref) {
                    Some(command::Command::Character(c)) => *c,
                    _ => return Ok(None),
                }
            }
        };
        input.consume()?;
        Ok(Some(c))
    }
}
