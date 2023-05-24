//! Parsing of relations (<, = and >)
//!
//! A relation is defined on p209 of the TeXBook to be a character token with
//! catcode 12 (other) and value <, = or >.

use crate::traits::*;
use crate::{error, token, vm};
use std::cmp::Ordering;

impl<S: TexlangState> Parsable<S> for Ordering {
    fn parse_impl(input: &mut vm::ExpandedStream<S>) -> anyhow::Result<Self> {
        get_element![
            input,
            parse_relation_error,
            token::Value::Other('<') => Ordering::Less,
            token::Value::Other('=') => Ordering::Equal,
            token::Value::Other('>') => Ordering::Greater,
        ]
    }
}

fn parse_relation_error(token: Option<token::Token>) -> anyhow::Error {
    match token {
      None => error::EndOfInputError::new(
          "unexpected end of input while parsing a relation").cast(),
      Some(token) => error::TokenError::new(
          token,
          "unexpected control sequence while parsing a relation")
          .add_note("a relation must be a character token of catcode 12 (other) and value '<', '=', or '<'.")
          .cast(),
  }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::testing::*;

    parse_success_tests![
        (less_than, r"<a", Ordering::Less),
        (equals, r"=a", Ordering::Equal),
        (greater_than, r">a", Ordering::Greater),
    ];

    #[derive(Default)]
    struct State;

    impl TexlangState for State {
        fn cat_code(&self, c: char) -> token::CatCode {
            if c == '<' {
                return token::CatCode::Letter;
            }
            token::CatCode::PLAIN_TEX_DEFAULTS
                .get(c as usize)
                .copied()
                .unwrap_or_default()
        }
    }

    parse_failure_tests![
        Ordering,
        State,
        (empty_input, ""),
        (letter, "a"),
        (control_sequence, r"\A"),
        (incorrect_catcode, "<"),
    ];
}
