//! Parsing of relations (<, = and >)
//!
//! A relation is defined on p209 of the TeXBook to be a character token with
//! catcode 12 (other) and value <, = or >.

use crate::prelude as txl;
use crate::traits::*;
use crate::{token, types, vm};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ordering(pub std::cmp::Ordering);

impl Default for Ordering {
    fn default() -> Self {
        Ordering(std::cmp::Ordering::Equal)
    }
}

impl Parsable for Ordering {
    fn parse_impl<S: TexlangState>(input: &mut vm::ExpandedStream<S>) -> txl::Result<Self> {
        let ordering_or = get_required_element![
            input,
            "a relation",
            format!["a relation is a token with character code {} and one of the following values: <, =, >", types::CatCode::Other],
            token::Value::Other('<') => Ordering(std::cmp::Ordering::Less),
            token::Value::Other('=') => Ordering(std::cmp::Ordering::Equal),
            token::Value::Other('>') => Ordering(std::cmp::Ordering::Greater),
        ];
        Ok(ordering_or.unwrap_or_default())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::testing::*;

    parse_success_tests![
        (less_than, r"<a", Ordering(std::cmp::Ordering::Less)),
        (equals, r"=a", Ordering(std::cmp::Ordering::Equal)),
        (greater_than, r">a", Ordering(std::cmp::Ordering::Greater)),
    ];

    #[derive(Default)]
    struct State;

    impl TexlangState for State {
        fn cat_code(&self, c: char) -> types::CatCode {
            if c == '<' {
                return types::CatCode::Letter;
            }
            types::CatCode::PLAIN_TEX_DEFAULTS
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
