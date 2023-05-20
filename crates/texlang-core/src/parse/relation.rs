//! Parsing of relations (<, = and >)

use crate::vm::TokenStream;
use crate::{error, token, vm};

/// Simple enum representing the three types of relation: <, = and >.
#[derive(Debug, PartialEq, Eq)]
pub enum Relation {
    LessThan,
    Equal,
    GreaterThan,
}

/// Parses a `Relation` from the provided stream.
///
/// A relation is defined on p209 of the TeXBook to be a character token with
/// catcode 12 (other) and value <, = or >.
pub fn parse_relation<S, I: AsMut<vm::ExpandedStream<S>>>(
    stream: &mut I,
) -> anyhow::Result<Relation> {
    get_element![
        stream.as_mut(),
        parse_relation_error,
        token::Value::Other('<') => Relation::LessThan,
        token::Value::Other('=') => Relation::Equal,
        token::Value::Other('>') => Relation::GreaterThan,
    ]
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
    use crate::token::catcode;
    use std::collections::HashMap;

    macro_rules! relation_success_tests {
        ($( ($name: ident, $input: expr, $expected: expr), )+) => {
            $(
            #[test]
            fn $name() {
                let mut vm = vm::VM::<()>::new(
                    catcode::CatCodeMap::new_with_tex_defaults(),
                    HashMap::new(),
                    (),
                    Default::default(),
                );
                vm.push_source("".to_string(), $input.to_string()).unwrap();
                let input = vm::ExecutionInput::new(&mut vm);
                let result = parse_relation(input).unwrap();
                assert_eq![result, $expected];
            }
            )+
        };
    }

    relation_success_tests![
        (less_than, r"<a", Relation::LessThan),
        (equals, r"=a", Relation::Equal),
        (greater_than, r">a", Relation::GreaterThan),
    ];

    macro_rules! relation_failure_tests {
        ($( ($name: ident, $input: expr), )+) => {
            $(
            #[test]
            fn $name() {
                let mut map = catcode::CatCodeMap::new_with_tex_defaults();
                map.insert('<', catcode::CatCode::Letter);
                let mut vm = vm::VM::<()>::new(map, HashMap::new(), (), Default::default());
                vm.push_source("".to_string(), $input.to_string()).unwrap();
                let input = vm::ExecutionInput::new(&mut vm);
                let result = parse_relation(input);
                if let Ok(_) = result {
                    panic!["Parsed a relation from invalid input"];
                }
            }
            )+
        };
    }

    relation_failure_tests![
        (empty_input, ""),
        (letter, "a"),
        (control_sequence, r"\A"),
        (incorrect_catcode, "<"),
    ];
}
