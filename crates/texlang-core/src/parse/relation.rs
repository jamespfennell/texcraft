//! Parsing of relations (<, = and >)

use crate::prelude::*;

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
pub fn parse_relation<T: stream::Stream>(stream: &mut T) -> anyhow::Result<Relation> {
    get_element![
        stream,
        parse_relation_error,
        Value::Other('<') => Relation::LessThan,
        Value::Other('=') => Relation::Equal,
        Value::Other('>') => Relation::GreaterThan,
    ]
}

fn parse_relation_error(token: Option<Token>) -> anyhow::Error {
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

    macro_rules! relation_success_test {
        ($name: ident, $input: expr, $expected: expr) => {
            #[test]
            fn $name() {
                let mut env = runtime::Env::<()>::new(CatCodeMap::new_with_tex_defaults(), ());
                env.push_source($input.to_string()).unwrap();
                let mut input = runtime::ExecutionInput::new(env);
                let result = parse_relation(input.regular()).unwrap();
                assert_eq![result, $expected];
            }
        };
    }

    relation_success_test![less_than, r"<a", Relation::LessThan];
    relation_success_test![equals, r"=a", Relation::Equal];
    relation_success_test![greater_than, r">a", Relation::GreaterThan];

    macro_rules! relation_failure_test {
        ($name: ident, $input: expr) => {
            #[test]
            fn $name() {
                let mut map = CatCodeMap::new_with_tex_defaults();
                map.insert('<', catcode::CatCode::Letter);
                let mut env = runtime::Env::<()>::new(map, ());
                env.push_source($input.to_string()).unwrap();
                let mut input = runtime::ExecutionInput::new(env);
                let result = parse_relation(input.regular());
                if let Ok(_) = result {
                    panic!["Parsed a relation from invalid input"];
                }
            }
        };
    }

    relation_failure_test![empty_input, ""];
    relation_failure_test![letter, "a"];
    relation_failure_test![control_sequence, r"\A"];
    relation_failure_test![incorrect_catcode, "<"];
}
