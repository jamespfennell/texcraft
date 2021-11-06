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
    use crate::testutil;
    use crate::token::catcode;

    #[test]
    fn less_than() {
        let mut input = driver::ExecutionInput::new_with_str(
            Base::<()>::new(CatCodeMap::new_with_tex_defaults(), ()),
            r"<a",
        );
        let result = parse_relation(input.regular()).unwrap();
        assert_eq![result, Relation::LessThan];
        assert_eq![testutil::length(input.regular()), 1];
    }

    #[test]
    fn equals() {
        let mut input = driver::ExecutionInput::new_with_str(
            Base::<()>::new(CatCodeMap::new_with_tex_defaults(), ()),
            r"=a",
        );
        let result = parse_relation(input.regular()).unwrap();
        assert_eq![result, Relation::Equal];
        assert_eq![testutil::length(input.regular()), 1];
    }

    #[test]
    fn greater_than() {
        let mut input = driver::ExecutionInput::new_with_str(
            Base::<()>::new(CatCodeMap::new_with_tex_defaults(), ()),
            r">a",
        );
        let result = parse_relation(input.regular()).unwrap();
        assert_eq![result, Relation::GreaterThan];
        assert_eq![testutil::length(input.regular()), 1];
    }

    #[test]
    fn invalid_inputs() {
        let inputs = vec![r"", r"a", r"\A", r"<"];
        for str in inputs {
            let mut map = CatCodeMap::new_with_tex_defaults();
            map.insert('<', catcode::CatCode::Letter);
            let mut input = driver::ExecutionInput::new_with_str(Base::<()>::new(map, ()), str);
            let result = parse_relation(input.regular());
            if let Ok(_) = result {
                panic!["Parsed a relation from invalid input"];
            }
        }
    }
}
