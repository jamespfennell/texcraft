//! Parsing of relations (<, = and >)

use crate::tex::prelude::*;

/// Simple enum representing the three types of relation: <, = and >.
#[derive(Debug, PartialEq, Eq)]
pub enum Relation {
    LessThan,
    Equal,
    GreaterThan,
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

/// Parses a `Relation` from the provided stream.
///
/// A relation is defined on p209 of the TeXBook to be a character token with
/// catcode 12 (other) and value <, = or >.
pub fn parse_relation(stream: &mut dyn stream::Stream) -> anyhow::Result<Relation> {
    get_element![
        stream,
        parse_relation_error,
        Character('<', CatCode::Other) => Relation::LessThan,
        Character('=', CatCode::Other) => Relation::Equal,
        Character('>', CatCode::Other) => Relation::GreaterThan,
    ]
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tex::testutil;
    use crate::tex::token::catcode;

    #[test]
    fn less_than() {
        let mut input = testutil::tokenize(r"<a");
        let result = parse_relation(&mut input).unwrap();
        assert_eq![result, Relation::LessThan];
        assert_eq![testutil::length(&mut input), 1];
    }

    #[test]
    fn equals() {
        let mut input = testutil::tokenize(r"=a");
        let result = parse_relation(&mut input).unwrap();
        assert_eq![result, Relation::Equal];
        assert_eq![testutil::length(&mut input), 1];
    }

    #[test]
    fn greater_than() {
        let mut input = testutil::tokenize(r">a");
        let result = parse_relation(&mut input).unwrap();
        assert_eq![result, Relation::GreaterThan];
        assert_eq![testutil::length(&mut input), 1];
    }

    #[test]
    fn invalid_inputs() {
        let mut map = catcode::tex_defaults();
        map.insert(
            '<' as u32,
            catcode::RawCatCode::Regular(catcode::CatCode::Letter),
        );
        let inputs = vec![r"", r"a", r"\A", r"<"];
        for input in inputs.iter() {
            let mut stream = testutil::tokenize_with_map(input, &map);
            let result = parse_relation(&mut stream);
            if let Ok(_) = result {
                panic!["Parsed a relation from invalid input"];
            }
        }
    }
}