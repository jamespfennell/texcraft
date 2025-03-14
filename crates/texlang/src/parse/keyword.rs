use crate::prelude as txl;
use crate::token;
use crate::traits::*;
use crate::vm;

/// Parses a keyword from the input stream.
///
/// TeX.2021.407 scan_keyword
pub fn parse_keyword<S: TexlangState>(
    input: &mut vm::ExpandedStream<S>,
    keyword: &str,
) -> txl::Result<bool> {
    let Some(c) = keyword.chars().next() else {
        // keyword is empty
        return Ok(true);
    };
    let Some(token) = input.next()? else {
        // input ended, keyword does not match
        return Ok(false);
    };
    if token.value() != token::Value::Letter(c.to_ascii_lowercase())
        && token.value() != token::Value::Letter(c.to_ascii_uppercase())
    {
        input.back(token);
        return Ok(false);
    }
    // this character matched; now try to match the result of keyword
    let result = parse_keyword(input, &keyword[c.len_utf8()..]);
    if let Ok(false) = result {
        // some later character did not match, reverse consuming the token.
        input.back(token);
    }
    result
}
