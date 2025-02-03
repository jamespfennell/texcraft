use crate::prelude as txl;
use crate::token;
use crate::traits::*;
use crate::vm;

/// Parses a keyword from the input stream.
/// 
/// scan_keyword
pub fn parse_keyword<S: TexlangState>(
    input: &mut vm::ExpandedStream<S>,
    keyword: &str,
) -> txl::Result<bool> {
    let Some(c) = keyword.chars().next() else {
        // keyword is empty
        return Ok(true);
    };
    let Some(token) = input.peek()?.copied() else {
        // input ended, keywork does not match
        return Ok(false);
    };
    if token.value() != token::Value::Letter(c.to_ascii_lowercase())
        && token.value() != token::Value::Letter(c.to_ascii_uppercase())
    {
        return Ok(false);
    }
    // this character matched; now try to match the result of keyword
    input.consume()?;
    let result = parse_keyword(input, &keyword[c.len_utf8()..]);
    if let Ok(false) = result {
        // some later character did not match, reverse consuming the token.
        input.expansions_mut().push(token);
    }
    result
}
