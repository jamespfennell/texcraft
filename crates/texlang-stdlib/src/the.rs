//! The `\the` primitive

use std::char;
use std::convert::TryInto;
use texlang_core::traits::*;
use texlang_core::*;

pub const THE_DOC: &str = "Output text describing some inputted tokens";

/// Get the `\the` expansion primitive.
pub fn get_the<S>() -> command::BuiltIn<S> {
    command::BuiltIn::new_expansion(the_primitive_fn)
}

fn the_primitive_fn<S>(
    the_token: token::Token,
    input: &mut vm::ExpansionInput<S>,
) -> anyhow::Result<Vec<token::Token>> {
    // TODO: double check \the expands the input
    let token = match input.next()? {
        None => {
            return Err(error::EndOfInputError::new("").cast());
        }
        Some(token) => token,
    };
    Ok(match &token.value() {
        token::Value::ControlSequence(name) => {
            if let Some(command::Command::Variable(cmd)) =
                input.base().commands_map.get_command(name)
            {
                match cmd.clone().value(the_token, input.as_mut())? {
                    variable::ValueRef::Int(i) => int_to_tokens(the_token, *i),
                    variable::ValueRef::CatCode(i) => int_to_tokens(the_token, (*i as u8).into()),
                }
            } else {
                // TODO: push straight onto the expansions stack?
                vec![token]
            }
        }
        // TODO: push straight onto the expansions stack?
        _ => vec![token],
    })
}

fn int_to_tokens(the_token: token::Token, mut i: i32) -> Vec<token::Token> {
    if i == 0 {
        return vec![token::Token::new_other('0', the_token.trace_key())];
    }
    let negative = i < 0;
    // TODO: allocate the capacity precisely?
    // Even better: can push straight onto the expansions stack?
    let mut tokens = Vec::new();
    while i != 0 {
        let digit = (i % 10).abs();
        tokens.push(token::Token::new_other(
            char::from_digit(digit.try_into().unwrap(), 10).unwrap(),
            the_token.trace_key(),
        ));
        i /= 10;
    }
    if negative {
        tokens.push(token::Token::new_other('-', the_token.trace_key()));
    }
    tokens.reverse();
    tokens
}
