//! The `\the` primitive

use std::char;
use std::convert::TryInto;
use texlang::traits::*;
use texlang::*;

pub const THE_DOC: &str = "Output text describing some inputted tokens";

/// Get the `\the` expansion primitive.
pub fn get_the<S: TexlangState>() -> command::BuiltIn<S> {
    command::BuiltIn::new_expansion(the_primitive_fn)
}

fn the_primitive_fn<S: TexlangState>(
    the_token: token::Token,
    input: &mut vm::ExpansionInput<S>,
) -> command::Result<()> {
    let token = match input.next()? {
        None => return Err(error::SimpleEndOfInputError::new(input.vm(), "TODO").into()),
        Some(token) => token,
    };
    match &token.value() {
        token::Value::CommandRef(command_ref) => {
            match input.commands_map().get_command(command_ref) {
                Some(command::Command::Variable(cmd)) => {
                    match cmd.clone().value(token, input.as_mut())? {
                        variable::ValueRef::Int(i) => {
                            let i = *i;
                            int_to_tokens(input.expansions_mut(), the_token, i);
                        }
                        variable::ValueRef::CatCode(i) => {
                            let i = *i;
                            int_to_tokens(input.expansions_mut(), the_token, (i as u8).into());
                        }
                    };
                }
                Some(command::Command::Character(c)) => {
                    let c = *c;
                    int_to_tokens(
                        input.expansions_mut(),
                        the_token,
                        (c as u32).try_into().unwrap(),
                    );
                }
                None
                | Some(
                    command::Command::Expansion(..)
                    | command::Command::Macro(..)
                    | command::Command::Execution(..)
                    | command::Command::CharacterTokenAlias(..),
                ) => {
                    todo!("should return an error")
                }
            }
        }
        _ => todo!("should return an error"),
    };
    Ok(())
}

fn int_to_tokens(tokens: &mut Vec<token::Token>, the_token: token::Token, mut i: i32) {
    if i == 0 {
        tokens.push(token::Token::new_other('0', the_token.trace_key()));
        return;
    }
    let negative = i < 0;
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
}
