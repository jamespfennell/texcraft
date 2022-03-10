use std::char;
use std::convert::TryInto;
/// The `\the` expansion primitive.
use texlang_core::prelude::*;
use texlang_core::variable;

pub const THE_DOC: &str = "Output text describing some inputted tokens";

/// Get the `\the` expansion primitive.
pub fn get_the<S>() -> command::ExpansionFn<S> {
    the_primitive_fn
}

fn the_primitive_fn<S>(
    the_token: Token,
    input: &mut runtime::ExpansionInput<S>,
) -> anyhow::Result<Vec<Token>> {
    let token = match input.next()? {
        None => {
            return Err(error::EndOfInputError::new("").cast());
        }
        Some(token) => token,
    };
    Ok(match &token.value() {
        ControlSequence(name) => {
            if let Some(command::Command::Variable(cmd_ref)) = input.base().commands_map.get(name) {
                let cmd = *cmd_ref;
                let variable = cmd.resolve(token, input)?;
                match variable {
                    variable::Variable::Int(variable) => {
                        let value = *variable.get(input.state());
                        return Ok(int_to_tokens(the_token, value));
                    }
                    variable::Variable::BaseInt(variable) => {
                        let value = *variable.get(input.base());
                        return Ok(int_to_tokens(the_token, value));
                    }
                    variable::Variable::CatCode(v) => {
                        let val = (*v.get(input.base())).int();
                        return Ok(int_to_tokens(the_token, val.into()));
                    }
                }
            }
            vec![token]
        }
        _ => vec![token],
    })
}

fn int_to_tokens(the_token: Token, mut i: i32) -> Vec<Token> {
    if i == 0 {
        return vec![Token::new_other('0', the_token.traceback_id())];
    }
    let negative = i < 0;
    // TODO: allocate the capacity precisely?
    let mut tokens = Vec::new();
    while i != 0 {
        let digit = (i % 10).abs();
        tokens.push(Token::new_other(
            char::from_digit(digit.try_into().unwrap(), 10).unwrap(),
            the_token.traceback_id(),
        ));
        i /= 10;
    }
    if negative {
        tokens.push(Token::new_other('-', the_token.traceback_id()));
    }
    tokens.reverse();
    tokens
}
