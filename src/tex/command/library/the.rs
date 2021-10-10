use crate::tex::prelude::*;
use crate::tex::variable;
use std::char;
use std::convert::TryInto;

const THE_DOC: &str = "Output text describing some inputted tokens";

fn the_primitive_fn<S>(
    _the_token: Token,
    input: &mut ExpandedInput<S>,
) -> anyhow::Result<Vec<Token>> {
    let token = match input.next()? {
        None => {
            return Err(error::EndOfInputError::new("").cast());
        }
        Some(token) => token,
    };
    Ok(match &token.value() {
        Character(_, _) => new_singleton(token),
        ControlSequence(_, name) => {
            if let Some(command::Command::Variable(cmd_ref)) = input.base().get_command(name) {
                let cmd = *cmd_ref;
                let variable = cmd.variable(&token, input)?;
                match variable {
                    variable::Variable::Int(variable) => {
                        let value = *variable.get(input.state());
                        return Ok(int_to_tokens(value));
                    }
                    variable::Variable::BaseInt(variable) => {
                        let value = *variable.get(input.base());
                        return Ok(int_to_tokens(value));
                    }
                    variable::Variable::CatCode(v) => {
                        let val = (*v.get(input.base())).int();
                        return Ok(int_to_tokens(val.into()));
                    }
                }
            }
            new_singleton(token)
        }
    })
}

pub fn get_the<S>() -> command::ExpansionPrimitive<S> {
    command::ExpansionPrimitive {
        call_fn: the_primitive_fn,
        docs: THE_DOC,
        id: None,
    }
}

fn int_to_tokens(mut i: i32) -> Vec<Token> {
    if i == 0 {
        return new_singleton(Token::new_character('0', CatCode::Other));
    }
    let negative = i < 0;
    // TODO: allocate the capacity precisely?
    let mut tokens = Vec::new();
    while i != 0 {
        let digit = (i % 10).abs();
        tokens.push(Token::new_character(
            char::from_digit(digit.try_into().unwrap(), 10).unwrap(),
            CatCode::Other,
        ));
        i /= 10;
    }
    if negative {
        tokens.push(Token::new_character('-', CatCode::Other));
    }
    tokens.reverse();
    tokens
}

fn new_singleton(token: Token) -> Vec<Token> {
    vec![token]
}
