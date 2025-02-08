//! The `\the` primitive

use std::char;
use texlang::prelude as txl;
use texlang::traits::*;
use texlang::*;

pub const THE_DOC: &str = "Output text describing some inputted tokens";

/// Trait satisfied by states that can be used with the control sequence `\the`
pub trait TheCompatible: TexlangState {
    fn get_command_ref_for_font(&self, font: types::Font) -> Option<token::CommandRef> {
        _ = font;
        None
    }
}

/// Get the `\the` expansion primitive.
pub fn get_the<S: TheCompatible>() -> command::BuiltIn<S> {
    command::BuiltIn::new_expansion(the_primitive_fn)
}

fn the_primitive_fn<S: TheCompatible>(
    the_token: token::Token,
    input: &mut vm::ExpansionInput<S>,
) -> txl::Result<()> {
    let token = match input.next()? {
        None => {
            return Err(input
                .vm()
                .fatal_error(error::SimpleEndOfInputError::new("TODO")))
        }
        Some(token) => token,
    };
    match &token.value() {
        token::Value::CommandRef(command_ref) => {
            match input.commands_map().get_command(command_ref) {
                Some(command::Command::Variable(cmd)) => {
                    let variable = cmd.clone().resolve(token, input.as_mut())?;
                    let (state, expansions) = input.state_and_expansions_mut();
                    match variable.value(state) {
                        variable::ValueRef::Int(i) => {
                            int_to_tokens(expansions, the_token, *i);
                        }
                        variable::ValueRef::CatCode(i) => {
                            int_to_tokens(expansions, the_token, (*i as u8).into());
                        }
                        variable::ValueRef::MathCode(i) => {
                            int_to_tokens(expansions, the_token, i.0 as i32)
                        }
                        variable::ValueRef::Font(font) => {
                            let font = *font;
                            let command_ref = input.state().get_command_ref_for_font(font).unwrap();
                            let font_token =
                                token::Token::new_command_ref(command_ref, the_token.trace_key());
                            input.expansions_mut().push(font_token);
                        }
                        variable::ValueRef::TokenList(t) => {
                            expansions.extend(t.iter().rev());
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
                Some(command::Command::MathCharacter(c)) => {
                    let c = *c;
                    int_to_tokens(input.expansions_mut(), the_token, c.0 as i32);
                }
                Some(command::Command::Font(font)) => {
                    // \the is a no-op for font commands?
                    // It may return the frozen control sequence and this would
                    // be visible e.g. in macro matching and equality conditions
                    // in general.
                    font_to_tokens(the_token, input, *font);
                }
                Some(command::Command::Execution(_, Some(tag))) => {
                    if input.state().is_current_font_command(*tag) {
                        font_to_tokens(the_token, input, input.vm().current_font());
                    } else {
                        todo!("should return an error")
                    }
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

fn font_to_tokens<S: TexlangState + TheCompatible>(
    the_token: token::Token,
    input: &mut vm::ExpansionInput<S>,
    font: types::Font,
) {
    let command_ref = input.state().get_command_ref_for_font(font).unwrap();
    let font_token = token::Token::new_command_ref(command_ref, the_token.trace_key());
    input.expansions_mut().push(font_token);
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

#[cfg(test)]
mod tests {
    use super::*;
    impl TheCompatible for texlang_testing::State {}
}
