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
    let token = input.next(EndOfInputError {})?;
    // TeX.2021.465
    match &token.value() {
        token::Value::CommandRef(command_ref) => {
            match input.commands_map().get_command(command_ref) {
                Some(command::Command::Variable(cmd)) => {
                    let variable = cmd.clone().resolve(token, input.as_mut())?;
                    let (state, expansions) = input.state_and_expansions_mut();
                    match variable.value(state) {
                        variable::ValueRef::Int(i) => {
                            write(expansions, the_token, *i);
                        }
                        variable::ValueRef::CatCode(i) => {
                            write(expansions, the_token, (*i as u8) as i32);
                        }
                        variable::ValueRef::MathCode(i) => write(expansions, the_token, i.0 as i32),
                        variable::ValueRef::Dimen(d) => {
                            write(expansions, the_token, *d);
                        }
                        variable::ValueRef::Font(font) => {
                            let font = *font;
                            let command_ref = input.state().get_command_ref_for_font(font).unwrap();
                            let font_token =
                                token::Token::new_command_ref(command_ref, the_token.trace_key());
                            input.back(font_token);
                        }
                        variable::ValueRef::TokenList(t) => {
                            expansions.extend(t.iter().rev());
                        }
                    };
                }
                Some(command::Command::Character(c)) => {
                    let i = *c as i32;
                    write(input.expansions_mut(), the_token, i);
                }
                Some(command::Command::MathCharacter(c)) => {
                    let i = c.0 as i32;
                    write(input.expansions_mut(), the_token, i);
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
    input.back(font_token);
}

/// Implementation of [`std::fmt::Write`] that writes to a token buffer.
///
/// As well as being sort of elegant (?), converting values to tokens this way
/// avoids all allocations outside of the buffer.
struct TokenWrite<'a>(&'a mut Vec<token::Token>, token::trace::Key);

impl<'a> std::fmt::Write for TokenWrite<'a> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        for c in s.chars() {
            self.write_char(c)?;
        }
        Ok(())
    }
    fn write_char(&mut self, c: char) -> std::fmt::Result {
        let token = if c == ' ' {
            token::Token::new_space(c, self.1)
        } else if c.is_ascii_alphabetic() {
            token::Token::new_letter(c, self.1)
        } else {
            token::Token::new_other(c, self.1)
        };
        self.0.push(token);
        Ok(())
    }
}

fn write<D: std::fmt::Display>(buffer: &mut Vec<token::Token>, the_token: token::Token, value: D) {
    let start = buffer.len();
    use std::fmt::Write;
    let mut t = TokenWrite(buffer, the_token.trace_key());
    write!(t, "{value}").expect("the token writer cannot error");
    buffer[start..].reverse();
}

#[derive(Debug)]
struct EndOfInputError;

impl error::EndOfInputError for EndOfInputError {
    fn doing(&self) -> String {
        r"determining the argument to \the".into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    impl TheCompatible for texlang_testing::State {}
}
