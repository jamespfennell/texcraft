//! TeX types supported by Texlang

mod catcode;
mod mathcode;
use crate::command;
use crate::parse;
use crate::traits::*;

pub use catcode::CatCode;
pub use mathcode::MathCode;

/// A reference to a font that has been loaded.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(::serde::Serialize, ::serde::Deserialize))]
pub struct Font(pub u16);

impl Font {
    /// The null font.
    pub const NULL_FONT: Font = Font(0);
}

impl<S: TexlangState> Parsable<S> for Font {
    fn parse_impl(
        input: &mut crate::vm::ExpandedStream<S>,
    ) -> Result<Self, Box<crate::error::Error>> {
        match Option::<Font>::parse(input)? {
            None => {
                let token_or = input.peek()?.copied();
                input.vm().error(
                    parse::Error::new(
                    "a font reference",
                    token_or,
                    r"a font reference can either be the current font (e.g. \font), a font variable (e.g. \textfont 1) or the result of loading a font (e.g. \a after \font \a path/to/font)",
                    )
                )?;
                Ok(Font::NULL_FONT)
            }
            Some(font) => Ok(font),
        }
    }
}

impl<S: TexlangState> Parsable<S> for Option<Font> {
    fn parse_impl(
        input: &mut crate::vm::ExpandedStream<S>,
    ) -> Result<Self, Box<crate::error::Error>> {
        let Some(token) = input.peek()?.copied() else {
            return Ok(None);
        };
        let crate::token::Value::CommandRef(command_ref) = token.value() else {
            return Ok(None);
        };
        match input.commands_map().get_command(&command_ref) {
            Some(command::Command::Font(f)) => {
                let f = *f;
                input.consume()?;
                Ok(Some(f))
            }
            Some(command::Command::Variable(var)) => {
                let var = var.clone();
                input.consume()?;
                match var.resolve_type::<Font>(token, input)? {
                    None => {
                        input.expansions_mut().push(token);
                        Ok(None)
                    }
                    Some(typed_variable) => Ok(Some(*typed_variable.get(input.state()))),
                }
            }
            Some(command::Command::Execution(_, Some(tag))) => {
                if input.state().is_current_font_command(*tag) {
                    input.consume()?;
                    Ok(Some(input.vm().current_font()))
                } else {
                    Ok(None)
                }
            }
            _ => Ok(None),
        }
    }
}
