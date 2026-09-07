//! TeX types supported by Texlang

mod catcode;
mod mathcode;
use crate::command;
use crate::parse;
use crate::prelude as txl;
use crate::traits::*;
pub use catcode::CatCode;
use common::font;
pub use mathcode::MathCode;

impl Parsable for font::Id {
    fn parse_impl<S: TexlangState>(input: &mut crate::vm::ExpandedStream<S>) -> txl::Result<Self> {
        // Corresponds to scan_font_ident in TeX.2021.577.
        match parse_font_or(input)? {
            None => {
                let token_or = input.peek()?;
                input.error(
                    parse::Error::new(
                    "a font reference",
                    token_or,
                    r"a font reference can either be the current font (e.g. \font), a font variable (e.g. \textfont 1) or the result of loading a font (e.g. \a after \font \a path/to/font)",
                    )
                )?;
                Ok(font::Id::NULL)
            }
            Some(font) => Ok(font),
        }
    }
}

fn parse_font_or<S: TexlangState>(
    input: &mut crate::vm::ExpandedStream<S>,
) -> txl::Result<Option<font::Id>> {
    let Some(token) = input.next()? else {
        return Ok(None);
    };
    let crate::token::Value::CommandRef(command_ref) = token.value() else {
        input.back(token);
        return Ok(None);
    };
    match input.commands_map().get_command(&command_ref) {
        Some(command::Command::Font(f)) => {
            let f = *f;
            Ok(Some(f))
        }
        Some(command::Command::Variable(var)) => {
            let var = var.clone();
            match var.resolve_type::<font::Id>(token, input)? {
                None => {
                    input.back(token);
                    Ok(None)
                }
                Some(typed_variable) => Ok(Some(*typed_variable.get(input.state()))),
            }
        }
        Some(command::Command::Execution(_, Some(tag))) => {
            if input.state().is_current_font_command(*tag) {
                Ok(Some(input.vm().current_font()))
            } else {
                input.back(token);
                Ok(None)
            }
        }
        _ => {
            input.back(token);
            Ok(None)
        }
    }
}
