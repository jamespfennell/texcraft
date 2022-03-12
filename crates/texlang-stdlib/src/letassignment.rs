//! `\let` assignments

use crate::prefix;
use texlang_core::parse;
use texlang_core::prelude::*;

pub const LET_DOC: &str = "Assign a command or character to a control sequence";

/// Get the `\let` command.
pub fn get_let<S: HasComponent<prefix::Component>>() -> command::ExecutionPrimitive<S> {
    command::ExecutionPrimitive::new(let_primitive_fn, let_id())
}

struct Let;

pub fn let_id() -> std::any::TypeId {
    std::any::TypeId::of::<Let>()
}

fn let_primitive_fn<S: HasComponent<prefix::Component>>(
    let_token: Token,
    input: &mut runtime::ExecutionInput<S>,
) -> anyhow::Result<()> {
    let global = input.state_mut().component_mut().take_global();
    let name = parse::parse_command_target("\\let assignment", let_token, input.unexpanded())?;
    parse::parse_optional_equals(input.unexpanded())?;
    let command =
        match input.unexpanded().next()? {
            None => return Err(error::EndOfInputError::new(
                "unexpected end of input while reading the right hand side of a \\let assignment",
            )
            .cast()),
            Some(token) => match token.value() {
                ControlSequence(name) => match input.base().commands_map.get(&name) {
                    None => return Err(error::new_undefined_cs_error(token, input.env())),
                    Some(cmd) => cmd.clone(),
                },
                _ => command::Command::Character(token),
            },
        };
    let commands_map = &mut input.base_mut().commands_map;
    if global {
        commands_map.insert_global(name, command);
    } else {
        commands_map.insert(name, command);
    }
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::def;
    use crate::testutil::*;

    fn setup_expansion_test(s: &mut runtime::Env<State>) {
        s.set_command("def", def::get_def());
        s.set_command("global", prefix::get_global());
        s.set_command("let", get_let());
    }

    expansion_test![let_for_macro, r"\def\A{abc}\let\B\A\B", "abc"];
    expansion_test![local, r"\def\A{a}\def\B{b}\let\C=\A{\let\C=\B \C}\C", "ba"];
    expansion_test![
        global,
        r"\def\A{a}\def\B{b}\let\C=\A{\global\let\C=\B \C}\C",
        "bb"
    ];
    expansion_test![let_for_macro_equals, r"\def\A{abc}\let\B=\A\B", "abc"];
}
