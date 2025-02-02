//! `\let` aliasing command

use crate::prefix;
use texlang::parse::OptionalEqualsUnexpanded;
use texlang::prelude as txl;
use texlang::traits::*;
use texlang::*;

pub const LET_DOC: &str = "Assign a command or character to a control sequence";

/// Get the `\let` command.
pub fn get_let<S: HasComponent<prefix::Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(let_primitive_fn)
        .with_tag(let_tag())
        .with_doc(LET_DOC)
}

static LET_TAG: command::StaticTag = command::StaticTag::new();

pub fn let_tag() -> command::Tag {
    LET_TAG.get()
}

fn let_primitive_fn<S: HasComponent<prefix::Component>>(
    _: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> txl::Result<()> {
    let scope = TexlangState::variable_assignment_scope_hook(input.state_mut());
    let alias = token::CommandRef::parse(input)?;
    OptionalEqualsUnexpanded::parse(input)?;
    match input.unexpanded().next()? {
        None => {
            return Err(input.vm().fatal_error(error::SimpleEndOfInputError::new(
                "unexpected end of input while reading the right hand side of a \\let assignment",
            )))
        }
        Some(token) => match token.value() {
            token::Value::CommandRef(command_ref) => {
                input
                    .commands_map_mut()
                    .alias_control_sequence(alias, &command_ref, scope);
            }
            _ => {
                input.commands_map_mut().alias_token(alias, token, scope);
            }
        },
    };
    Ok(())
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use super::*;
    use crate::def;
    use crate::the;
    use texlang_testing::*;

    #[derive(Default, serde::Serialize, serde::Deserialize)]
    struct State {
        prefix: prefix::Component,
        testing: TestingComponent,
    }

    implement_has_component![State {
        prefix: prefix::Component,
        testing: TestingComponent,
    }];

    impl TexlangState for State {
        fn variable_assignment_scope_hook(
            state: &mut Self,
        ) -> texcraft_stdext::collections::groupingmap::Scope {
            prefix::variable_assignment_scope_hook(state)
        }
        fn recoverable_error_hook(
            &self,
            recoverable_error: error::TracedError,
        ) -> Result<(), Box<dyn error::TexError>> {
            TestingComponent::recoverable_error_hook(self, recoverable_error)
        }
    }

    fn built_in_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        HashMap::from([
            ("def", def::get_def()),
            ("global", prefix::get_global()),
            ("integer", TestingComponent::get_integer()),
            ("let", get_let()),
            ("the", the::get_the()),
        ])
    }

    test_suite![
        expansion_equality_tests(
            (let_for_macro, r"\def\A{abc}\let\B\A\B", "abc"),
            (let_for_macro_active_char, r"\def\A{abc}\let~\A~", "abc"),
            (local, r"\def\A{a}\def\B{b}\let\C=\A{\let\C=\B \C}\C", "ba"),
            (
                global,
                r"\def\A{a}\def\B{b}\let\C=\A{\global\let\C=\B \C}\C",
                "bb"
            ),
            (let_for_macro_equals, r"\def\A{abc}\let\B=\A\B", "abc"),
            (let_character, r"\let\A=B\A", "B"),
            (let_unknown_cs_name, r"\let \B=\undefined", ""),
        ),
        serde_tests(
            (
                alias_execution_primitive,
                r"\let\defNew=\def",
                r"\defNew\A{abc}\A"
            ),
            (
                alias_expansion_primitive,
                r"\let\theNew=\the",
                r"\integer=3\theNew\integer"
            ),
            (
                alias_variable_singleton,
                r"\let\integerNew=\integer",
                r"\integerNew=3\the\integer"
            ),
            (serde_macro, r"\def\A{Hello World}\let\B=\A ", r"\A \B",),
            (serde_character, r"\let\A=B ", r"\A",),
        ),
    ];
}
