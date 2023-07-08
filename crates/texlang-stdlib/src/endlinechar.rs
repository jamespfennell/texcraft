//! The `\endlinechar` primitive

use texlang::traits::*;
use texlang::*;

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Component {
    end_line_char_raw: i32,
}

impl Default for Component {
    fn default() -> Self {
        Self {
            end_line_char_raw: ('\r' as u32).try_into().unwrap(),
        }
    }
}

/// Get the `\endlinechar` command.
pub fn get_endlinechar<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    variable::Command::new_singleton(
        |state: &S, _: variable::Index| -> &i32 { &state.component().end_line_char_raw },
        |state: &mut S, _: variable::Index| -> &mut i32 {
            &mut state.component_mut().end_line_char_raw
        },
    )
    .into()
}

#[inline]
pub fn end_line_char<S: HasComponent<Component>>(state: &S) -> Option<char> {
    let raw = state.component().end_line_char_raw;
    if (0..128).contains(&raw) {
        // All of the conversions are guaranteed to succeed because
        // raw is in the ASCII range.
        Some(char::try_from(u32::try_from(raw).unwrap()).unwrap())
    } else {
        None
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use super::*;
    use crate::{def, prefix, script, testing::*};
    use texlang::vm::implement_has_component;

    #[derive(Default)]
    struct State {
        conditional: Component,
        prefix: prefix::Component,
        script: script::Component,
    }

    impl TexlangState for State {
        fn end_line_char(&self) -> Option<char> {
            end_line_char(self)
        }
    }

    implement_has_component![
        State,
        (Component, conditional),
        (prefix::Component, prefix),
        (script::Component, script),
    ];

    fn initial_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        HashMap::from([("def", def::get_def()), ("endlinechar", get_endlinechar())])
    }

    test_suite![expansion_equality_tests(
        (
            case_1,
            "\\endlinechar=`\\A Hello\nWorld\nMundo\n",
            "Hello WorldAMundoA"
        ),
        (
            case_2,
            "\\endlinechar=-1 Hello\nWorld\nMundo\n",
            "Hello WorldMundo"
        ),
        (
            case_3,
            "\\endlinechar=-1 Hello\nWorld  \nMundo\n",
            "Hello WorldMundo"
        ),
        (case_4, "\\endlinechar=`\\A\nHello\nWorld\n", "Hello WorldA"),
    ),];
}
