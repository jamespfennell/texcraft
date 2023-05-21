//! The `\catcode` primitive

use std::collections::HashMap;
use texlang_core::token::catcode;
use texlang_core::token::catcode::CatCode;
use texlang_core::traits::*;
use texlang_core::*;

pub struct Component {
    low: [CatCode; 128],
    high: HashMap<usize, CatCode>,
    default: CatCode,
}

impl Component {
    #[inline]
    pub fn get(&self, u: usize) -> &CatCode {
        match self.low.get(u) {
            None => self.high.get(&u).unwrap_or(&self.default),
            Some(cat_code) => cat_code,
        }
    }

    #[inline]
    pub fn get_mut(&mut self, u: usize) -> &mut CatCode {
        match self.low.get_mut(u) {
            None => self.high.entry(u).or_insert(self.default),
            Some(cat_code) => cat_code,
        }
    }
}

impl Default for Component {
    fn default() -> Self {
        Self {
            low: catcode::CatCode::PLAIN_TEX_DEFAULTS,
            high: Default::default(),
            default: Default::default(),
        }
    }
}

#[inline]
pub fn cat_code<S: HasComponent<Component>>(state: &S, c: char) -> catcode::CatCode {
    *state.component().get(c as usize)
}

pub const CATCODE_DOC: &str = "Get or set a catcode register";

/// Get the `\catcode` command.
pub fn get_catcode<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    variable::Command::new_array(
        |state: &S, index: variable::Index| -> &catcode::CatCode { state.component().get(index.0) },
        |state: &mut S, index: variable::Index| -> &mut catcode::CatCode {
            state.component_mut().get_mut(index.0)
        },
        variable::IndexResolver::Dynamic(
            |token: token::Token,
             input: &mut vm::ExpandedStream<S>|
             -> anyhow::Result<variable::Index> {
                let index: u32 = parse::parse_number(input)?;
                match char::from_u32(index) {
                    None => Err(error::TokenError::new(
                        token,
                        format![
                            "Argument {index} passed to {token} is not a valid UTF-8 code point"
                        ],
                    )
                    .cast()),
                    Some(_) => Ok((index as usize).into()),
                }
            },
        ),
    )
    .into()
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::script;
    use crate::testing::*;
    use crate::the;

    #[derive(Default)]
    struct State {
        catcode: Component,
        script: script::Component,
    }

    impl TexlangState for State {}

    implement_has_component![State, (Component, catcode), (script::Component, script),];

    fn initial_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        HashMap::from([("the", the::get_the()), ("catcode", get_catcode())])
    }

    test_suite![
        expansion_equality_tests(
            (catcode_base_case, r"\catcode 48 11 \the\catcode 48", r"11"),
            (
                grouping,
                r"{\catcode 48 11 \the\catcode 48}\the\catcode 48",
                r"1112"
            ),
            (catcode_default, r"\the\catcode 48", r"12"),
        ),
        failure_tests(
            (catcode_value_too_large, r"\catcode 48 16"),
            (catcode_value_is_negative_large, r"\catcode 48 -1"),
            (invalid_utf_8_number, r"\catcode 55296 12"),
        ),
    ];
}
