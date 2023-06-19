//! The `\catcode` primitive

use std::collections::HashMap;
use texlang::token;
use texlang::token::CatCode;
use texlang::traits::*;
use texlang::*;

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Component {
    #[cfg_attr(
        feature = "serde",
        serde(
            serialize_with = "texcraft_stdext::serde_tools::serialize_array",
            deserialize_with = "texcraft_stdext::serde_tools::deserialize_array",
        )
    )]
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
            low: CatCode::PLAIN_TEX_DEFAULTS,
            high: Default::default(),
            default: Default::default(),
        }
    }
}

#[inline]
pub fn cat_code<S: HasComponent<Component>>(state: &S, c: char) -> CatCode {
    *state.component().get(c as usize)
}

pub const CATCODE_DOC: &str = "Get or set a catcode register";

/// Get the `\catcode` command.
pub fn get_catcode<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    variable::Command::new_array(
        |state: &S, index: variable::Index| -> &CatCode { state.component().get(index.0) },
        |state: &mut S, index: variable::Index| -> &mut CatCode {
            state.component_mut().get_mut(index.0)
        },
        variable::IndexResolver::Dynamic(
            |_: token::Token,
             input: &mut vm::ExpandedStream<S>|
             -> command::Result<variable::Index> { Ok(usize::parse(input)?.into()) },
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
    #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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
        serde_tests(
            (serde_low, r"\catcode 48 11 ", r"\the\catcode 48"),
            (serde_high, r"\catcode 480 11 ", r"\the\catcode 480"),
        ),
        failure_tests(
            (catcode_value_too_large, r"\catcode 48 16"),
            (catcode_value_is_negative_large, r"\catcode 48 -1"),
        ),
    ];
}
