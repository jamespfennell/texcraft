//! Primitives for setting codes (`\catcode`, `\mathcode`, etc.)

use std::collections::HashMap;
use std::fmt::Debug;
use texlang::prelude as txl;
use texlang::traits::*;
use texlang::types::CatCode;
use texlang::*;

pub trait Code: Copy + Debug + Default + variable::SupportedType {
    const COMMAND_DOC: &'static str;
    fn default_low_values() -> [Self; 128] {
        [Self::default(); 128]
    }
}

impl Code for CatCode {
    const COMMAND_DOC: &'static str = "Get or set a cat code register";
    fn default_low_values() -> [Self; 128] {
        CatCode::PLAIN_TEX_DEFAULTS
    }
}

impl Code for types::MathCode {
    const COMMAND_DOC: &'static str = "Get or set a math code register";
    // TODO: default_low_values
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Component<T: Code> {
    #[cfg_attr(feature = "serde", serde(with = "texcraft_stdext::serde_tools::array"))]
    low: [T; 128],
    high: HashMap<usize, T>,
    default: T,
}

impl<T: Code> Component<T> {
    #[inline]
    pub fn get(&self, u: usize) -> &T {
        match self.low.get(u) {
            None => self.high.get(&u).unwrap_or(&self.default),
            Some(cat_code) => cat_code,
        }
    }
    #[inline]
    pub fn get_mut(&mut self, u: usize) -> &mut T {
        match self.low.get_mut(u) {
            None => self.high.entry(u).or_insert(self.default),
            Some(cat_code) => cat_code,
        }
    }
}

impl<T: Code> Default for Component<T> {
    fn default() -> Self {
        Self {
            low: T::default_low_values(),
            high: Default::default(),
            default: Default::default(),
        }
    }
}

/// Return the currently defined cat code of a character.
#[inline]
pub fn cat_code<S: HasComponent<Component<CatCode>>>(state: &S, c: char) -> CatCode {
    get_value(state, c)
}

/// Get the `\catcode` command.
pub fn get_catcode<S: HasComponent<Component<CatCode>>>() -> command::BuiltIn<S> {
    get_command::<CatCode, S>()
}

/// Return the currently defined math code of a character.
#[inline]
pub fn math_code<S: HasComponent<Component<types::MathCode>>>(
    state: &S,
    c: char,
) -> types::MathCode {
    get_value(state, c)
}

/// Get the `\mathcode` command.
pub fn get_mathcode<S: HasComponent<Component<types::MathCode>>>() -> command::BuiltIn<S> {
    get_command::<types::MathCode, S>()
}

/// Return the currently defined math code of a character.
#[inline]
fn get_value<T: Code, S: HasComponent<Component<T>>>(state: &S, c: char) -> T {
    *state.component().get(c as usize)
}

fn get_command<T: Code, S: HasComponent<Component<T>>>() -> command::BuiltIn<S> {
    let cmd: command::BuiltIn<S> = variable::Command::new_array(
        |state: &S, index: variable::Index| -> &T { state.component().get(index.0) },
        |state: &mut S, index: variable::Index| -> &mut T {
            state.component_mut().get_mut(index.0)
        },
        variable::IndexResolver::Dynamic(
            |_: token::Token, input: &mut vm::ExpandedStream<S>| -> txl::Result<variable::Index> {
                let c = char::parse(input)?;
                let u = c as usize;
                Ok(u.into())
            },
        ),
    )
    .into();
    cmd.with_doc(T::COMMAND_DOC)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::the;
    use texlang_testing::*;

    #[derive(Default)]
    #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
    struct State {
        catcode: Component<CatCode>,
        math_code: Component<types::MathCode>,
        testing: TestingComponent,
    }

    impl TexlangState for State {
        fn recoverable_error_hook(
            &self,
            recoverable_error: error::TracedError,
        ) -> Result<(), Box<dyn error::TexError>> {
            TestingComponent::recoverable_error_hook(self, recoverable_error)
        }
    }
    impl the::TheCompatible for State {}

    implement_has_component![State {
        catcode: Component<CatCode>,
        math_code: Component<types::MathCode>,
        testing: TestingComponent,
    }];

    fn built_in_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        HashMap::from([
            ("the", the::get_the()),
            ("catcode", get_catcode()),
            ("mathcode", get_mathcode()),
        ])
    }

    test_suite![
        expansion_equality_tests(
            (catcode_base_case, r"\catcode 48 11 \the\catcode 48", r"11"),
            (
                catcode_grouping,
                r"{\catcode 48 11 \the\catcode 48}-\the\catcode 48",
                r"11-12"
            ),
            (catcode_default, r"\the\catcode 48", r"12"),
            (
                mathcode_base_case,
                r"\mathcode 48 11 \the\mathcode 48",
                r"11"
            ),
            (
                mathcode_grouping,
                r"{\mathcode 48 11 \the\mathcode 48}-\the\mathcode 48",
                r"11-0"
            ),
            (mathcode_default, r"\the\mathcode 48", r"0"),
        ),
        serde_tests(
            (catcode_serde_low, r"\catcode 48 11 ", r"\the\catcode 48"),
            (catcode_serde_high, r"\catcode 480 11 ", r"\the\catcode 480"),
            (
                catcode_serde_group,
                r"\catcode 65 7 {r\catcode 65 8 ",
                r"\the\catcode 65 } \the\catcode 65"
            ),
            (mathcode_serde_low, r"\mathcode 48 11 ", r"\the\mathcode 48"),
            (
                mathcode_serde_high,
                r"\mathcode 480 11 ",
                r"\the\mathcode 480"
            ),
            (
                mathcode_serde_group,
                r"\mathcode 65 7 {r\mathcode 65 8 ",
                r"\the\mathcode 65 } \the\mathcode 65"
            ),
        ),
        recoverable_failure_tests(
            (
                catcode_value_too_large,
                r"\catcode 48 16 \the\catcode 48",
                "0"
            ),
            (
                catcode_value_is_negative,
                r"\catcode 48 -1 \the\catcode 48",
                "0"
            ),
            (
                mathcode_value_too_large,
                r"\mathcode 48 33000 \the\mathcode 48",
                "0"
            ),
            (
                mathcode_value_is_negative,
                r"\mathcode 48 -1 \the\mathcode 48",
                "0"
            ),
        ),
    ];
}
