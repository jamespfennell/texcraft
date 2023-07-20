//! Register variables (`\count`, `\countdef`)

use texcraft_stdext::collections::groupingmap;
use texlang::parse::OptionalEquals;
use texlang::traits::*;
use texlang::*;

pub const COUNT_DOC: &str = "Get or set an integer register";
pub const COUNTDEF_DOC: &str = "Bind an integer register to a control sequence";

pub struct Component<T, const N: usize>(
    // We currently box the values because putting them directly on the stack causes the
    // message pack decoder to stack overflow. It's a pity that we have to pay a runtime
    // cost due to this, and it would be nice to fix the issue another way.
    Box<[T; N]>,
);

#[cfg(feature = "serde")]
impl<T: serde::Serialize, const N: usize> serde::Serialize for Component<T, N> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let v: Vec<&T> = self.0.iter().collect();
        v.serialize(serializer)
    }
}

#[cfg(feature = "serde")]
impl<'de, T: std::fmt::Debug + serde::Deserialize<'de>, const N: usize> serde::Deserialize<'de>
    for Component<T, N>
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let v = Vec::<T>::deserialize(deserializer)?;
        let a: Box<[T; N]> = v.try_into().unwrap();
        Ok(Component(a))
    }
}

impl<const N: usize> Default for Component<i32, N> {
    fn default() -> Self {
        Self(Box::new([Default::default(); N]))
    }
}

impl<const N: usize> Default for Component<Vec<token::Token>, N> {
    fn default() -> Self {
        let mut v = vec![];
        for _ in 0..N {
            v.push(vec![])
        }
        Self(Box::new(v.try_into().unwrap()))
    }
}

/// Get the `\count` command.
pub fn get_count<S: HasComponent<Component<i32, N>>, const N: usize>() -> command::BuiltIn<S> {
    variable::Command::new_array(ref_fn, mut_fn, variable::IndexResolver::Dynamic(count_fn)).into()
}

/// Get the `\toks` command.
pub fn get_toks<S: HasComponent<Component<Vec<token::Token>, N>>, const N: usize>(
) -> command::BuiltIn<S> {
    variable::Command::new_array(ref_fn, mut_fn, variable::IndexResolver::Dynamic(count_fn)).into()
}

fn count_fn<T, S: HasComponent<Component<T, N>>, const N: usize>(
    _: token::Token,
    input: &mut vm::ExpandedStream<S>,
) -> command::Result<variable::Index> {
    let index = parse::Uint::<N>::parse(input)?;
    Ok(index.0.into())
}

/// Get the `\countdef` command.
pub fn get_countdef<S: HasComponent<Component<i32, N>>, const N: usize>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(countdef_fn)
}

/// Get the `\toksdef` command.
pub fn get_toksdef<S: HasComponent<Component<Vec<token::Token>, N>>, const N: usize>(
) -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(countdef_fn)
}

fn countdef_fn<T: variable::SupportedType, S: HasComponent<Component<T, N>>, const N: usize>(
    _: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> command::Result<()> {
    let (target, _, index) = <(token::CommandRef, OptionalEquals, parse::Uint<N>)>::parse(input)?;
    // TODO: I suspect \countdef should honor \global, but haven't checked pdfTeX.
    input.commands_map_mut().insert_variable_command(
        target,
        variable::Command::new_array(
            ref_fn,
            mut_fn,
            variable::IndexResolver::Static(index.0.into()),
        ),
        groupingmap::Scope::Local,
    );
    Ok(())
}

fn ref_fn<T, S: HasComponent<Component<T, N>>, const N: usize>(
    state: &S,
    index: variable::Index,
) -> &T {
    state.component().0.get(index.0).unwrap()
}

fn mut_fn<T, S: HasComponent<Component<T, N>>, const N: usize>(
    state: &mut S,
    index: variable::Index,
) -> &mut T {
    state.component_mut().0.get_mut(index.0).unwrap()
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::script;
    use crate::testing::*;
    use crate::the;
    use texlang::vm::implement_has_component;

    #[derive(Default)]
    #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
    struct State {
        registers_i32: Component<i32, 256>,
        registers_token_list: Component<Vec<token::Token>, 256>,
        script: script::Component,
    }

    impl TexlangState for State {}

    implement_has_component![
        State,
        (Component<i32, 256>, registers_i32),
        (Component<Vec<token::Token>, 256>, registers_token_list),
        (script::Component, script),
    ];

    fn initial_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        HashMap::from([
            ("the", the::get_the()),
            ("count", get_count()),
            ("countdef", get_countdef()),
            ("toks", get_toks()),
            ("toksdef", get_toksdef()),
        ])
    }

    test_suite![
        expansion_equality_tests(
            (write_and_read_register, r"\count 23 4 \the\count 23", r"4"),
            (
                write_and_read_register_eq,
                r"\count 23 = 4 \the\count 23",
                r"4"
            ),
            (countdef_base_case, r"\countdef\A 23\A 4 \the\A", r"4"),
            (countdef_base_case_eq, r"\countdef\A = 23\A 4 \the\A", r"4"),
            (
                countdef_with_count,
                r"\countdef\A 23\A 4\count 1 0 \the\A",
                r"4"
            ),
            (
                countdef_with_same_count,
                r"\countdef\A 23\A 4\count 23 5 \the\A",
                r"5"
            ),
            (
                toks_basic,
                r"\toks 1 = {Hola, Mundo}\the \toks 1",
                r"Hola, Mundo"
            ),
            (
                toksdef_basic,
                r"\toksdef\content 1 \toks 1 = {Hola, Mundo}\the \content",
                r"Hola, Mundo"
            ),
            (
                toks_copy,
                r"\toks 1 = {Hola, Mundo}\toks 2 = \toks 1 \the \toks 2",
                r"Hola, Mundo"
            ),
        ),
        serde_tests(
            (serde_basic, r"\count 100 200 ", r"\the \count 100"),
            (serde_countdef, r"\countdef \A 100 \A = 200 ", r"\the \A"),
            (
                serde_group,
                r"\count 1 2 {\count 1 3 ",
                r"\the \count 1 } \the \count 1"
            ),
        ),
        failure_tests(
            (write_register_index_too_big, r"\count 260 = 4"),
            (write_register_negative_index, r"\count -1 = 4"),
            (countdef_register_index_too_big, r"\countdef\A 260 \A= 4"),
        ),
    ];
}
