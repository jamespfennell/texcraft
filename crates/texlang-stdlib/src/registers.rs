//! Register variables (`\count`, `\countdef`)

use texcraft_stdext::collections::groupingmap;
use texlang::parse::{Command, OptionalEquals};
use texlang::token::trace;
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

impl<T: Default + Copy, const N: usize> Default for Component<T, N> {
    fn default() -> Self {
        Self(Box::new([Default::default(); N]))
    }
}

/// Get the `\count` command.
pub fn get_count<S: HasComponent<Component<i32, N>>, const N: usize>() -> command::BuiltIn<S> {
    variable::Command::new_array(ref_fn, mut_fn, variable::IndexResolver::Dynamic(count_fn)).into()
}

fn count_fn<T, S: HasComponent<Component<T, N>>, const N: usize>(
    count_token: token::Token,
    input: &mut vm::ExpandedStream<S>,
) -> command::Result<variable::Index> {
    let index = usize::parse(input)?;
    if index >= N {
        return Err(IndexTooLargeError {
            trace: input.vm().trace(count_token),
            index,
            num: N,
        }
        .into());
    }
    Ok(index.into())
}

/// Get the `\countdef` command.
pub fn get_countdef<S: HasComponent<Component<i32, N>>, const N: usize>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(countdef_fn)
}

fn countdef_fn<T: variable::SupportedType, S: HasComponent<Component<T, N>>, const N: usize>(
    countdef_token: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> command::Result<()> {
    let (target, _, index) = <(Command, OptionalEquals, usize)>::parse(input)?;
    let Command::ControlSequence(cs_name) = target;
    if index >= N {
        return Err(IndexTooLargeError {
            trace: input.vm().trace(countdef_token),
            index,
            num: N,
        }
        .into());
    }
    // TODO: I suspect \countdef should honor \global, but haven't checked pdfTeX.
    input.commands_map_mut().insert_variable_command(
        cs_name,
        variable::Command::new_array(
            ref_fn,
            mut_fn,
            variable::IndexResolver::Static(index.into()),
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

#[derive(Debug)]
struct IndexTooLargeError {
    trace: trace::SourceCodeTrace,
    index: usize,
    num: usize,
}

impl error::TexError for IndexTooLargeError {
    fn kind(&self) -> error::Kind {
        error::Kind::Token(&self.trace)
    }

    fn title(&self) -> String {
        format![
            "Index {} is too large; there are only {} integer registers",
            self.index, self.num,
        ]
    }
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
        registers: Component<i32, 256>,
        script: script::Component,
    }

    impl TexlangState for State {}

    implement_has_component![
        State,
        (Component<i32, 256>, registers),
        (script::Component, script),
    ];

    fn initial_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        HashMap::from([
            ("the", the::get_the()),
            ("count", get_count()),
            ("countdef", get_countdef()),
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
        ),
        serde_tests(
            (serde_basic, r"\count 100 200 ", r"\the \count 100"),
            (serde_countdef, r"\countdef \A 100 \A = 200 ", r"\the \A"),
        ),
        failure_tests(
            (write_register_index_too_big, r"\count 260 = 4"),
            (write_register_negative_index, r"\count -1 = 4"),
            (countdef_register_index_too_big, r"\countdef\A 260 \A= 4"),
        ),
    ];
}
