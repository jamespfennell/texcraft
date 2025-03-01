//! Register variables (`\count`, `\countdef`)

use texlang::parse::OptionalEquals;
use texlang::prelude as txl;
use texlang::traits::*;
use texlang::variable::SupportedType;
use texlang::*;

pub const COUNT_DOC: &str = "Get or set an integer register";
pub const COUNTDEF_DOC: &str = "Bind an integer register to a control sequence";

/// See [Component].
pub struct DefaultMarker;

/// Component required to have registers of type `T`.
///
/// The `Marker` generic parameter exists so that a single state type
/// can contain multiple copies of this component (`Component<MarkerOne>`,
/// `Component<MarkerTwo>`, etc.) and implement that HasComponent pattern
/// multiple times. This allows for multiple register commands of the same
/// type to be included in the same VM.
pub struct Component<T, const N: usize, Marker = DefaultMarker>(
    // We currently box the values because putting them directly on the stack causes the
    // message pack decoder to stack overflow. It's a pity that we have to pay a runtime
    // cost due to this, and it would be nice to fix the issue another way.
    Box<[T; N]>,
    std::marker::PhantomData<Marker>,
);

static COUNTDEF_TAG: command::StaticTag = command::StaticTag::new();

pub fn countdef_tag() -> command::Tag {
    COUNTDEF_TAG.get()
}

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
        Ok(Component(a, Default::default()))
    }
}

impl<T: std::fmt::Debug + Default, const N: usize, Marker> Default for Component<T, N, Marker> {
    fn default() -> Self {
        let mut v = vec![];
        for _ in 0..N {
            v.push(T::default())
        }
        Self(Box::new(v.try_into().unwrap()), Default::default())
    }
}

/// Get the `\count` command.
pub fn get_count<S: HasComponent<Component<i32, N>>, const N: usize>() -> command::BuiltIn<S> {
    new_registers_command()
}

/// Get the `\dimen` command.
pub fn get_dimen<S: HasComponent<Component<core::Scaled, N>>, const N: usize>(
) -> command::BuiltIn<S> {
    new_registers_command()
}

/// Get the `\toks` command.
pub fn get_toks<S: HasComponent<Component<Vec<token::Token>, N>>, const N: usize>(
) -> command::BuiltIn<S> {
    new_registers_command()
}

/// Creates a new registers command that stores values in the component.
pub fn new_registers_command<
    T: SupportedType,
    Marker: 'static,
    S: HasComponent<Component<T, N, Marker>>,
    const N: usize,
>() -> command::BuiltIn<S> {
    variable::Command::new_array(ref_fn, mut_fn, variable::IndexResolver::Dynamic(count_fn)).into()
}

fn count_fn<T, Marker: 'static, S: HasComponent<Component<T, N, Marker>>, const N: usize>(
    _: token::Token,
    input: &mut vm::ExpandedStream<S>,
) -> txl::Result<variable::Index> {
    let index = parse::Uint::<N>::parse(input)?;
    Ok(index.0.into())
}

/// Get the `\countdef` command.
pub fn get_countdef<S: HasComponent<Component<i32, N>>, const N: usize>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(countdef_fn).with_tag(countdef_tag())
}

/// Get the `\toksdef` command.
pub fn get_toksdef<S: HasComponent<Component<Vec<token::Token>, N>>, const N: usize>(
) -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(countdef_fn).with_tag(countdef_tag())
}

fn countdef_fn<T: variable::SupportedType, S: HasComponent<Component<T, N>>, const N: usize>(
    _: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> txl::Result<()> {
    let scope = TexlangState::variable_assignment_scope_hook(input.state_mut());
    let (cmd_ref_or, _, index) =
        <(Option<token::CommandRef>, OptionalEquals, parse::Uint<N>)>::parse(input)?;
    if let Some(cmd_ref) = cmd_ref_or {
        input.commands_map_mut().insert_variable_command(
            cmd_ref,
            variable::Command::new_array(
                ref_fn,
                mut_fn,
                variable::IndexResolver::Static(index.0.into()),
            ),
            scope,
        );
    }
    Ok(())
}

fn ref_fn<T, Marker: 'static, S: HasComponent<Component<T, N, Marker>>, const N: usize>(
    state: &S,
    index: variable::Index,
) -> &T {
    state.component().0.get(index.0).unwrap()
}

fn mut_fn<T, Marker: 'static, S: HasComponent<Component<T, N, Marker>>, const N: usize>(
    state: &mut S,
    index: variable::Index,
) -> &mut T {
    state.component_mut().0.get_mut(index.0).unwrap()
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::{prefix, the};
    use texlang::vm::implement_has_component;
    use texlang_testing::*;

    #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
    #[derive(Default)]
    struct State {
        registers_i32: Component<i32, 256>,
        registers_dimen: Component<core::Scaled, 256>,
        registers_token_list: Component<Vec<token::Token>, 256>,
        prefix: prefix::Component,
        testing: TestingComponent,
    }

    impl TexlangState for State {
        fn recoverable_error_hook(
            &self,
            recoverable_error: error::TracedError,
        ) -> Result<(), Box<dyn error::TexError>> {
            TestingComponent::recoverable_error_hook(self, recoverable_error)
        }
        fn variable_assignment_scope_hook(
            state: &mut Self,
        ) -> texcraft_stdext::collections::groupingmap::Scope {
            prefix::variable_assignment_scope_hook(state)
        }
    }
    impl the::TheCompatible for State {}

    implement_has_component![State{
        registers_i32: Component<i32, 256>,
        registers_dimen: Component<core::Scaled, 256>,
        registers_token_list: Component<Vec<token::Token>, 256>,
        prefix: prefix::Component,
        testing: TestingComponent,
    }];

    fn built_in_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        HashMap::from([
            ("the", the::get_the()),
            ("count", get_count()),
            ("countdef", get_countdef()),
            ("dimen", get_dimen()),
            ("global", prefix::get_global()),
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
            (
                negative_negative,
                r"\count 1=5000 \count 0=-1 \the \count -\count 0",
                r"5000"
            ),
            (countdef_base_case, r"\countdef\A 23\A 4 \the\A", r"4"),
            (countdef_base_case_eq, r"\countdef\A = 23\A 4 \the\A", r"4"),
            (
                countdef_with_count,
                r"\countdef\A 23\A 4\count 1 0 \the\A",
                r"4"
            ),
            (
                countdef_local,
                r"\count 1=1 \count 2=2 \countdef\A 1{\countdef\A 2}\the\A",
                r"1"
            ),
            (
                countdef_global,
                r"\count 1=1 \count 2=2 \countdef\A 1{\global\countdef\A 2}\the\A",
                r"2"
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
            (
                dimen_to_int_1,
                r"\dimen 1 = 40sp \count 1 = \dimen 1 \the \count 1",
                r"40",
            ),
            (
                dimen_to_int_2,
                r"\dimen 1 = 40in \count 1 = \dimen 1 \the \count 1",
                r"189451468",
            ),
            (
                int_to_dimen_1,
                r"\count 1 = 40 \dimen 1 = \count 1 pt \the \dimen 1",
                r"40.0pt",
            ),
            (
                int_to_dimen_2,
                r"\count 1 = -40 \dimen 1 = \count 1 pt \the \dimen 1",
                r"-40.0pt",
            ),
            (
                int_to_dimen_3,
                // <int><int> is interpreted as <int>*<int>sp, so the result here is 40*2sp
                r"\count 3 = 40 \count 5 = 2 \dimen 7 = \count 3 \count 5 \the \dimen 7",
                r"0.00122pt",
            ),
            (
                int_to_dimen_4,
                r"\count 1 = 40 \dimen 2 = 5sp \dimen 1 = \count 1 \dimen 2 \the \dimen 1",
                r"0.00305pt",
            ),
            (
                dimen_to_dimen_1,
                r"\dimen 1 = 10pt \dimen 2 = 5 \dimen 1 \the \dimen 2",
                r"50.0pt",
            ),
            (
                dimen_to_dimen_2,
                r"\dimen 2 = 10pt \dimen 1 = - 1.5 \dimen 2 \the \dimen 1",
                r"-15.0pt",
            ),
            (
                dimen_to_dimen_3,
                r"\dimen 2 = 5pt \dimen 1 = - 1.5 \dimen 2 \the \dimen 1",
                r"-7.5pt",
            ),
            (
                dimen_to_dimen_4,
                r"\dimen 2 = -10pt \dimen 1 = 1.5 \dimen 2 \the \dimen 1",
                r"-15.0pt",
            ),
            (
                dimen_to_dimen_5,
                r"\dimen 2 = -5pt \dimen 1 = 1.5 \dimen 2 \the \dimen 1",
                r"-7.5pt",
            ),
            (
                dimen_to_dimen_6,
                r"\dimen 2 = -10pt \dimen 1 = - 1.5 \dimen 2 \the \dimen 1",
                r"15.0pt",
            ),
            (
                dimen_to_dimen_7,
                r"\dimen 2 = -5pt \dimen 1 = - 1.5 \dimen 2 \the \dimen 1",
                r"7.5pt",
            ),
            (
                int_dimen_to_dimen_1,
                r"\count 1 = 2 \dimen 1 = 3pt \dimen 2 = \count 1 \dimen 1 \the \dimen 2",
                r"6.0pt",
            ),
            (
                int_dimen_to_dimen_2,
                r"\count 1 = -2 \dimen 1 = 3pt \dimen 2 = \count 1 \dimen 1 \the \dimen 2",
                r"-6.0pt",
            ),
            (
                int_dimen_to_dimen_3,
                r"\count 1 = 2 \dimen 1 = -3pt \dimen 2 = \count 1 \dimen 1 \the \dimen 2",
                r"-6.0pt",
            ),
            (
                int_dimen_to_dimen_4,
                r"\count 1 = 2 \dimen 1 = 3pt \dimen 2 = -\count 1 \dimen 1 \the \dimen 2",
                r"-6.0pt",
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
        recoverable_failure_tests(
            (
                write_register_index_too_big,
                r"\count 260 = 4 \the\count 0",
                "4"
            ),
            (
                write_register_negative_index,
                r"\count -1 = 4 \the\count 0",
                "4"
            ),
            (
                countdef_register_index_too_big,
                r"\countdef\A 260 \A= 4 \the\count 0",
                "4"
            ),
            (countdef_missing_cs, r"\countdef 260 End", "End"),
            (
                dimen_to_dimen_too_big_1,
                r"\dimen 1 = 10000pt \dimen 2 = 2 \dimen 1 \the \dimen 2",
                r"16383.99998pt",
            ),
            (
                dimen_to_dimen_too_big_2,
                r"\dimen 1 = -10000pt \dimen 2 = 2 \dimen 1 \the \dimen 2",
                r"-16383.99998pt",
            ),
            (
                dimen_to_dimen_too_big_3,
                r"\dimen 1 = 10000pt \dimen 2 = -2 \dimen 1 \the \dimen 2",
                r"-16383.99998pt",
            ),
            (
                dimen_to_dimen_too_big_4,
                r"\dimen 1 = -10000pt \dimen 2 = -2 \dimen 1 \the \dimen 2",
                r"16383.99998pt",
            ),
            (
                int_to_dimen_too_big_1,
                r"\count 1 = 400 \dimen 1 = \count 1 in \the \dimen 1",
                r"16383.99998pt",
            ),
            (
                int_to_dimen_too_big_2,
                r"\count 1 = -400 \dimen 1 = \count 1 in \the \dimen 1",
                r"-16383.99998pt",
            ),
            (
                int_to_dimen_too_big_3,
                r"\count 1 = 400 \dimen 1 = - \count 1 in \the \dimen 1",
                r"-16383.99998pt",
            ),
            (
                int_to_dimen_too_big_4,
                r"\count 1 = -400 \dimen 1 = - \count 1 in \the \dimen 1",
                r"16383.99998pt",
            ),
            (
                int_dimen_to_dimen_too_big_1,
                r"\count 1 = 2 \dimen 1 = 10000pt \dimen 2 = \count 1 \dimen 1 \the \dimen 2",
                r"16383.99998pt",
            ),
            (
                int_dimen_to_dimen_too_big_2,
                r"\count 1 = 2 \dimen 1 = 10000pt \dimen 2 = -\count 1 \dimen 1 \the \dimen 2",
                r"-16383.99998pt",
            ),
            (
                int_dimen_to_dimen_too_big_3,
                r"\count 1 = -2 \dimen 1 = 10000pt \dimen 2 = \count 1 \dimen 1 \the \dimen 2",
                r"-16383.99998pt",
            ),
            (
                int_dimen_to_dimen_too_big_4,
                r"\count 1 = 2 \dimen 1 = -10000pt \dimen 2 = \count 1 \dimen 1 \the \dimen 2",
                r"-16383.99998pt",
            ),
        ),
    ];
}
