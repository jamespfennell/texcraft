//! Dynamic allocation of variables and arrays
//!
//! This module contains implementations of brand new Texcraft commands
//! `\newInt` and `\newIntArray` which perform dynamic memory allocation.

use std::collections::HashMap;
use texcraft_stdext::collections::groupingmap;
use texlang::prelude as txl;
use texlang::traits::*;
use texlang::*;

pub const NEWINT_DOC: &str = r"Allocate a new integer

Usage: `\newInt <control sequence>`

The `\newInt` command allocates a new integer
that is referenced using the provided control sequence.
Simple example:

```
\newInt \myvariable
\myvariable = 4
\advance \myvariable by 5
\asserteq{\the \myvariable}{9}
```

You can think of `\newInt` as being a replacement for
Plain TeX's `\newcount` macro (TeXBook p346).
The benefit of `\newInt` is that different callers of the command
do not share the underlying memory; the allocated memory is unique
to the caller.
Under the hood `\newInt` works by allocating new memory on the TeX engine's heap.
";

pub const NEWINTARRAY_DOC: &str = r"Allocate a new integer array

Usage: `\newIntArray <control sequence> <array length>`

The `\newIntArray` command allocates a new array of integers that
is referenced using the provided control sequence.
This new control sequence works pretty much like `\count`, but you can create
    as many arrays as you like and don't need to worry about other
    TeX code reusing the memory.
Unlike `\count`, the size of the array is not fixed by
the engine.
The only constraint on the size is that you have enough RAM
    on the machine to store it.
Simple example:

```
\newIntArray \myarray 3
\myarray 0 = 4
\asserteq{\the \myarray 0}{4}
\myarray 1 = 5
\asserteq{\the \myarray 1}{5}
\myarray 2 = 6
\asserteq{\the \myarray 2}{6}
```

The new control sequence can *not* be aliased using \let.
";

/// Component required for the alloc commands.
#[derive(Default)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Component {
    singletons: Vec<i32>,
    arrays: Vec<i32>,
    #[cfg_attr(feature = "serde", serde(with = "texcraft_stdext::serde_tools::iter"))]
    array_refs: HashMap<token::CommandRef, (usize, usize)>,
}

/// Get the `\newInt` execution command.
pub fn get_newint<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(newint_primitive_fn)
}

fn newint_primitive_fn<S: HasComponent<Component>>(
    _: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> txl::Result<()> {
    let command_ref = Option::<token::CommandRef>::parse(input)?;
    let Some(command_ref) = command_ref else {
        return Ok(());
    };
    let component = input.state_mut().component_mut();
    let index = component.singletons.len();
    component.singletons.push(Default::default());
    input.commands_map_mut().insert_variable_command(
        command_ref,
        variable::Command::new_array(
            singleton_ref_fn,
            singleton_mut_ref_fn,
            variable::IndexResolver::Static(variable::Index(index)),
        ),
        groupingmap::Scope::Local,
    );
    Ok(())
}

fn singleton_ref_fn<S: HasComponent<Component>>(state: &S, index: variable::Index) -> &i32 {
    &state.component().singletons[index.0]
}

fn singleton_mut_ref_fn<S: HasComponent<Component>>(
    state: &mut S,
    index: variable::Index,
) -> &mut i32 {
    &mut state.component_mut().singletons[index.0]
}

/// Return a getter provider for the `\newInt` command.
///
/// The built-in commands for a VM must include this command in order for
///     the allocation component to be serializable.
pub fn get_newint_getter_provider<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    variable::Command::new_getter_provider(singleton_ref_fn, singleton_mut_ref_fn).into()
}

/// Get the `\newIntArray` execution command.
pub fn get_newintarray<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(newintarray_primitive_fn)
}

fn newintarray_primitive_fn<S: HasComponent<Component>>(
    _: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> txl::Result<()> {
    let command_ref = Option::<token::CommandRef>::parse(input)?;
    let len = parse::Uint::<{ parse::Uint::MAX }>::parse(input)?.0;
    let Some(command_ref) = command_ref else {
        return Ok(());
    };
    let component = input.state_mut().component_mut();
    let start = component.arrays.len();
    component.arrays.resize(start + len, Default::default());
    component.array_refs.insert(command_ref, (start, len));
    input.commands_map_mut().insert_variable_command(
        command_ref,
        variable::Command::new_array(
            array_element_ref_fn,
            array_element_mut_ref_fn,
            variable::IndexResolver::Dynamic(resolve),
        ),
        groupingmap::Scope::Local,
    );
    // TODO: Return the arraydef version
    Ok(())
}

fn resolve<S: HasComponent<Component>>(
    token: token::Token,
    input: &mut vm::ExpandedStream<S>,
) -> txl::Result<variable::Index> {
    let command_ref = match token.value() {
        token::Value::CommandRef(command_ref) => command_ref,
        _ => unreachable!(),
    };
    let (array_index, array_len) = *input
        .state()
        .component()
        .array_refs
        .get(&command_ref)
        .unwrap();
    let inner_index = parse::Uint::<{ parse::Uint::MAX }>::parse(input)?.0;
    if inner_index >= array_len {
        return Err(input.vm().fatal_error(error::SimpleTokenError::new(
            token,
            format![
                "Array out of bounds: cannot access index {inner_index} of array with length {array_len}"
            ],
        )
        ));
    }
    Ok(variable::Index(array_index + inner_index))
}

fn array_element_ref_fn<S: HasComponent<Component>>(state: &S, index: variable::Index) -> &i32 {
    &state.component().arrays[index.0]
}

fn array_element_mut_ref_fn<S: HasComponent<Component>>(
    state: &mut S,
    index: variable::Index,
) -> &mut i32 {
    &mut state.component_mut().arrays[index.0]
}

/// Return a getter provider for the `\newIntArray` command.
///
/// The built-in commands for a VM must include this command in order to support
///     the allocation component to be serializable.
pub fn get_newintarray_getter_provider<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    variable::Command::new_array(
        array_element_ref_fn,
        array_element_mut_ref_fn,
        variable::IndexResolver::Dynamic(resolve),
    )
    .into()
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::the::get_the;
    use texlang::vm::implement_has_component;
    use texlang_testing::*;

    #[derive(Default, serde::Serialize, serde::Deserialize)]
    struct State {
        alloc: Component,
        testing: TestingComponent,
    }

    impl TexlangState for State {}

    implement_has_component![State {
        alloc: Component,
        testing: TestingComponent,
    }];

    fn built_in_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        HashMap::from([
            ("newInt", get_newint()),
            ("newInt_getter_provider_\u{0}", get_newint_getter_provider()),
            ("newIntArray", get_newintarray()),
            (
                "newIntArray_getter_provider_\u{0}",
                get_newintarray_getter_provider(),
            ),
            ("the", get_the()),
        ])
    }

    test_suite![
        expansion_equality_tests(
            (newint_base_case, r"\newInt\a \a=3 \the\a", "3"),
            (
                newintarray_base_case_0,
                r"\newIntArray \a 3 \a 0 = 2 \the\a 0",
                "2"
            ),
            (newint_active_char, r"\newInt~~=3 \the~", "3"),
            (
                newintarray_base_case_1,
                r"\newIntArray \a 3 \a 1 = 2 \the\a 1",
                "2"
            ),
            (
                newintarray_base_case_2,
                r"\newIntArray \a 3 \a 2 = 5 \the\a 2",
                "5"
            ),
            (newintarray_active_char, r"\newIntArray~3~2=5 \the~2", "5"),
        ),
        serde_tests(
            (serde_singleton, r"\newInt\a \a=-1 ", r"\the\a"),
            (serde_array, r"\newIntArray\a 20 \a 3=-1 ", r"\the\a 3"),
        ),
        failure_tests(
            (newintarray_out_of_bounds, r"\newIntArray \a 3 \a 3 = 2"),
            (newintarray_negative_index, r"\newIntArray \a 3 \a -3 = 2"),
            (newintarray_negative_length, r"\newIntArray \a -3"),
        ),
    ];
}
