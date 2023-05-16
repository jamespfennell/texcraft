//! Dynamic allocation of variables and arrays
//!
//! This module contains implementations of brand new Texcraft commands
//! `\newint` and `\newarray` which perform dynamic memory allocation.

use std::collections::{BTreeMap, HashMap};
use std::ops::Bound::Included;
use texcraft_stdext::collections::groupingmap;
use texcraft_stdext::collections::nevec::Nevec;
use texlang_core::traits::*;
use texlang_core::*;

pub const NEWINT_DOC: &str = r"Allocate a new integer variable

Usage: `\newint <control sequence>`

The `\newint` command allocates a new integer variable
that is referenced using the provided control sequence.
Simple example:

```
\newint \myvariable
\myvariable = 4
\advance \myvariable by 5
\asserteq{\the \myvariable}{9}
```

You can think of `\newint` as being a replacement for
Plain TeX's `\newcount` macro (TeXBook p346).
The benefit of `\newint` is that different callers of the command
do not share the underlying memory; the allocated memory is unique
to the caller.
Under the hood `\newint` works by allocating new memory on
the TeX engine's heap. This memory is allocated in a large Rust vector,
so for n invocations of `\newint` there are log(n) heap allocations.

The variable is deallocated at the end of the current group.
If there is no current group (i.e., this is the global scope)
then the variable is never deallocated.
";

pub const NEWARRAY_DOC: &str = r"Allocate a new integer array

Usage: `\newarray <control sequence> <array length>`

The `\newarray` command allocates a new array of integers that
is referenced using the provided control sequence.
This new array works pretty much like `\count`, but you can create
as many arrays as you like and don't need to worry about other
TeX code reusing the memory.
Also, unlike `\count`, the size of the array is not fixed by
the engine.
The only constaint on the size is that you have enough RAM
on the machine to store it.
Simple example:

```
\newarray \myarray 3
\myarray 0 = 4
\asserteq{\the \myarray 0}{4}
\myarray 1 = 5
\asserteq{\the \myarray 1}{5}
\myarray 2 = 6
\asserteq{\the \myarray 2}{6}
```

The array is deallocated at the end of the current group.
If there is no current group (i.e., this is the global scope)
then the variable is never deallocated.
";

/// Component required for the alloc commands.
#[derive(Default)]
pub struct Component {
    allocations: Nevec<GroupAllocations>,

    // Map from addr to (group allocations index, allocations.singletons index)
    singleton_addr_map: HashMap<variable::Address, (usize, usize)>,
    next_singleton_addr: variable::Address,

    // Map from addr to (group allocations index, gallocations.singletons index)
    //
    // Note that addr points to the first element of the array; when
    // we add a new array, addr is incremented by the size of the array.
    // addr + i then references the ith element of the array, for
    // appropriate i. We use a BTreeMap to make it fast to obtain the
    // indices from addr + i for any i.
    array_addr_map: BTreeMap<variable::Address, (usize, usize)>,
    next_array_addr: variable::Address,
}

/// Contains all the allocations for a single group.
#[derive(Default)]
struct GroupAllocations {
    // TODO: rather than using a Vec of Vecs it may be more efficient
    // to use a single global Vec and a single non-global Vec.
    singletons: Vec<Singleton>,
    arrays: Vec<Array>,
}

struct Singleton {
    addr: variable::Address,
    value: i32,
}

struct Array {
    addr: variable::Address,
    value: Vec<i32>,
}

impl Component {
    fn alloc_int(&mut self) -> variable::Address {
        let i = (
            self.allocations.len() - 1,
            self.allocations.last().singletons.len(),
        );
        self.allocations.last_mut().singletons.push(Singleton {
            addr: self.next_singleton_addr,
            value: 0,
        });
        self.singleton_addr_map.insert(self.next_singleton_addr, i);
        self.next_singleton_addr.0 += 1;
        variable::Address(self.next_singleton_addr.0 - 1)
    }

    fn alloc_array(&mut self, len: usize) -> variable::Address {
        let i = (
            self.allocations.len() - 1,
            self.allocations.last().arrays.len(),
        );
        self.allocations.last_mut().arrays.push(Array {
            addr: self.next_array_addr,
            value: vec![0; len],
        });
        self.array_addr_map.insert(self.next_array_addr, i);
        self.next_array_addr.0 += len;
        variable::Address(self.next_array_addr.0 - len)
    }

    fn find_array(&self, elem_addr: variable::Address) -> (variable::Address, (usize, usize)) {
        let (a, (b, c)) = self
            .array_addr_map
            .range((Included(&variable::Address(0)), Included(&elem_addr)))
            .rev()
            .next()
            .unwrap();
        (*a, (*b, *c))
    }

    // TODO: why isn't this used?
    pub fn begin_group(&mut self) {
        self.allocations.push(GroupAllocations {
            singletons: vec![],
            arrays: vec![],
        });
    }

    // TODO: why isn't this used?
    pub fn end_group(&mut self) -> bool {
        let allocations = match self.allocations.pop_from_tail() {
            None => return false,
            Some(allocations) => allocations,
        };
        for singleton in &allocations.singletons {
            self.singleton_addr_map.remove(&singleton.addr);
        }
        for array in &allocations.arrays {
            self.array_addr_map.remove(&array.addr);
        }
        true
    }
}

/// Get the `\newint` exeuction command.
pub fn get_newint<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(newint_primitive_fn)
}

fn newint_primitive_fn<S: HasComponent<Component>>(
    newint_token: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> anyhow::Result<()> {
    let name = parse::parse_command_target("newint allocation", newint_token, input.unexpanded())?;
    let addr = input.state_mut().component_mut().alloc_int();
    input.base_mut().commands_map.insert_variable_command(
        name,
        variable::Command::new(
            singleton_ref_fn,
            singleton_mut_ref_fn,
            variable::AddressSpec::DynamicVirtual(Box::new(SingletonAddressSpec(addr))),
        ),
        groupingmap::Scope::Local,
    );
    Ok(())
}

struct SingletonAddressSpec(variable::Address);

impl<S> variable::DynamicAddressSpec<S> for SingletonAddressSpec {
    fn resolve(
        &self,
        _: texlang_core::token::Token,
        _: &mut vm::ExpansionInput<S>,
    ) -> anyhow::Result<variable::Address> {
        Ok(self.0)
    }
}

fn singleton_ref_fn<S: HasComponent<Component>>(state: &S, addr: variable::Address) -> &i32 {
    let a = state.component();
    let (allocations_i, inner_i) = a.singleton_addr_map[&addr];
    &a.allocations.get(allocations_i).unwrap().singletons[inner_i].value
}

fn singleton_mut_ref_fn<S: HasComponent<Component>>(
    state: &mut S,
    addr: variable::Address,
) -> &mut i32 {
    let a = state.component_mut();
    let (allocations_i, inner_i) = a.singleton_addr_map[&addr];
    &mut a.allocations.get_mut(allocations_i).unwrap().singletons[inner_i].value
}

/// Get the `\newarray` execution command.
pub fn get_newarray<S: HasComponent<Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(newarray_primitive_fn)
}

fn newarray_primitive_fn<S: HasComponent<Component>>(
    newarray_token: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> anyhow::Result<()> {
    let name =
        parse::parse_command_target("newarray allocation", newarray_token, input.unexpanded())?;
    let len: usize = parse::parse_number(input)?;
    let addr = input.state_mut().component_mut().alloc_array(len);
    input.base_mut().commands_map.insert_variable_command(
        name,
        variable::Command::new(
            array_element_ref_fn,
            array_element_mut_ref_fn,
            variable::AddressSpec::DynamicVirtual(Box::new(ArrayAddressSpec(addr))),
        ),
        groupingmap::Scope::Local,
    );
    // TODO: Return the arraydef version
    Ok(())
}

struct ArrayAddressSpec(variable::Address);

impl<S: HasComponent<Component>> variable::DynamicAddressSpec<S> for ArrayAddressSpec {
    fn resolve(
        &self,
        token: texlang_core::token::Token,
        input: &mut vm::ExpansionInput<S>,
    ) -> anyhow::Result<variable::Address> {
        let array_addr = self.0;
        let array_index: usize = parse::parse_number(input)?;
        let (allocations_i, inner_i) = input.state().component().array_addr_map[&array_addr];
        let array_len = input.state().component().allocations[allocations_i].arrays[inner_i]
            .value
            .len();
        if array_index >= array_len {
            return Err(error::TokenError::new(
            token,
            format![
                "Array out of bounds: cannot access index {array_index} of array with length {array_len}"
            ],
        )
        .cast());
        }
        Ok(variable::Address(array_addr.0 + array_index))
    }
}

fn array_element_ref_fn<S: HasComponent<Component>>(state: &S, addr: variable::Address) -> &i32 {
    let (addr_0, (allocations_i, inner_i)) = state.component().find_array(addr);
    &state.component().allocations[allocations_i].arrays[inner_i].value[addr.0 - addr_0.0]
}

fn array_element_mut_ref_fn<S: HasComponent<Component>>(
    state: &mut S,
    addr: variable::Address,
) -> &mut i32 {
    let (addr_0, (allocations_i, inner_i)) = state.component().find_array(addr);
    &mut state
        .component_mut()
        .allocations
        .get_mut(allocations_i)
        .unwrap()
        .arrays[inner_i]
        .value[addr.0 - addr_0.0]
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::the::get_the;
    use crate::{script, testing::*};
    use texlang_core::vm::implement_has_component;

    #[derive(Default)]
    struct State {
        alloc: Component,
        exec: script::Component,
    }

    implement_has_component![State, (Component, alloc), (script::Component, exec),];

    fn initial_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        HashMap::from([
            ("newint", get_newint()),
            ("newarray", get_newarray()),
            ("the", get_the()),
        ])
    }

    test_suite![
        expansion_equality_tests(
            (newint_base_case, r"\newint\a \a=3 \the\a", "3"),
            (
                newarray_base_case_0,
                r"\newarray \a 3 \a 0 = 2 \the\a 0",
                "2"
            ),
            (
                newarray_base_case_1,
                r"\newarray \a 3 \a 1 = 2 \the\a 1",
                "2"
            ),
            (
                newarray_base_case_2,
                r"\newarray \a 3 \a 2 = 2 \the\a 2",
                "2"
            ),
        ),
        failure_tests(
            (newarray_out_of_bounds, r"\newarray \a 3 \a 3 = 2"),
            (newarray_negative_index, r"\newarray \a 3 \a -3 = 2"),
            (newarray_negative_length, r"\newarray \a -3"),
        ),
    ];
}
