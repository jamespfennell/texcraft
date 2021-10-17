//! Allocation of variables and arrays
//!
//! This module contains implementions of brand new Texcraft commands
//! `\newint` and `\newarray` which perform dynamic memory allocation.

use crate::datastructures::nevec::Nevec;
use crate::tex::parse;
use crate::tex::prelude::*;
use crate::tex::variable::{TypedVariable, Variable};
use std::collections::{BTreeMap, HashMap};
use std::ops::Bound::Included;

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
pub struct Component {
    stack: Nevec<Stack>,

    // Map from addr to (stack index, stack.singletons index)
    singleton_addr_map: HashMap<usize, (usize, usize)>,
    next_singleton_addr: usize,

    // Map from addr to (stack index, stack.singletons index)
    //
    // Note that addr points to the first element of the array; when
    // we add a new array, addr is incremented by the size of the array.
    // addr + i then references the ith element of the array, for
    // appropriate i. We use a BTreeMap to make it fast to obtain the
    // indices from addr + i for any i.
    array_addr_map: BTreeMap<usize, (usize, usize)>,
    next_array_addr: usize,
}

struct Stack {
    // TODO: rather than using a Vec of Vecs it may be more efficient
    // to use a single global Vec and a single non-global Vec.
    singletons: Vec<Singleton>,
    arrays: Vec<Array>,
}

struct Singleton {
    addr: usize,
    value: i32,
}

struct Array {
    addr: usize,
    value: Vec<i32>,
}

/// Trait for a state that has an alloc component.
pub trait HasAlloc {
    /// Get a reference to the allocs component.
    fn alloc(&self) -> &Component;

    /// Get a mutable reference to the allocs component.
    fn alloc_mut(&mut self) -> &mut Component;
}

impl Component {
    /// Create a new alloc component.
    pub fn new() -> Component {
        Component {
            stack: Nevec::new(Stack {
                singletons: vec![],
                arrays: vec![],
            }),
            singleton_addr_map: HashMap::new(),
            next_singleton_addr: 0,
            array_addr_map: BTreeMap::new(),
            next_array_addr: 0,
        }
    }

    fn alloc_int(&mut self) -> usize {
        let i = (self.stack.len() - 1, self.stack.last().singletons.len());
        self.stack.last_mut().singletons.push(Singleton {
            addr: self.next_singleton_addr,
            value: 0,
        });
        self.singleton_addr_map.insert(self.next_singleton_addr, i);
        self.next_singleton_addr += 1;
        self.next_singleton_addr - 1
    }

    fn alloc_array(&mut self, len: usize) -> usize {
        let i = (self.stack.len() - 1, self.stack.last().arrays.len());
        self.stack.last_mut().arrays.push(Array {
            addr: self.next_array_addr,
            value: vec![0; len],
        });
        self.array_addr_map.insert(self.next_array_addr, i);
        self.next_array_addr += len;
        self.next_array_addr - len
    }

    fn find_array(&self, elem_addr: usize) -> (usize, (usize, usize)) {
        let (a, (b, c)) = self
            .array_addr_map
            .range((Included(&0), Included(&elem_addr)))
            .rev()
            .next()
            .unwrap();
        (*a, (*b, *c))
    }

    pub fn begin_group(&mut self) {
        self.stack.push(Stack {
            singletons: vec![],
            arrays: vec![],
        });
    }

    pub fn end_group(&mut self) {
        let stack = self.stack.pop_from_tail().unwrap();
        for singleton in &stack.singletons {
            self.singleton_addr_map.remove(&singleton.addr);
        }
        for array in &stack.arrays {
            self.array_addr_map.remove(&array.addr);
        }
    }
}

impl Default for Component {
    fn default() -> Self {
        Self::new()
    }
}

/// Get the `\newint` exeuction command.
pub fn get_newint<S: HasAlloc>() -> command::ExecutionFn<S> {
    newint_primitive_fn
}

fn newint_primitive_fn<S: HasAlloc>(
    newint_token: Token,
    input: &mut command::ExecutionInput<S>,
) -> anyhow::Result<()> {
    let name =
        parse::parse_command_target("newint allocation", newint_token, input.unexpanded_stream())?;
    let addr = input.state_mut().alloc_mut().alloc_int();
    input
        .base_mut()
        .primitives
        .insert(name, command::VariableCommand(singleton_fn, addr));
    Ok(())
}

fn singleton_fn<S: HasAlloc>(
    _: Token,
    _: &mut command::ExpandedInput<S>,
    addr: usize,
) -> anyhow::Result<Variable<S>> {
    Ok(Variable::Int(TypedVariable::new(
        singleton_ref_fn,
        singleton_mut_ref_fn,
        addr,
    )))
}

fn singleton_ref_fn<S: HasAlloc>(state: &S, addr: usize) -> &i32 {
    let a = state.alloc();
    let (stack_i, inner_i) = a.singleton_addr_map[&addr];
    &a.stack.get(stack_i).unwrap().singletons[inner_i].value
}

fn singleton_mut_ref_fn<S: HasAlloc>(state: &mut S, addr: usize) -> &mut i32 {
    let a = state.alloc_mut();
    let (stack_i, inner_i) = a.singleton_addr_map[&addr];
    &mut a.stack.get_mut(stack_i).unwrap().singletons[inner_i].value
}

/// Get the `\newarray` execution command.
pub fn get_newarray<S: HasAlloc>() -> command::ExecutionFn<S> {
    newarray_primitive_fn
}

fn newarray_primitive_fn<S: HasAlloc>(
    newarray_token: Token,
    input: &mut command::ExecutionInput<S>,
) -> anyhow::Result<()> {
    let name = parse::parse_command_target(
        "newarray allocation",
        newarray_token,
        input.unexpanded_stream(),
    )?;
    let len: usize = parse::parse_number(input.regular())?;
    let addr = input.state_mut().alloc_mut().alloc_array(len);
    input
        .base_mut()
        .primitives
        .insert(name, command::VariableCommand(array_fn, addr));
    // TODO: Return the arraydef version
    Ok(())
}

/// Variable command function for commands defined using \newarray.
fn array_fn<S: HasAlloc>(
    array_token: Token,
    input: &mut ExpandedInput<S>,
    array_addr: usize,
) -> anyhow::Result<Variable<S>> {
    let array_index: usize = parse::parse_number(input)?;
    let (stack_i, inner_i) = input.state().alloc().array_addr_map[&array_addr];
    let array_len = input.state().alloc().stack[stack_i].arrays[inner_i]
        .value
        .len();
    if array_index >= array_len {
        return Err(error::TokenError::new(
            array_token,
            format![
                "Array out of bounds: cannot access index {} of array with length {}",
                array_index, array_len,
            ],
        )
        .cast());
    }
    Ok(Variable::Int(TypedVariable::new(
        array_element_ref_fn,
        array_element_mut_ref_fn,
        array_addr + array_index,
    )))
}

fn array_element_ref_fn<S: HasAlloc>(state: &S, addr: usize) -> &i32 {
    let (addr_0, (stack_i, inner_i)) = state.alloc().find_array(addr);
    &state.alloc().stack[stack_i].arrays[inner_i].value[addr - addr_0]
}

fn array_element_mut_ref_fn<S: HasAlloc>(state: &mut S, addr: usize) -> &mut i32 {
    let (addr_0, (stack_i, inner_i)) = state.alloc().find_array(addr);
    &mut state.alloc_mut().stack.get_mut(stack_i).unwrap().arrays[inner_i].value[addr - addr_0]
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::tex::command::library::the::get_the;
    use crate::tex::driver;
    use crate::tex::token::catcode;

    struct State {
        alloc: Component,
    }

    // impl_has_component[State,
    // alloc::HasAlloc, alloc::Component, alloc, alloc_mut, self.alloc]
    impl HasAlloc for State {
        fn alloc(&self) -> &Component {
            &self.alloc
        }

        fn alloc_mut(&mut self) -> &mut Component {
            &mut self.alloc
        }
    }

    fn new_state() -> State {
        State {
            alloc: Component::new(),
        }
    }

    fn setup_expansion_test(s: &mut Base<State>) {
        s.set_command("newint", get_newint());
        s.set_command("newarray", get_newarray());
        s.set_command("the", get_the());
    }

    expansion_test![newint_base_case, r"\newint\a \a=3 \the\a", "3"];

    expansion_test![
        newarray_base_case_0,
        r"\newarray \a 3 \a 0 = 2 \the\a 0",
        "2"
    ];
    expansion_test![
        newarray_base_case_1,
        r"\newarray \a 3 \a 1 = 2 \the\a 1",
        "2"
    ];
    expansion_test![
        newarray_base_case_2,
        r"\newarray \a 3 \a 2 = 2 \the\a 2",
        "2"
    ];
    expansion_failure_test![newarray_out_of_bounds, r"\newarray \a 3 \a 3 = 2"];
    expansion_failure_test![newarray_negative_index, r"\newarray \a 3 \a -3 = 2"];
    expansion_failure_test![newarray_negative_length, r"\newarray \a -3"];
}
