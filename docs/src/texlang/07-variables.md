# TeX variables

The documentation so far has covered two of three type of primitives in TeX:
    expansion primitives and execution primtives.
In this section we discuss the third type: variables.

The TeX language includes typed variables.
There are a few possible types [which are listed below](#tex-variable-types).
The Texlang variables API is the mechanism by which TeX variables
    are implemented in Texlang.
Ultimately the API provides a way for memory in Rust to be reference in
    TeX source code.


## \[History\] Variables in the TeXBook

This section can be freely skipped.

The TeXBook talks a lot about the different variables that are available in
    the original TeX engine, like `\year` and `\count`.
The impression one sometimes gets from the TeXBook is that
    these variables are heterogeneous in terms of how they are handled by the interpreter.
For example, on page 271 when describing the grammar of the TeX language,
    we find the following rule:

```text
<internal integer> -> <integer parameter> | <special integer> | \lastpenalty
    <countdef token> | \count<8-bit number>
    ...
```

This makes it seems that an "integer parameter"
behaves differently to a "special integer", or to a register accessed via `\count`,
    even though all of these variables have the same concrete type (a 32 bit integer).
To the best of our current knowledge, this is not the case at all!
It appears that there is a uniform way to handle all variables of the same concrete type in TeX,
    and this is what Texlang does.
The benefit of this approach is that it makes for a much simpler API,
    both for adding new variables to a TeX engine or for
    consuming variables.


## Singleton versus array variables

In TeX, for each variable type, there are two categories of variable:
    singleton variables (like `\year`)
    and array variables (like `\count N`, where `N` is the index of the variable in the registers array).
Both of these cases are handled in the same way in the Texlang API.

In Texlang, the control sequences `\year` and  `\count` are not considered variables themselves.
This is because without reading the `N` after `\count`, we don't actually know which memory is being referred to.
Instead, `\year` and  `\count` are _variable commands_ (of type [`Command`]()).
A variable command is _resolved_ to obtain a variable (of type [`Variable`]()).
A variable is an object that points to a specific piece of memory like an `i32` in the state.

For singleton variables, resolving a command is a no-op.
The command itself has enough information to identify the memory being pointed at.

For array variables, resolving a command involves determining the index of the variable within the array.
The index has type [`Index`](), which is a wrapper around Rust's `usize`.
The index is determined using the command's index resolver, which has enum type [`IndexResolver`]().
There are a two different ways the index can be resolved, corresponding to different
    variants in the enum type.


## Implementing a singleton variable

Variables require some state in which to store the associated value.
We assume that the [component pattern](04-stateful-primitives.md) is being used.
In this case, the variable command is associated with a state struct which will be
    included in the VM state as a component.
The value of a variable is just a Rust field of the correct type in the component:

```rust
# extern crate texlang;
pub struct MyComponent {
    my_variable_value: i32
}
```

To make a Texlang variable out of this `i32` we need to provide two things:
  an immutable getter
  and a mutable getter.
These getters have the signature [`RefFn`]() and [`MutRefFn`]() respectively.
Both getters accept a reference to the state and an index, and return a reference to the variable.
(The index is only for array variables, and is ignored for singleton variables.)

For the component and variable above, our getters look like this:

```rust
# extern crate texlang;
# pub struct MyComponent {
#     my_variable_value: i32
# }
use texlang::vm::HasComponent;
use texlang::variable;

fn getter<S: HasComponent<MyComponent>>(state: &S, index: variable::Index) -> &i32 {
    &state.component().my_variable_value
}
fn mut_getter<S: HasComponent<MyComponent>>(state: &mut S, index: variable::Index) -> &mut i32 {
    &mut state.component_mut().my_variable_value
}
```
Once we have the getters, we can create the TeX variable command:

```rust
# extern crate texlang;
# pub struct MyComponent {
#     my_variable_value: i32
# }
# use texlang::vm::HasComponent;
# fn getter<S: HasComponent<MyComponent>>(state: &S, index: variable::Index) -> &i32 {
#    &state.component().my_variable_value
# }
# fn mut_getter<S: HasComponent<MyComponent>>(state: &mut S, index: variable::Index) -> &mut i32 {
#    &mut state.component_mut().my_variable_value
# }
# use texlang::variable;
use texlang::command;

pub fn my_variable<S: HasComponent<MyComponent>>() -> command::BuiltIn<S> {
    return variable::Command::new_singleton(
        getter,
        mut_getter,
    ).into()
}
```

The function [`Command::new_singleton`]() creates a new variable command associated to a singleton variable.
We cast the variable command into a generic command using the `into` method.
This command can now be included in the VM's command map and the value can be accessed in TeX scripts!

As usual with the component pattern, the code we write works for _any_ TeX engine
whose state contains our component.

Finally, as a matter of style, consider implementing the getters inline as closures.
This makes the code a little more compact and readable.
With this style, the full code listing is as follows:

```rust
# extern crate texlang;
use texlang::vm::HasComponent;
use texlang::variable;
use texlang::command;

pub struct MyComponent {
    my_variable_value: i32
}

pub fn my_variable<S: HasComponent<MyComponent>>() -> command::BuiltIn<S> {
    return variable::Command::new_singleton(
        |state: &S, index: variable::Index| -> &i32 {
            &state.component().my_variable_value
        },
        |state: &mut S, index: variable::Index| -> &mut i32 {
            &mut state.component_mut().my_variable_value
        },
    ).into()
}
```


## Implementing an array variable

The main difference between singleton and array variables is that we
    need to use the index arguments that were ignored above.

In this section we will implement an array variable with 10 entries.
In the component, we replace the `i32` with an array of `i32`s:
```rust
pub struct MyComponent {
    my_array_values: [i32; 10]
}
```

The getter functions use the provided index argument to determine the index to use for the array:
```rust
# extern crate texlang;
# pub struct MyComponent {
#     my_array_values: [i32; 10]
# }
# use texlang::vm::HasComponent;
# use texlang::variable;
fn getter<S: HasComponent<MyComponent>>(state: &S, index: variable::Index) -> &i32 {
    &state.component().my_array_values[index.0 as usize]
}
```

The above listing raises an important question: what if the array access is out of bounds?
The Rust code here will panic, and in Texlang this is the correct behavior.
Texlang always assumes that variable getters are infallible.
This is the same as assuming that an instantiated [`Variable`] type points to a valid piece of memory
and is not (say) dangling.

Next, we construct the command.
Unlike the singleton command, this command will need to figure out the index of the variable.
As with `\count`, our command will do this by reading the index from the input token stream.
In the variables API, we implement this by providing the following type of function:

```rust
# extern crate texlang;
use texlang::*;
use texlang::traits::*;

fn index<S: TexlangState>(token: token::Token, input: &mut vm::ExpandedStream<S>) -> command::Result<variable::Index> {
    let index = parse::Uint::<10>::parse(input)?;
    return Ok(index.0.into())
}
```

Finally we create the command.
This is the same as the singleton case, except we pass the index function above as an index resolver
with the `Dynamic` variant:

```rust
# extern crate texlang;
# use texlang::*;
# fn getter<S: HasComponent<MyComponent>>(state: &S, index: variable::Index) -> &i32 {
#   panic![""]
# }
# fn mut_getter<S: HasComponent<MyComponent>>(state: &mut S, index: variable::Index) -> &mut i32 {
#   panic![""]
# }
# fn index_resolver<S>(token: token::Token, input: &mut vm::ExpandedStream<S>) -> command::Result<variable::Index> {
#   panic![""]
# }
# pub struct MyComponent {
#     my_array_values: [i32; 10]
# }
# use texlang::vm::HasComponent;
pub fn my_array<S: HasComponent<MyComponent>>() -> command::BuiltIn<S> {
    return variable::Command::new_array(
        getter,
        mut_getter,
        variable::IndexResolver::Dynamic(index_resolver),
    ).into()
}
```


## Implementing a `\countdef` type command

In Knuth's TeX, the `\countdef` command is an execution command with the following semantics.
After executing the TeX code `\countdef \A 1`,
    the control sequence `\A` will be a variable command pointing to the same
    memory as `\count 1`.
One way of thinking about it is that `\A` aliases `\count 1`.

Using the Texlang variables API it is possible to implement the analogous command
for the `\myArray` command implemented above.
The implementation is in 3 steps:

1. The target (e.g. `\A`) is read using [`texlang::parse::Command::parse`]().

1. The index (e.g. `1`) is read using [`usize::parse`](), just like in the previous section.

1. A new variable command is then created and added to the commands map.
    This command is created using [`Command::new_array`] just as above, except in the index
    resolver we use the [`IndexResolver::Static`] variant with the index calculated in part 2.

For a full example where this is all worked out, consult
  [the implementation of `\countdef`](https://docs.rs/texlang-stdlib/latest/src/texlang_stdlib/registers.rs.html)
in the Texlang standard library.


## TeX variable types

Not all variable types have been implemented yet.

| Type      | Rust type      | Register accessor command | Implemented in Texlang?
|-----------|----------------|--------------------------|----------------
| Integer   | `i32`          | `\count`                 | Yes
| Dimension | TBD            | `\dimen`                 | No
| Glue      | TBD            | `\skip`                  | No
| Muglue    | TBD            | `\muskip`                | No
| Box       | TBD            | `\box` and `\setbox`     | No
| Category code  | [`CatCode`](super::token::CatCode) | `\catcode` | Yes
| Math code | TBD            | `\mathcode`              | No
| Delimiter code | TBD       | `\delcode`               | No
| Space factor code | TBD    | `\sfcode`                | No
| Token list | `Vec<`[`Token`](super::token::Token)`>`    | `\toks`                | No
