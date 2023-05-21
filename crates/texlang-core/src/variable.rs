//! Texlang variables API
//!
//! # Texlang variables API
//!
//! The TeX language includes typed variables.
//! There are a few possible types, which are [listed below](#tex-variable-types).
//! Texlang currently supports only a subset of these types,
//!     but has been designed so that it will be easy to support the other types in the future.
//!
//! This documentation is targeted at users of Texlang
//!     who want to add a new variable to their TeX engine,
//!     or consume variables that have already been defined.
//!
//! ## Historical note on variables in the TeXBook
//!
//! This section is not needed to understand how to use Texlang's variables API
//!     and can be freely skipped.
//!
//! The TeXBook talks a lot about the different variables that are available in
//!     the original TeX engine, like `\year` and `\count`.
//! The impression one sometimes gets from the TeXBook is that
//!     these variables are heterogeneous in terms of how they are handled by the interpreter.
//! For example, on page 271 when describing the grammar of the TeX language,
//!     we find the following rule:
//!
//! ```text
//! <internal integer> -> <integer parameter> | <special integer> | \lastpenalty
//!     <countdef token> | \count<8-bit number>
//!     ...
//! ```
//!
//! This makes it seems that an "integer parameter"
//! behaves differently to a "special integer", or to a register accessed via `\count`,
//!     even though all of these variables have the same concrete type (a 32 bit integer).
//! To the best of our current knowledge, this is not the case at all!
//! It appears that there is a uniform way to handle all variables of the same concrete type in TeX,
//!     and this is what Texlang does.
//! The benefit of this approach is that it makes for a much simpler API,
//!     both for adding new variables to a TeX engine or for
//!     consuming variables.
//!
//! ## The design of the Texlang variables API
//!
//! The TeXBook makes one distinction among variables which is
//!     important and actually reflective of the TeX grammar.
//! This is the distinction between singleton variables (like `\year`)
//!     and array variables (like `\count N`, where N is the index of the variable in the registers array).
//! Both of these cases are handled in the same way in the variables API here.
//!
//! In Texlang, the control sequences `\year` and  `\count` are not considered variables themselves.
//! This is because without reading the `N` after `\count`, we don't actually know which memory is being referred to.
//! Instead, `\year` and  `\count` are _variable commands_ (of type [Command]).
//! A variable command is _resolved_ to obtain a variable (of type [Variable]).
//! A variable is an object that points to a specific piece of memory like an `i32` in the state.
//!
//! For singleton variables, resolving a command essentially does nothing.
//! The command itself has enough information to identify the memory being pointed at.
//!
//! For array variables, resolving a command involves determining the index of the variable within the array.
//! The index has type [Index].
//! The index is determined using the command's index resolver, which has enum type [IndexResolver].
//! There are a few different ways the index can be figured out, corresponding to different
//!     variants in the enum type.
//!
//! The next sections in this document move out of theory land,
//!     and demonstrate with code how to implement singleton and array variables.
//!
//! ## Implementing a singleton variable
//!
//! Variables require some state in which to store the associated value.
//! We assume that the [component pattern](crate::vm::HasComponent) is being used.
//! In this case, the variable command is associated with a state struct which will be
//!     included in the VM state as a component.
//! The value of a variable is just a Rust field of the correct type in the component:
//! ```
//! pub struct MyComponent {
//!     my_variable_value: i32
//! }
//! ```
//!
//! To make a Texlang variable out of this `i32` we need to provide an immutable getter
//! and a mutable getter.
//! These getters have the signature [RefFn] and [MutRefFn] respectively.
//! Both getters accept a reference to the state and an index, and return a reference to the variable.
//! (The getters also contain an index argument, which is ignored for singleton variables.)
//!
//! For the component and variable above, our getters look like this:
//!
//! ```
//! # pub struct MyComponent {
//! #     my_variable_value: i32
//! # }
//! use texlang_core::vm::HasComponent;
//! use texlang_core::variable;
//!
//! fn getter<S: HasComponent<MyComponent>>(state: &S, index: variable::Index) -> &i32 {
//!     &state.component().my_variable_value
//! }
//! fn mut_getter<S: HasComponent<MyComponent>>(state: &mut S, index: variable::Index) -> &mut i32 {
//!     &mut state.component_mut().my_variable_value
//! }
//! ```
//! Once we have the getters, we can create the TeX variable command:
//!
//! ```
//! # pub struct MyComponent {
//! #     my_variable_value: i32
//! # }
//! # use texlang_core::vm::HasComponent;
//! # fn getter<S: HasComponent<MyComponent>>(state: &S, index: variable::Index) -> &i32 {
//! #    &state.component().my_variable_value
//! # }
//! # fn mut_getter<S: HasComponent<MyComponent>>(state: &mut S, index: variable::Index) -> &mut i32 {
//! #    &mut state.component_mut().my_variable_value
//! # }
//! use texlang_core::variable;
//! use texlang_core::command;
//!
//! pub fn my_variable<S: HasComponent<MyComponent>>() -> command::BuiltIn<S> {
//!     return variable::Command::new_singleton(
//!         getter,
//!         mut_getter,
//!     ).into()
//! }
//! ```
//!
//! The function [Command::new_singleton] creates a new variable command associated to a singleton variable.
//! We cast the variable command into a generic command using the `into` method.
//! This command can now be included in the VM's command map and the value can be accessed in TeX scripts!
//!
//! As usual with the component pattern, the code we write works for _any_ TeX engine
//! whose state contains our component.
//!
//! Finally, as a matter of style, consider implementing the getters inline as closures.
//! This makes the code a little more compact and readable.
//! With this style, the full code listing is as follows:
//!
//! ```
//! use texlang_core::vm::HasComponent;
//! use texlang_core::variable;
//! use texlang_core::command;
//!
//! pub struct MyComponent {
//!     my_variable_value: i32
//! }
//!
//! pub fn my_variable<S: HasComponent<MyComponent>>() -> command::BuiltIn<S> {
//!     return variable::Command::new_singleton(
//!         |state: &S, index: variable::Index| -> &i32 {
//!             &state.component().my_variable_value
//!         },
//!         |state: &mut S, index: variable::Index| -> &mut i32 {
//!             &mut state.component_mut().my_variable_value
//!         },
//!     ).into()
//! }
//! ```
//!
//!
//! ## Implementing an array variable
//!
//! The main difference between singleton and array variables is that we
//!     need to use the index arguments that were ignored above.
//!
//! In this section we will implement an array variable with 10 entries.
//! In the component, we replace the `i32` with an array of `i32`s:
//! ```
//! pub struct MyComponent {
//!     my_array_values: [i32; 10]
//! }
//! ```
//!
//! The getter functions use the provided index argument to determine the index to use for the array:
//! ```
//! # pub struct MyComponent {
//! #     my_array_values: [i32; 10]
//! # }
//! # use texlang_core::vm::HasComponent;
//! # use texlang_core::variable;
//! fn getter<S: HasComponent<MyComponent>>(state: &S, index: variable::Index) -> &i32 {
//!     &state.component().my_array_values[index.0 as usize]
//! }
//! ```
//!
//! The above listing raises an important question: what if the array access is out of bounds?
//! The Rust code here will panic, and in Texlang this is the correct behavior.
//! Texlang always assumes that variable getters are infallible.
//! This is the same as assuming that an instantiated [Variable] type points to a valid piece of memory
//! and is not (say) dangling.
//!
//! Next, we construct the command.
//! Unlike the singleton command, this command will need to figure out the index of the variable.
//! As with `\count`, our command will do this by reading the index from the input token stream.
//! In the variables API, we implement this by providing the following type of function:
//!
//! ```
//! use texlang_core::{parse, token, traits::*, variable, vm};
//! use anyhow;
//!
//! fn index<S: TexlangState>(token: token::Token, input: &mut vm::ExpandedStream<S>) -> anyhow::Result<variable::Index> {
//!     let index: usize = parse::parse_number(input)?;
//!     if index >= 10 {
//!         // for simplicity we panic, but in real code we should return an error
//!         panic!["out of bounds"]
//!     }
//!     return Ok(index.into())
//! }
//! ```
//!
//! Finally we create the command.
//! This is the same as the singleton case, except we pass the index function above as an index resolver
//! with the `Dynamic` variant:
//!
//! ```
//! # use texlang_core::{command, parse, token, variable, vm};
//! # use anyhow;
//! # fn getter<S: HasComponent<MyComponent>>(state: &S, index: variable::Index) -> &i32 {
//! #   panic![""]
//! # }
//! # fn mut_getter<S: HasComponent<MyComponent>>(state: &mut S, index: variable::Index) -> &mut i32 {
//! #   panic![""]
//! # }
//! # fn index_resolver<S>(token: token::Token, input: &mut vm::ExpandedStream<S>) -> anyhow::Result<variable::Index> {
//! #   panic![""]
//! # }
//! # pub struct MyComponent {
//! #     my_array_values: [i32; 10]
//! # }
//! # use texlang_core::vm::HasComponent;
//! pub fn my_array<S: HasComponent<MyComponent>>() -> command::BuiltIn<S> {
//!     return variable::Command::new_array(
//!         getter,
//!         mut_getter,
//!         variable::IndexResolver::Dynamic(index_resolver),
//!     ).into()
//! }
//! ```
//!
//! ## Implementing a `\countdef` type command
//!
//! In Knuth's TeX, the `\countdef` command is an execution command with the following semantics.
//! After executing the TeX code `\countdef \A 1`,
//!     the control sequence `\A` will be a variable command pointing to the same
//!     memory as `\count 1`.
//! One way of thinking about it is that `\A` aliases `\count 1`.
//!
//! Using the Texlang variables API it is possible to implement the analogous command
//! for the `\myarray` command implemented above.
//! The implementation is in 3 steps:
//!
//! 1. The target (e.g. `\A`) is read using [crate::parse::parse_command_target].
//!
//! 1. The index (e.g. `1`) is read using [crate::parse::parse_number], just like in the previous section.
//!
//! 1. A new variable command is then created and added to the commands map.
//!     This command is created using [Command::new_array] just as above, except in the index
//!     resolver we use the [IndexResolver::Static] variant with the index calculated in part 2.
//!
//! For a full example where this is all worked out, consult the implementation of `\countdef`
//! in the Texlang standard library.
//!
//! ## TeX variable types
//!
//! | Type      | Rust type      | Register accessor command | Texcraft status
//! |-----------|----------------|--------------------------|----------------
//! | Integer   | [i32]          | `\count`                 | Implemented
//! | Dimension | TBD            | `\dimen`                 | Not implemented
//! | Glue      | TBD            | `\skip`                  | Not implemented
//! | Muglue    | TBD            | `\muskip`                | Not implemented
//! | Box       | TBD            | `\box` and `\setbox`     | Not implemented
//! | Category code  | [CatCode](super::token::catcode::CatCode) | `\catcode` | Implemented
//! | Math code | TBD            | `\mathcode`              | Not implemented
//! | Delimiter code | TBD       | `\delcode`               | Not implemented
//! | Space factor code | TBD    | `\sfcode`                | Not implemented
//! | Token list | TBD, but presumably a [Vec] of [Tokens](super::token::Token)    | `\toks`                | Not implemented
//!

use crate::token::catcode::CatCode;
use crate::traits::*;
use crate::vm;
use crate::{parse, token};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use texcraft_stdext::collections::groupingmap;

/// Function signature for a variable's immutable getter.
///
/// In Texcraft all [Variable]s are built from an immutable getter and a mutable getter.
/// This type alias just defines the signature of immutable getters.
/// The first argument is the state, and the second argument is the index of the variable.
pub type RefFn<S, T> = fn(state: &S, index: Index) -> &T;

/// Function signature for a variable's mutable getters.
///
/// In Texcraft all [Variable]s are built from an immutable getter and a mutable getter.
/// This type alias just defines the signature of mutable getters.
/// The first argument is the state, and the second argument is the index of the variable.
pub type MutRefFn<S, T> = fn(state: &mut S, index: Index) -> &mut T;

/// Index of a variable within an array.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Index(pub usize);

impl From<usize> for Index {
    fn from(value: usize) -> Self {
        Index(value)
    }
}

/// Specification for how the index of an array variable is determined.
///
/// Obtaining a variable from a command involves determining the variable's [Index].
/// This index is ultimately passed into the variable's getters to get a reference or mutable reference
/// to the underlying Rust value.
pub enum IndexResolver<S> {
    /// A static index, provided in the enum variant.
    ///
    /// This resolver is used for commands that point to a specific entry in a array.
    /// For example, after executing `\countdef\A 30`, the `\A` control sequence points
    /// at the count register with index 30.
    /// The command backing `\A` uses a static resolver with index 30.
    Static(Index),
    /// A dynamic index that is determined by reading the input token stream.
    ///
    /// For example, in `\count 4` the index of `4` is determined by parsing a number
    /// from the input token stream.
    Dynamic(fn(token::Token, &mut vm::ExpandedStream<S>) -> anyhow::Result<Index>),
    /// A dynamic index, but determined using virtual method dispatch.
    ///
    /// This method is more flexible than the Dynamic variant, but less performant.
    DynamicVirtual(Box<dyn DynamicIndexResolver<S>>),
}

/// Trait used for dynamically determining an index using virtual method dispatch.
pub trait DynamicIndexResolver<S> {
    /// Determine the index of a variable.
    fn resolve(
        &self,
        token: token::Token,
        input: &mut vm::ExpandedStream<S>,
    ) -> anyhow::Result<Index>;
}

impl<S> IndexResolver<S> {
    /// Determine the index of a variable using the input token stream.
    fn resolve(
        &self,
        token: token::Token,
        input: &mut vm::ExpandedStream<S>,
    ) -> anyhow::Result<Index> {
        match self {
            IndexResolver::Static(addr) => Ok(*addr),
            IndexResolver::Dynamic(f) => f(token, input),
            IndexResolver::DynamicVirtual(v) => v.resolve(token, input),
        }
    }
}

/// A TeX variable command.
///
/// Variable commands are _resolved_ to obtain a [Variable].
///
/// A command consists of three parts.
///
/// The first two parts are an immutable getter (of type [RefFn])
/// and a mutable getter (of type [MutRefFn]).
/// Both of these getters accept a reference to the state (where the variable's value lives)
/// and an [Index].
/// The index can be used by the getters to return different values in memory.
/// For example, if the getters read from an array, the index may be just the index in the array.
/// This mechanism allows users of Texlang to write one pair of getters that can be used in many variables.
///
/// The third part of a command is an [IndexResolver] that is used to determine the aforementioned index
/// of a variable at runtime.
/// The process of resolving a command involves determining the [Index] and returning a [Variable] type,
/// which under the hood is just the two getters and this index.
pub struct Command<S> {
    getters: Getters<S>,
    index_resolver: Option<IndexResolver<S>>,
}

impl<S> Command<S> {
    /// Create a new variable command.
    pub fn new_singleton<T: SupportedType>(
        ref_fn: RefFn<S, T>,
        ref_mut_fn: MutRefFn<S, T>,
    ) -> Command<S> {
        SupportedType::new_command(ref_fn, ref_mut_fn, None)
    }

    /// Create a new variable command.
    pub fn new_array<T: SupportedType>(
        ref_fn: RefFn<S, T>,
        ref_mut_fn: MutRefFn<S, T>,
        index_resolver: IndexResolver<S>,
    ) -> Command<S> {
        SupportedType::new_command(ref_fn, ref_mut_fn, Some(index_resolver))
    }

    /// Resolve the command to obtain a [Variable].
    pub fn resolve(
        &self,
        token: token::Token,
        input: &mut vm::ExpandedStream<S>,
    ) -> anyhow::Result<Variable<S>> {
        let index = match &self.index_resolver {
            None => Index(0),
            Some(index_resolver) => index_resolver.resolve(token, input)?,
        };
        Ok(match self.getters {
            Getters::Int(a, b) => Variable::Int(TypedVariable(a, b, index)),
            Getters::CatCode(a, b) => Variable::CatCode(TypedVariable(a, b, index)),
        })
    }

    pub(crate) fn getter_key(&self) -> GetterKey {
        self.getters.key()
    }

    pub(crate) fn index_resolver(&self) -> &Option<IndexResolver<S>> {
        &self.index_resolver
    }
}

impl<S: TexlangState> Command<S> {
    /// Resolve the command to a variable and return the value of the variable.
    pub fn value<'a>(
        &self,
        token: token::Token,
        input: &'a mut vm::ExpandedStream<S>,
    ) -> anyhow::Result<ValueRef<'a>> {
        Ok(self.resolve(token, input)?.value(input))
    }

    /// Resolve the command to a variable and set the value of the variable using the following tokens in the input stream.
    ///
    /// This function is used in TeX code like `\variable = 3`.
    /// In this case `\variable` is a command which resolves to a variable without consuming any more input.
    /// The variable is populated using the input `= 3` that follows.
    pub fn set_value_using_input(
        &self,
        token: token::Token,
        input: &mut vm::ExecutionInput<S>,
        scope: groupingmap::Scope,
    ) -> anyhow::Result<()> {
        self.resolve(token, input.as_mut())?
            .set_value_using_input(input, scope)
    }
}

/// Immutable reference to the value of a variable.
pub enum ValueRef<'a> {
    Int(&'a i32),
    CatCode(&'a CatCode),
}

/// TeX variable of any type.
///
/// A variable uniquely identifies a Rust value in the state, like an `i32`.
/// Operations on this value (like reading or setting the value) can be done in two ways:
///
/// 1. (Easy, less flexible) Use the methods directly on this type like [Variable::value]
///     or [Variable::set_value_using_input] to read or set the value.
///     These methods are really ergonomic.
///     The problem with the value method specifically is that the result
///     is a reference which keeps the borrow of the state alive.
///     Thus, while holding onto the result of the value, you can't do anything this the
///     input stream like reading an argument.
///     This is especially a problem when you need to perform a different action depending on the concrete type of the variable.
///     
/// 2. (Trickier, more flexible) Match on the type's enum variants to determine the
///     concrete type of the variable.
///     The [TypedVariable] value obtained in this way can be used to perform operations on the value.
///     The main benefit of this approach is that after matching on the type, you can still use the input
///     stream to do things because there is not borrow alive.
///     
pub enum Variable<S> {
    Int(TypedVariable<S, i32>),
    CatCode(TypedVariable<S, CatCode>),
}

impl<S: TexlangState> Variable<S> {
    /// Return a reference to the value of the variable.
    pub fn value<'a>(&self, input: &'a mut vm::ExpandedStream<S>) -> ValueRef<'a> {
        match self {
            Variable::Int(variable) => ValueRef::Int(variable.value(input.state())),
            Variable::CatCode(variable) => ValueRef::CatCode(variable.value(input.state())),
        }
    }

    /// Set the value of a variable using the following tokens in the input stream.
    pub fn set_value_using_input(
        &self,
        input: &mut vm::ExecutionInput<S>,
        scope: groupingmap::Scope,
    ) -> anyhow::Result<()> {
        parse::parse_optional_equals(input)?;
        match self {
            Variable::Int(variable) => {
                let value: i32 = parse::parse_number(input)?;
                *variable.value_mut(input, scope) = value;
            }
            Variable::CatCode(variable) => {
                let value = parse::parse_catcode(input)?;
                *variable.value_mut(input, scope) = value;
            }
        };
        Ok(())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct GetterKey(usize, usize);

enum Getters<S> {
    Int(RefFn<S, i32>, MutRefFn<S, i32>),
    CatCode(RefFn<S, CatCode>, MutRefFn<S, CatCode>),
}

impl<S> Getters<S> {
    fn key(&self) -> GetterKey {
        match self {
            Getters::Int(a, b) => GetterKey(*a as usize, *b as usize),
            Getters::CatCode(a, b) => GetterKey(*a as usize, *b as usize),
        }
    }
}

/// A TeX variable of a specific Rust type `T`.
pub struct TypedVariable<S, T>(RefFn<S, T>, MutRefFn<S, T>, Index);

impl<S, T> Copy for TypedVariable<S, T> {}

impl<S, T> Clone for TypedVariable<S, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<S, T> TypedVariable<S, T> {
    /// Returns an immutable reference to the variable's value.
    pub fn value<'a>(&self, state: &'a S) -> &'a T {
        (self.0)(state, self.2)
    }

    fn key(&self) -> (usize, usize, Index) {
        (self.0 as usize, self.1 as usize, self.2)
    }
}

impl<S, T> TypedVariable<S, T>
where
    S: TexlangState,
    T: Copy + SupportedType,
{
    /// Returns a mutable reference to the variable's value. This method is used for regular non-base variables.
    ///
    /// The input and scope must be passed because of TeX's grouping semantics.
    /// When the current group ends, any variable assignments made in the group
    ///     are rolled back.
    /// Thus this function generally saves the current value of the variable in the VM so that it
    ///     can be restored later.
    /// This is why the full input must be provided, and not just the state.
    ///
    /// **Important note**: this function assumes that the mutable reference is being
    ///     requested in order to assign to the value.
    /// If no assignment happens, the variable may be in a weird state afterwards.
    /// For example, calling the function with the global scope will erase all non-global
    ///     values for the variable, even if no assignment occurs.
    /// To just read the variable, use [TypedVariable::value] instead.
    pub fn value_mut<'a>(
        &self,
        input: &'a mut vm::ExecutionInput<S>,
        scope: groupingmap::Scope,
    ) -> &'a mut T {
        let current_value = *(self.0)(input.state(), self.2);
        match scope {
            groupingmap::Scope::Global => {
                for group in input.groups() {
                    SupportedType::restore_map_mut(group).remove(self)
                }
            }
            groupingmap::Scope::Local => {
                if let Some((group, _, _)) = input.current_group_mut() {
                    SupportedType::restore_map_mut(group).save(*self, current_value);
                }
            }
        }
        (self.1)(input.state_mut(), self.2)
    }
}

impl<S, T> PartialEq for TypedVariable<S, T> {
    fn eq(&self, rhs: &TypedVariable<S, T>) -> bool {
        self.key() == rhs.key()
    }
}

impl<S, T> Eq for TypedVariable<S, T> {}

impl<S, T> Hash for TypedVariable<S, T> {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.key().hash(state);
    }
}

/// Trait satisfied by all Rust types that can be used as TeX variables.
///
/// It exists to make the variables API more ergonomic;
///     for example, it is used to provide a uniform constructor [Command::new_array] for commands.
pub trait SupportedType: Sized {
    /// This method exists so that the trait cannot be be implemented without changing the variable enum.
    fn new_variable<S>(ref_fn: RefFn<S, Self>, mut_fn: MutRefFn<S, Self>) -> Variable<S>;

    fn restore_map_mut<S>(_: &mut RestoreValues<S>) -> &mut RestoreMap<S, Self>;

    fn new_command<S>(
        ref_fn: RefFn<S, Self>,
        ref_mut_fn: MutRefFn<S, Self>,
        _: Option<IndexResolver<S>>,
    ) -> Command<S>;
}

impl SupportedType for i32 {
    fn new_variable<S>(ref_fn: RefFn<S, Self>, mut_fn: MutRefFn<S, Self>) -> Variable<S> {
        Variable::Int(TypedVariable(ref_fn, mut_fn, Index(0)))
    }

    fn restore_map_mut<S>(m: &mut RestoreValues<S>) -> &mut RestoreMap<S, Self> {
        &mut m.i32
    }
    fn new_command<S>(
        ref_fn: RefFn<S, Self>,
        ref_mut_fn: MutRefFn<S, Self>,
        index_resolver: Option<IndexResolver<S>>,
    ) -> Command<S> {
        Command {
            getters: Getters::Int(ref_fn, ref_mut_fn),
            index_resolver,
        }
    }
}

impl SupportedType for CatCode {
    fn new_variable<S>(ref_fn: RefFn<S, Self>, mut_fn: MutRefFn<S, Self>) -> Variable<S> {
        Variable::CatCode(TypedVariable(ref_fn, mut_fn, Index(0)))
    }

    fn restore_map_mut<S>(m: &mut RestoreValues<S>) -> &mut RestoreMap<S, Self> {
        &mut m.catcode
    }

    fn new_command<S>(
        ref_fn: RefFn<S, Self>,
        ref_mut_fn: MutRefFn<S, Self>,
        index_resolver: Option<IndexResolver<S>>,
    ) -> Command<S> {
        Command {
            getters: Getters::CatCode(ref_fn, ref_mut_fn),
            index_resolver,
        }
    }
}

/// Internal VM data structure used to implement TeX's grouping semantics.
///
/// This is not intended to be used outside of the VM.
// TODO: make this private somehow. Maybe the restore values doesn't need to be a part of the SupportedType trait.
pub struct RestoreValues<S> {
    i32: RestoreMap<S, i32>,
    catcode: RestoreMap<S, CatCode>,
}

impl<S> Default for RestoreValues<S> {
    fn default() -> Self {
        Self {
            i32: Default::default(),
            catcode: Default::default(),
        }
    }
}

impl<S> RestoreValues<S> {
    pub fn restore(self, state: &mut S) {
        self.i32.restore(state);
        self.catcode.restore(state);
    }
}

/// Internal VM data structure used to implement TeX's grouping semantics.
///
/// This is not intended to be used outside of the VM.
pub struct RestoreMap<S, T>(HashMap<TypedVariable<S, T>, T>);

impl<S, T> Default for RestoreMap<S, T> {
    fn default() -> Self {
        Self(HashMap::new())
    }
}

impl<S, T> RestoreMap<S, T> {
    pub(super) fn save(&mut self, variable: TypedVariable<S, T>, val: T) {
        self.0.entry(variable).or_insert(val);
    }

    pub(super) fn remove(&mut self, variable: &TypedVariable<S, T>) {
        self.0.remove(variable);
    }

    fn restore(self, state: &mut S) {
        for (v, restored_value) in self.0 {
            *(v.1)(state, v.2) = restored_value;
        }
    }
}
