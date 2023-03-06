//! Texlang variables API
//!
//! # Texlang variables API
//!
//! The TeX language includes typed variables.
//! There are a few possible types, which are [listed below](#tex-variable-types).
//! Texlang currently supports only a subset of these types,
//!     but has been designed so that it will be easy to support the other types in the near future.
//! 
//! This documentatation is targetted at users of Texlang
//!     who want to add a new variable to their TeX engine,
//!     or consume variables already defined.
//! 
//! ## Historical note on variables in the TeXBook
//! 
//! This section is not needed to understand how to use Texlang's variables API
//!     and can be freely skipped.
//! 
//! The TeXBook talks a lot about the different variables that are available in 
//!     the original TeX engine, like `\linepenalty` and `\count`.
//! The impression one sometimes gets from the TeXBook is that 
//!     these variables are highly heterogeneous in term of how they interact with the wider language.
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
//! To the best of our current knowledge, this is not the case at all.
//! It appears that there is a uniform way to handle all variables of the same concrete type in TeX,
//!     and this is what Texlang does.
//! The benefit of this approach is that it makes for a much simpler API,
//!     both for
//!     people who are adding variables to their TeX engines and for
//!     code that needs to consume these variables.
//! 
//! 
//! ## The design of the Texlang variables API
//! 
//! The TeXBook does make one distinction among variables which is
//!     important and actually reflective of the TeX grammar.
//! This is the distinction between singleton variables (like `\linepenalty`)
//!     and array variables (like `\count N`, where N is in the index in the array).
//! In the Texlang variables API, both of these cases are handled uniformly.
//! This is done by employing an indirection.
//! 
//! In Texlang, the control sequences `\linepenalty` and  `\count` are not considered variables themselves.
//! Instead they are _variable commands_.
//! A variable command is _resolved_ to obtain a variable.
//! A variable is an object that ultimately points to a specific piece of memory like an `i32` in the state.
//! 
//! The resolution process of going from a command to a variable is sometimes basically a no-op.
//! For `\linepenalty` nothing needs to be resolved: as soon as the interpreter sees the control sequence,
//!     it can identify the piece of memory where the `i32` line penalty is stored.
//! On the other hand, sometimes some work is needed to go from the command to the variable.
//! In the case of `\count`, the interpreter needs to read the array index from the input stream. 
//! 
//! We will first implement a singleton variable like `\linepenalty` before moving onto 
//!     the case of array variables like `\count`.
//! 
//! ## Implementing a singleton variable
//!
//! Variables require some state in which to store the associated value.
//! We assume that the [component pattern](crate::vm::HasComponent) is being used.
//! In this case your command is associated with a state struct which will be
//!     included in the VM state as a component.
//! The value of a variable is just a Rust field of the correct type in the component:
//! ```
//! pub struct MyComponent {
//!     my_variable_value: i32
//! }
//! ```
//! 
//! To make a Texlang variable out of this `i32` we need to provde an immutable getter
//! and a mutable getter.
//! These getters have the signature [RefFn] and [MutRefFn] respectively.
//! Both getters accept a reference to the state and an address, and return a reference to the variable.
//! Don't worry about the address argument for the moment: for singleton variables, it is ignored.
//! 
//! For the value above, our getters look like this:
//! 
//! ```
//! # pub struct MyComponent {
//! #     my_variable_value: i32
//! # }
//! use texlang_core::vm::HasComponent;
//! 
//! fn getter<S: HasComponent<MyComponent>>(state: &S, address: u32) -> &i32 {
//!     &state.component().my_variable_value
//! }
//! fn mut_getter<S: HasComponent<MyComponent>>(state: &mut S, address: u32) -> &mut i32 {
//!     &mut state.component_mut().my_variable_value
//! }
//! ```
//! Once we have the getters, we can create the TeX command:
//! 
//! ```
//! # pub struct MyComponent {
//! #     my_variable_value: i32
//! # }
//! # use texlang_core::vm::HasComponent;
//! # fn getter<S: HasComponent<MyComponent>>(state: &S, address: u32) -> &i32 {
//! #    &state.component().my_variable_value
//! # }
//! # fn mut_getter<S: HasComponent<MyComponent>>(state: &mut S, address: u32) -> &mut i32 {
//! #    &mut state.component_mut().my_variable_value
//! # }
//! use texlang_core::variable;
//! use texlang_core::command;
//! 
//! pub fn my_variable<S: HasComponent<MyComponent>>() -> command::Command<S> {
//!     return variable::Command::new(
//!         getter,
//!         mut_getter,
//!         variable::AddressSpec::NoAddress,
//!     ).into()
//! }
//! ```
//! 
//! The function [Command::new] creates a new variable command.
//! The last argument is an [address spec](AddressSpec) which we discuss below,
//!     but which is always [AddressSpec::NoAddress] for singleton variables.
//! We cast the variable command into a generic command using the `into` method.
//! This command can now be included in the VM's command map and the value can be accessed in TeX scripts!
//! 
//! As usual with the component pattern, the code we write works for _any_ TeX engine
//! whose state contains our compenent.
//! 
//! Finally, as a matter of style, we recommend implementing the getters inline as closures.
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
//! pub fn my_variable<S: HasComponent<MyComponent>>() -> command::Command<S> {
//!     return variable::Command::new(
//!         |state: &S, address: u32| -> &i32 {
//!             &state.component().my_variable_value
//!         },
//!         |state: &mut S, address: u32| -> &mut i32 {
//!             &mut state.component_mut().my_variable_value
//!         },
//!         variable::AddressSpec::NoAddress,
//!     ).into()
//! }
//! ```
//! 
//! 
//! ## Implementing an array variable
//! 
//! The main difference in the variables API between singleton and array variables is that we
//!     need to use the various address arguments that were ignored above.
//! 
//! In this section we will implement an array variable with 10 entries.
//! In the component, we replace the `i32` with an array of `i32`s:
//! ```
//! pub struct MyComponent {
//!     my_array_values: [i32; 10]
//! }
//! ```
//! 
//! The getter functions use the provided address argument to determine the index to use for the array:
//! ```
//! # pub struct MyComponent {
//! #     my_array_values: [i32; 10]
//! # }
//! # use texlang_core::vm::HasComponent;
//! fn getter<S: HasComponent<MyComponent>>(state: &S, address: u32) -> &i32 {
//!     &state.component().my_array_values[address as usize]
//! }
//! ```
//! 
//! The above listing raises an important question: what if the array access is out of bounds?
//! The Rust code here will panic, and in Texlang this is the correct behaviour.
//! Texlang always assumes that variable getters are infallible.
//! This is the same as assuming that an instantiated [Variable] type points to a valid piece of memory
//! and is not (say) dangling.
//! 
//! Next, we construct the command.
//! Unlike the singleton command, this command will need to figure out the address of the variable.
//! As with `\count`, our command will do this by reading the index from the input token stream.
//! In the variables API, we implement this by providing the following type of function:
//! 
//! ```
//! use texlang_core::{parse, token, vm};
//! use anyhow;
//! 
//! fn address<S>(token: token::Token, input: &mut vm::ExpansionInput<S>) -> anyhow::Result<u32> {
//!     let address: u32 = parse::parse_number(input)?;
//!     if address >= 10 {
//!         // for simplicity we panic, but in real code we should return an error
//!         panic!["out of bounds"]
//!     }
//!     return Ok(address)
//! }
//! ```
//! 
//! Finally we create the command.
//! This is the same as the singleton case, except we pass the address function above as an address spec
//! with the `Dynamic` variant:
//! 
//! ```
//! # use texlang_core::{command, parse, token, variable, vm};
//! # use anyhow;
//! # fn getter<S: HasComponent<MyComponent>>(state: &S, address: u32) -> &i32 {
//! #   panic![""]
//! # }
//! # fn mut_getter<S: HasComponent<MyComponent>>(state: &mut S, address: u32) -> &mut i32 {
//! #   panic![""]
//! # }
//! # fn address<S>(token: token::Token, input: &mut vm::ExpansionInput<S>) -> anyhow::Result<u32> {
//! #   panic![""]
//! # }
//! # pub struct MyComponent {
//! #     my_array_values: [i32; 10]
//! # }
//! # use texlang_core::vm::HasComponent;
//! pub fn my_array<S: HasComponent<MyComponent>>() -> command::Command<S> {
//!     return variable::Command::new(
//!         getter,
//!         mut_getter,
//!         variable::AddressSpec::Dynamic(address),
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
//! Using the Texlang variables API it is possible to implement the analagous command
//! for the `\myarray` command implemented above.
//! The implementation is in 3 steps:
//! 
//! 1. The target (e.g. `\A`) is read using [crate::parse::parse_command_target].
//! 
//! 1. The address (e.g. `1`) is read using [crate::parse::parse_number], just like in the previous section.
//! 
//! 1. A new variable command is then created and added to the commands map.
//!     This command is created using [Command::new] just as above, except in the address
//!     spec we use the [AddressSpec::StaticAddress] variant with the address calculated in part 2.
//! 
//! For a full example where this is all worked out, consult the implementation of `\countdef`
//! in the Texlang standard library.
//! 
//! ## Consuming TeX variables
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
use crate::vm;
use crate::{parse, token};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use texcraft_stdext::collections::groupingmap;
use vm::RefVM;

/// Function signature for a variable's immutable getter.
///
/// In Texcraft all [Variable]s are built from an immutable getter and a mutable getter.
/// This type alias just defines the signature of immutable getters.
/// The first argument is the state, and the second argument is the address of the variable.
/// Note that for many variables the address argument is ignored.
pub type RefFn<S, T> = fn(state: &S, address: u32) -> &T;

/// Function signature for a variable's mutable getters.
///
/// In Texcraft all [Variable]s are built from an immutable getter and a mutable getter.
/// This type alias just defines the signature of mutable getters.
/// The first argument is the state, and the second argument is the address of the variable.
/// Note that for many variables the address argument is ignored.
pub type MutRefFn<S, T> = fn(state: &mut S, address: u32) -> &mut T;

/// Specification for how the address of a variable is determined.
///
/// Obtaining a variable from a command involves determing the variable's `u32` address.
/// This address is ultimately passed into the variable's getters to get a reference or mutable reference
/// to the underlying Rust value.
pub enum AddressSpec<S> {
    /// No address. This currently resolves to `0_u32`.
    NoAddress,
    /// A specific static address, provided in the enum variant.
    StaticAddress(u32),
    /// A dynamic address that is determined by reading the input token stream.
    ///
    /// For example, in `\count 4` the address of `4` is determined by parsing a number
    /// from the input token stream.
    Dynamic(fn(token::Token, &mut vm::ExpansionInput<S>) -> anyhow::Result<u32>),
    /// A dynamic address, but determined using virtual method dispatch.
    ///
    /// This method is more flexible than the Dynamic variant, but less performant.
    DynamicVirtual(Box<dyn DynamicAddressSpec<S>>),
}

/// Trait used for dynamically determining an address using virtual method dispatch.
pub trait DynamicAddressSpec<S> {
    /// Determine the address of a variable.
    fn resolve(
        &self,
        token: token::Token,
        input: &mut vm::ExpansionInput<S>,
    ) -> anyhow::Result<u32>;
}

impl<S> AddressSpec<S> {
    /// Determine the address of a variable using the input token stream.
    fn determine_address(
        &self,
        token: token::Token,
        input: &mut vm::ExpansionInput<S>,
    ) -> anyhow::Result<u32> {
        match self {
            AddressSpec::NoAddress => Ok(0),
            AddressSpec::StaticAddress(addr) => Ok(*addr),
            AddressSpec::Dynamic(f) => f(token, input),
            AddressSpec::DynamicVirtual(v) => v.resolve(token, input),
        }
    }
}

/// A TeX command that is resolved to obtain a [Variable].
/// 
/// A command consists of three parts.
/// 
/// The first two parts are an immutable getter (of type [RefFn])
/// and a mutable getter (of type [MutRefFn]).
/// Both of these getters accept a reference to the state (where the variable's value lives)
/// and an `u32` address.
/// The address can be used by the getters to return different values in memory.
/// For example, if the getters read from an array, the address may be just the index in the array.
/// This mechanism allows users of Texlang to write one pair of getters that can be used in many variables.
/// 
/// The third part of a command is an [AddressSpec] that is used to determine the aforementioned address
/// of a variable at runtime.
/// The process of resolving a command involves determining the `u32` address and returning a [Variable] type,
/// which under the hood is just the two getters and this address.
pub struct Command<S> {
    getter: Getter<S>,
    address: AddressSpec<S>,
}

impl<S> Command<S> {
    /// Create a new variable command.
    pub fn new<T: SupportedType>(
        ref_fn: RefFn<S, T>,
        ref_mut_fn: MutRefFn<S, T>,
        address_spec: AddressSpec<S>,
    ) -> Command<S> {
        internal::SupportedTypeInternal::new_command(ref_fn, ref_mut_fn, address_spec)
    }

    /// Create a new base variable command.
    pub fn new_base<T: SupportedBaseType>(
        ref_fn: RefFn<vm::BaseState<S>, T>,
        ref_mut_fn: MutRefFn<vm::BaseState<S>, T>,
        address_spec: AddressSpec<S>,
    ) -> Command<S> {
        internal::SupportedBaseTypeInternal::new_command(ref_fn, ref_mut_fn, address_spec)
    }

    /// Resolve the command to obtain a [Variable].
    pub fn resolve(
        &self,
        token: token::Token,
        input: &mut vm::ExpansionInput<S>,
    ) -> anyhow::Result<Variable<S>> {
        let address = self.address.determine_address(token, input)?;
        Ok(match self.getter {
            Getter::Int(a, b) => Variable::Int(TypedVariable(a, b, address)),
            Getter::CatCode(a, b) => Variable::CatCode(TypedVariable(a, b, address)),
        })
    }

    /// Resolve the command to a variable and return the value of the variable.
    pub fn value<'a>(
        &self,
        token: token::Token,
        input: &'a mut vm::ExpansionInput<S>,
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
///     This is especially a problem when you need to perform a different action depending on the conrete type of the variable.
///     
/// 2. (Trickier, more flexible) Match on the type's enum variants to determine the
///     concrete type of the variable.
///     The [TypedVariable] value obtained in this way can be used to perform operations on the value.
///     The main benefit of this approach is that after matching on the type, you can still use the input
///     stream to do things because there is not borrow alive.
///     
pub enum Variable<S> {
    Int(TypedVariable<S, i32>),
    CatCode(TypedVariable<vm::BaseState<S>, CatCode>),
}

impl<S> Variable<S> {
    /// Return a reference to the value of the variable.
    pub fn value<'a>(&self, input: &'a mut vm::ExpansionInput<S>) -> ValueRef<'a> {
        match self {
            Variable::Int(variable) => ValueRef::Int(variable.value(input.state())),
            Variable::CatCode(variable) => ValueRef::CatCode(variable.value(input.base())),
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
                *variable.value_mut_base(input, scope) = value;
            }
        };
        Ok(())
    }
}

enum Getter<S> {
    Int(RefFn<S, i32>, MutRefFn<S, i32>),
    CatCode(
        RefFn<vm::BaseState<S>, CatCode>,
        MutRefFn<vm::BaseState<S>, CatCode>,
    ),
}

/// A TeX variable of a specific Rust type `T`.
pub struct TypedVariable<S, T>(RefFn<S, T>, MutRefFn<S, T>, u32);

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

    fn key(&self) -> (usize, usize, u32) {
        (self.0 as usize, self.1 as usize, self.2)
    }
}

impl<S, T> TypedVariable<S, T>
where
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
    /// If no assignment happens, the variable may be in a wierd state afterwards.
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
                    internal::SupportedTypeInternal::restore_map_mut(group).remove(self)
                }
            }
            groupingmap::Scope::Local => {
                if let Some((group, _, _)) = input.current_group_mut() {
                    internal::SupportedTypeInternal::restore_map_mut(group)
                        .save(*self, current_value);
                }
            }
        }
        (self.1)(input.state_mut(), self.2)
    }
}

impl<S, T> TypedVariable<vm::BaseState<S>, T>
where
    T: Copy + SupportedBaseType,
{
    /// Returns a mutable reference to the base variable's value. This method is used for base variables.
    /// 
    /// See the documentation on [TypedVariable::value_mut] for information on this method.
    pub fn value_mut_base<'a>(
        &self,
        input: &'a mut vm::ExecutionInput<S>,
        scope: groupingmap::Scope,
    ) -> &'a mut T {
        let current_value = *(self.0)(input.base(), self.2);
        match scope {
            groupingmap::Scope::Global => {
                for group in input.groups() {
                    internal::SupportedBaseTypeInternal::restore_map_mut(group).remove(self)
                }
            }
            groupingmap::Scope::Local => {
                if let Some((group, _, _)) = input.current_group_mut() {
                    internal::SupportedBaseTypeInternal::restore_map_mut(group)
                        .save(*self, current_value);
                }
            }
        }
        (self.1)(input.base_mut(), self.2)
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
/// It is not possibe to implement this trait outside of the Texlang core create.
/// It exists only to make the variables API more ergonomic;
///     for example, it is used to provide a uniform constructor [Command::new] for commands.
pub trait SupportedType: internal::SupportedTypeInternal {}

impl SupportedType for i32 {}

/// Trait satisfied by all Rust types that can be used as TeX base variables
pub trait SupportedBaseType: internal::SupportedBaseTypeInternal {}

impl SupportedBaseType for CatCode {}

/// Parts of the variable system that are private to the Texlang core crate.
/// 
/// This is basically a hack, and may not work in future versions of Rust.
/// The basic problem it solves is that for the variables API it's really nice to have the public [SupportedType]
///     trait.
/// Necessarily this trait encodes internal implementation details about Texlang core, like how to get
///     the restore values for variables of a particular type.
/// Because [SupportedType] is public, it's hard to do this without making some of these implementation
///     details public too.
/// This is because the Rust compiler (reasonably) enforces that public traits don't reference or leak private types.
/// 
/// The loophole is that placing all of this stuff as _public_ items in a _private_ module is allowed!
/// And it hides all of the stuff from end users.
/// Shhhhh! Don't tell the Rust team, they may fix the bug.
pub(crate) mod internal {
    use super::*;

    /// Internal VM data structure used to implement TeX's grouping semantics.
    ///
    /// This is not intended to be used outside of the VM.
    pub struct RestoreValues<S> {
        i32: RestoreMap<S, i32>,
        catcode_base: RestoreMap<vm::BaseState<S>, CatCode>,
    }

    impl<S> Default for RestoreValues<S> {
        fn default() -> Self {
            Self {
                i32: Default::default(),
                catcode_base: Default::default(),
            }
        }
    }

    impl<S> RestoreValues<S> {
        pub fn restore(self, base: &mut vm::BaseState<S>, state: &mut S) {
            self.i32.restore(state);
            self.catcode_base.restore(base);
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

    pub trait SupportedTypeInternal: Sized {
        fn restore_map_mut<S>(_: &mut RestoreValues<S>) -> &mut RestoreMap<S, Self>;

        fn new_command<S>(
            _: fn(state: &S, address: u32) -> &Self,
            _: fn(state: &mut S, address: u32) -> &mut Self,
            _: AddressSpec<S>,
        ) -> Command<S>;
    }

    impl SupportedTypeInternal for i32 {
        fn restore_map_mut<S>(m: &mut RestoreValues<S>) -> &mut RestoreMap<S, Self> {
            &mut m.i32
        }
        fn new_command<S>(
            ref_fn: fn(state: &S, address: u32) -> &Self,
            ref_mut_fn: fn(state: &mut S, address: u32) -> &mut Self,
            address: AddressSpec<S>,
        ) -> Command<S> {
            Command {
                getter: Getter::Int(ref_fn, ref_mut_fn),
                address,
            }
        }
    }

    pub trait SupportedBaseTypeInternal: Sized {
        fn restore_map_mut<S>(_: &mut RestoreValues<S>) -> &mut RestoreMap<vm::BaseState<S>, Self>;

        fn new_command<S>(
            _: fn(state: &vm::BaseState<S>, address: u32) -> &Self,
            _: fn(state: &mut vm::BaseState<S>, address: u32) -> &mut Self,
            _: AddressSpec<S>,
        ) -> Command<S>;
    }

    impl SupportedBaseTypeInternal for CatCode {
        fn restore_map_mut<S>(m: &mut RestoreValues<S>) -> &mut RestoreMap<vm::BaseState<S>, Self> {
            &mut m.catcode_base
        }

        fn new_command<S>(
            ref_fn: fn(state: &vm::BaseState<S>, address: u32) -> &Self,
            ref_mut_fn: fn(state: &mut vm::BaseState<S>, address: u32) -> &mut Self,
            address: AddressSpec<S>,
        ) -> Command<S> {
            Command {
                getter: Getter::CatCode(ref_fn, ref_mut_fn),
                address,
            }
        }
    }
}
