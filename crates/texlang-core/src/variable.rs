//! Texlang variables system
//!
//! # Variables in TeX
//!
//! The TeX language provides for typed variables.
//! There are a few possible types, which are [listed below](#tex-variable-types).
//! Other than type, variables fall into two categories: *registers* and *parameters*.
//!
//! Registers provide a fixed number of variables, of a particular type,
//!   that can be used for arbitrary purposes.
//! A TeX snippet that calculates prime numbers will use registers for working memory.
//! Registers are generally accessed by index: integer registers, for example, are accessed via `\count <index>`.
//!  The allowable indices (or equivalently, the number of available registers) varies by engine.
//!  In the original TeX there are 256 integer registers; in pdfTeX, there are 32768=2^15.
//!
//! Parameters are variables that are hard-coded into the engine and have a specific purpose.
//! The `\hyphenchar` parameter, for example, stores the character that is used when hyphenating words (e.g, `-`).
//! Parameters vary widely in their usage and expectations:
//!   parameters like \year are generally constant,
//!   while parameters like `\lastpenalty` are updated by the engine frequently.
//! Parameters are accessed using the specific control sequence assigned to them by the engine.
//!
//! ## The Texcraft variables system
//!
//! Texcraft has a system for handling all of these kinds of TeX variables uniformly.
//! At the center of this system is the [TypedVariable] Rust struct.
//! This struct can be viewed as a pointer to the particular place in the state
//!   where the value of the variable of type `T` is stored.
//! Of course, because this is Rust, storing a direct pointer is out of the question.
//! Instead the struct contains three elements:
//!
//! 1. A getter function that returns a reference to the memory where the value is stored.
//! 1. A mutable getter function that returns a mutable reference.
//! 1. An address index, which can optionally be used by these two functions to identify
//!     the correct reference to return.
//!
//! For example, suppose our state `S` stores the integer variable for `\year` in a member
//!   entitled `year` of type [i32].
//! The getter for this variable will be:
//!
//! ```
//! struct S {
//!     year: i32
//! }
//!
//! fn get_year(state: &S, _: u32) -> &i32 {
//!     &state.year
//! }
//! # fn get_mut_year(state: &mut S, _: u32) -> &mut i32 {
//! #    &mut state.year
//! # }
//! # use texlang_core::variable;
//! # variable::Variable::Int(variable::TypedVariable::new(get_year, get_mut_year, 0));
//! ```
//!
//! And the mutable getter will be:
//!
//! ```
//! # struct S {
//! #     year: i32
//! # }
//! #
//! # fn get_year(state: &S, _: u32) -> &i32 {
//! #    &state.year
//! # }
//! fn get_mut_year(state: &mut S, _: u32) -> &mut i32 {
//!    &mut state.year
//! }
//! # use texlang_core::variable;
//! # variable::Variable::Int(variable::TypedVariable::new(get_year, get_mut_year, 0));
//! ```
//!
//! Note how these two functions together provide the full capabilities of a mutable pointer to `state.year`
//! in a way that keeps the borrow checker happy.
//! Of course, the tradeoff is that wiring up a new variable is a bit tedious.
//!
//! For a case when the address index might be used,
//!   suppose that our state `S` also offers an integer buffer.
//! This buffer is a 10 element vector of [i32] types.
//! The getter would be:
//!
//! ```
//! struct S {
//!     buffer: Vec<i32>
//! }
//!
//! fn get_buffer(state: &S, addr: u32) -> &i32 {
//!     &state.buffer[addr as usize]
//! }
//! # fn get_mut_buffer(state: &mut S, addr: u32) -> &mut i32 {
//! #    &mut state.buffer[addr as usize]
//! # }
//! # use texlang_core::variable;
//! # variable::Variable::Int(variable::TypedVariable::new(get_buffer, get_mut_buffer, 0));
//! ```
//!
//! And the mutable setter:
//!
//! ```
//! # struct S {
//! #     buffer: Vec<i32>
//! # }
//!
//! # fn get_buffer(state: &S, addr: u32) -> &i32 {
//! #     &state.buffer[addr as usize]
//! # }
//! fn get_mut_buffer(state: &mut S, addr: u32) -> &mut i32 {
//!     &mut state.buffer[addr as usize]
//! }
//! # use texlang_core::variable;
//! # variable::Variable::Int(variable::TypedVariable::new(get_buffer, get_mut_buffer, 0));
//! ```
//!
//! There are 10 possible [TypedVariable] instances depending on the index referred to.
//!
//! This example raises the question: what if `addr` is out of bounds?
//! In Texcraft, it is always assumed that instantiated [TypedVariable] types point to a valid location in memory.
//!  It is the responsibility of the code that instantiates the [TypedVariable] to ensure an out of bounds error cannot occur.
//!
//! To refer to a variable of any type, the Rust enum [Variable] is used.
//! This enum contains a variant for each type `T` that is supported in TeX.
//! Each variant contains an element of type [TypedVariable].
//!
//! ## Variable commands
//!
//! We mentioned that `\count<index>` and `\lastpenalty` are used in TeX to refer to variables;
//!   the first a register; the second a parameter.
//! In Texcraft, `\count` and `\lastpenalty` are implemented as  *variable commands* of type [VariableCommand](super::command::VariableCommand).
//!  In the Rust code, these commands can be *resolved* to yield a valid instance of [Variable].
//!
//! Note how these two commands are slightly different.
//! The command `\lastpenalty` will return right away with the memory location it is hard-coded to point to.
//! The command `\count`, on the other hand, will first need to scan the input stream to determine the
//!   index of the register being referred to. If the index is invalid,
//!   an error will be raised; otherwise, a valid Variable is returned.
//!
//! What happens next depends on the context.
//! If the command is encountered when looking for a number,
//!   the variable will be read to provide the number
//!   (or an error, if the type is not right).
//! If encountered during execution, another number will be parsed from the
//!   input stream and the variable will be updated (e.g., `\year=1982`).
//!
//! ## TeX variable types
//!
//! | Type      | Rust type      | Register accessor command | Texcraft status
//! |-----------|----------------|--------------------------|----------------
//! | Integer   | [i32] or [i64] | `\count`                 | Implemented for [i32].[^note]
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
//! [^note]: Support for non-[i32] integer types in Texcraft, if needed, will be fairly straightforward. The plan
//! is to introduce another generic type on [Base](super::state::Base) for the integer type being used.
//! TeX engine implementers will pick a concrete integer type this way.
//!
//! ## Examples of Texcraft variables
//!
//! See the registers and time module in the Texlang standard library.

use std::collections::HashMap;
use std::hash::{Hash, Hasher};

use texcraft_stdext::collections::groupingmap;

use crate::parse;
use crate::token::catcode::CatCode;
use crate::vm::{self, BaseState, ExecutionInput};

/// Enum with a variant for each type of variable in TeX.
pub enum Variable<S> {
    Int(TypedVariable<S, i32>),
    CatCode(TypedVariable<vm::BaseState<S>, CatCode>),
}

impl<S> Copy for Variable<S> {}

impl<S> Clone for Variable<S> {
    fn clone(&self) -> Self {
        *self
    }
}

/// A pointer to a piece of memory of type `T` in the state `S`.
///
/// See the module documentation for information about this type.
pub struct TypedVariable<S, T> {
    ref_fn: fn(state: &S, i: u32) -> &T,
    ref_mut_fn: fn(state: &mut S, i: u32) -> &mut T,
    addr: u32,
}

impl<S, T> PartialEq for TypedVariable<S, T> {
    fn eq(&self, rhs: &TypedVariable<S, T>) -> bool {
        (self.ref_fn as usize) == (rhs.ref_fn as usize)
            && (self.ref_mut_fn as usize == rhs.ref_mut_fn as usize)
            && (self.addr == rhs.addr)
    }
}

impl<S, T> Eq for TypedVariable<S, T> {}

impl<S, T> Hash for TypedVariable<S, T> {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        (self.ref_fn as usize).hash(state);
        (self.ref_mut_fn as usize).hash(state);
        self.addr.hash(state);
    }
}

impl<S, T> Copy for TypedVariable<S, T> {}

impl<S, T> Clone for TypedVariable<S, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<S, T> TypedVariable<S, T> {
    /// Creates a new variable.
    ///
    /// See the module documentation for information on the parameters.
    pub fn new(
        ref_fn: fn(state: &S, i: u32) -> &T,
        ref_mut_fn: fn(state: &mut S, i: u32) -> &mut T,
        addr: u32,
    ) -> TypedVariable<S, T> {
        TypedVariable {
            ref_fn,
            ref_mut_fn,
            addr,
        }
    }

    /// Gets a reference to the variable.
    pub fn get<'a>(&'a self, state: &'a S) -> &'a T {
        (self.ref_fn)(state, self.addr)
    }

    /// Gets a mutable reference to the variable.
    fn get_mut<'a>(&'a self, state: &'a mut S) -> &'a mut T {
        (self.ref_mut_fn)(state, self.addr)
    }
}

/// Sets the value of a variable using the expanded token stream.
pub fn set_using_input<S>(
    variable: Variable<S>,
    input: &mut vm::ExecutionInput<S>,
    scope: groupingmap::Scope,
) -> anyhow::Result<()> {
    parse::parse_optional_equals(input)?;
    match variable {
        Variable::Int(variable) => {
            let value: i32 = parse::parse_number(input)?;
            set_i32(variable, value, input, scope)
        }
        Variable::CatCode(variable) => {
            let value = parse::parse_catcode(input)?;
            set_base_catcode(variable, value, input, scope)
        }
    }
    Ok(())
}

macro_rules! make_setter {
    ($fn_name: ident, $type: ident, $map_name: ident, $state_getter: ident, $t: path, $doc: expr) => {
        #[inline]
        #[doc = $doc]
        pub fn $fn_name<S>(
            variable: TypedVariable<$t, $type>,
            mut value: $type,
            input: &mut ExecutionInput<S>,
            scope: groupingmap::Scope,
        ) {
            std::mem::swap(&mut value, variable.get_mut(input.$state_getter()));
            match scope {
                groupingmap::Scope::Global => {
                    for group in input.groups() {
                        group.$map_name.remove(&variable);
                    }
                }
                groupingmap::Scope::Local => {
                    if let Some((group, _, _)) = input.current_group_mut() {
                        group.$map_name.save(variable, value);
                    }
                }
            }
        }
    };
}

macro_rules! make_state_setter {
    (
    #[doc = $doc:expr]
    fn $fn_name:ident(value: $type:ident) {}
  ) => {
        make_setter![$fn_name, $type, $type, state_mut, S, $doc];
    };
}

macro_rules! make_base_setter {
    (
    #[doc = $doc:expr, field=$field:ident]
    fn $fn_name:ident(value: $type:ident) {}
  ) => {
        make_setter![$fn_name, $type, $field, base_mut, BaseState<S>, $doc];
    };
}

make_state_setter![
    #[doc = "Set the value of a custom state variable of type [i32]"]
    fn set_i32(value: i32) {}
];

make_base_setter![
    #[doc = "Set the value of a base state variable of type [i32]", field=i32_base]
    fn set_base_i32(value: i32) {}
];

make_base_setter![
    #[doc = "Set the value of a base state variable of type [CatCode]", field=catcode_base]
    fn set_base_catcode(value: CatCode) {}
];

/// The variable values that are to be restored when a group ends.
pub struct RestoreValues<S> {
    i32: RestoreMap<S, i32>,
    i32_base: RestoreMap<vm::BaseState<S>, i32>,
    catcode_base: RestoreMap<vm::BaseState<S>, CatCode>,
}

impl<S> Default for RestoreValues<S> {
    fn default() -> Self {
        Self {
            i32: Default::default(),
            i32_base: Default::default(),
            catcode_base: Default::default(),
        }
    }
}

impl<S> RestoreValues<S> {
    pub fn restore(self, base: &mut vm::BaseState<S>, state: &mut S) {
        self.i32.restore(state);
        self.i32_base.restore(base);
        self.catcode_base.restore(base);
    }
}

struct RestoreMap<S, T>(HashMap<TypedVariable<S, T>, T>);

impl<S, T> Default for RestoreMap<S, T> {
    fn default() -> Self {
        Self(HashMap::new())
    }
}

impl<S, T> RestoreMap<S, T> {
    fn save(&mut self, variable: TypedVariable<S, T>, val: T) {
        self.0.entry(variable).or_insert(val);
    }

    fn remove(&mut self, variable: &TypedVariable<S, T>) {
        self.0.remove(variable);
    }

    fn restore(self, state: &mut S) {
        for (v, restored_value) in self.0 {
            *v.get_mut(state) = restored_value;
        }
    }
}
