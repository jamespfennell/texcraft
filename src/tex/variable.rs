//! Texcraft variables system
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
//! fn get_year(state: &S, addr: usize) -> &i32 {
//!     &state.year
//! }
//! # fn get_mut_year(state: &mut S, addr: usize) -> &mut i32 {
//! #    &mut state.year
//! # }
//! # use texcraft::tex::variable;
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
//! # fn get_year(state: &S, addr: usize) -> &i32 {
//! #    &state.year
//! # }
//! fn get_mut_year(state: &mut S, addr: usize) -> &mut i32 {
//!    &mut state.year
//! }
//! # use texcraft::tex::variable;
//! # variable::Variable::Int(variable::TypedVariable::new(get_year, get_mut_year, 0));
//! ```
//!
//! Note how these two functions together provide the full capabilities of a mutable pointer to `state.year`
//! in a way that keeps the borrow checker happy.
//! Of course, the tradeoff is that wiring up a new variable is a bit tedious.
//!
//! For a case when the address index might be used,
//!   suppose that our state `S` also offers some integer scratch space.
//! This is stored in member `scratch_space` which is a 10 element vector of [i32] types.
//! The getter would be:
//!
//! ```
//! struct S {
//!     scratch_space: Vec<i32>
//! }
//!
//! fn get_scratch_space(state: &S, addr: usize) -> &i32 {
//!     &state.scratch_space[addr]
//! }
//! # fn get_mut_scratch_space(state: &mut S, addr: usize) -> &mut i32 {
//! #    &mut state.scratch_space[addr]
//! # }
//! # use texcraft::tex::variable;
//! # variable::Variable::Int(variable::TypedVariable::new(get_scratch_space, get_mut_scratch_space, 0));
//! ```
//!
//! And the mutable setter:
//!
//! ```
//! # struct S {
//! #     scratch_space: Vec<i32>
//! # }
//!
//! # fn get_scratch_space(state: &S, addr: usize) -> &i32 {
//! #     &state.scratch_space[addr]
//! # }
//! fn get_mut_scratch_space(state: &mut S, addr: usize) -> &mut i32 {
//!     &mut state.scratch_space[addr]
//! }
//! # use texcraft::tex::variable;
//! # variable::Variable::Int(variable::TypedVariable::new(get_scratch_space, get_mut_scratch_space, 0));
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
//! In Texcraft, `\count` and `\lastpenalty` are implemented as  *variable commands* of type [Command].
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
//! - The [registers module](super::command::library::registers) contains an implementation of
//!     TeX registers.
//! - The [time module](super::command::library::time) contains an implementation of the time parameters.
//!
//!
//!

use crate::tex::command;
use crate::tex::error;
use crate::tex::parse;
use crate::tex::state::Base;
use crate::tex::token;
use crate::tex::token::catcode::CatCode;

/// Enum with a variant for each type of variable in TeX.
pub enum Variable<S> {
    Int(TypedVariable<S, i32>),
    BaseInt(TypedVariable<Base<S>, i32>),

    /// CatCode variables can live in the Base, so the type is a little different here.
    CatCode(TypedVariable<Base<S>, CatCode>),
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
    ref_fn: fn(state: &S, i: usize) -> &T,
    ref_mut_fn: fn(state: &mut S, i: usize) -> &mut T,
    addr: usize,
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
        ref_fn: fn(state: &S, i: usize) -> &T,
        ref_mut_fn: fn(state: &mut S, i: usize) -> &mut T,
        addr: usize,
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
    pub fn get_mut<'a>(&'a self, state: &'a mut S) -> &'a mut T {
        (self.ref_mut_fn)(state, self.addr)
    }

    /// Get the address field of the variable.
    pub fn addr(&self) -> usize {
        self.addr
    }
}

/// A variant of [Command](super::command::Command) that is invoked to produce a [Variable].
pub enum Command<S> {
    Static(Variable<S>, &'static str),
    Dynamic(
        fn(
            token: &token::Token,
            input: &mut command::ExpandedInput<S>,
            addr: usize,
        ) -> anyhow::Result<Variable<S>>,
        usize, // addr
        &'static str,
    ),
}

impl<S> Copy for Command<S> {}

impl<S> Clone for Command<S> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<S> Command<S> {
    /// Obtain the variable that this command refers to.
    pub fn variable(
        &self,
        token: &token::Token,
        input: &mut command::ExpandedInput<S>,
    ) -> anyhow::Result<Variable<S>> {
        match self {
            Command::Static(v, _) => Ok(*v),
            Command::Dynamic(f, addr, _) => (*f)(token, input, *addr),
        }
    }
}

impl<S> Command<S> {
    /// Execution command that sets the value of the variable this command refers to.
    pub fn call(
        &self,
        token: token::Token,
        input: &mut command::ExecutionInput<S>,
    ) -> anyhow::Result<()> {
        let variable = self.variable(&token, input.regular())?;
        parse::parse_optional_equals(input)?;
        match variable {
            Variable::Int(variable) => {
                let val: i32 = parse::parse_number(input.regular())?;
                *variable.get_mut(input.state_mut()) = val;
            }
            Variable::BaseInt(variable) => {
                let val: i32 = parse::parse_number(input.regular())?;
                *variable.get_mut(input.base_mut()) = val;
            }
            Variable::CatCode(variable) => {
                // TODO: don't use u8 here, use usize to get better error messages
                let val: u8 = parse::parse_number(input.regular())?;
                match CatCode::from_int(val) {
                    None => {
                        return Err(error::TokenError::new(
                            token,
                            format!["the number {} is not a valid category code", val],
                        )
                        .add_note("category codes must be between 0 and 15 inclusive")
                        .cast())
                    }
                    Some(cat_code) => {
                        *variable.get_mut(input.base_mut()) = cat_code;
                    }
                }
            }
        }
        Ok(())
    }
}
