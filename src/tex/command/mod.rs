//! Texcraft commands API and primitives library.
//!
//!
//! # Texcraft commands API
//!
//! One of the most important parts of any TeX engine is the primitives that it provides.
//!  This documentation describes the *Texcraft commands API*,
//! which is the mechanism by which TeX engines add new primitives.
//!
//! This is a theoretical overview; for a more hands-on experience, see [the command API tutorial](tutorial) which walks through
//! process of creating 3 new primitives.
//!
//! A note on terminology: *commands* can be categorized into primitives,
//! which are implemented in the TeX engine, and user defined macros,
//!  which are created in specific TeX documents using primitives like `\def`.
//! We often use the word command and primitive interchangeably here because in the context
//! of implementing TeX engines they’re basically synonymous.
//! A TeX engine could theoretically provide a native user defined macro...but it’s unlikely.
//!
//! ## Types of commands
//!
//! Commands in Texcraft are instances of [Command].
//! [Command] is an enum type where each variant corresponds to a different type of command in TeX.
//! The main variants are for expansion and execution commands, but there are others.
//! Some of the variants such as [variable commands](super::variable::Command)
//!     are themselves enums with sub-variants.
//! So it’s basically a tree with [Command] at the root,
//!     and concrete instances of particular command types at the leaves.
//!
//!
//! - [Command] The root command type with the following variants:
//!
//!     - `Expansion`, holding an instance of [Expansion].
//!         Any command that can be expanded. This an enum with two variants:
//!
//!         - `Primitive`, holding an instance of [ExpansionPrimitive].
//!             An expansion command that is implemented in the engine. Examples: `\the`, `\ifnum`.
//!
//!         - `Macro`, holding an instance of [def::UserDefinedMacro](library::def::UserDefinedMacro).
//!              Examples: `\newcommand` and `\include` in LaTeX.
//!
//!     - `Execution`, holding an instance of [ExecutionPrimitive].
//!         A non-expandable command that performs operations on the state. Examples: `\def`, `\par`.
//!
//!     - `Variable`, holding an instance of [variable::Command](super::variable::Command).
//!         A command that is used to reference a variable, like a parameter or a register.
//!         Such a command is can be *resolved* to get the variable.
//!         This command is an enum with two variants:
//!
//!         - `Static`, holding an instance of [variable::Variable](super::variable::Variable).
//!             A command that resolves to a fixed variable, which is stored in the enum variant.
//!             Examples: `\year`, `\tracingmacros`.
//!
//!         - `Dynamic`, holding a specific kind of function pointer.
//!             A command that reads input tokens before resolving.
//!             Examples: `\count` (which reads a number and resolves to the integer register at that index)
//!             and `\catcode` (same, but for cat code registers).
//!
//!     - `Character` (not yet implemented).
//!         A command that aliases a character.
//!         Depending on the context in which this command appears it may behave like a
//!         character (when typesetting) or like an unexpandable command (when parsing integers).
//!         Created using `\let\cmd=<character>`.
//!
//!     - ???
//!
//!         - As Texcraft is developed more variants may need to be added.
//!
//! ## Expansion vs execution
//!
//! Expansion and execution commands seem similar because they both optionally
//! read input tokens and then make changes to the engine’s environment.
//! However the differences are pretty significant in practice:
//!
//! |                                          | Expansion | Execution
//! |------------------------------------------|-----------|-----------
//! Can read tokens from the input stream?     | Yes       | Yes
//! Can add tokens to the input stream>        | Yes       | It’s possible, but the API discourages it.[^futurelet]
//! Can make changes to the state?             | No        | Yes
//! Can make changes to the input unit?        | Yes       | It’s possible, but the API discourages it.
//! Is evaluated when tokens are only being expanded, like in `\edef` | Yes | No
//!
//!
//! [^futurelet]: `\futurelet` is an example of an execution command that does this.
//!

use crate::tex::driver;
use crate::tex::prelude::*;
use crate::tex::variable;
use std::any::TypeId;
use std::rc;

pub use driver::ExecutionInput;
pub use driver::ExpansionInput;

pub mod examples;
pub mod library;
pub mod tutorial;

pub struct ExpansionPrimitive<S> {
    call_fn: fn(token: Token, input: &mut ExpansionInput<S>) -> anyhow::Result<stream::VecStream>,
    docs: &'static str,
    id: Option<TypeId>,
}

impl<S> Copy for ExpansionPrimitive<S> {}

impl<S> Clone for ExpansionPrimitive<S> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<S> ExpansionPrimitive<S> {
    pub fn call(
        &self,
        token: Token,
        input: &mut ExpansionInput<S>,
    ) -> anyhow::Result<stream::VecStream> {
        (self.call_fn)(token, input)
    }

    pub fn doc(&self) -> String {
        self.docs.to_string()
    }

    pub fn id(&self) -> Option<TypeId> {
        self.id
    }
}

pub trait ExpansionGeneric<S> {
    fn call(
        &self,
        token: Token,
        input: &mut ExpansionInput<S>,
    ) -> anyhow::Result<stream::VecStream>;

    fn doc(&self) -> String {
        "this command has no documentation".to_string()
    }

    fn id(&self) -> Option<TypeId> {
        None
    }
}

pub enum Expansion<S> {
    Primitive(ExpansionPrimitive<S>),
    Generic(rc::Rc<dyn ExpansionGeneric<S>>),
}

// We need to implement Clone manually as the derived implementation requires S to be Clone.
impl<S> Clone for Expansion<S> {
    fn clone(&self) -> Self {
        match self {
            Expansion::Primitive(e) => Expansion::Primitive(e.clone()),
            Expansion::Generic(r) => Expansion::Generic(r.clone()),
        }
    }
}

impl<S> Expansion<S> {
    pub fn call(
        &self,
        token: Token,
        input: &mut ExpansionInput<S>,
    ) -> anyhow::Result<stream::VecStream> {
        match self {
            Expansion::Primitive(e) => ExpansionPrimitive::call(e, token, input),
            Expansion::Generic(e) => ExpansionGeneric::call(e.as_ref(), token, input),
        }
    }

    pub fn doc(&self) -> String {
        match self {
            Expansion::Primitive(e) => ExpansionPrimitive::doc(e),
            Expansion::Generic(e) => ExpansionGeneric::doc(e.as_ref()),
        }
    }

    pub fn id(&self) -> Option<TypeId> {
        match self {
            Expansion::Primitive(e) => e.id,
            Expansion::Generic(e) => ExpansionGeneric::id(e.as_ref()),
        }
    }
}

pub struct ExecutionPrimitive<S> {
    call_fn: fn(token: Token, input: &mut ExecutionInput<S>) -> anyhow::Result<()>,
    docs: &'static str,
    id: Option<TypeId>,
}

impl<S> ExecutionPrimitive<S> {
    pub fn call(&self, token: Token, input: &mut ExecutionInput<S>) -> anyhow::Result<()> {
        (self.call_fn)(token, input)
    }
}

impl<S> Copy for ExecutionPrimitive<S> {}

impl<S> Clone for ExecutionPrimitive<S> {
    fn clone(&self) -> Self {
        *self
    }
}

pub enum Command<S> {
    Expansion(Expansion<S>),
    Execution(ExecutionPrimitive<S>),
    Variable(variable::Command<S>),
    Character(char, CatCode),
}

impl<S> Command<S> {
    pub fn id(&self) -> Option<TypeId> {
        match self {
            Command::Expansion(cmd) => cmd.id(),
            Command::Execution(cmd) => cmd.id,
            Command::Variable(..) => None,
            Command::Character(..) => None,
        }
    }

    pub fn doc(&self) -> String {
        match self {
            Command::Expansion(cmd) => cmd.doc(),
            Command::Execution(cmd) => cmd.docs.to_string(),
            Command::Variable(variable::Command::Static(_, docs)) => docs.to_string(),
            Command::Variable(variable::Command::Dynamic(_, _, docs)) => docs.to_string(),
            Command::Character(c, cat_code) => {
                format!["Implicit character '{}' with cat code {}", c, cat_code]
            }
        }
    }
}

impl<S> std::fmt::Debug for Command<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "This is a command",)?;
        Ok(())
    }
}

// We need to implement Clone manually as the derived implementation requires S to be Clone.
impl<S> Clone for Command<S> {
    fn clone(&self) -> Self {
        match self {
            Command::Expansion(e) => Command::Expansion::<S>(e.clone()),
            Command::Execution(e) => Command::Execution(e.clone()),
            Command::Variable(v) => Command::Variable(v.clone()),
            Command::Character(c, cat_code) => Command::Character(*c, *cat_code),
        }
    }
}

impl<S> From<variable::Variable<S>> for Command<S> {
    fn from(cmd: variable::Variable<S>) -> Self {
        Command::Variable(variable::Command::Static(cmd, "Unspecified variable"))
    }
}

impl<S> From<variable::Command<S>> for Command<S> {
    fn from(cmd: variable::Command<S>) -> Self {
        Command::Variable(cmd)
    }
}

impl<S> From<ExecutionPrimitive<S>> for Command<S> {
    fn from(cmd: ExecutionPrimitive<S>) -> Self {
        Command::Execution(cmd)
    }
}

impl<S> From<Expansion<S>> for Command<S> {
    fn from(cmd: Expansion<S>) -> Self {
        Command::Expansion(cmd)
    }
}

impl<S> From<ExpansionPrimitive<S>> for Command<S> {
    fn from(cmd: ExpansionPrimitive<S>) -> Self {
        Command::Expansion(Expansion::Primitive(cmd))
    }
}

impl<S, T: ExpansionGeneric<S> + 'static> From<rc::Rc<T>> for Command<S> {
    fn from(cmd: rc::Rc<T>) -> Self {
        Command::Expansion(Expansion::Generic(cmd))
    }
}
