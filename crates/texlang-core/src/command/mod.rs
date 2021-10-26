//! Texcraft commands API and primitives library.
//!
//!
//! # Texcraft commands API
//!
//! One of the most important parts of any TeX engine is the primitives that it provides.
//!  This documentation describes the *Texcraft commands API*,
//! which is the mechanism by which TeX engines add new primitives.
//!
//! This is a theoretical overview; for a more hands-on experience, see
//! [the command API tutorial](tutorial) which walks through
//! process of creating 3 new primitives.
//!
//! A note on terminology: *commands* can be categorized into primitives,
//! which are implemented in the TeX engine, and user defined macros,
//!  which are created in specific TeX documents using primitives like `\def`.
//! We often use the word command and primitive interchangeably here because in the context
//! of implementing TeX engines they’re basically synonymous.
//! A TeX engine could theoretically provide a native user defined macro...but it’s unlikely.
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

use crate::driver;
use crate::prelude::*;
use crate::token;
use crate::variable;
use std::any::TypeId;
use std::rc;

pub use driver::ExecutionInput;
pub use driver::ExpandedInput;

use super::texmacro;

pub mod tutorial;

enum NullTypeIdType {}

/// Returns a [TypeId] that can be used to represent "no type ID".
///
/// Ideally "no type ID" would be represented using the `None` variant of `Option<TypeId>`,
/// but this type takes up two words instead of one.
pub fn null_type_id() -> TypeId {
    std::any::TypeId::of::<NullTypeIdType>()
}

/// The Rust type of expansion primitive functions.
pub type ExpansionFn<S> =
    fn(token: Token, input: &mut ExpandedInput<S>) -> anyhow::Result<Vec<Token>>;

/// An expansion primitive in Texcraft.
///
/// This type bundles together an [ExpansionFn] and a [TypeId] that can be used to classify the
/// primitive.
pub struct ExpansionPrimitive<S> {
    call_fn: ExpansionFn<S>,
    id: TypeId,
}

impl<S> Copy for ExpansionPrimitive<S> {}

impl<S> Clone for ExpansionPrimitive<S> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<S> ExpansionPrimitive<S> {
    pub fn new(call_fn: ExpansionFn<S>, id: TypeId) -> ExpansionPrimitive<S> {
        ExpansionPrimitive { call_fn, id }
    }

    pub fn call(&self, token: Token, input: &mut ExpandedInput<S>) -> anyhow::Result<Vec<Token>> {
        (self.call_fn)(token, input)
    }

    pub fn id(&self) -> TypeId {
        self.id
    }
}

/// The Rust type of execution primitive functions.
pub type ExecutionFn<S> = fn(token: Token, input: &mut ExecutionInput<S>) -> anyhow::Result<()>;

/// An execution primitive in Texcraft.
///
/// This type bundles together an [ExecutionFn] and a [TypeId] that can be used to classify the
/// primitive.
pub struct ExecutionPrimitive<S> {
    call_fn: ExecutionFn<S>,
    id: TypeId,
}

impl<S> ExecutionPrimitive<S> {
    pub fn new(call_fn: ExecutionFn<S>, id: TypeId) -> ExecutionPrimitive<S> {
        ExecutionPrimitive { call_fn, id }
    }

    pub fn call(&self, token: Token, input: &mut ExecutionInput<S>) -> anyhow::Result<()> {
        (self.call_fn)(token, input)
    }

    pub fn id(&self) -> TypeId {
        self.id
    }
}

impl<S> Copy for ExecutionPrimitive<S> {}

impl<S> Clone for ExecutionPrimitive<S> {
    fn clone(&self) -> Self {
        *self
    }
}

pub type VariableFn<S> = fn(
    token: Token,
    input: &mut ExpandedInput<S>,
    addr: usize,
) -> anyhow::Result<variable::Variable<S>>;

pub struct VariableCommand<S>(VariableFn<S>, usize);

impl<S> Copy for VariableCommand<S> {}

impl<S> Clone for VariableCommand<S> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<S> VariableCommand<S> {
    pub fn new(call_fn: VariableFn<S>, addr: usize) -> VariableCommand<S> {
        VariableCommand(call_fn, addr)
    }

    /// Obtain the variable that this command refers to.
    pub fn resolve(
        &self,
        token: token::Token,
        input: &mut command::ExpandedInput<S>,
    ) -> anyhow::Result<variable::Variable<S>> {
        (self.0)(token, input, self.1)
    }
}

/// A TeX command in Texcraft.
pub enum Command<S> {
    /// An expansion command that is implemented in the engine. Examples: `\the`, `\ifnum`.
    Expansion(ExpansionPrimitive<S>),

    /// A user defined macro.
    /// Examples: `\newcommand` and `\include` in LaTeX.
    Macro(rc::Rc<texmacro::Macro>),

    /// A non-expandable command that performs operations on the state. Examples: `\def`, `\par`.
    Execution(ExecutionPrimitive<S>),

    /// A command that is used to reference a variable, like a parameter or a register.
    /// Such a command is *resolved* to get the variable using the function pointer it holds.
    Variable(VariableCommand<S>),

    /// A command that aliases a character.
    /// Depending on the context in which this command appears it may behave like a
    ///   character (when typesetting) or like an unexpandable command (when parsing integers).
    /// Created using `\let\cmd=<character>`.
    Character(token::Token),
}

impl<S> Command<S> {
    pub fn id(&self) -> TypeId {
        match self {
            Command::Expansion(cmd) => cmd.id(),
            Command::Macro(_) => null_type_id(),
            Command::Execution(cmd) => cmd.id(),
            Command::Variable(_) => null_type_id(),
            Command::Character(_) => null_type_id(),
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
            Command::Expansion(e) => Command::Expansion::<S>(*e),
            Command::Macro(m) => Command::Macro(m.clone()),
            Command::Execution(e) => Command::Execution(*e),
            Command::Variable(v) => Command::Variable(*v),
            Command::Character(tv) => Command::Character(*tv),
        }
    }
}

impl<S> From<ExpansionFn<S>> for Command<S> {
    fn from(cmd: ExpansionFn<S>) -> Self {
        Command::Expansion(ExpansionPrimitive {
            call_fn: cmd,
            id: null_type_id(),
        })
    }
}

impl<S> From<ExpansionPrimitive<S>> for Command<S> {
    fn from(cmd: ExpansionPrimitive<S>) -> Self {
        Command::Expansion(cmd)
    }
}

impl<S> From<rc::Rc<texmacro::Macro>> for Command<S> {
    fn from(cmd: rc::Rc<texmacro::Macro>) -> Self {
        Command::Macro(cmd)
    }
}

impl<S> From<ExecutionFn<S>> for Command<S> {
    fn from(cmd: ExecutionFn<S>) -> Self {
        Command::Execution(ExecutionPrimitive {
            call_fn: cmd,
            id: null_type_id(),
        })
    }
}

impl<S> From<ExecutionPrimitive<S>> for Command<S> {
    fn from(cmd: ExecutionPrimitive<S>) -> Self {
        Command::Execution(cmd)
    }
}

impl<S> From<VariableFn<S>> for Command<S> {
    fn from(f: VariableFn<S>) -> Self {
        Command::Variable(VariableCommand(f, 0))
    }
}

impl<S> From<VariableCommand<S>> for Command<S> {
    fn from(cmd: VariableCommand<S>) -> Self {
        Command::Variable(cmd)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn command_size() {
        assert_eq!(std::mem::size_of::<Command<()>>(), 24);
    }
}
