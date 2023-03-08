//! Texlang commands API
//!
//! # Texcraft commands API
//!
//! One of the most important parts of any TeX engine is the primitives that it provides.
//!  This documentation describes the *Texcraft commands API*,
//! which is the mechanism by which TeX engines add new primitives.
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
//! read input tokens and then make changes to the VM.
//! However the differences are pretty significant in practice:
//!
//! |                                          | Expansion | Execution
//! |------------------------------------------|-----------|-----------
//! Can read tokens from the input stream?     | Yes       | Yes
//! Can add tokens to the input stream>        | Yes       | It’s possible, but the API discourages it.[^futurelet]
//! Can make changes to the state?             | No        | Yes
//! Is evaluated when tokens are only being expanded, like in `\edef` | Yes | No
//!
//!
//! [^futurelet]: `\futurelet` is an example of an execution command that does this.
//!

use crate::prelude::*;
use crate::texmacro;
use crate::token;
use crate::variable;
use std::any::TypeId;
use std::rc;

mod map;

pub use map::Map;

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
    fn(token: Token, input: &mut vm::ExpansionInput<S>) -> anyhow::Result<Vec<Token>>;

/// The Rust type of execution primitive functions.
pub type ExecutionFn<S> = fn(token: Token, input: &mut vm::ExecutionInput<S>) -> anyhow::Result<()>;

/// Function that determines the actual behaviour of a TeX command.
pub enum Fn<S> {
    /// An expansion command that is implemented in the engine. Examples: `\the`, `\ifnum`.
    Expansion(ExpansionFn<S>),

    /// A user defined macro.
    /// Examples: `\newcommand` and `\include` in LaTeX.
    Macro(rc::Rc<texmacro::Macro>),

    /// A non-expandable command that performs operations on the state. Examples: `\def`, `\par`.
    Execution(ExecutionFn<S>),

    /// A command that is used to reference a variable, like a parameter or a register.
    /// Such a command is *resolved* to get the variable using the function pointer it holds.
    Variable(rc::Rc<variable::Command<S>>),

    /// A command that aliases a character.
    /// Depending on the context in which this command appears it may behave like a
    ///   character (when typesetting) or like an unexpandable command (when parsing integers).
    /// Created using `\let\cmd=<character>`.
    Character(token::Value),
}

/// Texlang representation of a TeX command.
///
/// Consists of a function, and optional ID, and an optional doc string.
pub struct Command<S> {
    func: Fn<S>,
    id: std::any::TypeId,
    doc: Option<&'static str>,
}

impl<S> Command<S> {
    /// Create a new expansion command definition.
    pub fn new_expansion(t: ExpansionFn<S>) -> Command<S> {
        t.into()
    }

    /// Create a new expansion command definition.
    pub fn new_execution(t: ExecutionFn<S>) -> Command<S> {
        t.into()
    }

    /// Create a new variable command definition.
    pub fn new_variable(cmd: variable::Command<S>) -> Command<S> {
        Fn::Variable(rc::Rc::new(cmd)).into()
    }

    /// Set the ID for this command definition.
    pub fn with_id(mut self, id: std::any::TypeId) -> Command<S> {
        self.id = id;
        self
    }

    // Set the doc for this command definition.
    pub fn with_doc(mut self, doc: &'static str) -> Command<S> {
        self.doc = Some(doc);
        self
    }

    pub fn func(&self) -> &Fn<S> {
        &self.func
    }

    pub fn id(&self) -> &TypeId {
        &self.id
    }

    pub fn doc(&self) -> Option<&'static str> {
        self.doc
    }
}

impl<S> std::fmt::Debug for Fn<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "This is a command",)?;
        Ok(())
    }
}

// We need to implement Clone manually as the derived implementation requires S to be Clone.
impl<S> Clone for Fn<S> {
    fn clone(&self) -> Self {
        match self {
            Fn::Expansion(e) => Fn::Expansion::<S>(*e),
            Fn::Macro(m) => Fn::Macro(m.clone()),
            Fn::Execution(e) => Fn::Execution(*e),
            Fn::Variable(v) => Fn::Variable(v.clone()),
            Fn::Character(tv) => Fn::Character(*tv),
        }
    }
}

// We need to implement Clone manually as the derived implementation requires S to be Clone.
impl<S> Clone for Command<S> {
    fn clone(&self) -> Self {
        Self {
            func: self.func.clone(),
            id: self.id,
            doc: self.doc,
        }
    }
}

impl<S> From<ExpansionFn<S>> for Command<S> {
    fn from(cmd: ExpansionFn<S>) -> Self {
        Fn::Expansion(cmd).into()
    }
}

impl<S> From<rc::Rc<texmacro::Macro>> for Command<S> {
    fn from(cmd: rc::Rc<texmacro::Macro>) -> Self {
        Fn::Macro(cmd).into()
    }
}

impl<S> From<ExecutionFn<S>> for Command<S> {
    fn from(cmd: ExecutionFn<S>) -> Self {
        Fn::Execution(cmd).into()
    }
}

impl<S> From<variable::Command<S>> for Command<S> {
    fn from(cmd: variable::Command<S>) -> Self {
        Fn::Variable(rc::Rc::new(cmd)).into()
    }
}

impl<S> From<Fn<S>> for Command<S> {
    fn from(cmd: Fn<S>) -> Self {
        Command {
            func: cmd,
            id: null_type_id(),
            doc: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn func_size() {
        assert_eq!(std::mem::size_of::<Fn<()>>(), 16);
    }
}
