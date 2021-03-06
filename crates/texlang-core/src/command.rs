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
//! read input tokens and then make changes to the engine’s environment.
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
use std::collections::HashMap;
use std::rc;
use texcraft_stdext::collections::groupingmap::GroupingVec;

enum NullTypeIdType {}

/// Returns a [TypeId] that can be used to represent "no type ID".
///
/// Ideally "no type ID" would be represented using the `None` variant of `Option<TypeId>`,
/// but this type takes up two words instead of one.
pub fn null_type_id() -> TypeId {
    std::any::TypeId::of::<NullTypeIdType>()
}

/// The Rust type of expansion primitives.
pub type ExpansionFn<S> =
    fn(token: Token, input: &mut runtime::ExpansionInput<S>) -> anyhow::Result<Vec<Token>>;

/// The Rust type of execution primitive functions.
pub type ExecutionFn<S> =
    fn(token: Token, input: &mut runtime::ExecutionInput<S>) -> anyhow::Result<()>;

pub type VariableFn<S> = fn(
    token: Token,
    input: &mut runtime::ExpansionInput<S>,
    addr: u32,
) -> anyhow::Result<variable::Variable<S>>;

/// Obtain the variable that the command refers to.
pub fn resolve<S, I: AsMut<runtime::ExpansionInput<S>>>(
    variable_fn: VariableFn<S>,
    addr: u32,
    token: token::Token,
    input: &mut I,
) -> anyhow::Result<variable::Variable<S>> {
    variable_fn(token, input.as_mut(), addr)
}

/// A TeX command in Texcraft.
pub enum Command<S> {
    /// An expansion command that is implemented in the engine. Examples: `\the`, `\ifnum`.
    Expansion(ExpansionFn<S>),

    /// A user defined macro.
    /// Examples: `\newcommand` and `\include` in LaTeX.
    Macro(rc::Rc<texmacro::Macro>),

    /// A non-expandable command that performs operations on the state. Examples: `\def`, `\par`.
    Execution(ExecutionFn<S>),

    /// A command that is used to reference a variable, like a parameter or a register.
    /// Such a command is *resolved* to get the variable using the function pointer it holds.
    Variable(VariableFn<S>, u32),

    /// A command that aliases a character.
    /// Depending on the context in which this command appears it may behave like a
    ///   character (when typesetting) or like an unexpandable command (when parsing integers).
    /// Created using `\let\cmd=<character>`.
    Character(token::Value),
}

pub struct Definition<S> {
    command: Command<S>,
    id: std::any::TypeId,
    doc: &'static str,
}

impl<S> Definition<S> {
    /// Create a new expansion command definition.
    pub fn new_expansion(t: ExpansionFn<S>) -> Definition<S> {
        t.into()
    }

    /// Create a new expansion command definition.
    pub fn new_execution(t: ExecutionFn<S>) -> Definition<S> {
        t.into()
    }

    /// Create a new variable command definition.
    pub fn new_variable(t: VariableFn<S>) -> Definition<S> {
        t.into()
    }

    /// Set the ID for this command definition.
    pub fn with_id(mut self, id: std::any::TypeId) -> Definition<S> {
        self.id = id;
        self
    }

    // Set the doc for this command definition.
    pub fn with_doc(mut self, doc: &'static str) -> Definition<S> {
        self.doc = doc;
        self
    }

    pub fn with_addr(mut self, addr: u32) -> Definition<S> {
        if let Command::Variable(_, old_addr) = &mut self.command {
            *old_addr = addr;
        } else {
            panic!("cannot set the address of a non-variable command");
        }
        self
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
            Command::Variable(v, a) => Command::Variable(*v, *a),
            Command::Character(tv) => Command::Character(*tv),
        }
    }
}

impl<S> From<ExpansionFn<S>> for Definition<S> {
    fn from(cmd: ExpansionFn<S>) -> Self {
        Command::Expansion(cmd).into()
    }
}

impl<S> From<rc::Rc<texmacro::Macro>> for Definition<S> {
    fn from(cmd: rc::Rc<texmacro::Macro>) -> Self {
        Command::Macro(cmd).into()
    }
}

impl<S> From<ExecutionFn<S>> for Definition<S> {
    fn from(cmd: ExecutionFn<S>) -> Self {
        Command::Execution(cmd).into()
    }
}

impl<S> From<VariableFn<S>> for Definition<S> {
    fn from(f: VariableFn<S>) -> Self {
        Command::Variable(f, 0).into()
    }
}

impl<S> From<Command<S>> for Definition<S> {
    fn from(cmd: Command<S>) -> Self {
        Definition {
            command: cmd,
            id: null_type_id(),
            doc: "",
        }
    }
}

pub struct CommandsMap<S> {
    map: GroupingVec<command::Command<S>>,
    len: usize,
    id_map: GroupingVec<std::any::TypeId>,
    doc_map: HashMap<CsName, &'static str>,
}

impl<S> Default for CommandsMap<S> {
    fn default() -> Self {
        Self {
            map: Default::default(),
            len: 0,
            id_map: Default::default(),
            doc_map: Default::default(),
        }
    }
}

impl<S> CommandsMap<S> {
    #[inline]
    pub fn get(&self, name: &token::CsName) -> Option<&command::Command<S>> {
        self.map.get(&name.to_usize())
    }

    pub fn get_id(&self, name: &token::CsName) -> std::any::TypeId {
        self.id_map
            .get(&name.to_usize())
            .copied()
            .unwrap_or_else(null_type_id)
    }

    pub fn get_doc(&self, name: &token::CsName) -> Option<&'static str> {
        match self.doc_map.get(name) {
            None => None,
            Some(s) => {
                let t = s.trim();
                if t.is_empty() {
                    // TODO: we should distinguish between no docs vs no command
                    None
                } else {
                    Some(t)
                }
            }
        }
    }

    #[inline]
    pub fn insert<B: Into<command::Definition<S>>>(&mut self, name: token::CsName, cmd: B) -> bool {
        let cmd = B::into(cmd);
        self.id_map.insert(name.to_usize(), cmd.id);
        self.doc_map.insert(name, cmd.doc);
        let existed = self.map.insert(name.to_usize(), cmd.command);
        if !existed {
            self.len += 1;
        }
        existed
    }

    #[inline]
    pub fn insert_global<B: Into<command::Definition<S>>>(
        &mut self,
        name: token::CsName,
        cmd: B,
    ) -> bool {
        let cmd = B::into(cmd);
        self.id_map.insert_global(name.to_usize(), cmd.id);
        let existed = self.map.insert_global(name.to_usize(), cmd.command);
        if !existed {
            self.len += 1;
        }
        existed
    }

    pub fn to_hash_map(&self) -> HashMap<CsName, command::Command<S>> {
        let mut result = HashMap::new();
        for (key, value) in self.map.backing_container().iter().enumerate() {
            let cmd = match value {
                None => continue,
                Some(cmd) => cmd.clone(),
            };
            let cs_name = match CsName::try_from_usize(key) {
                None => continue,
                Some(cs_name) => cs_name,
            };
            result.insert(cs_name, cmd);
        }
        result
    }

    pub fn begin_group(&mut self) {
        self.map.begin_group();
        self.id_map.begin_group();
    }

    pub fn end_group(&mut self) -> bool {
        self.map.end_group() && self.id_map.end_group()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        self.len
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn command_size() {
        assert_eq!(std::mem::size_of::<Command<()>>(), 16);
    }
}
