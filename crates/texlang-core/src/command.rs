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
use std::collections::HashMap;
use std::rc;
use texcraft_stdext::collections::groupingmap;
use texcraft_stdext::collections::groupingmap::GroupingVec;

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

/// The Rust type of variable primitive functions.
pub type VariableFn<S> = fn(
    token: Token,
    input: &mut vm::ExpansionInput<S>,
    addr: u32,
) -> anyhow::Result<variable::Variable<S>>;

/// Obtain the variable that the command refers to.
pub fn resolve<S, I: AsMut<vm::ExpansionInput<S>>>(
    variable_fn: VariableFn<S>,
    addr: u32,
    token: token::Token,
    input: &mut I,
) -> anyhow::Result<variable::Variable<S>> {
    variable_fn(token, input.as_mut(), addr)
}

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
    Variable(VariableFn<S>, u32),

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
    pub fn new_variable(f: VariableFn<S>, addr: u32) -> Command<S> {
        Fn::Variable(f, addr).into()
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
            Fn::Variable(v, a) => Fn::Variable(*v, *a),
            Fn::Character(tv) => Fn::Character(*tv),
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

impl<S> From<VariableFn<S>> for Command<S> {
    fn from(f: VariableFn<S>) -> Self {
        Fn::Variable(f, 0).into()
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

/// Map is a map type where the keys are control sequence names and the values are TeX commands.
///
/// There are a number of design goals for the map:
///
/// - Make retrieving the function for a command very fast. This is achieved primarily by storing
///   command functions by themselves in an array. The index in the array is the control sequence
///   name, which is an integer when interned. This implementation choice means fast-as-possible lookups.
///   Storing the function by itself means there is good cache locality.
///
///
/// - Insert built in commands at the start
///     insert_built_in(cs_name, cmd),
/// - Insert variable commands that aren't there at the start, but will be in the future. E.g., \countdef
///     register_built_in(cmd) <- must have a unique ID. Is type ID stable across binary builds? Maybe need to
///          use a string ID instead? But then need to feed that the library using this registered
/// - Should we enforce that the previous two steps can only be done at creation time? Probably
///   Maybe initialize the map using Map<csname, cmd> and Vec<cmd>. Can provide csname<->str wrapper at
///   the VM level
///
/// - While running, insert macros
///     insert_macro(&str, macro)
/// - While running, alias commands by current name or using the special ID inserts
///     alias_control_sequence(cs_name, cs_name) -> undefined control sequence error
///     alias_registered_built_in(cs_name, cmd_id, variable_addr)
///     alias_character(cs_name, token::Token)
pub struct Map<S> {
    len: usize,
    func_map: GroupingVec<command::Fn<S>>,
    id_map: GroupingVec<std::any::TypeId>,
    doc_map: HashMap<CsName, &'static str>,
    // initial_builtins: HashMap<CsName, Command<S>> or Vec<Command<S>>
    // extra_builtins: Vec<Command<S>>
    // aliases: GroupingMap<CsName, Rc<Alias>>
}

impl<S> Default for Map<S> {
    fn default() -> Self {
        Self {
            func_map: Default::default(),
            len: 0,
            id_map: Default::default(),
            doc_map: Default::default(),
        }
    }
}

impl<S> Map<S> {
    pub fn new(
        initial_built_ins: HashMap<CsName, Command<S>>,
        _additional_built_ins: Vec<Command<S>>,
    ) -> Map<S> {
        let len = initial_built_ins.len();
        let mut func_map: GroupingVec<command::Fn<S>> = Default::default();
        let mut id_map: GroupingVec<std::any::TypeId> = Default::default();
        for (name, cmd) in initial_built_ins {
            func_map.insert(name.to_usize(), cmd.func, groupingmap::Scope::Local);
            id_map.insert(name.to_usize(), cmd.id, groupingmap::Scope::Local);
        }
        Self {
            len,
            func_map,
            id_map,
            doc_map: Default::default(),
        }
    }

    #[inline]
    pub fn get_fn(&self, name: &token::CsName) -> Option<&command::Fn<S>> {
        self.func_map.get(&name.to_usize())
    }

    pub fn get_id(&self, name: &token::CsName) -> std::any::TypeId {
        self.id_map
            .get(&name.to_usize())
            .copied()
            .unwrap_or_else(null_type_id)
    }

    pub fn get_command_slow(&self, name: &token::CsName) -> Option<command::Command<S>> {
        let func = match self.get_fn(name) {
            None => return None,
            Some(func) => func,
        }
        .clone();
        Some(Command {
            func,
            id: self.get_id(name),
            doc: self.doc_map.get(name).cloned(),
        })
    }

    #[inline]
    pub fn insert(
        &mut self,
        name: token::CsName,
        cmd: command::Command<S>,
        scope: groupingmap::Scope,
    ) -> bool {
        self.id_map.insert(name.to_usize(), cmd.id, scope);
        let existed = self.func_map.insert(name.to_usize(), cmd.func, scope);
        if !existed {
            self.len += 1;
        }
        existed
    }

    pub fn to_hash_map_slow(&self) -> HashMap<CsName, command::Command<S>> {
        let mut result = HashMap::new();
        for (key, _value) in self.func_map.backing_container().iter().enumerate() {
            let cs_name = match CsName::try_from_usize(key) {
                None => continue,
                Some(cs_name) => cs_name,
            };
            let cmd = match self.get_command_slow(&cs_name) {
                None => continue,
                Some(cmd) => cmd,
            };
            result.insert(cs_name, cmd);
        }
        result
    }

    pub fn begin_group(&mut self) {
        self.func_map.begin_group();
        self.id_map.begin_group();
    }

    pub fn end_group(&mut self) -> bool {
        self.func_map.end_group() && self.id_map.end_group()
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
    fn func_size() {
        assert_eq!(std::mem::size_of::<Fn<()>>(), 16);
    }
}
