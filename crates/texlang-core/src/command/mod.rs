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
use once_cell::sync::OnceCell;
use std::num;
use std::rc;
use std::sync;

mod map;

pub use map::Map;

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
/// Consists of a function, and optional tag, and an optional doc string.
pub struct Command<S> {
    func: Fn<S>,
    // TODO: should we just put the tag on the two Fns (expansion, execution) that support it?
    tag: Option<Tag>,
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
    pub fn with_tag(mut self, tag: Tag) -> Command<S> {
        self.tag = Some(tag);
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

    pub fn tag(&self) -> Option<Tag> {
        self.tag
    }

    pub fn doc(&self) -> Option<&'static str> {
        self.doc
    }
}

impl<S> std::fmt::Debug for Fn<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "todo")?;
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
            tag: self.tag,
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
            tag: None,
            doc: None,
        }
    }
}

/// A tag is a piece of metadata that is optionally attached to a command.
///
/// Tags are used to implement certain TeX language semantics.
/// An example is TeX conditionals.
/// When a TeX conditional statement evaluates to false, the `\if` command must scan
///     the input stream until it finds either an `\else` or `\fi` command.
/// (The tokens scanned in this process are in the true branch of the conditional,
///     and must thus be discarded.)
/// Tags are the mechanism by which the scanning algorithm can
///     determine if a token corresponds to an `\else` of `\fi` command.
/// Concretely, both `\else` of `\fi` command have unique tags associated to them.
/// When scanning the stream,
///     if a token is a command token then the tag for the associated command is
///     compared to the known tags for `\else` and `\fi`.
/// If the tags match, the true branch is finished.
///
/// In general, TeX commands interface with the VM in two ways.
/// The first most common way is when the main VM loop or expansion loop encounters a command.
/// The loop invokes the command's associated Rust function.
/// One can think of the Rust function as providing the behaviour of the command in this context.
///
/// The second way is when a different command, like a conditional command, performs some operation
///     that is dependent on the commands it reads out of the input stream.
/// In this context the commands in the input stream provide behaviour using tags.
/// The `\else` command having the specific else tag results in the conditional branch processing completing.
///
/// Note that the same tag can be used for multiple commands,
/// but each command can only have one tag.
///
/// ## Implementation details
///
/// Tags are non-zero 32 bit integers.
/// The first tag created has value 1, the second tag has value 2, and so on.
/// A global mutex is used to store the next tag value.
/// Tags have the property that `Option<Tag>` takes up 4 bytes in memory.
#[derive(PartialEq, Eq, Clone, Copy, Debug, PartialOrd, Ord, Hash)]
pub struct Tag(num::NonZeroU16);

static NEXT_TAG_VALUE: sync::Mutex<u16> = sync::Mutex::new(1);

impl Tag {
    /// Creates a new unique tag.
    ///
    /// ```
    /// # use texlang_core::command::Tag;
    /// let tag_1 = Tag::new();
    /// let tag_2 = Tag::new();
    /// assert_ne!(tag_1, tag_2);
    /// ```
    pub fn new() -> Tag {
        let mut n = NEXT_TAG_VALUE.lock().unwrap();
        let tag = Tag(num::NonZeroU16::new(*n).unwrap());
        *n = n.checked_add(1).unwrap();
        tag
    }
}

/// A static tag enables creating a tag in a static variable.
///
/// ```
/// # use texlang_core::command::StaticTag;
/// static TAG: StaticTag = StaticTag::new();
///
/// let first_get = TAG.get();
/// let second_get = TAG.get();
/// assert_eq!(first_get, second_get);
/// ```
pub struct StaticTag(OnceCell<Tag>);

impl StaticTag {
    /// Create a new static tag.
    pub const fn new() -> StaticTag {
        StaticTag(OnceCell::new())
    }

    /// Get the actual [Tag] out of this [StaticTag].
    /// Repeated calls to this function return the same tag.
    ///
    /// This is not a trivial getter.
    /// The [Tag] is lazily constructed so even subsequent calls to this getter must do some work to check if the [Tag]
    ///     exists or not.
    /// For very hot code paths it is advised to cache the return value somewhere, for example in a relevent command's state.
    pub fn get(&self) -> Tag {
        *self.0.get_or_init(Tag::new)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn func_size() {
        assert_eq!(std::mem::size_of::<Fn<()>>(), 16);
    }

    static STATIC_TAG_1: StaticTag = StaticTag::new();
    static STATIC_TAG_2: StaticTag = StaticTag::new();

    #[test]
    fn tag() {
        let tag_1_val_1 = STATIC_TAG_1.get();
        let tag_2_val_1 = STATIC_TAG_2.get();
        let other_tag_1 = Tag::new();
        let tag_1_val_2 = STATIC_TAG_1.get();
        let tag_2_val_2 = STATIC_TAG_2.get();
        let other_tag_2 = Tag::new();

        assert_eq!(tag_1_val_1, tag_1_val_2);
        assert_eq!(tag_2_val_1, tag_2_val_2);

        assert_ne!(tag_1_val_1, tag_2_val_2);
        assert_ne!(tag_1_val_1, other_tag_1);
        assert_ne!(tag_1_val_1, other_tag_2);
    }

    #[test]
    fn tag_size() {
        assert_eq!(std::mem::size_of::<Option<Tag>>(), 2);
    }
}
