//! Error handling
//!
//! This is reference documentation.
//! The Texlang documentation has a [dedicated page about error handling
//! ](<https://texcraft.dev/texlang/09-errors.html>).

use std::collections::HashMap;

use crate::token;
use crate::token::trace;
use crate::vm;
use texcraft_stdext::algorithms::spellcheck::{self, WordDiff};

pub mod display;

/// A fully traced error
///
/// Note that serializing and deserializing this type results in type erasure.
/// Also the serialization format is private.
/// This is not by design: the minimal amount of work was done to make the type
///     serializable, and future work to make this better is welcome!
#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct TracedTexError {
    #[cfg_attr(
        feature = "serde",
        serde(
            serialize_with = "serialize_error",
            deserialize_with = "deserialize_error"
        )
    )]
    pub error: Box<dyn TexError>,
    pub stack_trace: Vec<StackTraceElement>,
    pub token_traces: HashMap<token::Token, trace::SourceCodeTrace>,
    pub end_of_input_trace: Option<trace::SourceCodeTrace>,
}

#[cfg(feature = "serde")]
#[allow(clippy::borrowed_box)] // we need this exact function signature for serde.
fn serialize_error<S>(value: &Box<dyn TexError>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    use serde::Serialize;
    let serializable_error = SerializableError {
        kind: value.kind().clone(),
        title: value.title(),
        notes: value.notes(),
        source_annotation: value.source_annotation(),
    };
    serializable_error.serialize(serializer)
}

#[cfg(feature = "serde")]
fn deserialize_error<'de, D>(deserializer: D) -> Result<Box<dyn TexError>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    use serde::Deserialize;
    let serializable_error = SerializableError::deserialize(deserializer)?;
    Ok(Box::new(serializable_error))
}

#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
struct SerializableError {
    kind: Kind,
    title: String,
    notes: Vec<display::Note>,
    source_annotation: String,
}

impl TexError for SerializableError {
    fn kind(&self) -> Kind {
        self.kind.clone()
    }
    fn title(&self) -> String {
        self.title.clone()
    }
    fn notes(&self) -> Vec<display::Note> {
        self.notes.clone()
    }
    fn source_annotation(&self) -> String {
        self.source_annotation.clone()
    }
}

impl TracedTexError {
    pub(crate) fn new(
        error: Box<dyn TexError>,
        tracer: &trace::Tracer,
        cs_name_interner: &token::CsNameInterner,
        stack_trace: Vec<StackTraceElement>,
    ) -> Self {
        let (end_of_input_trace, mut tokens) = match error.kind() {
            Kind::Token(token) => (None, vec![token]),
            Kind::EndOfInput => (Some(tracer.trace_end_of_input()), vec![]),
            Kind::FailedPrecondition => (None, vec![]),
        };
        for note in error.notes() {
            if let display::Note::SourceCodeTrace(_, token) = note {
                tokens.push(token);
            }
        }
        let token_traces: HashMap<token::Token, trace::SourceCodeTrace> = tokens
            .into_iter()
            .map(|token| (token, tracer.trace(token, cs_name_interner)))
            .collect();
        TracedTexError {
            error,
            stack_trace,
            token_traces,
            end_of_input_trace,
        }
    }
}

/// Implementations of this trait describe an error in which in the input ended prematurely.
pub trait EndOfInputError: std::fmt::Debug + 'static {
    fn doing(&self) -> String;
    fn notes(&self) -> Vec<display::Note> {
        vec![]
    }
}

/// An error for work-in-progress Texlang code.
///
/// When working on Texlang code it's often nice to figure out the logic first,
///     and then go through later to polish the error cases.
/// This function returns a "TODO" error that helps with this process.
///
/// Use the return value of this function in any place you plan to generate an error.
/// Later on, follow Texlang best practices and create a specific error
///     type for the case with a good error message.
#[allow(non_snake_case)]
pub fn TODO() -> impl TexError + EndOfInputError {
    TodoError {}
}

#[derive(Debug)]
struct TodoError {}

impl EndOfInputError for TodoError {
    fn doing(&self) -> String {
        "? (TODO: add a specific end of input error for this case.)".into()
    }
    fn notes(&self) -> Vec<display::Note> {
        vec![
            "the Rust source code uses `texlang::error::TODO()` for this error case".into(),
            "a more specific end of input error needs to be added".into(),
        ]
    }
}

impl TexError for TodoError {
    fn kind(&self) -> Kind {
        Kind::FailedPrecondition
    }
    fn title(&self) -> String {
        "? (TODO: add a specific error for this case.)".into()
    }
    fn notes(&self) -> Vec<display::Note> {
        vec![
            "the Rust source code uses `texlang::error::TODO()` for this error case".into(),
            "a more specific error needs to be added".into(),
        ]
    }
}

#[derive(Debug)]
pub(crate) struct EofError {
    doing: String,
    notes: Vec<display::Note>,
}

impl EofError {
    pub(crate) fn new<E: EndOfInputError>(err: E) -> Self {
        Self {
            doing: err.doing(),
            notes: err.notes(),
        }
    }
}

impl TexError for EofError {
    fn kind(&self) -> Kind {
        Kind::EndOfInput
    }

    fn title(&self) -> String {
        format!("Unexpected end of input while {}", self.doing)
    }
    fn notes(&self) -> Vec<display::Note> {
        self.notes.clone()
    }
}

/// Element of a stack trace.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct StackTraceElement {
    pub context: OperationKind,
    pub token: token::Token,
    pub trace: trace::SourceCodeTrace,
}

impl std::fmt::Display for TracedTexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        display::format_error(f, self)
    }
}

/// The type of an error.
#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Kind {
    /// An error at a particular TeX token.
    ///
    /// For example, a TeX command expects a number but the next token is a letter.
    Token(token::Token),
    /// An end-of-input error.
    ///
    /// For example, a TeX command expects a number but there is no more input.
    EndOfInput,
    /// Some external condition does not hold and so the TeX code is incorrect.
    ///
    /// For example, a TeX command tries to open a file a particular path,
    ///     but the file does not exist.
    FailedPrecondition,
}

/// Implementations of this trait describe an error in TeX source code.
pub trait TexError: std::fmt::Debug + 'static {
    fn kind(&self) -> Kind;

    fn title(&self) -> String;

    fn notes(&self) -> Vec<display::Note> {
        vec![]
    }

    fn source_annotation(&self) -> String {
        TexError::default_source_annotation(self)
    }

    fn default_source_annotation(&self) -> String {
        match TexError::kind(self) {
            Kind::Token(t) => match (t.char(), t.cat_code()) {
                (Some(c), Some(code)) => {
                    format!["character token with value {c} and category code {code}",]
                }
                _ => "control sequence".to_string(),
            },
            Kind::EndOfInput => "input ended here".into(),
            Kind::FailedPrecondition => "error occurred while running this command".into(),
        }
    }

    fn source_code_trace_override(&self) -> Option<&trace::SourceCodeTrace> {
        None
    }
    // TODO: have a method that returns the exact error messages as Knuth's TeX
    // The method will return a vector of static strings
}

#[derive(Copy, Clone, Debug)]
#[cfg_attr(feature = "serde", derive(::serde::Serialize, ::serde::Deserialize))]
pub enum OperationKind {
    Expansion,
    Execution,
    VariableIndex,
    VariableAssignment,
}

impl OperationKind {
    fn action(&self) -> &'static str {
        match self {
            OperationKind::Expansion => "expanding this command",
            OperationKind::Execution => "executing this command",
            OperationKind::VariableIndex => "determining the index of this variable",
            OperationKind::VariableAssignment => "determining the value to assign to this variable",
        }
    }
}

#[derive(Debug)]
pub struct SimpleTokenError {
    pub token: token::Token,
    pub title: String,
}

impl SimpleTokenError {
    /// Create a new simple token error.
    pub fn new<T: AsRef<str>>(token: token::Token, title: T) -> SimpleTokenError {
        SimpleTokenError {
            token,
            title: title.as_ref().into(),
        }
    }
}

impl TexError for SimpleTokenError {
    fn kind(&self) -> Kind {
        Kind::Token(self.token)
    }

    fn title(&self) -> String {
        self.title.clone()
    }
}

#[derive(Debug)]
pub struct SimpleFailedPreconditionError {
    pub title: String,
    pub text_notes: Vec<String>,
}

impl SimpleFailedPreconditionError {
    /// Create a new simple failed precondition error.
    pub fn new<T: AsRef<str>>(title: T) -> Self {
        Self {
            title: title.as_ref().into(),
            text_notes: vec![],
        }
    }

    pub fn with_note<T: Into<String>>(mut self, note: T) -> Self {
        self.text_notes.push(note.into());
        self
    }
}

impl TexError for SimpleFailedPreconditionError {
    fn kind(&self) -> Kind {
        Kind::FailedPrecondition
    }

    fn title(&self) -> String {
        self.title.clone()
    }

    fn notes(&self) -> Vec<display::Note> {
        let mut notes = vec![];
        for text_note in &self.text_notes {
            notes.push(text_note.into())
        }
        notes
    }
}

/// Concrete error for the case when a command is undefined.
///
/// This error is returned when a control sequence or active character
///     is not defined.
#[derive(Debug)]
pub struct UndefinedCommandError {
    /// The token that was referred to an undefined command.
    pub token: token::Token,
    /// Control sequences that are spelled similarly to the token.
    pub close_names: Vec<WordDiff>,
}

impl UndefinedCommandError {
    /// Create a new undefined command error.
    pub fn new<S>(vm: &vm::VM<S>, token: token::Token) -> UndefinedCommandError {
        let name = match &token.value() {
            token::Value::CommandRef(command_ref) => command_ref.to_string(vm.cs_name_interner()),
            _ => panic!("undefined command error does not work for non-command-ref tokens"),
        };
        let mut all_names = Vec::<&str>::new();
        for (cs_name, _) in vm.get_commands_as_map_slow().into_iter() {
            all_names.push(cs_name);
        }
        let close_names = spellcheck::find_close_words(&all_names, &name);

        UndefinedCommandError { token, close_names }
    }
}

impl TexError for UndefinedCommandError {
    fn kind(&self) -> Kind {
        Kind::Token(self.token)
    }

    fn title(&self) -> String {
        "undefined control sequence".into()
    }

    fn notes(&self) -> Vec<display::Note> {
        let mut notes: Vec<display::Note> = Default::default();
        use texcraft_stdext::color::Colorize;
        if let Some(close_name) = self.close_names.first() {
            notes.push(format!["did you mean \\{}?\n", close_name.right().bold(),].into());
        }
        notes
    }
}
