//! Error handling

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
pub struct TracedError {
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

impl TracedError {
    pub(crate) fn new(
        error: Box<dyn TexError>,
        tracer: &trace::Tracer,
        cs_name_interner: &token::CsNameInterner,
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
        TracedError {
            error,
            stack_trace: vec![],
            token_traces,
            end_of_input_trace,
        }
    }
}

#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct StackTraceElement {
    pub context: PropagationContext,
    pub token: token::Token,
    pub trace: trace::SourceCodeTrace,
}

/// Texlang error type
///
/// Note that serializing and deserializing this type results in type erasure.
/// Also the serialization format is private.
/// This is not by design: the minimal amount of work was done to make the type
///     serializable, and future work to make this better is welcome!
#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Error {
    pub(crate) traced: TracedError,
}

impl std::fmt::Display for TracedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        display::format_error(f, self)
    }
}

impl Error {
    pub fn new(traced: TracedError) -> Self {
        Self { traced }
    }
    pub fn new_propagated<S>(
        vm: &vm::VM<S>,
        context: PropagationContext,
        token: token::Token,
        mut error: Box<Error>,
    ) -> Box<Error> {
        error.traced.stack_trace.push(StackTraceElement {
            context,
            token,
            trace: vm.trace(token),
        });
        error
    }
}

#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Kind {
    Token(token::Token),
    EndOfInput,
    FailedPrecondition,
}

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

#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(::serde::Serialize, ::serde::Deserialize))]
pub enum PropagationContext {
    Expansion,
    Execution,
    VariableIndex,
    VariableAssignment,
}

impl PropagationContext {
    fn action(&self) -> &'static str {
        match self {
            PropagationContext::Expansion => "expanding this command",
            PropagationContext::Execution => "executing this command",
            PropagationContext::VariableIndex => "determining the index of this variable",
            PropagationContext::VariableAssignment => {
                "determining the value to assign to this variable"
            }
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

// After the errors work creating custom errors will be easy.
#[derive(Debug)]
pub struct SimpleEndOfInputError {
    pub title: String,
    pub text_notes: Vec<String>,
}

impl SimpleEndOfInputError {
    /// Create a new simple end of input error.
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

impl TexError for SimpleEndOfInputError {
    fn kind(&self) -> Kind {
        Kind::EndOfInput
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

#[derive(Debug)]
pub struct UndefinedCommandError {
    pub token: token::Token,
    pub close_names: Vec<WordDiff>,
}

impl UndefinedCommandError {
    pub fn new<S>(vm: &vm::VM<S>, token: token::Token) -> UndefinedCommandError {
        let name = match &token.value() {
            token::Value::CommandRef(command_ref) => command_ref.to_string(vm.cs_name_interner()),
            _ => panic!("undefined command error does not work for non-command-ref tokens"),
        };
        let mut all_names = Vec::<String>::new();
        for (cs_name, _) in vm.get_commands_as_map_slow().into_iter() {
            all_names.push(cs_name);
        }
        let close_names = spellcheck::find_close_words(all_names, &name);

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
        use colored::Colorize;
        if let Some(close_name) = self.close_names.first() {
            notes.push(format!["did you mean \\{}?\n", close_name.right().bold(),].into());
        }
        notes
    }
}
