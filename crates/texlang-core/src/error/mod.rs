//! Error types and error display logic.

use crate::token;
use crate::token::trace;
use crate::vm;
use texcraft_stdext::algorithms::spellcheck::{self, WordDiff};

pub mod display;

#[derive(Debug)]
pub enum Error {
    Tex(Box<dyn TexError + 'static>),
    Propagated(PropagatedError),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        display::format_error(f, self)
    }
}

impl Error {
    pub fn new_propagated<S>(
        vm: &vm::VM<S>,
        context: PropagationContext,
        token: token::Token,
        error: Box<Error>,
    ) -> Box<Error> {
        Box::new(Error::Propagated(PropagatedError {
            context,
            token,
            trace: vm.trace(token),
            error,
        }))
    }

    pub fn stack_view(&self) -> (Vec<&PropagatedError>, &dyn TexError) {
        let mut stack: Vec<&PropagatedError> = vec![];
        let mut last = self;
        loop {
            match last {
                Error::Tex(error) => {
                    return (stack, error.as_ref());
                }
                Error::Propagated(propagated) => {
                    stack.push(propagated);
                    last = &propagated.error;
                }
            }
        }
    }
}

impl<T: TexError + 'static> From<T> for Box<Error> {
    fn from(err: T) -> Self {
        Box::new(Error::Tex(Box::new(err)))
    }
}

#[derive(Debug)]
pub struct PropagatedError {
    pub context: PropagationContext,
    pub token: token::Token,
    pub trace: trace::SourceCodeTrace,
    pub error: Box<Error>,
}

#[derive(Debug)]
pub enum Kind<'a> {
    Token(&'a trace::SourceCodeTrace),
    EndOfInput(&'a trace::SourceCodeTrace),
    FailedPrecondition,
}

pub trait TexError: std::fmt::Debug {
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
            Kind::Token(s) => {
                let t = s.token.unwrap();
                match (t.char(), t.cat_code()) {
                    (Some(c), Some(code)) => {
                        format!["character token with value {c} and category code {code}",]
                    }
                    _ => "control sequence".to_string(),
                }
            }
            Kind::EndOfInput(_) => "input ended here".into(),
            Kind::FailedPrecondition => {
                "failed precondition error while running this command".into()
            }
        }
    }
}

#[derive(Debug)]
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
    pub trace: trace::SourceCodeTrace,
    pub title: String,
}

impl SimpleTokenError {
    /// Create a new simple token error.
    #[allow(clippy::new_ret_no_self)]
    pub fn new<S, T: AsRef<str>>(
        vm: &vm::VM<S>,
        token: token::Token,
        title: T,
    ) -> SimpleTokenError {
        let trace = vm.trace(token);
        SimpleTokenError {
            token,
            trace,
            title: title.as_ref().into(),
        }
    }
}

impl TexError for SimpleTokenError {
    fn kind(&self) -> Kind {
        Kind::Token(&self.trace)
    }

    fn title(&self) -> String {
        self.title.clone()
    }
}

#[derive(Debug)]
pub struct SimpleEndOfInputError {
    pub trace: trace::SourceCodeTrace,
    pub title: String,
    pub text_notes: Vec<String>,
}

impl SimpleEndOfInputError {
    /// Create a new simple end of input error.
    pub fn new<S, T: AsRef<str>>(vm: &vm::VM<S>, title: T) -> Self {
        Self {
            trace: vm.trace_end_of_input(),
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
        Kind::EndOfInput(&self.trace)
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
    pub trace: trace::SourceCodeTrace,
    pub close_names: Vec<WordDiff>,
}

impl UndefinedCommandError {
    pub fn new<S>(vm: &vm::VM<S>, token: token::Token) -> UndefinedCommandError {
        let name = match &token.value() {
            token::Value::ControlSequence(name) => vm.cs_name_interner().resolve(*name).expect(""),
            _ => panic!("undefined command error does not work for non-control sequences tokens"),
        };
        let mut all_names = Vec::<String>::new();
        for (cs_name, _) in vm.get_commands_as_map_slow().into_iter() {
            all_names.push(cs_name);
        }
        let close_names = spellcheck::find_close_words(all_names, name);

        UndefinedCommandError {
            trace: vm.trace(token),
            close_names,
        }
    }
}

impl TexError for UndefinedCommandError {
    fn kind(&self) -> Kind {
        Kind::Token(&self.trace)
    }

    fn title(&self) -> String {
        format!["undefined control sequence {}", &self.trace.value]
    }

    fn notes(&self) -> Vec<display::Note> {
        use colored::Colorize;
        vec![format!["did you mean \\{}?\n", self.close_names[0].right().bold(),].into()]
    }
}
