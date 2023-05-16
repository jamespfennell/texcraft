//! Error types and error display logic.

use crate::token;
use crate::token::trace;
use crate::token::Token;
use crate::vm;
use colored::*;
use texcraft_stdext::algorithms::spellcheck;
use vm::RefVM;

pub struct DisplayBuilder<'a> {
    token: &'a trace::Trace,
    title: String,
    annotation: String,
    notes: Vec<String>,
}

impl<'a> DisplayBuilder<'a> {
    pub fn new<T: Into<String>>(token: &'a trace::Trace, title: T) -> DisplayBuilder<'a> {
        DisplayBuilder {
            token,
            title: T::into(title),
            annotation: String::new(),
            notes: Vec::new(),
        }
    }

    pub fn set_annotation<T: Into<String>>(mut self, annotation: T) -> DisplayBuilder<'a> {
        self.annotation = T::into(annotation);
        self
    }

    pub fn add_note<T: Into<String>>(mut self, note: T) -> DisplayBuilder<'a> {
        self.notes.push(T::into(note));
        self
    }
}

impl<'a> std::fmt::Display for DisplayBuilder<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let margin_width = self.token.line_number.to_string().len() + 1;
        print_error_header(f, &self.title)?;
        // TODO: support adding an annotation to the token
        writeln!(f, "{}", self.token)?;
        if !self.notes.is_empty() {
            print_line_with_bar(f, margin_width)?;
        }
        for note in self.notes.iter() {
            print_line_with_note(f, margin_width, note)?;
        }
        Ok(())
    }
}

fn bar() -> ColoredString {
    "|".bright_yellow()
}

fn print_line_with_bar(f: &mut std::fmt::Formatter<'_>, margin_width: usize) -> std::fmt::Result {
    writeln!(f, "{}{} ", " ".repeat(margin_width), bar())?;
    Ok(())
}

fn print_line_with_note(
    f: &mut std::fmt::Formatter<'_>,
    margin_width: usize,
    note: &str,
) -> std::fmt::Result {
    writeln!(
        f,
        "{}{} {} {}",
        " ".repeat(margin_width),
        "=".bright_yellow().bold(),
        "note:".bold(),
        note
    )?;
    Ok(())
}

fn print_error_header(f: &mut std::fmt::Formatter<'_>, message: &str) -> std::fmt::Result {
    writeln!(f, "{}: {}", "Error".bright_red().bold(), message.bold())?;
    Ok(())
}

#[derive(Debug)]
pub struct TokenContext {
    note: String,
}

/// Error that is returned when an unexpected token is encountered.
#[derive(Debug)]
pub struct TokenError {
    token: Token,
    message: String,
    notes: Vec<String>,
    traceback_result: Option<trace::Trace>,
}

impl std::error::Error for TokenError {}

impl std::fmt::Display for TokenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let tr = match &self.traceback_result {
            None => {
                writeln![f, "Token error: {}", self.message]?;
                writeln![f, "This error does not have any context because `texcraft_core::error::add_context` was not called"]?;
                return Ok(());
            }
            Some(tr) => tr,
        };

        let margin_width = tr.line_number.to_string().len() + 1;
        print_error_header(f, &self.message)?;
        // TODO: support adding an annotation to the token
        writeln!(f, "{tr}")?;
        if !self.notes.is_empty() {
            print_line_with_bar(f, margin_width)?;
        }
        for note in self.notes.iter() {
            print_line_with_note(f, margin_width, note)?;
        }
        Ok(())
    }
}

impl TokenError {
    pub fn new<T: Into<String>>(token: Token, message: T) -> TokenError {
        let message = T::into(message);
        TokenError {
            token,
            message,
            notes: vec![],
            traceback_result: None,
        }
    }

    pub fn add_note<T: Into<String>>(mut self, note: T) -> TokenError {
        self.notes.push(T::into(note));
        self
    }

    pub fn cast(self) -> anyhow::Error {
        anyhow::Error::from(self)
    }

    fn add_context<S>(&mut self, execution_input: &vm::ExecutionInput<S>) {
        self.traceback_result = Some(execution_input.trace(self.token))
    }
}

/// Error that is returned with the input file ends unexpectedly.
#[derive(Debug)]
pub struct EndOfInputError {
    title: String,
    notes: Vec<String>,
    contexts: Vec<TokenContext>,
}

impl EndOfInputError {
    pub fn new<T: Into<String>>(title: T) -> EndOfInputError {
        EndOfInputError {
            title: T::into(title),
            notes: vec![],
            contexts: vec![],
        }
    }

    pub fn add_note<T: Into<String>>(mut self, note: T) -> EndOfInputError {
        self.notes.push(T::into(note));
        self
    }

    pub fn add_token_context<T: Into<String>>(self, _token: &Token, _note: T) -> EndOfInputError {
        self
    }

    pub fn cast(self) -> anyhow::Error {
        anyhow::Error::from(self)
    }

    pub fn add_context<S>(&mut self, _execution_input: &vm::ExecutionInput<S>) {}
}

impl std::error::Error for EndOfInputError {}

impl std::fmt::Display for EndOfInputError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        print_error_header(f, &self.title)?;
        if !self.notes.is_empty() {
            print_line_with_bar(f, 3)?;
        }
        for note in self.notes.iter() {
            print_line_with_note(f, 3, note)?;
        }
        for context in self.contexts.iter() {
            writeln!(f)?;
            print_error_header(f, &context.note)?;
            // context.line.fmt(f, 3, "")?;
        }
        Ok(())
    }
}

pub fn add_context<S>(error: &mut anyhow::Error, execution_input: &vm::ExecutionInput<S>) {
    if let Some(error) = error.downcast_mut::<EndOfInputError>() {
        error.add_context(execution_input);
    }
    if let Some(error) = error.downcast_mut::<TokenError>() {
        error.add_context(execution_input);
    }
}

pub fn new_undefined_command_error<S>(token: token::Token, state: &vm::VM<S>) -> anyhow::Error {
    let a = "expected a control sequence".to_string();
    let name = match &token.value() {
        token::Value::ControlSequence(name) => state.cs_name_interner().resolve(*name).expect(""),
        _ => &a,
    };

    let mut cs_names = Vec::<String>::new();
    for (cs_name, _) in state.get_commands_as_map_slow().into_iter() {
        cs_names.push(cs_name);
    }

    let close_cs_name = spellcheck::find_close_words(cs_names, name);
    let title = format!["undefined control sequence \\{}", &name];
    let mut err = TokenError::new(token, title);

    err = err.add_note(format![
        "did you mean \\{}?\n",
        close_cs_name[0].right().bold(),
    ]);

    err.cast()
}
