//! Error types and error display logic.

use crate::runtime;
use crate::token;
use crate::token::{Token, Value};
use colored::*;
use texcraft_stdext::algorithms::spellcheck;

#[derive(Debug)]
struct Line {
    line: String,
    line_number: usize,
    position: usize,
    width: usize,
    _file_description: String,
}

impl Line {
    fn new(token: &Token) -> Option<Line> {
        Some(Line {
            line: "todo".to_string(),
            line_number: 0,
            position: 0,
            width: match token.value() {
                Value::ControlSequence(_) => 100, // TODO + name.len(),
                _ => 1,
            },
            _file_description: "todo".to_string(),
        })
    }
}

impl Line {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        margin_width: usize,
        annotation: &str,
    ) -> std::fmt::Result {
        let _prefix = " ".repeat(margin_width);
        writeln!(
            f,
            "{}{} foo.tex:{}:{}",
            " ".repeat(margin_width - 1),
            ">>>".bright_yellow().bold(),
            self.line_number,
            self.position
        )?;
        print_line_with_bar(f, margin_width)?;
        writeln!(
            f,
            "{}{} {} {}",
            " ".repeat(margin_width - self.line_number.to_string().len() - 1),
            self.line_number.to_string().bright_yellow(),
            bar(),
            self.line.trim_end()
        )?;
        writeln!(
            f,
            "{}{} {}{} {}",
            " ".repeat(margin_width),
            bar(),
            " ".repeat(self.position),
            "^".repeat(self.width).bright_red().bold(),
            annotation.bright_red(),
        )?;
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
    line: Line,
}

/// Error that is returned when an unexpected token is encountered.
#[derive(Debug)]
pub struct TokenError {
    _token: Token,
    _message: String,
    notes: Vec<String>,
}

impl std::error::Error for TokenError {}

impl std::fmt::Display for TokenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        /*
        let margin_width = self.line.line_number.to_string().len() + 1;
        print_error_header(f, &self.message)?;
        self.line.fmt(f, margin_width, &self.s)?;
        if !self.notes.is_empty() {
            print_line_with_bar(f, 3)?;
        }
        for note in self.notes.iter() {
            print_line_with_note(f, 3, note)?;
        }
        */
        print_line_with_note(f, 3, "Token error")?;
        Ok(())
    }
}

impl TokenError {
    pub fn new<T: Into<String>>(token: Token, message: T) -> TokenError {
        TokenError {
            _token: token,
            _message: T::into(message),
            notes: vec![],
        }
    }

    pub fn add_note<T: Into<String>>(mut self, note: T) -> TokenError {
        self.notes.push(T::into(note));
        self
    }

    pub fn cast(self) -> anyhow::Error {
        anyhow::Error::from(self)
    }
}

/// Error that is returned with the input file ends unexpectedly.
#[derive(Debug)]
pub struct EndOfInputError {
    title: String,
    last_line: Option<Line>,
    notes: Vec<String>,
    contexts: Vec<TokenContext>,
}

impl EndOfInputError {
    pub fn new<T: Into<String>>(title: T) -> EndOfInputError {
        EndOfInputError {
            title: T::into(title),
            last_line: None,
            notes: vec![],
            contexts: vec![],
        }
    }

    pub fn add_note<T: Into<String>>(mut self, note: T) -> EndOfInputError {
        self.notes.push(T::into(note));
        self
    }

    pub fn add_token_context<T: Into<String>>(mut self, token: &Token, note: T) -> EndOfInputError {
        if let Some(line) = Line::new(token) {
            self.contexts.push(TokenContext {
                note: T::into(note),
                line,
            });
        }
        self
    }

    pub fn cast(self) -> anyhow::Error {
        anyhow::Error::from(self)
    }

    pub fn add_context<S>(&mut self, _execution_input: &runtime::ExecutionInput<S>) {
        /*
        if let Some(last_line) = execution_input.controller().last_non_empty_line() {
            self.last_line = Some(Line {
                line: last_line.content.clone(),
                line_number: last_line.line_number,
                position: last_line.content.trim_end().len(),
                width: 1,
                _file_description: String::clone(last_line.file.as_ref()),
            })
        }
        */
    }
}

impl std::error::Error for EndOfInputError {}

impl std::fmt::Display for EndOfInputError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        print_error_header(f, &self.title)?;
        if let Some(last_line) = &self.last_line {
            last_line.fmt(f, 3, "input ended here")?;
        }
        if !self.notes.is_empty() {
            print_line_with_bar(f, 3)?;
        }
        for note in self.notes.iter() {
            print_line_with_note(f, 3, note)?;
        }
        for context in self.contexts.iter() {
            writeln!(f)?;
            print_error_header(f, &context.note)?;
            context.line.fmt(f, 3, "")?;
        }
        Ok(())
    }
}

pub fn add_context<S>(error: &mut anyhow::Error, execution_input: &runtime::ExecutionInput<S>) {
    if let Some(error) = error.downcast_mut::<EndOfInputError>() {
        error.add_context(execution_input);
    }
}

pub fn new_undefined_cs_error<S>(token: token::Token, state: &runtime::Env<S>) -> anyhow::Error {
    let a = "expected a control sequence".to_string();
    let name = match &token.value() {
        token::Value::ControlSequence(name) => state.cs_name_interner().resolve(name).expect(""),
        _ => &a,
    };

    let mut cs_names = Vec::<String>::new();
    for (cs_name, _) in state.get_commands_as_map().into_iter() {
        cs_names.push(cs_name);
    }

    let close_cs_name = spellcheck::find_close_words(cs_names, name);
    let title = format!["undefined control sequence {}", &token];
    let mut err = TokenError::new(token, title);

    err = err.add_note(format![
        "did you mean to write \\{}? (diff: \\{})\n",
        close_cs_name[0].right().bold(),
        close_cs_name[0].colored()
    ]);

    err.cast()
}
