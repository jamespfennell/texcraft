//! Defitions of TeX tokens, cat codes and token streams; and the Texcraft lexer.

pub mod catcode;
pub mod lexer;
pub mod stream;

use crate::tex::token::catcode::CatCode;
use std::rc::Rc;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Value {
    Character(char, CatCode),
    ControlSequence(char, String),
}

#[derive(Debug, Eq, Clone)]
pub struct Token {
    pub value: Value,
    pub source: Option<Source>,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            Value::Character(c, _) => {
                write![f, "{}", c]?;
            }
            Value::ControlSequence(prefix, name) => {
                write![f, "{}{}", prefix, name]?;
            }
        }
        Ok(())
    }
}
impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl Token {
    pub fn new_letter(c: char) -> Token {
        return Token {
            value: Value::Character(c, CatCode::Letter),
            source: None,
        };
    }
    pub fn new_other(c: char) -> Token {
        return Token {
            value: Value::Character(c, CatCode::Other),
            source: None,
        };
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Line {
    pub content: String,
    pub line_number: usize,
    pub file: Rc<String>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Source {
    pub line: Rc<Line>,
    pub position: usize,
}

pub trait Recorder {
    fn record(&mut self, token: &Token);
}

/// `VecRecorder` is a token recorder that copies tokens to an internal vector that can be subsequently retrieved.
pub struct VecRecorder {
    tokens: Vec<Token>,
}

impl VecRecorder {
    /// Creates a new `VecRecorder`.
    pub fn new() -> VecRecorder {
        VecRecorder { tokens: Vec::new() }
    }

    /// Returns all the tokens that have been recorded so far.
    pub fn tokens(self) -> Vec<Token> {
        self.tokens
    }
}

impl Recorder for VecRecorder {
    fn record(&mut self, token: &Token) {
        self.tokens.push(token.clone())
    }
}

/// Write a collection of tokens to a string.
pub fn write_tokens<'a, T>(tokens: T, add_source_new_lines: bool) -> String
where
    T: IntoIterator<Item = &'a Token>,
{
    let mut result: String = String::default();
    let mut current_line = 1;
    let mut preceeding_newlines = 0;
    for token in tokens.into_iter() {
        if let Some(source) = &token.source {
            if add_source_new_lines && source.line.line_number != current_line {
                if preceeding_newlines == 0 {
                    preceeding_newlines += 1;
                    result.push('\n');
                }
                current_line = source.line.line_number;
            }
        }
        match &token.value {
            Value::Character('\n', _) => {
                if preceeding_newlines == 0 {
                    preceeding_newlines = 1;
                    result.push('\n');
                }
            }
            Value::Character(c, catcode::CatCode::Space) => {
                result.push(*c);
            }
            Value::Character(c, _) => {
                preceeding_newlines = 0;
                result.push(*c);
            }
            Value::ControlSequence(c, s) => {
                if s == "par" {
                    if preceeding_newlines == 0 {
                        result.push('\n');
                        preceeding_newlines += 1;
                    }
                    if preceeding_newlines == 1 {
                        result.push('\n');
                        preceeding_newlines += 1;
                    }
                } else {
                    preceeding_newlines = 0;
                    result.push(*c);
                    result.push_str(s.as_str());
                }
            }
        }
    }
    result
}