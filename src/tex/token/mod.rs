//! Defitions of TeX tokens, cat codes and token streams; and the Texcraft lexer.

pub mod catcode;
pub mod lexer;
pub mod stream;

use crate::tex::token::catcode::CatCode;
use smol_str::SmolStr;
use std::rc::Rc;

/// Immutable string type used for storing control sequence names.
///
/// The implementation of this type is opaque so that it can be performance optimized
/// without worrying about downstream consumers.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CsName(SmolStr);

impl CsName {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
}

impl From<&str> for CsName {
    fn from(s: &str) -> Self {
        CsName(SmolStr::new(s))
    }
}

impl std::fmt::Display for CsName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write![f, "{}", self.0]
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Value {
    Character(char, CatCode),
    ControlSequence(char, CsName),
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
                write![f, "{}", c]
            }
            Value::ControlSequence(prefix, name) => {
                write![f, "{}{}", prefix, name.as_str()]
            }
        }
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
pub fn write_tokens<'a, T>(tokens: T) -> String
where
    T: IntoIterator<Item = &'a Token>,
{
    let mut result: String = String::default();
    let mut preceeding_space = true;
    for token in tokens.into_iter() {
        match &token.value {
            Value::Character(c, cat_code) => {
                // If this space character is redundant, don't write it.
                // An exception is made for newline characters, which are considered intentional.
                if preceeding_space && cat_code == &CatCode::Space && *c != '\n' {
                    continue;
                }
                result.push(*c);
                preceeding_space = cat_code == &CatCode::Space;
            }
            Value::ControlSequence(c, s) => {
                result.push(*c);
                result.push_str(s.as_str());
                result.push(' ');
                preceeding_space = false;
            }
        }
    }
    result
}
