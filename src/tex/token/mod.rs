//! Defitions of TeX tokens, cat codes and token streams; and the Texcraft lexer.

pub mod catcode;
pub mod lexer;
pub mod stream;

use crate::tex::token::catcode::CatCode;
use std::rc::Rc;
use string_interner::{DefaultSymbol, StringInterner};

pub struct CsNameInterner {
    interner: StringInterner,
}

impl CsNameInterner {
    pub fn new() -> CsNameInterner {
        CsNameInterner {
            interner: StringInterner::new(),
        }
    }

    pub fn get_or_intern<T: AsRef<str>>(&mut self, string: T) -> CsName {
        CsName(self.interner.get_or_intern(string))
    }

    pub fn resolve(&self, cs_name: &CsName) -> Option<&str> {
        self.interner.resolve(cs_name.0)
    }
}

impl Default for CsNameInterner {
    fn default() -> Self {
        Self::new()
    }
}

/// Immutable string type used for storing control sequence names.
///
/// The implementation of this type is opaque so that it can be performance optimized
/// without worrying about downstream consumers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CsName(DefaultSymbol);

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Value {
    Character(char, CatCode),
    ControlSequence(char, CsName),
}

#[derive(Debug, Eq, Clone, Copy)]
pub struct Token {
    value: Value,
    // pub source: Option<Source>,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            Value::Character(c, _) => {
                write![f, "{}", c]
            }
            Value::ControlSequence(prefix, _) => {
                write![f, "{}todo", prefix] // TODO
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
    pub fn new_character(c: char, cat_code: CatCode) -> Token {
        Token {
            value: Value::Character(c, cat_code),
        }
    }

    pub fn new_control_sequence(prefix: char, name: CsName) -> Token {
        Token {
            value: Value::ControlSequence(prefix, name),
        }
    }

    pub fn value(&self) -> Value {
        self.value
    }

    pub fn source(&self) -> Option<&Source> {
        None
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

/// Write a collection of tokens to a string.
pub fn write_tokens<'a, T>(tokens: T, interner: &CsNameInterner) -> String
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
                let name = interner.resolve(s).unwrap_or("invalidCsName");
                result.push(*c);
                result.push_str(name);
                result.push(' ');
                preceeding_space = false;
            }
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn token_size() {
        assert_eq!(std::mem::size_of::<Token>(), 12);
        assert_eq!(std::mem::size_of::<Result<Token, ()>>(), 12);
        assert_eq!(std::mem::size_of::<Result<Option<Token>, ()>>(), 12);
        assert_eq!(std::mem::size_of::<anyhow::Result<Token>>(), 16);
    }
}
