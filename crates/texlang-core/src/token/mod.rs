//! Defitions of TeX tokens, cat codes and token streams; and the Texcraft lexer.

pub mod catcode;
pub mod lexer;

use crate::token::catcode::CatCode;
use string_interner::{DefaultSymbol, StringInterner, Symbol};

pub struct CsNameInterner {
    interner: StringInterner,
}

impl CsNameInterner {
    pub fn new() -> CsNameInterner {
        CsNameInterner {
            interner: StringInterner::new(),
        }
    }

    #[inline]
    pub fn get_or_intern<T: AsRef<str>>(&mut self, string: T) -> CsName {
        CsName(self.interner.get_or_intern(string))
    }

    #[inline]
    pub fn get<T: AsRef<str>>(&self, string: T) -> Option<CsName> {
        self.interner.get(string).map(CsName)
    }

    #[inline]
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

impl CsName {
    #[inline]
    pub fn to_usize(&self) -> usize {
        self.0.to_usize()
    }

    pub fn try_from_usize(u: usize) -> Option<CsName> {
        DefaultSymbol::try_from_usize(u).map(CsName)
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Value {
    BeginGroup(char),
    EndGroup(char),
    MathShift(char),
    AlignmentTab(char),
    Parameter(char),
    Superscript(char),
    Subscript(char),
    Space(char),
    Letter(char),
    Other(char),
    Active(char),
    ControlSequence(CsName),
}

impl Value {
    pub fn new(c: char, cat_code: CatCode) -> Value {
        match cat_code {
            CatCode::BeginGroup => Value::BeginGroup(c),
            CatCode::EndGroup => Value::EndGroup(c),
            CatCode::MathShift => Value::MathShift(c),
            CatCode::AlignmentTab => Value::AlignmentTab(c),
            CatCode::Parameter => Value::Parameter(c),
            CatCode::Superscript => Value::Superscript(c),
            CatCode::Subscript => Value::Subscript(c),
            CatCode::Space => Value::Space(c),
            CatCode::Letter => Value::Letter(c),
            CatCode::Other => Value::Other(c),
            CatCode::Active => Value::Active(c),
            _ => panic!("raw cat code not allowed"),
        }
    }
}

pub type TracebackId = u32;

#[derive(Debug, Eq, Clone, Copy)]
pub struct Token {
    value: Value,
    traceback_id: TracebackId,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            Value::ControlSequence(_) => {
                write![f, "todo"] // TODO
            }
            _ => {
                write![f, "{}", self.char().unwrap()]
            }
        }
    }
}
impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

macro_rules! token_constructor {
    ($name: ident, $value: expr) => {
        pub fn $name(c: char, traceback_id: TracebackId) -> Token {
            Token {
                value: $value(c),
                traceback_id,
            }
        }
    };
}

impl Token {
    token_constructor!(new_begin_group, Value::BeginGroup);
    token_constructor!(new_end_group, Value::EndGroup);
    token_constructor!(new_math_shift, Value::MathShift);
    token_constructor!(new_alignment_tab, Value::AlignmentTab);
    token_constructor!(new_parameter, Value::Parameter);
    token_constructor!(new_superscript, Value::Superscript);
    token_constructor!(new_subscript, Value::Subscript);
    token_constructor!(new_space, Value::Space);
    token_constructor!(new_letter, Value::Letter);
    token_constructor!(new_other, Value::Other);
    token_constructor!(new_active_character, Value::Active);

    pub fn new_control_sequence(name: CsName, traceback_id: TracebackId) -> Token {
        Token {
            value: Value::ControlSequence(name),
            traceback_id,
        }
    }

    #[inline]
    pub fn value(&self) -> Value {
        self.value
    }

    pub fn traceback_id(&self) -> TracebackId {
        self.traceback_id
    }

    pub fn char(&self) -> Option<char> {
        match self.value {
            Value::BeginGroup(c) => Some(c),
            Value::EndGroup(c) => Some(c),
            Value::MathShift(c) => Some(c),
            Value::AlignmentTab(c) => Some(c),
            Value::Parameter(c) => Some(c),
            Value::Superscript(c) => Some(c),
            Value::Subscript(c) => Some(c),
            Value::Space(c) => Some(c),
            Value::Letter(c) => Some(c),
            Value::Other(c) => Some(c),
            Value::Active(c) => Some(c),
            Value::ControlSequence(..) => None,
        }
    }
}

enum PendingWhitespace {
    None,
    Space,
    Newlines(usize),
}

impl PendingWhitespace {
    fn write_out(&mut self, s: &mut String) {
        if !s.is_empty() {
            match self {
                PendingWhitespace::None => {}
                PendingWhitespace::Space => {
                    s.push(' ');
                }
                PendingWhitespace::Newlines(n) => {
                    for _ in 0..*n {
                        s.push('\n');
                    }
                }
            }
        }
        *self = PendingWhitespace::None;
    }

    fn add_space(&mut self) {
        *self = match self {
            PendingWhitespace::None => PendingWhitespace::Space,
            PendingWhitespace::Space => PendingWhitespace::Space,
            PendingWhitespace::Newlines(n) => PendingWhitespace::Newlines(*n),
        }
    }

    fn add_newline(&mut self) {
        *self = match self {
            PendingWhitespace::None => PendingWhitespace::Newlines(1),
            PendingWhitespace::Space => PendingWhitespace::Newlines(1),
            PendingWhitespace::Newlines(n) => PendingWhitespace::Newlines(*n + 1),
        }
    }
}

/// Write a collection of tokens to a string.
pub fn write_tokens<'a, T>(tokens: T, interner: &CsNameInterner) -> String
where
    T: IntoIterator<Item = &'a Token>,
{
    let mut result: String = String::default();
    let mut pending_whitespace = PendingWhitespace::None;
    for token in tokens.into_iter() {
        match &token.value {
            Value::ControlSequence(s) => {
                pending_whitespace.write_out(&mut result);
                let name = interner.resolve(s).unwrap_or("invalidCsName");
                result.push('\\');
                result.push_str(name);
                pending_whitespace.add_space();
            }
            Value::Space(c) => {
                if *c == '\n' {
                    pending_whitespace.add_newline();
                } else {
                    pending_whitespace.add_space();
                }
            }
            _ => {
                pending_whitespace.write_out(&mut result);
                result.push(token.char().unwrap());
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
