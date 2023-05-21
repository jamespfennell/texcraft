//! TeX tokens and category codes.

pub mod catcode;
pub (crate) mod lexer;
pub mod trace;

use crate::token::catcode::CatCode;
use std::num;
use texcraft_stdext::collections::interner;

/// String type used to represent control sequence names in Texlang.
///
/// The implementation of this type is opaque so that it can be performance optimized
/// without worrying about downstream consumers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CsName(num::NonZeroU32);

impl CsName {
    #[inline]
    pub fn to_usize(&self) -> usize {
        self.0.get() as usize
    }

    pub fn try_from_usize(u: usize) -> Option<CsName> {
        let u = match u32::try_from(u) {
            Ok(u) => u,
            Err(_) => return None,
        };
        num::NonZeroU32::new(u).map(CsName)
    }
}

/// String interner for control sequence names.
pub type CsNameInterner = interner::Interner<CsName>;

impl interner::Key for CsName {
    fn try_from_usize(index: usize) -> Option<Self> {
        num::NonZeroU32::try_from_usize(index).map(CsName)
    }

    fn into_usize(self) -> usize {
        self.0.into_usize()
    }
}

/// The value of a token.
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

/// A TeX token.
#[derive(Debug, Eq, Clone, Copy)]
pub struct Token {
    value: Value,
    trace_key: trace::Key,
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
        pub fn $name(c: char, trace_key: trace::Key) -> Token {
            Token {
                value: $value(c),
                trace_key,
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

    pub fn new_control_sequence(name: CsName, trace_key: trace::Key) -> Token {
        Token {
            value: Value::ControlSequence(name),
            trace_key,
        }
    }

    pub fn new_from_value(value: Value, trace_key: trace::Key) -> Token {
        Token { value, trace_key }
    }

    #[inline]
    pub fn value(&self) -> Value {
        self.value
    }

    pub fn trace_key(&self) -> trace::Key {
        self.trace_key
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
                let name = interner
                    .resolve(*s)
                    .unwrap_or("corruptedControlSequenceName");
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

    enum PreInternedToken {
        ControlSequence(&'static str),
        Character(char, CatCode),
    }

    macro_rules! write_tokens_test {
        ($name: ident, $input: expr, $want: expr) => {
            #[test]
            fn $name() {
                let mut tokens: Vec<Token> = vec![];
                let mut interner = CsNameInterner::default();
                for pre_interned_token in $input {
                    let token = match pre_interned_token {
                        PreInternedToken::ControlSequence(name) => {
                            let cs_name = interner.get_or_intern(name);
                            Token::new_control_sequence(cs_name, trace::Key::dummy())
                        }
                        PreInternedToken::Character(c, code) => {
                            Token::new_from_value(Value::new(c, code), trace::Key::dummy())
                        }
                    };
                    tokens.push(token);
                }
                let got = write_tokens(&tokens, &interner);
                let want = $want.to_string();

                if got != want {
                    println!("Output is different:");
                    println!("------[got]-------");
                    println!("{}", got);
                    println!("------[want]------");
                    println!("{}", want);
                    println!("-----------------");
                    panic!("write_tokens test failed");
                }
            }
        };
    }

    write_tokens_test!(blank, vec!(), "");
    write_tokens_test![
        trim_whitespace_from_start,
        vec![
            PreInternedToken::Character('\n', CatCode::Space),
            PreInternedToken::Character('\n', CatCode::Space),
            PreInternedToken::Character('\n', CatCode::Space),
            PreInternedToken::Character('H', CatCode::Letter),
        ],
        "H"
    ];
    write_tokens_test![
        trim_whitespace_from_end,
        vec![
            PreInternedToken::Character('H', CatCode::Letter),
            PreInternedToken::Character('\n', CatCode::Space),
            PreInternedToken::Character('\n', CatCode::Space),
            PreInternedToken::Character('\n', CatCode::Space),
        ],
        "H"
    ];
    write_tokens_test![
        trim_whitespace_from_middle_1,
        vec![
            PreInternedToken::Character('H', CatCode::Letter),
            PreInternedToken::Character(' ', CatCode::Space),
            PreInternedToken::Character(' ', CatCode::Space),
            PreInternedToken::Character('W', CatCode::Letter),
        ],
        "H W"
    ];
    write_tokens_test![
        trim_whitespace_from_middle_2,
        vec![
            PreInternedToken::Character('H', CatCode::Letter),
            PreInternedToken::Character('\n', CatCode::Space),
            PreInternedToken::Character(' ', CatCode::Space),
            PreInternedToken::Character('\n', CatCode::Space),
            PreInternedToken::Character('W', CatCode::Letter),
        ],
        "H\n\nW"
    ];
    write_tokens_test![
        trim_whitespace_from_middle_3,
        vec![
            PreInternedToken::Character('H', CatCode::Letter),
            PreInternedToken::Character('\n', CatCode::Space),
            PreInternedToken::Character('\n', CatCode::Space),
            PreInternedToken::Character('\n', CatCode::Space),
            PreInternedToken::Character('W', CatCode::Letter),
        ],
        "H\n\n\nW"
    ];
    write_tokens_test![
        control_sequence,
        vec![PreInternedToken::ControlSequence("HelloWorld"),],
        "\\HelloWorld"
    ];

    #[test]
    fn token_size() {
        assert_eq!(std::mem::size_of::<Value>(), 8);
        assert_eq!(std::mem::size_of::<Token>(), 12);
        assert_eq!(std::mem::size_of::<Result<Token, ()>>(), 12);
        assert_eq!(std::mem::size_of::<Result<Option<Token>, ()>>(), 12);
        assert_eq!(std::mem::size_of::<anyhow::Result<Token>>(), 16);
    }
}
