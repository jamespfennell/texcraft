//! TeX tokens and category codes.

pub mod lexer;
pub mod trace;
use crate::types::CatCode;
use std::{fmt::Display, num};
use texcraft_stdext::collections::interner;

/// String type used to represent control sequence names in Texlang.
///
/// The implementation of this type is opaque so that it can be performance optimized
/// without worrying about downstream consumers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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
#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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
    CommandRef(CommandRef),
}

/// The value of a token that references a command
#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum CommandRef {
    ControlSequence(CsName),
    ActiveCharacter(char),
}

impl CommandRef {
    pub fn to_string(&self, cs_name_interner: &CsNameInterner) -> String {
        match self {
            CommandRef::ControlSequence(cs_name) => {
                format!("\\{}", cs_name_interner.resolve(*cs_name).unwrap())
            }
            CommandRef::ActiveCharacter(c) => format!("{c}"),
        }
    }
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
            CatCode::Active => Value::CommandRef(CommandRef::ActiveCharacter(c)),
            _ => panic!("raw cat code not allowed"),
        }
    }

    /// TODO: should have a char_and_catcode function
    pub fn char(&self) -> Option<char> {
        match *self {
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
            Value::CommandRef(command_ref) => match command_ref {
                CommandRef::ControlSequence(_) => None,
                CommandRef::ActiveCharacter(c) => Some(c),
            },
        }
    }

    pub fn cat_code(&self) -> Option<CatCode> {
        match self {
            Value::BeginGroup(_) => Some(CatCode::BeginGroup),
            Value::EndGroup(_) => Some(CatCode::EndGroup),
            Value::MathShift(_) => Some(CatCode::MathShift),
            Value::AlignmentTab(_) => Some(CatCode::AlignmentTab),
            Value::Parameter(_) => Some(CatCode::Parameter),
            Value::Superscript(_) => Some(CatCode::Superscript),
            Value::Subscript(_) => Some(CatCode::Subscript),
            Value::Space(_) => Some(CatCode::Space),
            Value::Letter(_) => Some(CatCode::Letter),
            Value::Other(_) => Some(CatCode::Other),
            Value::CommandRef(command_ref) => match command_ref {
                CommandRef::ControlSequence(_) => None,
                CommandRef::ActiveCharacter(_) => Some(CatCode::Active),
            },
        }
    }
}

/// A TeX token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Token {
    value: Value,
    trace_key: trace::Key,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            Value::CommandRef(_) => {
                write![f, "todo"] // TODO
            }
            _ => {
                write![f, "{}", self.char().unwrap()]
            }
        }
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

    pub fn new_command_ref(command_ref: CommandRef, trace_key: trace::Key) -> Token {
        Token {
            value: Value::CommandRef(command_ref),
            trace_key,
        }
    }

    pub fn new_active_character(c: char, trace_key: trace::Key) -> Token {
        Token {
            value: Value::CommandRef(CommandRef::ActiveCharacter(c)),
            trace_key,
        }
    }

    pub fn new_control_sequence(name: CsName, trace_key: trace::Key) -> Token {
        Token {
            value: Value::CommandRef(CommandRef::ControlSequence(name)),
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

    #[inline]
    pub fn trace_key(&self) -> trace::Key {
        self.trace_key
    }

    /// TODO: should have a char_and_catcode function
    pub fn char(&self) -> Option<char> {
        self.value.char()
    }

    pub fn cat_code(&self) -> Option<CatCode> {
        self.value.cat_code()
    }
}

#[derive(Default)]
enum PendingWhitespace {
    #[default]
    NotStarted,
    None,
    Space,
    Newlines(usize),
}

impl PendingWhitespace {
    fn reset(&mut self) {
        *self = PendingWhitespace::None;
    }

    fn add_space(&mut self) {
        *self = match self {
            PendingWhitespace::NotStarted => PendingWhitespace::NotStarted,
            PendingWhitespace::None => PendingWhitespace::Space,
            PendingWhitespace::Space => PendingWhitespace::Space,
            PendingWhitespace::Newlines(n) => PendingWhitespace::Newlines(*n),
        }
    }

    fn add_newline(&mut self) {
        *self = match self {
            PendingWhitespace::NotStarted => PendingWhitespace::NotStarted,
            PendingWhitespace::None => PendingWhitespace::Newlines(1),
            PendingWhitespace::Space => PendingWhitespace::Newlines(1),
            PendingWhitespace::Newlines(n) => PendingWhitespace::Newlines(*n + 1),
        }
    }

    fn new_paragraph(&mut self) {
        *self = match self {
            PendingWhitespace::NotStarted => PendingWhitespace::NotStarted,
            PendingWhitespace::None | PendingWhitespace::Space | PendingWhitespace::Newlines(1) => {
                PendingWhitespace::Newlines(2)
            }
            PendingWhitespace::Newlines(n) => PendingWhitespace::Newlines(*n),
        }
    }
}

impl Display for PendingWhitespace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PendingWhitespace::NotStarted | PendingWhitespace::None => Ok(()),
            PendingWhitespace::Space => {
                write!(f, " ")
            }
            PendingWhitespace::Newlines(n) => {
                for _ in 0..*n {
                    writeln!(f)?;
                }
                Ok(())
            }
        }
    }
}

/// Data structure for writing tokens
#[derive(Default)]
pub struct Writer {
    pending_whitespace: PendingWhitespace,
}

impl Writer {
    /// Write a token.
    pub fn write(
        &mut self,
        io_writer: &mut dyn std::io::Write,
        interner: &CsNameInterner,
        value: Value,
    ) -> Result<(), std::io::Error> {
        match &value {
            Value::CommandRef(CommandRef::ControlSequence(s)) => {
                write!(
                    io_writer,
                    "{}\\{}",
                    self.pending_whitespace,
                    interner.resolve(*s).unwrap()
                )?;
                self.pending_whitespace.reset();
            }
            Value::Space(_) => self.pending_whitespace.add_space(),
            _ => {
                write!(
                    io_writer,
                    "{}{}",
                    self.pending_whitespace,
                    value.char().unwrap()
                )?;
                self.pending_whitespace.reset();
            }
        }
        io_writer.flush()
    }

    pub fn add_newline(&mut self) {
        self.pending_whitespace.add_newline();
    }

    pub fn start_paragraph(&mut self) {
        self.pending_whitespace.new_paragraph();
    }
}

/// Write a collection of tokens to a string.
pub fn write_tokens<'a, T>(tokens: T, interner: &CsNameInterner) -> String
where
    T: IntoIterator<Item = &'a Token>,
{
    let mut buffer: Vec<u8> = Default::default();
    let mut writer: Writer = Default::default();
    for token in tokens.into_iter() {
        writer.write(&mut buffer, interner, token.value()).unwrap();
    }
    std::str::from_utf8(&buffer).unwrap().into()
}

/// Write a collection of token values to a string.
pub fn write_token_values<'a, T>(values: T, interner: &CsNameInterner) -> String
where
    T: IntoIterator<Item = &'a Value>,
{
    let mut buffer: Vec<u8> = Default::default();
    let mut writer: Writer = Default::default();
    for value in values.into_iter() {
        writer.write(&mut buffer, interner, *value).unwrap();
    }
    std::str::from_utf8(&buffer).unwrap().into()
}

#[cfg(test)]
mod tests {
    use super::*;

    enum Instruction {
        ControlSequence(&'static str),
        Character(char, CatCode),
        Newline,
        NewParagraph,
    }

    fn writer_test(input: Vec<Instruction>, want: &str) {
        let mut buffer: Vec<u8> = Default::default();
        let mut writer: Writer = Default::default();
        let mut interner = CsNameInterner::default();
        for pre_interned_token in input {
            match pre_interned_token {
                Instruction::ControlSequence(name) => {
                    let cs_name = interner.get_or_intern(name);
                    let token = Token::new_control_sequence(cs_name, trace::Key::dummy());
                    writer.write(&mut buffer, &interner, token.value()).unwrap();
                }
                Instruction::Character(c, code) => {
                    let token = Token::new_from_value(Value::new(c, code), trace::Key::dummy());
                    writer.write(&mut buffer, &interner, token.value()).unwrap();
                }
                Instruction::Newline => {
                    writer.add_newline();
                }
                Instruction::NewParagraph => {
                    writer.start_paragraph();
                }
            };
        }
        let got: String = std::str::from_utf8(&buffer).unwrap().into();
        let want = want.to_string();

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

    macro_rules! write_tokens_tests {
        ($( ($name: ident, $input: expr, $want: expr), )+) => {
            $(
            #[test]
            fn $name() {
                writer_test($input, $want);
            }
            )+
        };
    }

    write_tokens_tests!(
        (blank, vec!(), ""),
        (
            trim_whitespace_from_start,
            vec![
                Instruction::Character('\n', CatCode::Space),
                Instruction::Character('\n', CatCode::Space),
                Instruction::Character('\n', CatCode::Space),
                Instruction::Character('H', CatCode::Letter),
            ],
            "H"
        ),
        (
            trim_whitespace_from_end,
            vec![
                Instruction::Character('H', CatCode::Letter),
                Instruction::Character('\n', CatCode::Space),
                Instruction::Character('\n', CatCode::Space),
                Instruction::Character('\n', CatCode::Space),
            ],
            "H"
        ),
        (
            trim_whitespace_from_middle_1,
            vec![
                Instruction::Character('H', CatCode::Letter),
                Instruction::Character(' ', CatCode::Space),
                Instruction::Character(' ', CatCode::Space),
                Instruction::Character('W', CatCode::Letter),
            ],
            "H W"
        ),
        (
            trim_whitespace_from_middle_2,
            vec![
                Instruction::Character('H', CatCode::Letter),
                Instruction::Character('\n', CatCode::Space),
                Instruction::Character(' ', CatCode::Space),
                Instruction::Character('\n', CatCode::Space),
                Instruction::Character('W', CatCode::Letter),
            ],
            "H W"
        ),
        (
            trim_whitespace_from_middle_3,
            vec![
                Instruction::Character('H', CatCode::Letter),
                Instruction::Character('\n', CatCode::Space),
                Instruction::Character('\n', CatCode::Space),
                Instruction::Character('\n', CatCode::Space),
                Instruction::Character('W', CatCode::Letter),
            ],
            "H W"
        ),
        (
            control_sequence,
            vec![Instruction::ControlSequence("HelloWorld"),],
            "\\HelloWorld"
        ),
        (
            newline_1,
            vec![
                Instruction::Character('H', CatCode::Letter),
                Instruction::Newline,
                Instruction::Character('W', CatCode::Letter),
            ],
            "H\nW"
        ),
        (
            newline_2,
            vec![
                Instruction::Character('H', CatCode::Letter),
                Instruction::Newline,
                Instruction::Character(' ', CatCode::Space),
                Instruction::Newline,
                Instruction::Character('W', CatCode::Letter),
            ],
            "H\n\nW"
        ),
        (
            newline_3,
            vec![
                Instruction::Character('H', CatCode::Letter),
                Instruction::Newline,
                Instruction::Character(' ', CatCode::Space),
                Instruction::Newline,
                Instruction::Character(' ', CatCode::Space),
                Instruction::Newline,
                Instruction::Character('W', CatCode::Letter),
            ],
            "H\n\n\nW"
        ),
        (
            par_1,
            vec![
                Instruction::Character('H', CatCode::Letter),
                Instruction::NewParagraph,
                Instruction::NewParagraph,
                Instruction::Character('W', CatCode::Letter),
            ],
            "H\n\nW"
        ),
        (
            par_2,
            vec![
                Instruction::Character('H', CatCode::Letter),
                Instruction::NewParagraph,
                Instruction::NewParagraph,
                Instruction::NewParagraph,
                Instruction::Character('W', CatCode::Letter),
            ],
            "H\n\nW"
        ),
    );

    #[test]
    fn token_size() {
        assert_eq!(std::mem::size_of::<CommandRef>(), 8);
        assert_eq!(std::mem::size_of::<Value>(), 8);
        assert_eq!(std::mem::size_of::<Token>(), 12);
        assert_eq!(std::mem::size_of::<Result<Token, ()>>(), 12);
        assert_eq!(std::mem::size_of::<Result<Option<Token>, ()>>(), 12);
        assert_eq!(std::mem::size_of::<crate::prelude::Result<Token>>(), 12);
        assert_eq!(
            std::mem::size_of::<crate::prelude::Result<Option<Token>>>(),
            12
        );
    }
}
