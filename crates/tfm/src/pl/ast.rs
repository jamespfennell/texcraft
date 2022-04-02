use super::*;
use crate::{ClosingBraceStyle, PlStyle};
use std::{
    fmt::Debug,
    iter::{Iterator, Peekable},
};

/// Parses a property list file into the PL abstract syntax tree.
pub fn parse(input: &str) -> Result<Vec<Node>, ParseError> {
    let lexer = Lexer { s: input, pos_b: 0 };
    Vec::<Node>::parse(&mut lexer.peekable())
}

#[derive(Debug)]
pub enum ParseError<'a> {
    WordWhileOpeningElem(Word<'a>),
    EndWhileClosingElem,
    OpenWhileClosingElem(Word<'a>),
    WordWhileClosingElem(Word<'a>),
    EndWhileParsingWord,
    OpenWhileParsingWord(Word<'a>),
}

/// A node in the PL abstract syntax tree.
#[derive(Debug)]
pub struct Node<'a> {
    open: Word<'a>,
    key: Word<'a>,
    value: (Vec<Word<'a>>, Vec<Node<'a>>),
    close: Word<'a>,
}

impl<'a> Node<'a> {
    pub fn new(key: &'a str) -> Node<'a> {
        Node {
            open: Word::new("("),
            key: Word::new(key),
            value: (Vec::new(), Vec::new()),
            close: Word::new(")"),
        }
    }

    pub fn with_str(mut self, s: &'a str) -> Node<'a> {
        self.value.0.push(Word::new(s));
        self
    }

    pub fn with_string(mut self, s: String) -> Node<'a> {
        self.value.0.push(Word::Owned(s));
        self
    }

    pub fn with_octal(mut self, u: u32) -> Node<'a> {
        self.with_str("O").with_string(format!("{:o}", u))
    }

    pub fn with_fix_word(mut self, u: FixWord) -> Node<'a> {
        self.with_str("R").with_string(write_fix_word(u))
    }

    pub fn with_integer(mut self, i: u8) -> Node<'a> {
        self.with_str("D").with_string(format!["{}", i])
    }

    pub fn with_character(mut self, r: u8) -> Node<'a> {
        let c = match char::try_from(r) {
            Ok(c) => if c.is_alphanumeric() {
                Some(c)
            } else {
                None
            },
            Err(_) => None
        };
        match c {
            None => self.with_octal(r as u32),
            Some(c)  => self.with_string(format!["C {}", c]),
        }
    }

    pub fn with_tree(mut self, t: Vec<Node<'a>>) -> Node<'a> {
        self.value.1 = t;
        self
    }
}

struct Lexer<'a> {
    s: &'a str,
    pos_b: usize,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = (TokenType, Word<'a>);

    fn next(&mut self) -> Option<Self::Item> {
        let mut tail = self.s[self.pos_b..].chars().peekable();
        while let Some(next) = tail.peek() {
            if !next.is_whitespace() {
                break;
            }
            self.pos_b += next.len_utf8();
            tail.next();
        }
        let token = match tail.next() {
            None => None,
            Some('(') => Some((
                TokenType::Open,
                Word::FromFile {
                    file: self.s,
                    start: self.pos_b,
                    end: self.pos_b + 1,
                },
            )),
            Some(')') => Some((
                TokenType::Close,
                Word::FromFile {
                    file: self.s,
                    start: self.pos_b,
                    end: self.pos_b + 1,
                },
            )),
            Some(c) => {
                let mut end = self.pos_b + c.len_utf8();
                while let Some(next) = tail.peek() {
                    if next.is_whitespace() {
                        break;
                    }
                    if *next == ')' || *next == '(' {
                        break;
                    }
                    end += next.len_utf8();
                    tail.next();
                }
                Some((
                    TokenType::Word,
                    Word::FromFile {
                        file: self.s,
                        start: self.pos_b,
                        end,
                    },
                ))
            }
        };
        if let Some((
            _,
            Word::FromFile {
                file: _,
                end,
                start: _,
            },
        )) = &token
        {
            self.pos_b = *end;
        }
        token
    }
}

#[derive(Debug)]
enum TokenType {
    Open,
    Close,
    Word,
}

#[derive(Clone)]
pub enum Word<'a> {
    Ref(&'a str),
    Owned(String),
    FromFile {
        file: &'a str,
        start: usize,
        end: usize,
    },
}

impl<'a> Word<'a> {
    fn new(s: &'a str) -> Word<'a> {
        Word::Ref(s)
    }

    fn value(&self) -> &str {
        match self {
            Word::Ref(s) => s,
            Word::Owned(s) => s,
            Word::FromFile { file, end, start } => &file[*start..*end],
        }
    }
}

impl<'a> Debug for Word<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value())
    }
}

trait Parse<'a> {
    fn parse(lexer: &mut Peekable<Lexer<'a>>) -> Result<Self, ParseError<'a>>
    where
        Self: Sized;
}

impl<'a> Parse<'a> for Option<Node<'a>> {
    fn parse(lexer: &mut Peekable<Lexer<'a>>) -> Result<Self, ParseError<'a>> {
        let open = match lexer.peek() {
            None => return Ok(None),
            Some((TokenType::Open, open)) => {
                let open = open.clone();
                lexer.next();
                open
            }
            Some((TokenType::Close, _)) => return Ok(None),
            Some((TokenType::Word, word)) => {
                return Err(ParseError::WordWhileOpeningElem(word.clone()))
            }
        };
        let key = Word::parse(lexer)?;
        let value = (Vec::<Word>::parse(lexer)?, Vec::<Node>::parse(lexer)?);
        let close = match lexer.next() {
            None => return Err(ParseError::EndWhileClosingElem),
            Some((TokenType::Open, open)) => return Err(ParseError::OpenWhileClosingElem(open)),
            Some((TokenType::Close, close)) => close,
            Some((TokenType::Word, word)) => return Err(ParseError::WordWhileClosingElem(word)),
        };
        Ok(Some(Node {
            open,
            key,
            value,
            close,
        }))
    }
}

impl<'a> Parse<'a> for Word<'a> {
    fn parse(lexer: &mut Peekable<Lexer<'a>>) -> Result<Self, ParseError<'a>> {
        match lexer.next() {
            None => Err(ParseError::EndWhileParsingWord),
            Some((TokenType::Open, open)) => Err(ParseError::OpenWhileParsingWord(open)),
            Some((TokenType::Close, close)) => Err(ParseError::OpenWhileParsingWord(close)),
            Some((TokenType::Word, word)) => Ok(word),
        }
    }
}

impl<'a> Parse<'a> for Option<Word<'a>> {
    fn parse(lexer: &mut Peekable<Lexer<'a>>) -> Result<Self, ParseError<'a>> {
        match lexer.peek() {
            Some((TokenType::Word, word)) => {
                let word = word.clone();
                lexer.next();
                Ok(Some(word))
            }
            _ => Ok(None),
        }
    }
}

impl<'a, T> Parse<'a> for Vec<T>
where
    Option<T>: Parse<'a>,
{
    fn parse(lexer: &mut Peekable<Lexer<'a>>) -> Result<Self, ParseError<'a>> {
        let mut result = vec![];
        while let Some(t) = Option::<T>::parse(lexer)? {
            result.push(t);
        }
        Ok(result)
    }
}

pub fn write(tree: &[Node], style: &PlStyle) -> String {
    let mut o = Writer {
        style,
        buffer: String::new(),
        current_indent: 0,
        after_word: false,
    };
    o.write_list(tree);
    o.buffer
}

struct Writer<'a> {
    style: &'a PlStyle,

    buffer: String,
    current_indent: usize,
    after_word: bool,
}

impl<'a> Writer<'a> {
    fn write_word(&mut self, word: &Word) {
        if self.after_word {
            self.buffer.push(' ');
        }
        self.buffer.push_str(word.value());
        self.after_word = true;
    }

    fn write_list(&mut self, list: &[Node]) {
        for elem in list {
            self.write_elem(elem);
        }
    }

    fn write_elem(&mut self, elem: &Node) {
        for _ in 0..self.current_indent {
            self.buffer.push(' ');
        }
        self.buffer.push('(');
        self.after_word = false;
        self.write_word(&elem.key);
        for word in &elem.value.0 {
            self.write_word(word);
        }
        if elem.value.1.is_empty() {
            self.buffer.push(')');
            self.buffer.push('\n');
        } else {
            self.buffer.push('\n');
            self.current_indent += self.style.indent;
            self.write_list(&elem.value.1);
            self.current_indent -= self.style.indent;

            let indent = if self.style.closing_brace_style == ClosingBraceStyle::ExtraIndent {
                self.current_indent + self.style.indent
            } else {
                self.current_indent
            };
            for _ in 0..indent {
                self.buffer.push(' ');
            }
            self.buffer.push(')');
            self.buffer.push('\n');
        }
        self.after_word = false;
    }
}
