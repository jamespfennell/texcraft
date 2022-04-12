use super::*;
use std::{
    fmt::Debug,
    iter::{Iterator, Peekable},
};

/// Parses a property list file into the PL abstract syntax tree.
pub fn parse<'a>(
    file_name: &'a str,
    input: &'a str,
) -> Result<Vec<Node<Word<'a>>>, ParseError<Word<'a>>> {
    let lexer = Lexer {
        file_name,
        s: input,
        pos_b: 0,
    };
    Vec::<Node<Word>>::parse(&mut lexer.peekable())
}

#[derive(Debug)]
pub enum ParseError<T> {
    WordWhileOpeningElem(T),
    EndWhileClosingElem,
    OpenWhileClosingElem(T),
    WordWhileClosingElem(T),
    EndWhileParsingWord,
    OpenWhileParsingWord(T),
}

#[derive(Debug)]
pub enum ConversionError<T> {
    RealNumberUnexpectedList(T),
    RealNumberNotEnoughWords(T),
    RealNumberInvalidPrefix(T),
    /// The provided string could not be parsed as a real number
    RealNumberInvalidValue(T, String),
    RealNumberExtraWord(T),
}

/// A node in the PL abstract syntax tree.
#[derive(Debug)]
pub struct Node<T> {
    open: T,
    pub key: T,
    value: (Vec<T>, Vec<Node<T>>),
    close: T,
}

// TODO: move most of this to builder
impl Node<String> {
    pub fn new(key: &str) -> Node<String> {
        Node {
            open: "(".to_string(),
            key: key.to_string(),
            value: (Vec::new(), Vec::new()),
            close: ")".to_string(),
        }
    }

    pub fn with_str(mut self, s: &str) -> Node<String> {
        self.value.0.push(s.to_string());
        self
    }

    pub fn with_string(mut self, s: String) -> Node<String> {
        self.value.0.push(s);
        self
    }

    pub fn with_octal(self, u: u32) -> Node<String> {
        self.with_str("O").with_string(format!("{:o}", u))
    }

    pub fn with_fix_word(self, u: FixWord) -> Node<String> {
        self.with_str("R").with_string(write_fix_word(u))
    }

    pub fn with_integer(self, i: u8) -> Node<String> {
        self.with_str("D").with_string(format!["{}", i])
    }

    pub fn with_character(self, r: u8) -> Node<String> {
        let c = match char::try_from(r) {
            Ok(c) => {
                if c.is_alphanumeric() {
                    Some(c)
                } else {
                    None
                }
            }
            Err(_) => None,
        };
        match c {
            None => self.with_octal(r as u32),
            Some(c) => self.with_string(format!["C {}", c]),
        }
    }

    pub fn with_tree(mut self, t: Vec<Node<String>>) -> Node<String> {
        self.value.1 = t;
        self
    }
}

impl<T: AsRef<str> + Clone> Node<T> {
    pub fn key(&self) -> &str {
        self.key.as_ref()
    }

    pub fn into_fix_word(&self) -> Result<FixWord, ConversionError<T>> {
        if let Some(node) = self.value.1.first() {
            return Err(ConversionError::RealNumberUnexpectedList(node.open.clone()));
        }
        match self.value.0.len() {
            0 | 1 => Err(ConversionError::RealNumberNotEnoughWords(
                self.close.clone(),
            )),
            2 => {
                if self.value.0[0].as_ref() != "R" {
                    return Err(ConversionError::RealNumberInvalidPrefix(
                        self.value.0[0].clone(),
                    ));
                }
                let word = &self.value.0[1];
                match parse_fix_word(word.as_ref()) {
                    Ok(fix_word) => Ok(fix_word),
                    Err(explanation) => Err(ConversionError::RealNumberInvalidValue(
                        word.clone(),
                        explanation,
                    )),
                }
            }
            _ => Err(ConversionError::RealNumberExtraWord(
                self.value.0[2].clone(),
            )),
        }
    }
}

struct Lexer<'a> {
    file_name: &'a str,
    s: &'a str,
    pos_b: usize,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = (TokenType, Word<'a>);

    fn next(&mut self) -> Option<Self::Item> {
        let file_name = self.file_name;
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
                Word {
                    file_name,
                    file: self.s,
                    start: self.pos_b,
                    end: self.pos_b + 1,
                },
            )),
            Some(')') => Some((
                TokenType::Close,
                Word {
                    file_name,
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
                    Word {
                        file_name,
                        file: self.s,
                        start: self.pos_b,
                        end,
                    },
                ))
            }
        };
        if let Some((
            _,
            Word {
                file_name: _,
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

trait Parse<'a> {
    fn parse(lexer: &mut Peekable<Lexer<'a>>) -> Result<Self, ParseError<Word<'a>>>
    where
        Self: Sized;
}

impl<'a> Parse<'a> for Option<Node<Word<'a>>> {
    fn parse(lexer: &mut Peekable<Lexer<'a>>) -> Result<Self, ParseError<Word<'a>>> {
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
        let value = (
            Vec::<Word>::parse(lexer)?,
            Vec::<Node<Word<'a>>>::parse(lexer)?,
        );
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
    fn parse(lexer: &mut Peekable<Lexer<'a>>) -> Result<Self, ParseError<Word<'a>>> {
        match lexer.next() {
            None => Err(ParseError::EndWhileParsingWord),
            Some((TokenType::Open, open)) => Err(ParseError::OpenWhileParsingWord(open)),
            Some((TokenType::Close, close)) => Err(ParseError::OpenWhileParsingWord(close)),
            Some((TokenType::Word, word)) => Ok(word),
        }
    }
}

impl<'a> Parse<'a> for Option<Word<'a>> {
    fn parse(lexer: &mut Peekable<Lexer<'a>>) -> Result<Self, ParseError<Word<'a>>> {
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
    fn parse(lexer: &mut Peekable<Lexer<'a>>) -> Result<Self, ParseError<Word<'a>>> {
        let mut result = vec![];
        while let Some(t) = Option::<T>::parse(lexer)? {
            result.push(t);
        }
        Ok(result)
    }
}

pub fn write<T: AsRef<str>>(tree: &[Node<T>], style: &PlStyle) -> String {
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
    fn write_word<T: AsRef<str>>(&mut self, word: T) {
        if self.after_word {
            self.buffer.push(' ');
        }
        self.buffer.push_str(word.as_ref());
        self.after_word = true;
    }

    fn write_list<T: AsRef<str>>(&mut self, list: &[Node<T>]) {
        for elem in list {
            self.write_elem(elem);
        }
    }

    fn write_elem<T: AsRef<str>>(&mut self, elem: &Node<T>) {
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
