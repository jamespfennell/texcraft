//! Abstract syntax tree for the property list format [advanced].
//!

use super::*;
use std::{
    fmt::Debug,
    iter::{Iterator, Peekable},
};

#[derive(Debug, PartialEq, Eq)]
pub struct Tree<T>(Vec<Node<T>>);

impl Tree<String> {
    pub fn builder() -> TreeBuilder {
        TreeBuilder(Vec::new())
    }
}

impl<T: AsRef<str>> Tree<T> {
    pub fn nodes(&self) -> &[Node<T>] {
        &self.0
    }

    pub fn into_string_tree(&self) -> Tree<String> {
        let mut b = Tree(vec![]);
        for elem in self.nodes() {
            b.0.push(elem.into_string_elem())
        }
        b
    }
}

pub struct TreeBuilder(Vec<Builder>);

impl TreeBuilder {
    pub fn add(&mut self, key: &str) -> &mut Builder {
        self.0.push(Node::builder(key));
        self.0.last_mut().unwrap()
    }
}

impl From<TreeBuilder> for Tree<String> {
    fn from(builder: TreeBuilder) -> Self {
        let mut nodes = Vec::with_capacity(builder.0.len());
        for b in builder.0 {
            nodes.push(b.into());
        }
        Tree(nodes)
    }
}

/// Parse property list data into an abstract syntax tree.
pub fn parse<'a>(
    file_name: &'a str,
    input: &'a str,
) -> Result<Tree<Word<'a>>, ParseError<Word<'a>>> {
    let lexer = Lexer {
        file_name,
        s: input,
        pos_b: 0,
    };
    match Vec::<Node<Word>>::parse(&mut lexer.peekable()) {
        Ok(v) => Ok(Tree(v)),
        Err(e) => Err(e),
    }
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
    NumberUnexpectedList(T),
    NumberNotEnoughWords(T),
    NumberInvalidPrefix(T),
    /// The provided string could not be parsed as a real number
    RealNumberInvalidValue(T, String),
    NumberExtraWord(T),

    InvalidBoolean(T),
    InvalidCharacter(T),
    NumberInvalidOctal(T),
    NumberInvalidDecimal(T),
    NumberInvalidHexadcimal(T),
    NumberInvalidFace(T, &'static str),

    StringUnexpectedList(T),
}

/// A node in the PL abstract syntax tree.
#[derive(Debug, PartialEq, Eq)]
pub struct Node<T> {
    open: T,
    pub key: T,
    value: (Vec<T>, Tree<T>),
    close: T,
}

impl Node<String> {
    pub fn builder(key: &str) -> Builder {
        Builder(Node {
            open: "(".to_string(),
            key: key.to_string(),
            value: (Vec::new(), Tree(vec![])),
            close: ")".to_string(),
        })
    }
}

pub struct Builder(Node<String>);

impl Builder {
    pub fn with_str(&mut self, s: &str) -> &mut Builder {
        self.0.value.0.push(s.to_string());
        self
    }

    pub fn with_string(&mut self, s: String) -> &mut Builder {
        self.0.value.0.push(s);
        self
    }

    pub fn with_octal(&mut self, u: u32) -> &mut Builder {
        self.with_str("O").with_string(format!("{:o}", u))
    }

    pub fn with_fix_word(&mut self, u: FixWord) -> &mut Builder {
        self.with_str("R").with_string(format!("{}", u))
    }

    pub fn with_integer(&mut self, i: u8) -> &mut Builder {
        self.with_str("D").with_string(format!["{}", i])
    }

    pub fn with_character(&mut self, r: u8) -> &mut Builder {
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

    pub fn with_tree(&mut self, t: Tree<String>) -> &mut Builder {
        self.0.value.1 = t;
        self
    }
}

impl From<Builder> for Node<String> {
    fn from(builder: Builder) -> Self {
        builder.0
    }
}

impl<T: AsRef<str>> Node<T> {
    pub fn key(&self) -> &str {
        self.key.as_ref()
    }

    pub fn into_string_elem(&self) -> Node<String> {
        let mut words = vec![];
        for word in &self.value.0 {
            words.push(word.as_ref().to_string())
        }
        Node {
            open: self.open.as_ref().to_string(),
            key: self.key.as_ref().to_string(),
            value: (words, self.value.1.into_string_tree()),
            close: self.close.as_ref().to_string(),
        }
    }
}

impl<T: AsRef<str> + Clone> TryInto<bool> for &Node<T> {
    type Error = ConversionError<T>;

    fn try_into(self) -> Result<bool, Self::Error> {
        ensure_no_list(self)?;
        let value = match self.value.0.len() {
            0 => Err(ConversionError::NumberNotEnoughWords(self.close.clone())),
            1 => Ok(&self.value.0[0]),
            _ => Err(ConversionError::NumberExtraWord(self.value.0[1].clone())),
        }?;
        match value.as_ref() {
            "TRUE" => Ok(true),
            "FALSE" => Ok(false),
            _ => Err(ConversionError::InvalidBoolean(value.clone())),
        }
    }
}

impl<T: AsRef<str> + Clone> TryInto<u32> for &Node<T> {
    type Error = ConversionError<T>;

    fn try_into(self) -> Result<u32, Self::Error> {
        let (prefix, value) = prefix_and_value_for_number(self)?;
        match prefix.as_ref() {
            "O" => match u32::from_str_radix(value.as_ref(), 8) {
                Ok(u) => Ok(u),
                Err(_) => Err(ConversionError::NumberInvalidOctal(value.clone())),
            },
            "H" => match u32::from_str_radix(value.as_ref(), 16) {
                Ok(u) => Ok(u),
                Err(_) => Err(ConversionError::NumberInvalidHexadcimal(value.clone())),
            },
            _ => Err(ConversionError::NumberInvalidPrefix(prefix.clone())),
        }
    }
}

fn prefix_and_value_for_number<T: Clone>(node: &Node<T>) -> Result<(&T, &T), ConversionError<T>> {
    ensure_no_list(node)?;
    match node.value.0.len() {
        0 | 1 => Err(ConversionError::NumberNotEnoughWords(node.close.clone())),
        2 => Ok((&node.value.0[0], &node.value.0[1])),
        _ => Err(ConversionError::NumberExtraWord(node.value.0[2].clone())),
    }
}

fn ensure_no_list<T: Clone>(node: &Node<T>) -> Result<(), ConversionError<T>> {
    if let Some(node) = node.value.1 .0.first() {
        Err(ConversionError::NumberUnexpectedList(node.open.clone()))
    } else {
        Ok(())
    }
}

impl<T: AsRef<str> + Clone> TryInto<u8> for &Node<T> {
    type Error = ConversionError<T>;

    fn try_into(self) -> Result<u8, Self::Error> {
        let (prefix, value) = prefix_and_value_for_number(self)?;
        match prefix.as_ref() {
            "C" => {
                if value.as_ref().len() != 1 {
                    Err(ConversionError::InvalidCharacter(value.clone()))
                } else {
                    let c: char = value.as_ref().chars().next().unwrap();
                    match c.try_into() {
                        Ok(u) => Ok(u),
                        Err(_) => Err(ConversionError::InvalidCharacter(value.clone())),
                    }
                }
            }
            "O" => match u8::from_str_radix(value.as_ref(), 8) {
                Ok(u) => Ok(u),
                Err(_) => Err(ConversionError::NumberInvalidOctal(value.clone())),
            },
            "D" => match value.as_ref().parse::<u8>() {
                Ok(u) => Ok(u),
                Err(_) => Err(ConversionError::NumberInvalidDecimal(value.clone())),
            },
            "H" => match u8::from_str_radix(value.as_ref(), 16) {
                Ok(u) => Ok(u),
                Err(_) => Err(ConversionError::NumberInvalidHexadcimal(value.clone())),
            },
            "F" => match Face::try_from(value.as_ref()) {
                Ok(face) => Ok(face.0),
                Err(explanation) => Err(ConversionError::NumberInvalidFace(
                    value.clone(),
                    explanation,
                )),
            },
            _ => Err(ConversionError::NumberInvalidPrefix(prefix.clone())),
        }
    }
}

impl<T: AsRef<str> + Clone> TryInto<String> for &Node<T> {
    type Error = ConversionError<T>;

    fn try_into(self) -> Result<String, Self::Error> {
        if let Some(node) = self.value.1 .0.first() {
            return Err(ConversionError::StringUnexpectedList(node.open.clone()));
        }
        let mut s = String::new();
        for word in &self.value.0 {
            if !s.is_empty() {
                s.push(' ');
            }
            s.push_str(word.as_ref());
        }
        Ok(s)
    }
}

impl<T: AsRef<str> + Clone> TryInto<FixWord> for &Node<T> {
    type Error = ConversionError<T>;

    fn try_into(self) -> Result<FixWord, Self::Error> {
        let (prefix, value) = prefix_and_value_for_number(self)?;
        if prefix.as_ref() != "R" {
            return Err(ConversionError::NumberInvalidPrefix(
                self.value.0[0].clone(),
            ));
        }
        match value.as_ref().try_into() {
            Ok(fix_word) => Ok(fix_word),
            Err(explanation) => Err(ConversionError::RealNumberInvalidValue(
                value.clone(),
                explanation,
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
                let open = *open;
                lexer.next();
                open
            }
            Some((TokenType::Close, _)) => return Ok(None),
            Some((TokenType::Word, word)) => return Err(ParseError::WordWhileOpeningElem(*word)),
        };
        let key = Word::parse(lexer)?;
        let value = (
            Vec::<Word>::parse(lexer)?,
            Tree(Vec::<Node<Word<'a>>>::parse(lexer)?),
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
                let word = *word;
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

pub fn write<T: AsRef<str>>(tree: &Tree<T>, style: Style) -> String {
    let mut o = Writer {
        style: &style,
        buffer: String::new(),
        current_indent: 0,
        after_word: false,
    };
    o.write_list(tree.nodes());
    o.buffer
}

struct Writer<'a> {
    style: &'a Style,

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
        if elem.value.1 .0.is_empty() {
            self.buffer.push(')');
            self.buffer.push('\n');
        } else {
            self.buffer.push('\n');
            self.current_indent += self.style.indent;
            self.write_list(&elem.value.1 .0);
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
