use super::*;
use std::{
    fmt::Debug,
    iter::{Iterator, Peekable},
};

pub fn run<'a>(pl_source: &'a str) {
    let lexer = Lexer {
        s: pl_source,
        pos_b: 0,
    };
    let root = Vec::<PlElem>::parse(&mut lexer.peekable()).unwrap();

    let mut o = PlOutput {
      indent: 3,
      extra_indent_close: true,
        buffer: String::new(),
        current_indent: 0,
        after_word: false,
    };
    o.write_list(&root);
    print!["{}", o.buffer];
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
                Word {
                    file: self.s,
                    start: self.pos_b,
                    end: self.pos_b + 1,
                },
            )),
            Some(')') => Some((
                TokenType::Close,
                Word {
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
                        file: self.s,
                        start: self.pos_b,
                        end,
                    },
                ))
            }
        };
        if let Some((_, word)) = &token {
            self.pos_b = word.end;
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

#[derive(Clone, Copy)]
struct Word<'a> {
    file: &'a str,
    start: usize,
    end: usize,
}

impl<'a> Word<'a> {
    fn value(&self) -> &str {
        &self.file[self.start..self.end]
    }
}

impl<'a> Debug for Word<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value())
    }
}

#[derive(Debug)]
enum ParseError<'a> {
    WordWhileOpeningElem(Word<'a>),
    EndWhileClosingElem,
    OpenWhileClosingElem(Word<'a>),
    WordWhileClosingElem(Word<'a>),
    EndWhileParsingWord,
    OpenWhileParsingWord(Word<'a>),
}

trait Parse<'a> {
    fn parse(lexer: &mut Peekable<Lexer<'a>>) -> Result<Self, ParseError<'a>>
    where
        Self: Sized;
}

#[derive(Debug)]
struct PlElem<'a> {
    open: Word<'a>,
    key: Word<'a>,
    value: (Vec<Word<'a>>, Vec<PlElem<'a>>),
    close: Word<'a>,
}

impl<'a> Parse<'a> for Option<PlElem<'a>> {
    fn parse(lexer: &mut Peekable<Lexer<'a>>) -> Result<Self, ParseError<'a>> {
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
        let value = (Vec::<Word>::parse(lexer)?, Vec::<PlElem>::parse(lexer)?);
        let close = match lexer.next() {
            None => return Err(ParseError::EndWhileClosingElem),
            Some((TokenType::Open, open)) => return Err(ParseError::OpenWhileClosingElem(open)),
            Some((TokenType::Close, close)) => close,
            Some((TokenType::Word, word)) => return Err(ParseError::WordWhileClosingElem(word)),
        };
        Ok(Some(PlElem {
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
    fn parse(lexer: &mut Peekable<Lexer<'a>>) -> Result<Self, ParseError<'a>> {
        let mut result = vec![];
        while let Some(t) = Option::<T>::parse(lexer)? {
            result.push(t);
        }
        Ok(result)
    }
}

struct PlOutput {
  indent: usize,
  extra_indent_close: bool,

    buffer: String,
    current_indent: usize,
    after_word: bool,
}

impl PlOutput {
    fn write_word(&mut self, word: &Word) {
        if self.after_word {
            self.buffer.push(' ');
        }
        self.buffer.push_str(word.value());
        self.after_word = true;
    }

    fn write_list(&mut self, list: &[PlElem]) {
        for elem in list {
            self.write_elem(elem);
        }
    }

    fn write_elem(&mut self, elem: &PlElem) {
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
            self.current_indent += self.indent;
            self.write_list(&elem.value.1);
            self.current_indent -= self.indent;
            let indent = if self.extra_indent_close {
              self.current_indent + self.indent
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

pub trait SerializePl {
    fn serialize_pl(&self, output: &mut Output);
}

pub fn serialize_pl<T: SerializePl>(t: &T) -> String {
    let mut output = Output { s: String::new() };
    t.serialize_pl(&mut output);
    output.s
}

pub struct Output {
    s: String,
}

impl Output {
    fn write(&mut self, c: char) {
        self.s.push(c)
    }

    fn write_str(&mut self, s: &str) {
        self.s.push_str(s)
    }
}

trait DeserializePl {
    fn deserialize_pl(input: &mut Input) -> Self;
}

struct Input<'a> {
    c: std::str::Chars<'a>,
}

impl<'a> Input<'a> {
    fn next(&mut self) -> Option<char> {
        self.c.next()
    }
}

impl SerializePl for FixWord {
    fn serialize_pl<'a, 'b>(&self, output: &mut Output) {
        let abs: u32 = if self.0 < 0 {
            if self.0 == i32::MIN {
                output.write_str("-2047.9999999");
                return;
            } else {
                output.write('-');
                self.0.abs() as u32
            }
        } else {
            self.0 as u32
        };
        let mut integer = abs / (1 << 20);

        // The integer part is at most 2^11 < 10^4, so there are at most 4 decimal digits.
        let mut integer_digits = [0; 4];
        let mut i = 4;
        loop {
            integer_digits[i - 1] = integer % 10;
            integer /= 10;
            i -= 1;
            if integer == 0 {
                break;
            }
        }
        while i < 4 {
            output.write(std::char::from_digit(integer_digits[i], 10).unwrap());
            i += 1;
        }

        output.write('.');
        let mut delta = 10;
        let mut fraction = abs % (1 << 20);
        println!("fraction={}, modulus={}", fraction, abs);
        fraction = fraction * 10 + 5;
        loop {
            if delta > (1 << 20) {
                fraction = fraction + (1 << 19) - (delta / 2);
            }
            output.write(std::char::from_digit(fraction / (1 << 20), 10).unwrap());
            fraction = (fraction % (1 << 20)) * 10;
            delta *= 10;
            if fraction <= delta {
                break;
            }
        }
    }
}

impl DeserializePl for FixWord {
    fn deserialize_pl(input: &mut Input) -> Self {
        enum Char {
            Digit(i32),
            Other(char),
        }
        impl Char {
            fn new(c: char) -> Char {
                match c {
                    '0' => Char::Digit(0),
                    '1' => Char::Digit(1),
                    '2' => Char::Digit(2),
                    '3' => Char::Digit(3),
                    '4' => Char::Digit(4),
                    '5' => Char::Digit(5),
                    '6' => Char::Digit(6),
                    '7' => Char::Digit(7),
                    '8' => Char::Digit(8),
                    '9' => Char::Digit(9),
                    other => Char::Other(other),
                }
            }
        }

        let mut negative = false;
        let mut integer = None;
        while let Some(c) = input.next() {
            match Char::new(c) {
                Char::Other('+') | Char::Other(' ') => (),
                Char::Other('-') => {
                    negative = !negative;
                }
                Char::Digit(d) => {
                    integer = Some(d);
                    break;
                }
                Char::Other(_) => panic![""],
            }
        }
        let negative = negative;

        let mut integer = match integer {
            None => panic![""],
            Some(integer) => integer,
        };
        while let Some(c) = input.next() {
            match Char::new(c) {
                Char::Digit(d) => {
                    integer = integer * 10 + d;
                    if integer >= 2048 {
                        panic!("integer too big")
                    }
                }
                Char::Other('.') => break,
                Char::Other(other) => panic!["unexpected char {}", other],
            }
        }
        let integer = integer;

        let mut num_fractional_digits = 0;
        let mut fraction_digits = [0; 7];
        while let Some(c) = input.next() {
            match Char::new(c) {
                Char::Digit(d) => {
                    if num_fractional_digits < 7 {
                        fraction_digits[num_fractional_digits] = d * (1 << 21);
                        num_fractional_digits += 1;
                    }
                }
                Char::Other(_) => break,
            }
        }
        let mut fraction = 0;
        for i in (0..num_fractional_digits).rev() {
            fraction = fraction_digits[i] + fraction / 10;
        }
        let fraction = (fraction + 10) / 20;

        if integer == 2047 && fraction >= (1 << 20) {
            if negative {
                return FixWord(i32::MIN);
            }
            panic![""]
        }
        let mut result = integer * FixWord::UNITY + fraction;
        if negative {
            result *= -1;
        }
        FixWord(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! round_trip_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let value: i32 = $value;

                let start = FixWord(value);
                let mut output = Output{s: String::new() };
                start.serialize_pl(&mut output);

                let mut input = Input {c: output.s.chars() };
                let finish = FixWord::deserialize_pl(&mut input);
                assert_eq!(start, finish);

                let start = FixWord(value.wrapping_mul(-1));
                let mut output = Output{s: String::new() };
                start.serialize_pl(&mut output);

                let mut input = Input {c: output.s.chars() };
                let finish = FixWord::deserialize_pl(&mut input);
                assert_eq!(start, finish);
            }
        )*
        }
    }

    round_trip_tests!(
        zero: 0,
        one: 1,
        two: 2,
        three: 3,
        four: 4,
        five: 5,
        ten: 10,
        seventy: 70,
        one40: 140,
        pow10: 1 << 10,
        pow15: 1 << 15,
        pow18: 1 << 18,
        pow19: 1 << 19,
        pow20: 1 << 20,
        pow20_times_10: 10 * 1 << 20,
        pow21: 1 << 21,
        pow21_plus_pow15: 1 << 21 + 1 << 15,
        big: 15 * (1 << 20) + 1 << 15,
        min: i32::MIN,
        max: i32::MAX,
    );
}
