//! Parser and writer for the property list (.pl) text format
use super::*;
use std::{
    fmt::{Debug, Display},
    iter::{Iterator, Peekable},
};

pub mod ast;

const CHARACTER: &str = "CHARACTER";
const CHARACTER_WIDTH: &str = "CHARWD";
const CHARACTER_HEIGHT: &str = "CHARHT";
const CHARACTER_DEPTH: &str = "CHARDP";
const CHARACTER_ITALIC: &str = "CHARIT";
const CHECKSUM: &str = "CHECKSUM";
const CODING_SCHEME: &str = "CODINGSCHEME";
const COMMENT: &str = "COMMENT";
const DESIGNSIZE: &str = "DESIGNSIZE";
const FAMILY: &str = "FAMILY";
const HEADER: &str = "HEADER";
const SEVENBITSAFEFLAG: &str = "SEVENBITSAFEFLAG";

const FONT_DIMENSIONS: &str = "FONTDIMEN";
const SLANT: &str = "SLANT";
const SPACE: &str = "SPACE";
const STRETCH: &str = "STRETCH";
const SHRINK: &str = "SHRINK";
const XHEIGHT: &str = "XHEIGHT";
const QUAD: &str = "QUAD";
const EXTRA_SPACE: &str = "EXTRASPACE";

pub fn format(input: &str, style: &PlStyle) -> String {
    let tree = ast::parse(input).unwrap();
    ast::write(&tree, style)
}

pub fn parse<'a>(input: &'a str) -> Result<File, ParseError<ast::Word<'a>>> {
    let tree = ast::parse(input)?;
    let mut file: File = Default::default();
    file.header.design_size = FixWord(FixWord::UNITY.0 * 10);
    for node in tree {
        match node.key() {
            COMMENT => {}
            FAMILY => {
                file.header.font_family = Some("hello".to_string());
            }

            other => {
                return Err(ParseError::InvalidKey(node.key));
            }
        }
    }

    let output = serialize(&file);
    println!["{}", output];

    Ok(file)
}

#[derive(Debug)]
pub enum ParseError<T> {
    Parse(ast::ParseError<T>),
    ConversionError(ast::ConversionError<T>),
    InvalidKey(T),
}

trait PrintError {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, message: String) -> std::fmt::Result;
}

impl<'a> PrintError for ast::Word<'a> {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, message: String) -> std::fmt::Result {
        let tb = WordTraceback::new(self.clone());
        let line_number = format!["{}", tb.line_number];
        let padding = " ".repeat(line_number.len());
        write!(f, "{}\n", message)?;
        write!(
            f,
            "{}--> file.pl:{}:{}\n",
            padding,
            tb.line_number,
            tb.word_in_line.0 + 1
        )?;
        write!(f, "{} |\n", padding)?;
        write!(f, "{} | {}\n", tb.line_number, tb.line)?;
        write!(
            f,
            "{} | {}{}\n",
            padding,
            " ".repeat(tb.word_in_line.0),
            "^".repeat(tb.word_in_line.1)
        )?;
        Ok(())
    }
}

struct WordTraceback<'a> {
    line_number: usize,
    line: &'a str,
    // TODO: position and word length
    word_in_line: (usize, usize),
}

impl<'a> WordTraceback<'a> {
    fn new(word: ast::Word<'a>) -> WordTraceback<'a> {
        let mut line_number = 1;
        let mut line_start = 0;
        for (position, char) in word.file.char_indices() {
            if char == '\n' {
                line_number += 1;
                line_start = position + 1;
            }
            if position == word.start {
                let tail = &word.file[line_start..];
                let line = match tail.find('\n') {
                    None => tail,
                    Some(end) => &tail[..end],
                };
                return WordTraceback {
                    line_number,
                    line,
                    word_in_line: (position - line_start, (word.end - word.start)),
                };
            }
        }
        todo!()
    }
}

impl<T: PrintError + AsRef<str>> Display for ParseError<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::Parse(err) => todo!(),
            ParseError::ConversionError(_) => todo!(),
            ParseError::InvalidKey(key) => key.print(f, format!("invalid key '{}'", key.as_ref())),
        }
    }
}

impl<T> From<ast::ParseError<T>> for ParseError<T> {
    fn from(err: ast::ParseError<T>) -> Self {
        ParseError::Parse(err)
    }
}

pub fn serialize(file: &File) -> String {
    let mut root = Vec::<ast::Node<String>>::new();
    if let Some(font_family) = &file.header.font_family {
        root.push(ast::Node::new(FAMILY).with_str(font_family));
    }
    for word in &file.header.additional_data {
        // TODO: this is not quite right
        root.push(ast::Node::new(HEADER).with_octal(*word));
    }
    if let Some(coding_scheme) = &file.header.character_coding_scheme {
        root.push(ast::Node::new(CODING_SCHEME).with_str(coding_scheme));
    }
    root.push(ast::Node::new(DESIGNSIZE).with_fix_word(file.header.design_size));
    root.push(ast::Node::new(COMMENT).with_str("DESIGNSIZE IS IN POINTS"));
    root.push(ast::Node::new(COMMENT).with_str("OTHER SIZES ARE MULTIPLES OF DESIGNSIZE"));
    root.push(ast::Node::new(CHECKSUM).with_octal(file.header.checksum));
    if let Some(seven_bit_safe) = file.header.seven_bit_safe {
        root.push(
            ast::Node::new(SEVENBITSAFEFLAG).with_str(if seven_bit_safe {
                "TRUE"
            } else {
                "FALSE"
            }),
        );
    }
    for char_info in &file.char_infos {
        let mut char_tree = Vec::<ast::Node<String>>::new();
        if char_info.width != FixWord::ZERO {
            char_tree.push(ast::Node::new(CHARACTER_WIDTH).with_fix_word(char_info.width));
        }
        if char_info.height != FixWord::ZERO {
            char_tree.push(ast::Node::new(CHARACTER_HEIGHT).with_fix_word(char_info.height));
        }
        if char_info.depth != FixWord::ZERO {
            char_tree.push(ast::Node::new(CHARACTER_DEPTH).with_fix_word(char_info.depth));
        }
        if char_info.italic_correction != FixWord::ZERO {
            char_tree
                .push(ast::Node::new(CHARACTER_ITALIC).with_fix_word(char_info.italic_correction));
        }
        root.push(
            ast::Node::new(CHARACTER)
                .with_character(char_info.id)
                .with_tree(char_tree),
        )
    }
    let style = PlStyle::default();
    ast::write(&root, &style)
}

pub fn write_fix_word(fix_word: FixWord) -> String {
    let mut output = String::new();
    let abs: u32 = if fix_word.0 < 0 {
        if fix_word.0 == i32::MIN {
            return "-2047.9999999".to_string();
        } else {
            output.push('-');
            fix_word.0.abs() as u32
        }
    } else {
        fix_word.0 as u32
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
        output.push(std::char::from_digit(integer_digits[i], 10).unwrap());
        i += 1;
    }

    output.push('.');
    let mut delta = 10;
    let mut fraction = abs % (1 << 20);
    fraction = fraction * 10 + 5;
    loop {
        if delta > (1 << 20) {
            fraction = fraction + (1 << 19) - (delta / 2);
        }
        output.push(std::char::from_digit(fraction / (1 << 20), 10).unwrap());
        fraction = (fraction % (1 << 20)) * 10;
        delta *= 10;
        if fraction <= delta {
            break;
        }
    }
    output
}

fn parse_fix_word(input: &str) -> Result<FixWord, String> {
    let mut input = input.chars();
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
    for c in input.by_ref() {
        match Char::new(c) {
            Char::Other('+') | Char::Other(' ') => (),
            Char::Other('-') => {
                negative = !negative;
            }
            Char::Digit(d) => {
                integer = Some(d);
                break;
            }
            Char::Other(other) => return Err(format!["unexpected character {}", other]),
        }
    }
    let negative = negative;

    let mut integer = match integer {
        None => panic![""],
        Some(integer) => integer,
    };
    for c in input.by_ref() {
        match Char::new(c) {
            Char::Digit(d) => {
                integer = integer * 10 + d;
                if integer >= 2048 {
                    return Err(format!["real numbers must be in the range [-2048,2048]"]);
                }
            }
            Char::Other('.') => break,
            Char::Other(other) => return Err(format!["unexpected character {}", other]),
        }
    }
    let integer = integer;

    let mut num_fractional_digits = 0;
    let mut fraction_digits = [0; 7];
    for c in input.by_ref() {
        match Char::new(c) {
            Char::Digit(d) => {
                if num_fractional_digits < 7 {
                    fraction_digits[num_fractional_digits] = d * (1 << 21);
                    num_fractional_digits += 1;
                }
            }
            Char::Other(other) => return Err(format!["unexpected character {}", other]),
        }
    }
    let mut fraction = 0;
    for i in (0..num_fractional_digits).rev() {
        fraction = fraction_digits[i] + fraction / 10;
    }
    let fraction = (fraction + 10) / 20;

    if integer == 2047 && fraction >= (1 << 20) {
        if negative {
            return Ok(FixWord(i32::MIN));
        }
        return Err(format!["real numbers must be in the range [-2048,2048]"]);
    }
    let mut result = integer * FixWord::UNITY.0 + fraction;
    if negative {
        result *= -1;
    }
    Ok(FixWord(result))
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
                let output = write_fix_word(start);

                let finish = parse_fix_word(&output).unwrap();
                assert_eq!(start, finish);

                let start = FixWord(value.wrapping_mul(-1));
                let output = write_fix_word(start);

                let finish = parse_fix_word(&output).unwrap();
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
