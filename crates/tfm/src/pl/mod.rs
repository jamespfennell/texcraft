//! Parser and writer for the property list (.pl) text format
use super::*;
use std::{
    fmt::{Debug, Display},
    iter::Iterator,
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

/// Format property list data.
///
/// This function is sort of equivalent to parsing the PL file and then serializing it again,
///   except PL files with invalid keywords and values are accepted.
/// It basically only requires that the PL file balances parentheses correctly.
/// Internally, it constructs the crate's abstract syntax tree for the PL input and then writes it out
///   *before* reading and validating the font metric data.
pub fn format(file_name: &str, input: &str, style: &Style) -> String {
    let tree = ast::parse(file_name, input).unwrap();
    ast::write(&tree, style)
}

/// Parse property list data.
pub fn parse<'a>(file_name: &'a str, input: &'a str) -> Result<File, ParseError<Word<'a>>> {
    let tree = ast::parse(file_name, input)?;
    let mut file: File = Default::default();
    file.header.design_size = FixWord(FixWord::UNITY.0 * 10);
    for node in tree {
        match node.key() {
            COMMENT => {}
            FAMILY => {
                file.header.font_family = Some("hello".to_string());
            }

            _ => {
                return Err(ParseError::InvalidKey(node.key));
            }
        }
    }

    let output = write(&file, Style::default());
    println!["{}", output];

    Ok(file)
}

/// Style to apply when writing property list data.
#[derive(Debug)]
pub struct Style {
    /// Number of additional spaces to indent in each nested block.
    pub indent: usize,
    /// Closing brace style.
    pub closing_brace_style: ClosingBraceStyle,
}

impl Default for Style {
    fn default() -> Self {
        Self {
            indent: 3,
            closing_brace_style: Default::default(),
        }
    }
}

/// Style of closing braces when writing property list data.
#[derive(Debug, PartialEq, Eq)]
pub enum ClosingBraceStyle {
    /// Place closing braces on the same line.
    SameLine,
    /// Place closing bracess on a new line with the same indentation as the matching opening brace.
    MatchingOpening,
    /// Place closing bracess on a new line with one extra indent compared to the matching opening brace.
    ExtraIndent,
}

impl Default for ClosingBraceStyle {
    fn default() -> Self {
        ClosingBraceStyle::ExtraIndent
    }
}

/// A word in a property list file.
#[derive(Clone, Debug)]
pub struct Word<'a> {
    pub file_name: &'a str,
    pub file: &'a str,
    pub start: usize,
    pub end: usize,
}

impl<'a> AsRef<str> for Word<'a> {
    fn as_ref(&self) -> &str {
        &self.file[self.start..self.end]
    }
}

impl<'a> Display for Word<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (line_number, line, word_start) = {
            let mut line_number = 1;
            let mut line_start = 0;
            for (position, char) in self.file[..self.start].char_indices() {
                if char == '\n' {
                    line_number += 1;
                    line_start = position + 1;
                }
            }
            let tail = &self.file[line_start..];
            let line = match tail.find('\n') {
                None => tail,
                Some(end) => &tail[..end],
            };
            (line_number, line, self.start - line_start)
        };

        let line_number_str = format!["{}", line_number];
        let padding = " ".repeat(line_number_str.len());
        write!(
            f,
            "{}--> {}:{}:{}\n",
            padding,
            self.file_name,
            line_number,
            word_start + 1,
        )?;
        write!(f, "{} |\n", padding)?;
        write!(f, "{} | {}\n", line_number, line)?;
        write!(
            f,
            "{} | {}{}\n",
            padding,
            " ".repeat(word_start),
            "^".repeat(self.end - self.start),
        )?;
        Ok(())
    }
}

/// Error type for property list parsing.
#[derive(Debug)]
pub enum ParseError<T> {
    Parse(ast::ParseError<T>),
    ConversionError(ast::ConversionError<T>),
    InvalidKey(T),
}

impl<T: Display + AsRef<str>> Display for ParseError<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::Parse(_err) => todo!(),
            ParseError::ConversionError(_) => todo!(),
            ParseError::InvalidKey(key) => write!(f, "invalid key '{}'\n{}", key.as_ref(), key),
        }
    }
}

impl<T> From<ast::ParseError<T>> for ParseError<T> {
    fn from(err: ast::ParseError<T>) -> Self {
        ParseError::Parse(err)
    }
}

/// Write a [File] in property list format.
pub fn write(file: &File, style: Style) -> String {
    let mut builder = ast::Tree::builder();
    if let Some(font_family) = &file.header.font_family {
        builder.add(FAMILY).with_str(font_family);
    }
    for word in &file.header.additional_data {
        // TODO: this is not quite right
        builder.add(HEADER).with_octal(*word);
    }
    if let Some(coding_scheme) = &file.header.character_coding_scheme {
        builder.add(CODING_SCHEME).with_str(coding_scheme);
    }
    builder
        .add(DESIGNSIZE)
        .with_fix_word(file.header.design_size);
    builder.add(COMMENT).with_str("DESIGNSIZE IS IN POINTS");
    builder
        .add(COMMENT)
        .with_str("OTHER SIZES ARE MULTIPLES OF DESIGNSIZE");
    builder.add(CHECKSUM).with_octal(file.header.checksum);
    if let Some(seven_bit_safe) = file.header.seven_bit_safe {
        builder
            .add(SEVENBITSAFEFLAG)
            .with_str(if seven_bit_safe { "TRUE" } else { "FALSE" });
    }
    for char_info in &file.char_infos {
        let mut char_tree = ast::Tree::builder();
        if char_info.width != FixWord::ZERO {
            char_tree
                .add(CHARACTER_WIDTH)
                .with_fix_word(char_info.width);
        }
        if char_info.height != FixWord::ZERO {
            char_tree
                .add(CHARACTER_HEIGHT)
                .with_fix_word(char_info.height);
        }
        if char_info.depth != FixWord::ZERO {
            char_tree
                .add(CHARACTER_DEPTH)
                .with_fix_word(char_info.depth);
        }
        if char_info.italic_correction != FixWord::ZERO {
            char_tree
                .add(CHARACTER_ITALIC)
                .with_fix_word(char_info.italic_correction);
        }
        builder
            .add(CHARACTER)
            .with_character(char_info.id)
            .with_tree(char_tree.into());
    }
    let tree: ast::Tree<String> = builder.into();
    let style = Style::default();
    ast::write(tree.nodes(), &style)
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
