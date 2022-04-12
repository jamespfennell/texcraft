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
                let output = format!("{}", start);
                let output_ref: &str = &output;
                let finish: FixWord = output_ref.try_into().unwrap();
                assert_eq!(start, finish);

                let start = FixWord(value.wrapping_mul(-1));
                let output = format!("{}", start);
                let output_ref: &str = &output;
                let finish: FixWord = output_ref.try_into().unwrap();
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
