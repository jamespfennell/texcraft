//! Parser and writer for the property list (.pl) text format
use super::*;
use std::fmt::{Debug, Display};

pub mod ast;

const CHARACTER: &str = "CHARACTER";
const CHARACTER_WIDTH: &str = "CHARWD";
const CHARACTER_HEIGHT: &str = "CHARHT";
const CHARACTER_DEPTH: &str = "CHARDP";
const CHARACTER_ITALIC: &str = "CHARIT";
const CHECKSUM: &str = "CHECKSUM";
const CODING_SCHEME: &str = "CODINGSCHEME";
const COMMENT: &str = "COMMENT";
const DESIGN_SIZE: &str = "DESIGNSIZE";
const FACE: &str = "FACE";
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

const NUM_1: &str = "NUM1";
const NUM_2: &str = "NUM2";
const NUM_3: &str = "NUM3";
const DENOM_1: &str = "DENOM1";
const DENOM_2: &str = "DENOM2";
const SUP_1: &str = "SUP1";
const SUP_2: &str = "SUP2";
const SUP_3: &str = "SUP3";
const SUB_1: &str = "SUB1";
const SUB_2: &str = "SUB2";
const SUP_DROP: &str = "SUPDROP";
const SUB_DROP: &str = "SUBDROP";
const DELIM_1: &str = "DELIM1";
const DELIM_2: &str = "DELIM2";
const AXIS_HEIGHT: &str = "AXISHEIGHT";

const DEFAULT_THICKNESS: &str = "DEFAULTRULETHICKNESS";
const BIG_OP_SPACING_1: &str = "BIGOPSPACING1";
const BIG_OP_SPACING_2: &str = "BIGOPSPACING2";
const BIG_OP_SPACING_3: &str = "BIGOPSPACING3";
const BIG_OP_SPACING_4: &str = "BIGOPSPACING4";
const BIG_OP_SPACING_5: &str = "BIGOPSPACING5";

/// Format property list data.
///
/// This function is sort of equivalent to parsing the PL file and then serializing it again,
///   except PL files with invalid keywords and values are accepted.
/// It basically only requires that the PL file balances parentheses correctly.
/// Internally, it constructs the crate's abstract syntax tree for the PL input and then writes it out
///   *before* reading and validating the font metric data.
pub fn format(file_name: &str, input: &str, style: Style) -> String {
    let tree = ast::parse(file_name, input).unwrap();
    ast::write(&tree, style)
}

/// Parse property list data.
pub fn parse<'a>(file_name: &'a str, input: &'a str) -> Result<File, ParseError<Word<'a>>> {
    let tree = ast::parse(file_name, input)?;
    let mut file: File = Default::default();
    for node in tree.nodes() {
        match node.key() {
            CHECKSUM => {
                file.header.checksum = node.try_into()?;
            }
            CODING_SCHEME => {
                file.header.character_coding_scheme = Some(node.try_into()?);
            }
            COMMENT => {}
            DESIGN_SIZE => {
                file.header.design_size = node.try_into()?;
            }
            FACE => {
                file.header.face = Some(Face(node.try_into()?));
            }
            FAMILY => {
                file.header.font_family = Some(node.try_into()?);
            }
            SEVENBITSAFEFLAG => {
                file.header.seven_bit_safe = Some(node.try_into()?);
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
#[derive(Copy, Clone, Debug)]
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
        writeln!(
            f,
            "{}--> {}:{}:{}",
            padding,
            self.file_name,
            line_number,
            word_start + 1,
        )?;
        writeln!(f, "{} |", padding)?;
        writeln!(f, "{} | {}", line_number, line)?;
        writeln!(
            f,
            "{} | {}{}",
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

impl<T> From<ast::ConversionError<T>> for ParseError<T> {
    fn from(err: ast::ConversionError<T>) -> Self {
        ParseError::ConversionError(err)
    }
}

/// Convert a [File] to a PL AST.
pub fn to_ast(file: &File) -> ast::Tree<String> {
    let mut builder = ast::Tree::builder();
    if let Some(font_family) = &file.header.font_family {
        builder.add(FAMILY).with_str(font_family);
    }
    if let Some(face) = file.header.face {
        match face.try_into() {
            Ok::<String, _>(s) => {
                builder.add(FACE).with_str("F").with_string(s);
            }
            Err(_) => {
                builder.add(FACE).with_octal(face.0 as u32);
            }
        }
    }
    for word in &file.header.additional_data {
        // TODO: this is not quite right, the header needs to have indices also? ie.e HEADER O 23 O <word>
        builder.add(HEADER).with_octal(*word);
    }
    if let Some(coding_scheme) = &file.header.character_coding_scheme {
        builder.add(CODING_SCHEME).with_str(coding_scheme);
    }
    builder
        .add(DESIGN_SIZE)
        .with_fix_word(file.header.design_size);
    builder.add(COMMENT).with_str("DESIGNSIZE IS IN POINTS");
    builder
        .add(COMMENT)
        .with_str("OTHER SIZES ARE MULTIPLES OF DESIGNSIZE");
    builder.add(CHECKSUM).with_octal(file.header.checksum);
    if file.header.seven_bit_safe == Some(true) {
        builder.add(SEVENBITSAFEFLAG).with_str("TRUE");
    }

    {
        let mut params_tree = ast::Tree::builder();
        for (key, value) in convert_params(&file.params) {
            params_tree.add(&key).with_fix_word(value);
        }
        builder.add(FONT_DIMENSIONS).with_tree(params_tree.into());
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
    builder.into()
}

/// Write a [File] in property list format.
pub fn write(file: &File, style: Style) -> String {
    let tree: ast::Tree<String> = to_ast(file);
    ast::write(&tree, style)
}

fn convert_params(params: &Params) -> Vec<(String, FixWord)> {
    let v1 = vec![
        (SLANT, params.slant),
        (SPACE, params.space),
        (STRETCH, params.space_stretch),
        (SHRINK, params.space_shrink),
        (XHEIGHT, params.x_height),
        (QUAD, params.quad),
        (EXTRA_SPACE, params.extra_space),
    ];
    let v2 = match params.math_params {
        MathParams::None => vec![],
        MathParams::Symbols {
            num_1,
            num_2,
            num_3,
            denom_1,
            denom_2,
            sup_1,
            sup_2,
            sup_3,
            sub_1,
            sub_2,
            sup_drop,
            sub_drop,
            delim_1,
            delim_2,
            axis_height,
        } => vec![
            (NUM_1, num_1),
            (NUM_2, num_2),
            (NUM_3, num_3),
            (DENOM_1, denom_1),
            (DENOM_2, denom_2),
            (SUP_1, sup_1),
            (SUP_2, sup_2),
            (SUP_3, sup_3),
            (SUB_1, sub_1),
            (SUB_2, sub_2),
            (SUP_DROP, sup_drop),
            (SUB_DROP, sub_drop),
            (DELIM_1, delim_1),
            (DELIM_2, delim_2),
            (AXIS_HEIGHT, axis_height),
        ],
        MathParams::Extension {
            default_thickness,
            big_op_spacing,
        } => vec![
            (DEFAULT_THICKNESS, default_thickness),
            (BIG_OP_SPACING_1, big_op_spacing[0]),
            (BIG_OP_SPACING_2, big_op_spacing[1]),
            (BIG_OP_SPACING_3, big_op_spacing[2]),
            (BIG_OP_SPACING_4, big_op_spacing[3]),
            (BIG_OP_SPACING_5, big_op_spacing[4]),
        ],
    };
    let mut v = vec![];
    for (key, value) in v1 {
        v.push((key.to_string(), value));
    }
    for (key, value) in v2 {
        v.push((key.to_string(), value));
    }
    for additional_param in &params.additional_params {
        let key = format!["PARAMETER D {}", v.len() + 1];
        v.push((key, *additional_param))
    }
    v
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! fix_word_round_trip_tests {
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

    fix_word_round_trip_tests!(
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

    #[test]
    fn a() {
        let input = "
(FAMILY CMR)
(FACE O 352)
(CODINGSCHEME TEX TEXT)
(DESIGNSIZE R 10.0)
(COMMENT DESIGNSIZE IS IN POINTS)
(COMMENT OTHER SIZES ARE MULTIPLES OF DESIGNSIZE)
(CHECKSUM O 77)
(SEVENBITSAFEFLAG TRUE)
";
        let output = File {
            header: Header {
                checksum: 0o77,
                design_size: FixWord((1 << 20) * 10),
                character_coding_scheme: Some("TEX TEXT".to_string()),
                font_family: Some("CMR".to_string()),
                seven_bit_safe: Some(true),
                face: Some(Face(0o352)),
                additional_data: vec![],
            },
            char_infos: vec![],
            params: Params::default(),
        };

        let data = parse("", &input).unwrap();
        assert_eq!(output, data);

        let ast_1 = ast::parse("", &input).unwrap().into_string_tree();
        let ast_2 = to_ast(&data);
        assert_eq!(ast_1, ast_2);
    }
}
