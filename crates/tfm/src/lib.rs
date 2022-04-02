//! Crate for working with TeX font metric (.tfm) and property list (.pl) formats

use std::{collections::HashMap, panic};
mod format;
mod pl;

/// Complete contents of a TeX font metric (.tfm) or property list (.pl) file.
pub struct File {
    /// The file header.
    pub header: Header,

    /// Map from character code to the character information for that character.
    // TODO: maybe a vector based map?
    pub char_infos: HashMap<u8, CharInfo>,

    // TODO lig_kerns and extensible characters
    /// Additional parameters contained in the file.
    pub params: Params,
}

/// The TFM header, which contains metadata about the file.
#[derive(Debug, PartialEq, Eq)]
pub struct Header {
    pub checksum: u32,
    pub design_size: FixWord,
    pub character_coding_scheme: Option<String>,
    pub font_family: Option<String>,
    pub seven_bit_safe: Option<bool>,
    pub face: Option<Face>,

    /// The TFM format allows the header have arbitrary additional data in the header.
    pub additional_data: Vec<u32>,
}

/// Fixed-width numeric type used in TFM files.
///
/// This type has 11 bits for the integer part,
/// 20 bits for the fractional part, and a single signed bit.
///
/// In property list files, this type is represented as a decimal number
///   with up to 6 digits after the decimal point.
/// This is a non-lossy representation
///   because 10^(-6) is larger than 2^(-20).
#[derive(PartialEq, Eq, Debug)]
pub struct FixWord(i32);

impl FixWord {
    /// Representation of the number 1 as a [FixWord].
    pub const UNITY: FixWord = FixWord(1 << 20);
}

impl std::fmt::Display for FixWord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pl::write_fix_word(self))
    }
}

/// Information about a character.
pub struct CharInfo {
    /// The width of the character.
    pub width: FixWord,
    /// The height of the character.
    pub height: FixWord,
    /// The depth of the character.
    pub depth: FixWord,

    pub italic_correction: FixWord,
}

/// Parse TeX font metric (.tfm) data.
pub fn parse_tfm(input: &[u8]) -> File {
    format::parse(input)
}

/// Serialize a [File] to TeX font metric (.tfm) format.
pub fn serialize_tfm(file: &File) -> Vec<u8> {
    format::serialize(file)
}

/// Parse property list (.pl) data.
pub fn parse_pl(input: &str) -> File {
    pl::parse(input)
}

/// Serialize a [File] to property list (.pl) format.
pub fn serialize_pl(file: &File, style: PlStyle) -> String {
    pl::serialize(file)
}

/// Format property list (.pl) data
///
/// This function is sort of equivalent to parsing the PL file and then serializing it again,
///   except PL files with invalid keywords and values are accepted.
/// It basically only requires the the PL file balances parentheses correctly.
/// Internally, it constructs the crate's abstract syntax tree for the PL input and then writes it out
///   *before* reading and validating the font metric data within.
pub fn format_pl(input: &str, style: &PlStyle) -> String {
    pl::format(input, style)
}

#[derive(Debug)]
pub struct PlStyle {
    pub indent: usize,
    pub closing_brace_style: ClosingBraceStyle,
}

impl Default for PlStyle {
    fn default() -> Self {
        Self {
            indent: 3,
            closing_brace_style: Default::default(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ClosingBraceStyle {
    SameLine,
    MatchingOpening,
    ExtraIndent,
}

impl Default for ClosingBraceStyle {
    fn default() -> Self {
        ClosingBraceStyle::ExtraIndent
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Tag {
    None,
    Ligature(usize),
    List(usize),
    Extension(usize),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Weight {
    Medium,
    Bold,
    Light,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Slope {
    Roman,
    Italic,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expansion {
    Regular,
    Condensed,
    Expanded,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Face {
    pub weight: Weight,
    pub slope: Slope,
    pub expansion: Expansion,
}

impl Face {
    fn to_u8(&self) -> u8 {
        let a = match self.slope {
            Slope::Roman => 0_u8,
            Slope::Italic => 1_u8,
        };
        let b = match self.weight {
            Weight::Medium => 0_u8,
            Weight::Bold => 2_u8,
            Weight::Light => 4_u8,
        };
        let c = match self.expansion {
            Expansion::Regular => 0_u8,
            Expansion::Condensed => 6_u8,
            Expansion::Expanded => 12_u8,
        };
        a + b + c
    }

    fn from_u8(mut raw: u8) -> Option<Face> {
        if raw >= 18 {
            None
        } else {
            let slope = if raw % 2 == 0 {
                Slope::Roman
            } else {
                Slope::Italic
            };
            raw /= 2;
            let weight = match raw % 3 {
                0 => Weight::Medium,
                1 => Weight::Bold,
                _ => Weight::Light,
            };
            raw /= 3;
            let expansion = match raw % 3 {
                0 => Expansion::Regular,
                1 => Expansion::Condensed,
                _ => Expansion::Expanded,
            };
            Some(Face {
                weight,
                slope,
                expansion,
            })
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
struct RawCharInfo {
    width_index: usize,
    height_index: usize,
    depth_index: usize,
    italic_index: usize,
    tag: Tag,
}

#[derive(Debug, PartialEq, Eq)]
struct RawLigKern {
    next_raw_lig_kern: Option<usize>,
    next_char: u8,
    op: RawLigKernOp,
}

#[derive(Debug, PartialEq, Eq)]
enum RawLigKernOp {
    Kern(usize),
    Ligature {
        insert_char: u8,
        delete_current: bool,
        delete_next: bool,
        skip: u8,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub struct ExtensibleChar {
    top: u8,
    middle: u8,
    bottom: u8,
    rep: u8,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Params {
    slant: FixWord,
    space: FixWord,
    space_stretch: FixWord,
    space_shrink: FixWord,
    x_height: FixWord,
    quad: FixWord,
    extra_space: FixWord,
    additional_params: Vec<u32>,
}
