//! # tfm: TeX font metric data
//!
//! This is a crate for working with TeX font metric data.
//! It includes:
//!
//! - Functions to read and write TeX font metric (.tfm) files
//!     to and from a value of type [`format::File`]
//!     ([`deserialize`](format::File::deserialize), [`serialize`](format::File::serialize)).
//!
//! - Functions to read and write property list (.pl) files
//!     to and from a value of type [`pl::File`]
//!     ([`from_pl_source_code`](pl::File::from_pl_source_code), [`display`](pl::File::display)).
//!
//! - Converters from .tfm to .pl files and vice-versa
//!     (using Rust's [`From`] trait to go between [`format::File`] and [`pl::File`]).
//!
//! - A type [`Font`] that represents a fully validated and compiled TeX font
//!     and that can be used to efficiently query data about the font
//!     (e.g., "what is the width of the character A?").
//!     This type and its methods are performance optimized and
//!     designed for use in the hot main loops
//!     of typesetting software such as TeX.
//!
//! ## Background
//!
//! Probably the most famous part of the implementation of TeX is the Knuth-Plass line breaking algorithm.
//! This algorithm determines the "optimal" places to add line breaks when typesetting a paragraph of text.
//! In order to run the algorithm one needs to provide the dimensions of all characters in the current font.
//! These dimensions are used to size the boxes in the Knuth-Plass box and glue model.
//!
//! In TeX, character dimensions are provided using TeX font metric files.
//! These are binary files.
//! By convention they have a .tfm file extension.
//! Unlike more modern file formats like TrueType, .tfm files only contain the font dimensions;
//!     they don't contains the glyphs.
//! In general,
//!     .tfm files are produced by other software like Metafont,
//!     placed in some well-known directory in the TeX distribution,
//!     and then read into memory when TeX is running.
//!
//! Because .tfm files are binary files, it's hard to debug or tweak them.
//! To remedy this, Knuth and his team developed another file format called a property list file
//!     (extension .pl or .plst)
//!     that contains the same information but in a modifiable text format.
//! They then wrote two programs:
//!     `tftopl` to convert a .tfm file to a .pl file,
//!     and `pltotf` to convert a .pl file to a .tfm file.
//!
//! The general goal of this crate to fully re-implement all of the TeX font metric
//!     code written by Knuth and others.
//! This includes `tftopl`, `pltotf`, and also the parts of TeX itself that contain logic
//!     for reading and interpreting .tfm files.
//! However, unlike these monolithic software programs,
//!     this re-implementation is in the form of a modular library in which
//!     individual pieces of logic and be used and re-used.
//!
//! ## Basic example
//!
//! ```
//! // Include the .tfm file for Computer Modern in size 10pt.
//! let tfm_bytes = include_bytes!["../corpus/computer-modern/cmr10.tfm"];
//!
//! // Deserialize the .tfm file.
//! let (tfm_file_or_error, deserialization_warnings) = tfm::format::File::deserialize(tfm_bytes);
//! let mut tfm_file = tfm_file_or_error.expect("cmr10.tfm is a valid .tfm file");
//! assert_eq![deserialization_warnings, vec![], "the data in cmr10.tfm is 100% valid, so there are no deserialization warnings"];
//! // TODO assert_eq![tfm_file.header.design_size, tfm::FixWord::UNITY * 10]; make it 11 to be more interesting
//! // TODO query some data
//!
//! // Validate the .tfm file.
//! let validation_warnings = tfm_file.validate_and_fix();
//! assert_eq![validation_warnings, vec![], "the data in cmr10.tfm is 100% valid, so there are no validation warnings"];
//!
//! // Convert the .tfm file to a .pl file and print it.
//! let pl_file: tfm::pl::File = tfm_file.clone().into();
//! // TODO query some data
//! println!["cmr10.pl:\n{}", pl_file.display(/*indent=*/2, tfm::pl::CharDisplayFormat::Default)];
//!
//! // TODO Convert the .tfm file to the crate's Font type.
//! ```
//!
//!
//! ## Advanced functionality
//!
//! In addition to supporting the basic use cases of querying font metric data
//!     and converting between different formats,
//!     this crate has advanced functionality for performing additional tasks on font metric data.
//! The full set of functionality can be understood by navigating through the crate documentation.
//! But here are 3 highlights we think are interesting:
//!
//! - **Language analysis of .pl files**:
//!     In `pltotf`, Knuth parses .pl files in a single pass.
//!     This crate takes a common approach nowadays of parsing in multiple passes:
//!     first constructing a [concrete syntax tree](pl::cst::Cst) (or parse tree),
//!     next constructing a [fully typed and checked abstract syntax tree](pl::ast::Ast),
//!     and finally building the [`pl::File`] itself.
//!     Each of the passes is exposed, so you can e.g. just build the AST for the .pl file and
//!         do some analysis on it.
//!
//! - **Debug output for .tfm files**:
//!     
//! - **Compilation of lig/kern programs**:
//!
//!
//! ## Binaries
//!
//! The Texcraft project produces 3 binaries based on this crate:
//!
//! - `tftopl` and `pltotf`: re-implementations of Knuth's programs.
//! - `tfmtools`: a new binary that has a bunch of different tools
//!         for working with TeX font metric data.
//!         Run `tfmtools help` to list all of the available tools.
//!
//! In the root of [the Texcraft repository](https://github.com/jamespfennell/texcraft)
//!     these tools can be run with `cargo run --bin $NAME`
//!     and built with `cargo build --bin $NAME`.
//!
//!
//! ## Correctness
//!
//! As part of the development of this crate significant effort has been spent
//!     ensuring it exactly replicates the work of Knuth.
//! This correctness checking is largely based around diff testing the binaries
//!     `tftopl` and `pltotf`.
//! We verify that the Texcraft and Knuth implementations have the same output
//!     and generate the same error messages.
//!
//! This diff testing has been performed in a few different ways:
//!
//! - We have run diff tests over all ~100,000 .tfm files in CTAN.
//!     These tests verify that `tftopl` gives the correct result,
//!     and that running `pltotf` on the output .pl file gives the correct result too.
//!     Unfortunately running `pltotf` on the .pl files in CTAN is infeasible
//!     because most of these files are Perl scripts, not property list files.
//!
//! - We have developed a fuzz testing harness (so far just for `tftopl`)
//!     that generates highly heterogenous .tfm files and verifies that `tftopl` gives the correct result.
//!     This fuzz testing has uncovered many issues in the Texcraft implementation,
//!     and has even identified [a 30-year old bug](https://tug.org/pipermail/tex-k/2024-March/004031.html)
//!     in Knuth's implementation of `tftopl`.
//!
//! Any .tfm or .pl file that exposes a bug in this library is added to
//!     [our automated testing corpus](https://github.com/jamespfennell/texcraft/tree/main/crates/tfm/bin/tests/data).
//! Running `cargo t` validates that Texcraft's binaries give the same result as Knuth's binaries
//!     (the output of Knuth's binaries is in source control).
//! This ensures there are no regressions.
//!
//! If you discover a .tfm or .pl file such that the Texcraft and Knuth implementations
//!     diverge, this indicates there is a bug in this library.
//! Please create an issue on the Texcraft GitHub repo.
//! We will fix the bug and add your files to the testing corpus.

pub mod algorithms;
use std::{
    collections::{BTreeSet, HashMap, HashSet},
    num::NonZeroU8,
};
pub mod format;
pub mod ligkern;
pub mod pl;

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Font {
    /// Header.
    pub header: Header,

    /// Character dimensions.
    pub char_dimens: HashMap<Char, CharDimensions>,

    /// Lig kern program.
    pub lig_kern_program: ligkern::CompiledProgram,

    /// Next larger program
    pub next_larger_program: NextLargerProgram,

    /// Font parameters.
    pub params: Vec<FixWord>,
}

impl Font {
    pub fn deserialize_from_tfm(b: &[u8]) -> Result<Font, format::DeserializationError> {
        let (file_or, _) = format::File::deserialize(b);
        // TODO: maybe the following logic should live in the validate_and_fix function.
        // And that function should return the fully valid programs.
        let file = file_or?;
        let entrypoints: HashMap<Char, u16> = file
            .char_tags
            .iter()
            .filter_map(|(c, t)| t.ligature().map(|l| (*c, l as u16)))
            .collect();
        let (lig_kern_program, _) =
            ligkern::CompiledProgram::compile(&file.lig_kern_program, &file.kerns, entrypoints);
        let (next_larger_program, _) = NextLargerProgram::new(
            file.char_tags
                .iter()
                .filter_map(|(c, t)| t.list().map(|l| (*c, l))),
            |c| file.char_dimens.contains_key(&c),
            true,
        );
        let char_dimens = file
            .char_dimens
            .into_iter()
            .map(|(char, d)| {
                (
                    char,
                    CharDimensions {
                        width: file
                            .widths
                            .get(d.width_index.get() as usize)
                            .copied()
                            .unwrap_or_default(),
                        height: file
                            .heights
                            .get(d.height_index as usize)
                            .copied()
                            .unwrap_or_default(),
                        depth: file
                            .depths
                            .get(d.depth_index as usize)
                            .copied()
                            .unwrap_or_default(),
                        italic_correction: file
                            .italic_corrections
                            .get(d.italic_index as usize)
                            .copied()
                            .unwrap_or_default(),
                    },
                )
            })
            .collect();
        Ok(Font {
            header: file.header,
            char_dimens,
            lig_kern_program,
            next_larger_program,
            params: file.params,
        })
    }
}

/// The TFM header, which contains metadata about the file.
///
/// This is defined in TFtoPL.2014.10.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Header {
    /// The font checksum, if specified.
    ///
    /// In .tfm files checksums are always specified because the format has no
    ///     way to omit a checksum.
    ///
    /// In .pl files checksums are specified if the `CHECKSUM` node appears.
    /// If no checksum is specified in a .pl file, pltotf calculates the
    ///     correct value and writes that.
    ///
    /// In TeX82, this is stored in the `font_check` array (TeX82.2021.549).
    pub checksum: Option<u32>,
    /// In TeX82, this is stored in the `font_dsize` array (TeX82.2021.549).
    pub design_size: FixWord,
    pub design_size_valid: bool,
    pub character_coding_scheme: Option<String>,
    pub font_family: Option<String>,
    pub seven_bit_safe: Option<bool>,
    pub face: Option<Face>,
    /// The TFM format allows the header to contain arbitrary additional data.
    pub additional_data: Vec<u32>,
}

impl Header {
    /// Returns the default header when parsing property list files.
    ///
    /// This is defined in PLtoTF.2014.70.
    pub fn pl_default() -> Header {
        Header {
            checksum: None,
            design_size: FixWord::ONE * 10,
            design_size_valid: true,
            character_coding_scheme: Some("UNSPECIFIED".into()),
            font_family: Some("UNSPECIFIED".into()),
            seven_bit_safe: None,
            face: Some(0.into()),
            additional_data: vec![],
        }
    }

    /// Returns the default header when parsing .tfm files.
    ///
    /// This is defined in PLtoTF.2014.70.
    pub fn tfm_default() -> Header {
        Header {
            checksum: Some(0),
            design_size: FixWord::ZERO,
            design_size_valid: true,
            character_coding_scheme: None,
            font_family: None,
            seven_bit_safe: None,
            face: None,
            additional_data: vec![],
        }
    }
}

/// A character in a TFM file.
///
/// TFM and PL files only support 1-byte characters.
#[derive(Debug, Default, PartialEq, Eq, Hash, Clone, Copy, PartialOrd, Ord)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Char(pub u8);

impl std::fmt::Display for Char {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if (self.0 as char).is_ascii_graphic() {
            write!(f, "{}", self.0 as char)
        } else {
            write!(f, "0x{:02x}", self.0)
        }
    }
}

impl From<u8> for Char {
    fn from(value: u8) -> Self {
        Char(value)
    }
}

impl TryFrom<char> for Char {
    type Error = std::char::TryFromCharError;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        let u: u8 = value.try_into()?;
        Ok(Char(u))
    }
}

impl From<Char> for char {
    fn from(value: Char) -> Self {
        value.0 as char
    }
}

macro_rules! const_chars {
    ( $( ($name: ident, $value: expr), )+ ) => {
        $(
            pub const $name: Char = Char($value);
        )+
    };
}

impl Char {
    const_chars![
        (A, b'A'),
        (B, b'B'),
        (C, b'C'),
        (D, b'D'),
        (X, b'X'),
        (Y, b'Y'),
        (Z, b'Z'),
    ];

    pub fn is_seven_bit(&self) -> bool {
        self.0 <= 127
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct CharDimensions {
    pub width: FixWord,
    pub height: FixWord,
    pub depth: FixWord,
    pub italic_correction: FixWord,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum FaceWeight {
    Light,
    Medium,
    Bold,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum FaceSlope {
    Roman,
    Italic,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum FaceExpansion {
    Regular,
    Condensed,
    Extended,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Face {
    Valid(FaceWeight, FaceSlope, FaceExpansion),
    Other(u8),
}

impl From<u8> for Face {
    fn from(value: u8) -> Self {
        if value >= 18 {
            return Face::Other(value);
        }
        let a = match (value % 6) / 2 {
            0 => FaceWeight::Medium,
            1 => FaceWeight::Bold,
            2 => FaceWeight::Light,
            _ => unreachable!(),
        };
        let b = match value % 2 {
            0 => FaceSlope::Roman,
            1 => FaceSlope::Italic,
            _ => unreachable!(),
        };
        let c = match value / 6 {
            0 => FaceExpansion::Regular,
            1 => FaceExpansion::Condensed,
            2 => FaceExpansion::Extended,
            _ => unreachable!(),
        };
        Face::Valid(a, b, c)
    }
}

impl From<Face> for u8 {
    fn from(value: Face) -> Self {
        match value {
            Face::Valid(w, s, c) => {
                let a: u8 = match w {
                    FaceWeight::Medium => 0,
                    FaceWeight::Bold => 1,
                    FaceWeight::Light => 2,
                };
                let b: u8 = match s {
                    FaceSlope::Roman => 0,
                    FaceSlope::Italic => 1,
                };
                let c: u8 = match c {
                    FaceExpansion::Regular => 0,
                    FaceExpansion::Condensed => 1,
                    FaceExpansion::Extended => 2,
                };
                c * 6 + a * 2 + b
            }
            Face::Other(b) => b,
        }
    }
}

/// A named TeX font metric parameter.
#[derive(PartialEq, Eq, Debug, Copy, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum NamedParameter {
    Slant,
    Space,
    Stretch,
    Shrink,
    XHeight,
    Quad,
    ExtraSpace,
    Num1,
    Num2,
    Num3,
    Denom1,
    Denom2,
    Sup1,
    Sup2,
    Sup3,
    Sub1,
    Sub2,
    SupDrop,
    SubDrop,
    Delim1,
    Delim2,
    AxisHeight,
    DefaultRuleThickness,
    BigOpSpacing1,
    BigOpSpacing2,
    BigOpSpacing3,
    BigOpSpacing4,
    BigOpSpacing5,
}

impl NamedParameter {
    pub fn number(&self) -> u8 {
        match self {
            NamedParameter::Slant => 1,
            NamedParameter::Space => 2,
            NamedParameter::Stretch => 3,
            NamedParameter::Shrink => 4,
            NamedParameter::XHeight => 5,
            NamedParameter::Quad => 6,
            NamedParameter::ExtraSpace => 7,
            NamedParameter::Num1 => 8,
            NamedParameter::Num2 => 9,
            NamedParameter::Num3 => 10,
            NamedParameter::Denom1 => 11,
            NamedParameter::Denom2 => 12,
            NamedParameter::Sup1 => 13,
            NamedParameter::Sup2 => 14,
            NamedParameter::Sup3 => 15,
            NamedParameter::Sub1 => 16,
            NamedParameter::Sub2 => 17,
            NamedParameter::SupDrop => 18,
            NamedParameter::SubDrop => 19,
            NamedParameter::Delim1 => 20,
            NamedParameter::Delim2 => 21,
            NamedParameter::AxisHeight => 22,
            NamedParameter::DefaultRuleThickness => 8,
            NamedParameter::BigOpSpacing1 => 9,
            NamedParameter::BigOpSpacing2 => 10,
            NamedParameter::BigOpSpacing3 => 11,
            NamedParameter::BigOpSpacing4 => 12,
            NamedParameter::BigOpSpacing5 => 13,
        }
    }
}

/// Warning from the compilation of "next larger character" instructions.
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum NextLargerProgramWarning {
    NonExistentCharacter { original: Char, next_larger: Char },
    InfiniteLoop { original: Char, next_larger: Char },
}

impl NextLargerProgramWarning {
    pub fn bad_char(&self) -> Char {
        match self {
            NextLargerProgramWarning::NonExistentCharacter {
                original,
                next_larger: _,
            } => *original,
            NextLargerProgramWarning::InfiniteLoop { original, .. } => *original,
        }
    }

    /// Returns the warning message the TFtoPL program prints for this kind of error.
    pub fn tftopl_message(&self) -> String {
        match self {
            NextLargerProgramWarning::NonExistentCharacter {
                original: _,
                next_larger,
            } => {
                format![
                    "Bad TFM file: Character list link to nonexistent character '{:03o}.",
                    next_larger.0
                ]
            }
            NextLargerProgramWarning::InfiniteLoop { original, .. } => {
                format!["Bad TFM file: Cycle in a character list!\nCharacter '{:03o} now ends the list.", original.0]
            }
        }
    }

    /// Returns the section in Knuth's TFtoPL (version 2014) in which this warning occurs.
    pub fn tftopl_section(&self) -> u8 {
        84
    }

    /// Returns the section in Knuth's PLtoTF (version 2014) in which this warning occurs.
    pub fn pltotf_section(&self) -> u8 {
        match self {
            NextLargerProgramWarning::NonExistentCharacter { .. } => 111,
            NextLargerProgramWarning::InfiniteLoop { .. } => 113,
        }
    }

    /// Returns the warning message the PLtoTF program prints for this kind of error.
    pub fn pltotf_message(&self) -> String {
        match self {
            NextLargerProgramWarning::NonExistentCharacter {
                original,
                next_larger: _,
            } => {
                format![
                    "The character NEXTLARGER than '{:03o} had no CHARACTER spec.",
                    original.0
                ]
            }
            NextLargerProgramWarning::InfiniteLoop { original, .. } => {
                format![
                    "A cycle of NEXTLARGER characters has been broken at '{:03o}.",
                    original.0
                ]
            }
        }
    }
}
/// Compiled program of "next larger character" instructions
///
/// The .tfm file format can associate a "next larger" character to any character in a font.
/// Next larger characters form sequences: i.e. B can be the next larger character for A,
///     and C can be the next larger character for B,
///     leading to the sequences A-B-C.
/// These next larger characters are used at certain points in TeX.
/// TeX occasionally traverses the entire sequence for a given starting character (e.g. A).
///
/// As with ligatures, next larger specifications can contain infinite loops -
///     e.g, if X is the next larger character for Y
///      and Y is the next larger character for X.
/// These loops are invalid and removed by TFtoPL and PLtoTF.
///
/// Motivated by the idea of "parse don't validate", this type represents
///     a compiled version of the next larger specifications in which infinite loops
///     are statically guaranteed not to exist.
///
/// The basic use of a valid program looks like this:
///
/// ```
/// # use tfm::*;
/// let edges = vec![
///     (Char::A, Char::B),
///     (Char::B, Char::C),
/// ];
/// let (next_larger_program, warnings) = NextLargerProgram::new(edges.into_iter(), |_| true, true);
///
/// assert_eq![warnings, vec![]];
///
/// let sequence_A: Vec<Char> = next_larger_program.get(Char::A).collect();
/// assert_eq!(sequence_A, vec![Char::B, Char::C]);
///
/// let sequence_B: Vec<Char> = next_larger_program.get(Char::B).collect();
/// assert_eq!(sequence_B, vec![Char::C]);
///
/// let sequence_C: Vec<Char> = next_larger_program.get(Char::C).collect();
/// assert_eq!(sequence_C, vec![]);
///
/// // Character that is not in the program.
/// let sequence_D: Vec<Char> = next_larger_program.get(Char::D).collect();
/// assert_eq!(sequence_D, vec![]);
/// ```
///
/// ## Warnings
///
/// There are two types of error that can occur when constructing the next
///     larger program.
/// Both of these errors are handled gracefully, so we officially refer to them as warnings.
/// The constructor returns them as values of type [`NextLargerProgramWarning`].
///
/// ### Infinite loops
///
/// The first error is that next larger programs can contain infinite loops -
///     e.g, if X is the next larger character for Y
///      and Y is the next larger character for X.
/// In this case the loop is broken by removing the next larger program for the
///     character with the largest 8-bit code, in this case Y.
/// A [`NextLargerProgramWarning::InfiniteLoop`] warning is returned
///     from the program constructor.
///
/// ```
/// # use tfm::*;
/// let edges = vec![
///     (Char::X, Char::Y),
///     (Char::Y, Char::X),
/// ];
/// let (next_larger_program, warnings) = NextLargerProgram::new(edges.into_iter(), |_| true, true);
///
/// assert_eq!(warnings, vec![NextLargerProgramWarning::InfiniteLoop{
///     original: Char::Y,
///     next_larger: Char::X,
/// }]);
///
/// let sequence_X: Vec<Char> = next_larger_program.get(Char::X).collect();
/// assert_eq!(sequence_X, vec![Char::Y]);
///
/// let sequence_Y: Vec<Char> = next_larger_program.get(Char::Y).collect();
/// assert_eq!(sequence_Y, vec![]);
/// ```
///
/// ### Non-existent characters
///
/// The second error is that characters referred to in the next larger program
///     may not be defined in the .tfm or .pl file.
/// For example, a .pl file may contain the snippet `(CHARACTER C X (NEXTLARGER C Y))`
///     without defining the character Y.
/// The constructor [`NextLargerProgram::new`] accepts a function for checking if a
///     character exists.
/// In all cases a [`NextLargerProgramWarning::NonExistentCharacter`] warning is returned
///     if a non-existent character is encountered.
///
/// The behavior of the resulting next larger program is configured using the
///     `drop_non_existent_characters` argument.
/// If this is false, then the behavior is the same as PLtoTF and the program still
///     contains the character.
///
/// ```
/// # use tfm::*;
/// let edges = vec![
///     (Char::X, Char::Y),
/// ];
/// let character_exists = |c| {
///     if c == Char::Y {
///         false
///     } else {
///         true
///     }
/// };
/// let (next_larger_program, warnings) = NextLargerProgram::new(edges.into_iter(), character_exists, false);
///
/// assert_eq!(warnings, vec![NextLargerProgramWarning::NonExistentCharacter{
///     original: Char::X,
///     next_larger: Char::Y,
/// }]);
///
/// let sequence_X: Vec<Char> = next_larger_program.get(Char::X).collect();
/// assert_eq!(sequence_X, vec![Char::Y]);
/// ```
///
/// If `drop_non_existent_characters` is true, next larger instructions pointing at non-existent
///     characters are dropped.
/// This is how TFtoPL behaves.
///
///
/// ```
/// # use tfm::*;
/// let edges = vec![
///     (Char::X, Char::Y),
/// ];
/// let character_exists = |c| {
///     if c == Char::Y {
///         false
///     } else {
///         true
///     }
/// };
/// let (next_larger_program, warnings) = NextLargerProgram::new(edges.into_iter(), character_exists, true);
///
/// assert_eq!(warnings, vec![NextLargerProgramWarning::NonExistentCharacter{
///     original: Char::X,
///     next_larger: Char::Y,
/// }]);
///
/// let sequence_X: Vec<Char> = next_larger_program.get(Char::X).collect();
/// assert_eq!(sequence_X, vec![]);
/// ```
///
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct NextLargerProgram {
    entrypoints: HashMap<Char, u8>,
    next_larger: Vec<(Char, NonZeroU8)>,
}

impl NextLargerProgram {
    /// Build a new next larger program from an iterator over edges.
    pub fn new<I: Iterator<Item = (Char, Char)>, F: Fn(Char) -> bool>(
        edges: I,
        character_exists: F,
        drop_non_existent_characters: bool,
    ) -> (Self, Vec<NextLargerProgramWarning>) {
        // This function implements functionality in TFtoPL.2014.84 and PLtoTF.2014.{110,111,113}.
        let mut warnings: Vec<NextLargerProgramWarning> = vec![];

        let mut node_to_larger = HashMap::<Char, Char>::new();
        let mut node_to_num_smaller = HashMap::<Char, usize>::new();
        for (smaller, larger) in edges {
            if !character_exists(larger) {
                warnings.push(NextLargerProgramWarning::NonExistentCharacter {
                    original: smaller,
                    next_larger: larger,
                });
                if drop_non_existent_characters {
                    continue;
                }
            }
            node_to_larger.insert(smaller, larger);
            node_to_num_smaller.entry(smaller).or_default();
            *node_to_num_smaller.entry(larger).or_default() += 1;
        }

        let mut leaves: Vec<Char> = vec![];
        let mut non_leaves: BTreeSet<Char> = Default::default();
        for (node, num_smaller) in &node_to_num_smaller {
            if *num_smaller == 0 {
                leaves.push(*node);
            } else {
                non_leaves.insert(*node);
            }
        }

        let mut sorted_chars = vec![];
        let mut infinite_loop_warnings = vec![];
        loop {
            while let Some(smaller) = leaves.pop() {
                if let Some(larger) = node_to_larger.get(&smaller).copied() {
                    let num_smaller = node_to_num_smaller
                        .get_mut(&larger)
                        .expect("`node_to_num_smaller` contains all nodes");
                    *num_smaller = num_smaller
                        .checked_sub(1)
                        .expect("the larger of a smaller must have at least one smaller");
                    if *num_smaller == 0 {
                        leaves.push(larger);
                        non_leaves.remove(&larger);
                    }
                }
                sorted_chars.push(smaller);
            }

            // There could be pending nodes left because of a cycle.
            // We break the cycle at the largest node.
            let smaller = match non_leaves.last() {
                None => break,
                Some(child) => *child,
            };
            let larger = node_to_larger.remove(&smaller).expect(
                "General graph fact: sum_(node)#in_edges(node)=sum_(node)#out_edges(node).
                Fact about the next larger graph: #out_edges(node)<=1, because each char has at most one next larger char.
                If #out_edge(child)=0 then sum_(node)#in_edges(node)=sum_(node)#out_edges(node) < #nodes.
                Thus there exists another node with #in_edges(node)=0 and that node is a leaf.
                But `leaves.len()=0` at this line of code",
            );
            infinite_loop_warnings.push(NextLargerProgramWarning::InfiniteLoop {
                original: smaller,
                next_larger: larger,
            });
            leaves.push(larger);
            non_leaves.remove(&larger);
        }
        warnings.extend(infinite_loop_warnings.into_iter().rev());

        let next_larger = {
            let parents: HashSet<Char> = node_to_larger.values().copied().collect();
            let mut node_to_position = HashMap::<Char, u8>::new();
            let mut next_larger: Vec<(Char, NonZeroU8)> = vec![];

            for c in sorted_chars.iter().rev() {
                if !parents.contains(c) {
                    continue;
                }
                // The present character is the parent of at least one child, aka it is the next larger
                // character for another character. So it needs to be in the next_larger array.
                let child_position: u8 = next_larger
                    .len()
                    .try_into()
                    .expect("there are at most u8::MAX chars in the `next_larger` array");
                let offset = match node_to_larger.get(c) {
                    // The next_larger array contains at most 256 elements: one for each char.
                    // (Actually it contains at most 255 because one character necessarily does not
                    // have a child node and this character does not appear in the array.)
                    // Anyway, an offset of 256 sends the index outside the array bound, and so
                    // subsequent calls to iterator return None.
                    None => NonZeroU8::MAX,
                    Some(parent) => {
                        let parent_position = *node_to_position
                            .get(parent)
                            .expect("parent has already been inserted");
                        child_position
                            .checked_sub(parent_position)
                            .expect("parent inserted before so its position it is strictly smaller")
                            .try_into()
                            .expect("parent inserted before so its position it is strictly smaller")
                    }
                };
                next_larger.push((*c, offset));
                node_to_position.insert(*c, child_position);
            }
            next_larger.reverse();
            next_larger
        };

        let entrypoints = {
            let node_to_position: HashMap<Char, u8> = next_larger
                .iter()
                .enumerate()
                .map(|(i, (c, _))| {
                    let u: u8 = i
                        .try_into()
                        .expect("there are at most u8::MAX chars in the `next_larger` array");
                    (*c, u)
                })
                .collect();
            let mut entrypoints = HashMap::<Char, u8>::new();
            for c in sorted_chars.iter().rev() {
                if let Some(parent) = node_to_larger.get(c) {
                    entrypoints.insert(
                        *c,
                        *node_to_position
                            .get(parent)
                            .expect("parent has already been inserted"),
                    );
                }
            }
            entrypoints
        };

        (
            Self {
                entrypoints,
                next_larger,
            },
            warnings,
        )
    }

    /// Get the next larger sequence for a character
    pub fn get(&self, c: Char) -> impl Iterator<Item = Char> + '_ {
        NextLargerProgramIter {
            current: self.entrypoints.get(&c).copied(),
            program: self,
        }
    }

    /// Returns whether this program is seven-bit safe.
    ///
    /// A next larger program is seven-bit safe if the next larger sequences for
    ///     seven-bit characters only contain seven-bit characters.
    /// Conversely a program is seven-bit unsafe if there is a seven-bit
    ///     character whose next larger sequence contains a non-seven-bit character.
    ///
    /// ```
    /// # use tfm::*;
    /// let edges = vec![
    ///     (Char(250), Char(125)),
    ///     (Char(125), Char(126)),
    /// ];
    /// let (next_larger_program, _) = NextLargerProgram::new(edges.into_iter(), |_| true, true);
    /// assert_eq!(true, next_larger_program.is_seven_bit_safe());
    ///
    /// let edges = vec![
    ///     (Char(125), Char(250)),
    /// ];
    /// let (next_larger_program, _) = NextLargerProgram::new(edges.into_iter(), |_| true, true);
    /// assert_eq!(false, next_larger_program.is_seven_bit_safe());
    /// ```
    pub fn is_seven_bit_safe(&self) -> bool {
        // For each c, we only need to check the first element in c's next larger sequence.
        // If there is a subsequent element d of the sequence that is seven-bit unsafe,
        // we will find it when considering one of d's children.
        // This optimization makes this function O(n), rather than worst case O(n^2).
        self.entrypoints
            .keys()
            .copied()
            .filter(Char::is_seven_bit)
            .filter_map(|c| self.get(c).next())
            .all(|c| c.is_seven_bit())
    }
}

#[derive(Clone, Debug)]
struct NextLargerProgramIter<'a> {
    current: Option<u8>,
    program: &'a NextLargerProgram,
}

impl<'a> Iterator for NextLargerProgramIter<'a> {
    type Item = Char;

    fn next(&mut self) -> Option<Self::Item> {
        // Note that the iterator is statically guaranteed to terminate!
        // If it returns None, it has already terminated.
        // If it returns Some, then self.current will be incremented by a strictly
        //  positive number.
        // Incrementing like this can only happen a finite number of times before overflow
        //  occurs, and then self.current is None and the iterator is terminated.
        match self.current {
            None => None,
            Some(current) => match self.program.next_larger.get(current as usize) {
                None => None,
                Some((c, inc)) => {
                    self.current = current.checked_add(inc.get());
                    Some(*c)
                }
            },
        }
    }
}

impl core::FontFormat for Font {
    const DEFAULT_FILE_EXTENSION: &'static str = "tfm";
    type Error = format::DeserializationError;

    fn parse(b: &[u8]) -> Result<Self, Self::Error> {
        Font::deserialize_from_tfm(b)
    }
}

/// Fixed-width numeric type used in TFM files.
///
/// This numeric type has 11 bits for the integer part,
/// 20 bits for the fractional part, and a single signed bit.
/// The inner value is the number multiplied by 2^20.
/// It is called a `fix_word` in TFtoPL.
///
/// In property list files, this type is represented as a decimal number
///   with up to 6 digits after the decimal point.
/// This is a non-lossy representation
///   because 10^(-6) is larger than 2^(-20).
#[derive(Default, PartialEq, Eq, Debug, Copy, Clone, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct FixWord(pub i32);

impl FixWord {
    /// Representation of the number 0 as a [FixWord].
    pub const ZERO: FixWord = FixWord(0);

    /// Representation of the number 1 as a [FixWord].
    pub const ONE: FixWord = FixWord(1 << 20);

    /// Returns true if the number is less than 16.0 in magnitude according to Knuth.
    ///
    /// The number +16.0 is not allowed.
    /// This is covered in the E2E tests.
    /// See `check_fix` in TFtoPL.2014.60.
    pub fn is_abs_less_than_16(&self) -> bool {
        *self >= FixWord::ONE * -16 && *self < FixWord::ONE * 16
    }
}

impl std::fmt::Display for FixWord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TFtoPL.2014.40-43
        if self.0 < 0 {
            write!(f, "-")?;
        }
        let integer_part = (self.0 / FixWord::ONE.0).abs();
        write!(f, "{integer_part}.")?;
        let mut fp = (self.0 % FixWord::ONE.0).abs();
        fp = 10 * fp + 5;
        let mut delta = 10;
        loop {
            if delta > 0o4_000_000 {
                fp = fp + 0o2_000_000 - delta / 2;
            }
            write!(f, "{}", fp / 0o4_000_000)?;
            fp = 10 * (fp % 0o4_000_000);
            delta *= 10;
            if fp <= delta {
                break;
            }
        }
        Ok(())
    }
}

impl std::ops::Add<FixWord> for FixWord {
    type Output = FixWord;
    fn add(self, rhs: FixWord) -> Self::Output {
        FixWord(self.0 + rhs.0)
    }
}
impl std::ops::Sub<FixWord> for FixWord {
    type Output = FixWord;
    fn sub(self, rhs: FixWord) -> Self::Output {
        FixWord(self.0 - rhs.0)
    }
}

impl std::ops::Mul<i32> for FixWord {
    type Output = FixWord;

    fn mul(self, rhs: i32) -> Self::Output {
        FixWord(self.0 * rhs)
    }
}

impl std::ops::Div<i32> for FixWord {
    type Output = FixWord;

    fn div(self, rhs: i32) -> Self::Output {
        FixWord(self.0 / rhs)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod next_larger_tests {
        use super::*;

        fn run(
            edges: Vec<(Char, Char)>,
            want_sequences: HashMap<Char, Vec<Char>>,
            want_warnings: Vec<NextLargerProgramWarning>,
        ) {
            let (program, got_warnings) = NextLargerProgram::new(edges.into_iter(), |_| true, true);
            assert_eq!(got_warnings, want_warnings);
            for u in 0..=u8::MAX {
                let want_sequence = want_sequences
                    .get(&Char(u))
                    .map(Vec::as_slice)
                    .unwrap_or_default();
                let got_sequence: Vec<Char> = program.get(Char(u)).collect();
                assert_eq!(
                    got_sequence, want_sequence,
                    "got/want sequences for {u} do not match"
                );
            }
        }

        fn big_infinite_loop_edges() -> Vec<(Char, Char)> {
            (0..=u8::MAX)
                .into_iter()
                .map(|u| (Char(u), Char(u.wrapping_add(1))))
                .collect()
        }

        fn big_infinite_loop_sequences() -> HashMap<Char, Vec<Char>> {
            (0..=u8::MAX)
                .into_iter()
                .map(|u| {
                    let v: Vec<Char> = match u.checked_add(1) {
                        None => vec![],
                        Some(w) => (w..=u8::MAX).into_iter().map(Char).collect(),
                    };
                    (Char(u), v)
                })
                .collect()
        }

        macro_rules! next_larger_tests {
            ( $( ($name: ident, $edges: expr, $want_sequences: expr, $want_warnings: expr, ), )+ ) => {
                $(
                    #[test]
                    fn $name () {
                        run($edges, $want_sequences, $want_warnings);
                    }
                )+
            };
        }

        next_larger_tests!(
            (
                same_node_loop,
                vec![(Char::A, Char::A)],
                HashMap::from([(Char::A, vec![])]),
                vec![NextLargerProgramWarning::InfiniteLoop {
                    original: Char::A,
                    next_larger: Char::A,
                }],
            ),
            (
                two_loops,
                vec![
                    (Char::A, Char::B),
                    (Char::B, Char::C),
                    (Char::C, Char::B),
                    (Char::X, Char::Y),
                    (Char::Y, Char::Z),
                    (Char::Z, Char::X),
                ],
                HashMap::from([
                    (Char::A, vec![Char::B, Char::C]),
                    (Char::B, vec![Char::C]),
                    (Char::C, vec![]),
                    (Char::X, vec![Char::Y, Char::Z]),
                    (Char::Y, vec![Char::Z]),
                    (Char::Z, vec![]),
                ]),
                vec![
                    NextLargerProgramWarning::InfiniteLoop {
                        original: Char::C,
                        next_larger: Char::B,
                    },
                    NextLargerProgramWarning::InfiniteLoop {
                        original: Char::Z,
                        next_larger: Char::X,
                    },
                ],
            ),
            (
                path_leading_to_loop,
                vec![(Char::A, Char::B), (Char::B, Char::C), (Char::C, Char::B),],
                HashMap::from([
                    (Char::A, vec![Char::B, Char::C]),
                    (Char::B, vec![Char::C]),
                    (Char::C, vec![]),
                ]),
                vec![NextLargerProgramWarning::InfiniteLoop {
                    original: Char::C,
                    next_larger: Char::B,
                }],
            ),
            (
                big_infinite_loop,
                big_infinite_loop_edges(),
                big_infinite_loop_sequences(),
                vec![NextLargerProgramWarning::InfiniteLoop {
                    original: Char(u8::MAX),
                    next_larger: Char(0),
                }],
            ),
        );
    }
}
