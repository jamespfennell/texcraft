//! Parsers for the TeX font metric (.tfm) and property list (.pl) file formats

#[cfg(test)]
mod examples;
mod tfm;

pub use crate::tfm::{
    deserialize as deserialize_tfm, DeserializeError as DeserializeTfmError,
    DeserializeWarning as DeserializeTfmWarning, SubFileSizes as SubFileSizes,
};

/// Complete contents of a TeX font metric (.tfm) or property list (.pl) file.
///
/// The struct contain multiple vectors.
/// In TeX and TFtoPL there is an optimization in which all of data in the vectors
/// is stored in one large vector of 32-bit integers.
/// The conversion from [u32] to the specific types like [FixWord] are then done when the
/// data is needed.
/// This makes the memory footprint of this type much more compact,
///     and such a change may be considered in the future.
///
/// In fact in TeX the font data for all fonts is stored in one contiguous piece of memory
///     (`font_info`, defined in TeX82.2021.549).
/// This is a little too unsafe to pull off though.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct Font {
    /// Header.
    pub header: Header,

    /// The smallest character in the font.
    pub smallest_char_code: u8,

    /// Character infos.
    ///
    /// The char infos mostly contain indices for other vectors in this struct.
    pub char_infos: Vec<CharInfo>,

    /// Character widths
    pub widths: Vec<FixWord>,

    /// Character heights
    pub heights: Vec<FixWord>,

    /// Character depths
    pub depths: Vec<FixWord>,

    /// Character italic corrections
    pub italic_corrections: Vec<FixWord>,

    /// Lig kern commands.
    pub lig_kern_commands: Vec<LigKernCommand>,

    /// Kerns. These are referenced from inside the lig kern commands.
    pub kern: Vec<FixWord>,

    /// Extensible characters.
    pub extensible_chars: Vec<ExtensibleRecipe>,

    /// Font parameters.
    pub params: Params,
}

/// The TFM header, which contains metadata about the file.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct Header {
    /// In TeX82, this is stored in the `font_check` array (TeX82.2021.549).
    pub checksum: u32,
    /// In TeX82, this is stored in the `font_dsize` array (TeX82.2021.549).
    pub design_size: FixWord,
    pub character_coding_scheme: Option<StackString<39>>,
    pub font_family: Option<StackString<19>>,
    pub seven_bit_safe: Option<bool>,
    pub face: Option<Face>,
    /// The TFM format allows the header to contain arbitrary additional data.
    pub additional_data: Vec<u32>,
}

/// An ASCII string allocated on the stack.
///
/// The generic parameter is the maximum size of the string in bytes.
#[derive(PartialEq, Eq)]
pub struct StackString<const N: usize> {
    len: u8,
    data: [u8; N],
}

impl<const N: usize> Default for StackString<N> {
    fn default() -> Self {
        Self {
            len: 0,
            data: [0; N],
        }
    }
}

impl<const N: usize> std::fmt::Debug for StackString<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s: &str = self.as_ref();
        s.fmt(f)
    }
}

impl<const N: usize> AsRef<str> for StackString<N> {
    fn as_ref(&self) -> &str {
        std::str::from_utf8(&self.data[0..(self.len as usize)]).unwrap()
    }
}

impl<const N: usize> From<&str> for StackString<N> {
    fn from(value: &str) -> Self {
        let mut r: Self = Default::default();
        for c in value.chars() {
            if r.len as usize == N {
                return r;
            }
            r.data[r.len as usize] = match c {
                ' '..='~' => c,
                _ => '?',
            }
            .try_into()
            .unwrap();
            r.len += 1;
        }
        r
    }
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
#[derive(Default, PartialEq, Eq, Debug, Copy, Clone)]
pub struct FixWord(i32);

impl FixWord {
    /// Representation of the number 0 as a [FixWord].
    pub const ZERO: FixWord = FixWord(0);

    /// Representation of the number 1 as a [FixWord].
    pub const UNITY: FixWord = FixWord(1 << 20);
}

#[derive(Debug, PartialEq, Eq)]
pub struct CharInfo {
    pub width_index: u8,
    pub height_index: u8,
    pub depth_index: u8,
    pub italic_index: u8,
    pub tag: CharTag,
}

#[derive(Debug, PartialEq, Eq)]
pub enum CharTag {
    None,
    Ligature(u8),
    List(u8),
    Extension(u8),
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum FaceWeight {
    Light,
    Medium,
    Bold,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum FaceSlope {
    Roman,
    Italic,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum FaceExpansion {
    Regular,
    Condensed,
    Extended,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
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

/// We start with a current character
#[derive(Debug, PartialEq, Eq)]
pub struct LigKernCommand {
    /// Specifies the next command for the current character.
    /// Otherwise this is the final command.
    /// This field essentially makes the lig kern commands for a specific character into a linked list.
    pub next_command: Option<u8>,
    /// The operation is performed if `next_char` is the next character.
    /// Otherwise the next lig kern command for the current character is consulted, using the `next_command` field.
    ///
    /// After this operation is performed, no more operations need to be performed.
    /// This is because for any given pair of characters there is only one lig
    /// kern command for that pair.
    pub next_char: u8,
    /// The operation to perform.
    pub op: LigKernOp,
}

#[derive(Debug, PartialEq, Eq)]
pub enum LigKernOp {
    /// Insert a kern between the current character and the next character.
    /// The variant payload is the index of the kern in the kerns array.
    Kern(u16),
    /// Perform a ligature step.
    /// This means inserting `char_to_insert` between the current character and the next character,
    ///     potentially deleting one or both of these characters,
    ///     and then potentially moving the current character forward.
    Ligature {
        /// Character to insert.
        char_to_insert: u8,
        /// What to do after inserting the character.
        post_insert: PostInsert,
    },
}

// TODO: put all these in a ligkern namespace
#[derive(Debug, PartialEq, Eq)]
pub enum PostInsert {
    DeleteNoneMoveNowhere,
    DeleteNoneMoveToInserted,
    DeleteNoneMoveToNext,
    DeleteCurrentMoveToInserted,
    DeleteCurrentMoveToNext,
    DeleteNextMoveNowhere,
    DeleteNextMoveToInserted,
    DeleteBothMoveToInserted,
}

impl PostInsert {
    pub fn delete_current(&self) -> bool {
        match self {
            PostInsert::DeleteNoneMoveNowhere
            | PostInsert::DeleteNoneMoveToInserted
            | PostInsert::DeleteNoneMoveToNext
            | PostInsert::DeleteNextMoveNowhere
            | PostInsert::DeleteNextMoveToInserted => false,
            PostInsert::DeleteCurrentMoveToInserted
            | PostInsert::DeleteCurrentMoveToNext
            | PostInsert::DeleteBothMoveToInserted => true,
        }
    }
    pub fn delete_next(&self) -> bool {
        match self {
            PostInsert::DeleteNoneMoveNowhere
            | PostInsert::DeleteNoneMoveToInserted
            | PostInsert::DeleteNoneMoveToNext
            | PostInsert::DeleteCurrentMoveToInserted
            | PostInsert::DeleteCurrentMoveToNext => false,
            PostInsert::DeleteNextMoveNowhere
            | PostInsert::DeleteNextMoveToInserted
            | PostInsert::DeleteBothMoveToInserted => true,
        }
    }
    pub fn skip(&self) -> u8 {
        match self {
            PostInsert::DeleteNoneMoveNowhere
            | PostInsert::DeleteCurrentMoveToInserted
            | PostInsert::DeleteNextMoveNowhere => 0,
            PostInsert::DeleteBothMoveToInserted => 0,
            PostInsert::DeleteNoneMoveToInserted
            | PostInsert::DeleteCurrentMoveToNext
            | PostInsert::DeleteNextMoveToInserted => 1,
            PostInsert::DeleteNoneMoveToNext => 2,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ExtensibleRecipe {
    pub top: u8,
    pub middle: u8,
    pub bottom: u8,
    pub rep: u8,
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct Params {
    slant: FixWord,
    space: FixWord,
    space_stretch: FixWord,
    space_shrink: FixWord,
    x_height: FixWord,
    quad: FixWord,
    extra_space: FixWord,
    additional_params: Vec<FixWord>,
}
