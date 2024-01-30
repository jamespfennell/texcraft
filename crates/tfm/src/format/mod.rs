//! The TeX font metric (.tfm) file format

mod deserialize;
mod serialize;

use super::*;

pub use deserialize::Error as DeserializeError;
pub use deserialize::Warning as DeserializeWarning;

/// Complete contents of a TeX font metric (.tfm) file.
///
/// The struct contain multiple vectors.
/// In TeX and TFtoPL there is an optimization in which all of data in the vectors
/// is stored in one large vector of 32-bit integers.
/// The conversion from [u32] to the specific types like [Number] are then done when the
/// data is needed.
/// This makes the memory footprint of this type much more compact,
///     and such a change may be considered in the future.
///
/// In fact in TeX the font data for all fonts is stored in one contiguous piece of memory
///     (`font_info`, defined in TeX82.2021.549).
/// This is a little too unsafe to pull off though.
#[derive(Debug, PartialEq, Eq)]
pub struct File {
    /// Header.
    pub header: Header,

    /// The smallest character in the font.
    pub smallest_char_code: Char,

    /// Character infos.
    ///
    /// The char infos mostly contain indices for other vectors in this struct.
    pub char_infos: Vec<CharInfo>,

    /// Character widths
    pub widths: Vec<Number>,

    /// Character heights
    pub heights: Vec<Number>,

    /// Character depths
    pub depths: Vec<Number>,

    /// Character italic corrections
    pub italic_corrections: Vec<Number>,

    /// Lig kern instructions.
    pub lig_kern_instructions: Vec<ligkern::lang::Instruction>,

    /// Kerns. These are referenced from inside the lig kern commands.
    pub kern: Vec<Number>,

    /// Extensible characters.
    pub extensible_chars: Vec<ExtensibleRecipe>,

    /// Font parameters.
    pub params: Params,
}

impl Default for File {
    fn default() -> Self {
        Self {
            header: Default::default(),
            smallest_char_code: Default::default(),
            char_infos: vec![],
            widths: vec![Number::ZERO],
            heights: vec![Number::ZERO],
            depths: vec![Number::ZERO],
            italic_corrections: vec![Number::ZERO],
            lig_kern_instructions: vec![],
            kern: vec![],
            extensible_chars: vec![],
            params: Default::default(),
        }
    }
}

impl File {
    pub fn deserialize(b: &[u8]) -> Result<(File, Vec<DeserializeWarning>), DeserializeError> {
        deserialize::deserialize(b)
    }
}

/// Data about one character in a .tfm file.
#[derive(Debug, PartialEq, Eq)]
pub struct CharInfo {
    pub width_index: u8,
    pub height_index: u8,
    pub depth_index: u8,
    pub italic_index: u8,
    pub tag: CharTag,
}

/// Tag of a character in a .tfm file.
#[derive(Debug, Default, PartialEq, Eq)]
pub enum CharTag {
    #[default]
    None,
    Ligature(u8),
    List(Char),
    Extension(u8),
}

/// Extensible recipe instruction in a .tfm file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExtensibleRecipe {
    pub top: Option<Char>,
    pub middle: Option<Char>,
    pub bottom: Option<Char>,
    pub rep: Char,
}
