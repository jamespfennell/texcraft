//! Data structures representing category codes and operations on them.
use std::collections::HashMap;

use CatCode::*;

/// Enum representing all 16 category codes in TeX.
///
/// Each variant's documentation contains an example character which is mapped to that category code in plainTeX.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Default)]
pub enum CatCode {
    /// Marks the beginning of a control sequence.
    /// Example: `\`.
    ///
    /// This category code is never seen outside of the lexer.
    Escape = 0,
    /// Begins a new group.
    /// Example: `{`.
    BeginGroup = 1,
    /// Ends an existing new group.
    /// Example: `}`.
    EndGroup = 2,
    /// Starts or ends math mode.
    /// Example: `$`.
    MathShift = 3,
    /// Used in typesetting tables to align cells.
    /// Example: `&`.
    AlignmentTab = 4,
    /// Marks a new line in the input.
    /// Example: `\n`.
    ///
    /// This code behaves similarly to [Space], but has two additional properties.
    /// First, two or more consecutive new lines, modulo intervening [Space] characters, create a `\par` control sequence
    ///     instead of a regular space.
    /// Second, this code terminates a comment that started with a [Comment] character.
    ///
    /// This category code is never seen outside of the lexer.
    EndOfLine = 5,
    /// Marks the beginning of a parameter number.
    /// It must generally be followed by a digit.
    /// Example: `#`.
    Parameter = 6,
    /// Puts following character or group in a superscript.
    /// Example: `^`.
    Superscript = 7,
    /// Puts following character or group in a subscript.
    /// Example: `_`.
    Subscript = 8,
    /// Character that is ignored by the lexer.
    /// Example: ASCII null (0).
    ///
    /// This category code is never seen outside of the lexer.
    Ignored = 9,
    /// Whitespace. Example: ` `.
    Space = 10,
    /// A character that can be used as a control sequence name.
    /// Examples: `[a-zA-z]`.
    Letter = 11,
    /// A character than cannot be used as a control sequence name.
    /// Example: `@`.
    #[default]
    Other = 12,
    /// A single character that behaves like a control sequence.
    /// Example: `~`.
    Active = 13,
    /// Marks the beginning of a comment.
    /// All characters until the next [EndOfLine] are ignored.
    /// Example: `%`.
    ///
    /// This category code is never seen outside of the lexer.
    Comment = 14,
    /// An invalid character.
    /// If this is encountered in the input, the lexer will return an error.
    /// Example: ASCII delete (127).
    ///
    /// This category code is never seen outside of the lexer.
    Invalid = 15,
}

impl TryFrom<u8> for CatCode {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Escape),
            1 => Ok(BeginGroup),
            2 => Ok(EndGroup),
            3 => Ok(MathShift),
            4 => Ok(AlignmentTab),
            5 => Ok(EndOfLine),
            6 => Ok(Parameter),
            7 => Ok(Superscript),
            8 => Ok(Subscript),
            9 => Ok(Ignored),
            10 => Ok(Space),
            11 => Ok(Letter),
            12 => Ok(Other),
            13 => Ok(Active),
            14 => Ok(Comment),
            15 => Ok(Invalid),
            _ => Err(()),
        }
    }
}

impl std::fmt::Display for CatCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} ({})", self, *self as u8)?;
        Ok(())
    }
}

impl CatCode {
    /// Default category codes in INITEX for all ASCII characters.
    ///
    /// To find the category code for an ASCII character,
    ///     convert it to an integer and use it as an index for the array.
    ///
    /// This list was compiled by reading the source code TeX '82,
    ///     specifically section 232 in the "TeX: the program".
    /// These defaults are also described in the TeXBook p343.
    pub const INITEX_DEFAULTS: [CatCode; 128] = [
        Ignored, // ASCII null
        Other, Other, Other, Other, Other, Other, Other, Other, Other,
        EndOfLine, // carriage return
        Other, Other, Other, Other, Other, Other, Other, Other, Other, Other, Other, Other, Other,
        Other, Other, Other, Other, Other, Other, Other, Other, Space, // space
        Other, Other, Other, Other, Comment, // %
        Other, Other, Other, Other, Other, Other, Other, Other, Other, Other, Other, Other, Other,
        Other, Other, Other, Other, Other, Other, Other, Other, Other, Other, Other, Other, Other,
        Other, Letter, // A
        Letter, // B
        Letter, // C
        Letter, // D
        Letter, // E
        Letter, // F
        Letter, // G
        Letter, // H
        Letter, // I
        Letter, // J
        Letter, // K
        Letter, // L
        Letter, // M
        Letter, // N
        Letter, // O
        Letter, // P
        Letter, // Q
        Letter, // R
        Letter, // S
        Letter, // T
        Letter, // U
        Letter, // V
        Letter, // W
        Letter, // X
        Letter, // Y
        Letter, // Z
        Other, Escape, // \
        Other, Other, Other, Other, Letter, // a
        Letter, // b
        Letter, // c
        Letter, // d
        Letter, // e
        Letter, // f
        Letter, // g
        Letter, // h
        Letter, // i
        Letter, // j
        Letter, // k
        Letter, // l
        Letter, // m
        Letter, // n
        Letter, // o
        Letter, // p
        Letter, // q
        Letter, // r
        Letter, // s
        Letter, // t
        Letter, // u
        Letter, // v
        Letter, // w
        Letter, // x
        Letter, // y
        Letter, // z
        Other, Other, Other, Other, Invalid, // ASCII delete
    ];

    /// Default category codes in plainTeX for all ASCII characters.
    ///
    /// To find the category code for an ASCII character,
    ///     convert it to an integer and use it as an index for the array.
    ///
    /// This list was compiled by starting with [INITEX_DEFAULTS](CatCode::INITEX_DEFAULTS) and then applying
    ///     all category code changes in the plainTeX format.
    /// These changes are described on p343 of the TeXBook.
    pub const PLAIN_TEX_DEFAULTS: [CatCode; 128] = [
        Ignored, // ASCII null
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Space,     // horizontal tab
        EndOfLine, // carriage return
        Other,
        Active, // ASCII form-feed
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Space, // space
        Other,
        Other,
        Parameter,    // #
        MathShift,    // $
        Comment,      // %
        AlignmentTab, // &
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Other,
        Letter, // A
        Letter, // B
        Letter, // C
        Letter, // D
        Letter, // E
        Letter, // F
        Letter, // G
        Letter, // H
        Letter, // I
        Letter, // J
        Letter, // K
        Letter, // L
        Letter, // M
        Letter, // N
        Letter, // O
        Letter, // P
        Letter, // Q
        Letter, // R
        Letter, // S
        Letter, // T
        Letter, // U
        Letter, // V
        Letter, // W
        Letter, // X
        Letter, // Y
        Letter, // Z
        Other,
        Escape, // \
        Other,
        Superscript, // ^
        Subscript,   // _
        Other,
        Letter,     // a
        Letter,     // b
        Letter,     // c
        Letter,     // d
        Letter,     // e
        Letter,     // f
        Letter,     // g
        Letter,     // h
        Letter,     // i
        Letter,     // j
        Letter,     // k
        Letter,     // l
        Letter,     // m
        Letter,     // n
        Letter,     // o
        Letter,     // p
        Letter,     // q
        Letter,     // r
        Letter,     // s
        Letter,     // t
        Letter,     // u
        Letter,     // v
        Letter,     // w
        Letter,     // x
        Letter,     // y
        Letter,     // z
        BeginGroup, // {
        Other,
        EndGroup, // }
        Active,   // ~
        Invalid,  // ASCII delete
    ];
}

// TODO: should this just be called Map?
pub struct CatCodeMap {
    low: [CatCode; 128],
    high: HashMap<char, CatCode>,
    default: CatCode,
}

impl CatCodeMap {
    pub fn new() -> CatCodeMap {
        CatCodeMap {
            low: [Default::default(); 128],
            high: HashMap::new(),
            default: CatCode::default(),
        }
    }

    pub fn new_with_tex_defaults() -> CatCodeMap {
        let mut cat_code_map = CatCodeMap::new();
        set_tex_defaults(&mut cat_code_map);
        cat_code_map
    }

    #[inline]
    pub fn get(&self, c: &char) -> &CatCode {
        if (*c as u32) < 128 {
            self.low.get(*c as usize).unwrap()
        } else {
            self.high.get(c).unwrap_or(&self.default)
        }
    }

    #[inline]
    pub fn get_mut(&mut self, c: &char) -> &mut CatCode {
        if (*c as u32) < 128 {
            self.low.get_mut(*c as usize).unwrap()
        } else {
            self.high.entry(*c).or_insert_with(|| CatCode::Other)
        }
    }

    pub fn insert(&mut self, c: char, code: CatCode) {
        if (c as u32) < 128 {
            self.low[c as usize] = code
        } else {
            self.high.insert(c, code);
        }
    }
}

impl Default for CatCodeMap {
    fn default() -> Self {
        Self::new()
    }
}

fn set_tex_defaults(cat_code_map: &mut CatCodeMap) {
    let mut u: u8 = 0;
    for code in CatCode::PLAIN_TEX_DEFAULTS {
        let c: char = u.try_into().unwrap();
        cat_code_map.insert(c, code);
        u = u.wrapping_add(1);
    }
}

#[cfg(test)]
mod tests {
    use crate::token::catcode::CatCode;

    #[test]
    fn serialize_and_deserialize_cat_code() {
        let all_raw_cat_codes = vec![
            CatCode::BeginGroup,
            CatCode::EndGroup,
            CatCode::MathShift,
            CatCode::AlignmentTab,
            CatCode::Parameter,
            CatCode::Superscript,
            CatCode::Subscript,
            CatCode::Space,
            CatCode::Letter,
            CatCode::Other,
            CatCode::Active,
            CatCode::Escape,
            CatCode::EndOfLine,
            CatCode::Ignored,
            CatCode::Comment,
            CatCode::Invalid,
        ];
        for cat_code in all_raw_cat_codes {
            let u: u8 = cat_code as u8;
            let recovered: CatCode = u.try_into().unwrap();
            assert_eq!(recovered, cat_code);
        }
    }
}
