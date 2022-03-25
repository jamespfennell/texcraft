//! Data structures representing category codes and operations on them.
//!
//! The following table lists all 16 category codes in TeX. Names marked with * are never
//! returned from the lexer; instead, they are transformed into other category codes
//! or ignored.
//!
//! | name           | #  | e.g. | description |
//! |----------------|----|------|-------------|
//! | `Escape`*      | 0  | `\`  | Denotes the beginning of a control sequence.
//! | `BeginGroup`   | 1  | `{`  | Starts a new group/scope.
//! | `EndGroup`     | 2  | `}`  | Ends an existing new group/scope.
//! | `MathShift`    | 3  |      |
//! | `AlignmentTab` | 4  |      |
//! | `EndOfLine`*   | 5  | `\n` | New line in the input. Two consecutive new lines modulo whitespace create a `\par` control sequence.
//! | `Parameter`    | 6  | `#`  | Denotes the beginning of a parameter number; must generally be followed by a digit.
//! | `Superscript`  | 7  | `^`  | Puts following character or group in a superscript.
//! | `Subscript`    | 8  | `_`  | Puts following character or group in a subscript.
//! | `Ignored`*     | 9  |      | Ignored by the lexer.
//! | `Space`        | 10 | ` `  | Whitespace.
//! | `Letter`       | 11 | `A`  | A character that can be used as a control sequence name.
//! | `Other`        | 12 | `@`  | A character than cannot be used as a control sequence name.
//! | `Active`       | 13 |      |
//! | `Comment`*     | 14 | `%`  | Denotes the beginning of a comment; all remaining characters on the line will be ignored.
//! | `Invalid`*     | 15 |      | An invalid character; if this is read in the input, a error will fire.
//!
use std::collections::HashMap;

use CatCode::*;

/// Enum representing all 16 category codes in TeX.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum CatCode {
    Escape,
    BeginGroup,
    EndGroup,
    MathShift,
    AlignmentTab,
    EndOfLine,
    Parameter,
    Superscript,
    Subscript,
    Ignored,
    Space,
    Letter,
    Other,
    Active,
    Comment,
    Invalid,
}

impl CatCode {
    pub fn int(&self) -> u8 {
        match self {
            Escape => 0,
            BeginGroup => 1,
            EndGroup => 2,
            MathShift => 3,
            AlignmentTab => 4,
            EndOfLine => 5,
            Parameter => 6,
            Superscript => 7,
            Subscript => 8,
            Ignored => 9,
            Space => 10,
            Letter => 11,
            Other => 12,
            Active => 13,
            Comment => 14,
            Invalid => 15,
        }
    }

    pub fn from_int(int: u8) -> Option<CatCode> {
        match int {
            0 => Some(Escape),
            1 => Some(BeginGroup),
            2 => Some(EndGroup),
            3 => Some(MathShift),
            4 => Some(AlignmentTab),
            5 => Some(EndOfLine),
            6 => Some(Parameter),
            7 => Some(Superscript),
            8 => Some(Subscript),
            9 => Some(Ignored),
            10 => Some(Space),
            11 => Some(Letter),
            12 => Some(Other),
            13 => Some(Active),
            14 => Some(Comment),
            15 => Some(Invalid),
            _ => None,
        }
    }

    pub fn to_str(&self) -> &str {
        match self {
            Escape => "escape",
            BeginGroup => "begin group",
            EndGroup => "end group",
            MathShift => "math shift",
            AlignmentTab => "alignment tab",
            EndOfLine => "end of line",
            Parameter => "parameter",
            Superscript => "superscript",
            Subscript => "subscript",
            Ignored => "ignored",
            Space => "space",
            Letter => "letter",
            Other => "other",
            Active => "active",
            Comment => "comment",
            Invalid => "invalid",
        }
    }
}

impl std::default::Default for CatCode {
    fn default() -> Self {
        CatCode::Other
    }
}

impl std::fmt::Display for CatCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ({})", self.int(), self.to_str())?;
        Ok(())
    }
}

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
    let iter = [
        ('\\', Escape),
        ('{', BeginGroup),
        ('}', EndGroup),
        ('$', MathShift),
        ('&', AlignmentTab),
        ('\n', EndOfLine),
        ('#', Parameter),
        ('^', Superscript),
        ('_', Subscript),
        ('~', Active),
        ('%', Comment),
        //
        (' ', Space),
        //
        ('A', Letter),
        ('B', Letter),
        ('C', Letter),
        ('D', Letter),
        ('E', Letter),
        ('F', Letter),
        ('G', Letter),
        ('H', Letter),
        ('I', Letter),
        ('J', Letter),
        ('K', Letter),
        ('L', Letter),
        ('M', Letter),
        ('N', Letter),
        ('O', Letter),
        ('P', Letter),
        ('Q', Letter),
        ('R', Letter),
        ('S', Letter),
        ('T', Letter),
        ('U', Letter),
        ('V', Letter),
        ('W', Letter),
        ('X', Letter),
        ('Y', Letter),
        ('Z', Letter),
        //
        ('a', Letter),
        ('b', Letter),
        ('c', Letter),
        ('d', Letter),
        ('e', Letter),
        ('f', Letter),
        ('g', Letter),
        ('h', Letter),
        ('i', Letter),
        ('j', Letter),
        ('k', Letter),
        ('l', Letter),
        ('m', Letter),
        ('n', Letter),
        ('o', Letter),
        ('p', Letter),
        ('q', Letter),
        ('r', Letter),
        ('s', Letter),
        ('t', Letter),
        ('u', Letter),
        ('v', Letter),
        ('w', Letter),
        ('x', Letter),
        ('y', Letter),
        ('z', Letter),
    ]
    .into_iter();
    for (c, code) in iter {
        cat_code_map.insert(c, code);
    }
}

#[cfg(test)]
mod tests {
    use crate::token::catcode::CatCode;

    fn all_cat_codes() -> Vec<CatCode> {
        vec![
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
        ]
    }

    #[test]
    fn serialize_and_deserialize_cat_code() {
        let mut all_raw_cat_codes = vec![
            CatCode::Escape,
            CatCode::EndOfLine,
            CatCode::Ignored,
            CatCode::Comment,
            CatCode::Invalid,
        ];
        for cat_code in all_cat_codes() {
            all_raw_cat_codes.push(cat_code);
        }
        for cat_code in all_raw_cat_codes {
            assert_eq!(CatCode::from_int(cat_code.int()), Some(cat_code))
        }
    }
}
