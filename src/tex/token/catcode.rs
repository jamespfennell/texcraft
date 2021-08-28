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
use RawCatCode::*;

// Exercise 7.3 in the TeX book
/// Enum representing all 11 category codes that can be returned by the lexer.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum CatCode {
    BeginGroup,
    EndGroup,
    MathShift,
    AlignmentTab,
    Parameter,
    Superscript,
    Subscript,
    Space,
    Letter,
    Other,
    Active,
}

impl std::default::Default for CatCode {
    fn default() -> Self {
        CatCode::Other
    }
}

impl CatCode {
    pub fn int(&self) -> u8 {
        match self {
            BeginGroup => 1,
            EndGroup => 2,
            MathShift => 3,
            AlignmentTab => 4,
            Parameter => 6,
            Superscript => 7,
            Subscript => 8,
            Space => 10,
            Letter => 11,
            Other => 12,
            Active => 13,
        }
    }

    pub fn to_str(&self) -> &str {
        match self {
            BeginGroup => "begin group",
            EndGroup => "end group",
            MathShift => "math shift",
            AlignmentTab => "alignment tab",
            Parameter => "parameter",
            Superscript => "superscript",
            Subscript => "subscript",
            Space => "space",
            Letter => "letter",
            Other => "other",
            Active => "active",
        }
    }

    pub fn from_int(int: u8) -> Option<CatCode> {
        match int {
            1 => Some(BeginGroup),
            2 => Some(EndGroup),
            3 => Some(MathShift),
            4 => Some(AlignmentTab),
            6 => Some(Parameter),
            7 => Some(Superscript),
            8 => Some(Subscript),
            10 => Some(Space),
            11 => Some(Letter),
            12 => Some(Other),
            13 => Some(Active),
            _ => None,
        }
    }
}

impl std::fmt::Display for CatCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ({})", self.int(), self.to_str())?;
        Ok(())
    }
}

/// Enum representing all 16 category codes in TeX.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum RawCatCode {
    Regular(CatCode),
    Escape,
    EndOfLine,
    Ignored,
    Comment,
    Invalid,
}

impl From<CatCode> for RawCatCode {
    fn from(cat_code: CatCode) -> Self {
        RawCatCode::Regular(cat_code)
    }
}

impl std::default::Default for RawCatCode {
    fn default() -> Self {
        RawCatCode::Regular(CatCode::default())
    }
}

impl RawCatCode {
    pub fn int(&self) -> u8 {
        match self {
            Regular(cat_code) => cat_code.int(),
            Escape => 0,
            EndOfLine => 5,
            Ignored => 9,
            Comment => 14,
            Invalid => 15,
        }
    }

    pub fn from_int(int: u8) -> Option<RawCatCode> {
        match int {
            0 => Some(Escape),
            5 => Some(EndOfLine),
            9 => Some(Ignored),
            14 => Some(Comment),
            15 => Some(Invalid),
            int => CatCode::from_int(int).map(RawCatCode::from),
        }
    }
}

pub fn or_default(c: Option<&RawCatCode>) -> RawCatCode {
    match c {
        None => Regular(Other),
        Some(&c) => c,
    }
}

pub fn tex_defaults() -> HashMap<u32, RawCatCode> {
    let mut cat_code_map = HashMap::new();
    set_tex_defaults(&mut cat_code_map);
    cat_code_map
}

pub fn set_tex_defaults(cat_code_map: &mut HashMap<u32, RawCatCode>) {
    cat_code_map.extend(std::array::IntoIter::new([
        ('\\' as u32, Escape),
        ('{' as u32, Regular(BeginGroup)),
        ('}' as u32, Regular(EndGroup)),
        ('$' as u32, Regular(MathShift)),
        ('&' as u32, Regular(AlignmentTab)),
        ('\n' as u32, EndOfLine),
        ('#' as u32, Regular(Parameter)),
        ('^' as u32, Regular(Superscript)),
        ('_' as u32, Regular(Subscript)),
        ('~' as u32 as u32, Regular(Active)),
        ('%' as u32, Comment),
        //
        (' ' as u32, Regular(Space)), // TODO: other white space characters?
        //
        ('A' as u32, Regular(Letter)),
        ('B' as u32, Regular(Letter)),
        ('C' as u32, Regular(Letter)),
        ('D' as u32, Regular(Letter)),
        ('E' as u32, Regular(Letter)),
        ('F' as u32, Regular(Letter)),
        ('G' as u32, Regular(Letter)),
        ('H' as u32, Regular(Letter)),
        ('I' as u32, Regular(Letter)),
        ('J' as u32, Regular(Letter)),
        ('K' as u32, Regular(Letter)),
        ('L' as u32, Regular(Letter)),
        ('M' as u32, Regular(Letter)),
        ('N' as u32, Regular(Letter)),
        ('O' as u32, Regular(Letter)),
        ('P' as u32, Regular(Letter)),
        ('Q' as u32, Regular(Letter)),
        ('R' as u32, Regular(Letter)),
        ('S' as u32, Regular(Letter)),
        ('T' as u32, Regular(Letter)),
        ('U' as u32, Regular(Letter)),
        ('V' as u32, Regular(Letter)),
        ('W' as u32, Regular(Letter)),
        ('X' as u32, Regular(Letter)),
        ('Y' as u32, Regular(Letter)),
        ('Z' as u32, Regular(Letter)),
        //
        ('a' as u32, Regular(Letter)),
        ('b' as u32, Regular(Letter)),
        ('c' as u32, Regular(Letter)),
        ('d' as u32, Regular(Letter)),
        ('e' as u32, Regular(Letter)),
        ('f' as u32, Regular(Letter)),
        ('g' as u32, Regular(Letter)),
        ('h' as u32, Regular(Letter)),
        ('i' as u32, Regular(Letter)),
        ('j' as u32, Regular(Letter)),
        ('k' as u32, Regular(Letter)),
        ('l' as u32, Regular(Letter)),
        ('m' as u32, Regular(Letter)),
        ('n' as u32, Regular(Letter)),
        ('o' as u32, Regular(Letter)),
        ('p' as u32, Regular(Letter)),
        ('q' as u32, Regular(Letter)),
        ('r' as u32, Regular(Letter)),
        ('s' as u32, Regular(Letter)),
        ('t' as u32, Regular(Letter)),
        ('u' as u32, Regular(Letter)),
        ('v' as u32, Regular(Letter)),
        ('w' as u32, Regular(Letter)),
        ('x' as u32, Regular(Letter)),
        ('y' as u32, Regular(Letter)),
        ('z' as u32, Regular(Letter)),
    ]))
}

#[cfg(test)]
mod tests {
    use crate::tex::token::catcode::RawCatCode::Regular;
    use crate::tex::token::catcode::{CatCode, RawCatCode};

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
    fn serialize_and_deserialize_raw_cat_code() {
        for cat_code in all_cat_codes() {
            assert_eq!(CatCode::from_int(cat_code.int()), Some(cat_code))
        }
    }

    #[test]
    fn serialize_and_deserialize_cat_code() {
        let mut all_raw_cat_codes = vec![
            RawCatCode::Escape,
            RawCatCode::EndOfLine,
            RawCatCode::Ignored,
            RawCatCode::Comment,
            RawCatCode::Invalid,
        ];
        for cat_code in all_cat_codes() {
            all_raw_cat_codes.push(Regular(cat_code))
        }
        for cat_code in all_raw_cat_codes {
            assert_eq!(RawCatCode::from_int(cat_code.int()), Some(cat_code))
        }
    }
}
