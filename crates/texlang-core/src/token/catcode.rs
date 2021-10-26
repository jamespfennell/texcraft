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

pub fn tex_defaults() -> HashMap<u32, CatCode> {
    let mut cat_code_map = HashMap::new();
    set_tex_defaults(&mut cat_code_map);
    cat_code_map
}

pub fn set_tex_defaults(cat_code_map: &mut HashMap<u32, CatCode>) {
    cat_code_map.extend(std::array::IntoIter::new([
        ('\\' as u32, Escape),
        ('{' as u32, BeginGroup),
        ('}' as u32, EndGroup),
        ('$' as u32, MathShift),
        ('&' as u32, AlignmentTab),
        ('\n' as u32, EndOfLine),
        ('#' as u32, Parameter),
        ('^' as u32, Superscript),
        ('_' as u32, Subscript),
        ('~' as u32 as u32, Active),
        ('%' as u32, Comment),
        //
        (' ' as u32, Space),
        //
        ('A' as u32, Letter),
        ('B' as u32, Letter),
        ('C' as u32, Letter),
        ('D' as u32, Letter),
        ('E' as u32, Letter),
        ('F' as u32, Letter),
        ('G' as u32, Letter),
        ('H' as u32, Letter),
        ('I' as u32, Letter),
        ('J' as u32, Letter),
        ('K' as u32, Letter),
        ('L' as u32, Letter),
        ('M' as u32, Letter),
        ('N' as u32, Letter),
        ('O' as u32, Letter),
        ('P' as u32, Letter),
        ('Q' as u32, Letter),
        ('R' as u32, Letter),
        ('S' as u32, Letter),
        ('T' as u32, Letter),
        ('U' as u32, Letter),
        ('V' as u32, Letter),
        ('W' as u32, Letter),
        ('X' as u32, Letter),
        ('Y' as u32, Letter),
        ('Z' as u32, Letter),
        //
        ('a' as u32, Letter),
        ('b' as u32, Letter),
        ('c' as u32, Letter),
        ('d' as u32, Letter),
        ('e' as u32, Letter),
        ('f' as u32, Letter),
        ('g' as u32, Letter),
        ('h' as u32, Letter),
        ('i' as u32, Letter),
        ('j' as u32, Letter),
        ('k' as u32, Letter),
        ('l' as u32, Letter),
        ('m' as u32, Letter),
        ('n' as u32, Letter),
        ('o' as u32, Letter),
        ('p' as u32, Letter),
        ('q' as u32, Letter),
        ('r' as u32, Letter),
        ('s' as u32, Letter),
        ('t' as u32, Letter),
        ('u' as u32, Letter),
        ('v' as u32, Letter),
        ('w' as u32, Letter),
        ('x' as u32, Letter),
        ('y' as u32, Letter),
        ('z' as u32, Letter),
    ]))
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
