//! Crate for working with TeX font metric (.tfm) and property list (.pl) formats

use std::panic;
pub mod format;
pub mod pl;

/// Complete contents of a TeX font metric (.tfm) or property list (.pl) file.
#[derive(Default)]
pub struct File {
    /// The file header.
    pub header: Header,

    /// Ordered list of character information
    pub char_infos: Vec<CharInfo>,

    // TODO lig_kerns and extensible characters
    /// Additional parameters contained in the file.
    pub params: Params,
}

/// The TFM header, which contains metadata about the file.
#[derive(Debug, Default, PartialEq, Eq)]
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
#[derive(Default, PartialEq, Eq, Debug, Copy, Clone)]
pub struct FixWord(i32);

impl FixWord {
    /// Representation of the number 0 as a [FixWord].
    pub const ZERO: FixWord = FixWord(0);

    /// Representation of the number 1 as a [FixWord].
    pub const UNITY: FixWord = FixWord(1 << 20);
}

impl TryFrom<&str> for FixWord {
    type Error = String;

    fn try_from(input: &str) -> Result<Self, Self::Error> {
        let mut input = input.chars();
        enum Char {
            Digit(i32),
            Other(char),
        }
        impl Char {
            fn new(c: char) -> Char {
                match c {
                    '0' => Char::Digit(0),
                    '1' => Char::Digit(1),
                    '2' => Char::Digit(2),
                    '3' => Char::Digit(3),
                    '4' => Char::Digit(4),
                    '5' => Char::Digit(5),
                    '6' => Char::Digit(6),
                    '7' => Char::Digit(7),
                    '8' => Char::Digit(8),
                    '9' => Char::Digit(9),
                    other => Char::Other(other),
                }
            }
        }

        let mut negative = false;
        let mut integer = None;
        for c in input.by_ref() {
            match Char::new(c) {
                Char::Other('+') | Char::Other(' ') => (),
                Char::Other('-') => {
                    negative = !negative;
                }
                Char::Digit(d) => {
                    integer = Some(d);
                    break;
                }
                Char::Other(other) => return Err(format!["unexpected character {}", other]),
            }
        }
        let negative = negative;

        let mut integer = match integer {
            None => panic![""],
            Some(integer) => integer,
        };
        for c in input.by_ref() {
            match Char::new(c) {
                Char::Digit(d) => {
                    integer = integer * 10 + d;
                    if integer >= 2048 {
                        return Err(format!["real numbers must be in the range [-2048,2048]"]);
                    }
                }
                Char::Other('.') => break,
                Char::Other(other) => return Err(format!["unexpected character {}", other]),
            }
        }
        let integer = integer;

        let mut num_fractional_digits = 0;
        let mut fraction_digits = [0; 7];
        for c in input.by_ref() {
            match Char::new(c) {
                Char::Digit(d) => {
                    if num_fractional_digits < 7 {
                        fraction_digits[num_fractional_digits] = d * (1 << 21);
                        num_fractional_digits += 1;
                    }
                }
                Char::Other(other) => return Err(format!["unexpected character {}", other]),
            }
        }
        let mut fraction = 0;
        for i in (0..num_fractional_digits).rev() {
            fraction = fraction_digits[i] + fraction / 10;
        }
        let fraction = (fraction + 10) / 20;

        if integer == 2047 && fraction >= (1 << 20) {
            if negative {
                return Ok(FixWord(i32::MIN));
            }
            return Err(format!["real numbers must be in the range [-2048,2048]"]);
        }
        let mut result = integer * FixWord::UNITY.0 + fraction;
        if negative {
            result *= -1;
        }
        Ok(FixWord(result))
    }
}

impl std::fmt::Display for FixWord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut output = String::new();
        let abs: u32 = if self.0 < 0 {
            if self.0 == i32::MIN {
                return write!(f, "-2047.9999999");
            } else {
                output.push('-');
                self.0.abs() as u32
            }
        } else {
            self.0 as u32
        };
        let mut integer = abs / (1 << 20);

        // The integer part is at most 2^11 < 10^4, so there are at most 4 decimal digits.
        let mut integer_digits = [0; 4];
        let mut i = 4;
        loop {
            integer_digits[i - 1] = integer % 10;
            integer /= 10;
            i -= 1;
            if integer == 0 {
                break;
            }
        }
        while i < 4 {
            output.push(std::char::from_digit(integer_digits[i], 10).unwrap());
            i += 1;
        }

        output.push('.');
        let mut delta = 10;
        let mut fraction = abs % (1 << 20);
        fraction = fraction * 10 + 5;
        loop {
            if delta > (1 << 20) {
                fraction = fraction + (1 << 19) - (delta / 2);
            }
            output.push(std::char::from_digit(fraction / (1 << 20), 10).unwrap());
            fraction = (fraction % (1 << 20)) * 10;
            delta *= 10;
            if fraction <= delta {
                break;
            }
        }
        write!(f, "{}", output)
    }
}

/// Information about a character.
#[derive(Debug)]
pub struct CharInfo {
    pub id: u8,
    /// Width of the character.
    pub width: FixWord,
    /// Height of the character.
    pub height: FixWord,
    /// Depth of the character.
    pub depth: FixWord,
    /// Italic correction of the character.
    pub italic_correction: FixWord,
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

#[derive(Debug, Default, PartialEq, Eq)]
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
