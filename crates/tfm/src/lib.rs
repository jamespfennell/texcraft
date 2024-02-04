//! Parsers for the TeX font metric (.tfm) and property list (.pl) file formats

#[cfg(test)]
mod examples;
pub mod format;
pub mod ligkern;
pub mod pl;

/// The TFM header, which contains metadata about the file.
///
/// This is defined in TFtoPL.2014.10.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Header {
    /// In TeX82, this is stored in the `font_check` array (TeX82.2021.549).
    pub checksum: u32,
    /// In TeX82, this is stored in the `font_dsize` array (TeX82.2021.549).
    pub design_size: Number,
    pub character_coding_scheme: String,
    pub font_family: String,
    pub seven_bit_safe: Option<bool>,
    pub face: Option<Face>,
    /// The TFM format allows the header to contain arbitrary additional data.
    pub additional_data: Vec<u32>,
}

impl Header {
    /// Returns the default header when parsing property list files.
    ///
    /// This is defined in PLtoTF.2014.70.
    pub fn property_list_default() -> Header {
        Header {
            checksum: 0,
            design_size: Number::UNITY * 10,
            character_coding_scheme: "UNSPECIFIED".into(),
            font_family: "UNSPECIFIED".into(),
            seven_bit_safe: Some(false),
            face: Some(0.into()),
            additional_data: vec![],
        }
    }
}

/// A character in a TFM file.
///
/// TFM and PL files only support 1-byte characters.
#[derive(Debug, Default, PartialEq, Eq, Hash, Clone, Copy, PartialOrd, Ord)]
pub struct Char(pub u8);

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
pub struct Number(pub i32);

impl Number {
    /// Representation of the number 0 as a [Number].
    pub const ZERO: Number = Number(0);

    /// Representation of the number 1 as a [Number].
    pub const UNITY: Number = Number(1 << 20);
}

impl std::ops::Add<Number> for Number {
    type Output = Number;
    fn add(self, rhs: Number) -> Self::Output {
        Number(self.0 + rhs.0)
    }
}
impl std::ops::Sub<Number> for Number {
    type Output = Number;
    fn sub(self, rhs: Number) -> Self::Output {
        Number(self.0 - rhs.0)
    }
}

impl std::ops::Mul<i32> for Number {
    type Output = Number;

    fn mul(self, rhs: i32) -> Self::Output {
        Number(self.0 * rhs)
    }
}

impl std::ops::Div<i32> for Number {
    type Output = Number;

    fn div(self, rhs: i32) -> Self::Output {
        Number(self.0 / rhs)
    }
}

impl std::fmt::Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TFtoPL.2014.40-43
        if self.0 < 0 {
            write!(f, "-")?;
        }
        let integer_part = self.0 / Number::UNITY.0;
        write!(f, "{integer_part}.")?;
        let mut fp = (self.0 % Number::UNITY.0).abs();
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

/// TeX font metric parameters
#[derive(Debug, Default, PartialEq, Eq)]
pub struct Params(pub Vec<Number>);

impl Params {
    pub fn set(&mut self, number: usize, value: Number) {
        if self.0.len() <= number {
            self.0.resize(number, Default::default());
        }
        self.0[number - 1] = value;
    }

    pub fn set_named(&mut self, named_param: NamedParam, value: Number) {
        self.set(named_param.number() as usize, value)
    }
}

/// A named TeX font metric parameter.
#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum NamedParam {
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

impl NamedParam {
    pub fn number(&self) -> u8 {
        match self {
            NamedParam::Slant => 1,
            NamedParam::Space => 2,
            NamedParam::Stretch => 3,
            NamedParam::Shrink => 4,
            NamedParam::XHeight => 5,
            NamedParam::Quad => 6,
            NamedParam::ExtraSpace => 7,
            NamedParam::Num1 => 8,
            NamedParam::Num2 => 9,
            NamedParam::Num3 => 10,
            NamedParam::Denom1 => 11,
            NamedParam::Denom2 => 12,
            NamedParam::Sup1 => 13,
            NamedParam::Sup2 => 14,
            NamedParam::Sup3 => 15,
            NamedParam::Sub1 => 16,
            NamedParam::Sub2 => 17,
            NamedParam::SupDrop => 18,
            NamedParam::SubDrop => 19,
            NamedParam::Delim1 => 20,
            NamedParam::Delim2 => 21,
            NamedParam::AxisHeight => 22,
            NamedParam::DefaultRuleThickness => 8,
            NamedParam::BigOpSpacing1 => 9,
            NamedParam::BigOpSpacing2 => 10,
            NamedParam::BigOpSpacing3 => 11,
            NamedParam::BigOpSpacing4 => 12,
            NamedParam::BigOpSpacing5 => 13,
        }
    }
}
