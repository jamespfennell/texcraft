//! Font abstractions and types
//!
//!

/// Trait satisfied by font formats (like .tfm files).
pub trait Format: Sized {
    const DEFAULT_FILE_EXTENSION: &'static str;
    type Error: std::error::Error + 'static;

    /// Parse binary data into a font.
    fn parse(b: &[u8]) -> Result<Self, Self::Error>;
}

/// Fixed-width numeric type used in TeX.
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
pub struct Number(pub i32);

impl Number {
    /// Representation of the number 0 as a [Number].
    pub const ZERO: Number = Number(0);

    /// Representation of the number 1 as a [Number].
    pub const UNITY: Number = Number(1 << 20);

    /// Returns true if the number is less than 16.0 in magnitude according to Knuth.
    ///
    /// The number +16.0 is not allowed.
    /// This is covered in the E2E tests.
    /// See `check_fix` in TFtoPL.2014.60.
    pub fn is_abs_less_than_16(&self) -> bool {
        *self >= Number::UNITY * -16 && *self < Number::UNITY * 16
    }
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
        let integer_part = (self.0 / Number::UNITY.0).abs();
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

/// Glue.
///
/// In Knuth's TeX this struct is not passed around directly; instead
/// Knuth essentially uses `std::rc::Rc<Glue>`.
/// This optimization is based on the fact that very few distinct glue
/// values appear in a document, and that the pointer takes up less
/// space than the struct.
/// We might consider performing such an optimization.
///
/// Described in TeX.2021.150.
pub struct Glue {
    pub width: Number,
    pub stretch: Number,
    pub shrink: Number,
    pub stretch_order: GlueOrder,
    pub shrink_order: GlueOrder,
}

/// Order of infinity of a glue stretch or shrink.
///
/// When setting a list of boxes, TeX stretches or shrinks glue boxes.
/// In some cases it is desirable that TeX only stretches some subset of the
/// glue boxes.
/// For example, when setting centered text, TeX only stretches the two glue
/// boxes at each end of the list and leaves all other glue intact.
///
/// To achieve this, each glue stretch or shrink has an order of infinity.
/// If a list contains glue of some order (e.g. [GlueOrder::Fil]),
/// then glues of a lower order (e.g. [GlueOrder::Normal]) are not stretched
/// or shrunk.
pub enum GlueOrder {
    Normal,
    Fil,
    Fill,
    Filll,
}
