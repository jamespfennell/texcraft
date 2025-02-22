//! Core types and abstractions used in Texcraft.
//!
//!

/// Trait satisfied by font formats (like .tfm files).
pub trait FontFormat: Sized {
    const DEFAULT_FILE_EXTENSION: &'static str;
    type Error: std::error::Error + 'static;

    /// Parse binary data into a font.
    fn parse(b: &[u8]) -> Result<Self, Self::Error>;
}

/// Scaled numbers.
///
/// This is a fixed-width numeric type used in throughout TeX.
/// This type is defined and described in part 7 "arithmetic with scaled
/// dimensions" starting at TeX.2021.99.
///
/// This numeric type has 15 bits for the integer part,
/// 16 bits for the fractional part, and a single signed bit.
/// The inner value is the number multiplied by 2^16.
#[derive(Default, PartialEq, Eq, Debug, Copy, Clone, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Scaled(pub i32);

impl Scaled {
    /// Representation of the number 0 as a [Scaled].
    pub const ZERO: Scaled = Scaled(0);

    /// Representation of the number 1 as a [Scaled].
    pub const ONE: Scaled = Scaled(1 << 16);

    /// Representation of the number 2 as a [Scaled].
    pub const TWO: Scaled = Scaled(1 << 17);

    /// Creates a scaled number from a decimal fraction.
    /// 
    /// TeX.2021.102.
    pub fn from_decimal_fraction(digits: &[u8]) -> Scaled {
        let mut a = 0;
        for d in digits.iter().rev() {
            a = (a + (*d as i32) * Scaled::TWO.0) / 10
        }
        Scaled((a+1)/2)
    }
}

impl std::ops::Add<Scaled> for Scaled {
    type Output = Scaled;
    fn add(self, rhs: Scaled) -> Self::Output {
        Scaled(self.0 + rhs.0)
    }
}
impl std::ops::Sub<Scaled> for Scaled {
    type Output = Scaled;
    fn sub(self, rhs: Scaled) -> Self::Output {
        Scaled(self.0 - rhs.0)
    }
}

impl std::ops::Mul<i32> for Scaled {
    type Output = Scaled;

    fn mul(self, rhs: i32) -> Self::Output {
        Scaled(self.0 * rhs)
    }
}

impl std::ops::Div<i32> for Scaled {
    type Output = Scaled;

    fn div(self, rhs: i32) -> Self::Output {
        Scaled(self.0 / rhs)
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
    pub width: Scaled,
    pub stretch: Scaled,
    pub shrink: Scaled,
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
