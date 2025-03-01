//! Core types and abstractions used in Texcraft.
//!
//!

use std::fmt::Write;

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

    /// Maximum possible dimension in TeX, which is (2^30-1)/2^16.
    ///
    /// This is _not_ the maximum size of the Rust scaled number type, which is (2^31-1)/2^16.
    ///
    /// Defined in TeX.2021.421.
    pub const MAX_DIMEN: Scaled = Scaled((1 << 30) - 1);

    /// Create a scaled number corresponding the provided positive integer.
    ///
    /// Scaled numbers are in the range `(-2^14, 2^14)`.
    /// If _i_ is outside this range an overflow error is returned.
    pub fn from_integer(i: i32) -> Result<Scaled, OverflowError> {
        if i >= (1 << 14) || i <= -(1 << 14) {
            Err(OverflowError {})
        } else {
            Ok(Scaled(Scaled::ONE.0 * i))
        }
    }

    /// Creates a scaled number from a decimal fraction.
    ///
    /// TeX.2021.102.
    pub fn from_decimal_fraction(digits: &[u8]) -> Scaled {
        let mut a = 0;
        for d in digits.iter().rev() {
            a = (a + (*d as i32) * Scaled::TWO.0) / 10
        }
        Scaled((a + 1) / 2)
    }

    /// Calculates the integer division _xn_/_d_ and remainder, where _x_ is this scaled number
    /// and _n_ and _d_ are integers in the range `[0,2^16]`.
    ///
    /// This function appears in TeX.2021.107. Knuth is working with 32-bit integers
    /// and so calculating this number is tricky without overflowing. E.g. _xn_ may
    /// be larger than `2^32-1` even if the final result is in range.
    /// TeX has an algorithm that calculates the exact value without overflowing,
    /// in the case when the final result is in range.
    ///
    /// Our implementation simply uses 64-bit integers.
    pub fn xn_over_d(&self, n: i32, d: i32) -> Result<(Scaled, Scaled), OverflowError> {
        debug_assert!(n <= 0o200000);
        debug_assert!(d <= 0o200000);
        let mut b: i64 = self.0.into();
        b *= n as i64; // can't overflow because |b|<=2^31 and |n|<=2^16
        let remainder: i32 = (b % (d as i64)).try_into().expect("d<=2^16 so b%d<2^16");
        b /= d as i64;
        if b < -(Scaled::MAX_DIMEN.0 as i64) || b > Scaled::MAX_DIMEN.0 as i64 {
            return Err(OverflowError {});
        }
        let b: i32 = b.try_into().expect("b in (-2^30, +2^30");
        Ok((Scaled(b), Scaled(remainder)))
    }

    /// TeX.2021.105
    pub fn nx_plus_y(self, mut n: i32, y: Scaled) -> Result<Scaled, OverflowError> {
        let max_answer = Scaled::MAX_DIMEN;
        if n == 0 {
            return Ok(y);
        }
        let mut x = self;
        if n < 0 {
            n = -n;
            x = -x;
        }
        if x <= (max_answer - y) / n && -x <= (max_answer + y) / n {
            Ok(x * n + y)
        } else {
            Err(OverflowError {})
        }
    }

    pub fn integer_part(self) -> i32 {
        self.0 / Scaled::ONE.0
    }

    pub fn fractional_part(self) -> Scaled {
        self % Scaled::ONE.0
    }

    pub fn abs(self) -> Scaled {
        Scaled(self.0.abs())
    }
}

#[derive(Debug)]
pub struct OverflowError;

impl std::fmt::Display for Scaled {
    // TeX.2021.103
    fn fmt(&self, fm: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = *self;
        // Integer part
        write!(fm, "{}.", s.integer_part())?;
        // Fractional part
        let mut f = s.abs().fractional_part() * 10 + Scaled(5);
        let mut delta = Scaled(10);
        loop {
            if delta > Scaled::ONE {
                // round the last digit
                f = f + Scaled(0o100000 - 50000);
            }
            fm.write_char(char::from_digit(f.integer_part().try_into().unwrap(), 10).unwrap())?;
            f = f.fractional_part() * 10;
            delta = delta * 10;
            if f <= delta {
                break;
            }
        }
        // Units
        write!(fm, "pt")?;
        Ok(())
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

impl std::ops::Rem<i32> for Scaled {
    type Output = Scaled;
    fn rem(self, rhs: i32) -> Self::Output {
        Scaled(self.0 % rhs)
    }
}

impl std::ops::Neg for Scaled {
    type Output = Scaled;
    fn neg(self) -> Self::Output {
        Scaled(-self.0)
    }
}

/// Unit used to define a scaled integer
///
/// Defined in TeX.2021.458 and chapter 10 of the TeX book.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScaledUnit {
    Point,
    Pica,
    Inch,
    BigPoint,
    Centimeter,
    Millimeter,
    DidotPoint,
    Cicero,
    ScaledPoint,
}

impl ScaledUnit {
    /// Parses a unit from a two character abbreviation.
    ///
    /// E.g., `"pc"` is parsed to [`ScaledUnit::Pica`].
    /// These are abreviations are defined in TeX.2021.458 and chapter 10 of the TeX book.
    pub fn parse(s: &str) -> Option<Self> {
        use ScaledUnit::*;
        Some(match s {
            "pt" => Point,
            "pc" => Pica,
            "in" => Inch,
            "bp" => BigPoint,
            "cm" => Centimeter,
            "mm" => Millimeter,
            "dd" => DidotPoint,
            "cc" => Cicero,
            "sp" => ScaledPoint,
            _ => return None,
        })
    }

    /// Returns the fraction needed to convert to/from this unit to points.
    ///
    /// The return value is of the form (_n_, _d_).
    /// If a scaled number represents _x_in these units (e.g. y [`ScaledUnit::Pica`]),
    ///     then it is _y_=_nx_/_d_ points.
    ///
    /// Defined in TeX.2021.458.
    pub fn conversion_fraction(&self) -> (i32, i32) {
        use ScaledUnit::*;
        match self {
            Point => (1, 1),
            Pica => (12, 1),
            Inch => (7227, 100),
            BigPoint => (7227, 7200),
            Centimeter => (7227, 254),
            Millimeter => (7227, 2540),
            DidotPoint => (1238, 1157),
            Cicero => (14856, 1157),
            ScaledPoint => (1, 1 << 16),
        }
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
#[derive(Debug, PartialEq, Eq)]
pub struct Glue {
    pub width: Scaled,
    pub stretch: Scaled,
    pub stretch_order: GlueOrder,
    pub shrink: Scaled,
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
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GlueOrder {
    Normal,
    Fil,
    Fill,
    Filll,
}

impl GlueOrder {
    /// Parses an infinite glue order from a keyword.
    pub fn parse(s: &str) -> Option<Self> {
        use GlueOrder::*;
        Some(match s {
            "fil" => Fil,
            "fill" => Fill,
            "filll" => Filll,
            _ => return None,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn type_sizes() {
        assert_eq!(16, std::mem::size_of::<Glue>());
    }

    #[test]
    fn xn_over_d() {}
}
