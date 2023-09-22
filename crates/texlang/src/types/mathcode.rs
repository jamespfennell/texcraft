use crate::{parse, traits::*};

/// A math code.
///
/// The inner value is in the range [0, 32768).
///
/// See chapter 17 of the TeXBook for information on this type.
/// The TeXBook presents an _interpretation_ of the inner value.
/// For example, the highest 3 bits represent the "class" of the math character.
/// From Texlang's perspective, however, a math character is just a regular integer and
///     such interpretations are scoped to algorithms that consumes math characters.
#[derive(Clone, Copy, Debug, Default)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct MathCode(pub u16);

impl MathCode {
    /// The maximum value of the inner value. This is 2^15-1.
    pub const MAX: usize = 32767;
}

impl<S: TexlangState> Parsable<S> for MathCode {
    fn parse_impl(
        input: &mut crate::vm::ExpandedStream<S>,
    ) -> Result<Self, Box<crate::error::Error>> {
        let raw = parse::Uint::<{ MathCode::MAX + 1 }>::parse(input)?;
        Ok(MathCode(raw.0.try_into().unwrap()))
    }
}
