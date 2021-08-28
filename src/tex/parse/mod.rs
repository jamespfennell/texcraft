//! Logic for parsing elements of the TeX grammer from token streams.

#[macro_use]
mod helpers;

mod keyword;
mod number;
mod relation;
mod variable;

pub use keyword::parse_optional_by;
pub use number::parse_number;
pub use relation::parse_relation;
pub use relation::Relation;
pub use variable::parse_optional_equals;
pub use variable::parse_variable;

use crate::tex::prelude::*;

pub fn parse_optional_space<S>(input: &mut command::ExpansionInput<S>) -> anyhow::Result<()> {
    get_optional_element![input, Character(_, CatCode::Space) => (),];
    Ok(())
}
