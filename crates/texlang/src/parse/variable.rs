use crate::prelude as txl;
use crate::traits::*;
use crate::*;

/// When parsed, this type consumes an optional equals from the token stream.
pub struct OptionalEquals;

impl<S: TexlangState> Parsable<S> for OptionalEquals {
    fn parse_impl(input: &mut vm::ExpandedStream<S>) -> txl::Result<Self> {
        // scan_optional_equals
        parse_optional_equals(input)?;
        Ok(OptionalEquals {})
    }
}

/// When parsed, this type consumes an optional equals from the token stream without performing expansion.
pub struct OptionalEqualsUnexpanded;

impl<S: TexlangState> Parsable<S> for OptionalEqualsUnexpanded {
    fn parse_impl(input: &mut vm::ExpandedStream<S>) -> txl::Result<Self> {
        parse_optional_equals(input.unexpanded())?;
        Ok(OptionalEqualsUnexpanded {})
    }
}

// Corresponds to the `scan_optional_equals` procedure in Knuth's TeX (405)
fn parse_optional_equals<S: TexlangState, I: TokenStream<S = S>>(input: &mut I) -> txl::Result<()> {
    while let Some(found_equals) = get_optional_element![
        input,
        token::Value::Other('=') => true,
        token::Value::Space(_) => false,
    ] {
        if found_equals {
            break;
        }
    }
    // TODO: this is not correct: this function should not scan spaces after the equals.
    // A separate routine corresponding to Knuth TeX 404 needs to be added and used instead
    // at the right call sites.
    while get_optional_element![
        input,
        token::Value::Space(_) => (),
    ]
    .is_some()
    {}
    Ok(())
}
