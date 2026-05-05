//! Implementation of TeX primitives relating to text transforming (hyphenation, upper and lower casing).
//!
//! The theme of the primitives here (`\lccode`, `\lowercase`, `\patterns`) is that
//! they're all related to "tex transformation" (case change, hyphenation).
//! However the real reason they're grouped is that the hyphenator hyphenates based on
//! each character's lower case, and so has a dependency on `\lccode` which defines
//! these cases.
//!
//! This crate is also a good experiment in how defining registers works outside of the
//! standard lib. At time of writing (May 2026) I can say that it's not really great.
//! The problem is that having the exact component specified is a but too rigid.
//! For example, I might like multiple registers in the same component.

use texlang::prelude as txl;
use texlang::token::{Token, Value};
use texlang::traits::*;
use texlang::*;
use texlang_stdlib::registers;

/// Component that holds the 256 `\lccode` values.
///
/// Each entry is the lowercase character code for the corresponding character (0–255).
/// Although the values are semantically character codes (0–255), the storage type is
/// `i32` because that is the integer type supported by the Texlang variable system.
pub type LcCodeComponent = registers::Component<u8, 256, LcCodeMarker>;

pub struct LcCodeMarker;

/// Get the `\lccode` command.
pub fn get_lccode<S: HasComponent<LcCodeComponent>>() -> command::BuiltIn<S> {
    registers::new_registers_command()
}

/// Component that holds the 256 `\uccode` values.
///
/// Each entry is the uppercase character code for the corresponding character (0–255).
pub type UcCodeComponent = registers::Component<u8, 256, UcCodeMarker>;

pub struct UcCodeMarker;

/// Get the `\uccode` command.
pub fn get_uccode<S: HasComponent<UcCodeComponent>>() -> command::BuiltIn<S> {
    registers::new_registers_command()
}

/// Get the `\lowercase` expansion primitive.
pub fn get_lowercase<S: HasComponent<LcCodeComponent>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_expansion(lowercase_primitive_fn)
}

fn lowercase_primitive_fn<S: HasComponent<LcCodeComponent>>(
    _: token::Token,
    input: &mut vm::ExpansionInput<S>,
) -> txl::Result<()> {
    // TeX.2021.1288
    let mut list = texlang::parse::UnexpandedTokenList::parse(input)?.0;
    transform_list(&mut list, input.state().component().values());
    input.push_expansion(&list);
    input.return_token_buffer(list);
    Ok(())
}

/// Get the `\uppercase` expansion primitive.
pub fn get_uppercase<S: HasComponent<UcCodeComponent>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_expansion(uppercase_primitive_fn)
}

fn uppercase_primitive_fn<S: HasComponent<UcCodeComponent>>(
    _: token::Token,
    input: &mut vm::ExpansionInput<S>,
) -> txl::Result<()> {
    // TeX.2021.1288
    let mut list = texlang::parse::UnexpandedTokenList::parse(input)?.0;
    transform_list(&mut list, input.state().component().values());
    input.push_expansion(&list);
    input.return_token_buffer(list);
    Ok(())
}

fn transform_list(list: &mut [token::Token], values: &[u8]) {
    for token in list {
        let Some((c, cat_code)) = token.char_and_cat_code() else {
            continue;
        };
        let Some(new_c_raw) = values.get(c as usize) else {
            continue;
        };
        if *new_c_raw == 0 {
            continue;
        }
        let new_c = char::from(*new_c_raw);
        *token = Token::new_from_value(Value::new(new_c, cat_code), token.trace_key())
    }
}

#[derive(Default)]
pub struct HyphenationComponent {
    hyphenator: hyphenate::Hyphenator,
}

/// Get the `\patterns` primitive.
pub fn get_patterns<S: HasComponent<HyphenationComponent>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(patterns_primitive_fn)
}

fn patterns_primitive_fn<S: HasComponent<HyphenationComponent>>(
    _: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> txl::Result<()> {
    // TeX.2021.961
    texlang::parse::LeftBrace::parse(input)?;
    let mut s = String::new();
    loop {
        let token = input.next_or_err(EndOfInputError {})?;
        use Value::*;
        match token.value() {
            EndGroup(_) => {
                break;
            }
            Space(_) => {
                s.push(' ');
            }
            Letter(c) | Other(c) => {
                s.push(c);
            }
            _ => {
                // TODO: error
            }
        }
    }
    input
        .state_mut()
        .component_mut()
        .hyphenator
        .load_patterns(&s);
    Ok(())
}

/// Get the `\hyphenation` primitive.
pub fn get_hyphenation<S: HasComponent<HyphenationComponent>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(hyphenation_primitive_fn)
}

fn hyphenation_primitive_fn<S: HasComponent<HyphenationComponent>>(
    _: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> txl::Result<()> {
    // TeX.2021.935
    texlang::parse::LeftBrace::parse(input)?;
    let mut s = String::new();
    loop {
        let token = input.next_or_err(EndOfInputError {})?;
        use Value::*;
        match token.value() {
            EndGroup(_) => {
                break;
            }
            Space(_) => {
                s.push(' ');
            }
            Letter(c) | Other(c) => {
                // TODO: need to check the \lccode is valid
                // as in TeX.2021.937.
                s.push(c);
            }
            _ => {
                // TODO: error and handle other cases. TeX is pretty flexible in
                // handling exceptions here.
            }
        }
    }
    input
        .state_mut()
        .component_mut()
        .hyphenator
        .insert_exception(&s);
    Ok(())
}

#[derive(Debug)]
struct EndOfInputError;

impl error::EndOfInputError for EndOfInputError {
    fn doing(&self) -> String {
        r"determining the argument to \pattern or \hyphenation".into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;
    use texlang::vm::implement_has_component;
    use texlang_stdlib::{prefix, the};
    use texlang_testing::*;

    #[derive(Default)]
    struct State {
        lccode: LcCodeComponent,
        uccode: UcCodeComponent,
        prefix: prefix::Component,
        testing: TestingComponent,
    }

    impl TexlangState for State {
        fn recoverable_error_hook(
            &self,
            recoverable_error: error::TracedTexError,
        ) -> Result<(), Box<dyn error::TexError>> {
            TestingComponent::recoverable_error_hook(self, recoverable_error)
        }
        fn variable_assignment_scope_hook(
            state: &mut Self,
        ) -> texcraft_stdext::collections::groupingmap::Scope {
            prefix::variable_assignment_scope_hook(state)
        }
    }
    impl the::TheCompatible for State {}

    implement_has_component![State {
        lccode: LcCodeComponent,
        uccode: UcCodeComponent,
        prefix: prefix::Component,
        testing: TestingComponent,
    }];

    fn built_in_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        HashMap::from([
            ("the", the::get_the()),
            ("def", texlang_stdlib::def::get_def()),
            ("lccode", get_lccode()),
            ("uccode", get_uccode()),
            ("lowercase", get_lowercase()),
            ("uppercase", get_uppercase()),
            ("global", prefix::get_global()),
        ])
    }

    test_suite![expansion_equality_tests(
        (
            lccode_write_and_read,
            r"\lccode 65 97 \the\lccode 65",
            r"97"
        ),
        (
            uccode_write_and_read,
            r"\uccode 97 65 \the\uccode 97",
            r"65"
        ),
        (lccode_default, r"\the\lccode 65", r"0"),
        (uccode_default, r"\the\uccode 97", r"0"),
        (
            lccode_grouping,
            r"{\lccode 65 97 \the\lccode 65}-\the\lccode 65",
            r"97-0"
        ),
        (
            uccode_grouping,
            r"{\uccode 97 65 \the\uccode 97}-\the\uccode 97",
            r"65-0"
        ),
        // \uppercase tests
        // uccode 0 means leave the character unchanged
        (uppercase_zero_uccode, r"\uppercase{a}", r"a"),
        // basic single character substitution: 'a'(97) -> 'A'(65)
        (uppercase_single_char, r"\uccode 97 65 \uppercase{a}", r"A"),
        // multiple characters
        (
            uppercase_multiple_chars,
            r"\uccode 97 65 \uccode 98 66 \uppercase{ab}",
            r"AB"
        ),
        // characters with uccode=0 are left unchanged
        (uppercase_mixed, r"\uccode 97 65 \uppercase{a1}", r"A1"),
        // uccode mapping is read at expansion time, so changes inside the group don't apply
        (
            uppercase_uses_unexpanded_stream,
            r"\def\a{a}\uccode 97 65 \uppercase{\a}",
            r"a"
        ),
        // \lowercase tests
        // lccode 0 means leave the character unchanged
        (lowercase_zero_lccode, r"\lowercase{A}", r"A"),
        // basic single character substitution: 'A'(65) -> 'a'(97)
        (lowercase_single_char, r"\lccode 65 97 \lowercase{A}", r"a"),
        // multiple characters
        (
            lowercase_multiple_chars,
            r"\lccode 65 97 \lccode 66 98 \lowercase{AB}",
            r"ab"
        ),
        // characters with lccode=0 are left unchanged
        (lowercase_mixed, r"\lccode 65 97 \lowercase{A1}", r"a1"),
        // lccode mapping is read at expansion time, so changes inside the group don't apply
        (
            lowercase_uses_unexpanded_stream,
            r"\def\A{A}\lccode 65 97 \lowercase{\A}",
            r"A"
        ),
        // \uppercase and \lowercase don't interfere with each other
        (
            uppercase_unaffected_by_lccode,
            r"\lccode 65 97 \uccode 97 65 \uppercase{aA}",
            r"AA"
        ),
        (
            lowercase_unaffected_by_uccode,
            r"\uccode 97 65 \lccode 65 97 \lowercase{AA}",
            r"aa"
        ),
    ),];
}
