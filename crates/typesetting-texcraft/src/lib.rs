//! # Texcraft bindings for the typesetting crate

use texlang::{
    error,
    parse::Parsable,
    token,
    vm::{ExecutionInput, TexlangState},
};

/// Trait satisfied by an state that can be used with this crate.
pub trait HasEngine: TexlangState {
    type Engine;
    fn engine(&mut self) -> &Self::Engine;
    fn engine_mut(&mut self) -> &mut Self::Engine;
}

pub trait CharacterHandlerCompatible {
    fn char_sequence_start(&mut self, c: char);
    fn char_sequence_extend(&mut self, c: char);
    fn char_sequence_finish(&mut self);
}

impl CharacterHandlerCompatible for typesetting::Engine {
    fn char_sequence_start(&mut self, c: char) {
        self.char_sequence_start(c);
    }
    fn char_sequence_extend(&mut self, c: char) {
        self.char_sequence_extend(c);
    }
    fn char_sequence_finish(&mut self) {
        self.char_sequence_finish();
    }
}

/// Character handler
/// TODO: rename letter handler
pub fn character_handler<S: HasEngine>(
    input: &mut ExecutionInput<S>,
    _: token::Token,
    starting_character: char,
) -> Result<(), Box<error::Error>>
where
    <S as HasEngine>::Engine: CharacterHandlerCompatible,
{
    input
        .state_mut()
        .engine_mut()
        .char_sequence_start(starting_character);
    while let Some(right_char) = Option::<char>::parse(input)? {
        input
            .state_mut()
            .engine_mut()
            .char_sequence_extend(right_char);
    }
    input.state_mut().engine_mut().char_sequence_finish();
    Ok(())
}

pub trait NoBoundaryCompatible {
    fn suppress_next_boundary_ligature(&mut self);
}

impl NoBoundaryCompatible for typesetting::Engine {
    fn suppress_next_boundary_ligature(&mut self) {
        self.suppress_next_boundary_ligature();
    }
}

/// Get the `\noboundary` command.
pub fn get_noboundary<S>() -> texlang::command::BuiltIn<S>
where
    S: TexlangState + HasEngine,
    S::Engine: NoBoundaryCompatible,
{
    texlang::command::BuiltIn::new_execution(noboundary_primitive_fn)
}

fn noboundary_primitive_fn<S>(
    _: token::Token,
    input: &mut texlang::vm::ExecutionInput<S>,
) -> Result<(), Box<texlang::error::Error>>
where
    S: TexlangState + HasEngine,
    S::Engine: NoBoundaryCompatible,
{
    // TODO: only do this if the next token is a character
    // TODO: tests
    input
        .state_mut()
        .engine_mut()
        .suppress_next_boundary_ligature();
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;
    use texlang::{command, implement_has_component, vm::TexlangState};

    #[derive(Default)]
    struct FakeEngine {
        buffer: Vec<char>,
        typeset: Vec<Vec<char>>,
    }

    impl CharacterHandlerCompatible for FakeEngine {
        fn char_sequence_start(&mut self, c: char) {
            self.buffer.push(c);
        }
        fn char_sequence_extend(&mut self, c: char) {
            assert!(!self.buffer.is_empty());
            self.buffer.push(c);
        }
        fn char_sequence_finish(&mut self) {
            assert!(!self.buffer.is_empty());
            let mut buffer = vec![];
            std::mem::swap(&mut buffer, &mut self.buffer);
            self.typeset.push(buffer);
        }
    }

    #[derive(Default)]
    struct State {
        engine: FakeEngine,
        testing: texlang_testing::TestingComponent,
    }
    impl TexlangState for State {}
    implement_has_component![State {
        testing: texlang_testing::TestingComponent,
    }];
    impl HasEngine for State {
        type Engine = FakeEngine;

        fn engine(&mut self) -> &Self::Engine {
            &self.engine
        }

        fn engine_mut(&mut self) -> &mut Self::Engine {
            &mut self.engine
        }
    }

    struct Handlers;
    impl texlang::vm::Handlers<State> for Handlers {
        fn character_handler(
            input: &mut ExecutionInput<State>,
            token: token::Token,
            character: char,
        ) -> Result<(), Box<error::Error>> {
            super::character_handler(input, token, character)
        }
    }

    fn built_in_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        HashMap::from([
            ("def", texlang_stdlib::def::get_def()),
            ("chardef", texlang_stdlib::chardef::get_chardef()),
        ])
    }

    fn want_typeset(want: Vec<Vec<char>>) -> impl Fn(&State) {
        move |state: &State| {
            assert_eq!(state.engine.typeset, want);
        }
    }

    texlang_testing::test_suite![
        @handlers(Handlers),
        state_tests(
            // TODO: fix the simplist_case test.
            // It shouldn't typeset a space.
            (simplist_case, r"abc", want_typeset(vec![vec!['a','b','c'], vec![' ']])),
        ),
    ];
}
