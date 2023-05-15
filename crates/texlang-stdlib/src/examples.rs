//! Examples from the TeX literature for use in testing.
//!
//! This module contains testable exercises and examples of TeX snippets from the literature (right now the TeXBook).
//! The documentation for each exercise shows the input and the correct output.
//!
//! This collection is intended for testing and for verifing a TeX distribution
//! is conformant with the language.
//! This module's test module runs the exercises as tests for the primitives implemented in the
//! [Texcraft primitives library](super::library).

/// Data structure representing a testable TeX example.
pub struct Example {
    pub input: &'static str,
    pub solution: &'static str,
}

macro_rules! exercises {
    ( $ ( ($ name : ident , $page: expr, $input : expr, $solution: expr, ), ) * ) => {
             $(
                #[allow(non_upper_case_globals)]
                #[doc = $page]
                #[doc= ".\n\nInput:\n\n```text"]
                #[doc = $input]
                #[doc = "```\n\nExpected output:\n\n```text"]
                #[doc = $solution]
                #[doc = "```\n\n"]
                pub const $name: Example = Example{
                    input: $input,
                    solution: $solution,
                };
            )*

            #[doc="A static array containing all exercises in this module"]
            pub const ALL_EXERCISES: &[&Example] = &[
                $(
                    &$name,
                )*
            ];

            #[cfg(test)]
            mod tests{
                use crate::driver;
                use crate::state::Base;
                use crate::command::library::*;
                use crate::token::catcode;

                struct State;
                fn new_state() -> State {
                    State{}
                }

                fn initial_commands(s: &mut Base<State>) {
                    s.set_command("catcode", catcodecmd::get_catcode());
                    s.set_command("def", def::get_def());
                }

                $(
                    expansion_test![$name, $input, $solution];
                )*
            }
    };
}

exercises![
    (
        texbook_chap_20_exercise_4_pt_2,
        "TexBook p203",
        r#"
\def\mustnt{I must not \doit\ in \thatplace.}%
\def\five{\mustnt\mustnt\mustnt\mustnt\mustnt}%
\def\twenty{\five\five\five\five}%
\def\punishment#1#2{\def\doit{#1}\def\thatplace{#2}\twenty\twenty\twenty\twenty\twenty}%
\punishment{run}{the halls}"#,
        r"
I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.%
I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.%
I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.%
I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.%
I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.%
I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.%
I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.%
I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.%
I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.%
I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.%
I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.%
I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.%
I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.%
I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.%
I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.%
I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.%
I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.%
I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.%
I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.%
I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.%
I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.%
I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.%
I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.%
I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.%
I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.I must not run\ in the halls.",
    ),
    (
        texbook_chap_20_exercise_7,
        "TeXBook p205",
        r"
\catcode`!=6 \catcode`[=1 \catcode`]=2
\def\!!1#2![{!#]#!!2}\! x{[y]][z}",
        r"
\catcode`!=6 \catcode`[=1 \catcode`]=2
{#]![y][z}",
    ),
];
