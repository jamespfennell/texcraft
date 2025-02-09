//! User-defined macros (`\def` and friends)

use texcraft_stdext::algorithms::substringsearch::Matcher;
use texcraft_stdext::collections::groupingmap;
use texcraft_stdext::collections::nevec::Nevec;
use texcraft_stdext::nevec;
use texlang::prelude as txl;
use texlang::traits::*;
use texlang::*;

pub const DEF_DOC: &str = "Define a custom macro";

/// Get the `\def` command.
pub fn get_def<S: TexlangState>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(def_primitive_fn).with_tag(def_tag())
}

/// Get the `\gdef` command.
pub fn get_gdef<S: TexlangState>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(gdef_primitive_fn).with_tag(def_tag())
}

static DEF_TAG: command::StaticTag = command::StaticTag::new();

pub fn def_tag() -> command::Tag {
    DEF_TAG.get()
}

fn def_primitive_fn<S: TexlangState>(
    def_token: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> txl::Result<()> {
    parse_and_set_macro(def_token, input, false)
}

fn gdef_primitive_fn<S: TexlangState>(
    def_token: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> txl::Result<()> {
    parse_and_set_macro(def_token, input, true)
}

fn parse_and_set_macro<S: TexlangState>(
    _: token::Token,
    input: &mut vm::ExecutionInput<S>,
    set_globally_override: bool,
) -> txl::Result<()> {
    let mut scope = TexlangState::variable_assignment_scope_hook(input.state_mut());
    if set_globally_override {
        scope = groupingmap::Scope::Global;
    }
    let cmd_ref_or = Option::<token::CommandRef>::parse(input)?;
    let PrefixAndParameters {
        prefix,
        raw_parameters,
        replacement_end_token,
        skip_replacement_scan,
    } = parse_prefix_and_parameters(input.unexpanded())?;
    let parameters: Vec<texmacro::Parameter> = raw_parameters
        .into_iter()
        .map(|a| match a {
            RawParameter::Undelimited => texmacro::Parameter::Undelimited,
            RawParameter::Delimited(vec) => texmacro::Parameter::Delimited(Matcher::new(vec)),
        })
        .collect();
    let replacement = if skip_replacement_scan {
        vec![]
    } else {
        let mut rs =
            parse_replacement_text(input.unexpanded(), replacement_end_token, parameters.len())?;
        for r in rs.iter_mut() {
            if let texmacro::Replacement::Tokens(tokens) = r {
                tokens.reverse();
            }
        }
        rs
    };
    let user_defined_macro = texmacro::Macro::new(prefix, parameters, replacement);
    if let Some(cmd_ref) = cmd_ref_or {
        input
            .commands_map_mut()
            .insert_macro(cmd_ref, user_defined_macro, scope);
    }
    Ok(())
}

enum RawParameter {
    Undelimited,
    Delimited(Nevec<token::Value>),
}

impl RawParameter {
    fn push(&mut self, t: token::Token) {
        match self {
            RawParameter::Undelimited => {
                *self = RawParameter::Delimited(nevec![t.value()]);
            }
            RawParameter::Delimited(vec) => {
                vec.push(t.value());
            }
        }
    }
}

fn char_to_parameter_index(c: char) -> Option<usize> {
    match c {
        '1' => Some(0),
        '2' => Some(1),
        '3' => Some(2),
        '4' => Some(3),
        '5' => Some(4),
        '6' => Some(5),
        '7' => Some(6),
        '8' => Some(7),
        '9' => Some(8),
        _ => None,
    }
}

struct PrefixAndParameters {
    prefix: Vec<token::Token>,
    raw_parameters: Vec<RawParameter>,
    // For the weird #{ edge case
    replacement_end_token: Option<token::Token>,
    skip_replacement_scan: bool,
}

/// TeX.2021.474
fn parse_prefix_and_parameters<S: TexlangState>(
    input: &mut vm::UnexpandedStream<S>,
) -> txl::Result<PrefixAndParameters> {
    let mut prefix = Vec::new();
    let mut raw_parameters: Vec<RawParameter> = Vec::new();
    let mut replacement_end_token = None;
    let mut skip_replacement_scan = false;
    loop {
        let token = input.next(ParameterPartEndOfInputError {})?;
        match token.value() {
            token::Value::BeginGroup(_) => {
                break;
            }
            token::Value::EndGroup(_) => {
                input.vm().error(error::SimpleTokenError::new(
                    token,
                    "unexpected end group token while parsing the parameter of a macro definition",
                ))?;
                skip_replacement_scan = true;
                break;
            }
            token::Value::Parameter(_) => {
                // TeX.2021.476
                let parameter_token = input.next(ParameterPartEndOfInputError {})?;
                // "parsing a parameter")?;
                match parameter_token.value() {
                    token::Value::BeginGroup(_) => {
                        // In this case we end the group according to the special #{ rule
                        replacement_end_token = Some(parameter_token);
                        match raw_parameters.last_mut() {
                            None => {
                                prefix.push(parameter_token);
                            }
                            Some(spec) => {
                                spec.push(parameter_token);
                            }
                        }
                        break;
                    }
                    _ => {
                        if raw_parameters.len() == 9 {
                            input.vm().error(error::SimpleTokenError::new(
                                token,
                                "Too many parameters; you already have 9",
                            ))?;
                            continue;
                        }
                        let parameter_index_correct = match parameter_token.char() {
                            // control sequence
                            None => false,
                            // character token
                            Some(c) => {
                                match char_to_parameter_index(c) {
                                    // non-numeric character token
                                    None => false,
                                    // numeric character token
                                    Some(n) => n == raw_parameters.len(),
                                }
                            }
                        };
                        if !parameter_index_correct {
                            input.vm().error(InvalidParameterNumberError {
                                parameter_number_token: parameter_token,
                                parameters_so_far: raw_parameters.len(),
                            })?;
                            input.expansions_mut().push(parameter_token);
                        }
                        raw_parameters.push(RawParameter::Undelimited);
                    }
                }
            }
            _ => match raw_parameters.last_mut() {
                None => {
                    prefix.push(token);
                }
                Some(parameter) => {
                    parameter.push(token);
                }
            },
        }
    }
    // We may end up here because the input ended in which case we should error.
    // However this case will be handled when we try to scan the replacement
    // text.
    Ok(PrefixAndParameters {
        prefix,
        raw_parameters,
        replacement_end_token,
        skip_replacement_scan,
    })
}

#[derive(Debug)]
struct ParameterPartEndOfInputError;

impl error::EndOfInputError for ParameterPartEndOfInputError {
    fn doing(&self) -> String {
        r"parsing the parameter part of a macro being defined by \def".into()
    }
}

#[derive(Debug)]
struct ReplacementPartEndOfInputError;

impl error::EndOfInputError for ReplacementPartEndOfInputError {
    fn doing(&self) -> String {
        r"parsing the replacement part of a macro being defined by \def".into()
    }
}

fn parse_replacement_text<S: TexlangState>(
    input: &mut vm::UnexpandedStream<S>,
    opt_final_token: Option<token::Token>,
    num_parameters: usize,
) -> txl::Result<Vec<texmacro::Replacement>> {
    // TODO: could we use a pool of vectors to avoid some of the allocations here?
    let mut result = vec![];
    let mut scope_depth = 0;
    let push = |result: &mut Vec<texmacro::Replacement>, token| match result.last_mut() {
        Some(texmacro::Replacement::Tokens(tokens)) => {
            tokens.push(token);
        }
        _ => {
            result.push(texmacro::Replacement::Tokens(vec![token]));
        }
    };

    loop {
        let token = input.next(ReplacementPartEndOfInputError {})?;
        // "parsing the replacement text of a macro")?;
        match token.value() {
            token::Value::BeginGroup(_) => {
                scope_depth += 1;
            }
            token::Value::EndGroup(_) => {
                if scope_depth == 0 {
                    if let Some(final_token) = opt_final_token {
                        push(&mut result, final_token);
                    }
                    return Ok(result);
                }
                scope_depth -= 1;
            }
            token::Value::Parameter(_) => {
                let parameter_token = input.next(ReplacementPartEndOfInputError {})?;
                let c = match parameter_token.value() {
                    token::Value::Parameter(_) => {
                        // ## case
                        push(&mut result, parameter_token);
                        continue;
                    }
                    _ => parameter_token.char(),
                };
                let valid_index_or = match c {
                    // control sequence
                    None => None,
                    // character
                    Some(c) => char_to_parameter_index(c).filter(|&n| n < num_parameters),
                };
                match valid_index_or {
                    None => {
                        // TeX.2021.479
                        input.vm().error(error::SimpleTokenError::new(
                            parameter_token,
                            "illegal parameter number",
                        ))?;
                        // Fallback to the ## case
                        input.expansions_mut().push(parameter_token);
                        push(&mut result, token);
                    }
                    Some(valid_index) => {
                        result.push(texmacro::Replacement::Parameter(valid_index));
                    }
                }
                continue;
            }
            _ => {}
        }

        push(&mut result, token);
    }
}

#[derive(Debug)]
struct InvalidParameterNumberError {
    parameter_number_token: token::Token,
    parameters_so_far: usize,
}

impl error::TexError for InvalidParameterNumberError {
    fn kind(&self) -> error::Kind {
        error::Kind::Token(self.parameter_number_token)
    }

    fn title(&self) -> String {
        "unexpected parameter".to_string()
    }

    fn notes(&self) -> Vec<error::display::Note> {
        vec![format![
            "this macro has {} parameter(s) so far, so parameter number #{} was expected.",
            self.parameters_so_far,
            self.parameters_so_far + 1
        ]
        .into()]
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use super::*;
    use crate::prefix;

    #[derive(Default, serde::Serialize, serde::Deserialize)]
    struct State {
        prefix: prefix::Component,
        testing: texlang_testing::TestingComponent,
    }

    implement_has_component![State {
        prefix: prefix::Component,
        testing: texlang_testing::TestingComponent,
    }];

    impl TexlangState for State {
        fn variable_assignment_scope_hook(
            state: &mut Self,
        ) -> texcraft_stdext::collections::groupingmap::Scope {
            prefix::variable_assignment_scope_hook(state)
        }
        fn recoverable_error_hook(
            &self,
            recoverable_error: error::TracedError,
        ) -> Result<(), Box<dyn error::TexError>> {
            texlang_testing::TestingComponent::recoverable_error_hook(self, recoverable_error)
        }
    }

    fn built_in_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        HashMap::from([
            ("def", get_def()),
            ("gdef", get_gdef()),
            ("global", prefix::get_global()),
            ("assertGlobalIsFalse", prefix::get_assert_global_is_false()),
        ])
    }

    texlang_testing::test_suite![
        @option(texlang_testing::TestOption::BuiltInCommands(built_in_commands)),
        @option(texlang_testing::TestOption::AllowUndefinedCommands(true)),
        expansion_equality_tests(
            (def_parsed_successfully, r"\def\A{abc}", ""),
            (output_is_correct, r"\def\A{abc}\A", "abc"),
            (active_char, r"\def~{abc}~", "abc"),
            (output_twice, "\\def\\A{abc}\\A\\A", "abcabc"),
            (parse_one_parameter, "\\def\\A#1{a-#1-b}", ""),
            (one_undelimited_parameter, "\\def\\A#1{a-#1-b}\\A1", "a-1-b"),
            (
                one_undelimited_parameter_multiple_times,
                "\\def\\A#1{#1 #1 #1}\\A1",
                "1 1 1"
            ),
            (
                one_undelimited_parameter_multiple_tokens,
                "\\def\\A#1{a-#1-b}\\A{123}",
                "a-123-b"
            ),
            (
                two_undelimited_parameters,
                "\\def\\A#1#2{#2-#1}\\A56",
                "6-5"
            ),
            (
                two_undelimited_parameters_multiple_token_inputs,
                "\\def\\A#1#2{#2-#1}\\A{abc}{xyz}",
                "xyz-abc"
            ),
            (
                consume_prefix_correctly,
                "\\def\\A fgh{567}\\A fghi",
                "567i"
            ),
            (
                one_undelimited_parameter_with_prefix,
                "\\def\\A abc#1{y#1z}\\A abcdefg",
                "ydzefg"
            ),
            (
                one_undelimited_parameter_with_prefix_multiple_tokens,
                "\\def\\A abc#1{y#1z}\\A abcdefg",
                "ydzefg"
            ),
            (
                one_delimited_parameter,
                "\\def\\A #1xxx{y#1z}\\A abcxxx",
                "yabcz"
            ),
            (
                one_delimited_parameter_empty,
                "\\def\\A #1xxx{y#1z}\\A xxx",
                "yz"
            ),
            (
                one_delimited_parameter_with_scope,
                "\\def\\A #1xxx{#1}\\A abc{123xxx}xxx",
                "abc{123xxx}"
            ),
            (
                one_delimited_parameter_with_prefix,
                "\\def\\A a#1c{x#1y}\\A abcdef",
                "xbydef"
            ),
            (
                two_delimited_parameters_with_prefix,
                r"\def\A a#1c#2e{x#2y#1z}\A abcdef",
                "xdybzf"
            ),
            (
                one_delimited_parameter_grouped_value,
                r"\def\A #1c{x#1y}\A {Hello}c",
                "xHelloy"
            ),
            (
                parameter_brace_special_case,
                r"\def\A #{Mint says }\A{hello}",
                "Mint says {hello}"
            ),
            (
                grouping,
                r"\def\A{Hello}\A{\def\A{World}\A}\A",
                r"HelloWorldHello"
            ),
            (
                grouping_global,
                r"\def\A{Hello}\A{\global\def\A{World}\A}\A",
                r"HelloWorldWorld"
            ),
            (
                gdef,
                r"\def\A{Hello}\A{\gdef\A{World}\A}\A",
                r"HelloWorldWorld"
            ),
            (
                gdef_global,
                r"\def\A{Hello}\A{\global\gdef\A{World}\A}\A",
                r"HelloWorldWorld"
            ),
            (
                def_takes_global,
                r"\global\def\A{Hello}\assertGlobalIsFalse",
                r""
            ),
            (
                gdef_takes_global,
                r"\global\gdef\A{Hello}\assertGlobalIsFalse",
                r""
            ),
            (
                texbook_exercise_20_1,
                r"\def\mustnt{I must not talk in class.}%
          \def\five{\mustnt\mustnt\mustnt\mustnt\mustnt}%
          \def\twenty{\five\five\five\five}%
          \def\punishment{\twenty\twenty\twenty\twenty\twenty}%
          \punishment",
                "I must not talk in class.".repeat(100)
            ),
            (
                texbook_exercise_20_2,
                r"\def\a{\b}%
          \def\b{A\def\a{B\def\a{C\def\a{\b}}}}%
          \def\puzzle{\a\a\a\a\a}%
          \puzzle",
                "ABCAB"
            ),
            (
                texbook_exercise_20_3_part_1,
                "\\def\\row#1{(#1_1,\\ldots,#1_n)}\\row{\\bf x}",
                "(\\bf x_1,\\ldots,\\bf x_n)"
            ),
            (
                texbook_exercise_20_3_part_2,
                "\\def\\row#1{(#1_1,\\ldots,#1_n)}\\row{{\\bf x}}",
                "({\\bf x}_1,\\ldots,{\\bf x}_n)"
            ),
            (
                texbook_exercise_20_4_part_1,
                r#"\def\mustnt#1#2{I must not #1 in #2.}%
           \def\five#1#2{\mustnt{#1}{#2}\mustnt{#1}{#2}\mustnt{#1}{#2}\mustnt{#1}{#2}\mustnt{#1}{#2}}%
           \def\twenty#1#2{\five{#1}{#2}\five{#1}{#2}\five{#1}{#2}\five{#1}{#2}}%
           \def\punishment#1#2{\twenty{#1}{#2}\twenty{#1}{#2}\twenty{#1}{#2}\twenty{#1}{#2}\twenty{#1}{#2}}%
           \punishment{run}{the halls}"#,
                "I must not run in the halls.".repeat(100)
            ),
            (
                texbook_exercise_20_4_part_2,
                r#"\def\mustnt{I must not \doit\ in \thatplace.}%
           \def\five{\mustnt\mustnt\mustnt\mustnt\mustnt}%
           \def\twenty{\five\five\five\five}%
           \def\punishment#1#2{\def\doit{#1}\def\thatplace{#2}\twenty\twenty\twenty\twenty\twenty}%
           \punishment{run}{the halls}"#,
                r"I must not run\ in the halls.".repeat(100)
            ),
            (
                texbook_exercise_20_5,
                r"\def\a#1{\def\b##1{##1#1}}\a!\b{Hello}",
                "Hello!"
            ),
            (
                texbook_exercise_20_5_temp,
                r"\def\b#1{#1!}\b{Hello}",
                "Hello!"
            ),
            (
                texbook_exercise_20_5_example_below,
                "\\def\\a#1#{\\hbox to #1}\\a3pt{x}",
                "\\hbox to 3pt{x}"
            ),
            (
                texbook_exercise_20_6,
                r"\def\b#1{And #1, World!}\def\a#{\b}\a{Hello}",
                "And Hello, World!"
            ),
            (
                space_in_undelimited_param_1,
                r"\def\Hello#1#2{Hello-#1-#2-World}\Hello A B C",
                r"Hello-A-B-World C",
            ),
            (
                space_in_undelimited_param_2,
                r"\def\Space{ }\def\Hello#1#2{Hello-#1-#2-World}\Hello\Space B C",
                r"Hello- -B-World C",
            ),
        ),
        serde_tests((
            serde_basic,
            r"\def\helloWorld{Hello World} ",
            r"\helloWorld"
        ),),
        end_of_input_error_tests(
            (end_of_input_scanning_target, r"\def"),
            (end_of_input_scanning_argument_text, r"\def\A"),
            (end_of_input_scanning_replacement, r"\def\A{"),
            (end_of_input_scanning_nested_replacement, r"\def\A{{}"),
            // TODO (end_of_input_reading_parameter_number, r"\def\A#"),
            (end_of_input_scanning_argument, r"\def\A#1{X-#1-Z}\A Y{}\A"),
            (
                end_of_input_reading_value_for_parameter,
                r"\def\A#1{#1}\A{correct}\A{this {is parameter 1 but it never ends}",
            ),
            // TODO (end_of_input_reading_prefix, r"\def\A abc{def}\A abc\A ab"),
            (
                end_of_input_reading_delimiter,
                r"\def\A #1abc{#1}\A xyzabc\A {first parameter}ab",
            ),
        ),
        recoverable_failure_tests(
            (bad_token_target, r"\def a other stuff{}Hello", "Hello"),
            (unexpected_token_argument, r"\def\A{Hello}\def\A }\A", ""),
            (wrong_parameter_number_1, r"\def\A #2X{-#1-}\A Y2X", "-Y-"),
            (wrong_parameter_number_2, r"\def\A #GX{-#1-}\A YGX", "-Y-"),
            (wrong_parameter_number_3, r"\def\A #\def{-#1-}\A Y\def", "-Y-"),
            (unexpected_end_group, r"\def\A{M}\def\A X}\A XY", r"Y"),
            (too_many_parameters, r"\def\A#1#2#3#4#5#6#7#8#9#0{#9#8#7#6#5#4#3#2#1}\A abcdefghi", "ihgfedcba"),
            (
                invalid_parameter_in_replacement_1,
                // invalid #1 becomes ##1
                r"\def\A{\def\B##1{-#1-}}\A{}\B C",
                "-C-",
            ),
            (
                invalid_parameter_in_replacement_2,
                // invalid #A becomes ##A
                // we don't invoke the macro because this would output #A which is invalid.
                // We instead just verify that the macro definition ends where it should.
                r"\def\A{\def\B##1{-#A-}}Hello",
                "Hello",
            ),
            (
                invalid_parameter_in_replacement_3,
                // invalid #\cs becomes ##\cs
                // we don't invoke the macro because this would output #A which is invalid.
                // We instead just verify that the macro definition ends where it should.
                r"\def\A{\def\B##1{-#\cs-}}Hello",
                "Hello",
            ),
            (prefix_does_not_match, r"\def\A abc{d}\A abdef", "ef"),
        ),
    ];
}
