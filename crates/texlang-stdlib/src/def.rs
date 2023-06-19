//! User-defined macros (`\def` and friends)

use crate::prefix;
use texcraft_stdext::algorithms::substringsearch::Matcher;
use texcraft_stdext::collections::groupingmap;
use texcraft_stdext::collections::nevec::Nevec;
use texcraft_stdext::nevec;
use texlang::parse::Command;
use texlang::traits::*;
use texlang::*;

pub const DEF_DOC: &str = "Define a custom macro";

/// Get the `\def` command.
pub fn get_def<S: HasComponent<prefix::Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(def_primitive_fn).with_tag(def_tag())
}

/// Get the `\gdef` command.
pub fn get_gdef<S: HasComponent<prefix::Component>>() -> command::BuiltIn<S> {
    command::BuiltIn::new_execution(gdef_primitive_fn).with_tag(def_tag())
}

static DEF_TAG: command::StaticTag = command::StaticTag::new();

pub fn def_tag() -> command::Tag {
    DEF_TAG.get()
}

fn def_primitive_fn<S: HasComponent<prefix::Component>>(
    def_token: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> Result<(), Box<error::Error>> {
    parse_and_set_macro(def_token, input, false)
}

fn gdef_primitive_fn<S: HasComponent<prefix::Component>>(
    def_token: token::Token,
    input: &mut vm::ExecutionInput<S>,
) -> Result<(), Box<error::Error>> {
    parse_and_set_macro(def_token, input, true)
}

fn parse_and_set_macro<S: HasComponent<prefix::Component>>(
    _: token::Token,
    input: &mut vm::ExecutionInput<S>,
    set_globally_override: bool,
) -> Result<(), Box<error::Error>> {
    let mut scope = input.state_mut().component_mut().read_and_reset_global();
    if set_globally_override {
        scope = groupingmap::Scope::Global;
    }
    let Command::ControlSequence(name) = Command::parse(input)?;
    let (prefix, raw_parameters, replacement_end_token) =
        parse_prefix_and_parameters(input.unexpanded())?;
    let parameters: Vec<texmacro::Parameter> = raw_parameters
        .into_iter()
        .map(|a| match a {
            RawParameter::Undelimited => texmacro::Parameter::Undelimited,
            RawParameter::Delimited(vec) => texmacro::Parameter::Delimited(Matcher::new(vec)),
        })
        .collect();
    let mut replacement =
        parse_replacement_text(input.unexpanded(), replacement_end_token, parameters.len())?;
    for r in replacement.iter_mut() {
        if let texmacro::Replacement::Tokens(tokens) = r {
            tokens.reverse();
        }
    }
    let user_defined_macro = texmacro::Macro::new(prefix, parameters, replacement);
    input
        .commands_map_mut()
        .insert_macro(name, user_defined_macro, scope);
    Ok(())
}

enum RawParameter {
    Undelimited,
    Delimited(Nevec<token::Token>),
}

impl RawParameter {
    fn push(&mut self, t: token::Token) {
        match self {
            RawParameter::Undelimited => {
                *self = RawParameter::Delimited(nevec![t]);
            }
            RawParameter::Delimited(vec) => {
                vec.push(t);
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

fn parse_prefix_and_parameters<S: TexlangState>(
    input: &mut vm::UnexpandedStream<S>,
) -> command::Result<(Vec<token::Token>, Vec<RawParameter>, Option<token::Token>)> {
    let mut prefix = Vec::new();
    let mut parameters = Vec::new();
    let mut replacement_end_token = None;

    while let Some(token) = input.next()? {
        match token.value() {
            token::Value::BeginGroup(_) => {
                return Ok((prefix, parameters, replacement_end_token));
            }
            token::Value::EndGroup(_) => {
                return Err(error::SimpleTokenError::new(
                    input.vm(),
                    token,
                    "unexpected end group token while parsing the parameter of a macro definition",
                )
                .into());
            }
            token::Value::Parameter(_) => {
                let parameter_token = match input.next()? {
                    None => {
                        return Err(error::SimpleEndOfInputError::new(input.vm(),
                "unexpected end of input while reading the token after a parameter token").into());
                        // TODO .add_note("a parameter token must be followed by a single digit number, another parameter token, or a closing brace {.")
                    }
                    Some(token) => token,
                };
                match parameter_token.value() {
                    token::Value::BeginGroup(_) => {
                        // In this case we end the group according to the special #{ rule
                        replacement_end_token = Some(parameter_token);
                        match parameters.last_mut() {
                            None => {
                                prefix.push(parameter_token);
                            }
                            Some(spec) => {
                                spec.push(parameter_token);
                            }
                        }
                        return Ok((prefix, parameters, replacement_end_token));
                    }
                    token::Value::ControlSequence(..) => {
                        return Err(error::SimpleTokenError::new(
                            input.vm(),
                            parameter_token,
                            "unexpected control sequence after a parameter token",
                        )
                        .into());
                        // TODO "a parameter token must be followed by a single digit number, another parameter token, or a closing brace {.")
                    }
                    _ => {
                        let c = parameter_token.char().unwrap();
                        let parameter_index = match char_to_parameter_index(c) {
                            None => {
                                return Err(error::SimpleTokenError::new(
                                    input.vm(),
                                    parameter_token,
                                    "unexpected character after a parameter token",
                                )
                                .into());
                                // TODO .add_note("a parameter token must be followed by a single digit number, another parameter token, or a closing brace {.")
                            }
                            Some(n) => n,
                        };
                        if parameter_index != parameters.len() {
                            return Err(error::SimpleTokenError::new(
                                input.vm(),
                                parameter_token,
                                format!["unexpected parameter number {}", parameter_index + 1],
                            )
                            .into());
                            // TODO format!["this macro has {} parameter(s) so far, so parameter number #{} was expected.",
                            //TODO parameters.len(), parameters.len()+1
                        }
                        parameters.push(RawParameter::Undelimited);
                    }
                }
            }
            _ => match parameters.last_mut() {
                None => {
                    prefix.push(token);
                }
                Some(parameter) => {
                    parameter.push(token);
                }
            },
        }
    }
    Err(error::SimpleEndOfInputError::new(
        input.vm(),
        "unexpected end of input while reading the parameter text of a macro",
    )
    .into())
    // TODO .add_note("the parameter text of a macro must end with a closing brace { or another token with catcode 1 (begin group)")
}

fn parse_replacement_text<S: TexlangState>(
    input: &mut vm::UnexpandedStream<S>,
    opt_final_token: Option<token::Token>,
    num_parameters: usize,
) -> command::Result<Vec<texmacro::Replacement>> {
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

    while let Some(token) = input.next()? {
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
                let parameter_token = match input.next()? {
                    None => {
                        return Err(error::SimpleEndOfInputError::new(
                            input.vm(),
                            "unexpected end of input while reading a parameter number",
                        )
                        .into())
                    }
                    Some(token) => token,
                };
                let c = match parameter_token.value() {
                    token::Value::ControlSequence(..) => {
                        return Err(error::SimpleTokenError::new(
                            input.vm(),
                            parameter_token,
                            "unexpected character while reading a parameter number",
                        )
                        .into());
                        // TODO .add_note("expected a number between 1 and 9 inclusive")
                    }
                    token::Value::Parameter(_) => {
                        push(&mut result, parameter_token);
                        continue;
                    }
                    _ => parameter_token.char().unwrap(),
                };

                let parameter_index = match char_to_parameter_index(c) {
                    None => {
                        return Err(error::SimpleTokenError::new(
                            input.vm(),
                            parameter_token,
                            "unexpected character while reading a parameter number",
                        )
                        .into());
                        // TODO .add_note("expected a number between 1 and 9 inclusive")
                    }
                    Some(n) => n,
                };
                if parameter_index >= num_parameters {
                    return Err(error::SimpleTokenError::new(
                        input.vm(),
                        parameter_token,
                        "unexpected character while reading a parameter number",
                    )
                    .into());

                    /* TODO
                            var msg string
                            switch numParams {
                            case 0:
                                msg = "no parameter token because this macro has 0 parameters"
                            case 1:
                                msg = "the number 1 because this macro has only 1 parameter"
                            default:
                                msg = fmt.Sprintf(
                                    "a number between 1 and %[1]d inclusive because this macro has only %[1]d parameters",
                                    numParams)
                            }
                            return nil, errors.NewUnexpectedTokenError(t, msg, "the number "+t.Value(), parsingArgumentTemplate)
                    */
                }
                result.push(texmacro::Replacement::Parameter(parameter_index));
                continue;
            }
            _ => {}
        }

        push(&mut result, token);
    }

    Err(error::SimpleEndOfInputError::new(
        input.vm(),
        "unexpected end of input while reading a parameter number",
    )
    .into())
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use super::*;
    use crate::testing::*;

    fn initial_commands() -> HashMap<&'static str, command::BuiltIn<State>> {
        HashMap::from([
            ("def", get_def()),
            ("gdef", get_gdef()),
            ("global", prefix::get_global()),
            ("assertGlobalIsFalse", prefix::get_assert_global_is_false()),
        ])
    }

    test_suite![
        expansion_equality_tests(
            (def_parsed_successfully, "\\def\\A{abc}", ""),
            (output_is_correct, "\\def\\A{abc}\\A", "abc"),
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
        ),
        serde_tests((
            serde_basic,
            r"\def\helloWorld{Hello World} ",
            r"\helloWorld"
        ),),
        failure_tests(
            (end_of_input_scanning_target, "\\def"),
            (end_of_input_scanning_argument_text, "\\def\\A"),
            (end_of_input_scanning_replacement, "\\def\\A{"),
            (end_of_input_scanning_nested_replacement, "\\def\\A{{}"),
            (end_of_input_reading_parameter_number, "\\def\\A#"),
            (end_of_input_scanning_argument, "\\def\\A#1{} \\A"),
            (
                end_of_input_reading_value_for_parameter,
                "\\def\\A#1{} \\A{this {is parameter 1 but it never ends}"
            ),
            (end_of_input_reading_prefix, "\\def\\A abc{} \\A ab"),
            (
                end_of_input_reading_delimiter,
                "\\def\\A #1abc{} \\A {first parameter}ab"
            ),
            (unexpected_token_target, "\\def a"),
            (unexpected_token_argument, "\\def\\A }"),
            (unexpected_token_parameter_number, "\\def\\A #a}"),
            (unexpected_parameter_number_in_argument, "\\def\\A #2{}"),
            (unexpected_parameter_token_in_replacement, "\\def\\A #1{#a}"),
            (unexpected_parameter_number_in_replacement, "\\def\\A {#2}"),
            (
                unexpected_parameter_number_in_replacement_2,
                "\\def\\A #1{#2}"
            ),
            (unexpected_token_in_prefix, "\\def\\A abc{d} \\A abd"),
        ),
    ];

    /* TODO: renable using \catcode
    fn setup_texbook_exercise_20_7<S: TexState<S>>(s: &mut S) {
        initial_commands(s);
        s.cat_code_map_mut().insert(
            '[' as u32,
            catcode::RawCatCode::Regular(catcode::CatCode::BeginGroup),
        );
        s.cat_code_map_mut().insert(
            ']' as u32,
            catcode::RawCatCode::Regular(catcode::CatCode::EndGroup),
        );
        s.cat_code_map_mut().insert(
            '!' as u32,
            catcode::RawCatCode::Regular(catcode::CatCode::texmacro::Parameter),
        );
    }

    expansion_test![
        texbook_exercise_20_7,
        "\\def\\!!1#2![{!#]#!!2}\\! x{[y]][z}",
        "{#]![y][z}",
        setup_texbook_exercise_20_7
    ];
    */
}
