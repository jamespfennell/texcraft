//! Primitives for creating user-defined macros (`\def` and friends).

use crate::algorithms::substringsearch::KMPMatcherFactory;
use crate::datastructures::nevec::Nevec;
use crate::tex::error;
use crate::tex::parse;
use crate::tex::prelude::*;
use crate::tex::token::catcode;
use crate::tex::token::stream;

use crate::tex::macrotypes::*;
use std::rc;

const DEF_DOC: &str = "Define a custom macro";

/// Get the `\def` command.
pub fn get_def<S>() -> command::ExecutionPrimitive<S> {
    command::ExecutionPrimitive {
        call_fn: def_primitive_fn,
        docs: DEF_DOC,
        id: None,
    }
}

fn def_primitive_fn<S>(def_token: Token, input: &mut ExecutionInput<S>) -> anyhow::Result<()> {
    let name =
        parse::parse_command_target("macro definition", def_token, input.unexpanded_stream())?;
    let (prefix, raw_parameters, replacement_end_token) =
        parse_prefix_and_parameters(input.unexpanded_stream())?;
    let parameters: Vec<Parameter> = raw_parameters
        .into_iter()
        .map(|a| match a {
            RawParameter::Undelimited => Parameter::Undelimited,
            RawParameter::Delimited(vec) => Parameter::Delimited(KMPMatcherFactory::new(vec)),
        })
        .collect();
    let replacement = parse_replacement_text(
        input.unexpanded_stream(),
        replacement_end_token,
        parameters.len(),
    )?;
    let user_defined_macro = Macro::new(prefix, parameters, replacement);
    input
        .base_mut()
        .set_command_2(name, rc::Rc::new(user_defined_macro));
    Ok(())
}

/// Add all of the commands defined in this module to the provided state.
pub fn add_all_commands<S>(s: &mut Base<S>) {
    s.set_command("def", get_def());
}

enum RawParameter {
    Undelimited,
    Delimited(Nevec<Token>),
}

impl RawParameter {
    fn push(&mut self, t: Token) {
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

fn parse_prefix_and_parameters(
    input: &mut dyn stream::Stream,
) -> anyhow::Result<(Vec<Token>, Vec<RawParameter>, Option<Token>)> {
    let mut prefix = Vec::new();
    let mut parameters = Vec::new();
    let mut replacement_end_token = None;

    while let Some(token) = input.next()? {
        match token.value() {
            Character(_, catcode::CatCode::BeginGroup) => {
                return Ok((prefix, parameters, replacement_end_token));
            }
            Character(_, catcode::CatCode::EndGroup) => {
                return Err(error::TokenError::new(
                    token,
                    "unexpected end group token while parsing the parameter of a macro definition",
                )
                .cast());
            }
            Character(_, catcode::CatCode::Parameter) => {
                let parameter_token = match input.next()? {
                    None => {
                        return Err(error::EndOfInputError::new(
                "unexpected end of input while reading the token after a parameter token")
                .add_note("a parameter token must be followed by a single digit number, another parameter token, or a closing brace {.")
                .cast());
                    }
                    Some(token) => token,
                };
                match parameter_token.value() {
                    Character(_, catcode::CatCode::BeginGroup) => {
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
                    Character(c, _) => {
                        let parameter_index = match char_to_parameter_index(c) {
                            None => {
                                return Err(error::TokenError::new(
                                            parameter_token,
                                            "unexpected character after a parameter token")
                                    .add_note("a parameter token must be followed by a single digit number, another parameter token, or a closing brace {.")
                                    .cast());
                            }
                            Some(n) => n,
                        };
                        if parameter_index != parameters.len() {
                            return Err(error::TokenError::new(
                                parameter_token,
                                format!["unexpected parameter number {}", parameter_index+1])
                                .add_note(
                                    format!["this macro has {} parameter(s) so far, so parameter number #{} was expected.",
                                    parameters.len(), parameters.len()+1
                                    ]).cast());
                        }
                        parameters.push(RawParameter::Undelimited);
                    }
                    _ => {
                        return Err(error::TokenError::new(
                                    parameter_token,
                                    "unexpected control sequence after a parameter token")
                                    .add_note(
                                    "a parameter token must be followed by a single digit number, another parameter token, or a closing brace {.")
                                    .cast());
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
    Err(error::EndOfInputError::new(
                "unexpected end of input while reading the parameter text of a macro")
                .add_note("the parameter text of a macro must end with a closing brace { or another token with catcode 1 (begin group)")
                .cast())
}

fn parse_replacement_text(
    input: &mut dyn stream::Stream,
    opt_final_token: Option<Token>,
    num_parameters: usize,
) -> anyhow::Result<Vec<Replacement>> {
    let mut result = Vec::new();
    let mut scope_depth = 0;

    while let Some(token) = input.next()? {
        match token.value() {
            Character(_, catcode::CatCode::BeginGroup) => {
                scope_depth += 1;
            }
            Character(_, catcode::CatCode::EndGroup) => {
                if scope_depth == 0 {
                    if let Some(final_token) = opt_final_token {
                        result.push(Replacement::Token(final_token));
                    }
                    return Ok(result);
                }
                scope_depth -= 1;
            }
            Character(_, catcode::CatCode::Parameter) => {
                let parameter_token = match input.next()? {
                    None => {
                        return Err(error::EndOfInputError::new(
                            "unexpected end of input while reading a parameter number",
                        )
                        .cast());
                    }
                    Some(token) => token,
                };
                let c = match parameter_token.value() {
                    ControlSequence(..) => {
                        return Err(error::TokenError::new(
                            parameter_token,
                            "unexpected character while reading a parameter number",
                        )
                        .add_note("expected a number between 1 and 9 inclusive")
                        .cast());
                    }
                    Character(_, catcode::CatCode::Parameter) => {
                        result.push(Replacement::Token(parameter_token));
                        continue;
                    }
                    Character(c, _) => c,
                };

                let parameter_index = match char_to_parameter_index(c) {
                    None => {
                        return Err(error::TokenError::new(
                            parameter_token,
                            "unexpected character while reading a parameter number",
                        )
                        .add_note("expected a number between 1 and 9 inclusive")
                        .cast())
                    }
                    Some(n) => n,
                };
                if parameter_index >= num_parameters {
                    return Err(error::TokenError::new(
                        parameter_token,
                        "unexpected character while reading a parameter number",
                    )
                    .cast());

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
                result.push(Replacement::Parameter(parameter_index));
                continue;
            }
            _ => {}
        }
        result.push(Replacement::Token(token));
    }

    Err(
        error::EndOfInputError::new("unexpected end of input while reading a parameter number")
            .cast(),
    )
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::tex::driver;
    use crate::tex::input;
    use crate::tex::token::catcode;

    struct State;
    fn new_state() -> State {
        State {}
    }

    fn setup_expansion_test(s: &mut Base<State>) {
        add_all_commands(s);
    }

    expansion_test![def_parsed_succesfully, "\\def\\A{abc}", ""];
    expansion_test![output_is_correct, "\\def\\A{abc}\\A", "abc"];
    expansion_test![output_twice, "\\def\\A{abc}\\A\\A", "abcabc"];
    expansion_test![parse_one_parameter, "\\def\\A#1{a-#1-b}", ""];

    expansion_test![one_undelimited_parameter, "\\def\\A#1{a-#1-b}\\A1", "a-1-b"];

    expansion_test![
        one_undelimited_parameter_multiple_times,
        "\\def\\A#1{#1 #1 #1}\\A1",
        "1 1 1"
    ];

    expansion_test![
        one_undelimited_parameter_multiple_tokens,
        "\\def\\A#1{a-#1-b}\\A{123}",
        "a-123-b"
    ];

    expansion_test![
        two_undelimited_parameters,
        "\\def\\A#1#2{#2-#1}\\A56",
        "6-5"
    ];

    expansion_test![
        two_undelimited_parameters_multiple_token_inputs,
        "\\def\\A#1#2{#2-#1}\\A{abc}{xyz}",
        "xyz-abc"
    ];

    expansion_test![
        consume_prefix_correctly,
        "\\def\\A fgh{567}\\A fghi",
        "567i"
    ];

    expansion_test![
        one_undelimited_parameter_with_prefix,
        "\\def\\A abc#1{y#1z}\\A abcdefg",
        "ydzefg"
    ];

    expansion_test![
        one_undelimited_parameter_with_prefix_multiple_tokens,
        "\\def\\A abc#1{y#1z}\\A abcdefg",
        "ydzefg"
    ];

    expansion_test![
        one_delimited_parameter,
        "\\def\\A #1xxx{y#1z}\\A abcxxx",
        "yabcz"
    ];

    expansion_test![
        one_delimited_parameter_empty,
        "\\def\\A #1xxx{y#1z}\\A xxx",
        "yz"
    ];

    expansion_test![
        one_delimited_parameter_with_scope,
        "\\def\\A #1xxx{#1}\\A abc{123xxx}xxx",
        "abc{123xxx}"
    ];

    expansion_test![
        one_delimited_parameter_with_prefix,
        "\\def\\A a#1c{x#1y}\\A abcdef",
        "xbydef"
    ];

    expansion_test![
        two_delimited_parameters_with_prefix,
        r"\def\A a#1c#2e{x#2y#1z}\A abcdef",
        "xdybzf"
    ];

    expansion_test![
        one_delimited_parameter_grouped_value,
        r"\def\A #1c{x#1y}\A {Hello}c",
        "xHelloy"
    ];

    expansion_test![
        parameter_brace_special_case,
        r"\def\A #{Mint says }\A{hello}",
        "Mint says {hello}"
    ];

    expansion_test![
        texbook_exercise_20_1,
        r"\def\mustnt{I must not talk in class.}%
          \def\five{\mustnt\mustnt\mustnt\mustnt\mustnt}%
          \def\twenty{\five\five\five\five}%
          \def\punishment{\twenty\twenty\twenty\twenty\twenty}%
          \punishment",
        "I must not talk in class.".repeat(100)
    ];

    expansion_test![
        texbook_exercise_20_2,
        r"\def\a{\b}%
          \def\b{A\def\a{B\def\a{C\def\a{\b}}}}%
          \def\puzzle{\a\a\a\a\a}%
          \puzzle",
        "ABCAB"
    ];
    expansion_test![
        texbook_exercise_20_3_part_1,
        "\\def\\row#1{(#1_1,\\ldots,#1_n)}\\row{\\bf x}",
        "(\\bf x_1,\\ldots,\\bf x_n)"
    ];

    expansion_test![
        texbook_exercise_20_3_part_2,
        "\\def\\row#1{(#1_1,\\ldots,#1_n)}\\row{{\\bf x}}",
        "({\\bf x}_1,\\ldots,{\\bf x}_n)"
    ];

    expansion_test![
        texbook_exercise_20_4_part_1,
        r#"\def\mustnt#1#2{I must not #1 in #2.}%
           \def\five#1#2{\mustnt{#1}{#2}\mustnt{#1}{#2}\mustnt{#1}{#2}\mustnt{#1}{#2}\mustnt{#1}{#2}}%
           \def\twenty#1#2{\five{#1}{#2}\five{#1}{#2}\five{#1}{#2}\five{#1}{#2}}%
           \def\punishment#1#2{\twenty{#1}{#2}\twenty{#1}{#2}\twenty{#1}{#2}\twenty{#1}{#2}\twenty{#1}{#2}}%
           \punishment{run}{the halls}"#,
        "I must not run in the halls.".repeat(100)
    ];

    expansion_test![
        texbook_exercise_20_4_part_2,
        r#"\def\mustnt{I must not \doit\ in \thatplace.}%
           \def\five{\mustnt\mustnt\mustnt\mustnt\mustnt}%
           \def\twenty{\five\five\five\five}%
           \def\punishment#1#2{\def\doit{#1}\def\thatplace{#2}\twenty\twenty\twenty\twenty\twenty}%
           \punishment{run}{the halls}"#,
        r"I must not run\ in the halls.".repeat(100)
    ];

    expansion_test![
        texbook_exercise_20_5,
        r"\def\a#1{\def\b##1{##1#1}}\a!\b{Hello}",
        "Hello!"
    ];

    expansion_test![
        texbook_exercise_20_5_temp,
        r"\def\b#1{#1!}\b{Hello}",
        "Hello!"
    ];

    expansion_test![
        texbook_exercise_20_5_example_below,
        "\\def\\a#1#{\\hbox to #1}\\a3pt{x}",
        "\\hbox to 3pt{x}"
    ];

    expansion_test![
        texbook_exercise_20_6,
        r"\def\b#1{And #1, World!}\def\a#{\b}\a{Hello}",
        "And Hello, World!"
    ];

    /* TODO: renable using \catcode
    fn setup_texbook_exercise_20_7<S: TexState<S>>(s: &mut S) {
        setup_expansion_test(s);
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
            catcode::RawCatCode::Regular(catcode::CatCode::Parameter),
        );
    }

    expansion_test![
        texbook_exercise_20_7,
        "\\def\\!!1#2![{!#]#!!2}\\! x{[y]][z}",
        "{#]![y][z}",
        setup_texbook_exercise_20_7
    ];
    */

    expansion_failure_test![end_of_input_scanning_target, "\\def"];
    expansion_failure_test![end_of_input_scanning_argument_text, "\\def\\A"];
    expansion_failure_test![end_of_input_scanning_replacement, "\\def\\A{"];
    expansion_failure_test![end_of_input_scanning_nested_replacement, "\\def\\A{{}"];
    expansion_failure_test![end_of_input_reading_parameter_number, "\\def\\A#"];
    expansion_failure_test![end_of_input_scanning_argument, "\\def\\A#1{} \\A"];
    expansion_failure_test![
        end_of_input_reading_value_for_parameter,
        "\\def\\A#1{} \\A{this {is parameter 1 but it never ends}"
    ];
    expansion_failure_test![end_of_input_reading_prefix, "\\def\\A abc{} \\A ab"];
    expansion_failure_test![
        end_of_input_reading_delimiter,
        "\\def\\A #1abc{} \\A {first parameter}ab"
    ];

    expansion_failure_test![unexpected_token_target, "\\def a"];
    expansion_failure_test![unexpected_token_argument, "\\def\\A }"];
    expansion_failure_test![unexpected_token_parameter_number, "\\def\\A #a}"];
    expansion_failure_test![unexpected_parameter_number_in_argument, "\\def\\A #2{}"];
    expansion_failure_test![unexpected_parameter_token_in_replacement, "\\def\\A #1{#a}"];
    expansion_failure_test![unexpected_parameter_number_in_replacement, "\\def\\A {#2}"];
    expansion_failure_test![
        unexpected_parameter_number_in_replacement_2,
        "\\def\\A #1{#2}"
    ];
    expansion_failure_test![unexpected_token_in_prefix, "\\def\\A abc{d} \\A abd"];
}
