//! Primitives for creating user-defined macros (`\def` and friends).

use crate::prefix;
use std::rc;
use texcraft_stdext::algorithms::substringsearch::KMPMatcherFactory;
use texcraft_stdext::collections::groupingmap;
use texcraft_stdext::collections::nevec::Nevec;
use texcraft_stdext::nevec;
use texlang_core::error;
use texlang_core::parse;
use texlang_core::prelude::*;
use texlang_core::texmacro::*;

pub const DEF_DOC: &str = "Define a custom macro";

/// Get the `\def` command.
pub fn get_def<S: HasComponent<prefix::Component>>() -> command::Command<S> {
    command::Command::new_execution(def_primitive_fn).with_id(def_id())
}

/// Get the `\gdef` command.
pub fn get_gdef<S: HasComponent<prefix::Component>>() -> command::Command<S> {
    command::Command::new_execution(gdef_primitive_fn).with_id(def_id())
}

struct Def;

pub fn def_id() -> std::any::TypeId {
    std::any::TypeId::of::<Def>()
}

fn def_primitive_fn<S: HasComponent<prefix::Component>>(
    def_token: Token,
    input: &mut vm::ExecutionInput<S>,
) -> anyhow::Result<()> {
    parse_and_set_macro(def_token, input, false)
}

fn gdef_primitive_fn<S: HasComponent<prefix::Component>>(
    def_token: Token,
    input: &mut vm::ExecutionInput<S>,
) -> anyhow::Result<()> {
    parse_and_set_macro(def_token, input, true)
}

fn parse_and_set_macro<S: HasComponent<prefix::Component>>(
    def_token: Token,
    input: &mut vm::ExecutionInput<S>,
    set_globally_override: bool,
) -> anyhow::Result<()> {
    let mut scope = input.state_mut().component_mut().read_and_reset_global();
    if set_globally_override {
        scope = groupingmap::Scope::Global;
    }
    let name = parse::parse_command_target("macro definition", def_token, input.unexpanded())?;
    let (prefix, raw_parameters, replacement_end_token) =
        parse_prefix_and_parameters(input.unexpanded())?;
    let parameters: Vec<Parameter> = raw_parameters
        .into_iter()
        .map(|a| match a {
            RawParameter::Undelimited => Parameter::Undelimited,
            RawParameter::Delimited(vec) => Parameter::Delimited(KMPMatcherFactory::new(vec)),
        })
        .collect();
    let mut replacement =
        parse_replacement_text(input.unexpanded(), replacement_end_token, parameters.len())?;
    for r in replacement.iter_mut() {
        if let Replacement::Tokens(tokens) = r {
            tokens.reverse();
        }
    }
    let user_defined_macro = Macro::new(prefix, parameters, replacement);
    input
        .base_mut()
        .commands_map
        .insert(name, rc::Rc::new(user_defined_macro).into(), scope);
    Ok(())
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
    input: &mut dyn TokenStream,
) -> anyhow::Result<(Vec<Token>, Vec<RawParameter>, Option<Token>)> {
    let mut prefix = Vec::new();
    let mut parameters = Vec::new();
    let mut replacement_end_token = None;

    while let Some(token) = input.next()? {
        match token.value() {
            Value::BeginGroup(_) => {
                return Ok((prefix, parameters, replacement_end_token));
            }
            Value::EndGroup(_) => {
                return Err(error::TokenError::new(
                    token,
                    "unexpected end group token while parsing the parameter of a macro definition",
                )
                .cast());
            }
            Value::Parameter(_) => {
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
                    Value::BeginGroup(_) => {
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
                    ControlSequence(..) => {
                        return Err(error::TokenError::new(
                                    parameter_token,
                                    "unexpected control sequence after a parameter token")
                                    .add_note(
                                    "a parameter token must be followed by a single digit number, another parameter token, or a closing brace {.")
                                    .cast());
                    }
                    _ => {
                        let c = parameter_token.char().unwrap();
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
    input: &mut dyn TokenStream,
    opt_final_token: Option<Token>,
    num_parameters: usize,
) -> anyhow::Result<Vec<Replacement>> {
    // TODO: could we use a pool of vectors to avoid some of the allocations here?
    let mut result = vec![];
    let mut scope_depth = 0;
    let push = |result: &mut Vec<Replacement>, token| match result.last_mut() {
        Some(Replacement::Tokens(tokens)) => {
            tokens.push(token);
        }
        _ => {
            result.push(Replacement::Tokens(vec![token]));
        }
    };

    while let Some(token) = input.next()? {
        match token.value() {
            Value::BeginGroup(_) => {
                scope_depth += 1;
            }
            Value::EndGroup(_) => {
                if scope_depth == 0 {
                    if let Some(final_token) = opt_final_token {
                        push(&mut result, final_token);
                    }
                    return Ok(result);
                }
                scope_depth -= 1;
            }
            Value::Parameter(_) => {
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
                    Value::Parameter(_) => {
                        push(&mut result, parameter_token);
                        continue;
                    }
                    _ => parameter_token.char().unwrap(),
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

        push(&mut result, token);
    }

    Err(
        error::EndOfInputError::new("unexpected end of input while reading a parameter number")
            .cast(),
    )
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use super::*;
    use crate::testutil::*;

    fn setup_expansion_test() -> HashMap<&'static str, command::Command<State>> {
        HashMap::from([
            ("def", get_def()),
            ("gdef", get_gdef()),
            ("global", prefix::get_global()),
            ("assertGlobalIsFalse", prefix::get_assert_global_is_false()),
        ])
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
        grouping,
        r"\def\A{Hello}\A{\def\A{World}\A}\A",
        r"HelloWorldHello"
    ];

    expansion_test![
        grouping_global,
        r"\def\A{Hello}\A{\global\def\A{World}\A}\A",
        r"HelloWorldWorld"
    ];

    expansion_test![
        gdef,
        r"\def\A{Hello}\A{\gdef\A{World}\A}\A",
        r"HelloWorldWorld"
    ];

    expansion_test![
        gdef_global,
        r"\def\A{Hello}\A{\global\gdef\A{World}\A}\A",
        r"HelloWorldWorld"
    ];

    expansion_test![
        def_takes_global,
        r"\global\def\A{Hello}\assertGlobalIsFalse",
        r""
    ];

    expansion_test![
        gdef_takes_global,
        r"\global\gdef\A{Hello}\assertGlobalIsFalse",
        r""
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
