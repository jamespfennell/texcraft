//! Primitives for creating user-defined macros (`\def` and friends).

use crate::tex::prelude::*;
use crate::tex::token::stream;
use colored::*;
use std::rc;

const DEF_DOC: &str = "Define a custom macro";

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

fn def_primitive_fn<S>(def_token: Token, input: &mut ExecutionInput<S>) -> anyhow::Result<()> {
    let name = match input.unexpanded_stream().next()? {
        None => {
            return Err(error::TokenError::new(
                def_token,
                "unexpected end of input while reading the target of a macro definition",
            )
            .cast());
        }
        Some(token) => match token.value {
            ControlSequence(_, name) => name,
            _ => {
                return Err(error::TokenError::new(
                    token,
                    "unexpected target of a macro definition",
                )
                .add_note("the target of a macro definition must be a control sequence")
                .cast());
            }
        },
    };

    let (arguments, replacement_end_token) =
        arguments::Definition::build(input.unexpanded_stream())?;

    let replacement = replacement::Definition::build(
        input.unexpanded_stream(),
        replacement_end_token,
        arguments.num_parameters(),
    )?;

    let user_defined_macro = UserDefinedMacro {
        arguments: arguments,
        replacement: replacement,
    };

    input
        .base_mut()
        .set_command(name, rc::Rc::new(user_defined_macro));

    Ok(())
}

/// Get the `\def` command.
pub fn get_def<S>() -> command::ExecutionPrimitive<S> {
    command::ExecutionPrimitive {
        call_fn: def_primitive_fn,
        docs: DEF_DOC,
        id: None,
    }
}

/// Add all of the commands defined in this module to the provided state.
pub fn add_all_commands<S>(s: &mut Base<S>) -> () {
    s.set_command("def", get_def());
}

/// Representation of a user defined macro and logic for expanding it.
pub struct UserDefinedMacro {
    arguments: arguments::Definition,
    replacement: replacement::Definition,
}

impl<S> command::ExpansionGeneric<S> for UserDefinedMacro {
    fn call(
        &self,
        token: Token,
        input: &mut ExpansionInput<S>,
    ) -> anyhow::Result<stream::VecStream> {
        let arguments = match self
            .arguments
            .scan_for_arguments(&token, input.unexpanded_stream())
        {
            Ok(a) => a,
            Err(err) => {
                return Err(error::UserDefinedMacroError::new(
                    err,
                    command::ExpansionGeneric::<S>::doc(self),
                ));
            }
        };
        let result = self.replacement.perform_replacement(arguments);
        /*
        // TODO: figure out logging
        println!["expanding macro\nParameters:"];
        for i in 0..arguments.len() {
            println![
                " {}{}={}",
                "#".bright_yellow().bold(),
                (i + 1).to_string().bright_yellow().bold(),
                writer::write_tokens(&arguments[i]).bright_yellow()
            ]
        }

        println!["Expansion: {}", writer::write_tokens(&result)];

        println!["Docs for this macro:\n{}", ExpansionGeneric::<S>::doc(self)];
        */
        Ok(stream::VecStream::new(result))
    }

    fn doc(&self) -> String {
        let mut d = String::default();
        d.push_str(&format!["User defined macro\n\n",]);
        d.push_str(&format![
            "{}\n{}",
            "Argument definition".italic(),
            self.arguments.pretty_print()
        ]);
        d.push_str(&format![
            "\n\n{} `{}`\n",
            "Replacement definition:".italic(),
            self.replacement.pretty_print()
        ]);
        d
    }
}

fn colored_parameter_number(n: usize) -> String {
    let color = match n {
        1 => |s: String| s.bright_yellow(),
        _ => |s: String| s.bright_blue(),
    };
    format![
        "{}{}",
        color("#".to_string()).bold(),
        color(n.to_string()).bold()
    ]
}

/// Logic for handling arguments in user defined macros.
mod arguments {

    use super::char_to_parameter_index;
    use super::colored_parameter_number;
    use crate::algorithms::substringsearch::KMPMatcherFactory;
    use crate::datastructures::nevec::Nevec;
    use crate::tex::prelude::*;
    use crate::tex::token::catcode;
    use crate::tex::token::write_tokens;

    pub struct Definition {
        prefix: Vec<Token>,
        parameters: Vec<PreparedParameter>,
    }

    enum PreparedParameter {
        Undelimited,
        Delimited(KMPMatcherFactory<Token>),
    }

    enum Parameter {
        Undelimited,
        Delimited(Nevec<Token>),
    }

    impl Parameter {
        fn push(&mut self, t: Token) {
            match self {
                Parameter::Undelimited => {
                    *self = Parameter::Delimited(nevec![t]);
                }
                Parameter::Delimited(vec) => {
                    vec.push(t);
                }
            }
        }
    }

    impl Definition {
        pub fn build(
            input: &mut dyn stream::Stream,
        ) -> anyhow::Result<(Definition, Option<Token>)> {
            let mut prefix = Vec::new();
            let mut parameters = Vec::new();
            let mut replacement_end_token = None;

            while let Some(token) = input.next()? {
                match token.value {
                    Character(_, catcode::CatCode::BeginGroup) => {
                        return Ok((
                            Definition::build_post_process(prefix, parameters),
                            replacement_end_token,
                        ));
                    }
                    Character(_, catcode::CatCode::EndGroup) => {
                        return Err(error::TokenError::new(
                    token,
                    "unexpected end group token while parsing the parameter of a macro definition").cast());
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
                        match parameter_token.value {
                            Character(_, catcode::CatCode::BeginGroup) => {
                                // In this case we end the group according to the special #{ rule
                                replacement_end_token = Some(parameter_token.clone());
                                match parameters.last_mut() {
                                    None => {
                                        prefix.push(parameter_token);
                                    }
                                    Some(spec) => {
                                        spec.push(parameter_token);
                                    }
                                }
                                return Ok((
                                    Definition::build_post_process(prefix, parameters),
                                    replacement_end_token,
                                ));
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
                                format!["unexpected parameter number {}", parameter_index+0])
                                .add_note(
                                    format!["this macro has {} parameter(s) so far, so parameter number #{} was expected.",
                                    parameters.len(), parameters.len()+1
                                    ]).cast());
                                }
                                parameters.push(Parameter::Undelimited);
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
                        Some(spec) => {
                            spec.push(token);
                        }
                    },
                }
            }
            Err(error::EndOfInputError::new(
                "unexpected end of input while reading the parameter text of a macro")
                .add_note("the parameter text of a macro must end with a closing brace { or another token with catcode 1 (begin group)")
                .cast())
        }

        fn build_post_process(prefix: Vec<Token>, parameters: Vec<Parameter>) -> Definition {
            let processed_parameters: Vec<PreparedParameter> = parameters
                .into_iter()
                .map(|a| match a {
                    Parameter::Undelimited => PreparedParameter::Undelimited,
                    Parameter::Delimited(vec) => {
                        PreparedParameter::Delimited(KMPMatcherFactory::new(vec))
                    }
                })
                .collect();
            Definition {
                prefix,
                parameters: processed_parameters,
            }
        }

        pub fn num_parameters(&self) -> usize {
            self.parameters.len()
        }

        pub fn scan_for_arguments(
            &self,
            macro_token: &Token,
            stream: &mut dyn stream::Stream,
        ) -> anyhow::Result<Vec<Vec<Token>>> {
            self.consume_prefix(stream)?;

            let mut parameter_values = Vec::new();

            let mut index = 0;
            for parameter in self.parameters.iter() {
                parameter_values.push(match parameter {
                    PreparedParameter::Undelimited => {
                        read_undelimited_parameter(macro_token, stream, index + 1)?
                    }
                    PreparedParameter::Delimited(matcher_factory) => {
                        read_delimited_parameter(macro_token, stream, matcher_factory, index + 1)?
                    }
                });
                index += 1;
            }
            Ok(parameter_values)
        }

        fn consume_prefix(&self, stream: &mut dyn stream::Stream) -> anyhow::Result<()> {
            for prefix_token in self.prefix.iter() {
                let stream_token = match stream.next()? {
                    None => {
                        return Err(error::EndOfInputError::new(
                        "unexpected end of input while matching the argument prefix for a user-defined macro")
                        .cast());
                    }
                    Some(token) => token,
                };
                if &stream_token != prefix_token {
                    let note = match &prefix_token.value {
                        Character(c, catcode) => format![
                            "expected a character token with value '{}' and catcode {}",
                            c, catcode
                        ],
                        ControlSequence(_, name) => {
                            format!["expected a control sequence token \\{}", name]
                        }
                    };
                    return Err(error::TokenError::new(
                        stream_token,
                        "unexpected token while matching the prefix of user-defined macro",
                    )
                    .add_note(note)
                    .cast());
                }
            }
            Ok(())
        }

        pub fn pretty_print(&self) -> String {
            let mut d = String::default();
            if self.prefix.len() == 0 {
                d.push_str(&format![" . No prefix\n",]);
            } else {
                d.push_str(&format![
                    " . Prefix: `{}`\n",
                    write_tokens(&self.prefix, false)
                ]);
            }

            d.push_str(&format![" . Parameters ({}):\n", self.num_parameters()]);
            let mut parameter_number = 1;
            for parameter in &self.parameters {
                match parameter {
                    PreparedParameter::Undelimited => {
                        d.push_str(&format![
                            "    {}: undelimited\n",
                            colored_parameter_number(parameter_number),
                        ]);
                    }
                    PreparedParameter::Delimited(factory) => {
                        d.push_str(&format![
                            "    {}: delimited by `{}`\n",
                            colored_parameter_number(parameter_number),
                            write_tokens(factory.substring(), false)
                        ]);
                    }
                }
                parameter_number += 1;
            }

            d.push_str(&format![" . Full argument specification: `"]);
            d.push_str(&format!["{}", write_tokens(&self.prefix, false)]);
            let mut parameter_number = 1;
            for parameter in &self.parameters {
                d.push_str(&format!["{}", colored_parameter_number(parameter_number)]);
                if let PreparedParameter::Delimited(factory) = parameter {
                    d.push_str(&format!["{}", write_tokens(factory.substring(), false)]);
                }
                parameter_number += 1;
            }
            d.push_str(&format!["`"]);
            d
        }
    }

    fn read_delimited_parameter(
        macro_token: &Token,
        stream: &mut dyn stream::Stream,
        matcher_factory: &KMPMatcherFactory<Token>,
        param_num: usize,
    ) -> anyhow::Result<Vec<Token>> {
        let mut matcher = matcher_factory.matcher();
        let mut result = Vec::new();
        let mut scope_depth = 0;

        // This handles the case of a macro whose argument ends with the special #{ tokens. In this special case the parsing
        // will end with a scope depth of 1, because the last token parsed will be the { and all braces before that will
        // be balanced.
        let closing_scope_depth = match matcher_factory.substring().last().value {
            Character(_, catcode::CatCode::BeginGroup) => 1,
            _ => 0,
        };
        while let Some(token) = stream.next()? {
            match token.value {
                Character(_, catcode::CatCode::BeginGroup) => {
                    scope_depth += 1;
                }
                Character(_, catcode::CatCode::EndGroup) => {
                    scope_depth -= 1;
                }
                _ => (),
            };
            result.push(token);
            if scope_depth == closing_scope_depth && matcher.next(result.last().unwrap()) {
                // Remove the suffix.
                for _ in 0..matcher_factory.substring().len() {
                    result.pop();
                }
                trim_outer_braces_if_present(&mut result);
                return Ok(result);
            }
        }
        let mut e = error::EndOfInputError::new(format![
            "unexpected end of input while reading argument #{} for the macro {}",
            param_num, macro_token
        ])
        .add_note(format![
            "this argument is delimited and must be suffixed by the tokens `{}`",
            matcher_factory.substring()
        ]);
        if let Some(first_token) = result.first() {
            e = e.add_token_context(first_token, "the argument started here:");
            e = e.add_note(format![
                "{} tokens were read for the argument so far",
                result.len()
            ]);
        } else {
            e = e.add_note("no tokens were read for the argument so far");
        }
        Err(e
            .add_token_context(macro_token, "the macro invocation started here:")
            .cast())
    }

    fn trim_outer_braces_if_present(list: &mut Vec<Token>) {
        if list.len() <= 1 {
            return;
        }
        match list[0].value {
            Character(_, catcode::CatCode::BeginGroup) => (),
            _ => {
                return;
            }
        }
        match list[list.len() - 1].value {
            Character(_, catcode::CatCode::EndGroup) => (),
            _ => {
                return;
            }
        }
        list.remove(0); // TODO: This is concerning
        list.pop();
    }

    fn read_undelimited_parameter(
        macro_token: &Token,
        stream: &mut dyn stream::Stream,
        param_num: usize,
    ) -> anyhow::Result<Vec<Token>> {
        let opening_brace = match stream.next()? {
            None => {
                return Err(error::EndOfInputError::new(format![
                    "unexpected end of input while reading argument #{} for the macro {}",
                    param_num, macro_token
                ])
                .add_token_context(macro_token, "the macro invocation started here:")
                .cast());
            }
            Some(token) => match token.value {
                Character(_, catcode::CatCode::BeginGroup) => token,
                _ => {
                    return Ok(vec![token]);
                }
            },
        };
        let mut result = Vec::new();
        let mut scope_depth = 0;
        while let Some(token) = stream.next()? {
            match token.value {
                Character(_, catcode::CatCode::BeginGroup) => {
                    scope_depth += 1;
                }
                Character(_, catcode::CatCode::EndGroup) => {
                    if scope_depth == 0 {
                        return Ok(result);
                    }
                    scope_depth -= 1;
                }
                _ => (),
            }
            result.push(token);
        }
        Err(error::EndOfInputError::new(format![
            "unexpected end of input while reading argument #{} for the macro {}",
            param_num, macro_token
        ])
        .add_note(format![
            "this argument started with a `{}` and must be finished with a matching closing brace",
            &opening_brace
        ])
        .add_token_context(&opening_brace, "the argument started here:")
        .add_token_context(macro_token, "the macro invocation started here:")
        .cast())
    }
}

/// Logic for handling replacement text in user defined macros.
mod replacement {

    use super::char_to_parameter_index;
    use super::colored_parameter_number;
    use crate::tex::error;
    use crate::tex::prelude::*;
    use crate::tex::token::catcode;
    use crate::tex::token::stream;

    pub struct Definition {
        head: Vec<Token>,
        replacements: Vec<Replacement>,
    }

    pub struct Replacement {
        pub parameter_index: usize,
        pub tokens: Vec<Token>,
    }

    impl Definition {
        pub fn build(
            input: &mut dyn stream::Stream,
            opt_final_token: Option<Token>,
            num_parameters: usize,
        ) -> anyhow::Result<Definition> {
            let mut spec = Definition {
                head: Vec::new(),
                replacements: Vec::new(),
            };
            let mut scope_depth = 0;

            while let Some(token) = input.next()? {
                match token.value {
                    Character(_, catcode::CatCode::BeginGroup) => {
                        scope_depth += 1;
                    }
                    Character(_, catcode::CatCode::EndGroup) => {
                        if scope_depth == 0 {
                            if let Some(final_token) = opt_final_token {
                                spec.push(final_token);
                            }
                            return Ok(spec);
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
                        let c = match parameter_token.value {
                            ControlSequence(..) => {
                                return Err(error::TokenError::new(
                                    parameter_token,
                                    "unexpected character while reading a parameter number",
                                )
                                .add_note("expected a number between 1 and 9 inclusive")
                                .cast());
                            }
                            Character(_, catcode::CatCode::Parameter) => {
                                spec.push(parameter_token);
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
                        spec.replacements.push(Replacement {
                            parameter_index: parameter_index,
                            tokens: Vec::new(),
                        });
                        continue;
                    }
                    _ => {}
                }
                spec.push(token);
            }

            return Err(error::EndOfInputError::new(
                "unexpected end of input while reading a parameter number",
            )
            .cast());
        }

        pub fn perform_replacement(&self, arguments: Vec<Vec<Token>>) -> Vec<Token> {
            let mut output_size = self.head.len();
            for replacement in self.replacements.iter() {
                output_size += arguments[replacement.parameter_index].len();
                output_size += replacement.tokens.len();
            }
            let mut result = Vec::with_capacity(output_size);
            result.extend(self.head.iter().cloned());
            for replacement in self.replacements.iter() {
                result.extend(arguments[replacement.parameter_index].iter().cloned());
                result.extend(replacement.tokens.iter().cloned());
            }
            result
        }

        pub fn pretty_print(&self) -> String {
            let mut b = String::default();
            b.push_str(&format![
                "{}",
                crate::tex::token::write_tokens(&self.head, false)
            ]);
            for replacement in self.replacements.iter() {
                b.push_str(&format![
                    "{}",
                    colored_parameter_number(replacement.parameter_index + 1),
                ]);
                b.push_str(&format![
                    "{}",
                    crate::tex::token::write_tokens(&replacement.tokens, false)
                ]);
            }
            b
        }

        fn push(&mut self, t: Token) {
            match self.replacements.last_mut() {
                None => &mut self.head,
                Some(r) => &mut r.tokens,
            }
            .push(t);
        }
    }
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
