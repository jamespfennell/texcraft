//! Types used in the TeX macro system.

use crate::algorithms::substringsearch::KMPMatcherFactory;
use crate::tex::error;
use crate::tex::parse;
use crate::tex::prelude::*;
use crate::tex::token::catcode;
use crate::tex::token::stream;
use crate::tex::token::write_tokens;
use crate::tex::token::CsNameInterner;
use crate::tex::token::Token;
use colored::*;

/// Texcraft internal representation of a TeX macro.
pub struct Macro {
    prefix: Vec<Token>,
    parameters: Vec<Parameter>,
    replacement_text: Vec<Replacement>,
}

pub enum Replacement {
    Token(Token),
    Parameter(usize),
}

pub enum Parameter {
    Undelimited,
    Delimited(KMPMatcherFactory<Token>),
}

impl<S> command::ExpansionGeneric<S> for Macro {
    fn call(
        &self,
        token: Token,
        input: &mut ExpansionInput<S>,
    ) -> anyhow::Result<stream::VecStream> {
        stream::remove_tokens_from_stream(
            &self.prefix,
            input.unexpanded_stream(),
            "matching the prefix for a user-defined macro",
        )?;
        let mut arguments = Vec::with_capacity(self.parameters.len());
        for (i, argument) in self.parameters.iter().enumerate() {
            arguments.push(argument.parse_argument(&token, input.unexpanded_stream(), i)?);
        }
        let result = Macro::perform_replacement(&self.replacement_text, &arguments);

        if input.base().tracing_macros > 0 {
            println![" +---[ Tracing macro expansion of {} ]--+", token];
            for (i, argument) in arguments.iter().enumerate() {
                println![
                    " | {}{}={}",
                    "#".bright_yellow().bold(),
                    (i + 1).to_string().bright_yellow().bold(),
                    write_tokens(argument, &input.base().cs_names).bright_yellow()
                ]
            }
            println![
                " | Expansion:\n | ```\n | {}\n | ```",
                write_tokens(&result, &input.base().cs_names)
            ];
            println![" +--------------------------------+\n"];
        }

        Ok(stream::VecStream::new(result))
    }

    fn doc(&self) -> String {
        let mut d = String::default();
        // TODO: wire up the interner here
        let interner = CsNameInterner::new();
        d.push_str("User defined macro\n\n");
        d.push_str(&format![
            "{}\n{}",
            "Parameters definition".italic(),
            pretty_print_prefix_and_parameters(&self.prefix, &self.parameters, &interner),
        ]);
        d.push_str(&format![
            "\n\n{} `{}`\n",
            "Replacement definition:".italic(),
            pretty_print_replacement_text(&self.replacement_text),
        ]);
        d
    }
}

impl Macro {
    pub fn new(
        prefix: Vec<Token>,
        parameters: Vec<Parameter>,
        replacement_text: Vec<Replacement>,
    ) -> Macro {
        Macro {
            prefix,
            parameters,
            replacement_text,
        }
    }

    fn perform_replacement(replacements: &[Replacement], arguments: &[Vec<Token>]) -> Vec<Token> {
        let mut output_size = 0;
        for replacement in replacements.iter() {
            output_size += match replacement {
                Replacement::Token(_) => 1,
                Replacement::Parameter(i) => arguments[*i].len(),
            };
        }
        let mut result = Vec::with_capacity(output_size);
        for replacement in replacements.iter() {
            match replacement {
                Replacement::Token(t) => {
                    result.push(*t);
                }
                Replacement::Parameter(i) => {
                    result.extend(arguments[*i].iter().copied());
                }
            }
        }
        result
    }
}

impl Parameter {
    pub fn parse_argument(
        &self,
        macro_token: &Token,
        stream: &mut dyn stream::Stream,
        index: usize,
    ) -> anyhow::Result<Vec<Token>> {
        match self {
            Parameter::Undelimited => {
                Parameter::parse_undelimited_argument(macro_token, stream, index + 1)
            }
            Parameter::Delimited(matcher_factory) => {
                Parameter::parse_delimited_argument(macro_token, stream, matcher_factory, index + 1)
            }
        }
    }

    fn parse_delimited_argument(
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
        let closing_scope_depth = match matcher_factory.substring().last().value() {
            Character(_, catcode::CatCode::BeginGroup) => 1,
            _ => 0,
        };
        while let Some(token) = stream.next()? {
            match token.value() {
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
                Parameter::trim_outer_braces_if_present(&mut result);
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
        match list[0].value() {
            Character(_, catcode::CatCode::BeginGroup) => (),
            _ => {
                return;
            }
        }
        match list[list.len() - 1].value() {
            Character(_, catcode::CatCode::EndGroup) => (),
            _ => {
                return;
            }
        }
        list.remove(0); // TODO: This is concerning
        list.pop();
    }

    fn parse_undelimited_argument(
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
            Some(token) => match token.value() {
                Character(_, catcode::CatCode::BeginGroup) => token,
                _ => {
                    return Ok(vec![token]);
                }
            },
        };
        match parse::parse_balanced_tokens(stream)? {
            Some(result) => Ok(result),
            None => Err(error::EndOfInputError::new(format![
                "unexpected end of input while reading argument #{} for the macro {}",
                param_num, macro_token
            ])
            .add_note(format![
            "this argument started with a `{}` and must be finished with a matching closing brace",
            &opening_brace
        ])
            .add_token_context(&opening_brace, "the argument started here:")
            .add_token_context(macro_token, "the macro invocation started here:")
            .cast()),
        }
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

pub fn pretty_print_prefix_and_parameters(
    prefix: &[Token],
    parameters: &[Parameter],
    interner: &CsNameInterner,
) -> String {
    let mut d = String::default();
    if prefix.is_empty() {
        d.push_str(" . No prefix\n");
    } else {
        d.push_str(&format![
            " . Prefix: `{}`\n",
            write_tokens(prefix, interner)
        ]);
    }

    d.push_str(&format![" . Parameters ({}):\n", parameters.len()]);
    let mut parameter_number = 1;
    for parameter in parameters {
        match parameter {
            Parameter::Undelimited => {
                d.push_str(&format![
                    "    {}: undelimited\n",
                    colored_parameter_number(parameter_number),
                ]);
            }
            Parameter::Delimited(factory) => {
                d.push_str(&format![
                    "    {}: delimited by `{}`\n",
                    colored_parameter_number(parameter_number),
                    write_tokens(factory.substring(), interner)
                ]);
            }
        }
        parameter_number += 1;
    }

    d.push_str(" . Full argument specification: `");
    d.push_str(&write_tokens(prefix, interner));
    let mut parameter_number = 1;
    for parameter in parameters {
        d.push_str(&colored_parameter_number(parameter_number));
        if let Parameter::Delimited(factory) = parameter {
            d.push_str(write_tokens(factory.substring(), interner).as_str());
        }
        parameter_number += 1;
    }
    d.push('`');
    d
}

pub fn pretty_print_replacement_text(replacements: &[Replacement]) -> String {
    let mut b = String::default();
    for replacement in replacements.iter() {
        match replacement {
            Replacement::Parameter(i) => {
                b.push_str(colored_parameter_number(*i + 1).as_str());
            }
            Replacement::Token(token) => {
                b.push_str(&format!["{}", token,]);
            }
        }
    }
    b
}
