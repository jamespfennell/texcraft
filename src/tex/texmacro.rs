//! The TeX macro system.

use crate::algorithms::substringsearch::KMPMatcherFactory;
use crate::tex::error;
use crate::tex::parse;
use crate::tex::prelude::*;
use crate::tex::token::stream;
use crate::tex::token::write_tokens;
use crate::tex::token::CsNameInterner;
use crate::tex::token::Token;
use arrayvec::ArrayVec;
use colored::*;

/// A TeX Macro.
pub struct Macro {
    prefix: Vec<Token>,
    parameters: ArrayVec<Parameter, 9>,
    replacement_text: Vec<Replacement>,
}

/// A token list or parameter in a replacement text.
pub enum Replacement {
    /// A list of tokens.
    Tokens(Vec<Token>),

    /// A parameter.
    ///
    /// In order to be valid, the parameters index must be less than the number
    /// of parameters in the macro.
    Parameter(usize),
}

pub enum Parameter {
    Undelimited,
    Delimited(KMPMatcherFactory<Token>),
}

impl Macro {
    pub fn call<S>(&self, token: Token, input: &mut ExpandedInput<S>) -> anyhow::Result<()> {
        stream::remove_tokens_from_stream(
            &self.prefix,
            input.unexpanded_stream(),
            "matching the prefix for a user-defined macro",
        )?;
        let mut argument_indices: ArrayVec<(usize, usize), 9> = Default::default();
        let (unexpanded_stream, argument_tokens) = input.unexpanded_and_scratch_space();
        argument_tokens.clear();
        for (i, parameter) in self.parameters.iter().enumerate() {
            let start_index = argument_tokens.len();
            let trim_outer_braces =
                parameter.parse_argument(&token, unexpanded_stream, i, argument_tokens)?;
            let element = match trim_outer_braces {
                true => (start_index + 1, argument_tokens.len() - 1),
                false => (start_index, argument_tokens.len()),
            };
            unsafe {
                argument_indices.push_unchecked(element);
            }
        }

        let mut arguments: ArrayVec<&[Token], 9> = Default::default();
        for (i, j) in &argument_indices {
            let slice = unsafe { argument_tokens.get_unchecked(*i..*j) };
            unsafe {
                arguments.push_unchecked(slice);
            }
        }

        let result = unexpanded_stream.controller_mut().expansions_mut();
        Macro::perform_replacement(&self.replacement_text, &arguments, result);

        if unexpanded_stream.base().tracing_macros > 0 {
            println![" +---[ Tracing macro expansion of {} ]--+", token];
            for (i, argument) in arguments.iter().enumerate() {
                println![
                    " | {}{}={}",
                    "#".bright_yellow().bold(),
                    (i + 1).to_string().bright_yellow().bold(),
                    write_tokens(*argument, unexpanded_stream.base().cs_name_interner())
                        .bright_yellow()
                ]
            }
            println![
                " | Expansion:\n | ```\n | todo: renable this\n | ```",
                // write_tokens(result.iter(), &input.base().cs_names)
            ];
            println![" +--------------------------------+\n"];
        }
        Ok(())
    }

    pub fn doc(&self) -> String {
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

    /// Create a new macro.
    ///
    /// # Safety
    ///
    /// This constructor does not check that the replacement text is valid; i.e., that indices
    /// appearing in any [Replacement::Parameter] are less than the number of parameters. If
    /// this is not the case, undefined behavior will occur when the macro is expanded.
    pub unsafe fn new_unchecked(
        prefix: Vec<Token>,
        parameters: ArrayVec<Parameter, 9>,
        replacement_text: Vec<Replacement>,
    ) -> Macro {
        Macro {
            prefix,
            parameters,
            replacement_text,
        }
    }

    fn perform_replacement(
        replacements: &[Replacement],
        arguments: &[&[Token]],
        result: &mut Vec<Token>,
    ) {
        let mut output_size = 0;
        for replacement in replacements.iter() {
            output_size += match replacement {
                Replacement::Tokens(tokens) => tokens.len(),
                Replacement::Parameter(i) => unsafe { arguments.get_unchecked(*i) }.len(),
            };
        }
        result.reserve(output_size);
        for replacement in replacements.iter().rev() {
            match replacement {
                Replacement::Tokens(tokens) => {
                    result.extend(tokens);
                }
                Replacement::Parameter(i) => {
                    result.extend(unsafe { arguments.get_unchecked(*i) }.iter().rev().copied());
                }
            }
        }
    }
}

impl Parameter {
    pub fn parse_argument<S: stream::Stream>(
        &self,
        macro_token: &Token,
        stream: &mut S,
        index: usize,
        result: &mut Vec<Token>,
    ) -> anyhow::Result<bool> {
        match self {
            Parameter::Undelimited => {
                Parameter::parse_undelimited_argument(macro_token, stream, index + 1, result)?;
                Ok(false)
            }
            Parameter::Delimited(matcher_factory) => Parameter::parse_delimited_argument(
                macro_token,
                stream,
                matcher_factory,
                index + 1,
                result,
            ),
        }
    }

    fn parse_delimited_argument(
        macro_token: &Token,
        stream: &mut dyn stream::Stream,
        matcher_factory: &KMPMatcherFactory<Token>,
        param_num: usize,
        result: &mut Vec<Token>,
    ) -> anyhow::Result<bool> {
        let mut matcher = matcher_factory.matcher();
        let mut scope_depth = 0;

        // This handles the case of a macro whose argument ends with the special #{ tokens. In this special case the parsing
        // will end with a scope depth of 1, because the last token parsed will be the { and all braces before that will
        // be balanced.
        let closing_scope_depth = match matcher_factory.substring().last().value() {
            Value::BeginGroup(_) => 1,
            _ => 0,
        };
        let start_index = result.len();
        while let Some(token) = stream.next()? {
            match token.value() {
                Value::BeginGroup(_) => {
                    scope_depth += 1;
                }
                Value::EndGroup(_) => {
                    scope_depth -= 1;
                }
                _ => (),
            };
            let matches_delimiter = matcher.next(&token);
            result.push(token);
            if scope_depth == closing_scope_depth && matches_delimiter {
                // Remove the suffix.
                for _ in 0..matcher_factory.substring().len() {
                    result.pop();
                }
                return Ok(Parameter::should_trim_outer_braces_if_present(
                    &result[start_index..],
                ));
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

    fn should_trim_outer_braces_if_present(list: &[Token]) -> bool {
        if list.len() <= 1 {
            return false;
        }
        match list[0].value() {
            Value::BeginGroup(_) => (),
            _ => {
                return false;
            }
        }
        match list[list.len() - 1].value() {
            Value::EndGroup(_) => (),
            _ => {
                return false;
            }
        }
        true
    }

    fn parse_undelimited_argument<S: stream::Stream>(
        macro_token: &Token,
        stream: &mut S,
        param_num: usize,
        result: &mut Vec<Token>,
    ) -> anyhow::Result<()> {
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
                Value::BeginGroup(_) => token,
                _ => {
                    result.push(token);
                    return Ok(());
                }
            },
        };
        match parse::parse_balanced_tokens(stream, result)? {
            true => Ok(()),
            false => Err(error::EndOfInputError::new(format![
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
            Replacement::Tokens(_) => {
                b.push_str("TODO");
            }
        }
    }
    b
}
