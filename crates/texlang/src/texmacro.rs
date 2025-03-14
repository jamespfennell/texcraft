//! Implementation of TeX user defined macros.

use crate::error;
use crate::parse;
use crate::prelude as txl;
use crate::token;
use crate::token::Token;
use crate::token::Value;
use crate::traits::*;
use crate::vm;
use texcraft_stdext::algorithms::substringsearch::Matcher;
use texcraft_stdext::color::Colorize;

/// A TeX Macro.
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Macro {
    prefix: Vec<Token>,
    parameters: Vec<Parameter>,
    replacements: Vec<Replacement>,
}

impl Macro {
    pub fn replacements(&self) -> &[Replacement] {
        &self.replacements
    }
}

/// A token list or parameter in a replacement text.
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Replacement {
    /// A list of tokens.
    Tokens(Vec<Token>),

    /// A parameter.
    ///
    /// In order to be valid, the parameters index must be less than the number
    /// of parameters in the macro.
    Parameter(usize),
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Parameter {
    Undelimited,
    Delimited(Matcher<Value>),
}

// Input type for macro hooks
pub struct HookInput<'a, S> {
    pub vm: &'a vm::VM<S>,
    pub token: Token,
    pub tex_macro: &'a Macro,
    pub arguments: &'a [&'a [Token]],
    pub reverse_expansion: &'a [Token],
}

pub fn no_op_hook<S>(_: HookInput<S>) {}

impl Macro {
    pub fn call<S: TexlangState>(
        &self,
        token: Token,
        input: &mut vm::ExpansionInput<S>,
    ) -> txl::Result<()> {
        let prefix_matched = remove_tokens_from_stream(&self.prefix, input.unexpanded())?;
        if !prefix_matched {
            return Ok(());
        }
        let mut argument_indices: Vec<(usize, usize)> = Default::default();
        let mut argument_tokens = input.checkout_token_buffer();
        for (i, parameter) in self.parameters.iter().enumerate() {
            let start_index = argument_tokens.len();
            let trim_outer_braces = parameter.parse_argument(input, i, &mut argument_tokens)?;
            let element = match trim_outer_braces {
                true => (start_index + 1, argument_tokens.len() - 1),
                false => (start_index, argument_tokens.len()),
            };
            argument_indices.push(element);
        }

        let mut arguments: Vec<&[Token]> = Default::default();
        for (i, j) in &argument_indices {
            let slice = argument_tokens.get(*i..*j).unwrap();
            arguments.push(slice);
        }

        let result = input.expansions_mut();
        let num_tokens = Macro::perform_replacement(&self.replacements, &arguments, result);

        // To keep the borrow checker happy we need to downgrade result to a shared reference.
        let result = input.expansions();
        S::post_macro_expansion_hook(
            token,
            input,
            self,
            &arguments,
            &result[result.len() - num_tokens..result.len()],
        );

        input.return_token_buffer(argument_tokens);
        Ok(())
    }

    pub fn doc(&self, interner: &token::CsNameInterner) -> String {
        let mut d = String::default();
        d.push_str("User defined macro\n\n");
        d.push_str(&format![
            "{}\n{}",
            "Parameters definition".italic(),
            pretty_print_prefix_and_parameters(&self.prefix, &self.parameters, interner),
        ]);
        d.push_str(&format![
            "\n\n{} `{}`\n",
            "Replacement definition:".italic(),
            pretty_print_replacement_text(&self.replacements),
        ]);
        d
    }

    /// Create a new macro.
    pub fn new(
        prefix: Vec<Token>,
        parameters: Vec<Parameter>,
        replacement_text: Vec<Replacement>,
    ) -> Macro {
        Macro {
            prefix,
            parameters,
            replacements: replacement_text,
        }
    }

    fn perform_replacement(
        replacements: &[Replacement],
        arguments: &[&[Token]],
        result: &mut Vec<Token>,
    ) -> usize {
        let mut output_size = 0;
        for replacement in replacements.iter() {
            output_size += match replacement {
                Replacement::Tokens(tokens) => tokens.len(),
                Replacement::Parameter(i) => arguments.get(*i).unwrap().len(),
            };
        }
        result.reserve(output_size);
        for replacement in replacements.iter().rev() {
            match replacement {
                Replacement::Tokens(tokens) => {
                    result.extend(tokens);
                }
                Replacement::Parameter(i) => {
                    result.extend(arguments.get(*i).unwrap().iter().rev().copied());
                }
            }
        }
        output_size
    }
}

impl Parameter {
    pub fn parse_argument<S: TexlangState>(
        &self,
        input: &mut vm::ExpansionInput<S>,
        index: usize,
        result: &mut Vec<Token>,
    ) -> txl::Result<bool> {
        match self {
            Parameter::Undelimited => {
                Parameter::parse_undelimited_argument(input, index + 1, result)?;
                Ok(false)
            }
            Parameter::Delimited(matcher_factory) => Parameter::parse_delimited_argument(
                input.unexpanded(),
                matcher_factory,
                index + 1,
                result,
            ),
        }
    }

    fn parse_delimited_argument<S: TexlangState>(
        stream: &mut vm::UnexpandedStream<S>,
        matcher_factory: &Matcher<Value>,
        param_num: usize,
        result: &mut Vec<Token>,
    ) -> txl::Result<bool> {
        let mut matcher = matcher_factory.start();
        let mut scope_depth = 0;

        // This handles the case of a macro whose argument ends with the special #{ tokens. In this special case the parsing
        // will end with a scope depth of 1, because the last token parsed will be the { and all braces before that will
        // be balanced.
        let closing_scope_depth = match matcher_factory.substring().last() {
            token::Value::BeginGroup(_) => 1,
            _ => 0,
        };
        let start_index = result.len();
        loop {
            let token = stream.next_or_err(DelimitedArgumentEndOfInputError { param_num })?;
            match token.value() {
                token::Value::BeginGroup(_) => {
                    scope_depth += 1;
                }
                token::Value::EndGroup(_) => {
                    scope_depth -= 1;
                }
                _ => (),
            };
            let matches_delimiter = matcher.next(&token.value());
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
        /*
        TODO
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
         */
    }

    fn should_trim_outer_braces_if_present(list: &[Token]) -> bool {
        if list.len() <= 1 {
            return false;
        }
        match list[0].value() {
            token::Value::BeginGroup(_) => (),
            _ => {
                return false;
            }
        }
        match list[list.len() - 1].value() {
            token::Value::EndGroup(_) => (),
            _ => {
                return false;
            }
        }
        true
    }

    fn parse_undelimited_argument<S: TexlangState>(
        input: &mut vm::ExpansionInput<S>,
        param_num: usize,
        result: &mut Vec<Token>,
    ) -> txl::Result<()> {
        parse::SpacesUnexpanded::parse(input)?;
        let input = input.unexpanded();
        let token = input.next_or_err(UnDelimitedArgumentEndOfInputError { param_num })?;
        match token.value() {
            token::Value::BeginGroup(_) => token,
            _ => {
                result.push(token);
                return Ok(());
            }
        };
        parse::finish_parsing_balanced_tokens(input, result)?;
        Ok(())
        /* TODO
            .add_note(format![
            "this argument started with a `{opening_brace}` and must be finished with a matching closing brace"
        ])
            .add_token_context(&opening_brace, "the argument started here:")
            .add_token_context(macro_token, "the macro invocation started here:")
            .cast()),
             */
    }
}

#[derive(Debug)]
struct DelimitedArgumentEndOfInputError {
    param_num: usize,
}

impl error::EndOfInputError for DelimitedArgumentEndOfInputError {
    fn doing(&self) -> String {
        "parsing a delimited argument for a macro".into()
    }
    fn notes(&self) -> Vec<error::display::Note> {
        vec![format!("this is argument number {} for this macro", self.param_num).into()]
    }
}

#[derive(Debug)]
struct UnDelimitedArgumentEndOfInputError {
    param_num: usize,
}

impl error::EndOfInputError for UnDelimitedArgumentEndOfInputError {
    fn doing(&self) -> String {
        "parsing an undelimited argument for a macro".into()
    }
    fn notes(&self) -> Vec<error::display::Note> {
        vec![format!("this is argument number {} for this macro", self.param_num).into()]
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
    interner: &token::CsNameInterner,
) -> String {
    let mut d = String::default();
    if prefix.is_empty() {
        d.push_str(" . No prefix\n");
    } else {
        d.push_str(&format![
            " . Prefix: `{}`\n",
            token::write_tokens(prefix, interner)
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
                    token::write_token_values(factory.substring(), interner)
                ]);
            }
        }
        parameter_number += 1;
    }

    d.push_str(" . Full argument specification: `");
    d.push_str(&token::write_tokens(prefix, interner));
    let mut parameter_number = 1;
    for parameter in parameters {
        d.push_str(&colored_parameter_number(parameter_number));
        if let Parameter::Delimited(factory) = parameter {
            d.push_str(token::write_token_values(factory.substring(), interner).as_str());
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

/// Removes the provided vector of tokens from the front of the stream.
pub fn remove_tokens_from_stream<S: TexlangState>(
    tokens: &[Token],
    stream: &mut vm::UnexpandedStream<S>,
) -> txl::Result<bool> {
    for prefix_token in tokens.iter() {
        let stream_token = stream.next_or_err(PrefixEndOfInputError {})?;
        if stream_token.value() != prefix_token.value() {
            stream.error(error::SimpleTokenError::new(
                stream_token,
                "unexpected token while matching the prefix for a user-defined macro",
            ))?;
            return Ok(false);
        }
    }
    Ok(true)
}

#[derive(Debug)]
struct PrefixEndOfInputError;

impl error::EndOfInputError for PrefixEndOfInputError {
    fn doing(&self) -> String {
        "matching the prefix of a user-defined macro".into()
    }
}
