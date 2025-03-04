use super::Str;

/// Error encountered when parsing Box language.
#[derive(Debug, PartialEq, Eq)]
pub enum Error<'a> {
    /// A positional argument appears after a keyword argument.
    ///
    /// This is an error, just like in Python.
    /// The problem is that it's ambiguous which parameter the
    /// positional argument should apply to.
    PositionalArgAfterKeywordArg {
        positional_arg: Str<'a>,
        keyword_arg: Str<'a>,
    },

    /// A function argument has the wrong type.
    IncorrectType {
        wanted_type: &'static str,
        got_type: &'static str,
        got_raw_value: Str<'a>,
        function_name: Str<'a>,
        parameter_name: &'a str,
    },

    /// Too many positional arguments provided.
    TooManyPositionalArgs {
        extra_positional_args: Vec<Str<'a>>,
        function_name: Str<'a>,
        max_positional_args: usize,
    },

    /// No such argument to this function.
    NoSuchArgument {
        function_name: Str<'a>,
        argument: Str<'a>,
    },

    /// The same argument was provided multiple times.
    DuplicateArgument {
        parameter_name: &'a str,
        first_assignment: Str<'a>,
        second_assignment: Str<'a>,
    },
}

impl<'a> Error<'a> {
    pub fn message(&self) -> String {
        use Error::*;
        match self {
            PositionalArgAfterKeywordArg { .. } => {
                "A positional argument appears after a keyword argument".into()
            }
            IncorrectType { .. } => "An argument has the wrong type".into(),
            TooManyPositionalArgs { .. } => "Too many positional arguments provided".into(),
            NoSuchArgument { .. } => "No such argument to this function".into(),
            DuplicateArgument { parameter_name, .. } => {
                format!["The `{parameter_name}` argument was provided multiple times"]
            }
        }
    }
    pub fn main_span(&self) -> std::ops::Range<usize> {
        use Error::*;
        match self {
            PositionalArgAfterKeywordArg { positional_arg, .. } => positional_arg.span(),
            IncorrectType { got_raw_value, .. } => got_raw_value.span(),
            TooManyPositionalArgs {
                extra_positional_args,
                ..
            } => {
                let first = extra_positional_args.first().unwrap();
                let last = extra_positional_args.last().unwrap();
                first.start..last.end
            }
            NoSuchArgument { argument, .. } => argument.span(),
            DuplicateArgument {
                first_assignment, ..
            } => first_assignment.span(),
        }
    }
    pub fn labels(&self) -> Vec<ErrorLabel> {
        use Error::*;
        match self {
            PositionalArgAfterKeywordArg {
                positional_arg,
                keyword_arg,
            } => {
                vec![
                    ErrorLabel {
                        span: positional_arg.span(),
                        text: "The positional argument appears here".into(),
                    },
                    ErrorLabel {
                        span: keyword_arg.span(),
                        text: "The keyword argument appears here".into(),
                    },
                ]
            }
            IncorrectType {
                wanted_type,
                got_type: got,
                got_raw_value,
                function_name,
                parameter_name,
            } => vec![
                ErrorLabel {
                    span: got_raw_value.span(),
                    text: format!["The provided argument is {got}"],
                },
                ErrorLabel {
                    span: function_name.span(),
                    text: format![
                        "The `{parameter_name}` parameter of the `{}` function requires {wanted_type}",
                        function_name.str()
                    ],
                },
            ],
            TooManyPositionalArgs { extra_positional_args, function_name, max_positional_args } => vec![
                ErrorLabel {
                    span: self.main_span(),
                    text: format![
                        "{} extra arguments were provided",
                        extra_positional_args.len(),
                    ],
                },
                ErrorLabel {
                    span: function_name.span(),
                    text: format![
                        "The `{}` function accepts up to {max_positional_args} arguments",
                        function_name.str()
                    ],
                },
            ],
            NoSuchArgument { function_name, argument } => vec![
                ErrorLabel {
                    span: self.main_span(),
                    text: format![
                        "an argument with name `{}` appears here",
                        argument.str(),
                    ],
                },
                ErrorLabel {
                    span: function_name.span(),
                    text: format![
                        "The `{}` function does not have a parameter with name `{}`",
                        function_name.str(),
                        argument.str(),
                    ],
                },
            ],
            DuplicateArgument { parameter_name: _, first_assignment, second_assignment } => vec![
                ErrorLabel {
                    span: first_assignment.span(),
                    text: "the first value appears here".to_string(),
                },
                ErrorLabel {
                    span: second_assignment.span(),
                    text: "the second value appear here".to_string(),
                },
            ],
        }
    }
    pub fn notes(&self) -> Vec<String> {
        use Error::*;
        match self {
            PositionalArgAfterKeywordArg { .. } => {
                vec![
                    "Positional arguments must be appear before keyword arguments (as in Python)"
                        .to_string(),
                ]
            }
            IncorrectType { .. }
            | TooManyPositionalArgs { .. }
            | NoSuchArgument { .. }
            | DuplicateArgument { .. } => vec![],
        }
    }
}

/// Label on an error message.
///
/// A label identifies a particular piece of source code and some
/// information about it.
pub struct ErrorLabel {
    pub span: std::ops::Range<usize>,
    pub text: String,
}

impl<'a> Error<'a> {
    #[cfg(feature = "ariadne")]
    pub fn ariadne_report(
        &self,
        file_name: &'a str,
    ) -> ariadne::Report<'static, (&str, std::ops::Range<usize>)> {
        let mut report =
            ariadne::Report::build(ariadne::ReportKind::Error, (file_name, self.main_span()))
                .with_message(self.message());
        let mut color = ariadne::Color::BrightRed;
        for label in self.labels() {
            report = report.with_label(
                ariadne::Label::new((file_name, label.span))
                    .with_message(label.text)
                    .with_color(color),
            );
            color = ariadne::Color::BrightYellow;
        }
        for note in self.notes() {
            report = report.with_note(note);
        }
        report.finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    pub fn get_unique_err(source: &str) -> Error {
        let mut errs = super::super::parse_horizontal_list(source).unwrap_err();
        assert_eq!(errs.len(), 1);
        errs.pop().unwrap()
    }

    macro_rules! error_tests {
        ( $(
            ($name: ident, $source: expr, Error:: $want_variant: ident,),
        )+ ) => {
            $(
            #[test]
            fn $name() {
                let source = $source;
                let err = get_unique_err(source);
                println!["got: {err:?}"];
                assert!(matches!(err, Error::$want_variant {..}));
            }
            )+
        };
    }
    error_tests!(
        (
            invalid_type_positional,
            r#"text(1pc)"#,
            Error::IncorrectType,
        ),
        (
            invalid_type_keyword,
            r#"text(content=1pc)"#,
            Error::IncorrectType,
        ),
        (
            too_many_positional_args,
            r#"text("Hello", 3, 3, "World", "Mundo")"#,
            Error::TooManyPositionalArgs,
        ),
        (
            positional_arg_after_keyword_arg,
            r#"text(font=3, "Hello")"#,
            Error::PositionalArgAfterKeywordArg,
        ),
        (
            duplicate_keyword_args,
            r#"text(content="Hello", content="World")"#,
            Error::DuplicateArgument,
        ),
        (
            duplicate_positional_and_keyword_args,
            r#"text("Hello", content="Mundo")"#,
            Error::DuplicateArgument,
        ),
        (
            invalid_keyword_arg,
            r#"text(random="Hello")"#,
            Error::NoSuchArgument,
        ),
    );
}
