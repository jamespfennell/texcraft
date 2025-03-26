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
        extra_positional_arg: Str<'a>,
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

    /// The specified function does not exist.
    NoSuchFunction { function_name: Str<'a> },

    /// A closing square bracket token ']' was unmatched.
    UnmatchedClosingSquareBracket { square_close: Str<'a> },

    /// An invalid unit was provided for a dimension.
    InvalidDimensionUnit { dimension: Str<'a>, unit: Str<'a> },
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
            NoSuchFunction { function_name } => {
                format!["Unknown function `{function_name}`"]
            }
            UnmatchedClosingSquareBracket { .. } => "Unmatched closing square bracket".into(),
            InvalidDimensionUnit { dimension: _, unit } => {
                format!["Dimension has an invalid unit '{unit}'"]
            }
        }
    }
    // TODO: this looks like a waste of time?
    pub fn main_span(&self) -> std::ops::Range<usize> {
        use Error::*;
        match self {
            PositionalArgAfterKeywordArg { positional_arg, .. } => positional_arg.span(),
            IncorrectType { got_raw_value, .. } => got_raw_value.span(),
            TooManyPositionalArgs {
                extra_positional_arg: extra_positional_args,
                ..
            } => extra_positional_args.span(),
            NoSuchArgument { argument, .. } => argument.span(),
            DuplicateArgument {
                first_assignment, ..
            } => first_assignment.span(),
            NoSuchFunction { function_name } => function_name.span(),
            UnmatchedClosingSquareBracket { square_close } => square_close.span(),
            InvalidDimensionUnit { dimension, unit: _ } => dimension.span(),
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
            TooManyPositionalArgs { extra_positional_arg: _, function_name, max_positional_args } => vec![
                ErrorLabel {
                    span: self.main_span(),
                    text: "an extra positional arguments was provided".to_string(),
                },
                ErrorLabel {
                    span: function_name.span(),
                    text: format![
                        "The `{}` function accepts up to {max_positional_args} positional arguments",
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
            NoSuchFunction { function_name } => vec![
                ErrorLabel {
                    span: function_name.span(),
                    text: "there is no function with this name".to_string(),
                },
            ],
            UnmatchedClosingSquareBracket{ square_close } => vec![
                ErrorLabel {
                    span: square_close.span(),
                    text: "this bracket does not correspond to any preceding opening square bracket".to_string(),
                },
            ],
            InvalidDimensionUnit { dimension: _, unit } => vec![
                ErrorLabel {
                    span: unit.span(),
                    text: "valid units are the same as TeX".to_string(),
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
            UnmatchedClosingSquareBracket { .. } => {
                vec!["The token will be ignored".to_string()]
            }
            IncorrectType { .. }
            | TooManyPositionalArgs { .. }
            | NoSuchArgument { .. }
            | DuplicateArgument { .. }
            | NoSuchFunction { .. }
            | InvalidDimensionUnit { .. } => vec![],
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

/// A data structure for accumulating errors.
#[derive(Clone, Debug, Default)]
pub struct ErrorAccumulator<'a> {
    errs: std::rc::Rc<std::cell::RefCell<Vec<Error<'a>>>>,
}

impl<'a> ErrorAccumulator<'a> {
    pub fn add(&self, err: Error<'a>) {
        self.errs.borrow_mut().push(err);
    }
    pub fn len(&self) -> usize {
        self.errs.borrow().len()
    }
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
    pub fn check(self) -> Result<(), Vec<Error<'a>>> {
        let errs = self.errs.take();
        if errs.is_empty() {
            Ok(())
        } else {
            Err(errs)
        }
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
            invalid_dimension_unit,
            "glue(0plx)",
            Error::InvalidDimensionUnit,
        ),
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
            too_many_positional_args_1,
            r#"text("Hello", 3, "Mundo")"#,
            Error::TooManyPositionalArgs,
        ),
        (
            too_many_positional_args_2,
            r#"text("Hello", font=3, "Mundo")"#,
            Error::PositionalArgAfterKeywordArg,
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
        (invalid_func_name, r#"random()"#, Error::NoSuchFunction,),
        (
            trailing_closed_square,
            "text()]",
            Error::UnmatchedClosingSquareBracket,
        ),
        /*
        "hlist(contents=[]])"
        ",text()"
        "text,()"
        "text(,)"
        "text(content,="Hello")"
        "text(content=,"Hello")"
        "text(content=("Ignored"(3pt)[)"World)"
        "text(content[(ignored]="Hello")
        "text("Hello""
         */
    );
}
