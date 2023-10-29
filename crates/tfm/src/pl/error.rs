//! Errors relating to property list file parsing

#[derive(PartialEq, Eq, Debug)]
pub enum Error {
    InvalidCharacter(char, usize),
    InvalidPropertyName {
        name: String,
        name_span: std::ops::Range<usize>,
        allowed_property_names: &'static [&'static str],
    },
    UnbalancedOpeningParenthesis {
        opening_parenthesis_span: usize,
        end_span: usize,
    },
    JunkInPropertyList {
        value: String,
        span: std::ops::Range<usize>,
    },
    UnexpectedRightParenthesis {
        span: usize,
    },
    InvalidPrefixForInteger {
        span: std::ops::Range<usize>,
    },
    IntegerTooBig {
        // TODO: radix
        span: std::ops::Range<usize>,
    },
    InvalidOctalDigit {
        c: char,
        span: usize,
    },
    SmallIntegerTooBig {
        span: std::ops::Range<usize>,
        radix: u8,
    },
    InvalidFaceCode {
        span: std::ops::Range<usize>,
    },
    InvalidCharacterForSmallInteger {
        span: std::ops::Range<usize>,
    },
    InvalidPrefixForSmallInteger {
        span: std::ops::Range<usize>,
    },
    InvalidBoolean {
        span: std::ops::Range<usize>,
    },
    DecimalTooLarge {
        span: std::ops::Range<usize>,
    },
    InvalidPrefixForDecimal {
        span: std::ops::Range<usize>,
    },
}

impl Error {
    pub fn title(&self) -> String {
        match self {
            Error::InvalidCharacter(c, _) => {
                format!["invalid character {} (U+{:04X})", *c, *c as u32]
            }
            Error::InvalidPropertyName { name, .. } => {
                format!["invalid property name {name}"]
            }
            Error::UnbalancedOpeningParenthesis { .. } => {
                "file ended before opening parenthesis was closed".to_string()
            }
            Error::JunkInPropertyList { value, .. } => {
                format!["unexpected junk string '{value}' in the middle of a property list",]
            }
            Error::UnexpectedRightParenthesis { .. } => "unexpected right parenthesis".to_string(),
            Error::InvalidPrefixForInteger { .. } => {
                "invalid prefix for integer constant: an octal 'O' or hexadecimal 'H' is required"
                    .to_string()
            }
            Error::IntegerTooBig { .. } => {
                "integer too big: the largest allowed value is O37777777777 or HFFFFFFF".to_string()
            }
            Error::InvalidOctalDigit { c, .. } => {
                format!["invalid octal digit {c}: only digits 0 through 7 inclusive may appear"]
            }
            Error::SmallIntegerTooBig { radix, .. } => {
                let max = match radix {
                    8 => "0o377",
                    10 => "255",
                    _ => "0xFF,",
                };
                format!["integer too big: the largest allowed value is {max}"]
            }
            Error::InvalidFaceCode { .. } => "invalid face code; using MRR instead".to_string(),
            Error::InvalidCharacterForSmallInteger { .. } => {
                "invalid character; only printable ASCII characters are allowed".to_string()
            }
            Error::InvalidPrefixForSmallInteger { .. } => {
                "invalid prefix for integer constant: need C/O/D/H/F".to_string()
            }
            Error::InvalidBoolean { .. } => "invalid boolean: must be TRUE or FALSE".to_string(),
            Error::DecimalTooLarge { .. } => "real constants must be less than 2048".to_string(),
            Error::InvalidPrefixForDecimal { .. } => {
                "invalid prefix for decimal constant: need R or D".to_string()
            }
        }
    }

    pub fn pl_to_tf_section(&self) -> (u8, u8) {
        match self {
            Error::InvalidCharacter(_, _) => (32, 1),
            Error::InvalidPropertyName { .. } => (49, 1),
            Error::UnbalancedOpeningParenthesis { .. } => (33, 1),
            Error::JunkInPropertyList { .. } => (83, 1),
            Error::UnexpectedRightParenthesis { .. } => (82, 1),
            Error::InvalidPrefixForInteger { .. } => (59, 1),
            Error::IntegerTooBig { .. } => (60, 1),
            Error::InvalidOctalDigit { .. } => (60, 2),
            Error::SmallIntegerTooBig { radix, .. } => match radix {
                8 => (54, 1),
                10 => (53, 1),
                _ => (55, 1),
            },
            Error::InvalidFaceCode { .. } => (56, 1),
            Error::InvalidCharacterForSmallInteger { .. } => (52, 1),
            Error::InvalidPrefixForSmallInteger { .. } => (51, 1),
            Error::InvalidBoolean { .. } => (90, 1),
            Error::DecimalTooLarge { .. } => (64, 1),
            Error::InvalidPrefixForDecimal { .. } => (62, 1),
        }
    }

    #[cfg(feature = "ariadne")]
    pub fn ariadne_report(&self) -> ariadne::Report {
        report::build(self)
    }
}

#[cfg(feature = "ariadne")]
mod report {
    use super::*;
    use ariadne::*;

    pub fn build(error: &Error) -> Report {
        let a = Color::Fixed(81);

        let builder = Report::build(ReportKind::Error, (), 3)
            .with_code(format![
                "{}.{}",
                error.pl_to_tf_section().0,
                error.pl_to_tf_section().1
            ])
            .with_message(error.title());
        let builder = match error {
            Error::InvalidCharacter(c, offset) => {
                let range = *offset..*offset + c.len_utf8();
                builder
                    .with_label(Label::new(range).with_message(error.title()).with_color(a))
                    .with_note("property list files can only contain printable ASCII characters")
            }
            Error::InvalidPropertyName {
                name_span,
                allowed_property_names,
                ..
            } => builder
                .with_label(
                    Label::new(name_span.clone())
                        .with_message(error.title())
                        .with_color(a),
                )
                .with_note(format![
                    "the following property names are allowed: {}",
                    allowed_property_names.join(", ")
                ]),
            Error::UnbalancedOpeningParenthesis {
                opening_parenthesis_span,
                end_span,
            } => {
                let opening_span = *opening_parenthesis_span;
                builder
                    .with_label(
                        Label::new(opening_span..opening_span + 1)
                            .with_message("this is the opening parenthesis"),
                    )
                    .with_label(
                        Label::new(*end_span - 1..*end_span)
                            .with_message("this is where the file ends"),
                    )
                    .with_note("a additional closing parenthesis was added at the end of the file")
            }
            Error::JunkInPropertyList { span, .. } => builder
                .with_label(Label::new(span.clone()).with_message("unexpected junk string"))
                .with_note("the string was ignored"),
            Error::UnexpectedRightParenthesis { span, .. } => builder
                .with_label(
                    Label::new(*span..*span + 1).with_message("unexpected right parenthesis"),
                )
                .with_help("all right parentheses must be matched by an opening left parenthesis")
                .with_note("the parenthesis was ignored"),
            Error::InvalidOctalDigit { c: _, span } => {
                builder.with_label(Label::new(*span..*span + 1).with_message(error.title()))
            }
            Error::InvalidPrefixForInteger { span }
            | Error::IntegerTooBig { span }
            | Error::SmallIntegerTooBig { span, .. }
            | Error::InvalidFaceCode { span }
            | Error::InvalidCharacterForSmallInteger { span }
            | Error::InvalidBoolean { span }
            | Error::DecimalTooLarge { span }
            | Error::InvalidPrefixForDecimal { span }
            | Error::InvalidPrefixForSmallInteger { span } => {
                builder.with_label(Label::new(span.clone()).with_message(error.title()))
            }
        };
        builder.finish()
    }
}
