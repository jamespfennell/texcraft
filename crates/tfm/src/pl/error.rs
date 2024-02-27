//! Errors relating to property list file parsing

use crate::{ligkern::InfiniteLoopError, NextLargerProgramWarning};

/// Errors while parsing a property list file
#[derive(PartialEq, Eq, Debug)]
pub enum ParseError {
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
    InvalidHeaderIndex {
        span: std::ops::Range<usize>,
    },
    DecimalTooLarge {
        span: std::ops::Range<usize>,
    },
    InvalidPrefixForDecimal {
        span: std::ops::Range<usize>,
    },
    LigTableTooLong {
        span: std::ops::Range<usize>,
    },
    NextLargerWarning {
        warning: NextLargerProgramWarning,
        span: std::ops::Range<usize>,
    },
    InfiniteLoopInLigKernProgram(InfiniteLoopError),
    NotReallySevenBitSafe,
    ParameterNumberTooBig {
        span: std::ops::Range<usize>,
    },
    ParameterNumberIsZero {
        span: std::ops::Range<usize>,
    },
}

impl ParseError {
    pub fn pltotf_message(&self, pl_source: &str) -> String {
        use ParseError::*;
        let ctx = |a, b| add_pltotf_error_context(pl_source, a, b);
        match self {
            InvalidCharacter(c, _) => {
                format!["invalid character {} (U+{:04X})", *c, *c as u32]
            }
            InvalidPropertyName { name, .. } => {
                format!["invalid property name {name}"]
            }
            UnbalancedOpeningParenthesis { .. } => {
                "file ended before opening parenthesis was closed".to_string()
            }
            JunkInPropertyList { value, .. } => {
                format!["unexpected junk string '{value}' in the middle of a property list",]
            }
            UnexpectedRightParenthesis { .. } => "unexpected right parenthesis".to_string(),
            InvalidPrefixForInteger { .. } => {
                "invalid prefix for integer constant: an octal 'O' or hexadecimal 'H' is required"
                    .to_string()
            }
            IntegerTooBig { .. } => {
                "integer too big: the largest allowed value is O37777777777 or HFFFFFFF".to_string()
            }
            InvalidOctalDigit { c, .. } => {
                format!["invalid octal digit {c}: only digits 0 through 7 inclusive may appear"]
            }
            SmallIntegerTooBig { radix, span } => {
                let max = match radix {
                    8 => "0o377",
                    10 => "255",
                    _ => "0xFF,",
                };
                ctx(format!["This value shouldn't exceed {max}"], span.end)
            }
            InvalidFaceCode { .. } => "invalid face code; using MRR instead".to_string(),
            InvalidCharacterForSmallInteger { .. } => {
                "invalid character; only printable ASCII characters are allowed".to_string()
            }
            InvalidPrefixForSmallInteger { .. } => {
                "invalid prefix for integer constant: need C/O/D/H/F".to_string()
            }
            InvalidBoolean { .. } => "invalid boolean: must be TRUE or FALSE".to_string(),
            InvalidHeaderIndex { .. } => "invalid header index: must be 18 or larger".to_string(),
            DecimalTooLarge { .. } => "real constants must be less than 2048".to_string(),
            InvalidPrefixForDecimal { .. } => {
                "invalid prefix for decimal constant: need R or D".to_string()
            }
            LigTableTooLong { .. } => "Sorry, LIGTABLE to long for me to handle".to_string(),
            NextLargerWarning { warning, span: _ } => warning.pltotf_message(),
            InfiniteLoopInLigKernProgram(err) => {
                format!("{}\nAll ligatures will be cleared.", err.pltotf_message())
            }
            NotReallySevenBitSafe => "The font is not really seven-bit-safe!".to_string(),
            ParameterNumberTooBig { span } => ctx(
                "This PARAMETER index is too big for my present table size".into(),
                span.end,
            ),
            ParameterNumberIsZero { span } => {
                ctx("PARAMETER index must not be zero".into(), span.end - 1)
            }
        }
    }

    pub fn title(&self) -> String {
        use ParseError::*;
        match self {
            InvalidCharacter(c, _) => {
                format!["invalid character {} (U+{:04X})", *c, *c as u32]
            }
            InvalidPropertyName { name, .. } => {
                format!["invalid property name {name}"]
            }
            UnbalancedOpeningParenthesis { .. } => {
                "file ended before opening parenthesis was closed".to_string()
            }
            JunkInPropertyList { value, .. } => {
                format!["unexpected junk string '{value}' in the middle of a property list",]
            }
            UnexpectedRightParenthesis { .. } => "unexpected right parenthesis".to_string(),
            InvalidPrefixForInteger { .. } => {
                "invalid prefix for integer constant: an octal 'O' or hexadecimal 'H' is required"
                    .to_string()
            }
            IntegerTooBig { .. } => {
                "integer too big: the largest allowed value is O37777777777 or HFFFFFFF".to_string()
            }
            InvalidOctalDigit { c, .. } => {
                format!["invalid octal digit {c}: only digits 0 through 7 inclusive may appear"]
            }
            SmallIntegerTooBig { radix, .. } => {
                let max = match radix {
                    8 => "0o377",
                    10 => "255",
                    _ => "0xFF,",
                };
                format!["This value shouldn't exceed {max} (line X)"]
            }
            InvalidFaceCode { .. } => "invalid face code; using MRR instead".to_string(),
            InvalidCharacterForSmallInteger { .. } => {
                "invalid character; only printable ASCII characters are allowed".to_string()
            }
            InvalidPrefixForSmallInteger { .. } => {
                "invalid prefix for integer constant: need C/O/D/H/F".to_string()
            }
            InvalidBoolean { .. } => "invalid boolean: must be TRUE or FALSE".to_string(),
            InvalidHeaderIndex { .. } => "invalid header index: must be 18 or larger".to_string(),
            DecimalTooLarge { .. } => "real constants must be less than 2048".to_string(),
            InvalidPrefixForDecimal { .. } => {
                "invalid prefix for decimal constant: need R or D".to_string()
            }
            LigTableTooLong { .. } => "Sorry, LIGTABLE to long for me to handle".to_string(),
            NextLargerWarning { warning, span: _ } => warning.pltotf_message(),
            InfiniteLoopInLigKernProgram(err) => {
                format!("{}\nAll ligatures will be cleared.", err.pltotf_message())
            }
            NotReallySevenBitSafe => "The font is not really seven-bit-safe!".to_string(),
            ParameterNumberTooBig { span: _ } => {
                "This parameter number is too big: the maximum is 254".to_string()
            }
            ParameterNumberIsZero { span: _ } => "A parameter number cannot be 0".to_string(),
        }
    }

    pub fn pltotf_section(&self) -> (u8, u8) {
        use ParseError::*;
        match self {
            InvalidCharacter(_, _) => (32, 1),
            InvalidPropertyName { .. } => (49, 1),
            UnbalancedOpeningParenthesis { .. } => (33, 1),
            JunkInPropertyList { .. } => (83, 1),
            UnexpectedRightParenthesis { .. } => (82, 1),
            InvalidPrefixForInteger { .. } => (59, 1),
            IntegerTooBig { .. } => (60, 1),
            InvalidOctalDigit { .. } => (60, 2),
            SmallIntegerTooBig { radix, .. } => match radix {
                8 => (54, 1),
                10 => (53, 1),
                _ => (55, 1),
            },
            InvalidFaceCode { .. } => (56, 1),
            InvalidCharacterForSmallInteger { .. } => (52, 1),
            InvalidPrefixForSmallInteger { .. } => (51, 1),
            InvalidBoolean { .. } => (90, 1),
            InvalidHeaderIndex { .. } => (91, 1),
            DecimalTooLarge { .. } => (64, 1),
            InvalidPrefixForDecimal { .. } => (62, 1),
            LigTableTooLong { .. } => (101, 1),
            NextLargerWarning { warning, span: _ } => (warning.pltotf_section(), 1),
            InfiniteLoopInLigKernProgram(err) => (err.pltotf_section(), 1),
            NotReallySevenBitSafe => (110, 1),
            ParameterNumberTooBig { .. } => (93, 2),
            ParameterNumberIsZero { .. } => (93, 1),
        }
    }

    #[cfg(feature = "ariadne")]
    pub fn ariadne_report(&self) -> ariadne::Report {
        report::build(self)
    }
}

fn add_pltotf_error_context(pl_source: &str, error_message: String, error_point: usize) -> String {
    let mut line_index = 0;
    let mut line_offset = 0;
    for (i, c) in pl_source.chars().enumerate() {
        if error_point == i {
            break;
        }
        line_offset += c.len_utf8();
        if c == '\n' {
            line_index += 1;
            line_offset = 0;
        }
    }
    let line = pl_source
        .lines()
        .nth(line_index)
        .expect("we know there are line_index+1 lines in the file");
    let start = &line[..line_offset];
    let end = &line[line_offset..];
    let line_number = line_index + 1;
    format!(
        "{error_message} (line {line_number}).\n{start} \n{}{end}  ",
        " ".repeat(start.len())
    )
}

#[cfg(feature = "ariadne")]
mod report {
    use super::*;
    use ariadne::*;

    pub fn build(error: &ParseError) -> Report {
        let a = Color::Fixed(81);

        let builder = Report::build(ReportKind::Error, (), 3)
            .with_code(format![
                "{}.{}",
                error.pltotf_section().0,
                error.pltotf_section().1
            ])
            .with_message(error.title());
        use ParseError::*;
        let builder = match error {
            InvalidCharacter(c, offset) => {
                let range = *offset..*offset + c.len_utf8();
                builder
                    .with_label(Label::new(range).with_message(error.title()).with_color(a))
                    .with_note("property list files can only contain printable ASCII characters")
            }
            InvalidPropertyName {
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
            UnbalancedOpeningParenthesis {
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
            JunkInPropertyList { span, .. } => builder
                .with_label(Label::new(span.clone()).with_message("unexpected junk string"))
                .with_note("the string was ignored"),
            UnexpectedRightParenthesis { span, .. } => builder
                .with_label(
                    Label::new(*span..*span + 1).with_message("unexpected right parenthesis"),
                )
                .with_help("all right parentheses must be matched by an opening left parenthesis")
                .with_note("the parenthesis was ignored"),
            InvalidOctalDigit { c: _, span } => {
                builder.with_label(Label::new(*span..*span + 1).with_message(error.title()))
            }
            InvalidPrefixForInteger { span }
            | IntegerTooBig { span }
            | SmallIntegerTooBig { span, .. }
            | InvalidFaceCode { span }
            | InvalidCharacterForSmallInteger { span }
            | InvalidBoolean { span }
            | InvalidHeaderIndex { span }
            | DecimalTooLarge { span }
            | InvalidPrefixForDecimal { span }
            | InvalidPrefixForSmallInteger { span }
            | NextLargerWarning { warning: _, span }
            | ParameterNumberTooBig { span }
            | ParameterNumberIsZero { span }
            | LigTableTooLong { span } => {
                builder.with_label(Label::new(span.clone()).with_message(error.title()))
            }
            InfiniteLoopInLigKernProgram(_) | NotReallySevenBitSafe => builder,
        };
        builder.finish()
    }
}
