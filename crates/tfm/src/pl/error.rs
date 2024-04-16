//! Warnings relating to property list file parsing

use crate::{ligkern::InfiniteLoopError, NextLargerProgramWarning};

/// Warning generated while parsing a property list file.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ParseWarning {
    /// Location of the part of the property list file that generates this warning.
    ///
    /// The span is in number of Unicode code points, not bytes.
    pub span: std::ops::Range<usize>,
    /// Point in the file that Knuth's pltotf flags as problematic.
    ///
    /// Most warnings in pltotf are printed with a context line.
    /// This looks like this:
    /// ```txt
    /// PARAMETER index must not be zero (line 9).
    /// (PARAMETER D 0
    ///                D 5)  
    /// ```
    /// The context line is printed over 2 lines in the terminal,
    ///     with the break occurring at the part of the line that triggered the warning.
    ///
    /// This field gives the offset of the break within the entire file.
    /// The offset is in number of Unicode code points, not bytes.
    /// For warnings that don't have a context line (like next larger warnings)
    ///     this field has a value of [`None`].
    pub knuth_pltotf_offset: Option<usize>,
    /// Kind of the warning.
    pub kind: ParseWarningKind,
}

/// Kind of parse warning.
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum ParseWarningKind {
    /// The file contains an opening parenthesis that is not followed/balanced by a closing parenthesis.
    UnbalancedOpeningParenthesis {
        opening_parenthesis_span: std::ops::Range<usize>,
    },
    /// The file contains a closing parenthesis that is not preceded/balanced by an opening parenthesis.
    UnexpectedClosingParenthesis,
    /// There is some junk in the middle of a property list;
    ///     e.g. `(CHARWD D 9.0) junk (CHARDP D 8.0)`.
    JunkInsidePropertyList { junk: String },
    /// There is some junk after a property value;
    ///     e.g. `(CHECKSUM O 123 junk)`.
    JunkAfterPropertyValue { junk: String },
    /// A parameter number has value 255 which is too big.
    ///
    /// Due to how pltotf processes numbers, parameter numbers larger than
    ///     255 are reported with [ParseWarningKind::SmallIntegerIsTooBig]
    ///     and [ParseWarningKind::ParameterNumberIsZero] warnings.
    ParameterNumberIsTooBig,
    /// A parameter number has value 0 which is invalid as parameter numbers must be strictly positive.
    ParameterNumberIsZero,
    /// A small integer is bigger than 255.
    SmallIntegerIsTooBig { radix: u8 },
    /// An integer is bigger than 255^4.
    IntegerIsTooBig { radix: u8 },
    /// A decimal number is bigger than 2048 in absolute value.
    DecimalNumberIsTooBig,
    /// A property name is invalid.
    InvalidPropertyName {
        /// The name that was provided.
        provided_name: String,
        /// Names that are allowed in this position.
        allowed_property_names: &'static [&'static str],
    },
    /// Invalid prefix for an integer.
    InvalidPrefixForInteger { prefix: Option<char> },
    /// Invalid prefix for a small integer.
    InvalidPrefixForSmallInteger,
    /// Invalid prefix for a decimal number.
    InvalidPrefixForDecimalNumber,
    /// Invalid octal digit.
    InvalidOctalDigit { invalid_digit: char },
    /// No character value is provided.
    EmptyCharacterValue,
    /// Invalid face code
    InvalidFaceCode,
    /// Invalid boolean value.
    InvalidBoolean,
    /// A header index is smaller than 18
    HeaderIndexIsTooSmall,
    /// The lig table is too big (todo: larger than what?)
    LigTableIsTooBig,
    /// The lig/kern program contains a cycle
    CycleInLigKernProgram(InfiniteLoopError),
    /// The next larger program contains a cycle
    CycleInNextLargerProgram(NextLargerProgramWarning),
    /// The file is really seven bit safe, even though it says it is
    NotReallySevenBitSafe,
    /// The design size is less than 1.
    DesignSizeIsTooSmall,
    /// The file contains a non-visible ASCII character.
    NonVisibleAsciiCharacter { character: char },
}

struct Data {
    rule: String,
    problem: String,
    action: &'static str,
    pltotf_message: String,
    pltotf_section: (u8, u8),
}

impl ParseWarning {
    pub fn pltotf_message(&self, pl_source: &str) -> String {
        let data = self.kind.data();
        match self.knuth_pltotf_offset {
            None => data.pltotf_message,
            Some(pltotf_point) => {
                add_pltotf_error_context(pl_source, data.pltotf_message, pltotf_point)
            }
        }
    }
}

/* TODO
impl ParseError {
    fn pltotf_section(&self) -> (u8, u8) {
        use ParseError::*;
        match self {
            // TODO
            DecimalTooLarge { .. } => (64, 1),
            LigTableTooLong { .. } => (101, 1),
            NotReallySevenBitSafe => (110, 1),
            _ => (0, 0)
        }
    }
}
 */

impl ParseWarningKind {
    fn data(&self) -> Data {
        use ParseWarningKind::*;
        match self.clone() {
            EmptyCharacterValue => Data {
                rule: "an ASCII character must be specified after a \"C\"".into(),
                problem: "empty".into(),
                action: "",
                pltotf_message: "\"C\" value must be standard ASCII and not a paren".into(),
                pltotf_section: (52,1),
            },
            InvalidBoolean => Data {
                rule: "boolean values must start with T, t, F or f.".into(),
                problem: "invalid boolean value".into(),
                action: "this property list element will be skipped",
                pltotf_message: "The flag value should be \"TRUE\" or \"FALSE\"".into(),
                pltotf_section: (90, 1),
            },
            InvalidOctalDigit{ invalid_digit } => Data {
                rule: "octal numbers can only contain the characters 0 through 7 inclusive".into(),
                problem: format!["octal number contains the non-octal character {invalid_digit}"],
                action: "the octal number will be truncated to before the problematic character",
                pltotf_message: "Illegal digit".into(),
                pltotf_section: (60, 2),
            },
            NonVisibleAsciiCharacter{ character: c } => Data {
                rule: "property list files can only contain newlines and printable ASCII characters (space through tilde inclusive)".into(),
                problem: format!["invalid character {} (U+{:04X})", c, c as u32] ,
                action: "this character will be ignored",
                pltotf_message: if (c as usize) < 128 { "Illegal character in the file".into()} else {"".into()},
                pltotf_section: (32, 1),
            },
            UnbalancedOpeningParenthesis { opening_parenthesis_span: _ } => Data {
                rule: "all opening parentheses must be balanced by a subsequent closing parenthesis".into(),
                problem: "opening parenthesis is never closed".into(),
                action: "an additional closing parenthesis will be appended to the file",
                pltotf_message:  "File ended unexpectedly: No closing \")\"".into(),
                pltotf_section: (33,1),
            },
            JunkInsidePropertyList { junk } => Data {
                rule: "".into(),
                problem: format!["junk string '{junk}' in the middle of a property list"],
                action: "this junk will be ignored",
                pltotf_message: "There's junk here that is not in parentheses".into(),
                pltotf_section: (83,1),
            },
            JunkAfterPropertyValue { junk } => Data {
                rule: "".into(),
                problem: format!["junk string '{junk}' after a property list value"],
                action: "this junk will be ignored",
                pltotf_message: "Junk after property value will be ignored".into(),
                pltotf_section: (35,1),
            },
            UnexpectedClosingParenthesis => Data {
                rule: "all closing parentheses must be balanced by a preceding opening parenthesis".into(),
                problem: "closing parenthesis was never opened".into(),
                action: "the closing parenthesis will be ignored",
                pltotf_message:  "Extra right parenthesis".into(),
                pltotf_section: (82, 1),
            },
            InvalidPrefixForSmallInteger => Data {
                rule: "8-bit integers must be prefixed by C, D, F, H or O".into(),
                problem: "invalid prefix for an 8-bit integer".into(),
                action: "0 will be used instead",
                pltotf_message: "You need \"C\" or \"D\" or \"O\" or \"H\" or \"F\" here".into(),
                pltotf_section: (51,1),
            },
            InvalidPrefixForInteger{ prefix: _ } => Data {
                rule: "32-bit integers must be prefixed by H or O".into(),
                problem: "invalid prefix for an 32-bit integer".into(),
                action: "0 will be used instead",
                pltotf_message: "An octal (\"O\") or hex (\"H\") value is needed here".into(),
                pltotf_section: (59,1),
            },
            InvalidPropertyName {
                provided_name,
                allowed_property_names,
            } => {
                let allowed_property_names = {
                    let mut v: Vec<&'static str> = allowed_property_names.to_vec();
                    v.sort();
                    v
                };
                Data {
                    rule: format![
                        "the following property names are allowed here: {}",
                        allowed_property_names.join(", ")
                    ],
                    problem: format!("the property name {provided_name} is not allowed here"),
                    action: "this element will be ignored",
                    pltotf_message: "Sorry, I don't know that property name".into(),
                    pltotf_section: (49, 1),
                }
            }
            CycleInNextLargerProgram(warning) => {
                let (rule, problem, action) = match warning {
                    NextLargerProgramWarning::NonExistentCharacter { original, next_larger } => (
                        "next larger rules must reference characters defined in the file",
                        format!["the next larger rule for character {original} references an undefined character {next_larger}"],
                        "the element flagged here will be ignored",
                    ),
                    NextLargerProgramWarning::InfiniteLoop{ original, next_larger } => (
                        "the next larger rules cannot create cycles (e.g., X larger than Y and Y larger than X)",
                        format!("setting {} as the next larger character for {} creates a cycle", next_larger, original),
                        "the element flagged here will be skipped in order to break the cycle",
                    ),
                };
                Data {
                    rule: rule.into(),
                    problem,
                    action,
                    pltotf_message: warning.pltotf_message(),
                    pltotf_section: (warning.pltotf_section(), 1),
                }
            }
            ParameterNumberIsZero => Data {
                rule: "parameter indices must be integers in the range [1,254] inclusive".into(),
                problem: "0 is not a valid parameter index".into(),
                action: "this parameter element will be ignored",
                pltotf_message: "PARAMETER index must not be zero".into(),
                pltotf_section: (93, 1),
            },
            ParameterNumberIsTooBig => Data {
                rule: "parameter indices must be integers in the range [1,254] inclusive".into(),
                problem: "this parameter index is too big".into(),
                action: "this parameter element will be ignored",
                pltotf_message: "This PARAMETER index is too big for my present table size".into(),
                pltotf_section: (93, 2),
            },
            SmallIntegerIsTooBig { radix } => Data {
                rule: format!["8-bit numbers can be at most 2^8 = {} = 0x{:x} = 0o{:o}", u8::MAX, u8::MAX, u8::MAX],
                problem: "this number is too big".into(),
                action: "0 will be used instead",
                pltotf_message: {
                    let max = match radix {
                        8 => format!["'{:o}", u8::MAX],
                        10 => format!["{}", u8::MAX],
                        _ => format!["\"{:X}", u8::MAX],
                    };
                    format!["This value shouldn't exceed {max}"]
                },
                pltotf_section: match radix {
                    8 => (54, 1),
                    10 => (53, 1),
                    _ => (55, 1),
                },
            },
            IntegerIsTooBig { radix, } => Data {
              rule: format!["32-bit numbers can be at most 2^32 = {} = 0x{:x} = 0o{:o}", u32::MAX, u32::MAX, u32::MAX],
              problem: format!["this {} number is too big", if radix == 8 {
                "octal"
              } else {
                "hexadecimal"
              }],
              action: "0 will be used instead",
              pltotf_message: if radix == 8 {
                  format!["Sorry, the maximum octal value is O {:o}", u32::MAX]
              } else {
                  format!["Sorry, the maximum hex value is H {:X}", u32::MAX]
              },
              pltotf_section: (60, 1),
            },
            InvalidFaceCode => Data{
                rule: "face codes must satisfy the pattern [BLM][IR][CER]".into(),
                problem: "invalid face code".into(),
                action: "the face code MRR will be used instead",
                pltotf_message: "Illegal face code, I changed it to MRR".into(),
                pltotf_section: (56, 1),
            },
            CycleInLigKernProgram(err) => Data {
                rule: "ligature programs cannot contain cycles".into(),
                problem: format!["this ligature rule for the pair ({},{}) creates a cycle",
                match err.starting_pair.0 {
                  None => "boundary".to_string(),
                  Some(c) => format!["{}", c],
                },
                err.starting_pair.1,
                ],
                action: "all ligatures will be deleted",
                pltotf_message: format![
                    "Infinite ligature loop starting with {} and '{:o}!\nAll ligatures will be cleared.",
                    match err.starting_pair.0 {
                        None => "boundary".to_string(),
                        Some(c) => format!["'{:o}", c.0],
                    },
                    err.starting_pair.1.0,
                ],
                pltotf_section: (125, 1),
            },
            InvalidPrefixForDecimalNumber => Data{
                rule: "decimal numbers must be prefixed by D or R".into(),
                problem: "invalid prefix for a decimal number".into(),
                action: "0 will be used instead",
                pltotf_message: "An \"R\" or \"D\" value is needed here".into(),
                pltotf_section: (62,1),
            },
            HeaderIndexIsTooSmall => Data {
                rule: "header indices must be at least 18".into(),
                problem: "header index is too small".into(),
                action: "this property list element will be ignored",
                pltotf_message: "HEADER indices should be 18 or more".into(),
                pltotf_section: (91,1),
            },
            DesignSizeIsTooSmall => Data {
                rule: "the design size must be at least 1".into(),
                problem: "the design size is too small".into(),
                action: "this property list element will be ignored",
                pltotf_message: "The design size must be at least 1".into(),
                pltotf_section: (88, 1),
            },
            _ => todo!("unhandled {self:?}"),
        }
    }
}

impl ParseWarning {
    #[cfg(feature = "ariadne")]
    pub fn ariadne_report(&self) -> ariadne::Report {
        use ariadne::*;
        let light_blue = Color::Fixed(81);

        let data = self.kind.data();
        let mut builder = Report::build(ReportKind::Error, (), self.span.start)
            .with_code(format![
                "{}.{}",
                data.pltotf_section.0, data.pltotf_section.1,
            ])
            .with_message(&data.problem)
            .with_label(
                Label::new(self.span.clone())
                    .with_message(&data.problem)
                    .with_color(light_blue),
            )
            .with_note(data.action);
        if !data.rule.is_empty() {
            builder = builder.with_help(data.rule);
        }
        builder.finish()
    }
}

pub struct AriadneSource {
    source: ariadne::Source,
    path: std::path::PathBuf,
}

impl AriadneSource {
    pub fn new(pl_path: &std::path::Path, pl_source: &str) -> Self {
        let s: String = super::Chars::new(pl_source).collect();
        Self {
            source: s.into(),
            path: pl_path.into(),
        }
    }
}

#[cfg(feature = "ariadne")]
impl ariadne::Cache<()> for &AriadneSource {
    fn fetch(&mut self, _: &()) -> Result<&ariadne::Source, Box<dyn std::fmt::Debug + '_>> {
        Ok(&self.source)
    }

    fn display<'a>(&self, _: &'a ()) -> Option<Box<dyn std::fmt::Display + 'a>> {
        Some(Box::new(format!["{}", self.path.display()]))
    }
}

fn add_pltotf_error_context(pl_source: &str, error_message: String, error_point: usize) -> String {
    let mut line_index = 0;
    let mut line_offset = 0;
    let mut total_chars = 0;
    for (i, c) in super::Chars::new(pl_source).enumerate() {
        total_chars += 1;
        if error_point == i {
            break;
        }
        line_offset += c.len_utf8();
        if c == '\n' {
            line_index += 1;
            line_offset = 0;
        }
    }
    if error_point == total_chars {
        // This handles the unbalanced open parenthesis error.
        let num_lines = line_index + 1;
        return format!("{error_message} (line {num_lines}).\n) \n ...",);
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
