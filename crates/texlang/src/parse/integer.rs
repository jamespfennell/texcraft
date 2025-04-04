//! Number parsing.
//!
//! The number may be octal, decimal, hexadecimal, cast from a character token, or read
//! from an internal registers. The full definition of a number in the TeX grammar
//! is given on page X of the TeXBook.

use crate::prelude as txl;
use crate::token::{CommandRef, Value};
use crate::traits::*;
use crate::*;

impl Parsable for i32 {
    fn parse_impl<S: TexlangState>(input: &mut vm::ExpandedStream<S>) -> txl::Result<Self> {
        let (_, i, _) = parse_integer(input)?;
        Ok(i)
    }
}

/// When parsed, this type returns a nonnegative integer with the provided upper bound.
///
/// This type is used to implement the following parsing logic in TeX:
///
/// - TeX.2021.433 (scan_eight_bit_int) where N=256.
/// - TeX.2021.434 (scan_char_num) where N=256.
/// - TeX.2021.435 (scan_four_bit_int) where N=16.
/// - TeX.2021.436 (scan_fifteen_bit_int) where N=2^15.
/// - TeX.2021.437 (scan_twenty_seven_bit_int) where N=2^27.
#[derive(Debug, PartialEq, Eq, Default)]
pub struct Uint<const N: usize>(pub usize);

impl Uint<0> {
    pub const MAX: usize = i32::MAX as usize;
}

impl<const N: usize> Parsable for Uint<N> {
    fn parse_impl<S: TexlangState>(input: &mut vm::ExpandedStream<S>) -> txl::Result<Self> {
        let (first_token, i, _) = parse_integer(input)?;
        if i < 0 || i as usize >= N {
            input.error(OutOfBoundsError::<N> {
                first_token,
                got: i,
            })?;
            Ok(Uint(0))
        } else {
            Ok(Uint(i as usize))
        }
    }
}

#[derive(Debug)]
struct OutOfBoundsError<const N: usize> {
    first_token: token::Token,
    got: i32,
}

impl<const N: usize> error::TexError for OutOfBoundsError<N> {
    fn kind(&self) -> error::Kind {
        error::Kind::Token(self.first_token)
    }

    fn title(&self) -> String {
        format!(
            "expected an integer in the range [0, {}), got {}",
            N, self.got
        )
    }
}

impl Parsable for char {
    fn parse_impl<S: TexlangState>(input: &mut vm::ExpandedStream<S>) -> txl::Result<Self> {
        let u1 = Uint::<{ char::MAX as usize }>::parse(input)?;
        let u2: u32 = u1.0.try_into().unwrap();
        Ok(char::from_u32(u2).unwrap())
    }
}

// TODO: move to types/catcode.rs
impl Parsable for types::CatCode {
    fn parse_impl<S: TexlangState>(input: &mut vm::ExpandedStream<S>) -> txl::Result<Self> {
        let (token, i, _) = parse_integer(input)?;
        if let Ok(val_u8) = u8::try_from(i) {
            if let Ok(cat_code) = types::CatCode::try_from(val_u8) {
                return Ok(cat_code);
            }
        }
        input.error(parse::Error {
            expected: "a category code number (an integer in the range [0, 15])".into(),
            got: Some(token),
            got_override: format!["got the integer {i}"],
            annotation_override: "this is where the number started".into(),
            guidance: "".into(),
            additional_notes: vec![],
        })?;
        Ok(types::CatCode::try_from(0).unwrap())
    }
}

const GUIDANCE_BEGINNING: &str =
    "a number begins with zero or more minus signs followed by one of the following:
- A decimal digit (0-9), which begins a decimal number.
- The character ', which indicates the beginning of an octal number
- The character \", which indicates the beginning of a hexadecimal number
- The character `, followed by a character token. The character is converted into its UTF-8 number.
- A command that references a variable, like \\year.
";

/// TeX.2021.440 (scan_int)
pub(crate) fn parse_integer<S: TexlangState>(
    stream: &mut vm::ExpandedStream<S>,
) -> txl::Result<(token::Token, i32, Option<u8>)> {
    let sign = parse_optional_signs(stream)?;
    let first_token = stream.next_or_err(NumberEndOfInputError {})?;
    let (result, radix) = match first_token.value() {
        Value::Other('0') => (parse_constant::<S, 10>(stream, 0)?, Some(10_u8)),
        Value::Other('1') => (parse_constant::<S, 10>(stream, 1)?, Some(10_u8)),
        Value::Other('2') => (parse_constant::<S, 10>(stream, 2)?, Some(10_u8)),
        Value::Other('3') => (parse_constant::<S, 10>(stream, 3)?, Some(10_u8)),
        Value::Other('4') => (parse_constant::<S, 10>(stream, 4)?, Some(10_u8)),
        Value::Other('5') => (parse_constant::<S, 10>(stream, 5)?, Some(10_u8)),
        Value::Other('6') => (parse_constant::<S, 10>(stream, 6)?, Some(10_u8)),
        Value::Other('7') => (parse_constant::<S, 10>(stream, 7)?, Some(10_u8)),
        Value::Other('8') => (parse_constant::<S, 10>(stream, 8)?, Some(10_u8)),
        Value::Other('9') => (parse_constant::<S, 10>(stream, 9)?, Some(10_u8)),
        Value::Other('\'') => (parse_constant::<S, 8>(stream, 0)?, Some(8_u8)),
        Value::Other('"') => (parse_constant::<S, 16>(stream, 0)?, Some(16_u8)),
        Value::Other('`') => (parse_character(stream)?, None),
        Value::CommandRef(command_ref) => (
            parse_internal_number(stream, first_token, command_ref)?.integer(),
            None,
        ),
        // TeX.2021.446
        _ => {
            stream.back(first_token);
            stream.error(parse::Error::new(
                "the beginning of a number",
                Some(first_token),
                GUIDANCE_BEGINNING,
            ))?;
            (0, None)
        }
    };
    let result = match sign {
        None => result,
        // The only i32 that is not safe to multiply by -1 is i32::MIN.
        // Experimentally we observe in this case that TeX wraps and the result
        // is i32::MIN again.
        Some(_) => result.wrapping_mul(-1),
    };
    Ok((first_token, result, radix))
}

#[derive(Debug)]
pub(crate) enum InternalNumber {
    Integer(i32),
    Dimen(core::Scaled),
    Glue(core::Glue),
}

impl InternalNumber {
    pub(crate) fn integer(&self) -> i32 {
        use InternalNumber::*;
        match self {
            Integer(i) => *i,
            Dimen(scaled) => scaled.0,
            Glue(glue) => glue.width.0,
        }
    }
}

/// This function reimplements TeX.2021.413 (scan_something_internal) under the following
/// conditions:
///
/// - level is int_val, dimen_val, glue_val or mu_val; i.e., the call to this function
///   is looking for a number, not a token list or font. The token list or font cases
///   are handled elsewhere.
///
/// - negative is false. The negative=true case is handled by the caller to this function.
///
/// - The logic around casting between types (i.e. TeX.2021.429) is omitted. Instead
///   callers of this function perform the casting. The motivation is to make the code
///   more explicit and avoid parameters that change logic and/or return types.
pub(crate) fn parse_internal_number<S: TexlangState>(
    input: &mut vm::ExpandedStream<S>,
    first_token: token::Token,
    command_ref: CommandRef,
) -> txl::Result<InternalNumber> {
    let cmd = input.commands_map().get_command(&command_ref);
    match cmd {
        Some(command::Command::Variable(cmd)) => {
            match cmd.clone().value(first_token, input)? {
                variable::ValueRef::Int(i) => Ok(InternalNumber::Integer(*i)),
                variable::ValueRef::CatCode(c) => Ok(InternalNumber::Integer(*c as i32)),
                variable::ValueRef::MathCode(c) => Ok(InternalNumber::Integer(c.0 as i32)),
                variable::ValueRef::Dimen(d) => Ok(InternalNumber::Dimen(*d)),
                variable::ValueRef::Glue(g) => Ok(InternalNumber::Glue(*g)),
                variable::ValueRef::Font(_) => {
                    // This case behaves identically to the TokenListCase
                    todo!("scan a font into an int?");
                }
                variable::ValueRef::TokenList(_) => Err(input.fatal_error(
                    parse::Error::new(
                        "the beginning of a number",
                        Some(first_token),
                        GUIDANCE_BEGINNING,
                    )
                    .with_annotation_override("token list variable"),
                )),
            }
        }
        Some(command::Command::Character(c)) => Ok(InternalNumber::Integer(*c as i32)),
        Some(command::Command::MathCharacter(c)) => Ok(InternalNumber::Integer(c.0 as i32)),
        None
        | Some(
            command::Command::Execution(..)
            | command::Command::Expansion(..)
            | command::Command::Macro(..)
            | command::Command::CharacterTokenAlias(..)
            | command::Command::Font(..),
        ) => {
            let err = parse::Error::new(
                "the beginning of a number",
                Some(first_token),
                GUIDANCE_BEGINNING,
            )
            .with_annotation_override(match cmd {
                None => "undefined control sequence".to_string(),
                Some(cmd) => format!["control sequence referencing {cmd}"],
            });
            input.expansions_mut().push(first_token);
            Err(input.fatal_error(err))
        }
    }
}

#[derive(Debug)]
struct NumberEndOfInputError;

impl error::EndOfInputError for NumberEndOfInputError {
    fn doing(&self) -> String {
        "parsing a number".into()
    }
    fn notes(&self) -> Vec<error::display::Note> {
        vec![GUIDANCE_BEGINNING.into()]
    }
}

/// Parses optional signs and spaces.
///
/// If the combination of the signs is positive, [None] is returned.
/// Otherwise, the Token corresponding to the last negative sign is returned.
///
/// This is TeX.2021.441.
pub fn parse_optional_signs<S: TexlangState>(
    stream: &mut vm::ExpandedStream<S>,
) -> txl::Result<Option<token::Token>> {
    let mut result = None;
    while let Some((sign, token)) = get_optional_element_with_token![
        stream,
        Value::Other('+') => true,
        Value::Other('-') => false,
        Value::Space(_) => true,
    ] {
        result = match (result, sign) {
            (None, false) => Some(token),
            (Some(_), false) => None,
            (result, true) => result,
        };
    }
    Ok(result)
}

// TeX.2021.442
fn parse_character<S: TexlangState>(input: &mut vm::ExpandedStream<S>) -> txl::Result<i32> {
    // BUG: should be from the unexpanded stream
    let c = {
        let token = input.next_or_err(CharacterError {})?;
        match token.value() {
            Value::CommandRef(token::CommandRef::ControlSequence(cs_name)) => {
                let name = input.vm().cs_name_interner().resolve(cs_name).unwrap();
                let mut iter = name.chars();
                match (iter.next(), iter.count()) {
                    // (None, 0) => ?! TODO: add a test for this.
                    // Should be something like:
                    // \expandafter \i \expandafter ` \csname\endcsname
                    (Some(c), 0) => c,
                    _ => {
                        input.error(parse::Error::new(
                            "a character",
                            Some(token),
                            "a character is a character token or single-character control sequence like \\a",
                        ))?;
                        '0'
                    }
                }
            }
            _ => token.char().unwrap(),
        }
    };
    super::OptionalSpace::parse(input)?;
    Ok(c as i32)
}

#[derive(Debug)]
struct CharacterError;

impl error::EndOfInputError for CharacterError {
    fn doing(&self) -> String {
        "parsing a character".into()
    }

    fn notes(&self) -> Vec<error::display::Note> {
        vec![
            r"a character is a character token or single-character control sequence like \a".into(),
        ]
    }
}

/// TeX.2021.444-445
// TODO: why is the radix a const parameter?
fn parse_constant<S: TexlangState, const RADIX: i32>(
    stream: &mut vm::ExpandedStream<S>,
    mut result: i32,
) -> txl::Result<i32> {
    let mut started = RADIX == 10;
    let mut too_big = false;
    loop {
        let next = match stream.next()? {
            None => break,
            Some(next) => next,
        };
        let lsd_or = match next.value() {
            token::Value::Other(c) => {
                let d = (c as u32).wrapping_sub('0' as u32);
                if d < 10 && d < (RADIX as u32) {
                    Some(d as i32)
                } else if RADIX == 16 {
                    let d = (c as u32).wrapping_sub('A' as u32);
                    if d < 6 {
                        Some(d as i32 + 10)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            token::Value::Letter(c) => {
                let d = (c as u32).wrapping_sub('A' as u32);
                if RADIX == 16 && d < 6 {
                    Some(d as i32 + 10)
                } else {
                    None
                }
            }
            _ => None,
        };
        let lsd = match lsd_or {
            None => {
                stream.back(next);
                break;
            }
            Some(lsd) => lsd,
        };
        started = true;
        result = match add_lsd::<RADIX>(result, lsd) {
            Some(n) => n,
            None => {
                if !too_big {
                    stream.error(add_lsd_error::<RADIX>(next, result, lsd))?;
                    too_big = true;
                }
                i32::MAX
            }
        }
    }
    if !started {
        let (expected, guidance) = match RADIX {
            8 => {
                ("an octal digit",
                "an octal digit is a token with value 0-7 and category other")
            },
            16 => {
                ("a hexadecimal digit",
                "a hexadecimal digit is either:\n- A character token with value 0-9 and category other, or\n- A character token with value A-F and category letter or other")
            }
            _ => unreachable!(),
        };
        let got = stream.peek()?;
        stream.error(parse::Error::new(expected, got, guidance))?;
    }
    super::OptionalSpace::parse(stream)?;
    Ok(result)
}

fn add_lsd<const RADIX: i32>(n: i32, lsd: i32) -> Option<i32> {
    match n.checked_mul(RADIX) {
        None => None,
        Some(n) => n.checked_add(lsd),
    }
}

fn add_lsd_error<const RADIX: i32>(token: token::Token, n: i32, lsd: i32) -> parse::Error {
    let (got, range) = match RADIX {
        8 => (
            format!["got '{n:o}{lsd:o}"],
            format!["'{:o}, '{:o}", i32::MIN, i32::MAX],
        ),
        10 => (
            format!["got {n}{lsd}"],
            format!["{}, {}", i32::MIN, i32::MAX],
        ),
        16 => (
            format!["got 0x{n:X}{lsd:X}"],
            format!["0x{:X}, 0x{:X}", i32::MIN, i32::MAX],
        ),
        _ => panic!("radix must be 8, 10 or 16"),
    };
    parse::Error {
        expected: format!["a number in the range [{range}]"],
        got: Some(token),
        got_override: got,
        annotation_override: "this digit makes the number too big".into(),
        guidance: "".into(),
        additional_notes: vec![],
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::testing::*;

    parse_success_tests![
        (octal_0, "'0", 0),
        (octal_1, "'1", 1),
        (octal_2, "'2", 2),
        (octal_3, "'3", 3),
        (octal_4, "'4", 4),
        (octal_5, "'5", 5),
        (octal_6, "'6", 6),
        (octal_7, "'7", 7),
        (octal_8, "'10", 8),
        (octal_9, "'11", 9),
        (octal_19, "'12", 10),
        (octal_11, "'13", 11),
        (octal_12, "'14", 12),
        (octal_13, "'15", 13),
        (octal_14, "'16", 14),
        (octal_15, "'17", 15),
        (octal_129, "'201", 129),
        (octal_max, "'17777777777", 2147483647),
        (octal_min, "-'17777777777", -2147483647),
        (decimal_0, "0", 0),
        (decimal_1, "1", 1),
        (decimal_2, "2", 2),
        (decimal_3, "3", 3),
        (decimal_4, "4", 4),
        (decimal_5, "5", 5),
        (decimal_6, "6", 6),
        (decimal_7, "7", 7),
        (decimal_8, "8", 8),
        (decimal_9, "9", 9),
        (decimal_10, "10", 10),
        (decimal_11, "11", 11),
        (decimal_12, "12", 12),
        (decimal_13, "13", 13),
        (decimal_14, "14", 14),
        (decimal_15, "15", 15),
        (decimal_16, "16", 16),
        (decimal_17, "17", 17),
        (decimal_18, "18", 18),
        (decimal_19, "19", 19),
        (decimal_1_with_0_padding, "00019", 19),
        (decimal_201, "201", 201),
        (decimal_max, "2147483647", 2147483647),
        (decimal_min, "-2147483647", -2147483647),
        (hexadecimal_0, "\"0", 0),
        (hexadecimal_1, "\"1", 1),
        (hexadecimal_2, "\"2", 2),
        (hexadecimal_3, "\"3", 3),
        (hexadecimal_4, "\"4", 4),
        (hexadecimal_5, "\"5", 5),
        (hexadecimal_6, "\"6", 6),
        (hexadecimal_7, "\"7", 7),
        (hexadecimal_8, "\"8", 8),
        (hexadecimal_9, "\"9", 9),
        (hexadecimal_10, "\"A", 10),
        (hexadecimal_11, "\"B", 11),
        (hexadecimal_12, "\"C", 12),
        (hexadecimal_13, "\"D", 13),
        (hexadecimal_14, "\"E", 14),
        (hexadecimal_15, "\"F", 15),
        (hexadecimal_16, "\"10", 16),
        (hexadecimal_17, "\"11", 17),
        (hexadecimal_18, "\"12", 18),
        (hexadecimal_19, "\"13", 19),
        (hexadecimal_20, "\"14", 20),
        (hexadecimal_21, "\"15", 21),
        (hexadecimal_22, "\"16", 22),
        (hexadecimal_23, "\"17", 23),
        (hexadecimal_24, "\"18", 24),
        (hexadecimal_25, "\"19", 25),
        (hexadecimal_26, "\"1A", 26),
        (hexadecimal_27, "\"1B", 27),
        (hexadecimal_28, "\"1C", 28),
        (hexadecimal_29, "\"1D", 29),
        (hexadecimal_30, "\"1E", 30),
        (hexadecimal_31, "\"1F", 31),
        (hexadecimal_513, "\"201", 513),
        (hexadecimal_max, "\"7FFFFFFF", 2147483647),
        (hexadecimal_min, "-\"7FFFFFFF", -2147483647),
        (number_from_character, "`A", 65),
        (number_from_length_1_control_sequence, r"`\A", 65),
        (number_from_character_non_ascii, "`ö", 0x00F6),
        (
            number_from_length_1_control_sequence_non_ascii,
            r"`\ö",
            0x00F6
        ),
        (signs_plus, r"+4", 4),
        (signs_minus, r"-4", -4),
        (signs_plus_minus, r"+-4", -4),
        (signs_minus_minus, r"--4", 4),
        (signs_minus_minus_spaces, r"  -  - 4", 4),
    ];

    #[derive(Default)]
    struct State;

    impl TexlangState for State {
        fn cat_code(&self, c: char) -> types::CatCode {
            if c == '9' {
                return types::CatCode::Letter;
            }
            types::CatCode::PLAIN_TEX_DEFAULTS
                .get(c as usize)
                .copied()
                .unwrap_or_default()
        }
    }

    parse_failure_tests![
        i32,
        State,
        (number_with_letter_catcode, "9"),
        (octal_too_big, "'177777777770", i32::MAX),
        (octal_empty, "'"),
        (decimal_too_big_1, "2147483648", i32::MAX),
        (decimal_too_big_2, "500000000000000", i32::MAX),
        (decimal_too_negative_1, "-2147483648", -1 * i32::MAX),
        (decimal_too_negative_2, "-5000000000000", -1 * i32::MAX),
        (hexadecimal_too_big, "\"7FFFFFFF0", i32::MAX),
        (hexadecimal_empty, "\""),
        (character, "A"),
        // TODO: the test is messed up because a space gets appended to the input
        // (character_missing, r"`", '0' as i32),
        (control_sequence_too_big, r"`\BC", '0' as i32),
    ];

    parse_failure_tests![
        Uint::<16>,
        State,
        (number_too_big, "16"),
        (number_is_negative, "-1"),
    ];
}
