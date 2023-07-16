//! Number parsing.
//!
//! The number may be octal, decimal_, hexadecimal_, cast from a character token, or read
//! from an internal registers. The full definition of a number in the TeX grammar
//! is given on page X of the TeXBook.

use std::fmt::Display;
use std::rc;

use crate::token::trace;
use crate::token::CommandRef;
use crate::token::Value;
use crate::traits::*;
use crate::variable;
use crate::*;

impl<S: TexlangState> Parsable<S> for i32 {
    fn parse_impl(input: &mut vm::ExpandedStream<S>) -> Result<Self, Box<error::Error>> {
        let (_, i): (token::Token, i32) = parse_number_internal(input)?;
        Ok(i)
    }
}

pub struct Uint<const N: usize>(pub usize);

impl<S: TexlangState, const N: usize> Parsable<S> for Uint<N> {
    fn parse_impl(input: &mut vm::ExpandedStream<S>) -> Result<Self, Box<error::Error>> {
        let (first_token, i): (token::Token, i32) = parse_number_internal(input)?;
        if i < 0 || i as usize >= N {
            Err(OutOfBoundsError::<N> {
                first_token: input.trace(first_token),
                got: i,
            }
            .into())
        } else {
            Ok(Uint(i as usize))
        }
    }
}

#[derive(Debug)]
struct OutOfBoundsError<const N: usize> {
    first_token: trace::SourceCodeTrace,
    got: i32,
}

impl<const N: usize> error::TexError for OutOfBoundsError<N> {
    fn kind(&self) -> error::Kind {
        error::Kind::Token(&self.first_token)
    }

    fn title(&self) -> String {
        format!(
            "expected an integer in the range [0, {}), got {}",
            N, self.got
        )
    }
}

// TODO: I think we'll likely need to get rid of this implementation.
// In Knuth's TeX the integer scanning routine is only for i32 integers,
// and all other integers (like 4-bit integers for \openin) are determined
// by first getting an i32 and then casting if possible.
impl<S: TexlangState> Parsable<S> for usize {
    fn parse_impl(input: &mut vm::ExpandedStream<S>) -> Result<Self, Box<error::Error>> {
        let (_, i): (token::Token, usize) = parse_number_internal(input)?;
        Ok(i)
    }
}

impl<S: TexlangState> Parsable<S> for token::CatCode {
    fn parse_impl(input: &mut vm::ExpandedStream<S>) -> Result<Self, Box<error::Error>> {
        let (token, i): (token::Token, usize) = parse_number_internal(input)?;
        if let Ok(val_u8) = u8::try_from(i) {
            if let Ok(cat_code) = token::CatCode::try_from(val_u8) {
                return Ok(cat_code);
            }
        }
        Err(parse::Error {
            expected: "a category code number (an integer in the range [0, 15])".into(),
            got: input.vm().trace(token),
            got_override: format!["got the integer {i}"],
            annotation_override: "this is where the number started".into(),
            guidance: "".into(),
            additional_notes: vec![],
        }
        .into())
    }
}

trait PrimInt: Copy + Display + From<u8> + TryFrom<i32> + std::fmt::Octal + std::fmt::UpperHex {
    const MIN: Self;
    const MAX: Self;

    fn flip_sign(self) -> Option<Self>;

    fn checked_mul(self, rhs: u8) -> Option<Self>;
    fn checked_add(self, rhs: u8) -> Option<Self>;
}

impl PrimInt for usize {
    const MIN: Self = usize::MIN;
    const MAX: Self = usize::MAX;

    fn flip_sign(self) -> Option<Self> {
        match self {
            0 => Some(0),
            _ => None,
        }
    }

    fn checked_mul(self, rhs: u8) -> Option<Self> {
        self.checked_mul(rhs as usize)
    }

    fn checked_add(self, rhs: u8) -> Option<Self> {
        self.checked_add(rhs as usize)
    }
}

impl PrimInt for i32 {
    const MIN: Self = i32::MIN;
    const MAX: Self = i32::MAX;

    fn flip_sign(self) -> Option<Self> {
        self.checked_mul(-1)
    }

    fn checked_mul(self, rhs: u8) -> Option<Self> {
        self.checked_mul(rhs as i32)
    }

    fn checked_add(self, rhs: u8) -> Option<Self> {
        self.checked_add(rhs as i32)
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

fn parse_number_internal<S: TexlangState, T: PrimInt>(
    stream: &mut vm::ExpandedStream<S>,
) -> Result<(token::Token, T), Box<error::Error>> {
    let sign = parse_optional_signs(stream)?;
    let first_token = match stream.next()? {
        None => {
            return Err(parse::Error::new(
                stream.vm(),
                "the beginning of a number",
                None,
                GUIDANCE_BEGINNING,
            )
            .into())
        }
        Some(token) => token,
    };
    let modulus: T = match first_token.value() {
        Value::Other('0') => parse_decimal(stream, 0)?,
        Value::Other('1') => parse_decimal(stream, 1)?,
        Value::Other('2') => parse_decimal(stream, 2)?,
        Value::Other('3') => parse_decimal(stream, 3)?,
        Value::Other('4') => parse_decimal(stream, 4)?,
        Value::Other('5') => parse_decimal(stream, 5)?,
        Value::Other('6') => parse_decimal(stream, 6)?,
        Value::Other('7') => parse_decimal(stream, 7)?,
        Value::Other('8') => parse_decimal(stream, 8)?,
        Value::Other('9') => parse_decimal(stream, 9)?,
        Value::Other('\'') => parse_octal(stream)?,
        Value::Other('"') => parse_hexadecimal(stream)?,
        Value::Other('`') => parse_character(stream)?,
        Value::CommandRef(command_ref) => {
            let cmd = stream.commands_map().get_command(&command_ref);
            if let Some(command::Command::Variable(cmd)) = cmd {
                read_number_from_variable(first_token, cmd.clone(), stream)?
            } else {
                return Err(parse::Error::new(
                    stream.vm(),
                    "the beginning of a number",
                    Some(first_token),
                    GUIDANCE_BEGINNING,
                )
                .with_annotation_override(match cmd {
                    None => "undefined control sequence".to_string(),
                    Some(cmd) => format!["control sequence referencing {cmd}"],
                })
                .into());
            }
        }
        _ => {
            return Err(parse::Error::new(
                stream.vm(),
                "the beginning of a number",
                Some(first_token),
                GUIDANCE_BEGINNING,
            )
            .into());
        }
    };
    get_optional_element![stream, Value::Space(_) => (),];
    let signed_value = match sign {
        None => modulus,
        Some(minus_token) => match modulus.flip_sign() {
            None => {
                return Err(parse::Error::new(
                    stream.vm(),
                    "a positive number",
                    Some(minus_token),
                    "",
                )
                .with_got_override("got a negative number")
                .with_annotation_override("this minus sign makes the number negative")
                .into());
            }
            Some(signed_value) => signed_value,
        },
    };
    Ok((first_token, signed_value))
}

/// Parses optional signs and spaces.
///
/// If the combination of the signs is positive, [None] is returned.
/// Otherwise, the Token corresponding to the last negative sign is returned.
fn parse_optional_signs<S: TexlangState>(
    stream: &mut vm::ExpandedStream<S>,
) -> Result<Option<token::Token>, Box<error::Error>> {
    let mut result = None;
    while let Some((sign, token)) = get_optional_element_with_token![
        stream,
        Value::Other('+') => true,
        Value::Other('-') => false,
        Value::Space(_) => true,
    ] {
        result = match (result.is_none(), sign) {
            (true, true) => None,
            (true, false) => Some(token),
            (false, true) => result,
            (false, false) => None,
        };
    }
    Ok(result)
}

fn read_number_from_variable<S: TexlangState, T: PrimInt>(
    token: token::Token,
    cmd: rc::Rc<variable::Command<S>>,
    input: &mut vm::ExpandedStream<S>,
) -> Result<T, Box<error::Error>> {
    let i = match cmd.resolve(token, input)?.value(input) {
        variable::ValueRef::Int(i) => *i,
        variable::ValueRef::CatCode(c) => *c as i32,
    };
    match TryInto::<T>::try_into(i) {
        Ok(n) => Ok(n),
        Err(_) => Err(parse::Error::new(
            input.vm(),
            format!["a number in the range [{}, {}]", T::MIN, T::MAX],
            Some(token),
            "",
        )
        .with_got_override(format!["got the number {i}"])
        .with_annotation_override("the number was read from this variable")
        .into()),
    }
}

fn parse_character<S: TexlangState, T: PrimInt>(
    input: &mut vm::ExpandedStream<S>,
) -> Result<T, Box<error::Error>> {
    let c_or: Result<char, Option<token::Token>> = match input.next()? {
        None => Err(None),
        Some(token) => match token.value() {
            Value::CommandRef(CommandRef::ControlSequence(cs_name)) => {
                let name = input.vm().cs_name_interner().resolve(cs_name).unwrap();
                let mut iter = name.chars();
                match (iter.next(), iter.count()) {
                    (Some(c), 0) => Ok(c),
                    _ => Err(Some(token)),
                }
            }
            _ => Ok(token.char().unwrap()),
        },
    };
    let c =
        match c_or {
            Ok(c) => c,
            Err(optional_token) => return Err(parse::Error::new(
                input.vm(),
                "a character",
                optional_token,
                "a character is a character token or single-character control sequence like \\a",
            )
            .into()),
        };
    // This cast always succeeds at the time of writing.
    // This is because `c as u32` returns c's Unicode code point, which
    // is in the range [0, 2^21] and fits in the two types (i32, usize)
    // that the parsing code currently supports.
    match TryInto::<T>::try_into(c as i32) {
        Ok(t) => Ok(t),
        Err(_) => panic!(
            "can't cast unicode character {} ({}) to integer",
            c, c as u32
        ),
    }
}

fn parse_octal<S: TexlangState, T: PrimInt>(
    stream: &mut vm::ExpandedStream<S>,
) -> Result<T, Box<error::Error>> {
    let mut n = get_required_element![
        stream,
        "an octal digit",
        "an octal digit is a token with value 0-7 and category other",
        Value::Other('0') => 0,
        Value::Other('1') => 1,
        Value::Other('2') => 2,
        Value::Other('3') => 3,
        Value::Other('4') => 4,
        Value::Other('5') => 5,
        Value::Other('6') => 6,
        Value::Other('7') => 7,
    ]?
    .into();
    while let Some((lsd, token)) = get_optional_element_with_token![
        stream,
        Value::Other('0') => 0,
        Value::Other('1') => 1,
        Value::Other('2') => 2,
        Value::Other('3') => 3,
        Value::Other('4') => 4,
        Value::Other('5') => 5,
        Value::Other('6') => 6,
        Value::Other('7') => 7,
    ] {
        n = match add_lsd::<T, 8>(n, lsd) {
            Some(n) => n,
            None => return Err(add_lsd_error::<S, T, 8>(token, stream, n, lsd)),
        }
    }
    Ok(n)
}

fn parse_decimal<S: TexlangState, T: PrimInt>(
    stream: &mut vm::ExpandedStream<S>,
    n_start: u8,
) -> Result<T, Box<error::Error>> {
    let mut n: T = n_start.into();
    while let Some((lsd, token)) = get_optional_element_with_token![
        stream,
        Value::Other('0') => 0,
        Value::Other('1') => 1,
        Value::Other('2') => 2,
        Value::Other('3') => 3,
        Value::Other('4') => 4,
        Value::Other('5') => 5,
        Value::Other('6') => 6,
        Value::Other('7') => 7,
        Value::Other('8') => 8,
        Value::Other('9') => 9,
    ] {
        n = match add_lsd::<T, 10>(n, lsd) {
            Some(n) => n,
            None => return Err(add_lsd_error::<S, T, 10>(token, stream, n, lsd)),
        }
    }
    Ok(n)
}

fn parse_hexadecimal<S: TexlangState, T: PrimInt>(
    stream: &mut vm::ExpandedStream<S>,
) -> Result<T, Box<error::Error>> {
    let mut n: T = get_required_element![
        stream,
        "a hexadecimal digit",
        "a hexadecimal digit is either:\n- A character token with value 0-9 and category other, or\n- A character token with value A-F and category letter or other",
        Value::Other('0') => 0,
        Value::Other('1') => 1,
        Value::Other('2') => 2,
        Value::Other('3') => 3,
        Value::Other('4') => 4,
        Value::Other('5') => 5,
        Value::Other('6') => 6,
        Value::Other('7') => 7,
        Value::Other('8') => 8,
        Value::Other('9') => 9,

        Value::Other('A') => 10,
        Value::Other('B') => 11,
        Value::Other('C') => 12,
        Value::Other('D') => 13,
        Value::Other('E') => 14,
        Value::Other('F') => 15,

        Value::Letter('A') => 10,
        Value::Letter('B') => 11,
        Value::Letter('C') => 12,
        Value::Letter('D') => 13,
        Value::Letter('E') => 14,
        Value::Letter('F') => 15,
    ]?.into();
    while let Some((lsd, token)) = get_optional_element_with_token![
        stream,
        Value::Other('0') => 0,
        Value::Other('1') => 1,
        Value::Other('2') => 2,
        Value::Other('3') => 3,
        Value::Other('4') => 4,
        Value::Other('5') => 5,
        Value::Other('6') => 6,
        Value::Other('7') => 7,
        Value::Other('8') => 8,
        Value::Other('9') => 9,

        Value::Other('A') => 10,
        Value::Other('B') => 11,
        Value::Other('C') => 12,
        Value::Other('D') => 13,
        Value::Other('E') => 14,
        Value::Other('F') => 15,

        Value::Letter('A') => 10,
        Value::Letter('B') => 11,
        Value::Letter('C') => 12,
        Value::Letter('D') => 13,
        Value::Letter('E') => 14,
        Value::Letter('F') => 15,
    ] {
        n = match add_lsd::<T, 16>(n, lsd) {
            Some(n) => n,
            None => return Err(add_lsd_error::<S, T, 16>(token, stream, n, lsd)),
        }
    }
    Ok(n)
}

fn add_lsd<T: PrimInt, const RADIX: u8>(n: T, lsd: u8) -> Option<T> {
    match n.checked_mul(RADIX) {
        None => None,
        Some(n) => n.checked_add(lsd),
    }
}

fn add_lsd_error<S: TexlangState, T: PrimInt, const RADIX: u8>(
    token: token::Token,
    input: &mut vm::ExpandedStream<S>,
    n: T,
    lsd: u8,
) -> Box<error::Error> {
    let (got, range) = match RADIX {
        8 => (
            format!["got '{n:o}{lsd:o}"],
            format!["'{:o}, '{:o}", T::MIN, T::MAX],
        ),
        10 => (format!["got {n}{lsd}"], format!["{}, {}", T::MIN, T::MAX]),
        16 => (
            format!["got 0x{n:X}{lsd:X}"],
            format!["0x{:X}, 0x{:X}", T::MIN, T::MAX],
        ),
        _ => panic!("radix must be 8, 10 or 16"),
    };
    parse::Error {
        expected: format!["a number in the range [{range}]"],
        got: input.vm().trace(token),
        got_override: got,
        annotation_override: "this digit makes the number too big".into(),
        guidance: "".into(),
        additional_notes: vec![],
    }
    .into()
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
        // BUG: variable `\count 0 -4 \count -\count 0 3` works in pdftex!
        // POSSIBLE BUG: how about -(2^31)? Should work?
    ];

    #[derive(Default)]
    struct State;

    impl TexlangState for State {
        fn cat_code(&self, c: char) -> token::CatCode {
            if c == '1' {
                return token::CatCode::Letter;
            }
            token::CatCode::PLAIN_TEX_DEFAULTS
                .get(c as usize)
                .copied()
                .unwrap_or_default()
        }
    }

    parse_failure_tests![i32, State, (number_with_letter_catcode, "1"),];
}
