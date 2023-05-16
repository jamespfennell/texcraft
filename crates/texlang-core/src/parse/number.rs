use std::rc;

use crate::token::catcode;
use crate::token::Value;
use crate::variable;
use crate::vm::RefVM;
use crate::vm::TokenStream;
use crate::{command, error, token, vm};
use num_traits::PrimInt;

/// Parses a number from the provided stream.
///
/// The number may be octal, decimal_, hexadecimal_, cast from a character token, or read
/// from an internal registers. The full definition of a number in the TeX grammar
/// is given on page X of the TeXBook.
#[inline]
pub fn parse_number<S, I: AsMut<vm::ExpansionInput<S>>, T: PrimInt>(
    stream: &mut I,
) -> anyhow::Result<T> {
    parse_number_internal(stream.as_mut())
}

fn parse_number_internal<S, T: PrimInt>(stream: &mut vm::ExpansionInput<S>) -> anyhow::Result<T> {
    let sign = parse_optional_signs(stream)?;
    let modulus: T = match stream.next()? {
        None => return Err(parse_number_error(None)),
        Some(token) => match token.value() {
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
            Value::ControlSequence(name) => {
                let cmd = stream.base().commands_map.get_command(&name);
                if let Some(command::Command::Variable(cmd)) = cmd {
                    read_number_from_variable(token, cmd.clone(), stream)?
                } else {
                    return Err(parse_number_error(Some(token)));
                }
            }
            _ => return Err(parse_number_error(Some(token))),
        },
    };
    get_optional_element![stream, Value::Space(_) => (),];
    match sign {
        None => Ok(modulus),
        Some(minus_token) => match num_traits::cast::cast::<_, T>(-1) {
            None => Err(error::TokenError::new(minus_token, "unexpected negative number").cast()),
            Some(sign) => Ok(sign * modulus),
        },
    }
}

#[inline]
pub fn parse_catcode<S, I: AsMut<vm::ExpansionInput<S>>>(
    stream: &mut I,
) -> anyhow::Result<catcode::CatCode> {
    parse_catcode_internal(stream.as_mut())
}

fn parse_catcode_internal<S>(
    stream: &mut vm::ExpansionInput<S>,
) -> anyhow::Result<catcode::CatCode> {
    let val: usize = parse_number(stream)?;
    if let Ok(val_u8) = u8::try_from(val) {
        if let Some(cat_code) = catcode::CatCode::from_int(val_u8) {
            return Ok(cat_code);
        }
    }
    // TODO: this should be a token error with the last digit as the token
    Err(anyhow::anyhow!(
        "the number {} is not a valid category code",
        val
    ))
}

/// Parses optional signs and spaces.
///
/// If the combination of the signs is positive, [None] is returned.
/// Otherwise, the Token corresponding to the last negative sign is returned.
fn parse_optional_signs<S>(
    stream: &mut vm::ExpansionInput<S>,
) -> anyhow::Result<Option<token::Token>> {
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

fn parse_number_error(token: Option<token::Token>) -> anyhow::Error {
    match token {
        None => {
            error::EndOfInputError::new("unexpected end of input while parsing a relation").cast()
        }
        Some(token) => match token.value() {
            Value::ControlSequence(..) => {
                error::TokenError::new(token, "unexpected control sequence while parsing a number")
                    .cast()
            }
            _ => error::TokenError::new(token, "unexpected character token when parsing a number")
                .cast(),
        },
    }
}

fn read_number_from_variable<S, T: PrimInt>(
    token: token::Token,
    cmd: rc::Rc<variable::Command<S>>,
    stream: &mut vm::ExpansionInput<S>,
) -> anyhow::Result<T> {
    match cmd.resolve(token, stream)?.value(stream) {
        variable::ValueRef::Int(i) => {
            // TODO: this case may not work; e.g., requesting a register index u8 from a register value i32
            Ok(num_traits::cast::cast(*i).unwrap())
        }
        variable::ValueRef::CatCode(c) => {
            // This will always work because cat codes are between 0 and 15 inclusive and can
            // fit in any integral type.
            // TODO: implement From on catcode directly
            Ok(num_traits::cast::cast(c.int()).unwrap())
        }
    }
}

fn parse_character<S, T: PrimInt>(stream: &mut vm::ExpansionInput<S>) -> anyhow::Result<T> {
    match stream.next()? {
        None => Err(error::EndOfInputError::new(
            "unexpected end of input while parsing a character token",
        )
        .cast()),
        Some(token) => match token.value() {
            Value::ControlSequence(..) => Err(error::TokenError::new(
                token,
                "unexpected control sequence while parsing a character number",
            )
            .cast()),
            // TODO: error if the character is too big!
            _ => Ok(num_traits::cast::cast(token.char().unwrap() as i32).unwrap()),
        },
    }
}

fn parse_octal<S, T: PrimInt>(stream: &mut vm::ExpansionInput<S>) -> anyhow::Result<T> {
    let mut n = num_traits::cast::cast(get_element![
        stream,
        parse_number_error,
        Value::Other('0') => 0,
        Value::Other('1') => 1,
        Value::Other('2') => 2,
        Value::Other('3') => 3,
        Value::Other('4') => 4,
        Value::Other('5') => 5,
        Value::Other('6') => 6,
        Value::Other('7') => 7,
    ]?)
    .unwrap();
    while let Some(lsd) = get_optional_element![
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
        n = add_lsd(n, 8, lsd);
    }
    Ok(n)
}

fn parse_decimal<S, T: PrimInt>(
    stream: &mut vm::ExpansionInput<S>,
    n_start: i8,
) -> anyhow::Result<T> {
    let mut n: T = num_traits::cast::cast(n_start).unwrap();
    while let Some(lsd) = get_optional_element![
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
        n = add_lsd(n, 10, lsd);
    }
    Ok(n)
}

fn parse_hexadecimal<S, T: PrimInt>(stream: &mut vm::ExpansionInput<S>) -> anyhow::Result<T> {
    let mut n: T = num_traits::cast::cast(get_element![
        stream,
        parse_number_error,
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
    ]?)
    .unwrap();
    while let Some(lsd) = get_optional_element![
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
        n = add_lsd(n, 16, lsd);
    }
    Ok(n)
}

fn add_lsd<T: PrimInt>(n: T, base: i32, lsd: i32) -> T {
    n * num_traits::cast::cast(base).unwrap() + num_traits::cast::cast(lsd).unwrap()
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::parse::testing;
    use crate::token::catcode;

    macro_rules! parse_number_tests {
        ($( ($name: ident, $input: expr, $number: expr),)+) => {
            $(
            #[test]
            fn $name() {
                let mut vm = testing::new_vm($input);
                let result: i32 = parse_number(vm::ExpansionInput::new(&mut vm)).unwrap();
                assert_eq![result, $number];
            }
            )+
        };
    }

    parse_number_tests![
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
        (signs_plus, r"+4", 4),
        (signs_minus, r"-4", -4),
        (signs_plus_minus, r"+-4", -4),
        (signs_minus_minus, r"--4", 4),
        (signs_minus_minus_spaces, r"  -  - 4", 4),
    ];

    #[test]
    fn number_with_letter_catcode() {
        let mut map = catcode::CatCodeMap::new_with_tex_defaults();
        map.insert('1', catcode::CatCode::Letter);
        let mut vm = vm::VM::<()>::new(map, HashMap::new(), (), None);
        vm.push_source("".to_string(), r"1".to_string()).unwrap();
        let input = crate::vm::ExecutionInput::new(&mut vm);
        let result: anyhow::Result<i32> = parse_number(input);
        if let Ok(_) = result {
            panic!["Parsed a relation from invalid input"];
        }
    }
}
