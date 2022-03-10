use crate::prelude::*;
use crate::runtime::HasEnv;
use crate::variable;
use num_traits::PrimInt;

/// Parses a number from the provided stream.
///
/// The number may be octal, decimal, hexadecimal, cast from a character token, or read
/// from an internal registers. The full definition of a number in the TeX grammer
/// is given on page X of the TeXBook.
#[inline]
pub fn parse_number<S, I: AsMut<runtime::ExpandedInput<S>>, T: PrimInt>(
    stream: &mut I,
) -> anyhow::Result<T> {
    parse_number_internal(stream.as_mut())
}

fn parse_number_internal<S, T: PrimInt>(
    stream: &mut runtime::ExpandedInput<S>,
) -> anyhow::Result<T> {
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
            ControlSequence(name) => {
                let cmd = stream.base().commands_map.get(&name);
                if let Some(command::Command::Variable(cmd_ref)) = cmd {
                    // TODO: don't clone here, use the same trick as the driver?
                    let cmd = *cmd_ref;
                    let variable = cmd.resolve(token, stream)?;
                    read_number_from_address(variable, stream)?
                } else {
                    println!("Command: {:?}", cmd);
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

/// Parses optional signs and spaces.
///
/// If the combination of the signs is positive, [None] is returned.
/// Otherwise, the Token corresponding to the last negative sign is returned.
fn parse_optional_signs<S>(
    stream: &mut runtime::ExpandedInput<S>,
) -> anyhow::Result<Option<Token>> {
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

fn parse_number_error(token: Option<Token>) -> anyhow::Error {
    match token {
        None => {
            error::EndOfInputError::new("unexpected end of input while parsing a relation").cast()
        }
        Some(token) => match token.value() {
            ControlSequence(..) => {
                error::TokenError::new(token, "unexpected control sequence while parsing a number")
                    .cast()
            }
            _ => error::TokenError::new(token, "unexpected character token when parsing a number")
                .cast(),
        },
    }
}

fn read_number_from_address<S, T: PrimInt>(
    variable: variable::Variable<S>,
    stream: &mut runtime::ExpandedInput<S>,
) -> anyhow::Result<T> {
    match variable {
        variable::Variable::Int(variable) => {
            // TODO: this case may not work; e.g., requesting a register index u8 from a register value i32
            Ok(num_traits::cast::cast(*variable.get(stream.state())).unwrap())
        }
        variable::Variable::BaseInt(variable) => {
            // TODO: this case may not work; e.g., requesting a register index u8 from a register value i32
            Ok(num_traits::cast::cast(*variable.get(stream.base())).unwrap())
        }
        variable::Variable::CatCode(v) => {
            // This will always work becuase cat codes are between 0 and 15 inclusive and can
            // fit in any integral type.
            // TODO: implement From on catcode directly
            Ok(num_traits::cast::cast((*v.get(stream.base())).int()).unwrap())
        }
    }
}

fn parse_character<S, T: PrimInt>(stream: &mut runtime::ExpandedInput<S>) -> anyhow::Result<T> {
    match stream.next()? {
        None => Err(error::EndOfInputError::new(
            "unexpected end of input while parsing a character token",
        )
        .cast()),
        Some(token) => match token.value() {
            ControlSequence(..) => Err(error::TokenError::new(
                token,
                "unexpected control sequence while parsing a character number",
            )
            .cast()),
            // TODO: error if the character is too big!
            _ => Ok(num_traits::cast::cast(token.char().unwrap() as i32).unwrap()),
        },
    }
}

fn parse_octal<S, T: PrimInt>(stream: &mut runtime::ExpandedInput<S>) -> anyhow::Result<T> {
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
    stream: &mut runtime::ExpandedInput<S>,
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

fn parse_hexadecimal<S, T: PrimInt>(stream: &mut runtime::ExpandedInput<S>) -> anyhow::Result<T> {
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
    use super::*;
    use crate::parse::testutil;
    use crate::token::catcode;

    macro_rules! parse_number_test {
        ($input: expr, $number: expr) => {
            let mut env = testutil::new_env($input);
            let result: i32 = parse_number(runtime::ExpandedInput::new(&mut env)).unwrap();
            assert_eq![result, $number];
        };
    }

    #[test]
    fn octal_basic() {
        parse_number_test!["'0", 0];
        parse_number_test!["'1", 1];
        parse_number_test!["'2", 2];
        parse_number_test!["'3", 3];
        parse_number_test!["'4", 4];
        parse_number_test!["'5", 5];
        parse_number_test!["'6", 6];
        parse_number_test!["'7", 7];

        parse_number_test!["'10", 8];
        parse_number_test!["'11", 9];
        parse_number_test!["'12", 10];
        parse_number_test!["'13", 11];
        parse_number_test!["'14", 12];
        parse_number_test!["'15", 13];
        parse_number_test!["'16", 14];
        parse_number_test!["'17", 15];

        parse_number_test!["'201", 129];
    }

    #[test]
    fn decimal_basic() {
        parse_number_test!["0", 0];
        parse_number_test!["1", 1];
        parse_number_test!["2", 2];
        parse_number_test!["3", 3];
        parse_number_test!["4", 4];
        parse_number_test!["5", 5];
        parse_number_test!["6", 6];
        parse_number_test!["7", 7];
        parse_number_test!["8", 8];
        parse_number_test!["9", 9];

        parse_number_test!["10", 10];
        parse_number_test!["11", 11];
        parse_number_test!["12", 12];
        parse_number_test!["13", 13];
        parse_number_test!["14", 14];
        parse_number_test!["15", 15];
        parse_number_test!["16", 16];
        parse_number_test!["17", 17];
        parse_number_test!["18", 18];
        parse_number_test!["19", 19];

        parse_number_test!["00019", 19];
        parse_number_test!["201", 201];
    }

    #[test]
    fn hexadecimal_basic() {
        parse_number_test!["\"0", 0];
        parse_number_test!["\"1", 1];
        parse_number_test!["\"2", 2];
        parse_number_test!["\"3", 3];
        parse_number_test!["\"4", 4];
        parse_number_test!["\"5", 5];
        parse_number_test!["\"6", 6];
        parse_number_test!["\"7", 7];
        parse_number_test!["\"8", 8];
        parse_number_test!["\"9", 9];
        parse_number_test!["\"A", 10];
        parse_number_test!["\"B", 11];
        parse_number_test!["\"C", 12];
        parse_number_test!["\"D", 13];
        parse_number_test!["\"E", 14];
        parse_number_test!["\"F", 15];

        parse_number_test!["\"10", 16];
        parse_number_test!["\"11", 17];
        parse_number_test!["\"12", 18];
        parse_number_test!["\"13", 19];
        parse_number_test!["\"14", 20];
        parse_number_test!["\"15", 21];
        parse_number_test!["\"16", 22];
        parse_number_test!["\"17", 23];
        parse_number_test!["\"18", 24];
        parse_number_test!["\"19", 25];
        parse_number_test!["\"1A", 26];
        parse_number_test!["\"1B", 27];
        parse_number_test!["\"1C", 28];
        parse_number_test!["\"1D", 29];
        parse_number_test!["\"1E", 30];
        parse_number_test!["\"1F", 31];

        parse_number_test!["\"201", 513];
    }

    #[test]
    fn number_from_character() {
        parse_number_test!["`A", 65];
    }

    #[test]
    fn signs() {
        parse_number_test![r"+4", 4];
        parse_number_test![r"-4", -4];
        parse_number_test![r"+-4", -4];
        parse_number_test![r"--4", 4];
        parse_number_test![r"  -  - 4", 4];
    }

    #[test]
    fn number_with_letter_catcode() {
        let mut map = CatCodeMap::new_with_tex_defaults();
        map.insert('1', catcode::CatCode::Letter);
        let mut env = runtime::Env::<()>::new(map, ());
        env.push_source(r"1".to_string()).unwrap();
        let input = crate::runtime::ExecutionInput::new(&mut env);
        let result: anyhow::Result<i32> = parse_number(input);
        if let Ok(_) = result {
            panic!["Parsed a relation from invalid input"];
        }
    }
}
