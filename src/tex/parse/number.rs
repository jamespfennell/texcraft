use crate::tex::prelude::*;
use crate::tex::variable;
use num_traits::PrimInt;

/// Parses a number from the provided stream.
///
/// The number may be octal, decimal, hexadecimal, cast from a character token, or read
/// from an internal registers. The full definition of a number in the TeX grammer
/// is given on page X of the TeXBook.
pub fn parse_number<S, T: PrimInt>(stream: &mut ExpansionInput<S>) -> anyhow::Result<T> {
    let sign = parse_optional_signs(stream)?;
    let modulus: T = match stream.next()? {
        None => return Err(parse_number_error(None)),
        Some(token) => match token.value() {
            Character('0', CatCode::Other) => parse_decimal(stream, 0)?,
            Character('1', CatCode::Other) => parse_decimal(stream, 1)?,
            Character('2', CatCode::Other) => parse_decimal(stream, 2)?,
            Character('3', CatCode::Other) => parse_decimal(stream, 3)?,
            Character('4', CatCode::Other) => parse_decimal(stream, 4)?,
            Character('5', CatCode::Other) => parse_decimal(stream, 5)?,
            Character('6', CatCode::Other) => parse_decimal(stream, 6)?,
            Character('7', CatCode::Other) => parse_decimal(stream, 7)?,
            Character('8', CatCode::Other) => parse_decimal(stream, 8)?,
            Character('9', CatCode::Other) => parse_decimal(stream, 9)?,
            Character('\'', CatCode::Other) => parse_octal(stream)?,
            Character('"', CatCode::Other) => parse_hexadecimal(stream)?,
            Character('`', CatCode::Other) => parse_character(stream)?,
            ControlSequence(_, ref name) => {
                let cmd = stream.base().get_command(name);
                if let Some(command::Command::Variable(cmd_ref)) = cmd {
                    // TODO: don't clone here, use the same trick as the driver?
                    let cmd = *cmd_ref;
                    let variable = cmd.variable(&token, stream)?;
                    read_number_from_address(variable, stream)?
                } else {
                    println!("Command: {:?}", cmd);
                    return Err(parse_number_error(Some(token)));
                }
            }
            _ => return Err(parse_number_error(Some(token))),
        },
    };
    get_optional_element![stream, Character(_, CatCode::Space) => (),];
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
fn parse_optional_signs<T: stream::Stream>(stream: &mut T) -> anyhow::Result<Option<Token>> {
    let mut result = None;
    while let Some((sign, token)) = get_optional_element_with_token![
        stream,
        Character('+', CatCode::Other) => true,
        Character('-', CatCode::Other) => false,
        Character(_, CatCode::Space) => true,
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
            Character(_, _) => {
                error::TokenError::new(token, "unexpected character token when parsing a number")
                    .cast()
            }
        },
    }
}

fn read_number_from_address<S, T: PrimInt>(
    variable: variable::Variable<S>,
    stream: &mut ExpansionInput<S>,
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

fn parse_character<S: stream::Stream, T: PrimInt>(stream: &mut S) -> anyhow::Result<T> {
    match stream.next()? {
        None => Err(error::EndOfInputError::new(
            "unexpected end of input while parsing a character token",
        )
        .cast()),
        Some(token) => match token.value() {
            // TODO: error if the character is too big!
            Character(c, _) => Ok(num_traits::cast::cast(c as i32).unwrap()),
            ControlSequence(..) => Err(error::TokenError::new(
                token,
                "unexpected control sequence while parsing a character number",
            )
            .cast()),
        },
    }
}

fn parse_octal<S: stream::Stream, T: PrimInt>(stream: &mut S) -> anyhow::Result<T> {
    let mut n = num_traits::cast::cast(get_element![
        stream,
        parse_number_error,
            Character('0', CatCode::Other) => 0,
            Character('1', CatCode::Other) => 1,
            Character('2', CatCode::Other) => 2,
            Character('3', CatCode::Other) => 3,
            Character('4', CatCode::Other) => 4,
            Character('5', CatCode::Other) => 5,
            Character('6', CatCode::Other) => 6,
            Character('7', CatCode::Other) => 7,
    ]?)
    .unwrap();
    while let Some(lsd) = get_optional_element![
        stream,
        Character('0', CatCode::Other) => 0,
        Character('1', CatCode::Other) => 1,
        Character('2', CatCode::Other) => 2,
        Character('3', CatCode::Other) => 3,
        Character('4', CatCode::Other) => 4,
        Character('5', CatCode::Other) => 5,
        Character('6', CatCode::Other) => 6,
        Character('7', CatCode::Other) => 7,
    ] {
        n = add_lsd(n, 8, lsd);
    }
    Ok(n)
}

fn parse_decimal<S: stream::Stream, T: PrimInt>(stream: &mut S, n_start: i8) -> anyhow::Result<T> {
    let mut n: T = num_traits::cast::cast(n_start).unwrap();
    while let Some(lsd) = get_optional_element![
        stream,
        Character('0', CatCode::Other) => 0,
        Character('1', CatCode::Other) => 1,
        Character('2', CatCode::Other) => 2,
        Character('3', CatCode::Other) => 3,
        Character('4', CatCode::Other) => 4,
        Character('5', CatCode::Other) => 5,
        Character('6', CatCode::Other) => 6,
        Character('7', CatCode::Other) => 7,
        Character('8', CatCode::Other) => 8,
        Character('9', CatCode::Other) => 9,
    ] {
        n = add_lsd(n, 10, lsd);
    }
    Ok(n)
}

fn parse_hexadecimal<S: stream::Stream, T: PrimInt>(stream: &mut S) -> anyhow::Result<T> {
    let mut n: T = num_traits::cast::cast(get_element![
        stream,
        parse_number_error,
            Character('0', CatCode::Other) => 0,
            Character('1', CatCode::Other) => 1,
            Character('2', CatCode::Other) => 2,
            Character('3', CatCode::Other) => 3,
            Character('4', CatCode::Other) => 4,
            Character('5', CatCode::Other) => 5,
            Character('6', CatCode::Other) => 6,
            Character('7', CatCode::Other) => 7,
            Character('8', CatCode::Other) => 8,
            Character('9', CatCode::Other) => 9,

            Character('A', CatCode::Other) => 10,
            Character('B', CatCode::Other) => 11,
            Character('C', CatCode::Other) => 12,
            Character('D', CatCode::Other) => 13,
            Character('E', CatCode::Other) => 14,
            Character('F', CatCode::Other) => 15,

            Character('A', CatCode::Letter) => 10,
            Character('B', CatCode::Letter) => 11,
            Character('C', CatCode::Letter) => 12,
            Character('D', CatCode::Letter) => 13,
            Character('E', CatCode::Letter) => 14,
            Character('F', CatCode::Letter) => 15,
    ]?)
    .unwrap();
    while let Some(lsd) = get_optional_element![
        stream,
        Character('0', CatCode::Other) => 0,
        Character('1', CatCode::Other) => 1,
        Character('2', CatCode::Other) => 2,
        Character('3', CatCode::Other) => 3,
        Character('4', CatCode::Other) => 4,
        Character('5', CatCode::Other) => 5,
        Character('6', CatCode::Other) => 6,
        Character('7', CatCode::Other) => 7,
        Character('8', CatCode::Other) => 8,
        Character('9', CatCode::Other) => 9,

        Character('A', CatCode::Other) => 10,
        Character('B', CatCode::Other) => 11,
        Character('C', CatCode::Other) => 12,
        Character('D', CatCode::Other) => 13,
        Character('E', CatCode::Other) => 14,
        Character('F', CatCode::Other) => 15,

        Character('A', CatCode::Letter) => 10,
        Character('B', CatCode::Letter) => 11,
        Character('C', CatCode::Letter) => 12,
        Character('D', CatCode::Letter) => 13,
        Character('E', CatCode::Letter) => 14,
        Character('F', CatCode::Letter) => 15,
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
    use crate::tex::input;
    use crate::tex::state;

    use crate::tex::token::catcode;

    macro_rules! parse_number_test {
        ($input: expr, $number: expr) => {
            // TODO: put this in the test util
            // TODO: u32 hack undo please
            // TODO: replicate this for the relation tests and destroy the VecStream
            let mut state = state::Base::<u32>::new(catcode::tex_defaults(), 0);
            let mut input = input::Unit::new();
            input.push_new_str($input);
            let mut e_input = ExpansionInput::new(&mut state, &mut input);
            let result: i32 = parse_number(&mut e_input).unwrap();
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
}
