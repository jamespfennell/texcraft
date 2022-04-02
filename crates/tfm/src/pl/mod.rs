//! Parser and writer for the property list (.pl) text format
use super::*;
use std::{
    fmt::Debug,
    iter::{Iterator, Peekable},
};
// use std::fmt::Writer;

mod ast;

const CHECKSUM: &str = "CHECKSUM";
const CODING_SCHEME: &str = "CODINGSCHEME";
const FAMILY: &str = "FAMILY";
const HEADER: &str = "HEADER";

pub fn format(input: &str, style: &PlStyle) -> String {
    let tree = ast::parse(input).unwrap();
    ast::write(&tree, style)
}

pub fn parse(b: &str) -> File {
    todo!()
}

pub fn serialize(file: &File) -> String {
    let mut root = Vec::<ast::Node>::new();
    // root.push(PlElem::new(CHECKSUM, ))
    if let Some(font_family) = &file.header.font_family {
        root.push(ast::Node::new(FAMILY, &[font_family], vec![]));
    }
    for word in &file.header.additional_data {
        let value = format!("{:o}", word);
        // root.push(ast::Node::new(HEADER, &["O", &value], vec![]));
    }
    if let Some(coding_scheme) = &file.header.character_coding_scheme {
        root.push(ast::Node::new(CODING_SCHEME, &[coding_scheme], vec![]));
    }
    let style = PlStyle::default();
    ast::write(&root, &style)
}

pub trait SerializePl {
    fn serialize_pl(&self, output: &mut Output);
}

pub fn serialize_pl<T: SerializePl>(t: &T) -> String {
    let mut output = Output { s: String::new() };
    t.serialize_pl(&mut output);
    output.s
}

pub struct Output {
    s: String,
}

impl Output {
    fn write(&mut self, c: char) {
        self.s.push(c)
    }

    fn write_str(&mut self, s: &str) {
        self.s.push_str(s)
    }
}

trait DeserializePl {
    fn deserialize_pl(input: &mut Input) -> Self;
}

struct Input<'a> {
    c: std::str::Chars<'a>,
}

impl<'a> Input<'a> {
    fn next(&mut self) -> Option<char> {
        self.c.next()
    }
}

impl SerializePl for FixWord {
    fn serialize_pl<'a, 'b>(&self, output: &mut Output) {
        let abs: u32 = if self.0 < 0 {
            if self.0 == i32::MIN {
                output.write_str("-2047.9999999");
                return;
            } else {
                output.write('-');
                self.0.abs() as u32
            }
        } else {
            self.0 as u32
        };
        let mut integer = abs / (1 << 20);

        // The integer part is at most 2^11 < 10^4, so there are at most 4 decimal digits.
        let mut integer_digits = [0; 4];
        let mut i = 4;
        loop {
            integer_digits[i - 1] = integer % 10;
            integer /= 10;
            i -= 1;
            if integer == 0 {
                break;
            }
        }
        while i < 4 {
            output.write(std::char::from_digit(integer_digits[i], 10).unwrap());
            i += 1;
        }

        output.write('.');
        let mut delta = 10;
        let mut fraction = abs % (1 << 20);
        fraction = fraction * 10 + 5;
        loop {
            if delta > (1 << 20) {
                fraction = fraction + (1 << 19) - (delta / 2);
            }
            output.write(std::char::from_digit(fraction / (1 << 20), 10).unwrap());
            fraction = (fraction % (1 << 20)) * 10;
            delta *= 10;
            if fraction <= delta {
                break;
            }
        }
    }
}

impl DeserializePl for FixWord {
    fn deserialize_pl(input: &mut Input) -> Self {
        enum Char {
            Digit(i32),
            Other(char),
        }
        impl Char {
            fn new(c: char) -> Char {
                match c {
                    '0' => Char::Digit(0),
                    '1' => Char::Digit(1),
                    '2' => Char::Digit(2),
                    '3' => Char::Digit(3),
                    '4' => Char::Digit(4),
                    '5' => Char::Digit(5),
                    '6' => Char::Digit(6),
                    '7' => Char::Digit(7),
                    '8' => Char::Digit(8),
                    '9' => Char::Digit(9),
                    other => Char::Other(other),
                }
            }
        }

        let mut negative = false;
        let mut integer = None;
        while let Some(c) = input.next() {
            match Char::new(c) {
                Char::Other('+') | Char::Other(' ') => (),
                Char::Other('-') => {
                    negative = !negative;
                }
                Char::Digit(d) => {
                    integer = Some(d);
                    break;
                }
                Char::Other(_) => panic![""],
            }
        }
        let negative = negative;

        let mut integer = match integer {
            None => panic![""],
            Some(integer) => integer,
        };
        while let Some(c) = input.next() {
            match Char::new(c) {
                Char::Digit(d) => {
                    integer = integer * 10 + d;
                    if integer >= 2048 {
                        panic!("integer too big")
                    }
                }
                Char::Other('.') => break,
                Char::Other(other) => panic!["unexpected char {}", other],
            }
        }
        let integer = integer;

        let mut num_fractional_digits = 0;
        let mut fraction_digits = [0; 7];
        while let Some(c) = input.next() {
            match Char::new(c) {
                Char::Digit(d) => {
                    if num_fractional_digits < 7 {
                        fraction_digits[num_fractional_digits] = d * (1 << 21);
                        num_fractional_digits += 1;
                    }
                }
                Char::Other(_) => break,
            }
        }
        let mut fraction = 0;
        for i in (0..num_fractional_digits).rev() {
            fraction = fraction_digits[i] + fraction / 10;
        }
        let fraction = (fraction + 10) / 20;

        if integer == 2047 && fraction >= (1 << 20) {
            if negative {
                return FixWord(i32::MIN);
            }
            panic![""]
        }
        let mut result = integer * FixWord::UNITY.0 + fraction;
        if negative {
            result *= -1;
        }
        FixWord(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! round_trip_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let value: i32 = $value;

                let start = FixWord(value);
                let mut output = Output{s: String::new() };
                start.serialize_pl(&mut output);

                let mut input = Input {c: output.s.chars() };
                let finish = FixWord::deserialize_pl(&mut input);
                assert_eq!(start, finish);

                let start = FixWord(value.wrapping_mul(-1));
                let mut output = Output{s: String::new() };
                start.serialize_pl(&mut output);

                let mut input = Input {c: output.s.chars() };
                let finish = FixWord::deserialize_pl(&mut input);
                assert_eq!(start, finish);
            }
        )*
        }
    }

    round_trip_tests!(
        zero: 0,
        one: 1,
        two: 2,
        three: 3,
        four: 4,
        five: 5,
        ten: 10,
        seventy: 70,
        one40: 140,
        pow10: 1 << 10,
        pow15: 1 << 15,
        pow18: 1 << 18,
        pow19: 1 << 19,
        pow20: 1 << 20,
        pow20_times_10: 10 * 1 << 20,
        pow21: 1 << 21,
        pow21_plus_pow15: 1 << 21 + 1 << 15,
        big: 15 * (1 << 20) + 1 << 15,
        min: i32::MIN,
        max: i32::MAX,
    );
}
