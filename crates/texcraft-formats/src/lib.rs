use std::panic;

pub fn read(b: &[u8]) {
    if b.len() <= 24 {
        panic!["file too small"]
    }
    let lf = read_u16(&b[0..]);
    let lh = read_u16(&b[2..]);
    let bc = read_u16(&b[4..]);
    let ec = read_u16(&b[6..]);
    let nw = read_u16(&b[8..]);
    let nh = read_u16(&b[10..]);
    let nd = read_u16(&b[12..]);
    let ni = read_u16(&b[14..]);
    let nl = read_u16(&b[16..]);
    let nk = read_u16(&b[18..]);
    let ne = read_u16(&b[20..]);
    let np = read_u16(&b[22..]);
    if b.len() < lf {
        panic!["file size doesn't match"]
    }
    if lf != 24 + lh + (ec - bc + 4) + nw + nh + nd + ni + nl + nk + ne + np {
        panic!["inconsistent tfm file"]
    }
    let mut offset = 24;
    let header = &b[offset..offset + lh];

    let a: [u8; 4] = [0, 0, 0, 1];
    let f = FixWord::new(&a);
    println!("fix word of 1: {}", f);

    let design_size = FixWord::new(&header[4..]);
    println!("Design size: {}", design_size);

    offset += lh;
    let char_info = &b[offset..offset + ec - bc + 4];
    offset += ec - bc + 4;
    let width = &b[offset..offset + nw];

    println!("num char info: {:?}", char_info.len() / 4);
    parse_char_info(char_info, width);
}

fn read_u16(i: &[u8]) -> usize {
    (((i[0] as usize) << 8) + (i[1] as usize)) * 4
}

pub struct File {
    checksum: u32,
    char_infos: Vec<CharInfo>,
}

pub struct CharInfo {}

#[derive(PartialEq, Eq, Debug)]
struct FixWord(i32);

impl FixWord {
    const UNITY: i32 = 1 << 20;

    fn new(b: &[u8]) -> FixWord {
        FixWord(
            ((b[0] as i32) << 24)
                + ((b[1] as i32) << 16)
                + ((b[2] as i32) << 8)
                + ((b[3] as i32) << 0),
        )
    }

    fn from_str(s: &str) -> FixWord {
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

        let mut chars = s.chars();

        let mut negative = false;
        let mut integer = None;
        while let Some(c) = chars.next() {
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
        while let Some(c) = chars.next() {
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
        while let Some(c) = chars.next() {
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
        let mut result = integer * FixWord::UNITY + fraction;
        if negative {
            result *= -1;
        }
        FixWord(result)
    }
}

impl std::fmt::Display for FixWord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let abs: u32 = if self.0 < 0 {
            if self.0 == i32::MIN {
                return write!(f, "-2047.9999999");
            } else {
                write!(f, "-")?;
                self.0.abs() as u32
            }
        } else {
            self.0 as u32
        };
        let mut integer = abs / (1 << 20);
        /*
        println!(
            "integer={}, k={}, modulus={}",
            integer,
            integer * (1 << 20),
            modulus
        );
        */

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
            write!(f, "{}", integer_digits[i])?;
            i += 1;
        }

        write!(f, ".")?;
        let mut delta = 10;
        let mut fraction = abs % (1 << 20);
        println!("fraction={}, modulus={}", fraction, abs);
        fraction = fraction * 10 + 5;
        loop {
            if delta > (1 << 20) {
                fraction = fraction + (1 << 19) - (delta / 2);
            }
            write!(f, "{}", fraction / (1 << 20))?;
            fraction = (fraction % (1 << 20)) * 10;
            delta *= 10;
            if fraction <= delta {
                break;
            }
        }
        Ok(())
    }
}

fn parse_char_info(data: &[u8], widths: &[u8]) -> Vec<CharInfo> {
    let mut char_infos = Vec::new();
    let mut i = 0;
    while i + 4 < data.len() {
        let width_index = data[i] as usize;
        let width = FixWord::new(&widths[width_index * 4..(width_index * 4 + 4)]);
        println![
            "width: {:?} {}",
            &widths[width_index * 4..(width_index * 4 + 4)],
            width
        ];
        i += 4;
    }
    char_infos
}

#[cfg(test)]
mod tests {
    use crate::FixWord;

    macro_rules! round_trip_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let value: i32 = $value;

                let start = FixWord(value);
                let s = format!("{}", start);
                let finish = FixWord::from_str(&s);
                assert_eq!(start, finish, "{} ({}) != {} ({}) = FixWord::from_str({})", start.0, start, finish, finish.0, s);

                let start = FixWord(value.wrapping_mul(-1));
                let s = format!("{}", start);
                let finish = FixWord::from_str(&s);
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

    #[test]
    fn all() {
        for value in 0..(2 << 20) { //i32::MAX {
            let start = FixWord(value);
            let s = format!("{}", start);
            let finish = FixWord::from_str(&s);
            assert_eq!(
                start, finish,
                "{} ({}) != {} ({}) = FixWord::from_str({})",
                start.0, start, finish, finish.0, s
            );

            let start = FixWord(value.wrapping_mul(-1));
            let s = format!("{}", start);
            let finish = FixWord::from_str(&s);
            assert_eq!(start, finish);
        }
    }

    static CMR10_TFM: &'static [u8] = include_bytes!("cmr10.tfm");
    #[test]
    fn t() {}
}
