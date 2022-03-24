use std::panic;

pub fn read(b: &[u8]) {
    if b.len() <= 24 {
        panic!["file too small"]
    }
    let mut word_stream = WordStream { b };
    let (lf, lh) = word_stream.read_u16s();
    let (bc, ec) = word_stream.read_u16s();
    let (nw, nh) = word_stream.read_u16s();
    let (nd, ni) = word_stream.read_u16s();
    let (nl, nk) = word_stream.read_u16s();
    let (ne, np) = word_stream.read_u16s();
    if word_stream.len() * 4 < (lf as usize) {
        panic!["file size doesn't match"]
    }
    if lf != 6 + lh + (ec - bc + 1) + nw + nh + nd + ni + nl + nk + ne + np {
        panic!["inconsistent tfm file"]
    }
    let header = {
        let mut raw = word_stream.head(lh);
        Header::deserialize_tfm(&mut raw)
    };
    println!("{:?}", header);

    let raw_char_info = {
        let mut raw = word_stream.head(ec - bc + 4);
        Vec::<RawCharInfo>::deserialize_tfm(&mut raw)
    };
    let widths = {
        let mut raw = word_stream.head(nw);
        Vec::<FixWord>::deserialize_tfm(&mut raw)
    };
    let heights = {
        let mut raw = word_stream.head(nh);
        Vec::<FixWord>::deserialize_tfm(&mut raw)
    };
    let depths = {
        let mut raw = word_stream.head(nd);
        Vec::<FixWord>::deserialize_tfm(&mut raw)
    };
    let italic_corrections = {
        let mut raw = word_stream.head(nd);
        Vec::<FixWord>::deserialize_tfm(&mut raw)
    };

    println!("{:?}", raw_char_info);
    // println!("num char info: {:?}", char_info.len() / 4);
    // parse_char_info(char_info, width);
}

#[derive(Debug)]
pub struct Header {
    pub checksum: u32,
    pub design_size: FixWord,
    pub character_coding_scheme: Option<String>,
    pub font_family: Option<String>,
    pub seven_bit_safe: Option<bool>,
    pub face: Option<Face>,
    pub trailing_words: Vec<u32>,
}

impl DeserializeTfm for Header {
    fn deserialize_tfm<'a, 'b>(tfm_stream: &'b mut WordStream<'a>) -> Self {
        // todo: check for size and error
        let checksum = tfm_stream.read_u32();
        let design_size = FixWord::deserialize_tfm(tfm_stream);
        let character_coding_scheme = {
            let mut raw = tfm_stream.head(10_usize);
            match raw.len() == 10 {
                true => Some(String::deserialize_tfm(&mut raw)),
                false => None,
            }
        };
        let font_family = {
            let mut raw = tfm_stream.head(5_usize);
            match raw.len() == 5 {
                true => Some(String::deserialize_tfm(&mut raw)),
                false => None,
            }
        };
        let (seven_bit_safe, face) = if tfm_stream.empty() {
            (None, None)
        } else {
            let (seven_bit_safe_raw, _, _, face_raw) = tfm_stream.read_u8s();
            (Some(seven_bit_safe_raw >= 128), Face::deserialize(face_raw))
        };
        let trailing_words = Vec::<u32>::deserialize_tfm(tfm_stream);
        Header {
            checksum,
            design_size,
            character_coding_scheme,
            font_family,
            seven_bit_safe,
            face,
            trailing_words,
        }
    }
}

#[derive(Debug)]
struct RawCharInfo {
    width_index: usize,
    height_index: usize,
    depth_index: usize,
    italic_index: usize,
    tag: Tag,
}

#[derive(Debug)]
enum Tag {
    None,
    Ligature(usize),
    List(usize),
    Extension(usize),
}

impl DeserializeTfm for RawCharInfo {
    fn deserialize_tfm<'a, 'b>(word_stream: &'b mut WordStream<'a>) -> Self {
        let (width_index, a, b, remainder) = word_stream.read_u8s();
        RawCharInfo {
            width_index: width_index as usize,
            height_index: (a as usize) / (1 << 4),
            depth_index: (a as usize) % (1 << 4),
            italic_index: (b as usize) / (1 << 2),
            tag: match b % (1 << 2) {
                0 => Tag::None,
                1 => Tag::Ligature(remainder as usize),
                2 => Tag::List(remainder as usize),
                _ => Tag::Extension(remainder as usize),
            },
        }
    }
}

#[derive(Debug)]
pub enum Weight {
    Medium,
    Bold,
    Light,
}

#[derive(Debug)]
pub enum Slope {
    Roman,
    Italic,
}

#[derive(Debug)]
pub enum Expansion {
    Regular,
    Condensed,
    Expanded,
}

#[derive(Debug)]
pub struct Face {
    pub weight: Weight,
    pub slope: Slope,
    pub expansion: Expansion,
}

impl Face {
    fn deserialize(mut raw: u8) -> Option<Face> {
        if raw < 18 {
            None
        } else {
            let slope = if raw % 2 == 0 {
                Slope::Roman
            } else {
                Slope::Italic
            };
            raw /= 2;
            let weight = match raw % 3 {
                0 => Weight::Medium,
                1 => Weight::Bold,
                _ => Weight::Light,
            };
            raw /= 3;
            let expansion = match raw % 3 {
                0 => Expansion::Regular,
                1 => Expansion::Condensed,
                _ => Expansion::Expanded,
            };
            Some(Face {
                weight,
                slope,
                expansion,
            })
        }
    }
}

pub struct File {
    header: Header,
    char_infos: Vec<CharInfo>,
}

impl DeserializeTfm for String {
    fn deserialize_tfm<'a, 'b>(word_stream: &'b mut WordStream<'a>) -> Self {
        let (mut len, b, c, d) = word_stream.read_u8s();
        // todo validate len
        let mut stack = vec![d, c, b];
        let mut s = String::new();
        while len > 0 {
            let raw_char = match stack.pop() {
                None => {
                    let (a, b, c, d) = word_stream.read_u8s();
                    stack.push(d);
                    stack.push(c);
                    stack.push(b);
                    a
                }
                Some(b) => b,
            };
            // todo error if not ascii
            s.push(raw_char.try_into().unwrap());
            len -= 1;
        }
        s
    }
}

impl<T: DeserializeTfm> DeserializeTfm for Vec<T> {
    fn deserialize_tfm<'a, 'b>(word_stream: &'b mut WordStream<'a>) -> Self {
        let mut fix_words = Vec::with_capacity(word_stream.len());
        while !word_stream.empty() {
            fix_words.push(T::deserialize_tfm(word_stream));
        }
        fix_words
    }
}

impl DeserializeTfm for u32 {
    fn deserialize_tfm<'a, 'b>(word_stream: &'b mut WordStream<'a>) -> Self {
        word_stream.read_u32()
    }
}

struct WordStream<'a> {
    b: &'a [u8],
}

impl<'a> WordStream<'a> {
    fn read_u32(&mut self) -> u32 {
        if self.b.len() < 4 {
            0
        } else {
            let s = ((self.b[0] as u32) << 24)
                + ((self.b[1] as u32) << 16)
                + ((self.b[2] as u32) << 8)
                + ((self.b[3] as u32) << 0);
            self.b = &self.b[4..];
            s
        }
    }

    fn read_u16s(&mut self) -> (u16, u16) {
        if self.b.len() < 4 {
            (0, 0)
        } else {
            let s = (
                ((self.b[0] as u16) << 8) + ((self.b[1] as u16) << 0),
                ((self.b[2] as u16) << 8) + ((self.b[3] as u16) << 0),
            );
            self.b = &self.b[4..];
            s
        }
    }

    fn read_u8s(&mut self) -> (u8, u8, u8, u8) {
        if self.b.len() < 4 {
            (0, 0, 0, 0)
        } else {
            let s = (self.b[0], self.b[1], self.b[2], self.b[3]);
            self.b = &self.b[4..];
            s
        }
    }

    fn head<T: Into<usize>>(&mut self, num_words: T) -> WordStream<'a> {
        let num_words = T::into(num_words);
        if self.len() < num_words {
            let slice = WordStream { b: self.b };
            self.b = &[];
            slice
        } else {
            let slice = WordStream {
                b: &self.b[..num_words * 4],
            };
            self.b = &self.b[num_words * 4..];
            slice
        }
    }

    fn empty(&self) -> bool {
        self.len() == 0
    }
    fn len(&self) -> usize {
        self.b.len() / 4
    }
}

trait DeserializeTfm {
    fn deserialize_tfm<'a, 'b>(word_stream: &'b mut WordStream<'a>) -> Self;
}

trait SerializePl {
    fn serialize_pl(&self);
}

trait DeserializePl {
    fn deserialize_pl();
}

pub struct CharInfo {}

#[derive(PartialEq, Eq, Debug)]
pub struct FixWord(i32);

impl DeserializeTfm for FixWord {
    fn deserialize_tfm<'a, 'b>(tfm_stream: &'b mut WordStream<'a>) -> Self {
        FixWord(tfm_stream.read_u32() as i32)
    }
}

impl FixWord {
    const UNITY: i32 = 1 << 20;

    // serialize_from_tfm()
    // serialize_from_pl()
    // deserialize_to_pl()
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

    static CMR10_TFM: &'static [u8] = include_bytes!("cmr10.tfm");
    #[test]
    fn t() {
        super::read(CMR10_TFM);
        assert_eq!(1, 2);
    }
}
