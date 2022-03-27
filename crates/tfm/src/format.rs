//! Parser and serializer for the TeX font metric (.tfm) binary format.
use super::*;

pub fn parse(b: &[u8]) -> File {
    RawFile::into(RawFile::deserialize_tfm(&mut Input { b }))
}

pub fn serialize(file: &File) -> Vec<u8> {
    let raw_file: RawFile = RawFile::from(file);
    serialize_tfm(&raw_file)
}

#[derive(Debug, PartialEq, Eq)]
struct RawFile {
    header: Header,
    first_char: u16,
    raw_char_info: Vec<RawCharInfo>,
    widths: Vec<FixWord>,
    heights: Vec<FixWord>,
    depths: Vec<FixWord>,
    italic_corrections: Vec<FixWord>,
    lig_kerns: Vec<RawLigKern>,
    kerns: Vec<FixWord>,
    extensible_chars: Vec<ExtensibleChar>,
    params: Params,
}

impl Into<File> for RawFile {
    fn into(self) -> File {
        File {
            header: self.header,
            char_infos: HashMap::new(),
            params: self.params,
        }
    }
}

impl From<&File> for RawFile {
    fn from(file: &File) -> Self {
        todo!()
    }
}

trait SerializeTfm {
    fn serialize_tfm(&self, output: &mut Output);
}

fn serialize_tfm<T: SerializeTfm>(t: &T) -> Vec<u8> {
    let mut output = Output { b: Vec::new() };
    t.serialize_tfm(&mut output);
    output.b
}

struct Output {
    b: Vec<u8>,
}

impl Output {
    fn write_u32(&mut self, u: u32) {
        self.write_u16s(((u >> 16) as u16, u as u16))
    }

    fn write_u16s(&mut self, u: (u16, u16)) {
        self.write_u8s(((u.0 >> 8) as u8, u.0 as u8, (u.1 >> 8) as u8, u.1 as u8));
    }

    fn write_u8s(&mut self, u: (u8, u8, u8, u8)) {
        self.b.push(u.0);
        self.b.push(u.1);
        self.b.push(u.2);
        self.b.push(u.3);
    }

    fn len(&self) -> usize {
        self.b.len() / 4
    }
}

trait DeserializeTfm {
    fn deserialize_tfm(input: &mut Input) -> Self;
}

fn deserialize_tfm<T: DeserializeTfm>(b: &[u8]) -> T {
    let mut input = Input { b };
    T::deserialize_tfm(&mut input)
}

struct Input<'a> {
    b: &'a [u8],
}

impl<'a> Input<'a> {
    fn read_u32(&mut self) -> u32 {
        if self.b.len() < 4 {
            0
        } else {
            let s = ((self.b[0] as u32) << 24)
                + ((self.b[1] as u32) << 16)
                + ((self.b[2] as u32) << 8)
                + (self.b[3] as u32);
            self.b = &self.b[4..];
            s
        }
    }

    fn read_u16s(&mut self) -> (u16, u16) {
        if self.b.len() < 4 {
            (0, 0)
        } else {
            let s = (
                ((self.b[0] as u16) << 8) + (self.b[1] as u16),
                ((self.b[2] as u16) << 8) + (self.b[3] as u16),
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

    fn slice<T: Into<usize>>(&mut self, num_words: T) -> Input<'a> {
        let num_words = T::into(num_words);
        if self.len() < num_words {
            let slice = Input { b: self.b };
            self.b = &[];
            slice
        } else {
            let slice = Input {
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

impl SerializeTfm for Header {
    fn serialize_tfm(&self, output: &mut Output) {
        output.write_u32(self.checksum);
        self.design_size.serialize_tfm(output);
        match &self.character_coding_scheme {
            None => return,
            Some(s) => {
                let n = output.len();
                s.serialize_tfm(output);
                while output.len() - n < 10 {
                    output.write_u32(0);
                }
            }
        }
        match &self.font_family {
            None => return,
            Some(s) => {
                let n = output.len();
                s.serialize_tfm(output);
                while output.len() - n < 5 {
                    output.write_u32(0);
                }
            }
        }
        let seven_bit_safe_raw = match self.seven_bit_safe {
            None => return,
            Some(true) => 128_u8,
            Some(false) => 0_u8,
        };
        let face_raw = match &self.face {
            None => 234_u8, // TODO: wtf?
            Some(face) => face.to_u8(),
        };
        output.write_u8s((seven_bit_safe_raw, 0, 0, face_raw));
        self.additional_data.serialize_tfm(output);
    }
}

impl DeserializeTfm for Header {
    fn deserialize_tfm(input: &mut Input) -> Self {
        // todo: check for size and error
        let checksum = input.read_u32();
        let design_size = FixWord::deserialize_tfm(input);
        let character_coding_scheme = {
            let mut raw = input.slice(10_usize);
            match raw.len() == 10 {
                true => Some(String::deserialize_tfm(&mut raw)),
                false => None,
            }
        };
        let font_family = {
            let mut raw = input.slice(5_usize);
            match raw.len() == 5 {
                true => Some(String::deserialize_tfm(&mut raw)),
                false => None,
            }
        };
        let (seven_bit_safe, face) = if input.empty() {
            (None, None)
        } else {
            let (seven_bit_safe_raw, _, _, face_raw) = input.read_u8s();
            (Some(seven_bit_safe_raw >= 128), Face::from_u8(face_raw))
        };
        let trailing_words = Vec::<u32>::deserialize_tfm(input);
        Header {
            checksum,
            design_size,
            character_coding_scheme,
            font_family,
            seven_bit_safe,
            face,
            additional_data: trailing_words,
        }
    }
}

impl SerializeTfm for FixWord {
    fn serialize_tfm(&self, output: &mut Output) {
        output.write_u32(self.0 as u32);
    }
}

impl DeserializeTfm for FixWord {
    fn deserialize_tfm(input: &mut Input) -> Self {
        FixWord(input.read_u32() as i32)
    }
}

impl SerializeTfm for ExtensibleChar {
    fn serialize_tfm(&self, output: &mut Output) {
        output.write_u8s((self.top, self.bottom, self.middle, self.rep))
    }
}

impl DeserializeTfm for ExtensibleChar {
    fn deserialize_tfm(input: &mut Input) -> Self {
        let (top, middle, bottom, rep) = input.read_u8s();
        ExtensibleChar {
            top,
            middle,
            bottom,
            rep,
        }
    }
}

impl SerializeTfm for Params {
    fn serialize_tfm(&self, output: &mut Output) {
        for fix_word in [
            &self.slant,
            &self.space,
            &self.space_stretch,
            &self.space_shrink,
            &self.x_height,
            &self.quad,
            &self.extra_space,
        ] {
            fix_word.serialize_tfm(output);
        }
        self.additional_params.serialize_tfm(output);
    }
}

impl DeserializeTfm for Params {
    fn deserialize_tfm(input: &mut Input) -> Self {
        Params {
            slant: FixWord::deserialize_tfm(input),
            space: FixWord::deserialize_tfm(input),
            space_stretch: FixWord::deserialize_tfm(input),
            space_shrink: FixWord::deserialize_tfm(input),
            x_height: FixWord::deserialize_tfm(input),
            quad: FixWord::deserialize_tfm(input),
            extra_space: FixWord::deserialize_tfm(input),
            additional_params: Vec::<u32>::deserialize_tfm(input),
        }
    }
}

impl<T: SerializeTfm> SerializeTfm for Vec<T> {
    fn serialize_tfm(&self, output: &mut Output) {
        for elem in self {
            elem.serialize_tfm(output);
        }
    }
}

impl<T: DeserializeTfm> DeserializeTfm for Vec<T> {
    fn deserialize_tfm(input: &mut Input) -> Self {
        let mut fix_words = Vec::with_capacity(input.len());
        while !input.empty() {
            fix_words.push(T::deserialize_tfm(input));
        }
        fix_words
    }
}

impl SerializeTfm for u32 {
    fn serialize_tfm(&self, output: &mut Output) {
        output.write_u32(*self);
    }
}

impl DeserializeTfm for u32 {
    fn deserialize_tfm(input: &mut Input) -> Self {
        input.read_u32()
    }
}

impl SerializeTfm for String {
    fn serialize_tfm(&self, output: &mut Output) {
        let mut hopper = vec![self.len().try_into().unwrap()];
        for b in self.as_bytes() {
            hopper.push(*b);
            if hopper.len() == 4 {
                output.write_u8s((hopper[0], hopper[1], hopper[2], hopper[3]));
                hopper.clear();
            }
        }
        if hopper.is_empty() {
            return;
        }
        while hopper.len() < 4 {
            hopper.push(0);
        }
        output.write_u8s((hopper[0], hopper[1], hopper[2], hopper[3]));
    }
}

impl DeserializeTfm for String {
    fn deserialize_tfm(input: &mut Input) -> Self {
        let (mut len, b, c, d) = input.read_u8s();
        // todo validate len
        let mut stack = vec![d, c, b];
        let mut s = String::new();
        while len > 0 {
            let raw_char = match stack.pop() {
                None => {
                    let (a, b, c, d) = input.read_u8s();
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

impl SerializeTfm for RawFile {
    fn serialize_tfm(&self, output: &mut Output) {
        let mut lh = 2_u16;
        if self.header.character_coding_scheme.is_some() {
            lh += 10
        }
        if self.header.font_family.is_some() {
            lh += 5;
        }
        if self.header.seven_bit_safe.is_some() {
            lh += 1;
        }
        lh += len_u16(&self.header.additional_data);
        let bc = self.first_char;
        let ec = len_u16(&self.raw_char_info) + bc - 1;
        let nw = len_u16(&self.widths);
        let nh = len_u16(&self.heights);
        let nd = len_u16(&self.depths);
        let ni = len_u16(&self.italic_corrections);
        let nl = len_u16(&self.lig_kerns);
        let nk = len_u16(&self.kerns);
        let ne = len_u16(&self.extensible_chars);
        let np = 7 + len_u16(&self.params.additional_params);
        let lf = 6 + lh + (ec - bc + 1) + nw + nh + nd + ni + nl + nk + ne + np;
        output.write_u16s((lf, lh));
        output.write_u16s((bc, ec));
        output.write_u16s((nw, nh));
        output.write_u16s((nd, ni));
        output.write_u16s((nl, nk));
        output.write_u16s((ne, np));

        self.header.serialize_tfm(output);
        self.raw_char_info.serialize_tfm(output);
        self.widths.serialize_tfm(output);
        self.heights.serialize_tfm(output);
        self.depths.serialize_tfm(output);
        self.italic_corrections.serialize_tfm(output);
        self.lig_kerns.serialize_tfm(output);
        self.kerns.serialize_tfm(output);
        self.extensible_chars.serialize_tfm(output);
        self.params.serialize_tfm(output);
    }
}

fn len_u16<T>(v: &[T]) -> u16 {
    v.len().try_into().unwrap_or(u16::MAX)
}

impl DeserializeTfm for RawFile {
    fn deserialize_tfm(input: &mut Input) -> Self {
        let (lf, lh) = input.read_u16s();
        let (bc, ec) = input.read_u16s();
        let (nw, nh) = input.read_u16s();
        let (nd, ni) = input.read_u16s();
        let (nl, nk) = input.read_u16s();
        let (ne, np) = input.read_u16s();
        if input.len() * 4 < (lf as usize) {
            panic!["file size doesn't match"]
        }
        if lf != 6 + lh + (ec - bc + 1) + nw + nh + nd + ni + nl + nk + ne + np {
            panic!["inconsistent tfm file"]
        }

        fn deserialize_from_slice<T: DeserializeTfm>(input: &mut Input, num_words: u16) -> T {
            let mut raw = input.slice(num_words);
            T::deserialize_tfm(&mut raw)
        }

        RawFile {
            header: deserialize_from_slice(input, lh),
            first_char: bc,
            raw_char_info: deserialize_from_slice(input, ec - bc + 1),
            widths: deserialize_from_slice(input, nw),
            heights: deserialize_from_slice(input, nh),
            depths: deserialize_from_slice(input, nd),
            italic_corrections: deserialize_from_slice(input, ni),
            lig_kerns: deserialize_from_slice(input, nl),
            kerns: deserialize_from_slice(input, nk),
            extensible_chars: deserialize_from_slice(input, ne),
            params: deserialize_from_slice(input, np),
        }
    }
}

impl SerializeTfm for RawCharInfo {
    fn serialize_tfm(&self, output: &mut Output) {
        let a = self.width_index.try_into().unwrap_or(u8::MAX);
        let b = (self.height_index.try_into().unwrap_or(u8::MAX) << 4)
            + self.depth_index.try_into().unwrap_or(u8::MAX);
        let c = (self.italic_index.try_into().unwrap_or(u8::MAX) << 2)
            + (match self.tag {
                Tag::None => 0,
                Tag::Ligature(_) => 1,
                Tag::List(_) => 2,
                Tag::Extension(_) => 3,
            });
        let d = match self.tag {
            Tag::None => 0,
            Tag::Ligature(d) => d,
            Tag::List(d) => d,
            Tag::Extension(d) => d,
        }
        .try_into()
        .unwrap_or(u8::MAX);
        output.write_u8s((a, b, c, d));
    }
}

impl DeserializeTfm for RawCharInfo {
    fn deserialize_tfm(input: &mut Input) -> Self {
        let (width_index, a, b, remainder) = input.read_u8s();
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

impl SerializeTfm for RawLigKern {
    fn serialize_tfm(&self, output: &mut Output) {
        let next_raw_lig_kern = match self.next_raw_lig_kern {
            None => 128,
            Some(n) => n.try_into().unwrap_or(u8::MAX),
        };
        match self.op {
            RawLigKernOp::Kern(k) => {
                let remainder = (k % (1 << 8)).try_into().unwrap_or(u8::MAX);
                let op_byte = (k >> 8).try_into().unwrap_or(u8::MAX) + 128;
                output.write_u8s((next_raw_lig_kern, self.next_char, op_byte, remainder));
            }
            RawLigKernOp::Ligature {
                insert_char,
                delete_current,
                delete_next,
                skip,
            } => {
                let mut op_byte = skip << 2;
                if delete_next {
                    op_byte += 1;
                }
                if delete_current {
                    op_byte += 2;
                }
                output.write_u8s((next_raw_lig_kern, self.next_char, op_byte, insert_char));
            }
        }
    }
}

impl DeserializeTfm for RawLigKern {
    fn deserialize_tfm(input: &mut Input) -> Self {
        let (skip_byte, next_char, op_byte, remainder) = input.read_u8s();
        RawLigKern {
            next_raw_lig_kern: if skip_byte < 128 {
                Some(skip_byte as usize)
            } else {
                None
            },
            next_char,
            op: if op_byte < 128 {
                let delete_next = (op_byte % 2) == 1;
                let op_byte = op_byte / 2;
                let delete_current = (op_byte % 2) == 1;
                RawLigKernOp::Ligature {
                    insert_char: remainder,
                    delete_current,
                    delete_next,
                    skip: op_byte / 2,
                }
            } else {
                RawLigKernOp::Kern(256 * (op_byte as usize - 128) + remainder as usize)
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    static CMR10_TFM: &'static [u8] = include_bytes!("cmr10.tfm");

    macro_rules! serialize_deserialize_tests {
        ($(($name:ident, $deserialized:expr, $serialized: expr),)*) => {
        $(
            #[test]
            fn $name() {
                let serialized: &[u8] = $serialized;
                let deserialized = $deserialized;

                let got_serialized = serialize_tfm(&deserialized);
                assert_eq!(serialized, got_serialized);

                // This is a hack to avoid having to provide a type hint to DeserializeTfm::deserialize_tfm
                let mut v = vec![deserialized];
                v.push(deserialize_tfm(&serialized));
                assert_eq!(v[0], v[1]);
            }
        )*
        }
    }

    serialize_deserialize_tests!(
        (fix_word_0, FixWord(0), &[0, 0, 0, 0]),
        (fix_word_1, FixWord(1), &[0, 0, 0, 1]),
        (fix_word_2, FixWord(-2), &[255, 255, 255, 254]),
        (
            header,
            Header {
                checksum: 1,
                design_size: FixWord(2),
                character_coding_scheme: Some("a".to_string()),
                font_family: Some("b".to_string()),
                seven_bit_safe: Some(true),
                face: None,
                additional_data: vec![],
            },
            &[
                0, 0, 0, 1, 0, 0, 0, 2, 1, 97, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 98, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 0, 0, 234
            ]
        ),
        (
            raw_file,
            RawFile {
                header: Header {
                    checksum: 1,
                    design_size: FixWord(2),
                    character_coding_scheme: None,
                    font_family: None,
                    seven_bit_safe: None,
                    face: None,
                    additional_data: vec![],
                },
                first_char: 3,
                raw_char_info: vec!(RawCharInfo {
                    width_index: 201,
                    height_index: 0,
                    depth_index: 0,
                    italic_index: 0,
                    tag: Tag::None,
                }),
                widths: vec!(FixWord(3), FixWord(4)),
                heights: vec!(FixWord(5), FixWord(6), FixWord(7)),
                depths: vec!(FixWord(8), FixWord(9), FixWord(10), FixWord(11)),
                italic_corrections: vec!(
                    FixWord(12),
                    FixWord(13),
                    FixWord(14),
                    FixWord(15),
                    FixWord(16)
                ),
                lig_kerns: vec!(),
                kerns: vec!(FixWord(31)),
                extensible_chars: vec!(),
                params: Params {
                    slant: FixWord(51),
                    space: FixWord(52),
                    space_stretch: FixWord(53),
                    space_shrink: FixWord(54),
                    x_height: FixWord(55),
                    quad: FixWord(56),
                    extra_space: FixWord(57),
                    additional_params: vec!(),
                }
            },
            &[
                0, 31, 0, 2, 0, 3, 0, 3, 0, 2, 0, 3, 0, 4, 0, 5, 0, 0, 0, 1, 0, 0, 0, 7, 0, 0, 0,
                1, 0, 0, 0, 2, 201, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0,
                0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 10, 0, 0, 0, 11, 0, 0, 0, 12, 0, 0, 0, 13,
                0, 0, 0, 14, 0, 0, 0, 15, 0, 0, 0, 16, 0, 0, 0, 31, 0, 0, 0, 51, 0, 0, 0, 52, 0, 0,
                0, 53, 0, 0, 0, 54, 0, 0, 0, 55, 0, 0, 0, 56, 0, 0, 0, 57,
            ]
        ),
    );

    #[test]
    fn raw_deserialize_serialize_round_trip() {
        let deserialized = deserialize_tfm::<RawFile>(CMR10_TFM);
        let reserialized = serialize_tfm(&deserialized);
        assert_eq!(CMR10_TFM, reserialized);
    }
}
