use super::*;

pub fn deserialize_tfm(b: &[u8]) -> File {
    let mut input = Input { b };
    let raw_file = RawFile::deserialize_tfm(&mut input);
    println!("{:?}", raw_file);
    File {
        header: raw_file.header,
        char_infos: vec![],
    }
}

trait SerializeTfm {
    fn serialize_tfm<'a, 'b>(&self, output: &mut Output);
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
    fn deserialize_tfm<'a, 'b>(input: &'b mut Input<'a>) -> Self;
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

    // TODO: can we return &mut?
    fn head<T: Into<usize>>(&mut self, num_words: T) -> Input<'a> {
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
            Some(face) => face.serialize(),
        };
        output.write_u8s((seven_bit_safe_raw, 0, 0, face_raw));
        self.trailing_words.serialize_tfm(output);
    }
}

impl DeserializeTfm for Header {
    fn deserialize_tfm<'a, 'b>(input: &'b mut Input<'a>) -> Self {
        // todo: check for size and error
        let checksum = input.read_u32();
        let design_size = FixWord::deserialize_tfm(input);
        let character_coding_scheme = {
            let mut raw = input.head(10_usize);
            match raw.len() == 10 {
                true => Some(String::deserialize_tfm(&mut raw)),
                false => None,
            }
        };
        let font_family = {
            let mut raw = input.head(5_usize);
            match raw.len() == 5 {
                true => Some(String::deserialize_tfm(&mut raw)),
                false => None,
            }
        };
        let (seven_bit_safe, face) = if input.empty() {
            (None, None)
        } else {
            let (seven_bit_safe_raw, _, _, face_raw) = input.read_u8s();
            (Some(seven_bit_safe_raw >= 128), Face::deserialize(face_raw))
        };
        let trailing_words = Vec::<u32>::deserialize_tfm(input);
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

impl SerializeTfm for FixWord {
    fn serialize_tfm<'a, 'b>(&self, output: &mut Output) {
        output.write_u32(self.0 as u32);
    }
}

impl DeserializeTfm for FixWord {
    fn deserialize_tfm<'a, 'b>(input: &'b mut Input<'a>) -> Self {
        FixWord(input.read_u32() as i32)
    }
}

impl SerializeTfm for ExtensibleChar {
    fn serialize_tfm(&self, output: &mut Output) {
        output.write_u8s((self.top, self.bottom, self.middle, self.rep))
    }
}

impl DeserializeTfm for ExtensibleChar {
    fn deserialize_tfm<'a, 'b>(input: &'b mut Input<'a>) -> Self {
        let (top, middle, bottom, rep) = input.read_u8s();
        ExtensibleChar {
            top,
            middle,
            bottom,
            rep,
        }
    }
}

impl DeserializeTfm for Params {
    fn deserialize_tfm<'a, 'b>(input: &'b mut Input<'a>) -> Self {
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
    fn deserialize_tfm<'a, 'b>(input: &'b mut Input<'a>) -> Self {
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
    fn deserialize_tfm<'a, 'b>(input: &'b mut Input<'a>) -> Self {
        input.read_u32()
    }
}

impl SerializeTfm for String {
    fn serialize_tfm(&self, output: &mut Output) {
        let mut hopper = Vec::new();
        hopper.push(self.len().try_into().unwrap());
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
    fn deserialize_tfm<'a, 'b>(input: &'b mut Input<'a>) -> Self {
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

#[derive(Debug)]
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
        lh += len_u16(&self.header.trailing_words);
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
    }
}

fn len_u16<T>(v: &[T]) -> u16 {
    v.len().try_into().unwrap_or(u16::MAX)
}

impl DeserializeTfm for RawFile {
    fn deserialize_tfm<'a, 'b>(input: &'b mut Input<'a>) -> Self {
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
        RawFile {
            header: {
                let mut raw = input.head(lh);
                Header::deserialize_tfm(&mut raw)
            },
            first_char: bc,
            raw_char_info: {
                let mut raw = input.head(ec - bc + 1);
                Vec::<RawCharInfo>::deserialize_tfm(&mut raw)
            },
            widths: {
                let mut raw = input.head(nw);
                Vec::<FixWord>::deserialize_tfm(&mut raw)
            },
            heights: {
                let mut raw = input.head(nh);
                Vec::<FixWord>::deserialize_tfm(&mut raw)
            },
            depths: {
                let mut raw = input.head(nd);
                Vec::<FixWord>::deserialize_tfm(&mut raw)
            },
            italic_corrections: {
                let mut raw = input.head(ni);
                Vec::<FixWord>::deserialize_tfm(&mut raw)
            },
            lig_kerns: {
                let mut raw = input.head(nl);
                Vec::<RawLigKern>::deserialize_tfm(&mut raw)
            },
            kerns: {
                let mut raw = input.head(nk);
                Vec::<FixWord>::deserialize_tfm(&mut raw)
            },
            extensible_chars: {
                let mut raw = input.head(ne);
                Vec::<ExtensibleChar>::deserialize_tfm(&mut raw)
            },
            params: {
                let mut raw = input.head(np);
                Params::deserialize_tfm(&mut raw)
            },
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
    fn deserialize_tfm<'a, 'b>(input: &'b mut Input<'a>) -> Self {
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

#[derive(Debug)]
struct RawLigKern {
    next_raw_lig_kern: Option<usize>,
    next_char: u8,
    op: RawLigKernOp,
}

#[derive(Debug)]
enum RawLigKernOp {
    Kern(usize),
    Ligature {
        insert_char: u8,
        delete_current: bool,
        delete_next: bool,
        skip: u8,
    },
}

impl DeserializeTfm for RawLigKern {
    fn deserialize_tfm<'a, 'b>(input: &'b mut Input<'a>) -> Self {
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

    #[test]
    fn raw_deserialize_serialize_round_trip() {
        let raw_file = RawFile::deserialize_tfm(&mut Input { b: CMR10_TFM });
        let mut output = Output { b: vec![] };
        raw_file.serialize_tfm(&mut output);
        assert_eq!(CMR10_TFM[0..output.b.len()], output.b);
    }
}
