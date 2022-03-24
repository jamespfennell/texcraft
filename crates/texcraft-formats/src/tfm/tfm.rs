use super::*;

pub fn deserialize_tfm(b: &[u8]) -> File {
    let mut word_stream = WordStream { b };
    let raw_file = RawFile::deserialize_tfm(&mut word_stream);
    println!("{:?}", raw_file);
    File {
        header: raw_file.header,
        char_infos: vec![],
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

impl DeserializeTfm for FixWord {
    fn deserialize_tfm<'a, 'b>(tfm_stream: &'b mut WordStream<'a>) -> Self {
        FixWord(tfm_stream.read_u32() as i32)
    }
}

impl DeserializeTfm for ExtensibleChar {
    fn deserialize_tfm<'a, 'b>(tfm_stream: &'b mut WordStream<'a>) -> Self {
        let (top, middle, bottom, rep) = tfm_stream.read_u8s();
        ExtensibleChar {
            top,
            middle,
            bottom,
            rep,
        }
    }
}

impl DeserializeTfm for Params {
    fn deserialize_tfm<'a, 'b>(word_stream: &'b mut WordStream<'a>) -> Self {
        Params {
            slant: FixWord::deserialize_tfm(word_stream),
            space: FixWord::deserialize_tfm(word_stream),
            space_stretch: FixWord::deserialize_tfm(word_stream),
            space_shrink: FixWord::deserialize_tfm(word_stream),
            x_height: FixWord::deserialize_tfm(word_stream),
            quad: FixWord::deserialize_tfm(word_stream),
            extra_space: FixWord::deserialize_tfm(word_stream),
            additional_params: Vec::<u32>::deserialize_tfm(word_stream),
        }
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

#[derive(Debug)]
struct RawFile {
    header: Header,
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

impl DeserializeTfm for RawFile {
    fn deserialize_tfm<'a, 'b>(word_stream: &'b mut WordStream<'a>) -> Self {
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
        RawFile {
            header: {
                let mut raw = word_stream.head(lh);
                Header::deserialize_tfm(&mut raw)
            },
            raw_char_info: {
                let mut raw = word_stream.head(ec - bc + 4);
                Vec::<RawCharInfo>::deserialize_tfm(&mut raw)
            },
            widths: {
                let mut raw = word_stream.head(nw);
                Vec::<FixWord>::deserialize_tfm(&mut raw)
            },
            heights: {
                let mut raw = word_stream.head(nh);
                Vec::<FixWord>::deserialize_tfm(&mut raw)
            },
            depths: {
                let mut raw = word_stream.head(nd);
                Vec::<FixWord>::deserialize_tfm(&mut raw)
            },
            italic_corrections: {
                let mut raw = word_stream.head(ni);
                Vec::<FixWord>::deserialize_tfm(&mut raw)
            },
            lig_kerns: {
                let mut raw = word_stream.head(nl);
                Vec::<RawLigKern>::deserialize_tfm(&mut raw)
            },
            kerns: {
                let mut raw = word_stream.head(nk);
                Vec::<FixWord>::deserialize_tfm(&mut raw)
            },
            extensible_chars: {
                let mut raw = word_stream.head(ne);
                Vec::<ExtensibleChar>::deserialize_tfm(&mut raw)
            },
            params: {
                let mut raw = word_stream.head(np);
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
    fn deserialize_tfm<'a, 'b>(word_stream: &'b mut WordStream<'a>) -> Self {
        let (skip_byte, next_char, op_byte, remainder) = word_stream.read_u8s();
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
