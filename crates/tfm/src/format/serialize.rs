use super::*;
use crate::Number;

pub fn serialize(file: &File) -> Vec<u8> {
    let mut b = vec![0_u8; 24];

    serialize_header(&file.header, &mut b);
    let lh: i16 = (18 + file.header.additional_data.len())
        .try_into()
        .expect("header.len()=18+header.additional_data.len()<= i16::MAX");

    let (bc, ec) = match file.char_info_bounds() {
        None => (1, 0),
        Some((bc, ec)) => {
            serialize_char_infos(&file.char_infos, &file.char_tags, bc, ec, &mut b);
            (bc.0 as i16, ec.0 as i16)
        }
    };

    let nw = serialize_section(&file.widths, &mut b, None);

    let nh = serialize_section(&file.heights, &mut b, None);

    let nd = serialize_section(&file.depths, &mut b, None);

    let ni = serialize_section(&file.italic_corrections, &mut b, None);

    let nl = serialize_section(
        &file.lig_kern_program.instructions,
        &mut b,
        file.lig_kern_program.boundary_char,
    );
    let nk = serialize_section(&file.kerns, &mut b, None);
    let ne = serialize_section(&file.extensible_chars, &mut b, None);

    let np = serialize_section(&file.params.0, &mut b, None);

    let lf = 6 + lh + (ec + 1 - bc) + nw + nh + nd + ni + nl + nk + ne + np;
    for (u, v) in [lf, lh, bc, ec, nw, nh, nd, ni, nl, nk, ne, np]
        .into_iter()
        .enumerate()
    {
        [b[2 * u], b[2 * u + 1]] = v.to_be_bytes();
    }
    b
}

fn serialize_char_infos(
    char_infos: &HashMap<Char, CharInfo>,
    char_tags: &HashMap<Char, CharTag>,
    bc: Char,
    ec: Char,
    b: &mut Vec<u8>,
) {
    let mut v: Vec<(Option<CharInfo>, CharTag)> =
        vec![(None, CharTag::None); (ec.0 as usize) + 1 - (bc.0 as usize)];
    for (c, char_info) in char_infos {
        v[(c.0 - bc.0) as usize].0 = Some(char_info.clone());
    }
    for (c, char_tag) in char_tags {
        v[(c.0 - bc.0) as usize].1 = char_tag.clone();
    }
    serialize_section(&v, b, None);
}

fn serialize_section<T: Serializable>(t: &[T], b: &mut Vec<u8>, c: Option<Char>) -> i16 {
    let start = b.len();
    for element in t {
        element.serialize(b, c);
    }
    ((b.len() - start) / 4).try_into().unwrap()
}

trait Serializable: Sized {
    fn serialize(&self, b: &mut Vec<u8>, _: Option<Char>);
}

impl Serializable for u32 {
    fn serialize(&self, b: &mut Vec<u8>, _: Option<Char>) {
        b.extend(self.to_be_bytes())
    }
}

impl Serializable for (Option<CharInfo>, CharTag) {
    fn serialize(&self, b: &mut Vec<u8>, _: Option<Char>) {
        let italic = match &self.0 {
            None => {
                b.extend([0; 2]);
                0
            }
            Some(char_info) => {
                b.push(char_info.width_index.get());
                b.push(
                    char_info
                        .height_index
                        .wrapping_mul(16)
                        .wrapping_add(char_info.depth_index),
                );
                char_info.italic_index
            }
        };
        let (discriminant, payload) = match self.1 {
            CharTag::None => (0_u8, 0_u8),
            CharTag::Ligature(p) => (1, p),
            CharTag::List(p) => (2, p.0),
            CharTag::Extension(p) => (3, p),
        };
        b.push(italic.wrapping_mul(4).wrapping_add(discriminant));
        b.push(payload);
    }
}

impl Serializable for Number {
    fn serialize(&self, b: &mut Vec<u8>, _: Option<Char>) {
        (self.0 as u32).serialize(b, None)
    }
}

impl Serializable for ligkern::lang::Instruction {
    fn serialize(&self, b: &mut Vec<u8>, boundary_char: Option<Char>) {
        // PLtoTF.2014.142
        let first = [self.next_instruction.unwrap_or(128), self.right_char.0];
        match self.operation {
            ligkern::lang::Operation::Kern(_) => {
                panic!("tfm::format::File lig/kern programs cannot contain `Kern` operations. Use `KernAtIndex` operations instead and provide an appropriate kerns array.");
            }
            ligkern::lang::Operation::KernAtIndex(index) => {
                let [hi, lo] = index.to_be_bytes();
                b.extend(first);
                b.push(hi + 128);
                b.push(lo);
            }
            ligkern::lang::Operation::Ligature {
                char_to_insert,
                post_lig_operation,
            } => {
                use ligkern::lang::PostLigOperation::*;
                b.extend(first);
                b.push(match post_lig_operation {
                    RetainBothMoveNowhere => 3,
                    RetainBothMoveToInserted => 3 + 4,
                    RetainBothMoveToRight => 3 + 8,
                    RetainRightMoveToInserted => 1,
                    RetainRightMoveToRight => 1 + 4,
                    RetainLeftMoveNowhere => 2,
                    RetainLeftMoveToInserted => 2 + 4,
                    RetainNeitherMoveToInserted => 0,
                });
                b.push(char_to_insert.0);
            }
            ligkern::lang::Operation::EntrypointRedirect(index, char) => {
                b.extend(match char {
                    false => [255, 0],
                    true => match boundary_char {
                        None => [254, 0],
                        Some(c) => [255, c.0],
                    },
                });
                b.extend(index.to_be_bytes());
            }
        }
    }
}

impl Serializable for ExtensibleRecipe {
    fn serialize(&self, b: &mut Vec<u8>, _: Option<Char>) {
        b.push(self.top.unwrap_or(Char(0)).0);
        b.push(self.middle.unwrap_or(Char(0)).0);
        b.push(self.bottom.unwrap_or(Char(0)).0);
        b.push(self.rep.0);
    }
}

fn serialize_string(s: &Option<String>, size: u8, b: &mut Vec<u8>) {
    // TODO: issue a warning as in PLtoTF.2014.87 if the string doesn't fit
    let s = match s {
        None => "",
        Some(s) => s,
    };
    let len_padding_or = match s.len().try_into() {
        Ok(len) => size.checked_sub(len).map(|padding| (len, padding)),
        Err(_) => None,
    };
    match len_padding_or {
        None => {
            b.push(size);
            b.extend(s[0..size as usize].as_bytes())
        }
        Some((len, padding)) => {
            b.push(len);
            b.extend(s.as_bytes());
            b.extend(vec![0; padding as usize]);
        }
    }
}

fn serialize_header(header: &Header, b: &mut Vec<u8>) {
    header.checksum.serialize(b, None);
    header.design_size.serialize(b, None);
    serialize_string(&header.character_coding_scheme, 39, b);
    serialize_string(&header.font_family, 19, b);
    if header.seven_bit_safe == Some(true) {
        // Any value >=128 is interpreted as true, but PLtoTF.2014.133 uses 128 exactly...
        b.push(128);
    } else {
        // ...and 0 for false.
        b.push(0);
    }
    b.push(0);
    b.push(0);
    b.push(header.face.unwrap_or(0_u8.into()).into());
    serialize_section(&header.additional_data, b, None);
}
