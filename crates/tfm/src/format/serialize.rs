use super::*;
use crate::Number;

pub fn serialize(file: &File) -> Vec<u8> {
    let mut b = vec![0_u8; 24];

    let lh = serialize_section(&file.header, &mut b);

    let (bc, ec) = if file.char_infos.is_empty() {
        (1, 0)
    } else {
        let bc = file.smallest_char_code.0 as u16;
        let diff = serialize_section(&file.char_infos, &mut b);
        (bc, bc + diff - 1)
    };

    let nw = serialize_section(&file.widths, &mut b);

    let nh = serialize_section(&file.heights, &mut b);

    let nd = serialize_section(&file.depths, &mut b);

    let ni = serialize_section(&file.italic_corrections, &mut b);

    let nl = serialize_section(&file.lig_kern_instructions, &mut b);
    let nk = serialize_section(&file.kerns, &mut b);
    let ne = serialize_section(&file.extensible_chars, &mut b);

    let np = serialize_section(&file.params, &mut b);

    let lf = 6 + lh + (ec + 1 - bc) + nw + nh + nd + ni + nl + nk + ne + np;
    for (u, v) in [lf, lh, bc, ec, nw, nh, nd, ni, nl, nk, ne, np]
        .into_iter()
        .enumerate()
    {
        [b[2 * u], b[2 * u + 1]] = v.to_be_bytes();
    }
    b
}

fn serialize_section<T: Serializable>(t: &T, b: &mut Vec<u8>) -> u16 {
    let start = b.len();
    t.serialize(b);
    ((b.len() - start) / 4).try_into().unwrap()
}

trait Serializable: Sized {
    fn serialize(&self, b: &mut Vec<u8>);
}

impl Serializable for u32 {
    fn serialize(&self, b: &mut Vec<u8>) {
        b.extend(self.to_be_bytes())
    }
}

impl Serializable for Option<CharInfo> {
    fn serialize(&self, b: &mut Vec<u8>) {
        match self {
            None => b.extend([0; 4]),
            Some(char_info) => {
                b.push(char_info.width_index.get());
                b.push(
                    char_info
                        .height_index
                        .wrapping_mul(16)
                        .wrapping_add(char_info.depth_index),
                );
                let (discriminant, payload) = match char_info.tag {
                    CharTag::None => (0_u8, 0_u8),
                    CharTag::Ligature(p) => (1, p),
                    CharTag::List(p) => (2, p.0),
                    CharTag::Extension(p) => (3, p),
                };
                b.push(
                    char_info
                        .italic_index
                        .wrapping_mul(4)
                        .wrapping_add(discriminant),
                );
                b.push(payload);
            }
        }
    }
}

impl<T: Serializable> Serializable for Vec<T> {
    fn serialize(&self, b: &mut Vec<u8>) {
        for element in self {
            element.serialize(b)
        }
    }
}

impl Serializable for Number {
    fn serialize(&self, b: &mut Vec<u8>) {
        (self.0 as u32).serialize(b)
    }
}

impl Serializable for ligkern::lang::Instruction {
    fn serialize(&self, b: &mut Vec<u8>) {
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
            ligkern::lang::Operation::Stop(index) => {
                let [hi, lo] = index.to_be_bytes();
                // TODO: when we support boundary chars this will need to be updated
                b.push(254);
                b.push(0);
                b.push(hi);
                b.push(lo);
            }
        }
    }
}

impl Serializable for ExtensibleRecipe {
    fn serialize(&self, b: &mut Vec<u8>) {
        b.push(self.top.unwrap_or(Char(0)).0);
        b.push(self.middle.unwrap_or(Char(0)).0);
        b.push(self.bottom.unwrap_or(Char(0)).0);
        b.push(self.rep.0);
    }
}

impl Serializable for Params {
    fn serialize(&self, b: &mut Vec<u8>) {
        self.0.serialize(b)
    }
}

fn serialize_string(s: &str, size: u8, b: &mut Vec<u8>) {
    // TODO: issue a warning as in PLtoTF.2014.87 if the string doesn't fit
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

impl Serializable for Header {
    fn serialize(&self, b: &mut Vec<u8>) {
        self.checksum.serialize(b);
        self.design_size.serialize(b);
        serialize_string(&self.character_coding_scheme, 39, b);
        serialize_string(&self.font_family, 19, b);
        if self.seven_bit_safe == Some(true) {
            // Any value >=128 is interpreted as true, but PLtoTF.2014.133 uses 128 exactly...
            b.push(128);
        } else {
            // ...and 0 for false.
            b.push(0);
        }
        b.push(0);
        b.push(0);
        b.push(self.face.unwrap_or(0_u8.into()).into());
        self.additional_data.serialize(b);
    }
}
