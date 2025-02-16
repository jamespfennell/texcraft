use super::*;

pub fn deserialize(b: &[u8]) -> Result<Option<(Op, &[u8])>, InvalidDviFile> {
    let Some((&op_code, tail)) = b.split_first() else {
        return Ok(None);
    };
    let mut d = Deserializer { op_code, b: tail };
    let op = match op_code {
        0..128 => Op::TypesetChar {
            char: op_code as u32,
            move_h: true,
        },
        128 => Op::TypesetChar {
            char: d.u8()?.into(),
            move_h: true,
        },
        129 => Op::TypesetChar {
            char: d.u16()?.into(),
            move_h: true,
        },
        130 => Op::TypesetChar {
            char: d.u24()?,
            move_h: true,
        },
        131 => Op::TypesetChar {
            char: d.u32()?,
            move_h: true,
        },
        132 => Op::TypesetRule {
            height: d.u32()?,
            width: d.u32()?,
            move_h: true,
        },
        133 => Op::TypesetChar {
            char: d.u8()?.into(),
            move_h: false,
        },
        134 => Op::TypesetChar {
            char: d.u16()?.into(),
            move_h: false,
        },
        135 => Op::TypesetChar {
            char: d.u24()?,
            move_h: false,
        },
        136 => Op::TypesetChar {
            char: d.u32()?,
            move_h: false,
        },
        137 => Op::TypesetRule {
            height: d.u32()?,
            width: d.u32()?,
            move_h: false,
        },
        138 => Op::NoOp,
        139 => Op::BeginPage {
            parameters: [
                d.i32()?,
                d.i32()?,
                d.i32()?,
                d.i32()?,
                d.i32()?,
                d.i32()?,
                d.i32()?,
                d.i32()?,
                d.i32()?,
                d.i32()?,
            ],
            previous_begin_page: d.i32()?,
        },
        140 => Op::EndPage,
        141 => Op::Push,
        142 => Op::Pop,
        143 => Op::Right(d.i8()?.into()),
        144 => Op::Right(d.i16()?.into()),
        145 => Op::Right(d.i24()?),
        146 => Op::Right(d.i32()?),
        147 => Op::Move(Var::W),
        148 => Op::SetVar(Var::W, d.i8()?.into()),
        149 => Op::SetVar(Var::W, d.i16()?.into()),
        150 => Op::SetVar(Var::W, d.i24()?),
        151 => Op::SetVar(Var::W, d.i32()?),
        152 => Op::Move(Var::X),
        153 => Op::SetVar(Var::X, d.i8()?.into()),
        154 => Op::SetVar(Var::X, d.i16()?.into()),
        155 => Op::SetVar(Var::X, d.i24()?),
        156 => Op::SetVar(Var::X, d.i32()?),
        157 => Op::Down(d.i8()?.into()),
        158 => Op::Down(d.i16()?.into()),
        159 => Op::Down(d.i24()?),
        160 => Op::Down(d.i32()?),
        161 => Op::Move(Var::Y),
        162 => Op::SetVar(Var::Y, d.i8()?.into()),
        163 => Op::SetVar(Var::Y, d.i16()?.into()),
        164 => Op::SetVar(Var::Y, d.i24()?),
        165 => Op::SetVar(Var::Y, d.i32()?),
        166 => Op::Move(Var::Z),
        167 => Op::SetVar(Var::Z, d.i8()?.into()),
        168 => Op::SetVar(Var::Z, d.i16()?.into()),
        169 => Op::SetVar(Var::Z, d.i24()?),
        170 => Op::SetVar(Var::Z, d.i32()?),
        171..235 => Op::EnableFont((op_code - 171) as u32),
        235 => Op::EnableFont(d.u8()?.into()),
        236 => Op::EnableFont(d.u16()?.into()),
        237 => Op::EnableFont(d.u24()?),
        238 => Op::EnableFont(d.u32()?),
        239 => {
            let n = d.u8()?;
            d.extension(n.into())?
        }
        240 => {
            let n = d.u16()?;
            d.extension(n.into())?
        }
        241 => {
            let n = d.u24()?;
            d.extension(n)?
        }
        242 => {
            let n = d.u32()?;
            d.extension(n)?
        }
        243 => {
            let n = d.u8()?;
            d.font_def(n.into())?
        }
        244 => {
            let n = d.u16()?;
            d.font_def(n.into())?
        }
        245 => {
            let n = d.u24()?;
            d.font_def(n)?
        }
        246 => {
            let n = d.u32()?;
            d.font_def(n)?
        }
        247 => Op::Preamble {
            dvi_format: d.u8()?,
            unit_numerator: d.u32()?,
            unit_denominator: d.u32()?,
            magnification: d.u32()?,
            comment: d.string()?,
        },
        248 => Op::BeginPostamble {
            final_begin_page: d.i32()?,
            unit_numerator: d.u32()?,
            unit_denominator: d.u32()?,
            magnification: d.u32()?,
            largest_height: d.u32()?,
            largest_width: d.u32()?,
            max_stack_depth: d.u16()?,
            num_pages: d.u16()?,
        },
        249 => {
            let dvi_format = d.u8()?;
            let postamble = d.i32()?;
            let mut num_end_bytes = 0;
            while let Some((head, tail)) = d.b.split_first() {
                if *head != 223 {
                    break;
                }
                num_end_bytes += 1;
                d.b = tail;
            }
            Op::EndPostamble {
                dvi_format,
                postamble,
                num_223_bytes: num_end_bytes,
            }
        }
        250..=255 => {
            return Err(InvalidDviFile::InvalidOpCode(op_code));
        }
    };
    Ok(Some((op, d.b)))
}

pub struct Deserializer<'a> {
    op_code: u8,
    b: &'a [u8],
}

impl<'a> Deserializer<'a> {
    fn u8(&mut self) -> Result<u8, InvalidDviFile> {
        Ok(u8::from_be_bytes(self.get::<1>()?))
    }

    fn i8(&mut self) -> Result<i8, InvalidDviFile> {
        Ok(i8::from_be_bytes(self.get::<1>()?))
    }

    fn i16(&mut self) -> Result<i16, InvalidDviFile> {
        Ok(i16::from_be_bytes(self.get::<2>()?))
    }

    fn u16(&mut self) -> Result<u16, InvalidDviFile> {
        Ok(u16::from_be_bytes(self.get::<2>()?))
    }

    fn i24(&mut self) -> Result<i32, InvalidDviFile> {
        let bs = self.get::<3>()?;
        let u = u32::from_be_bytes([0, bs[0], bs[1], bs[2]]);
        let u = if u < 2_u32.pow(23) {
            u
        } else {
            // We manually handle the case of negative integers taking
            // up three bytes.
            // In u32, i is of the form 2^32+i.
            // In u24, i is of the form 2^24+1.
            // To go from u24 to u32, we add (2^32-2^24)=(2^24)(2^8-1).
            let shift: u32 = 256 * 256 * 256 * 255;
            u + shift
        };
        Ok(u as i32)
    }

    fn u24(&mut self) -> Result<u32, InvalidDviFile> {
        let bs = self.get::<3>()?;
        Ok(u32::from_be_bytes([0, bs[0], bs[1], bs[2]]))
    }

    fn i32(&mut self) -> Result<i32, InvalidDviFile> {
        Ok(i32::from_be_bytes(self.get::<4>()?))
    }

    fn u32(&mut self) -> Result<u32, InvalidDviFile> {
        Ok(u32::from_be_bytes(self.get::<4>()?))
    }

    fn string(&mut self) -> Result<String, InvalidDviFile> {
        let l: u8 = self.u8()?;
        self.string_l(l)
    }

    fn string_l(&mut self, l: u8) -> Result<String, InvalidDviFile> {
        let Some((comment, tail)) = self.b.split_at_checked(l as usize) else {
            return Err(InvalidDviFile::Truncated(self.op_code));
        };
        self.b = tail;
        Ok(String::from_utf8_lossy(comment).into())
    }

    fn extension(&mut self, l: u32) -> Result<Op, InvalidDviFile> {
        let Some((data, tail)) = self.b.split_at_checked(l as usize) else {
            return Err(InvalidDviFile::Truncated(self.op_code));
        };
        self.b = tail;
        Ok(Op::Extension(data.into()))
    }

    fn font_def(&mut self, number: u32) -> Result<Op, InvalidDviFile> {
        let checksum = self.u32()?;
        let at_size = self.u32()?;
        let design_size = self.u32()?;
        let area_l = self.u8()?;
        let name_l = self.u8()?;
        Ok(Op::DefineFont {
            number,
            checksum,
            at_size,
            design_size,
            area: self.string_l(area_l)?,
            name: self.string_l(name_l)?,
        })
    }

    fn get<const N: usize>(&mut self) -> Result<[u8; N], InvalidDviFile> {
        let Some((head, tail)) = self.b.split_at_checked(N) else {
            return Err(InvalidDviFile::Truncated(self.op_code));
        };
        self.b = tail;
        Ok(head.try_into().expect("slice has length N"))
    }
}
