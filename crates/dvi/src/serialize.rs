use super::*;

pub fn serialize(op: &Op, b: &mut Vec<u8>) {
    let mut w = Writer { b };
    match op {
        Op::TypesetChar { char: c, move_h } => match (move_h, (*c).try_into()) {
            // Fast path: small ASCII character
            (true, Ok(u @ 0..128)) => {
                w.u8(u);
            }
            // Slow path: larger characters
            _ => {
                let min_op_code = if *move_h { 128 } else { 133 };
                w.u32_var(min_op_code, *c);
            }
        },
        Op::TypesetRule {
            height,
            width,
            move_h,
        } => {
            w.u8(if *move_h { 132 } else { 137 });
            w.u32(*height);
            w.u32(*width);
        }
        Op::NoOp => {
            b.push(138);
        }
        Op::BeginPage {
            parameters,
            previous_begin_page,
        } => {
            w.u8(139);
            parameters.iter().for_each(|p| w.i32(*p));
            w.i32(*previous_begin_page);
        }
        Op::EndPage => {
            b.push(140);
        }
        Op::Push => {
            b.push(141);
        }
        Op::Pop => {
            b.push(142);
        }
        Op::Right(i) => {
            w.i32_var(143, *i);
        }
        Op::Move(v) => {
            w.u8(match v {
                Var::W => 147,
                Var::X => 152,
                Var::Y => 161,
                Var::Z => 166,
            });
        }
        Op::SetVar(v, i) => {
            let min_op_code = match v {
                Var::W => 148,
                Var::X => 153,
                Var::Y => 162,
                Var::Z => 167,
            };
            w.i32_var(min_op_code, *i);
        }
        Op::Down(i) => {
            w.i32_var(157, *i);
        }
        Op::EnableFont(u) => match TryInto::<u8>::try_into(*u) {
            Ok(u @ 0..64) => {
                w.u8(171 + u);
            }
            _ => {
                w.u32_var(235, *u);
            }
        },
        Op::Extension(v) => {
            let l: u32 = v.len().try_into().unwrap_or(u32::MAX);
            w.u32_var(239, l);
            w.b.extend_from_slice(&v[..l as usize]);
        }
        Op::DefineFont {
            number,
            checksum,
            at_size,
            design_size,
            area,
            name,
        } => {
            w.u32_var(243, *number);
            w.u32(*checksum);
            w.u32(*at_size);
            w.u32(*design_size);
            let area_l = w.str_len(area);
            let name_l = w.str_len(name);
            w.str_content(area, area_l);
            w.str_content(name, name_l);
        }
        Op::Preamble {
            dvi_format,
            unit_numerator,
            unit_denominator,
            magnification,
            comment,
        } => {
            w.u8(247);
            w.u8(*dvi_format);
            w.u32(*unit_numerator);
            w.u32(*unit_denominator);
            w.u32(*magnification);
            w.str(comment);
        }
        Op::BeginPostamble {
            final_begin_page,
            unit_numerator,
            unit_denominator,
            magnification,
            largest_height,
            largest_width,
            max_stack_depth,
            num_pages,
        } => {
            w.u8(248);
            w.i32(*final_begin_page);
            w.u32(*unit_numerator);
            w.u32(*unit_denominator);
            w.u32(*magnification);
            w.u32(*largest_height);
            w.u32(*largest_width);
            w.u16(*max_stack_depth);
            w.u16(*num_pages);
        }
        Op::EndPostamble {
            dvi_format,
            postamble: postable,
            num_223_bytes: num_end_bytes,
        } => {
            w.u8(249);
            w.u8(*dvi_format);
            w.i32(*postable);
            for _ in 0..*num_end_bytes {
                w.u8(223);
            }
        }
    }
}

struct Writer<'a> {
    b: &'a mut Vec<u8>,
}

impl<'a> Writer<'a> {
    fn u8(&mut self, u: u8) {
        self.b.push(u);
    }
    fn i32(&mut self, i: i32) {
        self.b.extend_from_slice(&i.to_be_bytes());
    }
    fn u32(&mut self, u: u32) {
        self.b.extend_from_slice(&u.to_be_bytes());
    }
    fn u16(&mut self, u: u16) {
        self.b.extend_from_slice(&u.to_be_bytes());
    }
    fn u32_var(&mut self, min_op_code: u8, u: u32) {
        let [b1, b2, b3, b4] = u.to_be_bytes();
        let b = &mut self.b;
        if b1 != 0 {
            b.push(min_op_code + 3);
            b.push(b1);
            b.push(b2);
            b.push(b3);
            b.push(b4);
        } else if b2 != 0 {
            b.push(min_op_code + 2);
            b.push(b2);
            b.push(b3);
            b.push(b4);
        } else if b3 != 0 {
            b.push(min_op_code + 1);
            b.push(b3);
            b.push(b4);
        } else {
            b.push(min_op_code);
            b.push(b4);
        }
    }
    fn i32_var(&mut self, min_op_code: u8, i: i32) {
        let b = &mut self.b;
        if let Ok(i) = TryInto::<i8>::try_into(i) {
            b.push(min_op_code);
            let [b1] = i.to_be_bytes();
            b.push(b1);
            return;
        }
        if let Ok(i) = TryInto::<i16>::try_into(i) {
            b.push(min_op_code + 1);
            let [b1, b2] = i.to_be_bytes();
            b.push(b1);
            b.push(b2);
            return;
        }
        let is_3_byte = -2_i32.pow(23) <= i && i < 2_i32.pow(23);
        let [b1, b2, b3, b4] = if is_3_byte && i < 0 {
            // We manually handle the case of negative integers taking
            // up three bytes.
            // In u32, i is of the form 2^32+i.
            // In u24, i is of the form 2^24+1.
            // To go from u32 to u24, we subtract (2^32-2^24)=(2^24)(2^8-1).
            let shift: u32 = 256 * 256 * 256 * 255;
            let u = i as u32 - shift;
            u.to_be_bytes()
        } else {
            i.to_be_bytes()
        };
        if is_3_byte {
            b.push(min_op_code + 2);
        } else {
            b.push(min_op_code + 3);
            b.push(b1);
        }
        b.push(b2);
        b.push(b3);
        b.push(b4);
    }
    fn str(&mut self, s: &str) {
        let l: u8 = self.str_len(s);
        self.str_content(s, l);
    }
    fn str_len(&mut self, s: &str) -> u8 {
        let l: u8 = s.as_bytes().len().try_into().unwrap_or(u8::MAX);
        self.b.push(l);
        l
    }
    fn str_content(&mut self, s: &str, l: u8) {
        self.b.extend_from_slice(&s.as_bytes()[..l as usize]);
    }
}
