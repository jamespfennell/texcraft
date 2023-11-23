use super::*;

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    /// The TFM file is empty (i.e., 0 bytes).
    ///
    /// Knuth's TFToPL doesn't handle this case explicitly.
    /// Providing an empty file to the TFtoPL always returns an error.
    /// However, the error message is non-deterministic
    ///     and depends on the initial value of a specific byte of memory (`tfm[0]`).
    /// If that byte is greater than 127 the message for [`Error::InternalFileLengthIsNegative`]
    ///     is printed; otherwise the message for [`Error::FileHasOneByte`] is printed.
    FileIsEmpty,
    /// The TFM file consists of a single byte.
    FileHasOneByte(u8),
    /// The file length specified inside the TFM file is invalid because it is zero.
    InternalFileLengthIsZero,
    /// The file length specified inside the TFM file is invalid because it is negative.
    ///
    /// The variant payload is the invalid length in this file.
    InternalFileLengthIsNegative(i16),
    /// The file length specified inside the TFM file is invalid because the file is smaller than it claims.
    ///
    /// The variant payload is the invalid length of this file (in words) and the actual file size (in bytes).
    /// One word is 4 bytes.
    InternalFileLengthIsTooBig(i16, usize),
    /// The file length specified inside the TFM file is invalid because it is too small.
    ///
    /// TFM files must contain at least 24 bytes of data: the 16-bit file size and the 11
    /// 16-bit numbers in [SubFileSizes].
    ///
    /// Knuth's TFToPL doesn't handle this case explicitly.
    /// When this buggy input is provided, a [Error::SubFileSizeIsNegative] error is thrown.
    InternalFileLengthIsTooSmall(i16, usize),
    /// One of the sub file sizes is negative.
    SubFileSizeIsNegative(SubFileSizes),
    /// The header length is too small (either 0 or 1).
    HeaderLengthIsTooSmall(i16),
    /// The character range is invalid.
    ///
    /// This means either that the lower bound of the range (the smallest character)
    /// is not smaller than the upper bound of the range (the largest character),
    /// or the upper bound is bigger than 255.
    InvalidCharacterRange(i16, i16),
    /// The character dimension sub-files are incomplete.
    ///
    /// This means the sub-file for either widths, heights, depths or italic corrections is empty.
    IncompleteSubFiles(SubFileSizes),
    /// There are more than 256 extensible characters.
    TooManyExtensibleCharacters(i16),
    /// The sub-file sizes are inconsistent.
    InconsistentSubFileSizes(i16, SubFileSizes),
}

impl Error {
    /// Returns the error message the TFtoPL program prints for this kind of error.
    pub fn tf_to_pl_message(&self) -> String {
        match self {
            Error::FileHasOneByte(0..=127) => "The input file is only one byte long!".into(),
            Error::InternalFileLengthIsZero => {
                "The file claims to have length zero, but that's impossible!".into()
            }
            Error::FileHasOneByte(128..=255)
            | Error::InternalFileLengthIsNegative(_)
            | Error::FileIsEmpty => {
                // See documentation on [Error::FileIsEmpty] for why we return this string in the case
                // when the file is empty. While two error messages are possible, this is the one I
                // observed on my machine today.
                "The first byte of the input file exceeds 127!".into()
            }
            Error::InternalFileLengthIsTooBig(_, _) => {
                "The file has fewer bytes than it claims!".into()
            }
            Error::InternalFileLengthIsTooSmall(_, _) | Error::SubFileSizeIsNegative(_) => {
                "One of the subfile sizes is negative!".into()
            }
            Error::HeaderLengthIsTooSmall(lh) => format!["The header length is only {lh}!"],
            Error::InvalidCharacterRange(l, u) => {
                format!("The character code range {l}..{u} is illegal!")
            }
            Error::IncompleteSubFiles(_) => "Incomplete subfiles for character dimensions!".into(),
            Error::TooManyExtensibleCharacters(n) => format!["There are {n} extensible recipes!"],
            Error::InconsistentSubFileSizes(_, _) => {
                "Subfile sizes don't add up to the stated total!".into()
            }
        }
    }

    /// Returns the section in Knuth's TFtoPL (version 2014) in which this error occurs.
    pub fn tf_to_pl_section(&self) -> usize {
        match self {
            Error::FileIsEmpty
            | Error::FileHasOneByte(_)
            | Error::InternalFileLengthIsZero
            | Error::InternalFileLengthIsNegative(_)
            | Error::InternalFileLengthIsTooBig(_, _) => 20,
            Error::InternalFileLengthIsTooSmall(_, _)
            | Error::SubFileSizeIsNegative(_)
            | Error::HeaderLengthIsTooSmall(_)
            | Error::InvalidCharacterRange(_, _)
            | Error::IncompleteSubFiles(_)
            | Error::TooManyExtensibleCharacters(_)
            | Error::InconsistentSubFileSizes(_, _) => 21,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Warning {
    /// The file length specified inside the TFM file is smaller than the actual file size.
    ///
    /// Additional data after the file length is ignored.
    InternalFileLengthIsSmall(i16, usize),
}

impl Warning {
    /// Returns the warning message the TFtoPL program prints for this kind of error.
    pub fn tf_to_pl_message(&self) -> String {
        match self {
            Warning::InternalFileLengthIsSmall(_, _) => {
                "There's some extra junk at the end of the TFM file,\nbut I'll proceed as if it weren't there.".into()
            },
        }
    }

    /// Returns the section in Knuth's TFtoPL (version 2014) in which this warning occurs.
    pub fn tf_to_pl_section(&self) -> usize {
        match self {
            Warning::InternalFileLengthIsSmall(_, _) => 20,
        }
    }
}

/// Deserialize a TeX font metric (.tfm) file.
pub fn deserialize(b: &[u8]) -> Result<(Font, Vec<Warning>), Error> {
    let mut warnings: Vec<Warning> = vec![];
    let actual_file_length = b.len();
    match actual_file_length {
        0 => return Err(Error::FileIsEmpty),
        1 => return Err(Error::FileHasOneByte(b[0])),
        _ => (),
    };
    let lf = i16::deserialize(b);
    match lf {
        ..=-1 => return Err(Error::InternalFileLengthIsNegative(lf)),
        0 => return Err(Error::InternalFileLengthIsZero),
        1.. => {
            let claimed_file_length = (lf as usize) * 4;
            match actual_file_length.cmp(&claimed_file_length) {
                std::cmp::Ordering::Less => {
                    return Err(Error::InternalFileLengthIsTooBig(lf, actual_file_length))
                }
                std::cmp::Ordering::Equal => (),
                std::cmp::Ordering::Greater => {
                    warnings.push(Warning::InternalFileLengthIsSmall(lf, actual_file_length))
                }
            }
            if lf < 3 {
                return Err(Error::InternalFileLengthIsTooSmall(lf, actual_file_length));
            }
        }
    }
    let sub_file_sizes = SubFileSizes::deserialize(&b[2..]);
    sub_file_sizes.validate(lf)?;
    #[rustfmt::skip]
    let [
        raw_header,
        raw_char_infos, 
        raw_widths, 
        raw_heights, 
        raw_depths, 
        raw_italic_corrections,
        raw_lig_kern, 
        raw_kerns,
        raw_extensible_chars, 
        raw_params,
    ] = sub_file_sizes.partition(&b[24..]);

    let font = Font {
        header: Deserialize::deserialize(raw_header),
        smallest_char_code: if sub_file_sizes.bc <= sub_file_sizes.ec {
            Char(sub_file_sizes.bc.try_into().unwrap()) // TODO: can't this panic?
        } else {
            Char(0)
        },
        char_infos: Deserialize::deserialize(raw_char_infos),
        widths: Deserialize::deserialize(raw_widths),
        heights: Deserialize::deserialize(raw_heights),
        depths: Deserialize::deserialize(raw_depths),
        italic_corrections: Deserialize::deserialize(raw_italic_corrections),
        lig_kern_commands: Deserialize::deserialize(raw_lig_kern),
        kern: Deserialize::deserialize(raw_kerns),
        extensible_chars: Deserialize::deserialize(raw_extensible_chars),
        params: Deserialize::deserialize(raw_params),
    };
    Ok((font, warnings))
}

trait Deserialize: Sized {
    fn deserialize(b: &[u8]) -> Self;
}

/// Implementations of this trait consume a fixed number of bytes when deserializing.
trait DeserializeFixed: Deserialize {
    const NUM_BYTES: usize;
}

impl Deserialize for i16 {
    #[inline]
    fn deserialize(b: &[u8]) -> Self {
        ((b[0] as u16 * 256) + (b[1] as u16)) as i16
    }
}

impl DeserializeFixed for i16 {
    const NUM_BYTES: usize = 2;
}

impl Deserialize for u32 {
    #[inline]
    fn deserialize(b: &[u8]) -> Self {
        ((b[0] as u32) << 24) + ((b[1] as u32) << 16) + ((b[2] as u32) << 8) + (b[3] as u32)
    }
}

impl DeserializeFixed for u32 {
    const NUM_BYTES: usize = 4;
}

impl Deserialize for Number {
    #[inline]
    fn deserialize(b: &[u8]) -> Self {
        Number(u32::deserialize(b) as i32)
    }
}

impl DeserializeFixed for Number {
    const NUM_BYTES: usize = 4;
}

impl Deserialize for bool {
    fn deserialize(b: &[u8]) -> Self {
        b[0] > 127
    }
}

impl DeserializeFixed for bool {
    const NUM_BYTES: usize = 1;
}

#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub struct SubFileSizes {
    /// Length of the header data, in words.
    pub lh: i16,
    /// Smallest character code in the font.
    pub bc: i16,
    /// Largest character code in the font.
    pub ec: i16,
    /// Number of words in the width table.
    pub nw: i16,
    /// Number of words in the height table.
    pub nh: i16,
    /// Number of words in the depth table.
    pub nd: i16,
    /// Number of words in the italic correction table.
    pub ni: i16,
    /// Number of words in the lig/kern table.
    pub nl: i16,
    /// Number of words in the kern table.
    pub nk: i16,
    /// Number of words in the extensible character table.
    pub ne: i16,
    /// Number of font parameter words.
    pub np: i16,
}

impl Deserialize for SubFileSizes {
    fn deserialize(b: &[u8]) -> Self {
        Self {
            lh: i16::deserialize(&b[0..2]),
            bc: i16::deserialize(&b[2..4]),
            ec: i16::deserialize(&b[4..6]),
            nw: i16::deserialize(&b[6..8]),
            nh: i16::deserialize(&b[8..10]),
            nd: i16::deserialize(&b[10..12]),
            ni: i16::deserialize(&b[12..14]),
            nl: i16::deserialize(&b[14..16]),
            nk: i16::deserialize(&b[16..18]),
            ne: i16::deserialize(&b[18..20]),
            np: i16::deserialize(&b[20..22]),
        }
    }
}

impl SubFileSizes {
    fn validate(&self, lf: i16) -> Result<(), Error> {
        if self.lh < 0
            || self.bc < 0
            || self.ec < 0
            || self.nw < 0
            || self.nh < 0
            || self.nd < 0
            || self.ni < 0
            || self.nl < 0
            || self.nk < 0
            || self.ne < 0
            || self.np < 0
        {
            return Err(Error::SubFileSizeIsNegative(self.clone()));
        }
        if self.lh < 2 {
            return Err(Error::HeaderLengthIsTooSmall(self.lh));
        }
        if self.ec > 255 || self.bc > self.ec + 1 {
            return Err(Error::InvalidCharacterRange(self.bc, self.ec));
        }
        if self.nw == 0 || self.nh == 0 || self.nd == 0 || self.ni == 0 {
            return Err(Error::IncompleteSubFiles(self.clone()));
        }
        if self.ne > 255 {
            return Err(Error::TooManyExtensibleCharacters(self.ne));
        }
        if lf
            != 6 + self.lh
                + (self.ec - self.bc + 1)
                + self.nw
                + self.nh
                + self.nd
                + self.ni
                + self.nl
                + self.nk
                + self.ne
                + self.np
        {
            return Err(Error::InconsistentSubFileSizes(lf, self.clone()));
        }
        Ok(())
    }

    fn partition<'a>(&self, mut b: &'a [u8]) -> [&'a [u8]; 10] {
        let lens = [
            self.lh,
            self.ec - self.bc + 1,
            self.nw,
            self.nh,
            self.nd,
            self.ni,
            self.nl,
            self.nk,
            self.ne,
            self.np,
        ];
        let mut r: [&[u8]; 10] = [&[0_u8; 0]; 10];
        for i in 0..10 {
            let len = (lens[i] as usize) * 4;
            r[i] = &b[..len];
            b = &b[len..];
        }
        r
    }
}

impl<const N: usize> Deserialize for StackString<N> {
    fn deserialize(b: &[u8]) -> Self {
        let max_len: u8 = N.try_into().unwrap_or(u8::MAX);
        let len = if b[0] > max_len { max_len } else { b[0] };
        let mut data = [0_u8; N];
        for (i, c) in b[1..=(len as usize)].iter().enumerate() {
            // The following transformation implements TFtoPL.2014.52.
            data[i] = match *c as char {
                '(' | ')' => '/'.try_into().unwrap(), // TODO: warnings
                'a'..='z' => c.to_ascii_uppercase(),
                ' '..='~' => *c,
                _ => '?'.try_into().unwrap(),
            };
        }
        Self { len, data }
    }
}

impl<const N: usize> DeserializeFixed for StackString<N> {
    const NUM_BYTES: usize = N + 1;
}

impl Deserialize for Header {
    fn deserialize(b: &[u8]) -> Self {
        Self {
            checksum: Deserialize::deserialize(b),
            design_size: Deserialize::deserialize(&b[4..]),
            character_coding_scheme: Deserialize::deserialize(b.get(4 + 4..).unwrap_or(&[0; 0])),
            font_family: Deserialize::deserialize(b.get(4 + 4 + 40..).unwrap_or(&[0; 0])),
            seven_bit_safe: Deserialize::deserialize(b.get(4 + 4 + 40 + 20..).unwrap_or(&[0; 0])),
            face: Deserialize::deserialize(b.get(4 + 4 + 40 + 20 + 3..).unwrap_or(&[0; 0])),
            additional_data: Deserialize::deserialize(
                b.get(4 + 4 + 40 + 20 + 4..).unwrap_or(&[0; 0]),
            ),
        }
    }
}

impl Deserialize for Option<Face> {
    fn deserialize(b: &[u8]) -> Self {
        b.first().map(|b| (*b).into())
    }
}

impl<T: DeserializeFixed> Deserialize for Option<T> {
    fn deserialize(b: &[u8]) -> Self {
        if b.len() < T::NUM_BYTES {
            None
        } else {
            Some(T::deserialize(b))
        }
    }
}

impl<T: DeserializeFixed> Deserialize for Vec<T> {
    fn deserialize(mut b: &[u8]) -> Self {
        let mut r: Self = Default::default();
        while !b.is_empty() {
            r.push(T::deserialize(b));
            b = &b[T::NUM_BYTES..]
        }
        r
    }
}

impl Deserialize for CharInfo {
    fn deserialize(b: &[u8]) -> Self {
        CharInfo {
            width_index: b[0],
            height_index: b[1] / (1 << 4),
            depth_index: b[1] % (1 << 4),
            italic_index: b[2] / (1 << 2),
            tag: match b[2] % (1 << 2) {
                0 => CharTag::None,
                1 => CharTag::Ligature(b[3]),
                2 => CharTag::List(b[3]),
                _ => CharTag::Extension(b[3]),
            },
        }
    }
}

impl DeserializeFixed for CharInfo {
    const NUM_BYTES: usize = 4;
}

impl Deserialize for ligkern::lang::Instruction {
    fn deserialize(b: &[u8]) -> Self {
        let (skip_byte, right_char, op_byte, remainder) = (b[0], b[1], b[2], b[3]);
        ligkern::lang::Instruction {
            next_instruction: if skip_byte < 128 {
                Some(skip_byte)
            } else {
                None
            },
            right_char: Char(right_char),
            operation: if op_byte < 128 {
                // TFtoPL.2014.77
                let delete_next_char = (op_byte % 2) == 0;
                let op_byte = op_byte / 2;
                let delete_current_char = (op_byte % 2) == 0;
                let skip = op_byte / 2;
                use ligkern::lang::PostLigOperation::*;
                ligkern::lang::Operation::Ligature {
                    char_to_insert: Char(remainder),
                    post_lig_operation: match (delete_current_char, delete_next_char, skip) {
                        (false, false, 0) => RetainBothMoveNowhere,
                        (false, false, 1) => RetainBothMoveToInserted,
                        (false, false, 2) => RetainBothMoveToRight,
                        (false, true, 0) => RetainLeftMoveNowhere,
                        (false, true, 1) => RetainLeftMoveToInserted,
                        (true, false, 0) => RetainRightMoveToInserted,
                        (true, false, 1) => RetainRightMoveToRight,
                        (true, true, 0) => RetainNeitherMoveToInserted,
                        _ => {
                            // TODO: issue a warning
                            RetainNeitherMoveToInserted
                        }
                    },
                }
            } else {
                ligkern::lang::Operation::Kern(Number(
                    256 * (op_byte as i32 - 128) + remainder as i32,
                ))
            },
        }
    }
}

impl DeserializeFixed for ligkern::lang::Instruction {
    const NUM_BYTES: usize = 4;
}

impl Deserialize for ExtensibleRecipe {
    fn deserialize(b: &[u8]) -> Self {
        ExtensibleRecipe {
            top: b[0],
            middle: b[1],
            bottom: b[2],
            rep: b[3],
        }
    }
}

impl DeserializeFixed for ExtensibleRecipe {
    const NUM_BYTES: usize = 4;
}

impl Deserialize for Params {
    fn deserialize(b: &[u8]) -> Self {
        Self(Vec::<Number>::deserialize(b))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! deserialize_tests {
        ( $( ($name: ident, $input: expr, $want: expr), )+ ) => {
            $(
                mod $name {
                    #[test]
                    fn unit_test() {
                        let input = $input;
                        let want = $want;
                        let got = deserialize(&input);
                        assert_eq!(got, want);
                    }

                    use super::*;
                    #[test]
                    fn tftopl_error_message_test() {
                        let input = $input;
                        let want: Result<(Font, Vec<Warning>), Error> = $want;
                        let want = match want {
                            Ok(_) => return,
                            Err(err) => err,
                        };
                        let name = std::module_path!();
                        tftopl_error_test(&input, want, name);
                    }
                }
            )+
        };
    }

    deserialize_tests!(
        (empty_file, [], Err(Error::FileIsEmpty)),
        (single_byte_1, [2], Err(Error::FileHasOneByte(2))),
        (single_byte_2, [255], Err(Error::FileHasOneByte(255))),
        (
            internal_file_length_is_negative,
            [255, 0],
            Err(Error::InternalFileLengthIsNegative(-256))
        ),
        (
            internal_file_length_is_zero,
            [0, 0, 1, 1],
            Err(Error::InternalFileLengthIsZero)
        ),
        (
            internal_file_length_is_too_big,
            [0, 2, 1, 1],
            Err(Error::InternalFileLengthIsTooBig(2, 4))
        ),
        (
            internal_file_length_is_too_small,
            extend(&[0, 2, 255, 0], 24),
            Err(Error::InternalFileLengthIsTooSmall(2, 24))
        ),
        (
            sub_file_size_too_small,
            extend(&[0, 6, 255, 0], 24),
            Err(Error::SubFileSizeIsNegative(SubFileSizes {
                lh: -256,
                ..Default::default()
            }))
        ),
        (
            header_length_too_small_0,
            extend(&[0, 6, 0, 0], 24),
            Err(Error::HeaderLengthIsTooSmall(0))
        ),
        (
            header_length_too_small_1,
            extend(&[0, 6, 0, 1], 24),
            Err(Error::HeaderLengthIsTooSmall(1))
        ),
        (
            invalid_character_range_1,
            extend(&[0, 6, 0, 2, 0, 2, 0, 0], 24),
            Err(Error::InvalidCharacterRange(2, 0))
        ),
        (
            invalid_character_range_2,
            extend(&[0, 6, 0, 2, 0, 2, 1, 0], 24),
            Err(Error::InvalidCharacterRange(2, 256))
        ),
        (
            incomplete_sub_files,
            extend(
                &[
                    /* lf */ 0, 6, /* lh */ 0, 2, /* bc */ 0, 1, /* ec */ 0, 2,
                    /* nw */ 0, 3, /* nh */ 0, 4, /* nd */ 0, 5, /* ni */ 0, 0,
                ],
                24
            ),
            Err(Error::IncompleteSubFiles(SubFileSizes {
                lh: 2,
                bc: 1,
                ec: 2,
                nw: 3,
                nh: 4,
                nd: 5,
                ..Default::default()
            }))
        ),
        (
            too_many_extensible_characters,
            extend(
                &[
                    /* lf */ 0, 6, /* lh */ 0, 2, /* bc */ 0, 1, /* ec */ 0, 2,
                    /* nw */ 0, 3, /* nh */ 0, 4, /* nd */ 0, 5, /* ni */ 0, 6,
                    /* nl */ 0, 0, /* nk */ 0, 0, /* ne */ 1, 1, /* np */ 0, 0,
                ],
                24
            ),
            Err(Error::TooManyExtensibleCharacters(257))
        ),
        (
            inconsistent_sub_file_sizes,
            extend(
                &[
                    /* lf */ 0, 6, /* lh */ 0, 2, /* bc */ 0, 3, /* ec */ 0, 4,
                    /* nw */ 0, 5, /* nh */ 0, 6, /* nd */ 0, 7, /* ni */ 0, 8,
                    /* nl */ 0, 9, /* nk */ 0, 10, /* ne */ 0, 11, /* np */ 0,
                    12,
                ],
                24
            ),
            Err(Error::InconsistentSubFileSizes(
                6,
                SubFileSizes {
                    lh: 2,
                    bc: 3,
                    ec: 4,
                    nw: 5,
                    nh: 6,
                    nd: 7,
                    ni: 8,
                    nl: 9,
                    nk: 10,
                    ne: 11,
                    np: 12,
                }
            ))
        ),
        (
            minimal_header,
            build_from_header(&[/* checksum */ 0, 0, 0, 7, /* design_size */ 0, 0, 0, 11,]),
            Ok((
                Font {
                    header: Header {
                        checksum: 7,
                        design_size: Number(11),
                        character_coding_scheme: None,
                        font_family: None,
                        seven_bit_safe: None,
                        face: None,
                        additional_data: vec![],
                    },
                    smallest_char_code: Char(0),
                    char_infos: vec![],
                    widths: vec![Number::ZERO],
                    heights: vec![Number::ZERO],
                    depths: vec![Number::ZERO],
                    italic_corrections: vec![Number::ZERO],
                    lig_kern_commands: vec![],
                    kern: vec![],
                    extensible_chars: vec![],
                    params: Default::default(),
                },
                vec![]
            ))
        ),
        (
            full_header,
            build_from_header(&[
                /* checksum */ 0, 0, 0, 7, /* design_size */ 0, 0, 0, 11,
                /* character_coding_scheme */ 3, 65, 66, 67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                /* font_family */ 3, 68, 69, 70, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, /* seven_bit_safe */ 200, 0, 0, /* face */ 9,
                /* additional_data */ 0, 0, 0, 13,
            ]),
            Ok((
                Font {
                    header: Header {
                        checksum: 7,
                        design_size: Number(11),
                        character_coding_scheme: Some("ABC".into()),
                        font_family: Some("DEF".into()),
                        seven_bit_safe: Some(true),
                        face: Some(Face::Valid(
                            FaceWeight::Bold,
                            FaceSlope::Italic,
                            FaceExpansion::Condensed
                        )),
                        additional_data: vec![13],
                    },
                    smallest_char_code: Char(0),
                    char_infos: vec![],
                    widths: vec![Number::ZERO],
                    heights: vec![Number::ZERO],
                    depths: vec![Number::ZERO],
                    italic_corrections: vec![Number::ZERO],
                    lig_kern_commands: vec![],
                    kern: vec![],
                    extensible_chars: vec![],
                    params: Default::default(),
                },
                vec![]
            ))
        ),
        (
            char_infos,
            extend(
                &[
                    /* lf */ 0, 14, /* lh */ 0, 2, /* bc */ 0, 70, /* ec */ 0,
                    71, /* nw */ 0, 1, /* nh */ 0, 1, /* nd */ 0, 1,
                    /* ni */ 0, 1, /* nl */ 0, 0, /* nk */ 0, 0, /* ne */ 0, 0,
                    /* np */ 0, 0, /* header.checksum */ 0, 0, 0, 0,
                    /* header.design_size */ 0, 0, 0, 0, /* char_infos */ 13, 35, 16, 0,
                    0, 0, 1, 23,
                ],
                14 * 4
            ),
            Ok((
                Font {
                    header: Default::default(),
                    smallest_char_code: Char(70),
                    char_infos: vec![
                        CharInfo {
                            width_index: 13,
                            height_index: 2,
                            depth_index: 3,
                            italic_index: 4,
                            tag: CharTag::None,
                        },
                        CharInfo {
                            width_index: 0,
                            height_index: 0,
                            depth_index: 0,
                            italic_index: 0,
                            tag: CharTag::Ligature(23),
                        },
                    ],
                    widths: vec![Number::ZERO],
                    heights: vec![Number::ZERO],
                    depths: vec![Number::ZERO],
                    italic_corrections: vec![Number::ZERO],
                    lig_kern_commands: vec![],
                    kern: vec![],
                    extensible_chars: vec![],
                    params: Default::default(),
                },
                vec![]
            ))
        ),
        (
            widths_heights_depths_italic_corrections_kerns,
            extend(
                &[
                    /* lf */ 0, 17, /* lh */ 0, 2, /* bc */ 0, 1, /* ec */ 0,
                    0, /* nw */ 0, 2, /* nh */ 0, 2, /* nd */ 0, 2, /* ni */ 0,
                    2, /* nl */ 0, 0, /* nk */ 0, 1, /* ne */ 0, 0, /* np */ 0,
                    0, /* header.checksum */ 0, 0, 0, 0, /* header.design_size */ 0, 0,
                    0, 0, /* widths */ 0, 0, 0, 0, 0, 0, 0, 23, /* heights */ 0, 0, 0, 0,
                    0, 0, 0, 29, /* depths */ 0, 0, 0, 0, 0, 0, 0, 31,
                    /* italic_corrections */ 0, 0, 0, 0, 0, 0, 0,
                    37, /* lig_kern_commands */
                    /* kerns */ 0, 0, 0, 37
                ],
                17 * 4
            ),
            Ok((
                Font {
                    header: Default::default(),
                    smallest_char_code: Char(0),
                    char_infos: vec![],
                    widths: vec![Number::ZERO, Number(23)],
                    heights: vec![Number::ZERO, Number(29)],
                    depths: vec![Number::ZERO, Number(31)],
                    italic_corrections: vec![Number::ZERO, Number(37)],
                    lig_kern_commands: vec![],
                    kern: vec![Number(37)],
                    extensible_chars: vec![],
                    params: Default::default(),
                },
                vec![]
            ))
        ),
        (
            lig_kern_commands,
            extend(
                &[
                    /* lf */ 0, 17, /* lh */ 0, 2, /* bc */ 0, 1, /* ec */ 0,
                    0, /* nw */ 0, 1, /* nh */ 0, 1, /* nd */ 0, 1, /* ni */ 0,
                    1, /* nl */ 0, 5, /* nk */ 0, 0, /* ne */ 0, 0, /* np */ 0,
                    0, /* header.checksum */ 0, 0, 0, 0, /* header.design_size */ 0, 0,
                    0, 0, /* widths */ 0, 0, 0, 0, /* heights */ 0, 0, 0, 0,
                    /* depths */ 0, 0, 0, 0, /* italic_corrections */ 0, 0, 0, 0,
                    /* lig_kern_commands */
                    3, 5, 130, 13, 3, 6, 0, 17, 3, 7, 11, 19, 200, 8, 5, 23, 200, 8, 48, 23,
                ],
                16 * 4
            ),
            Ok((
                Font {
                    header: Default::default(),
                    smallest_char_code: Char(0),
                    char_infos: vec![],
                    widths: vec![Number::ZERO],
                    heights: vec![Number::ZERO],
                    depths: vec![Number::ZERO],
                    italic_corrections: vec![Number::ZERO],
                    lig_kern_commands: vec![
                        ligkern::lang::Instruction {
                            next_instruction: Some(3),
                            right_char: Char(5),
                            operation: ligkern::lang::Operation::Kern(Number(256 * 2 + 13))
                        },
                        ligkern::lang::Instruction {
                            next_instruction: Some(3),
                            right_char: Char(6),
                            operation: ligkern::lang::Operation::Ligature {
                                char_to_insert: Char(17),
                                post_lig_operation:
                                    ligkern::lang::PostLigOperation::RetainNeitherMoveToInserted,
                            },
                        },
                        ligkern::lang::Instruction {
                            next_instruction: Some(3),
                            right_char: Char(7),
                            operation: ligkern::lang::Operation::Ligature {
                                char_to_insert: Char(19),
                                post_lig_operation:
                                    ligkern::lang::PostLigOperation::RetainBothMoveToRight,
                            },
                        },
                        ligkern::lang::Instruction {
                            next_instruction: None,
                            right_char: Char(8),
                            operation: ligkern::lang::Operation::Ligature {
                                char_to_insert: Char(23),
                                post_lig_operation:
                                    ligkern::lang::PostLigOperation::RetainRightMoveToRight,
                            },
                        },
                        ligkern::lang::Instruction {
                            next_instruction: None,
                            right_char: Char(8),
                            operation: ligkern::lang::Operation::Ligature {
                                char_to_insert: Char(23),
                                post_lig_operation:
                                    ligkern::lang::PostLigOperation::RetainNeitherMoveToInserted,
                            },
                        },
                    ],
                    kern: vec![],
                    extensible_chars: vec![],
                    params: Default::default(),
                },
                vec![]
            ))
        ),
        (
            extensible_chars,
            extend(
                &[
                    /* lf */ 0, 13, /* lh */ 0, 2, /* bc */ 0, 1, /* ec */ 0,
                    0, /* nw */ 0, 1, /* nh */ 0, 1, /* nd */ 0, 1, /* ni */ 0,
                    1, /* nl */ 0, 0, /* nk */ 0, 0, /* ne */ 0, 1, /* np */ 0,
                    0, /* header.checksum */ 0, 0, 0, 0, /* header.design_size */ 0, 0,
                    0, 0, /* widths */ 0, 0, 0, 0, /* heights */ 0, 0, 0, 0,
                    /* depths */ 0, 0, 0, 0, /* italic_corrections */ 0, 0, 0,
                    0, /* lig_kern_commands */
                    /* kerns */ /* extensible_chars */ 17, 19, 23, 27
                ],
                13 * 4
            ),
            Ok((
                Font {
                    header: Default::default(),
                    smallest_char_code: Char(0),
                    char_infos: vec![],
                    widths: vec![Number::ZERO],
                    heights: vec![Number::ZERO],
                    depths: vec![Number::ZERO],
                    italic_corrections: vec![Number::ZERO],
                    lig_kern_commands: vec![],
                    kern: vec![],
                    extensible_chars: vec![ExtensibleRecipe {
                        top: 17,
                        middle: 19,
                        bottom: 23,
                        rep: 27,
                    }],
                    params: Default::default(),
                },
                vec![]
            ))
        ),
        (
            params,
            extend(
                &[
                    /* lf */ 0, 22, /* lh */ 0, 2, /* bc */ 0, 1, /* ec */ 0,
                    0, /* nw */ 0, 1, /* nh */ 0, 1, /* nd */ 0, 1, /* ni */ 0,
                    1, /* nl */ 0, 0, /* nk */ 0, 0, /* ne */ 0, 0, /* np */ 0,
                    10, /* header.checksum */ 0, 0, 0, 0, /* header.design_size */ 0, 0,
                    0, 0, /* widths */ 0, 0, 0, 0, /* heights */ 0, 0, 0, 0,
                    /* depths */ 0, 0, 0, 0, /* italic_corrections */ 0, 0, 0,
                    0, /* lig_kern_commands */
                    /* kerns */ /* extensible_chars */ /* params */
                    0, 0, 0, 11, 0, 0, 0, 13, 0, 0, 0, 17, 0, 0, 0, 19, 0, 0, 0, 23, 0, 0, 0, 29, 0,
                    0, 0, 31, 0, 0, 0, 37, 0, 0, 0, 41, 0, 0, 0, 43,
                ],
                22 * 4
            ),
            Ok((
                Font {
                    header: Default::default(),
                    smallest_char_code: Char(0),
                    char_infos: vec![],
                    widths: vec![Number::ZERO],
                    heights: vec![Number::ZERO],
                    depths: vec![Number::ZERO],
                    italic_corrections: vec![Number::ZERO],
                    lig_kern_commands: vec![],
                    kern: vec![],
                    extensible_chars: vec![],
                    params: Params(vec![
                        Number(11),
                        Number(13),
                        Number(17),
                        Number(19),
                        Number(23),
                        Number(29),
                        Number(31),
                        Number(37),
                        Number(41),
                        Number(43),
                    ]),
                },
                vec![]
            ))
        ),
    );

    fn extend(input: &[u8], size: usize) -> Vec<u8> {
        let mut v: Vec<u8> = input.into();
        for _ in input.len()..size {
            v.push(0)
        }
        v
    }

    fn build_from_header(header: &[u8]) -> Vec<u8> {
        assert_eq!(header.len() % 4, 0);
        let num_words: i16 = (header.len() / 4).try_into().unwrap();
        let lf: u8 = (6 + num_words + 4).try_into().unwrap();
        let lh: u8 = num_words.try_into().unwrap();
        let mut v: Vec<u8> = vec![
            /* lf */ 0, lf, /* lh */ 0, lh, /* bc */ 0, 1, /* ec */ 0, 0,
            /* nw */ 0, 1, /* nh */ 0, 1, /* nd */ 0, 1, /* ni */ 0, 1,
            /* nl */ 0, 0, /* nk */ 0, 0, /* ne */ 0, 0, /* np */ 0, 0,
        ];
        v.extend(header);
        v.extend(&[0_u8; 16]); // the widths etc.
        v
    }

    fn tftopl_error_test(input: &[u8], want: Error, name: &str) {
        use std::process::Command;

        let name: String = name
            .chars()
            .map(|c| if c.is_alphanumeric() { c } else { '_' })
            .collect();
        let mut input_path = std::env::temp_dir();
        input_path.push(name);
        input_path.set_extension("tfm");
        std::fs::write(&input_path, input).expect("Unable to write file");

        let has_tftopl = Command::new("which")
            .arg("tftopl")
            .spawn()
            .expect("`which tftopl` command failed to start")
            .wait()
            .expect("failed to run `which tftopl`")
            .success();
        if !has_tftopl {
            return;
        }

        let output = Command::new("tftopl")
            .arg(&input_path)
            .output()
            .expect("tftopl command failed to start");
        let stderr = String::from_utf8_lossy(&output.stderr);
        let want_message = want.tf_to_pl_message();
        assert!(
            stderr.contains(&want_message),
            "got: {}, want: {}",
            stderr,
            want_message
        );
    }

    #[test]
    fn examples_can_be_deserialized() {
        let tfm_files = crate::examples::tfm_files();
        for file in tfm_files {
            println!("{}", file.path);
            deserialize(file.data).unwrap();
        }
    }
}
