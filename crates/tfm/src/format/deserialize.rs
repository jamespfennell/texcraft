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
    /// 16-bit numbers in the sub file sizes section.
    ///
    /// Knuth's TFToPL doesn't handle this case explicitly.
    /// When this buggy input is provided,
    ///     a [Error::SubFileSizeIsNegative] error is thrown in some cases,
    ///     and a [Error::HeaderLengthIsTooSmall] in other cases.
    /// Although the result is deterministic, there seems to be undefined behavior here
    ///     because it seems to depend on the value of uninitialized memory.
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
    InconsistentSubFileSizes(SubFileSizes),
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
            Error::InconsistentSubFileSizes(_) => {
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
            | Error::InconsistentSubFileSizes(_) => 21,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Warning {
    /// The file length specified inside the TFM file is smaller than the actual file size.
    ///
    /// Additional data after the file length is ignored.
    ///
    /// The first element is the number of words specified in the TFM header.
    /// The second element is the number of bytes in the file.
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
pub(super) fn deserialize(b: &[u8]) -> (Result<File, Error>, Vec<Warning>) {
    match RawFile::deserialize(b) {
        (Ok(raw_file), warnings) => (Ok(from_raw_file(&raw_file)), warnings),
        (Err(err), warnings) => (Err(err), warnings),
    }
}

pub(super) fn from_raw_file(raw_file: &RawFile) -> File {
    let mut char_infos = HashMap::<Char, CharInfo>::new();
    let char_infos_array: Vec<Option<CharInfo>> = deserialize_array(raw_file.char_infos);
    let mut c = raw_file.begin_char;
    for char_info in char_infos_array {
        if let Some(char_info) = char_info {
            char_infos.insert(c, char_info);
        }
        c = match c.0.checked_add(1) {
            None => break,
            Some(c) => Char(c),
        }
    }
    File {
        header: Header::deserialize(raw_file.header),
        char_infos,
        widths: deserialize_array(raw_file.widths),
        heights: deserialize_array(raw_file.heights),
        depths: deserialize_array(raw_file.depths),
        italic_corrections: deserialize_array(raw_file.italic_corrections),
        lig_kern_boundary_char: deserialize_boundary_char(raw_file.lig_kern_instructions),
        lig_kern_boundary_char_entrypoint: deserialize_boundary_char_entrypoint(
            raw_file.lig_kern_instructions,
        ),
        lig_kern_instructions: deserialize_array(raw_file.lig_kern_instructions),
        kerns: deserialize_array(raw_file.kerns),
        extensible_chars: deserialize_array(raw_file.extensible_recipes),
        params: Params(deserialize_array(raw_file.params)),
    }
}

/// Raw .tfm file.
pub struct RawFile<'a> {
    pub sub_file_sizes: SubFileSizes,
    pub raw_sub_file_sizes: &'a [u8],
    pub header: &'a [u8],
    pub begin_char: Char,
    pub end_char: Char,
    pub char_infos: &'a [u8],
    pub widths: &'a [u8],
    pub heights: &'a [u8],
    pub depths: &'a [u8],
    pub italic_corrections: &'a [u8],
    pub lig_kern_instructions: &'a [u8],
    pub kerns: &'a [u8],
    pub extensible_recipes: &'a [u8],
    pub params: &'a [u8],
}

/// Sub-file sizes in a .tfm file.
#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub struct SubFileSizes {
    /// Length of the file, in words.
    pub lf: i16,
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

impl<'a> RawFile<'a> {
    pub fn deserialize(b: &'a [u8]) -> (Result<Self, Error>, Vec<Warning>) {
        let deserialize_i16 = |u: usize| -> i16 { i16::from_be_bytes([b[u], b[u + 1]]) };
        let mut warnings: Vec<Warning> = vec![];
        let actual_file_length = b.len();
        match actual_file_length {
            0 => return (Err(Error::FileIsEmpty), warnings),
            1 => return (Err(Error::FileHasOneByte(b[0])), warnings),
            _ => (),
        };
        let lf = deserialize_i16(0);
        match lf {
            ..=-1 => return (Err(Error::InternalFileLengthIsNegative(lf)), warnings),
            0 => return (Err(Error::InternalFileLengthIsZero), warnings),
            1.. => {
                let claimed_file_length = (lf as usize) * 4;
                match actual_file_length.cmp(&claimed_file_length) {
                    std::cmp::Ordering::Less => {
                        return (
                            Err(Error::InternalFileLengthIsTooBig(lf, actual_file_length)),
                            warnings,
                        )
                    }
                    std::cmp::Ordering::Equal => (),
                    std::cmp::Ordering::Greater => {
                        warnings.push(Warning::InternalFileLengthIsSmall(lf, actual_file_length))
                    }
                }
                if lf <= 3 {
                    return (
                        Err(Error::InternalFileLengthIsTooSmall(lf, actual_file_length)),
                        // TFtoPL doesn't output a warning here.
                        // Which makes sense because the error already encompasses the warning.
                        vec![],
                    );
                }
            }
        }
        let s = SubFileSizes {
            lf,
            lh: deserialize_i16(2),
            bc: deserialize_i16(4),
            ec: deserialize_i16(6),
            nw: deserialize_i16(8),
            nh: deserialize_i16(10),
            nd: deserialize_i16(12),
            ni: deserialize_i16(14),
            nl: deserialize_i16(16),
            nk: deserialize_i16(18),
            ne: deserialize_i16(20),
            np: deserialize_i16(22),
        };

        if s.lh < 0
            || s.bc < 0
            || s.ec < 0
            || s.nw < 0
            || s.nh < 0
            || s.nd < 0
            || s.ni < 0
            || s.nl < 0
            || s.nk < 0
            || s.ne < 0
            || s.np < 0
        {
            return (Err(Error::SubFileSizeIsNegative(s.clone())), warnings);
        }
        if s.lh < 2 {
            return (Err(Error::HeaderLengthIsTooSmall(s.lh)), warnings);
        }
        let (bc, ec) = match s.bc.cmp(&s.ec.saturating_add(1)) {
            std::cmp::Ordering::Less => {
                let ec: u8 = match s.ec.try_into() {
                    Err(_) => return (Err(Error::InvalidCharacterRange(s.bc, s.ec)), warnings),
                    Ok(ec) => ec,
                };
                (
                    Char(s.bc.try_into().expect("bc<ec<=u8::MAX, so bc<=u8::MAX")),
                    Char(ec),
                )
            }
            std::cmp::Ordering::Equal => (Char(1), Char(0)),
            std::cmp::Ordering::Greater => {
                return (Err(Error::InvalidCharacterRange(s.bc, s.ec)), warnings)
            }
        };
        if s.nw == 0 || s.nh == 0 || s.nd == 0 || s.ni == 0 {
            return (Err(Error::IncompleteSubFiles(s.clone())), warnings);
        }
        if s.ne > 255 {
            return (Err(Error::TooManyExtensibleCharacters(s.ne)), warnings);
        }
        if s.lf
            != 6 + s.lh + (s.ec - s.bc + 1) + s.nw + s.nh + s.nd + s.ni + s.nl + s.nk + s.ne + s.np
        {
            return (Err(Error::InconsistentSubFileSizes(s.clone())), warnings);
        }

        let raw_file = {
            let mut b = b;
            let mut get = |u: i16| {
                let u = (u as usize) * 4;
                let a = &b[..u];
                b = &b[u..];
                a
            };
            RawFile {
                raw_sub_file_sizes: get(6),
                header: get(s.lh),
                begin_char: bc,
                end_char: ec,
                char_infos: get(s.ec - s.bc + 1),
                widths: get(s.nw),
                heights: get(s.nh),
                depths: get(s.nd),
                italic_corrections: get(s.ni),
                lig_kern_instructions: get(s.nl),
                kerns: get(s.nk),
                extensible_recipes: get(s.ne),
                params: get(s.np),
                sub_file_sizes: s,
            }
        };

        (Ok(raw_file), warnings)
    }
}

fn deserialize_string<const N: u8>(b: &[u8]) -> String {
    let len = match b.first() {
        None => return String::new(),
        Some(&tfm_len) => {
            if N - 1 < tfm_len {
                // TODO: need to return the error in TFtoPL.2014.52.1 here.
                N - 1
            } else {
                tfm_len
            }
        }
    };
    b[1..=(len as usize)].iter().map(|u| *u as char).collect()
}

impl Header {
    fn deserialize(mut b: &[u8]) -> Self {
        let checksum = u32::deserialize(b);
        b = &b[4..];
        let design_size = Number::deserialize(b);
        b = b.get(4..).unwrap_or(&[0; 0]);
        let character_coding_scheme = deserialize_string::<40>(b);
        b = b.get(40..).unwrap_or(&[0; 0]);
        let font_family = deserialize_string::<20>(b);
        b = b.get(20..).unwrap_or(&[0; 0]);
        let seven_bit_safe = b.first().map(|b| *b > 127);
        let face = b.get(3).map(|b| (*b).into());
        let b = b.get(4..).unwrap_or(&[0; 0]);
        Self {
            checksum,
            design_size,
            character_coding_scheme,
            font_family,
            seven_bit_safe,
            face,
            additional_data: deserialize_array(b),
        }
    }
}

fn deserialize_boundary_char(b: &[u8]) -> Option<Char> {
    // TFtoTPL.2014.69
    match b.get(..2) {
        None => None,
        Some(b) => {
            if b[0] == 255 {
                Some(Char(b[1]))
            } else {
                None
            }
        }
    }
}

fn deserialize_boundary_char_entrypoint(b: &[u8]) -> Option<u16> {
    // TFtoTPL.2014.69
    match b.len().checked_sub(4) {
        None => None,
        Some(r) => {
            let b = &b[r..];
            if b[0] == 255 {
                Some(u16::from_be_bytes([b[2], b[3]]))
            } else {
                None
            }
        }
    }
}

fn deserialize_array<T: Deserializable>(mut b: &[u8]) -> Vec<T> {
    let mut r = vec![];
    while !b.is_empty() {
        r.push(T::deserialize(b));
        b = &b[4..]
    }
    r
}

/// Implementations of this trait can be deserialized from a 4-byte word.
trait Deserializable: Sized {
    fn deserialize(b: &[u8]) -> Self;
}

impl Deserializable for u32 {
    #[inline]
    fn deserialize(b: &[u8]) -> Self {
        u32::from_be_bytes([b[0], b[1], b[2], b[3]])
    }
}

impl Deserializable for Number {
    #[inline]
    fn deserialize(b: &[u8]) -> Self {
        Number(u32::deserialize(b) as i32)
    }
}

impl Deserializable for Option<CharInfo> {
    fn deserialize(b: &[u8]) -> Self {
        match b[0].try_into() {
            Ok(width_index) => Some(CharInfo {
                width_index,
                height_index: b[1] / (1 << 4),
                depth_index: b[1] % (1 << 4),
                italic_index: b[2] / (1 << 2),
                tag: match b[2] % (1 << 2) {
                    0 => CharTag::None,
                    1 => CharTag::Ligature(b[3]),
                    2 => CharTag::List(Char(b[3])),
                    _ => CharTag::Extension(b[3]),
                },
            }),
            Err(_) => None,
        }
    }
}

impl Deserializable for ligkern::lang::Instruction {
    fn deserialize(b: &[u8]) -> Self {
        let (skip_byte, right_char, op_byte, remainder) = (b[0], b[1], b[2], b[3]);
        if skip_byte > 128 {
            return ligkern::lang::Instruction {
                next_instruction: None,
                right_char: Char(0),
                operation: ligkern::lang::Operation::EntrypointRedirect(
                    u16::from_be_bytes([b[2], b[3]]),
                    if skip_byte == 255 {
                        Some(Char(b[1]))
                    } else {
                        None
                    },
                ),
            };
        }
        ligkern::lang::Instruction {
            next_instruction: if skip_byte < 128 {
                Some(skip_byte)
            } else {
                None
            },
            right_char: Char(right_char),
            operation: match op_byte.checked_sub(128) {
                Some(r) => {
                    ligkern::lang::Operation::KernAtIndex(u16::from_be_bytes([r, remainder]))
                }
                None => {
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
                }
            },
        }
    }
}

impl Deserializable for ExtensibleRecipe {
    fn deserialize(b: &[u8]) -> Self {
        let char_or = |b: u8| {
            if b == 0 {
                None
            } else {
                Some(Char(b))
            }
        };
        ExtensibleRecipe {
            top: char_or(b[0]),
            middle: char_or(b[1]),
            bottom: char_or(b[2]),
            rep: Char(b[3]),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! deserialize_tests {
        ( $( ($name: ident, $input: expr, $want: expr $( , $warning: expr )? ), )+ ) => {
            $(
                mod $name {
                    use super::*;
                    #[test]
                    fn deserialize_test() {
                        let input = $input;
                        let want = $want;
                        let got = deserialize(&input);
                        let warnings = vec![ $( $warning )? ];
                        assert_eq!(got, (want, warnings));
                    }
                }
            )+
        };
    }

    macro_rules! serde_tests {
        ( $( ($name: ident, $bytes: expr, $file: expr $(,)? ), )+ ) => {
            $(
                mod $name {
                    use super::*;
                    #[test]
                    fn deserialize_test() {
                        let input = $bytes;
                        let want = $file;
                        let got = deserialize(&input);
                        assert_eq!(got, (Ok(want), vec![]));
                    }
                    #[test]
                    fn serialize_test() {
                        let input = $file;
                        let want = canonical_tfm_bytes($bytes);
                        let got = input.serialize();
                        assert_eq!(got, want);
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
            tfm_file_contains_only_sub_file_sizes,
            extend(&[0, 3, 0, 0], 12),
            Err(Error::InternalFileLengthIsTooSmall(3, 12))
        ),
        (
            sub_file_size_too_small,
            extend(&[0, 6, 255, 0], 24),
            Err(Error::SubFileSizeIsNegative(SubFileSizes {
                lf: 6,
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
                lf: 6,
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
            Err(Error::InconsistentSubFileSizes(SubFileSizes {
                lf: 6,
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
            }))
        ),
        (
            corrupt_string_in_header,
            build_from_header(&[
                /* checksum */ 0, 0, 0, 7, /* design_size */ 0, 0, 0, 11,
                /* character_coding_scheme */ 240, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65,
                65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65,
                65, 65, 65, 65, 65, 65, 65, /* font_family */ 100, 66, 66, 66, 66, 66, 66, 66,
                66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, /* seven_bit_safe */ 0, 0, 0,
                /* face */ 61,
            ]),
            Ok(File {
                header: Header {
                    checksum: 7,
                    design_size: Number(11),
                    character_coding_scheme: "A".repeat(39),
                    font_family: "B".repeat(19),
                    seven_bit_safe: Some(false),
                    face: Some(Face::Other(61)),
                    additional_data: vec![],
                },
                ..Default::default()
            },)
        ),
        (
            file_longer_than_expected,
            extend(
                &vec![
                    /* lf */ 0, 12, /* lh */ 0, 2, /* bc */ 0, 1, /* ec */ 0,
                    0, /* nw */ 0, 1, /* nh */ 0, 1, /* nd */ 0, 1, /* ni */ 0,
                    1, /* nl */ 0, 0, /* nk */ 0, 0, /* ne */ 0, 0, /* np */ 0,
                    0, /* header.checksum */ 0, 0, 0, 0, /* header.design_size */ 0, 0,
                    0, 0,
                ],
                40 * 4
            ),
            Ok(File {
                header: Default::default(),
                ..Default::default()
            }),
            Warning::InternalFileLengthIsSmall(12, 40 * 4)
        ),
    );

    serde_tests!(
        (
            minimal_header,
            build_from_header(&[/* checksum */ 0, 0, 0, 7, /* design_size */ 0, 0, 0, 11,]),
            File {
                header: Header {
                    checksum: 7,
                    design_size: Number(11),
                    character_coding_scheme: "".into(),
                    font_family: "".into(),
                    seven_bit_safe: None,
                    face: None,
                    additional_data: vec![],
                },
                ..Default::default()
            },
        ),
        (
            full_header,
            build_from_header(&[
                /* checksum */ 0, 0, 0, 7, /* design_size */ 0, 0, 0, 11,
                /* character_coding_scheme */ 3, 65, 66, 67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                /* font_family */ 3, 68, 69, 70, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, /* seven_bit_safe */ 128, 0, 0, /* face */ 9,
                /* additional_data */ 0, 0, 0, 13,
            ]),
            File {
                header: Header {
                    checksum: 7,
                    design_size: Number(11),
                    character_coding_scheme: "ABC".into(),
                    font_family: "DEF".into(),
                    seven_bit_safe: Some(true),
                    face: Some(Face::Valid(
                        FaceWeight::Bold,
                        FaceSlope::Italic,
                        FaceExpansion::Condensed
                    )),
                    additional_data: vec![13],
                },
                ..Default::default()
            },
        ),
        (
            char_infos,
            extend(
                &vec![
                    /* lf */ 0, 15, /* lh */ 0, 2, /* bc */ 0, 70, /* ec */ 0,
                    72, /* nw */ 0, 1, /* nh */ 0, 1, /* nd */ 0, 1,
                    /* ni */ 0, 1, /* nl */ 0, 0, /* nk */ 0, 0, /* ne */ 0, 0,
                    /* np */ 0, 0, /* header.checksum */ 0, 0, 0, 0,
                    /* header.design_size */ 0, 0, 0, 0, /* char_infos */ 13, 35, 16, 0,
                    0, 0, 0, 0, 1, 0, 1, 23,
                ],
                15 * 4
            ),
            File {
                header: Default::default(),
                char_infos: HashMap::from([
                    (
                        Char(70),
                        CharInfo {
                            width_index: 13.try_into().unwrap(),
                            height_index: 2,
                            depth_index: 3,
                            italic_index: 4,
                            tag: CharTag::None,
                        }
                    ),
                    (
                        Char(72),
                        CharInfo {
                            width_index: 1.try_into().unwrap(),
                            height_index: 0,
                            depth_index: 0,
                            italic_index: 0,
                            tag: CharTag::Ligature(23),
                        }
                    ),
                ]),
                ..Default::default()
            },
        ),
        (
            char_infos_large_char,
            extend(
                &vec![
                    /* lf */ 0, 13, /* lh */ 0, 2, /* bc */ 0, 255, /* ec */ 0,
                    255, /* nw */ 0, 1, /* nh */ 0, 1, /* nd */ 0, 1,
                    /* ni */ 0, 1, /* nl */ 0, 0, /* nk */ 0, 0, /* ne */ 0, 0,
                    /* np */ 0, 0, /* header.checksum */ 0, 0, 0, 0,
                    /* header.design_size */ 0, 0, 0, 0, /* char_infos */ 13, 35, 16, 0,
                ],
                13 * 4
            ),
            File {
                header: Default::default(),
                char_infos: HashMap::from([(
                    Char(255),
                    CharInfo {
                        width_index: 13.try_into().unwrap(),
                        height_index: 2,
                        depth_index: 3,
                        italic_index: 4,
                        tag: CharTag::None,
                    }
                ),]),
                ..Default::default()
            },
        ),
        (
            widths_heights_depths_italic_corrections_kerns,
            vec![
                /* lf */ 0, 17, /* lh */ 0, 2, /* bc */ 0, 1, /* ec */ 0, 0,
                /* nw */ 0, 2, /* nh */ 0, 2, /* nd */ 0, 2, /* ni */ 0, 2,
                /* nl */ 0, 0, /* nk */ 0, 1, /* ne */ 0, 0, /* np */ 0, 0,
                /* header.checksum */ 0, 0, 0, 0, /* header.design_size */ 0, 0, 0, 0,
                /* widths */ 0, 0, 0, 0, 0, 0, 0, 23, /* heights */ 0, 0, 0, 0, 0, 0, 0,
                29, /* depths */ 0, 0, 0, 0, 0, 0, 0, 31, /* italic_corrections */ 0, 0,
                0, 0, 0, 0, 0, 37, /* lig_kern_commands */
                /* kerns */ 0, 0, 0, 37
            ],
            File {
                widths: vec![Number::ZERO, Number(23)],
                heights: vec![Number::ZERO, Number(29)],
                depths: vec![Number::ZERO, Number(31)],
                italic_corrections: vec![Number::ZERO, Number(37)],
                kerns: vec![Number(37)],
                ..Default::default()
            },
        ),
        (
            lig_kern_command_1,
            tfm_file_with_one_lig_kern_command([3, 5, 130, 13]),
            File {
                lig_kern_instructions: vec![ligkern::lang::Instruction {
                    next_instruction: Some(3),
                    right_char: Char(5),
                    operation: ligkern::lang::Operation::KernAtIndex(256 * 2 + 13)
                },],
                ..Default::default()
            },
        ),
        (
            lig_kern_command_2,
            tfm_file_with_one_lig_kern_command([3, 6, 3, 17]),
            File {
                lig_kern_instructions: vec![ligkern::lang::Instruction {
                    next_instruction: Some(3),
                    right_char: Char(6),
                    operation: ligkern::lang::Operation::Ligature {
                        char_to_insert: Char(17),
                        post_lig_operation: ligkern::lang::PostLigOperation::RetainBothMoveNowhere,
                    },
                },],
                ..Default::default()
            },
        ),
        (
            lig_kern_command_3,
            tfm_file_with_one_lig_kern_command([3, 7, 3 + 4, 19]),
            File {
                lig_kern_instructions: vec![ligkern::lang::Instruction {
                    next_instruction: Some(3),
                    right_char: Char(7),
                    operation: ligkern::lang::Operation::Ligature {
                        char_to_insert: Char(19),
                        post_lig_operation:
                            ligkern::lang::PostLigOperation::RetainBothMoveToInserted,
                    },
                },],
                ..Default::default()
            },
        ),
        (
            lig_kern_command_4,
            tfm_file_with_one_lig_kern_command([128, 8, 3 + 8, 23]),
            File {
                lig_kern_instructions: vec![ligkern::lang::Instruction {
                    next_instruction: None,
                    right_char: Char(8),
                    operation: ligkern::lang::Operation::Ligature {
                        char_to_insert: Char(23),
                        post_lig_operation: ligkern::lang::PostLigOperation::RetainBothMoveToRight,
                    },
                },],
                ..Default::default()
            },
        ),
        (
            lig_kern_command_5,
            tfm_file_with_one_lig_kern_command([128, 8, 1, 23]),
            File {
                lig_kern_instructions: vec![ligkern::lang::Instruction {
                    next_instruction: None,
                    right_char: Char(8),
                    operation: ligkern::lang::Operation::Ligature {
                        char_to_insert: Char(23),
                        post_lig_operation:
                            ligkern::lang::PostLigOperation::RetainRightMoveToInserted,
                    },
                },],
                ..Default::default()
            },
        ),
        (
            lig_kern_command_6,
            tfm_file_with_one_lig_kern_command([128, 8, 1 + 4, 23]),
            File {
                lig_kern_instructions: vec![ligkern::lang::Instruction {
                    next_instruction: None,
                    right_char: Char(8),
                    operation: ligkern::lang::Operation::Ligature {
                        char_to_insert: Char(23),
                        post_lig_operation: ligkern::lang::PostLigOperation::RetainRightMoveToRight,
                    },
                },],
                ..Default::default()
            },
        ),
        (
            lig_kern_command_7,
            tfm_file_with_one_lig_kern_command([128, 8, 2, 23]),
            File {
                lig_kern_instructions: vec![ligkern::lang::Instruction {
                    next_instruction: None,
                    right_char: Char(8),
                    operation: ligkern::lang::Operation::Ligature {
                        char_to_insert: Char(23),
                        post_lig_operation: ligkern::lang::PostLigOperation::RetainLeftMoveNowhere,
                    },
                },],
                ..Default::default()
            },
        ),
        (
            lig_kern_command_8,
            tfm_file_with_one_lig_kern_command([128, 8, 2 + 4, 23]),
            File {
                lig_kern_instructions: vec![ligkern::lang::Instruction {
                    next_instruction: None,
                    right_char: Char(8),
                    operation: ligkern::lang::Operation::Ligature {
                        char_to_insert: Char(23),
                        post_lig_operation:
                            ligkern::lang::PostLigOperation::RetainLeftMoveToInserted,
                    },
                },],
                ..Default::default()
            },
        ),
        (
            lig_kern_command_9,
            tfm_file_with_one_lig_kern_command([128, 8, 0, 23]),
            File {
                lig_kern_instructions: vec![ligkern::lang::Instruction {
                    next_instruction: None,
                    right_char: Char(8),
                    operation: ligkern::lang::Operation::Ligature {
                        char_to_insert: Char(23),
                        post_lig_operation:
                            ligkern::lang::PostLigOperation::RetainNeitherMoveToInserted,
                    },
                },],
                ..Default::default()
            },
        ),
        (
            lig_kern_command_special,
            tfm_file_with_one_lig_kern_command([254, 0, 1, 2]),
            File {
                lig_kern_instructions: vec![ligkern::lang::Instruction {
                    next_instruction: None,
                    right_char: Char(0),
                    operation: ligkern::lang::Operation::EntrypointRedirect(
                        u16::from_be_bytes([1, 2]),
                        None,
                    ),
                },],
                ..Default::default()
            },
        ),
        (
            extensible_chars,
            vec![
                /* lf */ 0, 13, /* lh */ 0, 2, /* bc */ 0, 1, /* ec */ 0, 0,
                /* nw */ 0, 1, /* nh */ 0, 1, /* nd */ 0, 1, /* ni */ 0, 1,
                /* nl */ 0, 0, /* nk */ 0, 0, /* ne */ 0, 1, /* np */ 0, 0,
                /* header.checksum */ 0, 0, 0, 0, /* header.design_size */ 0, 0, 0, 0,
                /* widths */ 0, 0, 0, 0, /* heights */ 0, 0, 0, 0, /* depths */ 0,
                0, 0, 0, /* italic_corrections */ 0, 0, 0, 0, /* lig_kern_commands */
                /* kerns */ /* extensible_chars */ 17, 19, 23, 27
            ],
            File {
                extensible_chars: vec![ExtensibleRecipe {
                    top: Some(Char(17)),
                    middle: Some(Char(19)),
                    bottom: Some(Char(23)),
                    rep: Char(27),
                }],
                ..Default::default()
            },
        ),
        (
            params,
            vec![
                /* lf */ 0, 22, /* lh */ 0, 2, /* bc */ 0, 1, /* ec */ 0, 0,
                /* nw */ 0, 1, /* nh */ 0, 1, /* nd */ 0, 1, /* ni */ 0, 1,
                /* nl */ 0, 0, /* nk */ 0, 0, /* ne */ 0, 0, /* np */ 0, 10,
                /* header.checksum */ 0, 0, 0, 0, /* header.design_size */ 0, 0, 0, 0,
                /* widths */ 0, 0, 0, 0, /* heights */ 0, 0, 0, 0, /* depths */ 0,
                0, 0, 0, /* italic_corrections */ 0, 0, 0, 0, /* lig_kern_commands */
                /* kerns */ /* extensible_chars */ /* params */
                0, 0, 0, 11, 0, 0, 0, 13, 0, 0, 0, 17, 0, 0, 0, 19, 0, 0, 0, 23, 0, 0, 0, 29, 0, 0,
                0, 31, 0, 0, 0, 37, 0, 0, 0, 41, 0, 0, 0, 43,
            ],
            File {
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
                ..Default::default()
            },
        ),
    );

    fn extend(input: &[u8], size: usize) -> Vec<u8> {
        let mut v: Vec<u8> = input.into();
        for _ in input.len()..size {
            v.push(0)
        }
        v
    }

    fn tfm_file_with_one_lig_kern_command(lig_kern_command: [u8; 4]) -> Vec<u8> {
        let mut v = vec![
            /* lf */ 0, 13, /* lh */ 0, 2, /* bc */ 0, 1, /* ec */ 0, 0,
            /* nw */ 0, 1, /* nh */ 0, 1, /* nd */ 0, 1, /* ni */ 0, 1,
            /* nl */ 0, 1, /* nk */ 0, 0, /* ne */ 0, 0, /* np */ 0, 0,
            /* header.checksum */ 0, 0, 0, 0, /* header.design_size */ 0, 0, 0, 0,
            /* widths */ 0, 0, 0, 0, /* heights */ 0, 0, 0, 0, /* depths */ 0, 0, 0,
            0, /* italic_corrections */ 0, 0, 0, 0,
        ];
        /* lig_kern_commands */
        v.extend(lig_kern_command);
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

    // This function pads out the header with 0s so that it always contains at least 18 words.
    fn canonical_tfm_bytes(b: Vec<u8>) -> Vec<u8> {
        let lh = u16::from_be_bytes([b[2], b[3]]);
        if lh >= 18 {
            return b;
        }
        let mut lf = u16::from_be_bytes([b[0], b[1]]);
        lf += 18 - lh;
        let boundary = 24 + (lh as usize) * 4;
        let mut v: Vec<u8> = lf.to_be_bytes().into();
        v.extend(&[0, 18]);
        v.extend(&b[4..boundary]);
        v.resize(24 + 18 * 4, 0);
        v.extend(&b[boundary..]);
        v
    }
}
