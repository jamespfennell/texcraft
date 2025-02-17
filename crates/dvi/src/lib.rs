//! # DVI file format
//!
//! This crate implements the DVI ("device-independent file format") format.
//!
//! The most important type in the crate is [`Op`],
//! which describes a single operation or
//! command in a DVI file. A DVI file is just a list of such operations.
//!
//! The crate provides an iterator,
//! [`Deserializer`], that accepts raw DVI bytes and returns
//! the operations in the DVI data.
//! The inverse of this deserializer is the
//! [`serialize()`] function.
//!
//! ## Knuth's description of the format
//!
//! The text in this section was written by Donald Knuth.
//! It appears as documentation in TeX.2021.583.
//!
//! The most important output produced by a run of TeX is the "device
//! independent" (DVI) file that specifies where characters and rules
//! are to appear on printed pages. The form of these files was designed by
//! David R. Fuchs in 1979. Almost any reasonable typesetting device can be
//! driven by a program that takes DVI files as input, and dozens of such
//! DVI-to-whatever programs have been written. Thus, it is possible to
//! print the output of TeX on many different kinds of equipment, using TeX
//! as a device-independent "front end."
//!
//! A DVI file is a stream of 8-bit bytes, which may be regarded as a
//! series of commands in a machine-like language. The first byte of each command
//! is the operation code, and this code is followed by zero or more bytes
//! that provide parameters to the command. The parameters themselves may consist
//! of several consecutive bytes; for example, the `set_rule` command
//! ([`Op::TypesetRule`] with `move_h=true`) has two
//! parameters, each of which is four bytes long. Parameters are usually
//! regarded as nonnegative integers; but four-byte-long parameters,
//! and shorter parameters that denote distances, can be
//! either positive or negative. Such parameters are given in two's complement
//! notation. For example, a two-byte-long distance parameter has a value between
//! `-2^15` and `2^{15}-1`. As in TFM files, numbers that occupy
//! more than one byte position appear in big endian order.
//!
//! A DVI file consists of a "preamble," followed by a sequence of one
//! or more "pages," followed by a "postamble." The preamble is simply a
//! command ([`Op::Preamble`] command, with its parameters that define the dimensions used in the
//! file; this must come first.  Each "page" consists of a
//! [`Op::BeginPage`] command,
//! followed by any number of other commands that tell where characters are to
//! be placed on a physical page, followed by an
//! ([`Op::EndPage`] command. The pages
//! appear in the order that TeX generated them. If we ignore
//! [`Op::NoOp`] commands
//! and [`Op::DefineFont`] command, which are allowed between any two commands in
//! the file),
//! each [`Op::BeginPage`] command is immediately followed by a [`Op::EndPage`] command,
//! or by a [`Op::BeginPostamble`] command; in the latter case, there are no more pages in the
//! file, and the remaining bytes form the postamble.
//! Further details about
//! the postamble will be explained later.
//!
//! Some parameters in DVI commands are "pointers." These are four-byte
//! quantities that give the location number of some other byte in the file;
//! the first byte is number~0, then comes number~1, and so on. For example,
//! one of the parameters of a `bop` command points to the previous `bop`;
//! this makes it feasible to read the pages in backwards order, in case the
//! results are being directed to a device that stacks its output face up.
//! Suppose the preamble of a DVI file occupies bytes 0 to 99. Now if the
//! first page occupies bytes 100 to 999, say, and if the second
//! page occupies bytes 1000 to 1999, then the `bop` that starts in byte 1000
//! points to 100 and the `bop` that starts in byte 2000 points to 1000. (The
//! very first `bop`, i.e., the one starting in byte 100, has a pointer of -1.)

mod deserialize;
mod serialize;

/// A variable in DVI data.
///
/// DVI data has access to four variables.
/// These variables are set using the [`Op::SetVar`] operation
/// and used in the [`Op::Move`] operation.
///
/// These variables and associated operations exist to support
/// the following optimization.
/// It is possible to replace repeated
/// identical [`Op::Right`]/[`Op::Down`] operations with sets and moves
/// that serialize to a smaller number of bytes.
/// For example this DVI sequence:
/// ```
/// vec![
///     dvi::Op::Down(300),
///     dvi::Op::Down(300),
///     dvi::Op::Down(300),
/// ];
/// ```
/// is identical to this DVI sequence:
/// ```
/// vec![
///     // Set Y to 300 and move down by that value.
///     dvi::Op::SetVar(dvi::Var::Y, 300),
///     // Move down by the current value of Y, 300.
///     dvi::Op::Move(dvi::Var::Y),
///     dvi::Op::Move(dvi::Var::Y),
/// ];
/// ```
/// The first sequence serializes to 9 bytes,
/// while the second serializes to 5 bytes.
/// This optimization is performed in TeX.2021.595 and onwards.
///
/// Of the four variables, [`Var::W`] and [`Var::X`] operate on the
/// horizontal part of the cursor _h_, and [`Var::Y`] and [`Var::Z`]
/// operate on the vertical part of the cursor _v_.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Var {
    W = 0,
    X = 1,
    Y = 2,
    Z = 3,
}

/// Operation that appears in DVI data.
///
/// The documentation for each variant is adapted from TeX.2021.585.
/// However the variants don't map one-to-one on to commands as described
/// there. Instead, commands that are logically connected are represented
/// in the same variant. For example, `set_char_0`, `set1` and `put1` are
/// all represented using the [`Op::TypesetChar`] variant.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Op {
    /// Typeset the specified character from the current font _f_
    /// such that the reference point of the character is at (_h_,_v_).
    ///
    /// This op corresponds to the DVI commands `set_char_N`, `set_N`
    /// and `put_N`.
    TypesetChar {
        /// The character to typeset.
        char: u32,
        /// If true, after typesetting the character,
        /// increase _h_ by the width of that character. Note that a character may
        /// have zero or negative width, so one cannot be sure that _h_ will advance
        /// after this command; but _h_ usually does increase.
        ///
        /// This field is true for `setX` commands and false for `putX` commands.
        move_h: bool,
    },
    /// Typeset a solid black rectangle
    /// of the provided height and width, with its bottom left corner at (_h_,_v_).
    ///
    /// If either the width or height is not positive, nothing should be typeset.
    ///
    /// This op corresponds to the DVI commands `set_rule` and `put_rule`.
    ///
    /// [TeX.2021.589]
    /// Sometimes it is desirable to make horizontal or vertical rules line up
    /// precisely with certain features in characters of a font. It is possible to
    /// guarantee the correct matching between DVI output and the characters
    /// generated by MetaFont by adhering to the following principles:
    ///
    /// 1. The MetaFont
    ///     characters should be positioned so that a bottom edge or left edge that is
    ///     supposed to line up with the bottom or left edge of a rule appears at the
    ///     reference point, i.e., in row 0 and column 0 of the MetaFont raster. This
    ///     ensures that the position of the rule will not be rounded differently when
    ///     the pixel size is not a perfect multiple of the units of measurement in
    ///     the DVI file.
    ///
    /// 1. A typeset rule of positive height and positive width
    ///     should be equivalent to a MetaFont-generated character having black pixels in
    ///     precisely those raster positions whose MeatFont coordinates satisfy
    ///     0≤_x_<_wa_ and 0≤_y_<_ha_, where _a_ is the number
    ///     of pixels per DVI unit.
    TypesetRule {
        /// Height of the rule.
        height: i32,
        /// Width of the rule.
        width: i32,
        /// If true, after typesetting the rule,
        /// increase _h_ by the width of the rule.
        ///
        /// Note
        /// that if the width is negative, the value of _h_
        /// will decrease even though the rule is not typeset.
        ///
        /// This field is true for `set_rule` commands and false for `put_rule` commands.
        move_h: bool,
    },
    /// No operation, do nothing.
    ///
    /// This op corresponds to the DVI commands `nop`.
    NoOp,
    /// Beginning of a page.
    ///
    /// Set (_h_,_v_,_w_,_x_,_y_,_z_) equal to (0,0,0,0,0,0) and set the stack empty.
    /// Set the current font _f_ to an undefined value.
    ///
    /// This op corresponds to the DVI commands `bop`.
    BeginPage {
        /// The ten parameters.
        ///
        /// In the output from TeX, these hold
        /// the values of `\count 0`...`\count 9`
        /// at the time shipout was invoked for this page.
        /// They can be used to identify pages,
        /// if a user wants to print only part of a DVI file.
        parameters: [i32; 10],
        /// Pointer to the previous [`Op::BeginPage`] in the file,
        /// or -1 if this is the first begin page op.
        previous_begin_page: i32,
    },
    /// End of page: Print what you have read since the
    /// previous [`Op::BeginPage`].
    /// At this point the stack should be empty.
    ///
    /// The DVI-reading
    /// programs that drive most output devices will have kept a buffer of the
    /// material that appears on the page that has just ended. This material is
    /// largely, but not entirely, in order by _v_ coordinate and (for fixed _v_) by
    /// _h_ coordinate; so it usually needs to be sorted into some order that is
    /// appropriate for the device in question.
    ///
    /// This op corresponds to the DVI commands `eop`.
    EndPage,
    /// Push the current values of
    /// (_h_,_v_,_w_,_x_,_y_,_z_)
    ///  onto the
    /// top of the stack; do not change any of these values. Note that _f_ is
    /// not pushed.
    ///
    /// This op corresponds to the DVI commands `push`.
    Push,

    /// Pop the top six values off of the stack and assign
    /// them respectively to
    /// (_h_,_v_,_w_,_x_,_y_,_z_).
    /// The number of pops should never
    /// exceed the number of pushes, since it would be highly embarrassing if the
    /// stack were empty at the time of a pop command.
    ///
    /// This op corresponds to the DVI commands `pop`.
    Pop,
    /// Move _h_ right by the number in the payload.
    /// If the payload is negative, _h_ moves left.
    ///
    /// This op corresponds to the four DVI commands `rightN`.
    Right(i32),
    /// Move _h_ or _v_ by the value of the variable in the payload.
    ///
    /// If the variable is _w_ or _x_, _h_ is moved.
    /// If the variable is _y_ or _z_, _v_ is moved.
    ///
    /// With luck,
    /// this parameterless command will usually suffice for moving _h_ and _v_,
    /// because the same kind of motion
    /// will occur several times in succession.
    ///
    /// This op corresponds to the four DVI commands `w0`, `x0`, `y0` and `z0`.
    Move(Var),
    /// Set the value of the specified variable, and then move _h_ or _v_
    /// based on the new value.
    ///
    /// If the variable is _w_ or _x_, _h_ is moved.
    /// If the variable is _y_ or _z_, _v_ is moved.
    ///
    /// This op corresponds to the four DVI commands `wN`, `xN`, `yN` and `zN`
    /// for `N>0`.
    SetVar(Var, i32),
    /// Move _v_ down by the number in the payload.
    /// If the payload is negative, _v_ moves up.
    ///
    /// This op corresponds to the four DVI commands `downN`.
    Down(i32),
    /// Enable the specified font.
    /// This font must have been previously defined by a [`Op::DefineFont`]
    /// command.
    ///
    /// This op corresponds to the DVI commands `fnt_num_N` and `fntN`.
    EnableFont(u32),

    /// This command is undefined in
    /// general; it functions as a (_k_+2)-byte [`Op::NoOp`],
    /// where _k_ is the number of bytes,
    ///  unless special DVI-reading
    /// programs are being used.
    ///
    /// It
    /// is recommended that the payload be a string having the form of a keyword followed
    /// by possible parameters relevant to that keyword.
    ///
    /// This op corresponds to the DVI commands `xxxN`.
    Extension(Vec<u8>),
    /// Define a font.
    ///
    /// This op corresponds to the DVI commands `fnt_defN`.
    ///
    /// The rest of the documentation on this variant comes from TeX.2021.588.
    /// Font definitions must appear before the first use of a particular font number.
    /// Once a font is defined with a specific number, it must not be defined again; however, we
    /// definitions appear in the postamble as well as
    /// in the pages, so in this sense each font number is defined exactly twice,
    /// if at all. Like [`Op::NoOp`] commands, font definitions can
    /// appear before the first [`Op::BeginPage`],
    /// or between an [`Op::EndPage`] and a [`Op::BeginPage`].
    DefineFont {
        /// Number of the font.
        number: u32,
        /// Check sum that TeX found in the TFM
        /// file for this font; this should match the check sum of the font found by
        /// programs that read this DVI file.
        checksum: u32,
        /// A fixed-point scale factor that is applied to
        /// the character widths this font; font dimensions in TFM files and
        /// other font files are relative to this quantity, which is called the
        /// "at size" elsewhere in this documentation. The value of the parameter is
        /// always positive and less than `2^27`. It is given in the same units
        /// as the other DVI dimensions, i.e., in sp when TeX has made the
        /// file.  
        at_size: u32,
        /// Similar to the at size; it is the "design size," and
        /// like the at size it is given in DVI units. Thus, this font is to be used
        /// at _ms_/(1000 _d_) times its normal size, where _m_ is the magnification,
        /// _s_ is the at size and _d_ is the design size.
        design_size: u32,
        /// The "area" or directory of the font.
        /// The standard local system font area is supposed to be used this is empty.
        area: String,
        /// The external name of the font.
        name: String,
    },
    /// The preamble.
    /// This must come at the very beginning of the file.
    ///
    /// This op corresponds to the DVI command `pre`.
    ///
    /// The rest of the documentation on this variant comes from TeX.2021.587.
    /// The preamble contains basic information about the file as a whole.
    Preamble {
        /// The DVI format; currently this byte is always set
        /// to 2. (The value 3 is currently used for an extended format that
        /// allows a mixture of right-to-left and left-to-right typesetting.
        /// Some day we will set the format to 4, when DVI format makes another
        /// incompatible change---perhaps in the year 2048.
        dvi_format: u8,
        /// The next two parameters are positive integers that define
        /// the units of measurement; they are the numerator and denominator of a
        /// fraction by which all dimensions in the DVI file could be multiplied
        /// in order to get lengths in units of `10^(-7)` meters. Since 7227pt =
        /// 254cm, and since TeX works with scaled points where there are `2^16`
        /// sp in a point, TeX sets
        /// the numerator to `254x10^5=25400000`
        /// and the denominator to
        /// `7227x2^16=473628672`.
        unit_numerator: u32,
        /// See the description of the previous field.
        unit_denominator: u32,
        /// The magnification parameter is what TeX calls `\mag`, i.e., 1000 times the
        /// desired magnification. The actual fraction by which dimensions are
        /// multiplied is therefore _mn_/(1000 _d_).
        /// Note that if a TeX
        /// source document does not call for any "true" dimensions, and if you
        /// change it only by specifying a different `\mag` setting, the DVI
        /// file that TeX creates will be completely unchanged except for the value
        /// of the maginification in the preamble and postamble.
        /// (Fancy DVI-reading programs allow
        /// users to override the magnificiation setting when a DVI file is being printed.)
        magnification: u32,
        /// A comment, which is not interpreted further.
        comment: String,
    },
    /// The start of the postamble.
    ///
    /// This op corresponds to the DVI command `post`.
    ///
    /// The rest of the documentation on this variant comes from TeX.2021.590.
    ///
    ///
    /// This command
    /// introduces the postamble, which summarizes important facts that TeX has
    /// accumulated about the file, making it possible to print subsets of the data
    /// with reasonable efficiency. The postamble has the form:
    ///
    /// - [`Op::BeginPostamble`].
    /// - Multiple font definitions ([`Op::DefineFont`]).
    /// - [`Op::EndPostamble`].
    BeginPostamble {
        /// A pointer to the final [`Op::BeginPage`] in the file.
        final_begin_page: i32,
        /// Duplicate of the analagous parameter in [`Op::Preamble`]
        unit_numerator: u32,
        /// Duplicate of the analagous parameter in [`Op::Preamble`]
        unit_denominator: u32,
        /// Duplicate of the analagous parameter in [`Op::Preamble`]
        magnification: u32,
        /// The height-plus-depth of the tallest
        /// page, in the same units as other dimensions
        /// of the file. This height, along with the next width parameter,
        /// might be used by a DVI-reading program to
        /// position individual "pages" on large sheets of film or paper; however,
        /// the standard convention for output on normal size paper is to position each
        /// page so that the upper left-hand corner is exactly one inch from the left
        /// and the top. Experience has shown that it is unwise to design DVI-to-printer
        /// software that attempts cleverly to center the output; a fixed position of
        /// the upper left corner is easiest for users to understand and to work with.
        /// Therefore this field and the next field are are often ignored.
        largest_height: u32,
        /// The width of the widest page in the same units as other dimensions
        /// of the file.
        largest_width: u32,
        /// The maximum stack depth (i.e., the largest excess of
        /// [`Op::Push`] commands over [`Op::Pop`] commands) needed to process this file.
        max_stack_depth: u16,
        /// The total number of pages ([`Op::BeginPage`] commands) present.
        num_pages: u16,
    },
    /// The end of the postamble.
    ///
    /// This op corresponds to the DVI command `post_post`.
    ///
    /// The rest of the documentation on this variant comes from TeX.2021.591.
    EndPostamble {
        /// A pointer to the
        /// [`Op::BeginPostamble`] command that started the postamble.
        postamble: i32,
        /// Duplicate of the analagous parameter in [`Op::Preamble`]
        dvi_format: u8,
        /// The DVI format byte is followed by four or more bytes that are all equal to
        /// the decimal number 223 (i.e., 0x337). TeX puts out four to seven of
        /// these trailing bytes, until the total length of the file is a multiple of
        /// four bytes, since this works out best on machines that pack four bytes per
        /// word; but any number of 223's is allowed, as long as there are at least four
        /// of them. In effect, 223 is a sort of signature that is added at the very end.
        ///
        /// This curious way to finish off a DVI file makes it feasible for
        /// DVI-reading programs to find the postamble first, on most computers,
        /// even though TeX wants to write the postamble last. Most operating
        /// systems permit random access to individual words or bytes of a file, so
        /// the DVI reader can start at the end and skip backwards over the 223's
        /// until finding the DVI identification byte. Then it can back up four bytes, read
        /// the postamble pointer, and move to that byte of the file. This byte should, of course,
        /// contain the value 248 (the op code for [`Op::BeginPostamble`]);
        /// now the postamble can be read, so the
        /// DVI reader can discover all the information needed for typesetting the
        /// pages. Note that it is also possible to skip through the DVI file at
        /// reasonably high speed to locate a particular page, if that proves
        /// desirable. This saves a lot of time, since DVI files used in production
        /// jobs tend to be large.
        ///
        /// Unfortunately, however, standard Pascal does not include the ability to
        /// access a random position in a file, or even to determine the length of a file.
        ///  Almost all systems nowadays provide the necessary capabilities, so DVI
        ///  format has been designed to work most efficiently with modern operating systems.
        ///   But if DVI files have to be processed under the restrictions of standard
        ///  Pascal, one can simply read them from front to back, since the necessary
        ///   header information is present in the preamble and in the font definitions.
        num_223_bytes: usize,
    },
}

impl Op {
    /// Deserialize the next operation from the provided binary slice.
    ///
    /// Note that in general it is easier to perform deserialization using the
    /// [`Deserializer`] iterator.
    ///
    /// There are three possible return values from this method.
    /// If the deserialization succeeds, the return value contains the operation
    /// and the tail of the slice that was not consumed.
    /// This tail can be used to deserialize the next operation:
    ///
    /// ```
    /// let data = vec![128, 4, 129, 1, 0];
    /// let (op, tail) = dvi::Op::deserialize(&data).unwrap().unwrap();
    /// assert_eq![op, dvi::Op::TypesetChar{char: 4, move_h: true}];
    /// assert_eq![tail, &[129, 1, 0]];
    ///
    /// let (op, tail) = dvi::Op::deserialize(&tail).unwrap().unwrap();
    /// assert_eq![op, dvi::Op::TypesetChar{char: 256, move_h: true}];
    /// assert_eq![tail, &[]];
    /// ```
    ///
    /// If the slice is exhausted, the method returns [`None`]:
    ///
    /// ```
    /// let data = vec![];
    /// assert_eq![dvi::Op::deserialize(&data), Ok(None)];
    /// ```
    ///
    /// If the data is not valid DVI data, an error is returned:
    /// ```
    /// let data_1 = vec![254];
    /// assert_eq![
    ///     dvi::Op::deserialize(&data_1),
    ///     Err(dvi::InvalidDviData::InvalidOpCode(254)),
    /// ];
    ///
    /// let data_2 = vec![129, 1];
    /// assert_eq![
    ///     dvi::Op::deserialize(&data_2),
    ///     Err(dvi::InvalidDviData::Truncated(129)),
    /// ];
    /// ```
    pub fn deserialize(b: &[u8]) -> Result<Option<(Self, &[u8])>, InvalidDviData> {
        deserialize::deserialize(b)
    }

    /// Serialize this operation to bytes and append them to the provided vector.
    ///
    /// Unless you want close control over allocations, it's likely easier
    /// to use the top-level [`serialize()`] function.
    ///
    /// ```
    /// let mut data = vec![];
    /// let op = dvi::Op::Right(256);
    /// op.serialize(&mut data);
    /// assert_eq![data, vec![144, 1, 0]];
    /// ```
    pub fn serialize(&self, b: &mut Vec<u8>) {
        serialize::serialize(self, b)
    }
}

/// Error returned if deserializing DVI data fails.
///
/// This error is returned from the [`Op::deserialize`] method
/// and the [`Deserializer`] iterator.
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum InvalidDviData {
    /// An invalid op code appeared.
    InvalidOpCode(u8),
    /// The file ended while parsing the payload of an operation.
    /// The op code of the operation is provided.
    Truncated(u8),
}

impl std::error::Error for InvalidDviData {}

impl std::fmt::Display for InvalidDviData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InvalidDviData::InvalidOpCode(op_code) => {
                write!(f, "invalid op code {op_code}")
            }
            InvalidDviData::Truncated(op_code) => {
                write!(f, "data ended while parsing payload for op code {op_code}")
            }
        }
    }
}

/// Iterator that deserializes bytes into [`Op`] values.
///
/// ```
/// let data: Vec<u8> = vec![158, 1, 0, 68, 86, 73];
/// let mut result = Ok(());
/// let ops: Vec<dvi::Op> = dvi::Deserializer::new(&data, &mut result).collect();
/// assert_eq![result, Ok(())];
/// assert_eq![
///     ops,
///     vec![
///         dvi::Op::Down(256),
///         dvi::Op::TypesetChar{char: 'D' as u32, move_h: true},
///         dvi::Op::TypesetChar{char: 'V' as u32, move_h: true},
///         dvi::Op::TypesetChar{char: 'I' as u32, move_h: true},
///     ],
/// ];
/// ```
///
/// The deserializer returns [`Op`] values so that it can easily compose with other
/// iterators.
/// However the DVI data can be invalid.
/// This error is reported through a side channel result value:
///
/// ```
/// let invalid_data: Vec<u8> = vec![158, 1, 0, 255];
/// let mut result = Ok(());
/// let ops: Vec<dvi::Op> = dvi::Deserializer::new(&invalid_data, &mut result).collect();
/// assert_eq![result, Err(dvi::InvalidDviData::InvalidOpCode(255))];
/// assert_eq![
///     ops,
///     // Ops in the file before the error is hit are returned in the iterator.
///     vec![dvi::Op::Down(256)],
/// ];
/// ```
pub struct Deserializer<'a> {
    b: &'a [u8],
    result: &'a mut Result<(), InvalidDviData>,
}

impl<'a> Deserializer<'a> {
    /// Create a new iterator from the provided binary slice.
    pub fn new(b: &'a [u8], result: &'a mut Result<(), InvalidDviData>) -> Self {
        Self { b, result }
    }
}

impl<'a> Iterator for Deserializer<'a> {
    type Item = Op;

    fn next(&mut self) -> Option<Self::Item> {
        match Op::deserialize(self.b) {
            Ok(None) => None,
            Ok(Some((op, b))) => {
                self.b = b;
                Some(op)
            }
            Err(err) => {
                *self.result = Err(err);
                None
            }
        }
    }
}

/// Serialize operations to DVI data bytes.
///
/// ```
/// let ops = vec![
///     dvi::Op::Down(256),
///     dvi::Op::TypesetChar{char: 'D' as u32, move_h: true},
///     dvi::Op::TypesetChar{char: 'V' as u32, move_h: true},
///     dvi::Op::TypesetChar{char: 'I' as u32, move_h: true},
/// ];
/// let data = dvi::serialize(ops);
/// assert_eq!(data, vec![158, 1, 0, 68, 86, 73]);
/// ```
pub fn serialize<I: IntoIterator<Item = Op>>(i: I) -> Vec<u8> {
    let mut v = vec![];
    for op in i {
        op.serialize(&mut v);
    }
    v
}

/// Data structure for tracking values in DVI data.
///
/// The DVI format refers to seven runtime values that are modified based
/// on operations in the data.
/// These seven values are:
///
/// - The current font, _f_.
///
/// - The two coordinates of the current cursor position, (_h_,_v_).
///
/// - The four values of the four variables
///     [`Var::W`],
///     [`Var::X`],
///     [`Var::Y`],
///     [`Var::Z`].
///
/// This data structure provides a mechanism for calculating these values
/// as DVI operations are run:
///
/// ```
/// let mut values: dvi::Values = Default::default();
/// assert_eq!(values.y(), 0);
///
/// values.update(&dvi::Op::SetVar(dvi::Var::Y, 3));
/// assert_eq!(values.y(), 3);
///
/// values.update(&dvi::Op::Push);
/// values.update(&dvi::Op::SetVar(dvi::Var::Y, 5));
/// assert_eq!(values.y(), 5);
///
/// values.update(&dvi::Op::Pop);
/// assert_eq!(values.y(), 3);
/// ```
///
/// Six of the values are just integers, which are simple to deal with.
/// The value of _h_ is more complicated;
/// see the documentation on [`Values::h`] for more information.
///
/// ## Knuth's description of the values
///
/// This text is from TeX.2021.584.
///
/// The DVI format is intended to be both compact and easily interpreted
/// by a machine. Compactness is achieved by making most of the information
/// implicit instead of explicit. When a DVI-reading program reads the
/// commands for a page, it keeps track of several quantities:
///
/// 1. The current font _f_ is an integer; this value is changed only
///     by `fnt` and `fnt_num` commands
///     (both commands are represented by [`Op::EnableFont`]).
///
/// 2. The current position on the page
///     is given by two numbers called the horizontal and vertical coordinates,
///     _h_ and _v_. Both coordinates are zero at the upper left corner of the page;
///     moving to the right corresponds to increasing the horizontal coordinate, and
///     moving down corresponds to increasing the vertical coordinate. Thus, the
///     coordinates are essentially Cartesian, except that vertical directions are
///     flipped; the Cartesian version of (_h_,_v_) would be (_h_,_-v_).
///
/// 3. The current spacing amounts are given by four numbers _w_, _x_, _y_, and _z_,
///     where _w_ and _x_ are used for horizontal spacing and where _y_ and _z_
///     are used for vertical spacing.
///
/// 4. There is a stack containing
///     (_h_,_v_,_w_,_x_,_y_,_z_) values; the DVI commands `push`
///     ([`Op::Push`]) and `pop` ([`Op::Pop`]) are used to
///     change the current level of operation. Note that the current font _f_ is
///     not pushed and popped; the stack contains only information about
///     positioning.
///
/// The values of _h_, _v_, _w_, _x_, _y_, and _z_ are signed integers having up
/// to 32 bits, including the sign. Since they represent physical distances,
/// there is a small unit of measurement such that increasing _h_ by 1 means
/// moving a certain tiny distance to the right. The actual unit of
/// measurement is variable, as explained below; TeX sets things up so that
/// its DVI output is in sp units, i.e., scaled points, in agreement with
/// all the scaled dimensions in TeX's data structures.
#[derive(Default)]
pub struct Values {
    f: u32,
    top: StackValues,
    tail: Vec<StackValues>,
}

#[derive(Default, Clone, PartialEq, Eq)]
struct StackValues {
    h: i32,
    h_chars: Vec<(u32, u32)>,
    v: i32,
    vars: [i32; 4],
}

impl Values {
    /// Update the values by applying the provided operation.
    pub fn update(&mut self, op: &Op) -> bool {
        match op {
            Op::TypesetChar { char, move_h } => {
                if *move_h {
                    self.top.h_chars.push((*char, self.f()));
                    true
                } else {
                    false
                }
            }
            Op::TypesetRule {
                height: _,
                width,
                move_h,
            } => {
                if *move_h {
                    self.top.h += *width;
                    *width != 0
                } else {
                    false
                }
            }
            Op::NoOp => false,
            Op::BeginPage {
                parameters: _,
                previous_begin_page: _,
            } => {
                self.tail = vec![];
                let new: StackValues = Default::default();
                let changed = self.top != new;
                self.top = new;
                changed
            }
            Op::EndPage => false,
            Op::Push => {
                self.tail.push(self.top.clone());
                false
            }
            Op::Pop => {
                let Some(top) = self.tail.pop() else {
                    return false;
                };
                let changed = top != self.top;
                self.top = top;
                changed
            }
            Op::Right(d) => {
                self.top.h += *d;
                *d != 0
            }
            Op::Move(var) => {
                let d = self.top.vars[*var as usize];
                match var {
                    Var::W | Var::X => {
                        self.top.h += d;
                    }
                    Var::Y | Var::Z => {
                        self.top.v += d;
                    }
                }
                d != 0
            }
            Op::SetVar(var, i) => {
                let old = self.top.vars[*var as usize];
                self.top.vars[*var as usize] = *i;
                match var {
                    Var::W | Var::X => {
                        self.top.h += *i;
                    }
                    Var::Y | Var::Z => {
                        self.top.v += *i;
                    }
                }
                // This is only a noop if the old and new values are both zero.
                // In this case the variable assignment does nothing, and
                // the position is also unchanged.
                old != 0 || *i != 0
            }
            Op::Down(d) => {
                self.top.v += *d;
                *d != 0
            }
            Op::EnableFont(f) => {
                let old = self.f;
                self.f = *f;
                old != *f
            }
            Op::Extension(_)
            | Op::DefineFont { .. }
            | Op::Preamble { .. }
            | Op::BeginPostamble { .. }
            | Op::EndPostamble { .. } => false,
        }
    }
    /// Get the current value of the font, _f_.
    ///
    /// Note that unlike every other value, the font is not affected by
    /// push and pop operations.
    ///
    /// ```
    /// let mut values: dvi::Values = Default::default();
    /// values.update(&dvi::Op::EnableFont(1));
    /// assert_eq![values.f(), 1];
    /// values.update(&dvi::Op::Push);
    /// values.update(&dvi::Op::EnableFont(2));
    /// assert_eq![values.f(), 2];
    /// values.update(&dvi::Op::Pop);
    /// assert_eq![values.f(), 2];
    /// ```
    pub fn f(&self) -> u32 {
        self.f
    }
    /// Get the current value of the horizontal position, _h_.
    ///
    /// The value of _h_ is more complicated than other values.
    /// The DVI format includes a `set_char` command that typesets a character
    /// and then increases _h_ by the width of that character.
    /// The problem is that without looking up the font metric file,
    ///     the data structure doesn't know by how much to increase _h_.
    /// Thus, the value of _h_ in this data structure is an integer
    ///     plus a slice of characters whose widths should be added to
    ///     get the true value of _h_.
    ///
    /// ```
    /// let mut values: dvi::Values = Default::default();
    /// // move h 3 units to the right
    /// values.update(&dvi::Op::Right(1));
    /// // set the font
    /// values.update(&dvi::Op::EnableFont(2));
    /// // typeset DVI and move h each time
    /// values.update(&dvi::Op::TypesetChar{char: 'D' as u32, move_h: true});
    /// values.update(&dvi::Op::TypesetChar{char: 'V' as u32, move_h: true});
    /// values.update(&dvi::Op::TypesetChar{char: 'I' as u32, move_h: true});
    ///
    /// assert_eq![
    ///     values.h(),
    ///     (1_i32, [
    ///         ('D' as u32, 2_u32),
    ///         ('V' as u32, 2_u32),
    ///         ('I' as u32, 2_u32),
    ///     ].as_slice()),
    /// ];
    /// ```
    pub fn h(&self) -> (i32, &[(u32, u32)]) {
        (self.top.h, self.top.h_chars.as_slice())
    }
    /// Get the current value of the vertical position, _v_.
    pub fn v(&self) -> i32 {
        self.top.v
    }
    /// Get the current value of a variable.
    pub fn var(&self, var: Var) -> i32 {
        self.top.vars[var as usize]
    }
    /// Get the current value of the variable _w_.
    pub fn w(&self) -> i32 {
        self.var(Var::W)
    }
    /// Get the current value of the variable _x_.
    pub fn x(&self) -> i32 {
        self.var(Var::X)
    }
    /// Get the current value of the variable _y_.
    pub fn y(&self) -> i32 {
        self.var(Var::Y)
    }
    /// Get the current value of the variable _z_.
    pub fn z(&self) -> i32 {
        self.var(Var::Z)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run_serialize_test(want: Vec<u8>, op: Op) {
        let mut got = vec![];
        op.serialize(&mut got);
        assert_eq!(got, want);
    }

    fn run_deserialize_test(b: Vec<u8>, want: Op) {
        let mut result = Ok(());
        let got: Vec<Op> = Deserializer::new(&b, &mut result).into_iter().collect();
        assert_eq!(Ok(()), result);
        assert_eq!(got, vec![want]);
    }

    macro_rules! serde_tests {
        ( $( ($name: ident, [ $($elem: expr),+], $op: expr ), )+  ) => {
            $(
            mod $name {
                use super::*;

                #[test]
                fn test_serialize() {
                    let b = vec![ $( $elem, )+ ];
                    run_serialize_test(b, $op);
                }

                #[test]
                fn test_deserialize() {
                    let b = vec![ $( $elem, )+ ];
                    run_deserialize_test(b, $op);
                }
            }
            )+
        };
    }

    serde_tests!(
        (
            op_code_0,
            [0],
            Op::TypesetChar {
                char: 0,
                move_h: true
            }
        ),
        (
            op_code_1,
            [1],
            Op::TypesetChar {
                char: 1,
                move_h: true
            }
        ),
        (
            op_code_127,
            [127],
            Op::TypesetChar {
                char: 127,
                move_h: true
            }
        ),
        (
            op_code_128_case_1,
            [128, 128],
            Op::TypesetChar {
                char: 128,
                move_h: true
            }
        ),
        (
            op_code_128_case_2,
            [128, 129],
            Op::TypesetChar {
                char: 129,
                move_h: true
            }
        ),
        (
            op_code_128_case_3,
            [128, 255],
            Op::TypesetChar {
                char: 255,
                move_h: true
            }
        ),
        (
            op_code_129_case_1,
            [129, 1, 0],
            Op::TypesetChar {
                char: 256,
                move_h: true
            }
        ),
        (
            op_code_129_case_2,
            [129, 1, 2],
            Op::TypesetChar {
                char: 256 + 2,
                move_h: true
            }
        ),
        (
            op_code_129_case_3,
            [129, 255, 255],
            Op::TypesetChar {
                char: 256 * 256 - 1,
                move_h: true
            }
        ),
        (
            op_code_130_case_1,
            [130, 1, 0, 0],
            Op::TypesetChar {
                char: 256 * 256,
                move_h: true
            }
        ),
        (
            op_code_130_case_2,
            [130, 1, 2, 3],
            Op::TypesetChar {
                char: 256 * 256 + 2 * 256 + 3,
                move_h: true
            }
        ),
        (
            op_code_130_case_3,
            [130, 255, 255, 255],
            Op::TypesetChar {
                char: 256 * 256 * 256 - 1,
                move_h: true
            }
        ),
        (
            op_code_131_case_1,
            [131, 1, 0, 0, 0],
            Op::TypesetChar {
                char: 256 * 256 * 256,
                move_h: true
            }
        ),
        (
            op_code_131_case_2,
            [131, 1, 2, 3, 4],
            Op::TypesetChar {
                char: 256 * 256 * 256 + 2 * 256 * 256 + 3 * 256 + 4,
                move_h: true
            }
        ),
        (
            op_code_131_case_3,
            [131, 255, 255, 255, 255],
            Op::TypesetChar {
                char: u32::MAX,
                move_h: true
            }
        ),
        (
            op_code_132,
            [132, 0, 0, 0, 1, 0, 0, 0, 2],
            Op::TypesetRule {
                height: 1,
                width: 2,
                move_h: true
            }
        ),
        (
            op_code_133,
            [133, 1],
            Op::TypesetChar {
                char: 1,
                move_h: false,
            }
        ),
        (
            op_code_134,
            [134, 255, 255],
            Op::TypesetChar {
                char: 256 * 256 - 1,
                move_h: false,
            }
        ),
        (
            op_code_135,
            [135, 255, 255, 255],
            Op::TypesetChar {
                char: 256 * 256 * 256 - 1,
                move_h: false,
            }
        ),
        (
            op_code_136,
            [136, 255, 255, 255, 255],
            Op::TypesetChar {
                char: u32::MAX,
                move_h: false,
            }
        ),
        (
            op_code_137,
            [137, 0, 0, 0, 1, 0, 0, 0, 2],
            Op::TypesetRule {
                height: 1,
                width: 2,
                move_h: false,
            }
        ),
        (op_code_138, [138], Op::NoOp),
        (
            op_code_139,
            [
                139, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0,
                0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 255, 255, 255, 255, 0, 0, 0, 1
            ],
            Op::BeginPage {
                parameters: [1, 2, 3, 4, 5, 6, 7, 8, 9, -1],
                previous_begin_page: 1
            }
        ),
        (op_code_140, [140], Op::EndPage),
        (op_code_141, [141], Op::Push),
        (op_code_142, [142], Op::Pop),
        (op_code_143, [143, 2], Op::Right(2)),
        (op_code_144, [144, 1, 2], Op::Right(256 + 2)),
        (op_code_145, [145, 1, 0, 2], Op::Right(256 * 256 + 2)),
        (
            op_code_146,
            [146, 1, 0, 0, 2],
            Op::Right(256 * 256 * 256 + 2)
        ),
        (op_code_147, [147], Op::Move(Var::W)),
        (op_code_148, [148, 2], Op::SetVar(Var::W, 2)),
        (op_code_149, [149, 1, 2], Op::SetVar(Var::W, 256 + 2)),
        (
            op_code_150,
            [150, 1, 0, 2],
            Op::SetVar(Var::W, 256 * 256 + 2)
        ),
        (
            op_code_151,
            [151, 1, 0, 0, 2],
            Op::SetVar(Var::W, 256 * 256 * 256 + 2)
        ),
        (op_code_152, [152], Op::Move(Var::X)),
        (op_code_153, [153, 2], Op::SetVar(Var::X, 2)),
        (op_code_154, [154, 1, 2], Op::SetVar(Var::X, 256 + 2)),
        (
            op_code_155,
            [155, 1, 0, 2],
            Op::SetVar(Var::X, 256 * 256 + 2)
        ),
        (
            op_code_156,
            [156, 1, 0, 0, 2],
            Op::SetVar(Var::X, 256 * 256 * 256 + 2)
        ),
        // We have exhaustive tests for `Op::Down` in order to test all the
        // edge cases of i32 variable serding.
        (op_code_157_zero, [157, 0], Op::Down(0)),
        (op_code_157_least_positive, [157, 1], Op::Down(1)),
        (op_code_157_least_negative, [157, 255], Op::Down(-1)),
        (op_code_157_most_positive, [157, 127], Op::Down(127)),
        (op_code_157_most_negative, [157, 128], Op::Down(-128)),
        (
            op_code_158_least_positive,
            [158, 0, 128],
            Op::Down(2_i32.pow(7))
        ),
        (
            op_code_158_least_negative,
            [158, 255, 127],
            Op::Down(-1 * 2_i32.pow(7) - 1)
        ),
        (
            op_code_158_most_positive,
            [158, 127, 255],
            Op::Down(2_i32.pow(15) - 1)
        ),
        (
            op_code_158_most_negative,
            [158, 128, 0],
            Op::Down(-1 * 2_i32.pow(15))
        ),
        (
            op_code_159_least_positive,
            [159, 0, 128, 0],
            Op::Down(2_i32.pow(15))
        ),
        (
            op_code_159_least_negative,
            [159, 255, 127, 255],
            Op::Down(-1 * 2_i32.pow(15) - 1)
        ),
        (
            op_code_159_most_positive,
            [159, 127, 255, 255],
            Op::Down(2_i32.pow(23) - 1)
        ),
        (
            op_code_159_most_negative,
            [159, 128, 0, 0],
            Op::Down(-1 * 2_i32.pow(23))
        ),
        (
            op_code_160_least_positive,
            [160, 0, 128, 0, 0],
            Op::Down(2_i32.pow(23))
        ),
        (
            op_code_160_least_negative,
            [160, 255, 127, 255, 255],
            Op::Down(-1 * 2_i32.pow(23) - 1)
        ),
        (
            op_code_160_most_positive,
            [160, 127, 255, 255, 255],
            Op::Down(i32::MAX)
        ),
        (
            op_code_160_most_negative,
            [160, 128, 0, 0, 0],
            Op::Down(i32::MIN)
        ),
        (op_code_161, [161], Op::Move(Var::Y)),
        (op_code_162, [162, 2], Op::SetVar(Var::Y, 2)),
        (op_code_163, [163, 1, 2], Op::SetVar(Var::Y, 256 + 2)),
        (
            op_code_164,
            [164, 1, 0, 2],
            Op::SetVar(Var::Y, 256 * 256 + 2)
        ),
        (
            op_code_165,
            [165, 1, 0, 0, 2],
            Op::SetVar(Var::Y, 256 * 256 * 256 + 2)
        ),
        (op_code_166, [166], Op::Move(Var::Z)),
        (op_code_167, [167, 2], Op::SetVar(Var::Z, 2)),
        (op_code_168, [168, 1, 2], Op::SetVar(Var::Z, 256 + 2)),
        (
            op_code_169,
            [169, 1, 0, 2],
            Op::SetVar(Var::Z, 256 * 256 + 2)
        ),
        (
            op_code_170,
            [170, 1, 0, 0, 2],
            Op::SetVar(Var::Z, 256 * 256 * 256 + 2)
        ),
        (op_code_171, [171], Op::EnableFont(0)),
        (op_code_234, [234], Op::EnableFont(63)),
        (op_code_235, [235, 64], Op::EnableFont(64)),
        (op_code_236, [236, 1, 0], Op::EnableFont(256)),
        (op_code_237, [237, 1, 0, 0], Op::EnableFont(256 * 256)),
        (
            op_code_238,
            [238, 1, 0, 0, 0],
            Op::EnableFont(256 * 256 * 256)
        ),
        (
            op_code_239,
            [239, 5, 0, 1, 2, 3, 4],
            Op::Extension(vec![0, 1, 2, 3, 4])
        ),
        (
            op_code_243,
            [243, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 5, 99, 109, 114, 49, 48],
            Op::DefineFont {
                number: 1,
                checksum: 2,
                at_size: 3,
                design_size: 4,
                area: "".to_string(),
                name: "cmr10".to_string(),
            }
        ),
        (
            op_code_244,
            [244, 1, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 5, 0, 99, 109, 114, 49, 48],
            Op::DefineFont {
                number: 256 + 1,
                checksum: 2,
                at_size: 3,
                design_size: 4,
                area: "cmr10".to_string(),
                name: "".to_string(),
            }
        ),
        (
            op_code_245,
            [245, 1, 1, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 2, 3, 99, 109, 114, 49, 48],
            Op::DefineFont {
                number: 256 * 256 + 256 + 1,
                checksum: 2,
                at_size: 3,
                design_size: 4,
                area: "cm".to_string(),
                name: "r10".to_string(),
            }
        ),
        (
            op_code_246,
            [246, 1, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0],
            Op::DefineFont {
                number: 256 * 256 * 256,
                checksum: 2,
                at_size: 3,
                design_size: 4,
                area: "".to_string(),
                name: "".to_string(),
            }
        ),
        (
            op_code_247,
            [247, 2, 0, 0, 0, 3, 0, 0, 0, 5, 1, 2, 3, 4, 3, 65, 66, 67],
            Op::Preamble {
                dvi_format: 2,
                unit_numerator: 3,
                unit_denominator: 5,
                magnification: 1 * 256 * 256 * 256 + 2 * 256 * 256 + 3 * 256 + 4,
                comment: "ABC".to_string(),
            }
        ),
        (
            op_code_248,
            [
                248, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 7,
                0, 8
            ],
            Op::BeginPostamble {
                final_begin_page: 1,
                unit_numerator: 2,
                unit_denominator: 3,
                magnification: 4,
                largest_height: 5,
                largest_width: 6,
                max_stack_depth: 7,
                num_pages: 8,
            }
        ),
        (
            op_code_249,
            [249, 1, 0, 0, 0, 2, 223, 223, 223, 223, 223, 223],
            Op::EndPostamble {
                dvi_format: 1,
                postamble: 2,
                num_223_bytes: 6
            }
        ),
    );

    #[derive(Default)]
    struct WantValues {
        f: u32,
        h: i32,
        h_chars: Vec<(u32, u32)>,
        v: i32,
        w: i32,
        x: i32,
        y: i32,
        z: i32,
    }

    fn run_values_test(ops: Vec<Op>, changed: Vec<bool>, want_values: WantValues) {
        let mut values: Values = Default::default();
        for (i, op) in ops.into_iter().enumerate() {
            let want_changed = changed[i];
            let got_changed = values.update(&op);
            assert_eq!(want_changed, got_changed);
        }
        assert_eq!(values.f(), want_values.f, "unexpected f value");
        assert_eq!(values.h().0, want_values.h, "unexpected h.0 value");
        assert_eq!(values.h().1, want_values.h_chars, "unexpected h.1 value");
        assert_eq!(values.v(), want_values.v, "unexpected v value");
        assert_eq!(values.w(), want_values.w, "unexpected w value");
        assert_eq!(values.x(), want_values.x, "unexpected x value");
        assert_eq!(values.y(), want_values.y, "unexpected y value");
        assert_eq!(values.z(), want_values.z, "unexpected z value");
    }

    macro_rules! values_tests {
        ( $(
            (
                $name: ident,
                [ $( $op: expr ),+ ],
                [ $( $changed: expr ),+ ],
                $( f: $want_f: expr, )?
                $( h: $want_h: expr, )?
                $( h_chars: $want_h_chars: expr, )?
                $( v: $want_v: expr, )?
                $( w: $want_w: expr, )?
                $( x: $want_x: expr, )?
                $( y: $want_y: expr, )?
                $( z: $want_z: expr, )?
            ),
        )+ ) => {
            $(
                #[test]
                fn $name() {
                    let ops = vec![ $( $op, )+ ];
                    let changed = vec![ $( $changed, )+ ];
                    let mut want_values: WantValues = Default::default();
                    $( want_values.f = $want_f; )?
                    $( want_values.h = $want_h; )?
                    $( want_values.h_chars = $want_h_chars; )?
                    $( want_values.v = $want_v; )?
                    $( want_values.w = $want_w; )?
                    $( want_values.x = $want_x; )?
                    $( want_values.y = $want_y; )?
                    $( want_values.z = $want_z; )?
                    run_values_test(ops, changed, want_values);
                }
            )+
        };
    }

    values_tests!(
        (
            noop,
            [Op::NoOp],
            [false],
            f: 0,
        ),
        (
            extension,
            [Op::Extension(vec![1,2,3])],
            [false],
            f: 0,
        ),
        (
            define_font,
            [Op::DefineFont{ number: 0, checksum: 1, at_size: 2, design_size: 3, area: "".to_string(), name: "".to_string() }],
            [false],
            f: 0,
        ),
        (
            preamble,
            [Op::Preamble{ dvi_format: 1, unit_numerator: 2, unit_denominator: 3, magnification: 4, comment: "".to_string() }],
            [false],
            f: 0,
        ),
        (
            begin_postamble,
            [Op::BeginPostamble{ final_begin_page: 1, unit_numerator: 2, unit_denominator: 3, magnification: 4, largest_height: 5, largest_width: 6, max_stack_depth: 7, num_pages: 8 }],
            [false],
            f: 0,
        ),
        (
            end_postamble,
            [Op::EndPostamble{ postamble: 1, dvi_format: 2, num_223_bytes: 3 }],
            [false],
            f: 0,
        ),
        (
            enable_font_1,
            [Op::EnableFont(3), Op::EnableFont(3)],
            [true, false],
            f: 3,
        ),
        (
            enable_font_2,
            [Op::EnableFont(3), Op::Push, Op::EnableFont(5), Op::Pop],
            [true, false, true, false],
            f: 5,
        ),
        (
            var_w_1,
            [Op::SetVar(Var::W, 5), Op::SetVar(Var::W, 5)],
            [true, true],
            h: 10,
            w: 5,
        ),
        (
            var_w_2,
            [Op::SetVar(Var::W, 5), Op::Push, Op::SetVar(Var::W, 3), Op::Pop],
            [true, false, true, true],
            h: 5,
            w: 5,
        ),
        (
            var_w_3,
            [Op::SetVar(Var::W, 5), Op::Push, Op::SetVar(Var::W, 5), Op::Pop],
            [true, false, true, true],
            h: 5,
            w: 5,
        ),
        (
            var_w_4,
            [Op::SetVar(Var::W, 5), Op::SetVar(Var::W, 0), Op::SetVar(Var::W, 0)],
            [true, true, false],
            h: 5,
            w: 0,
        ),
        (
            var_w_5,
            [Op::SetVar(Var::W, 5), Op::Move(Var::W)],
            [true, true],
            h: 10,
            w: 5,
        ),
        (
            var_w_6,
            [Op::SetVar(Var::W, 0), Op::Move(Var::W)],
            [false, false],
            h: 0,
            w: 0,
        ),
        (
            var_x,
            [Op::SetVar(Var::X, 5), Op::SetVar(Var::X, 5), Op::Move(Var::X)],
            [true, true, true],
            h: 15,
            x: 5,
        ),
        (
            var_y,
            [Op::SetVar(Var::Y, 5), Op::SetVar(Var::Y, 5), Op::Move(Var::Y)],
            [true, true, true],
            v: 15,
            y: 5,
        ),
        (
            var_z,
            [Op::SetVar(Var::Z, 5), Op::SetVar(Var::Z, 5), Op::Move(Var::Z)],
            [true, true, true],
            v: 15,
            z: 5,
        ),
        (
            right_1,
            [Op::Right(5)],
            [true],
            h: 5,
        ),
        (
            right_2,
            [Op::Right(0)],
            [false],
            h: 0,
        ),
        (
            down_1,
            [Op::Down(5)],
            [true],
            v: 5,
        ),
        (
            down_2,
            [Op::Down(0)],
            [false],
            v: 0,
        ),
        (
            rule_1,
            [Op::TypesetRule{height: 2, width: 3, move_h: true}],
            [true],
            h: 3,
        ),
        (
            rule_2,
            [Op::TypesetRule{height: 2, width: 0, move_h: true}],
            [false],
            h: 0,
        ),
        (
            rule_3,
            [Op::TypesetRule{height: 2, width: 3, move_h: false}],
            [false],
            h: 0,
        ),
        (
            begin_page_1,
            [
                Op::SetVar(Var::W, 1),
                Op::SetVar(Var::X, 2),
                Op::SetVar(Var::Y, 3),
                Op::SetVar(Var::Z, 4),
                Op::BeginPage { parameters: Default::default(), previous_begin_page: 1 }
            ],
            [true, true, true, true, true],
            h: 0,
            v: 0,
            w: 0,
            x: 0,
            y: 0,
            z: 0,
        ),
        (
            begin_page_2,
            [
                Op::SetVar(Var::W, 0),
                Op::SetVar(Var::X, 0),
                Op::SetVar(Var::Y, 0),
                Op::SetVar(Var::Z, 0),
                Op::BeginPage { parameters: Default::default(), previous_begin_page: 1 }
            ],
            [false, false, false, false, false],
            h: 0,
            v: 0,
            w: 0,
            x: 0,
            y: 0,
            z: 0,
        ),
        (
            end_page,
            [Op::EndPage],
            [false],
            h: 0,
        ),
        (
            typeset_char_1,
            [Op::TypesetChar{char: 1, move_h: true}],
            [true],
            h: 0,
            h_chars: vec![(1, 0)],
        ),
        (
            typeset_char_2,
            [Op::TypesetChar{char: 1, move_h: false}],
            [false],
            h: 0,
            h_chars: vec![],
        ),
        (
            typeset_char_3,
            [Op::Push, Op::TypesetChar{char: 1, move_h: true}, Op::Pop],
            [false, true, true],
            h: 0,
            h_chars: vec![],
        ),
    );
}
