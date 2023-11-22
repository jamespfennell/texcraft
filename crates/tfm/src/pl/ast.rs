//! Abstract syntax tree for property list files
//!
//! The property list [AST](Ast) is a fully typed representation of a property list file.

use super::cst;
use super::error::Error;
use crate::{ligkern::lang::PostLigOperation, Char, Face, Number};

/// Abstract syntax tree for property list files
///
/// This is simply a list of [`Root`] nodes.
#[derive(Debug, PartialEq, Eq)]
pub struct Ast(pub Vec<Root>);

impl Ast {
    /// Build an AST directly from source code.
    pub fn build(source: &str) -> (Ast, Vec<Error>) {
        let lexer = super::lexer::Lexer::new(source);
        let mut errors = vec![];
        let cst = cst::Cst::build_from_lexer(lexer, &mut errors);
        let ast = Ast::build_from_cst(cst, &mut errors);
        (Ast(ast), errors)
    }

    /// Build an AST from a CST.
    pub fn build_from_cst(cst: cst::Cst, errors: &mut Vec<Error>) -> Vec<Root> {
        cst.0
            .into_iter()
            .filter_map(|c| Root::build(c, errors))
            .collect()
    }
}

/// Value of a leaf node in the AST that contains a single piece of data.
///
/// An example of this node is the `CHECKSUM` entry, which just contains a 32-bit checksum as data.
#[derive(PartialEq, Eq, Debug)]
pub struct SingleValue<D> {
    /// Data in this leaf node.
    pub data: D,
    // TODO: open paren spans? key spans? data spans?
    // TODO pub span: std::ops::Range<usize>,
}

/// Value of a leaf node in the AST that contains two pieces of data.
///
/// An example of this node is the `HEADER` entry, which contains a 8-bit header index
/// and a 32-bit value.
#[derive(PartialEq, Eq, Debug)]
pub struct TupleValue<D, E> {
    /// Data in this leaf node.
    pub data: (D, E),
    // TODO pub spans: (std::ops::Range<usize>, std::ops::Range<usize>),
}

/// Value of a branch node in the AST.
///
/// A branch node contains a property list and optionally a piece of data.
/// For example, `CHARACTER` nodes specify a character in the data, and then
///     a property list of [`Character`] nodes.
#[derive(PartialEq, Eq, Debug)]
pub struct Branch<D, E> {
    /// Data in this branch node.
    pub data: D,
    // TODO: open paren spans? key spans? data spans?
    /// Elements of the property list.
    pub children: Vec<E>,
}

/// The only reason we have this trait is to make it possible to invoke the `from`
/// associated function of a type without specifying the type itself.
/// I.e., instead of writing `LeafValue::from` we can write `FromCstNode::from`.
/// This makes the `node_impl!` macro simpler to implement because we don't need to provide
/// the type name to the macro.
trait FromCstNode: Sized {
    fn from(p: cst::RegularNodeValue, errors: &mut Vec<Error>) -> Self;
}

impl<D: Data> FromCstNode for SingleValue<D> {
    fn from(p: cst::RegularNodeValue, errors: &mut Vec<Error>) -> Self {
        let (mut input, _) = Input::new(p, errors);
        Self {
            data: D::build(&mut input),
        }
    }
}

impl<D: From<u8>, E: Data> FromCstNode for TupleValue<D, E> {
    fn from(p: cst::RegularNodeValue, errors: &mut Vec<Error>) -> Self {
        let (mut input, _) = Input::new(p, errors);
        let first = u8::build(&mut input);
        let second = E::build(&mut input);
        Self {
            data: (D::from(first), second),
        }
    }
}

impl<D: Data, E: Node> FromCstNode for Branch<D, E> {
    fn from(p: cst::RegularNodeValue, errors: &mut Vec<Error>) -> Self {
        {
            let (mut input, children) = Input::new(p, errors);
            Branch::<D, E> {
                data: D::build(&mut input),
                children: children
                    .into_iter()
                    .filter_map(|c| Node::build(c, errors))
                    .collect(),
            }
        }
    }
}

trait Node: Sized {
    fn build_regular(p: cst::RegularNodeValue, errors: &mut Vec<Error>) -> Option<Self>;
    fn build_comment(_: Vec<cst::BalancedElem>) -> Self;
    fn build(n: cst::Node, errors: &mut Vec<Error>) -> Option<Self> {
        match n.value {
            cst::NodeValue::Comment(c) => Some(Node::build_comment(c)),
            cst::NodeValue::Regular(r) => Node::build_regular(r, errors),
        }
    }
}

macro_rules! node_impl {
    ( $type: ident, $( ($key: ident, $str: expr, $variant: ident $(, $prefix: expr )? ), )+ ) => {

        impl $type {
            $(
                pub const $key: &str = $str;
            )+

            pub const ALL_PROPERTY_NAMES: &[&'static str] = &[$( $str, )+];
        }

        impl Node for $type {
            fn build_regular(mut r: cst::RegularNodeValue, errors: &mut Vec<Error>) -> Option<Self> {
                r.key.make_ascii_uppercase();
                match r.key.as_str() {
                    $(
                        $type::$key => Some($type::$variant( $( $prefix, )? FromCstNode::from(r, errors))),
                    )+
                    _ => {
                        errors.push(Error::InvalidPropertyName {
                            name: r.key.into(),
                            name_span: r.key_span.clone(),
                            allowed_property_names: $type::ALL_PROPERTY_NAMES,
                        });
                        None
                    },
                }
            }
            fn build_comment(v: Vec<cst::BalancedElem>) -> Self {
                $type::Comment(v)
            }
        }
    };
}

/// A root node in a property list file.
///
/// The documentation on each variant is based on the documentation in PFtoTF.2014.9.
#[derive(PartialEq, Eq, Debug)]
pub enum Root {
    /// The checksum value is used to identify a particular version of a font;
    ///     it should match the check sum value stored with the font itself.
    /// An explicit check sum of zero is used to bypass check sum testing.
    /// If no checksum is specified in the PL file,
    ///     PLtoTF will compute the checksum that METAFONT would compute from the same data
    Checksum(SingleValue<u32>),

    /// The design size, which should be in the range, `[1.0, 2048)`,
    ///     represents the default amount by which all quantities will be scaled
    ///     if the font is not loaded with an `at` specification.
    /// For example, if one says `\font\A=cmr10 at 15pt` in TeX language,
    ///     the design size in the TFM file is ignored and effectively replaced by 15 points;
    ///     but if one simply says `\font\A=cmr10` the stated design size is used.
    /// This quantity is always in units of printer's points.
    DesignSize(SingleValue<Number>),

    /// The design units specifies how many units equals the design size
    ///     (or the eventual ‘at’ size, if the font is being scaled).
    /// For example, suppose
    /// you have a font that has been digitized with 600 pixels per em,
    ///     and the design size is one em;
    ///     then you could say `(DESIGNUNITS R 600)` if you wanted to give all of your measurements in units of pixels.
    DesignUnits(SingleValue<Number>),

    /// A string that identifies the correspondence between the numeric codes and font characters.
    /// Its length must be less than 40.
    /// (TeX ignores this information, but other software programs make use of it.)
    CodingScheme(SingleValue<String>),

    /// A string that identifies the name of the family to which this font belongs, e.g., ‘HELVETICA’.
    /// Its length must be less than 20.
    /// (TeX ignores this information; but it is needed, for example, when converting DVI files to PRESS files
    /// for Xerox equipment.)
    Family(SingleValue<String>),

    /// This value is a subsidiary identification of the font within its family.
    /// For example, bold italic condensed fonts might have the same family
    /// name as light roman extended fonts, differing only in their face byte.
    /// (TeX ignores this information;
    ///     but it is needed, for example, when converting DVI files to PRESS files for Xerox equipment.)
    Face(SingleValue<Face>),

    /// If true,
    ///     character codes less than 128 cannot lead to codes of 128 or more via ligatures or
    ///     char lists or extensible characters.
    /// (TeX82 ignores this flag, but older versions of TeX would only accept TFM files that were seven-bit safe.)
    /// PLtoTF computes the correct value of this flag and gives an
    ///     error message only if a claimed "true" value is incorrect.
    SevenBitSafeFlag(SingleValue<bool>),

    /// Value of a header.
    /// The one-byte value should be at least 18.
    /// The four-byte value goes into the header word whose index is the one-byte value;
    ///     for example, to set `header[18]=1`, one may write `(HEADER D 18 O 1)`.
    /// This notation is used for header information that is presently unnamed.
    /// (TeX ignores it.)
    Header(TupleValue<u8, u32>),

    /// Font dimensions property list.
    FontDimension(Branch<(), FontDimension>),

    /// A lig table.
    LigTable(Branch<(), LigTable>),

    /// If the boundary character appears in a lig table command (`LIGTABLE`),
    ///     it matches "end of word" as well as itself.
    /// If no boundary character is given and no `LABEL BOUNDARYCHAR` occurs within a lig table,
    ///     word boundaries will not affect ligatures or kerning.
    BoundaryChar(SingleValue<Char>),

    /// Metrics for a character in the font.
    /// The value specifies the character and
    ///     the property list of [`Character`] nodes specifies metrics for the character.
    Character(Branch<Char, Character>),

    /// A comment that is ignored.
    Comment(Vec<cst::BalancedElem>),
}

node_impl!(
    Root,
    (CHECKSUM, "CHECKSUM", Checksum),
    (DESIGN_SIZE, "DESIGNSIZE", DesignSize),
    (DESIGN_UNITS, "DESIGNUNITS", DesignUnits),
    (CODING_SCHEME, "CODINGSCHEME", CodingScheme),
    (FAMILY, "FAMILY", Family),
    (FACE, "FACE", Face),
    (SEVEN_BIT_SAFE_FLAG, "SEVENBITSAFEFLAG", SevenBitSafeFlag),
    (HEADER, "HEADER", Header),
    (FONT_DIMENSION, "FONTDIMEN", FontDimension),
    (LIG_TABLE, "LIGTABLE", LigTable),
    (BOUNDARY_CHAR, "BOUNDARYCHAR", BoundaryChar),
    (CHARACTER, "CHARACTER", Character),
);

/// An element of a `FONTDIMEN` property list.
///
/// The property names allowed in a `FONTDIMEN` property list correspond to various TeX parameters,
///     each of which has a (real) numeric value.
/// All of the parameters except `SLANT` are in design units.
///
/// The documentation on each variant is based on the documentation in PFtoTF.2014.11.
#[derive(PartialEq, Eq, Debug)]
pub enum FontDimension {
    Slant(SingleValue<Number>),
    Space(SingleValue<Number>),
    Stretch(SingleValue<Number>),
    Shrink(SingleValue<Number>),
    XHeight(SingleValue<Number>),
    Quad(SingleValue<Number>),
    ExtraSpace(SingleValue<Number>),
    Num1(SingleValue<Number>),
    Num2(SingleValue<Number>),
    Num3(SingleValue<Number>),
    Denom1(SingleValue<Number>),
    Denom2(SingleValue<Number>),
    Sup1(SingleValue<Number>),
    Sup2(SingleValue<Number>),
    Sup3(SingleValue<Number>),
    Sub1(SingleValue<Number>),
    Sub2(SingleValue<Number>),
    SupDrop(SingleValue<Number>),
    SubDrop(SingleValue<Number>),
    Delim1(SingleValue<Number>),
    Delim2(SingleValue<Number>),
    AxisHeight(SingleValue<Number>),
    DefaultRuleThickness(SingleValue<Number>),
    BigOpSpacing1(SingleValue<Number>),
    BigOpSpacing2(SingleValue<Number>),
    BigOpSpacing3(SingleValue<Number>),
    BigOpSpacing4(SingleValue<Number>),
    BigOpSpacing5(SingleValue<Number>),

    /// The notation `PARAMETER n` provides another way to specify the nth parameter;
    ///     for example, `(PARAMETER D 1 R −.25)` is another way to specify that the `SLANT` is −0.25.
    /// The value of n must be positive and less than max param words.
    Parameter(TupleValue<u8, Number>),

    /// A comment that is ignored.
    Comment(Vec<cst::BalancedElem>),
}

node_impl!(
    FontDimension,
    (SLANT, "SLANT", Slant),
    (SPACE, "SPACE", Space),
    (STRETCH, "STRETCH", Stretch),
    (SHRINK, "SHRINK", Shrink),
    (X_HEIGHT, "XHEIGHT", XHeight),
    (QUAD, "QUAD", Quad),
    (EXTRA_SPACE, "EXTRASPACE", ExtraSpace),
    (NUM_1, "NUM1", Num1),
    (NUM_2, "NUM2", Num2),
    (NUM_3, "NUM3", Num3),
    (DENOM_1, "DENOM1", Denom1),
    (DENOM_2, "DENOM2", Denom2),
    (SUP_1, "SUP1", Sup1),
    (SUP_2, "SUP2", Sup2),
    (SUP_3, "SUP3", Sup3),
    (SUB_1, "SUB1", Sub1),
    (SUB_2, "SUB2", Sub2),
    (SUP_DROP, "SUPDROP", SupDrop),
    (SUB_DROP, "SUBDROP", SubDrop),
    (DELIM_1, "DELIM1", Delim1),
    (DELIM_2, "DELIM2", Delim2),
    (AXIS_HEIGHT, "AXISHEIGHT", AxisHeight),
    (
        DEFAULT_RULE_THICKNESS,
        "DEFAULTRULETHICKNESS",
        DefaultRuleThickness
    ),
    (BIG_OP_SPACING_1, "BIGOPSPACE1", BigOpSpacing1),
    (BIG_OP_SPACING_2, "BIGOPSPACE2", BigOpSpacing2),
    (BIG_OP_SPACING_3, "BIGOPSPACE3", BigOpSpacing3),
    (BIG_OP_SPACING_4, "BIGOPSPACE4", BigOpSpacing4),
    (BIG_OP_SPACING_5, "BIGOPSPACE5", BigOpSpacing5),
    (PARAMETER, "PARAMETER", Parameter),
);

/// An element of a `CHARACTER` property list.
///
/// The documentation on each variant is based on the documentation in PFtoTF.2014.12.
#[derive(PartialEq, Eq, Debug)]
pub enum Character {
    /// The character's width in design units.
    Width(SingleValue<Number>),

    /// The character's height in design units.
    Height(SingleValue<Number>),

    /// The character's depth in design units.
    Depth(SingleValue<Number>),

    /// The character's italic correction in design units.
    ItalicCorrection(SingleValue<Number>),

    /// Specifies the character that follows the present one in a "charlist."
    /// The value must be the number of a character in the font,
    ///     and there must be no infinite cycles of supposedly larger and larger characters.
    NextLarger(SingleValue<u8>),

    /// Specifies an extensible character.
    /// This option and `NEXTLARGER` are mutually exclusive;
    ///     i.e., they cannot both be used within the same `CHARACTER` list.
    ExtensibleCharacter(Branch<(), ExtensibleCharacter>),

    /// A comment that is ignored.
    Comment(Vec<cst::BalancedElem>),
}

node_impl!(
    Character,
    (WIDTH, "CHARWD", Width),
    (HEIGHT, "CHARHT", Height),
    (DEPTH, "CHARDP", Depth),
    (ITALIC_CORRECTION, "CHARIC", ItalicCorrection),
    (NEXT_LARGER, "NEXTLARGER", NextLarger),
    (EXTENSIBLE_CHARACTER, "VARCHAR", ExtensibleCharacter),
);

/// An element of a `VARCHAR` property list.
///
/// The documentation on each variant is based on the documentation in PFtoTF.2014.12.
#[derive(PartialEq, Eq, Debug)]
pub enum ExtensibleCharacter {
    /// The top piece of an extensible character, or 0 if the top piece is absent.
    Top(SingleValue<u8>),

    /// The middle piece of an extensible character, or 0 if the top piece is absent.
    Middle(SingleValue<u8>),

    /// The bottom piece of an extensible character, or 0 if the top piece is absent.
    Bottom(SingleValue<u8>),

    /// The replicated piece of an extensible character, or 0 if it is absent.
    Replicated(SingleValue<u8>),

    /// A comment that is ignored.
    Comment(Vec<cst::BalancedElem>),
}

node_impl!(
    ExtensibleCharacter,
    (TOP, "TOP", Top),
    (MIDDLE, "MID", Middle),
    (BOTTOM, "BOT", Bottom),
    (REPLICATED, "REP", Replicated),
);

/// An element of a `LIGTABLE` property list.
///
/// A lig table property list contains elements of four kinds,
///     specifying a program in a simple command language that TeX uses for ligatures and kerns.
/// If several lig table lists appear, they are effectively concatenated into a single list.
///
/// The documentation here and on each variant is based on the documentation in PFtoTF.2014.13.
#[derive(PartialEq, Eq, Debug)]
pub enum LigTable {
    /// A label specifies that the program for the stated character value starts here.
    /// The integer must be the number of a character in the font;
    ///     its `CHARACTER` property list must not have a `NEXTLARGER` or `VARCHAR` field.
    /// At least one `LIG` or `KRN` step must follow.
    ///
    /// `LABEL BOUNDARYCHAR` means that the program for beginning-of-word ligatures starts here.
    Label(SingleValue<LigTableLabel>),

    /// The instruction `(LIG c r)` means,
    ///     "If the next character is c,
    ///     then insert character r and possibly delete the current character and/or c;
    ///     otherwise go on to the next instruction."
    /// Characters r and c must be present in the font.
    ///
    /// The `LIG` keyword may be immediately preceded or followed by a slash,
    ///     and then immediately followed by > characters not exceeding the number of slashes.
    /// Thus there are eight possible forms:
    ///
    /// | keyword   | retain  | move to  | [`PostLigOperation`] value |
    /// |-----------|---------|----------|--------|
    /// | `LIG`     | neither | inserted | [`PostLigOperation::RetainNeitherMoveToInserted`] |
    /// | `/LIG`    | left    | left     | [`PostLigOperation::RetainLeftMoveNowhere`]       |
    /// | `/LIG>`   | left    | inserted | [`PostLigOperation::RetainLeftMoveToInserted`]    |
    /// | `LIG/`    | right   | inserted | [`PostLigOperation::RetainRightMoveToInserted`]   |
    /// | `LIG/>`   | right   | right    | [`PostLigOperation::RetainRightMoveToRight`]      |
    /// | `/LIG/`   | both    | left     | [`PostLigOperation::RetainBothMoveToInserted`]    |
    /// | `/LIG/>`  | both    | inserted | [`PostLigOperation::RetainBothMoveToInserted`]    |
    /// | `/LIG/>>` | both    | right    | [`PostLigOperation::RetainBothMoveToRight`]       |
    ///
    /// The slashes specify retention of the left or right original character; the > signs specify passing over
    /// the result without further ligature processing.
    Lig(PostLigOperation, TupleValue<Char, Char>),

    /// A kern instruction `(KRN c r)` means,
    ///     "If the next character is c, then insert a blank space of width r between the current character and c;
    ///     otherwise go on to the next instruction."
    /// The value of r, which is in design units, is often negative.
    /// Character code c must exist in the font.
    Kern(TupleValue<Char, Number>),

    /// A stop instruction ends a ligature/kern program.
    /// It must follow either a `LIG` or `KRN` instruction, not a `LABEL` or `STOP` or `SKIP`.
    Stop(SingleValue<()>),

    /// A skip instruction specifies continuation of a ligature/kern program after
    /// the specified number of LIG or KRN steps has been skipped over.
    /// The number of subsequent LIG and KRN instructions must therefore exceed this specified amount.
    Skip(SingleValue<u8>),

    /// A comment that is ignored.
    Comment(Vec<cst::BalancedElem>),
}

/// Value of a label in a lig table.
#[derive(PartialEq, Eq, Debug)]
pub enum LigTableLabel {
    /// A specific character.
    Char(Char),
    /// The boundary character.
    BoundaryChar,
}

impl Data for LigTableLabel {
    fn build(input: &mut Input) -> Self {
        match input.peek() {
            Some('B' | 'b') => {
                input.next();
                LigTableLabel::BoundaryChar
            }
            _ => LigTableLabel::Char(Data::build(input)),
        }
    }
}

node_impl!(
    LigTable,
    (LABEL, "LABEL", Label),
    (
        LIG_1,
        "LIG",
        Lig,
        PostLigOperation::RetainNeitherMoveToInserted
    ),
    (LIG_2, "/LIG", Lig, PostLigOperation::RetainLeftMoveNowhere),
    (
        LIG_3,
        "/LIG>",
        Lig,
        PostLigOperation::RetainLeftMoveToInserted
    ),
    (
        LIG_4,
        "LIG/",
        Lig,
        PostLigOperation::RetainRightMoveToInserted
    ),
    (
        LIG_5,
        "LIG/>",
        Lig,
        PostLigOperation::RetainRightMoveToRight
    ),
    (LIG_6, "/LIG/", Lig, PostLigOperation::RetainBothMoveNowhere),
    (
        LIG_7,
        "/LIG/>",
        Lig,
        PostLigOperation::RetainBothMoveToInserted
    ),
    (
        LIG_8,
        "/LIG/>>",
        Lig,
        PostLigOperation::RetainBothMoveToRight
    ),
    (KERN, "KRN", Kern),
    (STOP, "STOP", Stop),
    (SKIP, "SKIP", Skip),
);

trait Data {
    fn build(input: &mut Input) -> Self;
}

struct Input<'a> {
    raw_data: String,
    raw_data_offset: usize,
    raw_data_span: std::ops::Range<usize>,
    errors: &'a mut Vec<Error>,
}

impl<'a> Input<'a> {
    fn new(p: cst::RegularNodeValue, errors: &'a mut Vec<Error>) -> (Self, Vec<cst::Node>) {
        (
            Input {
                raw_data: p.data,
                raw_data_offset: 0,
                errors,
                raw_data_span: p.data_span,
            },
            p.children,
        )
    }
    fn skip_error(&mut self, error: Error) {
        self.errors.push(error);
        self.skip_to_end();
    }
    fn skip_to_end(&mut self) {
        self.raw_data_offset = self.raw_data.len();
        self.raw_data_span.start = self.raw_data_span.end;
    }
    fn peek(&self) -> Option<char> {
        self.raw_data[self.raw_data_offset..].chars().next()
    }
    fn consume_spaces(&mut self) {
        while self.raw_data[self.raw_data_offset..].starts_with(' ') {
            self.raw_data_offset += 1;
            self.raw_data_span.start += 1;
        }
    }
    fn last_span(&self, last: Option<char>) -> std::ops::Range<usize> {
        let end_span = self.raw_data_span.start;
        match last {
            None => end_span..end_span,
            Some(_) => end_span - 1..end_span,
        }
    }
    fn take_string(&mut self) -> String {
        assert_eq!(self.raw_data_offset, 0);
        let mut res = String::new();
        std::mem::swap(&mut res, &mut self.raw_data);
        self.skip_to_end();
        res
    }
}

impl<'a> Iterator for Input<'a> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        let res = self.raw_data[self.raw_data_offset..].chars().next();
        if let Some(c) = res {
            self.raw_data_offset += c.len_utf8();
            self.raw_data_span.start += c.len_utf8();
        }
        res
    }
}

impl Data for () {
    fn build(_: &mut Input) -> Self {}
}

impl Data for u32 {
    // PLtoTF.2014.59-60
    fn build(input: &mut Input) -> Self {
        let radix = match input.next() {
            Some('O' | 'o') => 8,
            Some('H' | 'h') => 16,
            c => {
                input.skip_error(Error::InvalidPrefixForInteger {
                    span: input.last_span(c),
                });
                return 0;
            }
        };
        input.consume_spaces();
        let start_span = input.raw_data_span.start;
        let mut acc: u32 = 0;
        while let Some(c) = input.next() {
            let n: u32 = match c.to_digit(16) {
                None => break,
                Some(d) => d,
            };
            if n >= radix {
                input.skip_error(Error::InvalidOctalDigit {
                    c,
                    span: input.raw_data_span.start - 1,
                });
                break;
            }
            match acc.checked_mul(radix).and_then(|acc| acc.checked_add(n)) {
                None => {
                    // Overflow has occurred.
                    // We advance to the end of the integer constant so that the span in the error is
                    // most accurate.
                    while let Some(c) = input.peek() {
                        match c.to_digit(radix) {
                            None => break,
                            Some(_) => input.next(),
                        };
                    }
                    let end_span = input.raw_data_span.start;
                    input.skip_error(Error::IntegerTooBig {
                        span: start_span..end_span,
                    });
                    break;
                }
                Some(new_acc) => acc = new_acc,
            }
        }
        acc
    }
}

impl Data for u8 {
    // PLtoTF.2014.51
    fn build(input: &mut Input) -> Self {
        let parse_number = |input: &mut Input, radix: u8| {
            input.consume_spaces();
            let start_span = input.raw_data_span.start;
            let mut acc: u8 = 0;
            while let Some(c) = input.peek() {
                let n: u8 = match c.to_digit(radix as u32) {
                    None => break,
                    Some(d) => d.try_into().unwrap(),
                };
                input.next();
                match acc.checked_mul(radix).and_then(|l| l.checked_add(n)) {
                    None => {
                        // Overflow has occurred.
                        // We advance to the end of the integer constant so that the span in the error is
                        // most accurate.
                        while let Some(c) = input.peek() {
                            match c.to_digit(radix as u32) {
                                None => break,
                                Some(_) => input.next(),
                            };
                        }
                        let end_span = input.raw_data_span.start;
                        input.skip_error(Error::SmallIntegerTooBig {
                            span: start_span..end_span,
                            radix,
                        });
                        acc = 0;
                        break;
                    }
                    Some(new_acc) => acc = new_acc,
                }
            }
            acc
        };
        let u = match input.next() {
            // PLtoTF.2014.52
            Some('C' | 'c') => {
                input.consume_spaces();
                match input.next() {
                    Some(c @ ' '..='~') => (c as usize).try_into().unwrap(),
                    c => {
                        input.skip_error(Error::InvalidCharacterForSmallInteger {
                            span: input.last_span(c),
                        });
                        0
                    }
                }
            }
            // PLtoTF.2014.52
            Some('D' | 'd') => parse_number(input, 10),
            // PLtoTF.2014.53
            Some('O' | 'o') => parse_number(input, 8),
            // PLtoTF.2014.54
            Some('H' | 'h') => parse_number(input, 16),
            // PLtoTF.2014.55
            Some('F' | 'f') => {
                input.consume_spaces();
                let span_start = input.raw_data_span.start;
                let mut acc: u8 = match input.next() {
                    Some('M' | 'm') => 0,
                    Some('B' | 'b') => 2,
                    Some('L' | 'l') => 4,
                    _ => 18,
                };
                acc += match input.next() {
                    Some('R' | 'r') => 0,
                    Some('I' | 'i') => 1,
                    _ => 18,
                };
                acc += match input.next() {
                    Some('R' | 'r') => 0,
                    Some('C' | 'c') => 6,
                    Some('E' | 'e') => 12,
                    _ => 18,
                };
                if acc >= 18 {
                    let span_end = input.raw_data_span.start;
                    input.skip_error(Error::InvalidFaceCode {
                        span: span_start..span_end,
                    });
                    acc = 0;
                }
                acc
            }
            c => {
                input.skip_error(Error::InvalidPrefixForSmallInteger {
                    span: input.last_span(c),
                });
                0
            }
        };
        input.consume_spaces();
        u
    }
}

impl Data for Char {
    fn build(input: &mut Input) -> Self {
        Char(u8::build(input))
    }
}

impl Data for String {
    fn build(input: &mut Input) -> Self {
        input.take_string()
    }
}

impl Data for Face {
    fn build(input: &mut Input) -> Self {
        u8::build(input).into()
    }
}

impl Data for bool {
    fn build(input: &mut Input) -> Self {
        // PLtoTF.2014.90
        let span_start = input.raw_data_span.start;
        let b = match input.next() {
            Some('T' | 't') => true,
            Some('F' | 'f') => false,
            _ => {
                input.skip_to_end();
                let span_end = input.raw_data_span.start;
                input.skip_error(Error::InvalidBoolean {
                    span: span_start..span_end,
                });
                false
            }
        };
        input.skip_to_end();
        b
    }
}

impl Data for Number {
    // PLtoTF.2014.62
    fn build(input: &mut Input) -> Self {
        match input.next() {
            Some('D' | 'd') | Some('R' | 'r') => (),
            c => {
                input.skip_error(Error::InvalidPrefixForDecimal {
                    span: input.last_span(c),
                });
                return Number::ZERO;
            }
        }
        input.consume_spaces();
        let span_start = input.raw_data_span.start;

        // PLtoTF.2014.63
        let negative = {
            let mut negative = false;
            loop {
                match input.peek() {
                    Some('+' | ' ') => (),
                    Some('-') => {
                        negative = !negative;
                    }
                    _ => break,
                };
                input.next();
            }
            negative
        };

        let integer_part = {
            let mut acc = 0_i32;
            while let Some(d) = input.peek().and_then(|c| c.to_digit(10)) {
                input.next();
                // PLtoTF.2014.64
                // The arithmetic here is guaranteed to succeed because we impose acc <= 2048
                acc = acc.checked_mul(10).unwrap().checked_add(d as i32).unwrap();
                if acc >= 2048 {
                    // We set the accumulator to 2048 and keep going. This allows us to capture
                    // the full span for the number in the error.
                    acc = 2048;
                }
            }
            acc
        };

        let fractional_part = {
            let mut acc = 0_i32;
            if input.peek() == Some('.') {
                input.next();
                // PLtoTF.2014.66
                let mut fractional_digits = [0_i32; 7];
                for slot in &mut fractional_digits {
                    match input.peek().and_then(|c| c.to_digit(10)) {
                        Some(d) => {
                            input.next();
                            *slot = 0o10000000_i32.checked_mul(d as i32).unwrap();
                        }
                        None => break,
                    }
                }
                for j in (0..7).rev() {
                    acc = fractional_digits[j].checked_add(acc / 10).unwrap();
                }
                acc = (acc + 10) / 20;
            }
            acc
        };

        if integer_part >= 2048 || (fractional_part >= Number::UNITY.0 && integer_part == 2047) {
            let span_end = input.raw_data_span.start;
            input.skip_error(Error::DecimalTooLarge {
                span: span_start..span_end,
            });
            return if integer_part == 2047 {
                Number::UNITY
            } else {
                Number::ZERO
            };
        }

        let modulus = integer_part
            .checked_mul(Number::UNITY.0)
            .unwrap()
            .checked_add(fractional_part)
            .unwrap();
        let result = if negative {
            modulus.checked_mul(-1).unwrap()
        } else {
            modulus
        };
        Number(result)
    }
}

#[cfg(test)]
mod tests {
    use crate::pl::cst::BalancedElem;

    use super::*;

    fn run(source: &str, want: Vec<Root>, want_errs: Vec<Error>) {
        let (got, got_errors) = Ast::build(source);
        assert_eq!(got_errors, want_errs);
        assert_eq!(got, Ast(want));
    }

    macro_rules! ast_test {
        ( $( ($name: ident, $input: expr, $want: expr, $want_errors: expr, ), )+ ) => {
            $(
                #[test]
                fn $name() {
                    let input = $input;
                    let want = $want;
                    let want_errors = $want_errors;
                    run(input, want, want_errors);
                }
            )+
        };
    }

    ast_test!(
        (
            string,
            r"(CODINGSCHEME MY CODING Scheme)",
            vec![Root::CodingScheme(SingleValue {
                data: "MY CODING Scheme".into()
            })],
            vec![],
        ),
        (
            boolean_true,
            r"(SEVENBITSAFEFLAG TRUE)",
            vec![Root::SevenBitSafeFlag(SingleValue { data: true })],
            vec![],
        ),
        (
            boolean_true_with_junk,
            r"(SEVENBITSAFEFLAG TRIPS)",
            vec![Root::SevenBitSafeFlag(SingleValue { data: true })],
            vec![],
        ),
        (
            boolean_false,
            r"(SEVENBITSAFEFLAG FALSE)",
            vec![Root::SevenBitSafeFlag(SingleValue { data: false })],
            vec![],
        ),
        (
            boolean_invalid,
            r"(SEVENBITSAFEFLAG INVALID)",
            vec![Root::SevenBitSafeFlag(SingleValue { data: false })],
            vec![Error::InvalidBoolean { span: 18..25 }],
        ),
        (
            one_byte_char_invalid_prefix,
            r"(BOUNDARYCHAR J a)",
            vec![Root::BoundaryChar(SingleValue { data: Char(0) })],
            vec![Error::InvalidPrefixForSmallInteger { span: 14..15 }],
        ),
        (
            one_byte_char_no_prefix,
            r"(BOUNDARYCHAR)",
            vec![Root::BoundaryChar(SingleValue { data: Char(0) })],
            vec![Error::InvalidPrefixForSmallInteger { span: 13..13 }],
        ),
        (
            one_byte_char,
            r"(BOUNDARYCHAR C a)",
            vec![Root::BoundaryChar(SingleValue {
                data: 'a'.try_into().unwrap()
            })],
            vec![],
        ),
        (
            one_byte_missing,
            r"(BOUNDARYCHAR C)",
            vec![Root::BoundaryChar(SingleValue { data: Char(0) })],
            vec![Error::InvalidCharacterForSmallInteger { span: 15..15 }],
        ),
        (
            one_byte_octal,
            r"(BOUNDARYCHAR O 77)",
            vec![Root::BoundaryChar(SingleValue { data: Char(0o77) })],
            vec![],
        ),
        (
            one_byte_octal_too_big,
            r"(BOUNDARYCHAR O 7777)",
            vec![Root::BoundaryChar(SingleValue { data: Char(0o0) })],
            vec![Error::SmallIntegerTooBig {
                span: 16..20,
                radix: 8
            }],
        ),
        (
            one_byte_decimal,
            r"(BOUNDARYCHAR D 77)",
            vec![Root::BoundaryChar(SingleValue { data: Char(77) })],
            vec![],
        ),
        (
            one_byte_decimal_too_big,
            r"(BOUNDARYCHAR D 7777)",
            vec![Root::BoundaryChar(SingleValue { data: Char(0) })],
            vec![Error::SmallIntegerTooBig {
                span: 16..20,
                radix: 10
            }],
        ),
        (
            one_byte_hexadecimal,
            r"(BOUNDARYCHAR H 17)",
            vec![Root::BoundaryChar(SingleValue { data: Char(0x17) })],
            vec![],
        ),
        (
            one_byte_hexadecimal_too_big,
            r"(BOUNDARYCHAR H 1777)",
            vec![Root::BoundaryChar(SingleValue { data: Char(0x0) })],
            vec![Error::SmallIntegerTooBig {
                span: 16..20,
                radix: 16
            }],
        ),
        (
            one_byte_face,
            r"(BOUNDARYCHAR F BIC)",
            vec![Root::BoundaryChar(SingleValue { data: Char(9) })],
            vec![],
        ),
        (
            one_byte_face_invalid,
            r"(BOUNDARYCHAR F ABC)",
            vec![Root::BoundaryChar(SingleValue { data: Char(0) })],
            vec![Error::InvalidFaceCode { span: 16..19 }],
        ),
        (
            one_byte_four_byte,
            r"(Header D1HA)",
            vec![Root::Header(TupleValue { data: (1, 0xA) })],
            vec![],
        ),
        (
            four_bytes_octal,
            r"(CHECKSUM O 77)",
            vec![Root::Checksum(SingleValue { data: 0o77 })],
            vec![],
        ),
        (
            four_bytes_hexadecimal,
            r"(CHECKSUM H 77)",
            vec![Root::Checksum(SingleValue { data: 0x77 })],
            vec![],
        ),
        (
            four_bytes_missing_prefix,
            r"(CHECKSUM)",
            vec![Root::Checksum(SingleValue { data: 0 })],
            vec![Error::InvalidPrefixForInteger { span: 9..9 }],
        ),
        (
            four_bytes_invalid_prefix,
            r"(CHECKSUM W 77)",
            vec![Root::Checksum(SingleValue { data: 0 })],
            vec![Error::InvalidPrefixForInteger { span: 10..11 }],
        ),
        (
            four_bytes_too_big,
            r"(CHECKSUM O 666666666666666666)",
            vec![Root::Checksum(SingleValue { data: 0o6666666666 })],
            vec![Error::IntegerTooBig { span: 12..30 }],
        ),
        (
            four_bytes_invalid_octal_digit,
            r"(CHECKSUM O 666686666666666666)",
            vec![Root::Checksum(SingleValue { data: 0o6666 })],
            vec![Error::InvalidOctalDigit { c: '8', span: 16 }],
        ),
        (
            fix_word_integer,
            r"(DESIGNSIZE D 1)",
            vec![Root::DesignSize(SingleValue {
                data: Number::UNITY
            })],
            vec![],
        ),
        (
            fix_word_decimal,
            r"(DESIGNSIZE D 11.5)",
            vec![Root::DesignSize(SingleValue {
                data: Number::UNITY * 23 / 2,
            })],
            vec![],
        ),
        (
            fix_word_negative,
            r"(DESIGNSIZE D -11.5)",
            vec![Root::DesignSize(SingleValue {
                data: Number::UNITY * -23 / 2,
            })],
            vec![],
        ),
        (
            fix_word_too_big_1,
            r"(DESIGNSIZE D 2049.1)",
            vec![Root::DesignSize(SingleValue { data: Number::ZERO })],
            vec![Error::DecimalTooLarge { span: 14..20 }],
        ),
        (
            fix_word_too_big_2,
            r"(DESIGNSIZE D 2047.9999999)",
            vec![Root::DesignSize(SingleValue {
                data: Number::UNITY,
            })],
            vec![Error::DecimalTooLarge { span: 14..26 }],
        ),
        (
            fix_word_invalid_prefix,
            r"(DESIGNSIZE W 2047.9999999)",
            vec![Root::DesignSize(SingleValue { data: Number::ZERO })],
            vec![Error::InvalidPrefixForDecimal { span: 12..13 }],
        ),
        (
            pl_to_tf_section_7_example,
            r"
            (FAMILY NOVA)
            (FACE F MIE)
            (CODINGSCHEME ASCII)
            (DESIGNSIZE D 10)
            (DESIGNUNITS D 18)
            (COMMENT A COMMENT IS IGNORED)
            (COMMENT (EXCEPT THIS ONE ISN'T))
            (COMMENT (ACTUALLY IT IS, EVEN THOUGH
                    IT SAYS IT ISN'T))
            (FONTDIMEN
               (SLANT R -.25)
               (SPACE D 6)
               (SHRINK D 2)
               (STRETCH D 3)
               (XHEIGHT R 10.55)
               (QUAD D 18)
               )
            (LIGTABLE
               (LABEL C f)
               (LIG C f O 200)
               (SKIP D 1)
               (LABEL O 200)
               (LIG C i O 201)
               (KRN O 51 R 1.5)
               (/LIG C ? C f)
               (STOP)
               )
            (CHARACTER C f
               (CHARWD D 6)
               (CHARHT R 13.5)
               (CHARIC R 1.5)
               )",
            vec![
                Root::Family(SingleValue {
                    data: "NOVA".into()
                }),
                Root::Face(SingleValue {
                    data: Face::Valid(
                        crate::FaceWeight::Medium,
                        crate::FaceSlope::Italic,
                        crate::FaceExpansion::Extended
                    )
                }),
                Root::CodingScheme(SingleValue {
                    data: "ASCII".into()
                }),
                Root::DesignSize(SingleValue {
                    data: Number::UNITY * 10,
                }),
                Root::DesignUnits(SingleValue {
                    data: Number::UNITY * 18,
                }),
                Root::Comment(vec![BalancedElem::String("A COMMENT IS IGNORED".into())]),
                Root::Comment(vec![BalancedElem::Vec(vec![BalancedElem::String(
                    "EXCEPT THIS ONE ISN'T".into()
                ),])]),
                Root::Comment(vec![BalancedElem::Vec(vec![
                    BalancedElem::String("ACTUALLY IT IS, EVEN THOUGH".into()),
                    BalancedElem::String("IT SAYS IT ISN'T".into()),
                ])]),
                Root::FontDimension(Branch {
                    data: (),
                    children: vec![
                        FontDimension::Slant(SingleValue {
                            data: Number::UNITY * -1 / 4,
                        }),
                        FontDimension::Space(SingleValue {
                            data: Number::UNITY * 6,
                        }),
                        FontDimension::Shrink(SingleValue {
                            data: Number::UNITY * 2,
                        }),
                        FontDimension::Stretch(SingleValue {
                            data: Number::UNITY * 3,
                        }),
                        FontDimension::XHeight(SingleValue {
                            data: Number(1055 * Number::UNITY.0 / 100 + 1)
                        }),
                        FontDimension::Quad(SingleValue {
                            data: Number::UNITY * 18,
                        }),
                    ]
                }),
                Root::LigTable(Branch {
                    data: (),
                    children: vec![
                        LigTable::Label(SingleValue {
                            data: LigTableLabel::Char('f'.try_into().unwrap())
                        }),
                        LigTable::Lig(
                            PostLigOperation::RetainNeitherMoveToInserted,
                            TupleValue {
                                data: ('f'.try_into().unwrap(), Char(0o200))
                            }
                        ),
                        LigTable::Skip(SingleValue { data: 1 }),
                        LigTable::Label(SingleValue {
                            data: LigTableLabel::Char(Char(0o200))
                        }),
                        LigTable::Lig(
                            PostLigOperation::RetainNeitherMoveToInserted,
                            TupleValue {
                                data: ('i'.try_into().unwrap(), Char(0o201))
                            }
                        ),
                        LigTable::Kern(TupleValue {
                            data: (Char(0o51), Number::UNITY * 3 / 2),
                        }),
                        LigTable::Lig(
                            PostLigOperation::RetainLeftMoveNowhere,
                            TupleValue {
                                data: ('?'.try_into().unwrap(), 'f'.try_into().unwrap())
                            }
                        ),
                        LigTable::Stop(SingleValue { data: () }),
                    ]
                }),
                Root::Character(Branch {
                    data: 'f'.try_into().unwrap(),
                    children: vec![
                        Character::Width(SingleValue {
                            data: Number::UNITY * 6,
                        }),
                        Character::Height(SingleValue {
                            data: Number::UNITY * 27 / 2,
                        }),
                        Character::ItalicCorrection(SingleValue {
                            data: Number::UNITY * 3 / 2,
                        }),
                    ]
                })
            ],
            vec![],
        ),
    );
}
