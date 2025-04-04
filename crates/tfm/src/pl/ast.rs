//! Abstract syntax tree for property list files
//!
//! The property list [AST](Ast) is a fully typed representation of a property list file.

use super::cst;
use super::error::*;
use crate::{ligkern::lang::PostLigOperation, Char, Face, FixWord, NamedParameter};
use std::ops::Range;

/// Abstract syntax tree for property list files
///
/// This is simply a list of [`Root`] nodes.
#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Ast(pub Vec<Root>);

impl Ast {
    /// Build an AST directly from source code.
    pub fn from_pl_source_code(source: &str) -> (Ast, Vec<ParseWarning>) {
        let (cst, mut errors) = cst::Cst::from_pl_source_code(source);
        let ast = Ast::from_cst(cst, &mut errors);
        (Ast(ast), errors)
    }

    /// Build an AST from a CST.
    pub fn from_cst(cst: cst::Cst, errors: &mut Vec<ParseWarning>) -> Vec<Root> {
        cst.0
            .into_iter()
            .filter_map(|c| Root::build(c, errors))
            .collect()
    }
    /// Lower an AST to a CST.
    pub fn lower(self, char_display_format: super::CharDisplayFormat) -> cst::Cst {
        cst::Cst(
            self.0
                .into_iter()
                .map(|c| Root::lower(c, char_display_format))
                .collect(),
        )
    }
}

/// Value of a leaf node in the AST that contains a single piece of data.
///
/// An example of this node is the `CHECKSUM` entry, which just contains a 32-bit checksum as data.
#[derive(PartialEq, Eq, Debug)]
pub struct SingleValue<D> {
    /// Data in this leaf node.
    pub data: D,
    /// Span of the data in the property list source code.
    pub data_span: Range<usize>,
    // TODO: open paren spans? key spans?
}

impl<D> From<D> for SingleValue<D> {
    fn from(data: D) -> Self {
        Self {
            data,
            data_span: 0..0,
        }
    }
}

#[cfg(feature = "arbitrary")]
impl<'a, D> arbitrary::Arbitrary<'a> for SingleValue<D>
where
    D: arbitrary::Arbitrary<'a>,
{
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(D::arbitrary(u)?.into())
    }
}

/// Value of a leaf node in the AST that contains two pieces of data.
///
/// An example of this node is the `HEADER` entry, which contains a 8-bit header index
/// and a 32-bit value.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct TupleValue<D, E> {
    /// Left piece of data in the tuple.
    pub left: D,
    /// Span of the left data in the property list source code.
    pub left_span: Range<usize>,
    /// Right piece of data in the tuple.
    pub right: E,
    /// Span of the right data in the property list source code.
    pub right_span: Range<usize>,
}

impl<D, E> From<(D, E)> for TupleValue<D, E> {
    fn from(value: (D, E)) -> Self {
        Self {
            left: value.0,
            left_span: 0..0,
            right: value.1,
            right_span: 0..0,
        }
    }
}

#[cfg(feature = "arbitrary")]
impl<'a, D, E> arbitrary::Arbitrary<'a> for TupleValue<D, E>
where
    D: arbitrary::Arbitrary<'a>,
    E: arbitrary::Arbitrary<'a>,
{
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(<(D, E)>::arbitrary(u)?.into())
    }
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
    /// Span of the data in the property list source code.
    pub data_span: Range<usize>,
    // TODO: open paren spans? key spans?
    /// Elements of the property list.
    pub children: Vec<E>,
}

impl<D, E> From<(D, Vec<E>)> for Branch<D, E> {
    fn from(value: (D, Vec<E>)) -> Self {
        Self {
            data: value.0,
            data_span: 0..0,
            children: value.1,
        }
    }
}

#[cfg(feature = "arbitrary")]
impl<'a, D, E> arbitrary::Arbitrary<'a> for Branch<D, E>
where
    D: arbitrary::Arbitrary<'a>,
    E: arbitrary::Arbitrary<'a>,
{
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(<(D, Vec<E>)>::arbitrary(u)?.into())
    }
}

/// The first reason we have this trait is to make it possible to invoke the `from`
/// associated function of a type without specifying the type itself.
/// I.e., instead of writing `LeafValue::from` we can write `FromCstNode::from`.
/// This makes the `node_impl!` macro simpler to implement because we don't need to provide
/// the type name to the macro.
///
/// Second, the implementations impose trait bounds on `D` and `E` in terms of private traits.
/// Doing this kinds of bounds on a regular impl block is not allowed by the Rust compiler
///     (it raises the private trait in public interface error (error E0445)).
/// But with a trait impl we get away with it.
trait ToFromCstNode: Sized {
    fn from_cst_node(p: cst::RegularNode, errors: &mut Vec<ParseWarning>) -> Option<Self>;
    fn to_cst_node(self, key: &'static str, opts: &LowerOpts) -> cst::RegularNode;
}

struct LowerOpts {
    char_display_format: super::CharDisplayFormat,
}

impl<D: TryParse> ToFromCstNode for SingleValue<D> {
    fn from_cst_node(p: cst::RegularNode, errors: &mut Vec<ParseWarning>) -> Option<Self> {
        let (mut input, _) = Input::new(p, errors);
        let result = D::try_parse(&mut input).map(|data| Self {
            data: data.0,
            data_span: data.1,
        });
        input.find_junk(false);
        result
    }
    fn to_cst_node(self, key: &'static str, opts: &LowerOpts) -> cst::RegularNode {
        cst::RegularNode {
            opening_parenthesis_span: 0..0,
            key: key.into(),
            key_span: 0..0,
            data: TryParse::to_string(self.data, opts),
            data_span: self.data_span,
            children: None,
            closing_parenthesis_span: 0..0,
        }
    }
}

impl<D: TryParse, E: Parse> ToFromCstNode for TupleValue<D, E> {
    fn from_cst_node(p: cst::RegularNode, errors: &mut Vec<ParseWarning>) -> Option<Self> {
        let (mut input, _) = Input::new(p, errors);
        let (left, left_span) = match D::try_parse(&mut input) {
            None => return None,
            Some(first) => first,
        };
        let (right, right_span) = E::parse(&mut input);
        input.find_junk(false);
        Some(Self {
            left,
            left_span,
            right,
            right_span,
        })
    }
    fn to_cst_node(self, key: &'static str, opts: &LowerOpts) -> cst::RegularNode {
        cst::RegularNode {
            opening_parenthesis_span: 0..0,
            key: key.into(),
            key_span: 0..0,
            data: Some(format![
                "{} {}",
                TryParse::to_string(self.left, opts).expect("In TupleValue<D, E>, D != ()"),
                Parse::to_string(self.right, opts).expect("In TupleValue<D, E>, E != ()")
            ]),
            data_span: self.left_span.start..self.right_span.end,
            children: None,
            closing_parenthesis_span: 0..0,
        }
    }
}

impl<D: Parse, E: Node> ToFromCstNode for Branch<D, E> {
    fn from_cst_node(p: cst::RegularNode, errors: &mut Vec<ParseWarning>) -> Option<Self> {
        {
            let (mut input, children) = Input::new(p, errors);
            let (data, data_span) = D::parse(&mut input);
            input.find_junk(true);
            Some(Branch::<D, E> {
                data,
                data_span,
                children: children
                    .into_iter()
                    .filter_map(|c| Node::build(c, errors))
                    .collect(),
            })
        }
    }
    fn to_cst_node(self, key: &'static str, opts: &LowerOpts) -> cst::RegularNode {
        cst::RegularNode {
            opening_parenthesis_span: 0..0,
            key: key.into(),
            key_span: 0..0,
            data: TryParse::to_string(self.data, opts),
            data_span: self.data_span,
            children: Some(
                self.children
                    .into_iter()
                    .map(|c| E::lower(c, opts))
                    .collect(),
            ),
            closing_parenthesis_span: 0..0,
        }
    }
}

trait Node: Sized {
    fn build_regular(p: cst::RegularNode, errors: &mut Vec<ParseWarning>) -> Option<Self>;
    fn build_comment(s: String) -> Self;
    fn build(n: cst::Node, errors: &mut Vec<ParseWarning>) -> Option<Self> {
        match n {
            cst::Node::Comment(c) => Some(Node::build_comment(c)),
            cst::Node::Regular(r) => Node::build_regular(r, errors),
        }
    }
    fn lower(self, opts: &LowerOpts) -> cst::Node;
}

macro_rules! node_impl {
    ( $type: ident, $( ($key: ident, $str: expr, $variant: ident $(, $prefix: path )? ), )+ ) => {

        impl $type {
            $(
                pub const $key: &'static str = $str;
            )+

            pub const ALL_PROPERTY_NAMES: &'static [&'static str] = &[$( $str, )+];
        }

        impl Node for $type {
            fn build_regular(mut r: cst::RegularNode, errors: &mut Vec<ParseWarning>) -> Option<Self> {
                r.key.make_ascii_uppercase();
                match r.key.as_str() {
                    $(
                        $type::$key => {
                            match ToFromCstNode::from_cst_node(r, errors) {
                                None => None,
                                Some(v) => Some($type::$variant( $( $prefix, )? v )),
                            }
                        },
                    )+
                    _ => {
                        errors.push(ParseWarning{
                            span: r.key_span.clone(),
                            knuth_pltotf_offset: Some(r.key_span.end),
                            kind: ParseWarningKind::InvalidPropertyName {
                                provided_name: r.key.into(),
                                allowed_property_names: $type::ALL_PROPERTY_NAMES,
                            }
                        });
                        None
                    },
                }
            }
            fn build_comment(s: String) -> Self {
                $type::Comment(s)
            }
            fn lower(self, opts: &LowerOpts) -> cst::Node {
                match self {
                    $(
                        $type::$variant($( $prefix, )? v) => {
                            cst::Node::Regular(v.to_cst_node($str, opts))
                        }
                    )+
                    $type::Comment(balanced_elements) => {
                            cst::Node::Comment(balanced_elements)
                    }
                }
            }
        }
        impl $type {
          pub fn lower(self, char_display_format: super::CharDisplayFormat) -> cst::Node {
            let opts = LowerOpts {
              char_display_format,
            };
            Node::lower(self, &opts)
          }
        }
    };
}

/// A root node in a property list file.
///
/// The documentation on each variant is based on the documentation in PFtoTF.2014.9.
#[derive(PartialEq, Eq, Debug)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
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
    DesignSize(SingleValue<DesignSize>),

    /// The design units specifies how many units equals the design size
    ///     (or the eventual ‘at’ size, if the font is being scaled).
    /// For example, suppose
    /// you have a font that has been digitized with 600 pixels per em,
    ///     and the design size is one em;
    ///     then you could say `(DESIGNUNITS R 600)` if you wanted to give all of your measurements in units of pixels.
    DesignUnits(SingleValue<FixWord>),

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
    Header(TupleValue<DecimalU8, u32>),

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
    Comment(String),
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
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum FontDimension {
    /// A named parameters like `(SLANT R -.25)`.
    NamedParam(NamedParameter, SingleValue<FixWord>),

    /// The notation `PARAMETER n` provides another way to specify the nth parameter;
    ///     for example, `(PARAMETER D 1 R −.25)` is another way to specify that the `SLANT` is −0.25.
    /// The value of n must be strictly positive and less than max param words.
    IndexedParam(TupleValue<ParameterNumber, FixWord>),

    /// A comment that is ignored.
    Comment(String),
}

/// A [`u8`] that is output in decimal when lowering the AST to a CST.
#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct DecimalU8(pub u8);

impl Parse for DecimalU8 {
    fn parse(input: &mut Input) -> (Self, Range<usize>) {
        let (a, b) = u8::parse(input);
        (DecimalU8(a), b)
    }

    fn to_string(self, _: &LowerOpts) -> Option<String> {
        Some(format!["D {}", self.0])
    }
}

#[derive(PartialEq, Eq, Debug)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct ParameterNumber(pub u16);

impl TryParse for ParameterNumber {
    fn try_parse(input: &mut Input) -> Option<(Self, Range<usize>)> {
        input.consume_spaces();
        let span_start = input.raw_data_span.start;
        let (a, err_or) = match parse_u8(input) {
            Ok(u) => (u, None),
            Err(err) => {
                input.skip_error(err.clone().into());
                (0, Some(err))
            }
        };
        let b = span_start..input.raw_data_span.start;
        match a {
            0 => {
                input.skip_error(ParseWarning {
                    span: b.clone(),
                    knuth_pltotf_offset: Some(match err_or {
                        // After this error occurs pltotf calls `backup` which shifts
                        // the scanner back 1 character.
                        Some(ParseU8Error::SmallIntegerIsTooBig { .. }) => b.end - 1,
                        _ => b.end,
                    }),
                    kind: ParseWarningKind::ParameterNumberIsZero,
                });
                None
            }
            u8::MAX => {
                input.skip_error(ParseWarning {
                    span: b.clone(),
                    knuth_pltotf_offset: Some(b.end),
                    kind: ParseWarningKind::ParameterNumberIsTooBig,
                });
                None
            }
            n => Some((ParameterNumber(n as u16), b)),
        }
    }

    fn to_string(self, _: &LowerOpts) -> Option<String> {
        Some(format!["D {}", self.0])
    }
}

node_impl!(
    FontDimension,
    (SLANT, "SLANT", NamedParam, NamedParameter::Slant),
    (SPACE, "SPACE", NamedParam, NamedParameter::Space),
    (STRETCH, "STRETCH", NamedParam, NamedParameter::Stretch),
    (SHRINK, "SHRINK", NamedParam, NamedParameter::Shrink),
    (X_HEIGHT, "XHEIGHT", NamedParam, NamedParameter::XHeight),
    (QUAD, "QUAD", NamedParam, NamedParameter::Quad),
    (
        EXTRA_SPACE,
        "EXTRASPACE",
        NamedParam,
        NamedParameter::ExtraSpace
    ),
    (NUM_1, "NUM1", NamedParam, NamedParameter::Num1),
    (NUM_2, "NUM2", NamedParam, NamedParameter::Num2),
    (NUM_3, "NUM3", NamedParam, NamedParameter::Num3),
    (DENOM_1, "DENOM1", NamedParam, NamedParameter::Denom1),
    (DENOM_2, "DENOM2", NamedParam, NamedParameter::Denom2),
    (SUP_1, "SUP1", NamedParam, NamedParameter::Sup1),
    (SUP_2, "SUP2", NamedParam, NamedParameter::Sup2),
    (SUP_3, "SUP3", NamedParam, NamedParameter::Sup3),
    (SUB_1, "SUB1", NamedParam, NamedParameter::Sub1),
    (SUB_2, "SUB2", NamedParam, NamedParameter::Sub2),
    (SUP_DROP, "SUPDROP", NamedParam, NamedParameter::SupDrop),
    (SUB_DROP, "SUBDROP", NamedParam, NamedParameter::SubDrop),
    (DELIM_1, "DELIM1", NamedParam, NamedParameter::Delim1),
    (DELIM_2, "DELIM2", NamedParam, NamedParameter::Delim2),
    (
        AXIS_HEIGHT,
        "AXISHEIGHT",
        NamedParam,
        NamedParameter::AxisHeight
    ),
    (
        DEFAULT_RULE_THICKNESS,
        "DEFAULTRULETHICKNESS",
        NamedParam,
        NamedParameter::DefaultRuleThickness
    ),
    (
        BIG_OP_SPACING_1,
        "BIGOPSPACING1",
        NamedParam,
        NamedParameter::BigOpSpacing1
    ),
    (
        BIG_OP_SPACING_2,
        "BIGOPSPACING2",
        NamedParam,
        NamedParameter::BigOpSpacing2
    ),
    (
        BIG_OP_SPACING_3,
        "BIGOPSPACING3",
        NamedParam,
        NamedParameter::BigOpSpacing3
    ),
    (
        BIG_OP_SPACING_4,
        "BIGOPSPACING4",
        NamedParam,
        NamedParameter::BigOpSpacing4
    ),
    (
        BIG_OP_SPACING_5,
        "BIGOPSPACING5",
        NamedParam,
        NamedParameter::BigOpSpacing5
    ),
    (PARAMETER, "PARAMETER", IndexedParam),
);

/// An element of a `CHARACTER` property list.
///
/// The documentation on each variant is based on the documentation in PFtoTF.2014.12.
#[derive(PartialEq, Eq, Debug)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum Character {
    /// The character's width in design units.
    Width(SingleValue<Option<FixWord>>),

    /// The character's height in design units.
    Height(SingleValue<FixWord>),

    /// The character's depth in design units.
    Depth(SingleValue<FixWord>),

    /// The character's italic correction in design units.
    ItalicCorrection(SingleValue<FixWord>),

    /// Specifies the character that follows the present one in a "charlist."
    /// The value must be the number of a character in the font,
    ///     and there must be no infinite cycles of supposedly larger and larger characters.
    NextLarger(SingleValue<Char>),

    /// Specifies an extensible character.
    /// This option and `NEXTLARGER` are mutually exclusive;
    ///     i.e., they cannot both be used within the same `CHARACTER` list.
    ExtensibleCharacter(Branch<(), ExtensibleCharacter>),

    /// A comment that is ignored.
    Comment(String),
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
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum ExtensibleCharacter {
    /// The top piece of an extensible character, or 0 if the top piece is absent.
    Top(SingleValue<Char>),

    /// The middle piece of an extensible character, or 0 if the top piece is absent.
    Middle(SingleValue<Char>),

    /// The bottom piece of an extensible character, or 0 if the top piece is absent.
    Bottom(SingleValue<Char>),

    /// The replicated piece of an extensible character, or 0 if it is absent.
    Replicated(SingleValue<Char>),

    /// A comment that is ignored.
    Comment(String),
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
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
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
    Kern(TupleValue<Char, FixWord>),

    /// A stop instruction ends a ligature/kern program.
    /// It must follow either a `LIG` or `KRN` instruction, not a `LABEL` or `STOP` or `SKIP`.
    Stop(SingleValue<()>),

    /// A skip instruction specifies continuation of a ligature/kern program after
    /// the specified number of LIG or KRN steps has been skipped over.
    /// The number of subsequent LIG and KRN instructions must therefore exceed this specified amount.
    Skip(SingleValue<DecimalU8>),

    /// A comment that is ignored.
    Comment(String),
}

/// Value of a label in a lig table.
#[derive(PartialEq, Eq, Debug)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum LigTableLabel {
    /// A specific character.
    Char(Char),
    /// The boundary character.
    BoundaryChar,
}

impl Parse for LigTableLabel {
    fn parse(input: &mut Input) -> (Self, Range<usize>) {
        match input.peek() {
            Some('B' | 'b') => {
                let span_start = input.raw_data_offset;
                input.skip_to_end();
                (LigTableLabel::BoundaryChar, span_start..span_start + 1)
            }
            _ => {
                let (c, span) = Parse::parse(input);
                (LigTableLabel::Char(c), span)
            }
        }
    }
    fn to_string(self, opts: &LowerOpts) -> Option<String> {
        match self {
            LigTableLabel::Char(c) => Parse::to_string(c, opts),
            LigTableLabel::BoundaryChar => Some("BOUNDARYCHAR".into()),
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

trait Parse: Sized {
    fn parse(input: &mut Input) -> (Self, Range<usize>);
    fn to_string(self, opts: &LowerOpts) -> Option<String>;
}

trait TryParse: Sized {
    fn try_parse(input: &mut Input) -> Option<(Self, Range<usize>)>;
    fn to_string(self, opts: &LowerOpts) -> Option<String>;
}

impl<T: Parse> TryParse for T {
    fn try_parse(input: &mut Input) -> Option<(Self, Range<usize>)> {
        Some(Parse::parse(input))
    }
    fn to_string(self, opts: &LowerOpts) -> Option<String> {
        Parse::to_string(self, opts)
    }
}

#[derive(Debug)]
struct Input<'a> {
    raw_data: String,
    raw_data_offset: usize,
    raw_data_span: Range<usize>,
    errors: &'a mut Vec<ParseWarning>,
}

impl<'a> Input<'a> {
    fn new(p: cst::RegularNode, errors: &'a mut Vec<ParseWarning>) -> (Self, Vec<cst::Node>) {
        (
            Input {
                raw_data: p.data.unwrap_or_default(),
                raw_data_offset: 0,
                errors,
                raw_data_span: p.data_span,
            },
            p.children.unwrap_or_default(),
        )
    }
    fn skip_error(&mut self, error: ParseWarning) {
        self.errors.push(error);
        self.skip_to_end();
    }
    fn skip_to_end(&mut self) {
        while self.next().is_some() {}
    }
    fn peek(&self) -> Option<char> {
        self.raw_data[self.raw_data_offset..].chars().next()
    }
    fn consume_spaces(&mut self) {
        while self.raw_data[self.raw_data_offset..].starts_with([' ', '\n']) {
            self.raw_data_offset += 1;
            self.raw_data_span.start += 1;
        }
    }
    fn last_span(&self, last: Option<char>) -> Range<usize> {
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
        self.raw_data_span.start = self.raw_data_span.end;
        res
    }
    fn find_junk(&mut self, is_branch: bool) {
        self.consume_spaces();
        if !self.raw_data_span.is_empty() {
            let span = self.raw_data_span.clone();
            let knuth_pltotf_offset = Some(self.raw_data_span.start + 1);
            let junk: String = self.collect();
            self.errors.push(ParseWarning {
                span,
                knuth_pltotf_offset,
                kind: if is_branch {
                    ParseWarningKind::JunkInsidePropertyList { junk }
                } else {
                    ParseWarningKind::JunkAfterPropertyValue { junk }
                },
            })
        }
    }
}

impl<'a> Iterator for Input<'a> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        let res = self.raw_data[self.raw_data_offset..].chars().next();
        if let Some(c) = res {
            if !c.is_ascii_graphic() && c != ' ' {
                self.errors.push(ParseWarning {
                    span: self.raw_data_span.start..self.raw_data_span.start + 1,
                    knuth_pltotf_offset: Some(self.raw_data_span.start + 1),
                    kind: ParseWarningKind::NonVisibleAsciiCharacter { character: c },
                });
            }
            self.raw_data_offset += c.len_utf8();
            self.raw_data_span.start += 1;
        }
        res
    }
}

impl Parse for () {
    fn parse(input: &mut Input) -> (Self, Range<usize>) {
        ((), input.raw_data_span.start..input.raw_data_span.start)
    }
    fn to_string(self, _: &LowerOpts) -> Option<String> {
        None
    }
}

impl Parse for u32 {
    // PLtoTF.2014.59-60
    fn parse(input: &mut Input) -> (Self, Range<usize>) {
        input.consume_spaces();
        let start_span = input.raw_data_span.start;
        let radix = match input.next() {
            Some('O' | 'o') => 8_u8,
            Some('H' | 'h') => 16,
            c => {
                let span = input.last_span(c);
                input.skip_error(ParseWarning {
                    span: span.clone(),
                    knuth_pltotf_offset: Some(span.end),
                    kind: ParseWarningKind::InvalidPrefixForInteger { prefix: c },
                });
                return (0, span);
            }
        };
        input.consume_spaces();
        let number_start_span = input.raw_data_span.start;
        let mut acc: u32 = 0;
        while let Some(c) = input.peek() {
            let n: u32 = match c.to_digit(16) {
                None => break,
                Some(d) => d,
            };
            input.next();
            if n >= radix as u32 {
                let span_start = input.raw_data_span.start - 1;
                input.skip_error(ParseWarning {
                    span: span_start..span_start + 1,
                    knuth_pltotf_offset: Some(span_start + 1),
                    kind: ParseWarningKind::InvalidOctalDigit { invalid_digit: c },
                });
                break;
            }
            match acc
                .checked_mul(radix as u32)
                .and_then(|acc| acc.checked_add(n))
            {
                None => {
                    // Overflow has occurred.
                    // We advance to the end of the integer constant so that the span in the error is
                    // most accurate.
                    let knuth_pltotf_offset = input.raw_data_span.start;
                    while let Some(c) = input.peek() {
                        match c.to_digit(radix as u32) {
                            None => break,
                            Some(_) => input.next(),
                        };
                    }
                    let end_span = input.raw_data_span.start;
                    input.skip_error(ParseWarning {
                        span: number_start_span..end_span,
                        knuth_pltotf_offset: Some(knuth_pltotf_offset),
                        kind: ParseWarningKind::IntegerIsTooBig { radix },
                    });
                    break;
                }
                Some(new_acc) => acc = new_acc,
            }
        }
        (acc, start_span..input.raw_data_span.start)
    }
    fn to_string(self, _: &LowerOpts) -> Option<String> {
        Some(format!("O {self:o}"))
    }
}

impl Parse for u8 {
    fn parse(input: &mut Input) -> (Self, Range<usize>) {
        input.consume_spaces();
        let span_start = input.raw_data_span.start;
        let u = match parse_u8(input) {
            Ok(u) => u,
            Err(err) => {
                input.skip_error(err.into());
                0
            }
        };
        let span_end = input.raw_data_span.start;
        (u, span_start..span_end)
    }
    fn to_string(self, _: &LowerOpts) -> Option<String> {
        Some(format!["O {self:o}"])
    }
}

impl Parse for Char {
    fn parse(input: &mut Input) -> (Self, Range<usize>) {
        let (u, span) = u8::parse(input);
        (Char(u), span)
    }
    fn to_string(self, opts: &LowerOpts) -> Option<String> {
        // TFtoPL.2014.38 and my interpretation of `man tftopl`
        // Note that the Pascal code is changed as part of compiling web2c, and these
        // changes influence the behavior here.
        use super::CharDisplayFormat;
        let output_as_ascii = match (opts.char_display_format, self.0 as char) {
            (CharDisplayFormat::Default, 'a'..='z' | 'A'..='Z' | '0'..='9') => true,
            (CharDisplayFormat::Ascii, '(' | ')') => false,
            (CharDisplayFormat::Ascii, '!'..='~') => true,
            _ => false,
        };
        if output_as_ascii {
            Some(format!("C {}", self.0 as char))
        } else {
            Parse::to_string(self.0, opts)
        }
    }
}

impl Parse for String {
    fn parse(input: &mut Input) -> (Self, Range<usize>) {
        let span_start = input.raw_data_span.start;
        let s = input.take_string();
        let l = s.len();
        (s, span_start..span_start + l)
    }
    fn to_string(self, _: &LowerOpts) -> Option<String> {
        Some(self)
    }
}

impl Parse for Face {
    fn parse(input: &mut Input) -> (Self, Range<usize>) {
        let (u, span) = u8::parse(input);
        (u.into(), span)
    }
    fn to_string(self, opts: &LowerOpts) -> Option<String> {
        // TFtoPL.2014.39
        match self {
            Face::Valid(w, s, e) => Some(format!(
                "F {}{}{}",
                match w {
                    crate::FaceWeight::Light => 'L',
                    crate::FaceWeight::Medium => 'M',
                    crate::FaceWeight::Bold => 'B',
                },
                match s {
                    crate::FaceSlope::Roman => 'R',
                    crate::FaceSlope::Italic => 'I',
                },
                match e {
                    crate::FaceExpansion::Regular => 'R',
                    crate::FaceExpansion::Condensed => 'C',
                    crate::FaceExpansion::Extended => 'E',
                },
            )),
            Face::Other(u) => Parse::to_string(u, opts),
        }
    }
}

impl TryParse for bool {
    fn try_parse(input: &mut Input) -> Option<(Self, Range<usize>)> {
        // PLtoTF.2014.90
        let span_start = input.raw_data_span.start;
        let b = match input.next() {
            Some('T' | 't') => true,
            Some('F' | 'f') => false,
            _ => {
                input.skip_to_end();
                let span_end = input.raw_data_span.start;
                input.skip_error(ParseWarning {
                    span: span_start..span_end,
                    knuth_pltotf_offset: Some(span_start + 1),
                    kind: ParseWarningKind::InvalidBoolean,
                });
                return None;
            }
        };
        let span_end = input.raw_data_span.start;
        input.skip_to_end();
        Some((b, span_start..span_end))
    }
    fn to_string(self, _: &LowerOpts) -> Option<String> {
        Some(if self { "TRUE" } else { "FALSE" }.into())
    }
}

impl Parse for FixWord {
    // PLtoTF.2014.62
    fn parse(input: &mut Input) -> (Self, Range<usize>) {
        input.consume_spaces();
        let span_start = input.raw_data_span.start;
        match input.next() {
            Some('D' | 'd') | Some('R' | 'r') => (),
            c => {
                let span = input.last_span(c);
                input.skip_error(ParseWarning {
                    span: span.clone(),
                    knuth_pltotf_offset: Some(span.end),
                    kind: ParseWarningKind::InvalidPrefixForDecimalNumber,
                });
                return (FixWord::ZERO, span);
            }
        }
        input.consume_spaces();
        let number_span_start = input.raw_data_span.start;

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

        if integer_part >= 2048 || (fractional_part >= FixWord::ONE.0 && integer_part == 2047) {
            let span_end = input.raw_data_span.start;
            input.skip_error(ParseWarning {
                span: number_span_start..span_end,
                knuth_pltotf_offset: Some(number_span_start),
                kind: ParseWarningKind::DecimalNumberIsTooBig,
            });
            return if integer_part == 2047 {
                (FixWord::ONE, span_start..span_end)
            } else {
                (FixWord::ZERO, span_start..span_end)
            };
        }

        let modulus = integer_part
            .checked_mul(FixWord::ONE.0)
            .unwrap()
            .checked_add(fractional_part)
            .unwrap();
        let result = if negative {
            modulus.checked_mul(-1).unwrap()
        } else {
            modulus
        };
        (FixWord(result), span_start..input.raw_data_span.start)
    }
    fn to_string(self, _: &LowerOpts) -> Option<String> {
        Some(format!["R {self}"])
    }
}

/// Design size of the font.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum DesignSize {
    Valid(FixWord),
    Invalid,
}

impl Default for DesignSize {
    fn default() -> Self {
        DesignSize::Valid(FixWord::ONE * 10)
    }
}

impl DesignSize {
    pub fn get(&self) -> FixWord {
        match self {
            DesignSize::Valid(v) => *v,
            DesignSize::Invalid => FixWord::ONE * 10,
        }
    }
}

impl From<FixWord> for DesignSize {
    fn from(value: FixWord) -> Self {
        Self::Valid(value)
    }
}

impl Parse for DesignSize {
    fn parse(input: &mut Input) -> (Self, Range<usize>) {
        let (n, r) = FixWord::parse(input);
        let d = if n < FixWord::ONE {
            input.errors.push(ParseWarning {
                span: r.start..input.raw_data_span.end,
                knuth_pltotf_offset: Some(input.raw_data_span.end),
                kind: ParseWarningKind::DesignSizeIsTooSmall,
            });
            DesignSize::Invalid
        } else {
            DesignSize::Valid(n)
        };
        (d, r)
    }
    fn to_string(self, opts: &LowerOpts) -> Option<String> {
        match self {
            DesignSize::Valid(n) => Parse::to_string(n, opts),
            DesignSize::Invalid => Some("D 10".to_string()),
        }
    }
}

impl Parse for Option<FixWord> {
    fn parse(input: &mut Input) -> (Self, Range<usize>) {
        let (n, r) = FixWord::parse(input);
        (Some(n), r)
    }

    fn to_string(self, opts: &LowerOpts) -> Option<String> {
        match self {
            None => None,
            Some(n) => Parse::to_string(n, opts),
        }
    }
}

#[derive(Clone)]
enum ParseU8Error {
    SmallIntegerIsTooBig {
        span: std::ops::Range<usize>,
        knuth_pltotf_offset: usize,
        radix: u8,
    },
    EmptyCharacterValue {
        span: std::ops::Range<usize>,
    },
    InvalidFaceCode {
        span: std::ops::Range<usize>,
    },
    InvalidPrefixForSmallInteger {
        span: std::ops::Range<usize>,
    },
}

impl From<ParseU8Error> for ParseWarning {
    fn from(value: ParseU8Error) -> Self {
        match value {
            ParseU8Error::SmallIntegerIsTooBig {
                span,
                knuth_pltotf_offset,
                radix,
            } => ParseWarning {
                span,
                knuth_pltotf_offset: Some(knuth_pltotf_offset),
                kind: ParseWarningKind::SmallIntegerIsTooBig { radix },
            },
            ParseU8Error::EmptyCharacterValue { span } => ParseWarning {
                span: span.clone(),
                knuth_pltotf_offset: Some(span.end),
                kind: ParseWarningKind::EmptyCharacterValue,
            },
            ParseU8Error::InvalidFaceCode { span } => ParseWarning {
                span: span.clone(),
                knuth_pltotf_offset: Some(span.end),
                kind: ParseWarningKind::InvalidFaceCode,
            },
            ParseU8Error::InvalidPrefixForSmallInteger { span } => ParseWarning {
                span: span.clone(),
                knuth_pltotf_offset: Some(span.end + 1),
                kind: ParseWarningKind::InvalidPrefixForSmallInteger,
            },
        }
    }
}

fn parse_u8(input: &mut Input) -> Result<u8, ParseU8Error> {
    // PLtoTF.2014.51
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
                    let knuth_pltotf_offset = input.raw_data_span.start;
                    while let Some(c) = input.peek() {
                        match c.to_digit(radix as u32) {
                            None => break,
                            Some(_) => input.next(),
                        };
                    }
                    let end_span = input.raw_data_span.start;
                    return Err(ParseU8Error::SmallIntegerIsTooBig {
                        span: start_span..end_span,
                        knuth_pltotf_offset,
                        radix,
                    });
                }
                Some(new_acc) => acc = new_acc,
            }
        }
        Ok(acc)
    };
    let u = match input.next() {
        // PLtoTF.2014.52
        Some('C' | 'c') => {
            input.consume_spaces();
            match input.next() {
                None => {
                    let span = input.last_span(None);
                    return Err(ParseU8Error::EmptyCharacterValue { span });
                }
                Some(c @ ' '..='~') => (c as usize).try_into().unwrap(),
                // if the character is not a visible ASCII char it is set to invalid=127.
                _ => 127,
            }
        }
        // PLtoTF.2014.52
        Some('D' | 'd') => parse_number(input, 10)?,
        // PLtoTF.2014.53
        Some('O' | 'o') => parse_number(input, 8)?,
        // PLtoTF.2014.54
        Some('H' | 'h') => parse_number(input, 16)?,
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
                return Err(ParseU8Error::InvalidFaceCode {
                    span: span_start..span_end,
                });
            }
            acc
        }
        c => {
            let span = input.last_span(c);
            return Err(ParseU8Error::InvalidPrefixForSmallInteger { span });
        }
    };
    Ok(u)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run(source: &str, want: Vec<Root>, want_errs: Vec<ParseWarning>) {
        let (got, got_errors) = Ast::from_pl_source_code(source);
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
                data: "MY CODING Scheme".into(),
                data_span: 14..30,
            })],
            vec![],
        ),
        (
            boolean_true,
            r"(SEVENBITSAFEFLAG TRUE)",
            vec![Root::SevenBitSafeFlag(SingleValue {
                data: true,
                data_span: 18..19,
            })],
            vec![],
        ),
        (
            boolean_true_with_junk,
            r"(SEVENBITSAFEFLAG TRIPS)",
            vec![Root::SevenBitSafeFlag(SingleValue {
                data: true,
                data_span: 18..19
            })],
            vec![],
        ),
        (
            boolean_false,
            r"(SEVENBITSAFEFLAG FALSE)",
            vec![Root::SevenBitSafeFlag(SingleValue {
                data: false,
                data_span: 18..19
            })],
            vec![],
        ),
        (
            boolean_invalid,
            r"(SEVENBITSAFEFLAG INVALID)",
            vec![],
            vec![ParseWarning {
                span: 18..25,
                knuth_pltotf_offset: Some(19),
                kind: ParseWarningKind::InvalidBoolean,
            }],
        ),
        (
            one_byte_char_invalid_prefix,
            r"(BOUNDARYCHAR J a)",
            vec![Root::BoundaryChar(SingleValue {
                data: Char(0),
                data_span: 14..17
            })],
            vec![ParseWarning {
                span: 14..15,
                knuth_pltotf_offset: Some(16),
                kind: ParseWarningKind::InvalidPrefixForSmallInteger,
            }],
        ),
        (
            one_byte_char_no_prefix,
            r"(BOUNDARYCHAR)",
            vec![Root::BoundaryChar(SingleValue {
                data: Char(0),
                data_span: 13..13,
            })],
            vec![ParseWarning {
                span: 13..13,
                knuth_pltotf_offset: Some(14),
                kind: ParseWarningKind::InvalidPrefixForSmallInteger,
            }],
        ),
        (
            one_byte_char,
            r"(BOUNDARYCHAR C a)",
            vec![Root::BoundaryChar(SingleValue {
                data: 'a'.try_into().unwrap(),
                data_span: 14..17,
            })],
            vec![],
        ),
        (
            one_byte_missing,
            r"(BOUNDARYCHAR C)",
            vec![Root::BoundaryChar(SingleValue {
                data: Char(0),
                data_span: 14..15
            })],
            vec![ParseWarning {
                span: 15..15,
                knuth_pltotf_offset: Some(15),
                kind: ParseWarningKind::EmptyCharacterValue,
            }],
        ),
        /*
        TODO: re-enable when the bug is fixed
        (
            invalid_character_value_tab,
            "(BOUNDARYCHAR C \t)",
            vec![Root::BoundaryChar(SingleValue {
                data: Char(0), // probably Char(127)
                data_span: 14..16
            })],
            vec![
                ParseWarning::NonVisibleAsciiCharacter('\t', 16),
                ParseWarning::EmptyCharacterValue { span: 16..16 }
            ],
        ),
         */
        (
            one_byte_octal,
            r"(BOUNDARYCHAR O 77)",
            vec![Root::BoundaryChar(SingleValue {
                data: Char(0o77),
                data_span: 14..18
            })],
            vec![],
        ),
        (
            one_byte_octal_too_big,
            r"(BOUNDARYCHAR O 7777)",
            vec![Root::BoundaryChar(SingleValue {
                data: Char(0o0),
                data_span: 14..20
            })],
            vec![ParseWarning {
                span: 16..20,
                knuth_pltotf_offset: Some(19),
                kind: ParseWarningKind::SmallIntegerIsTooBig { radix: 8 }
            }],
        ),
        (
            one_byte_decimal,
            r"(BOUNDARYCHAR D 77)",
            vec![Root::BoundaryChar(SingleValue {
                data: Char(77),
                data_span: 14..18
            })],
            vec![],
        ),
        (
            one_byte_decimal_too_big,
            r"(BOUNDARYCHAR D 7777)",
            vec![Root::BoundaryChar(SingleValue {
                data: Char(0),
                data_span: 14..20
            })],
            vec![ParseWarning {
                span: 16..20,
                knuth_pltotf_offset: Some(19),
                kind: ParseWarningKind::SmallIntegerIsTooBig { radix: 10 }
            }],
        ),
        (
            one_byte_hexadecimal,
            r"(BOUNDARYCHAR H 17)",
            vec![Root::BoundaryChar(SingleValue {
                data: Char(0x17),
                data_span: 14..18
            })],
            vec![],
        ),
        (
            one_byte_hexadecimal_too_big,
            r"(BOUNDARYCHAR H 1777)",
            vec![Root::BoundaryChar(SingleValue {
                data: Char(0x0),
                data_span: 14..20
            })],
            vec![ParseWarning {
                span: 16..20,
                knuth_pltotf_offset: Some(19),
                kind: ParseWarningKind::SmallIntegerIsTooBig { radix: 16 }
            }],
        ),
        (
            one_byte_face,
            r"(BOUNDARYCHAR F BIC)",
            vec![Root::BoundaryChar(SingleValue {
                data: Char(9),
                data_span: 14..19
            })],
            vec![],
        ),
        (
            one_byte_face_invalid,
            r"(BOUNDARYCHAR F ABC)",
            vec![Root::BoundaryChar(SingleValue {
                data: Char(0),
                data_span: 14..19
            })],
            vec![ParseWarning {
                span: 16..19,
                knuth_pltotf_offset: Some(19),
                kind: ParseWarningKind::InvalidFaceCode,
            }],
        ),
        (
            one_byte_four_byte,
            r"(Header D19HA)",
            vec![Root::Header(TupleValue {
                left: DecimalU8(19),
                left_span: 8..11,
                right: 0xA,
                right_span: 11..13,
            })],
            vec![],
        ),
        (
            four_bytes_octal,
            r"(CHECKSUM O 77)",
            vec![Root::Checksum(SingleValue {
                data: 0o77,
                data_span: 10..14
            })],
            vec![],
        ),
        (
            four_bytes_hexadecimal,
            r"(CHECKSUM H 77)",
            vec![Root::Checksum(SingleValue {
                data: 0x77,
                data_span: 10..14
            })],
            vec![],
        ),
        (
            four_bytes_missing_prefix,
            r"(CHECKSUM)",
            vec![Root::Checksum(SingleValue {
                data: 0,
                data_span: 9..9
            })],
            vec![ParseWarning {
                span: 9..9,
                knuth_pltotf_offset: Some(9),
                kind: ParseWarningKind::InvalidPrefixForInteger { prefix: None },
            }],
        ),
        (
            four_bytes_invalid_prefix,
            r"(CHECKSUM W 77)",
            vec![Root::Checksum(SingleValue {
                data: 0,
                data_span: 10..11
            })],
            vec![ParseWarning {
                span: 10..11,
                knuth_pltotf_offset: Some(11),
                kind: ParseWarningKind::InvalidPrefixForInteger { prefix: Some('W') },
            }],
        ),
        (
            four_bytes_too_big,
            r"(CHECKSUM O 666666666666666666)",
            vec![Root::Checksum(SingleValue {
                data: 0o6666666666,
                data_span: 10..30
            })],
            vec![ParseWarning {
                span: 12..30,
                knuth_pltotf_offset: Some(23),
                kind: ParseWarningKind::IntegerIsTooBig { radix: 8 }
            }],
        ),
        (
            four_bytes_invalid_octal_digit,
            r"(CHECKSUM O 666686666666666666)",
            vec![Root::Checksum(SingleValue {
                data: 0o6666,
                data_span: 10..30
            })],
            vec![ParseWarning {
                span: 16..17,
                knuth_pltotf_offset: Some(17),
                kind: ParseWarningKind::InvalidOctalDigit { invalid_digit: '8' }
            }],
        ),
        (
            fix_word_integer,
            r"(DESIGNSIZE D 1)",
            vec![Root::DesignSize(SingleValue {
                data: FixWord::ONE.into(),
                data_span: 12..15,
            })],
            vec![],
        ),
        (
            fix_word_decimal,
            r"(DESIGNSIZE D 11.5)",
            vec![Root::DesignSize(SingleValue {
                data: (FixWord::ONE * 23 / 2).into(),
                data_span: 12..18,
            })],
            vec![],
        ),
        (
            fix_word_negative,
            r"(CHARACTER C X (CHARWD D -11.5))",
            vec![Root::Character(Branch {
                data: Char::X,
                data_span: 11..14,
                children: vec![Character::Width(SingleValue {
                    data: Some(FixWord::ONE * -23 / 2),
                    data_span: 23..30,
                })]
            }),],
            vec![],
        ),
        (
            fix_word_too_big,
            r"(DESIGNSIZE D 2047.9999999)",
            vec![Root::DesignSize(SingleValue {
                data: FixWord::ONE.into(),
                data_span: 12..26,
            })],
            vec![ParseWarning {
                span: 14..26,
                knuth_pltotf_offset: Some(14),
                kind: ParseWarningKind::DecimalNumberIsTooBig,
            }],
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
                    data: "NOVA".into(),
                    data_span: 21..25,
                }),
                Root::Face(SingleValue {
                    data: Face::Valid(
                        crate::FaceWeight::Medium,
                        crate::FaceSlope::Italic,
                        crate::FaceExpansion::Extended
                    ),
                    data_span: 45..50,
                }),
                Root::CodingScheme(SingleValue {
                    data: "ASCII".into(),
                    data_span: 78..83,
                }),
                Root::DesignSize(SingleValue {
                    data: (FixWord::ONE * 10).into(),
                    data_span: 109..113,
                }),
                Root::DesignUnits(SingleValue {
                    data: FixWord::ONE * 18,
                    data_span: 140..144,
                }),
                Root::Comment(" A COMMENT IS IGNORED".into()),
                Root::Comment(" (EXCEPT THIS ONE ISN'T)".into()),
                Root::Comment(format![
                    " (ACTUALLY IT IS, EVEN THOUGH\n{}IT SAYS IT ISN'T)",
                    " ".repeat(20)
                ]),
                Root::FontDimension(Branch {
                    data: (),
                    data_span: 362..362,
                    children: vec![
                        FontDimension::NamedParam(
                            NamedParameter::Slant,
                            SingleValue {
                                data: FixWord::ONE * -1 / 4,
                                data_span: 369..375,
                            }
                        ),
                        FontDimension::NamedParam(
                            NamedParameter::Space,
                            SingleValue {
                                data: FixWord::ONE * 6,
                                data_span: 399..402,
                            }
                        ),
                        FontDimension::NamedParam(
                            NamedParameter::Shrink,
                            SingleValue {
                                data: FixWord::ONE * 2,
                                data_span: 427..430,
                            }
                        ),
                        FontDimension::NamedParam(
                            NamedParameter::Stretch,
                            SingleValue {
                                data: FixWord::ONE * 3,
                                data_span: 456..459,
                            }
                        ),
                        FontDimension::NamedParam(
                            NamedParameter::XHeight,
                            SingleValue {
                                data: FixWord(1055 * FixWord::ONE.0 / 100 + 1),
                                data_span: 485..492,
                            }
                        ),
                        FontDimension::NamedParam(
                            NamedParameter::Quad,
                            SingleValue {
                                data: FixWord::ONE * 18,
                                data_span: 515..519,
                            }
                        ),
                    ]
                }),
                Root::LigTable(Branch {
                    data: (),
                    data_span: 575..575,
                    children: vec![
                        LigTable::Label(SingleValue {
                            data: LigTableLabel::Char('f'.try_into().unwrap()),
                            data_span: 582..585,
                        }),
                        LigTable::Lig(
                            PostLigOperation::RetainNeitherMoveToInserted,
                            TupleValue {
                                left: 'f'.try_into().unwrap(),
                                left_span: 607..610,
                                right: Char(0o200),
                                right_span: 611..616,
                            }
                        ),
                        LigTable::Skip(SingleValue {
                            data: DecimalU8(1),
                            data_span: 639..642
                        }),
                        LigTable::Label(SingleValue {
                            data: LigTableLabel::Char(Char(0o200)),
                            data_span: 666..671,
                        }),
                        LigTable::Lig(
                            PostLigOperation::RetainNeitherMoveToInserted,
                            TupleValue {
                                left: 'i'.try_into().unwrap(),
                                left_span: 693..696,
                                right: Char(0o201),
                                right_span: 697..702,
                            }
                        ),
                        LigTable::Kern(TupleValue {
                            left: Char(0o51),
                            left_span: 724..728,
                            right: FixWord::ONE * 3 / 2,
                            right_span: 729..734,
                        }),
                        LigTable::Lig(
                            PostLigOperation::RetainLeftMoveNowhere,
                            TupleValue {
                                left: '?'.try_into().unwrap(),
                                left_span: 757..760,
                                right: 'f'.try_into().unwrap(),
                                right_span: 761..764,
                            }
                        ),
                        LigTable::Stop(SingleValue {
                            data: (),
                            data_span: 786..786,
                        }),
                    ]
                }),
                Root::Character(Branch {
                    data: 'f'.try_into().unwrap(),
                    data_span: 828..831,
                    children: vec![
                        Character::Width(SingleValue {
                            data: Some(FixWord::ONE * 6),
                            data_span: 855..858,
                        }),
                        Character::Height(SingleValue {
                            data: FixWord::ONE * 27 / 2,
                            data_span: 883..889,
                        }),
                        Character::ItalicCorrection(SingleValue {
                            data: FixWord::ONE * 3 / 2,
                            data_span: 914..919,
                        }),
                    ]
                })
            ],
            vec![],
        ),
    );
}
