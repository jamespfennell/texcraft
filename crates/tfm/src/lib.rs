//! Parsers for the TeX font metric (.tfm) and property list (.pl) file formats

pub mod algorithms;
use std::{
    collections::{BTreeSet, HashMap, HashSet},
    num::{NonZeroI16, NonZeroU8},
};

pub mod format;
pub mod ligkern;
pub mod pl;

/// The TFM header, which contains metadata about the file.
///
/// This is defined in TFtoPL.2014.10.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Header {
    /// The font checksum, if specified.
    ///
    /// In .tfm files checksums are always specified because the format has no
    ///     way to omit a checksum.
    ///
    /// In .pl files checksums are specified if the `CHECKSUM` node appears.
    /// If no checksum is specified in a .pl file, pltotf calculates the
    ///     correct value and writes that.
    ///
    /// In TeX82, this is stored in the `font_check` array (TeX82.2021.549).
    pub checksum: Option<u32>,
    /// In TeX82, this is stored in the `font_dsize` array (TeX82.2021.549).
    pub design_size: DesignSize,
    pub character_coding_scheme: Option<String>,
    pub font_family: Option<String>,
    pub seven_bit_safe: Option<bool>,
    pub face: Option<Face>,
    /// The TFM format allows the header to contain arbitrary additional data.
    pub additional_data: Vec<u32>,
}

/// Design size of the font.
// TODO: instead of having a specific type, consider adding another boolean field to the header
// that specifies how the design size should be serialized.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DesignSize {
    Valid(Number),
    Invalid,
}

impl Default for DesignSize {
    fn default() -> Self {
        DesignSize::Valid(Number::UNITY * 10)
    }
}

impl DesignSize {
    pub fn get(&self) -> Number {
        match self {
            DesignSize::Valid(v) => *v,
            DesignSize::Invalid => Number::UNITY * 10,
        }
    }
}

impl Header {
    /// Returns the default header when parsing property list files.
    ///
    /// This is defined in PLtoTF.2014.70.
    pub fn pl_default() -> Header {
        Header {
            checksum: None,
            design_size: Default::default(),
            character_coding_scheme: Some("UNSPECIFIED".into()),
            font_family: Some("UNSPECIFIED".into()),
            seven_bit_safe: None,
            face: Some(0.into()),
            additional_data: vec![],
        }
    }

    /// Returns the default header when parsing .tfm files.
    ///
    /// This is defined in PLtoTF.2014.70.
    pub fn tfm_default() -> Header {
        Header {
            checksum: Some(0),
            design_size: DesignSize::Valid(Number::ZERO),
            character_coding_scheme: None,
            font_family: None,
            seven_bit_safe: None,
            face: None,
            additional_data: vec![],
        }
    }
}

impl From<Number> for DesignSize {
    fn from(value: Number) -> Self {
        Self::Valid(value)
    }
}

/// A character in a TFM file.
///
/// TFM and PL files only support 1-byte characters.
#[derive(Debug, Default, PartialEq, Eq, Hash, Clone, Copy, PartialOrd, Ord)]
pub struct Char(pub u8);

impl From<u8> for Char {
    fn from(value: u8) -> Self {
        Char(value)
    }
}

impl TryFrom<char> for Char {
    type Error = std::char::TryFromCharError;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        let u: u8 = value.try_into()?;
        Ok(Char(u))
    }
}

macro_rules! const_chars {
    ( $( ($name: ident, $value: expr), )+ ) => {
        $(
            pub const $name: Char = Char($value);
        )+
    };
}

impl Char {
    const_chars![
        (A, b'A'),
        (B, b'B'),
        (C, b'C'),
        (D, b'D'),
        (X, b'X'),
        (Y, b'Y'),
    ];

    pub fn is_seven_bit(&self) -> bool {
        self.0 <= 127
    }
}

/// Fixed-width numeric type used in TFM files.
///
/// This numeric type has 11 bits for the integer part,
/// 20 bits for the fractional part, and a single signed bit.
/// The inner value is the number multiplied by 2^20.
/// It is called a `fix_word` in TFtoPL.
///
/// In property list files, this type is represented as a decimal number
///   with up to 6 digits after the decimal point.
/// This is a non-lossy representation
///   because 10^(-6) is larger than 2^(-20).
#[derive(Default, PartialEq, Eq, Debug, Copy, Clone, PartialOrd, Ord, Hash)]
pub struct Number(pub i32);

impl Number {
    /// Representation of the number 0 as a [Number].
    pub const ZERO: Number = Number(0);

    /// Representation of the number 1 as a [Number].
    pub const UNITY: Number = Number(1 << 20);

    /// Returns true if the number is less than 16.0 in magnitude according to Knuth.
    ///
    /// The number +16.0 is not allowed.
    /// This is covered in the E2E tests.
    /// See `check_fix` in TFtoPL.2014.60.
    pub fn is_abs_less_than_16(&self) -> bool {
        *self >= Number::UNITY * -16 && *self < Number::UNITY * 16
    }
}

impl std::ops::Add<Number> for Number {
    type Output = Number;
    fn add(self, rhs: Number) -> Self::Output {
        Number(self.0 + rhs.0)
    }
}
impl std::ops::Sub<Number> for Number {
    type Output = Number;
    fn sub(self, rhs: Number) -> Self::Output {
        Number(self.0 - rhs.0)
    }
}

impl std::ops::Mul<i32> for Number {
    type Output = Number;

    fn mul(self, rhs: i32) -> Self::Output {
        Number(self.0 * rhs)
    }
}

impl std::ops::Div<i32> for Number {
    type Output = Number;

    fn div(self, rhs: i32) -> Self::Output {
        Number(self.0 / rhs)
    }
}

impl std::fmt::Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TFtoPL.2014.40-43
        if self.0 < 0 {
            write!(f, "-")?;
        }
        let integer_part = self.0.abs() / Number::UNITY.0;
        write!(f, "{integer_part}.")?;
        let mut fp = (self.0 % Number::UNITY.0).abs();
        fp = 10 * fp + 5;
        let mut delta = 10;
        loop {
            if delta > 0o4_000_000 {
                fp = fp + 0o2_000_000 - delta / 2;
            }
            write!(f, "{}", fp / 0o4_000_000)?;
            fp = 10 * (fp % 0o4_000_000);
            delta *= 10;
            if fp <= delta {
                break;
            }
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum FaceWeight {
    Light,
    Medium,
    Bold,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum FaceSlope {
    Roman,
    Italic,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum FaceExpansion {
    Regular,
    Condensed,
    Extended,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Face {
    Valid(FaceWeight, FaceSlope, FaceExpansion),
    Other(u8),
}

impl From<u8> for Face {
    fn from(value: u8) -> Self {
        if value >= 18 {
            return Face::Other(value);
        }
        let a = match (value % 6) / 2 {
            0 => FaceWeight::Medium,
            1 => FaceWeight::Bold,
            2 => FaceWeight::Light,
            _ => unreachable!(),
        };
        let b = match value % 2 {
            0 => FaceSlope::Roman,
            1 => FaceSlope::Italic,
            _ => unreachable!(),
        };
        let c = match value / 6 {
            0 => FaceExpansion::Regular,
            1 => FaceExpansion::Condensed,
            2 => FaceExpansion::Extended,
            _ => unreachable!(),
        };
        Face::Valid(a, b, c)
    }
}

impl From<Face> for u8 {
    fn from(value: Face) -> Self {
        match value {
            Face::Valid(w, s, c) => {
                let a: u8 = match w {
                    FaceWeight::Medium => 0,
                    FaceWeight::Bold => 1,
                    FaceWeight::Light => 2,
                };
                let b: u8 = match s {
                    FaceSlope::Roman => 0,
                    FaceSlope::Italic => 1,
                };
                let c: u8 = match c {
                    FaceExpansion::Regular => 0,
                    FaceExpansion::Condensed => 1,
                    FaceExpansion::Extended => 2,
                };
                c * 6 + a * 2 + b
            }
            Face::Other(b) => b,
        }
    }
}

/// TeX font metric parameters
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Params(pub Vec<Number>);

impl Params {
    pub fn set(&mut self, number: ParameterNumber, value: Number) {
        let i = number.index();
        let min_len = number.get() as usize;
        if self.0.len() <= min_len {
            self.0.resize(min_len, Default::default());
        }
        self.0[i] = value;
    }

    pub fn set_named(&mut self, named_param: NamedParam, value: Number) {
        self.set(named_param.number(), value)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParameterNumber(NonZeroI16);

impl ParameterNumber {
    pub fn new(u: i16) -> Option<Self> {
        if u < 0 {
            None
        } else {
            match u.try_into() {
                Ok(u) => Some(Self(u)),
                Err(_) => None,
            }
        }
    }
    fn get(&self) -> i16 {
        self.0.get()
    }
    fn index(&self) -> usize {
        self.0
            .get()
            .checked_sub(1)
            .expect("payload is non-zero")
            .try_into()
            .expect("payload is non-negative")
    }
}

/// A named TeX font metric parameter.
#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum NamedParam {
    Slant,
    Space,
    Stretch,
    Shrink,
    XHeight,
    Quad,
    ExtraSpace,
    Num1,
    Num2,
    Num3,
    Denom1,
    Denom2,
    Sup1,
    Sup2,
    Sup3,
    Sub1,
    Sub2,
    SupDrop,
    SubDrop,
    Delim1,
    Delim2,
    AxisHeight,
    DefaultRuleThickness,
    BigOpSpacing1,
    BigOpSpacing2,
    BigOpSpacing3,
    BigOpSpacing4,
    BigOpSpacing5,
}

impl NamedParam {
    pub fn number(&self) -> ParameterNumber {
        let i = match self {
            NamedParam::Slant => 1,
            NamedParam::Space => 2,
            NamedParam::Stretch => 3,
            NamedParam::Shrink => 4,
            NamedParam::XHeight => 5,
            NamedParam::Quad => 6,
            NamedParam::ExtraSpace => 7,
            NamedParam::Num1 => 8,
            NamedParam::Num2 => 9,
            NamedParam::Num3 => 10,
            NamedParam::Denom1 => 11,
            NamedParam::Denom2 => 12,
            NamedParam::Sup1 => 13,
            NamedParam::Sup2 => 14,
            NamedParam::Sup3 => 15,
            NamedParam::Sub1 => 16,
            NamedParam::Sub2 => 17,
            NamedParam::SupDrop => 18,
            NamedParam::SubDrop => 19,
            NamedParam::Delim1 => 20,
            NamedParam::Delim2 => 21,
            NamedParam::AxisHeight => 22,
            NamedParam::DefaultRuleThickness => 8,
            NamedParam::BigOpSpacing1 => 9,
            NamedParam::BigOpSpacing2 => 10,
            NamedParam::BigOpSpacing3 => 11,
            NamedParam::BigOpSpacing4 => 12,
            NamedParam::BigOpSpacing5 => 13,
        };
        ParameterNumber(i.try_into().expect("all numbers are in range"))
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum NextLargerProgramWarning {
    NonExistentCharacter { original: Char, next_larger: Char },
    InfiniteLoop(Char),
}

impl NextLargerProgramWarning {
    pub fn bad_char(&self) -> Char {
        match self {
            NextLargerProgramWarning::NonExistentCharacter {
                original,
                next_larger: _,
            } => *original,
            NextLargerProgramWarning::InfiniteLoop(c) => *c,
        }
    }

    /// Returns the warning message the TFtoPL program prints for this kind of error.
    pub fn tftopl_message(&self) -> String {
        match self {
            NextLargerProgramWarning::NonExistentCharacter {
                original: _,
                next_larger,
            } => {
                format![
                    "Bad TFM file: Character list link to nonexistent character '{:03o}.",
                    next_larger.0
                ]
            }
            NextLargerProgramWarning::InfiniteLoop(c) => {
                format!["Bad TFM file: Cycle in a character list!\nCharacter '{:03o} now ends the list.", c.0]
            }
        }
    }

    /// Returns the section in Knuth's TFtoPL (version 2014) in which this warning occurs.
    pub fn tftopl_section(&self) -> u8 {
        84
    }

    /// Returns the section in Knuth's PLtoTF (version 2014) in which this warning occurs.
    pub fn pltotf_section(&self) -> u8 {
        match self {
            NextLargerProgramWarning::NonExistentCharacter { .. } => 111,
            NextLargerProgramWarning::InfiniteLoop(_) => 113,
        }
    }

    /// Returns the warning message the PLtoTF program prints for this kind of error.
    pub fn pltotf_message(&self) -> String {
        match self {
            NextLargerProgramWarning::NonExistentCharacter {
                original,
                next_larger: _,
            } => {
                format![
                    "The character NEXTLARGER than '{:03o} had no CHARACTER spec.",
                    original.0
                ]
            }
            NextLargerProgramWarning::InfiniteLoop(c) => {
                format![
                    "A cycle of NEXTLARGER characters has been broken at '{:03o}.",
                    c.0
                ]
            }
        }
    }
}
/// Next larger characters
///
/// The .tfm file format can associate a "next larger" character to any character in a font.
/// Next larger characters form sequences: i.e. B can be the next larger character for A,
///     and C can be the next larger character for B,
///     leading to the sequences A-B-C.
/// These next larger characters are used at certain points in TeX.
/// TeX occasionally traverses the entire sequence for a given starting character (e.g. A).
///
/// As with ligatures, next larger specifications can contain infinite loops -
///     e.g, if X is the next larger character for Y
///      and Y is the next larger character for X.
/// These loops are invalid and removed by TFtoPL and PLtoTF.
///
/// Motivated by the idea of "parse don't validate", this type represents
///     a compiled version of the next larger specifications in which infinite loops
///     are statically guaranteed not to exist.
///
/// The basic use of a valid program looks like this:
///
/// ```
/// # use tfm::*;
/// let edges = vec![
///     (Char::A, Char::B),
///     (Char::B, Char::C),
/// ];
/// let (next_larger_program, warnings) = NextLargerProgram::new(edges.into_iter(), |_| true, true);
///
/// assert_eq![warnings, vec![]];
///
/// let sequence_A: Vec<Char> = next_larger_program.get(Char::A).collect();
/// assert_eq!(sequence_A, vec![Char::B, Char::C]);
///
/// let sequence_B: Vec<Char> = next_larger_program.get(Char::B).collect();
/// assert_eq!(sequence_B, vec![Char::C]);
///
/// let sequence_C: Vec<Char> = next_larger_program.get(Char::C).collect();
/// assert_eq!(sequence_C, vec![]);
///
/// // Character that is not in the program.
/// let sequence_D: Vec<Char> = next_larger_program.get(Char::D).collect();
/// assert_eq!(sequence_D, vec![]);
/// ```
///
/// ## Warnings
///
/// There are two types of error that can occur when constructing the next
///     larger program.
/// Both of these errors are handled gracefully, so we officially refer to them as warnings.
/// The constructor returns them as values of type [`NextLargerProgramWarning`].
///
/// ### Infinite loops
///
/// The first error is that next larger programs can contain infinite loops -
///     e.g, if X is the next larger character for Y
///      and Y is the next larger character for X.
/// In this case the loop is broken by removing the next larger program for the
///     character with the largest 8-bit code, in this case Y.
/// A [`NextLargerProgramWarning::InfiniteLoop`] warning is returned
///     from the program constructor.
///
/// ```
/// # use tfm::*;
/// let edges = vec![
///     (Char::X, Char::Y),
///     (Char::Y, Char::X),
/// ];
/// let (next_larger_program, warnings) = NextLargerProgram::new(edges.into_iter(), |_| true, true);
///
/// assert_eq!(warnings, vec![NextLargerProgramWarning::InfiniteLoop(Char::Y)]);
///
/// let sequence_X: Vec<Char> = next_larger_program.get(Char::X).collect();
/// assert_eq!(sequence_X, vec![Char::Y]);
///
/// let sequence_Y: Vec<Char> = next_larger_program.get(Char::Y).collect();
/// assert_eq!(sequence_Y, vec![]);
/// ```
///
/// ### Non-existent characters
///
/// The second error is that characters referred to in the next larger program
///     may not be defined in the .tfm or .pl file.
/// For example, a .pl file may contain the snippet `(CHARACTER C X (NEXTLARGER C Y))`
///     without defining the character Y.
/// The constructor [`NextLargerProgram::new`] accepts a function for checking if a
///     character exists.
/// In all cases a [`NextLargerProgramWarning::NonExistentCharacter`] warning is returned
///     if a non-existent character is encountered.
///
/// The behavior of the resulting next larger program is configured using the
///     `drop_non_existent_characters` argument.
/// If this is false, then the behavior is the same as PLtoTF and the program still
///     contains the character.
///
/// ```
/// # use tfm::*;
/// let edges = vec![
///     (Char::X, Char::Y),
/// ];
/// let character_exists = |c| {
///     if c == Char::Y {
///         false
///     } else {
///         true
///     }
/// };
/// let (next_larger_program, warnings) = NextLargerProgram::new(edges.into_iter(), character_exists, false);
///
/// assert_eq!(warnings, vec![NextLargerProgramWarning::NonExistentCharacter{
///     original: Char::X,
///     next_larger: Char::Y,
/// }]);
///
/// let sequence_X: Vec<Char> = next_larger_program.get(Char::X).collect();
/// assert_eq!(sequence_X, vec![Char::Y]);
/// ```
///
/// If `drop_non_existent_characters` is true, next larger instructions pointing at non-existent
///     characters are dropped.
/// This is how TFtoPL behaves.
///
///
/// ```
/// # use tfm::*;
/// let edges = vec![
///     (Char::X, Char::Y),
/// ];
/// let character_exists = |c| {
///     if c == Char::Y {
///         false
///     } else {
///         true
///     }
/// };
/// let (next_larger_program, warnings) = NextLargerProgram::new(edges.into_iter(), character_exists, true);
///
/// assert_eq!(warnings, vec![NextLargerProgramWarning::NonExistentCharacter{
///     original: Char::X,
///     next_larger: Char::Y,
/// }]);
///
/// let sequence_X: Vec<Char> = next_larger_program.get(Char::X).collect();
/// assert_eq!(sequence_X, vec![]);
/// ```
///
#[derive(Clone, Debug)]
pub struct NextLargerProgram {
    entrypoints: HashMap<Char, u8>,
    next_larger: Vec<(Char, NonZeroU8)>,
}

impl NextLargerProgram {
    /// Build a new next larger program from an iterator over edges.
    pub fn new<I: Iterator<Item = (Char, Char)>, F: Fn(Char) -> bool>(
        edges: I,
        character_exists: F,
        drop_non_existent_characters: bool,
    ) -> (Self, Vec<NextLargerProgramWarning>) {
        // This function implements functionality in TFtoPL.2014.84 and PLtoTF.2014.{110,111,113}.
        let mut warnings: Vec<NextLargerProgramWarning> = vec![];

        let mut node_to_parent = HashMap::<Char, Char>::new();
        let mut node_to_num_children = HashMap::<Char, usize>::new();
        for (child, parent) in edges {
            if !character_exists(parent) {
                warnings.push(NextLargerProgramWarning::NonExistentCharacter {
                    original: child,
                    next_larger: parent,
                });
                if drop_non_existent_characters {
                    continue;
                }
            }
            node_to_parent.insert(child, parent);
            node_to_num_children.entry(child).or_default();
            *node_to_num_children.entry(parent).or_default() += 1;
        }

        let mut leaves: Vec<Char> = vec![];
        let mut non_leaves: BTreeSet<Char> = Default::default();
        for (node, num_children) in &node_to_num_children {
            if *num_children == 0 {
                leaves.push(*node);
            } else {
                non_leaves.insert(*node);
            }
        }

        let mut sorted_chars = vec![];
        let mut infinite_loop_warnings = vec![];
        loop {
            while let Some(leaf) = leaves.pop() {
                if let Some(parent) = node_to_parent.get(&leaf).copied() {
                    let n = node_to_num_children
                        .get_mut(&parent)
                        .expect("`node_to_num_children` contains all nodes");
                    *n = n
                        .checked_sub(1)
                        .expect("the parent of a child must have at least one child");
                    if *n == 0 {
                        leaves.push(parent);
                        non_leaves.remove(&parent);
                    }
                }
                sorted_chars.push(leaf);
            }

            // There could be pending nodes left because of a cycle.
            // We break the cycle at the largest node.
            let child = match non_leaves.last() {
                None => break,
                Some(child) => *child,
            };
            infinite_loop_warnings.push(NextLargerProgramWarning::InfiniteLoop(child));
            let parent = node_to_parent.remove(&child).expect(
                "General graph fact: sum_(node)#in_edges(node)=sum_(node)#out_edges(node).
                Fact about the next larger graph: #out_edges(node)<=1, because each char has at most one next larger char.
                If #out_edge(child)=0 then sum_(node)#in_edges(node)=sum_(node)#out_edges(node) < #nodes.
                Thus there exists another node with #in_edges(node)=0 and that node is a leaf.
                But `leaves.len()=0` at this line of code",
            );
            leaves.push(parent);
            non_leaves.remove(&parent);
        }
        warnings.extend(infinite_loop_warnings.into_iter().rev());

        let next_larger = {
            let parents: HashSet<Char> = node_to_parent.values().copied().collect();
            let mut node_to_position = HashMap::<Char, u8>::new();
            let mut next_larger: Vec<(Char, NonZeroU8)> = vec![];

            for c in sorted_chars.iter().rev() {
                if !parents.contains(c) {
                    continue;
                }
                // The present character is the parent of at least one child, aka it is the next larger
                // character for another character. So it needs to be in the next_larger array.
                let child_position: u8 = next_larger
                    .len()
                    .try_into()
                    .expect("there are at most u8::MAX chars in the `next_larger` array");
                let offset = match node_to_parent.get(c) {
                    // The next_larger array contains at most 256 elements: one for each char.
                    // (Actually it contains at most 255 because one character necessarily does not
                    // have a child node and this character does not appear in the array.)
                    // Anyway, an offset of 256 sends the index outside the array bound, and so
                    // subsequent calls to iterator return None.
                    None => NonZeroU8::MAX,
                    Some(parent) => {
                        let parent_position = *node_to_position
                            .get(parent)
                            .expect("parent has already been inserted");
                        child_position
                            .checked_sub(parent_position)
                            .expect("parent inserted before so its position it is strictly smaller")
                            .try_into()
                            .expect("parent inserted before so its position it is strictly smaller")
                    }
                };
                next_larger.push((*c, offset));
                node_to_position.insert(*c, child_position);
            }
            next_larger.reverse();
            next_larger
        };

        let entrypoints = {
            let node_to_position: HashMap<Char, u8> = next_larger
                .iter()
                .enumerate()
                .map(|(i, (c, _))| {
                    let u: u8 = i
                        .try_into()
                        .expect("there are at most u8::MAX chars in the `next_larger` array");
                    (*c, u)
                })
                .collect();
            let mut entrypoints = HashMap::<Char, u8>::new();
            for c in sorted_chars.iter().rev() {
                if let Some(parent) = node_to_parent.get(c) {
                    entrypoints.insert(
                        *c,
                        *node_to_position
                            .get(parent)
                            .expect("parent has already been inserted"),
                    );
                }
            }
            entrypoints
        };

        (
            Self {
                entrypoints,
                next_larger,
            },
            warnings,
        )
    }

    /// Get the next larger sequence for a character
    pub fn get(&self, c: Char) -> impl Iterator<Item = Char> + '_ {
        NextLargerProgramIter {
            current: self.entrypoints.get(&c).copied(),
            program: self,
        }
    }

    /// Returns whether this program is seven-bit safe.
    ///
    /// A next larger program is seven-bit safe if the next larger sequences for
    ///     seven-bit characters only contain seven-bit characters.
    /// Conversely a program is seven-bit unsafe if there is a seven-bit
    ///     character whose next larger sequence contains a non-seven-bit character.
    ///
    /// ```
    /// # use tfm::*;
    /// let edges = vec![
    ///     (Char(250), Char(125)),
    ///     (Char(125), Char(126)),
    /// ];
    /// let (next_larger_program, _) = NextLargerProgram::new(edges.into_iter(), |_| true, true);
    /// assert_eq!(true, next_larger_program.is_seven_bit_safe());
    ///
    /// let edges = vec![
    ///     (Char(125), Char(250)),
    /// ];
    /// let (next_larger_program, _) = NextLargerProgram::new(edges.into_iter(), |_| true, true);
    /// assert_eq!(false, next_larger_program.is_seven_bit_safe());
    /// ```
    pub fn is_seven_bit_safe(&self) -> bool {
        // For each c, we only need to check the first element in c's next larger sequence.
        // If there is a subsequent element d of the sequence that is seven-bit unsafe,
        // we will find it when considering one of d's children.
        // This optimization makes this function O(n), rather than worst case O(n^2).
        self.entrypoints
            .keys()
            .copied()
            .filter(Char::is_seven_bit)
            .filter_map(|c| self.get(c).next())
            .all(|c| c.is_seven_bit())
    }
}

#[derive(Clone, Debug)]
struct NextLargerProgramIter<'a> {
    current: Option<u8>,
    program: &'a NextLargerProgram,
}

impl<'a> Iterator for NextLargerProgramIter<'a> {
    type Item = Char;

    fn next(&mut self) -> Option<Self::Item> {
        // Note that the iterator is statically guaranteed to terminate!
        // If it returns None, it has already terminated.
        // If it returns Some, then self.current will be incremented by a strictly
        //  positive number.
        // Incrementing like this can only happen a finite number of times before overflow
        //  occurs, and then self.current is None and the iterator is terminated.
        match self.current {
            None => None,
            Some(current) => match self.program.next_larger.get(current as usize) {
                None => None,
                Some((c, inc)) => {
                    self.current = current.checked_add(inc.get());
                    Some(*c)
                }
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod next_larger_tests {
        use super::*;

        fn run(
            edges: Vec<(Char, Char)>,
            want_sequences: HashMap<Char, Vec<Char>>,
            want_warnings: Vec<NextLargerProgramWarning>,
        ) {
            let (program, got_warnings) = NextLargerProgram::new(edges.into_iter(), |_| true, true);
            assert_eq!(got_warnings, want_warnings);
            for u in 0..=u8::MAX {
                let want_sequence = want_sequences
                    .get(&Char(u))
                    .map(Vec::as_slice)
                    .unwrap_or_default();
                let got_sequence: Vec<Char> = program.get(Char(u)).collect();
                assert_eq!(
                    got_sequence, want_sequence,
                    "got/want sequences for {u} do not match"
                );
            }
        }

        fn big_infinite_loop_edges() -> Vec<(Char, Char)> {
            (0..=u8::MAX)
                .into_iter()
                .map(|u| (Char(u), Char(u.wrapping_add(1))))
                .collect()
        }

        fn big_infinite_loop_sequences() -> HashMap<Char, Vec<Char>> {
            (0..=u8::MAX)
                .into_iter()
                .map(|u| {
                    let v: Vec<Char> = match u.checked_add(1) {
                        None => vec![],
                        Some(w) => (w..=u8::MAX).into_iter().map(Char).collect(),
                    };
                    (Char(u), v)
                })
                .collect()
        }

        macro_rules! next_larger_tests {
            ( $( ($name: ident, $edges: expr, $want_sequences: expr, $want_warnings: expr, ), )+ ) => {
                $(
                    #[test]
                    fn $name () {
                        run($edges, $want_sequences, $want_warnings);
                    }
                )+
            };
        }

        next_larger_tests!(
            (
                same_node_loop,
                vec![(Char::A, Char::A)],
                HashMap::from([(Char::A, vec![])]),
                vec![NextLargerProgramWarning::InfiniteLoop(Char::A)],
            ),
            (
                big_infinite_loop,
                big_infinite_loop_edges(),
                big_infinite_loop_sequences(),
                vec![NextLargerProgramWarning::InfiniteLoop(Char(u8::MAX))],
            ),
        );
    }
}
