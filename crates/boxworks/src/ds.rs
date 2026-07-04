//! Core data structures for the typesetting engine.
//!
//! This module contains the fundamental data structures for the Boxworks typesetting engine.
//! As in TeX, the Boxworks is based around various lists (horizontal, vertical, etc.)
//!     that contains elements (which themselves may be nested lists).
//! The Rust representations of these lists and their elements are defined here.
//!
//! This module implements the entirety of TeX.2021 part 10, "data structures
//! for boxes and their friends".

use common::GlueOrder;
use common::Scaled as Number;
use std::rc::Rc;

/// Element of a horizontal list.
#[derive(Debug, Clone)]
pub enum Horizontal {
    Char(Char),
    HBox(HBox),
    VBox(VBox),
    Rule(Rule),
    Mark(Mark),
    Insertion(Insertion),
    Adjust(Adjust),
    Ligature(Ligature),
    Discretionary(Discretionary),
    Whatsit(Rc<dyn Whatsit>),
    Math(Math),
    Glue(Glue),
    Kern(Kern),
    Penalty(Penalty),
}

macro_rules! horizontal_impl {
    ( $( $variant: ident , )+ ) => {
        impl PartialEq for Horizontal {
            fn eq(&self, other: &Self) -> bool {
                match (self, other) {
                    $(
                    (Self::$variant(l), Self::$variant(r)) => l == r,
                    )+
                    _ => false,
                }
            }
        }
        $(
        impl From<$variant> for Horizontal {
            fn from(value: $variant) -> Self {
                Horizontal::$variant(value)
            }
        }
        )+
    };
}

horizontal_impl!(
    Char,
    HBox,
    VBox,
    Rule,
    Mark,
    Insertion,
    Adjust,
    Ligature,
    Discretionary,
    Math,
    Glue,
    Kern,
    Penalty,
);

/// Element of a vertical list.
#[derive(Clone, Debug)]
pub enum Vertical {
    HBox(HBox),
    VBox(VBox),
    Rule(Rule),
    Mark(Mark),
    Insertion(Insertion),
    Whatsit(Rc<dyn Whatsit>),
    Math(Math),
    Glue(Glue),
    Kern(Kern),
    Penalty(Penalty),
}

macro_rules! vertical_impl {
    ( $( $variant: ident , )+ ) => {
        impl PartialEq for Vertical {
            fn eq(&self, other: &Self) -> bool {
                match (self, other) {
                    $(
                    (Self::$variant(l), Self::$variant(r)) => l == r,
                    )+
                    _ => false,
                }
            }
        }
        $(
        impl From<$variant> for Vertical {
            fn from(value: $variant) -> Self {
                Vertical::$variant(value)
            }
        }
        )+
    };
}

vertical_impl!(HBox, VBox, Rule, Mark, Insertion, Math, Glue, Kern, Penalty,);

/// A character in a specific font.
///
/// This node can only appear in horizontal mode.
///
/// Described in TeX.2021.134.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Char {
    pub char: char,
    pub font: u32,
}

/// A box made from a horizontal list.
///
/// Described in TeX.2021.135.
#[derive(Clone, Debug, PartialEq)]
pub struct HBox {
    pub height: Number,
    pub width: Number,
    pub depth: Number,
    /// How much this box should be lowered (if it appears in a horizontal list),
    /// or how much it should be moved to the right (if it appears in a vertical
    /// list).
    pub shift_amount: Number,
    pub list: Vec<Horizontal>,
    pub glue_ratio: GlueRatio,
    pub glue_order: GlueOrder,
}

/// Pack width specifies how width is handled when packing
pub enum PackWidth {
    /// Make the box exactly this width, generally by stretching or shrinking
    /// glue within the box.
    Exact(common::Scaled),

    /// Make the box its natural width, plus the additional width specified here.
    Additional(common::Scaled),
}

impl HBox {
    /// Create a horizontal from a vertical list.
    ///
    pub fn pack<F: super::FontRepo>(
        font_repo: &F,
        list: Vec<Horizontal>,
        pack_width: PackWidth,
    ) -> HBox {
        // This function corresponds to hpack in TeX.2021.649.
        let mut hbox = HBox {
            list,
            ..Default::default()
        };
        let mut total_glue = common::Glue::default();
        let mut natural_width = common::Scaled::ZERO;
        for elem in &hbox.list {
            // TeX.2021.658
            use Horizontal as H;
            let [w, h, d] = match elem {
                H::Ligature(Ligature { char, font, .. }) | H::Char(Char { char, font }) => {
                    // TeX.2021.654
                    let Some([w, h, d]) = font_repo.width_height_depth(*char, *font) else {
                        continue;
                    };
                    [w, h, d]
                }
                // The next 3 cases are TeX.2021.653.
                H::HBox(HBox {
                    height,
                    width,
                    depth,
                    shift_amount,
                    ..
                })
                | H::VBox(VBox {
                    height,
                    width,
                    depth,
                    shift_amount,
                    ..
                }) => [*height - *shift_amount, *width, *depth + *shift_amount],
                H::Rule(Rule {
                    height,
                    width,
                    depth,
                }) => [*height, *width, *depth],
                // The next 3 cases are TeX.2021.655
                H::Mark(_) | H::Insertion(_) | H::Adjust(_) => {
                    todo!("support more nodes here")
                }
                H::Discretionary(_discretionary) => {
                    // Do nothing. Discretionaries are only relevant if they are break points.
                    continue;
                }
                H::Whatsit(_whatsit) => {
                    // Do nothing for the moment. But maybe support a callback here.
                    // TeX.2021.1360.
                    continue;
                }
                H::Math(_math) => {
                    todo!("support math nodes here")
                }
                H::Glue(glue) => {
                    // TeX.2021.656
                    use std::cmp::Ordering::*;
                    match total_glue.shrink_order.cmp(&glue.value.shrink_order) {
                        Less => {
                            total_glue.shrink = glue.value.shrink;
                            total_glue.shrink_order = glue.value.shrink_order;
                        }
                        Equal => {
                            total_glue.shrink += glue.value.shrink;
                        }
                        Greater => {
                            // Do nothing.
                            // This glue has smaller order than some other glue in the box, so will
                            // not be used for shrinking.
                        }
                    }
                    match total_glue.stretch_order.cmp(&glue.value.stretch_order) {
                        Less => {
                            total_glue.stretch = glue.value.stretch;
                            total_glue.stretch_order = glue.value.stretch_order;
                        }
                        Equal => {
                            total_glue.stretch += glue.value.stretch;
                        }
                        Greater => {
                            // Do nothing.
                            // This glue has smaller order than some other glue in the box, so will
                            // not be used for stretching.
                        }
                    }
                    // TODO: implement leader support.
                    [glue.value.width, common::Scaled::ZERO, common::Scaled::ZERO]
                }
                H::Kern(kern) => [kern.width, common::Scaled::ZERO, common::Scaled::ZERO],
                H::Penalty(_) => {
                    // Do nothing.
                    continue;
                }
            };
            natural_width += w;
            if h > hbox.height {
                hbox.height = h;
            }
            if d > hbox.depth {
                hbox.depth = d;
            }
        }

        // TeX.2021.657
        hbox.width = match pack_width {
            PackWidth::Exact(exact) => exact,
            PackWidth::Additional(additional) => natural_width + additional,
        };
        let excess = hbox.width - natural_width;
        use std::cmp::Ordering::*;
        match excess.cmp(&common::Scaled::ZERO) {
            Less => {
                // TeX.2021.664
                hbox.glue_order = total_glue.shrink_order;
                if total_glue.shrink_order == GlueOrder::Normal && total_glue.shrink < -excess {
                    // The box is overfull: the glue shrinks by exactly its
                    // shrinkability (TeX sets the glue ratio to unity) and
                    // the content overflows the box.
                    // TODO(TeX.2021.666): report the overfull box and append
                    // the \overfullrule rule.
                    hbox.glue_ratio = GlueRatio {
                        num: -common::Scaled::ONE,
                        den: common::Scaled::ONE,
                    };
                } else {
                    hbox.glue_ratio = GlueRatio {
                        num: excess,
                        den: total_glue.shrink,
                    };
                }
            }
            Equal => {
                // Do nothing: hbox defaults cover this case.
            }
            Greater => {
                // TeX.2021.658
                if total_glue.stretch != common::Scaled::ZERO {
                    hbox.glue_order = total_glue.stretch_order;
                    hbox.glue_ratio = GlueRatio {
                        num: excess,
                        den: total_glue.stretch,
                    };
                }
                if total_glue.stretch_order == GlueOrder::Normal {
                    // TODO(TeX.2021.660): report an underfull box
                }
            }
        }
        hbox
    }
}

/// Ratio by which glue should shrink or stretch.
///
/// This is one of the few (only?) places in Knuth's TeX where a floating point
/// number is used.
/// In general TeX uses fixed point integers to ensure that the results are
/// the same on every computer/CPU.
/// But the exact semantics of the glue ratio don't affect the output, so
/// using a float is deemed okay by Knuth.
///
/// However we opt to use a real ratio: i.e., a numerator and a denominator.
///
/// Described in TeX.2021.109.
#[derive(Copy, Clone, Debug, Eq)]
pub struct GlueRatio {
    pub num: common::Scaled,
    pub den: common::Scaled,
}

impl PartialEq for GlueRatio {
    fn eq(&self, other: &Self) -> bool {
        // We would prefer to use:
        // (self.num.0 as i64) * (other.den.0 as i64) == (other.num.0 as i64) * (self.den.0 as i64)
        // but the maping from ratios to floats and back to ratios is unfortunately
        // lossy. A better approach might be to "canonicalize" glue ratios when we construct
        // them. This would be equivalent to writing the string and parsing it back in.
        let lhs = format!["{}", self];
        let rhs = format!["{}", other];
        lhs == rhs
    }
}

impl Default for GlueRatio {
    fn default() -> Self {
        Self {
            num: common::Scaled(0),
            den: common::Scaled(1),
        }
    }
}

impl GlueRatio {
    pub fn as_float(&self) -> f32 {
        (self.num.0 as f32) / (self.den.0 as f32)
    }

    pub fn from_float_str(s: &str) -> Option<Self> {
        let s = format!("{s}pt");
        let num = common::Scaled::parse_from_string(&s).ok()?;
        Some(Self {
            num,
            den: common::Scaled::ONE,
        })
    }
}

impl std::fmt::Display for GlueRatio {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TeX.2021.186
        let g = self.as_float();
        let g = g.abs();
        let g = if g.abs() >= 20000.0 { 20000.0 } else { g };
        let g = ((common::Scaled::ONE.0 as f32) * g).round() as i32;
        write!(f, "{}", common::Scaled(g).display_no_units())
    }
}

impl HBox {
    /// Returns a hbox node corresponding to the TeX snippet `\hbox{}`.
    ///
    /// Described in TeX.2021.136.
    pub fn new_null_box() -> Self {
        Self {
            height: Number::ZERO,
            width: Number::ZERO,
            depth: Number::ZERO,
            shift_amount: Number::ZERO,
            list: vec![],
            glue_ratio: Default::default(),
            glue_order: GlueOrder::Normal,
        }
    }
}

impl Default for HBox {
    fn default() -> Self {
        Self::new_null_box()
    }
}

/// A box made from a vertical list.
///
/// This is the same as [HBox], except the list inside holds [Vertical] nodes
/// instead of [Horizontal] nodes.
///
/// Described in TeX.2021.137.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct VBox {
    pub height: Number,
    pub width: Number,
    pub depth: Number,
    pub shift_amount: Number,
    pub list: Vec<Vertical>,
    pub glue_ratio: GlueRatio,
    pub glue_order: GlueOrder,
}

/// A rule stands for a solid black rectangle.
///
/// It has width, depth and height fields.
/// However if any of these dimensions is -2^30, the actual value will be
/// determined by running rule up to the boundary of the innermost, enclosing box.
/// This is called a "running dimension".
/// The width is never running in an hlist; the height and depth are never running
/// in a vlist.
///
/// Described in TeX.2021.138.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Rule {
    pub height: Number,
    pub width: Number,
    pub depth: Number,
}

impl Rule {
    pub const RUNNING: Number = Number(-2 << 30);

    /// Creates a new rule.
    ///
    /// All of the dimensions are running.
    ///
    /// Described in TeX.2021.139.
    pub fn new() -> Self {
        Self {
            height: Self::RUNNING,
            width: Self::RUNNING,
            depth: Self::RUNNING,
        }
    }
}

impl Default for Rule {
    fn default() -> Self {
        Self::new()
    }
}

/// Vertical material to be inserted.
///
/// This node is related to the TeX primitive `\insert`.
///
/// Described in TeX.2021.140.
#[derive(Clone, Debug, PartialEq)]
pub struct Insertion {
    pub box_number: u8,
    /// Slightly misnamed: it actually holds the natural height plus depth
    /// of the vertical list being inserted.
    pub height: Number,
    /// Used in case this insertion is split.
    pub split_max_depth: Number,
    pub split_top_skip: common::Glue,
    /// Penalty to be used if this insertion floats to a subsequent
    /// page after a split insertion of the same class.
    pub float_penalty: u32,
    pub vbox: Vec<Vertical>,
}

/// Contents of a user's `\mark` text.
///
/// TODO: At time of writing I don't know what to do with this node.
/// In Knuth's TeX it references a token list, but I don't want Boxworks
/// to depend on Texlang. So for the moment just leaving a dummy list.
///
/// Described in TeX.2021.141.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Mark {
    pub list: Vec<()>,
}

/// Specifies material that will be moved out into the surrounding vertical list.
///
/// E.g., used to implement the TeX primitive `\vadjust`.
///
/// Described in TeX.2021.142.
#[derive(Clone, Debug, PartialEq)]
pub struct Adjust {
    pub list: Vec<Vertical>,
}

/// A ligature.
///
/// Described in TeX.2021.143.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ligature {
    pub included_left_boundary: bool,
    pub included_right_boundary: bool,
    pub char: char,
    pub font: u32,
    /// The original characters that were replaced by the ligature.
    /// This is used if the engine needs to break apart the ligature
    /// in order to perform hyphenation.
    ///
    /// While most ligatures come from 2 characters (e.g. ff), TeX's
    /// lig/kern programming language allows for a single ligature to come
    /// from arbitrarily many characters.
    pub original_chars: Rc<str>,
}

// Two constructors for ligature nodes are provided in TeX.2021.144
// but they don't seem that useful so I'm omitting them.

/// A discretionary break.
///
/// Described in TeX.2021.145.
#[derive(Clone, Debug, PartialEq)]
pub struct Discretionary {
    /// Material to insert before this node, if the break occurs here.
    pub pre_break: Vec<DiscretionaryElem>,
    /// Material to insert after this node, if the break occurs here.
    pub post_break: Vec<DiscretionaryElem>,
    /// Number of subsequent nodes to skip if the break occurs here.
    pub replace_count: u32,
}

impl Discretionary {
    pub fn new() -> Self {
        Self {
            pre_break: vec![],
            post_break: vec![],
            replace_count: 0,
        }
    }
}

impl Default for Discretionary {
    fn default() -> Self {
        Self::new()
    }
}

/// Element of a discretionary list.
#[derive(Clone, Debug, PartialEq)]
pub enum DiscretionaryElem {
    Char(Char),
    HBox(HBox),
    VBox(VBox),
    Rule(Rule),
    Ligature(Ligature),
    Kern(Kern),
}

impl From<Char> for DiscretionaryElem {
    fn from(value: Char) -> Self {
        DiscretionaryElem::Char(value)
    }
}

impl From<Kern> for DiscretionaryElem {
    fn from(value: Kern) -> Self {
        DiscretionaryElem::Kern(value)
    }
}

impl From<Ligature> for DiscretionaryElem {
    fn from(value: Ligature) -> Self {
        DiscretionaryElem::Ligature(value)
    }
}

impl From<DiscretionaryElem> for Horizontal {
    fn from(value: DiscretionaryElem) -> Self {
        use DiscretionaryElem as In;
        use Horizontal as Out;
        match value {
            In::Char(char) => Out::Char(char),
            In::HBox(hbox) => Out::HBox(hbox),
            In::VBox(vbox) => Out::VBox(vbox),
            In::Rule(rule) => Out::Rule(rule),
            In::Ligature(ligature) => Out::Ligature(ligature),
            In::Kern(kern) => Out::Kern(kern),
        }
    }
}

impl DiscretionaryElem {
    pub fn width<F: super::FontRepo>(&self, font_width: &F) -> Number {
        use DiscretionaryElem::*;
        match self {
            Char(char) => font_width
                .width(char.char, char.font)
                .unwrap_or(common::Scaled::ZERO),
            HBox(hlist) => hlist.width,
            VBox(vlist) => vlist.width,
            Rule(rule) => rule.width,
            Ligature(ligature) => font_width
                .width(ligature.char, ligature.font)
                .unwrap_or(common::Scaled::ZERO),
            Kern(kern) => kern.width,
        }
    }
}

impl TryFrom<Horizontal> for DiscretionaryElem {
    type Error = ();

    fn try_from(value: Horizontal) -> Result<Self, Self::Error> {
        use DiscretionaryElem as Out;
        use Horizontal::*;
        let out = match value {
            Char(char) => Out::Char(char),
            HBox(hlist) => Out::HBox(hlist),
            VBox(vlist) => Out::VBox(vlist),
            Rule(rule) => Out::Rule(rule),
            Ligature(ligature) => Out::Ligature(ligature),
            Kern(kern) => Out::Kern(kern),
            _ => return Err(()),
        };
        Ok(out)
    }
}

/// A whatsit node
///
/// This is used to facilitate extensions to TeX.
/// It's unclear right now how what the API of it will be, though
/// it can be figured out by reading the Chapter 53 Extensions of
/// TeX.
///
/// Knuth uses this node type to implement both `\write` and `\special`
/// so we'll eventually find out.
///
/// Described in TeX.2021.146.
pub trait Whatsit: std::fmt::Debug {
    // Invoked when this node is invoked when hyphenating.
    //
    // This is TeX.2021.1363 but given how we've architected the code, the logic in TeX.2021.1382
    // (which changes the current language) should run here for \language whatsits.
    fn hyphenation_hook(&self) {}
}

/// A marker placed before or after math mode.
///
/// Described in TeX.2021.147.
///
/// TODO: this also needs a width and so is wrong.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Math {
    Before,
    After,
}

impl Horizontal {
    /// Whether a glue node that comes after this node may be broken.
    ///
    /// For char nodes, this function is essentially undefined in Knuth's
    /// TeX. More specifically, the value depends on the exact character code.
    /// In TeX this function is never called for char nodes which is why this
    /// is not a problem. Here, we return `true` for char nodes based on
    /// my analysis of all places in Knuth's TeX where it is invoked:
    ///
    /// - TeX.2021.868: `precedes_break` is called on variable `cur_p` which
    ///   is a pointer to a horizontal list. Before this call, the calling code
    ///   first checks if the node is a character and if so follows the same
    ///   code path. Thus returning `true` here is the right thing to do.
    ///
    /// - TeX.2021.973: the function is called on a variable `prev_p` which
    ///   is a pointer to a vertical list and so the char case never arises.
    ///
    /// - TeX.2021.1000: same as the last case.
    ///
    /// This function is defined in TeX.2021.148.
    pub fn precedes_break(&self) -> bool {
        use Horizontal::*;
        match self {
            Char(_) | HBox(_) | VBox(_) | Rule(_) | Mark(_) | Insertion(_) | Adjust(_)
            | Ligature(_) | Discretionary(_) | Whatsit(_) => true,
            Kern(kern) => kern.kind != KernKind::Explicit,
            Math(_) | Glue(_) | Penalty(_) => false,
        }
    }

    /// Whether this node is discarded after a break.
    ///
    /// As with [Self::precedes_break], this function is essentially undefined
    /// for char nodes in Knuth's TeX. However there is only one call site
    /// (TeX.2021.879) and in that call site char nodes behave as if this
    /// function returns true.
    ///
    /// This function is defined in TeX.2021.148.
    pub fn non_discardable(&self) -> bool {
        self.precedes_break()
    }
}

impl Vertical {
    /// Whether a glue node that comes after this node may be broken.
    ///
    /// This function is defined in TeX.2021.148.
    pub fn precedes_break(&self) -> bool {
        use Vertical::*;
        matches!(
            self,
            HBox(_) | VBox(_) | Rule(_) | Mark(_) | Insertion(_) | Whatsit(_)
        )
    }
}

/// A piece of glue.
///
/// Described in TeX.2021.149.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Glue {
    pub value: common::Glue,
    pub kind: GlueKind,
}

impl From<common::Glue> for Glue {
    fn from(value: common::Glue) -> Self {
        Self {
            value,
            kind: Default::default(),
        }
    }
}

/// The kind of a glue node.
///
/// Described in TeX.2021.149.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub enum GlueKind {
    #[default]
    Normal,
    ConditionalMath,
    Math,
    AlignedLeader,
    CenteredLeader,
    ExpandedLeader,
}

// TeX.2021.150 and TeX.2021.151 define the [font::Glue] type itself,
// which is not in this crate.

// Three constructors for glue nodes are provided in TeX.2021.152,
// TeX.2021.153 and TeX.2021.154 but they don't seem that
// useful so I'm omitting them.

/// A kern.
///
/// Described in TeX.2021.155.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Kern {
    pub width: Number,
    pub kind: KernKind,
}

/// The kind of a kern node.
///
/// Described in TeX.2021.155.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum KernKind {
    /// Inserted from font information or math mode calculations.
    Normal,
    /// Inserted using e.g. TeX's `\kern` primitive.
    Explicit,
    /// Inserted from non-math accents.
    Accent,
    /// Inserted from e.g. `\mkern` specifications in math formulas.
    Math,
}

// A constructor for kern nodes is provided in TeX.2021.156,
// but it doesn't seem useful.

/// A penalty.
///
/// Described in TeX.2021.157.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Penalty(pub i32);

impl Penalty {
    /// Any penalty bigger than this is considered infinite and no
    /// break will be allowed for such high values.
    pub const INFINITE: Penalty = Penalty(10000);

    /// Any penalty smaller than this will result in a forced break.
    pub const EJECT: Penalty = Penalty(-10000);
}

// A constructor for penalty nodes is provided in TeX.2021.157,
// but it doesn't seem useful.

// TODO: Unset node(s) in TeX.2021.159

pub fn short_display_hlist(w: &mut dyn std::fmt::Write, hlist: &[Horizontal]) -> std::fmt::Result {
    // TeX.2021.174
    let mut i = 0;
    while let Some(elem) = hlist.get(i) {
        use Horizontal::*;
        match elem {
            Char(char) => write!(w, "{}", char.char)?,
            HBox(_) | VBox(_) | Whatsit(_) | Mark(_) | Adjust(_) => write!(w, "[]")?,
            Rule(_) => write!(w, "|")?,
            Ligature(ligature) => write!(w, "{}", ligature.original_chars)?,
            Discretionary(discretionary) => {
                short_display_dlist(w, &discretionary.pre_break)?;
                short_display_dlist(w, &discretionary.post_break)?;
                i += discretionary.replace_count as usize;
            }
            Math(_) => write!(w, "$")?,
            Glue(glue) => {
                if !glue.value.is_zero() {
                    write!(w, " ")?;
                }
            }
            Kern(_) | Penalty(_) | Insertion(_) => {}
        };
        i += 1;
    }
    Ok(())
}

fn short_display_dlist(
    w: &mut dyn std::fmt::Write,
    dlist: &[DiscretionaryElem],
) -> std::fmt::Result {
    // TeX.2021.174
    let mut i = 0;
    while let Some(elem) = dlist.get(i) {
        use DiscretionaryElem::*;
        match elem {
            Char(char) => write!(w, "{}", char.char)?,
            HBox(_) | VBox(_) => write!(w, "[]")?,
            Rule(_) => write!(w, "|")?,
            Ligature(ligature) => write!(w, "{}", ligature.original_chars)?,
            Kern(_) => {}
        };
        i += 1;
    }
    Ok(())
}
