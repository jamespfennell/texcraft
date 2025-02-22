use core;
/// Data structures for boxes
///
/// This module implements the entirety of TeX.2021 part 10, data structures
/// for boxes and their friends.
use core::GlueOrder;
use core::Scaled as Number;

/// Horizontal node.
pub enum Horizontal {
    Char(Char),
    HList(HList),
    VList(VList),
    Rule(Rule),
    Mark(Mark),
    Insertion(Insertion),
    Adjust(Adjust),
    Ligature(Ligature),
    Discretionary(Discretionary),
    Whatsit(Box<dyn Whatsit>),
    Math(Math),
    Glue(Glue),
    Kern(Kern),
    Penalty(Penalty),
}

/// Vertical node.
pub enum Vertical {
    HList(HList),
    VList(VList),
    Rule(Rule),
    Mark(Mark),
    Insertion(Insertion),
    Whatsit(Box<dyn Whatsit>),
    Math(Math),
    Glue(Glue),
    Kern(Kern),
    Penalty(Penalty),
}

/// A character in a specific font.
///
/// This node can only appear in horizontal mode.
///
/// Described in TeX.2021.134.
pub struct Char {
    pub char: char,
    pub font: u32,
}

/// A box made from a horizontal list.
///
/// Described in TeX.2021.135.
pub struct HList {
    pub height: Number,
    pub width: Number,
    pub depth: Number,
    /// How much this box should be lowered (if it appears in a horizontal list),
    /// or how much it should be moved to the right (if it appears in a vertical
    /// list).
    pub shift_amount: Number,
    pub list: Vec<Horizontal>,
    pub glue_ratio: GlueRatio,
    pub glue_sign: GlueSign,
    pub glue_order: GlueOrder,
}

/// Ratio by which glue should shrink or stretch.
///
/// This is one of the few (only?) places in TeX where a floating point
/// number is used.
/// In general TeX uses fixed point integers to ensure that the results are
/// the same on every computer/CPU.
/// But the exact semantics of the glue ratio don't affect the output, so
/// using a float is okay.
///
/// Described in TeX.2021.109.
pub struct GlueRatio(pub f32);

/// Description of whether the glue should stretch, shrink, or remain rigid.
pub enum GlueSign {
    Stretching,
    Shrinking,
    Normal,
}

impl HList {
    /// Returns a hlist node corresponding to the TeX snippet `\hbox{}`.
    ///
    /// Described in TeX.2021.136.
    pub fn new_null_box() -> Self {
        Self {
            height: Number::ZERO,
            width: Number::ZERO,
            depth: Number::ZERO,
            shift_amount: Number::ZERO,
            list: vec![],
            glue_ratio: GlueRatio(0.0),
            glue_sign: GlueSign::Normal,
            glue_order: GlueOrder::Normal,
        }
    }
}

impl Default for HList {
    fn default() -> Self {
        Self::new_null_box()
    }
}

/// A box made from a vertical list.
///
/// This is the same as [HList], except the list inside holds [Vertical] nodes
/// instead of [Horizontal] nodes.
///
/// Described in TeX.2021.137.
pub struct VList {
    pub height: Number,
    pub width: Number,
    pub depth: Number,
    pub shift_amount: Number,
    pub list: Vec<Vertical>,
    pub glue_ratio: GlueRatio,
    pub glue_sign: GlueSign,
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
pub struct Insertion {
    pub box_number: u8,
    /// Slightly misnamed: it actually holds the natural height plus depth
    /// of the vertical list being inserted.
    pub height: Number,
    /// Used in case this insertion is split.
    pub split_max_depth: Number,
    pub split_top_skip: core::Glue,
    /// Penalty to be used if this insertion floats to a subsequent
    /// page after a split insertion of the same class.
    pub float_penalty: u32,
    pub vlist: Vec<Vertical>,
}

/// Contents of a user's `\mark` text.
///
/// TODO: At time of writing I don't know what to do with this node.
/// In Knuth's TeX it references a token list, but I don't want Boxworks
/// to depend on Texlang. So for the moment just leaving a dummy list.
///
/// Described in TeX.2021.141.
pub struct Mark {
    pub list: Vec<()>,
}

/// Specifies material that will be moved out into the surrounding vertical list.
///
/// E.g., used to implement the TeX primitive `\vadjust`.
///
/// Described in TeX.2021.142.
pub struct Adjust {
    pub list: Vec<Vertical>,
}

/// A ligature.
///
/// Described in TeX.2021.143.
pub struct Ligature {
    pub included_left_boundary: bool,
    pub included_right_boundary: bool,
    pub char: char,
    pub font: u32,
    /// The original characters that were replaced by the ligature.
    /// This is used if the engine needs to break apart the ligature
    /// in order to perform hyphenation.
    /// For the moment this is a vector, but we probably should avoid allocating
    /// per-ligature-node as they may be common.
    pub original_chars: Vec<char>,
}

// Two constructors for ligature nodes are provided in TeX.2021.144
// but they don't seem that useful so I'm omitting them.

/// A discretionary break.
///
/// The pre-break and post-break lists must only contain nodes
/// of type char, kern, box, rule or ligature.
/// We could have a specific node type for this, but for the moment
/// we just piggy back on the hlist type.
///
/// Described in TeX.2021.145.
pub struct Discretionary {
    /// Material to insert before this node, if the break occurs here.
    pub pre_break: Vec<Horizontal>,
    /// Material to insert after this node, if the break occurs here.
    pub post_break: Vec<Horizontal>,
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
pub trait Whatsit {}

/// A marker placed before or after math mode.
///
/// Described in TeX.2021.147.
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
        matches!(
            self,
            Char(_)
                | HList(_)
                | VList(_)
                | Rule(_)
                | Mark(_)
                | Insertion(_)
                | Adjust(_)
                | Ligature(_)
                | Discretionary(_)
                | Whatsit(_)
        )
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
            HList(_) | VList(_) | Rule(_) | Mark(_) | Insertion(_) | Whatsit(_)
        )
    }
}

/// A piece of glue.
///
/// Described in TeX.2021.149.
pub struct Glue {
    pub kind: GlueKind,
    pub glue: core::Glue,
}

/// The kind of a glue node.
///
/// Described in TeX.2021.149.
pub enum GlueKind {
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
pub struct Kern {
    pub kind: KernKind,
    pub width: Number,
}

/// The kind of a kern node.
///
/// Described in TeX.2021.155.
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
pub struct Penalty {
    pub value: i32,
}

impl Penalty {
    /// Any penalty bigger than this is considered infinite and no
    /// break will be allowed for such high values.
    pub const INFINITE: i32 = 10000;

    /// Any penalty smaller than this will result in a forced break.
    pub const EJECT: i32 = -10000;
}

// A constructor for penalty nodes is provided in TeX.2021.157,
// but it doesn't seem useful.

// TODO: Unset node(s) in TeX.2021.159
