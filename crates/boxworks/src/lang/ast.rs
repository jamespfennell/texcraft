//! Box language abstract syntax tree
//!

use super::cst::TreeIter;
use super::ErrorAccumulator;

use super::cst;
use super::error::Error;
use super::Str;
use std::borrow::Cow;

/// Element of a vertical list.
///
/// Corresponds to the [`super::ds::Vertical`] type.
#[derive(Debug, PartialEq, Eq, Clone)]
#[allow(clippy::large_enum_variant)]
pub enum Vertical<'a> {
    HBox(HBox<'a>),
    VBox(VBox<'a>),
    Glue(Glue<'a>),
    Kern(Kern<'a>),
    Penalty(Penalty<'a>),
    Rule(Rule<'a>),
    Mark(Mark<'a>),
    Insertion(Insertion<'a>),
    Math(Math<'a>),
}

/// Element of a horizontal list.
///
/// Corresponds to the [`super::ds::Horizontal`] type.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Horizontal<'a> {
    Chars(Chars<'a>),
    Glue(Glue<'a>),
    Penalty(Penalty<'a>),
    Kern(Kern<'a>),
    HBox(HBox<'a>),
    VBox(VBox<'a>),
    Ligature(Ligature<'a>),
    Discretionary(Discretionary<'a>),
    Rule(Rule<'a>),
    Mark(Mark<'a>),
    Adjust(Adjust<'a>),
    Insertion(Insertion<'a>),
    Math(Math<'a>),
}

impl<'a> From<Chars<'a>> for Horizontal<'a> {
    fn from(value: Chars<'a>) -> Self {
        Horizontal::Chars(value)
    }
}

/// Element of a discretionary pre- or post-break list.
///
/// Corresponds to the [`super::ds::DiscretionaryElem`] type.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum DiscretionaryElem<'a> {
    Chars(Chars<'a>),
    Kern(Kern<'a>),
    HBox(HBox<'a>),
    VBox(VBox<'a>),
    Ligature(Ligature<'a>),
    Rule(Rule<'a>),
}

/// Lower a horizontal list to a CST tree.
pub fn lower_hbox<'a, 'b>(list: &'b [Horizontal<'a>]) -> impl cst::TreeIter<'a> + 'b {
    lower_hbox_impl(list)
}

fn lower_hbox_impl<'a, 'b>(list: &'b [Horizontal<'a>]) -> CstTreeIter<'a, 'b> {
    CstTreeIter::H { list, next: 0 }
}

/// Lower a vertical list to a CST tree.
pub fn lower_vbox<'a, 'b>(list: &'b [Vertical<'a>]) -> impl cst::TreeIter<'a> + 'b {
    lower_vbox_impl(list)
}

fn lower_vbox_impl<'a, 'b>(list: &'b [Vertical<'a>]) -> CstTreeIter<'a, 'b> {
    CstTreeIter::V { list, next: 0 }
}

fn lower_dlist_impl<'a, 'b>(list: &'b [DiscretionaryElem<'a>]) -> CstTreeIter<'a, 'b> {
    CstTreeIter::D { list, next: 0 }
}

impl<'a> Horizontal<'a> {
    /// Lower this element to a CST tree.
    pub fn lower<'b>(&'b self) -> impl cst::TreeIter<'a> + 'b {
        lower_hbox(std::slice::from_ref(self))
    }
    /// Lower the arguments of this element to a CST args iterator.
    pub fn lower_args<'b>(&'b self) -> impl cst::ArgsIter<'a> + 'b {
        self.lower_args_impl()
    }
    fn lower_args_impl<'b>(&'b self) -> CstArgsIter<'a, 'b> {
        CstArgsIter::H {
            elem: self,
            next: 0,
        }
    }
}

impl<'a> Vertical<'a> {
    /// Lower this element to a CST tree.
    pub fn lower<'b>(&'b self) -> impl cst::TreeIter<'a> + 'b {
        lower_vbox(std::slice::from_ref(self))
    }
    /// Lower the arguments of this element to a CST args iterator.
    pub fn lower_args<'b>(&'b self) -> impl cst::ArgsIter<'a> + 'b {
        self.lower_args_impl()
    }
    fn lower_args_impl<'b>(&'b self) -> CstArgsIter<'a, 'b> {
        CstArgsIter::V {
            elem: self,
            next: 0,
        }
    }
}

impl<'a> DiscretionaryElem<'a> {
    /// Lower this element to a CST tree.
    pub fn lower<'b>(&'b self) -> impl cst::TreeIter<'a> + 'b {
        lower_dlist_impl(std::slice::from_ref(self))
    }
    /// Lower the arguments of this element to a CST args iterator.
    pub fn lower_args<'b>(&'b self) -> impl cst::ArgsIter<'a> + 'b {
        self.lower_args_impl()
    }
    fn lower_args_impl<'b>(&'b self) -> CstArgsIter<'a, 'b> {
        CstArgsIter::D {
            elem: self,
            next: 0,
        }
    }
}

enum CstTreeIter<'a, 'b> {
    H {
        list: &'b [Horizontal<'a>],
        next: usize,
    },
    V {
        list: &'b [Vertical<'a>],
        next: usize,
    },
    D {
        list: &'b [DiscretionaryElem<'a>],
        next: usize,
    },
    Other(&'b Horizontal<'a>),
    Exausted,
}

enum CstArgsIter<'a, 'b> {
    H {
        elem: &'b Horizontal<'a>,
        next: usize,
    },
    V {
        elem: &'b Vertical<'a>,
        next: usize,
    },
    D {
        elem: &'b DiscretionaryElem<'a>,
        next: usize,
    },
}

impl<'a, 'b> Iterator for CstTreeIter<'a, 'b> {
    type Item = cst::TreeItem<'a, CstArgsIter<'a, 'b>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            CstTreeIter::H { list, next } => {
                let h = list.get(*next)?;
                *next += 1;
                Some(cst::TreeItem::FuncCall {
                    func_name: h.func_name().into(),
                    args: h.lower_args_impl(),
                })
            }
            CstTreeIter::V { list, next } => {
                let h = list.get(*next)?;
                *next += 1;
                Some(cst::TreeItem::FuncCall {
                    func_name: h.func_name().into(),
                    args: h.lower_args_impl(),
                })
            }
            CstTreeIter::D { list, next } => {
                let h = list.get(*next)?;
                *next += 1;
                Some(cst::TreeItem::FuncCall {
                    func_name: h.func_name().into(),
                    args: h.lower_args_impl(),
                })
            }
            CstTreeIter::Other(h) => {
                let r = cst::TreeItem::FuncCall {
                    func_name: h.func_name().into(),
                    args: h.lower_args_impl(),
                };
                *self = CstTreeIter::Exausted;
                Some(r)
            }
            CstTreeIter::Exausted => None,
        }
    }
}

impl<'a, 'b> cst::TreeIter<'a> for CstTreeIter<'a, 'b> {
    type ArgsIter = CstArgsIter<'a, 'b>;
    fn remaining_source(&self) -> Str<'a> {
        "".into()
    }
}

impl<'a, 'b> Iterator for CstArgsIter<'a, 'b> {
    type Item = cst::ArgsItem<'a, CstTreeIter<'a, 'b>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            CstArgsIter::H { elem, next } => {
                // keep looking for argrs until next is too big?
                let l = elem.lower_arg(*next)?;
                *next += 1;
                Some(l)
            }
            CstArgsIter::V { elem, next } => {
                let l = elem.lower_arg(*next)?;
                *next += 1;
                Some(l)
            }
            CstArgsIter::D { elem, next } => {
                let l = elem.lower_arg(*next)?;
                *next += 1;
                Some(l)
            }
        }
    }
}

impl<'a, 'b> cst::ArgsIter<'a> for CstArgsIter<'a, 'b> {
    type TreeIter = CstTreeIter<'a, 'b>;
}

impl<'a> std::fmt::Display for Vertical<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        cst::pretty_print(f, self.lower())
    }
}

impl<'a> std::fmt::Display for Horizontal<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        cst::pretty_print(f, self.lower())
    }
}

/// Parse Box language source code into a horizontal list.
pub fn parse_hbox(source: &str) -> Result<Vec<Horizontal<'_>>, Vec<Error<'_>>> {
    let errs: ErrorAccumulator = Default::default();
    let calls = cst::parse(source, errs.clone());
    let v = parse_hbox_using_cst(calls, &errs);
    errs.check()?;
    Ok(v)
}

/// Parse a hbox using an explicitly provided CST.
pub fn parse_hbox_using_cst<'a>(
    cst: impl cst::TreeIter<'a>,
    errs: &ErrorAccumulator<'a>,
) -> Vec<Horizontal<'a>> {
    let mut v: Vec<Horizontal> = vec![];
    for call in cst {
        match call {
            cst::TreeItem::FuncCall { func_name, args } => {
                if let Some(elem) = convert_call_to_hbox_elem(func_name, args, errs) {
                    v.push(elem);
                }
            }
            cst::TreeItem::Comment { value: _ } => continue,
        }
    }
    v
}

/// Parse a hbox using an explicitly provided CST.
pub fn parse_vbox_using_cst<'a>(
    cst: impl cst::TreeIter<'a>,
    errs: &ErrorAccumulator<'a>,
) -> Vec<Vertical<'a>> {
    let mut v: Vec<Vertical> = vec![];
    for call in cst {
        match call {
            cst::TreeItem::FuncCall { func_name, args } => {
                if let Some(elem) = convert_call_to_vbox_elem(func_name, args, errs) {
                    v.push(elem);
                }
            }
            cst::TreeItem::Comment { value: _ } => continue,
        }
    }
    v
}

/// Parse a dlist using an explicitly provided CST.
fn parse_dlist_using_cst<'a>(
    cst: impl cst::TreeIter<'a>,
    errs: &ErrorAccumulator<'a>,
) -> Vec<DiscretionaryElem<'a>> {
    let mut v: Vec<DiscretionaryElem> = vec![];
    for call in cst {
        match call {
            cst::TreeItem::FuncCall { func_name, args } => {
                if let Some(elem) = convert_call_to_dlist_elem(func_name, args, errs) {
                    v.push(elem);
                }
            }
            cst::TreeItem::Comment { value: _ } => continue,
        }
    }
    v
}

macro_rules! functions {
    ( $( (
        struct $name: ident <$lifetime: lifetime>  {
            $(
                $field_name: ident : $field_type: ty,
            )+
        }
        impl Func {
            func_name: $func_name: expr,
            default_num_pos_arg: $default_num_pos_arg: expr,
        }
        $(
            impl Horizontal {
                variant: $horizontal_variant: ident,
            }
        )?
        $(
            impl Vertical {
                variant: $vertical_variant: ident,
            }
        )?
        $(
            impl DiscretionaryElem {
                variant: $discretionary_variant: ident,
            }
        )?
    ), )+ ) => {
        $(
        #[derive(Debug, Default, PartialEq, Eq, Clone)]
        pub struct $name <$lifetime> {
            $(
                pub $field_name : Arg<$lifetime, $field_type>,
            )+
        }
        impl<$lifetime> Func for $name <$lifetime> {
            const NAME: &'static str = "todo";
            const FIELD_NAMES: &'static[&'static str] = &[ $( stringify!($field_name), )+];
            const DEFAULT_NUM_POS_ARG: usize = $default_num_pos_arg;
        }
        impl<$lifetime> Args<$lifetime> for $name <$lifetime> {
            fn assign_to_field<T: cst::TreeIter<$lifetime>>(&mut self, field_name: Str<$lifetime>, arg: cst::ArgsItem<$lifetime, T>, func_name: Str<$lifetime>, value_source: Str<$lifetime>,  errs: &ErrorAccumulator<$lifetime>)
            {
                match field_name.str() {
                $(
                    stringify!($field_name) => {
                        self.$field_name.assign(arg, field_name.str(), func_name, value_source, errs);
                    }
                )+
                    _ => {
                        errs.add(Error::NoSuchArgument{function_name: func_name, argument: field_name });
                    }
                }
            }
            fn lower_arg<'b>(&'b self, u: usize) -> Option<cst::ArgsItem<'a, CstTreeIter<'a, 'b>>> {
                let field_name = *Self::FIELD_NAMES.get(u)?;
                match field_name {
                $(
                    stringify!($field_name) => {
                        let key = if u < Self::DEFAULT_NUM_POS_ARG {
                            None
                        } else {
                            Some(field_name.into())
                        };
                        Some(value_to_cst(&self.$field_name.value, key))
                    }
                )+
                    _ => None,
                }
            }
        }
        )+
        impl<'a> Horizontal<'a> {
            pub fn func_name(&self) -> &'static str {
                match self {
                    $( $(
                        Horizontal::$horizontal_variant(_) => $func_name,
                    )? )+
                }
            }
            pub fn field_names(&self) -> &'static [&'static str ] {
                match self {
                    $( $(
                        Horizontal::$horizontal_variant(_) => $name::FIELD_NAMES,
                    )? )+
                }
            }
            fn lower_arg<'b>(&'b self, u: usize) -> Option<cst::ArgsItem<'a, CstTreeIter<'a, 'b>>> {
                match self {
                    $( $(
                        Horizontal::$horizontal_variant(args) => args.lower_arg(u),
                    )? )+
                }
            }
        }
        fn convert_call_to_hbox_elem<'a>(
            func_name: Str<'a>,
            call: impl cst::ArgsIter<'a>,
            errs: &ErrorAccumulator<'a>,
        ) -> Option<Horizontal<'a>> {
            let h = match func_name.str() {
                $( $(
                    $func_name => Horizontal::$horizontal_variant($name::build(func_name, call, errs)?),
                )? )+
                _ => {
                    errs.add(Error::NoSuchFunction {
                        function_name: func_name,
                    });
                    return None;
                }
            };
            Some(h)
        }
        impl<'a> DiscretionaryElem<'a> {
            pub fn func_name(&self) -> &'static str {
                match self {
                    $( $(
                        DiscretionaryElem::$discretionary_variant(_) => $func_name,
                    )? )+
                }
            }
            pub fn field_names(&self) -> &'static [&'static str ] {
                match self {
                    $( $(
                        DiscretionaryElem::$discretionary_variant(_) => $name::FIELD_NAMES,
                    )? )+
                }
            }
            fn lower_arg<'b>(&'b self, u: usize) -> Option<cst::ArgsItem<'a, CstTreeIter<'a, 'b>>> {
                match self {
                    $( $(
                        DiscretionaryElem::$discretionary_variant(args) => args.lower_arg(u),
                    )? )+
                }
            }
        }
        fn convert_call_to_dlist_elem<'a>(
            func_name: Str<'a>,
            call: impl cst::ArgsIter<'a>,
            errs: &ErrorAccumulator<'a>,
        ) -> Option<DiscretionaryElem<'a>> {
            let d = match func_name.str() {
                $( $(
                    $func_name => DiscretionaryElem::$discretionary_variant($name::build(func_name, call, errs)?),
                )? )+
                _ => {
                    errs.add(Error::NoSuchFunction {
                        function_name: func_name,
                    });
                    return None;
                }
            };
            Some(d)
        }
        impl<'a> Vertical<'a> {
            pub fn func_name(&self) -> &'static str {
                match self {
                    $( $(
                        Vertical::$vertical_variant(_) => $func_name,
                    )? )+
                }
            }
            pub fn field_names(&self) -> &'static [&'static str ] {
                match self {
                    $( $(
                        Vertical::$vertical_variant(_) => $name::FIELD_NAMES,
                    )? )+
                }
            }
            fn lower_arg<'b>(&'b self, u: usize) -> Option<cst::ArgsItem<'a, CstTreeIter<'a, 'b>>> {
                match self {
                    $( $(
                        Vertical::$vertical_variant(args) => args.lower_arg(u),
                    )? )+
                }
            }
        }
        fn convert_call_to_vbox_elem<'a>(
            func_name: Str<'a>,
            call: impl cst::ArgsIter<'a>,
            errs: &ErrorAccumulator<'a>,
        ) -> Option<Vertical<'a>> {
            let h = match func_name.str() {
                $( $(
                    $func_name => Vertical::$vertical_variant($name::build(func_name, call, errs)?),
                )? )+
                _ => {
                    errs.add(Error::NoSuchFunction {
                        function_name: func_name,
                    });
                    return None;
                }
            };
            Some(h)
        }
    };
}

/// A function like `text` or `glue`.
pub trait Func {
    /// Name of the function.
    const NAME: &'static str;
    /// Ordered list of field names.
    const FIELD_NAMES: &'static [&'static str];
    /// When printing, the number of arguments to print positionally.
    const DEFAULT_NUM_POS_ARG: usize = 0;
}

/// Concrete strongly-type arguments to a function.
trait Args<'a>: Func + Default {
    fn assign_to_field<T: cst::TreeIter<'a>>(
        &mut self,
        field_name: Str<'a>,
        arg: cst::ArgsItem<'a, T>,
        func_name: Str<'a>,
        value_source: Str<'a>,
        errs: &ErrorAccumulator<'a>,
    );

    fn build(
        func_name: Str<'a>,
        args: impl cst::ArgsIter<'a>,
        errs: &ErrorAccumulator<'a>,
    ) -> Option<Self> {
        let mut p: Self = Default::default();
        let start = errs.len();
        let mut field_names = Self::FIELD_NAMES.iter();
        let mut last_keyword_arg: Option<Str> = None;
        for arg in args {
            let (key, value_source) = match &arg {
                cst::ArgsItem::Regular {
                    key,
                    value: _,
                    value_source,
                } => (key, value_source.clone()),
                cst::ArgsItem::List {
                    key,
                    square_open: _,
                    tree,
                } => (key, tree.remaining_source()),
                cst::ArgsItem::Comment { .. } => continue,
            };
            let field_name = match key {
                // Positional argument
                None => {
                    if let Some(keyword_arg) = &last_keyword_arg {
                        errs.add(Error::PositionalArgAfterKeywordArg {
                            positional_arg: value_source,
                            keyword_arg: keyword_arg.clone(),
                        });
                        continue;
                    }
                    let Some(field_name) = field_names.next() else {
                        errs.add(Error::TooManyPositionalArgs {
                            extra_positional_arg: value_source,
                            function_name: func_name.clone(),
                            max_positional_args: Self::FIELD_NAMES.len(),
                        });
                        continue;
                    };
                    Str::new(field_name)
                }
                // Keyword argument
                Some(field_name) => {
                    last_keyword_arg = Some(Str {
                        end: value_source.end,
                        ..*field_name
                    });
                    field_name.clone()
                }
            };
            p.assign_to_field(field_name, arg, func_name.clone(), value_source, errs);
        }
        if errs.len() == start {
            Some(p)
        } else {
            None
        }
    }

    /// Lower the ith argument.
    fn lower_arg<'b>(&'b self, i: usize) -> Option<cst::ArgsItem<'a, CstTreeIter<'a, 'b>>>;
}

functions!(
    (
        struct Chars<'a> {
            content: Cow<'a, str>,
            font: i32,
        }
        impl Func {
            func_name: "chars",
            default_num_pos_arg: 1,
        }
        impl Horizontal {
            variant: Chars,
        }
        impl DiscretionaryElem {
            variant: Chars,
        }
    ),
    (
        struct Glue<'a> {
            width: common::Scaled,
            stretch: (common::Scaled, common::GlueOrder),
            shrink: (common::Scaled, common::GlueOrder),
        }
        impl Func {
            func_name: "glue",
            default_num_pos_arg: 3,
        }
        impl Horizontal {
            variant: Glue,
        }
        impl Vertical {
            variant: Glue,
        }
    ),
    (
        struct Penalty<'a> {
            value: i32,
        }
        impl Func {
            func_name: "penalty",
            default_num_pos_arg: 1,
        }
        impl Horizontal {
            variant: Penalty,
        }
        impl Vertical {
            variant: Penalty,
        }
    ),
    (
        struct Kern<'a> {
            width: common::Scaled,
        }
        impl Func {
            func_name: "kern",
            default_num_pos_arg: 1,
        }
        impl Horizontal {
            variant: Kern,
        }
        impl Vertical {
            variant: Kern,
        }
        impl DiscretionaryElem {
            variant: Kern,
        }
    ),
    (
        struct HBox<'a> {
            height: common::Scaled,
            width: common::Scaled,
            depth: common::Scaled,
            shift_amount: common::Scaled,
            glue_ratio: crate::ds::GlueRatio,
            glue_order: common::GlueOrder,
            content: Vec<Horizontal<'a>>,
        }
        impl Func {
            func_name: "hbox",
            default_num_pos_arg: 0,
        }
        impl Horizontal {
            variant: HBox,
        }
        impl Vertical {
            variant: HBox,
        }
        impl DiscretionaryElem {
            variant: HBox,
        }
    ),
    (
        struct Ligature<'a> {
            char: char,
            original_chars: Cow<'a, str>,
            font: i32,
        }
        impl Func {
            func_name: "lig",
            default_num_pos_arg: 2,
        }
        impl Horizontal {
            variant: Ligature,
        }
        impl DiscretionaryElem {
            variant: Ligature,
        }
    ),
    (
        struct VBox<'a> {
            height: common::Scaled,
            width: common::Scaled,
            depth: common::Scaled,
            shift_amount: common::Scaled,
            content: Vec<Vertical<'a>>,
        }
        impl Func {
            func_name: "vbox",
            default_num_pos_arg: 0,
        }
        impl Horizontal {
            variant: VBox,
        }
        impl Vertical {
            variant: VBox,
        }
        impl DiscretionaryElem {
            variant: VBox,
        }
    ),
    (
        struct Discretionary<'a> {
            pre_break: Vec<DiscretionaryElem<'a>>,
            post_break: Vec<DiscretionaryElem<'a>>,
            replace_count: i32,
        }
        impl Func {
            func_name: "disc",
            default_num_pos_arg: 0,
        }
        impl Horizontal {
            variant: Discretionary,
        }
    ),
    (
        struct Rule<'a> {
            height: MaybeRunning,
            width: MaybeRunning,
            depth: MaybeRunning,
        }
        impl Func {
            func_name: "rule",
            default_num_pos_arg: 3,
        }
        impl Horizontal {
            variant: Rule,
        }
        impl Vertical {
            variant: Rule,
        }
        impl DiscretionaryElem {
            variant: Rule,
        }
    ),
    (
        struct Mark<'a> {
            dummy: i32,
        }
        impl Func {
            func_name: "mark",
            default_num_pos_arg: 0,
        }
        impl Horizontal {
            variant: Mark,
        }
        impl Vertical {
            variant: Mark,
        }
    ),
    (
        struct Adjust<'a> {
            content: Vec<Vertical<'a>>,
        }
        impl Func {
            func_name: "adjust",
            default_num_pos_arg: 0,
        }
        impl Horizontal {
            variant: Adjust,
        }
    ),
    (
        struct Insertion<'a> {
            box_number: i32,
            height: common::Scaled,
            split_max_depth: common::Scaled,
            split_top_skip_width: common::Scaled,
            split_top_skip_stretch: (common::Scaled, common::GlueOrder),
            split_top_skip_shrink: (common::Scaled, common::GlueOrder),
            float_penalty: i32,
            vbox: Vec<Vertical<'a>>,
        }
        impl Func {
            func_name: "insertion",
            default_num_pos_arg: 1,
        }
        impl Horizontal {
            variant: Insertion,
        }
        impl Vertical {
            variant: Insertion,
        }
    ),
    (
        struct Math<'a> {
            kind: Cow<'a, str>,
        }
        impl Func {
            func_name: "math",
            default_num_pos_arg: 1,
        }
        impl Horizontal {
            variant: Math,
        }
        impl Vertical {
            variant: Math,
        }
    ),
);

impl<'a> std::fmt::Display for VBox<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let v_box: VBox<'a> = self.clone();
        let tree = CstTreeIter::Other(&Horizontal::VBox(v_box));
        cst::pretty_print(f, tree)?;
        Ok(())
    }
}

/// An argument of type `T` to a function.
#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct Arg<'a, T> {
    /// Value of the argument.
    pub value: T,
    /// Source of the argument in Box source code.
    ///
    /// If this is [`None`], a value was not provided in source
    ///     code and the default value is being used.
    pub source: Option<Str<'a>>,
}

impl<'a, T> From<T> for Arg<'a, T> {
    fn from(value: T) -> Self {
        Arg {
            value,
            source: None,
        }
    }
}

// I think this private_bounds warning is a Rust bug?
// All of the impl methods are private to this module so the bound
// can't be seen from outside the module.
#[allow(private_bounds)]
impl<'a, T> Arg<'a, T>
where
    T: Value<'a>,
{
    fn assign<F: cst::TreeIter<'a>>(
        &mut self,
        arg: cst::ArgsItem<'a, F>,
        field_name: &'a str,
        func_name: Str<'a>,
        value_source: Str<'a>,
        errs: &ErrorAccumulator<'a>,
    ) {
        if let Some(first_assignment) = &self.source {
            errs.add(Error::DuplicateArgument {
                parameter_name: field_name,
                first_assignment: first_assignment.clone(),
                second_assignment: value_source,
            });
            return;
        }
        let (cast_result, value_type) = match arg {
            cst::ArgsItem::Regular { value, .. } => match &value {
                cst::Value::List(tree) => (T::try_cast_list(tree.iter(), errs), "a list"),
                cst::Value::Integer(i) => (T::try_cast_integer(*i), "an integer"),
                cst::Value::Scaled(scaled) => (T::try_cast_scaled(*scaled), "a number"),
                cst::Value::InfiniteGlue(scaled, glue_order) => (
                    T::try_cast_infinite_glue(*scaled, *glue_order),
                    "an infinite glue",
                ),
                cst::Value::String(cow) => (T::try_cast_string(cow.clone()), "a string"),
            },
            cst::ArgsItem::List { tree, .. } => (T::try_cast_list(tree, errs), "a list"),
            cst::ArgsItem::Comment { .. } => return,
        };
        match cast_result {
            Some(val) => {
                self.value = val;
                self.source = Some(value_source);
            }
            None => errs.add(Error::IncorrectType {
                wanted_type: T::DESCRIPTION,
                got_type: value_type,
                got_raw_value: value_source,
                function_name: func_name,
                parameter_name: field_name,
            }),
        }
    }
}

/// A rule dimension that is either a fixed length or running (i.e. determined by context).
///
/// Written as a dimension like `3pt` or as `"running"` for running in Box language.
/// A running dimension corresponds to [`super::ds::Rule::RUNNING`].
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum MaybeRunning {
    Running,
    Scaled(common::Scaled),
}

impl Default for MaybeRunning {
    fn default() -> Self {
        MaybeRunning::Scaled(common::Scaled::ZERO)
    }
}

impl MaybeRunning {
    pub fn to_scaled(self) -> common::Scaled {
        match self {
            MaybeRunning::Running => common::Scaled(i32::MIN),
            MaybeRunning::Scaled(s) => s,
        }
    }

    pub fn from_scaled(s: common::Scaled) -> Self {
        if s.0 == i32::MIN {
            MaybeRunning::Running
        } else {
            MaybeRunning::Scaled(s)
        }
    }
}

/// Values in the AST.
///
/// These can possibly be obtained from a [`cst::Value`]
///     and always lowered to a [`cst::Value`].
trait Value<'a>: Sized {
    const DESCRIPTION: &'static str;

    fn try_cast_integer(_i: i32) -> Option<Self> {
        None
    }
    fn try_cast_string(_s: Cow<'a, str>) -> Option<Self> {
        None
    }
    fn try_cast_scaled(_s: common::Scaled) -> Option<Self> {
        None
    }
    fn try_cast_infinite_glue(_s: common::Scaled, _o: common::GlueOrder) -> Option<Self> {
        None
    }
    fn try_cast_list<F: cst::TreeIter<'a>>(
        _value: F,
        _errs: &ErrorAccumulator<'a>,
    ) -> Option<Self> {
        None
    }

    fn lower<'b>(&'b self, key: Option<Str<'a>>) -> cst::ArgsItem<'a, CstTreeIter<'a, 'b>>;
}

impl<'a> Value<'a> for common::GlueOrder {
    const DESCRIPTION: &'static str = "a glue sign (normal, stretching or shrinking)";
    fn try_cast_string(s: Cow<'a, str>) -> Option<Self> {
        match s.as_ref() {
            "normal" => Some(Self::Normal),
            "fil" => Some(Self::Fil),
            "fill" => Some(Self::Fill),
            "filll" => Some(Self::Filll),
            _ => None,
        }
    }
    fn lower<'b>(&'b self, key: Option<Str<'a>>) -> cst::ArgsItem<'a, CstTreeIter<'a, 'b>> {
        use common::GlueOrder::*;
        cst::ArgsItem::Regular {
            key,
            value: cst::Value::String(
                match self {
                    Normal => "normal",
                    Fil => "fil",
                    Fill => "fill",
                    Filll => "filll",
                }
                .into(),
            ),
            value_source: "".into(),
        }
    }
}

impl<'a> Value<'a> for crate::ds::GlueRatio {
    const DESCRIPTION: &'static str = "a glue ratio (floating point number)";
    fn try_cast_string(s: Cow<'a, str>) -> Option<Self> {
        crate::ds::GlueRatio::from_float_str(&s)
    }
    fn lower<'b>(&'b self, key: Option<Str<'a>>) -> cst::ArgsItem<'a, CstTreeIter<'a, 'b>> {
        cst::ArgsItem::Regular {
            key,
            value: cst::Value::String(format!["{}", self].into()),
            value_source: "".into(),
        }
    }
}

impl<'a> Value<'a> for char {
    const DESCRIPTION: &'static str = "a character (i.e. a string containing single character)";
    fn try_cast_string(s: Cow<'a, str>) -> Option<Self> {
        let mut iter = s.chars();
        let c = iter.next()?;
        match iter.next() {
            Some(_) => None,
            None => Some(c),
        }
    }
    fn lower<'b>(&'b self, key: Option<Str<'a>>) -> cst::ArgsItem<'a, CstTreeIter<'a, 'b>> {
        let s = format!("{self}");
        cst::ArgsItem::Regular {
            key,
            value: cst::Value::String(s.into()),
            value_source: "".into(),
        }
    }
}

impl<'a> Value<'a> for Cow<'a, str> {
    const DESCRIPTION: &'static str = "a string";
    fn try_cast_string(s: Cow<'a, str>) -> Option<Self> {
        Some(s)
    }
    fn lower<'b>(&'b self, key: Option<Str<'a>>) -> cst::ArgsItem<'a, CstTreeIter<'a, 'b>> {
        cst::ArgsItem::Regular {
            key,
            value: cst::Value::String(self.clone()),
            value_source: "".into(),
        }
    }
}

impl<'a> Value<'a> for i32 {
    const DESCRIPTION: &'static str = "an integer";
    fn try_cast_integer(i: i32) -> Option<Self> {
        Some(i)
    }
    fn lower<'b>(&'b self, key: Option<Str<'a>>) -> cst::ArgsItem<'a, CstTreeIter<'a, 'b>> {
        cst::ArgsItem::Regular {
            key,
            value: cst::Value::Integer(*self),
            value_source: "".into(),
        }
    }
}

impl<'a> Value<'a> for common::Scaled {
    const DESCRIPTION: &'static str = "a number";
    fn try_cast_scaled(s: common::Scaled) -> Option<Self> {
        Some(s)
    }
    fn lower<'b>(&'b self, key: Option<Str<'a>>) -> cst::ArgsItem<'a, CstTreeIter<'a, 'b>> {
        cst::ArgsItem::Regular {
            key,
            value: cst::Value::Scaled(*self),
            value_source: "".into(),
        }
    }
}

impl<'a> Value<'a> for MaybeRunning {
    const DESCRIPTION: &'static str = "a dimension or *";
    fn try_cast_scaled(s: common::Scaled) -> Option<Self> {
        Some(MaybeRunning::Scaled(s))
    }
    fn try_cast_string(s: Cow<'a, str>) -> Option<Self> {
        if s == "running" {
            Some(MaybeRunning::Running)
        } else {
            None
        }
    }
    fn lower<'b>(&'b self, key: Option<Str<'a>>) -> cst::ArgsItem<'a, CstTreeIter<'a, 'b>> {
        let value = match self {
            MaybeRunning::Running => cst::Value::String("running".into()),
            MaybeRunning::Scaled(s) => cst::Value::Scaled(*s),
        };
        cst::ArgsItem::Regular {
            key,
            value,
            value_source: "".into(),
        }
    }
}

impl<'a> Value<'a> for (common::Scaled, common::GlueOrder) {
    const DESCRIPTION: &'static str = "a stretch or shrink glue component";
    fn try_cast_scaled(s: common::Scaled) -> Option<Self> {
        Some((s, common::GlueOrder::Normal))
    }
    fn try_cast_infinite_glue(s: common::Scaled, o: common::GlueOrder) -> Option<Self> {
        Some((s, o))
    }
    fn lower<'b>(&'b self, key: Option<Str<'a>>) -> cst::ArgsItem<'a, CstTreeIter<'a, 'b>> {
        cst::ArgsItem::Regular {
            key,
            value: cst::Value::InfiniteGlue(self.0, self.1),
            value_source: "".into(),
        }
    }
}

impl<'a> Value<'a> for Vec<Vertical<'a>> {
    const DESCRIPTION: &'static str = "a vbox";
    fn try_cast_list<F: cst::TreeIter<'a>>(value: F, errs: &ErrorAccumulator<'a>) -> Option<Self> {
        Some(parse_vbox_using_cst(value, errs))
    }
    fn lower<'b>(&'b self, key: Option<Str<'a>>) -> cst::ArgsItem<'a, CstTreeIter<'a, 'b>> {
        cst::ArgsItem::List {
            key,
            square_open: "".into(),
            tree: lower_vbox_impl(self),
        }
    }
}

impl<'a> Value<'a> for Vec<Horizontal<'a>> {
    const DESCRIPTION: &'static str = "a hbox";
    fn try_cast_list<F: cst::TreeIter<'a>>(value: F, errs: &ErrorAccumulator<'a>) -> Option<Self> {
        Some(parse_hbox_using_cst(value, errs))
    }
    fn lower<'b>(&'b self, key: Option<Str<'a>>) -> cst::ArgsItem<'a, CstTreeIter<'a, 'b>> {
        cst::ArgsItem::List {
            key,
            square_open: "".into(),
            tree: lower_hbox_impl(self),
        }
    }
}

impl<'a> Value<'a> for Vec<DiscretionaryElem<'a>> {
    const DESCRIPTION: &'static str = "a dlist";
    fn try_cast_list<F: cst::TreeIter<'a>>(value: F, errs: &ErrorAccumulator<'a>) -> Option<Self> {
        Some(parse_dlist_using_cst(value, errs))
    }
    fn lower<'b>(&'b self, key: Option<Str<'a>>) -> cst::ArgsItem<'a, CstTreeIter<'a, 'b>> {
        cst::ArgsItem::List {
            key,
            square_open: "".into(),
            tree: lower_dlist_impl(self),
        }
    }
}

fn value_to_cst<'a, 'b, V: Value<'a>>(
    value: &'b V,
    key: Option<Str<'a>>,
) -> cst::ArgsItem<'a, CstTreeIter<'a, 'b>> {
    // TODO: if the argument is positional and has its default value,
    // don't return anything.
    value.lower(key)
}

#[cfg(test)]
mod tests {
    use super::*;
    /// FYI: most of the tests are doc tests.
    #[test]
    fn hbox() {
        let input = r#"
            hbox(
                width=1pt,
                content=[chars("Hello")],
            )
        "#;

        let want = vec![Horizontal::HBox(HBox {
            width: Arg {
                value: common::Scaled::ONE.into(),
                source: Some("1pt".into()),
            },
            content: Arg {
                value: vec![Horizontal::Chars(Chars {
                    content: Arg {
                        value: "Hello".into(),
                        source: Some(r#""Hello""#.into()),
                    },
                    font: Arg {
                        value: 0,
                        source: None,
                    },
                    ..Default::default()
                })],
                source: Some(r#"[chars("Hello")]"#.into()),
            },
            ..Default::default()
        })];

        let got = parse_hbox(&input).expect("parsing succeeds");

        assert_eq!(got, want);
    }
}
