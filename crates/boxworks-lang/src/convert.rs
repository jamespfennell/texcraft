//! Conversions between Box language and Boxworks data structures

use std::borrow::Cow;

use crate::ast;
use boxworks::ds;

/// Convert a Boxworks data structure to a Box language data structure.
pub trait ToBoxLang {
    type Output;
    fn to_box_lang(&self) -> Self::Output;
}

/// Convert a Box language data structure to a Boxworks data structure.
pub trait ToBoxworks {
    type Output;
    fn to_boxworks(&self) -> Self::Output;
}

impl ToBoxLang for ds::Vertical {
    type Output = ast::Vertical<'static>;
    fn to_box_lang(&self) -> Self::Output {
        use boxworks::ds::Vertical::*;
        match self {
            HList(hlist) => ast::Vertical::Hlist(hlist.to_box_lang()),
            VList(vlist) => ast::Vertical::Vlist(vlist.to_box_lang()),
            Rule(rule) => ast::Vertical::Rule(rule.to_box_lang()),
            Mark(mark) => ast::Vertical::Mark(mark.to_box_lang()),
            Insertion(insertion) => ast::Vertical::Insertion(insertion.to_box_lang()),
            Whatsit(_whatsit) => todo!(),
            Math(math) => ast::Vertical::Math(math.to_box_lang()),
            Glue(glue) => ast::Vertical::Glue(glue.to_box_lang()),
            Kern(kern) => ast::Vertical::Kern(kern.to_box_lang()),
            Penalty(penalty) => ast::Vertical::Penalty(penalty.to_box_lang()),
        }
    }
}

impl<'a> ToBoxworks for ast::Vertical<'a> {
    type Output = ds::Vertical;

    fn to_boxworks(&self) -> Self::Output {
        use ast::Vertical::*;
        match self {
            Hlist(hlist_args) => ds::Vertical::HList(hlist_args.to_boxworks()),
            Vlist(vlist_args) => ds::Vertical::VList(vlist_args.to_boxworks()),
            Glue(glue_args) => ds::Vertical::Glue(glue_args.to_boxworks()),
            Kern(kern_args) => ds::Vertical::Kern(kern_args.to_boxworks()),
            Penalty(penalty_args) => ds::Vertical::Penalty(penalty_args.to_boxworks()),
            Rule(rule_args) => ds::Vertical::Rule(rule_args.to_boxworks()),
            Mark(mark_args) => ds::Vertical::Mark(mark_args.to_boxworks()),
            Insertion(insertion_args) => ds::Vertical::Insertion(insertion_args.to_boxworks()),
            Math(math_args) => ds::Vertical::Math(math_args.to_boxworks()),
        }
    }
}

impl ToBoxLang for ds::Horizontal {
    type Output = ast::Horizontal<'static>;
    fn to_box_lang(&self) -> Self::Output {
        use boxworks::ds::Horizontal::*;
        match self {
            Char(char) => ast::Horizontal::Text(char.to_box_lang()),
            HList(hlist) => ast::Horizontal::Hlist(hlist.to_box_lang()),
            VList(vlist) => ast::Horizontal::Vlist(vlist.to_box_lang()),
            Rule(rule) => ast::Horizontal::Rule(rule.to_box_lang()),
            Mark(mark) => ast::Horizontal::Mark(mark.to_box_lang()),
            Insertion(insertion) => ast::Horizontal::Insertion(insertion.to_box_lang()),
            Adjust(adjust) => ast::Horizontal::Adjust(adjust.to_box_lang()),
            Ligature(ligature) => ast::Horizontal::Ligature(ligature.to_box_lang()),
            Discretionary(discretionary) => {
                ast::Horizontal::Discretionary(discretionary.to_box_lang())
            }
            Whatsit(_whatsit) => todo!(),
            Math(math) => ast::Horizontal::Math(math.to_box_lang()),
            Glue(glue) => ast::Horizontal::Glue(glue.to_box_lang()),
            Kern(kern) => ast::Horizontal::Kern(kern.to_box_lang()),
            Penalty(penalty) => ast::Horizontal::Penalty(penalty.to_box_lang()),
        }
    }
}

impl<T: ToBoxLang> ToBoxLang for Vec<T> {
    type Output = Vec<<T as ToBoxLang>::Output>;
    fn to_box_lang(&self) -> Self::Output {
        self.iter().map(|b| b.to_box_lang()).collect()
    }
}

impl<'a> ToBoxworks for Vec<ast::Horizontal<'a>> {
    type Output = Vec<ds::Horizontal>;
    fn to_boxworks(&self) -> Self::Output {
        self.iter().flat_map(|b| b.to_boxworks()).collect()
    }
}

impl<'a> ToBoxworks for Vec<ast::Vertical<'a>> {
    type Output = Vec<ds::Vertical>;
    fn to_boxworks(&self) -> Self::Output {
        self.iter().map(|b| b.to_boxworks()).collect()
    }
}

impl<'a> ToBoxworks for ast::Horizontal<'a> {
    type Output = Vec<ds::Horizontal>;

    fn to_boxworks(&self) -> Self::Output {
        use ast::Horizontal::*;
        match self {
            Text(text_args) => text_args.to_boxworks(),
            Glue(glue_args) => vec![ds::Horizontal::Glue(glue_args.to_boxworks())],
            Kern(kern_args) => vec![ds::Horizontal::Kern(kern_args.to_boxworks())],
            Hlist(hlist_args) => vec![ds::Horizontal::HList(hlist_args.to_boxworks())],
            Vlist(vlist_args) => vec![ds::Horizontal::VList(vlist_args.to_boxworks())],
            Ligature(lig_args) => vec![ds::Horizontal::Ligature(lig_args.to_boxworks())],
            Discretionary(disc_args) => {
                vec![ds::Horizontal::Discretionary(disc_args.to_boxworks())]
            }
            Rule(rule_args) => vec![ds::Horizontal::Rule(rule_args.to_boxworks())],
            Penalty(penalty_args) => vec![ds::Horizontal::Penalty(penalty_args.to_boxworks())],
            Mark(mark_args) => vec![ds::Horizontal::Mark(mark_args.to_boxworks())],
            Adjust(adjust_args) => vec![ds::Horizontal::Adjust(adjust_args.to_boxworks())],
            Insertion(insertion_args) => {
                vec![ds::Horizontal::Insertion(insertion_args.to_boxworks())]
            }
            Math(math_args) => vec![ds::Horizontal::Math(math_args.to_boxworks())],
        }
    }
}

impl ToBoxLang for ds::VList {
    type Output = ast::Vlist<'static>;
    fn to_box_lang(&self) -> Self::Output {
        ast::Vlist {
            content: self.list.to_box_lang().into(),
        }
    }
}

impl ToBoxLang for ds::HList {
    type Output = ast::Hlist<'static>;
    fn to_box_lang(&self) -> Self::Output {
        ast::Hlist {
            width: self.width.into(),
            content: self.list.to_box_lang().into(),
        }
    }
}

impl<'a> ToBoxworks for ast::Hlist<'a> {
    type Output = ds::HList;
    fn to_boxworks(&self) -> Self::Output {
        ds::HList {
            width: self.width.value,
            list: self.content.value.to_boxworks(),
            // TODO: all the other stuff
            ..Default::default()
        }
    }
}

impl<'a> ToBoxworks for ast::Vlist<'a> {
    type Output = ds::VList;
    fn to_boxworks(&self) -> Self::Output {
        ds::VList {
            list: self.content.value.to_boxworks(),
            // TODO: all the other stuff
            ..Default::default()
        }
    }
}

impl<'a> ToBoxworks for ast::Ligature<'a> {
    type Output = ds::Ligature;

    fn to_boxworks(&self) -> Self::Output {
        ds::Ligature {
            included_left_boundary: false,  // TODO
            included_right_boundary: false, // TODO,
            char: self.char.value,
            font: self.font.value as u32,
            original_chars: self.original_chars.value.clone().into(),
        }
    }
}

impl ToBoxLang for ds::Ligature {
    type Output = ast::Ligature<'static>;

    fn to_box_lang(&self) -> Self::Output {
        ast::Ligature {
            char: self.char.into(),
            original_chars: Cow::<'static, str>::Owned(format!["{}", self.original_chars]).into(),
            font: (self.font as i32).into(),
        }
    }
}

impl ToBoxLang for ds::Char {
    type Output = ast::Text<'static>;
    fn to_box_lang(&self) -> Self::Output {
        ast::Text {
            content: Cow::<'static, str>::Owned(format!["{}", self.char]).into(),
            font: (self.font as i32).into(),
        }
    }
}

impl<'a> ToBoxworks for ast::Text<'a> {
    type Output = Vec<ds::Horizontal>;
    fn to_boxworks(&self) -> Self::Output {
        // TODO: accept a text preprocessor
        self.content
            .value
            .as_ref()
            .chars()
            .map(|char| {
                if char.is_whitespace() {
                    ds::Glue {
                        kind: ds::GlueKind::Normal,
                        value: core::Glue {
                            width: core::Scaled::ONE * 10,
                            stretch: core::Scaled::ONE * 4,
                            shrink: core::Scaled::ONE * 4,
                            ..Default::default()
                        },
                    }
                    .into()
                } else {
                    ds::Char {
                        char,
                        font: self.font.value as u32,
                    }
                    .into()
                }
            })
            .collect()
    }
}

impl ToBoxLang for ds::Penalty {
    type Output = ast::Penalty<'static>;
    fn to_box_lang(&self) -> Self::Output {
        ast::Penalty {
            value: self.value.into(),
        }
    }
}

impl<'a> ToBoxworks for ast::Penalty<'a> {
    type Output = ds::Penalty;
    fn to_boxworks(&self) -> Self::Output {
        ds::Penalty {
            value: self.value.value,
        }
    }
}

impl ToBoxLang for ds::Glue {
    type Output = ast::Glue<'static>;
    fn to_box_lang(&self) -> Self::Output {
        ast::Glue {
            width: self.value.width.into(),
            stretch: (self.value.stretch, self.value.stretch_order).into(),
            shrink: (self.value.shrink, self.value.shrink_order).into(),
        }
    }
}

impl<'a> ToBoxworks for ast::Glue<'a> {
    type Output = ds::Glue;
    fn to_boxworks(&self) -> Self::Output {
        ds::Glue {
            value: core::Glue {
                width: self.width.value,
                stretch: self.stretch.value.0,
                stretch_order: self.stretch.value.1,
                shrink: self.shrink.value.0,
                shrink_order: self.shrink.value.1,
            },
            kind: ds::GlueKind::Normal,
        }
    }
}

impl ToBoxLang for ds::Kern {
    type Output = ast::Kern<'static>;
    fn to_box_lang(&self) -> Self::Output {
        ast::Kern {
            width: self.width.into(),
        }
    }
}

impl<'a> ToBoxworks for ast::Kern<'a> {
    type Output = ds::Kern;
    fn to_boxworks(&self) -> Self::Output {
        ds::Kern {
            width: self.width.value,
            kind: ds::KernKind::Normal,
        }
    }
}

impl ToBoxLang for ds::Discretionary {
    type Output = ast::Discretionary<'static>;
    fn to_box_lang(&self) -> Self::Output {
        ast::Discretionary {
            pre_break: self.pre_break.to_box_lang().into(),
            post_break: self.post_break.to_box_lang().into(),
            replace_count: (self.replace_count as i32).into(),
        }
    }
}

impl<'a> ToBoxworks for ast::Discretionary<'a> {
    type Output = ds::Discretionary;
    fn to_boxworks(&self) -> Self::Output {
        ds::Discretionary {
            pre_break: self.pre_break.value.to_boxworks(),
            post_break: self.post_break.value.to_boxworks(),
            replace_count: self.replace_count.value as u32,
        }
    }
}

impl ToBoxLang for ds::Rule {
    type Output = ast::Rule<'static>;
    fn to_box_lang(&self) -> Self::Output {
        ast::Rule {
            height: self.height.into(),
            width: self.width.into(),
            depth: self.depth.into(),
        }
    }
}

impl<'a> ToBoxworks for ast::Rule<'a> {
    type Output = ds::Rule;
    fn to_boxworks(&self) -> Self::Output {
        ds::Rule {
            height: self.height.value,
            width: self.width.value,
            depth: self.depth.value,
        }
    }
}

impl ToBoxLang for ds::Mark {
    type Output = ast::Mark<'static>;
    fn to_box_lang(&self) -> Self::Output {
        ast::Mark::default()
    }
}

impl<'a> ToBoxworks for ast::Mark<'a> {
    type Output = ds::Mark;
    fn to_boxworks(&self) -> Self::Output {
        ds::Mark { list: vec![] }
    }
}

impl ToBoxLang for ds::Adjust {
    type Output = ast::Adjust<'static>;
    fn to_box_lang(&self) -> Self::Output {
        ast::Adjust {
            content: self.list.to_box_lang().into(),
        }
    }
}

impl<'a> ToBoxworks for ast::Adjust<'a> {
    type Output = ds::Adjust;
    fn to_boxworks(&self) -> Self::Output {
        ds::Adjust {
            list: self.content.value.to_boxworks(),
        }
    }
}

impl ToBoxLang for ds::Insertion {
    type Output = ast::Insertion<'static>;
    fn to_box_lang(&self) -> Self::Output {
        ast::Insertion {
            box_number: (self.box_number as i32).into(),
            height: self.height.into(),
            split_max_depth: self.split_max_depth.into(),
            split_top_skip_width: self.split_top_skip.width.into(),
            split_top_skip_stretch: (
                self.split_top_skip.stretch,
                self.split_top_skip.stretch_order,
            )
                .into(),
            split_top_skip_shrink: (self.split_top_skip.shrink, self.split_top_skip.shrink_order)
                .into(),
            float_penalty: (self.float_penalty as i32).into(),
            vlist: self.vlist.to_box_lang().into(),
        }
    }
}

impl<'a> ToBoxworks for ast::Insertion<'a> {
    type Output = ds::Insertion;
    fn to_boxworks(&self) -> Self::Output {
        ds::Insertion {
            box_number: self.box_number.value as u8,
            height: self.height.value,
            split_max_depth: self.split_max_depth.value,
            split_top_skip: core::Glue {
                width: self.split_top_skip_width.value,
                stretch: self.split_top_skip_stretch.value.0,
                stretch_order: self.split_top_skip_stretch.value.1,
                shrink: self.split_top_skip_shrink.value.0,
                shrink_order: self.split_top_skip_shrink.value.1,
            },
            float_penalty: self.float_penalty.value as u32,
            vlist: self.vlist.value.to_boxworks(),
        }
    }
}

impl ToBoxLang for ds::Math {
    type Output = ast::Math<'static>;
    fn to_box_lang(&self) -> Self::Output {
        ast::Math {
            kind: match self {
                ds::Math::Before => Cow::Borrowed("before"),
                ds::Math::After => Cow::Borrowed("after"),
            }
            .into(),
        }
    }
}

impl<'a> ToBoxworks for ast::Math<'a> {
    type Output = ds::Math;
    fn to_boxworks(&self) -> Self::Output {
        match self.kind.value.as_ref() {
            "after" => ds::Math::After,
            _ => ds::Math::Before,
        }
    }
}
