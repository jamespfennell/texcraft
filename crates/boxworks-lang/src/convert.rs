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
            HBox(hbox) => ast::Vertical::HBox(hbox.to_box_lang()),
            VBox(vbox) => ast::Vertical::VBox(vbox.to_box_lang()),
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
            HBox(hbox_args) => ds::Vertical::HBox(hbox_args.to_boxworks()),
            VBox(vbox_args) => ds::Vertical::VBox(vbox_args.to_boxworks()),
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

impl ToBoxLang for Vec<ds::Horizontal> {
    type Output = Vec<ast::Horizontal<'static>>;
    fn to_box_lang(&self) -> Self::Output {
        let mut out = vec![];
        let mut current_font: Option<u32> = None;
        let mut buf: String = Default::default();
        let flush_chars = |out: &mut Vec<ast::Horizontal<'static>>,
                           current_font: &mut Option<u32>,
                           buf: &mut String| {
            let Some(current_font) = current_font.take() else {
                // Nothing to flush.
                return;
            };
            let current_font: i32 = current_font.try_into().unwrap();
            out.push(ast::Horizontal::Chars(ast::Chars {
                content: Cow::<str>::Owned(buf.clone()).into(),
                font: current_font.into(),
            }));
            buf.clear();
        };
        for elem in self {
            match elem {
                ds::Horizontal::Char(ds::Char { char, font }) => {
                    if Some(*font) != current_font {
                        flush_chars(&mut out, &mut current_font, &mut buf);
                    }
                    current_font = Some(*font);
                    buf.push(*char);
                }
                _ => {
                    flush_chars(&mut out, &mut current_font, &mut buf);
                    out.push(elem.to_box_lang());
                }
            }
        }
        flush_chars(&mut out, &mut current_font, &mut buf);
        out
    }
}

impl ToBoxLang for Vec<ds::Vertical> {
    type Output = Vec<ast::Vertical<'static>>;
    fn to_box_lang(&self) -> Self::Output {
        self.iter().map(|b| b.to_box_lang()).collect()
    }
}

impl ToBoxLang for Vec<ds::DiscretionaryElem> {
    type Output = Vec<ast::DiscretionaryElem<'static>>;
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

impl<'a> ToBoxworks for Vec<ast::DiscretionaryElem<'a>> {
    type Output = Vec<ds::DiscretionaryElem>;
    fn to_boxworks(&self) -> Self::Output {
        self.iter().flat_map(|b| b.to_boxworks()).collect()
    }
}

impl<'a> ToBoxworks for ast::Horizontal<'a> {
    type Output = Vec<ds::Horizontal>;

    fn to_boxworks(&self) -> Self::Output {
        use ast::Horizontal::*;
        match self {
            Chars(chars_args) => chars_args.to_boxworks(),
            Glue(glue_args) => vec![ds::Horizontal::Glue(glue_args.to_boxworks())],
            Kern(kern_args) => vec![ds::Horizontal::Kern(kern_args.to_boxworks())],
            HBox(hbox_args) => vec![ds::Horizontal::HBox(hbox_args.to_boxworks())],
            VBox(vbox_args) => vec![ds::Horizontal::VBox(vbox_args.to_boxworks())],
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

impl ToBoxLang for ds::Horizontal {
    type Output = ast::Horizontal<'static>;
    fn to_box_lang(&self) -> Self::Output {
        use boxworks::ds::Horizontal::*;
        match self {
            Char(char) => ast::Horizontal::Chars(char.to_box_lang()),
            HBox(hbox) => ast::Horizontal::HBox(hbox.to_box_lang()),
            VBox(vbox) => ast::Horizontal::VBox(vbox.to_box_lang()),
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

impl<'a> ToBoxworks for ast::DiscretionaryElem<'a> {
    type Output = Vec<ds::DiscretionaryElem>;

    fn to_boxworks(&self) -> Self::Output {
        use ast::DiscretionaryElem::*;
        use ds::DiscretionaryElem as Out;
        match self {
            Chars(chars_args) => chars_to_discretionary_elems(chars_args),
            Kern(kern_args) => vec![Out::Kern(kern_args.to_boxworks())],
            HBox(hbox_args) => vec![Out::HBox(hbox_args.to_boxworks())],
            VBox(vbox_args) => vec![Out::VBox(vbox_args.to_boxworks())],
            Ligature(lig_args) => vec![Out::Ligature(lig_args.to_boxworks())],
            Rule(rule_args) => vec![Out::Rule(rule_args.to_boxworks())],
        }
    }
}

impl ToBoxLang for ds::DiscretionaryElem {
    type Output = ast::DiscretionaryElem<'static>;
    fn to_box_lang(&self) -> Self::Output {
        use ast::DiscretionaryElem as Out;
        use boxworks::ds::DiscretionaryElem::*;
        match self {
            Char(char) => Out::Chars(char.to_box_lang()),
            HBox(hbox) => Out::HBox(hbox.to_box_lang()),
            VBox(vbox) => Out::VBox(vbox.to_box_lang()),
            Rule(rule) => Out::Rule(rule.to_box_lang()),
            Ligature(ligature) => Out::Ligature(ligature.to_box_lang()),
            Kern(kern) => Out::Kern(kern.to_box_lang()),
        }
    }
}

impl ToBoxLang for ds::VBox {
    type Output = ast::VBox<'static>;
    fn to_box_lang(&self) -> Self::Output {
        ast::VBox {
            content: self.list.to_box_lang().into(),
        }
    }
}

impl ToBoxLang for ds::HBox {
    type Output = ast::HBox<'static>;
    fn to_box_lang(&self) -> Self::Output {
        ast::HBox {
            width: self.width.into(),
            content: self.list.to_box_lang().into(),
        }
    }
}

impl<'a> ToBoxworks for ast::HBox<'a> {
    type Output = ds::HBox;
    fn to_boxworks(&self) -> Self::Output {
        ds::HBox {
            width: self.width.value,
            list: self.content.value.to_boxworks(),
            // TODO: all the other stuff
            ..Default::default()
        }
    }
}

impl<'a> ToBoxworks for ast::VBox<'a> {
    type Output = ds::VBox;
    fn to_boxworks(&self) -> Self::Output {
        ds::VBox {
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
    type Output = ast::Chars<'static>;
    fn to_box_lang(&self) -> Self::Output {
        ast::Chars {
            content: Cow::<'static, str>::Owned(format!["{}", self.char]).into(),
            font: (self.font as i32).into(),
        }
    }
}

impl<'a> ToBoxworks for ast::Chars<'a> {
    type Output = Vec<ds::Horizontal>;
    fn to_boxworks(&self) -> Self::Output {
        self.content
            .value
            .chars()
            .map(|c| {
                ds::Horizontal::Char(ds::Char {
                    char: c,
                    font: self.font.value as u32,
                })
            })
            .collect()
    }
}

fn chars_to_discretionary_elems<'a>(chars: &ast::Chars<'a>) -> Vec<ds::DiscretionaryElem> {
    chars
        .content
        .value
        .chars()
        .map(|c| {
            ds::DiscretionaryElem::Char(ds::Char {
                char: c,
                font: chars.font.value as u32,
            })
        })
        .collect()
}

impl ToBoxLang for ds::Penalty {
    type Output = ast::Penalty<'static>;
    fn to_box_lang(&self) -> Self::Output {
        ast::Penalty {
            value: self.0.into(),
        }
    }
}

impl<'a> ToBoxworks for ast::Penalty<'a> {
    type Output = ds::Penalty;
    fn to_boxworks(&self) -> Self::Output {
        ds::Penalty(self.value.value)
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
            value: common::Glue {
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
            vbox: self.vbox.to_box_lang().into(),
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
            split_top_skip: common::Glue {
                width: self.split_top_skip_width.value,
                stretch: self.split_top_skip_stretch.value.0,
                stretch_order: self.split_top_skip_stretch.value.1,
                shrink: self.split_top_skip_shrink.value.0,
                shrink_order: self.split_top_skip_shrink.value.1,
            },
            float_penalty: self.float_penalty.value as u32,
            vbox: self.vbox.value.to_boxworks(),
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

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! tests {
        ( $( ($name: ident, $input: expr, $want: expr,), )+ ) => {
            $(
                #[test]
                fn $name() {
                    let input: Vec<ds::Horizontal> = $input;
                    let want: Vec<ast::Horizontal<'static>> = $want;
                    let got = input.to_box_lang();
                    assert_eq!(got, want);
                }
            )+
        };
    }

    tests!(
        (
            chars_same_font,
            vec![
                ds::Char { char: 'B', font: 0 }.into(),
                ds::Char { char: 'o', font: 0 }.into(),
                ds::Char { char: 'x', font: 0 }.into(),
            ],
            vec![ast::Chars {
                content: Cow::Borrowed("Box").into(),
                font: 0_i32.into(),
            }
            .into()],
        ),
        (
            chars_different_font,
            vec![
                ds::Char { char: 'B', font: 0 }.into(),
                ds::Char { char: 'o', font: 0 }.into(),
                ds::Char { char: 'x', font: 0 }.into(),
                ds::Char { char: 'e', font: 1 }.into(),
                ds::Char { char: 'd', font: 1 }.into(),
            ],
            vec![
                ast::Chars {
                    content: Cow::Borrowed("Box").into(),
                    font: 0_i32.into(),
                }
                .into(),
                ast::Chars {
                    content: Cow::Borrowed("ed").into(),
                    font: 1_i32.into(),
                }
                .into(),
            ],
        ),
    );
}
