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

impl ToBoxLang for ds::Horizontal {
    type Output = ast::Horizontal<'static>;
    fn to_box_lang(&self) -> Self::Output {
        use boxworks::ds::Horizontal::*;
        match self {
            Char(char) => ast::Horizontal::Text(char.to_box_lang()),
            HList(hlist) => ast::Horizontal::Hlist(hlist.to_box_lang()),
            VList(_vlist) => todo!(),
            Rule(_rule) => todo!(),
            Mark(_mark) => todo!(),
            Insertion(_insertion) => todo!(),
            Adjust(_adjust) => todo!(),
            Ligature(ligature) => todo!("ligature {ligature:?}"),
            Discretionary(_discretionary) => todo!(),
            Whatsit(_whatsit) => todo!(),
            Math(_math) => todo!(),
            Glue(glue) => ast::Horizontal::Glue(glue.to_box_lang()),
            Kern(kern) => ast::Horizontal::Kern(kern.to_box_lang()),
            Penalty(_penalty) => todo!(),
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

impl<'a> ToBoxworks for ast::Horizontal<'a> {
    type Output = Vec<ds::Horizontal>;

    fn to_boxworks(&self) -> Self::Output {
        use ast::Horizontal::*;
        match self {
            Text(text_args) => text_args.to_boxworks(),
            Glue(glue_args) => vec![ds::Horizontal::Glue(glue_args.to_boxworks())],
            Kern(kern_args) => vec![ds::Horizontal::Kern(kern_args.to_boxworks())],
            Hlist(hlist_args) => vec![ds::Horizontal::HList(hlist_args.to_boxworks())],
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
