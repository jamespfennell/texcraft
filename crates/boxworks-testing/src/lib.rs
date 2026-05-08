//! Testing utilities for Boxworks-based code.
//!
//! This crate makes writing tests for Boxworks code easier.
//! It provides the [`assert_box_eq`] macro,
//!     which validates that two Boxworks data structures are the same.
//! The data structures can be provided as Rust values from [`boxworks::ds`],
//!     or as Boxworks language strings.
//! If there is a mismatch, the diff is printed nicely.
//!
//! ## Example
//!
//! ```rust
//! # use boxworks::ds;
//! # use boxworks_testing::assert_box_eq;
//! let v_box = ds::VBox {
//!     list: vec![ds::HBox {
//!         list: vec![
//!             ds::Char { char: 'A', font: 33 }.into(),
//!             ds::Char { char: 'Z', font: 33 }.into(),
//!         ],
//!         ..Default::default()
//!     }.into()],
//!     ..Default::default()
//! };
//! assert_box_eq!(
//!     v_box,
//!     r#"
//!     vbox(
//!       content=[
//!         hbox(
//!           content=[
//!             chars("AZ", 33)
//!           ]
//!         )
//!       ]
//!     )
//!     "#,
//! );
//! ```

use boxworks::ds;
use boxworks::lang;
use boxworks::lang::convert::ToBoxLang;

#[macro_export]
macro_rules! assert_box_eq {
    ($left:expr, $right:expr$(,)?) => {{
        let lhs: boxworks_testing::Value = $left.into();
        let rhs: boxworks_testing::Value = $right.into();
        boxworks_testing::assert_eq(lhs, rhs);
    }};
}

pub enum Value {
    String(String),
    Box(Vec<ds::Horizontal>),
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Value::String(value.into())
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::String(value)
    }
}

impl From<ds::VBox> for Value {
    fn from(value: ds::VBox) -> Self {
        Value::Box(vec![ds::Horizontal::VBox(value)])
    }
}

pub fn assert_eq(lhs: Value, rhs: Value) {
    let (lhs_list, lhs_s) = normalize(lhs, "lhs.box");
    let (rhs_list, rhs_s) = normalize(rhs, "rhs.box");
    // We first diff the boxlang representation because this is clearer.
    use pretty_assertions::assert_eq;
    assert_eq!(lhs_s, rhs_s);
    // But we also diff the data structure, in case the ds to lang process is lossy
    // and causes different lists be the same.
    assert_eq!(rhs_list, lhs_list);
}

fn normalize(val: Value, side: &str) -> (Vec<ds::Horizontal>, String) {
    let list = match val {
        Value::String(s) => match lang::parse_horizontal_list(&s.clone()) {
            Ok(v) => v,
            Err(err) => {
                let source = ariadne::Source::from(s);
                let cache: (&str, _) = (side, source);
                for err in err {
                    err.ariadne_report(side).eprint(cache.clone()).unwrap();
                }
                panic!("failed to parse boxlang input; errors printed above.")
            }
        },
        Value::Box(v) => v,
    };
    let mut s = String::new();
    for elem in &list {
        use std::fmt::Write;
        write!(&mut s, "{}", elem.to_box_lang()).unwrap();
    }
    (list, s)
}
