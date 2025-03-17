//! # Boxworks language
//!
//! This crate defines a domain-specific language (DSL) for Boxworks.
//! This language is used to describe Boxworks elements in Knuth's box-and-glue model.
//! The initial motivation for the language is to make it
//!     easy to create Boxworks primitives,
//!     like horizontal and vertical lists,
//!     for use in unit testing.
//!
//! In the long run, the language will probably support running the Boxworks engine
//!     and actually performing typesetting.
//! If this happens, this language will be a sort of "intermediate representation"
//!     for the Texcraft project.
//!
//! This is a basic example of converting some of the language into a
//! horizontal list:
//!
//! ```
//! use core;
//! use boxworks::ds;
//! use boxworks_lang as bwl;
//!
//! let source = r#"
//!     ## The text() function typesets some text.
//!     ## The language automatically converts spaces into glue.
//!     text("Box Lang")
//!     ## Or glue can be added manually.
//!     glue(1pt, 5fil, 0.075in)
//!     ## The following elements illustrate the prototypical example of a kern.
//!     text("A")
//!     kern(-0.1pt)
//!     text("V")
//! "#;
//! let got = bwl::parse_horizontal_list(&source);
//! let want: Vec<ds::Horizontal> = vec![
//!     ds::Char{char: 'B', font: 0}.into(),
//!     ds::Char{char: 'o', font: 0}.into(),
//!     ds::Char{char: 'x', font: 0}.into(),
//!     ds::Glue{
//!         kind: ds::GlueKind::Normal,
//!         // Right now spaces are converted to the glue 10pt plus 4pt minus 4pt.
//!         // We will eventually support customizing this.
//!         value: core::Glue{
//!             width: core::Scaled::ONE * 10,
//!             stretch: core::Scaled::ONE * 4,
//!             stretch_order: core::GlueOrder::Normal,
//!             shrink: core::Scaled::ONE * 4,
//!             shrink_order: core::GlueOrder::Normal,
//!         }
//!     }.into(),
//!     ds::Char{char: 'L', font: 0}.into(),
//!     ds::Char{char: 'a', font: 0}.into(),
//!     ds::Char{char: 'n', font: 0}.into(),
//!     ds::Char{char: 'g', font: 0}.into(),
//!     ds::Glue{
//!         kind: ds::GlueKind::Normal,
//!         value: core::Glue{
//!             width: core::Scaled::new(
//!                 1,  // integer part
//!                 core::Scaled::ZERO,  // fractional part
//!                 core::ScaledUnit::Point,  // units
//!             ).unwrap(),
//!             stretch: core::Scaled::new(
//!                 5,  // integer part
//!                 core::Scaled::ZERO,  // fractional part
//!                 core::ScaledUnit::Point,  // units
//!             ).unwrap(),
//!             stretch_order: core::GlueOrder::Fil,
//!             shrink: core::Scaled::new(
//!                 0,  // integer part
//!                 core::Scaled::from_decimal_digits(&[0, 7, 5]),  // fractional part
//!                 core::ScaledUnit::Inch,  // units
//!             ).unwrap(),
//!             shrink_order: core::GlueOrder::Normal,
//!         }
//!     }.into(),
//!     ds::Char{char: 'A', font: 0}.into(),
//!     ds::Kern{
//!         kind: ds::KernKind::Normal,
//!         width: -core::Scaled::new(
//!                 0,  // integer part
//!                 core::Scaled::from_decimal_digits(&[1]),  // fractional part
//!                 core::ScaledUnit::Point,  // units
//!             ).unwrap(),
//!     }.into(),
//!     ds::Char{char: 'V', font: 0}.into(),
//! ];
//! assert_eq![got, Ok(want)];
//! ```
//!
//! The main takeaway from this example is that you start with a very
//! terse description of the horizontal list, and the library outputs
//! the long and tedious Rust struct definitions.
//!
//! ## Language specification
//!
//! A Boxworks language program is a sequence of a function calls
//! like `text("ABC")` or `glue(10pt, 3pt, 2pt)`.
//! Most function calls add an item or items to the current
//! box-and-glue list.
//!
//! ### Function arguments
//!
//! Each function accepts a number of arguments.
//! For simplicity, every argument to every function is optional.
//!
//! Arguments can be provided positionally:
//!
//! ```
//! # use boxworks_lang as bwl;
//! # use boxworks::ds;
//! let source = r#"
//!     text("A", 1)
//! "#;
//! assert_eq![
//!     bwl::parse_horizontal_list(&source),
//!     Ok(vec![ds::Char{char: 'A', font: 1}.into()])
//! ];
//! ```
//!
//! Or by keyword, potentially out of order:
//!
//! ```
//! # use boxworks_lang as bwl;
//! # use boxworks::ds;
//! let source = r#"
//!     text(font=2, content="B")
//! "#;
//! assert_eq![
//!     bwl::parse_horizontal_list(&source),
//!     Ok(vec![ds::Char{char: 'B', font: 2}.into()])
//! ];
//! ```
//!
//! Or by a combination of positional and by keyword:
//!
//! ```
//! # use boxworks_lang as bwl;
//! # use boxworks::ds;
//! let source = r#"
//!     text("C", font=3)
//! "#;
//! assert_eq![
//!     bwl::parse_horizontal_list(&source),
//!     Ok(vec![ds::Char{char: 'C', font: 3}.into()])
//! ];
//! ```
//!
//! However, all positional arguments must be provided before
//! keyword arguments:
//!
//! ```
//! # use boxworks_lang as bwl;
//! # use boxworks::ds;
//! let source = r#"
//!     text(content="C", 3)
//! "#;
//! let errs = bwl::parse_horizontal_list(&source).unwrap_err();
//! assert![matches![
//!     errs[0],
//!     bwl::Error::PositionalArgAfterKeywordArg{..}
//! ]];
//! ```
//!
//! ### Function argument types
//!
//! Every function argument expects a specific concrete type.
//! These are the types:
//!
//! | Name | Description | Examples
//! |------|-------------|---------
//! | String | Arbitrary UTF-8 characters between double quotes. Currently the string can't contain a double quote character. | `"a string"`
//! | Integer | Decimal integer in the range (-2^31,2^31). | `123`, `-456`
//! | Dimension | Decimal number with a unit attached. The format and the allowable units are the same as in TeX. | `1pt`, `2.04in`, `-10sp`
//! | Glue stretch or shrink | A dimension where the unit can alternatively be an infinite stretch/shrink unit. | `1fil`, `-2fill`, `3filll`
//!
//! ### Available functions
//!
//! More functions will be added over time.
//! These are the currently supported functions.
//!
//! #### `glue`: add a glue node to the current list
//!
//! Adds a value of the Rust type [`boxworks::ds::Glue`]
//! to the current list.
//!
//! Parameters:
//!
//! | Number | Name    | Type      | Default |
//! |--------|---------|-----------|---------|
//! | 1      | `width` | dimension | `0pt`   |
//! | 2      | `stretch` | glue stretch or shrink | `0pt`   |
//! | 3      | `shrink` | glue stretch or shrink | `0pt`   |
//!
//! #### `kern`: add a kern node to the current list
//!
//! Adds a value of the Rust type [`boxworks::ds::Kern`]
//! to the current list.
//!
//! Parameters:
//!
//! | Number | Name    | Type      | Default |
//! |--------|---------|-----------|---------|
//! | 1      | `width` | dimension | `0pt`   |
//!
//! #### `text`: typeset some text
//!
//! The goal of this function is to add character and glue nodes
//! like TeX does when it is processing normal text.
//! In TeX this is actually a complicated process that includes:
//!
//! - Adding kerns between characters.
//! - Applying ligature rules.
//! - Adjusting the space factor that determines the size of inter-word glue.
//!
//! Right now the `text` function is much simpler.
//! It iterates over all characters in the string and:
//!
//! - For space characters, adds a glue node corresponding
//!     to the glue `10pt plus 4pt minus 4pt`.
//!
//! - For all other characters, adds a value of the Rust type
//!     [`boxworks::ds::Char`].
//!
//! Parameters:
//!
//! | Number | Name    | Type      | Default |
//! |--------|---------|-----------|---------|
//! | 1      | `content` | string | `""`   |
//! | 2      | `font` | integer | `0`   |
pub mod ast;
mod error;
pub mod lexer;
pub mod parse;
pub use error::{Error, ErrorLabel};

use boxworks::ds;
use core::Scaled;

/// String type used in error messages.
#[derive(Debug, Clone)]
pub struct Str<'a> {
    value: &'a str,
    start: usize,
    end: usize,
}

impl<'a> Str<'a> {
    fn new(value: &'a str) -> Str<'a> {
        Str {
            value,
            start: 0,
            end: value.len(),
        }
    }
    fn span(&self) -> std::ops::Range<usize> {
        self.start..self.end
    }
    fn str(&self) -> &'a str {
        &self.value[self.span()]
    }
}

impl<'a> From<&'a str> for Str<'a> {
    fn from(value: &'a str) -> Self {
        Str {
            value,
            start: 0,
            end: value.len(),
        }
    }
}

impl<'a> std::fmt::Display for Str<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.str())
    }
}

impl<'a> PartialEq for Str<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.str() == other.str()
    }
}

impl<'a> Eq for Str<'a> {}

/// Pretty-format Box source code.
pub fn format(source: &str) -> Result<String, Vec<error::Error>> {
    let mut l = lexer::Lexer::new(source);
    let func_calls = parse::parse_list(&mut l);
    l.check_errors()?;
    let mut s = String::new();
    for func_call in func_calls {
        use std::fmt::Write;
        write!(&mut s, "{func_call}").unwrap();
    }
    Ok(s)
}

/// Write a horizontal list as Box language.
pub fn write_horizontal_list(list: &ds::HList) -> String {
    use std::fmt::Write;
    let mut s = String::new();
    writeln!(&mut s, "hlist(").unwrap();
    writeln!(&mut s, "  width={},", list.width).unwrap();
    writeln!(&mut s, "  depth={},", list.depth).unwrap();
    writeln!(&mut s, "  height={},", list.height).unwrap();
    writeln!(&mut s, "  content=[").unwrap();
    for elem in &list.list {
        use ds::Horizontal::*;
        match elem {
            Char(char) => {
                writeln!(&mut s, "    text(\"{}\", font={})", char.char, char.font).unwrap();
            }
            Glue(glue) => {
                writeln!(
                    &mut s,
                    "    glue({}, {}, {})",
                    glue.value.width, glue.value.stretch, glue.value.shrink
                )
                .unwrap();
            }
            Kern(kern) => {
                writeln!(&mut s, "    kern({})", kern.width).unwrap();
            }
            Ligature(lig) => {
                writeln!(
                    &mut s,
                    "    lig(\"{}\", font={}, original=\"{}\")",
                    lig.char.escape_default(),
                    lig.font,
                    lig.original_chars
                )
                .unwrap();
            }
            _ => todo!(),
        }
    }
    writeln!(&mut s, "  ],\n)").unwrap();
    s
}

/// Parse Box language source code into a horizontal list.
pub fn parse_horizontal_list(source: &str) -> Result<Vec<ds::Horizontal>, Vec<Error>> {
    let ast_nodes = ast::parse_horizontal_list(source)?;
    let mut v = vec![];
    for h in ast_nodes {
        use ast::Horizontal::*;
        match h {
            Glue(args) => {
                v.push(
                    ds::Glue {
                        kind: ds::GlueKind::Normal,
                        value: core::Glue {
                            width: args.width.value,
                            stretch: args.stretch.value.0,
                            stretch_order: args.stretch.value.1,
                            shrink: args.shrink.value.0,
                            shrink_order: args.shrink.value.1,
                        },
                    }
                    .into(),
                );
            }
            Kern(args) => {
                v.push(
                    ds::Kern {
                        kind: ds::KernKind::Normal,
                        width: args.kern.value,
                    }
                    .into(),
                );
            }
            Text(args) => {
                for c in args.content.value.chars() {
                    if c.is_whitespace() {
                        v.push(
                            ds::Glue {
                                kind: ds::GlueKind::Normal,
                                value: core::Glue {
                                    width: Scaled::ONE * 10,
                                    stretch: Scaled::ONE * 4,
                                    shrink: Scaled::ONE * 4,
                                    ..Default::default()
                                },
                            }
                            .into(),
                        );
                        continue;
                    }
                    v.push(
                        ds::Char {
                            char: c,
                            font: args.font.value as u32,
                        }
                        .into(),
                    );
                }
            }
            Hlist(_) => todo!(),
        }
    }
    Ok(v)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format() {
        let input = r#"hlist

        (

    1.0pt, height =2.0pt,

    contents = [ glue(  ) 
    
    text("Hello", font =  
    
    0) text("World")] ,

    other=3.0fill
)
"#;
        let want = r#"hlist(
  1.0pt,
  height=2.0pt,
  contents=[
    glue()
    text("Hello", font=0)
    text("World")
  ],
  other=3.0fill,
)
"#;
        let got = format(&input).unwrap();
        assert_eq!(got, want);
    }
}
