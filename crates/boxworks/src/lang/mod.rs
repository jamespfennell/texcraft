//! # Boxworks language
//!
//! This module defines a domain-specific language (DSL) for Boxworks.
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
//! use boxworks::ds;
//! use boxworks::lang as bwl;
//!
//! let source = r#"
//!     ## The chars() function typesets characters.
//!     chars("Box")
//!     ## Glue can be added manually.
//!     glue(1pt, 5fil, 0.075in)
//!     ## The following elements illustrate the prototypical example of a kern.
//!     chars("A")
//!     kern(-0.1pt)
//!     chars("V")
//! "#;
//! let got = bwl::parse_horizontal_list(&source);
//! let want: Vec<ds::Horizontal> = vec![
//!     ds::Char{char: 'B', font: 0}.into(),
//!     ds::Char{char: 'o', font: 0}.into(),
//!     ds::Char{char: 'x', font: 0}.into(),
//!     ds::Glue{
//!         kind: ds::GlueKind::Normal,
//!         value: common::Glue{
//!             width: common::Scaled::new(
//!                 1,  // integer part
//!                 common::Scaled::ZERO,  // fractional part
//!                 common::ScaledUnit::Point,  // units
//!             ).unwrap(),
//!             stretch: common::Scaled::new(
//!                 5,  // integer part
//!                 common::Scaled::ZERO,  // fractional part
//!                 common::ScaledUnit::Point,  // units
//!             ).unwrap(),
//!             stretch_order: common::GlueOrder::Fil,
//!             shrink: common::Scaled::new(
//!                 0,  // integer part
//!                 common::Scaled::from_decimal_digits(&[0, 7, 5]),  // fractional part
//!                 common::ScaledUnit::Inch,  // units
//!             ).unwrap(),
//!             shrink_order: common::GlueOrder::Normal,
//!         }
//!     }.into(),
//!     ds::Char{char: 'A', font: 0}.into(),
//!     ds::Kern{
//!         kind: ds::KernKind::Normal,
//!         width: -common::Scaled::new(
//!                 0,  // integer part
//!                 common::Scaled::from_decimal_digits(&[1]),  // fractional part
//!                 common::ScaledUnit::Point,  // units
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
//! like `chars("ABC")` or `glue(10pt, 3pt, 2pt)`.
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
//! # use boxworks::lang as bwl;
//! # use boxworks::ds;
//! let source = r#"
//!     chars("A", 1)
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
//! # use boxworks::lang as bwl;
//! # use boxworks::ds;
//! let source = r#"
//!     chars(font=2, content="B")
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
//! # use boxworks::lang as bwl;
//! # use boxworks::ds;
//! let source = r#"
//!     chars("C", font=3)
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
//! # use boxworks::lang as bwl;
//! # use boxworks::ds;
//! let source = r#"
//!     chars(content="C", 3)
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
//! | Name | Description | Examples |
//! |------|-------------|---------|
//! | String | Arbitrary UTF-8 characters between double quotes. Currently the string can't contain a double quote character. | `"a string"` |
//! | Integer | Decimal integer in the range (-2^31,2^31). | `123`, `-456` |
//! | Dimension | Decimal number with a unit attached. The format and the allowable units are the same as in TeX. | `1pt`, `2.04in`, `-10sp` |
//! | Glue stretch or shrink | A dimension where the unit can alternatively be an infinite stretch/shrink unit. | `1fil`, `-2fill`, `3filll` |
//! | Character | A string containing exactly one UTF-8 character. | `"A"`, `"ñ"` |
//! | Glue order | One of the strings `"normal"`, `"fil"`, `"fill"`, or `"filll"`. | `"normal"`, `"fill"` |
//! | Glue ratio | A floating-point number represented as a string. | `"1.5"`, `"-0.25"` |
//! | Dimension or running | Either a dimension, or the string `"running"` to indicate the value is determined by context. | `1pt`, `"running"` |
//! | Horizontal list | A bracket-enclosed list of horizontal-mode function calls. | `[chars("Hi") glue()]` |
//! | Vertical list | A bracket-enclosed list of vertical-mode function calls. | `[glue() kern(1pt)]` |
//! | Discretionary list | A bracket-enclosed list of function calls valid in discretionary pre/post-break lists. | `[chars("-") kern(0.5pt)]` |
//!
//! ### Available functions
//!
//! More functions will be added over time.
//! These are the currently supported functions.
//!
//! #### `chars`: typeset some characters
//!
//! Adds a value of the Rust type [`super::ds::Char`] for each character in the
//! input string.
//!
//! Only available in horizontal and discretionary lists, not vertical lists.
//!
//! Parameters:
//!
//! | Number | Name      | Type    | Default |
//! |--------|-----------|---------|---------|
//! | 1      | `content` | string  | `""`    |
//! | 2      | `font`    | integer | `0`     |
//!
//! #### `glue`: add a glue node to the current list
//!
//! Adds a value of the Rust type [`super::ds::Glue`]
//! to the current list.
//!
//! Only available in horizontal and vertical lists, not discretionary lists.
//!
//! Parameters:
//!
//! | Number | Name      | Type                   | Default |
//! |--------|-----------|------------------------|---------|
//! | 1      | `width`   | dimension              | `0pt`   |
//! | 2      | `stretch` | glue stretch or shrink | `0pt`   |
//! | 3      | `shrink`  | glue stretch or shrink | `0pt`   |
//!
//! #### `penalty`: add a penalty node to the current list
//!
//! Adds a value of the Rust type [`super::ds::Penalty`]
//! to the current list.
//!
//! Only available in horizontal and vertical lists, not discretionary lists.
//!
//! Parameters:
//!
//! | Number | Name    | Type    | Default |
//! |--------|---------|---------|---------|
//! | 1      | `value` | integer | `0`     |
//!
//! #### `kern`: add a kern node to the current list
//!
//! Adds a value of the Rust type [`super::ds::Kern`]
//! to the current list.
//!
//! Parameters:
//!
//! | Number | Name    | Type      | Default |
//! |--------|---------|-----------|---------|
//! | 1      | `width` | dimension | `0pt`   |
//!
//! #### `hbox`: add a horizontal box to the current list
//!
//! Adds a value of the Rust type [`super::ds::HBox`]
//! to the current list.
//!
//! Parameters:
//!
//! | Number | Name           | Type            | Default    |
//! |--------|----------------|-----------------|------------|
//! | 1      | `height`       | dimension       | `0pt`      |
//! | 2      | `width`        | dimension       | `0pt`      |
//! | 3      | `depth`        | dimension       | `0pt`      |
//! | 4      | `shift_amount` | dimension       | `0pt`      |
//! | 5      | `glue_ratio`   | glue ratio      | `"0.0"`    |
//! | 6      | `glue_order`   | glue order      | `"normal"` |
//! | 7      | `content`      | horizontal list | `[]`       |
//!
//! #### `lig`: add a ligature node to the current list
//!
//! Adds a value of the Rust type [`super::ds::Ligature`]
//! to the current list.
//!
//! Only available in horizontal and discretionary lists, not vertical lists.
//!
//! Parameters:
//!
//! | Number | Name             | Type      | Default  |
//! |--------|------------------|-----------|----------|
//! | 1      | `char`           | character | `"\0"`   |
//! | 2      | `original_chars` | string    | `""`     |
//! | 3      | `font`           | integer   | `0`      |
//!
//! #### `vbox`: add a vertical box to the current list
//!
//! Adds a value of the Rust type [`super::ds::VBox`]
//! to the current list.
//!
//! Parameters:
//!
//! | Number | Name           | Type          | Default |
//! |--------|----------------|---------------|---------|
//! | 1      | `height`       | dimension     | `0pt`   |
//! | 2      | `width`        | dimension     | `0pt`   |
//! | 3      | `depth`        | dimension     | `0pt`   |
//! | 4      | `shift_amount` | dimension     | `0pt`   |
//! | 5      | `content`      | vertical list | `[]`    |
//!
//! #### `disc`: add a discretionary node to the current list
//!
//! Adds a value of the Rust type [`super::ds::Discretionary`]
//! to the current list.
//!
//! Only available in horizontal lists.
//!
//! Parameters:
//!
//! | Number | Name            | Type               | Default |
//! |--------|-----------------|--------------------|---------|
//! | 1      | `pre_break`     | discretionary list | `[]`    |
//! | 2      | `post_break`    | discretionary list | `[]`    |
//! | 3      | `replace_count` | integer            | `0`     |
//!
//! #### `rule`: add a rule to the current list
//!
//! Adds a value of the Rust type [`super::ds::Rule`]
//! to the current list.
//!
//! Parameters:
//!
//! | Number | Name     | Type                 | Default |
//! |--------|----------|----------------------|---------|
//! | 1      | `height` | dimension or running | `0pt`   |
//! | 2      | `width`  | dimension or running | `0pt`   |
//! | 3      | `depth`  | dimension or running | `0pt`   |
//!
//! #### `mark`: add a mark node to the current list
//!
//! Adds a value of the Rust type [`super::ds::Mark`]
//! to the current list.
//!
//! Only available in horizontal and vertical lists, not discretionary lists.
//!
//! Parameters: none.
//!
//! #### `adjust`: add an adjust node to the current list
//!
//! Adds a value of the Rust type [`super::ds::Adjust`]
//! to the current list.
//!
//! Only available in horizontal lists.
//!
//! Parameters:
//!
//! | Number | Name      | Type          | Default |
//! |--------|-----------|---------------|---------|
//! | 1      | `content` | vertical list | `[]`    |
//!
//! #### `insertion`: add an insertion node to the current list
//!
//! Adds a value of the Rust type [`super::ds::Insertion`]
//! to the current list.
//!
//! Only available in horizontal and vertical lists, not discretionary lists.
//!
//! Parameters:
//!
//! | Number | Name                      | Type                   | Default |
//! |--------|---------------------------|------------------------|---------|
//! | 1      | `box_number`              | integer                | `0`     |
//! | 2      | `height`                  | dimension              | `0pt`   |
//! | 3      | `split_max_depth`         | dimension              | `0pt`   |
//! | 4      | `split_top_skip_width`    | dimension              | `0pt`   |
//! | 5      | `split_top_skip_stretch`  | glue stretch or shrink | `0pt`   |
//! | 6      | `split_top_skip_shrink`   | glue stretch or shrink | `0pt`   |
//! | 7      | `float_penalty`           | integer                | `0`     |
//! | 8      | `vbox`                    | vertical list          | `[]`    |
//!
//! #### `math`: add a math node to the current list
//!
//! Adds a value of the Rust type [`super::ds::Math`]
//! to the current list.
//!
//! Only available in horizontal and vertical lists, not discretionary lists.
//!
//! Parameters:
//!
//! | Number | Name   | Type   | Default |
//! |--------|--------|--------|---------|
//! | 1      | `kind` | string | `""`    |
pub mod ast;
pub mod convert;
pub mod cst;
mod error;
pub mod lexer;
use convert::ToBoxworks;
pub use error::{Error, ErrorAccumulator, ErrorLabel};

use crate::ds;

/// String type used in the crate's public API.
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
    fn is_empty(&self) -> bool {
        self.start == self.end
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
pub fn format(source: &str) -> Result<String, Vec<error::Error<'_>>> {
    let errs: ErrorAccumulator = Default::default();
    let l = lexer::Lexer::new(source, errs.clone());
    let func_calls = cst::parse_using_lexer(l, errs.clone());
    errs.check()?;
    let mut s = String::new();
    cst::pretty_print(&mut s, func_calls).expect("no errors writing to string");
    Ok(s)
}

/// Parse Box language source code into a horizontal list.
pub fn parse_horizontal_list(source: &str) -> Result<Vec<ds::Horizontal>, Vec<Error<'_>>> {
    let ast_nodes = ast::parse_hbox(source)?;
    Ok(ast_nodes.to_boxworks())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format() {
        let input = r#"# This is a
#  list of things
hlist

        (
    1.0pt, height =2.0pt,

    contents = [ # glue is good
        glue(  ) 
    
chars("Hello", font = 
# we use an unusual font here
1)

    chars("Hello", font =  


    0) chars("World")] ,
        # Infinite glue
    other=3.0fill,
    # there are no more arguments
)
"#;
        let want = r#"# This is a
#  list of things
hlist(
  1.0pt,
  height=2.0pt,
  contents=[
    # glue is good
    glue()
    chars(
      "Hello",
      # we use an unusual font here
      font=1,
    )
    chars("Hello", font=0)
    chars("World")
  ],
  # Infinite glue
  other=3.0fill,
  # there are no more arguments
)
"#;
        let got = format(&input).unwrap();
        assert_eq!(got, want);
    }
}
