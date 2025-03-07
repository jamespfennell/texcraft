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
mod lexer;
use core::{GlueOrder, Scaled};

use boxworks::ds;
mod error;
pub use error::{Error, ErrorLabel};

/// String type used in error messages.
#[derive(Debug, PartialEq, Eq, Clone)]
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

/// Write a horizontal list as Box language.
pub fn write_horizontal_list(list: &[ds::Horizontal]) -> String {
    let mut s = String::new();
    for elem in list {
        use ds::Horizontal::*;
        use std::fmt::Write;
        match elem {
            Char(char) => {
                writeln!(&mut s, "text(\"{}\", font={})", char.char, char.font).unwrap();
            }
            Glue(glue) => {
                writeln!(
                    &mut s,
                    "glue({}, {}, {})",
                    glue.value.width, glue.value.stretch, glue.value.shrink
                )
                .unwrap();
            }
            Kern(kern) => {
                writeln!(&mut s, "kern({})", kern.width).unwrap();
            }
            Ligature(lig) => {
                writeln!(
                    &mut s,
                    "lig(\"{}\", font={}, original=\"{}\")",
                    lig.char.escape_default(),
                    lig.font,
                    lig.original_chars
                )
                .unwrap();
            }
            _ => todo!(),
        }
    }
    s
}

/// Parse Box language source code into a horizontal list.
pub fn parse_horizontal_list(source: &str) -> Result<Vec<ds::Horizontal>, Vec<Error>> {
    let mut lexer = lexer::Lexer::new(source);
    let mut v: Vec<ds::Horizontal> = vec![];
    let calls = parse_list(&mut lexer);
    for call in calls {
        match call.func_name {
            "glue" => {
                let mut args = GlueParams {
                    width: Scaled::ZERO.into(),
                    stretch: (Scaled::ZERO, core::GlueOrder::Normal).into(),
                    shrink: (Scaled::ZERO, core::GlueOrder::Normal).into(),
                };
                if !args.apply_args(&call, lexer.errs_mut()) {
                    continue;
                }
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
            "kern" => {
                let mut args = KernParams {
                    kern: Scaled::ZERO.into(),
                };
                if !args.apply_args(&call, lexer.errs_mut()) {
                    continue;
                }
                v.push(
                    ds::Kern {
                        kind: ds::KernKind::Normal,
                        width: args.kern.value,
                    }
                    .into(),
                );
            }
            "text" => {
                let mut args = TextParams {
                    content: "".into(),
                    font: 0.into(),
                };
                if !args.apply_args(&call, lexer.errs_mut()) {
                    continue;
                }
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
            _ => continue,
        }
    }
    lexer.check_errors()?;
    Ok(v)
}

macro_rules! func_params {
    (
        struct $name: ident <$lifetime: lifetime>  {
            $(
                $field_name: ident : $field_type: ty,
            )+
        }
    ) => {
        #[derive(Debug)]
        struct $name <$lifetime> {
            $(
                $field_name : ParamField<$lifetime, $field_type>,
            )+
        }
        impl<$lifetime> Params<$lifetime> for $name <$lifetime> {
            const FIELD_NAMES: &'static[&'static str] = &[ $( stringify!($field_name), )+];
            fn assign_to_field(&mut self, field_name: Str<$lifetime>, node: &Node<$lifetime>, call: &FuncCall<$lifetime>, errs: &mut Vec<Error<$lifetime>>)
            {
                match field_name.str() {
                $(
                    stringify!($field_name) => {
                        self.$field_name.apply(field_name.str(), node, call, errs);
                    }
                )+
                    _ => {
                        errs.push(Error::NoSuchArgument{function_name: call.func_name_source.clone(), argument: field_name });
                    }
                }
            }
        }
    };
}

trait Params<'b> {
    const FIELD_NAMES: &'static [&'static str];
    fn assign_to_field(
        &mut self,
        n: Str<'b>,
        node: &Node<'b>,
        call: &FuncCall<'b>,
        errs: &mut Vec<Error<'b>>,
    );

    fn apply_args(&mut self, call: &FuncCall<'b>, errs: &mut Vec<Error<'b>>) -> bool {
        let start = errs.len();
        let mut iter = call.pos_args.iter();
        for field_name in Self::FIELD_NAMES {
            let Some(node) = iter.next() else {
                break;
            };
            self.assign_to_field(Str::new(field_name), node, call, errs);
        }
        let extra_positional_args: Vec<Str<'b>> = iter.map(|node| node.source.clone()).collect();
        if !extra_positional_args.is_empty() {
            errs.push(Error::TooManyPositionalArgs {
                extra_positional_args,
                function_name: call.func_name_source.clone(),
                max_positional_args: Self::FIELD_NAMES.len(),
            });
        }
        for (field_name, node) in &call.keyword_args {
            // TODO: check it's already been assigned
            self.assign_to_field(field_name.clone(), node, call, errs);
        }
        errs.len() == start
    }
}

func_params!(
    struct TextParams<'a> {
        content: &'a str,
        font: i32,
    }
);

func_params!(
    struct GlueParams<'a> {
        width: Scaled,
        stretch: (Scaled, core::GlueOrder),
        shrink: (Scaled, core::GlueOrder),
    }
);

func_params!(
    struct KernParams<'a> {
        kern: Scaled,
    }
);

#[derive(Debug)]
struct ParamField<'a, T> {
    value: T,
    source: Option<Str<'a>>,
}

impl<'a, T> From<T> for ParamField<'a, T> {
    fn from(value: T) -> Self {
        Self {
            value,
            source: None,
        }
    }
}

impl<'a, T> ParamField<'a, T>
where
    T: TryFromNodeValue<'a>,
{
    fn apply(
        &mut self,
        field_name: &'a str,
        node: &Node<'a>,
        call: &FuncCall<'a>,
        errs: &mut Vec<Error<'a>>,
    ) {
        if let Some(first_assignment) = &self.source {
            errs.push(Error::DuplicateArgument {
                parameter_name: field_name,
                first_assignment: first_assignment.clone(),
                second_assignment: node.source.clone(),
            });
            return;
        }
        match T::try_from(&node.value) {
            None => errs.push(Error::IncorrectType {
                wanted_type: T::DESCRIPTION,
                got_type: node.value.description(),
                got_raw_value: node.source.clone(),
                function_name: call.func_name_source.clone(),
                parameter_name: field_name,
            }),
            Some(val) => {
                self.value = val;
                self.source = Some(node.source.clone());
            }
        }
    }
}

trait TryFromNodeValue<'a>: Sized {
    const DESCRIPTION: &'static str;

    fn try_from(nv: &NodeValue<'a>) -> Option<Self>;
}

impl<'a> TryFromNodeValue<'a> for &'a str {
    const DESCRIPTION: &'static str = "a string";
    fn try_from(nv: &NodeValue<'a>) -> Option<Self> {
        match nv {
            NodeValue::String(s) => Some(*s),
            _ => None,
        }
    }
}

impl<'a> TryFromNodeValue<'a> for i32 {
    const DESCRIPTION: &'static str = "an integer";
    fn try_from(nv: &NodeValue<'a>) -> Option<Self> {
        match nv {
            NodeValue::Integer(i) => Some(*i),
            _ => None,
        }
    }
}

impl<'a> TryFromNodeValue<'a> for Scaled {
    const DESCRIPTION: &'static str = "a number";
    fn try_from(nv: &NodeValue<'a>) -> Option<Self> {
        match nv {
            NodeValue::Scaled(i) => Some(*i),
            _ => None,
        }
    }
}

impl<'a> TryFromNodeValue<'a> for (Scaled, GlueOrder) {
    const DESCRIPTION: &'static str = "a stretch or shrink glue component";
    fn try_from(nv: &NodeValue<'a>) -> Option<Self> {
        match nv {
            NodeValue::Scaled(i) => Some((*i, GlueOrder::Normal)),
            NodeValue::InfiniteGlue(s, o) => Some((*s, *o)),
            _ => None,
        }
    }
}

#[derive(Debug)]
struct Node<'a> {
    pub value: NodeValue<'a>,
    pub source: Str<'a>,
}

#[derive(Debug)]
enum NodeValue<'a> {
    Integer(i32),
    Scaled(core::Scaled),
    InfiniteGlue(core::Scaled, core::GlueOrder),
    String(&'a str),
    // List(Vec<FuncCall<'a>>),
}

impl<'a> NodeValue<'a> {
    fn description(&self) -> &'static str {
        use NodeValue::*;
        match self {
            Integer(_) => "an integer",
            Scaled(_) => "a number",
            InfiniteGlue(_, _) => "an infinite glue component",
            String(_) => "a string",
            // List(_) => "a list",
        }
    }
}

impl<'a> Node<'a> {
    fn parse(l: &mut lexer::Lexer<'a>) -> Self {
        let token = l.next().unwrap();
        let value = match token.value {
            lexer::TokenValue::String(s) => NodeValue::String(s),
            lexer::TokenValue::Integer(n) => NodeValue::Integer(n),
            lexer::TokenValue::Scaled(n) => NodeValue::Scaled(n),
            lexer::TokenValue::InfiniteGlue(s, o) => NodeValue::InfiniteGlue(s, o),
            a => {
                panic!("can't parse value {a:?}")
            }
        };
        Node {
            value,
            source: token.source,
        }
    }
}

#[derive(Debug)]
struct FuncCall<'a> {
    func_name: &'a str,
    func_name_source: Str<'a>,
    pos_args: Vec<Node<'a>>,
    keyword_args: Vec<(Str<'a>, Node<'a>)>,
}

fn parse_list<'a>(l: &mut lexer::Lexer<'a>) -> Vec<FuncCall<'a>> {
    let mut v = vec![];
    // One loop iterator for each func call.
    loop {
        let Some(next) = l.next() else { return v };
        // parse function name
        let func_name = match next.value {
            lexer::TokenValue::Keyword(func_name) => func_name,
            _ => todo!("error: expected keyword"),
        };
        let mut func_call = FuncCall {
            func_name,
            func_name_source: next.source,
            pos_args: vec![],
            keyword_args: vec![],
        };
        // parse (
        assert_eq!(
            l.next().map(|t| t.value),
            Some(lexer::TokenValue::RoundOpen)
        );
        // parse the arguments
        loop {
            let next = l.peek().unwrap();
            match next.value {
                // no more arguments
                lexer::TokenValue::RoundClose => {}
                // key=value argument
                lexer::TokenValue::Keyword(_) => {
                    l.next();
                    assert_eq!(l.next().map(|t| t.value), Some(lexer::TokenValue::Equal));
                    let value = Node::parse(l);
                    func_call.keyword_args.push((next.source, value));
                }
                // positional argument
                _ => {
                    let value = Node::parse(l);
                    if let Some(kw) = func_call.keyword_args.last() {
                        l.error(Error::PositionalArgAfterKeywordArg {
                            positional_arg: value.source.clone(),
                            keyword_arg: Str {
                                value: kw.1.source.value,
                                start: kw.0.start,
                                end: kw.1.source.end,
                            },
                        });
                    } else {
                        func_call.pos_args.push(value);
                    }
                }
            }
            match l.next().map(|t| t.value) {
                Some(lexer::TokenValue::RoundClose) => {
                    break;
                }
                Some(lexer::TokenValue::Comma) => {
                    continue;
                }
                a => panic!("unexpected token {a:?}"),
            }
        }
        v.push(func_call);
    }
}
