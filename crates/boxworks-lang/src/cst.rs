//! Box language concrete syntax tree (CST).
//!
//! This module contains two representations of the Box CST.
//!
//! The type [`Tree`] is an **explicit representation** of a CST.
//! It consists of Rust vectors of function calls ([`FuncCall`]).
//! These calls contain Rust vectors of arguments ([`Arg`]).
//! Some of these arguments may, in turn, contain nested trees if the argument
//!     is a list type.
//!
//! The explicit representation is easy to use, but is inefficient
//!     and makes using the CST a costly abstraction.
//! The alternative representation is an **iterator representation**,
//!     based on two traits [`TreeIter`] and [`ArgsIter`].
//! With this representation, instead of materializing the CST at once
//!     with a type like a [`Tree`], calling code instead iterates
//!     over the tree.
//!

use super::lexer;
use super::Str;
use std::borrow::Cow;
use std::collections::VecDeque;
use std::fmt::Write;

/// Iterator representation of a CST.
pub trait TreeIter<'a>: Iterator<Item = TreeItem<'a, Self::ArgsIter>> {
    /// Args iterator associated to this iterator.
    type ArgsIter: ArgsIter<'a, TreeIter = Self>;
    
    /// Return the remaining source to be parsed.
    fn remaining_source(&self) -> Str<'a>;
}

/// Item returned by [`TreeIter`].
pub enum TreeItem<'a, A> {
    /// A function call, like `text("Hello", font=3)`.
    FuncCall {
        /// Name of the function
        func_name: Str<'a>,
        /// Iterator over the arguments of the function.
        iter: A,
    },
    /// A comment, like `# this is a comment`.
    Comment {
        /// Value of the comment.
        value: &'a str,
    },
}

/// Iterator over the arguments of a function.
pub trait ArgsIter<'a>: Iterator<Item = ArgsItem<'a, Self::TreeIter>> {
    /// Tree iterator associated to this iterator.
    type TreeIter: TreeIter<'a, ArgsIter = Self>;
}

/// Item returned by [`ArgsIter`].
pub enum ArgsItem<'a, F> {
    /// A non-list argument, like `3pt` or `font=5`.
    Regular {
        /// Key of the argument, if present.
        ///
        /// This is populated for keyword arguments like `font=5`.
        /// Positional arguments like `3pt` will have a value of `None` here.
        key: Option<Str<'a>>,
        /// Value of the argument.
        value: Value<'a>,
        /// Source of the value in the source code.
        value_source: Str<'a>,
    },
    /// A list argument.
    List {
        /// Key of the argument, if present.
        ///
        /// This is populated for keyword arguments like `font=5`.
        /// Positional arguments like `3pt` will have a value of `None` here.
        key: Option<Str<'a>>,
        /// Opening square brace.
        square_open: Str<'a>,
        /// Iterator over the items in this list.
        ///
        /// This will satisfy the [`TreeIter`] trait.
        iter: F,
    },
    /// A comment.
    Comment {
        /// Value of the comment.
        value: &'a str,
    },
}

/// Pretty print a Box CST.
pub fn pretty_print<'a, W: std::fmt::Write>(
    w: &mut W,
    iter: impl TreeIter<'a>,
) -> std::fmt::Result {
    pretty_print_impl(w, iter, 0)?;
    Ok(())
}

#[derive(Default)]
struct ArgsPrinter<'a> {
    indent: usize,
    multiline: bool,
    buf: Vec<(Option<Str<'a>>, Value<'a>)>,
}

impl<'a> ArgsPrinter<'a> {
    fn activate_multiline<W: std::fmt::Write>(&mut self, w: &mut W) -> std::fmt::Result {
        self.multiline = true;
        for (key, value) in &self.buf {
            self.print_multiline(w, key, value)?;
        }
        self.buf.clear();
        Ok(())
    }
    fn print<W: std::fmt::Write>(
        &mut self,
        w: &mut W,
        key: Option<Str<'a>>,
        value: Value<'a>,
    ) -> std::fmt::Result {
        if self.buf.len() >= 4 {
            self.activate_multiline(w)?;
        }
        if self.multiline {
            self.print_multiline(w, &key, &value)?;
        } else {
            self.buf.push((key, value));
        }
        Ok(())
    }
    fn print_multiline<W: std::fmt::Write>(
        &self,
        w: &mut W,
        key: &Option<Str<'a>>,
        value: &Value<'a>,
    ) -> std::fmt::Result {
        let indent = Indent(self.indent);
        write!(w, "\n{indent}  ")?;
        if let Some(key) = key {
            write!(w, "{key}=")?;
        }
        write!(w, "{},", value)?;
        Ok(())
    }
    fn flush<W: std::fmt::Write>(&mut self, w: &mut W) -> std::fmt::Result {
        let mut remaining_args = self.buf.len();
        for (keyword, arg) in &self.buf {
            if let Some(keyword) = keyword {
                write!(w, "{keyword}=")?;
            }
            write!(w, "{}", arg)?;
            remaining_args = remaining_args.saturating_sub(1);
            if remaining_args > 0 {
                write!(w, ", ")?;
            }
        }
        let indent = Indent(self.indent);
        if self.multiline {
            write!(w, "\n{indent}")?;
        }
        self.buf.clear();
        self.multiline = false;
        Ok(())
    }
}

struct Indent(usize);

impl std::fmt::Display for Indent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for _ in 0..self.0 {
            f.write_char(' ')?;
        }
        Ok(())
    }
}

fn pretty_print_impl<'a, W: std::fmt::Write, T: TreeIter<'a>>(
    w: &mut W,
    mut iter: T,
    depth: usize,
) -> std::fmt::Result {
    let indent = Indent(depth);
    let mut ap: ArgsPrinter = ArgsPrinter {
        indent: depth,
        ..Default::default()
    };
    loop {
        match iter.next() {
            Some(TreeItem::FuncCall {
                func_name,
                mut iter,
            }) => {
                write!(w, "{indent}{func_name}(")?;
                loop {
                    match iter.next() {
                        Some(ArgsItem::Comment { value }) => {
                            ap.activate_multiline(w)?;
                            write!(w, "\n{indent}  #{value}")?;
                        }
                        Some(ArgsItem::Regular {
                            key,
                            value,
                            value_source: _,
                        }) => {
                            ap.print(w, key, value)?;
                        }
                        Some(ArgsItem::List {
                            key,
                            square_open: _,
                            iter,
                        }) => {
                            ap.activate_multiline(w)?;
                            write!(w, "\n{indent}  ")?;
                            if let Some(key) = key {
                                write!(w, "{key}=")?;
                            }
                            writeln!(w, "[")?;
                            pretty_print_impl(w, iter, depth + 4)?;
                            write!(w, "{indent}  ],")?;
                        }
                        None => {
                            ap.flush(w)?;
                            writeln!(w, ")")?;
                            break;
                        }
                    }
                }
            }
            Some(TreeItem::Comment { value }) => {
                writeln!(w, "{indent}#{value}").unwrap();
            }
            None => {
                break Ok(());
            }
        };
    }
}

/// Explicit representation of a CST.
#[derive(Debug, Default, PartialEq, Clone)]
pub struct Tree<'a> {
    pub calls: Vec<FuncCall<'a>>,
    pub trailing_comments: Vec<&'a str>,
}

impl<'a> Tree<'a> {
    /// Built a CST from a tree iterator.
    pub fn build(iter: impl TreeIter<'a>) -> Self {
        Self::build_impl(iter)
    }

    fn build_impl<I: TreeIter<'a>>(mut iter: I) -> Self {
        let mut comments = vec![];
        let take_comments = |comments: &mut Vec<&'a str>| {
            let mut v = vec![];
            std::mem::swap(&mut v, comments);
            v
        };
        let mut calls = vec![];
        loop {
            match iter.next() {
                Some(TreeItem::FuncCall {
                    func_name,
                    mut iter,
                }) => {
                    let func_comments = take_comments(&mut comments);
                    let mut args: Vec<Arg<'a>> = vec![];
                    loop {
                        match iter.next() {
                            None => break,
                            Some(ArgsItem::Regular {
                                key,
                                value,
                                value_source,
                            }) => {
                                args.push(Arg {
                                    comments: take_comments(&mut comments),
                                    key,
                                    value,
                                    value_source,
                                });
                            }
                            Some(ArgsItem::List {
                                key,
                                square_open: _,
                                iter,
                            }) => {
                                let value_source = iter.remaining_source();
                                // Include the enclosing [] braces.
                                let inner_tree = Tree::build_impl(iter);
                                args.push(Arg {
                                    comments: take_comments(&mut comments),
                                    key,
                                    value: Value::List(inner_tree),
                                    value_source,
                                });
                            }
                            Some(ArgsItem::Comment { value }) => {
                                comments.push(value);
                            }
                        };
                    }
                    calls.push(FuncCall {
                        comments: func_comments,
                        func_name,
                        args,
                        trailing_comments: take_comments(&mut comments),
                    });
                }
                Some(TreeItem::Comment { value }) => {
                    comments.push(value);
                }
                None => break,
            };
        }
        Self {
            calls,
            trailing_comments: comments,
        }
    }

    pub fn iter<'b>(&'b self) -> impl TreeIter<'a> + 'b {
        let mut iter = ExplicitIter { tree_stack: vec![] };
        iter.push_tree(self);
        iter
    }
}

impl<'a> std::fmt::Display for Tree<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        pretty_print(f, self.iter())
    }
}

/// Iterator over an explicit CST.
struct ExplicitIter<'a, 'b> {
    tree_stack: Vec<TreeStackElem<'a, 'b>>,
}

enum TreeStackElem<'a, 'b> {
    FuncCall(&'b FuncCall<'a>),
    Comments(&'b [&'a str]),
}

/// Iterator over an explicit CST.
struct ExplicitArgsIter<'a, 'b> {
    args_stack: Vec<ArgsStackElem<'a, 'b>>,
}

enum ArgsStackElem<'a, 'b> {
    Arg(&'b Arg<'a>),
    Comments(&'b [&'a str]),
}

impl<'a, 'b> ExplicitIter<'a, 'b> {
    fn push_tree(&mut self, t: &'b Tree<'a>) {
        self.tree_stack
            .push(TreeStackElem::Comments(&t.trailing_comments));
        for call in t.calls.iter().rev() {
            self.push_func_call(call);
        }
    }
    fn push_func_call(&mut self, call: &'b FuncCall<'a>) {
        self.tree_stack.push(TreeStackElem::FuncCall(call));
        self.tree_stack
            .push(TreeStackElem::Comments(&call.comments));
    }
}

impl<'a, 'b> ExplicitArgsIter<'a, 'b> {
    fn push_args(&mut self, call: &'b FuncCall<'a>) {
        self.args_stack
            .push(ArgsStackElem::Comments(&call.trailing_comments));
        for a in call.args.iter().rev() {
            self.args_stack.push(ArgsStackElem::Arg(a));
            self.args_stack.push(ArgsStackElem::Comments(&a.comments));
        }
    }
}

impl<'a, 'b> TreeIter<'a> for ExplicitIter<'a, 'b> {
    type ArgsIter = ExplicitArgsIter<'a, 'b>;

    fn remaining_source(&self) -> Str<'a> {
        "".into()
    }
}

impl<'a, 'b> Iterator for ExplicitIter<'a, 'b> {
    type Item = TreeItem<'a, ExplicitArgsIter<'a, 'b>>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.tree_stack.pop()? {
                TreeStackElem::FuncCall(func_call) => {
                    let mut iter = ExplicitArgsIter { args_stack: vec![] };
                    iter.push_args(func_call);
                    return Some(TreeItem::FuncCall {
                        func_name: func_call.func_name.clone(),
                        iter,
                    });
                }
                TreeStackElem::Comments(items) => {
                    if let Some((head, tail)) = items.split_first() {
                        self.tree_stack.push(TreeStackElem::Comments(tail));
                        return Some(TreeItem::Comment { value: head });
                    }
                }
            }
        }
    }
}

impl<'a, 'b> ArgsIter<'a> for ExplicitArgsIter<'a, 'b> {
    type TreeIter = ExplicitIter<'a, 'b>;
}

impl<'a, 'b> Iterator for ExplicitArgsIter<'a, 'b> {
    type Item = ArgsItem<'a, ExplicitIter<'a, 'b>>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.args_stack.pop()? {
                ArgsStackElem::Arg(arg) => {
                    return match arg.value.try_clone_value() {
                        Ok(v) => Some(ArgsItem::Regular {
                            key: arg.key.clone(),
                            value: v,
                            value_source: arg.value_source.clone(),
                        }),
                        Err(tree) => {
                            let mut iter = ExplicitIter { tree_stack: vec![] };
                            iter.push_tree(tree);
                            Some(ArgsItem::List {
                                key: arg.key.clone(),
                                square_open: "".into(),
                                iter,
                            })
                        }
                    };
                }
                ArgsStackElem::Comments(items) => {
                    if let Some((head, tail)) = items.split_first() {
                        self.args_stack.push(ArgsStackElem::Comments(tail));
                        return Some(ArgsItem::Comment { value: head });
                    }
                }
            }
        }
    }
}

/// Parse Box language source code into a CST.
pub fn parse<'a>(source: &'a str, errs: super::ErrorAccumulator<'a>) -> impl TreeIter<'a> {
    let lexer = lexer::Lexer::new(source, errs.clone());
    parse_using_lexer(lexer, errs)
}

/// Parse into a CST using an explicitly provided lexer.
///
/// In general it's easier to use the [`parse`] function.
pub fn parse_using_lexer<'a>(
    lexer: lexer::Lexer<'a>,
    errs: super::ErrorAccumulator<'a>,
) -> impl TreeIter<'a> {
    ParseTreeIter {
        next_tree: None,
        c: ParseIterCommon {
            comments: Default::default(),
            next_token: None,
            lexer,
            errs,
        },
    }
}

struct ParseTreeIter<'a> {
    next_tree: Option<(Str<'a>, lexer::Lexer<'a>)>,
    c: ParseIterCommon<'a>,
}

struct ParseArgsIter<'a> {
    next_arg: Option<ArgsItem<'a, ParseTreeIter<'a>>>,
    c: ParseIterCommon<'a>,
}

struct ParseIterCommon<'a> {
    comments: VecDeque<&'a str>,
    next_token: Option<lexer::Token<'a>>,
    lexer: lexer::Lexer<'a>,
    errs: super::ErrorAccumulator<'a>,
}

impl<'a> ParseIterCommon<'a> {
    fn next_token(&mut self) -> Option<lexer::Token<'a>> {
        let token_or = self.peek_token();
        self.next_token = None;
        token_or
    }
    fn peek_token(&mut self) -> Option<lexer::Token<'a>> {
        match self.next_token.clone() {
            None => {
                for token in self.lexer.by_ref() {
                    match token.value {
                        lexer::TokenValue::Comment => {
                            self.comments.push_back(token.source.str());
                        }
                        lexer::TokenValue::SquareClose => {
                            self.errs.add(crate::Error::UnmatchedClosingSquareBracket {
                                square_close: token.source,
                            });
                        }
                        _ => {
                            self.next_token = Some(token);
                            break;
                        }
                    }
                }
                self.next_token.clone()
            }
            Some(token) => Some(token),
        }
    }
}

impl<'a> TreeIter<'a> for ParseTreeIter<'a> {
    type ArgsIter = ParseArgsIter<'a>;
    fn remaining_source(&self) -> Str<'a> {
        let mut s = self.c.lexer.remaining_source();
        // We include the '[' before...
        if let Some(i) = s.start.checked_sub(1) {
            if (s.value.as_bytes()[i] as u32) == ('[' as u32) {
                s.start = i;
            }
        }
        // ...and the ']' after
        if s.value[s.end..].starts_with(']') {
            s.end += 1;
        }
        s
    }
}

impl<'a> Iterator for ParseTreeIter<'a> {
    type Item = TreeItem<'a, ParseArgsIter<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(comment) = self.c.comments.pop_front() {
            return Some(TreeItem::Comment { value: comment });
        }
        if let Some((func_name, lexer)) = self.next_tree.take() {
            return Some(TreeItem::FuncCall {
                func_name,
                iter: ParseArgsIter {
                    next_arg: None,
                    c: ParseIterCommon {
                        comments: Default::default(),
                        next_token: None,
                        lexer,
                        errs: self.c.errs.clone(),
                    },
                },
            });
        }
        let Some(next) = self.c.next_token() else {
            if self.c.comments.is_empty() {
                return None;
            }
            return self.next();
        };
        // parse function name
        let func_name = match next.value {
            lexer::TokenValue::Keyword => next.source,
            _ => todo!("error: expected keyword"),
        };
        // parse (
        let closing = match self.c.next_token().map(|t| t.value) {
            Some(lexer::TokenValue::RoundOpen { closing }) => closing.unwrap(),
            _ => todo!("error"),
        };
        let inner = self.c.lexer.split_nested(closing);
        self.next_tree = Some((func_name, inner));
        self.next()
    }
}

impl<'a> ArgsIter<'a> for ParseArgsIter<'a> {
    type TreeIter = ParseTreeIter<'a>;
}

impl<'a> Iterator for ParseArgsIter<'a> {
    type Item = ArgsItem<'a, ParseTreeIter<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(comment) = self.c.comments.pop_front() {
            return Some(ArgsItem::Comment { value: comment });
        }
        if let Some(next) = self.next_arg.take() {
            return Some(next);
        }
        let next = match self.c.peek_token() {
            None => {
                if self.c.comments.is_empty() {
                    return None;
                }
                return self.next();
            }
            Some(next) => next,
        };
        let key = match next.value {
            // key=value argument
            lexer::TokenValue::Keyword => {
                // consume the keyword token
                self.c.next_token();
                assert_eq!(
                    self.c.next_token().map(|t| t.value),
                    Some(lexer::TokenValue::Equal)
                );
                Some(next.source)
            }
            // positional argument
            _ => None,
        };
        let value_token = self.c.next_token().unwrap();
        let value = match value_token.value {
            lexer::TokenValue::String(s) => Value::String(s),
            lexer::TokenValue::Integer(n) => Value::Integer(n),
            lexer::TokenValue::Scaled(n) => Value::Scaled(n),
            lexer::TokenValue::InfiniteGlue(s, o) => Value::InfiniteGlue(s, o),
            lexer::TokenValue::SquareOpen { closing } => {
                let lexer = self.c.lexer.split_nested(closing.unwrap());
                self.next_arg = Some(ArgsItem::List {
                    key,
                    square_open: value_token.source,
                    iter: ParseTreeIter {
                        next_tree: None,
                        c: ParseIterCommon {
                            comments: Default::default(),
                            next_token: None,
                            lexer,
                            errs: self.c.errs.clone(),
                        },
                    },
                });
                // Consume an optional ',' after the value.
                if let Some(lexer::TokenValue::Comma) = self.c.peek_token().map(|t| t.value) {
                    // Consume the ',' token.
                    self.c.next_token();
                }
                return self.next();
            }
            a => {
                panic!("expected value, instead got {a:?}")
            }
        };
        // Consume an optional ',' after the value.
        if let Some(lexer::TokenValue::Comma) = self.c.peek_token().map(|t| t.value) {
            // Consume the ',' token.
            self.c.next_token();
        }
        self.next_arg = Some(ArgsItem::Regular {
            key,
            value,
            value_source: value_token.source,
        });
        self.next()
    }
}

/// An argument to a function.
#[derive(Debug, Clone)]
pub struct Arg<'a> {
    /// Comments preceding the argument.
    pub comments: Vec<&'a str>,
    /// For positional arguments the key is [`None`];
    /// for keyword arguments the key is the name provided.
    pub key: Option<Str<'a>>,
    pub value: Value<'a>,
    pub value_source: Str<'a>,
}

impl<'a> PartialEq for Arg<'a> {
    fn eq(&self, other: &Self) -> bool {
        let eq = self.comments == other.comments && self.value == other.value;
        // We only compare sources if they are both provided.
        if self.value_source.is_empty() || other.value_source.is_empty() {
            eq
        } else {
            eq && self.value_source == other.value_source
        }
    }
}

/// The value of an argument to a function.
#[derive(Debug, PartialEq, Clone)]
pub enum Value<'a> {
    Integer(i32),
    Scaled(core::Scaled),
    InfiniteGlue(core::Scaled, core::GlueOrder),
    String(Cow<'a, str>),
    List(Tree<'a>),
}

impl<'a> Value<'a> {
    /// Attempts to clone the value.
    ///
    /// This method is indented for fast code, and will accordingly not clone a tree.
    /// If the value is a tree, it returns the tree in the error payload.
    pub fn try_clone_value(&self) -> Result<Self, &Tree<'a>> {
        use Value::*;
        let ok = match self {
            Integer(i) => Integer(*i),
            Scaled(scaled) => Scaled(*scaled),
            InfiniteGlue(scaled, glue_order) => InfiniteGlue(*scaled, *glue_order),
            String(cow) => String(cow.clone()),
            List(tree) => return Err(tree),
        };
        Ok(ok)
    }
    pub fn description(&self) -> &'static str {
        use Value::*;
        match self {
            Integer(_) => "an integer",
            Scaled(_) => "a number",
            InfiniteGlue(_, _) => "an infinite glue component",
            String(_) => "a string",
            List(_) => "a list",
        }
    }
}

impl<'a> std::fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Value::*;
        match self {
            Integer(i) => write!(f, "{i}"),
            Scaled(scaled) => write!(f, "{scaled}"),
            InfiniteGlue(scaled, glue_order) => {
                write!(f, "{}{glue_order}", scaled.display_no_units())
            }
            String(str) => write!(f, r#""{str}""#),
            List(tree) => write!(f, "{tree}"),
        }
    }
}

/// A function call.
#[derive(Debug, PartialEq, Clone)]
pub struct FuncCall<'a> {
    /// Comments before the function call.
    pub comments: Vec<&'a str>,
    /// Name of the function.
    pub func_name: Str<'a>,
    /// Arguments to the function.
    pub args: Vec<Arg<'a>>,
    /// Comments between the last argument and the closing parenthesis.
    pub trailing_comments: Vec<&'a str>,
}

impl<'a> FuncCall<'a> {
    pub fn iter<'b>(&'b self) -> impl TreeIter<'a> + 'b {
        let mut iter = ExplicitIter { tree_stack: vec![] };
        iter.push_func_call(self);
        iter
    }
}

impl<'a> std::fmt::Display for FuncCall<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        pretty_print(f, self.iter())
    }
}

#[cfg(test)]
mod tests {
    use crate::ErrorAccumulator;

    use super::*;

    fn run_parse_test(input: &str, want: Vec<FuncCall>) {
        let want = Tree {
            calls: want,
            trailing_comments: vec![],
        };
        let errs: ErrorAccumulator = Default::default();
        let got = Tree::build(parse(input, errs.clone()));
        assert_eq!(got, want);
        assert_eq!(Ok(()), errs.check());
    }

    fn run_convert_test(input: Vec<FuncCall>) {
        let tree = Tree {
            calls: input,
            trailing_comments: vec![],
        };
        let got = Tree::build(tree.iter());
        assert_eq!(got, tree);
    }

    macro_rules! cst_tests {
        ( $( (
            $name: ident,
            $input: expr,
            $want: expr,
        ), )+ ) => {
            $(
                mod $name {
                    use super::*;
                    #[test]
                    fn parse_test() {
                        let input = $input;
                        let want = $want;
                        run_parse_test(&input, want);
                    }
                    #[test]
                    fn convert_test() {
                        let input = $want;
                        run_convert_test(input);
                    }
                }
            )+
        };
    }

    cst_tests!(
        (
            basic_case,
            "a(b=[c()])",
            vec![FuncCall {
                comments: vec![],
                func_name: "a".into(),
                args: vec![Arg {
                    comments: vec![],
                    key: Some("b".into()),
                    value: Value::List(Tree {
                        calls: vec![FuncCall {
                            comments: vec![],
                            func_name: "c".into(),
                            args: vec![],
                            trailing_comments: vec![],
                        }],
                        trailing_comments: vec![],
                    }),
                    value_source: "[c()]".into(),
                },],
                trailing_comments: vec![],
            }],
        ),
        (
            comma_after_list,
            "a(b=[],)",
            vec![FuncCall {
                comments: vec![],
                func_name: "a".into(),
                args: vec![Arg {
                    comments: vec![],
                    key: Some("b".into()),
                    value: Value::List(Default::default()),
                    value_source: "[]".into(),
                },],
                trailing_comments: vec![],
            }],
        ),
        (
            comment_in_empty_list,
            "a(b=[#X\n])",
            vec![FuncCall {
                comments: vec![],
                func_name: "a".into(),
                args: vec![Arg {
                    comments: vec![],
                    key: Some("b".into()),
                    value: Value::List(Tree {
                        calls: vec![],
                        trailing_comments: vec!["X"]
                    }),
                    value_source: "[#X\n]".into(),
                },],
                trailing_comments: vec![],
            }],
        ),
    );

    fn run_comments_test(
        input: &str,
        func_comments: Vec<&'static str>,
        arg_1_comments: Vec<&'static str>,
        arg_2_comments: Vec<&'static str>,
        trailing_comments: Vec<&'static str>,
    ) {
        let want = vec![FuncCall {
            comments: func_comments,
            func_name: "f".into(),
            args: vec![
                Arg {
                    comments: arg_1_comments,
                    key: None,
                    value: Value::Integer(3),
                    value_source: "3".into(),
                },
                Arg {
                    comments: arg_2_comments,
                    key: Some("key".into()),
                    value: Value::Integer(4),
                    value_source: "4".into(),
                },
            ],
            trailing_comments,
        }];
        run_parse_test(input, want.clone());
        run_convert_test(want);
    }

    macro_rules! comment_tests {
        ( $( (
            $name: ident,
            $input: expr,
            $(
                func_comments: $func_comments: expr,
            )?
            $(
                arg_1_comments: $arg_1_comments: expr,
            )?
            $(
                arg_2_comments: $arg_2_comments: expr,
            )?
            $(
                trailing_comments: $trailing_comments: expr,
            )?
        ), )+ ) => {
            $(
                #[test]
                #[allow(unused_assignments, unused_mut)]
                fn $name() {
                    let input = $input;
                    let mut func_comments = vec![];
                    $(
                        func_comments = $func_comments;
                    )?
                    let mut arg_1_comments = vec![];
                    $(
                        arg_1_comments = $arg_1_comments;
                    )?
                    let mut arg_2_comments = vec![];
                    $(
                        arg_2_comments = $arg_2_comments;
                    )?
                    let mut trailing_comments = vec![];
                    $(
                        trailing_comments = $trailing_comments;
                    )?
                    run_comments_test(&input, func_comments, arg_1_comments, arg_2_comments, trailing_comments);
                }
            )+
        };
    }

    comment_tests!(
        (
            comment_0,
            "f(3,key=4,)",
        ),
        (
            comment_1,
            "#X\nf(3,key=4,)",
            func_comments: vec!["X"],
        ),
        (
            comment_2,
            "f#X\n(3,key=4,)",
            func_comments: vec!["X"],
        ),
        (
            comment_3,
            "f(#X\n3,key=4,)",
            arg_1_comments: vec!["X"],
        ),
        (
            comment_4,
            "f(3#X\n,key=4,)",
            arg_1_comments: vec!["X"],
        ),
        (
            comment_5,
            "f(3,#X\nkey=4,)",
            arg_2_comments: vec!["X"],
        ),
        (
            comment_6,
            "f(3,key#X\n=4,)",
            arg_2_comments: vec!["X"],
        ),
        (
            comment_7,
            "f(3,key=#X\n4,)",
            arg_2_comments: vec!["X"],
        ),
        (
            comment_8,
            "f(3,key=4#X\n,)",
            arg_2_comments: vec!["X"],
        ),
        (
            comment_9,
            "f(3,key=4,#X\n)",
            trailing_comments: vec!["X"],
        ),
        (
            comment_10,
            "f(3,key=4#X\n)",
            arg_2_comments: vec!["X"],
        ),
        // TODO: f([#X\n],)
        // TODO: f([],#X\n)
        // TODO: f([]#X\n,)
    );
}
