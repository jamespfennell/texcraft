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
//! The [`TreeIter`] is not a standard Rust iterator.
//! It's somewhere between a state machine and a Rust iterator.
//! To advance to the next stage of iteration the method [`TreeIter::next`]
//! is invoked.
//! This consumes the iterator, and returns a value of type [`TreeItem`].
//! This value describes the next element in the tree and the iterator
//!     that can be used to continue the iteration.
//! The next iterator may be the same [`TreeIter`], or if the next element
//!     of the tree is a function call ([`TreeItem::FuncCallStart`]), the iterator will be an
//!     [`ArgsIter`] that iterates over arguments to that call.
//! The [`ArgsIter`] works the same as the tree iterator but returns values
//!     of type [`ArgsItem`].

use std::borrow::Cow;
use std::collections::VecDeque;
use std::fmt::Write;

use super::error::Error;
use super::lexer;
use super::Str;

/// Iterator representation of a CST.
pub trait TreeIter<'a>: Sized {
    /// Args iterator associated to this iterator.
    type ArgsIter: ArgsIter<'a, TreeIter = Self>;
    /// Move to the next stage of iterator.
    fn next(self) -> TreeItem<'a, Self::ArgsIter, Self>;
}

/// Item returned by [`TreeIter`].
pub enum TreeItem<'a, A, F> {
    /// A function call, like `text("Hello", font=3)`.
    FuncCallStart {
        /// Name of the function
        func_name: Str<'a>,
        /// Iterator over the arguments of the function.
        iter: A,
    },
    /// A comment, like `# this is a comment`.
    Comment {
        /// Value of the comment.
        value: &'a str,
        /// The same iterator that returned this comment.
        iter: F,
    },
    /// Marker that the iteration has ended.
    Exhausted {
        /// Closing brace token the ended the list, if present.
        closing_brace: Str<'a>,
        /// Parent iterator.
        ///
        /// If this is the root tree, then this field is ignored.
        ///
        /// Otherwise, this tree is the argument to a function.
        /// In this case the parent iterator is the iterator over that function's arguments.
        /// Using this iterator the rest of the arguments of the function can be obtained.
        iter: A,
    },
}

impl<'a> TreeItem<'a, (), ()> {
    /// Populate the iterators on an item.
    pub fn into<T, A: From<T>, F: From<T>>(self, t: T) -> TreeItem<'a, A, F> {
        use TreeItem::*;
        match self {
            FuncCallStart { func_name, iter: _ } => FuncCallStart {
                func_name,
                iter: t.into(),
            },
            Comment { value, iter: _ } => Comment {
                value,
                iter: t.into(),
            },
            Exhausted {
                closing_brace,
                iter: _,
            } => Exhausted {
                closing_brace,
                iter: t.into(),
            },
        }
    }
}

/// Iterator over the arguments of a function.
pub trait ArgsIter<'a>: Sized {
    /// Tree iterator associated to this iterator.
    type TreeIter: TreeIter<'a, ArgsIter = Self>;
    /// Move to the next stage of iterator.
    fn next(self) -> ArgsItem<'a, Self, Self::TreeIter>;
}

/// Item returned by [`ArgsIter`].
pub enum ArgsItem<'a, A, F> {
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
        /// Iterator over the remaining arguments of the function.
        /// This is the same iterator that returned this item.
        iter: A,
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
    /// Marker that the iteration has ended.
    Exhausted {
        /// Parent iterator.
        ///
        /// The current [`ArgsIter`] that returned this item is
        ///     an iterator over the arguments of a function.
        /// This parent iterator is an iterator over the list of functions
        ///     that included that function.
        iter: F,
    },
    /// A comment.
    Comment {
        /// Value of the comment.
        value: &'a str,
        /// The same iterator that returned this comment.
        iter: A,
    },
}

impl<'a> ArgsItem<'a, (), ()> {
    /// Populate the iterators on an item.
    pub fn into<T, A: From<T>, F: From<T>>(self, t: T) -> ArgsItem<'a, A, F> {
        use ArgsItem::*;
        match self {
            Regular {
                key,
                value,
                value_source,
                iter: _,
            } => Regular {
                key,
                value,
                value_source,
                iter: t.into(),
            },
            List {
                key,
                square_open,
                iter: _,
            } => List {
                key,
                square_open,
                iter: t.into(),
            },
            Exhausted { iter: _ } => Exhausted { iter: t.into() },
            Comment { value, iter: _ } => Comment {
                value,
                iter: t.into(),
            },
        }
    }
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
) -> Result<T::ArgsIter, std::fmt::Error> {
    let indent = Indent(depth);
    let mut ap: ArgsPrinter = ArgsPrinter {
        indent: depth,
        ..Default::default()
    };
    loop {
        iter = match iter.next() {
            TreeItem::FuncCallStart {
                func_name,
                mut iter,
            } => {
                write!(w, "{indent}{func_name}(")?;
                loop {
                    iter = match iter.next() {
                        ArgsItem::Comment { value, iter } => {
                            ap.activate_multiline(w)?;
                            write!(w, "\n{indent}  #{value}")?;
                            iter
                        }
                        ArgsItem::Regular {
                            key,
                            value,
                            value_source: _,
                            iter,
                        } => {
                            ap.print(w, key, value)?;
                            iter
                        }
                        ArgsItem::List {
                            key,
                            square_open: _,
                            iter,
                        } => {
                            ap.activate_multiline(w)?;
                            write!(w, "\n{indent}  ")?;
                            if let Some(key) = key {
                                write!(w, "{key}=")?;
                            }
                            writeln!(w, "[")?;
                            let iter = pretty_print_impl(w, iter, depth + 4)?;
                            write!(w, "{indent}  ],")?;
                            iter
                        }
                        ArgsItem::Exhausted { iter } => {
                            ap.flush(w)?;
                            writeln!(w, ")")?;
                            break iter;
                        }
                    }
                }
            }
            TreeItem::Comment { value, iter } => {
                writeln!(w, "{indent}#{value}").unwrap();
                iter
            }
            TreeItem::Exhausted {
                closing_brace: _,
                iter,
            } => {
                break Ok(iter);
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
        Self::build_impl(iter).0
    }

    fn build_impl<I: TreeIter<'a>>(mut iter: I) -> (Self, Str<'a>, I::ArgsIter) {
        let mut comments = vec![];
        let take_comments = |comments: &mut Vec<&'a str>| {
            let mut v = vec![];
            std::mem::swap(&mut v, comments);
            v
        };
        let mut calls = vec![];
        let s = loop {
            iter = match iter.next() {
                TreeItem::FuncCallStart {
                    func_name,
                    mut iter,
                } => {
                    let func_comments = take_comments(&mut comments);
                    let mut args: Vec<Arg<'a>> = vec![];
                    let iter = loop {
                        iter = match iter.next() {
                            ArgsItem::Exhausted { iter } => break iter,
                            ArgsItem::Regular {
                                key,
                                value,
                                value_source,
                                iter,
                            } => {
                                args.push(Arg {
                                    comments: take_comments(&mut comments),
                                    key,
                                    value,
                                    value_source,
                                });
                                iter
                            }
                            ArgsItem::List {
                                key,
                                square_open,
                                iter,
                            } => {
                                let (inner_tree, end, iter) = Tree::build_impl(iter);
                                let s = Str {
                                    end: end.end,
                                    ..square_open
                                };
                                args.push(Arg {
                                    comments: take_comments(&mut comments),
                                    key,
                                    value: Value::List(inner_tree),
                                    value_source: s,
                                });
                                iter
                            }
                            ArgsItem::Comment { value, iter } => {
                                comments.push(value);
                                iter
                            }
                        };
                    };
                    calls.push(FuncCall {
                        comments: func_comments,
                        func_name,
                        args,
                        trailing_comments: take_comments(&mut comments),
                    });
                    iter
                }
                TreeItem::Comment { value, iter } => {
                    comments.push(value);
                    iter
                }
                TreeItem::Exhausted {
                    closing_brace,
                    iter,
                } => break (closing_brace, iter),
            };
        };
        (
            Self {
                calls,
                trailing_comments: comments,
            },
            s.0,
            s.1,
        )
    }

    pub fn iter<'b>(&'b self) -> impl TreeIter<'a> + 'b {
        let mut iter = ExplicitIter {
            tree_stack: vec![],
            args_stack: vec![],
        };
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
    args_stack: Vec<ArgsStackElem<'a, 'b>>,
}

enum TreeStackElem<'a, 'b> {
    FuncCall(&'b FuncCall<'a>),
    Comments(&'b [&'a str]),
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
    type ArgsIter = Self;

    fn next(mut self) -> TreeItem<'a, Self::ArgsIter, Self> {
        loop {
            let Some(last) = self.tree_stack.pop() else {
                return TreeItem::Exhausted {
                    closing_brace: "".into(),
                    iter: self,
                };
            };
            match last {
                TreeStackElem::FuncCall(func_call) => {
                    self.push_args(func_call);
                    return TreeItem::FuncCallStart {
                        func_name: func_call.func_name.clone(),
                        iter: self,
                    };
                }
                TreeStackElem::Comments(items) => {
                    if let Some((head, tail)) = items.split_first() {
                        self.tree_stack.push(TreeStackElem::Comments(tail));
                        return TreeItem::Comment {
                            value: head,
                            iter: self,
                        };
                    }
                }
            }
        }
    }
}

impl<'a, 'b> ArgsIter<'a> for ExplicitIter<'a, 'b> {
    type TreeIter = Self;
    fn next(mut self) -> ArgsItem<'a, Self, Self::TreeIter> {
        loop {
            let Some(last) = self.args_stack.pop() else {
                return ArgsItem::Exhausted { iter: self };
            };
            match last {
                ArgsStackElem::Arg(arg) => {
                    return match arg.value.try_clone_value() {
                        Ok(v) => ArgsItem::Regular {
                            key: arg.key.clone(),
                            value: v,
                            value_source: arg.value_source.clone(),
                            iter: self,
                        },
                        Err(tree) => {
                            self.push_tree(tree);
                            ArgsItem::List {
                                key: arg.key.clone(),
                                square_open: "".into(),
                                iter: self,
                            }
                        }
                    };
                }
                ArgsStackElem::Comments(items) => {
                    if let Some((head, tail)) = items.split_first() {
                        self.args_stack.push(ArgsStackElem::Comments(tail));
                        return ArgsItem::Comment {
                            value: head,
                            iter: self,
                        };
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
    lexer: impl Iterator<Item = lexer::Token<'a>>,
    errs: super::ErrorAccumulator<'a>,
) -> impl TreeIter<'a> {
    ParseTreeIter {
        next_tree: None,
        next_arg: None,
        comments: Default::default(),
        next_token: None,
        list_stack: vec![],
        lexer,
        errs,
    }
}

struct ParseTreeIter<'a, I> {
    next_tree: Option<TreeItem<'a, (), ()>>,
    next_arg: Option<ArgsItem<'a, (), ()>>,
    comments: VecDeque<&'a str>,
    next_token: Option<lexer::Token<'a>>,
    lexer: I,
    list_stack: Vec<lexer::Token<'a>>,
    errs: super::ErrorAccumulator<'a>,
}

impl<'a, I> ParseTreeIter<'a, I>
where
    I: Iterator<Item = lexer::Token<'a>>,
{
    fn next_token(&mut self) -> Option<lexer::Token<'a>> {
        let token_or = self.peek_token();
        self.next_token = None;
        token_or
    }
    fn peek_token(&mut self) -> Option<lexer::Token<'a>> {
        match self.next_token.clone() {
            None => {
                for token in self.lexer.by_ref() {
                    if token.value == lexer::TokenValue::Comment {
                        self.comments.push_back(token.source.str());
                        continue;
                    }
                    self.next_token = Some(token);
                    break;
                }
                self.next_token.clone()
            }
            Some(token) => Some(token),
        }
    }
}

impl<'a, I> TreeIter<'a> for ParseTreeIter<'a, I>
where
    I: Iterator<Item = lexer::Token<'a>>,
{
    type ArgsIter = Self;
    fn next(mut self) -> TreeItem<'a, Self::ArgsIter, Self> {
        if let Some(comment) = self.comments.pop_front() {
            return TreeItem::Comment {
                value: comment,
                iter: self,
            };
        }
        if let Some(next) = self.next_tree.take() {
            return next.into(self);
        }
        let Some(next) = self.next_token() else {
            // We assume that this function was called WITHOUT parsing [.
            // Otherwise we would expect the list to end with ].
            // TODO: return an error if this is not true
            // We should pass Option<Token>
            // TODO: return any trailing comments too!
            assert!(self.list_stack.is_empty());
            if self.comments.is_empty() {
                return TreeItem::Exhausted {
                    closing_brace: "".into(),
                    iter: self,
                };
            }
            return TreeIter::next(self);
        };
        // parse function name
        let func_name = match next.value {
            lexer::TokenValue::Keyword => next.source,
            lexer::TokenValue::SquareClose => {
                if self.list_stack.pop().is_none() {
                    self.errs.add(Error::UnmatchedClosingSquareBracket {
                        square_close: next.source.clone(),
                    });
                    return TreeIter::next(self);
                }
                if let Some(lexer::TokenValue::Comma) = self.peek_token().map(|t| t.value) {
                    self.next_token();
                };
                self.next_tree = Some(TreeItem::Exhausted {
                    closing_brace: next.source,
                    iter: (),
                });
                return TreeIter::next(self);
            }
            _ => todo!("error: expected keyword"),
        };
        // parse (
        assert_eq!(
            self.next_token().map(|t| t.value),
            Some(lexer::TokenValue::RoundOpen)
        );
        self.next_tree = Some(TreeItem::FuncCallStart {
            func_name,
            iter: (),
        });
        return TreeIter::next(self);
    }
}

impl<'a, I> ArgsIter<'a> for ParseTreeIter<'a, I>
where
    I: Iterator<Item = lexer::Token<'a>>,
{
    type TreeIter = Self;
    fn next(mut self) -> ArgsItem<'a, Self, Self::TreeIter> {
        if let Some(comment) = self.comments.pop_front() {
            return ArgsItem::Comment {
                value: comment,
                iter: self,
            };
        }
        if let Some(next) = self.next_arg.take() {
            return next.into(self);
        }
        let next = self.peek_token().unwrap();
        let key = match next.value {
            // no more arguments
            lexer::TokenValue::RoundClose => {
                // consume the ) token
                self.next_token();
                self.next_arg = Some(ArgsItem::Exhausted { iter: () });
                return ArgsIter::next(self);
            }
            // key=value argument
            lexer::TokenValue::Keyword => {
                // consume the keyword token
                self.next_token();
                assert_eq!(
                    self.next_token().map(|t| t.value),
                    Some(lexer::TokenValue::Equal)
                );
                Some(next.source)
            }
            // positional argument
            _ => None,
        };
        let value_token = self.next_token().unwrap();
        let value = match value_token.value {
            lexer::TokenValue::String(s) => Value::String(s),
            lexer::TokenValue::Integer(n) => Value::Integer(n),
            lexer::TokenValue::Scaled(n) => Value::Scaled(n),
            lexer::TokenValue::InfiniteGlue(s, o) => Value::InfiniteGlue(s, o),
            lexer::TokenValue::SquareOpen => {
                self.list_stack.push(value_token.clone());
                self.next_arg = Some(ArgsItem::List {
                    key,
                    square_open: value_token.source,
                    iter: (),
                });
                return ArgsIter::next(self);
            }
            a => {
                panic!("expected value, instead got {a:?}")
            }
        };
        // Consume an optional ',' after the value.
        if let Some(lexer::TokenValue::Comma) = self.peek_token().map(|t| t.value) {
            // Consume the ',' token.
            self.next_token();
        }
        self.next_arg = Some(ArgsItem::Regular {
            key,
            value,
            value_source: value_token.source,
            iter: (),
        });
        return ArgsIter::next(self);
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
        let mut iter = ExplicitIter {
            tree_stack: vec![],
            args_stack: vec![],
        };
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
