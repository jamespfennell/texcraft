//! Lexer and tokens for Box language.

use super::Str;
use crate::Error;
use std::{borrow::Cow, rc::Rc};

/// Box language lexer.
pub struct Lexer<'a> {
    /// The full source file being lexed.
    s: &'a str,
    /// Inclusive lower bound on the part of the file being lexed by this lexer.
    l: usize,
    /// Exclusive upper bound on the part of the file being lexed by this lexer.
    u: usize,
    /// Opening parens.
    op: Rc<[Option<ClosingParen>]>,
    /// Index of the next opening paren that is expected.
    op_i: usize,
    /// Error accumulator.
    errs: super::ErrorAccumulator<'a>,
}

/// Opaque marker of a closing parenthesis.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ClosingParen {
    /// Index of the closing parenthesis in the source.
    /// Because this closing paren matches an opening paren,
    /// it cannot come at the start of the file and thus the index
    /// is strictly bigger than 0.
    source_idx: std::num::NonZeroUsize,
    /// Starting index of parens after this closing paren in the parens array.
    op_i: usize,
}

impl<'a> Lexer<'a> {
    /// Create a new Box language lexer.
    pub fn new(source: &'a str, errs: super::ErrorAccumulator<'a>) -> Self {
        Self {
            s: source,
            l: 0,
            u: source.len(),
            op: Self::build(source),
            op_i: 0,
            errs,
        }
    }

    /// Splits off a nested lexer.
    pub fn split_nested(&mut self, closing_paren: ClosingParen) -> Self {
        let inner = Self {
            s: self.s,
            l: self.l,
            u: closing_paren.source_idx.get(),
            op: self.op.clone(),
            op_i: self.op_i,
            errs: self.errs.clone(),
        };
        self.l = closing_paren.source_idx.get() + 1;
        self.op_i = closing_paren.op_i;
        inner
    }

    pub fn remaining_source(&self) -> Str<'a> {
        Str {
            value: self.s,
            start: self.l,
            end: self.u,
        }
    }
    fn build(source: &'a str) -> Rc<[Option<ClosingParen>]> {
        #[derive(Clone, Copy)]
        enum State {
            Regular,
            Comment,
            String,
        }
        struct Stack {
            i: usize,
            is_round: bool,
        }
        let mut v: Vec<Option<ClosingParen>> = vec![];
        let mut stack = vec![];
        let mut state = State::Regular;
        let mut i = 0;
        for c in source.chars() {
            match (c, state) {
                ('(' | '[', State::Regular) => {
                    stack.push(Stack {
                        i: v.len(),
                        is_round: c == '(',
                    });
                    v.push(None);
                }
                (')' | ']', State::Regular) => {
                    if let Some(s) = stack.pop() {
                        v[s.i] = Some(ClosingParen {
                            source_idx: i.try_into().unwrap(),
                            op_i: v.len(),
                        });
                        let is_round = c == ')';
                        if s.is_round != is_round {
                            stack.push(s);
                        }
                    }
                }
                ('\n', State::Comment) => {
                    state = State::Regular;
                }
                ('#', State::Regular) => {
                    state = State::Comment;
                }
                ('"', State::Regular) => {
                    state = State::String;
                }
                ('"', State::String) => {
                    state = State::Regular;
                }
                _ => {}
            }
            i += c.len_utf8();
        }
        v.into()
    }
}

/// A token in the Box language.
#[derive(Clone, Debug)]
pub struct Token<'a> {
    pub value: TokenValue<'a>,
    pub source: Str<'a>,
}

/// Value of a token in the Box language.
#[derive(Clone, Debug, PartialEq)]
pub enum TokenValue<'a> {
    SquareOpen {
        /// The closing bracket that matches this opening bracket.
        ///
        /// If `None`, this opening bracket is not matched.
        /// If provided, the closing bracket may be either `)` or `]`.
        closing: Option<ClosingParen>,
    },
    SquareClose,
    /// Opening round bracket `(`.
    RoundOpen {
        /// The closing bracket that matches this opening bracket.
        ///
        /// If `None`, this opening bracket is not matched.
        /// If provided, the closing bracket may be either `)` or `]`.
        closing: Option<ClosingParen>,
    },
    RoundClose,
    Comma,
    Equal,
    Keyword,
    String(Cow<'a, str>),
    Integer(i32),
    Scaled(core::Scaled),
    InfiniteGlue(core::Scaled, core::GlueOrder),
    Comment,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Token<'a>> {
        // Consume whitespace and comments
        let mut comment_start: Option<usize> = None;
        while let Some(c) = self.s[self.l..self.u].chars().next() {
            let should_skip = match c {
                '\n' => {
                    if let Some(comment_start) = comment_start.take() {
                        return Some(Token {
                            value: TokenValue::Comment,
                            source: Str {
                                value: self.s,
                                start: comment_start,
                                end: self.l,
                            },
                        });
                    }
                    true
                }
                '#' => {
                    if comment_start.is_none() {
                        comment_start = Some(self.l + 1);
                    }
                    true
                }
                c => comment_start.is_some() || c.is_whitespace(),
            };
            if !should_skip {
                break;
            }
            self.l += c.len_utf8();
        }
        // Now look at the token
        let mut iter = self.s[self.l..self.u].chars();
        let c = iter.next()?;
        let start = self.l;
        self.l += c.len_utf8();
        use TokenValue::*;
        let value = match c {
            '[' => {
                let closing = self.op.get(self.op_i).cloned().flatten();
                self.op_i += 1;
                SquareOpen { closing }
            }
            ']' => SquareClose,
            '(' => {
                let closing = self.op.get(self.op_i).cloned().flatten();
                self.op_i += 1;
                RoundOpen { closing }
            }
            ')' => RoundClose,
            '=' => Equal,
            ',' => Comma,
            'a'..='z' | 'A'..='Z' => {
                while let Some(n @ 'a'..='z' | n @ 'A'..='Z' | n @ '_') = iter.next() {
                    self.l += n.len_utf8();
                }
                Keyword
            }
            '"' => {
                loop {
                    match iter.next() {
                        None => return None,
                        Some(n @ '"') => {
                            self.l += n.len_utf8();
                            break;
                        }
                        Some(n) => {
                            self.l += n.len_utf8();
                        }
                    }
                }
                String(Cow::Borrowed(&self.s[start + 1..self.l - 1]))
            }
            '0'..='9' => {
                let initial_value = (c as i32) - ('0' as i32);
                self.parse_number(false, initial_value, start)
            }
            '-' => self.parse_number(true, 0, start),
            c => todo!("{c:?}"),
        };
        Some(Token {
            value,
            source: Str {
                value: self.s,
                start,
                end: self.l,
            },
        })
    }
}

impl<'a> Lexer<'a> {
    fn parse_number(
        &mut self,
        negative: bool,
        initial_value: i32,
        start_idx: usize,
    ) -> TokenValue<'a> {
        let mut iter = self.s[self.l..self.u].chars();
        let mut n = initial_value;
        let mut parsing_n = true;
        let mut d = [0_u8; 17];
        let mut next_d = 0_usize;
        loop {
            match iter.next() {
                Some(c @ '0'..='9') => {
                    let i = (c as i32) - ('0' as i32);
                    if parsing_n {
                        n = n.checked_mul(10).unwrap();
                        n = n.checked_add(i).unwrap();
                    } else {
                        if let Some(d) = d.get_mut(next_d) {
                            *d = i.try_into().expect("i in [0,9]")
                        }
                        next_d += 1;
                    }
                    self.l += c.len_utf8();
                }
                Some(d @ '.') => {
                    if !parsing_n {
                        panic!("two decimal points")
                    }
                    parsing_n = false;
                    self.l += d.len_utf8();
                }
                Some(c @ 'a'..='z' | c @ 'A'..='Z') => {
                    let u = self.l;
                    self.l += c.len_utf8();
                    while let Some(n @ 'a'..='z' | n @ 'A'..='Z' | n @ '_') = iter.next() {
                        self.l += n.len_utf8();
                    }

                    let mut s = core::Scaled::from_decimal_digits(&d) + core::Scaled::ONE * n;
                    if negative {
                        s.0 *= -1;
                    }
                    let raw_unit = &self.s[u..self.l];
                    if let Some(unit) = core::ScaledUnit::parse(raw_unit) {
                        let mut s =
                            core::Scaled::new(n, core::Scaled::from_decimal_digits(&d), unit)
                                .unwrap();
                        if negative {
                            s = -s;
                        }
                        return TokenValue::Scaled(s);
                    }
                    if let Some(glue_order) = core::GlueOrder::parse(raw_unit) {
                        return TokenValue::InfiniteGlue(s, glue_order);
                    }
                    self.errs.add(Error::InvalidDimensionUnit {
                        dimension: Str {
                            value: self.s,
                            start: start_idx,
                            end: self.l,
                        },
                        unit: Str {
                            value: self.s,
                            start: u,
                            end: self.l,
                        },
                    });
                    return TokenValue::Scaled(core::Scaled::ZERO);
                }
                _ => {
                    if !parsing_n {
                        panic!("decimal number provided, but not unit like pt or fil");
                    }
                    if negative {
                        n *= -1;
                    }
                    return TokenValue::Integer(n);
                }
            }
        }
    }
}
