//! Lexer and tokens for Box language.

use super::Str;
use crate::Error;
use std::borrow::Cow;

/// Box language lexer.
pub struct Lexer<'a> {
    s: &'a str,
    u: usize,
    errs: super::ErrorAccumulator<'a>,
}

impl<'a> Lexer<'a> {
    /// Create a new Box language lexer.
    pub fn new(source: &'a str, errs: super::ErrorAccumulator<'a>) -> Self {
        Self {
            s: source,
            u: 0,
            errs,
        }
    }
}

/// A token in the Box language.
#[derive(Clone)]
pub struct Token<'a> {
    pub value: TokenValue<'a>,
    pub source: Str<'a>,
}

/// Value of a token in the Box language.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenValue<'a> {
    SquareOpen,
    SquareClose,
    RoundOpen,
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
        while let Some(c) = self.s[self.u..].chars().next() {
            let should_skip = match c {
                '\n' => {
                    if let Some(comment_start) = comment_start.take() {
                        return Some(Token {
                            value: TokenValue::Comment,
                            source: Str {
                                value: self.s,
                                start: comment_start,
                                end: self.u,
                            },
                        });
                    }
                    true
                }
                '#' => {
                    if comment_start.is_none() {
                        comment_start = Some(self.u + 1);
                    }
                    true
                }
                c => comment_start.is_some() || c.is_whitespace(),
            };
            if !should_skip {
                break;
            }
            self.u += c.len_utf8();
        }
        // Now look at the token
        let mut iter = self.s[self.u..].chars();
        let c = iter.next()?;
        let u = self.u;
        self.u += c.len_utf8();
        use TokenValue::*;
        let value = match c {
            '[' => SquareOpen,
            ']' => SquareClose,
            '(' => RoundOpen,
            ')' => RoundClose,
            '=' => Equal,
            ',' => Comma,
            'a'..='z' | 'A'..='Z' => {
                while let Some(n @ 'a'..='z' | n @ 'A'..='Z' | n @ '_') = iter.next() {
                    self.u += n.len_utf8();
                }
                Keyword
            }
            '"' => {
                loop {
                    match iter.next() {
                        None => return None,
                        Some(n @ '"') => {
                            self.u += n.len_utf8();
                            break;
                        }
                        Some(n) => {
                            self.u += n.len_utf8();
                        }
                    }
                }
                String(Cow::Borrowed(&self.s[u + 1..self.u - 1]))
            }
            '0'..='9' => {
                let start = (c as i32) - ('0' as i32);
                self.parse_number(false, start, u)
            }
            '-' => self.parse_number(true, 0, u),
            c => todo!("{c:?}"),
        };
        Some(Token {
            value,
            source: Str {
                value: self.s,
                start: u,
                end: self.u,
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
        let mut iter = self.s[self.u..].chars();
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
                    self.u += c.len_utf8();
                }
                Some(d @ '.') => {
                    if !parsing_n {
                        panic!("two decimal points")
                    }
                    parsing_n = false;
                    self.u += d.len_utf8();
                }
                Some(c @ 'a'..='z' | c @ 'A'..='Z') => {
                    let u = self.u;
                    self.u += c.len_utf8();
                    while let Some(n @ 'a'..='z' | n @ 'A'..='Z' | n @ '_') = iter.next() {
                        self.u += n.len_utf8();
                    }

                    let mut s = core::Scaled::from_decimal_digits(&d) + core::Scaled::ONE * n;
                    if negative {
                        s.0 *= -1;
                    }
                    let raw_unit = &self.s[u..self.u];
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
                            end: self.u,
                        },
                        unit: Str {
                            value: self.s,
                            start: u,
                            end: self.u,
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
