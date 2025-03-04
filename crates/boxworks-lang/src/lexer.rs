pub struct Lexer<'a> {
    s: &'a str,
    u: usize,
    cache: Option<Token<'a>>,
    errs: Vec<super::Error<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(s: &'a str) -> Self {
        Self {
            s,
            u: 0,
            cache: None,
            errs: vec![],
        }
    }
    pub fn errs_mut(&mut self) -> &mut Vec<super::Error<'a>> {
        &mut self.errs
    }
}

#[derive(Clone)]
pub struct Token<'a> {
    pub value: TokenValue<'a>,
    pub source: super::Str<'a>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenValue<'a> {
    SquareOpen,
    SquareClose,
    RoundOpen,
    RoundClose,
    Comma,
    Equal,
    Keyword(&'a str),
    String(&'a str),
    Integer(i32),
    Scaled(core::Scaled),
    InfiniteGlue(core::Scaled, core::GlueOrder),
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(cached) = self.cache.take() {
            return Some(cached);
        }
        // Consume whitespace and comments
        let mut in_comment = false;
        while let Some(c) = self.s[self.u..].chars().next() {
            let should_skip = match c {
                '\n' => {
                    in_comment = false;
                    true
                }
                '#' => {
                    in_comment = true;
                    true
                }
                c => in_comment || c.is_whitespace(),
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
                Keyword(&self.s[u..self.u])
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
                String(&self.s[u + 1..self.u - 1])
            }
            '0'..='9' => {
                let start = (c as i32) - ('0' as i32);
                self.parse_number(false, start)
            }
            '-' => self.parse_number(true, 0),
            c => todo!("{c:?}"),
        };
        Some(Token {
            value,
            source: super::Str {
                value: self.s,
                start: u,
                end: self.u,
            },
        })
    }
}

impl<'a> Lexer<'a> {
    pub fn check_errors(self) -> Result<(), Vec<super::Error<'a>>> {
        if self.errs.is_empty() {
            Ok(())
        } else {
            Err(self.errs)
        }
    }
    pub fn error(&mut self, err: super::Error<'a>) {
        self.errs.push(err);
    }
    pub fn peek(&mut self) -> Option<Token<'a>> {
        let next = self.next();
        self.cache = next.clone();
        next
    }
    fn parse_number(&mut self, negative: bool, start: i32) -> TokenValue<'a> {
        let mut iter = self.s[self.u..].chars();
        let mut n = start;
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
                    panic!("did not recognize unit");
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
