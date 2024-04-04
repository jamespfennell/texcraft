//! Lexer for property list files.
//!
//! TODO: this lexer is pretty pointless. We should just parse straight to a CST.

/// Type of a token.
#[derive(PartialEq, Eq, Debug)]
pub enum TokenKind {
    /// An open parenthesis `(`.
    OpenParenthesis,
    /// An closed parenthesis `)`.
    ClosedParenthesis,
    /// An single word.
    /// This is string consisting of visible ASCII symbols without whitespace or open or closed parenthesis.
    Word { value: String },
    /// Potentially significant whitespace.
    Whitespace { len: usize, contains_newlines: bool },
}

/// Token in a property list file.
///
/// The second element is the start of the span of the token;
///     i.e., the index in the file where the token starts.
#[derive(PartialEq, Eq, Debug)]
pub struct Token(pub TokenKind, pub usize);

/// Lexer for property list files.
pub struct Lexer {
    source: String,
    /// The current bytes position within the source string.
    current: usize,
    current_char: usize,
    /// The current char position within the source string.
    start_of_line: bool,
}

impl Lexer {
    /// Create a new lexer from source code.
    pub fn new(source: &str) -> Lexer {
        Lexer {
            source: source.into(),
            current: 0,
            current_char: 0,
            start_of_line: true,
        }
    }
    /// Return the length of the source code.
    pub fn source_length(&self) -> usize {
        self.source.chars().map(|_| 1).sum()
    }
    fn accumulate_string(&mut self) -> TokenKind {
        let mut value: String = Default::default();
        for c in self.source[self.current..].chars() {
            let c = match c {
                '(' | ')' | '\n' | ' ' => break,
                '\r' => {
                    if self.source[self.current + 1..].starts_with('\n') {
                        break;
                    }
                    '\r'
                }
                _ => c,
            };
            value.push(c);
        }
        self.current += value.len();
        self.current_char += value.chars().map(|_| 1).sum::<usize>();
        TokenKind::Word { value }
    }

    fn accumulate_whitespace(&mut self) -> Option<TokenKind> {
        let mut l: usize = 0;
        let mut contains_newlines: bool = false;
        loop {
            let c = match self.source[self.current..].chars().next() {
                None => break,
                Some(c) => c,
            };
            let (len, is_newline) = match c {
                ' ' => (1, false),
                '\n' => (1, true),
                '\r' => {
                    if !self.source[self.current + 1..].starts_with('\n') {
                        break;
                    }
                    (2, true)
                }
                _ => break,
            };
            self.current += len;
            self.current_char += len;
            if self.start_of_line {
                continue;
            }
            l += 1;
            if is_newline {
                contains_newlines = true;
                self.start_of_line = true;
            }
        }
        if l == 0 {
            None
        } else {
            Some(TokenKind::Whitespace {
                len: l,
                contains_newlines,
            })
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let start_of_token = self.current_char;
            let c = match self.source[self.current..].chars().next() {
                None => return None,
                Some(c) => c,
            };
            let kind = match c {
                '\n' | ' ' => match self.accumulate_whitespace() {
                    None => continue,
                    Some(kind) => kind,
                },
                '\r' => {
                    if self.source[self.current + 1..].starts_with('\n') {
                        match self.accumulate_whitespace() {
                            None => continue,
                            Some(kind) => kind,
                        }
                    } else {
                        self.accumulate_string()
                    }
                }
                '(' => {
                    self.start_of_line = false;
                    self.current += 1;
                    self.current_char += 1;
                    TokenKind::OpenParenthesis
                }
                ')' => {
                    self.start_of_line = false;
                    self.current += 1;
                    self.current_char += 1;
                    TokenKind::ClosedParenthesis
                }
                _ => {
                    self.start_of_line = false;
                    self.accumulate_string()
                }
            };
            return Some(Token(kind, start_of_token));
        }
    }
}

#[cfg(test)]
mod test {
    use super::TokenKind::*;
    use super::*;

    macro_rules! lexer_tests {
        ($( ($name: ident, $input: expr, $want: expr, ), )+ ) => {
            $(
                #[test]
                fn $name() {
                    let input = $input;
                    let want: Vec<(TokenKind, usize)> = $want;
                    let want: Vec<Token> = want.into_iter().map(|(a,b)| Token(a,b)).collect();
                    let lexer = Lexer::new(&input);
                    let got: Vec<Token> = lexer.collect();
                    assert_eq!(got, want);
                }
            )+
        };
    }

    lexer_tests!(
        (
            basic_1,
            " (HELLO WORLD) ",
            vec![
                (OpenParenthesis, 1),
                (
                    Word {
                        value: "HELLO".into(),
                    },
                    2
                ),
                (
                    Whitespace {
                        len: 1,
                        contains_newlines: false
                    },
                    7
                ),
                (
                    Word {
                        value: "WORLD".into(),
                    },
                    8
                ),
                (ClosedParenthesis, 13),
                (
                    Whitespace {
                        len: 1,
                        contains_newlines: false
                    },
                    14
                ),
            ],
        ),
        (
            basic_2,
            "(HELLO   WORLD   )",
            vec![
                (OpenParenthesis, 0),
                (
                    Word {
                        value: "HELLO".into(),
                    },
                    1
                ),
                (
                    Whitespace {
                        len: 3,
                        contains_newlines: false
                    },
                    6
                ),
                (
                    Word {
                        value: "WORLD".into(),
                    },
                    9
                ),
                (
                    Whitespace {
                        len: 3,
                        contains_newlines: false
                    },
                    14
                ),
                (ClosedParenthesis, 17),
            ],
        ),
        (
            nested,
            "(HELLO WORLD (MUNDO))",
            vec![
                (OpenParenthesis, 0),
                (
                    Word {
                        value: "HELLO".into(),
                    },
                    1
                ),
                (
                    Whitespace {
                        len: 1,
                        contains_newlines: false
                    },
                    6
                ),
                (
                    Word {
                        value: "WORLD".into(),
                    },
                    7
                ),
                (
                    Whitespace {
                        len: 1,
                        contains_newlines: false
                    },
                    12
                ),
                (OpenParenthesis, 13),
                (
                    Word {
                        value: "MUNDO".into(),
                    },
                    14
                ),
                (ClosedParenthesis, 19),
                (ClosedParenthesis, 20),
            ],
        ),
        (
            newline_1,
            "(HELLO \n)",
            vec![
                (OpenParenthesis, 0),
                (
                    Word {
                        value: "HELLO".into(),
                    },
                    1
                ),
                (
                    Whitespace {
                        len: 2,
                        contains_newlines: true
                    },
                    6
                ),
                (ClosedParenthesis, 8),
            ],
        ),
        (
            newline_2,
            "(HELLO \n  )",
            vec![
                (OpenParenthesis, 0),
                (
                    Word {
                        value: "HELLO".into(),
                    },
                    1
                ),
                (
                    Whitespace {
                        len: 2,
                        contains_newlines: true
                    },
                    6
                ),
                (ClosedParenthesis, 10),
            ],
        ),
        (
            newline_3,
            "(HELLO \r\n)",
            vec![
                (OpenParenthesis, 0),
                (
                    Word {
                        value: "HELLO".into(),
                    },
                    1
                ),
                (
                    Whitespace {
                        len: 2,
                        contains_newlines: true
                    },
                    6
                ),
                (ClosedParenthesis, 9),
            ],
        ),
        (
            newline_4,
            "(HELLO\n\n\n\n)",
            vec![
                (OpenParenthesis, 0),
                (
                    Word {
                        value: "HELLO".into(),
                    },
                    1
                ),
                (
                    Whitespace {
                        len: 1,
                        contains_newlines: true
                    },
                    6
                ),
                (ClosedParenthesis, 10),
            ],
        ),
        (
            invalid_char_1,
            "(HËLLO)",
            vec![
                (OpenParenthesis, 0),
                (
                    Word {
                        value: "HËLLO".into(),
                    },
                    1
                ),
                (ClosedParenthesis, 6),
            ],
        ),
        (
            invalid_char_2,
            "(H\rLLO)",
            vec![
                (OpenParenthesis, 0),
                (
                    Word {
                        value: "H\rLLO".into(),
                    },
                    1
                ),
                (ClosedParenthesis, 6),
            ],
        ),
        (
            invalid_char_3,
            "(H \rLLO)",
            vec![
                (OpenParenthesis, 0),
                (Word { value: "H".into() }, 1),
                (
                    Whitespace {
                        len: 1,
                        contains_newlines: false
                    },
                    2
                ),
                (
                    Word {
                        value: "\rLLO".into(),
                    },
                    3
                ),
                (ClosedParenthesis, 7),
            ],
        ),
    );
}
