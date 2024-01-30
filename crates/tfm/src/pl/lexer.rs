//! Lexer for property list files.

use super::error::ParseError;

/// Type of a token.
#[derive(PartialEq, Eq, Debug)]
pub enum TokenKind {
    /// An open parenthesis `(`.
    OpenParenthesis,
    /// An closed parenthesis `)`.
    ClosedParenthesis,
    /// An single word.
    /// This is string consisting of visible ASCII symbols without whitespace or open or closed parenthesis.
    Word(String),
    /// Potentially significant whitespace.
    /// The first element is the number of characters of significant whitespace,
    /// and the second element specifies whether the whitespace contains a newline character.
    Whitespace(usize, bool),
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
    /// The current position within the source string.
    current: usize,
    start_of_line: bool,
}

impl Lexer {
    /// Create a new lexer from source code.
    pub fn new(source: &str) -> Lexer {
        Lexer {
            source: source.into(),
            current: 0,
            start_of_line: true,
        }
    }
    /// Return the length of the source code.
    pub fn source_length(&self) -> usize {
        self.source.len()
    }
    fn accumulate_string(&mut self) -> String {
        let mut s: String = Default::default();
        for c in self.source[self.current..].chars() {
            match c {
                '(' | ')' => break,
                '!'..='~' => s.push(c),
                _ => break,
            }
        }
        self.current += s.len();
        s
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
                        // An error for this case is created in the main iterator loop.
                        // We don't create one here because we may need to return whitespace
                        // that has already been accumulated.
                        break;
                    }
                    (2, true)
                }
                _ => break,
            };
            self.current += len;
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
            Some(TokenKind::Whitespace(l, contains_newlines))
        }
    }
}

impl Iterator for Lexer {
    type Item = Result<Token, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let start_of_token = self.current;
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
                    if !self.source[self.current + 1..].starts_with('\n') {
                        self.current += 1;
                        return Some(Err(ParseError::InvalidCharacter('\r', start_of_token)));
                    }
                    match self.accumulate_whitespace() {
                        None => continue,
                        Some(kind) => kind,
                    }
                }
                '(' => {
                    self.start_of_line = false;
                    self.current += 1;
                    TokenKind::OpenParenthesis
                }
                ')' => {
                    self.start_of_line = false;
                    self.current += 1;
                    TokenKind::ClosedParenthesis
                }
                '!'..='~' => {
                    self.start_of_line = false;
                    TokenKind::Word(self.accumulate_string())
                }
                c => {
                    self.current += c.len_utf8();
                    return Some(Err(ParseError::InvalidCharacter(c, start_of_token)));
                }
            };
            return Some(Ok(Token(kind, start_of_token)));
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
                    let want: Vec<Result<Token, ParseError>> = $want;
                    let lexer = Lexer::new(&input);
                    let got: Vec<Result<Token, ParseError>> = lexer.collect();
                    assert_eq!(got, want);
                }
            )+
        };
    }

    fn token(kind: TokenKind, span: usize) -> Result<Token, ParseError> {
        Ok(Token(kind, span))
    }

    lexer_tests!(
        (
            basic_1,
            " (HELLO WORLD) ",
            vec![
                token(OpenParenthesis, 1),
                token(Word("HELLO".into()), 2),
                token(Whitespace(1, false), 7),
                token(Word("WORLD".into()), 8),
                token(ClosedParenthesis, 13),
                token(Whitespace(1, false), 14),
            ],
        ),
        (
            basic_2,
            "(HELLO   WORLD   )",
            vec![
                token(OpenParenthesis, 0),
                token(Word("HELLO".into()), 1),
                token(Whitespace(3, false), 6),
                token(Word("WORLD".into()), 9),
                token(Whitespace(3, false), 14),
                token(ClosedParenthesis, 17),
            ],
        ),
        (
            nested,
            "(HELLO WORLD (MUNDO))",
            vec![
                token(OpenParenthesis, 0),
                token(Word("HELLO".into()), 1),
                token(Whitespace(1, false), 6),
                token(Word("WORLD".into()), 7),
                token(Whitespace(1, false), 12),
                token(OpenParenthesis, 13),
                token(Word("MUNDO".into()), 14),
                token(ClosedParenthesis, 19),
                token(ClosedParenthesis, 20),
            ],
        ),
        (
            newline_1,
            "(HELLO \n)",
            vec![
                token(OpenParenthesis, 0),
                token(Word("HELLO".into()), 1),
                token(Whitespace(2, true), 6),
                token(ClosedParenthesis, 8),
            ],
        ),
        (
            newline_2,
            "(HELLO \n  )",
            vec![
                token(OpenParenthesis, 0),
                token(Word("HELLO".into()), 1),
                token(Whitespace(2, true), 6),
                token(ClosedParenthesis, 10),
            ],
        ),
        (
            newline_3,
            "(HELLO \r\n)",
            vec![
                token(OpenParenthesis, 0),
                token(Word("HELLO".into()), 1),
                token(Whitespace(2, true), 6),
                token(ClosedParenthesis, 9),
            ],
        ),
        (
            newline_4,
            "(HELLO\n\n\n\n)",
            vec![
                token(OpenParenthesis, 0),
                token(Word("HELLO".into()), 1),
                token(Whitespace(1, true), 6),
                token(ClosedParenthesis, 10),
            ],
        ),
        (
            invalid_char_1,
            "(HËLLO)",
            vec![
                token(OpenParenthesis, 0),
                token(Word("H".into()), 1),
                Err(ParseError::InvalidCharacter('Ë', 2)),
                token(Word("LLO".into()), 4),
                token(ClosedParenthesis, 7),
            ],
        ),
        (
            invalid_char_2,
            "(H\rLLO)",
            vec![
                token(OpenParenthesis, 0),
                token(Word("H".into()), 1),
                Err(ParseError::InvalidCharacter('\r', 2)),
                token(Word("LLO".into()), 3),
                token(ClosedParenthesis, 6),
            ],
        ),
        (
            invalid_char_3,
            "(H \rLLO)",
            vec![
                token(OpenParenthesis, 0),
                token(Word("H".into()), 1),
                token(Whitespace(1, false), 2),
                Err(ParseError::InvalidCharacter('\r', 3)),
                token(Word("LLO".into()), 4),
                token(ClosedParenthesis, 7),
            ],
        ),
    );
}
