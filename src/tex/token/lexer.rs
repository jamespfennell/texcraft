//! The TeX lexer, which reads input streams of characters and outputs TeX tokens.
//!
//! Because of restrictions of the TeX language itself, the lexer is "just in time". It only
//! produces the next token when that token is requested. In general, it is an error to request
//! many TeX tokens and process them as batch. This is because lexing in TeX is controlled by
//! catcodes which can dynamically change at runtime based on the results of the lexer. Let's
//! consider a TeX snippet and assume that the catcode mappings are at their default:
//! ```tex
//! \change_catcode_of_A_to_whitespace AB
//! ```
//! If tokenized as a batch, the lexer will return a control sequence `\change_...`, and two
//! letter tokens A and B. However the control sequence itself changes the letter A to be a
//! whitespace character, and so the lexer must in fact trim it away as part of trimming all
//! whitespace characters after a control sequence. The correct result is thus the control sequence
//! followed by the single letter token B.

use crate::tex::error;
use crate::tex::token;
use crate::tex::token::catcode::{CatCode, RawCatCode};
use std::collections::HashMap;
use std::fmt;
use std::io;
use std::iter::FromIterator;
use std::rc::Rc;

const MALFORMED_CONTROL_SEQUENCE_ERROR_TITLE: &str = "Unexpected end of file";
const MALFORMED_CONTROL_SEQUENCE_ERROR_HELP: &str =
    "expected the escape character to be followed by the name of a control sequence";

#[derive(Debug)]
pub enum LexerError {
    MalformedControlSequence(anyhow::Error),
    InvalidToken,
    IO(io::Error),
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "LEXER ERROR")
    }
}

impl std::error::Error for LexerError {}

impl From<io::Error> for LexerError {
    fn from(io_error: std::io::Error) -> Self {
        LexerError::IO(io_error)
    }
}

/// The Lexer...
pub struct Lexer {
    raw_lexer: RawLexer,
    trim_next_whitespace: bool,
    new_par_control_sequence_name: String,
}

impl Lexer {
    pub fn next(
        &mut self,
        map: &HashMap<u32, RawCatCode>,
    ) -> Result<Option<token::Token>, LexerError> {
        while let Some(raw_token) = self.raw_lexer.next(map)? {
            let value = match raw_token.code {
                RawCatCode::Escape => self.read_control_sequence(&raw_token, map)?,
                RawCatCode::EndOfLine | RawCatCode::Regular(CatCode::Space) => {
                    let num_consumed_new_lines = self.consume_whitespace(map)?
                        + match raw_token.code == RawCatCode::EndOfLine {
                            true => 1, // we consumed an additional new line for the first token
                            false => 0,
                        };
                    match (num_consumed_new_lines < 2, self.trim_next_whitespace) {
                        (true, true) => {
                            continue;
                        }
                        (true, false) => token::Value::Character(raw_token.char, CatCode::Space),
                        (false, _) => token::Value::ControlSequence(
                            '\\',
                            self.new_par_control_sequence_name.clone(),
                        ),
                    }
                }
                RawCatCode::Regular(code) => token::Value::Character(raw_token.char, code),
                RawCatCode::Comment => {
                    while let Some(next_raw_token) = self.raw_lexer.peek(map)? {
                        if next_raw_token.code == RawCatCode::EndOfLine {
                            break;
                        }
                        self.raw_lexer.advance();
                    }
                    self.trim_next_whitespace = true;
                    continue;
                }
                RawCatCode::Ignored => {
                    continue;
                }
                RawCatCode::Invalid => return Err(LexerError::InvalidToken),
            };
            self.trim_next_whitespace = matches!(value, token::Value::ControlSequence(..));
            return Ok(Some(token::Token {
                value,
                source: Some(raw_token.source),
            }));
        }
        Ok(None)
    }

    fn consume_whitespace(&mut self, map: &HashMap<u32, RawCatCode>) -> Result<usize, LexerError> {
        let mut num_new_lines: usize = 0;
        while let Some(RawToken { code, .. }) = self.raw_lexer.peek(map)? {
            num_new_lines += match code {
                RawCatCode::EndOfLine => 1,
                RawCatCode::Regular(CatCode::Space) => 0,
                _ => {
                    break;
                }
            };
            self.raw_lexer.advance();
        }
        Ok(num_new_lines)
    }

    fn read_control_sequence(
        &mut self,
        raw_token: &RawToken,
        map: &HashMap<u32, RawCatCode>,
    ) -> Result<token::Value, LexerError> {
        let name = match self.raw_lexer.next(map)? {
            None => {
                return Err(LexerError::MalformedControlSequence(
                    error::TokenError::new(
                        token::Token {
                            value: token::Value::Character(raw_token.char, CatCode::Other),
                            source: Some(raw_token.source.clone()),
                        },
                        MALFORMED_CONTROL_SEQUENCE_ERROR_TITLE,
                    )
                    .add_note(MALFORMED_CONTROL_SEQUENCE_ERROR_HELP)
                    .cast(),
                ));
            }
            Some(RawToken {
                char,
                code: RawCatCode::Regular(CatCode::Letter),
                ..
            }) => {
                let mut name = String::new();
                name.push(char);
                while let Some(RawToken {
                    char: subsequent_char,
                    code: RawCatCode::Regular(CatCode::Letter),
                    ..
                }) = self.raw_lexer.peek(map)?
                {
                    self.raw_lexer.advance();
                    name.push(subsequent_char);
                }
                name
            }
            Some(first_raw_token) => first_raw_token.char.to_string(),
        };
        Ok(token::Value::ControlSequence(raw_token.char, name))
    }

    pub fn last_non_empty_line(&self) -> Option<Rc<token::Line>> {
        self.raw_lexer.last_non_empty_line.clone()
    }

    pub fn new(file: Box<dyn io::BufRead>) -> Lexer {
        Lexer {
            raw_lexer: RawLexer::new(file),
            trim_next_whitespace: false,
            new_par_control_sequence_name: "par".to_string(),
        }
    }
}

struct RawToken {
    code: RawCatCode,
    char: char,
    source: token::Source,
}

struct RawLexer {
    reader: Box<dyn io::BufRead>,
    last_non_empty_line: Option<Rc<token::Line>>,
    current_line: Rc<token::Line>,
    current_line_as_chars: Vec<char>,
    next_char_index: usize,
}

impl RawLexer {
    fn next(&mut self, map: &HashMap<u32, RawCatCode>) -> Result<Option<RawToken>, LexerError> {
        let result = self.peek(map);
        self.advance();
        result
    }

    fn advance(&mut self) {
        self.next_char_index += 1;
    }

    fn peek(&mut self, map: &HashMap<u32, RawCatCode>) -> Result<Option<RawToken>, LexerError> {
        self.fill_buffer()?;
        Ok(self
            .current_line_as_chars
            .get(self.next_char_index)
            .copied()
            .map(|char| RawToken {
                code: match map.get(&(char as u32)) {
                    None => RawCatCode::Regular(CatCode::Other),
                    Some(&code) => code,
                },
                char,
                source: token::Source {
                    line: self.current_line.clone(),
                    position: self.next_char_index,
                },
            }))
    }

    fn fill_buffer(&mut self) -> Result<(), LexerError> {
        if self.next_char_index >= self.current_line_as_chars.len() {
            let mut line = String::new();
            self.reader.read_line(&mut line)?;
            self.current_line_as_chars = Vec::from_iter(line.chars());
            self.next_char_index = 0;
            self.current_line = Rc::new(token::Line {
                content: line,
                line_number: self.current_line.line_number + 1,
                file: self.current_line.file.clone(),
            });
            if self.last_non_empty_line.is_none() || !self.current_line.content.is_empty() {
                self.last_non_empty_line = Some(self.current_line.clone());
            };
        }
        Ok(())
    }

    pub fn new(file: Box<dyn io::BufRead>) -> RawLexer {
        RawLexer {
            reader: file,
            current_line_as_chars: Vec::new(),
            next_char_index: 0,
            last_non_empty_line: None,
            current_line: Rc::new(token::Line {
                content: "".to_string(),
                line_number: 0,
                file: Rc::new("".to_string()),
            }),
        }
    }
}

// what about the TeX edge case \input{file}b where file ends in \a. Do as \ab control sequence
// get created? If so, can't isolate inputs behind an expansion runner
#[cfg(test)]
mod tests {
    use super::*;
    use crate::tex::token::catcode;
    use crate::tex::token::catcode::{CatCode::*, RawCatCode::*};
    use crate::tex::token::Value::Character;
    use crate::tex::token::Value::ControlSequence;
    use std::array::IntoIter;

    #[test]
    fn case_1() {
        run_test(
            r"\a{b}",
            Vec::from_iter(IntoIter::new([
                ControlSequence('\\', "a".to_string()),
                Character('{', BeginGroup),
                Character('b', Letter),
                Character('}', EndGroup),
            ])),
        );
    }

    #[test]
    fn case_2() {
        run_test(
            r"\a b",
            Vec::from_iter(IntoIter::new([
                ControlSequence('\\', "a".to_string()),
                Character('b', Letter),
            ])),
        );
    }
    #[test]
    fn case_3() {
        run_test(
            "\\a  b",
            Vec::from_iter(IntoIter::new([
                ControlSequence('\\', "a".to_string()),
                Character('b', Letter),
            ])),
        );
    }
    #[test]
    fn case_4() {
        run_test(
            "\\a\n b",
            Vec::from_iter(IntoIter::new([
                ControlSequence('\\', "a".to_string()),
                Character('b', Letter),
            ])),
        );
    }
    #[test]
    fn case_5() {
        run_test(
            "\\ABC{D}",
            Vec::from_iter(IntoIter::new([
                ControlSequence('\\', "ABC".to_string()),
                Character('{', BeginGroup),
                Character('D', Letter),
                Character('}', EndGroup),
            ])),
        );
    }
    #[test]
    fn multi_character_control_sequence() {
        run_test(
            "\\ABC",
            Vec::from_iter(IntoIter::new([ControlSequence('\\', "ABC".to_string())])),
        );
    }
    #[test]
    fn single_non_letter_character_control_sequence() {
        run_test(
            "\\{{",
            Vec::from_iter(IntoIter::new([
                ControlSequence('\\', "{".to_string()),
                Character('{', BeginGroup),
            ])),
        );
    }

    #[test]
    fn single_non_letter_character_control_sequence_followed_by_letter() {
        run_test(
            "\\{A",
            Vec::from_iter(IntoIter::new([
                ControlSequence('\\', "{".to_string()),
                Character('A', Letter),
            ])),
        );
    }

    #[test]
    fn case_8() {
        run_test(
            "A%a comment here\nC",
            Vec::from_iter(IntoIter::new([
                Character('A', Letter),
                Character('C', Letter),
            ])),
        );
    }
    #[test]
    fn case_9() {
        run_test(
            "A%a comment here\n%A second comment\nC",
            Vec::from_iter(IntoIter::new([
                Character('A', Letter),
                Character('C', Letter),
            ])),
        );
    }
    #[test]
    fn case_10() {
        run_test(
            "A%a comment here",
            Vec::from_iter(IntoIter::new([Character('A', Letter)])),
        );
    }
    #[test]
    fn case_11() {
        run_test(
            "A%\n B",
            Vec::from_iter(IntoIter::new([
                Character('A', Letter),
                Character('B', Letter),
            ])),
        );
    }
    #[test]
    fn case_12() {
        run_test(
            "A%\n\n B",
            Vec::from_iter(IntoIter::new([
                Character('A', Letter),
                ControlSequence('\\', "par".to_string()),
                Character('B', Letter),
            ])),
        );
    }
    #[test]
    fn case_13() {
        run_test(
            "\\A %\nB",
            Vec::from_iter(IntoIter::new([
                ControlSequence('\\', "A".to_string()),
                Character('B', Letter),
            ])),
        );
    }
    #[test]
    fn double_space_creates_one_space() {
        run_test(
            "A  B",
            Vec::from_iter(IntoIter::new([
                Character('A', Letter),
                Character(' ', Space),
                Character('B', Letter),
            ])),
        );
    }
    #[test]
    fn single_newline_creates_one_space() {
        run_test(
            "A\nB",
            Vec::from_iter(IntoIter::new([
                Character('A', Letter),
                Character('\n', Space),
                Character('B', Letter),
            ])),
        );
    }
    #[test]
    fn space_and_newline_creates_space() {
        run_test(
            "A \nB",
            Vec::from_iter(IntoIter::new([
                Character('A', Letter),
                Character(' ', Space),
                Character('B', Letter),
            ])),
        );
    }
    #[test]
    fn double_newline_creates_par() {
        run_test(
            "A\n\nB",
            Vec::from_iter(IntoIter::new([
                Character('A', Letter),
                ControlSequence('\\', "par".to_string()),
                Character('B', Letter),
            ])),
        );
    }
    #[test]
    fn newline_space_newline_creates_par() {
        run_test(
            "A\n \nB",
            Vec::from_iter(IntoIter::new([
                Character('A', Letter),
                ControlSequence('\\', "par".to_string()),
                Character('B', Letter),
            ])),
        );
    }

    #[test]
    fn non_standard_whitespace_character() {
        run_test(
            "AYB",
            Vec::from_iter(IntoIter::new([
                Character('A', Letter),
                Character('Y', Space),
                Character('B', Letter),
            ])),
        );
    }

    #[test]
    fn non_standard_newline_character() {
        run_test(
            "AXB",
            Vec::from_iter(IntoIter::new([
                Character('A', Letter),
                Character('X', Space),
                Character('B', Letter),
            ])),
        );
    }

    #[test]
    fn single_ignored_character() {
        run_test("Z", Vec::new());
    }

    fn run_test(input: &str, expected: Vec<token::Value>) {
        let f = Box::new(io::Cursor::new(input.to_string()));
        let mut lexer = Lexer::new(f);
        let mut map = catcode::tex_defaults();
        map.insert('X' as u32, EndOfLine);
        map.insert('Y' as u32, Regular(Space));
        map.insert('Z' as u32, Ignored);
        let mut actual = Vec::new();
        while let Some(t) = lexer.next(&map).unwrap() {
            actual.push(t.value);
        }
        assert_eq!(expected, actual);
    }
}
