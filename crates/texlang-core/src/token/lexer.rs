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

use crate::error;
use crate::state::InputRelatedState;
use crate::token;
use crate::token::catcode::CatCode;
use crate::token::Token;
use std::collections::HashMap;
use std::fmt;
use std::io;
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
}

impl Lexer {
    pub fn next(
        &mut self,
        input_related_state: &mut InputRelatedState,
    ) -> Result<Option<token::Token>, LexerError> {
        while let Some(raw_token) = self.raw_lexer.next(input_related_state.cat_codes())? {
            let c = raw_token.char;
            let value = match raw_token.code {
                CatCode::Escape => Token::new_control_sequence(
                    self.read_control_sequence(&raw_token, input_related_state)?,
                ),
                CatCode::EndOfLine | CatCode::Space => {
                    let num_consumed_new_lines = self
                        .consume_whitespace(input_related_state.cat_codes())?
                        + match raw_token.code == CatCode::EndOfLine {
                            true => 1, // we consumed an additional new line for the first token
                            false => 0,
                        };
                    match (num_consumed_new_lines < 2, self.trim_next_whitespace) {
                        (true, true) => {
                            continue;
                        }
                        (true, false) => Token::new_space(raw_token.char),
                        (false, _) => Token::new_control_sequence(
                            input_related_state
                                .cs_name_interner_mut()
                                .get_or_intern("par"),
                        ),
                    }
                }
                CatCode::BeginGroup => Token::new_begin_group(c),
                CatCode::EndGroup => Token::new_end_group(c),
                CatCode::MathShift => Token::new_math_shift(c),
                CatCode::AlignmentTab => Token::new_alignment_tab(c),
                CatCode::Parameter => Token::new_parameter(c),
                CatCode::Superscript => Token::new_superscript(c),
                CatCode::Subscript => Token::new_subscript(c),
                CatCode::Letter => Token::new_letter(c),
                CatCode::Other => Token::new_other(c),
                CatCode::Active => Token::new_active_character(c),
                CatCode::Comment => {
                    while let Some(next_raw_token) =
                        self.raw_lexer.peek(input_related_state.cat_codes())?
                    {
                        if next_raw_token.code == CatCode::EndOfLine {
                            break;
                        }
                        self.raw_lexer.advance();
                    }
                    self.trim_next_whitespace = true;
                    continue;
                }
                CatCode::Ignored => {
                    continue;
                }
                CatCode::Invalid => return Err(LexerError::InvalidToken),
            };
            self.trim_next_whitespace = matches!(value.value(), token::Value::ControlSequence(..));
            return Ok(Some(value));
        }
        Ok(None)
    }

    fn consume_whitespace(&mut self, map: &HashMap<u32, CatCode>) -> Result<usize, LexerError> {
        let mut num_new_lines: usize = 0;
        while let Some(RawToken { code, .. }) = self.raw_lexer.peek(map)? {
            num_new_lines += match code {
                CatCode::EndOfLine => 1,
                CatCode::Space => 0,
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
        input_related_state: &mut InputRelatedState,
    ) -> Result<token::CsName, LexerError> {
        let name = match self.raw_lexer.next(input_related_state.cat_codes())? {
            None => {
                return Err(LexerError::MalformedControlSequence(
                    error::TokenError::new(
                        token::Token::new_other(raw_token.char),
                        MALFORMED_CONTROL_SEQUENCE_ERROR_TITLE,
                    )
                    .add_note(MALFORMED_CONTROL_SEQUENCE_ERROR_HELP)
                    .cast(),
                ));
            }
            Some(RawToken {
                char,
                code: CatCode::Letter,
                ..
            }) => {
                // _____  _____  ____   _____
                //   |    |   |  |   \  |   |  | | |
                //   |    |   |  |   |  |   |  | | |
                //   |    |___|  |___/  |___|  o o o
                //
                // TODO: optimize this! We shouldn't allocate a string. Instead get the underlying &str
                let mut name = String::new();
                name.push(char);
                while let Some(RawToken {
                    char: subsequent_char,
                    code: CatCode::Letter,
                    ..
                }) = self.raw_lexer.peek(input_related_state.cat_codes())?
                {
                    self.raw_lexer.advance();
                    name.push(subsequent_char);
                }
                name
            }
            Some(first_raw_token) => first_raw_token.char.to_string(),
        };
        Ok(input_related_state
            .cs_name_interner_mut()
            .get_or_intern(name))
    }

    pub fn last_non_empty_line(&self) -> Option<Rc<token::Line>> {
        self.raw_lexer.last_non_empty_line.clone()
    }

    pub fn new(file: Box<dyn io::BufRead>) -> Lexer {
        Lexer {
            raw_lexer: RawLexer::new(file),
            trim_next_whitespace: false,
        }
    }
}

struct RawToken {
    code: CatCode,
    char: char,
}

struct RawLexer {
    reader: Box<dyn io::BufRead>,
    last_non_empty_line: Option<Rc<token::Line>>,
    current_line: Rc<token::Line>,
    current_line_as_chars: Vec<char>,
    next_char_index: usize,
}

impl RawLexer {
    fn next(&mut self, map: &HashMap<u32, CatCode>) -> Result<Option<RawToken>, LexerError> {
        let result = self.peek(map);
        self.advance();
        result
    }

    fn advance(&mut self) {
        self.next_char_index += 1;
    }

    fn peek(&mut self, map: &HashMap<u32, CatCode>) -> Result<Option<RawToken>, LexerError> {
        self.fill_buffer()?;
        Ok(self
            .current_line_as_chars
            .get(self.next_char_index)
            .copied()
            .map(|char| RawToken {
                code: match map.get(&(char as u32)) {
                    None => CatCode::Other,
                    Some(&code) => code,
                },
                char,
                /*
                source: token::Source {
                    line: self.current_line.clone(),
                    position: self.next_char_index,
                },
                 */
            }))
    }

    fn fill_buffer(&mut self) -> Result<(), LexerError> {
        if self.next_char_index >= self.current_line_as_chars.len() {
            let mut line = String::new();
            self.reader.read_line(&mut line)?;
            self.current_line_as_chars = line.chars().collect();
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
    use crate::token::catcode;
    use crate::token::catcode::CatCode::*;
    use crate::token::CsNameInterner;
    use crate::token::Value;

    enum TokenValue {
        Character(char, catcode::CatCode),
        ControlSequence(&'static str),
    }
    use TokenValue::Character;
    use TokenValue::ControlSequence;

    impl TokenValue {
        fn convert(self, interner: &mut CsNameInterner) -> Value {
            match self {
                ControlSequence(name) => Value::ControlSequence(interner.get_or_intern(name)),
                Character(c, cat_code) => Value::new(c, cat_code),
            }
        }
    }

    macro_rules! lexer_test {
        ( $name: ident, $input: expr, $ ( $expected_token : expr, ) * ) => {
            #[test]
            fn $name() {
                let f = Box::new(io::Cursor::new($input.to_string()));
                let mut lexer = Lexer::new(f);
                let mut map = catcode::tex_defaults();
                map.insert('X' as u32, EndOfLine);
                map.insert('Y' as u32, Space);
                map.insert('Z' as u32, Ignored);
                let mut input_related_state = InputRelatedState::new(map);
                let mut actual = Vec::new();
                while let Some(t) = lexer.next(&mut input_related_state).unwrap() {
                    actual.push(t.value);
                }
                let expected: Vec<Value> = vec![$ ( $expected_token.convert(input_related_state.cs_name_interner_mut()) ) , * ];
                assert_eq!(expected, actual);
            }
        };
    }

    lexer_test![
        case_1,
        r"\a{b}",
        ControlSequence("a"),
        Character('{', BeginGroup),
        Character('b', Letter),
        Character('}', EndGroup),
    ];

    lexer_test![
        case_2,
        r"\a b",
        ControlSequence("a"),
        Character('b', Letter),
    ];

    lexer_test![
        case_3,
        "\\a  b",
        ControlSequence("a"),
        Character('b', Letter),
    ];

    lexer_test![
        case_4,
        "\\a\n b",
        ControlSequence("a"),
        Character('b', Letter),
    ];

    lexer_test![
        case_5,
        "\\ABC{D}",
        ControlSequence("ABC"),
        Character('{', BeginGroup),
        Character('D', Letter),
        Character('}', EndGroup),
    ];

    lexer_test![
        multi_character_control_sequence,
        "\\ABC",
        ControlSequence("ABC"),
    ];

    lexer_test![
        single_non_letter_character_control_sequence,
        "\\{{",
        ControlSequence("{"),
        Character('{', BeginGroup),
    ];

    lexer_test![
        single_non_letter_character_control_sequence_followed_by_letter,
        "\\{A",
        ControlSequence("{"),
        Character('A', Letter),
    ];

    lexer_test![
        case_8,
        "A%a comment here\nC",
        Character('A', Letter),
        Character('C', Letter),
    ];

    lexer_test![
        case_9,
        "A%a comment here\n%A second comment\nC",
        Character('A', Letter),
        Character('C', Letter),
    ];

    lexer_test![case_10, "A%a comment here", Character('A', Letter),];

    lexer_test![
        case_11,
        "A%\n B",
        Character('A', Letter),
        Character('B', Letter),
    ];

    lexer_test![
        case_12,
        "A%\n\n B",
        Character('A', Letter),
        ControlSequence("par"),
        Character('B', Letter),
    ];

    lexer_test![
        case_13,
        "\\A %\nB",
        ControlSequence("A"),
        Character('B', Letter),
    ];

    lexer_test![
        double_space_creates_one_space,
        "A  B",
        Character('A', Letter),
        Character(' ', Space),
        Character('B', Letter),
    ];

    lexer_test![
        single_newline_creates_one_space,
        "A\nB",
        Character('A', Letter),
        Character('\n', Space),
        Character('B', Letter),
    ];

    lexer_test![
        space_and_newline_creates_space,
        "A \nB",
        Character('A', Letter),
        Character(' ', Space),
        Character('B', Letter),
    ];

    lexer_test![
        double_newline_creates_par,
        "A\n\nB",
        Character('A', Letter),
        ControlSequence("par"),
        Character('B', Letter),
    ];

    lexer_test![
        newline_space_newline_creates_par,
        "A\n \nB",
        Character('A', Letter),
        ControlSequence("par"),
        Character('B', Letter),
    ];

    lexer_test![
        non_standard_whitespace_character,
        "AYB",
        Character('A', Letter),
        Character('Y', Space),
        Character('B', Letter),
    ];

    lexer_test![
        non_standard_newline_character,
        "AXB",
        Character('A', Letter),
        Character('X', Space),
        Character('B', Letter),
    ];

    lexer_test![single_ignored_character, "Z",];
}
