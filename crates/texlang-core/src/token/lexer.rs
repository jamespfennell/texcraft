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
use crate::token;
use crate::token::catcode::CatCode;
use crate::token::catcode::CatCodeMap;
use crate::token::trace;
use crate::token::CsNameInterner;
use crate::token::Token;
use std::fmt;
use texcraft_stdext::str::OwningChars;

const MALFORMED_CONTROL_SEQUENCE_ERROR_TITLE: &str = "Unexpected end of file";
const MALFORMED_CONTROL_SEQUENCE_ERROR_HELP: &str =
    "expected the escape character to be followed by the name of a control sequence";

#[derive(Debug)]
pub enum LexerError {
    MalformedControlSequence(anyhow::Error),
    InvalidToken,
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "LEXER ERROR")
    }
}

impl std::error::Error for LexerError {}

/// The Texlang lexer
pub struct Lexer {
    raw_lexer: RawLexer,
    trim_next_whitespace: bool,
    // We read control sequence names into a shared buffer to avoid allocating for each one.
    buffer: String,
}

impl Lexer {
    pub fn new(source_code: String, trace_key_range: trace::KeyRange) -> Lexer {
        Lexer {
            raw_lexer: RawLexer::new(source_code, trace_key_range),
            trim_next_whitespace: false,
            buffer: Default::default(),
        }
    }

    pub fn next(
        &mut self,
        cat_code_map: &CatCodeMap,
        cs_name_interner: &mut CsNameInterner,
    ) -> Result<Option<token::Token>, LexerError> {
        while let Some(raw_token) = self.raw_lexer.next(cat_code_map) {
            let c = raw_token.char;
            let value = match raw_token.code {
                CatCode::Escape => Token::new_control_sequence(
                    self.read_control_sequence(&raw_token, cat_code_map, cs_name_interner)?,
                    raw_token.trace_key,
                ),
                CatCode::EndOfLine | CatCode::Space => {
                    let num_consumed_new_lines = self.consume_whitespace(cat_code_map)
                        + match raw_token.code == CatCode::EndOfLine {
                            true => 1, // we consumed an additional new line for the first token
                            false => 0,
                        };
                    match (num_consumed_new_lines < 2, self.trim_next_whitespace) {
                        (true, true) => {
                            continue;
                        }
                        (true, false) => Token::new_space(raw_token.char, raw_token.trace_key),
                        (false, _) => Token::new_control_sequence(
                            cs_name_interner.get_or_intern("par"),
                            raw_token.trace_key,
                        ),
                    }
                }
                CatCode::BeginGroup => Token::new_begin_group(c, raw_token.trace_key),
                CatCode::EndGroup => Token::new_end_group(c, raw_token.trace_key),
                CatCode::MathShift => Token::new_math_shift(c, raw_token.trace_key),
                CatCode::AlignmentTab => Token::new_alignment_tab(c, raw_token.trace_key),
                CatCode::Parameter => Token::new_parameter(c, raw_token.trace_key),
                CatCode::Superscript => Token::new_superscript(c, raw_token.trace_key),
                CatCode::Subscript => Token::new_subscript(c, raw_token.trace_key),
                CatCode::Letter => Token::new_letter(c, raw_token.trace_key),
                CatCode::Other => Token::new_other(c, raw_token.trace_key),
                CatCode::Active => Token::new_active_character(c, raw_token.trace_key),
                CatCode::Comment => {
                    while let Some(next_raw_token) = self.raw_lexer.peek(cat_code_map) {
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

    fn consume_whitespace(&mut self, map: &CatCodeMap) -> usize {
        let mut num_new_lines: usize = 0;
        while let Some(RawToken { code, .. }) = self.raw_lexer.peek(map) {
            num_new_lines += match code {
                CatCode::EndOfLine => 1,
                CatCode::Space => 0,
                _ => {
                    break;
                }
            };
            self.raw_lexer.advance();
        }
        num_new_lines
    }

    fn read_control_sequence(
        &mut self,
        raw_token: &RawToken,
        cat_code_map: &CatCodeMap,
        cs_name_interner: &mut CsNameInterner,
    ) -> Result<token::CsName, LexerError> {
        self.buffer.clear();
        match self.raw_lexer.next(cat_code_map) {
            None => {
                // TODO: this should be an end of input errro
                return Err(LexerError::MalformedControlSequence(
                    error::TokenError::new(
                        token::Token::new_other(raw_token.char, raw_token.trace_key),
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
                self.buffer.push(char);
                while let Some(RawToken {
                    char: subsequent_char,
                    code: CatCode::Letter,
                    ..
                }) = self.raw_lexer.peek(cat_code_map)
                {
                    self.raw_lexer.advance();
                    self.buffer.push(subsequent_char);
                }
            }
            Some(first_raw_token) => {
                self.buffer.push(first_raw_token.char);
            }
        };
        Ok(cs_name_interner.get_or_intern(&self.buffer))
    }

    pub fn source_code(&self) -> &str {
        self.raw_lexer.iter.str()
    }
}

struct RawToken {
    code: CatCode,
    char: char,
    trace_key: trace::Key,
}

struct RawLexer {
    iter: OwningChars,
    trace_key_range: trace::KeyRange,
}

impl RawLexer {
    pub fn new(source_code: String, trace_key_range: trace::KeyRange) -> RawLexer {
        RawLexer {
            iter: OwningChars::new(source_code),
            trace_key_range,
        }
    }

    fn next(&mut self, map: &CatCodeMap) -> Option<RawToken> {
        match self.iter.next() {
            Some(c) => {
                let code = *map.get(&c);
                Some(RawToken {
                    char: c,
                    code,
                    trace_key: self.trace_key_range.next(),
                })
            }
            None => None,
        }
    }

    fn peek(&mut self, map: &CatCodeMap) -> Option<RawToken> {
        match self.iter.peek() {
            Some(c) => {
                let code = *map.get(&c);
                Some(RawToken {
                    char: c,
                    code,
                    trace_key: self.trace_key_range.peek(),
                })
            }
            None => None,
        }
    }

    fn advance(&mut self) {
        self.iter.next();
        self.trace_key_range.next();
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
                let mut lexer = Lexer::new($input.to_string(), trace::KeyRange::for_testing());
                let mut map = CatCodeMap::new_with_tex_defaults();
                map.insert('X', EndOfLine);
                map.insert('Y', Space);
                map.insert('Z', Ignored);
                let mut cs_name_interner = CsNameInterner::new();
                let mut actual = Vec::new();
                while let Some(t) = lexer.next(&map, &mut cs_name_interner).unwrap() {
                    actual.push(t.value);
                }
                let expected: Vec<Value> = vec![$ ( $expected_token.convert(&mut cs_name_interner) ) , * ];
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
