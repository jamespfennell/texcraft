//! The TeX lexer, which reads input streams of characters and outputs TeX tokens.
//!
//! Because of restrictions of the TeX language itself, the lexer is "just in time". It only
//! produces the next token when that token is requested. In general, it is an error to request
//! many TeX tokens and process them as batch. This is because lexing in TeX is controlled by
//! cat codes which can dynamically change at runtime based on the results of the lexer. Let's
//! consider a TeX snippet and assume that the catcode mappings are at their default:
//! ```tex
//! \change_catcode_of_A_to_whitespace AB
//! ```
//! If tokenized as a batch, the lexer will return a control sequence `\change_...`, and two
//! letter tokens A and B. However the control sequence itself changes the letter A to be a
//! whitespace character, and so the lexer must in fact trim it away as part of trimming all
//! whitespace characters after a control sequence. The correct result is thus the control sequence
//! followed by the single letter token B.

use crate::token;
use crate::token::catcode::CatCode;
use crate::token::trace;
use crate::token::CsNameInterner;
use crate::token::Token;

#[derive(Debug)]
pub(crate) enum Error {
    InvalidCharacter(char, trace::Key),
    EmptyControlSequence(trace::Key),
}

pub trait CatCodeFn {
    fn cat_code(&self, c: char) -> CatCode;
}

impl CatCodeFn for std::collections::HashMap<char, CatCode> {
    fn cat_code(&self, c: char) -> CatCode {
        self.get(&c).copied().unwrap_or_default()
    }
}

/// The Texlang lexer
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Lexer {
    raw_lexer: RawLexer,
    trim_next_whitespace: bool,
    // We read control sequence names into a shared buffer to avoid allocating for each one.
    #[cfg_attr(feature = "serde", serde(skip))]
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

    pub(crate) fn next<F: CatCodeFn>(
        &mut self,
        cat_code_fn: &F,
        cs_name_interner: &mut CsNameInterner,
    ) -> Result<Option<token::Token>, Error> {
        while let Some(raw_token) = self.raw_lexer.next(cat_code_fn) {
            let c = raw_token.char;
            let value = match raw_token.code {
                CatCode::Escape => Token::new_control_sequence(
                    self.read_control_sequence(&raw_token, cat_code_fn, cs_name_interner)?,
                    raw_token.trace_key,
                ),
                CatCode::EndOfLine | CatCode::Space => {
                    let num_consumed_new_lines = self.consume_whitespace(cat_code_fn)
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
                CatCode::Superscript => {
                    if self
                        .raw_lexer
                        .maybe_apply_caret_notation(raw_token.char, true)
                    {
                        continue;
                    }
                    Token::new_superscript(c, raw_token.trace_key)
                }
                CatCode::Subscript => Token::new_subscript(c, raw_token.trace_key),
                CatCode::Letter => Token::new_letter(c, raw_token.trace_key),
                CatCode::Other => Token::new_other(c, raw_token.trace_key),
                CatCode::Active => Token::new_active_character(c, raw_token.trace_key),
                CatCode::Comment => {
                    while let Some(next_raw_token) = self.raw_lexer.peek(cat_code_fn) {
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
                CatCode::Invalid => return Err(Error::InvalidCharacter(c, raw_token.trace_key)),
            };
            self.trim_next_whitespace = matches!(value.value(), token::Value::ControlSequence(..));
            return Ok(Some(value));
        }
        Ok(None)
    }

    fn consume_whitespace<F: CatCodeFn>(&mut self, cat_code_fn: &F) -> usize {
        let mut num_new_lines: usize = 0;
        while let Some(RawToken { code, .. }) = self.raw_lexer.peek(cat_code_fn) {
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

    fn read_control_sequence<F: CatCodeFn>(
        &mut self,
        raw_token: &RawToken,
        cat_code_fn: &F,
        cs_name_interner: &mut CsNameInterner,
    ) -> Result<token::CsName, Error> {
        self.buffer.clear();
        let first_raw_token = match self.raw_lexer.next(cat_code_fn) {
            None => return Err(Error::EmptyControlSequence(raw_token.trace_key)),
            Some(first_raw_token) => first_raw_token,
        };
        match first_raw_token.code {
            CatCode::Letter => {
                self.buffer.push(first_raw_token.char);
                while let Some(raw_token) = self.raw_lexer.peek(cat_code_fn) {
                    match raw_token.code {
                        CatCode::Letter => {
                            self.raw_lexer.advance();
                            self.buffer.push(raw_token.char);
                        }
                        CatCode::Superscript => {
                            if self
                                .raw_lexer
                                .maybe_apply_caret_notation(raw_token.char, false)
                            {
                                continue;
                            }
                            break;
                        }
                        _ => break,
                    }
                }
            }
            CatCode::Superscript => {
                if self
                    .raw_lexer
                    .maybe_apply_caret_notation(first_raw_token.char, true)
                {
                    return self.read_control_sequence(raw_token, cat_code_fn, cs_name_interner);
                }
                self.buffer.push(first_raw_token.char);
            }
            _ => {
                self.buffer.push(first_raw_token.char);
            }
        };
        Ok(cs_name_interner.get_or_intern(&self.buffer))
    }
}

struct RawToken {
    code: CatCode,
    char: char,
    trace_key: trace::Key,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
struct RawLexer {
    // TODO: when serializing we only need to serialize self.source_code[self.pos..]
    source_code: String,
    pos: usize,
    trace_key_range: trace::KeyRange,
}

impl RawLexer {
    pub fn new(source_code: String, trace_key_range: trace::KeyRange) -> RawLexer {
        RawLexer {
            source_code,
            pos: 0,
            trace_key_range,
        }
    }

    fn next<F: CatCodeFn>(&mut self, cat_code_fn: &F) -> Option<RawToken> {
        match self.source_code[self.pos..].chars().next() {
            Some(c) => {
                self.pos += c.len_utf8();
                let code = cat_code_fn.cat_code(c);
                Some(RawToken {
                    char: c,
                    code,
                    trace_key: self.trace_key_range.next(),
                })
            }
            None => None,
        }
    }

    fn peek<F: CatCodeFn>(&mut self, cat_code_fn: &F) -> Option<RawToken> {
        match self.source_code[self.pos..].chars().next() {
            Some(c) => {
                let code = cat_code_fn.cat_code(c);
                Some(RawToken {
                    char: c,
                    code,
                    trace_key: self.trace_key_range.peek(),
                })
            }
            None => None,
        }
    }

    fn maybe_apply_caret_notation(
        &mut self,
        char_1: char,
        char_1_consumed: bool,
    ) -> bool {
        let char_2_start = if char_1_consumed {
            self.pos
        } else {
            self.pos + char_1.len_utf8()
        };
        let char_2 = match self.source_code[char_2_start..].chars().next() {
            None => return false,
            Some(next_char) => next_char,
        };
        if char_2 != char_1 {
            return false;
        }
        let char_3_start = char_2_start+char_2.len_utf8();
        let char_3 = match self.source_code[char_3_start..].chars().next() {
            // If the input is over, don't transform. This is what TeX does; see
            // the TeXBook section 355 and related sections.
            None => return false,
            Some(c) => c,
        };
        if !char_1_consumed {
            self.advance();
        }
        self.advance();
        if !char_3.is_ascii() {
            return true;
        }
        let u: u8 = match (char_3 as u32).try_into() {
            Ok(u) => u,
            Err(_) => return true, // unreachable because char_3 is ASCII
        };
        let m = match u {
            0x00..=0x3F => u + 0x40,
            0x40..=0x7F => u - 0x40,
            _ => return true, // unreachable because char_3 is ASCII
        };
        // Given the way `m` is calculated, we're guaranteed that it is ASCII. However,
        // this assert is to have defense-in-depth in anticipation of the unsafe block next.
        assert!(char::from_u32(m as u32).unwrap().is_ascii());
        // SAFETY: the original character `char_3` is single-byte/ASCII and
        // the replacement character `m` is single-byte/ASCII so the replacement
        // preserves the UTF-8 structure of the string.
        unsafe {
            self.source_code.as_bytes_mut()[self.pos] = m;
        }
        true
    }

    fn advance(&mut self) {
        if let Some(c) = self.source_code[self.pos..].chars().next() {
            self.pos += c.len_utf8();
        }
        self.trace_key_range.next();
    }
}

// TODO: in TeX each line in a file is read in separately, spaces at the end are trimmed,
// and the character in \endlinechar is appended. See section 362 of the TeXBook and also
// the implementation of `input_ln` in the TeXBook. We should be doing that, too.
#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::catcode;
    use crate::token::catcode::CatCode::*;
    use crate::token::CsNameInterner;
    use crate::token::Value;
    use std::collections::HashMap;

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

    fn lexer_test(input: &str, expected_tokens: Vec<TokenValue>) {
        let mut lexer = Lexer::new(input.into(), trace::KeyRange::for_testing());
        let mut map: HashMap<char, CatCode> = CatCode::PLAIN_TEX_DEFAULTS
            .iter()
            .enumerate()
            .map(|(a, b)| (char::from_u32(a.try_into().unwrap()).unwrap(), *b))
            .collect();
        map.insert('X', EndOfLine);
        map.insert('Y', Space);
        map.insert('Z', Ignored);
        let mut cs_name_interner: CsNameInterner = Default::default();
        let mut actual = Vec::new();
        while let Some(t) = lexer.next(&map, &mut cs_name_interner).unwrap() {
            actual.push(t.value);
        }
        let expected: Vec<Value> = expected_tokens
            .into_iter()
            .map(|t| t.convert(&mut cs_name_interner))
            .collect();
        assert_eq!(expected, actual);
    }

    macro_rules! lexer_tests {
        ($( ( $name: ident, $input: expr, $ ( $expected_token : expr, ) * ), )+) => {
            $(
            #[test]
            fn $name() {
                let input = $input;
                let expected_tokens = vec!( $( $expected_token ),* );
                lexer_test(&input, expected_tokens);
            }
            )+
        };
    }

    lexer_tests![
        (
            case_1,
            r"\a{b}",
            ControlSequence("a"),
            Character('{', BeginGroup),
            Character('b', Letter),
            Character('}', EndGroup),
        ),
        (
            case_2,
            r"\a b",
            ControlSequence("a"),
            Character('b', Letter),
        ),
        (
            case_3,
            "\\a  b",
            ControlSequence("a"),
            Character('b', Letter),
        ),
        (
            case_4,
            "\\a\n b",
            ControlSequence("a"),
            Character('b', Letter),
        ),
        (
            case_5,
            "\\ABC{D}",
            ControlSequence("ABC"),
            Character('{', BeginGroup),
            Character('D', Letter),
            Character('}', EndGroup),
        ),
        (
            multi_character_control_sequence,
            "\\ABC",
            ControlSequence("ABC"),
        ),
        (
            single_non_letter_character_control_sequence,
            "\\{{",
            ControlSequence("{"),
            Character('{', BeginGroup),
        ),
        (
            single_non_letter_character_control_sequence_followed_by_letter,
            "\\{A",
            ControlSequence("{"),
            Character('A', Letter),
        ),
        (
            case_8,
            "A%a comment here\nC",
            Character('A', Letter),
            Character('C', Letter),
        ),
        (
            case_9,
            "A%a comment here\n%A second comment\nC",
            Character('A', Letter),
            Character('C', Letter),
        ),
        (case_10, "A%a comment here", Character('A', Letter),),
        (
            case_11,
            "A%\n B",
            Character('A', Letter),
            Character('B', Letter),
        ),
        (
            case_12,
            "A%\n\n B",
            Character('A', Letter),
            ControlSequence("par"),
            Character('B', Letter),
        ),
        (
            case_13,
            "\\A %\nB",
            ControlSequence("A"),
            Character('B', Letter),
        ),
        (case_14, "\\A1", ControlSequence("A"), Character('1', Other),),
        (
            double_space_creates_one_space,
            "A  B",
            Character('A', Letter),
            Character(' ', Space),
            Character('B', Letter),
        ),
        (
            single_newline_creates_one_space,
            "A\nB",
            Character('A', Letter),
            Character('\n', Space),
            Character('B', Letter),
        ),
        (
            space_and_newline_creates_space,
            "A \nB",
            Character('A', Letter),
            Character(' ', Space),
            Character('B', Letter),
        ),
        (
            double_newline_creates_par,
            "A\n\nB",
            Character('A', Letter),
            ControlSequence("par"),
            Character('B', Letter),
        ),
        (
            newline_space_newline_creates_par,
            "A\n \nB",
            Character('A', Letter),
            ControlSequence("par"),
            Character('B', Letter),
        ),
        (
            non_standard_whitespace_character,
            "AYB",
            Character('A', Letter),
            Character('Y', Space),
            Character('B', Letter),
        ),
        (
            non_standard_newline_character,
            "AXB",
            Character('A', Letter),
            Character('X', Space),
            Character('B', Letter),
        ),
        (single_ignored_character, "Z",),
        (double_superscript_1, "^^k", Character('+', Other),),
        (double_superscript_2, "^^+", Character('k', Letter),),
        (double_superscript_3, "^^\n", Character('J', Letter),),
        (
            double_superscript_end_of_input_1,
            "^^",
            Character('^', Superscript),
            Character('^', Superscript),
        ),
        (
            double_superscript_end_of_input_2,
            "\\^^",
            ControlSequence("^"),
            Character('^', Superscript),
        ),
        (
            double_superscript_end_of_input_3,
            "\\a^^",
            ControlSequence("a"),
            Character('^', Superscript),
            Character('^', Superscript),
        ),
        (
            double_superscript_boundary_1,
            "^^\u{00}",
            Character(char::from_u32(0x40).unwrap(), Other),
        ),
        (
            double_superscript_boundary_3,
            "^^\u{40}",
            // skipped character
        ),
        (
            double_superscript_boundary_4,
            "^^\u{7F}",
            Character(char::from_u32(0x3F).unwrap(), Other),
        ),
        (double_superscript_cs_1, "\\^^m", ControlSequence("-"),),
        (
            double_superscript_cs_2,
            "\\^^ma",
            ControlSequence("-"),
            Character('a', Letter),
        ),
        (double_superscript_cs_3, "\\^^-", ControlSequence("m"),),
        (double_superscript_cs_4, "\\^^-a", ControlSequence("ma"),),
        (double_superscript_cs_5, "\\^^-^^-", ControlSequence("mm"),),
        (double_superscript_cs_6, "\\a^^-", ControlSequence("am"),),
        (
            double_superscript_cs_7,
            "\\^a",
            ControlSequence("^"),
            Character('a', Letter),
        ),
        (
            double_superscript_cs_8,
            "\\a^a",
            ControlSequence("a"),
            Character('^', Superscript),
            Character('a', Letter),
        ),
    ];
}
