//! The Texlang lexer
//!
//! This module contains Texlang's TeX lexer, which converts TeX source code into TeX tokens.
//!
//! ## Just-in-time lexing
//!
//! A TeX lexer is different to most other lexers because TeX's lexing rules
//!     are dynamic and can be changed from within TeX source code.
//! In particular, TeX has the following primitives which can change the lexing rules:
//!
//! - The `\catcode` primitive is used to change the category code
//!     that is applied to each character in the input, and thus the kind
//!     of token that each character gets converted to.
//!
//! - The `\endlinechar` primitive is used to change the character
//!     that TeX appends to each line in the input file.
//!     TeX appends this character after stripping the new line characters (`\n` or `\r\n`)
//!     and any trailing space characters (` `, ASCII code 32) from the end of the line.
//!
//! The most important implication of TeX having dynamic lexing rules is that
//!     the lexer is lazy, or "just in time".
//! One cannot, in general, run the lexer for a entire input file to obtain a list of tokens.
//! Instead one must request new tokens just as they are needed.
//! Here is an example of TeX source code that relies on this behavior:
//! ```tex
//! \def\Hello{The macro `Hello' was expanded.\par}
//! \def\HelloWorld{The macro `Hello World' was expanded.\par}
//! % change the category code of the character W to category code other
//! \catcode`\W = 12
//! \HelloWorld
//! ```
//! If the lexer were run over the whole source file at once, the last line
//!     would be tokenized as the single control sequence `\HelloWorld`.
//! However the third line redefines the category code of `W` to other.
//! Because non-singleton control sequence names consist only of characters with the letter category code,
//!     the last line is tokenized as the control sequence `\Hello`
//!     followed by the other token `W` and then four letter tokens for `orld`.
//!
//! Due to this "just in time" behavior, the API for the Texlang lexer looks somewhat like a Rust iterator.
//! The next token is retrieved on-demand using the Lexer's [`next`](Lexer::next) method.
//!
//! ## Subtle lexing behavior
//!
//! Another implication of TeX's dynamic lexing rules is that the process of lexing is fragile
//!     and susceptible to subtle bugs.
//! Consider, for example, the following TeX source code:
//! ```tex
//! A\endlinechar=`\X
//! B
//! C
//! ```
//! What is the output of this code?
//! One might expect that the end of line character will be `<return>` on the first line,
//!     and `X` on the subsequent two lines, thus giving `A BXCX`.
//! However the result is actually `A B CX`!
//! The exact order of operations here is:
//!
//! 1. The `\endlinechar` control sequence is returned from the lexer, and the
//!     primitive starts running.
//! 1. The optional `=` is parsed from the input.
//! 1. TeX starts parsing a number.
//!     The first character from the lexer is `` ` ``, which indicates that
//!     the number will be provided via a single character control sequence.
//! 1. The control sequence `\A` is then returned from the lexer.
//!     At this point the lexer is at the end of line 1, and hasn't started the new line.
//! 1. Next, following TeX's rules for scanning numbers of the form `` `\A ``,
//!     an optional space is parsed from the input.
//!     See e.g. section 442 in TeX The Program.
//!     Parsing this optional space triggers the lexer to return another token.
//!     Because the current line is over, the lexer loads the next line and -- crucially -
//!     uses the _current_ definition of the end of line character, which is `\r`.
//! 1. In this case it happens that there is no optional space.
//!     So at this point the end of line character is changed to `X`.
//!     At the end of the second line, this will be used when loading the third line.
//!
//! The lesson from this example is that the output of TeX source code
//!     is dependent on the precise order of lexing operations.
//! It is very easy for implementations to get this wrong.
//! To minimize the chances of bugs in the Texlang lexer, its implementation
//!     very closely follows the implementation of Knuth's TeX
//!     (see sections 343 - 356 of TeX The Program).
//!
//! ## Using the Texlang lexer
//!
//! The Texlang lexer is used internally in the Texlang VM to read from input files,
//!     but may also be used outside the VM in libraries.
//! For example, implementations of the `\openin`/`\read`
//!     primitives also need to tokenize TeX source code.
//! For this reason the lexer is a public part of Texlang's API.
//!
//!
use crate::error;
use crate::token;
use crate::token::catcode::CatCode;
use crate::token::trace;
use crate::token::CsNameInterner;
use crate::token::Token;
use crate::vm;

/// Error possibly returned when the input contains an invalid character
#[derive(Debug)]
pub struct InvalidCharacterError {
    /// The character. Its catcode is [`CatCode::Invalid`].
    pub char: char,
    /// Trace of the character.
    pub trace: trace::SourceCodeTrace,
}

impl InvalidCharacterError {
    /// Create a new invalid character error.
    pub fn new<S: vm::TexlangState>(vm: &vm::VM<S>, char: char, trace_key: trace::Key) -> Self {
        Self {
            char,
            trace: vm.trace(Token::new_letter(char, trace_key)),
        }
    }
}

impl error::TexError for InvalidCharacterError {
    fn kind(&self) -> error::Kind {
        error::Kind::Token(&self.trace)
    }

    fn title(&self) -> String {
        format![
            "input contains a character {} (Unicode code point {}) with category code {}",
            self.char,
            self.char as u32,
            CatCode::Invalid
        ]
    }

    fn source_annotation(&self) -> String {
        "invalid character".into()
    }

    fn notes(&self) -> Vec<error::display::Note> {
        vec![format![
            "characters with category code {} cannot appear in the input",
            CatCode::Invalid
        ]
        .into()]
    }
}

/// Configuration for a specific instance of the [`Lexer`].
///
/// A Rust type implementing this trait is provided
///     when getting the next token from the lexer.
pub trait Config {
    /// Return the current category code of the provided character.
    fn cat_code(&self, c: char) -> CatCode;

    /// Return the current end of line character.
    fn end_line_char(&self) -> Option<char>;

    // TODO
    // should_report_end_of_line(&self) -> bool;
}

/// Result of calling [Lexer::next].
pub enum Result {
    /// A token.
    Token(token::Token),
    /// An invalid character appeared in the input.
    InvalidCharacter(char, trace::Key),
    /// The end of input was reached.
    EndOfInput,
}

#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
enum State {
    NewLine,
    MidLine,
    SkipBlanks,
}

/// The Texlang lexer
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Lexer {
    raw_lexer: RawLexer,
    state: State,
    // We read control sequence names into a shared buffer to avoid allocating for each one.
    #[cfg_attr(feature = "serde", serde(skip))]
    buffer: String,
}

impl Lexer {
    /// Create a new lexer.
    pub fn new(source_code: String, trace_key_range: trace::KeyRange) -> Lexer {
        Lexer {
            raw_lexer: RawLexer::new(source_code, trace_key_range),
            state: State::NewLine,
            buffer: Default::default(),
        }
    }

    /// Get the next token.
    pub fn next<C: Config>(&mut self, config: &C, cs_name_interner: &mut CsNameInterner) -> Result {
        while let Some(raw_token) = self.raw_lexer.next(config) {
            let c = raw_token.char;
            let (value, next_state) = match raw_token.code {
                CatCode::Escape => {
                    let (cs_name, new_state) = self.read_control_sequence(config, cs_name_interner);
                    (
                        Token::new_control_sequence(cs_name, raw_token.trace_key),
                        new_state,
                    )
                }
                CatCode::EndOfLine => {
                    self.raw_lexer.start_new_line(config);
                    match self.state {
                        State::NewLine => (
                            Token::new_control_sequence(
                                cs_name_interner.get_or_intern("par"),
                                raw_token.trace_key,
                            ),
                            State::NewLine,
                        ),
                        State::MidLine => {
                            (Token::new_space(' ', raw_token.trace_key), State::NewLine)
                        }
                        State::SkipBlanks => {
                            self.state = State::NewLine;
                            continue;
                        }
                    }
                }
                CatCode::Space => match self.state {
                    State::NewLine | State::SkipBlanks => continue,
                    State::MidLine => (
                        Token::new_space(' ', raw_token.trace_key),
                        State::SkipBlanks,
                    ),
                },
                CatCode::BeginGroup => (
                    Token::new_begin_group(c, raw_token.trace_key),
                    State::MidLine,
                ),
                CatCode::EndGroup => (Token::new_end_group(c, raw_token.trace_key), State::MidLine),
                CatCode::MathShift => (
                    Token::new_math_shift(c, raw_token.trace_key),
                    State::MidLine,
                ),
                CatCode::AlignmentTab => (
                    Token::new_alignment_tab(c, raw_token.trace_key),
                    State::MidLine,
                ),
                CatCode::Parameter => {
                    (Token::new_parameter(c, raw_token.trace_key), State::MidLine)
                }
                CatCode::Superscript => {
                    if self
                        .raw_lexer
                        .maybe_apply_caret_notation(raw_token.char, true)
                    {
                        continue;
                    }
                    (
                        Token::new_superscript(c, raw_token.trace_key),
                        State::MidLine,
                    )
                }
                CatCode::Subscript => {
                    (Token::new_subscript(c, raw_token.trace_key), State::MidLine)
                }
                CatCode::Letter => (Token::new_letter(c, raw_token.trace_key), State::MidLine),
                CatCode::Other => (Token::new_other(c, raw_token.trace_key), State::MidLine),
                CatCode::Active => (
                    Token::new_active_character(c, raw_token.trace_key),
                    State::MidLine,
                ),
                CatCode::Comment => {
                    self.raw_lexer.start_new_line(config);
                    self.state = State::NewLine;
                    continue;
                }
                CatCode::Ignored => {
                    continue;
                }
                CatCode::Invalid => {
                    return Result::InvalidCharacter(raw_token.char, raw_token.trace_key)
                }
            };
            self.state = next_state;
            return Result::Token(value);
        }
        Result::EndOfInput
    }

    fn read_control_sequence<F: Config>(
        &mut self,
        config: &F,
        cs_name_interner: &mut CsNameInterner,
    ) -> (token::CsName, State) {
        self.buffer.clear();
        let first_raw_token = match self.raw_lexer.peek(config) {
            None => return (cs_name_interner.get_or_intern(""), State::NewLine),
            Some(first_raw_token) => first_raw_token,
        };
        self.raw_lexer.advance();
        match first_raw_token.code {
            CatCode::Letter => {
                self.buffer.push(first_raw_token.char);
                while let Some(raw_token) = self.raw_lexer.peek(config) {
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
                    return self.read_control_sequence(config, cs_name_interner);
                }
                self.buffer.push(first_raw_token.char);
            }
            _ => {
                self.buffer.push(first_raw_token.char);
            }
        };
        let new_state = match first_raw_token.code {
            CatCode::Letter | CatCode::Space => State::SkipBlanks,
            _ => State::MidLine,
        };
        (cs_name_interner.get_or_intern(&self.buffer), new_state)
    }
}

#[derive(Debug)]
struct RawToken {
    code: CatCode,
    char: char,
    trace_key: trace::Key,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
struct RawLexer {
    // TODO: when serializing we only need to serialize self.source_code[self.pos..]
    source_code: String,
    current_line: String,
    pos: usize,

    next_line: usize,
    // Number of characters that were trimmed from the right side of the current line.
    // End line char is factored in here.
    // This is used to calculate the correct trace key for the first character in a new line.
    num_trimmed_right: usize,
    trace_key_range: trace::KeyRange,
}

impl RawLexer {
    fn new(source_code: String, trace_key_range: trace::KeyRange) -> RawLexer {
        RawLexer {
            source_code,
            current_line: "".into(),
            pos: 0,
            next_line: 0,
            num_trimmed_right: 0,
            trace_key_range,
        }
    }

    fn start_new_line<C: Config>(&mut self, config: &C) -> bool {
        let num_skipped_chars: usize = self.current_line[self.pos..].chars().count();
        self.trace_key_range.advance_by(num_skipped_chars);
        self.trace_key_range.advance_by(self.num_trimmed_right);
        self.pos = 0;
        self.current_line.clear();
        if self.next_line >= self.source_code.len() {
            return false;
        }
        let start = self.next_line;
        let mut end = self.next_line;
        let mut num_spaces = 0;
        for c in self.source_code[self.next_line..].chars() {
            if c == '\n' {
                // TODO: if end_line_char = None and start==end
                // we should continue scanning? This ensures that after this function is invoked
                // either the input is ended or the current line is non empty.
                // Only thing is we have to be mindful of is the eventual \read use case where signaling
                // on every line end may be needed.
                num_spaces += 1;
                break;
            }
            if c != ' ' {
                end += c.len_utf8();
                end += num_spaces;
                num_spaces = 0;
            }
            if c == ' ' {
                num_spaces += 1;
            }
        }
        self.num_trimmed_right = num_spaces;
        self.next_line = end + num_spaces;
        self.current_line.push_str(&self.source_code[start..end]);
        if let Some(end_line_char) = config.end_line_char() {
            // num_spaces may be 0 if this is the end of the file. In this case num_trimmed_right
            // because this is the last line, so we do a safe saturating_sub.
            self.num_trimmed_right = self.num_trimmed_right.saturating_sub(1);
            self.current_line.push(end_line_char)
        }
        true
    }

    fn next<C: Config>(&mut self, config: &C) -> Option<RawToken> {
        match self.next_char() {
            Some(c) => {
                let trace_key = self.trace_key_range.peek();
                self.advance();
                let code = config.cat_code(c);
                Some(RawToken {
                    char: c,
                    code,
                    trace_key,
                })
            }
            None => {
                if !self.start_new_line(config) {
                    return None;
                }
                self.next(config)
            }
        }
    }

    fn peek<C: Config>(&mut self, config: &C) -> Option<RawToken> {
        match self.next_char() {
            Some(c) => {
                let code = config.cat_code(c);
                Some(RawToken {
                    char: c,
                    code,
                    trace_key: self.trace_key_range.peek(),
                })
            }
            None => None,
        }
    }

    fn maybe_apply_caret_notation(&mut self, char_1: char, char_1_consumed: bool) -> bool {
        let char_2_start = if char_1_consumed {
            self.pos
        } else {
            self.pos + char_1.len_utf8()
        };
        let char_2 = match self.current_line[char_2_start..].chars().next() {
            // If the line is over, don't transform.
            // This is what TeX does; see the TeXBook section 355 and related sections.
            None => return false,
            Some(next_char) => next_char,
        };
        if char_2 != char_1 {
            return false;
        }
        let char_3_start = char_2_start + char_2.len_utf8();
        let char_3 = match self.current_line[char_3_start..].chars().next() {
            // If the line is over, don't transform.
            // This is what TeX does; see the TeXBook section 355 and related sections.
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
        // this assert is to have defense-in-depth in light of the unsafe block next.
        assert!(char::from_u32(m as u32).unwrap().is_ascii());
        // SAFETY: the original character `char_3` is single-byte/ASCII and
        // the replacement character `m` is single-byte/ASCII so the replacement
        // preserves the UTF-8 structure of the string.
        unsafe {
            self.current_line.as_bytes_mut()[self.pos] = m;
        }
        true
    }

    fn next_char(&self) -> Option<char> {
        self.current_line[self.pos..].chars().next()
    }

    fn advance(&mut self) {
        self.pos += self.current_line[self.pos..]
            .chars()
            .next()
            .unwrap()
            .len_utf8();
        self.trace_key_range.next();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::catcode;
    use crate::token::catcode::CatCode::*;
    use crate::token::CsNameInterner;
    use crate::token::Value;
    use std::collections::HashMap;

    #[derive(Debug, PartialEq, Eq)]
    enum TokenValue<'a> {
        Character(char, catcode::CatCode, u32),
        ControlSequence(&'a str, u32),
    }
    use TokenValue::Character;
    use TokenValue::ControlSequence;

    impl<'a> TokenValue<'a> {
        fn new(token: Token, interner: &'a CsNameInterner) -> TokenValue<'a> {
            let trace_key = token.trace_key().as_u32();
            if let Value::ControlSequence(cs_name) = token.value() {
                TokenValue::ControlSequence(interner.resolve(cs_name).unwrap(), trace_key)
            } else {
                TokenValue::Character(token.char().unwrap(), token.cat_code().unwrap(), trace_key)
            }
        }
    }

    struct TestConfig {
        cat_codes: HashMap<char, CatCode>,
        end_line_char: Option<char>,
    }

    impl Config for TestConfig {
        fn cat_code(&self, c: char) -> CatCode {
            self.cat_codes.get(&c).copied().unwrap_or_default()
        }
        fn end_line_char(&self) -> Option<char> {
            self.end_line_char
        }
    }

    fn lexer_test(
        input: &str,
        expected_tokens: Vec<TokenValue>,
        end_line_char: Option<char>,
        cat_code_overrides: Vec<(char, CatCode)>,
    ) {
        let mut cat_codes: HashMap<char, CatCode> = CatCode::PLAIN_TEX_DEFAULTS
            .iter()
            .enumerate()
            .map(|(a, b)| (char::from_u32(a.try_into().unwrap()).unwrap(), *b))
            .collect();
        for (c, cat_code) in cat_code_overrides {
            cat_codes.insert(c, cat_code);
        }
        let config = TestConfig {
            cat_codes,
            end_line_char,
        };
        let mut cs_name_interner: CsNameInterner = Default::default();
        let mut actual = Vec::new();
        let mut lexer = Lexer::new(input.into(), trace::KeyRange::for_testing());
        while let Result::Token(t) = lexer.next(&config, &mut cs_name_interner) {
            actual.push(t);
        }
        let actual: Vec<TokenValue<'_>> = actual
            .into_iter()
            .map(|t| TokenValue::new(t, &cs_name_interner))
            .collect();
        assert_eq!(expected_tokens, actual);
    }

    macro_rules! lexer_tests {
        (
            end_line_char( $end_line_char: expr ),
            cat_code_overrides $cat_code_overrides: tt,
            $( ( $name: ident, $input: expr, $ ( $expected_token : expr, ) * ), )+
        ) => {
            $(
            #[test]
            fn $name() {
                let end_line_char = $end_line_char;
                let cat_code_overrides = vec! $cat_code_overrides;
                let input = $input;
                let expected_tokens = vec!( $( $expected_token ),* );
                lexer_test(&input, expected_tokens, end_line_char, cat_code_overrides);
            }
            )+
        };
    }

    lexer_tests![
        end_line_char(Some('\r')),
        cat_code_overrides(),
        (
            control_sequence_basic_1,
            r"\a{b}",
            ControlSequence("a", 0),
            Character('{', BeginGroup, 2),
            Character('b', Letter, 3),
            Character('}', EndGroup, 4),
            Character(' ', Space, 5),
        ),
        (
            control_sequence_basic_2,
            r"\A1",
            ControlSequence("A", 0),
            Character('1', Other, 2),
            Character(' ', Space, 3),
        ),
        (
            control_sequence_single_letter_trailing_space_1,
            r"\a b",
            ControlSequence("a", 0),
            Character('b', Letter, 3),
            Character(' ', Space, 4),
        ),
        (
            control_sequence_single_letter_trailing_space_2,
            r"\a  b",
            ControlSequence("a", 0),
            Character('b', Letter, 4),
            Character(' ', Space, 5),
        ),
        (
            control_sequence_single_letter_trailing_newline_1,
            "\\a\n b",
            ControlSequence("a", 0),
            Character('b', Letter, 4),
            Character(' ', Space, 5),
        ),
        (
            control_sequence_single_letter_trailing_newline_2,
            "\\a\n\nb",
            ControlSequence("a", 0),
            ControlSequence("par", 3),
            Character('b', Letter, 4),
            Character(' ', Space, 5),
        ),
        (
            control_sequence_multi_letter_1,
            "\\ABC{D}",
            ControlSequence("ABC", 0),
            Character('{', BeginGroup, 4),
            Character('D', Letter, 5),
            Character('}', EndGroup, 6),
            Character(' ', Space, 7),
        ),
        (
            control_sequence_multi_letter_2,
            "\\ABC",
            ControlSequence("ABC", 0),
        ),
        (
            control_sequence_single_other_1,
            "\\{{",
            ControlSequence("{", 0),
            Character('{', BeginGroup, 2),
            Character(' ', Space, 3),
        ),
        (
            control_sequence_single_other_2,
            "\\+A",
            ControlSequence("+", 0),
            Character('A', Letter, 2),
            Character(' ', Space, 3),
        ),
        (
            control_sequence_single_other_trailing_space,
            "\\+ A",
            ControlSequence("+", 0),
            Character(' ', Space, 2),
            Character('A', Letter, 3),
            Character(' ', Space, 4),
        ),
        (
            control_sequence_single_space_trailing_space,
            "\\  A",
            ControlSequence(" ", 0),
            Character('A', Letter, 3),
            Character(' ', Space, 4),
        ),
        (
            comment_1,
            "A%B\nC",
            Character('A', Letter, 0),
            Character('C', Letter, 4),
            Character(' ', Space, 5),
        ),
        (
            comment_1_with_space,
            "A%B \nC",
            Character('A', Letter, 0),
            Character('C', Letter, 5),
            Character(' ', Space, 6),
        ),
        (
            comment_2,
            "A%B\n%C\nD",
            Character('A', Letter, 0),
            Character('D', Letter, 7),
            Character(' ', Space, 8),
        ),
        (comment_3, "A%a comment here", Character('A', Letter, 0),),
        (
            comment_4,
            "A%\n B",
            Character('A', Letter, 0),
            Character('B', Letter, 4),
            Character(' ', Space, 5),
        ),
        (
            comment_5,
            "A%\n\n B",
            Character('A', Letter, 0),
            ControlSequence("par", 3),
            Character('B', Letter, 5),
            Character(' ', Space, 6),
        ),
        (
            comment_6,
            "\\A %\nB",
            ControlSequence("A", 0),
            Character('B', Letter, 5),
            Character(' ', Space, 6),
        ),
        (
            texbook_exercise_8_2_e,
            "A%\n B%",
            Character('A', Letter, 0),
            Character('B', Letter, 4),
        ),
        (
            texbook_exercise_8_4,
            r" $x^2$~ \Tex ^^C",
            Character('$', MathShift, 1),
            Character('x', Letter, 2),
            Character('^', Superscript, 3),
            Character('2', Other, 4),
            Character('$', MathShift, 5),
            Character('~', Active, 6),
            Character(' ', Space, 7),
            ControlSequence("Tex", 8),
            Character('\u{3}', Other, 15),
            Character(' ', Space, 16),
        ),
        (
            texbook_exercise_8_5,
            "Hi!\n\n\n",
            Character('H', Letter, 0),
            Character('i', Letter, 1),
            Character('!', Other, 2),
            Character(' ', Space, 3),
            ControlSequence("par", 4),
            ControlSequence("par", 5),
        ),
        (
            double_space_creates_one_space,
            "A  B",
            Character('A', Letter, 0),
            Character(' ', Space, 1),
            Character('B', Letter, 3),
            Character(' ', Space, 4),
        ),
        (
            single_newline_creates_one_space,
            "A\nB",
            Character('A', Letter, 0),
            Character(' ', Space, 1),
            Character('B', Letter, 2),
            Character(' ', Space, 3),
        ),
        (
            space_and_newline_creates_space,
            "A \nB",
            Character('A', Letter, 0),
            Character(' ', Space, 1),
            Character('B', Letter, 3),
            Character(' ', Space, 4),
        ),
        (
            par_1,
            "A\n\nB",
            Character('A', Letter, 0),
            Character(' ', Space, 1),
            ControlSequence("par", 2),
            Character('B', Letter, 3),
            Character(' ', Space, 4),
        ),
        (
            par_2,
            "A\n \nB",
            Character('A', Letter, 0),
            Character(' ', Space, 1),
            ControlSequence("par", 2),
            Character('B', Letter, 4),
            Character(' ', Space, 5),
        ),
        (
            par_3,
            "A\n\n\nB",
            Character('A', Letter, 0),
            Character(' ', Space, 1),
            ControlSequence("par", 2),
            ControlSequence("par", 3),
            Character('B', Letter, 4),
            Character(' ', Space, 5),
        ),
        (
            caret_notation_1,
            "^^k",
            Character('+', Other, 2),
            Character(' ', Space, 3),
        ),
        (
            caret_notation_2,
            "^^+",
            Character('k', Letter, 2),
            Character(' ', Space, 3),
        ),
        (
            caret_notation_3,
            "^^+m",
            Character('k', Letter, 2),
            Character('m', Letter, 3),
            Character(' ', Space, 4),
        ),
        (caret_notation_4, "^^\n", Character('M', Letter, 2),),
        (caret_notation_5, "^^", Character('M', Letter, 2),),
        (
            caret_notation_6,
            "^^\nA",
            Character('M', Letter, 2),
            Character('A', Letter, 3),
            Character(' ', Space, 4),
        ),
        (
            caret_notation_recursive_1,
            "^^\u{1E}^+",
            Character('k', Letter, 4),
            Character(' ', Space, 5),
        ),
        (
            caret_notation_recursive_2,
            "\\^^\u{1E}^+",
            ControlSequence("k", 0),
        ),
        (
            caret_notation_recursive_3,
            "\\j^^\u{1E}^+",
            ControlSequence("jk", 0),
        ),
        (
            // This test case triggers a recursive call to `new_control_sequence`.
            caret_notation_recursive_4,
            format!["\\^^{}+", "\u{1E}^".repeat(200)],
            ControlSequence("k", 0),
        ),
        (
            caret_notation_end_of_input_1,
            "^^",
            Character('M', Letter, 2),
        ),
        (
            caret_notation_end_of_input_2,
            "\\^^",
            ControlSequence("M", 0),
        ),
        (
            caret_notation_end_of_input_3,
            "\\a^^",
            ControlSequence("aM", 0),
        ),
        (
            caret_notation_boundary_1,
            "^^\u{00}",
            Character(char::from_u32(0x40).unwrap(), Other, 2),
            Character(' ', Space, 3),
        ),
        (
            caret_notation_boundary_3,
            "^^\u{40}",
            // skipped character
            ControlSequence("par", 3),
        ),
        (
            caret_notation_boundary_4,
            "^^\u{7F}",
            Character(char::from_u32(0x3F).unwrap(), Other, 2),
            Character(' ', Space, 3),
        ),
        (
            caret_notation_cs_1,
            r"\^^m",
            ControlSequence("-", 0),
            Character(' ', Space, 4),
        ),
        (
            caret_notation_cs_2,
            r"\^^ma",
            ControlSequence("-", 0),
            Character('a', Letter, 4),
            Character(' ', Space, 5),
        ),
        (caret_notation_cs_3, r"\^^-", ControlSequence("m", 0),),
        (caret_notation_cs_4, r"\^^-a", ControlSequence("ma", 0),),
        (
            caret_notation_cs_5,
            r"\^^-^^-+",
            ControlSequence("mm", 0),
            Character('+', Other, 7),
            Character(' ', Space, 8),
        ),
        (caret_notation_cs_6, r"\a^^-", ControlSequence("am", 0),),
        (
            caret_notation_cs_7,
            "\\^a",
            ControlSequence("^", 0),
            Character('a', Letter, 2),
            Character(' ', Space, 3),
        ),
        (
            caret_notation_cs_8,
            "\\a^a",
            ControlSequence("a", 0),
            Character('^', Superscript, 2),
            Character('a', Letter, 3),
            Character(' ', Space, 4),
        ),
    ];

    lexer_tests![
        end_line_char(Some('\r')),
        cat_code_overrides(('Z', Ignored)),
        (
            control_sequence_single_ignored,
            r"\Z",
            ControlSequence("Z", 0),
            Character(' ', Space, 2),
        ),
        (ignored_character_1, "Z", ControlSequence("par", 1),),
        (
            ignored_character_2,
            "AZB",
            Character('A', Letter, 0),
            Character('B', Letter, 2),
            Character(' ', Space, 3),
        ),
        (
            texbook_exercise_8_2_f,
            r"\AZB",
            ControlSequence("A", 0),
            Character('B', Letter, 3),
            Character(' ', Space, 4),
        ),
    ];

    lexer_tests![
        end_line_char(Some('\r')),
        cat_code_overrides(('W', Invalid)),
        (
            control_sequence_single_invalid,
            r"\W",
            ControlSequence("W", 0),
            Character(' ', Space, 2),
        ),
    ];

    lexer_tests![
        end_line_char(Some('\r')),
        cat_code_overrides(('X', EndOfLine)),
        (
            non_standard_newline_character,
            "AXB",
            Character('A', Letter, 0),
            Character(' ', Space, 1),
        ),
        (
            non_standard_newline_character_2,
            "AXXB",
            Character('A', Letter, 0),
            Character(' ', Space, 1),
        ),
        (single_non_standard_newline, "X", ControlSequence("par", 0),),
    ];

    lexer_tests![
        end_line_char(Some('\r')),
        cat_code_overrides(('Y', Space)),
        (
            non_standard_whitespace_1,
            "AYB",
            Character('A', Letter, 0),
            Character(' ', Space, 1),
            Character('B', Letter, 2),
            Character(' ', Space, 3),
        ),
    ];

    lexer_tests![
        end_line_char(Some('\r')),
        cat_code_overrides(
            ('\u{01}', Escape),
            ('\u{02}', Superscript),
            ('\u{03}', Space),
            ('\u{0D}', Letter),
        ),
        (
            texbook_exercise_8_6,
            r"^^B^^BM^^A^^B^^C^^M^^@\M ",
            Character('\u{02}', Superscript, 2),
            Character('\u{02}', Superscript, 5),
            Character('M', Letter, 6),
            ControlSequence("\u{02}", 9),
            Character(' ', Space, 15),
            Character('\u{0D}', Letter, 18),
            ControlSequence("M\u{0D}", 22),
        ),
    ];

    lexer_tests![
        end_line_char(Some('B')),
        cat_code_overrides(),
        (
            control_sequence_includes_end_line_char_1,
            r"\A",
            ControlSequence("AB", 0),
        ),
        (
            control_sequence_includes_end_line_char_2,
            r"\A  ",
            ControlSequence("AB", 0),
        ),
        (
            control_sequence_includes_end_line_char_3,
            r"\",
            ControlSequence("B", 0),
        ),
        (
            control_sequence_includes_end_line_char_4,
            r"\  ",
            ControlSequence("B", 0),
        ),
        (
            control_sequence_does_not_span_lines,
            "\\A\nC",
            ControlSequence("AB", 0),
            Character('C', Letter, 3),
            Character('B', Letter, 4),
        ),
        (
            repeated_end_line_char_1,
            "\n\n\n",
            Character('B', Letter, 0),
            Character('B', Letter, 1),
            Character('B', Letter, 2),
        ),
        (
            repeated_end_line_char_2,
            "A\nA\nA\n",
            Character('A', Letter, 0),
            Character('B', Letter, 1),
            Character('A', Letter, 2),
            Character('B', Letter, 3),
            Character('A', Letter, 4),
            Character('B', Letter, 5),
        ),
        (
            right_size_trimming,
            "A  \nA  \n",
            Character('A', Letter, 0),
            Character('B', Letter, 1),
            Character('A', Letter, 4),
            Character('B', Letter, 5),
        ),
    ];

    lexer_tests![
        end_line_char(None),
        cat_code_overrides(),
        (
            multiple_skipped_lines,
            "A\n\n\nB",
            Character('A', Letter, 0),
            Character('B', Letter, 4),
        ),
        (
            empty_cs_name,
            "\\\nB",
            ControlSequence("", 0),
            Character('B', Letter, 2),
        ),
    ];
}
