//! Tracing system for determining the origin of a token.
//!
//! This module implements a [Tracer] for tokens.
//! When building helpful error messages we need to know the origin of tokens -
//!     e.g., the file and line they came from.
//! The tracing functionality here enables obtaining this information in the form of a [SourceCodeTrace].
//!
//! Rather than the custom system here,
//!     a simpler solution would be to include this information on the token itself.
//! The token could include a line position number and a reference
//!     counting pointer to some data structure containing information about the line.
//! The problem with this solution is that it makes the [Token] type very large, and
//!     this causes unacceptably poor performance in Texlang's tight inner loops.
//! With the tracer here, each token only needs to hold onto a 32-bit [Key] which is enough
//!     to perform a full trace.
//!
//! # How the tracer works
//!
//! When adding source code to the input, the tracer is informed using the
//!     [register_source_code](Tracer::register_source_code) method.
//! The tracer allocates a contiguous range of [Keys](Key) that is large enough
//!     to give each UTF-8 character in the input a unique key.
//! These keys are returned using the opaque [KeyRange] type, which enables the caller to retrieve
//!     these keys.
//! It is assumed that the caller will assign keys in order to each UTF-8 character in the source code.
//!
//! In addition to returning the range, the tracer associates the key range with the source code in an
//!     internal data structure.
//!
//! When tracing a token (using [trace](Tracer::trace)), the token's key is used to identify
//!     which key range the key came from.
//! This key range is then used to identify the source code associated to the token.
//! The difference between the token's key and the first key for the source code is the UTF-8 offset
//!     into the source code.
//! Thus we can uniquely identify the UTF-8 character the token is a associated to.
use crate::token::{CsNameInterner, Token, Value};
use std::collections::BTreeMap;
use std::ops::Bound::Included;
use std::path::PathBuf;

use super::CommandRef;

/// Key attached to tokens to enable tracing them.
///
/// This type is 32 bits.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Key(u32);

impl Key {
    pub fn dummy() -> Key {
        Key(u32::MAX)
    }

    #[cfg(test)]
    pub fn as_u32(&self) -> u32 {
        self.0
    }
}

/// Range of free keys that may be assigned to tokens.
#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct KeyRange {
    next: u32,
    limit: u32,
}

impl KeyRange {
    /// Get the next trace [Key].
    ///
    /// Panics if all of the keys in this range have been used.
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Key {
        if self.next >= self.limit {
            panic!["requested more trace keys than are in the range"]
        }
        let n = self.next;
        self.next += 1;
        Key(n)
    }

    /// Peek at the next trace [Key].
    ///
    /// Panics if all of the keys in this range have been used.
    pub fn peek(&mut self) -> Key {
        if self.next >= self.limit {
            panic!["requested more trace keys than are in the range"]
        }
        Key(self.next)
    }

    /// Advances the range forward by the provided offset.
    ///
    /// Panics if the provided offset cannot be cast to u32.
    pub fn advance_by(&mut self, u: usize) {
        self.next = self.next.checked_add(u.try_into().unwrap()).unwrap();
    }
}

impl KeyRange {
    pub fn empty() -> KeyRange {
        KeyRange { next: 0, limit: 0 }
    }

    #[cfg(test)]
    pub fn for_testing() -> KeyRange {
        KeyRange {
            next: 0,
            limit: u32::MAX,
        }
    }
}

/// A token trace
#[derive(Debug, PartialEq, Eq)]
pub struct SourceCodeTrace {
    /// Origin of the source code this token came from.
    pub origin: Origin,
    /// Content of the line this token came from.
    pub line_content: String,
    /// Number of the line within the file, starting at 1.
    pub line_number: usize,
    /// Index within the line that the token starts.
    pub index: usize,
    /// Value of the token.
    pub value: String,
    /// If this is for a token, the value of the token.
    /// Otherwise this is an end of input snippet.
    pub token: Option<Token>,
}

/// Data structure that records information for token tracing
#[derive(Default)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Tracer {
    checkpoints: BTreeMap<u32, Checkpoint>,
    next_key: u32,

    // A key use to get the last file that was inputted manually; i.e., not via an \input
    // or other command in a TeX file
    last_external_input: Option<u32>,
}

/// Enum describing the possible origins of source code
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Origin {
    File(PathBuf),
    Terminal,
}

impl Tracer {
    /// Registers source code with the tracer.
    ///
    /// The returned [KeyRange] should be used to assign [Keys](Key) to the tokens that
    ///     are lexed from the referenced source code.
    /// The tracer assumes that the first key is assigned to token corresponding to the
    ///     first UTF-8 character in their source code,
    ///     the second key to the second UTF-8 character, and so on.
    pub fn register_source_code(
        &mut self,
        token: Option<Token>,
        origin: Origin,
        source_code: &str,
    ) -> KeyRange {
        let len = match u32::try_from(source_code.len()) {
            Err(_) => {
                panic!(
                    "source code too big ({} bytes); max is 2^32={} bytes",
                    source_code.len(),
                    u32::MAX
                )
            }
            // For empty files, we still assign one key because this allows us to trace end of input errors.
            Ok(0) => 1_u32,
            // We add 1 to handle the case when the file does not end in a newline character. In this case
            // the extra key will be used to refer to a character added using the \endlinechar mechanism.
            Ok(limit) => limit + 1,
        };
        let range = KeyRange {
            next: self.next_key,
            limit: self.next_key + len,
        };
        self.checkpoints.insert(
            range.next,
            Checkpoint::SourceCode {
                origin,
                content: source_code.to_string(),
            },
        );
        if token.is_none() {
            self.last_external_input = Some(self.next_key);
        }
        self.next_key = range.limit;
        range
    }

    /// Return a trace for the provided token.
    pub fn trace(&self, token: Token, cs_name_interner: &CsNameInterner) -> SourceCodeTrace {
        let value = match token.value() {
            Value::CommandRef(CommandRef::ControlSequence(cs_name)) => {
                format!["\\{}", cs_name_interner.resolve(cs_name).unwrap()]
            }
            // TODO: maybe have a cs or char method?
            _ => token.char().unwrap().to_string(),
        };

        let (&first_key, checkpoint) = self
            .checkpoints
            .range((Included(&0), Included(&token.trace_key.0)))
            .rev()
            .next()
            .unwrap();

        match checkpoint {
            Checkpoint::SourceCode { origin, content } => {
                let char_offset = (token.trace_key().0 - first_key) as usize;
                let mut line_number = 1;
                let mut byte_line_start = 0;
                let mut char_line_start = 0;
                for (char_index, (byte_index, c)) in content.char_indices().enumerate() {
                    if char_index == char_offset {
                        break;
                    }
                    if c == '\n' {
                        byte_line_start = byte_index + 1;
                        char_line_start = char_index + 1;
                        line_number += 1;
                    }
                }
                let position = char_offset - char_line_start;
                let tail = &content[byte_line_start..];
                let line_content = match tail.split_once('\n') {
                    None => tail.to_string(),
                    Some((a, _)) => a.to_string(),
                };
                SourceCodeTrace {
                    origin: origin.clone(),
                    line_content,
                    line_number,
                    index: position,
                    value,
                    token: Some(token),
                }
            }
        }
    }

    pub fn trace_end_of_input(&self) -> SourceCodeTrace {
        let f = self
            .checkpoints
            .get(&self.last_external_input.unwrap())
            .unwrap();
        match f {
            Checkpoint::SourceCode { origin, content } => {
                // (line index, byte index of first character)
                let mut last_line: (usize, usize) = (0, 0);
                let mut last_non_empty_line: (usize, usize) = (0, 0);
                for (i, c) in content.char_indices() {
                    if !c.is_whitespace() {
                        last_non_empty_line = last_line;
                    } else if c == '\n' {
                        last_line.0 += 1;
                        last_line.1 = i + 1;
                    }
                }
                let last_line = content[last_non_empty_line.1..].trim_end();
                SourceCodeTrace {
                    origin: origin.clone(),
                    line_content: last_line.to_string(),
                    line_number: last_non_empty_line.0 + 1,
                    index: last_line.len(),
                    value: " ".to_string(),
                    token: None,
                }
            }
        }
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
enum Checkpoint {
    SourceCode {
        origin: Origin,
        content: String, // TODO: should be rc::Rc<str>?
    },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn one_source_code() {
        let file_name: PathBuf = "input.tex".into();
        let origin = Origin::File(file_name);
        let line_1 = "hël".to_string();
        let line_2 = "wor\\cömmand".to_string();
        let line_3 = "hël".to_string();
        let source_code = format!("{}\n{}\n{}", line_1, line_2, line_3);

        let mut tracer: Tracer = Default::default();
        let mut interner: CsNameInterner = Default::default();
        let command = interner.get_or_intern("command");
        let mut range = tracer.register_source_code(None, origin.clone(), &source_code);
        let mut tokens = vec![
            Token::new_letter('h', range.next()),
            Token::new_letter('e', range.next()),
            Token::new_letter('l', range.next()),
            Token::new_space('\n', range.next()),
            Token::new_letter('w', range.next()),
            Token::new_letter('o', range.next()),
            Token::new_letter('r', range.next()),
            Token::new_control_sequence(command, range.next()),
        ];
        for _ in 0.."command".len() {
            range.next();
        }
        let mut extra_tokens = vec![
            Token::new_space('\n', range.next()),
            Token::new_letter('h', range.next()),
            Token::new_letter('e', range.next()),
            Token::new_letter('l', range.next()),
        ];
        tokens.append(&mut extra_tokens);

        let got_traces: Vec<SourceCodeTrace> = tokens
            .iter()
            .map(|token| tracer.trace(*token, &interner))
            .collect();

        let want_traces = vec![
            SourceCodeTrace {
                origin: origin.clone(),
                line_content: line_1.clone(),
                line_number: 1,
                index: 0,
                value: "h".to_string(),
                token: Some(tokens[0]),
            },
            SourceCodeTrace {
                origin: origin.clone(),
                line_content: line_1.clone(),
                line_number: 1,
                index: 1,
                value: "e".to_string(),
                token: Some(tokens[1]),
            },
            SourceCodeTrace {
                origin: origin.clone(),
                line_content: line_1.clone(),
                line_number: 1,
                index: 2,
                value: "l".to_string(),
                token: Some(tokens[2]),
            },
            SourceCodeTrace {
                origin: origin.clone(),
                line_content: line_1.clone(),
                line_number: 1,
                index: 3,
                value: "\n".to_string(),
                token: Some(tokens[3]),
            },
            SourceCodeTrace {
                origin: origin.clone(),
                line_content: line_2.clone(),
                line_number: 2,
                index: 0,
                value: "w".to_string(),
                token: Some(tokens[4]),
            },
            SourceCodeTrace {
                origin: origin.clone(),
                line_content: line_2.clone(),
                line_number: 2,
                index: 1,
                value: "o".to_string(),
                token: Some(tokens[5]),
            },
            SourceCodeTrace {
                origin: origin.clone(),
                line_content: line_2.clone(),
                line_number: 2,
                index: 2,
                value: "r".to_string(),
                token: Some(tokens[6]),
            },
            SourceCodeTrace {
                origin: origin.clone(),
                line_content: line_2.clone(),
                line_number: 2,
                index: 3,
                value: "\\command".to_string(),
                token: Some(tokens[7]),
            },
            SourceCodeTrace {
                origin: origin.clone(),
                line_content: line_2.clone(),
                line_number: 2,
                index: 11,
                value: "\n".to_string(),
                token: Some(tokens[8]),
            },
            SourceCodeTrace {
                origin: origin.clone(),
                line_content: line_3.clone(),
                line_number: 3,
                index: 0,
                value: "h".to_string(),
                token: Some(tokens[9]),
            },
            SourceCodeTrace {
                origin: origin.clone(),
                line_content: line_3.clone(),
                line_number: 3,
                index: 1,
                value: "e".to_string(),
                token: Some(tokens[10]),
            },
            SourceCodeTrace {
                origin: origin.clone(),
                line_content: line_3.clone(),
                line_number: 3,
                index: 2,
                value: "l".to_string(),
                token: Some(tokens[11]),
            },
        ];
        assert_eq!(want_traces, got_traces);
    }

    #[test]
    fn multiple_source_code() {
        let mut tokens = Vec::new();
        let mut tracer: Tracer = Default::default();
        let interner: CsNameInterner = Default::default();

        let file_1 = Origin::File("a.tex".into());
        let file_1_content = "a".to_string();
        let mut range = tracer.register_source_code(None, file_1.clone(), &file_1_content);
        tokens.push(Token::new_letter('a', range.next()));

        let file_2 = Origin::File("b.tex".into());
        let file_2_content = "b".to_string();
        let mut range = tracer.register_source_code(None, file_2.clone(), &file_2_content);
        tokens.push(Token::new_letter('b', range.next()));

        let file_3 = Origin::Terminal;
        let file_3_content = "c".to_string();
        let mut range = tracer.register_source_code(None, file_3.clone(), &file_3_content);
        tokens.push(Token::new_letter('c', range.next()));

        let got_traces: Vec<SourceCodeTrace> = tokens
            .iter()
            .map(|token| tracer.trace(*token, &interner))
            .collect();

        let want_traces = vec![
            SourceCodeTrace {
                origin: file_1,
                line_content: file_1_content,
                line_number: 1,
                index: 0,
                value: "a".to_string(),
                token: Some(tokens[0]),
            },
            SourceCodeTrace {
                origin: file_2,
                line_content: file_2_content,
                line_number: 1,
                index: 0,
                value: "b".to_string(),
                token: Some(tokens[1]),
            },
            SourceCodeTrace {
                origin: file_3,
                line_content: file_3_content,
                line_number: 1,
                index: 0,
                value: "c".to_string(),
                token: Some(tokens[2]),
            },
        ];
        assert_eq!(want_traces, got_traces);
    }
}
