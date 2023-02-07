//! Token tracing system.
//!
//! This module implements a [Tracer] for tokens.
//! When building helpful error messages we need to know where tokens involved in the error came from.
//! The tracing functionality here enables obtaining this information in the form of a [Trace].
//!
//! Rather than a custom system,
//!     a simpler solution would be to include this information on the token itself.
//! The token could include a line position number and a reference
//!     counting pointer to some data structure containing information about the line.
//! The initial tracing system for Texlang did exactly this.
//! The problem with this solution is that it makes the [Token] type very large, and
//!     this causes unacceptably poor performance in Texlang's tight inner loops.
//!
//! With the tracer here, each token only needs to hold onto a 32-bit [Key] which is enough
//!     to perform a full trace.
//!
//! # How the tracer works
//!
//! When adding source code to the input, the tracer is informed using the
//!     [register_source_code](Tracer::register_source_code) method.
//! The tracer allocates a contiguous range of [Keys](Key) that is large enough
//!     to give each UTF-8 character in the input a unique key.
//! These keys are returned using the opaque [KeyRange] type, which enables the caller to generate
//!     these keys.
//! It is assumed that the caller will assign keys in order to each UTF-8 character in the source code.
//! As well as returning the range, the tracer associates this key range with the source code in an
//!     internal data structure.
//!
//! Then, when tracing a token (using [trace](Tracer::trace)), the token's key is used to identify
//!     which key range the key came from.
//! This key range is then used to identify the source code associated to the token.
//! The difference between the token's key and the first key for the source code is the UTF-8 offset
//!     into the source code.
//! Thus we can uniquely identify the UTF-8 character the key is a associated to.
use crate::token::{CsNameInterner, Token, Value};
use colored::*;
use std::collections::BTreeMap;
use std::ops::Bound::Included;

/// Key attached to tokens to enable tracing them.
///
/// This type is 32 bits.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Key(u32);

impl Key {
    pub fn dummy() -> Key {
        Key(u32::MAX)
    }
}

/// Range of free keys that may be assigned to tokens.
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
pub struct Trace {
    /// Name of the file this token came from.
    pub file_name: String,
    /// Content of the line this token came from.
    pub line_content: String,
    /// Number of the line within the file, starting at 1.
    pub line_number: usize,
    /// Index within the line that the token starts.
    pub index: usize,
    /// Value of the token.
    pub value: String,
}

/// Data structure that records information for token tracing
#[derive(Default)]
pub struct Tracer {
    checkpoints: BTreeMap<u32, Checkpoint>,
    next_key: u32,

    // A key use to get the last file that was inputted manually; i.e., not via an \input
    // or other command in a TeX file
    last_external_input: Option<u32>,
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
        file_name: String,
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
            Ok(0) => return KeyRange::empty(),
            Ok(limit) => limit,
        };
        let range = KeyRange {
            next: self.next_key,
            limit: self.next_key + len,
        };
        self.checkpoints.insert(
            range.next,
            Checkpoint::SourceCode {
                file_name,
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
    pub fn trace(&self, token: Token, cs_name_interner: &CsNameInterner) -> Trace {
        let value = match token.value() {
            Value::ControlSequence(cs_name) => {
                format!["\\{}", cs_name_interner.resolve(&cs_name).unwrap()]
            }
            _ => token.char().unwrap().to_string(),
        };

        let (&first_key, checkpoint) = self
            .checkpoints
            .range((Included(&0), Included(&token.trace_key.0)))
            .rev()
            .next()
            .unwrap();

        match checkpoint {
            Checkpoint::SourceCode { file_name, content } => {
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
                Trace {
                    file_name: file_name.clone(),
                    line_content,
                    line_number,
                    index: position,
                    value,
                }
            }
        }
    }

    pub fn trace_end_of_input(&self) -> Trace {
        let f = self
            .checkpoints
            .get(&self.last_external_input.unwrap())
            .unwrap();
        match f {
            Checkpoint::SourceCode { file_name, content } => {
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
                Trace {
                    file_name: file_name.clone(),
                    line_content: last_line.to_string(),
                    line_number: last_non_empty_line.0 + 1,
                    index: last_line.len(),
                    value: " ".to_string(),
                }
            }
        }
    }
}

impl std::fmt::Display for Trace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let margin_width = self.line_number.to_string().len() + 1;
        let bar = "|".bright_yellow();
        writeln!(
            f,
            "{}{} {}:{}:{}",
            " ".repeat(margin_width - 1),
            ">>>".bright_yellow().bold(),
            self.file_name,
            self.line_number,
            self.index + 1
        )?;
        writeln!(f, "{}{} ", " ".repeat(margin_width), bar)?;
        writeln!(
            f,
            "{}{} {} {}",
            " ".repeat(margin_width - self.line_number.to_string().len() - 1),
            self.line_number.to_string().bright_yellow(),
            bar,
            self.line_content.trim_end()
        )?;
        write!(
            f,
            "{}{} {}{}",
            " ".repeat(margin_width),
            bar,
            " ".repeat(self.index),
            "^".repeat(self.value.len()).bright_red().bold(),
        )?;
        Ok(())
    }
}

enum Checkpoint {
    SourceCode { file_name: String, content: String },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn one_source_code() {
        let file_name = "input.tex".to_string();
        let line_1 = "hël".to_string();
        let line_2 = "wor\\cömmand".to_string();
        let line_3 = "hël".to_string();
        let source_code = format!("{}\n{}\n{}", line_1, line_2, line_3);

        let mut tracer: Tracer = Default::default();
        let mut interner: CsNameInterner = Default::default();
        let command = interner.get_or_intern("command");
        let mut range = tracer.register_source_code(None, file_name.clone(), &source_code);
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

        let got_traces: Vec<Trace> = tokens
            .into_iter()
            .map(|token| tracer.trace(token, &interner))
            .collect();

        let want_traces = vec![
            Trace {
                file_name: file_name.clone(),
                line_content: line_1.clone(),
                line_number: 1,
                index: 0,
                value: "h".to_string(),
            },
            Trace {
                file_name: file_name.clone(),
                line_content: line_1.clone(),
                line_number: 1,
                index: 1,
                value: "e".to_string(),
            },
            Trace {
                file_name: file_name.clone(),
                line_content: line_1.clone(),
                line_number: 1,
                index: 2,
                value: "l".to_string(),
            },
            Trace {
                file_name: file_name.clone(),
                line_content: line_1.clone(),
                line_number: 1,
                index: 3,
                value: "\n".to_string(),
            },
            Trace {
                file_name: file_name.clone(),
                line_content: line_2.clone(),
                line_number: 2,
                index: 0,
                value: "w".to_string(),
            },
            Trace {
                file_name: file_name.clone(),
                line_content: line_2.clone(),
                line_number: 2,
                index: 1,
                value: "o".to_string(),
            },
            Trace {
                file_name: file_name.clone(),
                line_content: line_2.clone(),
                line_number: 2,
                index: 2,
                value: "r".to_string(),
            },
            Trace {
                file_name: file_name.clone(),
                line_content: line_2.clone(),
                line_number: 2,
                index: 3,
                value: "\\command".to_string(),
            },
            Trace {
                file_name: file_name.clone(),
                line_content: line_2.clone(),
                line_number: 2,
                index: 11,
                value: "\n".to_string(),
            },
            Trace {
                file_name: file_name.clone(),
                line_content: line_3.clone(),
                line_number: 3,
                index: 0,
                value: "h".to_string(),
            },
            Trace {
                file_name: file_name.clone(),
                line_content: line_3.clone(),
                line_number: 3,
                index: 1,
                value: "e".to_string(),
            },
            Trace {
                file_name: file_name.clone(),
                line_content: line_3.clone(),
                line_number: 3,
                index: 2,
                value: "l".to_string(),
            },
        ];
        assert_eq!(want_traces, got_traces);
    }

    #[test]
    fn multiple_source_code() {
        let mut tokens = Vec::new();
        let mut tracer: Tracer = Default::default();
        let interner: CsNameInterner = Default::default();

        let file_1 = "a.tex".to_string();
        let file_1_content = "a".to_string();
        let mut range = tracer.register_source_code(None, file_1.clone(), &file_1_content);
        tokens.push(Token::new_letter('a', range.next()));

        let file_2 = "b.tex".to_string();
        let file_2_content = "b".to_string();
        let mut range = tracer.register_source_code(None, file_2.clone(), &file_2_content);
        tokens.push(Token::new_letter('b', range.next()));

        let file_3 = "c.tex".to_string();
        let file_3_content = "c".to_string();
        let mut range = tracer.register_source_code(None, file_3.clone(), &file_3_content);
        tokens.push(Token::new_letter('c', range.next()));

        let got_traces: Vec<Trace> = tokens
            .into_iter()
            .map(|token| tracer.trace(token, &interner))
            .collect();

        let want_traces = vec![
            Trace {
                file_name: file_1,
                line_content: file_1_content,
                line_number: 1,
                index: 0,
                value: "a".to_string(),
            },
            Trace {
                file_name: file_2,
                line_content: file_2_content,
                line_number: 1,
                index: 0,
                value: "b".to_string(),
            },
            Trace {
                file_name: file_3,
                line_content: file_3_content,
                line_number: 1,
                index: 0,
                value: "c".to_string(),
            },
        ];
        assert_eq!(want_traces, got_traces);
    }
}
