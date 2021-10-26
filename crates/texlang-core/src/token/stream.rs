//! Definition of the token stream trait and some general-purpose implementations.
//!
//! The simplest example of a stream is a vector of tokens. However, streams are more general
//! than this and can encompass situations in which the full contents cannot be determined in
//! advance. The classic example of the latter kind of stream comes from the following LaTeX
//! snippet:
//! ```tex
//! \makeatletter \do@
//! ```
//! Assuming the default TeX catcode map, if we were to parse this input all at once we would
//! get three tokens: the control sequence `makeatletter`, the control sequence `do`, and a
//! single character token with value `@` and catcode "other". This is not the correct result,
//! though: the first control sequence changes the tokenization rules such that `@` is now
//! an admissible character in the name of a control sequence. The correct input is thus
//! the control sequence `makeatletter` followed by the control sequence `do@`.
//!
//! This example demonstrates that one must be careful when processing streams. After reading
//! a token, all possible side effects must take place before the next token is read.
//!
//! # Getting the next token
//!
//! Tokens are consume from a stream using the `next` method. This method is almost the same
//! as the `next` method in Rust's iterator trait, except a stream can return an error.
//! ```
//! # use texlang_core::token::stream::VecStream;
//! # use texlang_core::token::stream::Stream;
//! # use texlang_core::token::Token;
//! # use texlang_core::token::catcode::CatCode;
//! let mut stream = VecStream::new(vec![
//!     Token::new_letter('a'),
//!     Token::new_letter('b'),
//!     Token::new_letter('c'),
//! ]);
//!
//! assert_eq!(stream.next().unwrap(), Some(Token::new_letter('a')));
//! assert_eq!(stream.next().unwrap(), Some(Token::new_letter('b')));
//! assert_eq!(stream.next().unwrap(), Some(Token::new_letter('c')));
//! assert_eq!(stream.next().unwrap(), None);
//! ```
//! As with iterators, a result of `Ok(None)` indicates that the stream is exhausted.
//!
//! # Peeking at the next token
//!
//! In many sitations it is necessary to examine the next token without consuming it; i.e.,
//! _peek_ at the next token. An example is reading an integer from a stream, in which one needs
//! to peek at the next token to see if it is a digit. Consuming the token with `next` is not
//! correct in this situation if the token is not a digit. The simplist way to peek is with
//! the `peek` method.
//! ```
//! # use texlang_core::token::stream::VecStream;
//! # use texlang_core::token::stream::Stream;
//! # use texlang_core::token::Token;
//! # use texlang_core::token::catcode::CatCode;
//! let mut stream = VecStream::new(vec![
//!     Token::new_letter('a'),
//!     Token::new_letter('b'),
//!     Token::new_letter('c'),
//! ]);
//!
//! assert_eq!(stream.peek().unwrap(), Some(&Token::new_letter('a')));
//! assert_eq!(stream.peek().unwrap(), Some(&Token::new_letter('a')));
//! assert_eq!(stream.next().unwrap(), Some(Token::new_letter('a')));
//! assert_eq!(stream.peek().unwrap(), Some(&Token::new_letter('b')));
//! ```
//! The `peek` method returns an immutable reference to the token: because the token is not
//! being consumed, ownership cannot be transferred as in `next`.
//!
//! The `peek` method must be idempotent: consecutive calls to `peek` with no intervening
//! change to the state or the stream must return the same result.
//!
//! For consumers, it is important to note that the peek method requires a mutable reference
//! to the stream. This is because some mutable processing may be needed in order to determine
//! what the next token is. For example:
//!
//! 1. When reading tokens from a file, peeking at the next token may involve reading more bytes
//!     from the file and thus mutating the file pointer. This mutations is easy to undo in
//!     general.
//!
//! 1. When performing expansion on a stream, the next token in the stream may need to be expanded
//!     rather than returned. The next token will be the first token in the expansion in this case,
//!     or the following token in the remaining stream if the expansion returns no tokens.
//!     This mutation is generally irreversable.
//!
use crate::error;
use crate::token::Token;

/// A `Stream` is a source of tokens that are possibly generated on demand.
///
/// See the module documentation for details.
pub trait Stream {
    /// Retrieves the next token in the stream.
    fn next(&mut self) -> anyhow::Result<Option<Token>>;

    /// Peeks at the next token in the stream.
    ///
    /// To peek using an immutable borrow of the stream, use the methods `prepare_imut_peek`
    /// and `imut_peek`.
    fn peek(&mut self) -> anyhow::Result<Option<&Token>>;

    /// Consumes the next token in the stream without returning it.
    ///
    /// This method is mostly to make code self-documenting. It is typically used in
    /// situations where a peek has already occurred, and the token itself is not needed.
    fn consume(&mut self) -> anyhow::Result<()> {
        self.next().map(|_| ())
    }
}

/// Removes the provided vector of tokens from the front of the stream.
///
/// Returns an error if the stream does not start with the tokens.
pub fn remove_tokens_from_stream(
    tokens: &[Token],
    stream: &mut dyn Stream,
    action: &str,
) -> anyhow::Result<()> {
    for prefix_token in tokens.iter() {
        let stream_token = match stream.next()? {
            None => {
                return Err(error::EndOfInputError::new(format![
                    "unexpected end of input while {}",
                    action
                ])
                .cast());
            }
            Some(token) => token,
        };
        if &stream_token != prefix_token {
            /*
            let note = match &prefix_token.value {
                ControlSequence(_) => {
                    format!["expected a control sequence token \\{}", "name"]
                }
                _ => format![ //Character(c, catcode) => format![
                    "expected a character token with value 'todo' and catcode todo",
                    //c, catcode
                ],
            };
             */
            let note = "todo";
            return Err(error::TokenError::new(
                stream_token,
                format!["unexpected token while {}", action],
            )
            .add_note(note)
            .cast());
        }
    }
    Ok(())
}

/// A `VecStream` is a stream consisting of a vector of tokens that are returned in order.
#[derive(Debug)]
pub struct VecStream {
    data: Vec<Token>,
}

impl VecStream {
    /// Returns a new `VecStream` consisting of the tokens in the provided vector.
    pub fn new(mut vec: Vec<Token>) -> VecStream {
        vec.reverse();
        VecStream { data: vec }
    }
}

impl Stream for VecStream {
    fn next(&mut self) -> anyhow::Result<Option<Token>> {
        Ok(self.data.pop())
    }

    fn peek(&mut self) -> anyhow::Result<Option<&Token>> {
        Ok(self.data.last())
    }
}
