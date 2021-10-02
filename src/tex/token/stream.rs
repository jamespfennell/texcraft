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
//! # use texcraft::tex::token::stream::VecStream;
//! # use texcraft::tex::token::stream::Stream;
//! # use texcraft::tex::token::Token;
//! # use texcraft::tex::token::catcode::CatCode;
//! let mut stream = VecStream::new(vec![
//!     Token::new_character('a', CatCode::Letter),
//!     Token::new_character('b', CatCode::Letter),
//!     Token::new_character('c', CatCode::Letter),
//! ]);
//!
//! assert_eq!(stream.next().unwrap(), Some(Token::new_character('a', CatCode::Letter)));
//! assert_eq!(stream.next().unwrap(), Some(Token::new_character('b', CatCode::Letter)));
//! assert_eq!(stream.next().unwrap(), Some(Token::new_character('c', CatCode::Letter)));
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
//! # use texcraft::tex::token::stream::VecStream;
//! # use texcraft::tex::token::stream::Stream;
//! # use texcraft::tex::token::Token;
//! # use texcraft::tex::token::catcode::CatCode;
//! let mut stream = VecStream::new(vec![
//!     Token::new_character('a', CatCode::Letter),
//!     Token::new_character('b', CatCode::Letter),
//!     Token::new_character('c', CatCode::Letter),
//! ]);
//!
//! assert_eq!(stream.peek().unwrap(), Some(&Token::new_character('a', CatCode::Letter)));
//! assert_eq!(stream.peek().unwrap(), Some(&Token::new_character('a', CatCode::Letter)));
//! assert_eq!(stream.next().unwrap(), Some(Token::new_character('a', CatCode::Letter)));
//! assert_eq!(stream.peek().unwrap(), Some(&Token::new_character('b', CatCode::Letter)));
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
use crate::tex::error;
use crate::tex::token::Token;
use crate::tex::token::Value::*;

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
            let note = match &prefix_token.value {
                Character(c, catcode) => format![
                    "expected a character token with value '{}' and catcode {}",
                    c, catcode
                ],
                ControlSequence(_, _) => {
                    format!["expected a control sequence token \\{}", "name"]
                }
            };
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
///
/// The implementation is optimized for empty streams and streams with a single token: these will
/// be stored entirely on the stack and will not incur a heap allocation.
///
/// A `VecStream` may be peeked at immutably without invoking `prepare_imut_peek` first.
#[derive(Debug)]
pub enum VecStream {
    Singleton(Option<Token>),
    Vector(Vec<Token>),
}

impl VecStream {
    /// Returns a new `VecStream` consisting of the tokens in the provided vector.
    pub fn new(mut vec: Vec<Token>) -> VecStream {
        vec.reverse();
        VecStream::Vector(vec)
    }

    /// Returns a new `VecStream` that is empty.
    /// ```
    /// # use texcraft::tex::token::stream::Stream;
    /// # use texcraft::tex::token::stream::VecStream;
    /// let mut s = VecStream::new_empty();
    /// assert_eq!(s.peek().unwrap(), None);
    /// ```
    pub fn new_empty() -> VecStream {
        VecStream::Singleton(None)
    }

    /// Returns a new `VecStream` consisting of a single token. This stream will be stored entirely
    /// on the stack.
    /// ```
    /// # use texcraft::tex::token::stream::Stream;
    /// # use texcraft::tex::token::stream::VecStream;
    /// # use texcraft::tex::token::{Token, Value};
    /// # use texcraft::tex::token::catcode::CatCode;
    /// let t = Token::new_character('a', CatCode::Letter);
    /// let mut s = VecStream::new_singleton(t.clone());
    /// assert_eq!(s.peek().unwrap(), Some(&t));
    /// assert_eq!(s.next().unwrap(), Some(t));
    /// assert_eq!(s.peek().unwrap(), None);
    /// assert_eq!(s.next().unwrap(), None);
    /// ```
    pub fn new_singleton(t: Token) -> VecStream {
        VecStream::Singleton(Some(t))
    }
}

impl Stream for VecStream {
    fn next(&mut self) -> anyhow::Result<Option<Token>> {
        Ok(match self {
            VecStream::Singleton(t) => t.take(),
            VecStream::Vector(v) => v.pop(),
        })
    }

    fn peek(&mut self) -> anyhow::Result<Option<&Token>> {
        Ok(match self {
            VecStream::Singleton(t) => t.as_ref(),
            VecStream::Vector(v) => v.last(),
        })
    }
}
