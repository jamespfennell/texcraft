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
//! # use texcraft::tex::token::stream::VecStream;
//! # use texcraft::tex::token::stream::Stream;
//! # use texcraft::tex::token::Token;
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
//! # Immutable peeking
//!
//! Requiring a mutable borrow to peek at the stream is problematical for stream consumers.
//! The following code seems entirely reasonable: the stream is peeked at, and a lookup occurs
//! to check if the immutable token corresponds to an expansion control sequence:
//! ```compile_fail
//! let state_and_stream: dyn StateAndStream<State> = // some state and stream
//! let token = state_and_stream.stream().peek();
//! let is_expansion_command = match token.value {
//!     None => false,
//!     Some(ref name) => {
//!         state_and_stream.state().get_expansion_command(name).is_value()
//!     }
//! }
//! ```
//! This however does not compile: searching in the expansion map requires an immutable borrow
//! of the state (and thus the `StateAndStream`) which the borrow checker forbids because the
//! immutable reference to the token is keeping the mutable borrow of the stream alive.
//!
//! However, idempotency of the `peek` method means that in general there should be possible
//! to retrieve an immutable reference to the next token if a mutable peek has already occured.
//! In the worst case, stream implementations can maintain an internal cache of the next token,
//! populate this cache on the first peek, and then return an immutable refernce to this cache
//! on subsequent peeks.
//!
//! This idea is implemetented with a pair of methods on the stream trait:
//! `prepare_imut_peek` and `imut_peek`. The first method takes a mutable reference to the
//! stream and performs all necessary mutable processing to generate the next token in the
//! stream. The second method then returns an immutable reference to this generated token:
//! ```
//!
//! ```
//! The previous code sample can now be made to work:
//! ```
//!
//! ```
//!
//! ## Restrictions on immutable peeking
//!
//! In general, it is an error to invoke `imut_peek` without first invoking `prepare_imut_peek`.
//! Stream implementations which require mutation before peeking (i.e., have a non-empty
//! implementation of `prepare_imut_peek`) should always return an error from imut_peek` if the
//! prepare function has not been called first.
//!
//! It is also an error to mutate state between `prepare_imut_peek` and `imut_peek`. The borrow
//! checker will sometimes detect this situation. Stream implementations are not expected to
//! error in this case because detecting state changes is expensive.
//!
//! For some stream implementations, like `VecStream`, it is admissible to skip
//! `prepare_imut_state`. This exception is on an per-implementation basis.

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
    fn peek(&mut self) -> anyhow::Result<Option<&Token>> {
        self.prepare_imut_peek()?;
        self.imut_peek()
    }

    /// Performs any mutations needed so as to be able to peek using an immutable borrow
    /// of the stream. See the module documentation for information on why this method
    /// exists.
    fn prepare_imut_peek(&mut self) -> anyhow::Result<()> {
        Ok(())
    }

    /// Immutably peeks at the next token in the stream. The method `prepare_imut_peek`
    /// *must* be invoked before this function, otherwise an error will be returned.
    ///
    /// The name of this method is intentionally janky so that consumers think
    /// twice before using it.
    fn imut_peek(&self) -> anyhow::Result<Option<&Token>>;

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
                ControlSequence(_, name) => {
                    format!["expected a control sequence token \\{}", name]
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
    /// let t = Token::new_letter('a');
    /// let mut s = VecStream::new_singleton(t.clone());
    /// assert_eq!(s.imut_peek().unwrap(), Some(&t));
    /// assert_eq!(s.next().unwrap(), Some(t));
    /// assert_eq!(s.imut_peek().unwrap(), None);
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

    fn imut_peek(&self) -> anyhow::Result<Option<&Token>> {
        Ok(match self {
            VecStream::Singleton(t) => t.as_ref(),
            VecStream::Vector(v) => v.last(),
        })
    }
}

/// A StackStream is a stream consisting of a stack of other streams. The next token is read from
/// the first stream in the stack that is still returning tokens.
/// ```
/// # use texcraft::tex::token::stream::StackStream;
/// # use texcraft::tex::token::stream::Stream;
/// # use texcraft::tex::token::stream::VecStream;
/// # use texcraft::tex::token::Token;
/// let token_1 = Token::new_letter('a');
/// let stream_1 = VecStream::new(vec![token_1.clone()]);
/// let token_2 = Token::new_letter('b');
/// let stream_2 = VecStream::new(vec![token_2.clone()]);
///
/// let mut stack_stream = StackStream::new();
/// stack_stream.push(stream_1);
/// stack_stream.push(stream_2);
///
/// assert_eq!(stack_stream.next().unwrap(), Some(token_2));
/// assert_eq!(stack_stream.next().unwrap(), Some(token_1));
/// assert_eq!(stack_stream.next().unwrap(), None);
/// ```
pub struct StackStream<T: Stream> {
    stack: Vec<T>,
}

impl<T: Stream> StackStream<T> {
    /// Push a new stream onto the top of the stack.
    pub fn push(&mut self, stream: T) {
        self.stack.push(stream)
    }

    /// Return a mutable reference to the underlying stack of streams
    pub fn stack_mut(&mut self) -> &mut Vec<T> {
        &mut self.stack
    }

    /// Create a new empty stack stream.
    pub fn new() -> StackStream<T> {
        StackStream { stack: Vec::new() }
    }
}

impl<T: Stream> Stream for StackStream<T> {
    fn next(&mut self) -> anyhow::Result<Option<Token>> {
        self.prepare_imut_peek()?;
        match self.stack.last_mut() {
            None => Ok(None),
            Some(stream) => stream.next(),
        }
    }

    fn prepare_imut_peek(&mut self) -> anyhow::Result<()> {
        loop {
            match self.stack.last_mut() {
                None => return Ok(()),
                Some(top) => match top.peek()? {
                    None => {
                        self.stack.pop();
                        continue;
                    }
                    Some(..) => return Ok(()),
                },
            }
        }
    }

    fn imut_peek(&self) -> anyhow::Result<Option<&Token>> {
        match self.stack.last() {
            None => Ok(None),
            Some(stream) => stream.imut_peek(),
        }
    }
}

impl<T: Stream> Default for StackStream<T> {
    fn default() -> Self {
        Self::new()
    }
}
