use crate::command;
use crate::runtime;
use crate::token::trace;
use crate::token::Token;
use crate::variable;

use super::BaseState;

/// A stream of tokens generated on demand.
///
/// This trait describes a general stream of tokens where the front of the stream may
/// retrieved using [TokenStream::next] or peeked at using [TokenStream::peek].
/// In practice, all [TokenStreams](TokenStream) in Texlang
/// are either [ExecutionInput], [ExpansionInput] or [UnexpandedStream]. We have a trait
/// only to make handling of these types uniform.
///
/// # Note on lazy loading
///
/// The simplest example of a stream is a vector of tokens. However, streams are more general
/// than this and can encompass situations in which the full contents cannot be determined in
/// advance. This can be thought of as "lazy loading" for the tokens.
/// The classic example of this kind of stream comes from the following LaTeX
/// snippet:
/// ```tex
/// \makeatletter \do@
/// ```
/// Assuming the default TeX catcode map, if we were to parse this input all at once we would
/// get three tokens: the control sequence `makeatletter`, the control sequence `do`, and a
/// single character token with value `@` and catcode "other". This is not the correct result,
/// though: the first control sequence changes the tokenization rules such that `@` is now
/// an admissible character in the name of a control sequence. The correct input is thus
/// the control sequence `makeatletter` followed by the control sequence `do@`.
pub trait TokenStream {
    /// Gets the next token in the stream.
    ///
    /// This method is almost the same
    /// as the `next` method in Rust's iterator trait, except a stream can return an error.
    ///
    /// As with iterators, a result of `Ok(None)` indicates that the stream is exhausted.
    fn next(&mut self) -> anyhow::Result<Option<Token>>;

    /// Peeks at the next token in the stream without removing it.
    ///
    /// In many sitations it is necessary to examine the next token without consuming it.
    /// For example when reading an integer from a stream, one needs to peek at the next token
    /// to see if it is a digit and thus extends the currently parsed interger.
    /// Consuming the token with [TokenStream::next] is not
    /// correct in this situation if the token is not a digit.
    ///
    /// The [TokenStream::peek] method must be idempotent: consecutive calls to `peek` with no intervening
    /// change to the state or the stream must return the same result.
    ///
    /// For consumers, it is important to note that the peek method requires a mutable reference
    /// to the stream. This is because some mutable processing may be needed in order to determine
    /// what the next token is. For example:
    ///
    /// 1. When reading tokens from a file, peeking at the next token may involve reading more bytes
    ///     from the file and thus mutating the file pointer. This mutations is easy to undo in
    ///     general.
    ///
    /// 1. When performing expansion on a stream, the next token in the stream may need to be expanded
    ///     rather than returned. The next token will be the first token in the expansion in this case,
    ///     or the following token in the remaining stream if the expansion returns no tokens.
    ///     This mutation is generally irreversable.
    fn peek(&mut self) -> anyhow::Result<Option<&Token>>;

    /// Consumes the next token in the stream without returning it.
    ///
    /// This method is mostly to make code self-documenting. It is typically used in
    /// situations where a peek has already occurred, and the token itself is not needed.
    fn consume(&mut self) -> anyhow::Result<()> {
        self.next().map(|_| ())
    }
}

/// Trait indicating a type has read only access to the environment.
pub trait HasEnv {
    // The type of the custom state in the environment.
    type S;

    /// Returns a reference to the environment.
    fn env(&self) -> &runtime::Env<Self::S>;

    /// Returns a reference to the base state.
    #[inline]
    fn base(&self) -> &runtime::BaseState<Self::S> {
        &self.env().base_state
    }

    // Returns a reference to the custom state.
    #[inline]
    fn state(&self) -> &Self::S {
        &self.env().custom_state
    }

    fn trace(&self, token: Token) -> trace::Trace {
        self.env()
            .internal
            .tracer
            .trace(token, &self.env().internal.cs_name_interner)
    }
}

/// A [TokenStream] that performs expansion.
///
/// The unexpanded tokens are retrieved from the unexpanded stream returned by the
/// [unexpanded](ExpandedStream::unexpanded) method.
pub trait ExpandedStream {
    /// The type of the state.
    type S;

    /// Returns the underlying unexpanded stream.
    fn unexpanded(&mut self) -> &mut UnexpandedStream<Self::S>;

    /// Expand the next token in the input.
    ///
    /// This method only expands a single token. If, after the expansion, the next token
    /// is expandable it will not be expanded.
    fn expand_once(&mut self) -> anyhow::Result<bool> {
        stream::expand_once(&mut self.unexpanded().0)
    }
}

impl<T: ExpandedStream> TokenStream for T {
    #[inline]
    fn next(&mut self) -> anyhow::Result<Option<Token>> {
        stream::next_expanded(&mut self.unexpanded().0)
    }

    #[inline]
    fn peek(&mut self) -> anyhow::Result<Option<&Token>> {
        stream::peek_expanded(&mut self.unexpanded().0)
    }
}

/// Trait indicating some part of the state is mutably accesible to expansion commands.
///
/// In general only execution commands can mutate the state.
/// Expansion commands cannot.
/// This is important for maintaining the rough invariant that commands either modify
/// the state (execution) or modify the token stream (expansion)
/// but not both.
/// To enforce this, the [ExpansionInput] type does not have a way of returning a mutable
/// reference to the state.
///
/// However, there are special situations in which expansion commands do need to maintain
/// some mutable state.
/// Currently, the only example is the collection of conditional commands
/// in the standard libary (`\ifodd`, `\else`, `\fi`, etc.).
/// These commands maintain a record of the conditional brances that were taken
///     and uses this for error reporting.
///
/// This trait enables this use case.
/// The part of the state that can be mutated by expansion commands is called the expansion state.
/// If the state implements this trait, a mutable reference to the expansion state
/// can be obtained through the [ExpansionInput] type's [expansion_state_mut](ExpansionInput::expansion_state_mut) method.
///
/// The standard library's `StdLibExpansionState` is an example of this pattern.
pub trait HasExpansionState {
    /// The type of the expansion state.
    type E;

    /// Returns a mutable reference to the expansion state.
    fn expansion_state_mut(&mut self) -> &mut Self::E;
}

/// Stream that returns input tokens without performing expansion.
///
/// The unexpanded stream is used when reading tokens without performing expansion;
/// e.g., when reading the replacement text for a macro defined using `\def`.
///
/// It be obtained from either the [ExecutionInput] or the [ExpansionInput]
/// using the [ExpandedStream] trait methods.
#[repr(transparent)]
pub struct UnexpandedStream<S>(runtime::Env<S>);

impl<S> TokenStream for UnexpandedStream<S> {
    #[inline]
    fn next(&mut self) -> anyhow::Result<Option<Token>> {
        stream::next_unexpanded(&mut self.0)
    }

    #[inline]
    fn peek(&mut self) -> anyhow::Result<Option<&Token>> {
        stream::peek_unexpanded(&mut self.0)
    }
}

/// Input type for expansion primitives.
///
/// This type provides:
///
/// - Access to the input stream (with or without expansion). Its implementation of the [TokenStream]
///     trait returns expanded tokens.
///     To read the input strean without performing expansion, use the
///     [unexpanded](ExpandedStream::unexpanded) method.
///
/// - Read only access to the environment using the [HasEnv] trait.
///
/// - Mutable access to the expansion state if the underlying state has such a state.
///     This is related to the [HasExpansionState] trait.
///
/// - The ability to push source code or token expansions to the front of the input stream.
///     For source code use [ExpansionInput::push_source];
///     for tokens use [ExpansionInput::push_expansion] or [ExpansionInput::expansions_mut].
///
/// - Access to scratch space using the [ExpansionInput::scratch_space] and
///     [ExpansionInput::return_scratch_space] methods.
///
/// This type is also used in the parsing code for situations where both an
/// [ExpansionInput] or [ExecutionInput] is accepted. We use this type because
/// it has only read access to the env, and so casting does not escalate priviliges.
#[repr(transparent)]
pub struct ExpansionInput<S>(runtime::Env<S>);

impl<S> HasEnv for ExpansionInput<S> {
    type S = S;

    #[inline]
    fn env(&self) -> &runtime::Env<S> {
        &self.0
    }
}

impl<S> ExpandedStream for ExpansionInput<S> {
    type S = S;

    #[inline]
    fn unexpanded(&mut self) -> &mut UnexpandedStream<S> {
        unsafe { &mut *(self as *mut ExpansionInput<S> as *mut UnexpandedStream<S>) }
    }
}

impl<S> std::convert::AsMut<ExpansionInput<S>> for ExpansionInput<S> {
    fn as_mut(&mut self) -> &mut ExpansionInput<S> {
        self
    }
}

impl<S: HasExpansionState> ExpansionInput<S> {
    #[inline]
    pub fn expansion_state_mut(&mut self) -> &mut S::E {
        self.0.custom_state.expansion_state_mut()
    }
}

impl<S> ExpansionInput<S> {
    /// Creates a mutable reference to this type from the [Env](runtime::Env) type.
    #[inline]
    pub fn new(env: &mut runtime::Env<S>) -> &mut ExpansionInput<S> {
        unsafe { &mut *(env as *mut runtime::Env<S> as *mut ExpansionInput<S>) }
    }

    /// Push source code to the front of the input stream.
    #[inline]
    pub fn push_source(&mut self, file_name: String, source_code: String) -> anyhow::Result<()> {
        self.0.push_source(file_name, source_code)
    }

    /// Push tokens to the front of the input stream.
    ///
    /// The first token in the provided slice will be the next token read.
    #[inline]
    pub fn push_expansion(&mut self, expansion: &[Token]) {
        self.0.internal.push_expansion(expansion)
    }

    /// Returns a mutable reference to the expanded tokens stack for the current input source.
    ///
    /// The tokens are a stack, so the next token is the last token in the vector.
    ///
    /// Adding tokens to the front of the input using this method can be more efficient
    /// than using [ExpansionInput::push_expansion] because an allocation is avoided.
    #[inline]
    pub fn expansions_mut(&mut self) -> &mut Vec<Token> {
        self.0.internal.expansions_mut()
    }

    /// Gets a vector of tokens that may be used as scratch space.
    ///
    /// By reusing the same vector of tokens for scratch space we can avoid allocating a new
    /// vector each time we need it. This was originally created for the TeX macros system.
    ///
    /// When finished with the vector, return it using [return_scratch_space](ExpansionInput::return_scratch_space).
    pub fn scratch_space(&mut self) -> Vec<Token> {
        // Note: the scratch space system here makes the benchmarks about 3% slower versus the previous system.
        // In the previous system a mutable reference to the scratch space was returned with the unexpanded
        // stream and so no swapping occured. If 3% is a big deal, we can bring it back.
        let mut s = Vec::new();
        std::mem::swap(&mut s, &mut self.0.internal.scratch_space);
        s
    }

    /// Return scratch space, allowing it to be reused.
    pub fn return_scratch_space(&mut self, mut scratch_space: Vec<Token>) {
        if scratch_space.len() < self.0.internal.scratch_space.len() {
            return;
        }
        scratch_space.clear();
        std::mem::swap(&mut scratch_space, &mut self.0.internal.scratch_space);
    }
}

/// Input type for execution primitives.
///
/// This type provides:
///
/// - Access to the input stream (with or without expansion). Its implementation of the [TokenStream]
///     trait returns expanded tokens.
///     To read the input strean without performing expansion, use the
///     [unexpanded](ExpandedStream::unexpanded) method.
///
/// - Read only access to the environment using the [HasEnv] trait.
///
/// - Mutable access to the base state and custom state using the [ExecutionInput::base]
///     and [ExecutionInput::state] methods.
#[repr(transparent)]
pub struct ExecutionInput<S>(runtime::Env<S>);

impl<S> HasEnv for ExecutionInput<S> {
    type S = S;

    #[inline]
    fn env(&self) -> &runtime::Env<S> {
        &self.0
    }
}

impl<S> ExpandedStream for ExecutionInput<S> {
    type S = S;

    #[inline]
    fn unexpanded(&mut self) -> &mut UnexpandedStream<S> {
        unsafe { &mut *(self as *mut ExecutionInput<S> as *mut UnexpandedStream<S>) }
    }
}

impl<S> ExecutionInput<S> {
    /// Creates a mutable reference to this type from the [Env](runtime::Env) type.
    #[inline]
    pub fn new(state: &mut runtime::Env<S>) -> &mut ExecutionInput<S> {
        unsafe { &mut *(state as *mut runtime::Env<S> as *mut ExecutionInput<S>) }
    }

    /// Returns a mutable reference to the base state.
    #[inline]
    pub fn base_mut(&mut self) -> &mut runtime::BaseState<S> {
        &mut self.0.base_state
    }

    /// Returns a mutable reference to the custom state.
    #[inline]
    pub fn state_mut(&mut self) -> &mut S {
        &mut self.0.custom_state
    }

    // TODO: pass in the token and keep it as a reference
    pub fn begin_group(&mut self) {
        self.0.begin_group()
    }

    // TODO: error if there is not active group
    pub fn end_group(&mut self) {
        self.0.end_group()
    }

    pub fn groups(&mut self) -> &mut [variable::RestoreValues<S>] {
        &mut self.0.internal.groups
    }

    pub fn current_group_mut(
        &mut self,
    ) -> Option<(&mut variable::RestoreValues<S>, &BaseState<S>, &S)> {
        match self.0.internal.groups.last_mut() {
            None => None,
            Some(g) => Some((g, &self.0.base_state, &self.0.custom_state)),
        }
    }
}

impl<S> std::convert::AsMut<ExpansionInput<S>> for ExecutionInput<S> {
    fn as_mut(&mut self) -> &mut ExpansionInput<S> {
        unsafe { &mut *(self as *mut ExecutionInput<S> as *mut ExpansionInput<S>) }
    }
}

/// Strips the lifetime from the token.
///
/// This function is intended to get around limitations of the borrow checker only. It
/// should only be used when the code is actually fine but the borrow checker is being
/// too conservative. Don't do anything fancy.
///
/// See this question for the type of code this function is designed for:
/// https://stackoverflow.com/questions/69680201/is-this-use-of-unsafe-trivially-safe
#[inline]
unsafe fn launder<'a, 'b>(token: &'a Token) -> &'b Token {
    &*(token as *const Token)
}

mod stream {
    use super::*;
    use crate::token::Value::ControlSequence;

    #[inline]
    pub fn next_unexpanded<S>(env: &mut runtime::Env<S>) -> anyhow::Result<Option<Token>> {
        if let Some(token) = env.internal.current_source.expansions.pop() {
            return Ok(Some(token));
        }
        if let Some(token) = env.internal.current_source.root.next(
            &env.base_state.cat_code_map,
            &mut env.internal.cs_name_interner,
        )? {
            return Ok(Some(token));
        }
        next_unexpanded_recurse(env)
    }

    fn next_unexpanded_recurse<S>(env: &mut runtime::Env<S>) -> anyhow::Result<Option<Token>> {
        if env.internal.pop_source() {
            next_unexpanded(env)
        } else {
            Ok(None)
        }
    }

    #[inline]
    pub fn peek_unexpanded<S>(env: &mut runtime::Env<S>) -> anyhow::Result<Option<&Token>> {
        if let Some(token) = env.internal.current_source.expansions.last() {
            return Ok(Some(unsafe { launder(token) }));
        }
        if let Some(token) = env.internal.current_source.root.next(
            &env.base_state.cat_code_map,
            &mut env.internal.cs_name_interner,
        )? {
            env.internal.current_source.expansions.push(token);
            return Ok(env.internal.current_source.expansions.last());
        }
        peek_unexpanded_recurse(env)
    }

    fn peek_unexpanded_recurse<S>(env: &mut runtime::Env<S>) -> anyhow::Result<Option<&Token>> {
        if env.internal.pop_source() {
            peek_unexpanded(env)
        } else {
            Ok(None)
        }
    }

    pub fn next_expanded<S>(env: &mut runtime::Env<S>) -> anyhow::Result<Option<Token>> {
        let (token, command) = match next_unexpanded(env)? {
            None => return Ok(None),
            Some(token) => match token.value() {
                ControlSequence(name) => (token, env.base_state.commands_map.get(&name)),
                _ => return Ok(Some(token)),
            },
        };
        match command {
            Some(command::Command::Expansion(command)) => {
                let command = *command;
                let output = command.call(token, ExpansionInput::new(env))?;
                env.internal.push_expansion(&output);
                next_expanded(env)
            }
            Some(command::Command::Macro(command)) => {
                let command = command.clone();
                command.call(token, ExpansionInput::new(env))?;
                next_expanded(env)
            }
            _ => Ok(Some(token)),
        }
    }

    pub fn peek_expanded<S>(env: &mut runtime::Env<S>) -> anyhow::Result<Option<&Token>> {
        let (token, command) = match peek_unexpanded(env)? {
            None => return Ok(None),
            Some(token) => match token.value() {
                ControlSequence(name) => (
                    unsafe { launder(token) },
                    env.base_state.commands_map.get(&name),
                ),
                _ => return Ok(Some(unsafe { launder(token) })),
            },
        };
        match command {
            Some(command::Command::Expansion(command)) => {
                let command = *command;
                let token = *token;
                let _ = next_unexpanded(env);
                let output = command.call(token, ExpansionInput::new(env))?;
                env.internal.push_expansion(&output);
                peek_expanded(env)
            }
            Some(command::Command::Macro(command)) => {
                let command = command.clone();
                let token = *token;
                let _ = next_unexpanded(env);
                command.call(token, ExpansionInput::new(env))?;
                peek_expanded(env)
            }
            _ => Ok(Some(unsafe { launder(token) })),
        }
    }

    pub fn expand_once<S>(env: &mut runtime::Env<S>) -> anyhow::Result<bool> {
        let (token, command) = match peek_unexpanded(env)? {
            None => return Ok(false),
            Some(token) => match token.value() {
                ControlSequence(name) => (
                    unsafe { launder(token) },
                    env.base_state.commands_map.get(&name),
                ),
                _ => return Ok(false),
            },
        };
        match command {
            Some(command::Command::Expansion(command)) => {
                let command = *command;
                let token = *token;
                let _ = next_unexpanded(env);
                let output = command.call(token, ExpansionInput::new(env))?;
                env.internal.push_expansion(&output);
                Ok(true)
            }
            Some(command::Command::Macro(command)) => {
                let command = command.clone();
                let token = *token;
                let _ = next_unexpanded(env);
                command.call(token, ExpansionInput::new(env))?;
                Ok(true)
            }
            _ => Ok(false),
        }
    }
}
