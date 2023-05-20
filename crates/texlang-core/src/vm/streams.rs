use crate::command;
use crate::token::trace;
use crate::token::Token;
use crate::variable;
use crate::vm;

use super::BaseState;

/// A stream of tokens generated on demand.
///
/// This trait describes a general stream of tokens where the front of the stream may
/// retrieved using [TokenStream::next] or peeked at using [TokenStream::peek].
/// In practice, all [TokenStreams](TokenStream) in Texlang
/// are either [ExecutionInput], [ExpansionInput] or [UnexpandedStream].
/// This trait exists to allow a generic function to accept any of these types.
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
    /// The type of the custom state in the VM.
    type S;

    /// Gets the next token in the stream.
    ///
    /// This method is almost the same
    /// as the `next` method in Rust's iterator trait, except a stream can return an error.
    ///
    /// As with iterators, a result of `Ok(None)` indicates that the stream is exhausted.
    fn next(&mut self) -> anyhow::Result<Option<Token>>;

    /// Peeks at the next token in the stream without removing it.
    ///
    /// In many situations it is necessary to examine the next token without consuming it.
    /// For example when reading an integer from a stream, one needs to peek at the next token
    /// to see if it is a digit and thus extends the currently parsed integer.
    /// Consuming the token with [TokenStream::next] is not
    /// correct in this situation if the token is not a digit.
    ///
    /// For consumers, it is important to note that the peek method requires a mutable reference
    /// to the stream. This is because some mutable processing may be needed in order to determine
    /// what the next token is. For example:
    ///
    /// 1. When reading tokens from a file, peeking at the next token may involve reading more bytes
    ///     from the file and thus mutating the file pointer. sThis mutations is easy to undo in
    ///     general.
    ///
    /// 1. When performing expansion on a stream, the next token in the stream may need to be expanded
    ///     rather than returned. The next token will be the first token in the expansion in this case,
    ///     or the following token in the remaining stream if the expansion returns no tokens.
    ///     This mutation is generally irreversible.
    fn peek(&mut self) -> anyhow::Result<Option<&Token>>;

    /// Consumes the next token in the stream without returning it.
    ///
    /// This method is mostly to make code self-documenting. It is typically used in
    /// situations where a peek has already occurred, and the token itself is not needed.
    fn consume(&mut self) -> anyhow::Result<()> {
        self.next().map(|_| ())
    }

    /// Returns a reference to the VM.
    fn vm(&self) -> &vm::VM<Self::S>;

    /// Returns a reference to the base state.
    #[inline]
    fn base(&self) -> &vm::BaseState<Self::S> {
        &self.vm().base_state
    }

    /// Returns a reference to the custom state.
    #[inline]
    fn state(&self) -> &Self::S {
        &self.vm().custom_state
    }

    fn trace(&self, token: Token) -> trace::Trace {
        self.vm().trace(token)
    }

    fn trace_end_of_input(&self) -> trace::Trace {
        self.vm().internal.tracer.trace_end_of_input()
    }
}

/// A [TokenStream] that performs expansion.
///
/// The unexpanded tokens are retrieved from the unexpanded stream returned by the
/// [unexpanded](ExpandedStream::unexpanded) method.
#[repr(transparent)]
pub struct ExpandedStream<S>(UnexpandedStream<S>);

impl<S> std::convert::AsMut<ExpandedStream<S>> for ExpandedStream<S> {
    fn as_mut(&mut self) -> &mut ExpandedStream<S> {
        self
    }
}

impl<S> ExpandedStream<S> {
    /// Returns the underlying unexpanded stream.
    pub fn unexpanded(&mut self) -> &mut UnexpandedStream<S> {
        &mut self.0
    }

    /// Expand the next token in the input.
    ///
    /// This method only expands a single token. If, after the expansion, the next token
    /// is expandable it will not be expanded.
    pub fn expand_once(&mut self) -> anyhow::Result<bool> {
        stream::expand_once(&mut self.unexpanded().0)
    }
}

impl<S> TokenStream for ExpandedStream<S> {
    type S = S;

    #[inline]
    fn next(&mut self) -> anyhow::Result<Option<Token>> {
        stream::next_expanded(&mut self.unexpanded().0)
    }

    #[inline]
    fn peek(&mut self) -> anyhow::Result<Option<&Token>> {
        stream::peek_expanded(&mut self.unexpanded().0)
    }

    #[inline]
    fn vm(&self) -> &vm::VM<Self::S> {
        &self.0 .0
    }
}

/// Stream that returns input tokens without performing expansion.
///
/// The unexpanded stream is used when reading tokens without performing expansion;
/// e.g., when reading the replacement text for a macro defined using `\def`.
///
/// It be obtained from either the [ExecutionInput] or the [ExpansionInput]
/// using the [ExpandedStream] trait methods.
#[repr(transparent)]
pub struct UnexpandedStream<S>(vm::VM<S>);

impl<S> TokenStream for UnexpandedStream<S> {
    type S = S;

    #[inline]
    fn next(&mut self) -> anyhow::Result<Option<Token>> {
        stream::next_unexpanded(&mut self.0)
    }

    #[inline]
    fn peek(&mut self) -> anyhow::Result<Option<&Token>> {
        stream::peek_unexpanded(&mut self.0)
    }

    #[inline]
    fn vm(&self) -> &vm::VM<S> {
        &self.0
    }
}

/// Input type for expansion primitives.
///
/// This type provides:
///
/// - Access to the input stream (with or without expansion). Its implementation of the [TokenStream]
///     trait returns expanded tokens.
///     To read the input stream without performing expansion, use the
///     [unexpanded](ExpandedStream::unexpanded) method.
///
/// - Read only access to the VM.
///
/// - The ability to push source code or token expansions to the front of the input stream.
///     For source code use [ExpansionInput::push_source];
///     for tokens use [ExpansionInput::push_expansion] or [ExpansionInput::expansions_mut].
///
/// - Access to token buffers using the [ExpansionInput::checkout_token_buffer] and
///     [ExpansionInput::return_token_buffer] methods.
///
/// This type is also used in the parsing code for situations where both an
/// [ExpansionInput] or [ExecutionInput] is accepted. We use this type because
/// it has only read access to the VM, and so casting does not escalate privileges.
#[repr(transparent)]
// TODO: shouldn't this be in the command module no vm module?
// TODO: it should wrap the ExpandedStream
pub struct ExpansionInput<S>(ExpandedStream<S>);

impl<S> std::convert::AsMut<ExpandedStream<S>> for ExpansionInput<S> {
    fn as_mut(&mut self) -> &mut ExpandedStream<S> {
        &mut self.0
    }
}

impl<S> TokenStream for ExpansionInput<S> {
    type S = S;

    fn next(&mut self) -> anyhow::Result<Option<Token>> {
        self.0.next()
    }

    fn peek(&mut self) -> anyhow::Result<Option<&Token>> {
        self.0.peek()
    }

    fn vm(&self) -> &vm::VM<Self::S> {
        self.0.vm()
    }
}

impl<S> ExpansionInput<S> {
    /// Creates a mutable reference to this type from the [VM](vm::VM) type.
    #[inline]
    pub fn new(vm: &mut vm::VM<S>) -> &mut ExpansionInput<S> {
        unsafe { &mut *(vm as *mut vm::VM<S> as *mut ExpansionInput<S>) }
    }

    /// Push source code to the front of the input stream.
    #[inline]
    pub fn push_source(
        &mut self,
        token: Token,
        file_name: String,
        source_code: String,
    ) -> anyhow::Result<()> {
        self.0 .0 .0.internal.push_source(
            Some(token),
            file_name,
            source_code,
            self.0 .0 .0.base_state.max_input_levels,
        )
    }

    #[inline]
    pub fn unexpanded(&mut self) -> &mut UnexpandedStream<S> {
        &mut self.0 .0
    }

    #[inline]
    pub fn expanded(&mut self) -> &mut ExpandedStream<S> {
        &mut self.0
    }

    /// Push tokens to the front of the input stream.
    ///
    /// The first token in the provided slice will be the next token read.
    #[inline]
    pub fn push_expansion(&mut self, expansion: &[Token]) {
        self.0 .0 .0.internal.push_expansion(expansion)
    }

    /// Returns a reference to the expanded tokens stack for the current input source.
    ///
    /// The tokens are a stack, so the next token is the last token in the vector.
    ///
    /// Adding tokens to the front of the input using this method can be more efficient
    /// than using [ExpansionInput::push_expansion] because an allocation is avoided.
    #[inline]
    pub fn expansions(&self) -> &Vec<Token> {
        self.0 .0 .0.internal.expansions()
    }

    /// Returns a mutable reference to the expanded tokens stack for the current input source.
    ///
    /// The tokens are a stack, so the next token is the last token in the vector.
    ///
    /// Adding tokens to the front of the input using this method can be more efficient
    /// than using [ExpansionInput::push_expansion] because an allocation is avoided.
    #[inline]
    pub fn expansions_mut(&mut self) -> &mut Vec<Token> {
        self.0 .0 .0.internal.expansions_mut()
    }

    /// Returns a vector than can be used as a token buffer, potentially without allocating memory.
    ///
    /// The returned vector is empty, but will generally have non-zero capacity from previous uses of the buffer.
    /// Reusing the allocated memory results in fewer allocations overall.
    /// This buffer mechanism was first introduced in a successful attempt to improve the performance of the
    /// TeX macros implementation.
    ///
    /// When finished with the buffer, please return it using [return_token_buffer](ExpansionInput::return_token_buffer).
    ///
    /// This API may feel a bit awkward - it would seem nicer to return a mutable reference to a buffer instead.
    /// Doing this while keeping the borrow checker happy is very difficult and (as is often the case) for good reason.
    /// Token buffers are often used in macro expansion, and at any point in time multiple macros may be in
    ///     the process of expansion.
    /// This getting "the" token buffer to use for expansion would be incorrect, as the multiple expansions
    /// would step on each other.
    pub fn checkout_token_buffer(&mut self) -> Vec<Token> {
        self.0
             .0
             .0
            .internal
            .token_buffers
            .pop()
            .unwrap_or_default()
            .0
    }

    /// Return a token buffer, allowing it to be reused.
    pub fn return_token_buffer(&mut self, mut token_buffer: Vec<Token>) {
        token_buffer.clear();
        self.0
             .0
             .0
            .internal
            .token_buffers
            .push(super::TokenBuffer(token_buffer))
    }
}

/// Input type for execution primitives.
///
/// This type provides:
///
/// - Access to the input stream (with or without expansion). Its implementation of the [TokenStream]
///     trait returns expanded tokens.
///     To read the input stream without performing expansion, use the
///     [unexpanded](ExpandedStream::unexpanded) method.
///
/// - Mutable access to the base state and custom state using the [ExecutionInput::base_mut]
///     and [ExecutionInput::state_mut] methods.
#[repr(transparent)]
pub struct ExecutionInput<S>(ExpandedStream<S>);

impl<S> std::convert::AsMut<ExpandedStream<S>> for ExecutionInput<S> {
    fn as_mut(&mut self) -> &mut ExpandedStream<S> {
        &mut self.0
    }
}

impl<S> TokenStream for ExecutionInput<S> {
    type S = S;

    fn next(&mut self) -> anyhow::Result<Option<Token>> {
        self.0.next()
    }

    fn peek(&mut self) -> anyhow::Result<Option<&Token>> {
        self.0.peek()
    }

    fn vm(&self) -> &vm::VM<Self::S> {
        self.0.vm()
    }
}

impl<S> ExecutionInput<S> {
    /// Creates a mutable reference to this type from the [VM](vm::VM) type.
    #[inline]
    pub fn new(state: &mut vm::VM<S>) -> &mut ExecutionInput<S> {
        unsafe { &mut *(state as *mut vm::VM<S> as *mut ExecutionInput<S>) }
    }

    #[inline]
    pub fn unexpanded(&mut self) -> &mut UnexpandedStream<S> {
        &mut self.0 .0
    }

    /// Returns a mutable reference to the base state.
    #[inline]
    pub fn base_mut(&mut self) -> &mut vm::BaseState<S> {
        &mut self.0 .0 .0.base_state
    }

    /// Returns a mutable reference to the custom state.
    #[inline]
    pub fn state_mut(&mut self) -> &mut S {
        &mut self.0 .0 .0.custom_state
    }

    // TODO: pass in the token and keep it as a reference
    pub fn begin_group(&mut self) {
        self.0 .0 .0.begin_group()
    }

    pub fn end_group(&mut self, token: Token) -> anyhow::Result<()> {
        self.0 .0 .0.end_group(token)
    }

    pub(crate) fn groups(&mut self) -> &mut [variable::RestoreValues<S>] {
        &mut self.0 .0 .0.internal.groups
    }

    pub(crate) fn current_group_mut(
        &mut self,
    ) -> Option<(&mut variable::RestoreValues<S>, &BaseState<S>, &S)> {
        match self.0 .0 .0.internal.groups.last_mut() {
            None => None,
            Some(g) => Some((g, &self.0 .0 .0.base_state, &self.0 .0 .0.custom_state)),
        }
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
unsafe fn launder<'a>(token: &Token) -> &'a Token {
    &*(token as *const Token)
}

mod stream {
    use super::*;
    use crate::token::Value::ControlSequence;

    #[inline]
    pub fn next_unexpanded<S>(vm: &mut vm::VM<S>) -> anyhow::Result<Option<Token>> {
        if let Some(token) = vm.internal.current_source.expansions.pop() {
            return Ok(Some(token));
        }
        if let Some(token) = vm.internal.current_source.root.next(
            &vm.base_state.cat_code_map,
            &mut vm.internal.cs_name_interner,
        )? {
            return Ok(Some(token));
        }
        next_unexpanded_recurse(vm)
    }

    fn next_unexpanded_recurse<S>(vm: &mut vm::VM<S>) -> anyhow::Result<Option<Token>> {
        if vm.internal.pop_source() {
            next_unexpanded(vm)
        } else {
            Ok(None)
        }
    }

    #[inline]
    pub fn peek_unexpanded<S>(vm: &mut vm::VM<S>) -> anyhow::Result<Option<&Token>> {
        if let Some(token) = vm.internal.current_source.expansions.last() {
            return Ok(Some(unsafe { launder(token) }));
        }
        if let Some(token) = vm.internal.current_source.root.next(
            &vm.base_state.cat_code_map,
            &mut vm.internal.cs_name_interner,
        )? {
            vm.internal.current_source.expansions.push(token);
            return Ok(vm.internal.current_source.expansions.last());
        }
        peek_unexpanded_recurse(vm)
    }

    fn peek_unexpanded_recurse<S>(vm: &mut vm::VM<S>) -> anyhow::Result<Option<&Token>> {
        if vm.internal.pop_source() {
            peek_unexpanded(vm)
        } else {
            Ok(None)
        }
    }

    pub fn next_expanded<S>(vm: &mut vm::VM<S>) -> anyhow::Result<Option<Token>> {
        let (token, command) = match next_unexpanded(vm)? {
            None => return Ok(None),
            Some(token) => match token.value() {
                ControlSequence(name) => (token, vm.base_state.commands_map.get_command(&name)),
                _ => return Ok(Some(token)),
            },
        };
        match command {
            Some(command::Command::Expansion(command, tag)) => {
                let command = *command;
                let tag = *tag;
                if let Some(override_expansion) =
                    (vm.hooks().expansion_override_hook)(token, ExpansionInput::new(vm), tag)?
                {
                    return Ok(Some(override_expansion));
                }
                let output = command(token, ExpansionInput::new(vm))?;
                vm.internal.push_expansion(&output);
                next_expanded(vm)
            }
            Some(command::Command::Macro(command)) => {
                let command = command.clone();
                command.call(token, ExpansionInput::new(vm))?;
                next_expanded(vm)
            }
            _ => Ok(Some(token)),
        }
    }

    pub fn peek_expanded<S>(vm: &mut vm::VM<S>) -> anyhow::Result<Option<&Token>> {
        let (token, command) = match peek_unexpanded(vm)? {
            None => return Ok(None),
            Some(token) => match token.value() {
                ControlSequence(name) => (
                    unsafe { launder(token) },
                    vm.base_state.commands_map.get_command(&name),
                ),
                _ => return Ok(Some(unsafe { launder(token) })),
            },
        };
        match command {
            Some(command::Command::Expansion(command, tag)) => {
                let command = *command;
                let token = *token;
                let tag = *tag;
                consume_peek(vm);
                if let Some(override_expansion) =
                    (vm.hooks().expansion_override_hook)(token, ExpansionInput::new(vm), tag)?
                {
                    vm.internal.expansions_mut().push(override_expansion);
                    return Ok(vm.internal.expansions().last());
                }
                let output = command(token, ExpansionInput::new(vm))?;
                vm.internal.push_expansion(&output);
                peek_expanded(vm)
            }
            Some(command::Command::Macro(command)) => {
                let command = command.clone();
                let token = *token;
                consume_peek(vm);
                command.call(token, ExpansionInput::new(vm))?;
                peek_expanded(vm)
            }
            _ => Ok(Some(unsafe { launder(token) })),
        }
    }

    pub fn expand_once<S>(vm: &mut vm::VM<S>) -> anyhow::Result<bool> {
        let (token, command) = match peek_unexpanded(vm)? {
            None => return Ok(false),
            Some(token) => match token.value() {
                ControlSequence(name) => (
                    unsafe { launder(token) },
                    vm.base_state.commands_map.get_command(&name),
                ),
                _ => return Ok(false),
            },
        };
        match command {
            Some(command::Command::Expansion(command, tag)) => {
                let command = *command;
                let token = *token;
                let tag = *tag;
                consume_peek(vm);
                if let Some(override_expansion) =
                    (vm.hooks().expansion_override_hook)(token, ExpansionInput::new(vm), tag)?
                {
                    vm.internal.expansions_mut().push(override_expansion);
                    return Ok(true);
                }
                let output = command(token, ExpansionInput::new(vm))?;
                vm.internal.push_expansion(&output);
                Ok(true)
            }
            Some(command::Command::Macro(command)) => {
                let command = command.clone();
                let token = *token;
                consume_peek(vm);
                command.call(token, ExpansionInput::new(vm))?;
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    #[inline]
    pub fn consume_peek<S>(vm: &mut vm::VM<S>) {
        // When we peek at a token, it is placed on top of the expansions stack.
        // So to consume the token, we just need to remove it from the stack.
        vm.internal.current_source.expansions.pop();
    }
}
