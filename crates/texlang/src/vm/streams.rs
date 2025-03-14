use std::path::PathBuf;

use super::TexlangState;
use crate::prelude as txl;
use crate::token::trace;
use crate::token::Token;
use crate::*;

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
    type S: TexlangState;

    /// Gets the next token in the stream or error if the stream is exhausted.
    ///
    /// This method is almost the same
    /// as the `next` method in Rust's iterator trait, except a stream can return an error.
    fn next_or_err<E: error::EndOfInputError>(&mut self, err: E) -> txl::Result<Token> {
        match self.next() {
            Ok(None) => Err(self.fatal_error(error::EofError::new(err))),
            Ok(Some(token)) => Ok(token),
            Err(err) => Err(err),
        }
    }

    /// Gets the next token in the stream or `Ok(None)` if the stream is exhausted.
    fn next(&mut self) -> txl::Result<Option<Token>>;

    fn peek(&mut self) -> txl::Result<Option<Token>> {
        let token_or = self.next()?;
        if let Some(token) = token_or {
            self.back(token);
        }
        Ok(token_or)
    }

    /// Returns a token to the front of the token stream.
    fn back(&mut self, token: Token);

    /// Returns a reference to the VM.
    fn vm(&self) -> &vm::VM<Self::S>;

    /// Informs the VM that a fatal error has occurred.
    ///
    /// This fatal error causes the VM to shutdown.
    ///
    /// The returned shutdown signal must be propagated
    ///     by returning `Err(shutdown_signal)` to the calling code.
    /// If the shutdown signal is ignored, the VM will eventually panic.
    /// See [`super::ShutdownSignal`] for more information.
    #[must_use]
    fn fatal_error<E: error::TexError>(&mut self, err: E) -> super::ShutdownSignal;

    /// Informs the VM that a recoverable error has occurred.
    ///
    /// The VM responds either with `Ok(())`,
    ///     indicating that the error should be recovered from,
    /// or `Err(ShutdownSignal{})`,
    ///     indicating that the error is fatal and the VM is shutting down.
    ///
    /// In the error case,
    ///     the returned shutdown signal must be propagated using Rust's `?` operator.
    /// If the shutdown signal is ignored, the VM will eventually panic.
    /// See [`super::ShutdownSignal`] for more information.
    fn error<E: error::TexError>(&mut self, err: E) -> txl::Result<()>;

    /// Returns a reference to the commands map.
    #[inline]
    fn commands_map(&self) -> &command::Map<Self::S> {
        &self.vm().commands_map
    }

    /// Returns a reference to the custom state.
    #[inline]
    fn state(&self) -> &Self::S {
        &self.vm().state
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

impl<S: TexlangState> ExpandedStream<S> {
    /// Returns the underlying unexpanded stream.
    pub fn unexpanded(&mut self) -> &mut UnexpandedStream<S> {
        &mut self.0
    }

    /// Expand the next token in the input.
    ///
    /// This method only expands a single token. If, after the expansion, the next token
    /// is expandable it will not be expanded.
    pub fn expand_once(&mut self) -> txl::Result<bool> {
        stream::expand_once(self.vm_mut())
    }

    pub fn checkout_token_buffer(&mut self) -> Vec<Token> {
        self.0 .0.internal.token_buffers.pop().unwrap_or_default().0
    }

    /// Return a token buffer, allowing it to be reused.
    pub fn return_token_buffer(&mut self, mut token_buffer: Vec<Token>) {
        token_buffer.clear();
        self.0
             .0
            .internal
            .token_buffers
            .push(super::TokenBuffer(token_buffer))
    }

    pub(crate) fn vm_mut(&mut self) -> &mut vm::VM<S> {
        &mut self.0 .0
    }

    /// Returns a mutable reference to the expanded tokens stack for the current input source.
    ///
    /// The tokens are a stack, so the next token is the last token in the vector.
    #[inline]
    pub fn expansions_mut(&mut self) -> &mut Vec<Token> {
        self.0 .0.internal.expansions_mut()
    }
}

impl<S: TexlangState> TokenStream for ExpandedStream<S> {
    type S = S;

    #[inline]
    fn next(&mut self) -> txl::Result<Option<Token>> {
        stream::next_expanded(&mut self.unexpanded().0)
    }

    #[inline]
    fn vm(&self) -> &vm::VM<Self::S> {
        &self.0 .0
    }

    #[inline]
    fn back(&mut self, token: Token) {
        self.0.back(token)
    }
    fn fatal_error<E: error::TexError>(&mut self, err: E) -> super::ShutdownSignal {
        self.0 .0.fatal_error(err)
    }
    fn error<E: error::TexError>(&mut self, err: E) -> txl::Result<()> {
        self.0 .0.error(err)
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

impl<S: TexlangState> TokenStream for UnexpandedStream<S> {
    type S = S;

    #[inline]
    fn next(&mut self) -> txl::Result<Option<Token>> {
        stream::next_unexpanded(&mut self.0)
    }

    #[inline]
    fn vm(&self) -> &vm::VM<S> {
        &self.0
    }

    #[inline]
    fn back(&mut self, token: Token) {
        self.0.internal.expansions_mut().push(token)
    }
    fn fatal_error<E: error::TexError>(&mut self, err: E) -> super::ShutdownSignal {
        self.0.fatal_error(err)
    }
    fn error<E: error::TexError>(&mut self, err: E) -> txl::Result<()> {
        self.0.error(err)
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
// TODO: should this be in the command module, not in the vm module?
pub struct ExpansionInput<S>(ExpandedStream<S>);

impl<S> std::convert::AsMut<ExpandedStream<S>> for ExpansionInput<S> {
    fn as_mut(&mut self) -> &mut ExpandedStream<S> {
        &mut self.0
    }
}

impl<S: TexlangState> TokenStream for ExpansionInput<S> {
    type S = S;

    fn next(&mut self) -> txl::Result<Option<Token>> {
        self.0.next()
    }

    fn vm(&self) -> &vm::VM<Self::S> {
        self.0.vm()
    }

    fn back(&mut self, token: Token) {
        self.0.back(token);
    }
    fn fatal_error<E: error::TexError>(&mut self, err: E) -> super::ShutdownSignal {
        self.0 .0 .0.fatal_error(err)
    }
    fn error<E: error::TexError>(&mut self, err: E) -> txl::Result<()> {
        self.0 .0 .0.error(err)
    }
}

impl<S> ExpansionInput<S> {
    /// Creates a mutable reference to this type from the [VM](vm::VM) type.
    #[inline]
    pub fn new(vm: &mut vm::VM<S>) -> &mut ExpansionInput<S> {
        unsafe { &mut *(vm as *mut vm::VM<S> as *mut ExpansionInput<S>) }
    }
}

impl<S: TexlangState> ExpansionInput<S> {
    /// Push source code to the front of the input stream.
    #[inline]
    pub fn push_source(
        &mut self,
        token: Token,
        file_name: PathBuf,
        source_code: String,
    ) -> txl::Result<()> {
        self.0
             .0
             .0
            .internal
            .push_source(Some(token), file_name, source_code)
    }

    /// End the current file.
    ///
    /// This method is used by `\endinput` primitive.
    pub fn end_current_file(&mut self) {
        self.0 .0 .0.internal.end_current_file()
    }

    pub fn push_string_tokens(&mut self, token: Token, s: &str) {
        let trace_key = token.trace_key();
        for c in s.chars().rev() {
            let token = match c {
                ' ' => token::Token::new_space(' ', trace_key),
                _ => token::Token::new_letter(c, trace_key),
            };
            self.expansions_mut().push(token);
        }
    }
}

impl<S> ExpansionInput<S> {
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
    // TODO: destroy
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

    #[inline]
    pub fn state_and_expansions_mut(&mut self) -> (&S, &mut Vec<Token>) {
        (&self.0 .0 .0.state, self.0 .0 .0.internal.expansions_mut())
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
/// - Mutable access to the state and the commands map
///     the [ExecutionInput::state_mut]
///     and [ExecutionInput::commands_map_mut] methods.
#[repr(transparent)]
pub struct ExecutionInput<S>(ExpandedStream<S>);

impl<S> std::convert::AsMut<ExpandedStream<S>> for ExecutionInput<S> {
    fn as_mut(&mut self) -> &mut ExpandedStream<S> {
        &mut self.0
    }
}

impl<S: TexlangState> TokenStream for ExecutionInput<S> {
    type S = S;

    fn next(&mut self) -> txl::Result<Option<Token>> {
        self.0.next()
    }

    fn vm(&self) -> &vm::VM<Self::S> {
        self.0.vm()
    }

    fn back(&mut self, token: Token) {
        self.0.back(token);
    }
    fn fatal_error<E: error::TexError>(&mut self, err: E) -> super::ShutdownSignal {
        self.0 .0 .0.fatal_error(err)
    }
    fn error<E: error::TexError>(&mut self, err: E) -> txl::Result<()> {
        self.0 .0 .0.error(err)
    }
}

impl<S: TexlangState> ExecutionInput<S> {
    /// Shutdown the VM.
    pub fn shutdown(&mut self) -> super::ShutdownSignal {
        self.0 .0 .0.shutdown()
    }
}

impl<S> ExecutionInput<S> {
    /// Creates a mutable reference to this type from the [VM](vm::VM) type.
    #[inline]
    pub fn new(vm: &mut vm::VM<S>) -> &mut ExecutionInput<S> {
        unsafe { &mut *(vm as *mut vm::VM<S> as *mut ExecutionInput<S>) }
    }

    #[inline]
    pub fn unexpanded(&mut self) -> &mut UnexpandedStream<S> {
        &mut self.0 .0
    }

    #[inline]
    pub fn commands_map_mut(&mut self) -> &mut command::Map<S> {
        &mut self.0 .0 .0.commands_map
    }

    /// Returns a mutable reference to the state.
    #[inline]
    pub fn state_mut(&mut self) -> &mut S {
        &mut self.0 .0 .0.state
    }
    /// Returns a mutable reference to the tracer.
    pub fn tracer_mut(&mut self) -> &mut trace::Tracer {
        &mut self.0 .0 .0.internal.tracer
    }
    /// Returns a [vm::Parts] struct contains mutable references to different parts of the VM.
    #[inline]
    pub fn vm_parts(&mut self) -> vm::Parts<'_, S> {
        let vm = &mut self.0 .0 .0;
        vm::Parts {
            state: &mut vm.state,
            cs_name_interner: &mut vm.internal.cs_name_interner,
            tracer: &mut vm.internal.tracer,
        }
    }

    // TODO: pass in the token and keep it as a reference
    pub fn begin_group(&mut self) {
        self.0 .0 .0.begin_group()
    }

    #[inline]
    pub(crate) fn groups(&mut self) -> &mut [variable::SaveStackElement<S>] {
        &mut self.0 .0 .0.internal.save_stack
    }

    pub(crate) fn current_group_mut(&mut self) -> Option<(&mut variable::SaveStackElement<S>, &S)> {
        match self.0 .0 .0.internal.save_stack.last_mut() {
            None => None,
            Some(g) => Some((g, &self.0 .0 .0.state)),
        }
    }
    pub(crate) fn vm_mut(&mut self) -> &mut vm::VM<S> {
        &mut self.0 .0 .0
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

impl<S: TexlangState> ExecutionInput<S> {
    pub fn end_group(&mut self, token: Token) -> txl::Result<()> {
        self.0 .0 .0.end_group(token)
    }
}

mod stream {
    use super::*;
    use crate::token::lexer;
    use crate::token::lexer::Config;

    impl<T: TexlangState> Config for T {
        #[inline]
        fn cat_code(&self, c: char) -> crate::types::CatCode {
            self.cat_code(c)
        }
        // TODO: implement \endlinechar
        #[inline]
        fn end_line_char(&self) -> Option<char> {
            self.end_line_char()
        }
    }

    #[inline]
    pub fn next_unexpanded<S: TexlangState>(vm: &mut vm::VM<S>) -> txl::Result<Option<Token>> {
        if let Some(token) = vm.internal.current_source.expansions.pop() {
            return Ok(Some(token));
        }
        match vm.internal.current_source.root.next(
            &vm.state,
            &mut vm.internal.cs_name_interner,
            false,
        ) {
            lexer::Result::Token(token) => {
                return Ok(Some(token));
            }
            lexer::Result::InvalidCharacter(c, trace_key) => {
                return Err(build_invalid_character_error(vm, c, trace_key));
            }
            // The EndOfLine case is never returned from the lexer but we silently handle it.
            lexer::Result::EndOfLine | lexer::Result::EndOfInput => {}
        }
        if !vm.internal.pop_source() {
            return Ok(None);
        }
        next_unexpanded(vm)
    }

    fn build_invalid_character_error<S: TexlangState>(
        vm: &mut vm::VM<S>,
        c: char,
        trace_key: trace::Key,
    ) -> vm::ShutdownSignal {
        vm.fatal_error(lexer::InvalidCharacterError::new(vm, c, trace_key))
    }

    pub fn next_expanded<S: TexlangState>(vm: &mut vm::VM<S>) -> txl::Result<Option<Token>> {
        let (token, command) = match next_unexpanded(vm)? {
            None => return Ok(None),
            Some(token) => match token.value() {
                token::Value::CommandRef(command_ref) => {
                    (token, vm.commands_map.get_command(&command_ref))
                }
                _ => return Ok(Some(token)),
            },
        };
        match command {
            Some(command::Command::Expansion(command, tag)) => {
                let command = *command;
                let tag = *tag;
                match S::expansion_override_hook(token, ExpansionInput::new(vm), tag) {
                    Ok(None) => (),
                    Ok(Some(override_expansion)) => {
                        return Ok(Some(override_expansion));
                    }
                    Err(err) => return Err(err),
                };
                vm.stack_push(token, error::OperationKind::Expansion);
                let err_or = command(token, ExpansionInput::new(vm));
                vm.stack_pop();
                err_or?;
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

    pub fn expand_once<S: TexlangState>(vm: &mut vm::VM<S>) -> txl::Result<bool> {
        let (token, command) = match next_unexpanded(vm)? {
            None => return Ok(false),
            Some(token) => match token.value() {
                token::Value::CommandRef(command_ref) => {
                    (token, vm.commands_map.get_command(&command_ref))
                }
                _ => {
                    vm.internal.expansions_mut().push(token);
                    return Ok(false);
                }
            },
        };
        match command {
            Some(command::Command::Expansion(command, tag)) => {
                let command = *command;
                let tag = *tag;
                match S::expansion_override_hook(token, ExpansionInput::new(vm), tag) {
                    Ok(None) => (),
                    Ok(Some(override_expansion)) => {
                        vm.internal.expansions_mut().push(override_expansion);
                        return Ok(true);
                    }
                    Err(err) => return Err(err),
                };
                vm.stack_push(token, error::OperationKind::Expansion);
                let err_or = command(token, ExpansionInput::new(vm));
                vm.stack_pop();
                err_or?;
                Ok(true)
            }
            Some(command::Command::Macro(command)) => {
                let command = command.clone();
                command.call(token, ExpansionInput::new(vm))?;
                Ok(true)
            }
            _ => {
                vm.internal.expansions_mut().push(token);
                Ok(false)
            }
        }
    }
}
