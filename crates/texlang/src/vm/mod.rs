//! The Texlang virtual machine (VM).
//!
//! This module contains the definition of the runtime VM,
//!     various input streams that wrap the VM
//!     and the main function that is used to run Texlang.
//! See the VM documentation in the Texlang book for full documentation.

use super::token::CsName;
use crate::command;
use crate::command::BuiltIn;
use crate::command::Command;
use crate::error;
use crate::prelude as txl;
use crate::texmacro;
use crate::token;
use crate::token::lexer;
use crate::token::trace;
use crate::token::CsNameInterner;
use crate::token::Token;
use crate::token::Value;
use crate::types;
use crate::variable;
use std::collections::HashMap;
use std::path::PathBuf;
use texcraft_stdext::collections::groupingmap;

#[cfg(feature = "serde")]
pub mod serde;
mod streams;
pub use streams::*;

/// Implementations of this trait determine how the VM handles non-execution-command tokens.
///
/// The main loop of the VM reads the next expanded token and performs
///     some action based on the token.
/// Many cases are handled automatically based on the semantics of the TeX language:
///
/// | token type | example | action |
/// | -- | -- | -- |
/// | execution command | `\def` | run the command |
/// | variable command | `\count` | assign a value to the corresponding variable |
/// | token alias | `\a` after `\let\a=a` | run the main VM loop for the token that is aliased |
/// | begin group character | `{` | begin a group
/// | end group character | `}` | end the current group
///
/// Note that the first three rows can arise from both control sequences and active character tokens.
///
/// The remaining cases are not specified by the TeX language but instead by
///     the business logic of the TeX engine being built.
/// The behavior in these cases is specified by implementing the associated handler.
/// These cases and handlers are:
///
/// | token type | example | handler | default |
/// | --- | --- | --- | --- |
/// | character token | `b` | [character_handler](Handlers::character_handler) | do nothing
/// | undefined command | `\b` where `\b` was never defined | [undefined_command_handler](Handlers::undefined_command_handler) | return an undefined control sequence error
/// | unexpanded expansion command | `\the` in `\noexpand\the` | [unexpanded_expansion_command](Handlers::unexpanded_expansion_command) | do nothing
///
/// Each of the handlers has the same function signature as an execution command.
pub trait Handlers<S: TexlangState> {
    /// Handler to invoke for character tokens.
    ///
    /// This token is _not_ invoked for tokens whose category code is begin group (1), end group (2) or active character (13).
    /// These cases are handled automatically by the VM based on the semantics of the TeX language.
    ///
    /// The default implementation is a no-op.
    fn character_handler(
        input: &mut ExecutionInput<S>,
        token: token::Token,
        character: char,
    ) -> txl::Result<()> {
        _ = (input, token, character);
        Ok(())
    }

    /// Handler to invoke for math character tokens.
    ///
    /// The default implementation throws an error because math character tokens are
    /// only valid in math mode which is implemented outside of the main VM loop.
    fn math_character_handler(
        input: &mut ExecutionInput<S>,
        token: token::Token,
        math_character: types::MathCode,
    ) -> txl::Result<()> {
        _ = math_character;
        Err(input.fatal_error(error::SimpleTokenError::new(
            token,
            "math characters can only appear in math mode",
        )))
    }

    /// Handler to invoke for a control sequence or active character for which no command is defined.
    ///
    /// The default implementation throws an undefined command error.
    fn undefined_command_handler(
        input: &mut ExecutionInput<S>,
        token: token::Token,
    ) -> txl::Result<()> {
        Err(input.fatal_error(error::UndefinedCommandError::new(input.vm(), token)))
    }

    /// Handler to invoke for expansion commands that were not expanded.
    ///
    /// For example, in the TeX snippet `\noexpand\the`, this handler handles
    /// the unexpanded `\the` token.
    ///
    /// The default implementation is a no-op.
    fn unexpanded_expansion_command(
        input: &mut ExecutionInput<S>,
        token: token::Token,
    ) -> txl::Result<()> {
        _ = (token, input);
        Ok(())
    }

    /// Handler to invoke when the input ends.
    ///
    /// In TeX the user is prompted to add additional input and if no
    ///     input is provided a fatal error is thrown.
    /// To end the VM without an error the user has to write `\end`
    ///     or `\dump`.
    ///
    /// In this handler, if `Ok(())` is returned, the VM starts running again
    ///     under the assumption that additional TeX source has been added to the VM.
    /// Otherwise the shutdown signal causes the VM to stop.
    ///
    /// The default implementation shuts down the VM with no error.
    fn end_of_input_handler(input: &mut ExecutionInput<S>) -> txl::Result<()> {
        Err(input.shutdown())
    }
}

#[derive(Default)]
pub struct DefaultHandlers;

impl<S: TexlangState> Handlers<S> for DefaultHandlers {}

impl<S: TexlangState> VM<S> {
    /// Run the VM.
    ///
    /// It is assumed that the VM has been preloaded with TeX source code using the
    /// [VM::push_source] method.
    pub fn run<H: Handlers<S>>(&mut self) -> Result<(), Box<error::TracedTexError>> {
        self.run_impl::<H>();
        match self.internal.shutdown_status.take() {
            ShutdownStatus::None => unreachable!(),
            ShutdownStatus::Normal => Ok(()),
            ShutdownStatus::Error(traced_error) => Err(Box::new(traced_error)),
        }
    }
    fn run_impl<H: Handlers<S>>(&mut self) -> ShutdownSignal {
        let input = ExecutionInput::new(self);

        loop {
            let token = match input.next() {
                Ok(None) => match H::end_of_input_handler(input) {
                    Ok(_) => continue,
                    Err(signal) => return signal,
                },
                Ok(Some(token)) => token,
                Err(signal) => return signal,
            };
            let r = match token.value() {
                Value::CommandRef(command_ref) => {
                    match input.commands_map().get_command(&command_ref) {
                        Some(Command::Execution(cmd, _)) => {
                            let cmd = *cmd;
                            input
                                .vm_mut()
                                .stack_push(token, error::OperationKind::Execution);
                            let err_or = cmd(token, input);
                            input.vm_mut().stack_pop();
                            err_or
                        }
                        Some(Command::Variable(cmd)) => {
                            let cmd = cmd.clone();
                            let scope = S::variable_assignment_scope_hook(input.state_mut());
                            cmd.set_value_using_input(token, input, scope)
                        }
                        Some(Command::CharacterTokenAlias(token_value)) => {
                            // TODO: should add tests for when this is begin group and end group.
                            input.back(Token::new_from_value(*token_value, token.trace_key()));
                            Ok(())
                        }
                        Some(Command::Expansion(_, _)) | Some(Command::Macro(_)) => {
                            H::unexpanded_expansion_command(input, token)
                        }
                        Some(Command::Character(c)) => {
                            let token = Token::new_other(*c, token.trace_key()); // Remove
                            H::character_handler(input, token, *c)
                        }
                        Some(Command::MathCharacter(c)) => {
                            H::math_character_handler(input, token, *c)
                        }
                        Some(Command::Font(font)) => {
                            let font = *font;
                            let scope =
                                TexlangState::variable_assignment_scope_hook(input.state_mut());
                            let internal = &mut input.vm_mut().internal;
                            match scope {
                                groupingmap::Scope::Local => {
                                    // If this is the first font assignment in this group,
                                    // save the current value to the top of the stack. It will
                                    // be restored from here when the group ends.
                                    let current_font = internal.current_font;
                                    if let Some(top) = internal.fonts_save_stack.last_mut() {
                                        if top.is_none() {
                                            *top = Some(current_font);
                                        }
                                    }
                                }
                                groupingmap::Scope::Global => {
                                    // If this is a global font assignment, clear the stack
                                    // entirely so that no font will be restored when groups end.
                                    for font_or in &mut internal.fonts_save_stack {
                                        *font_or = None;
                                    }
                                }
                            }
                            internal.current_font = font;
                            input.state_mut().enable_font_hook(font);
                            Ok(())
                        }
                        None => H::undefined_command_handler(input, token),
                    }
                }
                Value::BeginGroup(_) => {
                    input.begin_group();
                    Ok(())
                }
                Value::EndGroup(_) => input.end_group(token),
                Value::MathShift(c)
                | Value::AlignmentTab(c)
                | Value::Parameter(c)
                | Value::Superscript(c)
                | Value::Subscript(c)
                | Value::Space(c)
                | Value::Letter(c)
                | Value::Other(c) => H::character_handler(input, token, c),
            };
            if let Err(signal) = r {
                return signal;
            }
        }
    }

    pub(crate) fn shutdown(&mut self) -> ShutdownSignal {
        self.internal.shutdown_status.transition_to_normal();
        ShutdownSignal {}
    }
    pub(crate) fn fatal_error<E: error::TexError>(&mut self, err: E) -> ShutdownSignal {
        let err: Box<dyn error::TexError> = Box::new(err);
        let traced = error::TracedTexError::new(
            err,
            &self.internal.tracer,
            &self.internal.cs_name_interner,
            self.generate_stack_trace(),
        );
        self.internal.shutdown_status.transition_to_error(traced);
        ShutdownSignal {}
    }
    pub(crate) fn error<E: error::TexError>(&mut self, err: E) -> txl::Result<()> {
        let err: Box<dyn error::TexError> = Box::new(err);
        let traced = error::TracedTexError::new(
            err,
            &self.internal.tracer,
            &self.internal.cs_name_interner,
            self.generate_stack_trace(),
        );
        match self.state.recoverable_error_hook(traced) {
            Ok(_) => Ok(()),
            Err(err) => {
                let traced = error::TracedTexError::new(
                    err,
                    &self.internal.tracer,
                    &self.internal.cs_name_interner,
                    self.generate_stack_trace(),
                );
                self.internal.shutdown_status.transition_to_error(traced);
                Err(ShutdownSignal {})
            }
        }
    }
}

#[derive(Debug)]
struct EndOfGroupError {
    trace: token::Token,
}

impl error::TexError for EndOfGroupError {
    fn kind(&self) -> error::Kind {
        error::Kind::Token(self.trace)
    }

    fn title(&self) -> String {
        "there is no group to end".into()
    }
}

/// The Texlang virtual machine.
pub struct VM<S> {
    /// The state
    pub state: S,

    /// The commands map
    pub commands_map: command::Map<S>,

    /// The working directory which is used as the root for relative file paths
    ///
    /// This is [None] if the working directory could not be determined.
    pub working_directory: Option<std::path::PathBuf>,

    internal: Internal<S>,
}

/// Mutable references to different parts of the VM.
pub struct Parts<'a, S> {
    pub state: &'a mut S,
    pub cs_name_interner: &'a mut token::CsNameInterner,
    pub tracer: &'a mut trace::Tracer,
}

/// Implementations of this trait may be used as the state in a Texlang VM.
///
/// The most important thing to know about this trait is that it has no required methods.
/// For any type it can be implemented trivially:
/// ```
/// # use texlang::traits::TexlangState;
/// struct SomeNewType;
///
/// impl TexlangState for SomeNewType {}
/// ```
///
/// Methods of the trait are invoked at certain points when the VM is running,
///     and in general offer a way of customizing the behavior of the VM.
/// The trait methods are all dispatched statically, which is important for performance.
pub trait TexlangState: Sized {
    /// Get the cat code for the provided character.
    ///
    /// The default implementation returns the cat code used in plain TeX.
    fn cat_code(&self, c: char) -> types::CatCode {
        types::CatCode::PLAIN_TEX_DEFAULTS
            .get(c as usize)
            .copied()
            .unwrap_or_default()
    }

    /// Get current end line char, or [None] if it's undefined.
    ///
    /// The default implementation returns `Some(\r)`.
    fn end_line_char(&self) -> Option<char> {
        Some('\r')
    }

    /// Get the em width for the current font.
    ///
    /// The default implementation returns `12pt`.
    fn em_width(&self) -> core::Scaled {
        core::Scaled::ONE * 12
    }

    /// Get the ex height for the current font.
    ///
    /// The default implementation returns `12pt`.
    fn ex_height(&self) -> core::Scaled {
        core::Scaled::ONE * 12
    }

    /// Get the current magnification ratio (e.g. value of \mag).
    ///
    /// The default implementation returns `1000`, which corresponds to
    /// no magnification.
    fn magnification_ratio(&self) -> i32 {
        1000
    }

    /// Hook that is invoked after a TeX macro is expanded.
    ///
    /// This hook is designed to support the `\tracingmacros` primitive.
    fn post_macro_expansion_hook(
        token: Token,
        input: &ExpansionInput<Self>,
        tex_macro: &texmacro::Macro,
        arguments: &[&[Token]],
        reversed_expansion: &[Token],
    ) {
        _ = (token, input, tex_macro, arguments, reversed_expansion);
    }

    /// Hook that potentially overrides the expansion of a command.
    ///
    /// This hook is invoked before an expandable token is expanded.
    /// If the result of the hook is a non-empty, that result is considered the expansion of
    ///   the token
    /// The result of the hook is not expanded before being returned.
    ///
    /// This hook is designed to support the `\noexpand` primitive.
    fn expansion_override_hook(
        token: token::Token,
        input: &mut ExpansionInput<Self>,
        tag: Option<command::Tag>,
    ) -> txl::Result<Option<Token>> {
        _ = (token, input, tag);
        Ok(None)
    }

    /// Hook that determines the scope of a variable assignment.
    ///
    /// This hook is designed to support the \global and \globaldefs commands.
    fn variable_assignment_scope_hook(state: &mut Self) -> groupingmap::Scope {
        _ = state;
        groupingmap::Scope::Local
    }

    /// Hook that determines what to do when a recoverable error occurs.
    ///
    /// If the hook returns `Ok(())` then the recovery process should run.
    /// If the hook returns an error, then that error should be returned from the enclosing
    ///     function and propagated through the VM.
    ///
    /// Note that there is no requirement that an error returned from this hook
    ///     is the same as the error provided to the hook.
    /// For example, when Knuth's TeX is running in batch mode errors are
    ///      logged but otherwise ignored.
    /// However if 100 such errors occur, the interpreter fails.
    /// To implement this in Texlang, the result of this function would be `Ok(())`
    ///     for the first 99 errors,
    ///     but after the 100th error a "too many errors" error would be returned from the hook.
    /// Note that the returned error in this case is not the 100th error itself.
    fn recoverable_error_hook(
        &self,
        error: error::TracedTexError,
    ) -> Result<(), Box<dyn error::TexError>> {
        _ = self;
        Err(error.error)
    }

    /// Hook that is invoked when a font is enabled.
    ///
    /// For example, after the TeX snippet `\the \textfont 1`, this hook
    /// is invoked for the font stored in `\textfont 1`.
    /// The hook is also called if a font needs to be reenabled after
    /// a group ends.
    ///
    /// The default implementation is a no-op.
    fn enable_font_hook(&mut self, font: types::Font) {
        _ = font
    }

    /// Returns whether the command corresponding to the provided tag references
    /// the currnet font when provided as an argument to a variable.
    ///
    /// This is used to implement the `\font` primitive.
    fn is_current_font_command(&self, tag: command::Tag) -> bool {
        _ = tag;
        false
    }
}

impl TexlangState for () {}

impl<S: Default> VM<S> {
    /// Create a new VM with the provided built-in commands.
    ///
    /// If the state type satisfies the [`HasDefaultBuiltInCommands`] trait,
    ///     and you are using the default built-ins,
    ///     use the [`VM::new`] method instead.
    pub fn new_with_built_in_commands(built_in_commands: HashMap<&str, BuiltIn<S>>) -> VM<S> {
        let mut internal = Internal::new(Default::default());
        let built_in_commands = built_in_commands
            .into_iter()
            .map(|(key, value)| (internal.cs_name_interner.get_or_intern(key), value))
            .collect();
        VM {
            state: Default::default(),
            commands_map: command::Map::new(built_in_commands),
            internal,
            working_directory: match std::env::current_dir() {
                Ok(path_buf) => Some(path_buf),
                Err(err) => {
                    println!("failed to determine the working directory: {err}");
                    None
                }
            },
        }
    }
}

impl<S: Default + HasDefaultBuiltInCommands> VM<S> {
    /// Create a new VM.
    pub fn new() -> VM<S> {
        VM::<S>::new_with_built_in_commands(S::default_built_in_commands())
    }
}

impl<S: Default + HasDefaultBuiltInCommands> Default for VM<S> {
    fn default() -> Self {
        Self::new()
    }
}

/// Deserialize a Texlang VM using the provided built-in commands.
///
/// If the state type satisfies the [`HasDefaultBuiltInCommands`] trait,
///     and you are deserializing using the default built-ins,
///     you don't need to use this function.
/// You can use the serde deserialize trait directly.
/// See the [`serde` submodule](serde) for more information on deserialization.
#[cfg(feature = "serde")]
impl<'de, S: ::serde::Deserialize<'de>> VM<S> {
    pub fn deserialize_with_built_in_commands<D: ::serde::Deserializer<'de>>(
        deserializer: D,
        built_in_commands: HashMap<&str, BuiltIn<S>>,
    ) -> Result<Self, D::Error> {
        serde::deserialize(deserializer, built_in_commands)
    }
}

/// States that implement this trait have a default set of built-in commands associated to them.
///
/// In general in Texlang, the same state type can be used with different sets of built-in
///     commands.
/// However in many situations the state type has a specific set of built-ins
///     associated to it.
/// For example, the state type corresponding to pdfTeX is associated with the set of built-ins
///     provided by pdfTeX.
///
/// This trait is used to specify this association.
/// The benefit is that creating new VMs and deserializing VMs is a bit easier
///     because the built-in commands don't need to be provided explicitly.
/// Moreover, if a state implements this trait the associated VM implements serde's deserialize trait.
pub trait HasDefaultBuiltInCommands: TexlangState {
    fn default_built_in_commands() -> HashMap<&'static str, BuiltIn<Self>>;
}

#[cfg(feature = "serde")]
impl<'de, S: ::serde::Deserialize<'de> + HasDefaultBuiltInCommands> ::serde::Deserialize<'de>
    for VM<S>
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: ::serde::Deserializer<'de>,
    {
        let built_ins = S::default_built_in_commands();
        serde::deserialize(deserializer, built_ins)
    }
}

impl<S: TexlangState> VM<S> {
    /// Add new source code to the VM.
    ///
    /// TeX input source code is organized as a stack.
    /// Pushing source code onto the stack will mean it is executed first.
    pub fn push_source<T1: Into<PathBuf>, T2: Into<String>>(
        &mut self,
        file_name: T1,
        source_code: T2,
    ) -> txl::Result<()> {
        self.internal
            .push_source(None, file_name.into(), source_code.into())
    }
}

impl<S> VM<S> {
    /// Clear all source code from the VM.
    pub fn clear_sources(&mut self) {
        self.internal.clear_sources()
    }

    /// Return a regular hash map with all the commands as they are currently defined.
    ///
    /// This function is extremely slow and is only intended to be invoked on error paths.
    pub fn get_commands_as_map_slow(&self) -> HashMap<&str, BuiltIn<S>> {
        let map_1: HashMap<CsName, BuiltIn<S>> = self.commands_map.to_hash_map_slow();
        let mut map = HashMap::new();
        for (cs_name, cmd) in map_1 {
            let cs_name_str = match self.internal.cs_name_interner.resolve(cs_name) {
                None => continue,
                Some(cs_name_str) => cs_name_str,
            };
            map.insert(cs_name_str, cmd);
        }
        map
    }

    /// Return a reference to the control sequence name string interner.
    ///
    /// This interner can be used to resolve [CsName] types into regular strings.
    #[inline]
    pub fn cs_name_interner(&self) -> &CsNameInterner {
        &self.internal.cs_name_interner
    }
    #[inline]
    /// TODO: just put the CS name interner in the VM?
    pub fn cs_name_interner_mut(&mut self) -> &mut CsNameInterner {
        &mut self.internal.cs_name_interner
    }

    fn begin_group(&mut self) {
        self.commands_map.begin_group();
        self.internal.save_stack.push(Default::default());
        self.internal.fonts_save_stack.push(None);
    }

    pub fn trace(&self, token: Token) -> trace::SourceCodeTrace {
        self.internal
            .tracer
            .trace(token, &self.internal.cs_name_interner)
    }

    pub fn trace_end_of_input(&self) -> trace::SourceCodeTrace {
        self.internal.tracer.trace_end_of_input()
    }

    /// Returns the number of current sources on the source stack
    pub fn num_current_sources(&self) -> usize {
        self.internal.sources.len() + 1
    }

    pub fn generate_stack_trace(&self) -> Vec<error::StackTraceElement> {
        self.internal
            .execution_stack
            .iter()
            .map(|(op_kind, token)| error::StackTraceElement {
                context: *op_kind,
                token: *token,
                trace: self
                    .internal
                    .tracer
                    .trace(*token, &self.internal.cs_name_interner),
            })
            .collect()
    }
    pub(crate) fn stack_push(&mut self, token: Token, op_kind: error::OperationKind) {
        self.internal.execution_stack.push((op_kind, token));
    }
    pub(crate) fn stack_pop(&mut self) {
        self.internal.execution_stack.pop();
    }
    pub fn current_font(&self) -> types::Font {
        self.internal.current_font
    }
}

impl<S: TexlangState> VM<S> {
    fn end_group(&mut self, token: token::Token) -> txl::Result<()> {
        // Restore commands
        match self.commands_map.end_group() {
            Ok(()) => (),
            Err(_) => return Err(self.fatal_error(EndOfGroupError { trace: token })),
        }
        // Restore variable values
        let group = self.internal.save_stack.pop().unwrap();
        group.restore(ExecutionInput::new(self));
        // Restore fonts
        if let Some(font) = self.internal.fonts_save_stack.pop().unwrap() {
            self.internal.current_font = font;
            self.state.enable_font_hook(font);
        }
        Ok(())
    }
}

/// Parts of the VM that are private.
// We have serde(bound="") because otherwise serde tries to put a `Default` bound on S.
#[cfg_attr(
    feature = "serde",
    derive(::serde::Serialize, ::serde::Deserialize),
    serde(bound = "")
)]
struct Internal<S> {
    // The sources form a stack. We store the top element directly on the VM
    // for performance reasons.
    current_source: Source,
    sources: Vec<Source>,

    cs_name_interner: CsNameInterner,

    tracer: trace::Tracer,

    // Token buffers are thrown away in serialization - there's nothing we need to keep.
    #[cfg_attr(feature = "serde", serde(skip))]
    token_buffers: std::collections::BinaryHeap<TokenBuffer>,

    // The save stack is handled manually in (de)serialization.
    // We need to use special logic in combination with the command map in order to serialize the
    // variable pointers that are in the stack.
    #[cfg_attr(feature = "serde", serde(skip))]
    save_stack: Vec<variable::SaveStackElement<S>>,

    current_font: types::Font,
    fonts_save_stack: Vec<Option<types::Font>>,
    execution_stack: Vec<(error::OperationKind, Token)>,

    // We assume the VM is never saved during shutdown.
    #[cfg_attr(feature = "serde", serde(skip))]
    shutdown_status: ShutdownStatus,
}

impl<S> Internal<S> {
    fn new(cs_name_interner: CsNameInterner) -> Self {
        Internal {
            current_source: Default::default(),
            sources: Default::default(),
            cs_name_interner,
            tracer: Default::default(),
            token_buffers: Default::default(),
            save_stack: Default::default(),
            current_font: types::Font::NULL_FONT,
            fonts_save_stack: Default::default(),
            execution_stack: Default::default(),
            shutdown_status: Default::default(),
        }
    }
}
impl<S: TexlangState> Internal<S> {
    fn push_source(
        &mut self,
        token: Option<Token>,
        file_name: PathBuf,
        source_code: String,
    ) -> txl::Result<()> {
        let trace_key_range =
            self.tracer
                .register_source_code(token, trace::Origin::File(file_name), &source_code);
        let mut new_source = Source::new(source_code, trace_key_range);
        std::mem::swap(&mut new_source, &mut self.current_source);
        // TODO: if the current top source is empty, we should skip this.
        // Check this is working by looking at the JSON serialization.
        self.sources.push(new_source);
        Ok(())
    }

    fn end_current_file(&mut self) {
        self.current_source.root.end()
    }
}
impl<S> Internal<S> {
    fn clear_sources(&mut self) {
        self.current_source = Default::default();
        self.sources.clear();
    }

    #[inline]
    fn push_expansion(&mut self, expansion: &[Token]) {
        self.current_source
            .expansions
            .extend(expansion.iter().rev());
    }

    #[inline]
    fn expansions(&self) -> &Vec<Token> {
        &self.current_source.expansions
    }

    #[inline]
    fn expansions_mut(&mut self) -> &mut Vec<Token> {
        &mut self.current_source.expansions
    }

    fn pop_source(&mut self) -> bool {
        // We should set the current_source to be Default::default() if there is no additional source.
        // Check this is working by looking at the JSON serialization.
        match self.sources.pop() {
            None => false,
            Some(source) => {
                self.current_source = source;
                true
            }
        }
    }
}

#[cfg_attr(feature = "serde", derive(::serde::Serialize, ::serde::Deserialize))]
struct Source {
    expansions: Vec<Token>,
    root: lexer::Lexer,
}

impl Source {
    pub fn new(source_code: String, trace_key_range: trace::KeyRange) -> Source {
        Source {
            expansions: Vec::with_capacity(32),
            root: lexer::Lexer::new(source_code, trace_key_range),
        }
    }
}

impl Default for Source {
    fn default() -> Self {
        Source::new("".into(), trace::KeyRange::empty())
    }
}

#[derive(Default)]
struct TokenBuffer(Vec<Token>);

impl PartialEq for TokenBuffer {
    fn eq(&self, other: &Self) -> bool {
        self.0.capacity() == other.0.capacity()
    }
}

impl Eq for TokenBuffer {}

impl PartialOrd for TokenBuffer {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TokenBuffer {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.capacity().cmp(&other.0.capacity())
    }
}

/// A signal that the VM is shutting down.
///
/// A value of this type is returned in the error payload of
///     the [`Result`](crate::prelude::Result) of Texlang commands and basically all other Texlang functions.
/// The only thing to do with the signal is to propagate it up the
///     Rust call stack using Rust's `?` operator.
/// Eventually the signal will reach the main VM loop, and the VM will stop.
///
/// The stop signal should _not_ be ignored or otherwise "handled".
/// For example, this code is incorrect:
///
/// ```
/// # use texlang::token;
/// # use texlang::vm;
/// # use texlang::traits::*;
/// # use texlang::prelude as txl;
/// fn execution_primitive_fn<S: TexlangState>(
///    token: token::Token,
///    input: &mut vm::ExecutionInput<S>,
///) -> txl::Result<()> {
///     let i = match i32::parse(input) {
///         Ok(i) => i,
///         Err(_shutdown_signal) => {
///             // This is incorrect - the shutdown signal must be propagated!
///             0
///         }
///     };
///     println!["Parsed integer {i}"];
///     Ok(())
/// }
/// ```
///
/// In this case the VM will eventually panic when it realizes that the shutdown was ignored.
/// The correct code is this:
///
/// ```
/// # use texlang::token;
/// # use texlang::vm;
/// # use texlang::traits::*;
/// # use texlang::prelude as txl;
/// fn execution_primitive_fn<S: TexlangState>(
///    token: token::Token,
///    input: &mut vm::ExecutionInput<S>,
///) -> txl::Result<()> {
///     let i = i32::parse(input)?;
///     println!["Parsed integer {i}"];
///     Ok(())
/// }
/// ```
/// ## Generating the shutdown signal
///
/// The signal can originate either with a fatal error,
///     or from a TeX control
///     sequence that wants to stop execution (e.g. the `\end` primitive).
#[derive(Debug)]
pub struct ShutdownSignal {}

#[derive(Debug, Default)]
enum ShutdownStatus {
    /// The VM is not shutting down.
    #[default]
    None,
    /// The VM is shuting down for an expected reason.
    Normal,
    /// The VM is shuting down because of a fatal error.
    Error(error::TracedTexError),
}

impl ShutdownStatus {
    fn transition_to_normal(&mut self) {
        if !matches!(self, ShutdownStatus::None) {
            panic!("shutdown signal ignored")
        }
        *self = ShutdownStatus::Normal;
    }
    fn transition_to_error(&mut self, err: error::TracedTexError) {
        if !matches!(self, ShutdownStatus::None) {
            panic!("shutdown signal ignored")
        }
        *self = ShutdownStatus::Error(err);
    }
    fn take(&mut self) -> ShutdownStatus {
        let mut s = ShutdownStatus::None;
        std::mem::swap(self, &mut s);
        s
    }
}

/// Helper trait for implementing the component pattern in Texlang.
///
/// The component pattern is a ubiquitous design pattern in Texlang.
/// It is used when implementing TeX commands that require state.
/// An example of a stateful TeX command is `\year`, which needs to store the current year somewhere.
///
/// When the component pattern is used, a stateful TeX command
///     can have a single implementation that
///     is used by multiple TeX engines built with Texlang.
/// Additionally, a specific TeX engine can compose many different
///     stateful TeX commands together without worrying about conflicts between their state.
/// The component pattern is Texlang's main solution to the problem of
///     global mutable state that is pervasive in the original implementation of TeX.
///
/// In the component pattern, the state
///     needed by a specific command like `\year` is isolated in a _component_, which is a concrete
///     Rust type like a struct.
/// This Rust type is the generic type `C` in the trait.
/// The stateful command (e.g. `\year`) is defined in the same Rust module as the component.
/// The internals of the component are made private to the module it is defined in.
/// This means the state can only be mutated by the command (or commands) implemented in the module.
///
/// In order to function, the command needs to have access to an instance of the component in which
///     the command will maintain its state.
/// The `HasComponent` trait enforces this.
/// Any VM state type that contains the component can implement the trait.
/// The Rust code defining the
///     command specifies the trait in its trait bounds, and uses the trait to access the component.
///
/// The pattern enables Texlang code to be composed as follows.
/// Different VM states can include the same component and thus reuse the same commands.
/// Combining multiple commands into one state just involves having the
///     VM state include all of the relevant components.
///
/// Notes:
///
/// - In general state is shared by multiple commands. Such commands must be defined in the
///     same Rust module to support this.
///     For example, `\countdef` shares state with `\count`,
///     and they are implemented together.
///
/// - Commands don't necessarily have state: for example, `\def`, `\advance` and `\the`.
///     These commands
///     are defined without trait bounds on the state, and work automatically with any TeX
///     software built with Texlang.
///
/// - The easiest way to include a component in the state is to make it a direct field
///     of the state.
///     In this case the [implement_has_component] macro can be used to easily implement the
///     trait.
///     The Texlang standard library uses this approach.
///
/// ## The [TexlangState] requirement
///
/// This trait requires that the type also implements [TexlangState].
/// This is only to reduce the number of trait bounds that need to be explicitly
///     specified when implementing TeX commands.
/// In general every command needs to have a bound of the form `S: TexlangState`.
/// Commands that have a `HasComponent` bound don't need to include this other bound explicitly.
pub trait HasComponent<C>: TexlangState {
    /// Return a immutable reference to the component.
    fn component(&self) -> &C;

    /// Return a mutable reference to the component.
    fn component_mut(&mut self) -> &mut C;
}

/// This macro is for implementing the [HasComponent] trait in the special (but common)
///     case when the state is a struct and the component is a direct field of the struct.
///
/// ## Examples
///
/// Implementing a single component:
///
/// ```
/// # mod library_1{
/// #   pub struct Component;
/// # }
/// # use texlang::vm::implement_has_component;
/// # use texlang::traits::*;
/// #
/// struct MyState {
///     component: library_1::Component,
/// }
///
/// impl TexlangState for MyState {}
///
/// implement_has_component![MyState{
///     component: library_1::Component,
/// }];
/// ```
///
/// Implementing multiple components:
///
/// ```
/// # mod library_1{
/// #   pub struct Component;
/// # }
/// # mod library_2{
/// #   pub struct Component;
/// # }
/// # use texlang::vm::implement_has_component;
/// # use texlang::traits::*;
/// #
/// struct MyState {
///     component_1: library_1::Component,
///     component_2: library_2::Component,
/// }
///
/// impl TexlangState for MyState {}
///
/// implement_has_component![MyState{
///     component_1: library_1::Component,
///     component_2: library_2::Component,
/// }];
/// ```
#[macro_export]
macro_rules! implement_has_component {
    ($type: path {
        $( $field: ident: $component: path ),+ $(,)?
    }) => {
        $(
            impl ::texlang::vm::HasComponent<$component> for $type {
                #[inline]
                fn component(&self) -> &$component {
                    &self.$field
                }
                #[inline]
                fn component_mut(&mut self) -> &mut $component {
                    &mut self.$field
                }
            }
        )*
    };
}

pub use implement_has_component;
