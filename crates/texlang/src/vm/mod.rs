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
use crate::texmacro;
use crate::token;
use crate::token::lexer;
use crate::token::trace;
use crate::token::CsNameInterner;
use crate::token::Token;
use crate::token::Value;
use crate::variable;
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;
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
    fn character_handler(
        token: token::Token,
        input: &mut ExecutionInput<S>,
    ) -> Result<(), Box<error::Error>> {
        _ = (token, input);
        Ok(())
    }

    /// Handler to invoke for a control sequence or active character for which no command is defined.
    fn undefined_command_handler(
        token: token::Token,
        input: &mut ExecutionInput<S>,
    ) -> Result<(), Box<error::Error>> {
        Err(error::UndefinedCommandError::new(input.vm(), token).into())
    }

    /// Handler to invoke for expansion commands that were not expanded.
    ///
    /// This handles the `\the` token in `\noexpand\the`.
    fn unexpanded_expansion_command(
        token: token::Token,
        input: &mut ExecutionInput<S>,
    ) -> Result<(), Box<error::Error>> {
        _ = (token, input);
        Ok(())
    }
}

pub struct DefaultHandlers;

impl<S: TexlangState> Handlers<S> for DefaultHandlers {}

impl<S: TexlangState> VM<S> {
    /// Run the VM.
    ///
    /// It is assumed that the VM has been preloaded with TeX source code using the
    /// [VM::push_source] method.
    pub fn run<H: Handlers<S>>(&mut self) -> Result<(), Box<error::Error>> {
        let input = ExecutionInput::new(self);
        loop {
            let token = match input.next()? {
                None => break,
                Some(token) => token,
            };
            // TODO: propagate all of these
            match token.value() {
                Value::CommandRef(command_ref) => {
                    match input.commands_map().get_command(&command_ref) {
                        Some(Command::Execution(cmd, _)) => {
                            if let Err(err) = cmd(token, input) {
                                return Err(error::Error::new_propagated(
                                    input.vm(),
                                    error::PropagationContext::Execution,
                                    token,
                                    err,
                                ));
                            }
                        }
                        Some(Command::Variable(cmd)) => {
                            let cmd = cmd.clone();
                            let scope = S::variable_assignment_scope_hook(input.state_mut());
                            cmd.set_value_using_input(token, input, scope)?;
                        }
                        Some(Command::CharacterTokenAlias(token_value)) => {
                            // TODO: the token may be a begin group, or end group token.
                            // What to do in this case? Check pdfTeX.
                            // Probably we need to push it to the front of the token stack;
                            // because it is not a command, the right handler will be called.
                            H::character_handler(
                                Token::new_from_value(*token_value, token.trace_key()),
                                input,
                            )?
                        }
                        Some(Command::Expansion(_, _)) | Some(Command::Macro(_)) => {
                            H::unexpanded_expansion_command(token, input)?
                        }
                        Some(Command::Character(c)) => H::character_handler(
                            token::Token::new_other(*c, token.trace_key()),
                            input,
                        )?,
                        None => H::undefined_command_handler(token, input)?,
                    }
                }
                Value::BeginGroup(_) => {
                    input.begin_group();
                }
                Value::EndGroup(_) => {
                    input.end_group(token)?;
                }
                Value::MathShift(_)
                | Value::AlignmentTab(_)
                | Value::Parameter(_)
                | Value::Superscript(_)
                | Value::Subscript(_)
                | Value::Space(_)
                | Value::Letter(_)
                | Value::Other(_) => H::character_handler(token, input)?,
            };
        }
        Ok(())
    }
}

#[derive(Debug)]
struct EndOfGroupError {
    trace: trace::SourceCodeTrace,
}

impl error::TexError for EndOfGroupError {
    fn kind(&self) -> error::Kind {
        error::Kind::Token(&self.trace)
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

    /// File system operations
    ///
    /// By default this is real operations on the file system.
    /// It is replaceable to support both unit testing, and running Texcraft
    ///     in environments like WASM in the browser.
    pub file_system: Box<dyn FileSystem>,

    /// Input operations from the terminal.
    ///
    /// By default this reads from standard in.
    pub terminal_in: Rc<RefCell<dyn TerminalIn>>,

    /// Writer that writes to the terminal
    ///
    /// Defaults to standard error.
    /// In other contexts (for example WASM) this may be changed to e.g. write to a string
    ///     that can be displayed in the browser.
    pub terminal_out: Rc<RefCell<dyn std::io::Write>>,

    /// Writer that writes to the log file
    ///
    /// Defaults to a sink writer that writes nothing.
    pub log_file: Rc<RefCell<dyn std::io::Write>>,

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

/// File system operations that TeX may need to perform.
///
/// These operations are extracted to a trait so that they be mocked out in unit testing
///     and in execution contexts like WASM.
pub trait FileSystem {
    /// Read the entire contents of a file into a string.
    ///
    /// This is implemented by [std::fs::read_to_string].
    fn read_to_string(&self, path: &std::path::Path) -> std::io::Result<String>;

    /// Write a slice of bytes to a file.
    ///
    /// This is implemented by [std::fs::write].
    fn write_bytes(&self, path: &std::path::Path, contents: &[u8]) -> std::io::Result<()>;
}

struct RealFileSystem;

impl FileSystem for RealFileSystem {
    fn read_to_string(&self, path: &std::path::Path) -> std::io::Result<String> {
        std::fs::read_to_string(path)
    }
    fn write_bytes(&self, path: &std::path::Path, contents: &[u8]) -> std::io::Result<()> {
        std::fs::write(path, contents)
    }
}

/// Input operations from the terminal.
///
/// These operations are extracted to a trait so that they be mocked out in unit testing
///     and in execution contexts like WASM.
pub trait TerminalIn {
    /// Read a line from the terminal and append it to the provided buffer.
    fn read_line(&mut self, prompt: Option<&str>, buffer: &mut String) -> std::io::Result<()>;
}

struct RealTerminalIn;

impl TerminalIn for RealTerminalIn {
    fn read_line(&mut self, prompt: Option<&str>, buffer: &mut String) -> std::io::Result<()> {
        if let Some(prompt) = prompt {
            eprint!("\n{prompt}")
        }
        let stdin = std::io::stdin();
        stdin.read_line(buffer)?;
        Ok(())
    }
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
    /// The default implementation returns the cat code used in plainTeX.
    fn cat_code(&self, c: char) -> token::CatCode {
        token::CatCode::PLAIN_TEX_DEFAULTS
            .get(c as usize)
            .copied()
            .unwrap_or_default()
    }

    /// Get current end line char, or [None] if it's undefined.
    ///
    /// The default implementation returns `\r`.
    fn end_line_char(&self) -> Option<char> {
        Some('\r')
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
    ) -> Result<Option<Token>, Box<command::Error>> {
        _ = (token, input, tag);
        Ok(None)
    }

    /// Hook that runs before new source code is added.
    ///
    /// This hook is designed to support the max input levels constant.
    fn pre_source_code_addition_hook(
        token: Option<token::Token>,
        num_existing_sources: usize,
    ) -> Result<(), Box<error::Error>> {
        _ = token;
        // TODO: make this a token error
        if num_existing_sources + 1 >= 100 {
            /* TODO
             */
        }
        Ok(())
    }

    /// Hook that determines the scope of a variable assignment.
    ///
    /// This hook is designed to support the \global and \globaldefs commands.
    fn variable_assignment_scope_hook(state: &mut Self) -> groupingmap::Scope {
        _ = state;
        groupingmap::Scope::Local
    }
}

impl TexlangState for () {}

impl<S: Default> VM<S> {
    /// Create a new VM.
    pub fn new(initial_built_ins: HashMap<&str, BuiltIn<S>>) -> Box<VM<S>> {
        let mut internal = Internal::new(Default::default());
        let initial_built_ins = initial_built_ins
            .into_iter()
            .map(|(key, value)| (internal.cs_name_interner.get_or_intern(key), value))
            .collect();
        Box::new(VM {
            state: Default::default(),
            commands_map: command::Map::new(initial_built_ins),
            internal,
            file_system: Box::new(RealFileSystem {}),
            terminal_in: Rc::new(RefCell::new(RealTerminalIn {})),
            terminal_out: Rc::new(RefCell::new(std::io::stderr())),
            log_file: Rc::new(RefCell::new(std::io::sink())),
            working_directory: match std::env::current_dir() {
                Ok(path_buf) => Some(path_buf),
                Err(err) => {
                    println!("failed to determine the working directory: {err}");
                    None
                }
            },
        })
    }
}

/// Deserialize a Texlang VM.
///
/// See the [`serde` submodule](serde) for more information on deserialization,
///     and for functions that don't require a deserializer.
#[cfg(feature = "serde")]
impl<'de, S: ::serde::Deserialize<'de>> VM<S> {
    pub fn deserialize<D: ::serde::Deserializer<'de>>(
        deserializer: D,
        initial_built_ins: HashMap<&str, BuiltIn<S>>,
    ) -> Box<Self> {
        serde::deserialize(deserializer, initial_built_ins)
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
    ) -> Result<(), Box<error::Error>> {
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
    pub fn get_commands_as_map_slow(&self) -> HashMap<String, BuiltIn<S>> {
        let map_1: HashMap<CsName, BuiltIn<S>> = self.commands_map.to_hash_map_slow();
        let mut map = HashMap::new();
        for (cs_name, cmd) in map_1 {
            let cs_name_str = match self.internal.cs_name_interner.resolve(cs_name) {
                None => continue,
                Some(cs_name_str) => cs_name_str,
            };
            map.insert(cs_name_str.to_string(), cmd);
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

    fn begin_group(&mut self) {
        self.commands_map.begin_group();
        self.internal.save_stack.push(Default::default());
    }

    fn end_group(&mut self, token: token::Token) -> Result<(), Box<error::Error>> {
        match self.commands_map.end_group() {
            Ok(()) => (),
            Err(_) => {
                return Err(EndOfGroupError {
                    trace: self.trace(token),
                }
                .into())
            }
        }
        let group = self.internal.save_stack.pop().unwrap();
        group.restore(ExecutionInput::new(self));
        Ok(())
    }

    pub fn trace(&self, token: Token) -> trace::SourceCodeTrace {
        self.internal
            .tracer
            .trace(token, &self.internal.cs_name_interner)
    }

    pub fn trace_end_of_input(&self) -> trace::SourceCodeTrace {
        self.internal.tracer.trace_end_of_input()
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

    #[cfg_attr(feature = "serde", serde(skip))]
    token_buffers: std::collections::BinaryHeap<TokenBuffer>,

    // The save stack is handled manually in (de)serialization.
    // We need to use special logic in combination with the command map in order to serialize the
    // variable pointers that are in the stack.
    #[cfg_attr(feature = "serde", serde(skip))]
    save_stack: Vec<variable::SaveStackElement<S>>,
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
        }
    }
}
impl<S: TexlangState> Internal<S> {
    fn push_source(
        &mut self,
        token: Option<Token>,
        file_name: PathBuf,
        source_code: String,
    ) -> Result<(), Box<error::Error>> {
        S::pre_source_code_addition_hook(token, self.sources.len())?;
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
        self.0.capacity().partial_cmp(&other.0.capacity())
    }
}

impl Ord for TokenBuffer {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.capacity().cmp(&other.0.capacity())
    }
}

/// Helper trait for implementing the component pattern in Texlang.
///
/// The component pattern is a design pattern used when implementing TeX commands that require some state.
/// An example of a stateful TeX command is `\year`, which needs to store the current year somewhere.
/// When the component pattern is used, a stateful TeX command
///     can have a single implementation that
///     is used by multiple TeX engines built with Texlang.
/// Additionally, a specific TeX engine can compose many different
///     stateful TeX commands together without worrying about conflicts between their state.
/// The component pattern is Texlang's main solution to the problem of
///     global mutable state that is pervasive in the original implementations of TeX.
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
/// # mod mylibrary{
/// #   pub struct Component;
/// # }
/// # use texlang::vm::implement_has_component;
/// # use texlang::traits::*;
/// #
/// struct MyState {
///     component: mylibrary::Component,
/// }
///
/// impl TexlangState for MyState {}
///
/// implement_has_component![MyState, mylibrary::Component, component];
/// ```
///
/// Implementing multiple components:
///
/// ```
/// # mod mylibrary1{
/// #   pub struct Component;
/// # }
/// # mod mylibrary2{
/// #   pub struct Component;
/// # }
/// # use texlang::vm::implement_has_component;
/// # use texlang::traits::*;
/// #
/// struct MyState {
///     component_1: mylibrary1::Component,
///     component_2: mylibrary2::Component,
/// }
///
/// impl TexlangState for MyState {}
///
/// implement_has_component![
///     MyState,
///     (mylibrary1::Component, component_1),
///     (mylibrary2::Component, component_2),
/// ];
/// ```
#[macro_export]
macro_rules! implement_has_component {
    ( $type: path, $component: path, $field: ident ) => {
        implement_has_component![$type, ($component, $field),];
    };
    ( $type: path, $(($component: path, $field: ident),)+) => {
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
