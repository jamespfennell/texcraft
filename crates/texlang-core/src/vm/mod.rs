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
use crate::token::catcode::CatCodeMap;
use crate::token::lexer;
use crate::token::trace;
use crate::token::CsNameInterner;
use crate::token::Token;
use crate::token::Value;
use crate::variable;
use std::collections::HashMap;
use texcraft_stdext::collections::groupingmap;

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
pub trait Handlers<S> {
    /// Handler to invoke for character tokens.
    ///
    /// This token is _not_ invoked for tokens whose category code is begin group (1), end group (2) or active character (13).
    /// These cases are handled automatically by the VM based on the semantics of the TeX language.
    fn character_handler(token: token::Token, input: &mut ExecutionInput<S>) -> anyhow::Result<()> {
        _ = (token, input);
        Ok(())
    }

    /// Handler to invoke for a control sequence or active character for which no command is defined.
    fn undefined_command_handler(
        token: token::Token,
        input: &mut ExecutionInput<S>,
    ) -> anyhow::Result<()> {
        Err(error::new_undefined_command_error(token, input.vm()))
    }

    /// Handler to invoke for expansion commands that were not expanded.
    ///
    /// This handles the `\the` token in `\noexpand\the`.
    fn unexpanded_expansion_command(
        token: token::Token,
        input: &mut ExecutionInput<S>,
    ) -> anyhow::Result<()> {
        _ = (token, input);
        Ok(())
    }
}

/// Run the VM.
///
/// It is assumed that the VM has been preloaded with TeX source code using the
/// [VM::push_source] method.
pub fn run<S, H: Handlers<S>>(vm: &mut VM<S>) -> anyhow::Result<()> {
    let execution_input = ExecutionInput::new(vm);
    loop {
        let token = execution_input.next();
        let result = match token {
            Ok(token) => match token {
                None => {
                    break;
                }
                Some(token) => match token.value() {
                    Value::ControlSequence(name) => {
                        match execution_input.base().commands_map.get_command(&name) {
                            Some(Command::Execution(cmd, _)) => cmd(token, execution_input),
                            Some(Command::Variable(cmd)) => cmd.clone().set_value_using_input(
                                token,
                                execution_input,
                                groupingmap::Scope::Local,
                            ),
                            Some(Command::Character(token_value)) => {
                                // TODO: the token may be a begin group, end group token, or active character token.
                                // What to do in this case? Check pdfTeX.
                                H::character_handler(
                                    Token::new_from_value(*token_value, token.trace_key()),
                                    execution_input,
                                )
                            }
                            Some(Command::Expansion(_, _)) | Some(Command::Macro(_)) => {
                                H::unexpanded_expansion_command(token, execution_input)
                            }
                            None => H::undefined_command_handler(token, execution_input),
                        }
                    }
                    Value::Active(_) => {
                        // TODO: look up the command and dispatch
                        todo!()
                    },
                    Value::BeginGroup(_) => {
                        execution_input.begin_group();
                        Ok(())
                    }
                    Value::EndGroup(_) => execution_input.end_group(token),
                    Value::MathShift(_)
                    | Value::AlignmentTab(_)
                    | Value::Parameter(_)
                    | Value::Superscript(_)
                    | Value::Subscript(_)
                    | Value::Space(_)
                    | Value::Letter(_)
                    | Value::Other(_) => H::character_handler(token, execution_input),
                },
            },
            Err(err) => Err(err),
        };
        if let Err(mut err) = result {
            error::add_context(&mut err, execution_input);
            return Err(err);
        }
    }
    Ok(())
}

/// The Texlang virtual machine.
pub struct VM<S> {
    pub base_state: BaseState<S>,
    pub custom_state: S,
    pub file_system_ops: Box<dyn FileSystemOps>,
    internal: Internal<S>,
}

/// File system operations that TeX may need to perform.
///
/// These operations are extracted to a trait so that they be mocked out in unit testing.
pub trait FileSystemOps {
    /// Read the entire contents of a file into a string.
    ///
    /// This is implemented by [std::fs::read_to_string].
    fn read_to_string(&self, path: &std::path::Path) -> std::io::Result<String>;
}

struct RealFileSystemOps;

impl FileSystemOps for RealFileSystemOps {
    fn read_to_string(&self, path: &std::path::Path) -> std::io::Result<String> {
        std::fs::read_to_string(path)
    }
}

/// Parts of the state that are required in every Texlang VM, such as the commands map.
///
/// Note that state means specifically parts of the VM that can be modified by
/// execution commands.
pub struct BaseState<S> {
    pub cat_code_map: CatCodeMap,
    pub commands_map: command::Map<S>,
    pub max_input_levels: i32,
}

impl<S> BaseState<S> {
    pub fn new(
        cat_code_map: CatCodeMap,
        initial_built_ins: HashMap<CsName, BuiltIn<S>>,
    ) -> BaseState<S> {
        BaseState {
            cat_code_map,
            commands_map: command::Map::new(initial_built_ins, Default::default()),
            max_input_levels: 500,
        }
    }
}

impl<S> VM<S> {
    /// Create a new VM.
    pub fn new(
        initial_cat_codes: CatCodeMap,
        initial_built_ins: HashMap<&str, BuiltIn<S>>,
        state: S,
        tex_macro_hook: Option<fn(texmacro::HookInput<S>)>,
    ) -> VM<S> {
        let mut internal: Internal<S> = Default::default();
        if let Some(tex_macro_hook) = tex_macro_hook {
            internal.tex_macro_hook = tex_macro_hook;
        }
        let initial_built_ins = initial_built_ins
            .into_iter()
            .map(|(key, value)| (internal.cs_name_interner.get_or_intern(key), value))
            .collect();
        VM {
            custom_state: state,
            base_state: BaseState::new(initial_cat_codes, initial_built_ins),
            internal,
            file_system_ops: Box::new(RealFileSystemOps {}),
        }
    }

    /// Add new source code to the VM.
    ///
    /// TeX input source code is organized as a stack.
    /// Pushing source code onto the stack will mean it is executed first.
    pub fn push_source(&mut self, file_name: String, source_code: String) -> anyhow::Result<()> {
        self.internal.push_source(
            None,
            file_name,
            source_code,
            self.base_state.max_input_levels,
        )
    }

    /// Clear all source code from the VM.
    pub fn clear_sources(&mut self) {
        self.internal.clear_sources()
    }

    /// Return the current working directory, or [None] if it could not be determined.
    pub fn working_directory(&self) -> Option<&std::path::Path> {
        self.internal.working_directory.as_deref()
    }

    /// Return a regular hash map with all the commands as they are currently defined.
    ///
    /// This function is extremely slow and is only intended to be invoked on error paths.
    pub fn get_commands_as_map_slow(&self) -> HashMap<String, BuiltIn<S>> {
        let map_1: HashMap<CsName, BuiltIn<S>> = self.base_state.commands_map.to_hash_map_slow();
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

    /// Return the VM's TeX macro hook.
    #[inline]
    pub fn tex_macro_hook(&self) -> fn(texmacro::HookInput<S>) {
        self.internal.tex_macro_hook
    }

    fn begin_group(&mut self) {
        self.base_state.commands_map.begin_group();
        self.internal.groups.push(Default::default());
    }

    fn end_group(&mut self, token: Token) -> anyhow::Result<()> {
        match self.internal.groups.pop() {
            None => Err(error::TokenError::new(token, "unexpected end of group").into()),
            Some(group) => {
                assert![self.base_state.commands_map.end_group()];
                group.restore(&mut self.base_state, &mut self.custom_state);
                Ok(())
            }
        }
    }

    pub fn trace(&self, token: Token) -> trace::Trace {
        self.internal
            .tracer
            .trace(token, &self.internal.cs_name_interner)
    }
}

/// Parts of the VM that cannot be edited by TeX commands at runtime.
struct Internal<S> {
    // The sources form a stack. We store the top element directly on the VM
    // for performance reasons.
    current_source: Source,
    sources: Vec<Source>,

    // The working directory is used as the root for relative file paths.
    working_directory: Option<std::path::PathBuf>,
    cs_name_interner: CsNameInterner,

    tracer: trace::Tracer,

    token_buffers: std::collections::BinaryHeap<TokenBuffer>,

    groups: Vec<variable::internal::RestoreValues<S>>,
    tex_macro_hook: fn(texmacro::HookInput<S>),
}

impl<S> Default for Internal<S> {
    fn default() -> Self {
        Internal {
            current_source: Default::default(),
            sources: Default::default(),
            working_directory: match std::env::current_dir() {
                Ok(path_buf) => Some(path_buf),
                Err(err) => {
                    print!("failed to determine the working directory: {err}");
                    None
                }
            },
            cs_name_interner: Default::default(),
            tracer: Default::default(),
            token_buffers: Default::default(),
            groups: Default::default(),
            tex_macro_hook: texmacro::no_op_hook,
        }
    }
}

impl<S> Internal<S> {
    fn push_source(
        &mut self,
        token: Option<Token>,
        file_name: String,
        source_code: String,
        max_input_levels: i32,
    ) -> anyhow::Result<()> {
        if self.sources.len() + 1 >= max_input_levels.try_into().unwrap() {
            return Err(anyhow::anyhow!(
                "maximum number of input levels ({}) exceeded",
                max_input_levels
            ));
        }
        let trace_key_range = self
            .tracer
            .register_source_code(token, file_name, &source_code);
        let mut new_source = Source::new(source_code, trace_key_range);
        std::mem::swap(&mut new_source, &mut self.current_source);
        self.sources.push(new_source);
        Ok(())
    }

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
        match self.sources.pop() {
            None => false,
            Some(source) => {
                self.current_source = source;
                true
            }
        }
    }
}

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
        Source::new("".to_string(), trace::KeyRange::empty())
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
///     software built with Texlang. (Yay!)
///
/// - The easiest way to include a component in the state is to make it a direct field
///     of the state.
///     In this case the [implement_has_component] macro can be used to easily implement the
///     trait.
///     The Texlang standard library uses this approach.
pub trait HasComponent<C> {
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
/// # use texlang_core::vm::implement_has_component;
/// #
/// struct MyState {
///     component: mylibrary::Component,
/// }
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
/// # use texlang_core::vm::implement_has_component;
/// #
/// struct MyState {
///     component_1: mylibrary1::Component,
///     component_2: mylibrary2::Component,
/// }
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
            impl ::texlang_core::vm::HasComponent<$component> for $type {
                fn component(&self) -> &$component {
                    &self.$field
                }
                fn component_mut(&mut self) -> &mut $component {
                    &mut self.$field
                }
            }
        )*
    };
}

pub use implement_has_component;
