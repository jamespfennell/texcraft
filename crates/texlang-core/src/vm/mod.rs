//! The Texlang virtual machine (VM).
//!
//! This module contains the definition of the runtime VM,
//!     various input streams that wrap the VM
//!     and the main function that is used to run Texlang.
//! See the VM documentation in the Texlang book for full documentation.

use super::token::CsName;
use crate::command;
use crate::command::Command;
use crate::command::Fn;
use crate::error;
use crate::token;
use crate::token::catcode::CatCodeMap;
use crate::token::lexer;
use crate::token::trace;
use crate::token::CsNameInterner;
use crate::token::Token;
use crate::token::Value::ControlSequence;
use crate::variable;
use std::collections::HashMap;

mod streams;
pub use streams::*;
/*
ExecutionInput;
pub use streams::ExpandedInput;
pub use streams::HasExpansionState;
pub use streams::TokenStream;
pub use streams::UnexpandedStream;
*/

/// Run the Texlang interpreter for the provided VM.
///
/// It is assumed that the VM has been preloaded with TeX source using the
/// [VM::push_source] method.
pub fn run<S>(
    vm: &mut VM<S>,
    character_handler: command::ExecutionFn<S>,
    undefined_cs_handler: command::ExecutionFn<S>,
) -> anyhow::Result<()> {
    let execution_input = ExecutionInput::new(vm);
    loop {
        let fully_expanded_token = execution_input.next();
        let result = match fully_expanded_token {
            Ok(token) => match token {
                None => {
                    break;
                }
                Some(token) => match token.value() {
                    ControlSequence(name) => {
                        match execution_input.base().commands_map.get_fn(&name) {
                            Some(Fn::Execution(cmd)) => cmd(token, execution_input),
                            Some(Fn::Variable(cmd, addr)) => {
                                let var = command::resolve(*cmd, *addr, token, execution_input)?;
                                variable::set_using_input(
                                    var,
                                    execution_input,
                                    groupingmap::Scope::Local,
                                )
                            }
                            Some(Fn::Character(token_value)) => character_handler(
                                Token::new_from_value(*token_value, token.trace_key()),
                                execution_input,
                            ),
                            None | Some(Fn::Expansion(_)) | Some(Fn::Macro(_)) => {
                                undefined_cs_handler(token, execution_input)
                            }
                        }
                    }
                    token::Value::BeginGroup(_) => {
                        execution_input.begin_group();
                        Ok(())
                    }
                    token::Value::EndGroup(_) => execution_input.end_group(token),
                    _ => character_handler(token, execution_input),
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

/// Handler that returns an error when a control sequence is undefined.
pub fn default_undefined_cs_handler<S>(
    token: Token,
    input: &mut streams::ExecutionInput<S>,
) -> anyhow::Result<()> {
    Err(error::new_undefined_cs_error(token, input.vm()))
}

/// The Texlang VM.
///
/// This is sort of a God object that is passed around all of Texcraft code.
/// However we have many strategies for limiting visibility of various parts of it;
///   see the VM documentation.
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
    pub tracing_macros: i32,
    pub max_input_levels: i32,
}

impl<S> BaseState<S> {
    pub fn new(
        cat_code_map: CatCodeMap,
        initial_built_ins: HashMap<CsName, Command<S>>,
    ) -> BaseState<S> {
        BaseState {
            cat_code_map,
            commands_map: command::Map::new(initial_built_ins, vec![]),
            tracing_macros: 0,
            max_input_levels: 500,
        }
    }
}

impl<S> VM<S> {
    /// Create a new VM.
    pub fn new(
        initial_cat_codes: CatCodeMap,
        initial_built_ins: HashMap<&str, Command<S>>,
        state: S,
    ) -> VM<S> {
        let mut internal: Internal<S> = Default::default();
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
    pub fn get_commands_as_map_slow(&self) -> HashMap<String, Command<S>> {
        let map_1: HashMap<CsName, Command<S>> = self.base_state.commands_map.to_hash_map_slow();
        let mut map = HashMap::new();
        for (cs_name, cmd) in map_1 {
            let cs_name_str = match self.internal.cs_name_interner.resolve(&cs_name) {
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
}

/// Parts of the VM that are only visible to the VM.
struct Internal<S> {
    // The sources form a stack. We store the top element directly on the VM
    // for performance reasons.
    current_source: Source,
    sources: Vec<Source>,

    // The working directory is used as the root for relative file paths.
    working_directory: Option<std::path::PathBuf>,
    cs_name_interner: CsNameInterner,

    tracer: trace::Tracer,

    scratch_space: Vec<Token>,

    groups: Vec<variable::RestoreValues<S>>,
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
            scratch_space: Default::default(),
            groups: Default::default(),
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

/// Helper trait for implementing the component pattern in Texlang.
///
/// With this pattern, a TeX command (like `\time`) can have a single implementation that
///     is shared by many different programs built with Texlang.
/// Additionally, a specific TeX engine can compose many different
///     TeX commands together without worrying about incompatibilities.
/// In both cases the key is making the state associated with the command private.
/// The state being private also means Texlang avoids the globally mutable state problem
///     that is prevalent in the legacy TeX implemenations.
///
/// In the component pattern, the state
///     needed by a specific command like `\count` is isolated in a component, which is a concrete
///     Rust type like a struct.
/// This Rust type is the generic type `C` in the trait.
/// The command (e.g. `\count`) is defined in the same Rust module as the component.
/// The internals of the component are private to the module it is defined in,
///     meaning this piece of state can only be mutated by the command.
/// This makes the state highly local and easier to reason about.
///
/// In order to work the command needs to have access to an instance of the component in which
///     the command will maintain its state.
/// The `HasComponent` trait enforces this.
/// Any state
///     that contains the component can implement the trait.
/// The Rust code defining the
///     command specifies the trait in its trait bounds, and uses the trait to access the component.
///
/// The pattern enables composibility of Texlang code as follows.
/// Different states can include the same component and thus reuse the same commands.
/// Combining multiple commands into one state just involves having the
///     state include all of the relevant components.
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
///     The Texlang standard library exemplifies this approach.
pub trait HasComponent<C> {
    // Returns a immutable reference to the component.
    fn component(&self) -> &C;

    // Returns a mutable reference to the component.
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
use texcraft_stdext::collections::groupingmap;