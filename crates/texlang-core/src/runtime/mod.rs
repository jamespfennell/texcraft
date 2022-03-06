//! The Texlang runtime.
//!
//! ## Historical context
//!
//! Knuth's original implementation of TeX stores its state in global mutable variables.
//! Most other TeX distributions do the same because they are forked from Knuth’s implementation.
//!
//! This situation is problematic for two reasons. First, global mutable state is just bad.
//! We mention specifically that global mutable state makes software very hard to reason about
//!     because it precludes real separation of concerns.
//! Any two parts of the system can potentially interact implicitly through the shared state.
//!
//! Second, each TeX distribution makes assumptions about the specific structure of the state.
//! Are integers 32-bit or 64-bit?
//! Answers to such questions are hard coded into the global variables in the source code.
//! This makes it difficult or impossible for multiple distributions with conflicting state structures to share code.
//!
//! Texcraft solves the second problem,
//!     and works towards solving the first,
//!     by moving all of the state out of the global namespace and into a state data structure.
//! After that, Texcraft uses two Rust features - the trait system and name visibility -
//!     to implement what we call state segmentation.
//! This system segments the state into small local components,
//!     each of which can generally be altered only by commands defined in the same Rust module as the component.
//!
//! ## State in Texcraft
//!
//! State in Texcraft is stored in structs of type `Base<S>`.
//! Parts of the state that every TeX distribution needs to have (like the map of available commands)
//!     is stored in the `Base` struct directly.
//! However, most of the state is stored in an auxiliary structure of type `S`,
//!     which is included in the base state using the `S` generic parameter.
//! This system allows each TeX engine to customize the structure of the state for its own needs by
//!     providing different concrete types for `S`.
//! Once all of the state is in a data structure, multiple distributions can coexist peacefully.
//! The same source code and binary can instantiate states for two TeX distributions simultaneously
//!     (`Base<TeXState>` and `Base<pdfTeXState>`).
//!
//! The state object is passed around to basically every part of Texcraft,
//! and so at this point it is still global and mutable.
//! Texcraft has two strategies for segmenting the state into small,
//! local components with restricted access. We call this process **state segmentation**.
//!
//! ### Strategy one: trait bounds on `S`.
//!
//! Individual parts of a TeX distribution do not need access to all of the state.
//! As an example let’s consider the time commands which are implemented in the library::time module.
//! (The command `\year` returns the current year as an integer.)
//! These commands only need to access the time variables, and need nothing else from the state.
//! The time variables are stored in a `TimeComponent`.
//! The constructor of the `TimeComponent` initializes these variables to the current time when the engine starts up.
//!
//! For the time commands to work, they only require one thing: that the state in use has a TimeComponent.
//!  This requirement is stated directly using the HasTime trait:
//!
//! ```
//! # struct TimeComponent;
//! trait HasTime {
//!     fn get_time(&self) -> &TimeComponent;
//!     fn get_timemut(&mut self) -> &mut TimeComponent;
//! }
//! ```
//!
//! The time command definitions use this trait bound:
//!
//! <fill this in when time is implemented>
//!
//! Using trait bounds like this gives us two things:
//!
//! - We can be 100% confident that the time commands are not altering any other part of the state.
//!     From inspecting the trait bounds only, we know that these commands won’t,
//!     for example, redefine some primitives.
//!     We don’t need to examine the implementations: looking at the trait bounds is sufficient.
//!
//! - Our time commands will work with any state: TeX, pdfTex, XeTeX!
//!     As long as the state implemented satisfies the HasTime trait,
//!     the time commands can be used. Note that implementing the trait is completely trivial:
//!     the state type just needs to have a TimeComponent field and be wired up appropriately.
//!
//!
//! ### Strategy two: Rust's name visibility rules.
//!
//! We know that the time commands won't edit other parts of the state because
//!     they use restrictive trait bounds.
//! But what about the inverse problem - can other rogue commands interfere with time?
//! Could we write a malicious command that randomly changes the current time?
//!
//! Based on the previous section, this seems possible:
//!     our new malicious command just needs to require `HasTimeComponent` as a trait bound on the state,
//!     and then it can access and edit time.
//! But if we try to implement this scheme it won’t work.
//! The Rust code will refuse to compile.
//!
//! Take a look at the TimeComponent again.
//! Other than its constructor, the TimeComponent has no public methods.
//! This means that no command defined outside of the time module can actually change the internals of the TimeComponent.
//! This is true even if the command has access to it using the TimeComponent trait bound.[^note]
//!
//! [^note]: there is an exception: with a mutable reference to the `TimeCopmonent` a malicious command
//!     could overrite the component entirely using a deference assignment. Even then, it could only
//!     replace the component with a new component creating using the public constructor.
//!
//! This is the other big ingredient of Texcraft's state segmentation system.
//! We place parts of the state in private fields of component types defined in separate Rust modules.
//! Then, using Rust's visibility rules, we can guarantee that the only changes to that Component are
//! coming from commands defined inside the module.
//!
//! ## Summary
//!
//! - Texcraft commands are only given access to parts of the state they need using trait bounds.
//! - By putting their state in private fields of a component type,
//!     commands can ensure their state cannot be edited by other commands defined outside their Rust module.
//! - Commands built with this framework can be used in any TeX engine.
//!

use super::token::CsName;
use crate::command::CommandsMap;
use crate::command::{self, Command};
use crate::error;
use crate::prelude::*;
use crate::token;
use crate::token::catcode::CatCodeMap;
use crate::token::lexer;
use crate::token::stream::Stream;
use crate::token::CsNameInterner;
use crate::variable;
use std::collections::HashMap;

mod streams;
pub use streams::ExecutionInput;
pub use streams::ExpandedInput;
pub use streams::HasExpansionState;
pub use streams::UnexpandedStream;

pub fn run<S>(
    execution_input: &mut ExecutionInput<S>,
    character_handler: fn(token::Token, &mut ExecutionInput<S>) -> anyhow::Result<()>,
    undefined_cs_handler: fn(token::Token, &mut ExecutionInput<S>) -> anyhow::Result<()>,
) -> anyhow::Result<()> {
    loop {
        let fully_expanded_token = execution_input.next();
        match fully_expanded_token {
            Ok(token) => match token {
                None => {
                    break;
                }
                Some(token) => match token.value() {
                    ControlSequence(name) => {
                        match execution_input.base().commands_map.get(&name) {
                            Some(Command::Execution(cmd_ref)) => {
                                // We need to copy the command to avoid a borrow checker error. This is because
                                // `cmd_ref` keeps an immutable reference to the state alive, but in the `call`
                                // invocation below we pass in a mutable reference to the state. The borrow
                                // checker is actually surfacing a genuine TeX edge case here: if you input
                                // something like \let\let=\def then the reference to the command \let in the
                                // commands map is overwritten at some point during the execution of \let. You
                                // could also imagine implementing an \undef command that would result in the
                                // reference becoming null during the execution.
                                let cmd = *cmd_ref;
                                cmd.call(token, execution_input)?;
                            }
                            Some(Command::Variable(cmd_ref)) => {
                                let cmd = *cmd_ref;
                                let var = cmd.resolve(token, execution_input.regular())?;
                                variable::set(var, token, execution_input)?;
                            }
                            Some(Command::Character(token)) => {
                                character_handler(*token, execution_input)?;
                            }
                            None | Some(Command::Expansion(_)) | Some(Command::Macro(_)) => {
                                undefined_cs_handler(token, execution_input)?;
                            }
                        }
                    }
                    _ => {
                        character_handler(token, execution_input)?;
                    }
                },
            },
            Err(mut error) => {
                error::add_context(&mut error, execution_input);
                return Err(error);
            }
        };
    }
    Ok(())
}

pub fn default_undefined_cs_handler<S>(
    token: token::Token,
    input: &mut streams::ExecutionInput<S>,
) -> anyhow::Result<()> {
    Err(error::new_undefined_cs_error(token, input.env()))
}

/// The Texlang runtime environment.
///
/// This is sort of a God object that is passed around all of Texcraft code.
/// However we have many strategies for limiting visibility of various parts of it.
pub struct Env<S> {
    pub base_state: BaseState<S>,
    pub custom_state: S,
    pub internal: InternalEnv,
    pub file_system_ops: Box<dyn FileSystemOps>,
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

/// Parts of the state that are required in every Texlang environment, such as the commands map.
pub struct BaseState<S> {
    pub cat_code_map: CatCodeMap,
    pub commands_map: CommandsMap<S>,
    pub tracing_macros: i32,
    pub max_input_levels: i32,
}

impl<S> BaseState<S> {
    pub fn new(cat_code_map: CatCodeMap) -> BaseState<S> {
        BaseState {
            cat_code_map,
            commands_map: Default::default(),
            tracing_macros: 0,
            max_input_levels: 500,
        }
    }
}

impl<S> Env<S> {
    /// Create a new runtime environment.
    pub fn new(initial_cat_codes: CatCodeMap, state: S) -> Env<S> {
        Env {
            custom_state: state,
            base_state: BaseState::new(initial_cat_codes),
            internal: Default::default(),
            file_system_ops: Box::new(RealFileSystemOps {}),
        }
    }

    pub fn push_source(&mut self, source_code: String) -> anyhow::Result<()> {
        self.internal
            .push_source(source_code, self.base_state.max_input_levels)
    }

    pub fn set_command<A: AsRef<str>, B: Into<command::Command<S>>>(&mut self, name: A, cmd: B) {
        self.base_state.commands_map.insert(
            self.internal.cs_name_interner.get_or_intern(name),
            B::into(cmd),
        )
    }

    /// Return the current working directory, or [None] if it could not be determined.
    pub fn working_directory(&self) -> Option<&std::path::Path> {
        self.internal.working_directory.as_deref()
    }

    /// Return a regular hash map with all the commands as they are currently defined.
    ///
    /// This function is extremely slow and is only intended to be invoked on error paths.
    pub fn get_commands_as_map(&self) -> HashMap<String, command::Command<S>> {
        let map_1: HashMap<CsName, Command<S>> = self.base_state.commands_map.to_hash_map();
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

    #[inline]
    pub fn cs_name_interner(&self) -> &CsNameInterner {
        &self.internal.cs_name_interner
    }
}

/// Parts of the runtime environment that are only visible to the runtime itself.
pub struct InternalEnv {
    // The sources form a stack. We store the top element directly on the env
    // for performance reasons.
    current_source: Source,
    sources: Vec<Source>,

    // The working directory is used as the root for relative file paths.
    working_directory: Option<std::path::PathBuf>,
    cs_name_interner: CsNameInterner,
    traceback_checkpoints: HashMap<token::TracebackId, String>,
    next_free_traceback_id: token::TracebackId,
}

impl Default for InternalEnv {
    fn default() -> Self {
        InternalEnv {
            current_source: Default::default(),
            sources: Default::default(),
            working_directory: match std::env::current_dir() {
                Ok(path_buf) => Some(path_buf),
                Err(err) => {
                    print!("failed to determine the working directory: {}", err);
                    None
                }
            },
            cs_name_interner: Default::default(),
            traceback_checkpoints: Default::default(),
            next_free_traceback_id: Default::default(),
        }
    }
}

impl InternalEnv {
    fn push_source(&mut self, source_code: String, max_input_levels: i32) -> anyhow::Result<()> {
        if self.sources.len() + 1 >= max_input_levels.try_into().unwrap() {
            return Err(anyhow::anyhow!(
                "maximum number of input levels ({}) exceeded",
                max_input_levels
            ));
        }
        self.traceback_checkpoints
            .insert(self.next_free_traceback_id, source_code.clone());
        let source_code_len = source_code.len();
        let mut new_source = Source::new(source_code, self.next_free_traceback_id);
        self.next_free_traceback_id += token::TracebackId::try_from(source_code_len).unwrap();
        std::mem::swap(&mut new_source, &mut self.current_source);
        self.sources.push(new_source);
        Ok(())
    }

    #[inline]
    fn push_expansion(&mut self, expansion: &[token::Token]) {
        self.current_source
            .expansions
            .extend(expansion.iter().rev());
    }

    #[inline]
    fn expansions_mut(&mut self) -> &mut Vec<token::Token> {
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
    pub fn new(source_code: String, next_free_traceback_id: token::TracebackId) -> Source {
        Source {
            expansions: Vec::with_capacity(32),
            root: lexer::Lexer::new(source_code, next_free_traceback_id),
        }
    }
}

impl Default for Source {
    fn default() -> Self {
        Source::new("".to_string(), 0)
    }
}

/// Helper trait for implementing the component pattern in Texlang.
///
/// With this pattern, a TeX command (like `\time`) can have a single implementation that
///     is shared by many different pieces of software built with Texlang.
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
/// # use texlang_core::runtime::implement_has_component;
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
/// # use texlang_core::runtime::implement_has_component;
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
            impl ::texlang_core::runtime::HasComponent<$component> for $type {
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
