//! How state is organized in Texcraft, and the base state.
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

use crate::command;
use crate::token;
use crate::token::catcode::CatCodeMap;
use crate::token::CsNameInterner;
use std::collections::HashMap;
use texcraft_stdext::collections::groupingmap::GroupingVec;

use super::token::CsName;

/// The parts of state that every TeX engine must have.
pub struct Base<S> {
    input_related: InputRelatedState,
    primitives: GroupingVec<command::Command<S>>,
    pub state: S,

    // Only used in exec mode.
    // TODO: this kind of feels like it shouldn't be on the state?
    pub exec_output: Vec<token::Token>,
    pub num_trailing_newlines: usize,
    pub tracing_macros: i32,
}

/// Base state that every TeX state is expected to include using composition.
impl<S> Base<S> {
    /// Create a new BaseState.
    pub fn new(initial_cat_codes: CatCodeMap, state: S) -> Base<S> {
        Base {
            input_related: InputRelatedState::new(initial_cat_codes),
            primitives: Default::default(),
            state,
            exec_output: Vec::new(),
            num_trailing_newlines: 0,
            tracing_macros: 0,
        }
    }

    #[inline]
    pub fn get_command(&self, name: &token::CsName) -> Option<&command::Command<S>> {
        self.primitives.get(&name.to_usize())
    }

    pub fn set_command<A: AsRef<str>, B: Into<command::Command<S>>>(&mut self, name: A, cmd: B) {
        self.primitives.insert(
            self.input_related
                .cs_name_interner
                .get_or_intern(name)
                .to_usize(),
            B::into(cmd),
        )
    }

    pub fn set_command_using_csname<B: Into<command::Command<S>>>(
        &mut self,
        name: token::CsName,
        cmd: B,
    ) {
        self.primitives.insert(name.to_usize(), B::into(cmd))
    }

    /// Return a regular hash map with all the commands as they are currently defined.
    ///
    /// This function is extremely slow and is only intended to be invoked on error paths.
    pub fn get_commands_as_map(&self) -> HashMap<String, command::Command<S>> {
        let mut map = HashMap::new();
        for (key, value) in self.primitives.backing_container().iter().enumerate() {
            // We should potentially be panicing instead of continuing in the following 3 expressions.
            let cmd = match value {
                None => continue,
                Some(cmd) => cmd.clone(),
            };
            let cs_name = match CsName::try_from_usize(key) {
                None => continue,
                Some(cs_name) => cs_name,
            };
            let cs_name_str = match self.input_related.cs_name_interner.resolve(&cs_name) {
                None => continue,
                Some(cs_name_str) => cs_name_str,
            };
            map.insert(cs_name_str.to_string(), cmd);
        }
        map
    }

    #[inline]
    pub fn cs_name_interner(&self) -> &CsNameInterner {
        &self.input_related.cs_name_interner
    }

    #[inline]
    pub fn cat_codes(&self) -> &CatCodeMap {
        &self.input_related.cat_codes
    }

    #[inline]
    pub fn cat_codes_mut(&mut self) -> &mut CatCodeMap {
        &mut self.input_related.cat_codes
    }

    #[inline]
    pub fn input_related(&mut self) -> &mut InputRelatedState {
        &mut self.input_related
    }

    #[inline]
    pub fn input_related_and_commands(
        &mut self,
    ) -> (&mut InputRelatedState, &GroupingVec<command::Command<S>>) {
        (&mut self.input_related, &self.primitives)
    }
}

pub struct InputRelatedState {
    cs_name_interner: CsNameInterner,
    cat_codes: CatCodeMap,
}

impl InputRelatedState {
    pub fn new(initial_cat_codes: CatCodeMap) -> InputRelatedState {
        InputRelatedState {
            cs_name_interner: Default::default(),
            cat_codes: initial_cat_codes,
        }
    }

    #[inline]
    pub fn cat_codes(&self) -> &CatCodeMap {
        &self.cat_codes
    }

    #[inline]
    pub fn cs_name_interner_mut(&mut self) -> &mut CsNameInterner {
        &mut self.cs_name_interner
    }
}
