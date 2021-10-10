//! TeX language parsing and execution.
//!
//! This document is a high-level overview of how Texcraft processes the TeX language,
//!   with links to the specific Rust modules that implement different pieces.
//! Feel free to jump to the end to explore the modules directly.
//!
//! # The Texcraft model for processing TeX
//!
//! Parsing and executing a TeX file consists of a number of sequential steps:
//!
//! 1. Reading the input into tokens (lexing).
//! 1. Expanding user defined macros and expansion primitives.
//! 1. Executing commands that make it through expansion.
//!     An execution command can do basically anything: perform some typesetting,
//!     make a variable assignment, start a new group, etc.
//! 1. Outputting the document.
//!
//! Ideally, each of these steps would be implemented as a separate compiler “pass”.
//! Unfortunately, due to the structure of the TeX language,
//!   these steps are all highly interconnected and cannot be split up.
//! It's easy to create pathological TeX files that perform some typesetting and based on the result
//!   immediately change the lexing rules.
//! These types of TeX files show that the different steps cannot happen in isolation.
//!
//! ## Input and lexing
//!
//! The core object in the input step is the [TeX token](token::Token).
//!
//! The [input unit](input::Unit) manages all aspects of input.
//! It keeps track of which files are currently being read.
//! It also tracks which expansions have occured because those change the input stream.
//!
//! New tokens are read using methods of the input unit like [peek](input::Unit::peek) and [next](input::Unit::next).
//! Most of the time, to create a new token the unit will need to read a character from the input file and tokenize it;
//!   this lexing step is done by the [Texcraft lexer](token::lexer) which is embedded in the input unit.
//!
//! So, while we do model TeX processing sequentially, these steps are all happening at the same time.
//! An execution command may read some input, which may trigger some expansions to happen, which
//! may in turn require some new input to be read and lexed.
//!
//! ## Expansion and execution: prelude
//!
//! Next up is expansion.
//! Expansion and execution are both TeX engine specific:
//!   different engines provide different expansion and execution primitives.
//! First we will describe how Texcraft handles this diversity.
//!
//! One of the core philosophies of Texcraft is that the same code should be directly usable for distinct TeX engines.
//! A single implementation of `\expandafter` should be usable in any engine built with Texcraft.
//!
//! On the other hand, TeX engines are highly variable and this needs to be taken into account.
//! Some of the things that can vary cross distributions include:
//!
//! - **Primitives**: New TeX engines generally add additional primitive commands
//!     (i.e., commands implemented in the interpreter and not as user defined macros).
//!     PdfTeX has the `\currentifbranch` command which is not in TeX 82.
//!
//! - **State**: When a TeX document is being parsed there is a large amount of dynamic state in play.
//!     This state includes the collection of user defined macros, the available fonts, the current vertical list, and so on.
//!     The structure of this state can vary across distributions.
//!     In Knuth’s original TeX, the state includes 256 numeric registers, whereas in pdfTeX there are more over 30 thousand.
//!     In XeTeX, the internal state can reference Unicode characters, whereas other distributions are limited to Ascii.
//!
//! - **File formats**: different distributions support different formats, both on the input side (font files) and output side (DVI vs PDF).
//!
//!
//! The first two are particularly relevant for TeX execution and expansion.
//! In order to support multiple distributions at once, Texcraft is designed to be flexible
//!   about the structure of the state it can work with.
//! It additionally supports plugging in arbitrary new primitives.
//!
//! ###  The state
//!
//! Throughout the Texcraft codebase, most of the state is encapsulated in an instance of type [state::Base].
//! This type has a generic parameter `S` for the state that is specific to each TeX engine.
//!
//! Any state that is needed for core TeX functionality is placed in the [state::Base],
//! for example catcode mappings (needed for lexing)
//! and the commands map (needed for expansion).
//! Additional state specific to a TeX engine is placed in a concrete state instance type,
//! which is substituted for the generic parameter `S`.
//! Parts of the Texcraft code base that need to make assumptions about the structure state do so by providing _Rust trait bounds_ on `S`.
//! The [time commands](command::library::time) are a good, simple example.
//! They require that the state in used contain time variables, and this is imposed via the [HasTime](command::library::time::HasTime)
//! trait bound.
//!
//! The [state module documentation](state) does a deep dive on all this.
//!
//! Note that some "state" specific to input
//!   (e.g., the list of open files, the conditional branches currently being expanded)
//!   is stored in the input unit instead.
//!
//! ### Commands
//!
//! Texcraft provides [an abstract API](command) for writing new commands (aka primitives).
//! There are many different types of commands, but the two most common are expansion commands and execution commands.
//! Expansion commands read input, potentially read the state, and then make adjustments to the input stream.
//! Execution commands read input and can make modifications to the state.
//! All commands operate on the generic state `S`, mentioned above.
//!
//! All the commands currently available are stored in the commands map, which is a part of the Base struct.
//!
//! ## Expansion
//!
//! Back to proccessing TeX.
//!
//! Expansion is implemented in the [driver module](driver),
//!   specifically in the `expand_next` private method.
//! The implementation consults the map of commands to see if the control sequence is referencing an expansion command,
//!    and if so evaluates it.
//! The result of this evaluation of a vector of tokens to be placed at the front of the input stream.
//! These tokens are then given to the input unit, which pushes them to the front.
//!
//! Each expansion command is provided with a immutable reference to the state mentioned before.
//! In all of the driver code, this state type is generic.
//!
//! ## Execution
//!
//! Execution is also implemented in the driver module,
//! in the main [run](driver::run) method.
//! This is simpler than expansion: the command itself is evaluated and nothing is returned.
//! Note that each execution command is provided with a *mutable* reference to the state.
//!
//! ## Output
//!
//! Output is not implemented yet.
//!
//! ## Important parts of Texcraft not mentioned above
//!
//! - The [variables system](variables) provides a uniform API for variables (parameters and registers)
//!
//! - The [primitives library](command::library) contains Texcraft implementations of TeX primitives.
//!
//! - The [parsing modules](parse) are used to read elements of the TeX grammer (like numbers) from streams of tokens.
//!

#[macro_use]
#[cfg(test)]
pub mod testutil;

pub mod command;
pub mod driver;
pub mod error;
pub mod macrotypes;
pub mod parse;
pub mod prelude;
pub mod state;
pub mod token;
pub mod variable;
