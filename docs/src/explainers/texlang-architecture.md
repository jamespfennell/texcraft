---
title: Architecture of Texlang
weight: 20
---

This page mostly discusses code in Texlang's [runtime module](#todo).

## Historical context

Knuth's original implementation of TeX stores its state in many global mutable variables.
Other TeX distributions are forked from Knuth’s source code and thus do the same.

This is not great for two reasons.
First, global mutable state is just bad in general.
Among the general criticisms, we mention specifically that global mutable state
    makes software very hard to reason about
    because it blocs real separation of concerns.
Any two parts of the system can potentially interact implicitly through the shared state.

Second, each TeX distribution makes assumptions about the specific structure of the global state
  and the distribution's code has to follow these assumptions.
For example, Knuth's original TeX has 256 integer registers which correspond to a
    globally mutable 256-element array.
On the other hand, pdfTex has over 15 thousand registers and its associated
  globally mutable array is that much larger.
It is difficult or impossible for these different TeX implementations to share code because
  of these different global assumptions.

## Texlang's environment type

Texlang's solution to this problem is based on the runtime environment type.
Roughly speaking, all of the shared mutable state in TeX is lifted from the global
  namespace into the environment type.
This environment type is then passed around interally so different parts of the system
  and read and write to it.

This by itself does not solve the global mutable state problem.
The environment type is a sort of "God object" that is accesible everywhere and so is
  still basically global.
However, putting the state in type does have the advantages that:

1.  Different state structures can coexist in the same binary.
    To take the registers example above, you could imagine the environment type
    having a const generic parameter that specifies the number of registrs to
    allocate.
    States with different numbers of registers could easily be handled together.

2.  The same binary can process multiple TeX documents concurrently.
    A use case here is an HTTP server that accepts TeX source code and returns
    the compiled document.
    Each HTTP request spawns its own thread which initalizes its own environment
    object.
    Global state precludes this from happening.


As mentioned before, the environment type does not solve the global mutable state problem.
To solve this, Texlang has a number of different strategies.
All of these strategies use Rust's name visibilty rules to
  restrict the scope of code that can modify certain parts of the environment.

In order to describe these strategies,
  we need to first describe how Texlang's environment type is subdivided into 4 components.
These 4 components correspond to the 4 fields of the Rust type.

1.  **Base state**.
    In Texlang, "state" has a specific meaning: it is the parts of the environment that can be modified
      by non-expansion TeX commands like `\def`.
    For example, the commands map is a map from control sequence names to commands, and is part of the state.
    TeX's `\def` command alters this map by associating a control sequence with a user defined macro.

    The base state contains all of the state that is required to Texlang to process TeX.
    The commands map is an element of the base state, because Texlang has to be able
      to retrieve commands from somewhere in order to process a TeX file.
    The category codes map is also in the base state because it is needed for lexing.

1.  **Custom state**.
    Any other state needed by a specific TeX engine is placed in the custom state.
    In Rust terms, the custom state appears as a generic parameter `S` basically everywhere.
    Each engine defines its own `S`; there are no restrictions on it whatsover.

1.  **Expansion controller**.
    State is concerned with non-expansion commands (generally called execution commands).
    Some expansion commands are also stateful.
    The principle examples are TeX's conditional commands (`\ifnum`, `\else`, `\fi`, etc.)
      which need to keep track of which conditional branches the input is currently in.
    This kind of information is kept in the expansion controller, which
      can be freely edited by expansion commands.
    As described later, expansion commands are statically prevented from editing the regular state.

1.  **Internal environment**.
    Some parts of the environment are only used by the runtime.
    For example, the internal environment contains a string interner used to intern control
      sequence names for better performance.

## Scope limiting strategies

Texlang's scope limiting strategies ensure the environment is only mutated in
  controlled ways.

### The internal environment

The internal environment type has only private fields.
By Rust's visibility rules, these can only be mutated from within the runtime module itself.

The internal environment can, of course, be changed indirectly.
For example, the environment type has a push_source method which is used to add a new
  input source to the environment.
This is ultimitaley stored in the internal environment.

### Runtime input streams

The environment type is passed around everywhere in Texlang.
Done naively, would allow arbitrary access and mutation to its contents.
However the environment is never passed around directy:
  instead it is wrapped in one of Texlang's input streams.
These streams limit which parts of environment can be mutably accessed
  depending on the context in which the streams appear.
They are also used to read the next token of input - hence the name stream.

The streams are:

-   **Execution input**.
    This is the most permissible stream.
    It is used as the input the execution commands.
    This stream permits mutable access to the base state and the custom state
      (this is how `\def` is able to mutate the commands map).
    It does not permit mutations to the expansion controller.
    Using the [expanded method](#todo), it can be converted into an expanded input stream.

    Tokens read from the execution input have been expanded.

-   **Expanded input**.
    This stream is used as input to expansion commands.
    Expansion commands are not allowed to edit the state, and so this stream can
    only return an immutable reference to the custom and base states.
    So even though under the hood it holds a mutable reference to the
      environment, having a mutable reference to this stream does not permit
      changes to the state.
    This stream does permit mutations to be made to the expansion controller.

    The expanded input stream is also responsible for performing expansion,
    and tokens read from it have been expanded.

-   **Unexpanded input**.
    In terms of permissions, this stream is the same as to expanded input.
    The difference is that tokens read from the stream are not expanded.

    This stream is responsible for lexing new tokens.


### Trait bounds on the custom state

Each of a TeX engine only needs access to a small part of the state.
As an example let's consider the register commands which are implemented in
  Texlang's standard library's [registers module](#todo).
(The `\count` command is used to get and set the Nth register.)
Commands in this module only need to access the registers themselves,
  and need nothing else from the environment.
The registers are stored as an array in a [registers component](#todo).

The registers component could be included as a public field of the custom state object.
The `\count` command could then modify the registers through the custom state.
This has two problems:

1.  Any other command could edit the registers too!
    With this approach the registers array would be global mutable state.

1.  Our `\count` command would only work with a specific hardcoded state type.
    It couldn't be reused for different TeX environments without duplicating the implementation.

To fix these problems, we first introduce a Rust
trait that will be applied to any custom state containing the component.
The trait is simple:
```
trait HasRegisters {
    // Get a reference to the registers component.
    fn registers(&self) -> &RegistersComponent;

    // Get a mutable reference to the registers component.
    fn registers_mut(&mut self) -> &mut RegistersComponent;
}
```
We implement the trait for our custom state object:
```
struct MyState {
    registers: RegistersComponent,
}

impl HasRegisters for MyState {
    fn registers(&self) -> &RegistersComponent {
        &self.registers
    }
    // ...
}
```
To access the registers object, the `\count` command then uses the trait methods.
Automatically, this implemention of `\count` will work with any state object that
   has a registers component in it.

Next, we make the fields of the registers component private to the Rust module it is in.
Only code inside that module, like the `\count` implementation, can actually access
  the internals of the component.
Other code can impose the `HasRegisters` trait, but even with a reference to the component
  there is nothing it can do because of Rust's visibility rules.

## Executing TeX code

In order to actually run TeX code, the runtime module's [run](#todo) function is invoked.
Before doing this, an environment needs to be instantiated and
  TeX source code loaded using the environment's [push_source](#todo) method.
The environment is then wrapped in an execution input type, and provided to the run function.

