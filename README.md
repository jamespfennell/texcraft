# Texcraft

Texcraft is a project to create a composable, LLVM-style infrastructure for building TeX software.

Existing TeX engines (Knuth's original TeX '82, pdfTeX, XeTeX, etc.)
    all have monolithic software architectures and do not share code amongst themselves.
The goal of Texcraft is change this by doing for TeX engines what
    [LLVM](https://en.wikipedia.org/wiki/LLVM) did for compilers.
In Texcraft, a TeX engine is implemented as a loose collection of libraries that compose together
    using well-defined APIs.
The same code can be shared between engines - for example, 
[Texcraft's implementation of the `\count`
    primitive](https://github.com/jamespfennell/texcraft/blob/main/crates/texlang-stdlib/src/registers.rs)
    can be used both for TeX '82 (which has 256 memory registers) and pdfTeX (which has over 32,000 registers).

Initial work has focused on building a [fast](https://github.com/jamespfennell/texcraft/tree/main/performance)
  TeX language interpreter called Texlang,
  and a standard library of primitives such as `\def` that work with this interpreter.
No typesetting work has been done yet.

## Trying it out

The [Texcraft playground](https://play.texcraft.dev) is an example of TeX software
    built with Texcraft.
It can be used to run TeX scripts in the browser.

Locally, with the Git repo checked out,
    the Texcraft binary can also be used to run TeX scripts; e.g.,

```
cargo run --bin texcraft run performance/benches/digits_of_pi.tex
```

The Texcraft binary also has a REPL for writing TeX interactively:
```
cargo run --bin texcraft repl
```

These all work with the limited subset of TeX commands that have been implemented.
Run `cargo run --bin texcraft doc` for a list of available commands.

Note the main point of Texcraft is not to produce specific binaries, but rather to be 
    a library for building TeX software.
The [documentation website](https://texcraft.dev) will eventually beginner-friendly tutorials on 
    working with Texcraft as a library.

## Getting involved

There is a lot of low hanging fruit that is intentionally left unpicked so
    people who want to contribute to Texcraft have a good starting point.
Right now the main focus is on completing the Texlang standard library, which is a collection
    of non-typesetting TeX commands like `\def` that every TeX distribution includes.

## Related projects

### TeX

- [XymosTeX](https://github.com/xymostech/XymosTeX): ongoing project to implement TeX82 in Rust that has made
    significant progress in many areas, including input and output formats.

- [New Typesetting System](https://github.com/jamespfennell/new-typesetting-system):
    fully functional implementation of TeX82, written in Java and completed in 2001.
    This project turned out be to non-viable for some reasons like performance, but the source code is an excellent
    reference for those who are writing their own TeX implementations.
    
- [KeenType](https://github.com/DaveJarvis/KeenType):
    a modernized version of the New Typesetting System
    than can be used for typesetting math formulae.

### Non-TeX Typesetting

There are number of active projects that are building typesetters
    based on non-TeX languages.
The TeX language itself is very complex and certain features like
    incremental compilation are hard or impossible to implement.

- [Typst](https://github.com/typst/typst)

- [Sile](https://github.com/sile-typesetter/sile)
