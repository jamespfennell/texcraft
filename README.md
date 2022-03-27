# Texcraft

Texcraft is a project to create a composible, LLVM-style infrastructure for building TeX engines and distributions.

The big picture goals are:

- Creating modern reimplementations of existing TeX distributions (TeX, pdfTeX, XeTeX, etc.)
    with a focus on modularity and code reuse.
    For example, 
    [the single Texcraft implementation of TeX registers](https://texcraft.dev/rustdoc/texlang_stdlib/registers/index.html) 
    can be used to create both the 256 registers of TeX82 and the 32768 registers of pdfTeX.

- Directly integrating TeX engines with IDEs, webservices, and JavaScript/WASM 
    (see the [Texcraft playground](https://play.texcraft.dev) for an example of the latter).

- Providing a framework for building new TeX engines with novel features like new page
    breaking algorithms, support for different font formats, etc.

Initial work has focused on building a [fast](https://github.com/jamespfennell/texcraft/tree/main/performance)
  TeX language interpreter called Texlang,
  and [writing documentation](https://texcraft.dev) on how to use it.

## Trying it out

No typesetting work has been done yet, but Texcraft can already be used to run TeX scripts.

The [Texcraft playground](https://play.texcraft.dev) does this in the browser - no downloads needed.

Locally, with the Git repo checked out,
    the Texcraft binary can be used to run TeX scripts; e.g.,

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
The [documentation website](https://texcraft.dev) has beginner-friendly tutorials on 
    working with Texcraft as a library.

## Getting involved

There is a lot of low hanging fruit that is intentionally left unpicked so
    people who want to contribute to Texcraft have a good starting point.
Right now the main focus is on completing the Texlang standard library, which is a collection
    of non-typesetting TeX commands like `\def` that every TeX distribution includes.

## See also

- [XymosTeX](https://github.com/xymostech/XymosTeX): ongoing project to implement TeX82 in Rust that has made
    significant progress in many areas, including input and output formats.

- [New Typesetting System](https://github.com/jamespfennell/new-typesetting-system):
    fully functional implementation of TeX82, written in Java and completed in 2001.
    This project turned out be to non-viable for some reasons like performance, but the source code is an excellent
    reference for those who are writing their own TeX implementations.

