# Texcraft

Texcraft is a research project to develop a fully modular, LLVM-style framework for
  building TeX typesetting software.
We say "research project" because the work is somewhat speculative,
  though over time the goal seems more and more attainable.

Existing TeX engines (Knuth's original TeX '82, pdfTeX, XeTeX, etc.)
  all have monolithic software architectures that are hard to change
  and essentially impossible to use outside the context of compiling full documents.
They are not [malleable](https://www.inkandswitch.com/essay/malleable-software/).
The goal of Texcraft is change this by essentially doing for TeX engines what
    [LLVM](https://en.wikipedia.org/wiki/LLVM) did for compilers.
In Texcraft, a TeX engine is implemented as a loose collection of libraries that compose together
    using well-defined APIs.
The same code can be shared between engines - for example, 
[Texcraft's implementation of the `\count`
    primitive](https://github.com/jamespfennell/texcraft/blob/main/crates/texlang-stdlib/src/registers.rs)
    can be used both for TeX '82 (which has 256 memory registers) and pdfTeX (which has over 32,000 registers).

The project is divided into two main sub-projects:

- **Boxworks** is a language-agnostic implementation of the typesetting engine inside TeX.
    It is designed to be fully independent of the TeX language.
    One of the main goals of Boxworks is to support creating new non-TeX typesetting languages
    that use the engine for typesetting.

- **Texlang** is a framework for building [fast](https://github.com/jamespfennell/texcraft/tree/main/performance)
    TeX language interpreters.
    It is primarily designed for building TeX language frontends for Boxworks,
    but it can be used in other contexts too like the [Texcraft playground](https://play.texcraft.dev).
    Texlang's standard library contains implementations of many TeX primitives like `\count`, `\def` and `\expandafter`.

There are other smaller parts of the project, for example
    a Rust crates for
    [TeX font metric data](https://texcraft.dev/reference/tfm/),
    [DVI output files](https://texcraft.dev/reference/dvi/)
    and [hyphenation](https://texcraft.dev/reference/hyphenate/).

The [project manifesto](https://texcraft.dev/manifesto.html)
  describes goals of the project in greater depth.

## Trying it out

The [Texcraft playground](https://play.texcraft.dev) is built with Texcraft
    and can be used to run TeX scripts in the browser.

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
The [documentation website](https://texcraft.dev) has more information.

## Getting involved

The contribution bar for Texcraft is unfortunately high right now.
Texcraft is being built to produce identical output to Knuth's TeX, so working
  on the project largely involves studying [Knuth's TeX source code](https://tug.ctan.org/info/knuth-pdf/tex/tex.pdf)
  and strategizing ways to write diff tests.

## Related projects

### TeX

- [XymosTeX](https://github.com/xymostech/XymosTeX): ongoing project to implement TeX82 in Rust that has made
    significant progress in many areas, including input and output formats.

- [New Typesetting System](https://github.com/jamespfennell/new-typesetting-system):
    fully functional implementation of TeX82, written in Java and completed in 2001.
    This project turned out be to non-viable for some reasons like performance, but the source code is an excellent
    reference for those who are writing their own TeX implementations.
    
- [KeenType](https://gitlab.com/DaveJarvis/KeenType):
    a modernized version of the New Typesetting System
    than can be used for typesetting math formulae.

### Non-TeX Typesetting

There are number of active projects that are building typesetters
    based on non-TeX languages.
The TeX language itself is very complex and certain features like
    incremental compilation are hard or impossible to implement.

- [Typst](https://github.com/typst/typst)

- [Sile](https://github.com/sile-typesetter/sile)

## License

Licensed under either of Apache License, Version 2.0 or MIT license at your option.

There is more information about our licensing [on the documentation site](https://texcraft.dev/license.html).
