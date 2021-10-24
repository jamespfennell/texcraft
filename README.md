# Texcraft

Texcraft is a project to create an LLVM-style infrastructure for building TeX engines and distributions.
Goals:

- Creating modern reimplementations of existing TeX distributions (TeX, pdfTeX, XeTeX, etc.)
    with a focus on modularity and code reuse.

- Enabling direct integration of TeX engines with IDEs, webservices, and JavaScript (using WASM).

- Providing a framework for building new TeX engines with novel features (new page
    breaking algorithms, support for different font formats, etc.).

Initial implemention work has focused on the core internal APIs (commands, variables, state),
    expansion primitives, and performance.


## Trying it out

Try it out in the [Texcraft playground](https://play.texcraft.dev)!


## Performance

One of the hardest parts of implementing a TeX engine is matching the
  high performance of Knuth's original implementation.
The Texcraft project maintains benchmarks for measuring performance
  and comparing them to pdfTeX.
These currently report that Texcraft is 5-10% faster,
  though we expect that Texcraft's numbers will regress as more features are added.

For context, when the first benchmark was introduced
  Texcraft was over seven times slower than pdfTeX!


## See also

- [XymosTeX](https://github.com/xymostech/XymosTeX): ongoing project to implement TeX82 in Rust that has made
    significant progress in many areas, including input and output formats.

- [New Typesetting System](https://github.com/jamespfennell/new-typesetting-system):
    fully functional implementation of TeX82, written in Java and completed in 2001.
    This project turned out be to non-viable for some reasons like performance, but the source code is an excellent
    reference for those who are writing their own TeX implementations.

