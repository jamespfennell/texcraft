# Texcraft

Texcraft is a project to create an LLVM-style infrastructure for building TeX engines and distributions.
The goals of the project include:

- Creating modern reimplementations of existing TeX distributions (TeX, pdfTeX, XeTeX, etc.)
    with a focus on modularity and code reuse.

- Enabling direct integration of TeX engines with things like IDEs and webservices. 

- Providing a framework for building new TeX engines with novel features (new page
    breaking algorithms, support for different font formats, etc.).

Initial implemention work has focused on the core internal APIs (commands, variables, state)
    and the expansion primitives.


## Trying it out

Try it out in the [Texcraft playground](https://play.texcraft.dev)!


## Contributing

## See also

- [XymosTeX](https://github.com/xymostech/XymosTeX): ongoing project to implement TeX82 in Rust that has made
    significant progress in many areas, including input and output formats.

- [New Typesetting System](https://github.com/jamespfennell/new-typesetting-system):
    fully functional implementation of TeX82, written in Java and completed in 2001.
    This project turned out be to non-viable for some reasons like performance, but the source code is an excellent
    reference for those who are writing their own TeX implementations.

## License

Some open source license, but details TBD.


