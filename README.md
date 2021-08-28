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
As of September 2021, Texcraft can parse and execute a Turing complete subset of TeX.


## Trying it out



## What's in a name?

Texcraft is a contraction of TeX and craft.
The verb craft in part pays homage to Robert Nystrom's book [_Crafting Interpreters_](https://craftinginterpreters.com/),
    which is an inspiration both because of its wonderful presentation of the process of writing language a parser and
    because the book itself is so beautifully typeset.
(Unfortunately the methods of the book do not directly apply to TeX becaues TeX is not context free
    and has dynamic lexing rules, among other problems.)
We hope Texcraft will eventually enable people to craft their own TeX distributions.

The name Texcraft is written using the letter casing rule for proper nouns shared by most langauges that use a Roman alphabet:
    the first letter is in uppercase, and the remaining letters are in lowercase.
To quote [Robert Bringhurst](https://en.wikipedia.org/wiki/The_Elements_of_Typographic_Style), 
    "an increasing number of persons and institutions, from archy and mehitabel,
    to PostScript and TrueType, come to the typographer in search of special treatment
    \[...\] Logotypes and logograms push typography in the direction of heiroglyphics, which
    tend to be looked at rather than read."

## See also

- [XymosTeX](https://github.com/xymostech/XymosTeX): ongoing project to implement TeX82 in Rust that has made
    significant progress in many areas, including input and output formats.

- [New Typesetting System](https://github.com/jamespfennell/new-typesetting-system):
    fully functional implementation of TeX82, written in Java and completed in 2001.
    This project turned out be to non-viable for some reasons like performance, but the source code is an excellent
    reference for those who are writing their own TeX implementations.

## License

Some open source license, but details TBD.


