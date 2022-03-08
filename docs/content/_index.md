# Texcraft

Texcraft is a project to create an LLVM-style infrastructure for building TeX software.
It is designed as a loosely coupled collection of Rust libraries which compose
    together to create different kinds of TeX programs.
The high level motivation of the project is described in the explainer
    [what Texcraft is trying to achieve](explainers/why-texcraft.md).


This website largely just hosts the user-facing documentation.
The [Github repository](https://github.com/jamespfennell/texcraft) has information on contributing to the project.
The [Texcraft Playground](https://play.texcraft.dev) is example of what can be built with Texcraft.

## Layout of this site

The Texcraft documentation is structured based on the
    [Divio taxonomy of documentation](https://documentation.divio.com/).
Based on this system we have four types of documentation:

1. **Explainers** are high-level, theoretical guides to Texcraft or one of its component parts.
    The explainer
    [what Texcraft is trying to achieve](explainers/why-texcraft.md)
    has the highest
        vantage point and is designed as a the starting point to those who are new to Texcraft.

1. **Tutorials** are hands-on lessons that guide readers through some aspect of building TeX software with Texcraft.
    Following a tutorial fully means writing Rust code, and so we assume some knowledge of Rust.
    The tutorial [building a TeX interpreter with Texlang]() is designed to be the entrypoint to 
        actually coding with Texcraft.

1. **How to guides** describe how to solve particular problems in the Texcraft framework,
    for example how to create a custom TeX primitive.

1. The **reference** documentation describes the various Rust APIs of Texcraft.
    This documentation is taken from the Rust code and built using `rustdoc`.

## What's in a name?

Texcraft is a contraction of TeX and craft.
The verb craft was chosen, in part, to pay homage to Robert Nystrom's book [_Crafting Interpreters_](https://craftinginterpreters.com/).
This book is an inspiration both because of its wonderful exposition of writing a language parser and
    because the book itself is so beautifully typeset.
(Unfortunately the methods of the book do not directly apply to TeX becaues TeX is not context free,
    has dynamic lexing rules, and other problems besides.)
We hope Texcraft will eventually enable people to craft their own TeX distributions.

The name Texcraft is written using the letter casing rule for proper nouns shared by most langauges that use a Roman alphabet:
    the first letter is in uppercase, and the remaining letters are in lowercase.
To quote [Robert Bringhurst](https://en.wikipedia.org/wiki/The_Elements_of_Typographic_Style), 
    "an increasing number of persons and institutions, from archy and mehitabel,
    to PostScript and TrueType, come to the typographer in search of special treatment
    \[...\] Logotypes and logograms push typography in the direction of heiroglyphics, which
    tend to be looked at rather than read."
