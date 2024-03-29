# Texcraft

Texcraft is a project to create an LLVM-style infrastructure for building TeX software.
It is designed as a loosely coupled collection of Rust libraries which compose
    together to create different kinds of TeX programs.

This work-in-progress website hosts the user-facing documentation.

The [Texcraft Playground](https://play.texcraft.dev) is example of what can be built with Texcraft.

## Design of the documentation

The Texcraft documentation is designed with the
    [Divio taxonomy of documentation](https://documentation.divio.com/) in mind.
In this taxonomy, there are four kinds of documentation: tutorials, how-to guides, references, and explainers.
Because Texcraft is a small project so far, we don't have significant documentation of each type.
Right now we have:

- The Texlang user guide, which is mostly a grounds-up _tutorial_ on how to use Texlang.
    The goal is for it to be possible to read the user guide from
        [the starting introduction](texlang/introduction.md) through to the end.
    But we also hope that you can jump into arbitrary sections that interest you,
        without having to slog through the prior chapters.

- [_Reference_ documentation](reference.md) that is autogenerated using `rustdoc`.

- Occasionally some _explainers_ that are high-level and theoretical.
    Some parts of the Texlang user guide (such as the introduction) have this style.

There are currently no how-to guides - we think the tutorials are enough for the moment.


## What's in a name?

Texcraft is a contraction of TeX and craft.
The verb craft was chosen, in part, to pay homage to Robert Nystrom's book [_Crafting Interpreters_](https://craftinginterpreters.com/).
This book is an inspiration both because of its wonderful exposition of the process of developing a language interpreter and
    because the book itself is so beautifully typeset.
(Unfortunately the methods of the book do not directly apply to TeX because TeX is not context free,
    has dynamic lexing rules, and many other problems besides.)
We hope Texcraft will eventually enable people to craft their own TeX distributions.

The name Texcraft is written using the letter casing rule for proper nouns shared by most langauges that use a Roman alphabet:
    the first letter is in uppercase, and the remaining letters are in lowercase.
To quote [Robert Bringhurst](https://en.wikipedia.org/wiki/The_Elements_of_Typographic_Style), 
    "an increasing number of persons and institutions, from archy and mehitabel,
    to PostScript and TrueType, come to the typographer in search of special treatment
    \[...\] Logotypes and logograms push typography in the direction of heiroglyphics, which
    tend to be looked at rather than read."
