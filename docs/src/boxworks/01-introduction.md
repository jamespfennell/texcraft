# Introduction to Boxworks

Boxworks is a work-in-progress re-implementation of the typesetting engine
    inside TeX.
It is language-agnostic in the sense that it doesn't rely on the TeX language.
We envisage a future in which you can take, say, Typst source
    code and typeset it using the Boxworks engine.

Like the wider Texcraft project, the plan is to implement Boxworks
    as a collection of libraries that plug together to create a typesetting engine.
Each individual library (e.g. a page builder) can be easily replaced if a
    different algorithm is desired.
So far we've identified the following parts of TeX that can probably
    be individual decoupled modules in the Boxworks system.
(This is not exhaustive and does not cover everything that will need to be implemented.)

### Text preprocessor

This module takes Unicode characters as input and generates
horizontal list elements.
This sounds trivial but in TeX it's not because this process includes:

1. Adding kerns.
1. Replacing multiple characters with ligatures.
1. Figuring out the right inter-word glue to add
        based on a variety of considerations like the current space factor and the values
        of variables like `\spaceskip`.

Most of the relevant TeX code is in "part 46: the chief executive".

### Line width calculator

Calculates line widths based on values of `\parshape` and `\hangindent`.
In TeX this logic seems to be duplicated between the line breaker and math processing.
In the line breaker, relevant sections are TeX.2021.847-850.

### Line breaker

Breaks up a horizontal list into multiple lines and puts these on
    the enclosing vertical list.
In TeX this is the famous Knuth-Plass line-breaking algorithm.
Implemented in parts 38 and 39 of TeX.

### Output driver

Takes a vertical list corresponding to a page and outputs it in DVI format.
Implemented in part 32: shipping pages out.
In the long run there would also be an output driver for PDF and other formats.

