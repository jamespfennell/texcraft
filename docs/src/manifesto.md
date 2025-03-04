# Manifesto; or, why Texcraft?

Texcraft's ultimate goal is to advance the current
    state of open-source typesetting.
There are other large projects that share the same goal.
[Typst](https://typst.app/) is a brand new typesetting system designed to be more
    user friendly than TeX.
Within the TeX world, there is still significant work being done on LaTeX.

Texcraft is taking a different approach.
The project was started after a connection was made
    between the technical problems with the existing implementations of TeX,
    and some of the ideas behind the enormously successful LLVM project.

Donald Knuth's original Pascal/WEB implementation of TeX
    is simultaneously a critical part of the world's research and typesetting infrastructure
    but also basically impossible to iteratively improve upon because of its legacy software design.
The original TeX is 25 thousand lines of extremely monolithic code.
It makes extensive use of global state,
    has a custom memory management system,
    very few abstractions,
    and no test coverage.
It is very difficult to change.
It is impossible to reuse subparts of the code,
    for example the box-and-glue typesetting engine it contains.
(The same observations apply to other engines like pdfTeX, which are forks of TeX with more functionality and code added.)

Texcraft was started with the observation: we've been here before.
In the late 1990s the GCC project
    simultaneously dominated open-source C/C++ compiler space
    but also had a software architecture that made it difficult to evolve.
When Chris Lattner started the LLVM project, one of his ideas
    was to implement a compiler as a loosely coupled collection of libraries:

> "The world needs better compiler tools, tools which are built as libraries. This
    design point allows reuse of the tools in new and novel ways. However, building
    the tools as libraries isn't enough: they must have clean APIs, be as
    decoupled from each other as possible, and be easy to modify/extend.  This
    requires clean layering, decent design, and avoiding tying the libraries to a
>    specific use.  Oh yeah, did I mention that we want the resultant libraries to
>    be as fast as possible?"
>
>   [Chris Lattner in an old version of the Clang README](https://github.com/llvm/llvm-project/tree/llvmorg-2.6.0/clang)

Texcraft is based on the belief that
    the world needs better typesetting tools, tools which are built as libraries.
And in a world dominated by TeX, these tools should probably be compatible with TeX.

## In concrete terms

Texcraft's concrete goal is to reimplement the existing TeX engines (TeX82 and pdfTeX at least)
with a modular library-based software architecture.
As part of this, there is an opportunity to improve some of the user-experience around TeX,
    like returning better error messages or being smarter about
    when a recompilation is needed.
However we think the most promise of the project is how such a base could be built upon.
A modular code base would make it possible to:

1. Make small improvement to existing TeX engines,
    or even non-trivial improvements like adding new pagination algorithms.

1. Develop new languages that perform typesetting by using the existing
    box-and-glue engine of TeX as reimplemented in Texcraft.
    No need to write an engine from scratch
    (just as Rust's LLVM-based compiler didn't need to implement code generation).

1. Some hybrid of these: a TeX engine that can, say, `\inputTypst{chapter1.typst}`
    and allow authors to use Typst source files in their projects.
    This is potentially exciting as it gives a roadmap for migrating off of the TeX language,
    which has some fundamental usability issues.

## Finding the right abstractions

One of the main challenges in Texcraft is determining what parts of the TeX system
    can be decoupled; i.e., where the "clean APIs" can be introduced.

It's important to recognize straight away that the decoupled multi-pass architecture
    that is common in programming language compilers cannot work for TeX.
In TeX, all stages of the compilation process from lexing through to page building are tied together.
In TeX, it's possible to change the lexing rules depending on how many pages have been typeset so far:

```tex
% If the page number is odd...
\ifodd \pageno
    % ...change the meaning of the letter T to be open brace and X to be close brace
    \catcode`\T 1
    \catcode`\X 2
\fi
% The lexing rules for the next line depend on which page we're on, and are thus
% a function of the entire document so far.
% If this is an odd page, TeX will tokenize this as {e} and typeset e;
% otherwise, it will typeset TeX.
TeX
```

In this example, we can't run the lexer on the line `TeX` until we've fully processed and typeset
every single thing that has come before it.
This means that we can't run the lexer, or TeX macro expander, or line-breaker, or page-builder
    in isolation: they must all run concurrently.


However, after a few years of working on Texcraft we think the TeX source code is extremely amendable to 
    modularization, once all the modules can be be made to run together.
At the highest possible level, TeX can be divided into two parts
    following a traditional frontend/backend split:

- Backend: Knuth's box-and-glue typesetting engine.
    In the Pascal/WEB implementation, this engine mostly (completely?) works on internal data
    structures that are agnostic to the TeX language.
    It seems possible to reimplement this without any dependency on TeX.
    In Texcraft the backend is being implemented as the **Boxworks** sub-project.

- Frontend: a TeX language interpreter that reads TeX source code and then pushes
    the correct buttons on the backend.
    In Texcraft this is the **Texlang** sub-project.


Within these two halves there is also lots of opportunity for modularization.
In Texlang, the implementation of conditional logic is [a single Rust source
    file](https://github.com/jamespfennell/texcraft/blob/main/crates/texlang-stdlib/src/conditional.rs)
    that's completely decoupled from the rest of the project 
    (except of course for using some "clean" Texlang APIs).
Work on Boxworks has just started,
    but for example it seems that the Knuth-Plass line-breaking algorithm can be put behind
    a generic "line-breaking" API.


## Correctness and speed

Texcraft's interesting software architecture is not enough.
In order to be viable, Texcraft needs to generate the same results
    as TeX (be correct) and do it in about the same time (be fast).

For correctness, TeX clearly falls under [Hyrum's law](https://www.hyrumslaw.com/).
It doesn't matter what Knuth _says_ in the TeXBook:
after 45 years in production, every observable behavior of the TeX system
    is probably relied upon by someone.
The Texcraft's project goal is to exactly replicate the output of TeX.
This is fairly non-trivial because TeX is, ultimately, 
[a fragile language](https://jpfennell.com/posts/tex-expansion-edge-case/]).
To achieve this, Texcraft development generally works by closely examining the Pascal/WEB source code
    and sometimes translating it by hand.

There is a silver lining though.
Once you've committed to replicating a program exactly, you now have access
    to many test cases to verify your new implementation is correct.
In the very long run, we envisage running Texcraft's pdfTeX implementation
    on [papers in the Arxiv](https://arxiv.org/) and automatically verifying correctness.

As for speed, this is also challenging.
Knuth performance optimized the Pascal/WEB code so that it would run tolerably on early 1980s computers.
This means that on today's hardware TeX is really really fast.
Our initial work here has been promising, and our goal of being
    "about as fast as TeX" seems very achievable.
We do have some advantages over Knuth:
    access to a modern optimizing compiler toolchain
    and the option of better data structures in hot parts of the code like macro expansion
    and list building.




