# Texcraft code by TeX section

This index maps the sections/chapters in Knuth's original TeX to the
corresponding Texcraft code.
This is not meant to be exhaustive in either direction, but rather a general guide.

Empty cells generally indicate that work on that section hasn't started.

Texcraft code is frequently annotated with the analogous TeX section
(e.g. TeX.2021.855) and `git grep` can be used to find these annotations.

| # | Name | Texcraft code |
|-|-|-|
1 | Introduction | N/A. This section defines global variables specific to the TeX Pascal code.
2 | The character set | N/A. Texcraft uses Rust's Unicode character facilities.
3 | Input and output | N/A. Texcraft uses Rust's stdlib for reading and writing files.
4 | String handling | Largely N/A because Texcraft uses Rust's standard string facilities but some of this code is related to the string interner, which in Texcraft is in `collections/interner.rs` in `texcraft-stdext`.
5 | On-line and off-line printing | N/A. Texcraft uses Rust's string formatting and printing facilities.
6 | Reporting errors | The `error` module in `texlang` and `errormode.rs` in `texlang-stdlib`. See also the [dedicated documentation on Texlang's error system](texlang/09-errors.md).
7 | Arithmetic with scaled dimensions | `common`
8 | Packed data | N/A. Texcraft doesn't have any memory usage optimizations of this type.
9 | Dynamic memory allocation | N/A. Memory allocation is done via Rust stdlib.
10 | Data structures for boxes and their friends | `ds.rs` in `boxworks`
11 | Memory layout | N/A. Memory allocation is done via Rust stdlib.
12 | Displaying boxes | The `boxworks-lang` crate contains a DSL for box data structures. The format is different than in TeX. TeX's format can be parsed using code in `tex.rs` in `boxworks`.
13 | Destroying boxes | N/A. Memory including boxes is managed by Rust.
14 | Copying boxes | N/A. Copying is done via implementations of Rust's `Clone` trait.
15 | The command codes | N/A. Texlang doesn't have global command codes. [Texlang's command tags feature](texlang/05-primitive-tags.md) is used for cases when the command needs to be identified.
16 | The semantic nest 
17 | The table of equivalents | N/A. Texlang's doesn't have a global table of state. Instead, the [component pattern](texlang/04-stateful-primitives.md) is used for per-command state.
18 | The hash table | Mostly N/A because Texcraft uses Rust's stdlib data structures. But `command/map.rs` in `texlang` is the equivalent thing. The hash table is related to string interning, which in Texcraft is in `collections/interner.rs` in `texcraft-stdext`.
19 | Saving and restoring equivalents  | Implemented via `collections/groupingmap.rs` in `texcraft-stdext`.
20 | Token lists 
21 | Introduction to the syntactic routines | N/A. This section is essentially just documentation.
22 | Input stacks and states | The stacks are in `Internal` in `vm/mod.rs` in `texlang`. Error context is provided using Texlang's token tracing system, `token/trace.rs` in `texlang`.
23 | Maintaining the input stacks | The stacks are modified throughout the `vm` module in `texlang`.
24 | Getting the next token | `token/lexer.rs` and `vm/streams.rs` (especially the function `next_unexpanded`) in `texlang`.
25 | Expanding the next token | `vm/streams.rs` in `texlang`, especially the `next_expanded` function.
26 | Basic scanning subroutines | The `parse` module in `texlang`. In particular the `Parsable` trait determines what can be "scanned" and implementations of this trait generally map to subroutines in this section.
27 | Building token lists 
28 | Conditional processing | `conditional.rs` in `texlang-stdlib`.
29 | File names |
30 | Font metric data | `tfm`.
31 | Device-independent file format | `dvi`.
32 | Shipping pages out | `boxworks-dvi` (planned).
33 | Packaging 
34 | Data structures for math mode 
35 | Subroutines for math mode 
36 | Typesetting math formulas 
37 | Alignment 
38 | Breaking paragraphs into lines | `boxworks-knuthplass`.
39 | Breaking paragraphs into lines, continued | `boxworks-knuthplass`.
40 | Pre-hyphenation | `boxworks-hyphenation` (planned).
41 | Post-hyphenation | `boxworks-hyphenation` (planned). 
42 | Hyphenation | `boxworks-hyphenation` (planned).
43 | Initializing the hyphenation tables | `boxworks-hyphenation` (planned).
44 | Breaking vertical lists into pages 
45 | The page builder 
46 | The chief executive 
47 | Building boxes and lists 
48 | Building math lists 
49 | Mode-independent processing | Various modules in `texlang-stdlib`. E.g. `\advance` is implemented in `math.rs`.
50 | Dumping and undumping the tables | Implemented by Texlang's serializable VMs feature which has [dedicated documentation](texlang/11-serde.md). The Texlang framework code for serding is in `vm/serde.rs` in `texlang`, but serialization code is spread throughout the project (e.g. `#[derive(Serialize)]` on any component type).
51 | The main program 
52 | Debugging 
53 | Extensions