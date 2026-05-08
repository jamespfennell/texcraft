# Index of Texcraft Rust crates

## Common to the whole project

- [`common`](/reference/common/) — core types and abstractions used across Texcraft
- [`texcraft-stdext`](/reference/texcraft_stdext) — general Rust data structures and algorithms used across Texcraft

## Texlang

- [`texlang`](/reference/texlang/) — the core Texlang VM, tokenizer, and macro expansion engine
- [`texlang-common`](/reference/texlang_common) — shared traits (`FileSystem`, `Logging`, `TerminalIn`) for Texlang components
- [`texlang-font`](/reference/texlang_font) — font integration for Texlang
- [`texlang-stdlib`](/reference/texlang_stdlib) — the Texlang standard library of TeX primitives
- [`texlang-testing`](/reference/texlang_testing) — unit testing framework for Texlang-based code
- [`texlang-texttransform`](/reference/texlang_texttransform) — TeX primitives for text transformation (hyphenation, upper and lower casing)

## Boxworks

- [`boxworks`](/reference/boxworks/) — the Knuth/TeX typesetting engine, including a domain-specific language for specifying typesetting operations
- [`boxworks-hyphenate`](/reference/boxworks_hyphenate/) — hyphenation integration for Boxworks
- [`boxworks-knuthplass`](/reference/boxworks_knuthplass/) — Knuth-Plass line breaking algorithm
- [`boxworks-tex`](/reference/boxworks_tex/) — tools for converting TeX's internal data structures to Boxworks
- [`boxworks-testing`](/reference/boxworks_testing/) — testing utilities for Boxworks-based code
- [`boxworks-texlang`](/reference/boxworks_texlang/) — Texlang integration for Boxworks
- [`boxworks-text`](/reference/boxworks_text/) — text preprocessing (ligatures, kerning, space factor)

## Other

- [`dvi`](/reference/dvi/) — DVI document format support
- [`hyphenate`](/reference/hyphenate/) — implementation of TeX's hyphenation algorithm (Knuth-Liang)
- [`tfm`](/reference/tfm/) — parsers for the TeX font metric (`.tfm`) and property list (`.pl`) file formats

## Binaries

- [`boxworks-bin`](/reference/boxworks_bin/) — the `box` CLI
- [`dvi-bin`](/reference/dvi_bin/) — the `dvitools` CLI
- [`hyphenate-bin`](/reference/hyphenate_bin/) — the `hyphenate` CLI
- [`texcraft`](/reference/texcraft/) — the main Texcraft CLI (`doc`, `repl`, `run` subcommands)
- [`tfm-bin`](/reference/tfm_bin/) — the `tfmtools`, `tftopl`, and `pltotf` CLIs
