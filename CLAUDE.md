# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Project Is

Texcraft is a modular framework for building TeX engines and typesetting software — analogous to what LLVM did for compilers. The primary goal is to produce libraries, not binaries.

## Build & Test Commands

```bash
# Build everything
cargo build

# Run all tests
cargo test

# Run tests for a specific crate
cargo test --package texlang
cargo test --package tfm

# Lint (CI enforces this with -D warnings)
cargo clippy --all-features -- -D warnings

# Run benchmarks
cargo bench

# Run a TeX script
cargo run --bin texcraft run performance/benches/digits_of_pi.tex

# Interactive REPL
cargo run --bin texcraft repl
```

## High-Level Architecture

The project is organized into two main sub-systems plus supporting libraries, all under `crates/`:

### Texlang (TeX Language Framework)
A composable VM-based framework for building TeX language interpreters. The key abstraction is that TeX primitives are modular components that can be mixed and matched to build custom interpreters.

- **`texlang/`** — Core VM, tokenizer, macro expansion, variable system
- **`texlang-stdlib/`** — ~30+ TeX primitives (count, def, expand, conditionals, I/O, etc.)
- **`texlang-common/`** — Shared abstractions: `FileSystem`, `Logging`, `TerminalIn` traits
- **`texlang-testing/`** — Unit testing framework for Texlang-based code
- **`texlang-font/`** — Font integration

### Boxworks (Typesetting Engine)
Intentionally **independent of Texlang** — the typesetting engine can be used with non-TeX languages.

- **`boxworks/`** — Core typesetting data structures and algorithms
- **`boxworks-text/`** — Text preprocessing: ligature/kerning, space factor
- **`boxworks-lang/`** — DSL for specifying typesetting operations

### Foundation
- **`common/`** — Fundamental types: `Scaled` (fixed-point arithmetic), `Glue`, `FontFormat` trait
- **`texcraft-stdext/`** — General Rust utilities used across crates
- **`tfm/`** — Parser for TeX Font Metric (`.tfm`) and Property List (`.pl`) formats
- **`dvi/`** — DVI document format support

### Binaries (in `crates/`)
- **`texcraft/`** — Main CLI: `doc`, `repl`, `run` subcommands
- **`tfm-bin/`** — `tfmtools`, `tftopl`, `pltotf`
- **`dvi-bin/`** — `dvitools`
- **`boxworks-bin/`** — `box` CLI

### Other Workspace Members
- **`performance/`** — Criterion benchmarks (digits_of_pi, lexer, primes)
- **`playground/`** — WebAssembly build for https://play.texcraft.dev (Go backend + WASM frontend)
- **`docs/`** — mdBook documentation website

## Key Feature Flags

- `serde` — serialization support (many crates)
- `repl` — interactive REPL (texlang-stdlib)
- `ariadne` — human-friendly parse error reporting (tfm, boxworks-lang)
- `arbitrary` — fuzzing support (tfm)
- `color` — colored terminal output (texcraft-stdext)

## Rust Configuration

- Edition: 2021
- Toolchain: 1.82.0 (pinned)
- Release profile uses LTO
- Fuzzing targets exist under `tfm/`
