# hyphenate.dev

A website that hyphenates English words using TeX's Knuth-Liang algorithm,
implemented in Rust and compiled to WebAssembly.

## Prerequisites

- Rust toolchain (see `rust-toolchain.toml` in the repo root)
- `wasm-pack`: `cargo install wasm-pack`
- The `wasm32-unknown-unknown` target: `rustup target add wasm32-unknown-unknown`

## Build

From this directory, compile the Rust crate to WebAssembly:

```
cd hyphenate-wasm
wasm-pack build --target web --out-dir ../pkg
```

## Run locally

WASM modules must be served over HTTP (browsers block `file://` imports).
From this directory:

```
python3 -m http.server 8765
```

Then open http://localhost:8765 in a browser.

## Docker

Build and run the site using Caddy, from the **repo root**:

```
docker build -f sites/hyphenate.dev/Dockerfile -t hyphenate.dev .
docker run -p 8080:80 hyphenate.dev
```

Then open http://localhost:8080 in a browser.
