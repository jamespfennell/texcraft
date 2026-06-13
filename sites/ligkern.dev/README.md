# ligkern.dev

A website that visualizes the lig/kern algorithm from TeX font metric (TFM) files,
implemented in Rust and compiled to WebAssembly.

## Prerequisites

- Rust toolchain (see `rust-toolchain.toml` in the repo root)
- `wasm-pack`: `cargo install wasm-pack`
- The `wasm32-unknown-unknown` target: `rustup target add wasm32-unknown-unknown`

## Build

From this directory, compile the Rust crate to WebAssembly:

```
cd ligkern-wasm
wasm-pack build --target web --out-dir ../pkg
```

## Run locally

WASM modules must be served over HTTP (browsers block `file://` imports).
Use Caddy for local development:

```
caddy run --config Caddyfile.local
```

Then open http://localhost:8766 in a browser. Stop the server with `caddy stop` or `Ctrl+C`.

To install Caddy: `brew install caddy` (macOS) or see https://caddyserver.com/docs/install.

## Test locally

With the Caddy server running on port 8766, install dependencies and run the smoke tests:

```
npm install
npm test
```

## Docker

Build and run the site using Caddy, from the **repo root**:

```
docker build -f sites/ligkern.dev/Dockerfile -t ligkern.dev .
docker run -p 8080:80 ligkern.dev
```

Then open http://localhost:8080 in a browser.
