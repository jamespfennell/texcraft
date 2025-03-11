FROM rust:1.82 AS builder

# Add the files into the container
WORKDIR /texcraft
COPY Cargo.lock .
COPY Cargo.toml .
COPY performance performance
COPY playground playground
COPY crates crates
RUN cargo fetch
# In order to run the MD book tests we need one and only one build of the Texlang code
# in the target/debug/deps. So we clean the workspace before building the lib just to be sure.
RUN cargo clean
RUN cargo build --lib --all-features

# Build the MD book
RUN curl -sSL https://github.com/rust-lang/mdBook/releases/download/v0.4.30/mdbook-v0.4.30-x86_64-unknown-linux-gnu.tar.gz | tar -xz --directory=/usr/bin
COPY docs .
RUN mdbook build
RUN mdbook test -L target/debug/deps

# Built the Rust reference
RUN RUSTDOCFLAGS="-D warnings" cargo doc --all-features --lib --no-deps \
  -p "boxworks*" \
  -p core \
  -p dvi \
  -p texcraft-stdext \
  -p "texlang*" \
  -p tfm

# Serve the static files using Caddy
FROM caddy
WORKDIR /texcraft
COPY --from=builder /texcraft/book /texcraft
COPY --from=builder /texcraft/target/doc /texcraft/reference
ENTRYPOINT caddy file-server --listen :8080 --root /texcraft
