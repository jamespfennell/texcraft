# Build from the repo root:
#   docker build -f sites/hyphenate.dev/Dockerfile -t hyphenate.dev .
#   docker run -p 8080:80 hyphenate.dev

FROM rust:1.82.0 AS builder

RUN cargo install wasm-pack && rustup target add wasm32-unknown-unknown

WORKDIR /build
COPY . .

RUN cd sites/hyphenate.dev/hyphenate-wasm && \
    wasm-pack build --target web --out-dir ../pkg

FROM caddy

COPY sites/hyphenate.dev/index.html /usr/share/caddy/
COPY --from=builder /build/sites/hyphenate.dev/pkg/ /usr/share/caddy/pkg/
