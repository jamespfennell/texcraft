# Build from the repo root:
#   docker build -f sites/ligkern.dev/Dockerfile -t ligkern.dev .
#   docker run -p 8080:80 ligkern.dev

FROM rust:1.95.0 AS builder

RUN cargo install wasm-pack && rustup target add wasm32-unknown-unknown

WORKDIR /build
COPY . .

RUN cd sites/ligkern.dev/ligkern-wasm && \
    wasm-pack build --target web --out-dir ../pkg

FROM caddy

COPY sites/ligkern.dev/Caddyfile /etc/caddy/Caddyfile
COPY sites/ligkern.dev/index.html /usr/share/caddy/
COPY --from=builder /build/sites/ligkern.dev/pkg/ /usr/share/caddy/pkg/
