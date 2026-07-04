# Build from the repo root:
#   docker build -f sites/knuthplass.dev/Dockerfile -t knuthplass.dev .
#   docker run -p 8080:80 knuthplass.dev

FROM rust:1.95.0 AS builder

RUN cargo install wasm-pack && rustup target add wasm32-unknown-unknown

WORKDIR /build
COPY . .

RUN cd sites/knuthplass.dev/knuthplass-wasm && \
    wasm-pack build --target web --out-dir ../pkg

FROM caddy

COPY sites/knuthplass.dev/Caddyfile /etc/caddy/Caddyfile
COPY sites/knuthplass.dev/index.html /usr/share/caddy/
COPY sites/knuthplass.dev/fonts/ /usr/share/caddy/fonts/
COPY --from=builder /build/sites/knuthplass.dev/pkg/ /usr/share/caddy/pkg/
