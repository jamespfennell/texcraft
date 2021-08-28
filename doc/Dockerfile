FROM rust:1.51 AS builder

WORKDIR /usr/src/texcraft-docs

COPY Cargo.lock .
COPY Cargo.toml .
RUN mkdir src
RUN echo "fn main() {}" > src/main.rs
RUN cargo fetch

COPY src src
RUN cargo doc --no-deps --lib

FROM nginx

WORKDIR /usr/share/nginx/html

RUN rm ./*
COPY doc/* ./
COPY --from=builder /usr/src/texcraft-docs/target/doc rustdoc
