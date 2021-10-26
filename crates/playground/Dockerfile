# Stage 1: build the Texcraft WebAssembly binary
FROM rust:1.56 as rust-wasm-builder
# This installs the wasm-pack tool
RUN  curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
WORKDIR /texcraft
COPY Cargo.toml Cargo.toml
COPY Cargo.lock Cargo.lock
COPY crates crates
RUN wasm-pack build --release crates/playground

# Stage 2: build the React UI
FROM node:16.8 AS js-builder
COPY --from=rust-wasm-builder /texcraft/crates/playground/pkg /texcraft/crates/playground/pkg
WORKDIR /texcraft/crates/playground/ui
COPY crates/playground/ui .
RUN npm install && npm run build 

# Stage 3: build the backend server
FROM golang:1.17 as go-builder
COPY --from=js-builder /texcraft/crates/playground/ui/dist /texcraft/crates/playground/ui/dist
WORKDIR /texcraft/crates/playground
COPY crates/playground/go.mod .
COPY crates/playground/go.sum .
COPY crates/playground/backend.go .
COPY crates/playground/backend_test.go .
RUN go test .
RUN go build -o playground backend.go

# Stage 4: put the backend binary in a regular Debian image
FROM debian:latest
COPY --from=go-builder texcraft/crates/playground/playground /usr/bin/
ENTRYPOINT ["playground"]
