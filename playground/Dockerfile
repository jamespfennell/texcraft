# Stage 1: build the Texcraft WebAssembly binary
FROM rust:1.67 as rust-wasm-builder
# This installs the wasm-pack tool
RUN  curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
WORKDIR /texcraft
COPY Cargo.toml Cargo.toml
COPY Cargo.lock Cargo.lock
COPY performance performance
COPY playground playground
COPY crates crates
RUN wasm-pack build --release playground

# Stage 2: build the React UI
FROM node:16.8 AS js-builder
COPY --from=rust-wasm-builder /texcraft/playground/pkg /texcraft/playground/pkg
WORKDIR /texcraft/playground/ui
COPY playground/ui .
RUN npm install && npm run build 

# Stage 3: build the backend server
FROM golang:1.17 as go-builder
COPY --from=js-builder /texcraft/playground/ui/dist /texcraft/playground/ui/dist
WORKDIR /texcraft/playground
COPY playground/go.mod .
COPY playground/go.sum .
COPY playground/backend.go .
COPY playground/backend_test.go .
RUN go test .
RUN go build -o playground backend.go

# Stage 4: put the backend binary in a regular Debian image
FROM debian:latest
COPY --from=go-builder texcraft/playground/playground /usr/bin/
ENTRYPOINT ["playground"]
