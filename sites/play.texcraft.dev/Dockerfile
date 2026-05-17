# Stage 1: build the Texcraft WebAssembly binary
FROM rust:1.94 as rust-wasm-builder
# This installs the wasm-pack tool
RUN  curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
WORKDIR /texcraft
COPY Cargo.toml Cargo.toml
COPY Cargo.lock Cargo.lock
COPY performance performance
COPY sites sites
COPY crates crates
RUN wasm-pack build --release sites/play.texcraft.dev

# Stage 2: build the React UI
FROM node:16.8 AS js-builder
COPY --from=rust-wasm-builder /texcraft/sites/play.texcraft.dev/pkg /texcraft/sites/play.texcraft.dev/pkg
WORKDIR /texcraft/sites/play.texcraft.dev/ui
COPY sites/play.texcraft.dev/ui .
RUN npm install && npm run build

# Stage 3: build the backend server
FROM golang:1.17 as go-builder
COPY --from=js-builder /texcraft/sites/play.texcraft.dev/ui/dist /texcraft/sites/play.texcraft.dev/ui/dist
WORKDIR /texcraft/sites/play.texcraft.dev
COPY sites/play.texcraft.dev/go.mod .
COPY sites/play.texcraft.dev/go.sum .
COPY sites/play.texcraft.dev/backend.go .
COPY sites/play.texcraft.dev/backend_test.go .
RUN go test .
RUN go build -o playground backend.go

# Stage 4: put the backend binary in a regular Debian image
FROM debian:latest
COPY --from=go-builder texcraft/sites/play.texcraft.dev/playground /usr/bin/
ENTRYPOINT ["playground"]
