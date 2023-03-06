FROM golang:1.16 AS hugo-builder

# Install Hugo
WORKDIR /usr/bin
RUN curl -L -o hugo.tar.gz https://github.com/gohugoio/hugo/releases/download/v0.87.0/hugo_extended_0.87.0_Linux-64bit.tar.gz
RUN tar -zxvf hugo.tar.gz

# Build the Hugo HTML files
WORKDIR /website
COPY . .
RUN cd docs && hugo

FROM rust:1.67 AS rust-builder

# Build the Rustdoc HTML files
WORKDIR /rustdoc
COPY Cargo.lock .
COPY Cargo.toml .
COPY bin bin
COPY performance performance
COPY playground playground
COPY crates crates
RUN cargo fetch
RUN RUSTDOCFLAGS="-D warnings" cargo doc --no-deps --lib

FROM nginx

# Put it all together
RUN rm -rf /usr/share/nginx/html
COPY --from=hugo-builder /website/docs/public /usr/share/nginx/html
COPY --from=rust-builder /rustdoc/target/doc /usr/share/nginx/html/rustdoc
