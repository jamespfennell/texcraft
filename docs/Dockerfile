FROM golang:1.16 AS hugo-builder

# Install Hugo
WORKDIR /usr/bin
RUN curl -L -o hugo.tar.gz https://github.com/gohugoio/hugo/releases/download/v0.87.0/hugo_extended_0.87.0_Linux-64bit.tar.gz
RUN tar -zxvf hugo.tar.gz

# Build the Hugo HTML files
WORKDIR /website
COPY . .
RUN cd docs && hugo

FROM rust:1.56 AS rust-builder

# Build the Rustdoc HTML files
WORKDIR /rustdoc
COPY Cargo.lock .
COPY Cargo.toml .
RUN mkdir src
RUN echo "fn main() {}" > src/main.rs
# TODO: remove the following line when multi-crate happens
COPY benches benches
RUN cargo fetch
COPY src src
RUN cargo doc --no-deps --lib

FROM nginx

# Put it all together
RUN rm -rf /usr/share/nginx/html
COPY --from=hugo-builder /website/docs/public /usr/share/nginx/html
COPY --from=rust-builder /rustdoc/target/doc /usr/share/nginx/html/rustdoc
