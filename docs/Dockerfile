FROM rust:1.56 AS builder

# Build the Rustdoc HTML files
WORKDIR /rustdoc
COPY Cargo.lock .
COPY Cargo.toml .
COPY crates crates
RUN cargo fetch
RUN cargo doc --no-deps --lib

# Build the MD Book
WORKDIR /mddoc
COPY docs .
RUN curl -sSL https://github.com/rust-lang/mdBook/releases/download/v0.4.15/mdbook-v0.4.15-x86_64-unknown-linux-gnu.tar.gz | tar -xz --directory=/usr/bin
RUN ls -lah
RUN mdbook build

# Put it all together
FROM nginx
RUN rm -rf /usr/share/nginx/html
COPY --from=builder /mddoc/book /usr/share/nginx/html
COPY --from=builder /rustdoc/target/doc /usr/share/nginx/html/reference
RUN ls -lah /usr/share/nginx/html/reference
