# Hyphenate binary

Validation testing:

```
cargo run --bin hyphenate -- --words-file /usr/share/dict/words --validate --no-output --time
```

Performance testing:

```
cargo run --release --bin hyphenate -- --words-file /usr/share/dict/words --no-output --time
```
