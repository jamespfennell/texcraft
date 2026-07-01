# Box CLI E2E tests

Regenerate the golden files with:

```
cargo run --bin box \
  hlists \
  --texts-file=crates/boxworks-bin/tests/alice_in_wonderland.txt \
  --tex-engine=tex > crates/boxworks-bin/tests/alice_in_wonderland_hlists.txt
```

```
cargo run --bin box \
  linebreak --widths=10in \
  --texts-file=crates/boxworks-bin/tests/alice_in_wonderland.txt \
  --tex-engine=tex > crates/boxworks-bin/tests/alice_in_wonderland_linebreak.txt
```
