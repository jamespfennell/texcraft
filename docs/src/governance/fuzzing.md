# Fuzz testing

## tfm crate

From the root of the repository:

```bash
cargo fuzz run --fuzz-dir crates/tfm/fuzz --dev fuzz_tftopl
```

The `--dev` flag may be optional, but on my computer it results in linker errors.

If a failing input is found and Texcraft does not panic,
  the fuzzing harness will automatically add data for a test case to
  `crates/tfm/bin/tests/data`.
A new test can then be added in `convert.rs`.

In the case when Texcraft panics (or in general),
  run the fuzzer with `TEXCRAFT_FUZZ_OUTPUT_ALWAYS=1`
  to always write test cases out.
