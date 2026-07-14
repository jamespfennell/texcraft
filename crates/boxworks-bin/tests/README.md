# Box CLI E2E tests

Each test runs the `box` binary and compares its output to a golden file.
The golden files contain TeX's output for the same invocation
(via `--tex-engine=tex`), so the tests verify that Box matches TeX.

To verify the golden files against TeX:

```
TEXCRAFT_VERIFY=tex cargo test --package boxworks-bin
```

To regenerate the golden files from TeX (e.g. after adding a test with an
empty golden file, or changing a test's arguments):

```
TEXCRAFT_VERIFY=tex TEXCRAFT_VERIFY_OVERWRITE=true cargo test --package boxworks-bin
```
