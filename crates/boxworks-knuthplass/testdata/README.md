Expected outputs are generated with real TeX via the `box` CLI:

```
cargo run --bin box -- linebreak --width=5in --tex-engine=tex \
    "$(tr '\n' ' ' < crates/boxworks-knuthplass/testdata/wolf_hall_input.txt)" \
    > crates/boxworks-knuthplass/testdata/wolf_hall_5in_want.txt
```

Note the input must be passed as a single text: `--texts-file` treats each
line of the file as a *separate* paragraph, which is not what the unit tests
do with these input files.

To check that the `*_want.txt` and `*_log.txt` files still match what TeX
produces, run the unit tests in verify mode (requires a `tex` binary on the
path):

```
TEXCRAFT_VERIFY=tex cargo test --package boxworks-knuthplass
```

The `*_log.txt` files are TeX's `\tracingparagraphs=1` trace. Note they are
unwrapped: when capturing one manually, set the `max_print_line` environment
variable to a large value so TeX does not wrap lines at 79 characters
(verify mode does this automatically).
