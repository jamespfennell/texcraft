# Unit testing

When writing code that uses Texlang it's generally expected that unit tests
    will also be written to verify the primitives being implemented work as expected.

Software projects are more likely to have high-quality,
    extensive unit tests if it is easy to write and maintain such tests.
In order to support easy unit-testing of code that uses Texlang,
    the Texcraft project includes a specific crate for writing unit tests called
    [`texlang_testing`](https://texcraft.dev/reference/texlang_testing).
The crate currently supports three kinds of unit tests:

- Expansion equality tests: verifying that two TeX snippets expand to the same output.

- Failure tests: verifying that a TeX snippet fails to run.

- Serde tests: verifying that a Texlang VM can be successfully serialized and deserialized
    in the middle of executing a TeX snippet.

This crate is used extensively in the Texlang standard library.
Browsing some of the Rust source code will give a good sense of how tests are written using this library.

For information on writing unit tests using this crate,
    [consult the crate's documentation](https://texcraft.dev/reference/texlang_testing).
