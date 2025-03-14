# Dependencies policy

Rust and Cargo make it very easy to add third-party dependencies.
This can be useful for building software quickly.
However there is a well known phenomenon of "dependency bloat"
    for Rust projects in which even small crates will have
    a huge number of dependencies, direct and transitive.
This slows compilation times and adds maintenance costs
    as the dependencies need to be kept up to date.
There is also some supply chain and code quality risk because at some
    point no one knows what's in the dependencies.

This point is especially compelling for a project that reimplements
    Knuth's TeX, because TeX has no third-party dependencies at all!

In order to mitigate the risk of dependency bloat,
    we have adopted the following
    fairly strict dependencies policies:

- Texcraft libraries are not allowed to have any required third-party dependencies.
    Third-party dependencies must be gated behind a Cargo feature.

- It is okay if a Cargo feature corresponding to a dependency is default enabled.
    For example, the Texlang standard library default enables the `time`
    feature which uses the third-party Crate `chrono`.
    In this case disabling the feature by default would be a footgun because
    the values of `\day`, `\month`, etc. would not be initialized correctly.

- There is a small curated collection of
    third-party Rust crates are always okay to use.
    (But if they are used in a Texcraft library, they must be gated behind a Cargo feature.)
    If you want to use one of these crates, don't think twice:
        the project is already deeply invested in using them,
        so there's no downside.
    These crates are:

    - Serde. This is always gated behind a `serde` feature.

    - Clap. This is used for building Texcraft binaries and is not relevant for libraries.

- For other third-party dependencies, try to think critically about whether
    the dependency is really worth it.
    It can be very tempting to add some cool dependency-based feature.
    For example, Texcraft's word diffing library originally had support for
    color-based diffs because I thought it would be cool.
    But in the end such a niche feature is not worth taking on a third-party
    dependency.


You can verify that Texcraft's libraries don't have any third-party
    dependencies by running the following command.
The output should only reference Texcraft libraries:

```sh
cargo tree -e normal --features="" --no-default-features \
    -p "boxworks*" \
    -p core \
    -p dvi \
    -p texcraft-stdext \
    -p "texlang*" \
    -p tfm
```

Conversely, this shows the dependency tree when all features are enabled:

```sh
cargo tree -e normal --all-features \
    -p "boxworks*" \
    -p core \
    -p dvi \
    -p texcraft-stdext \
    -p "texlang*" \
    -p tfm
```

## Dev dependencies

For dev dependencies we are less strict right now and there are
    no policy restrictions.
