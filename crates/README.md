# Texcraft crates

This directory contains all of the Texcraft crates that
  are expected to be used from outside the Texcraft project.
The naming convention for the crates here is:

- `texlang-*`: crates related to the TeX language parser Texlang. 
  These have nothing to do with typesetting!

- `texcraft-*`: non-Texlang crates that are mostly useful only for software
  that is working with the Texcraft project (e.g., a TeX distribution built with Texcraft).

- Any others (e.g. `tfm`): general purpose crates that are developed by the Texcraft project,
  but don't use any Texcraft specific APIs and could potentially be useful
  in non-Texcraft software.

## Other crates in the repository

There are some other crates in the Git respository, but they
  are kept separate because they are mostly internal to the Texcraft project:

- `/bin`: single crate for all Texcraft binaries.

- `/performance`: performance benchmarks and tooling.

- `/playground`: Rust code for the Texcraft Playground.
