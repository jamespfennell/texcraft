# License

The Texcraft project is dual-licensed under
    the [Apache License, Version 2.0]() or the [MIT license]().

This dual-license scheme is the convention in the Rust ecosystem.
The Texcraft project aims to keep this license scheme in the long run.
However the TeX ecosystem at large generally prefers the GPL,
    and some future parts of Texcraft may need to be GPL'd too depending on how the project develops.
See [use of existing TeX source code](#use-of-existing-tex-source-code) below for more information.

There are some additional files in the repository, used only for testing,
    that have different licenses.

## Contributing

Copyrights in the Texcraft project are retained by their contributors.
No copyright assignment is required to contribute to the Texcraft project.
Unless you explicitly state otherwise,
    any contribution intentionally submitted for inclusion in Texcraft by you,
    as defined in the Apache-2.0 license, shall be dual licensed as described above,
    without any additional terms or conditions. 

## Use of existing TeX source code

Rust code in the Texcraft project is written manually.
However its logical content is sometimes heavily based on source code in the wider TeX ecosystem
    written by Donald Knuth and others.
When developing parts of Texcraft that are clear re-implementations of existing TeX functionality
    (for example implementing TeX user-defined macros)
    we typically study the original source code closely,
    reference the original source code within the Texcraft source ([example](https://github.com/jamespfennell/texcraft/blob/0e70e10ef94c19cce0817137eca1735a75e259d4/crates/tfm/src/ligkern/mod.rs#L58))
    and in some cases simply translate small source code fragments into Rust ([example](https://github.com/jamespfennell/texcraft/blob/0e70e10ef94c19cce0817137eca1735a75e259d4/crates/tfm/src/lib.rs#L117-L137)).
This development process is essentially required in order to fulfill our
    goal of 100% compatibility with the original implementation of TeX.

This process does impact licensing because from a copyright
    perspective Texcraft is a derived work of the source code we consult this closely.
(Our process is the opposite of a ["clean room design"](https://en.wikipedia.org/wiki/Clean_room_design)
    in which one attempts to re-implement a system without consulting the source code
    and thus be free of copyright restrictions.)
So far all of the sources we've used are in the public domain,
    and this allows us to dual-license our code as above.

We have used the following sources:

| Program | Source file | License |
|---------|-------------|---------|
| TeX     | [`tex.web`](https://github.com/TeX-Live/texlive-source/blob/f1b12d7a20aaad9200cc4948ff481ffa90648c34/texk/web2c/tex.web) | [Public domain](https://www.tug.org/texinfohtml/web2c.html#Legalisms)
| TFtoPL  | [`tftopl.web`](https://github.com/TeX-Live/texlive-source/blob/f1b12d7a20aaad9200cc4948ff481ffa90648c34/texk/web2c/tftopl.web) | [Public domain](https://www.tug.org/texinfohtml/web2c.html#Legalisms)
| PLtoTF  | [`pltotf.web`](https://github.com/TeX-Live/texlive-source/blob/f1b12d7a20aaad9200cc4948ff481ffa90648c34/texk/web2c/pltotf.web) | [Public domain](https://www.tug.org/texinfohtml/web2c.html#Legalisms)




