# Texcraft performance

This directory contains performance testing facilities for Texcraft.
In order to be a viable alternative to the original TeX implementations,
    Texcraft needs to be about as performant as they are.
This is really hard!
Knuth's TeX is very, very fast.

## Digits of pi benchmark

The TeX script `digits-of-pi.tex` computes the first n digits of pi.
It uses an O(n^2) algorithm that is somewhat slow and involves a ton of TeX macro expansions,
    making it a good script for exercising Texcraft's TeX language driver.
It also works with pdfTeX, so it can be used for comparitive testing.

The script can be run with profiling software to get a sense for where Texcraft's performance problems are.
[Flamegraph](https://github.com/flamegraph-rs/flamegraph) is nice because its results are
in a simple SVG format:

    cargo build --release
    flamegraph target/release/texcraft expand performance/digits-of-pi.tex

### pdfTeX vs Texcraft performance as of 2021-09-22

These performance numbers were obtained on 
    [a machine running an 8-core Ryzen 7 CPU](https://pcpartpicker.com/list/Y3FbBc).

| n    | pdfTeX | Texcraft | Multiplier (lower is better, target is 1)
|------|--------|----------|----
| 250  | 0.5s   | 3.2s     | 6.4
| 500  | 1.9s   | 12.7s    | 6.7
| 1000 | 7.5s   | 50.4s    | 6.7
| 2500 | \*     | 362.5s   | N/A

\*pdfTeX cannot execute the script for higher values of n as it runs out of stack space.
