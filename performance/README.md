# Texcraft performance

This directory contains performance benchmarks for Texcraft.
By default the benchmarks are also run for pdfTeX so that performance can be compared.
It is a first class goal of Texcraft to be about as fast as Knuth's original implementation of TeX.

Set the environment variable `SKIP_PDFTEX=1` to skip the pdfTeX comparison.

All of the benchmarks can be configured to run for longer using per-benchmark environment variables
  as described below.
The defaults are chosen so that each benchmark takes ~250ms in pdfTeX on my Linux machine.
Note that by default `cargo bench` runs each benchmark 100 times.

At time of writing (August 2023) Texcraft performs about ~5% worse than pdfTeX.
We suspect this may regress further as Texcraft becomes more feature complete.
This is because some features (like supporting `\afterassignment`) require adding logic to some hot loops.
However, on the other hand, there is likely room to performance optimize Texcraft to compensate for this.

## Benchmarks

### Digits of pi

```
cargo bench digits-of-pi
```

The TeX script `digits-of-pi.tex` computes the first n digits of pi.
It uses an O(n^2) algorithm that is somewhat slow and involves many TeX macro expansions
  and variable lookups.
It is thus a good script for benchmarking the core TeX semantics in Texlang.
Additionally it typesets very little content,
  so even though pdfTeX is doing more work (it generates a PDF) the benchmark is fair because
  almost all the time is spent calculating the content to produce.

Use the environment variable `DIGITS_OF_PI_N` to customize the `n` when running using `cargo bench`.
It defaults to 175.

### Primes

```
cargo bench primes
```

This is very similar to the digits of pi benchmark in terms of its performance coverage.
It calculates and prints the nth digit of pi using the `primes.tex` script.

Use the environment variable `PRIMES_N` to customize the `n` when running using `cargo bench`.
It defaults to 130.

### Lexer throughput

```
cargo bench lexer-throughput
```

The lexer throughput test generates a large (default 18mb) random TeX file and times how
  long it takes Texcraft and pdfTeX to process it.
The TeX file is a no-op from the execution perspective - it doesn't do any typesetting
  or expand any macros.
For this reason the processing time is dominated by tokenization.

At time of writing, both Texcraft and pdfTeX take about ~250ms in the benchmark.
One takeaway here is that the lexer is not a bottleneck and shouldn't
  be performance optimized quite yet.
  
Use the environment variable `LEXER_THROUGHPUT_MB` to customize the the number of megabytes in the TeX file.
It defaults to 18.

Random TeX files like the one used in the test can be generated using the `randomtex`
  binary.

## Profiling

```
SKIP_PDFTEX=1 cargo flamegraph  --bench lexer_throughput -- --bench
```
