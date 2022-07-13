# Texcraft performance

This directory contains performance benchmarks for Texcraft.
By default the benchmarks are also run for pdfTeX so that performance can be compared.
The general goal of Texcraft to be about as fast as than the original implementions of TeX.

At time of writing (March 2022) Texcraft performs a little bit better than pdfTeX.
But, we suspect this will regress as Texcraft becomes more feature complete.
Right now it seems some of the good performance is due to CPU caching effects and
  Rust compiler inlining decisions, which will change as the binary gets bigger.

## Benchmarks

### Digits of pi

```
cargo bench digits-of-pi
```

The TeX script `digits-of-pi.tex` computes the first n digits of pi.
It uses an O(n^2) algorithm that is somewhat slow and involves a ton of TeX macro expansions
  and variable lookups, making it a good script for exercising Texlang.

Use the environment variable `DIGITS_OF_PI_N` to customize the `n` when running using `cargo bench`.

### Lexer throughput

```
cargo bench lexer-throughput
```

The lexer throughput test generates an ~18mb random TeX file and times how
  long it takes Texcraft and pdfTeX to process it.
The TeX file is a no-op from the execution perspective - it doesn't do any typesetting
  or expand any macros.
For this reason the processing time is dominated by tokenization.

At time of writing, Texcraft takes 270ms and pdfTeX 450ms in the benchmark.
There is a bias here, though, because the pdfTeX time includes the time it takes to pipe the file into the input.
In any case, the takeaway here is that the lexer is not a bottleneck and shouldn't
  be performance optimized quite yet.

Random TeX files like the one used in the test can be generated using the `randomtex`
  binary.

### Advance

```
cargo bench advance
```

This is a microbenchmark to measure how fast it takes to expand a macro whose
  replacement text is `\advance \k by 1` and then execute this expansion.
This benchmark is sufficiently micro that you can think about the result in terms of time
  or clock cycles elapsed.

The benchmark actually executes the macro 1000 times because the instrumentation overhead
  dominates for one execution.
Criterion reports the result in microseconds; to get the actual benchmark just switch the unit
  to nanoseconds.

It's tricky to run this one for pdfTeX because the execution time would be dominated
  by wiring the input to the running process. 
Instead, the number below was obtained by running `advance.tex`
  with the macro executed 10 times in the main while loop, then 11 times, and taking the diff.
  
- Texcraft: 80ns, ~300 clock cycles

- pdfTeX: around 80ns
