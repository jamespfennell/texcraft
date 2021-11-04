# Texcraft performance

This directory contains performance tests for Texcraft.
The general goal is to be as fast or faster than the original implementions of TeX.
This initially seemed too ambitious, but in late October 2021 as more
  optimizations were commited, Texcraft overtook pdfTeX in the benchmarks.

## Benchmarks

### Digits of pi

The TeX script `digits-of-pi.tex` computes the first n digits of pi.
It uses an O(n^2) algorithm that is somewhat slow and involves a ton of TeX macro expansions,
    making it a good script for exercising Texcraft's TeX language driver.
It also works with pdfTeX, so it can be used for comparitive testing.

Use the environment variable `DIGITS_OF_PI_N` to customize the `n` when running using `cargo bench`.

These performance numbers were obtained on 
    [a machine running an 8-core Ryzen 7 CPU](https://pcpartpicker.com/list/Y3FbBc).

| n    | pdfTeX | Texcraft | Multiplier (lower is better, target is <1)
|------|--------|----------|----
| 250  | 487ms  | 405ms    | 0.83
| 1000 | 7.60s  | 6.48s    | 0.85
| 2500 | \*     | 48s      | N/A

\*pdfTeX cannot execute the script for higher values of n as it runs out of stack space.

### Lexer throughput

The lexer throughput test generates an ~18mb random TeX file and times how
  long it takes Texcraft and pdfTeX to process it.
The TeX file is a no-op from the execution perspective - it doesn't do any typesetting
  or expand any macros.
For this reason the processing time is dominated by tokenization.

At time of writing, both programs process the file in around 550ms.
The big takeaway here is that this part of the system is not a bottleneck and shouldn't
  be stressed about.

Random TeX files like the one used in the test can be generated using the `randomtex`
  binary.

### Advance

This is a microbenchmark to measure how fast it takes to execute a macro whose
  replacement text is `\advance \k by 1`.
The benchmark actually executes this 1000 times because the instrumentation overhead
  dominates for one execution.
Criterion reports in microseconds; to get the actual benchmark just switch the unit
  to nanoseconds.

It's tricky to run this one for pdfTeX because the execution time would be dominated
  by wiring the input to the running process. 
Instead, the number below was obtained by running `advance.tex`
  with the macro executed 10 times in the main while loop, then 11 times, and finally taking the diff.
  
- Texcraft: 80ns

- pdfTeX: around 80ns
