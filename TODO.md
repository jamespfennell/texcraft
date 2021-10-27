# TODO
 
Documentation:

1. The input module and the input unit
1. The primitives tutorial

Turorial:

//! 1. An IP address command.
//! 1. A random number generator.
//! 1. A new assignment operator.

The Texcraft book?

Conditionals

1. implement \if and \ifcat
1. Add a system for writing if commands outside of the module? Or don't bother?


- Macros
  - Check all the errors in the \def message and ensure they're good - also extract them to a submodule
  - Figure out logging and \tracingmacros <- need a logging module
  - Implement \edef \xdef and \gdef etc
  - Implement \let
  - Implement \long, which will just complain when it sees literally "par". Add tests for this \def\par{A}
  - Implement \outer?


- Expansion tests
  - Write some tests for multiple input files, the harness will be the hardest part. Will need to implement \input
  - Add a test for Plain TeX looping.
  - Add a test for Plain TeX \newif command thing.
  - Finish extracting TeXBook tests to their own package. 


In exec mode, should print as soon as data becomes available.
This is related to logging

## Low priority
- show if conditional token in the error message
- Write a unit test to verify that TeX also does caching in the lexer ... or does it


## Optimization ideas:
- Lazily construct the KMP matcher for macros
- Don't use a KMP matcher for small macros
