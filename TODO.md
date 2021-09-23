# TODO
 
Documentation:

1. The input module and the input unit
1. The primitives tutorial


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
- Cleanup the input controller, especailly the hacky use of the expansions stack for caching lexing results
- Write a unit test to verify that TeX also does caching in the lexer ... or does it


## Optimization ideas:
- profile the primes script to see where stuff is happening
- Make the `Command` type flat/not nested and remove the docs so that it has better cache locality. Also maybe move
  - out the type?
- Lazily construct the KMP matcher for macros
- Don't use a KMP matcher for small macros
- Don't use if statements in the driver, instead use static dispatch on different ExpansionInput types
- Don't put user defined macros in Rc<> automatically - wait if/until the macro is cloned by \let
- Make one allocation for each UDM
- https://stackoverflow.com/questions/31264670/do-i-have-to-be-concerned-about-the-overhead-of-rc
- General optimizations around datastuctures used
- Use a different hash function for maps
