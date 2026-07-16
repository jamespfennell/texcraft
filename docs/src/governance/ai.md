## AI coding policy

Texcraft's policy around AI usage depends on which part of the codebase is
being touched.

The **core Texcraft Rust libraries** are the library crates under `crates/` that
  reimplement Knuth's TeX.
Changes to non-testing code here are made by hand without AI.
There are a few reasons for this:

- One of the main value propositions of the Texcraft project is that it provides
  well-designed modular APIs for the internal components of TeX.
  We think this is currently a task that requires a lot of quality human judgment.

- We have the highest possible code quality standards for this code, and in our
  experience the AI cannot (yet?) achieve it.

- Specifically for the last point: there is a tension between writing the code
  in a way very similar to Knuth's TeX (very few data structures, no abstractions, highly procedural) and writing it in a more modern way.
  The former is better for correctness; the latter better for maintainability
  and re-usability.
  We think this decision requires quality human judgment.

- The whole point of the project is to have fun reimplementing TeX's algorithms
  and getting an AI to do it sort of defeats the point. :)

There are a couple of small exceptions to this policy for these libraries:

- Any kind of mechanical refactoring is of course okay to do with AI.

- Claude Code is great at RCAing bugs, in part because it has access to Knuth's
  original source code and can see where the difference is.
  It will generally suggest a fix to the Rust code.
  In our experience this fix fails our code quality and/or architecture standards.
  We do use the AI-proposed fix as a guide to understanding the bug and how to
  create a better quality human fix.

All other Rust code under `crates/` (testing and binaries) _can_ be written with
arbitrary AI assistance, but must be human reviewed and human readable.
We do not want this code to devolve into some AI slop state such that it can't
be worked on by humans any more.

All of the code under `sites/` (Rust, Javascript, HTML, etc.) is maintained
exclusively by AI and there is no expectation that the code be maintainable by
humans.
