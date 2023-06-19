---
title: Building a TeX interpreter with Texlang
---

# Building a TeX interpreter with Texlang

## Getting started

- Create a new binary crate
- Read in input and then run it - should do nothing

## Adding some state

- Add an i32 number
- Add an `\increment` command and print the value when its incremented
- Exercise: add a `\square` command that squares the number

## Add a print command

- Implement a print command

## Execution commands with input

- Implement `\incrementBy <number>`
- Exercise: add a `\setTo <number>` command

## Expansion command

- What if we want to increment the number by its current value?
- `\value`

```tex
\increment
\print
\incrementBy \value
\print
\incrementBy \value
\print
\incrementBy \value
\print
```

should output
```
1
2
4
8
```


## Adding a Texlang variable

`\number`
- Can then do `\number=30`


## Adding some standard library commands

`\advance`

`\the`

Exercise: add some other commands like `\input`
