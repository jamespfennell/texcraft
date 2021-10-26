//! Tutorial for creating new primitives.
//!
//! # Creating new primitives: a tutorial
//!
//! This tutorial walks through creating three new primitives.
//! These primitives all have the feature that they cannot be created using user defined macros, so
//! their inclusion in a TeX engine would make the engine strictly more powerful!
//!
//! We will create:
//!
//! 1. An IP address command.
//! 1. A random number generator.
//! 1. A new assignment operator.
//!
//! ## 1: An IP address command
//!
//! Our first example is an expansion primitive that expands to the public IP address of the computer the engine is running on. The high level idea is pretty simple: we make a HTTP request to `checkip.amazonaws.com`, get the result as a string, break the string into characters, and convert these characters into character TeX tokens.
//!
//!
//! ## 2: A random number generator
//!
//! Our second example is a random number generating expansion primitive. The command will first peek at the next token in the stream. If the token is a digit, the expansion will be a random number between 0 and that digit inclusive. Otherwise the expansion will be a random number between 0 and 9 inclusive. (At the end of this example we will show how to use parse_number to make this less contrived.)
//!
//! ## 3: A new assignment operator
//!
//! This one is really contrived, but will illustrate
//!
//!
