# Error handling

Like every programming language, TeX files can contain errors.
The Texlang errors system is used to implement TeX's error handling behavior.

## Errors in TeX

Before describing Texlang's error system
    we will describe the semantics of errors in TeX.

Most errors in TeX are recoverable.
Take the following TeX code,
    which tries to assign a non-integer value to the integer variable `\month`:

```tex
\month = March
```

Knuth's TeX prints the following error message:

```tex
! Missing number, treated as zero.
<to be read again> 
                   M
l.1 \month = M
              arch
```

TeX tried to interpret `March` as an integer and of course could not.
So instead, it recovered from the error by assuming the right hand side
    is 0, assigned the value 0 to `\month`,
    and then continued processing with the next token `M`.
Most TeX errors are like this:
    the system falls back to some default behavior and then optionally continues
    processing.

When it hits a TeX error like the one above,
    TeX can either continue or abort
    or even prompt the user to edit the input.
This depends on which of the four "interaction modes" is currently enabled:

1. `\errorstopmode`: the default mode in which errors drop the user
    into an interactive terminal and the user then instructs TeX
    what to do (abort, or ignore the error and move on, or switch to a different interaction mode, or edit the input inline).

1. `\scrollmode`:  all recoverable errors are recovered from,
    but the error messages are printed to the log file and terminal.
    However if more than 100 errors are hit, the program aborts.

1.  `\nonstopmode`: like `\scrollmode`, but other forms of terminal
    interaction like `\read` are also suppressed.

1. `\batchmode`: like `\nonstopmode`, except
    error messages are only printed to the log file and not the terminal

In Texlang, the decision to continue or abort is made inside
    the implementation of the 
    [`TexlangState::recoverable_error_hook`](/reference/texlang/vm/trait.TexlangState.html#method.recoverable_error_hook).

Finally, some errors like a file-not-found when using `\input`
    are fatal errors that can't be recovered from.
TeX aborts in such rare cases.

## Handling errors in Texlang

For a given error case you first need to define a Rust type for that case.
This type must implement then [`error::TexError`] trait.
At a minimum you must provide an error kind and a title.

Suppose you're writing a Texlang function to parse a yes/no answer.
The function will accept `Y` or `y` to mean to mean yes,
    and `N` or `n` to mean no.
The error that can occur here is that the user provides a different
    character like `A`.

First define a type for this error.
This type is a token error because the error occurs at a specific TeX token
    (in this case the character `A`).

```rust
# extern crate texlang;
# use texlang::prelude as txl;
# use texlang::traits::*;
# use texlang::{vm, token, error};
#[derive(Debug)]
struct YesOrNoError {
    token: token::Token,
}
impl error::TexError for YesOrNoError {
    fn kind(&self) -> error::Kind {
        error::Kind::Token(self.token)
    }
    fn title(&self) -> String {
        format!["invalid response to yes or no; expected Y or N"]
    }
}
```

Next, in the function that parses the yes/no,
    on the error path first construct the error,
    pass it to the [`TokenStream::error`] method of the function input.
If that doesn't error out, return the default value.

```rust
# extern crate texlang;
# use texlang::prelude as txl;
# use texlang::traits::*;
# use texlang::{vm, token, error};
# #[derive(Debug)]
# struct YesOrNoError {
#     token: token::Token,
# }
# impl error::TexError for YesOrNoError {
#     fn kind(&self) -> error::Kind {
#         error::Kind::Token(self.token)
#     }
#     fn title(&self) -> String {
#         format!["invalid response to yes or no; expected Y or N"]
#     }
# }
/// Parses a yes (character 'Y' or 'y') or no (character 'N' or 'n').
/// Returns true if the parsed value is "yes".
fn parse_yes_or_now<S: TexlangState>(input: &mut vm::ExecutionInput<S>) -> txl::Result<bool> {
    let token = input.next()?.expect("input has not ended");
    let yes = match token.value() {
        token::Value::Letter('Y' | 'y') => true,
        token::Value::Letter('N' | 'n') => false,
        _ => {
            // If the VM treats the error as fatal, a Rust error will be returned
            // here and then propagated using the `?` operator.
            input.error(YesOrNoError{token})?;
            // Otherwise, the VM has treated the error as recoverable and we
            // fallback to the recovery behavior.
            false
        }
    };
    Ok(yes)
}
```

## End of input errors

