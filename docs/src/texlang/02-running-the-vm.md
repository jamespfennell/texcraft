# Running the Texlang VM

Before writing any custom TeX primitives it's good to know how to run the
    [Texlang virtual machine (VM)](https://docs.rs/texlang/latest/texlang/vm/index.html).
This way you can manually test the primitives you write
    to ensure they're working as you expect.
In the long run you may decide to lean more
    on [unit testing](unit-testing.md), even when initially developing commands,
    rather than manual testing things out.
But it's still good to know how to run the VM.

If you just want to see the minimal code to do this, jump down to the first code listing.

Running the VM is generally a four step process:

1. Specify the built-in commands/primitives you want the VM to include.

1. Initialize the VM using its [`new`](https://docs.rs/texlang/latest/texlang/vm/struct.VM.html#method.new) constructor.
    At this point you will need to decide which concrete state type you're using.
    The state concept was described in high-level terms in 
        [the previous section](introduction.md), and we will gain hands-on experience with it
        [the primitives with state section](stateful-primtives.md).
    For the moment, to keep things simple, we're just going to use a
        pre-existing state type that exists in the Texlang standard library crate:
        [`::texlang_stdlib::testing::State`](https://docs.rs/texlang-stdlib/latest/texlang_stdlib/testing/struct.State.html).

1. Load some TeX source code into the VM using the VM's [`push_source`](https://docs.rs/texlang/latest/texlang/vm/struct.VM.html#method.push_source) method.

1. Call the VM's [`run`](https://docs.rs/texlang/latest/texlang/vm/struct.VM.html#method.run) method, or some other helper function, to run the VM.

One minor complication at this point is that to call the VM's `run` method directly
    one needs to provide so-called "VM handlers".
These are Rust functions that tell the VM what to do when it encounters certain
    kinds of TeX tokens.
For example, when a real typesetting VM sees the character `a`,
    it typesets the character `a`.
Handlers are described in detail in the [VM hooks and handlers section](hooks-and-handlers.md).
For the moment, we're going to get around the handlers problem entirely
    by instead running the VM using the [`::texlang_stdlib::script::run`](https://docs.rs/texlang-stdlib/latest/texlang_stdlib/script/fn.run.html) function.
This function automatically provides handlers such that when the VM sees a character,
    it just prints the character to the terminal.

With all of this context,
    here is a minimal code listing that runs the Texlang VM:

```rust
# extern crate texlang_stdlib;
# extern crate texlang;
use texlang::{vm, command};
use texlang_stdlib::StdLibState;
use texlang_stdlib::script;

// 1. Create a new map of built-in commands.
//    In this book's next section we will add some commands here.
let built_in_commands = std::collections::HashMap::new();

// 2. Initialize the VM.
let mut vm = vm::VM::<StdLibState>::new_with_built_in_commands(built_in_commands);

// 3. Add some TeX source code the VM.
vm.push_source("input.tex", r"Hello, World.");

// 4. Run the VM and write the results to stdout.
script::set_io_writer(&mut vm, std::io::stdout());
script::run(&mut vm).unwrap();
```

When you run this code, you will simply see:

```text
Hello, World.
```

In this case the VM has essentially nothing to do:
    it just passes characters from the input to the handler and thus the terminal.
To see the VM doing a little work at least, change the source code to this:

```rust
# extern crate texlang_stdlib;
# extern crate texlang;
# use texlang::{vm, command};
# let mut vm = vm::VM::<()>::new_with_built_in_commands(Default::default());
// 3. Add some TeX source code the VM.
vm.push_source("input.tex", r"Hello, {World}.");
```

The output here is the same as before - in particular, the braces `{}` disappear:

```text
Hello, World.
```

The braces disappear because they are special characters in the TeX grammar,
    and are used to denote the beginning and ending of a group.
The VM consumes these characters internally when processing the input.

Another thing the VM can do is surface an error if the input contains an undefined control sequence.
Because we haven't provided any built-in commands, every control sequence is undefined.
Changing the VM setup to the following:

```rust
# extern crate texlang_stdlib;
# extern crate texlang;
# use texlang::{vm, command};
# use texlang_stdlib::StdLibState;
# use texlang_stdlib::script;
# let mut vm = vm::VM::<StdLibState>::new_with_built_in_commands(Default::default());
// 3. Add some TeX source code the VM.
vm.push_source("input.tex", r"\controlSequence");

// 4. Run the VM and write the results to stdout.
script::set_io_writer(&mut vm, std::io::stdout());
if let Err(err) = script::run(&mut vm){
    println!["{err}"];
}
```

results in the following output:

```text
Error: undefined control sequence \controlSequence
 >>> input.tex:1:1
  | 
1 | \controlSequence
  | ^^^^^^^^^^^^^^^^ control sequence
```

In general, though, the VM can't do much if no built-in commands are provided.
In the next section we will learn how to write some.
