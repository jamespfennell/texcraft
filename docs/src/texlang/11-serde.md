# Format files (a.k.a. serialization and deserialization)

This page describes Texlang's support for serializing and deserializing VMs.
This functionality allows Texlang to replicate Knuth's "format file" mechanism.

## Background

Knuth's original implementation of TeX includes a feature called "format files".
A TeX format is a set of general-purpose macros and other configurations such as category code mappings
    that are included as a preamble in TeX documents.
The plain TeX format was developed by Knuth concurrently with the initial implementation of TeX.
Nowadays the LaTeX format is so ubiquitous as to be synonymous with TeX itself.

A format file is created by running TeX, inputting the format definitions (which are in regular `.tex` files)
    and then dumping the state of the interpreter using the `\dump` primitive.
The resulting format file has the file extension `.fmt`.
Subsequent runs of the VM can then read the format file and apply the definitions "at high speed".
The format file mechanism is essentially a performance optimization that gets
    the format into the interpreter faster than parsing the `.tex` definitions each time.
This optimization was probably especially important when TeX was being developed in the early 80s
    and computers were much slower than today.

A modern perspective on format files is that they are a mechanism for serializing and
    deserializing the state of a TeX virtual machine.
Texlang includes support for such serialization and deserialization of VMs.
In fact, Texlang's (de)serialization feature is strictly more powerful than the
    format files mechanism in Knuth's TeX:

- Texlang's (de)serialization feature is implemented using the Rust library Serde,
    and is thus independent of any specific serialization format.
    Texlang VMs can be (de)serialized to and from any format compatible with Serde.
    All of the unit tests in the Texlang project are run for three formats:
        message pack, bincode, and JSON.

- Texlang VMs can be serialized irrespective of their internal state.
    With format files this is not the case:
        format files cannot be created when there is a current active group,
        or when typesetting has already started.

This latter property opens up some exciting use cases for Texlang (de)serialization,
    especially _checkpoint compilation_.
In theory, after shipping out each PDF page a Texlang VM could checkpoint its progress
    by serializing itself and persisting the bytes in the filesystem.
Then, when the same TeX document is compiled,
    the interpreter could check if the document hasn't changed up to a certain checkpoint.
If so, instead of recompiling the entire document, the checkpoint could
    be deserialized and compilation could continue from the checkpoint.
This would offer genuine O(1) generation of the Nth page in a TeX document.

## Serializing VMs

Texlang VMs are generic over the state `S`.
Whether or not you can serialize or deserialize the VM
    depends on properties of the state.
We will start by discussing serialization.

If the state `S` implements [`::serde::Serialize`] then
    the Texlang VM `vm::V<S>` satisfies the [`::serde::Serialize`] trait too.
VMs can thus be serialized using the standard Serde infrastructure.
Note that making `S` serializable with Serde us usually very easy
    and just involves adding type annotations.

Here's a simply example of serializing a VM to JSON:

```rust
# extern crate serde;
# extern crate serde_json;
# extern crate texlang;
use texlang::vm;

#[derive(Default, serde::Serialize, serde::Deserialize)]
struct State {
    number: i32,
}

let built_in_commands = Default::default();
let vm = vm::VM::<State>::new_with_built_in_commands(built_in_commands);
let serialized_vm = serde_json::to_string_pretty(&vm).unwrap();
println!["{serialized_vm}"];
```

## Deserializing VMs

Deserialization is a little more tricky that serialization
    because the serialized bytes
    are not enough to fully reconstruct the VM.
Specifically, the VM's built-in primitives are missing from the serialized bytes
    and must be provided again at deserialization time.
This is because, fundamentally, Texlang primitives are Rust function pointers
    and these cannot be serialized and deserialized.

The easiest way to support deserialization is to implement  [`vm::HasDefaultBuiltInCommands`]
    for the state.
This trait provides the default set of built-in commands for that state.

If the state `S` implements [`::serde::Deserialize`] and
    this trait [`super::HasDefaultBuiltInCommands`],
    the Texlang VM `vm::V<S>` satisfies the [`::serde::Deserialize`] trait too.
In this case deserialization can be done in the usual way with Serde:

```rust
# extern crate serde;
# extern crate serde_json;
# extern crate texlang;
# use std::collections::HashMap;
use texlang::vm;
use texlang::command;

#[derive(Default, serde::Serialize, serde::Deserialize)]
struct State {
    number: i32,
}
impl vm::TexlangState for State {}
impl vm::HasDefaultBuiltInCommands for State {
    fn default_built_in_commands() -> HashMap<&'static str, command::BuiltIn<Self>> {
        // Returning an empty set of built-in commands, but in general this will be non-empty.
        HashMap::new()
    }
}

// When `vm::HasDefaultBuiltInCommands` is implemented for the state,
// the VM's plain `new` constructor can be used.
let original_vm = vm::VM::<State>::new();
let serialized_vm = serde_json::to_string_pretty(&original_vm).unwrap();
println!["{serialized_vm}"];

let deserialized_vm: vm::VM::<State> = serde_json::from_str(&serialized_vm).unwrap();
```

If the state doesn't implement [`vm::HasDefaultBuiltInCommands`],
    or you are using a non-default set of built-in commands,
    deserialization can be done in one of two ways.
First way: use the [`VM::deserialize_with_built_in_commands`]() helper function that
    accepts a Serde deserializer and the built-in commands:

```rust
# extern crate serde_json;
# extern crate texlang;
# use texlang::vm;
#
# let built_in_commands = Default::default();
# let vm = vm::VM::<()>::new_with_built_in_commands(built_in_commands);
# let serialized_vm = serde_json::to_string_pretty(&vm).unwrap();
# let built_in_commands = Default::default();

let mut deserializer = serde_json::Deserializer::from_str(&serialized_vm);
let vm = vm::VM::<()>::deserialize_with_built_in_commands(&mut deserializer, built_in_commands);
```

Second way: first deserialize the bytes to a value of type [`vm::serde::DeserializedVM`](),
    and then convert this value into a regular VM using the [`vm::serde::finish_deserialization`]() function:

```rust
# extern crate serde_json;
# extern crate texlang;
# use texlang::vm;
#
# let built_in_commands = Default::default();
# let vm = vm::VM::<()>::new_with_built_in_commands(built_in_commands);
# let serialized_vm = serde_json::to_string_pretty(&vm).unwrap();
# let built_in_commands = Default::default();

let deserialized_vm: Box<vm::serde::DeserializedVM<()>> = serde_json::from_str(&serialized_vm).unwrap();
let vm = vm::serde::finish_deserialization(deserialized_vm, built_in_commands);
```

## Serializing VMs inside TeX commands

Using Rust code like in the previous subsection,
    it's possible to write TeX primitives that serialize the VM and write the result to a file -
    i.e., write a format file!
The Texlang standard library includes an implementation of the `\dump` primitive that does this.
The `texcraft` binary accepts a `--format-file` argument that reads the format files,
    and continues from where it left off.


## Primitive tags and (de)serialization

In a [previous section](05-primitive-tags.md) we discussed primitive tags.
These provide unique identifiers that are generated using a global counter at runtime.
Tags sometimes appear in the state, but they are not safe to serialize and deserialize.
Deserialized tags may collide with new tags generated using the global counter.

Instead, when (de)serializing a component that contains a tag,
    the tag field should be skipped when serializing
    and the value should be provided manually when deserializing.
This can be achieved using Serde's [skip field attribute](https://serde.rs/field-attrs.html#skip):

```rust
# extern crate serde;
# extern crate texlang;
use serde::{Serialize, Deserialize};
use texlang::command;

static TAG: command::StaticTag = command::StaticTag::new();

fn get_tag() -> command::Tag {
    TAG.get()
}

#[derive(Serialize, Deserialize)]
struct Component {
    variable_value: i32,
    #[serde(skip, default="get_tag")]
    tag: command::Tag,
}

// This Default implementation is not needed for (de)serializing.
// However it illustrates how to ensure that new and deserialized components have the same tag.
impl Default for Component {
    fn default() -> Self {
        Self {
            variable_value: 0,
            tag: get_tag(),
        }
    }
}
```

Another approach is to extract the tag to its own sub-struct
    and then manually implement the `Default` trait for that sub-struct.
In this case we instruct Serde to use the default value for the sub-struct when deserializing:

```rust
# extern crate serde;
# extern crate texlang;
use serde::{Serialize, Deserialize};
use texlang::command;

static TAG: command::StaticTag = command::StaticTag::new();

#[derive(Serialize, Deserialize)]
struct Component {
    variable_value: i32,
    #[serde(skip)]
    tags: Tags,
}

struct Tags {
    tag: command::Tag,
}

impl Default for Tags {
    fn default() -> Self {
        Self {
            tag: TAG.get(),
        }
    }
}
```
