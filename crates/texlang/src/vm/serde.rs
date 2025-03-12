//! Serialization and deserialization of VMs
//!
//! Texcraft's documentation website has a
//!     [dedicated page about serializing and deserializing VMs](https://texcraft.dev/texlang/11-serde.html).
//! The documentation here is for reference.
//!
//! ## Serialization
//!
//! If the state type `S` implements [`::serde::Serialize`] then
//!     the Texlang VM `vm::V<S>` satisfies the [`::serde::Serialize`] trait too.
//! VMs can thus be serialized using the standard Serde infrastructure.
//!
//! ## Deserialization
//!
//! If the state `S` implements [`::serde::Deserialize`] and
//!     the Texlang trait [`super::HasDefaultBuiltInCommands`],
//!     the Texlang VM `vm::V<S>` satisfies the [`::serde::Deserialize`] trait too.
//!
//! If the state doesn't implement the Texlang trait, deserialization is slightly more complicated
//!     because the set of built-in primitives needs to be provided at deserialization time.
//! This is because the built-in primitives which are regular Rust functions,
//!     and it is not possible to fully serialize and deserialize Rust functions.
//! Deserialization of VMs is thus a two-step process:
//!
//! 1. Deserialize the bytes to a [`DeserializedVM`] type.
//!
//! 2. Invoke [`finish_deserialization`] with the deserialized VM
//!     and a map of built-in commands in order to recover the regular Texlang VM.
//!
//! The Texlang VM has a [`deserialize_with_built_in_commands` convenience method](super::VM::deserialize_with_built_in_commands)
//!     which performs both of these steps at once.

use crate::*;
use serde::{Deserialize, Deserializer, Serialize};
use std::collections::HashMap;

#[derive(Serialize)]
struct SerializableVM<'a, S> {
    state: &'a S,
    commands_map: &'a command::Map<S>,
    internal: &'a vm::Internal<S>,
    save_stack: Vec<variable::SerializableSaveStackElement<'a>>,
}

impl<'a, S> SerializableVM<'a, S> {
    fn new(vm: &'a vm::VM<S>) -> Self {
        let variable_key_to_built_in = vm.commands_map.getters_key_to_built_in();
        Self {
            state: &vm.state,
            commands_map: &vm.commands_map,
            internal: &vm.internal,
            save_stack: vm
                .internal
                .save_stack
                .iter()
                .map(|element| element.serializable(&variable_key_to_built_in))
                .collect(),
        }
    }
}

impl<State: serde::Serialize> Serialize for vm::VM<State> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let serializable_vm = SerializableVM::new(self);
        serializable_vm.serialize(serializer)
    }
}

/// A VM that has been deserialized.
///
/// In order to recover a regular [Texlang `VM`](super::VM) it is necessary to
///     call the [`finish_deserialization`] function with the relevant built-in commands.
#[derive(Deserialize)]
pub struct DeserializedVM<'a, S> {
    state: S,
    commands_map: command::map::SerializableMap<'a>,
    internal: vm::Internal<S>,
    save_stack: Vec<variable::SerializableSaveStackElement<'a>>,
}

/// Finish the deserialization of a VM.
///
/// This function accepts a [`DeserializedVM`] and a collection of built-in commands
///     and returns a regular [Texlang `VM`](super::VM).
pub fn finish_deserialization<S>(
    #[allow(clippy::boxed_local)] mut deserialized: Box<DeserializedVM<'_, S>>,
    built_in_commands: HashMap<&str, command::BuiltIn<S>>,
) -> vm::VM<S> {
    let built_in_commands = built_in_commands
        .into_iter()
        .map(|(key, value)| {
            let cs_name = deserialized.internal.cs_name_interner.get_or_intern(key);
            (cs_name, value)
        })
        .collect();
    deserialized.internal.save_stack = deserialized
        .save_stack
        .into_iter()
        .map(|element| element.finish_deserialization(&built_in_commands))
        .collect();
    let commands_map = deserialized
        .commands_map
        .finish_deserialization(built_in_commands, &deserialized.internal.cs_name_interner);
    vm::VM {
        state: deserialized.state,
        commands_map,
        working_directory: match std::env::current_dir() {
            Ok(path_buf) => Some(path_buf),
            Err(err) => {
                println!("failed to determine the working directory: {err}");
                None
            }
        },
        internal: deserialized.internal,
    }
}

pub(super) fn deserialize<'de, D: Deserializer<'de>, S: serde::Deserialize<'de>>(
    deserializer: D,
    built_in_commands: HashMap<&str, command::BuiltIn<S>>,
) -> Result<vm::VM<S>, D::Error> {
    let deserialized_vm: Box<DeserializedVM<S>> = Deserialize::deserialize(deserializer)?;
    Ok(finish_deserialization(deserialized_vm, built_in_commands))
}
