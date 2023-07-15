//! Serialization and deserialization of VMs
//!
//! Texlang VMs can be serialized using the standard serde infrastructure
//!     because they satisfy the [`::serde::Serialize`] trait.
//!
//! Deserialization is slightly more complicated.
//! This is because VM contains built-in primitives which are regular Rust functions.
//! It is not possible to fully serialize and deserialize Rust functions.
//! Deserialization of VMs is thus a two-step process:
//!
//! 1. Deserialize the bytes to a [`DeserializedVM`] type.
//!
//! 2. Invoke [`finish_deserialization`] with the deserialized VM
//!     and a map of built-in primitives in order to recover the regular Texlang VM.
//!
//! The Texlang VM has a [`deserialize` convenience method](super::VM::deserialize)
//!     which performs both of these steps at once.
//!

use crate::*;
use serde::{Deserialize, Deserializer, Serialize};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

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
///     call the [`finish_deserialization`] function with the relevant initial built-in primitives.
#[derive(Deserialize)]
pub struct DeserializedVM<'a, S> {
    state: S,
    commands_map: command::map::SerializableMap<'a>,
    internal: vm::Internal<S>,
    save_stack: Vec<variable::SerializableSaveStackElement<'a>>,
}

/// Finish the deserialization of a VM.
///
/// This function accepts a [`DeserializedVM`] and a collection of initial built-in primitives
///     and returns a regular [Texlang `VM`](super::VM).
pub fn finish_deserialization<S>(
    #[allow(clippy::boxed_local)] mut deserialized: Box<DeserializedVM<'_, S>>,
    initial_built_ins: HashMap<&str, command::BuiltIn<S>>,
) -> Box<vm::VM<S>> {
    let initial_built_ins = initial_built_ins
        .into_iter()
        .map(|(key, value)| {
            let cs_name = deserialized.internal.cs_name_interner.get_or_intern(key);
            (cs_name, value)
        })
        .collect();
    deserialized.internal.save_stack = deserialized
        .save_stack
        .into_iter()
        .map(|element| element.finish_deserialization(&initial_built_ins))
        .collect();
    let commands_map = deserialized
        .commands_map
        .finish_deserialization(initial_built_ins, &deserialized.internal.cs_name_interner);
    Box::new(vm::VM {
        state: deserialized.state,
        commands_map,
        file_system: Box::new(super::RealFileSystem {}),
        terminal_in: Rc::new(RefCell::new(super::RealTerminalIn {})),
        terminal_out: Rc::new(RefCell::new(std::io::stderr())),
        log_file: Rc::new(RefCell::new(std::io::sink())),
        working_directory: match std::env::current_dir() {
            Ok(path_buf) => Some(path_buf),
            Err(err) => {
                println!("failed to determine the working directory: {err}");
                None
            }
        },
        internal: deserialized.internal,
    })
}

pub(super) fn deserialize<'de, D: Deserializer<'de>, S: serde::Deserialize<'de>>(
    deserializer: D,
    initial_built_ins: HashMap<&str, command::BuiltIn<S>>,
) -> Box<vm::VM<S>> {
    let deserialized_vm: Box<DeserializedVM<S>> = Deserialize::deserialize(deserializer).unwrap();
    finish_deserialization(deserialized_vm, initial_built_ins)
}
