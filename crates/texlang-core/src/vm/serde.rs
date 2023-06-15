use crate::*;
use serde::{Deserialize, Deserializer, Serialize};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Serialize)]
struct SerializableVM<'a, S> {
    state: &'a S,
    commands_map: &'a command::Map<S>,
    interner: &'a token::CsNameInterner,
}

impl<'a, S> SerializableVM<'a, S> {
    fn new(vm: &'a vm::VM<S>) -> Self {
        Self {
            state: &vm.state,
            commands_map: &vm.commands_map,
            interner: &vm.internal.cs_name_interner,
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

#[derive(Deserialize)]
struct DeserializedVM<'a, S> {
    state: S,
    commands_map: command::map::SerializableMap<'a>,
    interner: token::CsNameInterner,
}

fn finish_deserialization<S>(
    #[allow(clippy::boxed_local)] deserialized: Box<DeserializedVM<'_, S>>,
    initial_built_ins: HashMap<&str, command::BuiltIn<S>>,
) -> Box<vm::VM<S>> {
    let mut internal = vm::Internal::new(deserialized.interner);
    let initial_built_ins = initial_built_ins
        .into_iter()
        .map(|(key, value)| {
            let cs_name = internal.cs_name_interner.get_or_intern(key);
            (cs_name, value)
        })
        .collect();
    Box::new(vm::VM {
        state: deserialized.state,
        commands_map: deserialized
            .commands_map
            .finish_deserialization(initial_built_ins),
        file_system: Box::new(super::RealFileSystem {}),
        terminal: Rc::new(RefCell::new(std::io::stderr())),
        log_file: Rc::new(RefCell::new(std::io::sink())),
        working_directory: match std::env::current_dir() {
            Ok(path_buf) => Some(path_buf),
            Err(err) => {
                println!("failed to determine the working directory: {err}");
                None
            }
        },
        internal,
    })
}

pub fn deserialize<'de, D: Deserializer<'de>, S: serde::Deserialize<'de>>(
    deserializer: D,
    initial_built_ins: HashMap<&str, command::BuiltIn<S>>,
) -> Box<vm::VM<S>> {
    let deserialized_vm: Box<DeserializedVM<S>> = Deserialize::deserialize(deserializer).unwrap();
    finish_deserialization(deserialized_vm, initial_built_ins)
}
