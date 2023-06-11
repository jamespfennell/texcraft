use crate::*;
use serde::{Deserialize, Deserializer, Serialize};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Serialize)]
struct SerializableVM<'a, S> {
    state: &'a S,
    commands_map: command::map::SerializableMap,
    interner: &'a token::CsNameInterner,
}

impl<'a, S> SerializableVM<'a, S> {
    fn new(vm: &'a vm::VM<S>) -> Self {
        Self {
            state: &vm.state,
            commands_map: command::map::SerializableMap::new(&vm.commands_map),
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
struct DeserializedVM<S> {
    state: S,
    commands_map: command::map::SerializableMap,
    interner: token::CsNameInterner,
}

impl<S> DeserializedVM<S> {
    fn finish_deserialization(
        self,
        initial_built_ins: HashMap<&str, command::BuiltIn<S>>,
    ) -> vm::VM<S> {
        let mut internal = vm::Internal::new(self.interner);
        let initial_built_ins = initial_built_ins
            .into_iter()
            .map(|(key, value)| {
                let cs_name = internal.cs_name_interner.get_or_intern(key);
                (cs_name, value)
            })
            .collect();
        vm::VM {
            state: self.state,
            commands_map: self.commands_map.finish_deserialization(initial_built_ins),
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
        }
    }
}

pub fn deserialize<'de, D: Deserializer<'de>, S: serde::Deserialize<'de>>(
    deserializer: D,
    initial_built_ins: HashMap<&str, command::BuiltIn<S>>,
) -> vm::VM<S> {
    let deserialized_vm: DeserializedVM<S> = Deserialize::deserialize(deserializer).unwrap();
    deserialized_vm.finish_deserialization(initial_built_ins)
}
