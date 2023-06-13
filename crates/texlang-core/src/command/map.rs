//! Map type
use super::*;
use std::cell::{Ref, RefCell};
use std::collections::HashMap;
use std::fmt;
use texcraft_stdext::collections::groupingmap;
use texcraft_stdext::collections::groupingmap::GroupingHashMap;
use texcraft_stdext::collections::groupingmap::GroupingVec;

/// Map is a map type where the keys are control sequence names and the values are TeX commands.
///
/// There are a number of design goals for the map:
///
/// - Make retrieving the function for a command very fast. This is achieved primarily by storing
///   command functions by themselves in an array. The index in the array is the control sequence
///   name, which is an integer when interned. This implementation choice means fast-as-possible lookups.
///   Storing the function by itself means there is good cache locality.
///
///
/// - Insert built in commands at the start
///     insert_built_in(cs_name, cmd),
/// - Insert variable commands that aren't there at the start, but will be in the future. E.g., \countdef
///     register_built_in(cmd) <- must have a unique ID. Is type ID stable across binary builds? Maybe need to
///          use a string ID instead? But then need to feed that the library using this registered
/// - Should we enforce that the previous two steps can only be done at creation time? Probably
///   Maybe initialize the map using Map<cs_name, cmd> and `Vec<cmd>`. Can provide cs_name<->str wrapper at
///   the VM level
///
/// - While running, insert macros
///     insert_macro(&str, macro)
/// - While running, alias commands by current name or using the special ID inserts
///     alias_control_sequence(cs_name, cs_name) -> undefined control sequence error
///     alias_registered_built_in(cs_name, cmd_id, variable_addr)
///     alias_character(cs_name, token::Token)
pub struct Map<S> {
    commands: GroupingVec<Command<S>>,

    initial_built_ins: HashMap<token::CsName, BuiltIn<S>>,
    primitive_key_to_built_in_lazy: RefCell<Option<HashMap<PrimitiveKey, token::CsName>>>,
    variable_key_to_built_in_lazy: RefCell<Option<HashMap<variable::Key, token::CsName>>>,
}

impl TryFrom<PrimitiveKey> for variable::Key {
    type Error = ();

    fn try_from(value: PrimitiveKey) -> std::result::Result<Self, Self::Error> {
        match value {
            PrimitiveKey::Execution(_, _) => Err(()),
            PrimitiveKey::Expansion(_, _) => Err(()),
            PrimitiveKey::VariableSingleton(g) => Ok(g),
            PrimitiveKey::VariableArrayStatic(g, _) => Ok(g),
            PrimitiveKey::VariableArrayDynamic(g, _) => Ok(g),
        }
    }
}

impl<S> Map<S> {
    pub(crate) fn new(initial_built_ins: HashMap<token::CsName, BuiltIn<S>>) -> Map<S> {
        Self {
            commands: initial_built_ins
                .iter()
                .map(|(k, v)| (k.to_usize(), v.cmd.clone()))
                .collect(),
            initial_built_ins,
            primitive_key_to_built_in_lazy: Default::default(),
            variable_key_to_built_in_lazy: Default::default(),
        }
    }

    #[inline]
    pub fn get_command(&self, name: &token::CsName) -> Option<&Command<S>> {
        self.commands.get(&name.to_usize())
    }

    pub fn get_tag(&self, name: &token::CsName) -> Option<Tag> {
        match self.commands.get(&name.to_usize()) {
            None => None,
            Some(cmd) => cmd.tag(),
        }
    }

    pub fn get_command_slow(&self, name: &token::CsName) -> Option<BuiltIn<S>> {
        let command = match self.get_command(name) {
            None => return None,
            Some(t) => t,
        };
        if let Some(ref key) = PrimitiveKey::new(command) {
            if let Some(built_in) = self.primitive_key_to_built_in().get(key) {
                return self.initial_built_ins.get(built_in).cloned();
            }
        }
        Some(BuiltIn {
            cmd: command.clone(),
            doc: None,
        })
    }

    #[inline]
    pub fn insert_variable_command(
        &mut self,
        name: token::CsName,
        variable_command: variable::Command<S>,
        scope: groupingmap::Scope,
    ) {
        self.insert(
            name,
            Command::Variable(rc::Rc::new(variable_command)),
            scope,
        );
    }

    // TODO: reconsider this API of 4 setters ... it seems to be unnecessary complexity?
    pub fn insert_macro(
        &mut self,
        name: token::CsName,
        texmacro: texmacro::Macro,
        scope: groupingmap::Scope,
    ) {
        self.insert(name, Command::Macro(rc::Rc::new(texmacro)), scope);
    }

    pub fn alias_control_sequence(
        &mut self,
        alias: token::CsName,
        control_sequence: token::CsName,
        scope: groupingmap::Scope,
    ) -> std::result::Result<(), InvalidAlias> {
        let command = match self.get_command(&control_sequence) {
            None => return Err(InvalidAlias {}),
            Some(t) => t,
        };
        self.insert(alias, command.clone(), scope);
        Ok(())
    }

    pub fn alias_token(
        &mut self,
        alias: token::CsName,
        token: token::Token,
        scope: groupingmap::Scope,
    ) {
        self.insert(alias, Command::Character(token.value()), scope);
    }

    fn insert(&mut self, name: token::CsName, func: Command<S>, scope: groupingmap::Scope) {
        let key = name.to_usize();
        self.commands.insert(key, func, scope);
    }

    pub fn to_hash_map_slow(&self) -> HashMap<token::CsName, BuiltIn<S>> {
        let mut result = HashMap::new();
        for (key, _value) in self.commands.backing_container().iter().enumerate() {
            let cs_name = match token::CsName::try_from_usize(key) {
                None => continue,
                Some(cs_name) => cs_name,
            };
            let cmd = match self.get_command_slow(&cs_name) {
                None => continue,
                Some(cmd) => cmd,
            };
            result.insert(cs_name, cmd);
        }
        result
    }

    pub(crate) fn begin_group(&mut self) {
        self.commands.begin_group();
    }

    pub(crate) fn end_group(&mut self) -> std::result::Result<(), groupingmap::NoGroupToEndError> {
        self.commands.end_group()?;
        Ok(())
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        self.commands.len()
    }

    fn primitive_key_to_built_in(&self) -> Ref<'_, HashMap<PrimitiveKey, token::CsName>> {
        if let Ok(r) = Ref::filter_map(self.primitive_key_to_built_in_lazy.borrow(), Option::as_ref)
        {
            return r;
        }
        *self.primitive_key_to_built_in_lazy.borrow_mut() = Some(
            self.initial_built_ins
                .iter()
                .filter_map(|(cs_name, built_in)| {
                    PrimitiveKey::new(built_in.cmd()).map(|key| (key, *cs_name))
                })
                .collect(),
        );
        self.primitive_key_to_built_in()
    }

    fn variable_key_to_built_in(&self) -> Ref<'_, HashMap<variable::Key, token::CsName>> {
        if let Ok(r) = Ref::filter_map(self.variable_key_to_built_in_lazy.borrow(), Option::as_ref)
        {
            return r;
        }
        *self.variable_key_to_built_in_lazy.borrow_mut() = Some(
            self.primitive_key_to_built_in()
                .iter()
                .filter_map(|(key, cs_name)| {
                    key._variable_key()
                        .map(|variable_key| (variable_key, *cs_name))
                })
                .collect(),
        );
        self.variable_key_to_built_in()
    }
}

#[derive(Debug)]
pub struct InvalidAlias;

impl fmt::Display for InvalidAlias {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "invalid alias: the control sequence to alias is undefined"
        )
    }
}

impl std::error::Error for InvalidAlias {}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
enum SerializableCommand {
    BuiltIn(token::CsName),
    VariableArrayStatic(token::CsName, usize),
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub(crate) struct SerializableMap {
    // TODO: instead of serializing the map we could just serialize the iterator.
    // This would be more efficient on the deserialization path.
    // TODO: maybe the map should also serde itself using the iterator.
    commands: GroupingHashMap<token::CsName, SerializableCommand>,
}

impl SerializableMap {
    pub(crate) fn new<S>(map: &Map<S>) -> Self {
        let primitive_key_to_built_in = map.primitive_key_to_built_in();
        let variable_key_to_built_in = map.variable_key_to_built_in();
        let commands: GroupingHashMap<token::CsName, SerializableCommand> = map
            .commands
            .iter_all()
            .map(groupingmap::Item::adapt_map(|(u, command)| {
                let cs_name = token::CsName::try_from_usize(u).unwrap();
                if let Some(key) = PrimitiveKey::new(command) {
                    if let Some(built_in) = primitive_key_to_built_in.get(&key) {
                        return (cs_name, SerializableCommand::BuiltIn(*built_in));
                    }
                    // As a fallback, we can serialize static references into arrays when
                    // we've been provided with a reference to the array.
                    if let Command::Variable(variable_command) = command {
                        let key = variable::Key::new(variable_command.getters());
                        let built_in = variable_key_to_built_in.get(&key).unwrap();
                        if let Some(variable::IndexResolver::Static(index)) =
                            variable_command.index_resolver()
                        {
                            return (
                                cs_name,
                                SerializableCommand::VariableArrayStatic(*built_in, index.0),
                            );
                        }
                    }
                    // should error here
                }
                todo!();
            }))
            .collect();
        Self { commands }
    }

    pub(crate) fn finish_deserialization<S>(
        &self,
        initial_built_ins: HashMap<token::CsName, BuiltIn<S>>,
    ) -> Map<S> {
        let commands: GroupingVec<Command<S>> = self
            .commands
            .iter_all()
            .map(groupingmap::Item::adapt_map(
                |(cs_name, serialized_command): (token::CsName, &SerializableCommand)| {
                    let command = match serialized_command {
                        SerializableCommand::BuiltIn(cs_name) => {
                            initial_built_ins.get(cs_name).unwrap().cmd.clone()
                        }
                        SerializableCommand::VariableArrayStatic(cs_name, index) => {
                            match &initial_built_ins.get(cs_name).unwrap().cmd {
                                Command::Variable(variable_command) => {
                                    Command::Variable(std::rc::Rc::new(variable::Command::new(
                                        variable_command.getters().clone(),
                                        Some(variable::IndexResolver::Static(variable::Index(
                                            *index,
                                        ))),
                                    )))
                                }
                                _ => todo!(),
                            }
                        }
                    };
                    (cs_name.to_usize(), command)
                },
            ))
            .collect();
        Map {
            commands,
            initial_built_ins,
            primitive_key_to_built_in_lazy: Default::default(),
            variable_key_to_built_in_lazy: Default::default(),
        }
    }
}
