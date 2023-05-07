//! Map type
use super::*;
use std::collections::HashMap;
use std::fmt;
use texcraft_stdext::collections::groupingmap;
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
///   Maybe initialize the map using Map<csname, cmd> and `Vec<cmd>`. Can provide csname<->str wrapper at
///   the VM level
///
/// - While running, insert macros
///     insert_macro(&str, macro)
/// - While running, alias commands by current name or using the special ID inserts
///     alias_control_sequence(cs_name, cs_name) -> undefined control sequence error
///     alias_registered_built_in(cs_name, cmd_id, variable_addr)
///     alias_character(cs_name, token::Token)
pub struct Map<S> {
    commands: GroupingVec<command::Command<S>>,

    built_ins: HashMap<BuiltInKey, BuiltIn<S>>,
    key_to_built_in: HashMap<Key, BuiltInKey>,
    _getter_key_to_built_in: HashMap<variable::GetterKey, BuiltInKey>,
}

// TODO: move this into the mod.rs, make it public to the create, document what it's about,
// and add an actual new function not some indirect try_from thing.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Key {
    Execution(usize, Option<command::Tag>),
    Expansion(usize, Option<command::Tag>),
    // Maybe have a variable:Key
    VariableNoAddress(variable::GetterKey),
    VariableStaticAddress(variable::GetterKey, variable::Address),
    VariableDynamic(variable::GetterKey, usize),
}

impl<S> TryFrom<&BuiltIn<S>> for Key {
    type Error = ();

    fn try_from(value: &BuiltIn<S>) -> Result<Self, Self::Error> {
        (&value.cmd).try_into()
    }
}

impl<S> TryFrom<&command::Command<S>> for Key {
    type Error = ();

    fn try_from(value: &command::Command<S>) -> Result<Self, Self::Error> {
        match &value {
            Command::Expansion(f, tag) => Ok(Key::Expansion(*f as usize, *tag)),
            Command::Execution(f, tag) => Ok(Key::Execution(*f as usize, *tag)),
            Command::Variable(v) => {
                let v_id = v.getter_key();
                match v.address_spec() {
                    variable::AddressSpec::NoAddress => Ok(Key::VariableNoAddress(v_id)),
                    variable::AddressSpec::StaticAddress(a) => {
                        Ok(Key::VariableStaticAddress(v_id, *a))
                    }
                    variable::AddressSpec::Dynamic(f) => {
                        Ok(Key::VariableDynamic(v_id, *f as usize))
                    }
                    variable::AddressSpec::DynamicVirtual(_) => todo!(),
                }
            }
            Command::Macro(_) => Err(()),
            Command::Character(_) => Err(()),
        }
    }
}

impl TryFrom<Key> for variable::GetterKey {
    type Error = ();

    fn try_from(value: Key) -> Result<Self, Self::Error> {
        match value {
            Key::Execution(_, _) => Err(()),
            Key::Expansion(_, _) => Err(()),
            Key::VariableNoAddress(g) => Ok(g),
            Key::VariableStaticAddress(g, _) => Ok(g),
            Key::VariableDynamic(g, _) => Ok(g),
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum BuiltInKey {
    Initial(token::CsName),
    Additional(rc::Rc<str>),
}

impl<S> Map<S> {
    pub(crate) fn new(
        initial_built_ins: HashMap<CsName, BuiltIn<S>>,
        additional_built_ins: HashMap<rc::Rc<str>, BuiltIn<S>>,
    ) -> Map<S> {
        let mut commands: GroupingVec<command::Command<S>> = Default::default();
        let mut built_ins: HashMap<BuiltInKey, BuiltIn<S>> = Default::default();
        let mut key_to_built_in: HashMap<Key, BuiltInKey> = Default::default();
        let mut getter_key_to_built_in: HashMap<variable::GetterKey, BuiltInKey> =
            Default::default();

        let mut insert = |built_in: BuiltInKey, cmd: BuiltIn<S>| {
            if let Ok(cmd_key) = (&cmd).try_into() {
                key_to_built_in.insert(cmd_key, built_in.clone());
                if let Ok(getter_key) = cmd_key.try_into() {
                    getter_key_to_built_in.insert(getter_key, built_in.clone());
                }
            }
            built_ins.insert(built_in, cmd);
        };

        for (name, cmd) in initial_built_ins {
            let key = name.to_usize();
            commands.insert(key, cmd.cmd.clone(), groupingmap::Scope::Local);
            insert(BuiltInKey::Initial(name), cmd);
        }
        for (name, cmd) in additional_built_ins {
            insert(BuiltInKey::Additional(name), cmd);
        }
        Self {
            commands,
            built_ins,
            key_to_built_in,
            _getter_key_to_built_in: getter_key_to_built_in,
        }
    }

    #[inline]
    pub fn get_command(&self, name: &token::CsName) -> Option<&command::Command<S>> {
        self.commands.get(&name.to_usize())
    }

    pub fn get_tag(&self, name: &token::CsName) -> Option<command::Tag> {
        match self.commands.get(&name.to_usize()) {
            None => None,
            Some(cmd) => cmd.tag(),
        }
    }

    pub fn get_command_slow(&self, name: &token::CsName) -> Option<command::BuiltIn<S>> {
        let command = match self.get_command(name) {
            None => return None,
            Some(t) => t,
        };
        if let Ok(ref key) = command.try_into() {
            if let Some(built_in) = self.key_to_built_in.get(key) {
                return self.built_ins.get(built_in).cloned();
            }
        }
        Some(command::BuiltIn {
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
            command::Command::Variable(rc::Rc::new(variable_command)),
            scope,
        );
    }

    // TODO: reconsider this API of 4 setters ... it seems to be unneccessary complexity?
    pub fn insert_macro(
        &mut self,
        name: token::CsName,
        texmacro: texmacro::Macro,
        scope: groupingmap::Scope,
    ) {
        self.insert(name, command::Command::Macro(rc::Rc::new(texmacro)), scope);
    }

    pub fn alias_control_sequence(
        &mut self,
        alias: token::CsName,
        control_sequence: token::CsName,
        scope: groupingmap::Scope,
    ) -> Result<(), InvalidAlias> {
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
        self.insert(alias, command::Command::Character(token.value()), scope);
    }

    fn insert(
        &mut self,
        name: token::CsName,
        func: command::Command<S>,
        scope: groupingmap::Scope,
    ) {
        let key = name.to_usize();
        self.commands.insert(key, func, scope);
    }

    pub fn to_hash_map_slow(&self) -> HashMap<CsName, command::BuiltIn<S>> {
        let mut result = HashMap::new();
        for (key, _value) in self.commands.backing_container().iter().enumerate() {
            let cs_name = match CsName::try_from_usize(key) {
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

    pub(crate) fn end_group(&mut self) -> bool {
        self.commands.end_group()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        self.commands.len()
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
