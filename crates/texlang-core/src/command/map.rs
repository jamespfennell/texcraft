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
    funcs: GroupingVec<command::Fn<S>>,
    // TODO: the tags map is not optimal because the tags are stored in memory as Option<Option<Tag>>
    // which takes up 4 words instead of Option<Tag> which takes up 2 words. We should fix this.
    tags: GroupingVec<Option<command::Tag>>,

    built_ins: HashMap<BuiltIn, Command<S>>,
    key_to_built_in: HashMap<Key, BuiltIn>,
    _getter_key_to_built_in: HashMap<variable::GetterKey, BuiltIn>,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Key {
    Execution(usize, Option<command::Tag>),
    Expansion(usize, Option<command::Tag>),
    VariableNoAddress(variable::GetterKey),
    VariableStaticAddress(variable::GetterKey, variable::Address),
    VariableDynamic(variable::GetterKey, usize),
}

impl<S> TryFrom<&Command<S>> for Key {
    type Error = ();

    fn try_from(value: &Command<S>) -> Result<Self, Self::Error> {
        (&value.func, value.tag).try_into()
    }
}

impl<S> TryFrom<(&command::Fn<S>, Option<command::Tag>)> for Key {
    type Error = ();

    fn try_from(value: (&command::Fn<S>, Option<command::Tag>)) -> Result<Self, Self::Error> {
        match &value.0 {
            Fn::Expansion(f) => Ok(Key::Expansion(*f as usize, value.1)),
            Fn::Execution(f) => Ok(Key::Execution(*f as usize, value.1)),
            Fn::Variable(v) => {
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
            Fn::Macro(_) => Err(()),
            Fn::Character(_) => Err(()),
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
enum BuiltIn {
    Initial(token::CsName),
    Additional(rc::Rc<str>),
}

impl<S> Map<S> {
    pub(crate) fn new(
        initial_built_ins: HashMap<CsName, Command<S>>,
        additional_built_ins: HashMap<rc::Rc<str>, Command<S>>,
    ) -> Map<S> {
        let mut funcs: GroupingVec<command::Fn<S>> = Default::default();
        let mut tags: GroupingVec<Option<command::Tag>> = Default::default();
        let mut built_ins: HashMap<BuiltIn, Command<S>> = Default::default();
        let mut key_to_built_in: HashMap<Key, BuiltIn> = Default::default();
        let mut getter_key_to_built_in: HashMap<variable::GetterKey, BuiltIn> = Default::default();

        let mut insert = |built_in: BuiltIn, cmd: Command<S>| {
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
            funcs.insert(key, cmd.func.clone(), groupingmap::Scope::Local);
            tags.insert(key, cmd.tag, groupingmap::Scope::Local);
            insert(BuiltIn::Initial(name), cmd);
        }
        for (name, cmd) in additional_built_ins {
            insert(BuiltIn::Additional(name), cmd);
        }
        Self {
            funcs,
            tags,
            built_ins,
            key_to_built_in,
            _getter_key_to_built_in: getter_key_to_built_in,
        }
    }

    #[inline]
    pub fn get_fn(&self, name: &token::CsName) -> Option<&command::Fn<S>> {
        self.funcs.get(&name.to_usize())
    }

    pub fn get_tag(&self, name: &token::CsName) -> Option<command::Tag> {
        match self.tags.get(&name.to_usize()) {
            None => None,
            Some(tag) => *tag,
        }
    }

    pub fn get_command_slow(&self, name: &token::CsName) -> Option<command::Command<S>> {
        let (func, tag) = match self.get_all(name) {
            None => return None,
            Some(t) => t,
        };
        if let Ok(ref key) = (func, tag).try_into() {
            if let Some(built_in) = self.key_to_built_in.get(key) {
                return self.built_ins.get(built_in).cloned();
            }
        }
        Some(command::Command {
            func: func.clone(),
            tag,
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
            command::Fn::Variable(rc::Rc::new(variable_command)),
            None,
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
        self.insert(name, command::Fn::Macro(rc::Rc::new(texmacro)), None, scope);
    }

    pub fn alias_control_sequence(
        &mut self,
        alias: token::CsName,
        control_sequence: token::CsName,
        scope: groupingmap::Scope,
    ) -> Result<(), InvalidAlias> {
        let (func, tag) = match self.get_all(&control_sequence) {
            None => return Err(InvalidAlias {}),
            Some(t) => t,
        };
        self.insert(alias, func.clone(), tag, scope);
        Ok(())
    }

    pub fn alias_token(
        &mut self,
        alias: token::CsName,
        token: token::Token,
        scope: groupingmap::Scope,
    ) {
        self.insert(alias, command::Fn::Character(token.value()), None, scope);
    }

    fn insert(
        &mut self,
        name: token::CsName,
        func: command::Fn<S>,
        tag: Option<Tag>,
        scope: groupingmap::Scope,
    ) {
        let key = name.to_usize();
        self.funcs.insert(key, func, scope);
        self.tags.insert(key, tag, scope);
    }

    pub fn to_hash_map_slow(&self) -> HashMap<CsName, command::Command<S>> {
        let mut result = HashMap::new();
        for (key, _value) in self.funcs.backing_container().iter().enumerate() {
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
        self.funcs.begin_group();
        self.tags.begin_group();
    }

    pub(crate) fn end_group(&mut self) -> bool {
        self.funcs.end_group() && self.tags.end_group()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        self.funcs.len()
    }

    fn get_all(&self, cs_name: &token::CsName) -> Option<(&command::Fn<S>, Option<command::Tag>)> {
        let key = cs_name.to_usize();
        let func = match self.funcs.get(&key) {
            None => return None,
            Some(func) => func,
        };
        let tag = match self.tags.get(&key) {
            None => None,
            Some(tag) => *tag,
        };
        Some((func, tag))
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
