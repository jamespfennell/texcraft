//! Map type
use super::*;
use std::borrow::Cow;
use std::cell::{Ref, RefCell};
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
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
    active_char: GroupingHashMap<char, Command<S>>,

    built_in_commands: HashMap<token::CsName, BuiltIn<S>>,
    primitive_key_to_built_in_lazy: RefCell<Option<HashMap<PrimitiveKey, token::CsName>>>,
    getters_key_to_built_in_lazy: RefCell<Option<HashMap<variable::GettersKey, token::CsName>>>,
}

impl<S> Map<S> {
    pub(crate) fn new(built_in_commands: HashMap<token::CsName, BuiltIn<S>>) -> Map<S> {
        Self {
            commands: built_in_commands
                .iter()
                .map(|(k, v)| (k.to_usize(), v.cmd.clone()))
                .collect(),
            active_char: Default::default(),
            built_in_commands,
            primitive_key_to_built_in_lazy: Default::default(),
            getters_key_to_built_in_lazy: Default::default(),
        }
    }

    #[inline]
    pub fn get_command(&self, command_ref: &token::CommandRef) -> Option<&Command<S>> {
        match command_ref {
            token::CommandRef::ControlSequence(name) => self.commands.get(&name.to_usize()),
            token::CommandRef::ActiveCharacter(c) => self.active_char.get(c),
        }
    }

    pub fn get_tag(&self, command_ref: &token::CommandRef) -> Option<Tag> {
        let command = match command_ref {
            token::CommandRef::ControlSequence(name) => self.commands.get(&name.to_usize()),
            token::CommandRef::ActiveCharacter(c) => self.active_char.get(c),
        };
        command.map(Command::tag).unwrap_or(None)
    }

    pub fn built_in_commands(&self) -> &HashMap<token::CsName, BuiltIn<S>> {
        &self.built_in_commands
    }

    pub fn get_command_slow(&self, command_ref: &token::CommandRef) -> Option<BuiltIn<S>> {
        let command = match self.get_command(command_ref) {
            None => return None,
            Some(t) => t,
        };
        if let Some(ref key) = PrimitiveKey::new(command) {
            if let Some(built_in) = self.primitive_key_to_built_in().get(key) {
                return self.built_in_commands.get(built_in).cloned();
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
        command_ref: token::CommandRef,
        variable_command: variable::Command<S>,
        scope: groupingmap::Scope,
    ) {
        self.insert(
            command_ref,
            Command::Variable(rc::Rc::new(variable_command)),
            scope,
        );
    }

    // TODO: reconsider this API of 4 setters ... it seems to be unnecessary complexity?
    pub fn insert_macro(
        &mut self,
        name: token::CommandRef,
        texmacro: texmacro::Macro,
        scope: groupingmap::Scope,
    ) {
        self.insert(name, Command::Macro(rc::Rc::new(texmacro)), scope);
    }

    pub fn alias_control_sequence(
        &mut self,
        alias: token::CommandRef,
        command: &token::CommandRef,
        scope: groupingmap::Scope,
    ) -> std::result::Result<(), InvalidAlias> {
        let command = match self.get_command(command) {
            None => return Err(InvalidAlias {}),
            Some(t) => t,
        };
        self.insert(alias, command.clone(), scope);
        Ok(())
    }

    pub fn alias_token(
        &mut self,
        alias: token::CommandRef,
        token: token::Token,
        scope: groupingmap::Scope,
    ) {
        self.insert(alias, Command::CharacterTokenAlias(token.value()), scope);
    }

    pub fn insert(
        &mut self,
        command_ref: token::CommandRef,
        func: Command<S>,
        scope: groupingmap::Scope,
    ) {
        match command_ref {
            token::CommandRef::ControlSequence(name) => {
                let key = name.to_usize();
                self.commands.insert(key, func, scope);
            }
            token::CommandRef::ActiveCharacter(c) => {
                self.active_char.insert(c, func, scope);
            }
        }
    }

    // TODO: support active characters
    pub fn to_hash_map_slow(&self) -> HashMap<token::CsName, BuiltIn<S>> {
        let mut result = HashMap::new();
        for (key, _value) in self.commands.backing_container().iter().enumerate() {
            let cs_name = match token::CsName::try_from_usize(key) {
                None => continue,
                Some(cs_name) => cs_name,
            };
            let command_ref = token::CommandRef::ControlSequence(cs_name);
            let cmd = match self.get_command_slow(&command_ref) {
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
            self.built_in_commands
                .iter()
                .filter_map(|(cs_name, built_in)| {
                    PrimitiveKey::new(built_in.cmd()).map(|key| (key, *cs_name))
                })
                .collect(),
        );
        self.primitive_key_to_built_in()
    }

    pub(crate) fn getters_key_to_built_in(
        &self,
    ) -> Ref<'_, HashMap<variable::GettersKey, token::CsName>> {
        if let Ok(r) = Ref::filter_map(self.getters_key_to_built_in_lazy.borrow(), Option::as_ref) {
            return r;
        }
        *self.getters_key_to_built_in_lazy.borrow_mut() = Some(
            self.primitive_key_to_built_in()
                .iter()
                .filter_map(|(key, cs_name)| match key {
                    PrimitiveKey::Variable(variable_command_key) => {
                        Some((variable_command_key.getter_key(), *cs_name))
                    }
                    _ => None,
                })
                .collect(),
        );
        self.getters_key_to_built_in()
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
    Macro(usize),
    CharacterTokenAlias(token::Value),
    Character(char),
    MathCharacter(types::MathCode),
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub(crate) struct SerializableMap<'a> {
    // TODO: instead of serializing the map we could just serialize the iterator.
    // This would be more efficient on the deserialization path.
    // TODO: maybe the map should also serde itself using the iterator.
    commands: GroupingHashMap<token::CsName, SerializableCommand>,
    macros: Vec<Cow<'a, texmacro::Macro>>,
}

#[cfg(feature = "serde")]
impl<State> serde::Serialize for Map<State> {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let serializable_map = SerializableMap::new(self);
        serializable_map.serialize(serializer)
    }
}

impl<'a> SerializableMap<'a> {
    fn new<S>(map: &'a Map<S>) -> Self {
        let mut macros_de_dup = HashMap::<usize, usize>::new();
        let mut macros: Vec<Cow<'a, texmacro::Macro>> = Default::default();
        let primitive_key_to_built_in = map.primitive_key_to_built_in();
        let getters_key_to_built_in = map.getters_key_to_built_in();
        let commands: GroupingHashMap<token::CsName, SerializableCommand> = map
            .commands
            .iter_all()
            .map(groupingmap::Item::adapt_map(
                |(u, command): (usize, &Command<S>)| {
                    let command: SerializableCommand = match command {
                        Command::Expansion(_, _) | Command::Execution(_, _) => {
                            let key = PrimitiveKey::new(command).unwrap();
                            match primitive_key_to_built_in.get(&key) {
                                None => todo!("return an error"),
                                Some(built_in) => SerializableCommand::BuiltIn(*built_in),
                            }
                        }
                        Command::Variable(variable_command) => {
                            let key = PrimitiveKey::new(command).unwrap();
                            if let Some(built_in) = primitive_key_to_built_in.get(&key) {
                                SerializableCommand::BuiltIn(*built_in)
                            } else {
                                // As a fallback, we can serialize static references into arrays when
                                // we've been provided with a way to reference the array.
                                match variable_command.key() {
                                    variable::CommandKey::ArrayStatic(getters_key, index) => {
                                        let built_in =
                                            getters_key_to_built_in.get(&getters_key).unwrap();
                                        SerializableCommand::VariableArrayStatic(*built_in, index.0)
                                    }
                                    _ => todo!(),
                                }
                            }
                        }
                        Command::Macro(tex_macro) => {
                            let rc_addr = Rc::as_ptr(tex_macro) as usize;
                            let u = *macros_de_dup.entry(rc_addr).or_insert_with(|| {
                                let u = macros.len();
                                macros.push(Cow::Borrowed(tex_macro));
                                u
                            });
                            SerializableCommand::Macro(u)
                        }
                        Command::CharacterTokenAlias(v) => {
                            SerializableCommand::CharacterTokenAlias(*v)
                        }
                        Command::Character(c) => SerializableCommand::Character(*c),
                        Command::MathCharacter(c) => SerializableCommand::MathCharacter(*c),
                    };

                    let cs_name = token::CsName::try_from_usize(u).unwrap();
                    (cs_name, command)
                },
            ))
            .collect();
        Self { commands, macros }
    }

    pub(crate) fn finish_deserialization<S>(
        self,
        built_in_commands: HashMap<token::CsName, BuiltIn<S>>,
        interner: &token::CsNameInterner,
    ) -> Map<S> {
        let macros: Vec<Rc<texmacro::Macro>> = self
            .macros
            .into_iter()
            .map(std::borrow::Cow::into_owned)
            .map(Rc::new)
            .collect();
        let commands: GroupingVec<Command<S>> = self
            .commands
            .iter_all()
            .map(groupingmap::Item::adapt_map(
                |(cs_name, serialized_command): (token::CsName, &SerializableCommand)| {
                    let command = match serialized_command {
                        SerializableCommand::BuiltIn(cs_name) => {
                            match built_in_commands.get(cs_name) {
                                None => {
                                    panic!(
                                        "unknown control sequence {:?}",
                                        interner.resolve(*cs_name)
                                    )
                                }
                                Some(cmd) => cmd.cmd.clone(),
                            }
                        }
                        SerializableCommand::VariableArrayStatic(cs_name, index) => {
                            match &built_in_commands.get(cs_name).unwrap().cmd {
                                Command::Variable(variable_command) => {
                                    Command::Variable(std::rc::Rc::new(
                                        variable_command.new_array_element(variable::Index(*index)),
                                    ))
                                }
                                _ => todo!(),
                            }
                        }
                        SerializableCommand::Macro(u) => {
                            // TODO: error handling if the macro is missing
                            Command::Macro(macros.get(*u).unwrap().clone())
                        }
                        SerializableCommand::CharacterTokenAlias(v) => {
                            Command::CharacterTokenAlias(*v)
                        }
                        SerializableCommand::Character(c) => Command::Character(*c),
                        SerializableCommand::MathCharacter(c) => Command::MathCharacter(*c),
                    };
                    (cs_name.to_usize(), command)
                },
            ))
            .collect();
        Map {
            commands,
            active_char: Default::default(), // TODO
            built_in_commands,
            primitive_key_to_built_in_lazy: Default::default(),
            getters_key_to_built_in_lazy: Default::default(),
        }
    }
}
