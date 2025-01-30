//! Texlang variables API
//!
//! The documentation website contains
//!   [a tutorial specifically about Texlang's variables API](https://texcraft.dev/texlang/07-variables.html).
//! See that documentation for an overview of the API.
//! As usual, the documentation here is meant as reference.

use crate::command;
use crate::error;
use crate::parse::OptionalEquals;
use crate::prelude as txl;
use crate::token;
use crate::traits::*;
use crate::types;
use crate::vm;
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use texcraft_stdext::collections::groupingmap;

/// Function signature for a variable's immutable getter.
///
/// In Texcraft all [Variable]s are built from an immutable getter and a mutable getter.
/// This type alias just defines the signature of immutable getters.
/// The first argument is the state, and the second argument is the index of the variable.
pub type RefFn<S, T> = fn(state: &S, index: Index) -> &T;

/// Function signature for a variable's mutable getters.
///
/// In Texcraft all [Variable]s are built from an immutable getter and a mutable getter.
/// This type alias just defines the signature of mutable getters.
/// The first argument is the state, and the second argument is the index of the variable.
pub type MutRefFn<S, T> = fn(state: &mut S, index: Index) -> &mut T;

/// Index of a variable within an array.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Default)]
pub struct Index(pub usize);

impl From<usize> for Index {
    fn from(value: usize) -> Self {
        Index(value)
    }
}

/// Specification for how the index of an array variable is determined.
///
/// Obtaining a variable from a command involves determining the variable's [Index].
/// This index is ultimately passed into the variable's getters to get a reference or mutable reference
/// to the underlying Rust value.
pub enum IndexResolver<S> {
    /// A static index, provided in the enum variant.
    ///
    /// This resolver is used for commands that point to a specific entry in a array.
    /// For example, after executing `\countdef\A 30`, the `\A` control sequence points
    /// at the count register with index 30.
    /// The command backing `\A` uses a static resolver with index 30.
    Static(Index),
    /// A dynamic index that is determined by reading the input token stream.
    ///
    /// For example, in `\count 4` the index of `4` is determined by parsing a number
    /// from the input token stream.
    Dynamic(fn(token::Token, &mut vm::ExpandedStream<S>) -> txl::Result<Index>),
}

impl<S> IndexResolver<S> {
    /// Determine the index of a variable using the input token stream.
    fn resolve(
        &self,
        token: token::Token,
        input: &mut vm::ExpandedStream<S>,
    ) -> txl::Result<Index> {
        match self {
            IndexResolver::Static(addr) => Ok(*addr),
            IndexResolver::Dynamic(f) => f(token, input),
        }
    }
}

/// A TeX variable command.
///
/// Variable commands are _resolved_ to obtain a [Variable].
///
/// A command consists of three parts.
///
/// The first two parts are an immutable getter (of type [RefFn])
/// and a mutable getter (of type [MutRefFn]).
/// Both of these getters accept a reference to the state (where the variable's value lives)
/// and an [Index].
/// The index can be used by the getters to return different values in memory.
/// For example, if the getters read from an array, the index may be just the index in the array.
/// This mechanism allows users of Texlang to write one pair of getters that can be used in many variables.
///
/// The third part of a command is an [IndexResolver] that is used to determine the aforementioned index
/// of a variable at runtime.
/// The process of resolving a command involves determining the [Index] and returning a [Variable] type,
/// which under the hood is just the two getters and this index.
pub struct Command<S> {
    getters: Getters<S>,
    index_resolver: Option<IndexResolver<S>>,
}

impl<S> Command<S> {
    /// Create a new variable command.
    pub fn new_singleton<T: SupportedType>(
        ref_fn: RefFn<S, T>,
        ref_mut_fn: MutRefFn<S, T>,
    ) -> Command<S> {
        SupportedType::new_command(ref_fn, ref_mut_fn, None)
    }

    /// Create a new variable command.
    pub fn new_array<T: SupportedType>(
        ref_fn: RefFn<S, T>,
        ref_mut_fn: MutRefFn<S, T>,
        index_resolver: IndexResolver<S>,
    ) -> Command<S> {
        SupportedType::new_command(ref_fn, ref_mut_fn, Some(index_resolver))
    }

    /// Create a new getter provider.
    ///
    /// A getter provider is a variable command that is not intended to be invoked directly -
    ///     in fact, the variable command will panic the program if it is invoked.
    /// Instead the provider is included in a VM's initial commands so that
    ///     the VM has a reference to the getters inside the command.
    /// If a variable with the same getters is subsequently inserted into the commands map
    ///     (for example, by a `\countdef` type command), the VM can still serialize that
    ///     variable using the getters provided here.
    pub fn new_getter_provider<T: SupportedType>(
        ref_fn: RefFn<S, T>,
        ref_mut_fn: MutRefFn<S, T>,
    ) -> Command<S> {
        SupportedType::new_command(
            ref_fn,
            ref_mut_fn,
            Some(IndexResolver::Dynamic(|_, _| panic!())),
        )
    }

    /// Create a command that points to a specific element in the array referenced by this command.
    pub(crate) fn new_array_element(&self, index: Index) -> Self {
        Self {
            getters: self.getters.clone(),
            index_resolver: Some(IndexResolver::Static(index)),
        }
    }

    /// Returns true if the variable command corresponds to arithmetic variable(s) (e.g. integer or glue).
    ///
    /// TODO: we should just expose the type.
    pub fn is_arithmetic(&self) -> bool {
        match self.getters {
            Getters::Int(_, _) => true,
            Getters::CatCode(_, _) | Getters::MathCode(_, _) | Getters::TokenList(_, _) => false,
        }
    }
}

impl<S: TexlangState> Command<S> {
    /// Resolve the command to obtain a [Variable].
    pub fn resolve(
        &self,
        token: token::Token,
        input: &mut vm::ExpandedStream<S>,
    ) -> txl::Result<Variable<S>> {
        let index = match &self.index_resolver {
            None => Index(0),
            Some(index_resolver) => match index_resolver.resolve(token, input) {
                Ok(index) => index,
                Err(err) => {
                    return Err(error::Error::new_propagated(
                        input.vm(),
                        error::PropagationContext::VariableIndex,
                        token,
                        err,
                    ))
                }
            },
        };
        Ok(new_variable(&self.getters, index))
    }
}

impl<S> Command<S> {
    pub(crate) fn key(&self) -> CommandKey {
        let getters_key = self.getters.key();
        match &self.index_resolver {
            None => CommandKey::Singleton(getters_key),
            Some(index_resolver) => match index_resolver {
                IndexResolver::Static(a) => CommandKey::ArrayStatic(getters_key, *a),
                IndexResolver::Dynamic(f) => CommandKey::ArrayDynamic(getters_key, *f as usize),
            },
        }
    }
}

impl<S: TexlangState> Command<S> {
    /// Resolve the command to a variable and return the value of the variable.
    pub fn value<'a>(
        &self,
        token: token::Token,
        input: &'a mut vm::ExpandedStream<S>,
    ) -> txl::Result<ValueRef<'a>> {
        Ok(self.resolve(token, input)?.value(input.state()))
    }

    /// Resolve the command to a variable and set the value of the variable using the following tokens in the input stream.
    ///
    /// This function is used in TeX code like `\variable = 3`.
    /// In this case `\variable` is a command which resolves to a variable without consuming any more input.
    /// The variable is populated using the input `= 3` that follows.
    pub(crate) fn set_value_using_input(
        &self,
        token: token::Token,
        input: &mut vm::ExecutionInput<S>,
        scope: groupingmap::Scope,
    ) -> txl::Result<()> {
        match self
            .resolve(token, input.as_mut())?
            .set_value_using_input(input, scope)
        {
            Ok(()) => Ok(()),
            Err(err) => Err(error::Error::new_propagated(
                input.vm(),
                error::PropagationContext::VariableAssignment,
                token,
                err,
            )),
        }
    }
}

/// A key that uniquely identifies commands. If two commands have the same command key, they are the same command.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum CommandKey {
    Singleton(GettersKey),
    ArrayStatic(GettersKey, Index),
    ArrayDynamic(GettersKey, usize),
}

impl CommandKey {
    pub(crate) fn getter_key(&self) -> GettersKey {
        match self {
            CommandKey::Singleton(k) => *k,
            CommandKey::ArrayStatic(k, _) => *k,
            CommandKey::ArrayDynamic(k, _) => *k,
        }
    }
}

/// Immutable reference to the value of a variable.
pub enum ValueRef<'a> {
    Int(&'a i32),
    CatCode(&'a types::CatCode),
    MathCode(&'a types::MathCode),
    TokenList(&'a [token::Token]),
}

/// TeX variable of any type.
///
/// A variable uniquely identifies a Rust value in the state, like an `i32`.
/// Operations on this value (like reading or setting the value) can be done in two ways:
///
/// 1. (Easy, less flexible) Use the methods directly on this type like [Variable::value]
///     to read the value.
///     These methods are really ergonomic.
///     The problem with the value method specifically is that the result
///     is a reference which keeps the borrow of the state alive.
///     Thus, while holding onto the result of the value, you can't do anything this the
///     input stream like reading an argument.
///     This is especially a problem when you need to perform a different action depending on the concrete type of the variable.
///     
/// 2. (Trickier, more flexible) Match on the type's enum variants to determine the
///     concrete type of the variable.
///     The [TypedVariable] value obtained in this way can be used to perform operations on the value.
///     The main benefit of this approach is that after matching on the type, you can still use the input
///     stream to do things because there is not borrow alive.
///     
pub enum Variable<S> {
    Int(TypedVariable<S, i32>),
    CatCode(TypedVariable<S, types::CatCode>),
    MathCode(TypedVariable<S, types::MathCode>),
    TokenList(TypedVariable<S, Vec<token::Token>>),
}

/// A key that uniquely identifies the getters ([RefFn] and [MutRefFn]) in a command or variable.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct GettersKey(usize, usize);

/// A TeX variable of a specific Rust type `T`.
pub struct TypedVariable<S, T>(RefFn<S, T>, MutRefFn<S, T>, Index);

impl<S, T> Copy for TypedVariable<S, T> {}

impl<S, T> Clone for TypedVariable<S, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<S, T> TypedVariable<S, T> {
    /// Returns an immutable reference to the variable's value.
    pub fn get<'a>(&self, state: &'a S) -> &'a T {
        (self.0)(state, self.2)
    }

    fn key(&self) -> (usize, usize, Index) {
        (self.0 as usize, self.1 as usize, self.2)
    }
}

impl<S, T> TypedVariable<S, T>
where
    S: TexlangState,
    T: SupportedType,
{
    /// Sets the value of the variable.
    ///
    /// The input and scope must be passed because of TeX's grouping semantics.
    /// When the current group ends, any variable assignments made in the group
    ///     are rolled back.
    /// Thus this function generally saves the current value of the variable in the VM so that it
    ///     can be restored later.
    /// This is why the full input must be provided, and not just the state.
    pub fn set(&self, input: &mut vm::ExecutionInput<S>, scope: groupingmap::Scope, value: T) {
        let r: &mut T = (self.1)(input.state_mut(), self.2);
        let overwritten_value = std::mem::replace(r, value);
        // We guard the function call behind this conditional to make the current function small
        // to make it beneficial to inline it.
        if !input.groups().is_empty() {
            SupportedType::update_save_stack(input, self, scope, overwritten_value);
        } else {
            SupportedType::recycle(input, overwritten_value);
        }
    }
}
impl<S, T> TypedVariable<S, T>
where
    S: TexlangState,
    T: SupportedType + crate::parse::Parsable<S>,
{
    fn set_using_input(
        &self,
        input: &mut vm::ExecutionInput<S>,
        scope: groupingmap::Scope,
    ) -> txl::Result<()> {
        let (_, value) = <(OptionalEquals, T)>::parse(input)?;
        self.set(input, scope, value);
        Ok(())
    }
}

impl<S, T> PartialEq for TypedVariable<S, T> {
    fn eq(&self, rhs: &TypedVariable<S, T>) -> bool {
        self.key() == rhs.key()
    }
}

impl<S, T> Eq for TypedVariable<S, T> {}

impl<S, T> Hash for TypedVariable<S, T> {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.key().hash(state);
    }
}

/// Trait satisfied by all Rust types that can be used as TeX variables.
///
/// It exists to make the variables API more ergonomic.
/// For example, it is used to provide a uniform constructor [Command::new_array] for commands.
/// The trait cannot be implemented for new types.
pub trait SupportedType: Sized {
    /// Create a new command of this type with the provided reference functions and index resolver.
    fn new_command<S>(
        ref_fn: RefFn<S, Self>,
        ref_mut_fn: MutRefFn<S, Self>,
        index_resolver: Option<IndexResolver<S>>,
    ) -> Command<S>;

    /// Update the VM's save stack after a variable assignment.
    fn update_save_stack<S>(
        input: &mut vm::ExecutionInput<S>,
        variable: &TypedVariable<S, Self>,
        scope: groupingmap::Scope,
        overwritten_value: Self,
    );

    /// Recycle a value that's about to be dropped.
    ///
    /// This method is intended for token lists, in which case the vector is returned
    /// to the buffers pool.
    fn recycle<S>(input: &mut vm::ExecutionInput<S>, overwritten_value: Self) {
        (_, _) = (input, overwritten_value)
    }

    /// Create a new typed variable of this type from the getters in the command and the provided index.
    ///
    /// Return `None` if the command has a different type to `Self`.
    fn new_typed_variable<S>(command: &Command<S>, index: Index) -> Option<TypedVariable<S, Self>>;
}

/// This function is used to implement the [SupportedType::update_save_stack] method.
///
/// It's a bit janky to implement the logic as a free function, rather than, say, as
///     a default trait implementation.
/// The main problem is that the implementation calls into the save stack, which is an
///     internal VM data structure.
/// Any way that does this on the trait directly requires making some of the save stack
///     type public, because [SupportedType] is public.
fn update_save_stack<S, T: Clone + SupportedType, F>(
    input: &mut vm::ExecutionInput<S>,
    variable: &TypedVariable<S, T>,
    scope: groupingmap::Scope,
    overwritten_value: T,
    map_getter: F,
) where
    F: Fn(&mut SaveStackElement<S>) -> &mut SaveStackMap<S, T>,
{
    match scope {
        groupingmap::Scope::Global => {
            let n = input.groups().len();
            for _ in 0..n {
                let group = &mut input.groups()[0];
                if let Some(stale_value) = map_getter(group).remove(variable) {
                    SupportedType::recycle(input, stale_value);
                }
            }
        }
        groupingmap::Scope::Local => {
            if let Some((group, _)) = input.current_group_mut() {
                if let Some(stale_value) = map_getter(group).save(*variable, overwritten_value) {
                    SupportedType::recycle(input, stale_value);
                }
            }
        }
    }
}

macro_rules! supported_type_impl {
    ( $( ($type: path, $enum_variant: ident, $save_stack_field: ident $( , $recycle_fn: ident )? ), )+ ) => {
        fn new_variable<S>(getters: &Getters<S>, index: Index) -> Variable<S> {
            match getters {
                $(
                    Getters::$enum_variant(a, b) => Variable::$enum_variant(TypedVariable(*a, *b, index)),
                )+
            }
        }

        impl<S: TexlangState> Variable<S> {
            /// Return a reference to the value of the variable.
            pub fn value<'a>(&self, state: &'a S) -> ValueRef<'a> {
                match self {
                    $(
                        Variable::$enum_variant(variable) => ValueRef::$enum_variant(variable.get(state)),
                    )+
                }
            }

            /// Set the value of a variable using the following tokens in the input stream.
            fn set_value_using_input(
                &self,
                input: &mut vm::ExecutionInput<S>,
                scope: groupingmap::Scope,
            ) -> txl::Result<()> {
                match self {
                    $(
                        Variable::$enum_variant(variable) => variable.set_using_input(input, scope),
                    )+
                }
            }
        }

        enum Getters<S> {
            $(
                $enum_variant(RefFn<S, $type>, MutRefFn<S, $type>),
            )+
        }

        impl<S> Clone for Getters<S> {
            fn clone(&self) -> Self {
                match self {
                    $(
                        Self::$enum_variant(a, b) => Self::$enum_variant(*a, *b),
                    )+
                }
            }
        }

        impl<S> Getters<S> {
            fn key(&self) -> GettersKey {
                match self {
                    $(
                        Getters::$enum_variant(a, b) => GettersKey(*a as usize, *b as usize),
                    )+
                }
            }
        }

        $(
        impl SupportedType for $type {
            fn new_command<S>(
                ref_fn: RefFn<S, Self>,
                ref_mut_fn: MutRefFn<S, Self>,
                index_resolver: Option<IndexResolver<S>>,
            ) -> Command<S> {
                Command {
                    getters: Getters::$enum_variant(ref_fn, ref_mut_fn),
                    index_resolver,
                }
            }
            fn update_save_stack<S>(
                input: &mut vm::ExecutionInput<S>,
                variable: &TypedVariable<S, Self>,
                scope: groupingmap::Scope,
                overwritten_value: Self,
            ) {
                update_save_stack(input, variable, scope, overwritten_value, |element| {
                    &mut element.$save_stack_field
                })
            }
            $(
            fn recycle<S>(input: &mut vm::ExecutionInput<S>, overwritten_value: Self) {
                $recycle_fn(input, overwritten_value)
            }
            )?
            fn new_typed_variable<S>(
                command: &Command<S>,
                index: Index,
            ) -> Option<TypedVariable<S, Self>> {
                match command.getters {
                    Getters::$enum_variant(a, b) => Some(TypedVariable(a, b, index)),
                    _ => None,
                }
            }
        }
        )+

        /// Internal VM data structure used to implement TeX's grouping semantics.
        pub(crate) struct SaveStackElement<S> {
            $(
                $save_stack_field: SaveStackMap<S, $type>,
            )+
        }

        impl<S> Default for SaveStackElement<S> {
            fn default() -> Self {
                Self {
                    $(
                        $save_stack_field: Default::default(),
                    )+
                }
            }
        }

        impl<S> SaveStackElement<S> {
            pub(crate) fn restore(self, input: &mut vm::ExecutionInput<S>) {
                $(
                    self.$save_stack_field.restore(input);
                )+
            }

            pub(crate) fn serializable<'a>(
                &'a self,
                built_ins: &HashMap<GettersKey, token::CsName>,
            ) -> SerializableSaveStackElement<'a> {
                SerializableSaveStackElement {
                    $(
                        $save_stack_field: self.$save_stack_field.serializable(built_ins),
                    )+
                }
            }
        }

        #[cfg_attr(feature = "serde", derive(::serde::Serialize, ::serde::Deserialize))]
        pub(crate) struct SerializableSaveStackElement<'a> {
            $(
                $save_stack_field: Vec<(token::CsName, usize, Cow<'a, $type>)>,
            )+
        }

        impl<'a> SerializableSaveStackElement<'a> {
            pub(crate) fn finish_deserialization<S>(
                self,
                built_ins: &HashMap<token::CsName, command::BuiltIn<S>>,
            ) -> SaveStackElement<S> {
                SaveStackElement {
                    $(
                        $save_stack_field: SaveStackMap::from_deserialized(self.$save_stack_field, built_ins),
                    )+
                }
            }
        }
    };
}

supported_type_impl!(
    (i32, Int, i32),
    (types::CatCode, CatCode, catcode),
    (types::MathCode, MathCode, math_code),
    (Vec<token::Token>, TokenList, token_list, recycle_token_list),
);

fn recycle_token_list<S>(input: &mut vm::ExecutionInput<S>, overwritten_value: Vec<token::Token>) {
    input.return_token_buffer(overwritten_value);
}

/// Internal VM data structure used to implement TeX's grouping semantics.
struct SaveStackMap<S, T>(HashMap<TypedVariable<S, T>, T>);

impl<S, T> Default for SaveStackMap<S, T> {
    fn default() -> Self {
        Self(HashMap::new())
    }
}

impl<S, T: Clone + SupportedType> SaveStackMap<S, T> {
    fn save(&mut self, variable: TypedVariable<S, T>, value: T) -> Option<T> {
        match self.0.entry(variable) {
            std::collections::hash_map::Entry::Occupied(_) => Some(value),
            std::collections::hash_map::Entry::Vacant(v) => {
                v.insert(value);
                None
            }
        }
    }

    fn remove(&mut self, variable: &TypedVariable<S, T>) -> Option<T> {
        self.0.remove(variable)
    }

    fn restore(self, input: &mut vm::ExecutionInput<S>) {
        for (v, restored_value) in self.0 {
            let dest = (v.1)(input.state_mut(), v.2);
            let overwritten_value = std::mem::replace(dest, restored_value);
            SupportedType::recycle(input, overwritten_value);
        }
    }

    fn serializable<'a>(
        &'a self,
        built_ins: &HashMap<GettersKey, token::CsName>,
    ) -> Vec<(token::CsName, usize, Cow<'a, T>)> {
        self.0
            .iter()
            .map(|(typed_variable, value): (&TypedVariable<S, T>, &T)| {
                let key = GettersKey(typed_variable.0 as usize, typed_variable.1 as usize);
                let cs_name = built_ins.get(&key).unwrap();
                (*cs_name, typed_variable.2 .0, Cow::Borrowed(value))
            })
            .collect()
    }
}

impl<S, T: SupportedType + Clone> SaveStackMap<S, T> {
    fn from_deserialized<'a>(
        deserialized: Vec<(token::CsName, usize, Cow<'a, T>)>,
        built_ins: &HashMap<token::CsName, command::BuiltIn<S>>,
    ) -> Self {
        let m = deserialized
            .into_iter()
            .map(
                |(cs_name, index, value): (token::CsName, usize, Cow<'a, T>)| {
                    // TODO: surface error here
                    let built_in = built_ins.get(&cs_name).unwrap();
                    let typed_variable = match built_in.cmd() {
                        command::Command::Variable(variable_command) => {
                            // TODO: error instead of unwrap
                            SupportedType::new_typed_variable(variable_command, Index(index))
                                .unwrap()
                        }
                        _ => panic!("wrong type of built in TODO return an error here"),
                    };
                    (typed_variable, value.into_owned())
                },
            )
            .collect();
        Self(m)
    }
}
