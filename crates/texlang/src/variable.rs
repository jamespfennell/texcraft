//! Texlang variables API
//! 
//! The documentation website contains
//!   [a tutorial specifically about Texlang's variables API](https://texcraft.dev/texlang/07-variables.html).
//! See that documentation for an overview of the API.
//! As usual, the documentation here is meant as reference.


use crate::error;
use crate::parse::OptionalEquals;
use crate::traits::*;
use crate::vm;
use crate::{token, token::CatCode};
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
    Dynamic(fn(token::Token, &mut vm::ExpandedStream<S>) -> Result<Index, Box<error::Error>>),
}

impl<S> IndexResolver<S> {
    /// Determine the index of a variable using the input token stream.
    fn resolve(
        &self,
        token: token::Token,
        input: &mut vm::ExpandedStream<S>,
    ) -> Result<Index, Box<error::Error>> {
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
    /// A getter provider is a variable command that is not intended to be invoked directly
    /// In fact, the variable command will panic the program if it is invoked.
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

    /// Create a new variable using the provided getters.
    pub(crate) fn new(getters: Getters<S>, index_resolver: Option<IndexResolver<S>>) -> Self {
        Self {
            getters,
            index_resolver,
        }
    }
}

impl<S: TexlangState> Command<S> {
    /// Resolve the command to obtain a [Variable].
    pub fn resolve(
        &self,
        token: token::Token,
        input: &mut vm::ExpandedStream<S>,
    ) -> Result<Variable<S>, Box<error::Error>> {
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
        Ok(match self.getters {
            Getters::Int(a, b) => Variable::Int(TypedVariable(a, b, index)),
            Getters::CatCode(a, b) => Variable::CatCode(TypedVariable(a, b, index)),
        })
    }
}
impl<S> Command<S> {
    pub(crate) fn getters(&self) -> &Getters<S> {
        &self.getters
    }

    pub(crate) fn index_resolver(&self) -> &Option<IndexResolver<S>> {
        &self.index_resolver
    }
}

impl<S: TexlangState> Command<S> {
    /// Resolve the command to a variable and return the value of the variable.
    pub fn value<'a>(
        &self,
        token: token::Token,
        input: &'a mut vm::ExpandedStream<S>,
    ) -> Result<ValueRef<'a>, Box<error::Error>> {
        Ok(self.resolve(token, input)?.value(input))
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
    ) -> Result<(), Box<error::Error>> {
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

/// Immutable reference to the value of a variable.
pub enum ValueRef<'a> {
    Int(&'a i32),
    CatCode(&'a CatCode),
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
    CatCode(TypedVariable<S, CatCode>),
}

impl<S: TexlangState> Variable<S> {
    /// Return a reference to the value of the variable.
    pub fn value<'a>(&self, input: &'a mut vm::ExpandedStream<S>) -> ValueRef<'a> {
        match self {
            Variable::Int(variable) => ValueRef::Int(variable.get(input.state())),
            Variable::CatCode(variable) => ValueRef::CatCode(variable.get(input.state())),
        }
    }

    /// Set the value of a variable using the following tokens in the input stream.
    fn set_value_using_input(
        &self,
        input: &mut vm::ExecutionInput<S>,
        scope: groupingmap::Scope,
    ) -> Result<(), Box<error::Error>> {
        OptionalEquals::parse(input)?;
        match self {
            Variable::Int(variable) => {
                let value = i32::parse(input)?;
                variable.set(input, scope, value);
            }
            Variable::CatCode(variable) => {
                let value = CatCode::parse(input)?;
                variable.set(input, scope, value);
            }
        };
        Ok(())
    }
}

pub(crate) enum Getters<S> {
    Int(RefFn<S, i32>, MutRefFn<S, i32>),
    CatCode(RefFn<S, CatCode>, MutRefFn<S, CatCode>),
}

impl<S> Clone for Getters<S> {
    fn clone(&self) -> Self {
        match self {
            Self::Int(a, b) => Self::Int(*a, *b),
            Self::CatCode(a, b) => Self::CatCode(*a, *b),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Key(usize, usize);

impl Key {
    pub(crate) fn new<S>(getters: &Getters<S>) -> Key {
        match getters {
            Getters::Int(a, b) => Key(*a as usize, *b as usize),
            Getters::CatCode(a, b) => Key(*a as usize, *b as usize),
        }
    }
}

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
        }
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
}

/// This function is used to implement the [SupportedType::update_save_stack] method.
///
/// It's a bit janky to implement the logic as a free function, rather than, say, as
///     a default trait implementation.
/// The main problem is that the implementation calls into the save stack, which is an
///     internal VM data structure.
/// Any way that does this on the trait directly requires making some of the save stack
///     type public, because [SupportedType] is public.
fn update_save_stack<S, T, F>(
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
            for group in input.groups() {
                map_getter(group).remove(variable)
            }
        }
        groupingmap::Scope::Local => {
            if let Some((group, _)) = input.current_group_mut() {
                map_getter(group).save(*variable, overwritten_value);
            }
        }
    }
}

impl SupportedType for i32 {
    fn new_command<S>(
        ref_fn: RefFn<S, Self>,
        ref_mut_fn: MutRefFn<S, Self>,
        index_resolver: Option<IndexResolver<S>>,
    ) -> Command<S> {
        Command {
            getters: Getters::Int(ref_fn, ref_mut_fn),
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
            &mut element.i32
        })
    }
}

impl SupportedType for CatCode {
    fn new_command<S>(
        ref_fn: RefFn<S, Self>,
        ref_mut_fn: MutRefFn<S, Self>,
        index_resolver: Option<IndexResolver<S>>,
    ) -> Command<S> {
        Command {
            getters: Getters::CatCode(ref_fn, ref_mut_fn),
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
            &mut element.catcode
        })
    }
}

/// Internal VM data structure used to implement TeX's grouping semantics.
pub(crate) struct SaveStackElement<S> {
    i32: SaveStackMap<S, i32>,
    catcode: SaveStackMap<S, CatCode>,
}

impl<S> Default for SaveStackElement<S> {
    fn default() -> Self {
        Self {
            i32: Default::default(),
            catcode: Default::default(),
        }
    }
}

impl<S> SaveStackElement<S> {
    pub fn restore(self, state: &mut S) {
        self.i32.restore(state);
        self.catcode.restore(state);
    }
}

/// Internal VM data structure used to implement TeX's grouping semantics.
pub(crate) struct SaveStackMap<S, T>(HashMap<TypedVariable<S, T>, T>);

impl<S, T> Default for SaveStackMap<S, T> {
    fn default() -> Self {
        Self(HashMap::new())
    }
}

impl<S, T> SaveStackMap<S, T> {
    pub(super) fn save(&mut self, variable: TypedVariable<S, T>, val: T) {
        self.0.entry(variable).or_insert(val);
    }

    pub(super) fn remove(&mut self, variable: &TypedVariable<S, T>) {
        self.0.remove(variable);
    }

    fn restore(self, state: &mut S) {
        for (v, restored_value) in self.0 {
            *(v.1)(state, v.2) = restored_value;
        }
    }
}
