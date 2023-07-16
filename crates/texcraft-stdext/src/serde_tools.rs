//! A collection of tools for working with serde.
use serde::{Deserialize, Serialize};

/// Module for serializing and deserializing fixed size arrays.
///
/// This is intended for use with serde derive's
/// `#[serde(with = "texcraft_stdext::serde_tools::array")]` field attribute.
pub mod array {
    use super::*;

    /// Function that serializes fixed size arrays
    pub fn serialize<T, S, const N: usize>(input: &[T; N], serializer: S) -> Result<S::Ok, S::Error>
    where
        T: serde::Serialize,
        S: serde::Serializer,
    {
        // TODO: we should not allocate a vector
        let v: Vec<&T> = input.iter().collect();
        v.serialize(serializer)
    }

    /// Function that deserializes fixed size arrays
    pub fn deserialize<'de, T, D, const N: usize>(deserializer: D) -> Result<[T; N], D::Error>
    where
        T: Deserialize<'de> + std::fmt::Debug,
        D: serde::Deserializer<'de>,
    {
        // TODO: we should not allocate a vector
        let v = Vec::<T>::deserialize(deserializer)?;
        // TODO: error instead of panic
        let a: [T; N] = v.try_into().unwrap();
        Ok(a)
    }
}

/// Module for serializing and deserializing iterables collections.
///
/// This is intended for use with serde derive's
/// `#[serde(with = "texcraft_stdext::serde_tools::iter")]` field attribute.
pub mod iter {
    use super::*;

    /// Function that serializes an iterable type.
    pub fn serialize<T, I, S>(input: I, serializer: S) -> Result<S::Ok, S::Error>
    where
        T: serde::Serialize,
        I: IntoIterator<Item = T>,
        S: serde::Serializer,
    {
        serializer.collect_seq::<I::IntoIter>(input.into_iter())
    }

    /// Function that deserializes types that can be built from iterators
    pub fn deserialize<'de, T, C, D>(deserializer: D) -> Result<C, D::Error>
    where
        T: Deserialize<'de> + std::fmt::Debug,
        C: FromIterator<T>,
        D: serde::Deserializer<'de>,
    {
        // TODO: we should not allocate a vector
        let v = Vec::<T>::deserialize(deserializer)?;
        Ok(C::from_iter(v.into_iter()))
    }
}

/// Function that serializes strings
///
/// This is intended for use with serde derive's
/// `#[serde(serialize_with = "path")]` field attribute.
pub fn serialize_str<T, S>(input: T, serializer: S) -> Result<S::Ok, S::Error>
where
    T: AsRef<str>,
    S: serde::Serializer,
{
    let s: &str = input.as_ref();
    s.serialize(serializer)
}

/// Function that deserializes reference counted constant strings
///
/// This is intended for use with serde derive's
/// `#[serde(deserialize_with = "path")]` field attribute.
pub fn deserialize_rc_str<'de, D>(deserializer: D) -> Result<std::rc::Rc<str>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    // TODO: should probably use Cow<> here to support not coping the string
    let s = String::deserialize(deserializer)?;
    Ok(s.into())
}
