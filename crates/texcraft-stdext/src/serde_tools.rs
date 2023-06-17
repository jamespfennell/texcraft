//! A collection of tools for working with serde.
use serde::{Deserialize, Serialize};

/// Function that serializes fixed size arrays
///
/// This is intended for use with serde derive's
/// `#[serde(serialize_with = "path")]` field attribute.
pub fn serialize_array<T: serde::Serialize, S, const N: usize>(
    input: &[T; N],
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    let v: Vec<&T> = input.iter().collect();
    v.serialize(serializer)
}

/// Function that deserializes fixed size arrays
///
/// This is intended for use with serde derive's
/// `#[serde(deserialize_with = "path")]` field attribute.
pub fn deserialize_array<'de, T: Deserialize<'de> + std::fmt::Debug, D, const N: usize>(
    deserializer: D,
) -> Result<[T; N], D::Error>
where
    D: serde::Deserializer<'de>,
{
    let v = Vec::<T>::deserialize(deserializer)?;
    let a: [T; N] = v.try_into().unwrap();
    Ok(a)
}

/// Function that serializes strings
///
/// This is intended for use with serde derive's
/// `#[serde(serialize_with = "path")]` field attribute.
pub fn serialize_str<T: AsRef<str>, S>(input: T, serializer: S) -> Result<S::Ok, S::Error>
where
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
