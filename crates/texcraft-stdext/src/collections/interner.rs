//! String interning
//!
//! A string interner is a data structure that enables strings to be represented as integers
//! in a computer program.
//! Interning strings is often an optimization because only one copy of each distinct string is stored,
//! the string type is smaller and more cache friendly,
//! and string operations like comparisons are faster.
//! The cost of string interning (at least as implemented here) is that once a string is interned,
//! it is never deallocated.
//!
//! When using the [Interner] in this module, strings are interned using the [get_or_intern](Interner::get_or_intern) method.
//! This method returns a _key_.
//! If the same string is interned twice, the same key is returned.
//! The type of the key is fixed for each instance of the interner, and can be any
//! type that implements the [Key] trait.
//! By default the interner uses [std::num::NonZeroU32], which is a 32-bit integer.
//!
//! Given a key, the original string value can be recovered using the [resolve](Interner::resolve) method.
//!
//! ```
//! # use texcraft_stdext::collections::interner::Interner;
//! let mut interner: Interner = Default::default();
//! let hello_1 = interner.get_or_intern("hello");
//! let world_1 = interner.get_or_intern("world");
//! let hello_2 = interner.get_or_intern("hello");
//! assert_eq!(hello_1, hello_2);
//! assert_ne!(hello_1, world_1);
//!
//! assert_eq!(interner.resolve(hello_1), Some("hello"));
//! assert_eq!(interner.resolve(world_1), Some("world"));
//!
//! assert_eq!(interner.get("hello"), Some(hello_1));
//! assert_eq!(interner.get("other"), None);
//! ```
//!
//! The code in the interner is written from scratch, but all aspects of it
//! (the algorithm, the API, even some variable names) are based on Robin Freyler's
//! [string-interner](https://docs.rs/crate/string-interner/latest) crate.
//! For this reason the code is jointly copyrighted between Robin Freyler and the Texcraft contributors.
//!
//! ## The implementation
//!
//! The algorithm is based on the [string-interner](https://docs.rs/crate/string-interner/latest) crate.
//! This algorithm is also separately discovered and discussed in
//! [a post by Mat Klad](https://matklad.github.io/2020/03/22/fast-simple-rust-interner.html).
//!
//! The interner maintains a [String] buffer, and each time a new string is interned it's appended to the buffer.
//! A vector of indices is used to record the position of each string in the buffer.
//! When a new string is added to the buffer, the current length of the buffer (which is the end index
//! of the string in the buffer) is appended to the vector.
//! The key of the string is then the length of the vector when the index is appended.
//! Thus using the key, we can easily find the end index.
//!
//! To recover a string using its key, we get the end index from the vector.
//! We get the start index by getting the end index of the _previous_ string that was interned.
//! Given the process described above, the start index is stored in the vector just before the end index.
//! The recovered string is then the substring of the buffer between these two indices.
//!
//! This handles adding new strings.
//! A key property of the interner is that it also deduplicates strings.
//! The naive way to do this is to maintain a map from strings to keys, and first search
//! in this map for the string.
//! The problem with this approach is that it requires a costly second allocation
//!     of each interned string in this map.
//!
//! The string-interner crate and this module use different approaches to fix this.
//! In the crate, the map is keyed on the interned string's integer key.
//! When performing operations on the map, hash and equality of keys is based on the underlying string.
//!
//! In this module, the map is keyed on the [u64] hash of each string, which is computed outside of the map.
//! There is an edge case in which the hashes of two strings collide.
//! For this reason the value of the map is a linked list of all string keys that have the corresponding hash.
//! When checking if a string exists, we walk the linked list and check if the resolved string for each key
//!     matches.
//! If not, we intern the string and append to the linked list.
//! In the worst case this can result in O(n) lookups, but in reality hash collisions are rare.

use std::collections::hash_map;
use std::collections::HashMap;
use std::hash;
use std::num;

/// String interner.
///
/// See the module documentation for information about this data structure.
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct Interner<K = num::NonZeroU32, S = hash_map::RandomState> {
    buffer: String,
    ends: Vec<usize>,
    // When deserializing the interner, we reconstruct the deduplication map. We do this because the hash
    // builder in the deserialized interner will in general be different and so the keys of the map
    // will have changed. Additionally this is more efficient.
    #[cfg_attr(feature = "serde", serde(skip))]
    dedup: DeDupMap<K>,
    #[cfg_attr(feature = "serde", serde(skip))]
    hash_builder: S,
}

impl<K, S: Default> Default for Interner<K, S> {
    fn default() -> Self {
        Self {
            buffer: Default::default(),
            ends: Default::default(),
            dedup: Default::default(),
            hash_builder: Default::default(),
        }
    }
}

/// Types implementing this trait can be used as keys in the [Interner].
pub trait Key: Copy {
    /// Try to create a key from the provided [usize]. The first [usize]
    /// passed to this method will be 0; the second 1; and so on.
    ///
    /// This method is more or less the same as the well-known [`TryFrom<usize>`] trait.
    /// We use a custom trait so that consumers don't have to implement the well-known trait.
    fn try_from_usize(index: usize) -> Option<Self>;
    /// Convert the key into a [usize].
    ///
    /// This method is more or less the same as the well-known [`Into<usize>`] trait.
    /// We use a custom trait so that consumers don't have to implement the well-known trait.
    fn into_usize(self) -> usize;
}

impl Key for num::NonZeroU32 {
    fn try_from_usize(index: usize) -> Option<Self> {
        let u32: u32 = match index.try_into() {
            Ok(u32) => u32,
            Err(_) => return None,
        };
        num::NonZeroU32::new(u32 + 1)
    }

    fn into_usize(self) -> usize {
        self.get() as usize
    }
}

impl<K: Key, S: hash::BuildHasher> Interner<K, S> {
    /// Intern the provided string and return its key.
    pub fn get_or_intern(&mut self, s: &str) -> K {
        // First we check if the string has already been interned.
        let hash = hash(&self.hash_builder, s);
        if let Some(key) = self.get_internal(s, hash) {
            return key;
        }

        // If the string hasn't been interned, we now intern it.
        let key = K::try_from_usize(self.ends.len()).unwrap();
        self.buffer.push_str(s);
        let end = self.buffer.len();
        self.ends.push(end);
        populate_dedup_map(&mut self.dedup, hash, key);
        key
    }

    /// Get the key for the provided string if it has been already been interned.
    ///
    /// This method is useful when the caller only has a shared reference to the interner.
    pub fn get(&self, s: &str) -> Option<K> {
        self.get_internal(s, hash(&self.hash_builder, s))
    }

    fn get_internal(&self, s: &str, hash: u64) -> Option<K> {
        let mut node_or = self.dedup.get(&hash);
        while let Some(node) = node_or {
            if self.resolve(node.key).unwrap() == s {
                return Some(node.key);
            }
            node_or = match &node.next {
                None => None,
                Some(node) => Some(node),
            };
        }
        None
    }

    /// Return the interned string corresponding to the provided key.
    pub fn resolve(&self, k: K) -> Option<&str> {
        let i = k.into_usize().wrapping_sub(1);
        let start = match i.checked_sub(1) {
            None => 0,
            Some(prev_k) => match self.ends.get(prev_k) {
                None => return None,
                Some(start) => *start,
            },
        };
        let end = match self.ends.get(i) {
            None => return None,
            Some(end) => *end,
        };
        Some(&self.buffer[start..end])
    }
}

fn hash<S: hash::BuildHasher>(hash_builder: &S, s: &str) -> u64 {
    hash_builder.hash_one(s)
}

type DeDupMap<K> = HashMap<u64, LinkedList<K>, hash::BuildHasherDefault<SingleU64Hasher>>;

fn populate_dedup_map<K>(map: &mut DeDupMap<K>, hash: u64, key: K) {
    match map.entry(hash) {
        hash_map::Entry::Occupied(mut o) => {
            let first = o.get_mut();
            let second = std::mem::replace(first, LinkedList { key, next: None });
            first.next = Some(Box::new(second));
        }
        hash_map::Entry::Vacant(v) => {
            v.insert(LinkedList { key, next: None });
        }
    };
}

struct LinkedList<K> {
    key: K,
    next: Option<Box<LinkedList<K>>>,
}

/// A hasher that can only hash a single [u64] value, and whose result is simply the [u64] value.
///
/// This hasher is used to make the hashing in the interner's deduplication map a no-op.
/// We use this hasher because the [u64] key for the map is already a hash (of a string),
/// and hashing the value again is wasteful.
///
/// The implementation of this hasher uses safe Rust and performs at least two panic-able checks
/// on the hot path of hashing the value.
/// However when compiled, the entire hasher is completely optimized out, and the
/// hashing function inside the hash map becomes the identity function for the [u64] type.
#[derive(Default)]
struct SingleU64Hasher {
    val: Option<u64>,
}

impl hash::Hasher for SingleU64Hasher {
    #[inline]
    fn finish(&self) -> u64 {
        self.val.unwrap()
    }

    fn write(&mut self, _: &[u8]) {
        panic!("this hasher does not support writing arbitrary bytes, only a single u64 value")
    }

    #[inline]
    fn write_u64(&mut self, i: u64) {
        if self.val.is_some() {
            panic!("this hasher does not support writing multiple u64 values")
        }
        self.val = Some(i)
    }
}

#[cfg(feature = "serde")]
impl<'de, K: Key, S: Default + hash::BuildHasher> serde::Deserialize<'de> for Interner<K, S> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(serde::Deserialize)]
        struct DeserializedInterner {
            buffer: String,
            ends: Vec<usize>,
        }

        let DeserializedInterner { buffer, ends } =
            DeserializedInterner::deserialize(deserializer)?;
        let hash_builder = S::default();
        let mut dedup = DeDupMap::<K>::default();
        dedup.reserve(ends.len());

        let mut start: usize = 0;
        for (i, end) in ends.iter().enumerate() {
            let s = &buffer[start..*end];
            let hash = hash(&hash_builder, s);
            let key = K::try_from_usize(i).unwrap();
            populate_dedup_map(&mut dedup, hash, key);
            start = *end;
        }
        Ok(Self {
            buffer,
            ends,
            dedup,
            hash_builder,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A hasher that always returns the same fixed value.
    /// This is use to test hash collisions.
    #[derive(Default)]
    struct FixedHasher;

    impl hash::Hasher for FixedHasher {
        fn finish(&self) -> u64 {
            12
        }

        fn write(&mut self, _: &[u8]) {}
    }

    #[test]
    fn test_hash_collision() {
        let mut interner: Interner<num::NonZeroU32, hash::BuildHasherDefault<FixedHasher>> =
            Default::default();
        let hello_1 = interner.get_or_intern("hello");
        let world_1 = interner.get_or_intern("world");
        let hello_2 = interner.get_or_intern("hello");
        assert_eq!(hello_1, hello_2);
        assert_ne!(hello_1, world_1);

        assert_eq!(interner.resolve(hello_1), Some("hello"));
        assert_eq!(interner.resolve(world_1), Some("world"));
    }

    #[cfg(feature = "serde")]
    #[test]
    fn test_serde() {
        let mut interner: Interner = Default::default();
        let hello_1 = interner.get_or_intern("hello");
        let world_1 = interner.get_or_intern("world");

        let serialized = serde_json::to_string_pretty(&interner).unwrap();
        let mut interner_de: Interner = serde_json::from_str(&serialized).unwrap();
        let hello_2 = interner_de.get_or_intern("hello");
        let world_2 = interner_de.get_or_intern("world");

        assert_eq!(hello_1, hello_2);
        assert_eq!(world_1, world_2);
    }
}
