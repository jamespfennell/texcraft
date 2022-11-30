//! Containers with a grouping concept such that mutations are rolled back at the end of each group.
//!
//! This module provides a wrapper type [GroupingContainer] that wraps associative containers
//! to give them a particular kind of grouping semantics.
//! It can wrap any type satisfying the [BackingContainer] trait.
//! In the wrapped container, a group is started and finished using the
//! [begin_group](GroupingContainer::begin_group) and
//! [end_group](GroupingContainer::end_group) methods.
//! The grouping semantics are: all mutations performed on the container
//!     during the group are rolled back at the end of the group.
//!
//! The module also provides implementations where the backing container is a
//! [HashMap] ([GroupingHashMap]) and a vector ([GroupingVec]).
//!
//! # Examples
//!
//! These examples all use the [GroupingHashMap] type.
//! The same semantics will apply to any wrapped container.
//!
//! The basic associative methods are the same as the standard hash map.
//! ```
//! # use texcraft_stdext::collections::groupingmap::GroupingHashMap;
//! let mut cat_colors = GroupingHashMap::new();
//! cat_colors.insert("mint", "ginger");
//! assert_eq!(cat_colors.get(&"mint"), Some(&"ginger"));
//! ```
//! The grouping methods are the main addition.
//! ```
//! # use texcraft_stdext::collections::groupingmap::GroupingHashMap;
//! let mut cat_colors = GroupingHashMap::new();
//!
//! // Insert a new value, update the value in a new group, and then end the group to roll back
//! // the update.
//! cat_colors.insert("paganini", "black");
//! cat_colors.begin_group();
//! cat_colors.insert("paganini", "gray");
//! assert_eq!(cat_colors.get(&"paganini"), Some(&"gray"));
//! assert_eq!(cat_colors.end_group(), true);
//! assert_eq!(cat_colors.get(&"paganini"), Some(&"black"));
//!
//! // Begin a new group, insert a value, and then end the group to roll back the insert.
//! cat_colors.begin_group();
//! cat_colors.insert("mint", "ginger");
//! assert_eq!(cat_colors.get(&"mint"), Some(&"ginger"));
//! assert_eq!(cat_colors.end_group(), true);
//! assert_eq!(cat_colors.get(&"mint"), None);
//! ```
//! The `end_group` method returns a boolean which is false if there is no group to end, and true
//! otherwise. It is generally an error to end a group that hasn't been started, so the method is
//! annoted with `#[must_use]`.
//! ```
//! # use texcraft_stdext::collections::groupingmap::GroupingHashMap;
//! let mut cat_colors = GroupingHashMap::<String, String>::new();
//! assert_eq!(cat_colors.end_group(), false);
//! ```
//! There is also a "global" variant of the `insert` method. It inserts the value at the global
//! group, and erases all other values.
//! ```
//! # use texcraft_stdext::collections::groupingmap::GroupingHashMap;
//! let mut cat_colors = GroupingHashMap::new();
//! cat_colors.insert("paganini", "black");
//! cat_colors.begin_group();
//! cat_colors.insert_global("paganini", "gray");
//! assert_eq!(cat_colors.end_group(), true);
//! assert_eq!(cat_colors.get(&"paganini"), Some(&"gray"));
//! ```
//!
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::hash::Hash;

/// Trait for containers that can be wrapped using [GroupingContainer].
pub trait BackingContainer<K, V>: Default {
    /// Set the value at the provided key.
    fn insert(&mut self, k: K, v: V);

    /// Get a reference to the value at the provided key, or `None` if the value doesn't exist.
    fn get(&self, k: &K) -> Option<&V>;

    /// Get mutable a reference to the value at the provided key, or `None` if the value doesn't exist.
    fn get_mut(&mut self, k: &K) -> Option<&mut V>;

    /// Remove a value with the provided key, if it exists.
    fn remove(&mut self, k: &K);

    /// Invoke the provided function for all (key, value) pairs in the map.
    fn visit<F: FnMut(&K, &V)>(&self, f: F);
}

impl<K: Eq + Hash + Clone, V> BackingContainer<K, V> for HashMap<K, V> {
    #[inline]
    fn insert(&mut self, k: K, v: V) {
        HashMap::insert(self, k, v);
    }
    #[inline]
    fn get(&self, k: &K) -> Option<&V> {
        HashMap::get(self, k)
    }
    #[inline]
    fn get_mut(&mut self, k: &K) -> Option<&mut V> {
        HashMap::get_mut(self, k)
    }
    #[inline]
    fn remove(&mut self, k: &K) {
        HashMap::remove(self, k);
    }
    fn visit<F: FnMut(&K, &V)>(&self, mut f: F) {
        for (k, v) in HashMap::iter(self) {
            f(k, v);
        }
    }
}

impl<V> BackingContainer<usize, V> for Vec<Option<V>> {
    #[inline]
    fn insert(&mut self, k: usize, v: V) {
        match <[Option<V>]>::get_mut(self, k) {
            None => {
                self.resize_with(k, || None);
                self.push(Some(v))
            }
            Some(elem) => {
                *elem = Some(v);
            }
        }
    }

    #[inline]
    fn get(&self, k: &usize) -> Option<&V> {
        match <[Option<V>]>::get(self, *k) {
            None => None,
            Some(v) => v.as_ref(),
        }
    }
    #[inline]
    fn get_mut(&mut self, k: &usize) -> Option<&mut V> {
        match <[Option<V>]>::get_mut(self, *k) {
            None => None,
            Some(v) => v.as_mut(),
        }
    }

    #[inline]
    fn remove(self: &mut Vec<Option<V>>, k: &usize) {
        if let Some(elem) = <[Option<V>]>::get_mut(self, *k) {
            *elem = None;
        }
    }

    fn visit<F: FnMut(&usize, &V)>(&self, mut f: F) {
        for (k, v) in <[Option<V>]>::iter(self).enumerate() {
            if let Some(v) = v {
                f(&k, v);
            }
        }
    }
}

/// A wrapper around [BackingContainer] types that adds a specific kind of group semantics.
///
/// See the module docs for more information.
#[derive(Debug, PartialEq)]
pub struct GroupingContainer<K: Eq + Hash + Clone, V, T: BackingContainer<K, V>> {
    backing_container: T,

    // The groups stack does not contain the global group as no cleanup there is needed.
    groups: Vec<HashMap<K, EndOfGroupAction<V>>>,
}

/// A grouping container based on the [HashMap] type.
pub type GroupingHashMap<K, V> = GroupingContainer<K, V, HashMap<K, V>>;

/// A grouping container based on the [Vec] type.
///
/// The vector is given map semantics with keys of type [usize], which are used as
/// indices for the vector.
/// When inserting an element at a key, the vector is extended if needed so that it can
/// hold an element with that index.
pub type GroupingVec<V> = GroupingContainer<usize, V, Vec<Option<V>>>;

#[derive(Debug, PartialEq)]
enum EndOfGroupAction<V> {
    Revert(V),
    Delete,
}

impl<K: Eq + Hash + Clone, V, T: BackingContainer<K, V>> GroupingContainer<K, V, T> {
    /// Inserts the key, value pair.
    pub fn insert(&mut self, key: K, mut val: V) -> bool {
        match (self.backing_container.get_mut(&key), self.groups.last_mut()) {
            (None, None) => {
                self.backing_container.insert(key, val);
                false
            }
            (None, Some(group)) => {
                group.insert(key.clone(), EndOfGroupAction::Delete);
                self.backing_container.insert(key, val);
                false
            }
            (Some(val_ref), None) => {
                *val_ref = val;
                true
            }
            (Some(val_ref), Some(group)) => {
                std::mem::swap(&mut val, val_ref);
                if let Entry::Vacant(vac) = group.entry(key) {
                    vac.insert(EndOfGroupAction::Revert(val));
                };
                true
            }
        }
    }

    /// Inserts the key, value pair in the global group.
    pub fn insert_global(&mut self, key: K, val: V) -> bool {
        for group in &mut self.groups {
            group.remove(&key);
        }
        match self.backing_container.get_mut(&key) {
            None => {
                self.backing_container.insert(key, val);
                false
            }
            Some(val_ref) => {
                *val_ref = val;
                true
            }
        }
    }

    /// Retrieves the value at the provided key.
    ///
    /// This exists for convenience only. It is equivalent to obtaining the
    /// backing container using [backing_container](GroupingContainer::backing_container)
    /// and calling the [get](BackingContainer::get) method.
    #[inline]
    pub fn get(&self, key: &K) -> Option<&V> {
        self.backing_container.get(key)
    }

    /// Begins a new group.
    pub fn begin_group(&mut self) {
        // Note that `HashSet::new()` is basically a free operation: no allocations will occur
        // until elements are inserted into it. So even if no mutations are made in this group, we
        // don't pay much for adding the set eagerly.
        self.groups.push(HashMap::new());
    }

    #[must_use]
    /// Attempts to end the current group and returns true if there is a group to end, and false
    /// otherwise.
    pub fn end_group(&mut self) -> bool {
        match self.groups.pop() {
            None => false,
            Some(group) => {
                // Note that for the running time analysis we account each iteration of this loop
                // to the insert method that put the key in the changed_keys set. Put another way,
                // this can be considered a defer or cleanup step for all of the insert calls
                // in the group that is being ended.
                for (key, value) in group.into_iter() {
                    match value {
                        EndOfGroupAction::Delete => {
                            self.backing_container.remove(&key);
                        }
                        EndOfGroupAction::Revert(old_val) => {
                            self.backing_container.insert(key, old_val);
                        }
                    }
                }
                true
            }
        }
    }

    /// Extends the `GroupingMap` with (key, value) pairs.
    /// ```
    /// # use texcraft_stdext::collections::groupingmap::GroupingHashMap;
    /// let mut cat_colors = GroupingHashMap::new();
    /// cat_colors.extend(std::array::IntoIter::new([
    ///    ("paganini", "black"),
    ///    ("mint", "ginger"),
    /// ]));
    /// assert_eq!(cat_colors.get(&"paganini"), Some(&"black"));
    /// assert_eq!(cat_colors.get(&"mint"), Some(&"ginger"));
    /// ```
    pub fn extend<I: IntoIterator<Item = (K, V)>>(&mut self, iter: I) {
        for (key, val) in iter {
            self.insert(key, val);
        }
    }

    /// Gets an immutable reference to the backing container.
    ///
    /// It is not possible to obtain a mutable reference to the backing container, as
    /// mutations applied through such a reference could not be rolled back.
    #[inline]
    pub fn backing_container(&self) -> &T {
        &self.backing_container
    }

    /// Returns a new empty `groupingMap`.
    pub fn new() -> GroupingContainer<K, V, T> {
        GroupingContainer {
            backing_container: Default::default(),
            groups: Vec::new(),
        }
    }

    /// Builds a new [GroupingContainer] which is clone of this grouping map except
    /// each value is converted using the provided convert function.
    pub fn clone_with_conversion<W, R: BackingContainer<K, W>>(
        &self,
        convert: &mut dyn FnMut(&V) -> W,
    ) -> GroupingContainer<K, W, R> {
        let mut m: GroupingContainer<K, W, R> = Default::default();
        self.backing_container
            .visit(|k: &K, v: &V| m.backing_container.insert(k.clone(), convert(v)));
        for group in &self.groups {
            let mut new_group: HashMap<K, EndOfGroupAction<W>> = Default::default();
            for (k, action) in group {
                new_group.insert(
                    k.clone(),
                    match action {
                        EndOfGroupAction::Revert(v) => EndOfGroupAction::Revert(convert(v)),
                        EndOfGroupAction::Delete => EndOfGroupAction::Delete,
                    },
                );
            }
            m.groups.push(new_group);
        }
        m
    }
}

impl<K: Eq + Hash + Clone, V, T: BackingContainer<K, V>> Default for GroupingContainer<K, V, T> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use crate::collections::groupingmap::*;

    #[test]
    fn insert_after_nested_insert() {
        let mut map = GroupingHashMap::new();
        map.begin_group();
        map.insert(3, 5);
        assert_eq!(map.end_group(), true);
        assert_eq!(map.get(&3), None);
        map.insert(3, 4);
        assert_eq!(map.get(&3), Some(&4));
    }

    #[test]
    fn insert_global_after_no_insert() {
        let mut map = GroupingHashMap::new();
        map.begin_group();
        map.insert_global(3, 5);
        assert_eq!(map.end_group(), true);
        assert_eq!(map.get(&3), Some(&5));
    }

    #[test]
    fn clone_with_conversion() {
        let mut original = GroupingHashMap::new();
        original.insert("key1", 1);
        original.begin_group();
        original.insert("key1", 2);
        original.insert("key2", 3);
        original.insert("key3", 4);
        original.begin_group();
        original.insert("key1", 5);
        original.insert_global("key3", 6);
        assert_eq!(original.end_group(), true);

        let mut want = GroupingHashMap::new();
        want.insert("key1", "1".to_string());
        want.begin_group();
        want.insert("key1", "2".to_string());
        want.insert("key2", "3".to_string());
        want.insert("key3", "4".to_string());
        want.begin_group();
        want.insert("key1", "5".to_string());
        want.insert_global("key3", "6".to_string());
        assert_eq!(want.end_group(), true);

        let got: GroupingHashMap<&'static str, String> =
            original.clone_with_conversion(&mut |i| format!["{}", i]);

        assert_eq!(got, want);
    }
}
