//! A hash map with the concept of scope such that mutations are rolled back at the end of the scope.
//!
//! # Examples
//!
//! The map methods are the same as the standard hash map (although only a few methods are
//! implemented).
//! ```
//! # use texcraft::datastructures::scopedmap::ScopedMap;
//! let mut cat_colors = ScopedMap::new();
//! cat_colors.insert("mint", "ginger");
//! assert_eq!(cat_colors.get(&"mint"), Some(&"ginger"));
//! ```
//! The scoped map additionally has `begin_scope` and `end_scope` methods along with the following
//! behavior: when a scope is ended, all mutations to the map since the beginning of the scope are
//! rolled back.
//! ```
//! # use texcraft::datastructures::scopedmap::ScopedMap;
//! let mut cat_colors = ScopedMap::new();
//!
//! // Insert a new value, update the value in a new scope, and then end the scope to roll back
//! // the update.
//! cat_colors.insert("paganini", "black");
//! cat_colors.begin_scope();
//! cat_colors.insert("paganini", "gray");
//! assert_eq!(cat_colors.get(&"paganini"), Some(&"gray"));
//! assert_eq!(cat_colors.end_scope(), true);
//! assert_eq!(cat_colors.get(&"paganini"), Some(&"black"));
//!
//! // Begin a new scope, insert a value, and then end the scope to roll back the insert.
//! cat_colors.begin_scope();
//! cat_colors.insert("mint", "ginger");
//! assert_eq!(cat_colors.get(&"mint"), Some(&"ginger"));
//! assert_eq!(cat_colors.end_scope(), true);
//! assert_eq!(cat_colors.get(&"mint"), None);
//! ```
//! The `end_scope` method returns a boolean which is false if there is no scope to end, and true
//! otherwise. It is generally an error to end a scope that hasn't been started, so the method is
//! annoted with `#[must_use]`.
//! ```
//! # use texcraft::datastructures::scopedmap::ScopedMap;
//! let mut cat_colors = ScopedMap::<String, String>::new();
//! assert_eq!(cat_colors.end_scope(), false);
//! ```
//! There is also a "global" variant of the `insert` method. It inserts the value at the global
//! scope, and erases all other values.
//! ```
//! # use texcraft::datastructures::scopedmap::ScopedMap;
//! let mut cat_colors = ScopedMap::new();
//! cat_colors.insert("paganini", "black");
//! cat_colors.begin_scope();
//! cat_colors.insert_global("paganini", "gray");
//! assert_eq!(cat_colors.end_scope(), true);
//! assert_eq!(cat_colors.get(&"paganini"), Some(&"gray"));
//! ```
//!
use crate::datastructures::nevec::Nevec;
use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::Hash;
use std::rc::Rc;

/// Implementation of the `ScopedMap` data structure. See the module docs for more
/// information.
pub struct ScopedMap<K: Eq + Hash, V> {
    // The implementation is based on two internal data structures. The first is a map that for each
    // key contains the stack of values that have been inserted for that key. Each element of the
    // stack corresponds to a distinct scope: if a value in a scope is updated, we update the value
    // at the top of the stack, rather than pushing a new element on the stack. Not every scope
    // appears in this stack: if no mutation to a key occurs in a scope then the stack remains
    // the same.
    //
    // The second data structure is a stack that keeps track of which keys have been changed in
    // which scopes. This stack is used to rollback changes: at the end of each scope, we use it
    // to identify which keys need to have a value removed from their corresponding stack in the
    // map data structure. The stack has one element for each scope, with the exception of the
    // global scope (we never need to rollback changes to the global scope).
    //
    // The following invariants hold in the implementation:
    // (1) each value stack has at least 1 element. If all of a stack's values are popped off, the
    //     stack is removed from the map.
    // (2) The size of each value stack is equal to the number of times the corresponding key
    //     appears in the changed_keys_stack, plus 1 if the key has been set in the global scope.
    //
    // In the current implementation, all map operations are amortized O(1) with the exception
    // of insert_global which is O(number of current scopes). If we paid extra memory and in the
    // value stack also kept track of the scope index the value corresponds to, we could
    // make insert_global O(1) as well. The memory usage of the implementation is asymptotically
    // optimal: we store O(N) elements of data where N is the number of values of the map that can
    // be observed using get and end_scope.
    key_to_value_stack: HashMap<Rc<K>, Nevec<V>>,
    changed_keys_stack: Vec<HashSet<Rc<K>>>,
}

impl<K: Eq + Hash, V> ScopedMap<K, V> {
    /// Inserts the key, value pair.
    pub fn insert<A: Into<K>, B: Into<V>>(&mut self, key: A, val: B) {
        // If the key is already in the map we retrieve the reference counting smart pointer that
        // has already been created for that key. This ensures that each key is stored in memory
        // at most once.
        let key = A::into(key);
        let val = B::into(val);
        let key = match self.key_to_value_stack.get_key_value(&key) {
            None => Rc::new(key),
            Some(key_value) => key_value.0.clone(),
        };

        match self.key_to_value_stack.get_mut(&key) {
            None => {
                self.key_to_value_stack.insert(key.clone(), nevec![val]);
                // If we're not in the global scope, mark the key as being changed in this scope.
                if let Some(changed_keys) = self.changed_keys_stack.last_mut() {
                    changed_keys.insert(key);
                }
            }
            Some(value_stack) => {
                let new_in_this_scope = match self.changed_keys_stack.last_mut() {
                    None => true, // global scope
                    Some(changed_keys) => changed_keys.insert(key.clone()),
                };
                if new_in_this_scope {
                    value_stack.push(val);
                } else {
                    *value_stack.last_mut() = val;
                }
            }
        }
    }

    /// Inserts the key, value pair in the global scope.
    pub fn insert_global(&mut self, key: K, val: V) {
        for changed_keys in &mut self.changed_keys_stack {
            changed_keys.remove(&key);
        }
        self.key_to_value_stack
            .insert(Rc::new(key), Nevec::new(val));
    }

    // TODO: specialize this for copiable values? What does HashMap do?
    // TODO: get_or_default
    /// Retrieves the value at the provided key.
    pub fn get(&self, key: &K) -> Option<&V> {
        match self.key_to_value_stack.get(key) {
            None => None,
            Some(value_stack) => Some(value_stack.last()),
        }
    }

    /// Begins a new scope.
    pub fn begin_scope(&mut self) {
        // Note that `HashSet::new()` is basically a free operation: no allocations will occur
        // until elements are inserted into it. So even if no mutations are made in this scope, we
        // don't pay much for adding the set eagerly.
        self.changed_keys_stack.push(HashSet::new());
    }

    #[must_use]
    /// Attempts to end the current scope and returns true if there is a scope to end, and false
    /// otherwise.
    pub fn end_scope(&mut self) -> bool {
        match self.changed_keys_stack.pop() {
            None => false,
            Some(changed_keys) => {
                // Note that for the running time analysis we account each iteration of this loop
                // to the insert method that put the key in the changed_keys set. Put another way,
                // this can be considered a defer or cleanup step for all of the insert calls
                // in the scope that is being ended.
                for k in changed_keys {
                    // If there are no elements in the tail of the Nevec then the vector has only one
                    // element. In this case we need to remove the stack entirely.
                    if self
                        .key_to_value_stack
                        .get_mut(&k)
                        .unwrap()
                        .pop_from_tail()
                        .is_none()
                    {
                        self.key_to_value_stack.remove(&k);
                    }
                }
                true
            }
        }
    }

    /// Extends the `ScopedMap` with (key, value) pairs.
    /// ```
    /// # use texcraft::datastructures::scopedmap::ScopedMap;
    /// let mut cat_colors = ScopedMap::new();
    /// cat_colors.extend(std::array::IntoIter::new([
    ///    ("paganini", "black"),
    ///    ("mint", "ginger"),
    /// ]));
    /// assert_eq!(cat_colors.get(&"paganini"), Some(&"black"));
    /// assert_eq!(cat_colors.get(&"mint"), Some(&"ginger"));
    /// ```
    pub fn extend<T: IntoIterator<Item = (K, V)>>(&mut self, iter: T) {
        for (key, val) in iter {
            self.insert(key, val);
        }
    }

    pub fn len(&self) -> usize {
        self.key_to_value_stack.len()
    }

    pub fn is_empty(&self) -> bool {
        self.key_to_value_stack.is_empty()
    }

    pub fn keys(&self) -> std::collections::hash_map::Keys<'_, std::rc::Rc<K>, Nevec<V>> {
        self.key_to_value_stack.keys()
    }

    /// Returns a new empty `ScopedMap`.
    pub fn new() -> ScopedMap<K, V> {
        ScopedMap {
            key_to_value_stack: HashMap::new(),
            changed_keys_stack: Vec::<HashSet<Rc<K>>>::new(),
        }
    }
}

impl<K: Eq + Hash, V> std::iter::FromIterator<(K, V)> for ScopedMap<K, V> {
    /// Returns a new `ScopedMap` pre-populated with the provided key, values pairs
    /// ```
    /// # use texcraft::datastructures::scopedmap::ScopedMap;
    /// # use std::iter::FromIterator;
    /// let mut cat_colors = ScopedMap::from_iter(std::array::IntoIter::new([
    ///    ("paganini", "black"),
    ///    ("mint", "ginger"),
    /// ]));
    /// assert_eq!(cat_colors.get(&"paganini"), Some(&"black"));
    /// assert_eq!(cat_colors.get(&"mint"), Some(&"ginger"));
    /// ```
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> ScopedMap<K, V> {
        let mut map = ScopedMap::new();
        map.extend(iter);
        map
    }
}

impl<K: Eq + Hash, V> Default for ScopedMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use crate::datastructures::scopedmap::ScopedMap;

    #[test]
    fn insert_after_nested_insert() {
        let mut map = ScopedMap::new();
        map.begin_scope();
        map.insert(3, 5);
        assert_eq!(map.end_scope(), true);
        assert_eq!(map.get(&3), None);
        map.insert(3, 4);
        assert_eq!(map.get(&3), Some(&4));
    }

    #[test]
    fn insert_global_after_no_insert() {
        let mut map = ScopedMap::new();
        map.begin_scope();
        map.insert_global(3, 5);
        assert_eq!(map.end_scope(), true);
        assert_eq!(map.get(&3), Some(&5));
    }
}
