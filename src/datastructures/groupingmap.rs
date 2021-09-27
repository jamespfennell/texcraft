//! A hash map with a grouping concept such that mutations are rolled back at the end of each group.
//!
//! # Examples
//!
//! The map methods are the same as the standard hash map (although only a few methods are
//! implemented).
//! ```
//! # use texcraft::datastructures::groupingmap::GroupingMap;
//! let mut cat_colors = GroupingMap::new();
//! cat_colors.insert("mint", "ginger");
//! assert_eq!(cat_colors.get(&"mint"), Some(&"ginger"));
//! ```
//! The grouping map additionally has `begin_group` and `end_group` methods along with the following
//! behavior: when a group is ended, all mutations to the map since the beginning of the group are
//! rolled back.
//! ```
//! # use texcraft::datastructures::groupingmap::GroupingMap;
//! let mut cat_colors = GroupingMap::new();
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
//! # use texcraft::datastructures::groupingmap::GroupingMap;
//! let mut cat_colors = GroupingMap::<String, String>::new();
//! assert_eq!(cat_colors.end_group(), false);
//! ```
//! There is also a "global" variant of the `insert` method. It inserts the value at the global
//! group, and erases all other values.
//! ```
//! # use texcraft::datastructures::groupingmap::GroupingMap;
//! let mut cat_colors = GroupingMap::new();
//! cat_colors.insert("paganini", "black");
//! cat_colors.begin_group();
//! cat_colors.insert_global("paganini", "gray");
//! assert_eq!(cat_colors.end_group(), true);
//! assert_eq!(cat_colors.get(&"paganini"), Some(&"gray"));
//! ```
//!
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::hash::Hash;

/// Implementation of the `GroupingMap` data structure. See the module docs for more
/// information.
pub struct GroupingMap<K: Eq + Hash + Clone, V> {
    values: HashMap<K, V>,

    // The groups stack does not contain the global group as no cleanup there is needed.
    groups: Vec<HashMap<K, EndOfGroupAction<V>>>,
}

enum EndOfGroupAction<V> {
    Revert(V),
    Delete,
}

impl<K: Eq + Hash + Clone, V> GroupingMap<K, V> {
    /// Inserts the key, value pair.
    pub fn insert<A: Into<K>, B: Into<V>>(&mut self, key: A, val: B) {
        let key = A::into(key);
        let mut val = B::into(val);

        match (self.values.get_mut(&key), self.groups.last_mut()) {
            (None, None) => {
                self.values.insert(key, val);
            }
            (None, Some(group)) => {
                group.insert(key.clone(), EndOfGroupAction::Delete);
                self.values.insert(key, val);
            }
            (Some(val_ref), None) => {
                *val_ref = val;
            }
            (Some(val_ref), Some(group)) => {
                std::mem::swap(&mut val, val_ref);
                if let Entry::Vacant(vac) = group.entry(key) {
                    vac.insert(EndOfGroupAction::Revert(val));
                };
            }
        }
    }

    /// Inserts the key, value pair in the global group.
    pub fn insert_global(&mut self, key: K, val: V) {
        for group in &mut self.groups {
            group.remove(&key);
        }
        self.values.insert(key, val);
    }

    // TODO: specialize this for copiable values? What does HashMap do?
    // TODO: get_or_default
    /// Retrieves the value at the provided key.
    pub fn get(&self, key: &K) -> Option<&V> {
        self.values.get(key)
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
                            self.values.remove(&key);
                        }
                        EndOfGroupAction::Revert(old_val) => {
                            self.values.insert(key, old_val);
                        }
                    }
                }
                true
            }
        }
    }

    /// Extends the `GroupingMap` with (key, value) pairs.
    /// ```
    /// # use texcraft::datastructures::groupingmap::GroupingMap;
    /// let mut cat_colors = GroupingMap::new();
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
        self.values.len()
    }

    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    pub fn as_regular_map(&self) -> &HashMap<K, V> {
        &self.values
    }

    /// Returns a new empty `groupingMap`.
    pub fn new() -> GroupingMap<K, V> {
        GroupingMap {
            values: HashMap::new(),
            groups: Vec::new(),
        }
    }
}

impl<K: Eq + Hash + Clone, V> std::iter::FromIterator<(K, V)> for GroupingMap<K, V> {
    /// Returns a new `groupingMap` pre-populated with the provided key, values pairs
    /// ```
    /// # use texcraft::datastructures::groupingmap::GroupingMap;
    /// # use std::iter::FromIterator;
    /// let mut cat_colors = GroupingMap::from_iter(std::array::IntoIter::new([
    ///    ("paganini", "black"),
    ///    ("mint", "ginger"),
    /// ]));
    /// assert_eq!(cat_colors.get(&"paganini"), Some(&"black"));
    /// assert_eq!(cat_colors.get(&"mint"), Some(&"ginger"));
    /// ```
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> GroupingMap<K, V> {
        GroupingMap {
            values: HashMap::from_iter(iter),
            groups: Vec::new(),
        }
    }
}

impl<K: Eq + Hash + Clone, V> Default for GroupingMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use crate::datastructures::groupingmap::GroupingMap;

    #[test]
    fn insert_after_nested_insert() {
        let mut map = GroupingMap::new();
        map.begin_group();
        map.insert(3, 5);
        assert_eq!(map.end_group(), true);
        assert_eq!(map.get(&3), None);
        map.insert(3, 4);
        assert_eq!(map.get(&3), Some(&4));
    }

    #[test]
    fn insert_global_after_no_insert() {
        let mut map = GroupingMap::new();
        map.begin_group();
        map.insert_global(3, 5);
        assert_eq!(map.end_group(), true);
        assert_eq!(map.get(&3), Some(&5));
    }
}
