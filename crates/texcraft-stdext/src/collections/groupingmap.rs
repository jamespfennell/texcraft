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
//! # use texcraft_stdext::collections::groupingmap::Scope;
//! let mut cat_colors = GroupingHashMap::default();
//! cat_colors.insert("mint", "ginger", Scope::Local);
//! assert_eq!(cat_colors.get(&"mint"), Some(&"ginger"));
//! ```
//! The grouping methods are the main addition.
//! ```
//! # use texcraft_stdext::collections::groupingmap::GroupingHashMap;
//! # use texcraft_stdext::collections::groupingmap::Scope;
//! let mut cat_colors = GroupingHashMap::default();
//!
//! // Insert a new value, update the value in a new group, and then end the group to roll back
//! // the update.
//! cat_colors.insert("paganini", "black", Scope::Local);
//! cat_colors.begin_group();
//! cat_colors.insert("paganini", "gray", Scope::Local);
//! assert_eq!(cat_colors.get(&"paganini"), Some(&"gray"));
//! assert_eq!(cat_colors.end_group(), Ok(()));
//! assert_eq!(cat_colors.get(&"paganini"), Some(&"black"));
//!
//! // Begin a new group, insert a value, and then end the group to roll back the insert.
//! cat_colors.begin_group();
//! cat_colors.insert("mint", "ginger", Scope::Local);
//! assert_eq!(cat_colors.get(&"mint"), Some(&"ginger"));
//! assert_eq!(cat_colors.end_group(), Ok(()));
//! assert_eq!(cat_colors.get(&"mint"), None);
//! ```
//! The `end_group` method returns an error if there is no group to end.
//! ```
//! # use texcraft_stdext::collections::groupingmap::GroupingHashMap;
//! # use texcraft_stdext::collections::groupingmap::Scope;
//! # use texcraft_stdext::collections::groupingmap::NoGroupToEndError;
//! let mut cat_colors = GroupingHashMap::<String, String>::default();
//! assert_eq!(cat_colors.end_group(), Err(NoGroupToEndError{}));
//! ```
//! There is also a "global" variant of the `insert` method. It inserts the value at the global
//! group, and erases all other values.
//! ```
//! # use texcraft_stdext::collections::groupingmap::GroupingHashMap;
//! # use texcraft_stdext::collections::groupingmap::Scope;
//! let mut cat_colors = GroupingHashMap::default();
//! cat_colors.insert("paganini", "black", Scope::Local);
//! cat_colors.begin_group();
//! cat_colors.insert("paganini", "gray", Scope::Global);
//! assert_eq!(cat_colors.end_group(), Ok(()));
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

    /// Type of iterator returned by the [BackingContainer::iter] method.
    type Iter<'a>: Iterator<Item = (K, &'a V)>
    where
        V: 'a,
        Self: 'a;

    /// Iterate over all (key, value) tuples in the container.
    fn iter(&self) -> Self::Iter<'_>;

    /// Return the number of elements in the container.
    fn len(&self) -> usize;

    /// Return whether the container is empty.
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
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
    type Iter<'a> = std::iter::Map<
        std::collections::hash_map::Iter<'a, K, V>,
        fn(i: (&'a K, &'a V)) -> (K, &'a V)
    > where K:'a, V: 'a;
    fn iter(&self) -> Self::Iter<'_> {
        HashMap::iter(self).map(map_func)
    }
    fn len(&self) -> usize {
        HashMap::len(self)
    }
}

fn map_func<'a, K: Clone, V>(i: (&'a K, &'a V)) -> (K, &'a V) {
    (i.0.clone(), i.1)
}

impl<V> BackingContainer<usize, V> for Vec<Option<V>> {
    #[inline]
    fn insert(&mut self, k: usize, v: V) {
        match <[Option<V>]>::get_mut(self, k) {
            None => {
                self.resize_with(k, Default::default);
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
    fn remove(&mut self, k: &usize) {
        if let Some(elem) = <[Option<V>]>::get_mut(self, *k) {
            *elem = None;
        }
    }

    type Iter<'a>  = std::iter::FilterMap<
        std::iter::Enumerate<
            std::slice::Iter<'a, Option<V>>
        >,
        fn(i: (usize, &'a Option<V>)) -> Option<(usize, &'a V)>
    > where V: 'a;
    fn iter(&self) -> Self::Iter<'_> {
        <[Option<V>]>::iter(self)
            .enumerate()
            .filter_map(|i| i.1.as_ref().map(|v| (i.0, v)))
    }

    fn len(&self) -> usize {
        let mut l = 0;
        for v in <[Option<V>]>::iter(self) {
            if v.is_some() {
                l += 1;
            }
        }
        l
    }
}

/// A wrapper around [BackingContainer] types that adds a specific kind of group semantics.
///
/// See the module docs for more information.
#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct GroupingContainer<K, V, T> {
    backing_container: T,

    // The groups stack does not contain the global group as no cleanup there is needed.
    #[cfg_attr(
        feature = "serde",
        serde(bound(
            deserialize = "K: Eq + Hash + serde::Deserialize<'de>, V: serde::Deserialize<'de>"
        ))
    )]
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

/// Scope is used in the insertion method to determine the scope to insert at.
#[derive(Clone, Copy)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Scope {
    /// Insertions in the local scope are rolled back at the end of the current group.
    Local,
    /// Insertions in the global scope erase any other insertions for the same key, and
    /// persist beyond the end of the current groups.
    Global,
}

#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
enum EndOfGroupAction<V> {
    Revert(V),
    Delete,
}

/// Error returned if there is no group to end when [GroupingContainer::end_group] is invoked.
#[derive(Debug, PartialEq, Eq)]
pub struct NoGroupToEndError;

impl<K: Eq + Hash + Clone, V, T: BackingContainer<K, V>> GroupingContainer<K, V, T> {
    /// Inserts the key, value pair in the provided scope.
    pub fn insert(&mut self, key: K, mut val: V, scope: Scope) -> bool {
        let group = match scope {
            Scope::Local => self.groups.last_mut(),
            Scope::Global => {
                for group in &mut self.groups {
                    group.remove(&key);
                }
                None
            }
        };
        match (self.backing_container.get_mut(&key), group) {
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

    /// Retrieves the value at the provided key.
    ///
    /// It is equivalent to obtaining the
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

    /// Attempts to end the current group. Returns an error if there is no group to end.
    pub fn end_group(&mut self) -> Result<(), NoGroupToEndError> {
        match self.groups.pop() {
            None => Err(NoGroupToEndError {}),
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
                Ok(())
            }
        }
    }

    /// Extends the `GroupingMap` with (key, value) pairs.
    /// ```
    /// # use texcraft_stdext::collections::groupingmap::*;
    /// let mut cat_colors = GroupingHashMap::default();
    /// cat_colors.extend(std::array::IntoIter::new([
    ///    ("paganini", "black"),
    ///    ("mint", "ginger"),
    /// ]));
    /// assert_eq!(cat_colors.get(&"paganini"), Some(&"black"));
    /// assert_eq!(cat_colors.get(&"mint"), Some(&"ginger"));
    /// ```
    pub fn extend<I: IntoIterator<Item = (K, V)>>(&mut self, iter: I) {
        for (key, val) in iter {
            self.insert(key, val, Scope::Local);
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

    /// Iterate over all (key, value) tuples that are currently visible.
    pub fn iter(&self) -> T::Iter<'_> {
        self.backing_container.iter()
    }

    /// Iterate over all (key, value) tuples in the container,
    ///     including tuples that are not currently visible.
    ///
    /// See the documentation on [IterAll] for information on how this iterator works.
    ///
    /// To iterate over visible items only, use the [GroupingContainer::iter] method.
    pub fn iter_all(&self) -> IterAll<'_, K, V, T> {
        IterAll::new(self)
    }

    /// Returns the number of elements in the container.
    pub fn len(&self) -> usize {
        self.backing_container.len()
    }

    /// Returns whether the container is empty.
    pub fn is_empty(&self) -> bool {
        self.backing_container.is_empty()
    }
}

impl<K, V, T: Default> Default for GroupingContainer<K, V, T> {
    fn default() -> Self {
        Self {
            backing_container: Default::default(),
            groups: Default::default(),
        }
    }
}

impl<K: Eq + Hash, V: PartialEq, T: PartialEq> PartialEq for GroupingContainer<K, V, T> {
    fn eq(&self, other: &Self) -> bool {
        self.backing_container == other.backing_container && self.groups == other.groups
    }
}

impl<K: Eq + Hash, V: Eq, T: Eq> Eq for GroupingContainer<K, V, T> {}

/// The item for the [IterAll] iterator.
#[derive(PartialEq, Eq, Debug)]
pub enum Item<T> {
    /// Begin a new group.
    BeginGroup,
    /// Insert the `T=(key, value)` tuple into the container.
    Value(T),
}

impl<T> Item<T> {
    /// Adapt a lambda to use in [Iterator::map] for iterators over this item.
    ///
    /// When iterating over items of this type, one almost always wants to keep
    ///     [Item::BeginGroup] constant and apply a transformation to the [Item::Value] variant.
    /// This adaptor function helps with this by converting a lambda that operates on `T`
    ///     to a lambda that operates on [`Item<T>`].
    ///
    /// ```
    /// # use texcraft_stdext::collections::groupingmap::*;
    /// let start: Vec<Item<usize>> = vec![
    ///     Item::Value(1),
    ///     Item::BeginGroup,
    ///     Item::Value(2),
    /// ];
    /// let end: Vec<Item<usize>> = start.into_iter().map(Item::adapt_map(|i| { i *100 })).collect();
    /// assert_eq![end, vec![
    ///     Item::Value(100),
    ///     Item::BeginGroup,
    ///     Item::Value(200),
    /// ]];
    /// ```
    pub fn adapt_map<B, F: FnMut(T) -> B>(mut f: F) -> impl FnMut(Item<T>) -> Item<B> {
        move |item| match item {
            Item::BeginGroup => Item::BeginGroup,
            Item::Value(kv) => Item::Value(f(kv)),
        }
    }
}

/// An iterator over all values in a grouping container, including invisible values.
///
/// To understand this iterator, it's easiest to look at an example.
/// Suppose we have the following grouping map:
/// ```
/// # use texcraft_stdext::collections::groupingmap::*;
/// let mut cat_colors = GroupingHashMap::default();
/// cat_colors.insert("paganini", "black", Scope::Local);
/// cat_colors.begin_group();
/// cat_colors.insert("paganini", "gray", Scope::Local);
/// ```
/// After these mutations, the grouping map contains two tuples:
///     the tuple `("paganini", "gray")` that is visible, and
///     the tuple `("paganini", "black")` that is currently invisible.
/// The second tuple will become visible again when the group ends.
///
/// This iterator enables iterating over all tuples, visible and invisible.
/// In this example here this is the result:
/// ```
/// # use texcraft_stdext::collections::groupingmap::*;
/// # let mut cat_colors = GroupingHashMap::default();
/// # cat_colors.insert("paganini", "black", Scope::Local);
/// # cat_colors.begin_group();
/// # cat_colors.insert("paganini", "gray", Scope::Local);
/// let items: Vec<_> = cat_colors.iter_all().collect();
/// assert_eq![items, vec![
///     Item::Value(("paganini", &"black")),
///     Item::BeginGroup,
///     Item::Value(("paganini", &"gray")),
/// ]]
/// ```
/// A good mental model for this iterator is that it replays all mutations (inserts and begin groups)
///     that have been applied to the map.
/// However it doesn't replay the mutations exactly as they happened.
/// Instead, it returns the minimal number of mutations to recreate the map.
/// Thus:
/// ```
/// # use texcraft_stdext::collections::groupingmap::*;
/// let mut cat_colors = GroupingHashMap::default();
/// cat_colors.insert("local", "value_1", Scope::Local);
/// cat_colors.insert("local", "value_2", Scope::Local);
/// cat_colors.begin_group();
/// cat_colors.insert("local", "value_3", Scope::Local);
/// cat_colors.insert("local", "value_4", Scope::Local);
/// let items: Vec<_> = cat_colors.iter_all().collect();
/// assert_eq![items, vec![
///     Item::Value(("local", &"value_2")),
///     Item::BeginGroup,
///     Item::Value(("local", &"value_4")),
/// ]]
/// ```
/// It also converts global assignments within a group to regular assignments in the global scope:
/// ```
/// # use texcraft_stdext::collections::groupingmap::*;
/// let mut cat_colors = GroupingHashMap::default();
/// cat_colors.insert("global", "value_1", Scope::Local);
/// cat_colors.begin_group();
/// cat_colors.insert("global", "value_2", Scope::Global);
/// let items: Vec<_> = cat_colors.iter_all().collect();
/// assert_eq![items, vec![
///     Item::Value(("global", &"value_2")),
///     Item::BeginGroup,
/// ]]
/// ```
pub struct IterAll<'a, K, V, T: BackingContainer<K, V> + 'a> {
    visible_items: Option<T::Iter<'a>>,
    non_global_items: Vec<Item<(K, &'a V)>>,
    key_to_val: HashMap<K, Option<&'a V>>,
}

impl<'a, K: Eq + Hash + Clone, V, T: BackingContainer<K, V>> IterAll<'a, K, V, T> {
    fn new(map: &'a GroupingContainer<K, V, T>) -> Self {
        let mut key_to_val = HashMap::<K, Option<&'a V>>::with_capacity(map.groups.len());
        let save_stack_size: usize = map.groups.iter().map(HashMap::len).sum();
        let mut non_global_items = Vec::<Item<(K, &'a V)>>::with_capacity(save_stack_size);
        for group in map.groups.iter().rev() {
            for (k, action) in group {
                let v = match key_to_val.get(k) {
                    None => map.backing_container.get(k).unwrap(),
                    Some(v) => v.unwrap(),
                };
                non_global_items.push(Item::Value((k.clone(), v)));
                key_to_val.insert(
                    k.clone(),
                    match action {
                        EndOfGroupAction::Delete => None,
                        EndOfGroupAction::Revert(v) => Some(v),
                    },
                );
            }
            non_global_items.push(Item::BeginGroup);
        }
        Self {
            visible_items: Some(map.backing_container().iter()),
            non_global_items,
            key_to_val,
        }
    }
}

impl<'a, K: Eq + Hash, V, T: BackingContainer<K, V> + 'a> Iterator for IterAll<'a, K, V, T> {
    type Item = Item<(K, &'a V)>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(visible_items) = &mut self.visible_items {
            for visible_item in visible_items {
                match self.key_to_val.get(&visible_item.0) {
                    // The item is visible and appears nowhere in the save stack. It must have been defined
                    // in the global scope, and we thus return it.
                    None => return Some(Item::Value((visible_item.0, visible_item.1))),
                    // The item is visible and the last entry in the save stack is a delete instruction.
                    // This indicates the item was first defined inside a local group and is not defined in
                    // the global scope. We skip it.
                    Some(None) => continue,
                    // The item is visible and the last entry in the save stack is a revert instruction.
                    // We return the value in the revert instruction, as this is the value in the global scope.
                    Some(Some(global_value)) => {
                        return Some(Item::Value((visible_item.0, global_value)))
                    }
                }
            }
        }
        self.visible_items = None;
        self.non_global_items.pop()
    }
}

impl<K: Eq + Hash + Clone, V, T: BackingContainer<K, V>> FromIterator<Item<(K, V)>>
    for GroupingContainer<K, V, T>
{
    fn from_iter<I: IntoIterator<Item = Item<(K, V)>>>(iter: I) -> Self {
        let mut map: Self = GroupingContainer::default();
        for item in iter {
            match item {
                Item::BeginGroup => map.begin_group(),
                Item::Value((k, v)) => {
                    map.insert(k, v, Scope::Local);
                }
            }
        }
        map
    }
}

impl<K: Eq + Hash + Clone, V, T: BackingContainer<K, V>> FromIterator<(K, V)>
    for GroupingContainer<K, V, T>
{
    fn from_iter<I: IntoIterator<Item = (K, V)>>(iter: I) -> Self {
        let mut map: Self = GroupingContainer::default();
        for (k, v) in iter {
            map.backing_container.insert(k, v);
        }
        map
    }
}

#[cfg(test)]
mod tests {
    use crate::collections::groupingmap::*;

    #[test]
    fn insert_after_nested_insert() {
        let mut map = GroupingHashMap::default();
        map.begin_group();
        map.insert(3, 5, Scope::Local);
        assert_eq!(map.end_group(), Ok(()));
        assert_eq!(map.get(&3), None);
        map.insert(3, 4, Scope::Local);
        assert_eq!(map.get(&3), Some(&4));
    }

    #[test]
    fn insert_global_after_no_insert() {
        let mut map = GroupingHashMap::default();
        map.begin_group();
        map.insert(3, 5, Scope::Global);
        assert_eq!(map.end_group(), Ok(()));
        assert_eq!(map.get(&3), Some(&5));
    }

    fn run_iter_all_test(map: &GroupingHashMap<usize, usize>, want: &[Item<(usize, usize)>]) {
        let got: Vec<_> = map
            .iter_all()
            .map(|item| match item {
                Item::BeginGroup => Item::BeginGroup,
                Item::Value((k, v)) => Item::Value((k, *v)),
            })
            .collect();
        assert_eq!(got, want);
    }

    macro_rules! iter_all_tests {
        ( $( ($name: ident, $map: expr, $want: expr $(,)? ), )+ ) => {
            $(
            #[test]
            fn $name() {
                let map = $map;
                let want = $want;
                run_iter_all_test(&map, &want);
            }
            )+
        };
    }

    mod iter_all_tests {
        use super::*;
        iter_all_tests!(
            (empty_0, GroupingHashMap::default(), vec![]),
            (
                empty_1,
                {
                    let mut m = GroupingHashMap::default();
                    m.begin_group();
                    m
                },
                vec![Item::BeginGroup],
            ),
            (
                empty_2,
                {
                    let mut m = GroupingHashMap::default();
                    m.begin_group();
                    m.begin_group();
                    m.begin_group();
                    m.end_group().unwrap();
                    m
                },
                vec![Item::BeginGroup, Item::BeginGroup],
            ),
            (
                single_root_assignment,
                {
                    let mut m = GroupingHashMap::default();
                    m.insert(1, 1, Scope::Local);
                    m.begin_group();
                    m.begin_group();
                    m
                },
                vec![Item::Value((1, 1)), Item::BeginGroup, Item::BeginGroup],
            ),
            (
                single_global_assignment,
                {
                    let mut m = GroupingHashMap::default();
                    m.begin_group();
                    m.insert(1, 1, Scope::Global);
                    m.begin_group();
                    m
                },
                vec![Item::Value((1, 1)), Item::BeginGroup, Item::BeginGroup],
            ),
            (
                overwrite_root_assignment_1,
                {
                    let mut m = GroupingHashMap::default();
                    m.insert(1, 1, Scope::Local);
                    m.begin_group();
                    m.insert(1, 2, Scope::Local);
                    m.begin_group();
                    m
                },
                vec![
                    Item::Value((1, 1)),
                    Item::BeginGroup,
                    Item::Value((1, 2)),
                    Item::BeginGroup
                ],
            ),
            (
                overwrite_root_assignment_2,
                {
                    let mut m = GroupingHashMap::default();
                    m.insert(1, 1, Scope::Local);
                    m.begin_group();
                    m.insert(1, 2, Scope::Local);
                    m.begin_group();
                    m.insert(1, 3, Scope::Local);
                    m
                },
                vec![
                    Item::Value((1, 1)),
                    Item::BeginGroup,
                    Item::Value((1, 2)),
                    Item::BeginGroup,
                    Item::Value((1, 3)),
                ],
            ),
            (
                single_local_assignment,
                {
                    let mut m = GroupingHashMap::default();
                    m.begin_group();
                    m.insert(1, 1, Scope::Local);
                    m.begin_group();
                    m
                },
                vec![Item::BeginGroup, Item::Value((1, 1)), Item::BeginGroup],
            ),
            (
                overwrite_local_assignment_1,
                {
                    let mut m = GroupingHashMap::default();
                    m.begin_group();
                    m.insert(1, 1, Scope::Local);
                    m.begin_group();
                    m.insert(1, 2, Scope::Local);
                    m
                },
                vec![
                    Item::BeginGroup,
                    Item::Value((1, 1)),
                    Item::BeginGroup,
                    Item::Value((1, 2))
                ],
            ),
            (
                overwrite_local_assignment_2,
                {
                    let mut m = GroupingHashMap::default();
                    m.begin_group();
                    m.insert(1, 1, Scope::Local);
                    m.begin_group();
                    m.insert(1, 2, Scope::Local);
                    m.begin_group();
                    m.insert(1, 3, Scope::Local);
                    m
                },
                vec![
                    Item::BeginGroup,
                    Item::Value((1, 1)),
                    Item::BeginGroup,
                    Item::Value((1, 2)),
                    Item::BeginGroup,
                    Item::Value((1, 3))
                ],
            ),
        );
    }
}
