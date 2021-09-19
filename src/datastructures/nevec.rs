//! A vector type that is statically guaranteed to be non-empty
//!
//! In situations where a vector is known to have at least 1 element, this
//! data structure enables writing code without calls to `Vec::unwrap`. That is,
//! it provides a way to statically enforce the invariant.

use std::ops::Index;

/// Non-empty vector type.
pub struct Nevec<T> {
    first: T,
    tail: Vec<T>,
}

impl<T> Nevec<T> {
    /// Creates a new `Nevec` with the provided first element.
    pub fn new(first: T) -> Nevec<T> {
        Nevec::<T>::new_with_tail(first, Vec::new())
    }

    /// Creates a new `Nevec` with the provided first element and initial capacity.
    pub fn with_capacity(first: T, capacity: usize) -> Nevec<T> {
        Nevec::<T>::new_with_tail(first, Vec::with_capacity(capacity))
    }

    /// Creates a new `Nevec` with the provided first element and remaining elements ("the tail").
    pub fn new_with_tail(first: T, tail: Vec<T>) -> Nevec<T> {
        Nevec { first, tail }
    }

    /// Gets a reference to the last element of the vector.
    /// Because the vector is guaranteed to be non-empty, this will always succeed.
    pub fn last(&self) -> &T {
        match self.tail.last() {
            None => &self.first,
            Some(t) => t,
        }
    }

    /// Gets a mutable reference to the last element of the vector.
    /// Because the vector is guaranteed to be non-empty, this will always succeed.
    pub fn last_mut(&mut self) -> &mut T {
        match self.tail.last_mut() {
            None => &mut self.first,
            Some(t) => t,
        }
    }

    /// Pushes an element onto the end of the vector.
    pub fn push(&mut self, t: T) {
        self.tail.push(t)
    }

    /// Pops an element from the end of the vector.
    ///
    /// Because the vector is guaranteed to be non-empty, this will always succeed.
    /// However if the vector has only 1 element, then after popping it will no longer
    /// be non-empty. For this reason, the pop method takes ownership of the vector,
    /// and destroys it in the process.
    ///
    /// In the case when the vector has more than 1 element, the method `pop_from_tail`
    /// can be used to retrieve the last element without destroying the vector.
    pub fn pop(mut self) -> T {
        match self.tail.pop() {
            None => self.first,
            Some(t) => t,
        }
    }

    /// Pops an element from the tail of the vector; that is, the part of the vector
    /// after the first element.
    ///
    /// Returns `None` if and only if the vector has exactly 1 element. In this
    /// case the method `pop` is needed to take ownership of the element.
    pub fn pop_from_tail(&mut self) -> Option<T> {
        self.tail.pop()
    }

    /// Returns the length of the vector, which is guaranteed to be at least 1.
    pub fn len(&self) -> usize {
        1 + self.tail.len()
    }

    /// Get a reference to the element at the provided index.
    pub fn get(&self, i: usize) -> Option<&T> {
        if i == 0 {
            return Some(&self.first);
        }
        self.tail.get(i - 1)
    }

    /// Get a mutable reference to the element at the provided index.
    pub fn get_mut(&mut self, i: usize) -> Option<&mut T> {
        if i == 0 {
            return Some(&mut self.first);
        }
        self.tail.get_mut(i - 1)
    }
}

impl<T> Index<usize> for Nevec<T> {
    type Output = T;

    fn index(&self, i: usize) -> &Self::Output {
        if i == 0 {
            &self.first
        } else {
            &self.tail[i - 1]
        }
    }
}

impl<'a, T> IntoIterator for &'a Nevec<T> {
    type Item = &'a T;
    type IntoIter = NevecIter<'a, T>;

    fn into_iter(self) -> NevecIter<'a, T> {
        NevecIter { vec: self, i: 0 }
    }
}

impl<T: std::fmt::Display> std::fmt::Display for Nevec<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for t in self {
            write![f, "{}", t]?;
        }
        Ok(())
    }
}

pub struct NevecIter<'a, T> {
    vec: &'a Nevec<T>,
    i: usize,
}

impl<'a, T> Iterator for NevecIter<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        if self.i >= self.vec.len() {
            None
        } else {
            self.i += 1;
            Some(&self.vec[self.i - 1])
        }
    }
}

/// Create a new [Nevec] (non-empty vector).
#[macro_export]
macro_rules! nevec {
    ( $first: expr, $ ( $ x : expr ) , * ) => (
        Nevec::new_with_tail($first, vec![$ ( $ x ) , *])
    );
    ( $first: expr, $ ( $ x : expr , ) * ) => ( nevec ! [ $first, $ ( $ x ) , * ] );
    ( $first: expr ) => {
      Nevec::new($first)
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn singleton_vector() {
        let mut v = nevec![3];
        assert_eq![v.len(), 1];
        assert_eq![v.last(), &3];
        assert_eq![v.last_mut(), &3];
        assert_eq![v.pop_from_tail(), None];
        assert_eq![v.pop(), 3];
    }

    #[test]
    fn two_element_vector() {
        let mut v = nevec![3, 4];
        assert_eq![v.len(), 2];
        assert_eq![v.last(), &4];
        assert_eq![v.last_mut(), &4];
        assert_eq![v[0], 3];
        assert_eq![v[1], 4];
        assert_eq![v.pop_from_tail(), Some(4)];
        assert_eq![v.last(), &3];
        assert_eq![v.last_mut(), &3];
        assert_eq![v.pop_from_tail(), None];
    }

    #[test]
    fn vector_with_push() {
        let mut v = nevec![3, 4,];
        assert_eq![v.len(), 2];
        v.push(5);
        assert_eq![v.len(), 3];
        assert_eq![v.last(), &5];
        assert_eq![v.last_mut(), &5];
        assert_eq![v.pop_from_tail(), Some(5)];
    }

    #[test]
    fn other_macro_constructor_case() {
        let mut v = nevec![3,];
        assert_eq![v.len(), 1];
        assert_eq![v.last(), &3];
        assert_eq![v.last_mut(), &3];
        assert_eq![v.pop_from_tail(), None];
        assert_eq![v.pop(), 3];
    }
}
