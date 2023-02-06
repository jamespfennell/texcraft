//! A circular buffer.
//!
//! See the documentation on the [CircularBuffer] type.

use std::ops::IndexMut;

/// A circular buffer.
///
/// A circular buffer is an ordered buffer with a fixed capacity and the property that if it is full
/// and a new element is pushed to the front, the oldest element is deleted.
///
/// The buffer is created using the [new](CircularBuffer::new) method, elements are pushed to the
/// front using [push](CircularBuffer::push), and retrieved using
/// [std::ops::Index::index] trait method.
///
/// ```
/// # use texcraft_stdext::collections::circularbuffer::CircularBuffer;
/// # use std::ops::Index;
/// let mut buf = CircularBuffer::new(3);
/// buf.push(0);
/// buf.push(1);
/// assert_eq![buf.index(0), &1];
/// assert_eq![buf.index(1), &0];
/// buf.push(2);
/// buf.push(3);
/// assert_eq![buf.index(0), &3];
/// assert_eq![buf.index(1), &2];
/// assert_eq![buf.index(2), &1];
/// ```
pub struct CircularBuffer<T> {
    data: Vec<T>,
    head: usize,
}

impl<T> CircularBuffer<T> {
    /// Create a new circular buffer with the provided capacity.
    pub fn new(capacity: usize) -> CircularBuffer<T> {
        CircularBuffer {
            data: Vec::with_capacity(capacity),
            head: 0,
        }
    }

    /// Push a new element to the front of the buffer.
    pub fn push(&mut self, elem: T) {
        if self.data.len() < self.data.capacity() {
            self.data.push(elem);
            self.head = self.data.len() - 1;
            return;
        }
        self.head = (self.head + 1) % self.capacity();
        *self.data.index_mut(self.head) = elem;
    }

    /// Return the buffer's capacity.
    pub fn capacity(&self) -> usize {
        self.data.capacity()
    }

    fn internal_index(&self, i: usize) -> usize {
        (self.head + self.capacity() - i) % self.capacity()
    }
}

impl<T> std::ops::Index<usize> for CircularBuffer<T> {
    type Output = T;

    /// Get the element at the provided index.
    ///
    /// The first element in the buffer has index 0, the next element index 1, etc.
    ///
    /// This method may panic if the index is valid.
    fn index(&self, i: usize) -> &T {
        &self.data[self.internal_index(i)]
    }
}

impl<T: Clone> CircularBuffer<T> {
    /// Clone the element at the provided index to the front of the buffer.
    ///
    /// The buffer contains the following optimization: if the buffer is full and the
    /// referenced element is the tail element, this method is a no-op.
    /// If the method's clone function has side effects, these will not be seen, as in
    /// the following example.
    ///
    /// ```
    /// # use texcraft_stdext::collections::circularbuffer::CircularBuffer;
    /// # use std::rc::Rc;
    /// # use std::cell::RefCell;
    /// /// A struct whose clone function has a side effect; i.e., is not pure.
    /// /// The side effect is that a shared counter owned by all clones is incremented.
    /// /// The shared counter thus records the number of times a clone has occured.
    /// struct ImpureClone {
    ///     counter: Rc<RefCell<i64>>,
    /// }
    ///
    /// impl Clone for ImpureClone {
    ///     fn clone(&self) -> Self {
    ///         let old_count = *self.counter.borrow();
    ///         *self.counter.borrow_mut() = old_count + 1;
    ///         ImpureClone {
    ///             counter: self.counter.clone()
    ///         }
    ///     }
    /// }
    ///
    /// let mut buf = CircularBuffer::new(2);
    /// let counter = Rc::new(RefCell::new(0));
    /// buf.push(ImpureClone{counter: counter.clone()});
    /// assert_eq![*counter.borrow(), 0];
    ///
    /// buf.clone_to_front(0);
    /// assert_eq![*counter.borrow(), 1];
    ///
    /// // Clone from the tail - no clone occurs!
    /// buf.clone_to_front(1);
    /// assert_eq![*counter.borrow(), 1];
    /// ```
    pub fn clone_to_front(&mut self, i: usize) -> &mut T {
        let i = self.internal_index(i);
        if self.data.len() < self.data.capacity() {
            self.push(self.data[i].clone());
            return self.data.last_mut().unwrap();
        }
        let tail_i = (self.head + 1) % self.capacity();
        match i.cmp(&tail_i) {
            std::cmp::Ordering::Equal => {
                // If we're cloning the current tail, we just make it the head by incrementing
                // the head ptr. No need to clone in this case!
            }
            std::cmp::Ordering::Less => {
                let (front, back) = self.data.split_at_mut(tail_i);
                back[0].clone_from(&front[i]);
            }
            std::cmp::Ordering::Greater => {
                let (front, back) = self.data.split_at_mut(i);
                front[tail_i].clone_from(&back[0]);
            }
        }
        self.head = tail_i;
        self.data.index_mut(self.head)
    }
}
