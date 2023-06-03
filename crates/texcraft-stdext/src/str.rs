/// An owning iterator over the chars in a string.
///
/// This iterator is an alternative to the standard library's [Chars](std::str::Chars).
/// This iterator has shared ownership of the string being iterated over and avoids lifetime issues.
pub struct OwningChars {
    s: std::rc::Rc<str>,
    pos: usize,
}

impl OwningChars {
    pub fn new(s: std::rc::Rc<str>) -> OwningChars {
        OwningChars { s, pos: 0 }
    }

    pub fn str(&self) -> &str {
        &self.s
    }

    #[inline]
    pub fn peek(&self) -> Option<char> {
        self.s[self.pos..].chars().next()
    }
}

impl std::iter::Iterator for OwningChars {
    type Item = char;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let c_opt = self.s[self.pos..].chars().next();
        if let Some(c) = c_opt {
            self.pos += c.len_utf8();
        }
        c_opt
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn base_case() {
        let s: std::rc::Rc<str> = "Tör".into();
        let mut iter = OwningChars::new(s);
        assert_eq!(Some('T'), iter.next());
        assert_eq!(Some('ö'), iter.next());
        assert_eq!(Some('r'), iter.next());
        assert_eq!(None, iter.next());
    }
}
