use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::cell::RefCell;

/// A string interner that deduplicates strings and assigns them unique IDs
/// This reduces memory usage when the same strings are used repeatedly (like identifiers)
#[derive(Debug, Clone)]
pub struct StringInterner {
    /// Map from string to its ID (interior mutability for interning)
    string_to_id: RefCell<FxHashMap<String, StringId>>,
    /// Map from ID to string (interior mutability for interning)
    id_to_string: RefCell<Vec<String>>,
}

/// A unique identifier for an interned string
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct StringId(u32);

/// Pre-defined common identifiers used across the compiler
#[derive(Debug, Clone, Copy)]
pub struct CommonIdentifiers {
    pub nil: StringId,
    pub true_: StringId,
    pub false_: StringId,
    pub and: StringId,
    pub or: StringId,
    pub not: StringId,
    pub function: StringId,
    pub local: StringId,
    pub const_: StringId,
    pub return_: StringId,
    pub if_: StringId,
    pub elseif: StringId,
    pub else_: StringId,
    pub then: StringId,
    pub end: StringId,
    pub while_: StringId,
    pub do_: StringId,
    pub for_: StringId,
    pub in_: StringId,
    pub break_: StringId,
    pub continue_: StringId,
    pub repeat: StringId,
    pub until: StringId,
    pub key: StringId,
}

impl StringInterner {
    /// Create a new string interner
    pub fn new() -> Self {
        Self {
            string_to_id: RefCell::new(FxHashMap::default()),
            id_to_string: RefCell::new(Vec::new()),
        }
    }
}

impl Default for StringInterner {
    fn default() -> Self {
        Self::new()
    }
}

impl StringInterner {
    /// Create a new string interner with common identifiers pre-registered
    pub fn new_with_common_identifiers() -> (Self, CommonIdentifiers) {
        let interner = Self::new();
        let common = CommonIdentifiers {
            nil: interner.intern("nil"),
            true_: interner.intern("true"),
            false_: interner.intern("false"),
            and: interner.intern("and"),
            or: interner.intern("or"),
            not: interner.intern("not"),
            function: interner.intern("function"),
            local: interner.intern("local"),
            const_: interner.intern("const"),
            return_: interner.intern("return"),
            if_: interner.intern("if"),
            elseif: interner.intern("elseif"),
            else_: interner.intern("else"),
            then: interner.intern("then"),
            end: interner.intern("end"),
            while_: interner.intern("while"),
            do_: interner.intern("do"),
            for_: interner.intern("for"),
            in_: interner.intern("in"),
            break_: interner.intern("break"),
            continue_: interner.intern("continue"),
            repeat: interner.intern("repeat"),
            until: interner.intern("until"),
            key: interner.intern("key"),
        };
        (interner, common)
    }

    /// Intern a string and return its ID
    /// If the string is already interned, returns the existing ID
    pub fn intern(&self, s: &str) -> StringId {
        // Fast path: check if already interned (read-only)
        if let Some(&id) = self.string_to_id.borrow().get(s) {
            return id;
        }

        // Slow path: need to insert (write)
        let mut id_to_string = self.id_to_string.borrow_mut();
        let mut string_to_id = self.string_to_id.borrow_mut();

        // Double-check after acquiring write lock
        if let Some(&id) = string_to_id.get(s) {
            return id;
        }

        let id = StringId(id_to_string.len() as u32);
        id_to_string.push(s.to_string());
        string_to_id.insert(s.to_string(), id);
        id
    }

    /// Get the string for a given ID
    /// Panics if the ID is invalid
    pub fn resolve(&self, id: StringId) -> String {
        self.id_to_string.borrow()[id.0 as usize].clone()
    }

    /// Get the string for a given ID, if it exists
    pub fn try_resolve(&self, id: StringId) -> Option<String> {
        self.id_to_string.borrow().get(id.0 as usize).cloned()
    }

    /// Resolve with a callback to avoid allocation when possible
    pub fn with_resolved<F, R>(&self, id: StringId, f: F) -> R
    where
        F: FnOnce(&str) -> R,
    {
        let strings = self.id_to_string.borrow();
        f(&strings[id.0 as usize])
    }

    /// Get the number of unique strings interned
    pub fn len(&self) -> usize {
        self.id_to_string.borrow().len()
    }

    /// Check if the interner is empty
    pub fn is_empty(&self) -> bool {
        self.id_to_string.borrow().is_empty()
    }

    /// Get or intern a string, returning its ID
    /// Uses interior mutability for concurrent access
    pub fn get_or_intern(&self, s: &str) -> StringId {
        // Fast path: check if already interned (read-only)
        if let Some(&id) = self.string_to_id.borrow().get(s) {
            return id;
        }

        // Slow path: need to insert (write)
        let mut id_to_string = self.id_to_string.borrow_mut();
        let mut string_to_id = self.string_to_id.borrow_mut();

        // Double-check after acquiring write lock
        if let Some(&id) = string_to_id.get(s) {
            return id;
        }

        let id = StringId(id_to_string.len() as u32);
        id_to_string.push(s.to_string());
        string_to_id.insert(s.to_string(), id);
        id
    }
}

impl StringId {
    /// Get the raw u32 value of this ID
    pub fn as_u32(self) -> u32 {
        self.0
    }

    /// Create a StringId from a raw u32 value
    /// This is unchecked and doesn't validate the ID exists in the interner
    pub fn from_u32(id: u32) -> Self {
        Self(id)
    }
}

impl std::fmt::Display for StringId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "StringId({})", self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_intern_basic() {
        let interner = StringInterner::new();

        let id1 = interner.intern("hello");
        let id2 = interner.intern("world");
        let id3 = interner.intern("hello");

        assert_eq!(id1, id3);
        assert_ne!(id1, id2);

        assert_eq!(interner.resolve(id1), "hello");
        assert_eq!(interner.resolve(id2), "world");
    }

    #[test]
    fn test_intern_deduplication() {
        let interner = StringInterner::new();

        let ids: Vec<_> = (0..100).map(|_| interner.intern("test")).collect();

        assert!(ids.iter().all(|&id| id == ids[0]));

        assert_eq!(interner.len(), 1);
    }

    #[test]
    fn test_intern_many_unique() {
        let interner = StringInterner::new();

        let strings = vec!["foo", "bar", "baz", "qux", "test", "hello", "world"];
        let ids: Vec<_> = strings.iter().map(|s| interner.intern(s)).collect();

        for i in 0..ids.len() {
            for j in (i + 1)..ids.len() {
                assert_ne!(ids[i], ids[j]);
            }
        }

        for (i, &id) in ids.iter().enumerate() {
            assert_eq!(interner.resolve(id), strings[i]);
        }

        assert_eq!(interner.len(), strings.len());
    }

    #[test]
    fn test_try_resolve() {
        let interner = StringInterner::new();

        let id = interner.intern("test");
        assert_eq!(interner.try_resolve(id), Some("test".to_string()));

        let invalid_id = StringId::from_u32(9999);
        assert_eq!(interner.try_resolve(invalid_id), None);
    }

    #[test]
    fn test_with_resolved() {
        let interner = StringInterner::new();

        let id = interner.intern("callback_test");
        let result = interner.with_resolved(id, |s| s.len());
        assert_eq!(result, "callback_test".len());
    }
}
