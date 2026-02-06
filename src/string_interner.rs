use lasso::ThreadedRodeo;
use serde::{Deserialize, Serialize};
use std::sync::Arc;

/// A unique identifier for an interned string
/// Wraps lasso::Spur for compatibility with the existing API
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct StringId(lasso::Spur);

/// A thread-safe string interner that deduplicates strings and assigns them unique IDs
/// This reduces memory usage when the same strings are used repeatedly (like identifiers)
#[derive(Debug, Clone)]
pub struct StringInterner {
    rodeo: Arc<ThreadedRodeo>,
}

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
            rodeo: Arc::new(ThreadedRodeo::new()),
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
        StringId(self.rodeo.get_or_intern(s))
    }

    /// Get the string for a given ID
    /// Panics if the ID is invalid
    pub fn resolve(&self, id: StringId) -> String {
        self.rodeo.resolve(&id.0).to_string()
    }

    /// Get the string for a given ID, if it exists
    pub fn try_resolve(&self, id: StringId) -> Option<String> {
        self.rodeo.try_resolve(&id.0).map(|s| s.to_string())
    }

    /// Resolve with a callback to avoid allocation when possible
    pub fn with_resolved<F, R>(&self, id: StringId, f: F) -> R
    where
        F: FnOnce(&str) -> R,
    {
        f(self.rodeo.resolve(&id.0))
    }

    /// Export the string table for serialization.
    /// Returns the ordered list of interned strings.
    pub fn to_strings(&self) -> Vec<String> {
        self.rodeo.strings().map(|s| s.to_string()).collect()
    }

    /// Reconstruct an interner from a previously exported string table.
    /// StringId values from the original interner will resolve to the same strings.
    pub fn from_strings(strings: Vec<String>) -> Self {
        let rodeo = ThreadedRodeo::new();
        for s in strings {
            rodeo.get_or_intern(s);
        }
        Self { rodeo: Arc::new(rodeo) }
    }

    /// Get the number of unique strings interned
    pub fn len(&self) -> usize {
        self.rodeo.len()
    }

    /// Check if the interner is empty
    pub fn is_empty(&self) -> bool {
        self.rodeo.is_empty()
    }

    /// Get or intern a string, returning its ID
    /// Thread-safe version for concurrent access
    pub fn get_or_intern(&self, s: &str) -> StringId {
        self.intern(s)
    }
}

impl StringId {
    /// Get the raw Spur value
    #[inline(always)]
    pub fn as_spur(self) -> lasso::Spur {
        self.0
    }

    /// Get the raw usize value of this ID
    #[inline(always)]
    pub fn as_usize(self) -> usize {
        lasso::Key::into_usize(self.0)
    }

    /// Get the raw u32 value of this ID
    #[inline(always)]
    pub fn as_u32(self) -> u32 {
        self.as_usize() as u32
    }

    /// Create a StringId from a raw u32 value
    /// This is unchecked and doesn't validate the ID exists in the interner
    #[inline(always)]
    pub fn from_u32(id: u32) -> Self {
        Self(lasso::Key::try_from_usize(id as usize).expect("Invalid StringId"))
    }
}

impl std::fmt::Display for StringId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "StringId({})", self.as_usize())
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

        let strings = ["foo", "bar", "baz", "qux", "test", "hello", "world"];
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

    #[test]
    fn test_serialization_roundtrip() {
        let interner = StringInterner::new();

        let id1 = interner.intern("hello");
        let id2 = interner.intern("world");
        let id3 = interner.intern("hello");

        // Verify deduplication works
        assert_eq!(id1, id3);

        let strings = interner.to_strings();

        // Verify the strings are exported (order may not be preserved)
        assert!(strings.contains(&"hello".to_string()));
        assert!(strings.contains(&"world".to_string()));
        assert_eq!(strings.len(), 2); // Only 2 unique strings
    }

    #[test]
    fn test_common_identifiers() {
        let (interner, common) = StringInterner::new_with_common_identifiers();

        assert_eq!(interner.resolve(common.nil), "nil");
        assert_eq!(interner.resolve(common.true_), "true");
        assert_eq!(interner.resolve(common.false_), "false");
        assert_eq!(interner.resolve(common.function), "function");
        assert_eq!(interner.resolve(common.local), "local");
        assert_eq!(interner.resolve(common.const_), "const");
        assert_eq!(interner.resolve(common.return_), "return");
        assert_eq!(interner.resolve(common.if_), "if");
        assert_eq!(interner.resolve(common.else_), "else");
        assert_eq!(interner.resolve(common.end), "end");
    }

    #[test]
    fn test_thread_safe_intern() {
        use std::sync::{Arc, Barrier};
        use std::thread;

        let interner = Arc::new(StringInterner::new());
        let barrier = Arc::new(Barrier::new(4));

        let handles: Vec<_> = (0..4)
            .map(|_| {
                let interner = Arc::clone(&interner);
                let barrier = Arc::clone(&barrier);
                thread::spawn(move || {
                    barrier.wait();
                    // All threads interning the same strings
                    let _ = interner.intern("shared");
                    let _ = interner.intern("strings");
                    let id = interner.intern("unique");
                    id
                })
            })
            .collect();

        for handle in handles {
            let _ = handle.join().unwrap();
        }

        // Should have deduplicated properly
        assert_eq!(interner.len(), 3);
    }
}
