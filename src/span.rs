use serde::{Deserialize, Serialize};
use std::fmt;

/// Represents a location in source code with line and column information
/// Uses u32 instead of usize for memory efficiency (16 bytes vs 32 bytes on 64-bit)
/// This supports files up to 4GB and 4 billion lines, which is more than sufficient
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
pub struct Span {
    /// Starting byte offset in the source
    pub start: u32,
    /// Ending byte offset in the source (exclusive)
    pub end: u32,
    /// Line number (1-indexed)
    pub line: u32,
    /// Column number (1-indexed)
    pub column: u32,
}

impl Span {
    /// Create a new span
    #[inline]
    pub fn new(start: u32, end: u32, line: u32, column: u32) -> Self {
        Self {
            start,
            end,
            line,
            column,
        }
    }

    /// Create a dummy span for testing or generated code
    #[inline]
    pub fn dummy() -> Self {
        Self {
            start: 0,
            end: 0,
            line: 0,
            column: 0,
        }
    }

    /// Get the length of the span
    #[inline(always)]
    pub fn len(&self) -> u32 {
        self.end - self.start
    }

    /// Check if the span is empty
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    /// Merge two spans into one that covers both
    #[inline]
    pub fn merge(&self, other: &Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
            line: self.line.min(other.line),
            column: self.column.min(other.column),
        }
    }

    /// Alias for merge - combines two spans into one
    #[inline]
    pub fn combine(&self, other: &Span) -> Span {
        self.merge(other)
    }

    /// Check if this span contains a byte offset
    #[inline]
    pub fn contains(&self, offset: u32) -> bool {
        offset >= self.start && offset < self.end
    }

    /// Check if this span overlaps with another span
    #[inline]
    pub fn overlaps(&self, other: &Span) -> bool {
        self.start < other.end && other.start < self.end
    }

    /// Shift this span by a delta (for offset adjustment after edits)
    /// Returns None if the delta would make the span invalid
    pub fn shift(&self, delta: i32) -> Option<Span> {
        let new_start = self.start.checked_add_signed(delta)?;
        let new_end = self.end.checked_add_signed(delta)?;

        if new_end < new_start {
            return None;
        }

        Some(Span {
            start: new_start,
            end: new_end,
            line: self.line, // Line/column require full recalculation
            column: self.column,
        })
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_span_creation() {
        let span = Span::new(0, 5, 1, 1);
        assert_eq!(span.start, 0);
        assert_eq!(span.end, 5);
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1);
    }

    #[test]
    fn test_span_len() {
        let span = Span::new(10, 20, 2, 5);
        assert_eq!(span.len(), 10);
    }

    #[test]
    fn test_span_merge() {
        let span1 = Span::new(0, 5, 1, 1);
        let span2 = Span::new(10, 15, 1, 11);
        let merged = span1.merge(&span2);

        assert_eq!(merged.start, 0);
        assert_eq!(merged.end, 15);
    }

    #[test]
    fn test_span_display() {
        let span = Span::new(0, 5, 10, 15);
        assert_eq!(format!("{}", span), "10:15");
    }
}

#[cfg(test)]
mod incremental_tests {
    use super::*;

    #[test]
    fn test_contains() {
        let span = Span {
            start: 10,
            end: 20,
            line: 1,
            column: 10,
        };
        assert!(span.contains(10));
        assert!(span.contains(15));
        assert!(!span.contains(20)); // Exclusive end
        assert!(!span.contains(5));
    }

    #[test]
    fn test_overlaps() {
        let span1 = Span {
            start: 10,
            end: 20,
            line: 1,
            column: 10,
        };
        let span2 = Span {
            start: 15,
            end: 25,
            line: 1,
            column: 15,
        };
        let span3 = Span {
            start: 25,
            end: 30,
            line: 2,
            column: 1,
        };

        assert!(span1.overlaps(&span2)); // Overlap
        assert!(span2.overlaps(&span1)); // Symmetric
        assert!(!span1.overlaps(&span3)); // No overlap
    }

    #[test]
    fn test_shift() {
        let span = Span {
            start: 100,
            end: 120,
            line: 5,
            column: 10,
        };

        // Positive shift (insertion)
        let shifted = span.shift(10).unwrap();
        assert_eq!(shifted.start, 110);
        assert_eq!(shifted.end, 130);

        // Negative shift (deletion)
        let shifted = span.shift(-20).unwrap();
        assert_eq!(shifted.start, 80);
        assert_eq!(shifted.end, 100);

        // Invalid shift (would go negative)
        assert!(span.shift(-150).is_none());
    }
}
