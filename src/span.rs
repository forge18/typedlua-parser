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
    pub fn new(start: u32, end: u32, line: u32, column: u32) -> Self {
        Self {
            start,
            end,
            line,
            column,
        }
    }

    /// Create a dummy span for testing or generated code
    pub fn dummy() -> Self {
        Self {
            start: 0,
            end: 0,
            line: 0,
            column: 0,
        }
    }

    /// Get the length of the span
    pub fn len(&self) -> u32 {
        self.end - self.start
    }

    /// Check if the span is empty
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    /// Merge two spans into one that covers both
    pub fn merge(&self, other: &Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
            line: self.line.min(other.line),
            column: self.column.min(other.column),
        }
    }

    /// Alias for merge - combines two spans into one
    pub fn combine(&self, other: &Span) -> Span {
        self.merge(other)
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
