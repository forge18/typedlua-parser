//! Span validation for incremental parsing
//!
//! With multi-arena approach, we don't need deep cloning.
//! We just adjust span offsets in-place on cached statement references.

use crate::ast::statement::Statement;
use crate::incremental::cache::get_statement_span;
use crate::incremental::dirty::TextEdit;
use crate::span::Span;

/// Check if a statement's span overlaps with any edit.
///
/// A statement is **clean** if its byte range `[start, end)` does not overlap
/// any edit's byte range `[edit_start, edit_end)`. Clean statements can be
/// reused from the previous parse tree without re-parsing.
///
/// Overlap is defined as: `stmt.start < edit.end && stmt.end > edit.start`.
/// Edits that are exactly adjacent (edit ends where statement starts, or vice
/// versa) are **not** considered overlapping — the statement is still clean.
///
/// # Returns
/// - `true` — statement is clean, can be reused from cache
/// - `false` — statement is dirty, must be re-parsed
///
/// # Arguments
/// * `stmt` - The cached statement to validate
/// * `edits` - The text edits applied since last parse
pub fn is_statement_clean(stmt: &Statement, edits: &[TextEdit]) -> bool {
    let span = get_statement_span(stmt);
    !overlaps_any_edit(&span, edits)
}

/// Check if a span overlaps with any edit region
fn overlaps_any_edit(span: &Span, edits: &[TextEdit]) -> bool {
    for edit in edits {
        let edit_start = edit.range.0;
        let edit_end = edit.range.1;

        if edit_start == edit_end {
            // Zero-length insertion: dirty if insertion point is inside the
            // statement or at its end boundary, i.e. [span.start, span.end].
            // Insertions at span.end could extend the last token of the
            // statement (e.g., appending digits to a number literal).
            if edit_start >= span.start && edit_start <= span.end {
                return true;
            }
        } else {
            // Range edit (replacement or deletion): standard overlap check
            if span.start < edit_end && span.end > edit_start {
                return true;
            }
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::span::Span;

    #[test]
    fn test_no_overlap() {
        let stmt = Statement::Break(Span::new(0, 5, 1, 1));
        let edits = vec![TextEdit {
            range: (10, 15),
            new_text: "modified".to_string(),
        }];

        assert!(is_statement_clean(&stmt, &edits));
    }

    #[test]
    fn test_overlap() {
        let stmt = Statement::Break(Span::new(0, 10, 1, 1));
        let edits = vec![TextEdit {
            range: (5, 15),
            new_text: "modified".to_string(),
        }];

        assert!(!is_statement_clean(&stmt, &edits));
    }

    #[test]
    fn test_exact_boundary() {
        let stmt = Statement::Break(Span::new(0, 10, 1, 1));
        let edits = vec![TextEdit {
            range: (10, 15),
            new_text: "modified".to_string(),
        }];

        // Edit starts exactly at statement end - no overlap
        assert!(is_statement_clean(&stmt, &edits));
    }

    // --- NEW TESTS FOR OFFSET ADJUSTMENT ---

    #[test]
    fn test_statement_clean_after_insertion() {
        let stmt = Statement::Break(Span::new(20, 25, 1, 1));
        // Insertion before the statement
        let edits = vec![TextEdit {
            range: (10, 10),
            new_text: "inserted text".to_string(),
        }];

        // Statement is after the edit, so it's clean (no overlap)
        assert!(is_statement_clean(&stmt, &edits));
    }

    #[test]
    fn test_statement_dirty_after_overlapping_deletion() {
        let stmt = Statement::Break(Span::new(10, 20, 1, 1));
        // Deletion overlaps statement
        let edits = vec![TextEdit {
            range: (15, 25),
            new_text: String::new(),
        }];

        assert!(!is_statement_clean(&stmt, &edits));
    }

    #[test]
    fn test_statement_clean_before_edit() {
        let stmt = Statement::Break(Span::new(0, 10, 1, 1));
        // Edit is after the statement
        let edits = vec![TextEdit {
            range: (20, 25),
            new_text: "modified".to_string(),
        }];

        assert!(is_statement_clean(&stmt, &edits));
    }

    #[test]
    fn test_statement_dirty_when_edit_inside() {
        let stmt = Statement::Break(Span::new(10, 30, 1, 1));
        // Edit is completely inside the statement
        let edits = vec![TextEdit {
            range: (15, 20),
            new_text: "x".to_string(),
        }];

        assert!(!is_statement_clean(&stmt, &edits));
    }

    #[test]
    fn test_sequential_edits_overlap_detection() {
        let stmt = Statement::Break(Span::new(10, 20, 1, 1));
        // Multiple edits, one overlaps
        let edits = vec![
            TextEdit {
                range: (0, 5),
                new_text: "a".to_string(),
            },
            TextEdit {
                range: (15, 17), // Overlaps statement
                new_text: "b".to_string(),
            },
            TextEdit {
                range: (30, 35),
                new_text: "c".to_string(),
            },
        ];

        assert!(!is_statement_clean(&stmt, &edits));
    }

    #[test]
    fn test_replacement_edit_overlap() {
        let stmt = Statement::Break(Span::new(10, 20, 1, 1));
        // Replace text that partially overlaps
        let edits = vec![TextEdit {
            range: (18, 25),
            new_text: "replaced".to_string(),
        }];

        assert!(!is_statement_clean(&stmt, &edits));
    }

    #[test]
    fn test_insertion_at_statement_end_is_dirty() {
        let stmt = Statement::Break(Span::new(10, 20, 1, 1));
        // Zero-length insertion at exactly where statement ends
        let edits = vec![TextEdit {
            range: (20, 20),
            new_text: "suffix".to_string(),
        }];

        // Dirty — insertion could extend the last token of the statement
        assert!(!is_statement_clean(&stmt, &edits));
    }

    #[test]
    fn test_edit_one_char_before_statement_no_overlap() {
        let stmt = Statement::Break(Span::new(10, 20, 1, 1));
        // Edit ends exactly where statement starts
        let edits = vec![TextEdit {
            range: (5, 10),
            new_text: "prefix".to_string(),
        }];

        // No overlap - edit is before statement
        assert!(is_statement_clean(&stmt, &edits));
    }

    #[test]
    fn test_empty_edits_all_statements_clean() {
        let stmt = Statement::Break(Span::new(10, 20, 1, 1));
        let edits: Vec<TextEdit> = vec![];

        // No edits means all statements are clean
        assert!(is_statement_clean(&stmt, &edits));
    }

    #[test]
    fn test_large_edit_affects_all_statements() {
        let stmt1 = Statement::Break(Span::new(0, 10, 1, 1));
        let stmt2 = Statement::Break(Span::new(10, 20, 2, 1));
        let stmt3 = Statement::Break(Span::new(20, 30, 3, 1));

        // Large edit spans entire document
        let edits = vec![TextEdit {
            range: (0, 100),
            new_text: "completely replaced".to_string(),
        }];

        assert!(!is_statement_clean(&stmt1, &edits));
        assert!(!is_statement_clean(&stmt2, &edits));
        assert!(!is_statement_clean(&stmt3, &edits));
    }
}
