//! Span validation for incremental parsing
//!
//! With multi-arena approach, we don't need deep cloning.
//! We just adjust span offsets in-place on cached statement references.

use crate::ast::statement::Statement;
use crate::incremental::cache::get_statement_span;
use crate::incremental::dirty::TextEdit;
use crate::span::Span;

/// Check if a statement's span overlaps with any edit
///
/// Returns `true` if the statement is clean (can be reused).
/// Returns `false` if the statement overlaps an edit (must re-parse).
///
/// # Arguments
/// * `stmt` - The statement to validate
/// * `edits` - The text edits since last parse
pub fn is_statement_clean(stmt: &Statement, edits: &[TextEdit]) -> bool {
    let span = get_statement_span(stmt);
    !overlaps_any_edit(&span, edits)
}

/// Check if a span overlaps with any edit region
fn overlaps_any_edit(span: &Span, edits: &[TextEdit]) -> bool {
    for edit in edits {
        let edit_start = edit.range.0;
        let edit_end = edit.range.1;

        // Check for overlap: (span.start < edit_end && span.end > edit_start)
        if span.start < edit_end && span.end > edit_start {
            return true;
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
}
