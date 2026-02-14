//! Offset adjustment algorithm for incremental parsing

use crate::incremental::dirty::TextEdit;
use crate::span::Span;

/// Adjust a span's byte offsets to account for preceding text edits.
///
/// Iterates through edits in order and applies one of three cases per edit:
///
/// 1. **Span before edit** (`span.end <= edit_start`): No adjustment needed —
///    the edit is after this span and doesn't affect its position.
/// 2. **Span after edit** (`span.start >= edit_end`): Shift the span by the
///    edit's byte delta (positive for insertions, negative for deletions).
/// 3. **Span overlaps edit**: The span is invalidated — return `None` because
///    the statement must be re-parsed.
///
/// # Returns
/// - `Some(adjusted_span)` if the span can be safely relocated
/// - `None` if the span overlaps an edit and is invalidated
pub fn adjust_span(span: &Span, edits: &[TextEdit]) -> Option<Span> {
    let mut adjusted = *span;

    for edit in edits {
        let edit_start = edit.range.0;
        let edit_end = edit.range.1;
        let delta = edit.byte_delta();

        // Case 1: Span is before the edit - no change needed
        if adjusted.end <= edit_start {
            continue;
        }

        // Case 2: Span is after the edit - shift by delta
        if adjusted.start >= edit_end {
            adjusted = adjusted.shift(delta)?;
            continue;
        }

        // Case 3: Span overlaps with edit - invalidate
        return None;
    }

    Some(adjusted)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_adjust_span_before_edit() {
        let span = Span {
            start: 10,
            end: 20,
            line: 1,
            column: 10,
        };
        let edits = vec![TextEdit {
            range: (30, 35),
            new_text: "new".to_string(),
        }];

        let adjusted = adjust_span(&span, &edits).unwrap();
        assert_eq!(adjusted.start, 10);
        assert_eq!(adjusted.end, 20);
    }

    #[test]
    fn test_adjust_span_after_edit() {
        let span = Span {
            start: 50,
            end: 60,
            line: 3,
            column: 0,
        };
        let edits = vec![TextEdit {
            range: (10, 15),
            new_text: "inserted".to_string(), // Delta: +3
        }];

        let adjusted = adjust_span(&span, &edits).unwrap();
        assert_eq!(adjusted.start, 53);
        assert_eq!(adjusted.end, 63);
    }

    #[test]
    fn test_adjust_span_overlapping_edit() {
        let span = Span {
            start: 10,
            end: 20,
            line: 1,
            column: 10,
        };
        let edits = vec![TextEdit {
            range: (15, 25),
            new_text: "x".to_string(),
        }];

        // Should return None (invalidated)
        assert!(adjust_span(&span, &edits).is_none());
    }

    #[test]
    fn test_adjust_span_multiple_edits() {
        let span = Span {
            start: 100,
            end: 110,
            line: 5,
            column: 0,
        };
        let edits = vec![
            TextEdit {
                range: (10, 15),
                new_text: "ab".to_string(),
            }, // Delta: -3
            TextEdit {
                range: (50, 50),
                new_text: "xyz".to_string(),
            }, // Delta: +3
        ];

        let adjusted = adjust_span(&span, &edits).unwrap();
        assert_eq!(adjusted.start, 100); // -3 + 3 = 0 net change
        assert_eq!(adjusted.end, 110);
    }
}
