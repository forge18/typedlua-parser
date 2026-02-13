//! Dirty region calculation for incremental parsing

use crate::span::Span;
use std::cmp::max;

/// A text edit operation (from LSP or direct API)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TextEdit {
    /// Byte range being replaced (start, end)
    pub range: (u32, u32),
    /// New text to insert
    pub new_text: String,
}

impl TextEdit {
    /// Calculate the byte delta (net change in document size)
    pub fn byte_delta(&self) -> i32 {
        let old_len = (self.range.1 - self.range.0) as i32;
        let new_len = self.new_text.len() as i32;
        new_len - old_len
    }

    /// Get the ending offset after this edit
    pub fn new_end(&self) -> u32 {
        self.range.0 + self.new_text.len() as u32
    }
}

/// A region of the source text that was modified
#[derive(Debug, Clone)]
pub struct DirtyRegion {
    /// The byte range affected by edits (original coordinates)
    pub modified_range: (u32, u32),
    /// Indices of statements affected by this region
    pub affected_statements: Vec<usize>,
    /// Net byte delta for this region
    pub byte_delta: i32,
}

/// Collection of dirty regions computed from text edits
#[derive(Debug, Clone)]
pub struct DirtyRegionSet {
    pub regions: Vec<DirtyRegion>,
    pub total_delta: i32,
}

impl DirtyRegionSet {
    /// Calculate dirty regions from text edits and statement boundaries
    ///
    /// Algorithm:
    /// 1. Merge overlapping edits into contiguous regions
    /// 2. Binary search statement_ranges to find affected statements
    /// 3. Compute byte deltas for offset adjustment
    ///
    /// # Arguments
    /// * `edits` - Text edits (assumed sorted by start position)
    /// * `statement_ranges` - Statement boundaries [(index, span)]
    ///
    /// # Returns
    /// DirtyRegionSet with affected statements and byte deltas
    pub fn calculate(edits: &[TextEdit], statement_ranges: &[(usize, Span)]) -> Self {
        if edits.is_empty() {
            return DirtyRegionSet {
                regions: Vec::new(),
                total_delta: 0,
            };
        }

        // Step 1: Merge overlapping edits
        let merged_edits = Self::merge_overlapping_edits(edits);

        // Step 2: For each merged edit, find affected statements
        let mut regions = Vec::new();
        let mut cumulative_delta = 0;

        for edit in merged_edits {
            let edit_start = edit.range.0;
            let edit_end = edit.range.1;
            let delta = edit.byte_delta();

            // Binary search for first affected statement
            let first_idx = Self::find_first_affected(statement_ranges, edit_start);

            // Binary search for last affected statement
            let last_idx = Self::find_last_affected(statement_ranges, edit_end);

            let affected_statements = if let (Some(first), Some(last)) = (first_idx, last_idx) {
                (first..=last).collect()
            } else {
                Vec::new()
            };

            regions.push(DirtyRegion {
                modified_range: (edit_start, edit_end),
                affected_statements,
                byte_delta: delta,
            });

            cumulative_delta += delta;
        }

        DirtyRegionSet {
            regions,
            total_delta: cumulative_delta,
        }
    }

    /// Merge overlapping or adjacent edits into contiguous regions
    fn merge_overlapping_edits(edits: &[TextEdit]) -> Vec<TextEdit> {
        if edits.is_empty() {
            return Vec::new();
        }

        let mut sorted = edits.to_vec();
        sorted.sort_by_key(|e| e.range.0);

        let mut merged = Vec::new();
        let mut current = sorted[0].clone();

        for edit in &sorted[1..] {
            if edit.range.0 <= current.new_end() {
                // Overlapping or adjacent - merge them
                let combined_text = format!("{}{}", current.new_text, edit.new_text);
                current = TextEdit {
                    range: (current.range.0, max(current.range.1, edit.range.1)),
                    new_text: combined_text,
                };
            } else {
                // Non-overlapping - push current and start new
                merged.push(current);
                current = edit.clone();
            }
        }
        merged.push(current);

        merged
    }

    /// Binary search for first statement affected by edit
    fn find_first_affected(statement_ranges: &[(usize, Span)], edit_start: u32) -> Option<usize> {
        let idx = statement_ranges.binary_search_by(|(_, span)| {
            if span.end <= edit_start {
                std::cmp::Ordering::Less
            } else if span.start > edit_start {
                std::cmp::Ordering::Greater
            } else {
                std::cmp::Ordering::Equal
            }
        });

        match idx {
            Ok(i) => Some(statement_ranges[i].0),
            Err(i) if i < statement_ranges.len() => Some(statement_ranges[i].0),
            _ => None,
        }
    }

    /// Binary search for last statement affected by edit
    fn find_last_affected(statement_ranges: &[(usize, Span)], edit_end: u32) -> Option<usize> {
        let idx = statement_ranges.binary_search_by(|(_, span)| {
            if span.start >= edit_end {
                std::cmp::Ordering::Greater
            } else if span.end < edit_end {
                std::cmp::Ordering::Less
            } else {
                std::cmp::Ordering::Equal
            }
        });

        match idx {
            Ok(i) => Some(statement_ranges[i].0),
            Err(i) if i > 0 => Some(statement_ranges[i - 1].0),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_text_edit_delta() {
        // Insertion
        let edit = TextEdit {
            range: (10, 10),
            new_text: "hello".to_string(),
        };
        assert_eq!(edit.byte_delta(), 5);

        // Deletion
        let edit = TextEdit {
            range: (10, 20),
            new_text: String::new(),
        };
        assert_eq!(edit.byte_delta(), -10);

        // Replacement
        let edit = TextEdit {
            range: (10, 15),
            new_text: "replaced".to_string(),
        };
        assert_eq!(edit.byte_delta(), 3); // 8 - 5 = 3
    }

    #[test]
    fn test_merge_overlapping_edits() {
        let edits = vec![
            TextEdit {
                range: (10, 15),
                new_text: "abc".to_string(),
            },
            TextEdit {
                range: (13, 20),
                new_text: "def".to_string(),
            },
        ];

        let merged = DirtyRegionSet::merge_overlapping_edits(&edits);
        assert_eq!(merged.len(), 1);
        assert_eq!(merged[0].range, (10, 20));
    }

    #[test]
    fn test_calculate_dirty_regions() {
        // Statement ranges: stmt 0 @ 0-10, stmt 1 @ 10-20, stmt 2 @ 20-30
        let statement_ranges = vec![
            (
                0,
                Span {
                    start: 0,
                    end: 10,
                    line: 1,
                    column: 0,
                },
            ),
            (
                1,
                Span {
                    start: 10,
                    end: 20,
                    line: 2,
                    column: 0,
                },
            ),
            (
                2,
                Span {
                    start: 20,
                    end: 30,
                    line: 3,
                    column: 0,
                },
            ),
        ];

        // Edit in middle of statement 1
        let edits = vec![TextEdit {
            range: (15, 15),
            new_text: "x".to_string(),
        }];

        let dirty = DirtyRegionSet::calculate(&edits, &statement_ranges);

        assert_eq!(dirty.regions.len(), 1);
        assert_eq!(dirty.regions[0].affected_statements, vec![1]);
        assert_eq!(dirty.total_delta, 1);
    }

    #[test]
    fn test_edit_spanning_multiple_statements() {
        let statement_ranges = vec![
            (
                0,
                Span {
                    start: 0,
                    end: 10,
                    line: 1,
                    column: 0,
                },
            ),
            (
                1,
                Span {
                    start: 10,
                    end: 20,
                    line: 2,
                    column: 0,
                },
            ),
            (
                2,
                Span {
                    start: 20,
                    end: 30,
                    line: 3,
                    column: 0,
                },
            ),
        ];

        // Edit spans from stmt 0 to stmt 2
        let edits = vec![TextEdit {
            range: (5, 25),
            new_text: "replaced".to_string(),
        }];

        let dirty = DirtyRegionSet::calculate(&edits, &statement_ranges);

        assert_eq!(dirty.regions.len(), 1);
        assert_eq!(dirty.regions[0].affected_statements, vec![0, 1, 2]);
    }
}
