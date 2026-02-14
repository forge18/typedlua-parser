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

    /// Calculate ratio of affected statements to total statements
    ///
    /// This is used by heuristics to decide if incremental parsing is worthwhile.
    /// For example, if >50% of statements are affected, a full reparse is often faster.
    ///
    /// # Arguments
    /// * `total_statements` - Total number of statements in the document
    ///
    /// # Returns
    /// Ratio between 0.0 and 1.0 representing the fraction of statements affected
    pub fn affected_ratio(&self, total_statements: usize) -> f64 {
        if total_statements == 0 {
            return 0.0;
        }

        // Collect unique affected statement indices
        let mut affected = std::collections::HashSet::<usize>::new();
        for region in &self.regions {
            affected.extend(region.affected_statements.iter().copied());
        }

        affected.len() as f64 / total_statements as f64
    }

    /// Check if incremental parsing is worthwhile based on affected ratio
    ///
    /// This is a helper for parse strategy heuristics.
    ///
    /// # Arguments
    /// * `total_statements` - Total number of statements in the document
    /// * `threshold` - Maximum acceptable ratio (typically 0.5 for 50%)
    ///
    /// # Returns
    /// `true` if incremental parsing should be used, `false` if full reparse is better
    pub fn should_use_incremental(&self, total_statements: usize, threshold: f64) -> bool {
        self.affected_ratio(total_statements) < threshold
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

    // --- NEW TESTS FOR EDGE CASES ---

    #[test]
    fn test_edit_at_file_start() {
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
        ];

        // Edit at byte 0 (start of file)
        let edits = vec![TextEdit {
            range: (0, 0),
            new_text: "prefix ".to_string(),
        }];

        let dirty = DirtyRegionSet::calculate(&edits, &statement_ranges);

        assert_eq!(dirty.regions.len(), 1);
        assert_eq!(dirty.regions[0].affected_statements, vec![0]);
        assert_eq!(dirty.total_delta, 7);
    }

    #[test]
    fn test_edit_at_file_end() {
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
        ];

        // Edit at byte 20 (end of last statement)
        let edits = vec![TextEdit {
            range: (20, 20),
            new_text: "\nsuffix".to_string(),
        }];

        let dirty = DirtyRegionSet::calculate(&edits, &statement_ranges);

        // Edit after all statements - no statements affected
        assert_eq!(dirty.regions.len(), 1);
        assert_eq!(dirty.regions[0].affected_statements, Vec::<usize>::new());
        assert_eq!(dirty.total_delta, 7);
    }

    #[test]
    fn test_edit_deletes_entire_statement() {
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

        // Delete entire statement 1
        let edits = vec![TextEdit {
            range: (10, 20),
            new_text: String::new(),
        }];

        let dirty = DirtyRegionSet::calculate(&edits, &statement_ranges);

        assert_eq!(dirty.regions.len(), 1);
        assert_eq!(dirty.regions[0].affected_statements, vec![1]);
        assert_eq!(dirty.total_delta, -10);
    }

    #[test]
    fn test_edit_deletes_multiple_statements() {
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

        // Delete statements 0 and 1 completely
        let edits = vec![TextEdit {
            range: (0, 20),
            new_text: String::new(),
        }];

        let dirty = DirtyRegionSet::calculate(&edits, &statement_ranges);

        assert_eq!(dirty.regions.len(), 1);
        assert_eq!(dirty.regions[0].affected_statements, vec![0, 1]);
        assert_eq!(dirty.total_delta, -20);
    }

    // --- NEW TESTS FOR MULTIPLE EDITS ---

    #[test]
    fn test_multiple_non_overlapping_edits() {
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

        // Edit in stmt 0 and stmt 2 (not overlapping)
        let edits = vec![
            TextEdit {
                range: (5, 5),
                new_text: "a".to_string(),
            },
            TextEdit {
                range: (25, 25),
                new_text: "b".to_string(),
            },
        ];

        let dirty = DirtyRegionSet::calculate(&edits, &statement_ranges);

        assert_eq!(dirty.regions.len(), 2);
        assert_eq!(dirty.regions[0].affected_statements, vec![0]);
        assert_eq!(dirty.regions[1].affected_statements, vec![2]);
        assert_eq!(dirty.total_delta, 2);
    }

    #[test]
    fn test_multiple_overlapping_edits_merge() {
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
        ];

        // Two overlapping edits in stmt 0
        let edits = vec![
            TextEdit {
                range: (3, 5),
                new_text: "abc".to_string(),
            },
            TextEdit {
                range: (4, 7),
                new_text: "def".to_string(),
            },
        ];

        let dirty = DirtyRegionSet::calculate(&edits, &statement_ranges);

        // Should merge into one region
        assert_eq!(dirty.regions.len(), 1);
        assert_eq!(dirty.regions[0].affected_statements, vec![0]);
    }

    #[test]
    fn test_adjacent_edits_merge() {
        let statement_ranges = vec![(
            0,
            Span {
                start: 0,
                end: 20,
                line: 1,
                column: 0,
            },
        )];

        // Two adjacent insertions
        let edits = vec![
            TextEdit {
                range: (5, 5),
                new_text: "a".to_string(),
            },
            TextEdit {
                range: (6, 6), // Adjacent after first edit
                new_text: "b".to_string(),
            },
        ];

        let dirty = DirtyRegionSet::calculate(&edits, &statement_ranges);

        // Should merge because they're adjacent
        assert_eq!(dirty.regions.len(), 1);
        assert_eq!(dirty.total_delta, 2);
    }

    #[test]
    fn test_three_way_edit_merge() {
        let statement_ranges = vec![(
            0,
            Span {
                start: 0,
                end: 30,
                line: 1,
                column: 0,
            },
        )];

        // Three overlapping edits
        let edits = vec![
            TextEdit {
                range: (5, 10),
                new_text: "a".to_string(),
            },
            TextEdit {
                range: (8, 12),
                new_text: "b".to_string(),
            },
            TextEdit {
                range: (11, 15),
                new_text: "c".to_string(),
            },
        ];

        let dirty = DirtyRegionSet::calculate(&edits, &statement_ranges);

        // All should merge into one region
        assert_eq!(dirty.regions.len(), 1);
        assert_eq!(dirty.regions[0].affected_statements, vec![0]);
    }

    // --- NEW TESTS FOR BOUNDARY CONDITIONS ---

    #[test]
    fn test_edit_ends_at_statement_boundary() {
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
        ];

        // Edit ends exactly at boundary between stmt 0 and stmt 1
        let edits = vec![TextEdit {
            range: (5, 10),
            new_text: "x".to_string(),
        }];

        let dirty = DirtyRegionSet::calculate(&edits, &statement_ranges);

        assert_eq!(dirty.regions.len(), 1);
        assert_eq!(dirty.regions[0].affected_statements, vec![0]);
    }

    #[test]
    fn test_edit_starts_at_statement_boundary() {
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
        ];

        // Edit starts exactly at boundary between stmt 0 and stmt 1
        let edits = vec![TextEdit {
            range: (10, 15),
            new_text: "x".to_string(),
        }];

        let dirty = DirtyRegionSet::calculate(&edits, &statement_ranges);

        assert_eq!(dirty.regions.len(), 1);
        assert_eq!(dirty.regions[0].affected_statements, vec![1]);
    }

    #[test]
    fn test_zero_length_edit_insertion() {
        let statement_ranges = vec![(
            0,
            Span {
                start: 0,
                end: 10,
                line: 1,
                column: 0,
            },
        )];

        // Pure insertion (zero-length range)
        let edits = vec![TextEdit {
            range: (5, 5),
            new_text: "inserted".to_string(),
        }];

        let dirty = DirtyRegionSet::calculate(&edits, &statement_ranges);

        assert_eq!(dirty.regions.len(), 1);
        assert_eq!(dirty.regions[0].affected_statements, vec![0]);
        assert_eq!(dirty.total_delta, 8);
    }

    #[test]
    fn test_full_statement_replacement() {
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
        ];

        // Replace entire statement 1 with different text
        let edits = vec![TextEdit {
            range: (10, 20),
            new_text: "const y: number = 99".to_string(),
        }];

        let dirty = DirtyRegionSet::calculate(&edits, &statement_ranges);

        assert_eq!(dirty.regions.len(), 1);
        assert_eq!(dirty.regions[0].affected_statements, vec![1]);
        assert_eq!(dirty.total_delta, 10); // 20 new - 10 old
    }

    // --- NEW TESTS FOR RATIO CALCULATION ---

    #[test]
    fn test_affected_ratio_zero_statements() {
        let dirty = DirtyRegionSet {
            regions: vec![],
            total_delta: 0,
        };

        assert_eq!(dirty.affected_ratio(0), 0.0);
    }

    #[test]
    fn test_affected_ratio_single_statement() {
        let dirty = DirtyRegionSet {
            regions: vec![DirtyRegion {
                modified_range: (5, 10),
                affected_statements: vec![0],
                byte_delta: 5,
            }],
            total_delta: 5,
        };

        assert_eq!(dirty.affected_ratio(3), 1.0 / 3.0);
    }

    #[test]
    fn test_affected_ratio_multiple_statements() {
        let dirty = DirtyRegionSet {
            regions: vec![DirtyRegion {
                modified_range: (5, 25),
                affected_statements: vec![0, 1, 2],
                byte_delta: 10,
            }],
            total_delta: 10,
        };

        // 3 affected out of 10 total = 0.3
        assert_eq!(dirty.affected_ratio(10), 0.3);
    }

    #[test]
    fn test_affected_ratio_duplicate_statements() {
        // Two regions affecting the same statement
        let dirty = DirtyRegionSet {
            regions: vec![
                DirtyRegion {
                    modified_range: (5, 10),
                    affected_statements: vec![0, 1],
                    byte_delta: 5,
                },
                DirtyRegion {
                    modified_range: (15, 20),
                    affected_statements: vec![1, 2],
                    byte_delta: 5,
                },
            ],
            total_delta: 10,
        };

        // Unique affected: {0, 1, 2} = 3 out of 5 = 0.6
        assert_eq!(dirty.affected_ratio(5), 0.6);
    }

    #[test]
    fn test_should_use_incremental_below_threshold() {
        let dirty = DirtyRegionSet {
            regions: vec![DirtyRegion {
                modified_range: (5, 10),
                affected_statements: vec![0],
                byte_delta: 5,
            }],
            total_delta: 5,
        };

        // 1 affected out of 10 = 0.1, threshold 0.5 -> should use incremental
        assert!(dirty.should_use_incremental(10, 0.5));
    }

    #[test]
    fn test_should_use_incremental_above_threshold() {
        let dirty = DirtyRegionSet {
            regions: vec![DirtyRegion {
                modified_range: (5, 25),
                affected_statements: vec![0, 1, 2, 3, 4, 5],
                byte_delta: 20,
            }],
            total_delta: 20,
        };

        // 6 affected out of 10 = 0.6, threshold 0.5 -> should NOT use incremental
        assert!(!dirty.should_use_incremental(10, 0.5));
    }

    #[test]
    fn test_should_use_incremental_at_threshold() {
        let dirty = DirtyRegionSet {
            regions: vec![DirtyRegion {
                modified_range: (5, 25),
                affected_statements: vec![0, 1, 2, 3, 4],
                byte_delta: 20,
            }],
            total_delta: 20,
        };

        // 5 affected out of 10 = 0.5, threshold 0.5 -> should NOT use incremental (not strictly less)
        assert!(!dirty.should_use_incremental(10, 0.5));
    }
}
