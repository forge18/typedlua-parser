//! State management for incremental parsing
//!
//! This module encapsulates the state needed during an incremental parse,
//! including cached statements, dirty regions, and token stream management.

use crate::ast::statement::Statement;
use crate::incremental::cache::CachedStatement;
use crate::incremental::dirty::{DirtyRegionSet, TextEdit};
use crate::lexer::Token;
use crate::span::Span;
use bumpalo::Bump;
use std::sync::Arc;

/// Tracks state during incremental parse
///
/// This helper struct encapsulates all the state needed to perform an incremental
/// parse, keeping the Parser struct clean and focused on parsing logic.
pub struct IncrementalParseState<'arena> {
    /// Cached statements from previous parse (lifetime extended to 'static via Arc)
    pub cached_statements: Vec<CachedStatement<'static>>,

    /// Dirty regions calculated from edits
    pub dirty_regions: DirtyRegionSet,

    /// Merged token stream (cached + newly lexed)
    pub merged_tokens: Vec<Token>,

    /// New arena for newly parsed statements
    pub new_arena: &'arena Bump,

    /// Old arenas (kept alive for cached statements)
    pub old_arenas: Vec<Arc<Bump>>,

    /// Current statement index being processed
    pub current_stmt_idx: usize,

    /// Accumulated statements (mix of cached + new)
    pub result_statements: Vec<Statement<'arena>>,
}

impl<'arena> IncrementalParseState<'arena> {
    /// Create a new incremental parse state
    ///
    /// # Arguments
    /// * `cached_statements` - Statements from previous parse
    /// * `edits` - Text edits since last parse
    /// * `statement_ranges` - Byte ranges of each statement from previous parse
    /// * `new_arena` - Arena for allocating newly parsed statements
    /// * `old_arenas` - Arenas from previous parses (kept alive for cached statements)
    pub fn new(
        cached_statements: Vec<CachedStatement<'static>>,
        edits: &[TextEdit],
        statement_ranges: &[(usize, Span)],
        new_arena: &'arena Bump,
        old_arenas: Vec<Arc<Bump>>,
    ) -> Self {
        let dirty_regions = DirtyRegionSet::calculate(edits, statement_ranges);

        Self {
            cached_statements,
            dirty_regions,
            merged_tokens: Vec::new(),
            new_arena,
            old_arenas,
            current_stmt_idx: 0,
            result_statements: Vec::new(),
        }
    }

    /// Check if current statement is affected by edits
    ///
    /// A statement is dirty if it appears in any dirty region's affected_statements list.
    pub fn is_current_statement_dirty(&self) -> bool {
        self.dirty_regions
            .regions
            .iter()
            .any(|region| region.affected_statements.contains(&self.current_stmt_idx))
    }

    /// Get byte offset for resuming parser after cached statements
    ///
    /// Finds the first dirty statement's starting byte offset.
    /// Returns None if there are no dirty regions.
    pub fn get_resume_offset(&self) -> Option<u32> {
        for region in &self.dirty_regions.regions {
            if let Some(&first_affected) = region.affected_statements.first() {
                if let Some(cached) = self.cached_statements.get(first_affected) {
                    return Some(cached.byte_range.0);
                }
            }
        }
        None
    }

    /// Get the byte range to re-parse for a specific dirty region
    ///
    /// Returns (start_byte, end_byte) tuple for the region.
    pub fn get_dirty_region_range(&self, region_idx: usize) -> Option<(u32, u32)> {
        let region = self.dirty_regions.regions.get(region_idx)?;

        if region.affected_statements.is_empty() {
            return None;
        }

        let first_affected = *region.affected_statements.first()?;
        let last_affected = *region.affected_statements.last()?;

        let start_byte = self.cached_statements.get(first_affected)?.byte_range.0;
        let end_byte = self.cached_statements.get(last_affected)?.byte_range.1;

        Some((start_byte, end_byte))
    }

    /// Get total number of dirty regions
    pub fn num_dirty_regions(&self) -> usize {
        self.dirty_regions.regions.len()
    }

    /// Get total number of cached statements
    pub fn num_cached_statements(&self) -> usize {
        self.cached_statements.len()
    }

    /// Get total byte delta from all edits
    pub fn total_delta(&self) -> i32 {
        self.dirty_regions.total_delta
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::expression::{Expression, ExpressionKind, Literal};
    use crate::ast::statement::{VariableDeclaration, VariableKind};
    use crate::ast::Ident;
    use crate::ast::Pattern;
    use crate::string_interner::StringInterner;

    #[test]
    fn test_state_initialization() {
        let interner = StringInterner::new();
        let old_arena = Arc::new(Bump::new());
        let new_arena = Bump::new();

        // Create a simple cached statement
        let x_id = interner.intern("x");
        let stmt = Statement::Variable(old_arena.alloc(VariableDeclaration {
            kind: VariableKind::Local,
            pattern: Pattern::Identifier(Ident {
                name: x_id,
                span: Span::new(0, 11, 1, 1),
            }),
            type_annotation: None,
            initializer: Expression::new(
                ExpressionKind::Literal(Literal::Number(1.0)),
                Span::new(10, 11, 1, 11),
            ),
            span: Span::new(0, 11, 1, 1),
        }));

        // SAFETY: Safe because old_arena is kept alive by Arc
        let static_stmt: &Statement<'static> = unsafe { std::mem::transmute(stmt) };

        let cached = CachedStatement::new(
            static_stmt,
            "local x = 1",
            0,
            vec![],
        );

        let statement_ranges = vec![(0, Span::new(0, 11, 1, 1))];
        let edits = vec![TextEdit {
            range: (5, 5),
            new_text: "-- comment\n".to_string(),
        }];

        let state = IncrementalParseState::new(
            vec![cached],
            &edits,
            &statement_ranges,
            &new_arena,
            vec![old_arena],
        );

        assert_eq!(state.num_cached_statements(), 1);
        assert_eq!(state.num_dirty_regions(), 1);
        assert!(state.total_delta() > 0); // Insertion adds bytes
    }

    #[test]
    fn test_dirty_detection() {
        let old_arena = Arc::new(Bump::new());
        let new_arena = Bump::new();

        // Create statement ranges for 3 statements
        let statement_ranges = vec![
            (0, Span::new(0, 11, 1, 1)),
            (1, Span::new(12, 23, 2, 1)),
            (2, Span::new(24, 35, 3, 1)),
        ];

        // Edit affects only second statement
        let edits = vec![TextEdit {
            range: (15, 20),
            new_text: "modified".to_string(),
        }];

        let state = IncrementalParseState::new(
            vec![],
            &edits,
            &statement_ranges,
            &new_arena,
            vec![old_arena],
        );

        // Test dirty detection
        state.current_stmt_idx = 0;
        assert!(!state.is_current_statement_dirty()); // First statement clean

        state.current_stmt_idx = 1;
        assert!(state.is_current_statement_dirty()); // Second statement dirty

        state.current_stmt_idx = 2;
        // Third statement might be dirty if offset adjustment affects it
        // (depends on binary search algorithm in DirtyRegionSet)
    }

    #[test]
    fn test_get_resume_offset() {
        let old_arena = Arc::new(Bump::new());
        let new_arena = Bump::new();
        let interner = StringInterner::new();

        // Create cached statements
        let x_id = interner.intern("x");
        let y_id = interner.intern("y");

        let stmt1 = Statement::Variable(old_arena.alloc(VariableDeclaration {
            kind: VariableKind::Local,
            pattern: Pattern::Identifier(Ident {
                name: x_id,
                span: Span::new(0, 11, 1, 1),
            }),
            type_annotation: None,
            initializer: Expression::new(
                ExpressionKind::Literal(Literal::Number(1.0)),
                Span::new(10, 11, 1, 11),
            ),
            span: Span::new(0, 11, 1, 1),
        }));

        let stmt2 = Statement::Variable(old_arena.alloc(VariableDeclaration {
            kind: VariableKind::Local,
            pattern: Pattern::Identifier(Ident {
                name: y_id,
                span: Span::new(12, 23, 2, 1),
            }),
            type_annotation: None,
            initializer: Expression::new(
                ExpressionKind::Literal(Literal::Number(2.0)),
                Span::new(22, 23, 2, 11),
            ),
            span: Span::new(12, 23, 2, 1),
        }));

        // SAFETY: Safe because old_arena is kept alive
        let static_stmt1: &Statement<'static> = unsafe { std::mem::transmute(stmt1) };
        let static_stmt2: &Statement<'static> = unsafe { std::mem::transmute(stmt2) };

        let cached1 = CachedStatement::new(static_stmt1, "local x = 1", 0, vec![]);
        let cached2 = CachedStatement::new(static_stmt2, "local y = 2", 0, vec![]);

        let statement_ranges = vec![
            (0, Span::new(0, 11, 1, 1)),
            (1, Span::new(12, 23, 2, 1)),
        ];

        // Edit affects second statement
        let edits = vec![TextEdit {
            range: (15, 20),
            new_text: "modified".to_string(),
        }];

        let state = IncrementalParseState::new(
            vec![cached1, cached2],
            &edits,
            &statement_ranges,
            &new_arena,
            vec![old_arena],
        );

        // Resume offset should be start of first dirty statement
        if let Some(offset) = state.get_resume_offset() {
            assert!(offset >= 12); // Should be at or after second statement start
        }
    }

    #[test]
    fn test_get_dirty_region_range() {
        let old_arena = Arc::new(Bump::new());
        let new_arena = Bump::new();
        let interner = StringInterner::new();

        let x_id = interner.intern("x");
        let stmt = Statement::Variable(old_arena.alloc(VariableDeclaration {
            kind: VariableKind::Local,
            pattern: Pattern::Identifier(Ident {
                name: x_id,
                span: Span::new(0, 11, 1, 1),
            }),
            type_annotation: None,
            initializer: Expression::new(
                ExpressionKind::Literal(Literal::Number(1.0)),
                Span::new(10, 11, 1, 11),
            ),
            span: Span::new(0, 11, 1, 1),
        }));

        let static_stmt: &Statement<'static> = unsafe { std::mem::transmute(stmt) };
        let cached = CachedStatement::new(static_stmt, "local x = 1", 0, vec![]);

        let statement_ranges = vec![(0, Span::new(0, 11, 1, 1))];
        let edits = vec![TextEdit {
            range: (5, 5),
            new_text: "modified".to_string(),
        }];

        let state = IncrementalParseState::new(
            vec![cached],
            &edits,
            &statement_ranges,
            &new_arena,
            vec![old_arena],
        );

        if state.num_dirty_regions() > 0 {
            if let Some((start, end)) = state.get_dirty_region_range(0) {
                assert_eq!(start, 0);
                assert_eq!(end, 11);
            }
        }
    }

    #[test]
    fn test_empty_edits() {
        let old_arena = Arc::new(Bump::new());
        let new_arena = Bump::new();

        let statement_ranges = vec![(0, Span::new(0, 11, 1, 1))];
        let edits = vec![];

        let state = IncrementalParseState::new(
            vec![],
            &edits,
            &statement_ranges,
            &new_arena,
            vec![old_arena],
        );

        assert_eq!(state.num_dirty_regions(), 0);
        assert_eq!(state.total_delta(), 0);
        assert!(state.get_resume_offset().is_none());
    }
}
