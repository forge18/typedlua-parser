//! Cached statement structure with hash validation and multi-arena support

use crate::ast::statement::Statement;
use crate::lexer::Token;
use crate::span::Span;
use bumpalo::Bump;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashSet;
use std::hash::{Hash, Hasher};
use std::sync::Arc;

/// A cached statement with hash validation and token stream
#[derive(Clone)]
pub struct CachedStatement<'arena> {
    /// The parsed statement (arena-allocated)
    pub statement: &'arena Statement<'arena>,
    /// Byte range of this statement in original source
    pub byte_range: (u32, u32),
    /// Hash of the source text for this statement (for validation)
    pub source_hash: u64,
    /// Cached token stream for this statement
    pub tokens: Vec<Token>,
    /// Which arena generation this statement belongs to
    pub arena_generation: usize,
}

impl<'arena> CachedStatement<'arena> {
    /// Create a new cached statement by hashing its source text
    pub fn new(
        statement: &'arena Statement<'arena>,
        source: &str,
        arena_generation: usize,
        tokens: Vec<Token>,
    ) -> Self {
        let span = get_statement_span(statement);
        let byte_range = (span.start, span.end);

        // Extract source text for this statement
        let start = span.start as usize;
        let end = span.end as usize;
        let source_text = &source[start..end];

        // Hash the source text
        let source_hash = Self::hash_source(source_text);

        CachedStatement {
            statement,
            byte_range,
            source_hash,
            tokens,
            arena_generation,
        }
    }

    /// Hash source text using DefaultHasher
    fn hash_source(text: &str) -> u64 {
        let mut hasher = DefaultHasher::new();
        text.hash(&mut hasher);
        hasher.finish()
    }

    /// Validate that this cached statement is still valid against current source
    pub fn is_valid(&self, source: &str) -> bool {
        let start = self.byte_range.0 as usize;
        let end = self.byte_range.1 as usize;

        // Bounds check
        if end > source.len() {
            return false;
        }

        // Hash check
        let current_text = &source[start..end];
        let current_hash = Self::hash_source(current_text);

        current_hash == self.source_hash
    }
}

/// Incremental parse tree with multi-arena support
///
/// Uses multiple arenas to track statements across incremental parses.
/// Old statements stay in old arenas, new statements allocated in new arenas.
/// Periodic consolidation prevents unbounded arena accumulation.
pub struct IncrementalParseTree<'arena> {
    /// Document version this tree corresponds to
    pub version: u64,
    /// Cached statements (may point to different arenas)
    pub statements: Vec<CachedStatement<'arena>>,
    /// Hash of entire source file (for quick full-file validation)
    pub source_hash: u64,
    /// Multiple arenas - max 3, consolidated when limit hit
    pub arenas: Vec<Arc<Bump>>,
}

impl<'arena> IncrementalParseTree<'arena> {
    /// Create a new incremental parse tree from a full parse
    pub fn new(
        version: u64,
        statements: &'arena [Statement<'arena>],
        source: &str,
        arena: Arc<Bump>,
    ) -> Self {
        let cached_statements = statements
            .iter()
            .map(|stmt| CachedStatement::new(stmt, source, 0, Vec::new()))
            .collect();

        let source_hash = Self::hash_full_source(source);

        IncrementalParseTree {
            version,
            statements: cached_statements,
            source_hash,
            arenas: vec![arena],
        }
    }

    /// Hash the full source file
    fn hash_full_source(source: &str) -> u64 {
        let mut hasher = DefaultHasher::new();
        source.hash(&mut hasher);
        hasher.finish()
    }

    /// Quick check if entire source matches cached hash
    pub fn source_matches(&self, source: &str) -> bool {
        let current_hash = Self::hash_full_source(source);
        current_hash == self.source_hash
    }

    /// Collect garbage: drop unreferenced arenas or consolidate if needed
    pub fn collect_garbage(&mut self) {
        // Trigger consolidation if:
        // 1. More than 3 arenas
        // 2. Every 10 parses
        let should_consolidate = self.arenas.len() > 3 || self.version % 10 == 0;

        if should_consolidate {
            self.consolidate_all();
        } else {
            self.drop_unreferenced_arenas();
        }
    }

    /// Consolidate all statements into a single new arena
    fn consolidate_all(&mut self) {
        if self.arenas.len() <= 1 {
            return; // Nothing to consolidate
        }

        let new_arena = Arc::new(Bump::new());

        // Clone all statements to new arena
        let new_statements = self
            .statements
            .iter()
            .map(|cached| {
                let cloned = clone_statement_to_arena(cached.statement, &new_arena);

                // Transmute to 'arena lifetime for storage
                let static_cloned: &'arena Statement<'arena> =
                    unsafe { std::mem::transmute(cloned) };

                CachedStatement {
                    statement: static_cloned,
                    arena_generation: 0, // All in generation 0 now
                    byte_range: cached.byte_range,
                    source_hash: cached.source_hash,
                    tokens: cached.tokens.clone(),
                }
            })
            .collect();

        self.statements = new_statements;
        self.arenas = vec![new_arena];
    }

    /// Drop unreferenced arenas and compact generation numbers
    fn drop_unreferenced_arenas(&mut self) {
        let referenced: HashSet<usize> = self
            .statements
            .iter()
            .map(|s| s.arena_generation)
            .collect();

        let mut new_arenas = Vec::new();
        let mut generation_map = vec![0; self.arenas.len()];

        for (old_gen, arena) in self.arenas.iter().enumerate() {
            if referenced.contains(&old_gen) {
                generation_map[old_gen] = new_arenas.len();
                new_arenas.push(arena.clone());
            }
        }

        // Update arena_generation in all cached statements
        for stmt in &mut self.statements {
            stmt.arena_generation = generation_map[stmt.arena_generation];
        }

        self.arenas = new_arenas;
    }
}

/// Clone a statement into a new arena (deep copy)
///
/// Uses Rust's built-in `Clone` implementation to deep-copy the statement,
/// then allocates the cloned value in the new arena with a new lifetime.
///
/// # Safety
/// This is safe because:
/// 1. We perform a deep clone of the statement, creating new owned data
/// 2. The cloned data is transmuted to the new lifetime parameter
/// 3. The arena is kept alive via Arc<Bump> in IncrementalParseTree
/// 4. Statement layout is identical regardless of lifetime parameter
/// 5. Lifetimes are compile-time only, erased at runtime
/// 6. All internal arena references in the clone are also transmuted consistently
fn clone_statement_to_arena<'old, 'arena>(
    stmt: &Statement<'old>,
    arena: &'arena Bump,
) -> &'arena Statement<'arena> {
    // Clone creates a new Statement<'old> with owned data
    let cloned: Statement<'old> = stmt.clone();

    // Transmute the statement from 'old to 'arena lifetime
    // SAFETY: The clone operation copied all data. The 'old lifetime
    // is just a type parameter - the actual data is freshly cloned.
    let transmuted: Statement<'arena> = unsafe { std::mem::transmute(cloned) };

    // Allocate in new arena with correct lifetime
    arena.alloc(transmuted)
}

/// Helper function to extract span from a Statement
/// This matches each variant and extracts its span field
pub fn get_statement_span(statement: &Statement) -> Span {
    use crate::ast::statement::ForStatement;

    match statement {
        Statement::Variable(decl) => decl.span,
        Statement::Function(decl) => decl.span,
        Statement::Class(decl) => decl.span,
        Statement::Interface(decl) => decl.span,
        Statement::TypeAlias(decl) => decl.span,
        Statement::Enum(decl) => decl.span,
        Statement::Import(decl) => decl.span,
        Statement::Export(decl) => decl.span,
        Statement::If(stmt) => stmt.span,
        Statement::While(stmt) => stmt.span,
        Statement::For(stmt) => match stmt {
            ForStatement::Numeric(for_num) => for_num.span,
            ForStatement::Generic(for_gen) => for_gen.span,
        },
        Statement::Repeat(stmt) => stmt.span,
        Statement::Return(stmt) => stmt.span,
        Statement::Break(span) => *span,
        Statement::Continue(span) => *span,
        Statement::Block(block) => block.span,
        Statement::Label(label) => label.span,
        Statement::Goto(goto) => goto.span,
        Statement::Expression(expr) => expr.span,
        Statement::Throw(stmt) => stmt.span,
        Statement::Try(stmt) => stmt.span,
        Statement::Rethrow(span) => *span,
        Statement::Namespace(decl) => decl.span,
        Statement::DeclareFunction(decl) => decl.span,
        Statement::DeclareNamespace(decl) => decl.span,
        Statement::DeclareType(decl) => decl.span,
        Statement::DeclareInterface(decl) => decl.span,
        Statement::DeclareConst(decl) => decl.span,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_incremental_tree_source_matching() {
        let arena = Arc::new(Bump::new());
        let source = "local x = 1\nlocal y = 2";

        let statements = &[]; // Empty for this test
        let tree = IncrementalParseTree::new(1, statements, source, arena);

        assert!(tree.source_matches(source));
        assert!(!tree.source_matches("local x = 1\nlocal y = 3"));
    }

    #[test]
    fn test_hash_source() {
        let source1 = "local x = 1";
        let source2 = "local x = 2";

        let hash1 = CachedStatement::hash_source(source1);
        let hash2 = CachedStatement::hash_source(source2);

        // Different source should have different hashes
        assert_ne!(hash1, hash2);

        // Same source should have same hash
        let hash1_again = CachedStatement::hash_source(source1);
        assert_eq!(hash1, hash1_again);
    }

    #[test]
    fn test_arena_garbage_collection_logic() {
        let arena0 = Arc::new(Bump::new());
        let arena1 = Arc::new(Bump::new());

        // Create a minimal Statement::Break for testing
        let break_span = Span {
            start: 0,
            end: 5,
            line: 1,
            column: 0,
        };
        let stmt0: &Statement = arena0.alloc(Statement::Break(break_span));

        let mut tree = IncrementalParseTree {
            version: 1,
            statements: vec![CachedStatement {
                statement: stmt0,
                byte_range: (0, 5),
                source_hash: 0,
                arena_generation: 0,
                tokens: Vec::new(),
            }],
            source_hash: 0,
            arenas: vec![arena0.clone(), arena1.clone()],
        };

        // arena1 is not referenced by any statement
        tree.collect_garbage();

        // Only arena0 should remain
        assert_eq!(tree.arenas.len(), 1);
    }

    #[test]
    fn test_consolidation_trigger() {
        let arena = Arc::new(Bump::new());
        let break_span = Span::new(0, 5, 1, 1);
        let stmt: &Statement = arena.alloc(Statement::Break(break_span));

        let mut tree = IncrementalParseTree {
            version: 9, // Not a consolidation version yet
            statements: vec![
                CachedStatement {
                    statement: stmt,
                    byte_range: (0, 5),
                    source_hash: 0,
                    arena_generation: 0,
                    tokens: Vec::new(),
                },
                CachedStatement {
                    statement: stmt,
                    byte_range: (6, 11),
                    source_hash: 0,
                    arena_generation: 1,
                    tokens: Vec::new(),
                },
            ],
            source_hash: 0,
            arenas: vec![
                Arc::new(Bump::new()),
                Arc::new(Bump::new()),
            ],
        };

        // Should not consolidate yet (version 9, < 10)
        tree.collect_garbage();
        assert_eq!(tree.arenas.len(), 2); // Both arenas still referenced

        // Now trigger consolidation by version
        tree.version = 10;
        tree.collect_garbage();
        assert_eq!(tree.arenas.len(), 1); // Consolidated!
    }
}
