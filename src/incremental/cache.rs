//! Cached statement structure with hash validation

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
    /// Which arena generation owns this statement
    pub arena_generation: usize,
    /// Cached token stream for this statement
    pub tokens: Vec<Token>,
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
            arena_generation,
            tokens,
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

/// Incremental parse tree with multi-generation arena support
pub struct IncrementalParseTree<'arena> {
    /// Document version this tree corresponds to
    pub version: u64,
    /// Cached statements (may point to different arena generations)
    pub statements: Vec<CachedStatement<'arena>>,
    /// Hash of entire source file (for quick full-file validation)
    pub source_hash: u64,
    /// Arena generations (newest last, oldest first)
    /// Kept alive via Arc; dropped when no statements reference them
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

    /// Garbage collect old arena generations that are no longer referenced
    pub fn collect_garbage(&mut self) {
        // Track which generations are still in use
        let mut used_generations = HashSet::new();
        for stmt in &self.statements {
            used_generations.insert(stmt.arena_generation);
        }

        // Keep only referenced arenas
        let mut new_arenas = Vec::new();
        for (gen, arena) in self.arenas.iter().enumerate() {
            if used_generations.contains(&gen) {
                new_arenas.push(arena.clone());
            }
        }

        self.arenas = new_arenas;
    }
}

/// Helper function to extract span from a Statement
/// This matches each variant and extracts its span field
fn get_statement_span(statement: &Statement) -> Span {
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
        Statement::Break(span) => *span,    // Break is just a Span
        Statement::Continue(span) => *span, // Continue is just a Span
        Statement::Block(block) => block.span,
        Statement::Label(label) => label.span,
        Statement::Goto(goto) => goto.span,
        Statement::Expression(expr) => expr.span,
        Statement::Throw(stmt) => stmt.span,
        Statement::Try(stmt) => stmt.span,
        Statement::Rethrow(span) => *span, // Rethrow is just a Span
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
}
