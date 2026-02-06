//! Minimal test to verify arena allocation pattern works with parser-like code.
//!
//! This tests the exact pattern we need:
//! 1. Parser struct with arena reference
//! 2. Methods that call &mut self methods
//! 3. Methods that allocate results in arena
//! 4. Loops that build up expressions incrementally
//! 5. Return arena-allocated results

use bumpalo::Bump;

// Simplified AST types
#[derive(Debug)]
struct Expr<'arena> {
    kind: ExprKind<'arena>,
}

#[derive(Debug)]
enum ExprKind<'arena> {
    Number(i64),
    Member(&'arena Expr<'arena>, &'static str),
    Call(&'arena Expr<'arena>, &'arena [Expr<'arena>]),
}

// Simplified parser
struct TestParser<'arena> {
    position: usize,
    arena: &'arena Bump,
}

impl<'arena> TestParser<'arena> {
    fn new(arena: &'arena Bump) -> Self {
        Self { position: 0, arena }
    }

    // Helper using &self (interior mutability pattern)
    #[inline]
    fn alloc<T>(&self, value: T) -> &'arena T {
        self.arena.alloc(value)
    }

    #[inline]
    fn alloc_vec<T>(&self, vec: Vec<T>) -> &'arena [T] {
        self.arena.alloc_slice_fill_iter(vec)
    }

    // Simulates parse_primary - returns owned value, not reference
    fn parse_primary(&mut self) -> Expr<'arena> {
        self.position += 1;
        Expr {
            kind: ExprKind::Number(42),
        }
    }

    // Simulates parse_arguments - calls &mut self methods
    fn parse_arguments(&mut self) -> Vec<Expr<'arena>> {
        let mut args = Vec::new();
        args.push(self.parse_primary());
        args.push(self.parse_primary());
        args
    }

    // THE CRITICAL TEST: Loop that builds expressions incrementally
    // This is the pattern that was failing with 161 errors
    fn parse_postfix(&mut self) -> Expr<'arena> {
        let mut expr = self.parse_primary();

        // Simulate a loop that parses .member and (args) repeatedly
        for i in 0..3 {
            if i % 2 == 0 {
                // Member access: expr.foo
                expr = Expr {
                    kind: ExprKind::Member(self.alloc(expr), "field"),
                };
            } else {
                // Call: expr(args)
                let args = self.parse_arguments();
                let args_slice = self.alloc_vec(args);
                expr = Expr {
                    kind: ExprKind::Call(self.alloc(expr), args_slice),
                };
            }
        }

        expr
    }

    // Test method that returns arena-allocated result
    fn parse_expression(&mut self) -> &'arena Expr<'arena> {
        let expr = self.parse_postfix();
        self.alloc(expr)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_arena_parser_pattern() {
        let arena = Bump::new();
        let mut parser = TestParser::new(&arena);

        // This should compile and work
        let expr = parser.parse_expression();

        // Verify the structure
        println!("Parsed: {:?}", expr);
        assert!(matches!(expr.kind, ExprKind::Member(_, _)));
    }

    #[test]
    fn test_multiple_expressions() {
        let arena = Bump::new();
        let mut parser = TestParser::new(&arena);

        // Parse multiple expressions - this tests that the arena
        // can be used across multiple parse calls
        let expr1 = parser.parse_expression();
        let expr2 = parser.parse_expression();

        println!("Expr1: {:?}", expr1);
        println!("Expr2: {:?}", expr2);
    }
}
