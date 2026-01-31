//! TypedLua Parser
//!
//! A parser for TypedLua, a statically-typed superset of Lua with TypeScript-inspired features.
//!
//! # Features
//!
//! - Full lexer with source mapping support
//! - Parser for TypedLua syntax including:
//!   - Classes, interfaces, enums
//!   - Generics and type parameters
//!   - Pattern matching
//!   - Arrow functions
//!   - Optional chaining
//!   - And more!
//!
//! # Example
//!
//! ```
//! use typedlua_parser::prelude::*;
//! use std::sync::Arc;
//!
//! let source = r#"
//!     const greeting: string = "Hello, TypedLua!"
//!     function greet(name: string): string
//!         return greeting .. " " .. name
//!     end
//! "#;
//!
//! let handler = Arc::new(CollectingDiagnosticHandler::new());
//! let (interner, common) = StringInterner::new_with_common_identifiers();
//! let mut lexer = Lexer::new(source, handler.clone(), &interner);
//! let tokens = lexer.tokenize().unwrap();
//! let mut parser = Parser::new(tokens, handler, &interner, &common);
//! let program = parser.parse().unwrap();
//!
//! println!("Parsed {} statements", program.statements.len());
//! ```

pub mod ast;
pub mod diagnostics;
pub mod errors;
pub mod lexer;
pub mod parser;
pub mod span;
pub mod string_interner;

// Re-exports for convenience
pub use ast::{Ident, Program, Spanned};
pub use diagnostics::{CollectingDiagnosticHandler, Diagnostic, DiagnosticHandler};
pub use errors::LexerError;
pub use lexer::{Lexer, TemplatePart, Token, TokenKind};
pub use parser::{
    ExpressionParser, Parser, ParserError, PatternParser, StatementParser, TypeParser,
};
pub use span::Span;
pub use string_interner::{CommonIdentifiers, StringId, StringInterner};

/// Prelude module for convenient imports
pub mod prelude {
    pub use crate::ast::{
        expression::*, pattern::*, statement::*, types::*, Ident, Program, Spanned,
    };
    pub use crate::diagnostics::{CollectingDiagnosticHandler, Diagnostic, DiagnosticHandler};
    pub use crate::errors::LexerError;
    pub use crate::lexer::{Lexer, TemplatePart, Token, TokenKind};
    pub use crate::parser::{
        ExpressionParser, Parser, ParserError, PatternParser, StatementParser, TypeParser,
    };
    pub use crate::span::Span;
    pub use crate::string_interner::{CommonIdentifiers, StringId, StringInterner};
}

/// Parse source code into an AST
///
/// This is a convenience function that handles lexing and parsing in one call.
///
/// # Example
///
/// ```
/// use typedlua_parser::{parse, CollectingDiagnosticHandler};
/// use std::sync::Arc;
///
/// let source = "const x: number = 42";
/// let handler = Arc::new(CollectingDiagnosticHandler::new());
///
/// match parse(source, handler.clone()) {
///     Ok(program) => println!("Parsed {} statements", program.statements.len()),
///     Err(e) => eprintln!("Parse error: {}", e),
/// }
/// ```
pub fn parse(
    source: &str,
    diagnostic_handler: std::sync::Arc<dyn DiagnosticHandler>,
) -> Result<Program, ParserError> {
    let (interner, common) = StringInterner::new_with_common_identifiers();
    let mut lexer = Lexer::new(source, diagnostic_handler.clone(), &interner);

    let tokens = lexer.tokenize().map_err(|e| ParserError {
        message: format!("Lexer error: {:?}", e),
        span: Span::default(),
    })?;

    let mut parser = Parser::new(tokens, diagnostic_handler, &interner, &common);
    parser.parse()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    #[test]
    fn test_parse_function() {
        let source = r#"
            function greet(name: string): string
                return "Hello, " .. name
            end
        "#;
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let result = parse(source, handler);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_interface() {
        let source = r#"
            interface Point {
                x: number,
                y: number
            }
        "#;
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let result = parse(source, handler);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_class() {
        let source = r#"
            class Counter {
                count: number = 0

                increment(): number {
                    self.count = self.count + 1
                    return self.count
                }
            }
        "#;
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let result = parse(source, handler);
        assert!(result.is_ok());
    }

    #[test]
    fn test_lexer_basic() {
        let source = "const x = 42";
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let interner = StringInterner::new();
        let mut lexer = Lexer::new(source, handler, &interner);
        let tokens = lexer.tokenize().unwrap();

        assert!(matches!(tokens[0].kind, TokenKind::Const));
        assert!(matches!(tokens[1].kind, TokenKind::Identifier(_)));
        assert!(matches!(tokens[2].kind, TokenKind::Equal));
        assert!(matches!(tokens[3].kind, TokenKind::Number(_)));
        assert!(matches!(tokens[4].kind, TokenKind::Eof));
    }

    #[test]
    fn test_parse_gsub_function() {
        let source = r#"declare namespace string {
  export function gsub(s: string, pattern: string, repl: string | table | ((match: string) -> string), n?: number): (string, number)
}"#;
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let result = parse(source, handler.clone());

        let diagnostics = handler.get_diagnostics();
        if !diagnostics.is_empty() {
            println!("Parser reported {} diagnostics:", diagnostics.len());
            for diag in diagnostics.iter() {
                println!("  {:?}: {}", diag.level, diag.message);
            }
        }

        assert!(
            result.is_ok(),
            "Failed to parse string namespace: {:?}",
            result.err()
        );

        let program = result.unwrap();
        println!("Parsed {} statements", program.statements.len());
        for (i, stmt) in program.statements.iter().enumerate() {
            println!("Statement {}: {:?}", i, std::mem::discriminant(stmt));
        }
    }

    #[test]
    fn test_parse_lua54_stdlib() {
        let source = include_str!("../../typedlua-core/src/stdlib/lua54.d.tl");
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let result = parse(source, handler.clone());

        // Check for any errors that were reported during parsing
        let diagnostics = handler.get_diagnostics();
        if !diagnostics.is_empty() {
            println!("Parser reported {} diagnostics:", diagnostics.len());
            for (i, diag) in diagnostics.iter().enumerate() {
                println!("  {}: {:?} - {}", i, diag.level, diag.message);
            }
        }

        assert!(
            result.is_ok(),
            "Failed to parse lua54.d.tl: {:?}",
            result.err()
        );
        let program = result.unwrap();
        println!(
            "Parsed {} statements from lua54.d.tl",
            program.statements.len()
        );
        for (i, stmt) in program.statements.iter().enumerate() {
            println!("Statement {}: {:?}", i, std::mem::discriminant(stmt));
        }

        // We should have at least 9 namespaces
        assert!(
            program.statements.len() >= 7,
            "Expected at least 7 statements, got {}",
            program.statements.len()
        );
    }
}
