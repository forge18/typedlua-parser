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
pub mod di;
pub mod diagnostics;
pub mod errors;
pub mod lexer;
pub mod parser;
pub mod span;
pub mod string_interner;

use std::sync::Arc;

// Re-exports for convenience
pub use ast::{Ident, Program, Spanned};
pub use di::{DiContainer, ServiceLifetime};
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
    pub use crate::di::{DiContainer, ServiceLifetime};
    pub use crate::diagnostics::{CollectingDiagnosticHandler, Diagnostic, DiagnosticHandler};
    pub use crate::errors::LexerError;
    pub use crate::lexer::{Lexer, TemplatePart, Token, TokenKind};
    pub use crate::parser::{
        ExpressionParser, Parser, ParserError, PatternParser, StatementParser, TypeParser,
    };
    pub use crate::span::Span;
    pub use crate::string_interner::{CommonIdentifiers, StringId, StringInterner};
}

/// Parse source code into an AST using DI container
///
/// This is a convenience function that handles lexing and parsing in one call
/// using a DI container for dependency management.
///
/// # Example
///
/// ```
/// use typedlua_parser::{parse_with_container, DiContainer, ServiceLifetime, CollectingDiagnosticHandler, DiagnosticHandler};
/// use std::sync::Arc;
///
/// let source = "const x: number = 42";
/// let mut container = DiContainer::new();
/// container.register(
///     |_| Arc::new(CollectingDiagnosticHandler::new()) as Arc<dyn DiagnosticHandler>,
///     ServiceLifetime::Transient,
/// );
///
/// match parse_with_container(source, &mut container) {
///     Ok(program) => println!("Parsed {} statements", program.statements.len()),
///     Err(e) => eprintln!("Parse error: {}", e),
/// }
/// ```
pub fn parse_with_container(
    source: &str,
    container: &mut DiContainer,
) -> Result<Program, ParserError> {
    let diagnostic_handler = match container.resolve::<Arc<dyn DiagnosticHandler>>() {
        Some(handler) => handler,
        None => {
            return Err(ParserError {
                message: "No DiagnosticHandler registered in container".to_string(),
                span: Span::default(),
            });
        }
    };

    let (interner, common) = StringInterner::new_with_common_identifiers();
    let mut lexer = Lexer::new(source, diagnostic_handler.clone(), &interner);

    let tokens = lexer.tokenize().map_err(|e| ParserError {
        message: format!("Lexer error: {:?}", e),
        span: Span::default(),
    })?;

    let mut parser = Parser::new(tokens, diagnostic_handler, &interner, &common);
    parser.parse()
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
/// match parse(source, handler) {
///     Ok(program) => println!("Parsed {} statements", program.statements.len()),
///     Err(e) => eprintln!("Parse error: {}", e),
/// }
/// ```
pub fn parse(
    source: &str,
    diagnostic_handler: Arc<dyn DiagnosticHandler>,
) -> Result<Program, ParserError> {
    let mut container = DiContainer::new();
    container.register(
        move |_| diagnostic_handler.clone(),
        ServiceLifetime::Transient,
    );
    parse_with_container(source, &mut container)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    #[test]
    fn test_parse_simple() {
        let source = "const x: number = 42";
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let result = parse(source, handler);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().statements.len(), 1);
    }

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
    fn test_di_container_parse() {
        let source = "const x: number = 42";
        let mut container = DiContainer::new();
        container.register(
            |_| Arc::new(CollectingDiagnosticHandler::new()) as Arc<dyn DiagnosticHandler>,
            ServiceLifetime::Transient,
        );

        let result = parse_with_container(source, &mut container);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().statements.len(), 1);
    }

    #[test]
    fn test_di_container_singleton() {
        use crate::diagnostics::DiagnosticHandler;

        let mut container = DiContainer::new();
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        container.register(
            move |_| handler.clone() as Arc<dyn DiagnosticHandler>,
            ServiceLifetime::Singleton,
        );

        let handler1 = container.resolve::<Arc<dyn DiagnosticHandler>>();
        let handler2 = container.resolve::<Arc<dyn DiagnosticHandler>>();

        assert!(handler1.is_some());
        assert!(handler2.is_some());
        assert_eq!(
            Arc::as_ptr(handler1.as_ref().unwrap()),
            Arc::as_ptr(handler2.as_ref().unwrap())
        );
    }

    #[test]
    fn test_di_container_transient() {
        use crate::diagnostics::DiagnosticHandler;

        let mut container = DiContainer::new();
        let counter = Arc::new(std::sync::atomic::AtomicUsize::new(0));
        let counter_clone = counter.clone();
        container.register(
            move |_| {
                counter_clone.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                let handler = CollectingDiagnosticHandler::new();
                Arc::new(handler) as Arc<dyn DiagnosticHandler>
            },
            ServiceLifetime::Transient,
        );

        let handler1 = container.resolve::<Arc<dyn DiagnosticHandler>>();
        let handler2 = container.resolve::<Arc<dyn DiagnosticHandler>>();

        assert!(handler1.is_some());
        assert!(handler2.is_some());
        assert_eq!(counter.load(std::sync::atomic::Ordering::SeqCst), 2);
    }
}
