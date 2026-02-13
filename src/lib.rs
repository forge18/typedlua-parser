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
//! use luanext_parser::prelude::*;
//! use luanext_parser::{DiContainer, ServiceLifetime, Bump};
//! use std::sync::Arc;
//!
//! let source = r#"
//!     const greeting: string = "Hello, TypedLua!"
//!     function greet(name: string): string
//!         return greeting .. " " .. name
//!     end
//! "#;
//!
//! let mut container = DiContainer::new();
//! container.register(
//!     |_| Arc::new(CollectingDiagnosticHandler::new()) as Arc<dyn DiagnosticHandler>,
//!     ServiceLifetime::Transient,
//! );
//!
//! let arena = Bump::new();
//! let handler = container.resolve::<Arc<dyn DiagnosticHandler>>().unwrap();
//! let (interner, common) = StringInterner::new_with_common_identifiers();
//! let mut lexer = Lexer::new(source, handler.clone(), &interner);
//! let tokens = lexer.tokenize().unwrap();
//! let mut parser = Parser::new(tokens, handler, &interner, &common, &arena);
//! let program = parser.parse().unwrap();
//!
//! println!("Parsed {} statements", program.statements.len());
//! ```

pub mod ast;
pub mod di;
pub mod diagnostics;
pub mod errors;
pub mod incremental;
pub mod lexer;
pub mod parser;
pub mod span;
pub mod string_interner;

use std::sync::Arc;

// Re-exports for convenience
pub use ast::{Ident, Program, Spanned};
pub use bumpalo::Bump;
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
    pub use bumpalo::Bump;
}

/// Parse source code into an AST using DI container
///
/// This is a convenience function that handles lexing and parsing in one call
/// using a DI container for dependency management.
///
/// # Example
///
/// ```
/// use luanext_parser::{parse_with_container, DiContainer, ServiceLifetime, CollectingDiagnosticHandler, DiagnosticHandler, Bump};
/// use std::sync::Arc;
///
/// let source = "const x: number = 42";
/// let mut container = DiContainer::new();
/// container.register(
///     |_| Arc::new(CollectingDiagnosticHandler::new()) as Arc<dyn DiagnosticHandler>,
///     ServiceLifetime::Transient,
/// );
///
/// let arena = Bump::new();
/// match parse_with_container(source, &mut container, &arena) {
///     Ok(program) => println!("Parsed {} statements", program.statements.len()),
///     Err(e) => eprintln!("Parse error: {}", e),
/// }
/// ```
pub fn parse_with_container<'arena>(
    source: &str,
    container: &mut DiContainer,
    arena: &'arena Bump,
) -> Result<Program<'arena>, ParserError> {
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

    let mut parser = Parser::new(tokens, diagnostic_handler, &interner, &common, arena);
    parser.parse()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    #[test]
    fn test_parse_simple() {
        let source = "const x: number = 42";
        let mut container = DiContainer::new();
        container.register(
            |_| Arc::new(CollectingDiagnosticHandler::new()) as Arc<dyn DiagnosticHandler>,
            ServiceLifetime::Transient,
        );
        let arena = Bump::new();
        let result = parse_with_container(source, &mut container, &arena);
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
        let mut container = DiContainer::new();
        container.register(
            |_| Arc::new(CollectingDiagnosticHandler::new()) as Arc<dyn DiagnosticHandler>,
            ServiceLifetime::Transient,
        );
        let arena = Bump::new();
        let result = parse_with_container(source, &mut container, &arena);
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
        let mut container = DiContainer::new();
        container.register(
            |_| Arc::new(CollectingDiagnosticHandler::new()) as Arc<dyn DiagnosticHandler>,
            ServiceLifetime::Transient,
        );
        let arena = Bump::new();
        let result = parse_with_container(source, &mut container, &arena);
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
        let mut container = DiContainer::new();
        container.register(
            |_| Arc::new(CollectingDiagnosticHandler::new()) as Arc<dyn DiagnosticHandler>,
            ServiceLifetime::Transient,
        );
        let arena = Bump::new();
        let result = parse_with_container(source, &mut container, &arena);
        assert!(result.is_ok());
    }

    #[test]
    fn test_lexer_basic() {
        let source = "const x = 42";
        let mut container = DiContainer::new();
        container.register(
            |_| Arc::new(CollectingDiagnosticHandler::new()) as Arc<dyn DiagnosticHandler>,
            ServiceLifetime::Transient,
        );
        let handler = container.resolve::<Arc<dyn DiagnosticHandler>>().unwrap();
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

        let arena = Bump::new();
        let result = parse_with_container(source, &mut container, &arena);
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

    #[test]
    fn test_di_container_dependent_services() {
        trait StringProvider: Send + Sync {
            fn get_string(&self) -> String;
        }

        struct StaticStringProvider;

        impl StringProvider for StaticStringProvider {
            fn get_string(&self) -> String {
                "static".to_string()
            }
        }

        trait StringConsumer: Send + Sync {
            fn consume(&self) -> String;
        }

        struct StringConsumerImpl {
            provider: Arc<dyn StringProvider>,
        }

        impl StringConsumer for StringConsumerImpl {
            fn consume(&self) -> String {
                format!("consumed: {}", self.provider.get_string())
            }
        }

        let mut container = DiContainer::new();
        container.register::<Arc<dyn StringProvider>>(
            |_| Arc::new(StaticStringProvider) as Arc<dyn StringProvider>,
            ServiceLifetime::Singleton,
        );
        container.register::<Arc<dyn StringConsumer>>(
            |container| {
                let provider = container.resolve::<Arc<dyn StringProvider>>().unwrap();
                Arc::new(StringConsumerImpl { provider }) as Arc<dyn StringConsumer>
            },
            ServiceLifetime::Singleton,
        );

        let consumer = container.resolve::<Arc<dyn StringConsumer>>();
        assert!(consumer.is_some());
        assert_eq!(consumer.unwrap().consume(), "consumed: static");
    }

    #[test]
    fn test_di_container_wrapping_non_clone() {
        use crate::diagnostics::DiagnosticHandler;

        struct NonCloneHandler {
            data: String,
        }

        impl DiagnosticHandler for NonCloneHandler {
            fn report(&self, _diagnostic: crate::diagnostics::Diagnostic) {}
            fn has_errors(&self) -> bool {
                false
            }
            fn error_count(&self) -> usize {
                0
            }
            fn warning_count(&self) -> usize {
                0
            }
            fn get_diagnostics(&self) -> Vec<crate::diagnostics::Diagnostic> {
                Vec::new()
            }
        }

        let mut container = DiContainer::new();
        let handler_ptr = Arc::new(NonCloneHandler {
            data: "test".to_string(),
        });
        container.register(
            move |_| handler_ptr.clone() as Arc<dyn DiagnosticHandler>,
            ServiceLifetime::Singleton,
        );

        let resolved = container.resolve::<Arc<dyn DiagnosticHandler>>();
        assert!(resolved.is_some());
    }

    #[test]
    fn test_di_container_lifecycle() {
        let mut container = DiContainer::new();
        assert!(!container.is_registered::<String>());
        assert_eq!(container.service_count(), 0);
        assert_eq!(container.singleton_count(), 0);

        container.register::<String>(|_| "test".to_string(), ServiceLifetime::Singleton);

        assert!(container.is_registered::<String>());
        assert_eq!(container.service_count(), 1);
        assert_eq!(container.singleton_count(), 0);

        let _ = container.resolve::<String>();
        assert_eq!(container.singleton_count(), 1);

        let _ = container.resolve::<String>();
        assert_eq!(container.singleton_count(), 1);
    }

    #[test]
    fn test_di_container_unregistered_type() {
        let mut container = DiContainer::new();
        let result = container.resolve::<String>();
        assert!(result.is_none());
    }
}
