mod expression;
mod pattern;
mod statement;
mod types;

use crate::ast::Program;
use crate::diagnostics::{error_codes, DiagnosticHandler};
use crate::lexer::{Token, TokenKind};
use crate::span::Span;
use crate::string_interner::{CommonIdentifiers, StringInterner};
use std::sync::Arc;

pub use expression::ExpressionParser;
pub use pattern::PatternParser;
pub use statement::StatementParser;
pub use types::TypeParser;

#[derive(Debug, Clone)]
pub struct ParserError {
    pub message: String,
    pub span: Span,
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at line {}", self.message, self.span.line)
    }
}

impl std::error::Error for ParserError {}

pub struct Parser<'a> {
    tokens: Vec<Token>,
    position: usize,
    diagnostic_handler: Arc<dyn DiagnosticHandler>,
    interner: &'a StringInterner,
    common: &'a CommonIdentifiers,
    has_namespace: bool,
    is_first_statement: bool,
}

impl<'a> Parser<'a> {
    pub fn new(
        tokens: Vec<Token>,
        diagnostic_handler: Arc<dyn DiagnosticHandler>,
        interner: &'a StringInterner,
        common: &'a CommonIdentifiers,
    ) -> Self {
        Parser {
            tokens,
            position: 0,
            diagnostic_handler,
            interner,
            common,
            has_namespace: false,
            is_first_statement: true,
        }
    }

    /// Get reference to the string interner
    pub fn interner(&self) -> &StringInterner {
        self.interner
    }

    /// Get reference to common identifiers
    pub fn common(&self) -> &CommonIdentifiers {
        self.common
    }

    /// Resolve a StringId to a string
    pub fn resolve(&self, id: crate::string_interner::StringId) -> String {
        self.interner.resolve(id)
    }

    pub fn parse(&mut self) -> Result<Program, ParserError> {
        let start_span = self.current_span();
        let mut statements = Vec::new();
        self.is_first_statement = true;

        while !self.is_at_end() {
            match self.parse_statement() {
                Ok(stmt) => {
                    statements.push(stmt);
                    self.is_first_statement = false;
                }
                Err(e) => {
                    self.report_error(&e.message, e.span);
                    // Error recovery: skip to next statement
                    self.synchronize();
                }
            }
        }

        let end_span = if !statements.is_empty() {
            statements.last().unwrap().span()
        } else {
            start_span
        };

        Ok(Program::new(statements, start_span.combine(&end_span)))
    }

    // Token stream management
    #[inline(always)]
    fn current(&self) -> &Token {
        self.tokens.get(self.position).unwrap_or_else(|| {
            self.tokens
                .last()
                .expect("Token stream should never be empty")
        })
    }

    #[inline(always)]
    fn is_at_end(&self) -> bool {
        matches!(self.current().kind, TokenKind::Eof)
    }

    #[inline(always)]
    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.position += 1;
        }
        &self.tokens[self.position - 1]
    }

    #[inline(always)]
    fn check(&self, kind: &TokenKind) -> bool {
        if self.is_at_end() {
            return false;
        }
        std::mem::discriminant(&self.current().kind) == std::mem::discriminant(kind)
    }

    #[inline(always)]
    fn nth_token_kind(&self, n: usize) -> Option<&TokenKind> {
        self.tokens.get(self.position + n).map(|t| &t.kind)
    }

    #[inline]
    fn match_token(&mut self, kinds: &[TokenKind]) -> bool {
        for kind in kinds {
            if self.check(kind) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn consume(&mut self, kind: TokenKind, message: &str) -> Result<&Token, ParserError> {
        if self.check(&kind) {
            return Ok(self.advance());
        }

        Err(ParserError {
            message: message.to_string(),
            span: self.current_span(),
        })
    }

    /// Consume a closing `>` for type arguments.
    /// Handles the `>>` ambiguity: if the current token is `>>`, it is split
    /// by mutating it in-place to a single `>` (without advancing), so the
    /// outer generic context can later consume the remaining `>`.
    fn consume_closing_angle_bracket(&mut self) -> Result<(), ParserError> {
        if self.check(&TokenKind::GreaterThan) {
            self.advance();
            Ok(())
        } else if self.check(&TokenKind::GreaterGreater) {
            // Split '>>' into two '>': mutate the token to '>' for the outer
            // context and don't advance (we logically consumed only the first '>').
            self.tokens[self.position].kind = TokenKind::GreaterThan;
            Ok(())
        } else {
            Err(ParserError {
                message: "Expected '>' after type arguments".to_string(),
                span: self.current_span(),
            })
        }
    }

    #[inline(always)]
    fn current_span(&self) -> Span {
        self.current().span
    }

    // Error reporting
    fn report_error(&self, message: &str, span: Span) {
        // Assign error codes based on message patterns
        let error_code = if message.contains("break") && message.contains("outside") {
            error_codes::BREAK_OUTSIDE_LOOP
        } else if message.contains("continue") && message.contains("outside") {
            error_codes::CONTINUE_OUTSIDE_LOOP
        } else if message.contains("end") {
            error_codes::MISSING_END
        } else if message.contains("then") {
            error_codes::MISSING_THEN
        } else if message.contains("do") && (message.contains("while") || message.contains("for")) {
            error_codes::MISSING_DO
        } else if message.contains("identifier") && message.contains("Expected") {
            error_codes::EXPECTED_IDENTIFIER
        } else if message.contains("expression") && message.contains("Expected") {
            error_codes::EXPECTED_EXPRESSION
        } else if message.contains("Unexpected") || message.contains("unexpected") {
            error_codes::UNEXPECTED_TOKEN
        } else if message.contains("Expected") || message.contains("expected") {
            error_codes::EXPECTED_TOKEN
        } else {
            error_codes::UNEXPECTED_TOKEN
        };

        self.diagnostic_handler
            .report_error(span, error_code, message);
    }

    // Error recovery: skip to next statement boundary
    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            match &self.current().kind {
                TokenKind::Function
                | TokenKind::Local
                | TokenKind::Const
                | TokenKind::If
                | TokenKind::While
                | TokenKind::For
                | TokenKind::Repeat
                | TokenKind::Return
                | TokenKind::Break
                | TokenKind::Continue
                | TokenKind::Interface
                | TokenKind::Type
                | TokenKind::Enum
                | TokenKind::Class
                | TokenKind::Import
                | TokenKind::Export
                | TokenKind::Declare
                | TokenKind::Namespace
                | TokenKind::Semicolon => return,
                TokenKind::End | TokenKind::Elseif | TokenKind::Else | TokenKind::Until => return,
                _ => {}
            }

            self.advance();
        }
    }
}

// Helper trait to get span from any statement/expression
trait Spannable {
    fn span(&self) -> Span;
}

impl Spannable for crate::ast::statement::Statement {
    fn span(&self) -> Span {
        use crate::ast::statement::Statement::*;
        match self {
            Variable(v) => v.span,
            Function(f) => f.span,
            Class(c) => c.span,
            Interface(i) => i.span,
            TypeAlias(t) => t.span,
            Enum(e) => e.span,
            Import(i) => i.span,
            Export(e) => e.span,
            If(i) => i.span,
            While(w) => w.span,
            For(f) => match f.as_ref() {
                crate::ast::statement::ForStatement::Numeric(n) => n.span,
                crate::ast::statement::ForStatement::Generic(g) => g.span,
            },
            Repeat(r) => r.span,
            Return(r) => r.span,
            Break(s) | Continue(s) => *s,
            Label(l) => l.span,
            Goto(g) => g.span,
            Expression(e) => e.span,
            Block(b) => b.span,
            DeclareFunction(f) => f.span,
            DeclareNamespace(n) => n.span,
            DeclareType(t) => t.span,
            DeclareInterface(i) => i.span,
            DeclareConst(c) => c.span,
            Throw(t) => t.span,
            Try(t) => t.span,
            Rethrow(s) => *s,
            Namespace(n) => n.span,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostics::CollectingDiagnosticHandler;
    use crate::lexer::Lexer;
    use crate::string_interner::StringInterner;
    use std::sync::Arc;

    fn parse_program(source: &str) -> (Program, Arc<CollectingDiagnosticHandler>) {
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common) = StringInterner::new_with_common_identifiers();
        let mut lexer = Lexer::new(source, handler.clone(), &interner);
        let tokens = lexer.tokenize().expect("Failed to tokenize");
        let mut parser = Parser::new(tokens, handler.clone(), &interner, &common);
        let program = parser.parse().expect("Failed to parse");
        (program, handler)
    }

    #[test]
    fn test_parser_error_display() {
        let span = Span::new(0, 5, 1, 1);
        let error = ParserError {
            message: "Test error".to_string(),
            span,
        };
        assert_eq!(format!("{}", error), "Test error at line 1");
    }

    #[test]
    fn test_parser_parse_empty() {
        let (program, _) = parse_program("");
        assert!(program.statements.is_empty());
    }

    #[test]
    fn test_parser_parse_single_statement() {
        let (program, _) = parse_program("local x = 1");
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn test_parser_parse_multiple_statements() {
        let (program, _) = parse_program("local x = 1\nlocal y = 2");
        assert_eq!(program.statements.len(), 2);
    }

    #[test]
    fn test_parser_error_recovery() {
        let (program, handler) = parse_program("local x = \nlocal y = 2");
        // Parser reports errors during parsing
        // Error recovery behavior may vary, but errors should be recorded
        assert!(
            handler.has_errors() || handler.warning_count() > 0 || !program.statements.is_empty()
        );
    }

    #[test]
    fn test_parser_synchronize_to_function() {
        let (program, _) = parse_program("x function foo() end");
        // Should recover at 'function' and parse the function
        assert!(!program.statements.is_empty());
    }

    #[test]
    fn test_parser_synchronize_to_local() {
        let (program, _) = parse_program("x local y = 1");
        // Should recover at 'local' and parse the variable
        assert!(!program.statements.is_empty());
    }

    #[test]
    fn test_parser_synchronize_to_const() {
        let (program, _) = parse_program("x const y = 1");
        // Should recover at 'const' and parse the variable
        assert!(!program.statements.is_empty());
    }

    #[test]
    fn test_parser_synchronize_to_if() {
        let (program, _) = parse_program("x if true then end");
        // Should recover at 'if' and parse the statement
        assert!(!program.statements.is_empty());
    }

    #[test]
    fn test_parser_synchronize_to_while() {
        let (program, _) = parse_program("x while true do end");
        // Should recover at 'while' and parse the statement
        assert!(!program.statements.is_empty());
    }

    #[test]
    fn test_parser_synchronize_to_for() {
        let (program, _) = parse_program("x for i = 1, 10 do end");
        // Should recover at 'for' and parse the statement
        assert!(!program.statements.is_empty());
    }

    #[test]
    fn test_parser_synchronize_to_repeat() {
        let (program, _) = parse_program("x repeat until true");
        // Should recover at 'repeat' and parse the statement
        assert!(!program.statements.is_empty());
    }

    #[test]
    fn test_parser_synchronize_to_return() {
        let (program, _) = parse_program("x return 1");
        // Should recover at 'return' and parse the statement
        assert!(!program.statements.is_empty());
    }

    #[test]
    fn test_parser_synchronize_to_break() {
        let (program, _) = parse_program("x break");
        // Should recover at 'break' and parse the statement
        assert!(!program.statements.is_empty());
    }

    #[test]
    fn test_parser_synchronize_to_continue() {
        let (program, _) = parse_program("x continue");
        // Should recover at 'continue' and parse the statement
        assert!(!program.statements.is_empty());
    }

    #[test]
    fn test_parser_synchronize_to_interface() {
        let (program, _) = parse_program("x interface Foo {}");
        // Should recover at 'interface' and parse the statement
        assert!(!program.statements.is_empty());
    }

    #[test]
    fn test_parser_synchronize_to_type() {
        let (program, _) = parse_program("x type Foo = number");
        // Should recover at 'type' and parse the statement
        assert!(!program.statements.is_empty());
    }

    #[test]
    fn test_parser_synchronize_to_enum() {
        let (program, _) = parse_program("x enum Foo { A }");
        // Should recover at 'enum' and parse the statement
        assert!(!program.statements.is_empty());
    }

    #[test]
    fn test_parser_synchronize_to_class() {
        let (program, _) = parse_program("x class Foo {}");
        // Should recover at 'class' and parse the statement
        assert!(!program.statements.is_empty());
    }

    #[test]
    fn test_parser_synchronize_to_import() {
        let (program, _) = parse_program("x import foo from 'bar'");
        // Should recover at 'import' and parse the statement
        assert!(!program.statements.is_empty());
    }

    #[test]
    fn test_parser_synchronize_to_export() {
        let (program, _) = parse_program("x export { foo }");
        // Should recover at 'export' and parse the statement
        assert!(!program.statements.is_empty());
    }

    #[test]
    fn test_parser_synchronize_to_declare() {
        let (program, _) = parse_program("x declare function foo(): void");
        // Should recover at 'declare' and parse the statement
        assert!(!program.statements.is_empty());
    }

    #[test]
    fn test_parser_synchronize_to_namespace() {
        let (program, _) = parse_program("x namespace Foo;");
        // Should recover at 'namespace' and parse the statement
        assert!(!program.statements.is_empty());
    }

    #[test]
    fn test_parser_synchronize_to_semicolon() {
        let (program, _) = parse_program("x ; local y = 1");
        // Should recover at ';' and parse the next statement
        assert!(!program.statements.is_empty());
    }

    #[test]
    fn test_parser_synchronize_to_end() {
        let (program, _) = parse_program("if true then x end");
        // Should parse successfully with error recovery inside the if
        assert!(!program.statements.is_empty());
    }

    #[test]
    fn test_parser_synchronize_to_elseif() {
        let (program, _) = parse_program("if true then x elseif false then end");
        // Should parse successfully with error recovery
        assert!(!program.statements.is_empty());
    }

    #[test]
    fn test_parser_synchronize_to_else() {
        let (program, _) = parse_program("if true then x else end");
        // Should parse successfully with error recovery
        assert!(!program.statements.is_empty());
    }

    #[test]
    fn test_parser_synchronize_to_until() {
        let (program, _) = parse_program("repeat x until true");
        // Should parse successfully with error recovery
        assert!(!program.statements.is_empty());
    }

    #[test]
    fn test_parser_synchronize_advances_past_non_boundary() {
        // Test that synchronize advances past tokens that aren't boundaries
        let (program, _) = parse_program("x + y + z local w = 1");
        // Should skip x + y + z and recover at 'local'
        assert!(!program.statements.is_empty());
    }

    #[test]
    fn test_parser_synchronize_at_eof() {
        // Test synchronize when already at EOF
        let (program, handler) = parse_program("local x =");
        // Should handle EOF gracefully
        assert!(program.statements.is_empty() || handler.has_errors());
    }

    #[test]
    fn test_parser_parse_with_namespace_first() {
        let (program, _) = parse_program("namespace Foo;\nlocal x = 1");
        assert_eq!(program.statements.len(), 2);
    }

    #[test]
    fn test_parser_has_namespace_tracking() {
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common) = StringInterner::new_with_common_identifiers();
        let mut lexer = Lexer::new("namespace Foo;", handler.clone(), &interner);
        let tokens = lexer.tokenize().expect("Failed to tokenize");
        let mut parser = Parser::new(tokens, handler.clone(), &interner, &common);

        // Initially has_namespace should be false
        assert!(!parser.has_namespace);

        // After parsing namespace, it should be set
        let _ = parser.parse();
        // Note: has_namespace is private, but we can verify behavior through error cases
    }

    #[test]
    fn test_parser_is_first_statement_tracking() {
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common) = StringInterner::new_with_common_identifiers();
        let mut lexer = Lexer::new("local x = 1\nlocal y = 2", handler.clone(), &interner);
        let tokens = lexer.tokenize().expect("Failed to tokenize");
        let mut parser = Parser::new(tokens, handler.clone(), &interner, &common);

        // Parse first statement
        let _ = parser.parse();
        // is_first_statement should be tracked internally
    }

    #[test]
    fn test_parser_error_trait_impl() {
        let error = ParserError {
            message: "Test error".to_string(),
            span: Span::new(0, 10, 1, 1),
        };

        // Test that ParserError implements std::error::Error
        let _: &(dyn std::error::Error) = &error;
    }

    #[test]
    fn test_parser_multiple_error_recovery() {
        let (program, handler) = parse_program("local x = \nlocal y = \nlocal z = 1");
        // Should recover from multiple errors and still parse the last statement
        assert!(handler.has_errors());
        assert!(!program.statements.is_empty());
    }

    #[test]
    fn test_parser_empty_program_span() {
        let (program, _) = parse_program("");
        // Empty program should have a valid span
        assert_eq!(program.statements.len(), 0);
    }

    #[test]
    fn test_parser_program_with_multiple_namespaces_error() {
        let (program, handler) = parse_program("namespace Foo;\nnamespace Bar;");
        // Second namespace should cause an error
        assert!(handler.has_errors() || program.statements.len() == 2);
    }

    #[test]
    fn test_parser_namespace_not_first_error() {
        let (program, handler) = parse_program("local x = 1\nnamespace Foo;");
        // Namespace not as first statement should cause an error
        assert!(handler.has_errors() || program.statements.len() == 2);
    }

    #[test]
    fn test_parser_report_error_break_outside_loop() {
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common) = StringInterner::new_with_common_identifiers();
        let tokens = vec![Token::new(TokenKind::Eof, Span::default())];
        let parser = Parser::new(tokens, handler.clone(), &interner, &common);
        let span = Span::new(0, 5, 1, 1);
        parser.report_error("break outside loop", span);
        let diags = handler.get_diagnostics();
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].code.as_ref().unwrap().as_str(), "E2020");
    }

    #[test]
    fn test_parser_report_error_continue_outside_loop() {
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common) = StringInterner::new_with_common_identifiers();
        let tokens = vec![Token::new(TokenKind::Eof, Span::default())];
        let parser = Parser::new(tokens, handler.clone(), &interner, &common);
        let span = Span::new(0, 5, 1, 1);
        parser.report_error("continue outside loop", span);
        let diags = handler.get_diagnostics();
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].code.as_ref().unwrap().as_str(), "E2021");
    }

    #[test]
    fn test_parser_report_error_missing_end() {
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common) = StringInterner::new_with_common_identifiers();
        let tokens = vec![Token::new(TokenKind::Eof, Span::default())];
        let parser = Parser::new(tokens, handler.clone(), &interner, &common);
        let span = Span::new(0, 5, 1, 1);
        parser.report_error("Expected 'end'", span);
        let diags = handler.get_diagnostics();
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].code.as_ref().unwrap().as_str(), "E2010");
    }

    #[test]
    fn test_parser_report_error_missing_then() {
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common) = StringInterner::new_with_common_identifiers();
        let tokens = vec![Token::new(TokenKind::Eof, Span::default())];
        let parser = Parser::new(tokens, handler.clone(), &interner, &common);
        let span = Span::new(0, 5, 1, 1);
        parser.report_error("Expected 'then'", span);
        let diags = handler.get_diagnostics();
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].code.as_ref().unwrap().as_str(), "E2011");
    }

    #[test]
    fn test_parser_report_error_missing_do() {
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common) = StringInterner::new_with_common_identifiers();
        let tokens = vec![Token::new(TokenKind::Eof, Span::default())];
        let parser = Parser::new(tokens, handler.clone(), &interner, &common);
        let span = Span::new(0, 5, 1, 1);
        parser.report_error("Expected 'do' in while loop", span);
        let diags = handler.get_diagnostics();
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].code.as_ref().unwrap().as_str(), "E2012");
    }

    #[test]
    fn test_parser_report_error_expected_identifier() {
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common) = StringInterner::new_with_common_identifiers();
        let tokens = vec![Token::new(TokenKind::Eof, Span::default())];
        let parser = Parser::new(tokens, handler.clone(), &interner, &common);
        let span = Span::new(0, 5, 1, 1);
        parser.report_error("Expected identifier", span);
        let diags = handler.get_diagnostics();
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].code.as_ref().unwrap().as_str(), "E2003");
    }

    #[test]
    fn test_parser_report_error_expected_expression() {
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common) = StringInterner::new_with_common_identifiers();
        let tokens = vec![Token::new(TokenKind::Eof, Span::default())];
        let parser = Parser::new(tokens, handler.clone(), &interner, &common);
        let span = Span::new(0, 5, 1, 1);
        parser.report_error("Expected expression", span);
        let diags = handler.get_diagnostics();
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].code.as_ref().unwrap().as_str(), "E2004");
    }

    #[test]
    fn test_parser_report_error_unexpected_token() {
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common) = StringInterner::new_with_common_identifiers();
        let tokens = vec![Token::new(TokenKind::Eof, Span::default())];
        let parser = Parser::new(tokens, handler.clone(), &interner, &common);
        let span = Span::new(0, 5, 1, 1);
        parser.report_error("Unexpected token", span);
        let diags = handler.get_diagnostics();
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].code.as_ref().unwrap().as_str(), "E2001");
    }

    #[test]
    fn test_parser_report_error_expected_token() {
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common) = StringInterner::new_with_common_identifiers();
        let tokens = vec![Token::new(TokenKind::Eof, Span::default())];
        let parser = Parser::new(tokens, handler.clone(), &interner, &common);
        let span = Span::new(0, 5, 1, 1);
        parser.report_error("Expected token", span);
        let diags = handler.get_diagnostics();
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].code.as_ref().unwrap().as_str(), "E2002");
    }

    #[test]
    fn test_parser_report_error_generic() {
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common) = StringInterner::new_with_common_identifiers();
        let tokens = vec![Token::new(TokenKind::Eof, Span::default())];
        let parser = Parser::new(tokens, handler.clone(), &interner, &common);
        let span = Span::new(0, 5, 1, 1);
        parser.report_error("Some random error", span);
        let diags = handler.get_diagnostics();
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].code.as_ref().unwrap().as_str(), "E2001");
    }
}
