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
    fn current(&self) -> &Token {
        self.tokens.get(self.position).unwrap_or_else(|| {
            self.tokens
                .last()
                .expect("Token stream should never be empty")
        })
    }

    fn is_at_end(&self) -> bool {
        matches!(self.current().kind, TokenKind::Eof)
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.position += 1;
        }
        &self.tokens[self.position - 1]
    }

    fn check(&self, kind: &TokenKind) -> bool {
        if self.is_at_end() {
            return false;
        }
        std::mem::discriminant(&self.current().kind) == std::mem::discriminant(kind)
    }

    fn nth_token_kind(&self, n: usize) -> Option<&TokenKind> {
        self.tokens.get(self.position + n).map(|t| &t.kind)
    }

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
