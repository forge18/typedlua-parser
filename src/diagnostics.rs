use crate::span::Span;
use std::sync::Mutex;

/// Diagnostic severity level
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticLevel {
    Error,
    Warning,
    Info,
}

/// Diagnostic code for categorization and documentation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DiagnosticCode {
    /// Numeric code (e.g., 1001, 2004)
    pub code: u16,
    /// Category prefix (e.g., "E" for error, "W" for warning)
    pub prefix: char,
}

impl DiagnosticCode {
    pub const fn new(prefix: char, code: u16) -> Self {
        Self { code, prefix }
    }

    /// Format as string (e.g., "E1001", "W2004")
    pub fn as_str(&self) -> String {
        format!("{}{:04}", self.prefix, self.code)
    }
}

/// Related information for a diagnostic (additional context from other locations)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiagnosticRelatedInformation {
    pub span: Span,
    pub message: String,
}

/// Suggested fix for a diagnostic
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiagnosticSuggestion {
    pub span: Span,
    pub replacement: String,
    pub message: String,
}

/// A diagnostic message with location, severity, and optional metadata
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    pub level: DiagnosticLevel,
    pub span: Span,
    pub message: String,
    pub code: Option<DiagnosticCode>,
    pub related_information: Vec<DiagnosticRelatedInformation>,
    pub suggestions: Vec<DiagnosticSuggestion>,
}

impl Diagnostic {
    pub fn error(span: Span, message: impl Into<String>) -> Self {
        Self {
            level: DiagnosticLevel::Error,
            span,
            message: message.into(),
            code: None,
            related_information: Vec::new(),
            suggestions: Vec::new(),
        }
    }

    pub fn warning(span: Span, message: impl Into<String>) -> Self {
        Self {
            level: DiagnosticLevel::Warning,
            span,
            message: message.into(),
            code: None,
            related_information: Vec::new(),
            suggestions: Vec::new(),
        }
    }

    pub fn info(span: Span, message: impl Into<String>) -> Self {
        Self {
            level: DiagnosticLevel::Info,
            span,
            message: message.into(),
            code: None,
            related_information: Vec::new(),
            suggestions: Vec::new(),
        }
    }

    /// Create an error with a diagnostic code
    pub fn error_with_code(span: Span, code: DiagnosticCode, message: impl Into<String>) -> Self {
        Self {
            level: DiagnosticLevel::Error,
            span,
            message: message.into(),
            code: Some(code),
            related_information: Vec::new(),
            suggestions: Vec::new(),
        }
    }

    /// Add related information to this diagnostic
    pub fn with_related(mut self, span: Span, message: impl Into<String>) -> Self {
        self.related_information.push(DiagnosticRelatedInformation {
            span,
            message: message.into(),
        });
        self
    }

    /// Add a suggestion to this diagnostic
    pub fn with_suggestion(
        mut self,
        span: Span,
        replacement: String,
        message: impl Into<String>,
    ) -> Self {
        self.suggestions.push(DiagnosticSuggestion {
            span,
            replacement,
            message: message.into(),
        });
        self
    }

    /// Set the diagnostic code
    pub fn with_code(mut self, code: DiagnosticCode) -> Self {
        self.code = Some(code);
        self
    }
}

/// Trait for handling diagnostics
/// This allows for dependency injection and testing with mock handlers
pub trait DiagnosticHandler: Send + Sync {
    fn report(&self, diagnostic: Diagnostic);

    fn error(&self, span: Span, message: &str) {
        self.report(Diagnostic::error(span, message.to_string()));
    }

    fn warning(&self, span: Span, message: &str) {
        self.report(Diagnostic::warning(span, message.to_string()));
    }

    fn info(&self, span: Span, message: &str) {
        self.report(Diagnostic::info(span, message.to_string()));
    }

    /// Report an error with a diagnostic code
    fn report_error(&self, span: Span, code: DiagnosticCode, message: &str) {
        self.report(Diagnostic::error_with_code(span, code, message.to_string()));
    }

    /// Report a warning with a diagnostic code
    fn report_warning(&self, span: Span, code: DiagnosticCode, message: &str) {
        self.report(Diagnostic::warning(span, message.to_string()).with_code(code));
    }

    fn has_errors(&self) -> bool;
    fn error_count(&self) -> usize;
    fn warning_count(&self) -> usize;
    fn get_diagnostics(&self) -> Vec<Diagnostic>;
}

/// A diagnostic handler that collects all diagnostics for later retrieval
pub struct CollectingDiagnosticHandler {
    diagnostics: Mutex<Vec<Diagnostic>>,
}

impl CollectingDiagnosticHandler {
    pub fn new() -> Self {
        Self {
            diagnostics: Mutex::new(Vec::new()),
        }
    }
}

impl Default for CollectingDiagnosticHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl DiagnosticHandler for CollectingDiagnosticHandler {
    fn report(&self, diagnostic: Diagnostic) {
        self.diagnostics.lock().unwrap().push(diagnostic);
    }

    fn has_errors(&self) -> bool {
        self.diagnostics
            .lock()
            .unwrap()
            .iter()
            .any(|d| d.level == DiagnosticLevel::Error)
    }

    fn error_count(&self) -> usize {
        self.diagnostics
            .lock()
            .unwrap()
            .iter()
            .filter(|d| d.level == DiagnosticLevel::Error)
            .count()
    }

    fn warning_count(&self) -> usize {
        self.diagnostics
            .lock()
            .unwrap()
            .iter()
            .filter(|d| d.level == DiagnosticLevel::Warning)
            .count()
    }

    fn get_diagnostics(&self) -> Vec<Diagnostic> {
        self.diagnostics.lock().unwrap().clone()
    }
}

/// Common error codes used by the parser
pub mod error_codes {
    use super::DiagnosticCode;

    pub const UNEXPECTED_CHAR: DiagnosticCode = DiagnosticCode::new('E', 1001);
    pub const UNTERMINATED_COMMENT: DiagnosticCode = DiagnosticCode::new('E', 1002);
    pub const UNTERMINATED_STRING: DiagnosticCode = DiagnosticCode::new('E', 1003);
    pub const UNEXPECTED_TOKEN: DiagnosticCode = DiagnosticCode::new('E', 2001);
    pub const EXPECTED_TOKEN: DiagnosticCode = DiagnosticCode::new('E', 2002);
    pub const EXPECTED_IDENTIFIER: DiagnosticCode = DiagnosticCode::new('E', 2003);
    pub const EXPECTED_EXPRESSION: DiagnosticCode = DiagnosticCode::new('E', 2004);
    pub const EXPECTED_TYPE: DiagnosticCode = DiagnosticCode::new('E', 2005);
    pub const EXPECTED_PATTERN: DiagnosticCode = DiagnosticCode::new('E', 2006);
    pub const MISSING_END: DiagnosticCode = DiagnosticCode::new('E', 2010);
    pub const MISSING_THEN: DiagnosticCode = DiagnosticCode::new('E', 2011);
    pub const MISSING_DO: DiagnosticCode = DiagnosticCode::new('E', 2012);
    pub const BREAK_OUTSIDE_LOOP: DiagnosticCode = DiagnosticCode::new('E', 2020);
    pub const CONTINUE_OUTSIDE_LOOP: DiagnosticCode = DiagnosticCode::new('E', 2021);
    pub const CLASSES_DISABLED: DiagnosticCode = DiagnosticCode::new('E', 3001);
    pub const DECORATORS_DISABLED: DiagnosticCode = DiagnosticCode::new('E', 3002);
    pub const FP_DISABLED: DiagnosticCode = DiagnosticCode::new('E', 3003);
}
