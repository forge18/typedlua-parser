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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_diagnostic_code_new() {
        let code = DiagnosticCode::new('E', 1001);
        assert_eq!(code.prefix, 'E');
        assert_eq!(code.code, 1001);
    }

    #[test]
    fn test_diagnostic_code_as_str() {
        let code = DiagnosticCode::new('E', 1001);
        assert_eq!(code.as_str(), "E1001");

        let code = DiagnosticCode::new('W', 2004);
        assert_eq!(code.as_str(), "W2004");
    }

    #[test]
    fn test_diagnostic_error() {
        let span = Span::new(0, 10, 1, 1);
        let diag = Diagnostic::error(span, "test error");
        assert_eq!(diag.level, DiagnosticLevel::Error);
        assert_eq!(diag.message, "test error");
        assert!(diag.code.is_none());
        assert!(diag.related_information.is_empty());
        assert!(diag.suggestions.is_empty());
    }

    #[test]
    fn test_diagnostic_warning() {
        let span = Span::new(0, 10, 1, 1);
        let diag = Diagnostic::warning(span, "test warning");
        assert_eq!(diag.level, DiagnosticLevel::Warning);
        assert_eq!(diag.message, "test warning");
    }

    #[test]
    fn test_diagnostic_info() {
        let span = Span::new(0, 10, 1, 1);
        let diag = Diagnostic::info(span, "test info");
        assert_eq!(diag.level, DiagnosticLevel::Info);
        assert_eq!(diag.message, "test info");
    }

    #[test]
    fn test_diagnostic_error_with_code() {
        let span = Span::new(0, 10, 1, 1);
        let code = DiagnosticCode::new('E', 1001);
        let diag = Diagnostic::error_with_code(span, code, "test error with code");
        assert_eq!(diag.level, DiagnosticLevel::Error);
        assert!(diag.code.is_some());
        assert_eq!(diag.code.unwrap().as_str(), "E1001");
    }

    #[test]
    fn test_diagnostic_with_related() {
        let span = Span::new(0, 10, 1, 1);
        let related_span = Span::new(20, 30, 2, 1);
        let diag = Diagnostic::error(span, "test error").with_related(related_span, "related info");
        assert_eq!(diag.related_information.len(), 1);
        assert_eq!(diag.related_information[0].message, "related info");
    }

    #[test]
    fn test_diagnostic_with_suggestion() {
        let span = Span::new(0, 10, 1, 1);
        let diag = Diagnostic::error(span, "test error").with_suggestion(
            span,
            "replacement".to_string(),
            "suggestion message",
        );
        assert_eq!(diag.suggestions.len(), 1);
        assert_eq!(diag.suggestions[0].replacement, "replacement");
        assert_eq!(diag.suggestions[0].message, "suggestion message");
    }

    #[test]
    fn test_diagnostic_with_code_method() {
        let span = Span::new(0, 10, 1, 1);
        let code = DiagnosticCode::new('W', 2004);
        let diag = Diagnostic::warning(span, "test warning").with_code(code);
        assert!(diag.code.is_some());
        assert_eq!(diag.code.unwrap().as_str(), "W2004");
    }

    #[test]
    fn test_collecting_diagnostic_handler_new() {
        let handler = CollectingDiagnosticHandler::new();
        assert!(!handler.has_errors());
        assert_eq!(handler.error_count(), 0);
        assert_eq!(handler.warning_count(), 0);
        assert!(handler.get_diagnostics().is_empty());
    }

    #[test]
    fn test_collecting_diagnostic_handler_report_error() {
        let handler = CollectingDiagnosticHandler::new();
        let span = Span::new(0, 10, 1, 1);
        handler.error(span, "test error");
        assert!(handler.has_errors());
        assert_eq!(handler.error_count(), 1);
        assert_eq!(handler.warning_count(), 0);
        let diags = handler.get_diagnostics();
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].level, DiagnosticLevel::Error);
    }

    #[test]
    fn test_collecting_diagnostic_handler_report_warning() {
        let handler = CollectingDiagnosticHandler::new();
        let span = Span::new(0, 10, 1, 1);
        handler.warning(span, "test warning");
        assert!(!handler.has_errors());
        assert_eq!(handler.error_count(), 0);
        assert_eq!(handler.warning_count(), 1);
        let diags = handler.get_diagnostics();
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].level, DiagnosticLevel::Warning);
    }

    #[test]
    fn test_collecting_diagnostic_handler_report_info() {
        let handler = CollectingDiagnosticHandler::new();
        let span = Span::new(0, 10, 1, 1);
        handler.info(span, "test info");
        assert!(!handler.has_errors());
        assert_eq!(handler.error_count(), 0);
        assert_eq!(handler.warning_count(), 0);
        let diags = handler.get_diagnostics();
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].level, DiagnosticLevel::Info);
    }

    #[test]
    fn test_collecting_diagnostic_handler_report_error_with_code() {
        let handler = CollectingDiagnosticHandler::new();
        let span = Span::new(0, 10, 1, 1);
        let code = error_codes::UNEXPECTED_TOKEN;
        handler.report_error(span, code, "unexpected token");
        assert!(handler.has_errors());
        let diags = handler.get_diagnostics();
        assert_eq!(diags[0].code.as_ref().unwrap().as_str(), "E2001");
    }

    #[test]
    fn test_collecting_diagnostic_handler_report_warning_with_code() {
        let handler = CollectingDiagnosticHandler::new();
        let span = Span::new(0, 10, 1, 1);
        let code = DiagnosticCode::new('W', 3001);
        handler.report_warning(span, code, "test warning");
        assert_eq!(handler.warning_count(), 1);
        let diags = handler.get_diagnostics();
        assert_eq!(diags[0].code.as_ref().unwrap().as_str(), "W3001");
    }

    #[test]
    fn test_collecting_diagnostic_handler_multiple_diagnostics() {
        let handler = CollectingDiagnosticHandler::new();
        let span = Span::new(0, 10, 1, 1);

        handler.error(span, "error 1");
        handler.error(span, "error 2");
        handler.warning(span, "warning 1");
        handler.info(span, "info 1");

        assert!(handler.has_errors());
        assert_eq!(handler.error_count(), 2);
        assert_eq!(handler.warning_count(), 1);
        assert_eq!(handler.get_diagnostics().len(), 4);
    }

    #[test]
    fn test_error_codes() {
        assert_eq!(error_codes::UNEXPECTED_CHAR.as_str(), "E1001");
        assert_eq!(error_codes::UNTERMINATED_COMMENT.as_str(), "E1002");
        assert_eq!(error_codes::UNTERMINATED_STRING.as_str(), "E1003");
        assert_eq!(error_codes::UNEXPECTED_TOKEN.as_str(), "E2001");
        assert_eq!(error_codes::EXPECTED_TOKEN.as_str(), "E2002");
        assert_eq!(error_codes::EXPECTED_IDENTIFIER.as_str(), "E2003");
        assert_eq!(error_codes::EXPECTED_EXPRESSION.as_str(), "E2004");
        assert_eq!(error_codes::EXPECTED_TYPE.as_str(), "E2005");
        assert_eq!(error_codes::EXPECTED_PATTERN.as_str(), "E2006");
        assert_eq!(error_codes::MISSING_END.as_str(), "E2010");
        assert_eq!(error_codes::MISSING_THEN.as_str(), "E2011");
        assert_eq!(error_codes::MISSING_DO.as_str(), "E2012");
        assert_eq!(error_codes::BREAK_OUTSIDE_LOOP.as_str(), "E2020");
        assert_eq!(error_codes::CONTINUE_OUTSIDE_LOOP.as_str(), "E2021");
        assert_eq!(error_codes::CLASSES_DISABLED.as_str(), "E3001");
        assert_eq!(error_codes::DECORATORS_DISABLED.as_str(), "E3002");
        assert_eq!(error_codes::FP_DISABLED.as_str(), "E3003");
    }

    #[test]
    fn test_diagnostic_level_equality() {
        assert_eq!(DiagnosticLevel::Error, DiagnosticLevel::Error);
        assert_eq!(DiagnosticLevel::Warning, DiagnosticLevel::Warning);
        assert_eq!(DiagnosticLevel::Info, DiagnosticLevel::Info);
        assert_ne!(DiagnosticLevel::Error, DiagnosticLevel::Warning);
    }

    #[test]
    fn test_diagnostic_clone() {
        let span = Span::new(0, 10, 1, 1);
        let diag = Diagnostic::error(span, "test error").with_code(error_codes::UNEXPECTED_TOKEN);
        let cloned = diag.clone();
        assert_eq!(diag.message, cloned.message);
        assert_eq!(diag.level, cloned.level);
        assert_eq!(diag.code.unwrap().as_str(), cloned.code.unwrap().as_str());
    }

    #[test]
    fn test_collecting_handler_default() {
        let handler: CollectingDiagnosticHandler = Default::default();
        assert!(!handler.has_errors());
        assert!(handler.get_diagnostics().is_empty());
    }
}
