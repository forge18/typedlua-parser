use bumpalo::Bump;
use luanext_parser::ast::expression::Literal;
use luanext_parser::ast::pattern::{Pattern, TemplatePatternPart};
use luanext_parser::diagnostics::CollectingDiagnosticHandler;
use luanext_parser::lexer::Lexer;
use luanext_parser::parser::{Parser, PatternParser};
use luanext_parser::span::Span;
use luanext_parser::string_interner::StringInterner;
use std::sync::Arc;

fn parse_pattern(source: &str) -> Result<Pattern<'static>, String> {
    let handler = Arc::new(CollectingDiagnosticHandler::new());
    let (interner, common) = StringInterner::new_with_common_identifiers();
    let mut lexer = Lexer::new(source, handler.clone(), &interner);
    let tokens = lexer
        .tokenize()
        .map_err(|e| format!("Lexer error: {:?}", e))?;
    let arena = Box::leak(Box::new(Bump::new()));
    let mut parser = Parser::new(tokens, handler.clone(), &interner, &common, arena);
    parser
        .parse_pattern()
        .map_err(|e| format!("Parser error: {}", e.message))
}

#[test]
fn test_basic_template_pattern() {
    let result = parse_pattern("`https://${host}/${path}`");
    assert!(result.is_ok(), "Failed to parse: {:?}", result);

    match result.unwrap() {
        Pattern::Template(template) => {
            assert_eq!(template.parts.len(), 4);
            assert!(matches!(
                &template.parts[0],
                TemplatePatternPart::String(s) if s == "https://"
            ));
            assert!(matches!(&template.parts[1], TemplatePatternPart::Capture(_)));
            assert!(matches!(
                &template.parts[2],
                TemplatePatternPart::String(s) if s == "/"
            ));
            assert!(matches!(&template.parts[3], TemplatePatternPart::Capture(_)));
        }
        _ => panic!("Expected template pattern"),
    }
}

#[test]
fn test_single_capture() {
    let result = parse_pattern("`error: ${msg}`");
    assert!(result.is_ok());

    match result.unwrap() {
        Pattern::Template(template) => {
            assert_eq!(template.parts.len(), 2);
            assert!(matches!(
                &template.parts[0],
                TemplatePatternPart::String(s) if s == "error: "
            ));
            assert!(matches!(&template.parts[1], TemplatePatternPart::Capture(_)));
        }
        _ => panic!("Expected template pattern"),
    }
}

#[test]
fn test_capture_only() {
    let result = parse_pattern("`${value}`");
    assert!(result.is_ok());

    match result.unwrap() {
        Pattern::Template(template) => {
            assert_eq!(template.parts.len(), 1);
            assert!(matches!(&template.parts[0], TemplatePatternPart::Capture(_)));
        }
        _ => panic!("Expected template pattern"),
    }
}

#[test]
fn test_adjacent_captures_error() {
    let result = parse_pattern("`${a}${b}`");
    assert!(result.is_err());
    assert!(result
        .unwrap_err()
        .contains("Adjacent template pattern captures"));
}

#[test]
fn test_expression_in_capture_error() {
    let result = parse_pattern("`${x + y}`");
    assert!(result.is_err());
    assert!(result
        .unwrap_err()
        .contains("Template pattern captures must be simple identifiers"));
}

#[test]
fn test_no_captures_becomes_literal() {
    let result = parse_pattern("`hello`");
    assert!(result.is_ok());

    match result.unwrap() {
        Pattern::Literal(Literal::String(s), _) => {
            assert_eq!(s, "hello");
        }
        _ => panic!("Expected literal pattern for template without captures"),
    }
}

#[test]
fn test_multiple_delimiters() {
    let result = parse_pattern("`${year}-${month}-${day}`");
    assert!(result.is_ok());

    match result.unwrap() {
        Pattern::Template(template) => {
            assert_eq!(template.parts.len(), 5);
            assert!(matches!(&template.parts[0], TemplatePatternPart::Capture(_)));
            assert!(matches!(
                &template.parts[1],
                TemplatePatternPart::String(s) if s == "-"
            ));
            assert!(matches!(&template.parts[2], TemplatePatternPart::Capture(_)));
            assert!(matches!(
                &template.parts[3],
                TemplatePatternPart::String(s) if s == "-"
            ));
            assert!(matches!(&template.parts[4], TemplatePatternPart::Capture(_)));
        }
        _ => panic!("Expected template pattern"),
    }
}

#[test]
fn test_special_chars() {
    let result = parse_pattern("`100% ${value}`");
    assert!(result.is_ok());

    match result.unwrap() {
        Pattern::Template(template) => {
            assert_eq!(template.parts.len(), 2);
            assert!(matches!(
                &template.parts[0],
                TemplatePatternPart::String(s) if s == "100% "
            ));
            assert!(matches!(&template.parts[1], TemplatePatternPart::Capture(_)));
        }
        _ => panic!("Expected template pattern"),
    }
}
