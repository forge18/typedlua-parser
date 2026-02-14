use super::types::TypeParser;
use super::{ExpressionParser, Parser, ParserError};
use crate::ast::expression::Literal;
use crate::ast::pattern::*;
use crate::ast::Spanned;
use crate::lexer::TokenKind;
use crate::span::Span;

pub trait PatternParser<'arena> {
    fn parse_pattern(&mut self) -> Result<Pattern<'arena>, ParserError>;
}

impl<'a, 'arena> PatternParser<'arena> for Parser<'a, 'arena> {
    fn parse_pattern(&mut self) -> Result<Pattern<'arena>, ParserError> {
        self.parse_or_pattern()
    }
}

impl<'a, 'arena> Parser<'a, 'arena> {
    fn parse_or_pattern(&mut self) -> Result<Pattern<'arena>, ParserError> {
        let mut alternatives = vec![self.parse_primary_pattern()?];

        // Keep consuming | tokens while in pattern context
        while self.check(&TokenKind::Pipe) {
            self.advance();
            alternatives.push(self.parse_primary_pattern()?);
        }

        if alternatives.len() == 1 {
            // Single alternative - not an or-pattern
            Ok(alternatives.into_iter().next().unwrap())
        } else {
            // Multiple alternatives - create or-pattern
            let span = alternatives
                .first()
                .unwrap()
                .span()
                .combine(&alternatives.last().unwrap().span());
            let alternatives = self.alloc_vec(alternatives);
            Ok(Pattern::Or(OrPattern { alternatives, span }))
        }
    }

    fn parse_primary_pattern(&mut self) -> Result<Pattern<'arena>, ParserError> {
        let start_span = self.current_span();

        match &self.current().kind.clone() {
            TokenKind::Identifier(name) if self.interner.resolve(*name) == "_" => {
                self.advance();
                Ok(Pattern::Wildcard(start_span))
            }
            TokenKind::Identifier(name) => {
                let id = *name;
                self.advance();
                Ok(Pattern::Identifier(Spanned::new(id, start_span)))
            }
            // Boolean literals must be checked before the general keyword check
            TokenKind::True => {
                self.advance();
                Ok(Pattern::Literal(Literal::Boolean(true), start_span))
            }
            TokenKind::False => {
                self.advance();
                Ok(Pattern::Literal(Literal::Boolean(false), start_span))
            }
            // Allow keywords as identifiers in patterns (for function type parameters)
            kind if kind.is_keyword() => {
                if let Some(s) = kind.to_keyword_str() {
                    let id = self.interner.intern(s);
                    self.advance();
                    Ok(Pattern::Identifier(Spanned::new(id, start_span)))
                } else {
                    Err(ParserError {
                        message: format!(
                            "Internal error: keyword {:?} missing string representation",
                            kind
                        ),
                        span: start_span,
                    })
                }
            }
            TokenKind::Number(s) => {
                let num = s.parse::<f64>().map_err(|_| ParserError {
                    message: "Invalid number in pattern".to_string(),
                    span: start_span,
                })?;
                self.advance();
                Ok(Pattern::Literal(Literal::Number(num), start_span))
            }
            TokenKind::String(s) => {
                let string = s.clone();
                self.advance();
                Ok(Pattern::Literal(Literal::String(string), start_span))
            }
            TokenKind::Nil => {
                self.advance();
                Ok(Pattern::Literal(Literal::Nil, start_span))
            }
            TokenKind::LeftBracket => self.parse_array_pattern(),
            TokenKind::LeftBrace => self.parse_object_pattern(),
            TokenKind::DotDotDot => {
                // Variadic parameter: ...
                self.advance();
                Ok(Pattern::Wildcard(start_span))
            }
            TokenKind::TemplateString(parts) => {
                let parts_clone = parts.clone();
                self.parse_template_pattern(parts_clone, start_span)
            }
            _ => Err(ParserError {
                message: format!("Unexpected token in pattern: {:?}", self.current().kind),
                span: start_span,
            }),
        }
    }
}

impl<'a, 'arena> Parser<'a, 'arena> {
    fn parse_array_pattern(&mut self) -> Result<Pattern<'arena>, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::LeftBracket, "Expected '['")?;

        let mut elements = Vec::new();

        while !self.check(&TokenKind::RightBracket) && !self.is_at_end() {
            if self.match_token(&[TokenKind::DotDotDot]) {
                // Rest pattern: ...name
                let name = match &self.current().kind {
                    TokenKind::Identifier(s) => {
                        let span = self.current_span();
                        let ident = Spanned::new(*s, span);
                        self.advance();
                        ident
                    }
                    _ => {
                        return Err(ParserError {
                            message: "Expected identifier after '...'".to_string(),
                            span: self.current_span(),
                        })
                    }
                };
                elements.push(ArrayPatternElement::Rest(name));
            } else if self.match_token(&[TokenKind::Comma]) {
                // Hole: skipped element
                elements.push(ArrayPatternElement::Hole);
                continue;
            } else {
                // Regular pattern with optional type annotation and default value
                let pattern = self.parse_pattern()?;
                // Consume optional type annotation: a: number
                if self.match_token(&[TokenKind::Colon]) {
                    let _type_ann = self.parse_type()?;
                }
                let default = if self.match_token(&[TokenKind::Equal]) {
                    Some(self.parse_expression()?)
                } else {
                    None
                };
                elements.push(ArrayPatternElement::Pattern(PatternWithDefault {
                    pattern,
                    default,
                }));
            }

            if !self.check(&TokenKind::RightBracket) {
                self.consume(
                    TokenKind::Comma,
                    "Expected ',' between array pattern elements",
                )?;
            }
        }

        let end_span = self.current_span();
        self.consume(TokenKind::RightBracket, "Expected ']' after array pattern")?;

        let elements = self.alloc_vec(elements);
        Ok(Pattern::Array(ArrayPattern {
            elements,
            span: start_span.combine(&end_span),
        }))
    }

    fn parse_object_pattern(&mut self) -> Result<Pattern<'arena>, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::LeftBrace, "Expected '{'")?;

        let mut properties = Vec::new();
        let mut rest = None;

        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            // Check for rest pattern: ...name
            if self.match_token(&[TokenKind::DotDotDot]) {
                let name = match &self.current().kind {
                    TokenKind::Identifier(s) => {
                        let span = self.current_span();
                        let ident = Spanned::new(*s, span);
                        self.advance();
                        ident
                    }
                    _ => {
                        return Err(ParserError {
                            message: "Expected identifier after '...'".to_string(),
                            span: self.current_span(),
                        })
                    }
                };
                rest = Some(name);
                // Consume trailing comma if present
                if self.check(&TokenKind::Comma) {
                    self.advance();
                }
                break;
            }

            // Check for computed property: [expr]: pattern
            if self.check(&TokenKind::LeftBracket) {
                self.advance();
                let key_expr = self.parse_expression()?;
                self.consume(TokenKind::RightBracket, "Expected ']' after computed key")?;
                self.consume(TokenKind::Colon, "Expected ':' after computed key")?;
                let value_pattern = self.parse_pattern()?;
                let default = if self.match_token(&[TokenKind::Equal]) {
                    Some(self.parse_expression()?)
                } else {
                    None
                };

                // Store computed key with a placeholder key identifier
                let key_str = format!("__computed_{}", properties.len());
                let key_id = self.interner.intern(&key_str);
                let prop_span = key_expr.span;
                let key_ident = Spanned::new(key_id, prop_span);

                properties.push(ObjectPatternProperty {
                    key: key_ident,
                    computed_key: Some(key_expr),
                    value: Some(value_pattern),
                    default,
                    span: prop_span,
                });

                if !self.check(&TokenKind::RightBrace) {
                    self.consume(
                        TokenKind::Comma,
                        "Expected ',' between object pattern properties",
                    )?;
                }
                continue;
            }

            let key = match &self.current().kind {
                TokenKind::Identifier(s) => {
                    let span = self.current_span();
                    let ident = Spanned::new(*s, span);
                    self.advance();
                    ident
                }
                _ => {
                    return Err(ParserError {
                        message: "Expected identifier as object pattern key".to_string(),
                        span: self.current_span(),
                    })
                }
            };

            let (value, default) = if self.match_token(&[TokenKind::Colon]) {
                // key: pattern or key: pattern = default
                let pattern = self.parse_pattern()?;
                let default = if self.match_token(&[TokenKind::Equal]) {
                    Some(self.parse_expression()?)
                } else {
                    None
                };
                (Some(pattern), default)
            } else if self.match_token(&[TokenKind::Equal]) {
                // key = default (shorthand)
                let default = self.parse_expression()?;
                (None, Some(default))
            } else {
                // Just key (shorthand for key: key)
                (None, None)
            };

            let span = key.span;

            properties.push(ObjectPatternProperty {
                key,
                computed_key: None,
                value,
                default,
                span,
            });

            if !self.check(&TokenKind::RightBrace) {
                self.consume(
                    TokenKind::Comma,
                    "Expected ',' between object pattern properties",
                )?;
            }
        }

        let end_span = self.current_span();
        self.consume(TokenKind::RightBrace, "Expected '}' after object pattern")?;

        let properties = self.alloc_vec(properties);
        Ok(Pattern::Object(ObjectPattern {
            properties,
            rest,
            span: start_span.combine(&end_span),
        }))
    }

    fn parse_template_pattern(
        &mut self,
        parts: Vec<crate::lexer::TemplatePart>,
        start_span: Span,
    ) -> Result<Pattern<'arena>, ParserError> {
        use crate::ast::pattern::TemplatePatternPart;

        self.advance(); // Consume the template string token

        let mut pattern_parts = Vec::new();
        let mut prev_was_capture = false;

        for part in parts {
            match part {
                crate::lexer::TemplatePart::String(s) => {
                    pattern_parts.push(TemplatePatternPart::String(s));
                    prev_was_capture = false;
                }
                crate::lexer::TemplatePart::Expression(tokens) => {
                    // Check for adjacent captures
                    if prev_was_capture {
                        return Err(ParserError {
                            message: "Adjacent template pattern captures are not supported. Add literal text between captures.".to_string(),
                            span: start_span,
                        });
                    }

                    // Parse as identifier only
                    if tokens.len() != 1 {
                        return Err(ParserError {
                            message: "Template pattern captures must be simple identifiers".to_string(),
                            span: start_span,
                        });
                    }

                    let token = &tokens[0];
                    match &token.kind {
                        TokenKind::Identifier(id) => {
                            let ident = Spanned::new(*id, token.span);
                            pattern_parts.push(TemplatePatternPart::Capture(ident));
                            prev_was_capture = true;
                        }
                        _ => {
                            return Err(ParserError {
                                message: "Template pattern captures must be simple identifiers".to_string(),
                                span: token.span,
                            });
                        }
                    }
                }
            }
        }

        // If no captures, convert to literal pattern
        if pattern_parts.iter().all(|p| matches!(p, TemplatePatternPart::String(_))) {
            let combined: String = pattern_parts
                .into_iter()
                .filter_map(|p| match p {
                    TemplatePatternPart::String(s) => Some(s),
                    _ => None,
                })
                .collect();
            return Ok(Pattern::Literal(Literal::String(combined), start_span));
        }

        let parts_arena = self.alloc_vec(pattern_parts);
        Ok(Pattern::Template(crate::ast::pattern::TemplatePattern {
            parts: parts_arena,
            span: start_span,
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostics::CollectingDiagnosticHandler;
    use crate::lexer::Lexer;
    use crate::span::Span;
    use crate::string_interner::StringInterner;
    use std::sync::Arc;

    fn parse_pattern(source: &str) -> Result<Pattern<'static>, ParserError> {
        use bumpalo::Bump;
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common) = StringInterner::new_with_common_identifiers();
        let mut lexer = Lexer::new(source, handler.clone(), &interner);
        let tokens = lexer.tokenize().map_err(|e| ParserError {
            message: format!("Lexer error: {:?}", e),
            span: Span::default(),
        })?;
        let arena = Box::leak(Box::new(Bump::new()));
        let mut parser = Parser::new(tokens, handler, &interner, &common, arena);
        parser.parse_pattern()
    }

    #[test]
    fn test_parse_identifier_pattern() {
        let result = parse_pattern("x");
        assert!(result.is_ok());
        match result.unwrap() {
            Pattern::Identifier(_) => {}
            _ => panic!("Expected identifier pattern"),
        }
    }

    #[test]
    fn test_parse_wildcard_pattern() {
        let result = parse_pattern("_");
        assert!(result.is_ok());
        match result.unwrap() {
            Pattern::Wildcard(_) => {}
            _ => panic!("Expected wildcard pattern"),
        }
    }

    #[test]
    fn test_parse_literal_true_pattern() {
        let result = parse_pattern("true");
        assert!(result.is_ok());
        match result.unwrap() {
            Pattern::Literal(Literal::Boolean(true), _) => {}
            _ => panic!("Expected true literal pattern"),
        }
    }

    #[test]
    fn test_parse_literal_false_pattern() {
        let result = parse_pattern("false");
        assert!(result.is_ok());
        match result.unwrap() {
            Pattern::Literal(Literal::Boolean(false), _) => {}
            _ => panic!("Expected false literal pattern"),
        }
    }

    #[test]
    fn test_parse_literal_nil_pattern() {
        let result = parse_pattern("nil");
        assert!(result.is_ok());
        // nil is parsed as a keyword identifier in patterns
        match result.unwrap() {
            Pattern::Identifier(_) => {}
            _ => panic!("Expected identifier pattern for nil keyword"),
        }
    }

    #[test]
    fn test_parse_literal_number_pattern() {
        let result = parse_pattern("42");
        assert!(result.is_ok());
        match result.unwrap() {
            Pattern::Literal(Literal::Number(n), _) => assert_eq!(n, 42.0),
            _ => panic!("Expected number literal pattern"),
        }
    }

    #[test]
    fn test_parse_literal_string_pattern() {
        let result = parse_pattern("\"hello\"");
        assert!(result.is_ok());
        match result.unwrap() {
            Pattern::Literal(Literal::String(s), _) => assert_eq!(s, "hello"),
            _ => panic!("Expected string literal pattern"),
        }
    }

    #[test]
    fn test_parse_array_pattern() {
        let result = parse_pattern("[a, b, c]");
        assert!(result.is_ok());
        match result.unwrap() {
            Pattern::Array(arr) => {
                assert_eq!(arr.elements.len(), 3);
            }
            _ => panic!("Expected array pattern"),
        }
    }

    #[test]
    fn test_parse_array_pattern_with_rest() {
        let result = parse_pattern("[first, ...rest]");
        assert!(result.is_ok());
        match result.unwrap() {
            Pattern::Array(arr) => {
                assert_eq!(arr.elements.len(), 2);
                match &arr.elements[1] {
                    ArrayPatternElement::Rest(_) => {}
                    _ => panic!("Expected rest element"),
                }
            }
            _ => panic!("Expected array pattern with rest"),
        }
    }

    #[test]
    fn test_parse_array_pattern_with_hole() {
        let result = parse_pattern("[a, , b]");
        assert!(result.is_ok());
        match result.unwrap() {
            Pattern::Array(arr) => {
                assert_eq!(arr.elements.len(), 3);
                match &arr.elements[1] {
                    ArrayPatternElement::Hole => {}
                    _ => panic!("Expected hole element"),
                }
            }
            _ => panic!("Expected array pattern with hole"),
        }
    }

    #[test]
    fn test_parse_empty_array_pattern() {
        let result = parse_pattern("[]");
        assert!(result.is_ok());
        match result.unwrap() {
            Pattern::Array(arr) => {
                assert!(arr.elements.is_empty());
            }
            _ => panic!("Expected empty array pattern"),
        }
    }

    #[test]
    fn test_parse_object_pattern() {
        let result = parse_pattern("{ x, y, z }");
        assert!(result.is_ok());
        match result.unwrap() {
            Pattern::Object(obj) => {
                assert_eq!(obj.properties.len(), 3);
            }
            _ => panic!("Expected object pattern"),
        }
    }

    #[test]
    fn test_parse_object_pattern_with_alias() {
        let result = parse_pattern("{ x: newX }");
        assert!(result.is_ok());
        match result.unwrap() {
            Pattern::Object(obj) => {
                assert_eq!(obj.properties.len(), 1);
                assert!(obj.properties[0].value.is_some());
            }
            _ => panic!("Expected object pattern with alias"),
        }
    }

    #[test]
    fn test_parse_object_pattern_with_default() {
        let result = parse_pattern("{ x = 42 }");
        assert!(result.is_ok());
        match result.unwrap() {
            Pattern::Object(obj) => {
                assert_eq!(obj.properties.len(), 1);
                assert!(obj.properties[0].default.is_some());
            }
            _ => panic!("Expected object pattern with default"),
        }
    }

    #[test]
    fn test_parse_object_pattern_with_alias_and_default() {
        let result = parse_pattern("{ x: newX = 42 }");
        assert!(result.is_ok());
        match result.unwrap() {
            Pattern::Object(obj) => {
                assert_eq!(obj.properties.len(), 1);
                assert!(obj.properties[0].value.is_some());
                assert!(obj.properties[0].default.is_some());
            }
            _ => panic!("Expected object pattern with alias and default"),
        }
    }

    #[test]
    fn test_parse_empty_object_pattern() {
        let result = parse_pattern("{}");
        assert!(result.is_ok());
        match result.unwrap() {
            Pattern::Object(obj) => {
                assert!(obj.properties.is_empty());
            }
            _ => panic!("Expected empty object pattern"),
        }
    }

    #[test]
    fn test_parse_or_pattern() {
        let result = parse_pattern("A | B");
        assert!(result.is_ok());
        match result.unwrap() {
            Pattern::Or(or) => {
                assert_eq!(or.alternatives.len(), 2);
            }
            _ => panic!("Expected or pattern"),
        }
    }

    #[test]
    fn test_parse_or_pattern_multiple() {
        let result = parse_pattern("A | B | C");
        assert!(result.is_ok());
        match result.unwrap() {
            Pattern::Or(or) => {
                assert_eq!(or.alternatives.len(), 3);
            }
            _ => panic!("Expected or pattern with multiple alternatives"),
        }
    }

    #[test]
    fn test_parse_nested_array_pattern() {
        let result = parse_pattern("[[a, b], c]");
        assert!(result.is_ok());
        match result.unwrap() {
            Pattern::Array(arr) => {
                assert_eq!(arr.elements.len(), 2);
            }
            _ => panic!("Expected nested array pattern"),
        }
    }

    #[test]
    fn test_parse_nested_object_pattern() {
        let result = parse_pattern("{ x: { y } }");
        assert!(result.is_ok());
        match result.unwrap() {
            Pattern::Object(obj) => {
                assert_eq!(obj.properties.len(), 1);
                assert!(obj.properties[0].value.is_some());
            }
            _ => panic!("Expected nested object pattern"),
        }
    }

    #[test]
    fn test_parse_variadic_pattern() {
        let result = parse_pattern("...");
        assert!(result.is_ok());
        match result.unwrap() {
            Pattern::Wildcard(_) => {}
            _ => panic!("Expected wildcard pattern for variadic"),
        }
    }

    #[test]
    fn test_parse_keyword_as_identifier() {
        // Keywords should be allowed as identifiers in patterns
        let result = parse_pattern("type");
        assert!(result.is_ok());
        match result.unwrap() {
            Pattern::Identifier(_) => {}
            _ => panic!("Expected keyword as identifier pattern"),
        }
    }

    #[test]
    fn test_parse_complex_pattern() {
        let result = parse_pattern("{ x: [a, b], y: { z } }");
        assert!(result.is_ok());
        match result.unwrap() {
            Pattern::Object(obj) => {
                assert_eq!(obj.properties.len(), 2);
            }
            _ => panic!("Expected complex pattern"),
        }
    }

    #[test]
    fn test_parse_array_pattern_single_element() {
        let result = parse_pattern("[x]");
        assert!(result.is_ok());
        match result.unwrap() {
            Pattern::Array(arr) => {
                assert_eq!(arr.elements.len(), 1);
            }
            _ => panic!("Expected single element array pattern"),
        }
    }

    #[test]
    fn test_parse_object_pattern_single_property() {
        let result = parse_pattern("{ x }");
        assert!(result.is_ok());
        match result.unwrap() {
            Pattern::Object(obj) => {
                assert_eq!(obj.properties.len(), 1);
            }
            _ => panic!("Expected single property object pattern"),
        }
    }

    #[test]
    fn test_parse_or_pattern_with_literals() {
        let result = parse_pattern("1 | 2 | 3");
        assert!(result.is_ok());
        match result.unwrap() {
            Pattern::Or(or) => {
                assert_eq!(or.alternatives.len(), 3);
            }
            _ => panic!("Expected or pattern with literals"),
        }
    }

    #[test]
    fn test_parse_array_pattern_with_only_rest() {
        // Test array pattern with just rest element
        let result = parse_pattern("[...rest]");
        assert!(result.is_ok());
        match result.unwrap() {
            Pattern::Array(arr) => {
                assert_eq!(arr.elements.len(), 1);
                match &arr.elements[0] {
                    ArrayPatternElement::Rest(_) => {}
                    _ => panic!("Expected rest element"),
                }
            }
            _ => panic!("Expected array pattern with rest"),
        }
    }
}
