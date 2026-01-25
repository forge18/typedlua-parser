use super::{ExpressionParser, Parser, ParserError};
use crate::ast::expression::Literal;
use crate::ast::pattern::*;
use crate::ast::Spanned;
use crate::lexer::TokenKind;

pub trait PatternParser {
    fn parse_pattern(&mut self) -> Result<Pattern, ParserError>;
}

impl PatternParser for Parser<'_> {
    fn parse_pattern(&mut self) -> Result<Pattern, ParserError> {
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
            TokenKind::True => {
                self.advance();
                Ok(Pattern::Literal(Literal::Boolean(true), start_span))
            }
            TokenKind::False => {
                self.advance();
                Ok(Pattern::Literal(Literal::Boolean(false), start_span))
            }
            TokenKind::Nil => {
                self.advance();
                Ok(Pattern::Literal(Literal::Nil, start_span))
            }
            TokenKind::LeftBracket => self.parse_array_pattern(),
            TokenKind::LeftBrace => self.parse_object_pattern(),
            _ => Err(ParserError {
                message: format!("Unexpected token in pattern: {:?}", self.current().kind),
                span: start_span,
            }),
        }
    }
}

impl Parser<'_> {
    fn parse_array_pattern(&mut self) -> Result<Pattern, ParserError> {
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
                // Regular pattern
                let pattern = self.parse_pattern()?;
                elements.push(ArrayPatternElement::Pattern(pattern));
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

        Ok(Pattern::Array(ArrayPattern {
            elements,
            span: start_span.combine(&end_span),
        }))
    }

    fn parse_object_pattern(&mut self) -> Result<Pattern, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::LeftBrace, "Expected '{'")?;

        let mut properties = Vec::new();

        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
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

        Ok(Pattern::Object(ObjectPattern {
            properties,
            span: start_span.combine(&end_span),
        }))
    }
}
