use super::{Parser, ParserError, PatternParser, StatementParser, TypeParser};
use crate::ast::expression::*;
use crate::lexer::TokenKind;

pub trait ExpressionParser {
    fn parse_expression(&mut self) -> Result<Expression, ParserError>;
}

impl ExpressionParser for Parser<'_> {
    fn parse_expression(&mut self) -> Result<Expression, ParserError> {
        self.parse_assignment()
    }
}

// Expression parsing using Pratt parsing for precedence
impl Parser<'_> {
    fn parse_assignment(&mut self) -> Result<Expression, ParserError> {
        // Check for arrow function: identifier => expr or (params) => expr
        let checkpoint = self.position;

        // Try to parse as arrow function
        if let Ok(arrow) = self.try_parse_arrow_function() {
            return Ok(arrow);
        }

        // Reset and parse normally
        self.position = checkpoint;

        let expr = self.parse_conditional()?;

        if let Some(op) = self.match_assignment_op() {
            let right = self.parse_assignment()?;
            let span = expr.span.combine(&right.span);
            return Ok(Expression {
                kind: ExpressionKind::Assignment(Box::new(expr), op, Box::new(right)),
                span,
                ..Default::default()
            });
        }

        Ok(expr)
    }

    fn try_parse_arrow_function(&mut self) -> Result<Expression, ParserError> {
        let start_span = self.current_span();

        // Parse parameters - either single identifier or (param list)
        let parameters = if self.check(&TokenKind::LeftParen) {
            self.advance();
            let params = self.parse_parameter_list()?;
            self.consume(TokenKind::RightParen, "Expected ')'")?;
            params
        } else if matches!(&self.current().kind, TokenKind::Identifier(_)) {
            // Single parameter without parens
            let param_name = self.parse_identifier()?;
            vec![crate::ast::statement::Parameter {
                pattern: crate::ast::pattern::Pattern::Identifier(param_name),
                type_annotation: None,
                default: None,
                is_rest: false,
                is_optional: false,
                span: start_span,
            }]
        } else {
            return Err(ParserError {
                message: "Expected parameter or '(' in arrow function".to_string(),
                span: start_span,
            });
        };

        // Optional return type
        let return_type = if self.match_token(&[TokenKind::Colon]) {
            Some(self.parse_type()?)
        } else {
            None
        };

        // Must have =>
        self.consume(TokenKind::FatArrow, "Expected '=>' in arrow function")?;

        // Parse body - either expression or block
        let body = if self.check(&TokenKind::LeftBrace) {
            self.advance();
            let block = self.parse_block()?;
            self.consume(
                TokenKind::RightBrace,
                "Expected '}' after arrow function body",
            )?;
            ArrowBody::Block(block)
        } else {
            let expr = self.parse_assignment()?;
            ArrowBody::Expression(Box::new(expr))
        };

        let end_span = self.current_span();

        Ok(Expression {
            kind: ExpressionKind::Arrow(ArrowFunction {
                parameters,
                return_type,
                body,
                span: start_span.combine(&end_span),
            }),
            span: start_span.combine(&end_span),
            ..Default::default()
        })
    }

    fn parse_conditional(&mut self) -> Result<Expression, ParserError> {
        // Check for try expression at the beginning
        if self.check(&TokenKind::Try) {
            return self.parse_try_expression_full();
        }

        let expr = self.parse_logical_or()?;

        if self.match_token(&[TokenKind::Question]) {
            let then_expr = self.parse_expression()?;
            self.consume(TokenKind::Colon, "Expected ':' in conditional expression")?;
            let else_expr = self.parse_expression()?;
            let span = expr.span.combine(&else_expr.span);
            return Ok(Expression {
                kind: ExpressionKind::Conditional(
                    Box::new(expr),
                    Box::new(then_expr),
                    Box::new(else_expr),
                ),
                span,
                ..Default::default()
            });
        }

        Ok(expr)
    }

    fn parse_try_expression_full(&mut self) -> Result<Expression, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::Try, "Expected 'try'")?;

        let expression = self.parse_expression()?;
        self.consume(TokenKind::Catch, "Expected 'catch' after try expression")?;

        let catch_variable: crate::ast::Ident;
        let catch_expression: Expression;

        // Check if next token is an identifier (catch variable) or the fallback
        if matches!(&self.current().kind, TokenKind::Identifier(_))
            && !matches!(self.nth_token_kind(1), Some(TokenKind::Catch))
        {
            // Parse catch variable first, then fallback
            catch_variable = self.parse_identifier()?;
            self.consume(TokenKind::Catch, "Expected 'catch' after catch variable")?;
            catch_expression = self.parse_expression()?;
        } else {
            // No catch variable, use __error as the variable name
            let error_var = self.interner.intern("__error");
            catch_variable = crate::ast::Spanned::new(error_var, self.current_span());
            catch_expression = self.parse_expression()?;
        }

        let end_span = catch_expression.span;

        Ok(Expression {
            kind: ExpressionKind::Try(TryExpression {
                expression: Box::new(expression),
                catch_variable,
                catch_expression: Box::new(catch_expression),
                span: start_span.combine(&end_span),
            }),
            span: start_span.combine(&end_span),
            ..Default::default()
        })
    }

    fn parse_logical_or(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.parse_null_coalesce()?;

        while self.match_token(&[TokenKind::Or]) {
            let right = self.parse_null_coalesce()?;
            let span = expr.span.combine(&right.span);
            expr = Expression {
                kind: ExpressionKind::Binary(BinaryOp::Or, Box::new(expr), Box::new(right)),
                span,
                ..Default::default()
            };
        }

        Ok(expr)
    }

    fn parse_null_coalesce(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.parse_logical_and()?;

        while self.match_token(&[TokenKind::QuestionQuestion]) {
            let right = self.parse_logical_and()?;
            let span = expr.span.combine(&right.span);
            expr = Expression {
                kind: ExpressionKind::Binary(
                    BinaryOp::NullCoalesce,
                    Box::new(expr),
                    Box::new(right),
                ),
                span,
                ..Default::default()
            };
        }

        Ok(expr)
    }

    fn parse_logical_and(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.parse_bitwise_or()?;

        while self.match_token(&[TokenKind::And]) {
            let right = self.parse_bitwise_or()?;
            let span = expr.span.combine(&right.span);
            expr = Expression {
                kind: ExpressionKind::Binary(BinaryOp::And, Box::new(expr), Box::new(right)),
                span,
                ..Default::default()
            };
        }

        Ok(expr)
    }

    fn parse_bitwise_or(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.parse_bitwise_xor()?;

        while self.match_token(&[TokenKind::Pipe]) {
            let right = self.parse_bitwise_xor()?;
            let span = expr.span.combine(&right.span);
            expr = Expression {
                kind: ExpressionKind::Binary(BinaryOp::BitwiseOr, Box::new(expr), Box::new(right)),
                span,
                ..Default::default()
            };
        }

        Ok(expr)
    }

    fn parse_bitwise_xor(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.parse_bitwise_and()?;

        while self.match_token(&[TokenKind::Tilde]) {
            let right = self.parse_bitwise_and()?;
            let span = expr.span.combine(&right.span);
            expr = Expression {
                kind: ExpressionKind::Binary(BinaryOp::BitwiseXor, Box::new(expr), Box::new(right)),
                span,
                ..Default::default()
            };
        }

        Ok(expr)
    }

    fn parse_bitwise_and(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.parse_equality()?;

        while self.match_token(&[TokenKind::Ampersand]) {
            let right = self.parse_equality()?;
            let span = expr.span.combine(&right.span);
            expr = Expression {
                kind: ExpressionKind::Binary(BinaryOp::BitwiseAnd, Box::new(expr), Box::new(right)),
                span,
                ..Default::default()
            };
        }

        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.parse_comparison()?;

        while let Some(op) = self.match_equality_op() {
            let right = self.parse_comparison()?;
            let span = expr.span.combine(&right.span);
            expr = Expression {
                kind: ExpressionKind::Binary(op, Box::new(expr), Box::new(right)),
                span,
                ..Default::default()
            };
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.parse_concatenation()?;

        while let Some(op) = self.match_comparison_op() {
            let right = self.parse_concatenation()?;
            let span = expr.span.combine(&right.span);
            expr = Expression {
                kind: ExpressionKind::Binary(op, Box::new(expr), Box::new(right)),
                span,
                ..Default::default()
            };
        }

        Ok(expr)
    }

    fn parse_concatenation(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.parse_shift()?;

        while self.match_token(&[TokenKind::DotDot]) {
            let right = self.parse_shift()?;
            let span = expr.span.combine(&right.span);
            expr = Expression {
                kind: ExpressionKind::Binary(
                    BinaryOp::Concatenate,
                    Box::new(expr),
                    Box::new(right),
                ),
                span,
                ..Default::default()
            };
        }

        Ok(expr)
    }

    fn parse_shift(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.parse_additive()?;

        while let Some(op) = self.match_shift_op() {
            let right = self.parse_additive()?;
            let span = expr.span.combine(&right.span);
            expr = Expression {
                kind: ExpressionKind::Binary(op, Box::new(expr), Box::new(right)),
                span,
                ..Default::default()
            };
        }

        Ok(expr)
    }

    fn parse_additive(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.parse_multiplicative()?;

        while let Some(op) = self.match_additive_op() {
            let right = self.parse_multiplicative()?;
            let span = expr.span.combine(&right.span);
            expr = Expression {
                kind: ExpressionKind::Binary(op, Box::new(expr), Box::new(right)),
                span,
                ..Default::default()
            };
        }

        Ok(expr)
    }

    fn parse_multiplicative(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.parse_power()?;

        while let Some(op) = self.match_multiplicative_op() {
            let right = self.parse_power()?;
            let span = expr.span.combine(&right.span);
            expr = Expression {
                kind: ExpressionKind::Binary(op, Box::new(expr), Box::new(right)),
                span,
                ..Default::default()
            };
        }

        Ok(expr)
    }

    fn parse_power(&mut self) -> Result<Expression, ParserError> {
        let expr = self.parse_unary()?;

        if self.match_token(&[TokenKind::Caret]) {
            let right = self.parse_power()?; // Right associative
            let span = expr.span.combine(&right.span);
            return Ok(Expression {
                kind: ExpressionKind::Binary(BinaryOp::Power, Box::new(expr), Box::new(right)),
                span,
                ..Default::default()
            });
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expression, ParserError> {
        // Handle 'new' keyword for class instantiation
        if self.check(&TokenKind::New) {
            let start_span = self.current_span();
            self.advance(); // consume 'new'

            let constructor = self.parse_postfix()?;

            // new expressions must be followed by a call
            if let ExpressionKind::Call(callee, args, _) = constructor.kind {
                let span = start_span.combine(&constructor.span);
                return Ok(Expression {
                    kind: ExpressionKind::New(callee, args),
                    span,
                    ..Default::default()
                });
            } else {
                return Err(ParserError {
                    message: "Expected function call after 'new' keyword".to_string(),
                    span: constructor.span,
                });
            }
        }

        if let Some(op) = self.match_unary_op() {
            let expr = self.parse_unary()?;
            let start_span = self.current_span();
            let span = start_span.combine(&expr.span);
            return Ok(Expression {
                kind: ExpressionKind::Unary(op, Box::new(expr)),
                span,
                ..Default::default()
            });
        }

        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.parse_primary()?;

        loop {
            match &self.current().kind {
                TokenKind::Dot => {
                    self.advance();
                    let member = self.parse_identifier()?;
                    let span = expr.span.combine(&member.span);
                    expr = Expression {
                        kind: ExpressionKind::Member(Box::new(expr), member),
                        span,
                        ..Default::default()
                    };
                }
                TokenKind::LeftBracket => {
                    self.advance();
                    let index = self.parse_expression()?;
                    self.consume(TokenKind::RightBracket, "Expected ']' after index")?;
                    let span = expr.span.combine(&index.span);
                    expr = Expression {
                        kind: ExpressionKind::Index(Box::new(expr), Box::new(index)),
                        span,
                        ..Default::default()
                    };
                }
                TokenKind::LeftParen => {
                    self.advance();
                    let arguments = self.parse_argument_list()?;
                    let end_span = self.current_span();
                    self.consume(TokenKind::RightParen, "Expected ')' after arguments")?;
                    let span = expr.span.combine(&end_span);
                    expr = Expression {
                        kind: ExpressionKind::Call(Box::new(expr), arguments, None),
                        span,
                        ..Default::default()
                    };
                }
                // Try to parse `expr<TypeArgs>(args)` as a generic function call.
                // Uses backtracking: if `<TypeArgs>` doesn't end with `>` followed
                // by `(`, treat `<` as the comparison operator instead.
                TokenKind::LessThan => {
                    let checkpoint = self.position;
                    self.advance(); // consume '<'
                    if let Ok(type_args) = self.parse_type_arguments() {
                        if self.check(&TokenKind::GreaterThan) {
                            self.advance(); // consume '>'
                            if self.check(&TokenKind::LeftParen) {
                                self.advance(); // consume '('
                                let arguments = self.parse_argument_list()?;
                                let end_span = self.current_span();
                                self.consume(
                                    TokenKind::RightParen,
                                    "Expected ')' after arguments",
                                )?;
                                let span = expr.span.combine(&end_span);
                                expr = Expression {
                                    kind: ExpressionKind::Call(
                                        Box::new(expr),
                                        arguments,
                                        Some(type_args),
                                    ),
                                    span,
                                    ..Default::default()
                                };
                                continue;
                            }
                        }
                    }
                    // Backtrack: not a generic call, treat '<' as comparison
                    self.position = checkpoint;
                    break;
                }
                TokenKind::ColonColon => {
                    self.advance();
                    let method = self.parse_identifier()?;
                    self.consume(TokenKind::LeftParen, "Expected '(' after method name")?;
                    let arguments = self.parse_argument_list()?;
                    let end_span = self.current_span();
                    self.consume(TokenKind::RightParen, "Expected ')' after arguments")?;
                    let span = expr.span.combine(&end_span);
                    expr = Expression {
                        kind: ExpressionKind::MethodCall(Box::new(expr), method, arguments, None),
                        span,
                        ..Default::default()
                    };
                }
                TokenKind::PipeOp => {
                    self.advance();
                    let right = self.parse_unary()?;
                    let span = expr.span.combine(&right.span);
                    expr = Expression {
                        kind: ExpressionKind::Pipe(Box::new(expr), Box::new(right)),
                        span,
                        ..Default::default()
                    };
                }
                TokenKind::QuestionDot => {
                    self.advance();
                    match &self.current().kind {
                        TokenKind::LeftBracket => {
                            self.advance();
                            let index = self.parse_expression()?;
                            self.consume(TokenKind::RightBracket, "Expected ']' after index")?;
                            let span = expr.span.combine(&index.span);
                            expr = Expression {
                                kind: ExpressionKind::OptionalIndex(
                                    Box::new(expr),
                                    Box::new(index),
                                ),
                                span,
                                ..Default::default()
                            };
                        }
                        TokenKind::LeftParen => {
                            self.advance();
                            let arguments = self.parse_argument_list()?;
                            let end_span = self.current_span();
                            self.consume(TokenKind::RightParen, "Expected ')' after arguments")?;
                            let span = expr.span.combine(&end_span);
                            expr = Expression {
                                kind: ExpressionKind::OptionalCall(Box::new(expr), arguments, None),
                                span,
                                ..Default::default()
                            };
                        }
                        TokenKind::ColonColon => {
                            self.advance();
                            let method = self.parse_identifier()?;
                            self.consume(TokenKind::LeftParen, "Expected '(' after method name")?;
                            let arguments = self.parse_argument_list()?;
                            let end_span = self.current_span();
                            self.consume(TokenKind::RightParen, "Expected ')' after arguments")?;
                            let span = expr.span.combine(&end_span);
                            expr = Expression {
                                kind: ExpressionKind::OptionalMethodCall(
                                    Box::new(expr),
                                    method,
                                    arguments,
                                    None,
                                ),
                                span,
                                ..Default::default()
                            };
                        }
                        _ => {
                            let member = self.parse_identifier()?;
                            let span = expr.span.combine(&member.span);
                            expr = Expression {
                                kind: ExpressionKind::OptionalMember(Box::new(expr), member),
                                span,
                                ..Default::default()
                            };
                        }
                    }
                }
                TokenKind::BangBang => {
                    self.advance();
                    let right = self.parse_postfix()?;
                    let span = expr.span.combine(&right.span);
                    expr = Expression {
                        kind: ExpressionKind::ErrorChain(Box::new(expr), Box::new(right)),
                        span,
                        ..Default::default()
                    };
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expression, ParserError> {
        let start_span = self.current_span();

        match &self.current().kind.clone() {
            TokenKind::Nil => {
                self.advance();
                Ok(Expression {
                    kind: ExpressionKind::Literal(Literal::Nil),
                    span: start_span,
                    ..Default::default()
                })
            }
            TokenKind::True => {
                self.advance();
                Ok(Expression {
                    kind: ExpressionKind::Literal(Literal::Boolean(true)),
                    span: start_span,
                    ..Default::default()
                })
            }
            TokenKind::False => {
                self.advance();
                Ok(Expression {
                    kind: ExpressionKind::Literal(Literal::Boolean(false)),
                    span: start_span,
                    ..Default::default()
                })
            }
            TokenKind::Number(s) => {
                let num = if s.starts_with("0x") || s.starts_with("0X") {
                    // Parse hexadecimal number
                    i64::from_str_radix(&s[2..], 16)
                        .map(|i| i as f64)
                        .map_err(|_| ParserError {
                            message: "Invalid hexadecimal number literal".to_string(),
                            span: start_span,
                        })?
                } else if s.starts_with("0b") || s.starts_with("0B") {
                    // Parse binary number
                    i64::from_str_radix(&s[2..], 2)
                        .map(|i| i as f64)
                        .map_err(|_| ParserError {
                            message: "Invalid binary number literal".to_string(),
                            span: start_span,
                        })?
                } else {
                    // Parse decimal number
                    s.parse::<f64>().map_err(|_| ParserError {
                        message: "Invalid number literal".to_string(),
                        span: start_span,
                    })?
                };
                self.advance();
                Ok(Expression {
                    kind: ExpressionKind::Literal(Literal::Number(num)),
                    span: start_span,
                    ..Default::default()
                })
            }
            TokenKind::String(s) => {
                let string = s.clone();
                self.advance();
                Ok(Expression {
                    kind: ExpressionKind::Literal(Literal::String(string)),
                    span: start_span,
                    ..Default::default()
                })
            }
            TokenKind::Identifier(name) => {
                let id = *name;
                self.advance();
                Ok(Expression {
                    kind: ExpressionKind::Identifier(id),
                    span: start_span,
                    ..Default::default()
                })
            }
            TokenKind::LeftParen => {
                let mut paren_count = 0;

                while self.check(&TokenKind::LeftParen) {
                    self.advance();
                    paren_count += 1;
                }

                let mut expr = self.parse_expression()?;

                for _ in 0..paren_count {
                    let end_span = self.current_span();
                    self.consume(TokenKind::RightParen, "Expected ')' after expression")?;
                    expr = Expression {
                        kind: ExpressionKind::Parenthesized(Box::new(expr)),
                        span: start_span.combine(&end_span),
                        ..Default::default()
                    };
                }

                Ok(expr)
            }
            TokenKind::LeftBrace => self.parse_object_or_table(),
            TokenKind::LeftBracket => self.parse_array(),
            TokenKind::Function => self.parse_function_expression(),
            TokenKind::Match => self.parse_match_expression(),
            TokenKind::TemplateString(parts) => {
                self.parse_template_literal(parts.clone(), start_span)
            }
            TokenKind::Super => {
                self.advance();
                Ok(Expression {
                    kind: ExpressionKind::SuperKeyword,
                    span: start_span,
                    ..Default::default()
                })
            }
            _ => Err(ParserError {
                message: format!("Unexpected token in expression: {:?}", self.current().kind),
                span: start_span,
            }),
        }
    }

    fn parse_object_or_table(&mut self) -> Result<Expression, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::LeftBrace, "Expected '{'")?;

        let mut properties = Vec::new();

        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            if self.match_token(&[TokenKind::DotDotDot]) {
                let value = self.parse_expression()?;
                let span = start_span.combine(&value.span);
                properties.push(ObjectProperty::Spread {
                    value: Box::new(value),
                    span,
                });
            } else if self.check(&TokenKind::LeftBracket) {
                self.advance();
                let key = self.parse_expression()?;
                self.consume(TokenKind::RightBracket, "Expected ']' after computed key")?;
                self.consume(TokenKind::Equal, "Expected '=' after property key")?;
                let value = self.parse_expression()?;
                let span = start_span.combine(&value.span);
                properties.push(ObjectProperty::Computed {
                    key: Box::new(key),
                    value: Box::new(value),
                    span,
                });
            } else {
                let key = self.parse_identifier()?;
                if !self.match_token(&[TokenKind::Equal, TokenKind::Colon]) {
                    return Err(ParserError {
                        message: "Expected '=' or ':' after property key".to_string(),
                        span: self.current_span(),
                    });
                }
                let value = self.parse_expression()?;
                let span = key.span.combine(&value.span);
                properties.push(ObjectProperty::Property {
                    key,
                    value: Box::new(value),
                    span,
                });
            }

            if !self.check(&TokenKind::RightBrace) {
                self.consume(TokenKind::Comma, "Expected ',' between properties")?;
            }
        }

        let end_span = self.current_span();
        self.consume(TokenKind::RightBrace, "Expected '}' after object")?;

        Ok(Expression {
            kind: ExpressionKind::Object(properties),
            span: start_span.combine(&end_span),
            ..Default::default()
        })
    }

    fn parse_array(&mut self) -> Result<Expression, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::LeftBracket, "Expected '['")?;

        let mut elements = Vec::new();

        while !self.check(&TokenKind::RightBracket) && !self.is_at_end() {
            if self.match_token(&[TokenKind::DotDotDot]) {
                let expr = self.parse_expression()?;
                elements.push(ArrayElement::Spread(expr));
            } else {
                let expr = self.parse_expression()?;
                elements.push(ArrayElement::Expression(expr));
            }

            if !self.check(&TokenKind::RightBracket) {
                self.consume(TokenKind::Comma, "Expected ',' between array elements")?;
            }
        }

        let end_span = self.current_span();
        self.consume(TokenKind::RightBracket, "Expected ']' after array")?;

        Ok(Expression {
            kind: ExpressionKind::Array(elements),
            span: start_span.combine(&end_span),
            ..Default::default()
        })
    }

    fn parse_function_expression(&mut self) -> Result<Expression, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::Function, "Expected 'function'")?;

        let type_parameters = if self.match_token(&[TokenKind::LessThan]) {
            Some(self.parse_type_parameters()?)
        } else {
            None
        };

        self.consume(TokenKind::LeftParen, "Expected '(' after 'function'")?;
        let parameters = self.parse_parameter_list()?;
        self.consume(TokenKind::RightParen, "Expected ')' after parameters")?;

        let return_type = if self.match_token(&[TokenKind::Colon]) {
            Some(self.parse_type()?)
        } else {
            None
        };

        let body = self.parse_block()?;
        self.consume(TokenKind::End, "Expected 'end' after function body")?;
        let end_span = self.current_span();

        Ok(Expression {
            kind: ExpressionKind::Function(FunctionExpression {
                type_parameters,
                parameters,
                return_type,
                body,
                span: start_span.combine(&end_span),
            }),
            span: start_span.combine(&end_span),
            ..Default::default()
        })
    }

    fn parse_match_expression(&mut self) -> Result<Expression, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::Match, "Expected 'match'")?;

        let value = Box::new(self.parse_expression()?);

        self.consume(TokenKind::LeftBrace, "Expected '{' after match value")?;

        let mut arms = Vec::new();
        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            arms.push(self.parse_match_arm()?);
        }

        let end_span = self.current_span();
        self.consume(TokenKind::RightBrace, "Expected '}' after match arms")?;

        Ok(Expression {
            kind: ExpressionKind::Match(MatchExpression {
                value,
                arms,
                span: start_span.combine(&end_span),
            }),
            span: start_span.combine(&end_span),
            ..Default::default()
        })
    }

    fn parse_match_arm(&mut self) -> Result<MatchArm, ParserError> {
        let start_span = self.current_span();
        let pattern = self.parse_pattern()?;

        let guard = if self.match_token(&[TokenKind::When]) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.consume(TokenKind::FatArrow, "Expected '=>' in match arm")?;

        let body = if self.check(&TokenKind::LeftBrace) {
            self.consume(TokenKind::LeftBrace, "Expected '{'")?;
            let block = self.parse_block()?;
            self.consume(TokenKind::RightBrace, "Expected '}'")?;
            MatchArmBody::Block(block)
        } else {
            let expr = self.parse_expression()?;
            MatchArmBody::Expression(Box::new(expr))
        };

        let end_span = self.current_span();

        self.match_token(&[TokenKind::Comma]);

        Ok(MatchArm {
            pattern,
            guard,
            body,
            span: start_span.combine(&end_span),
        })
    }

    fn parse_template_literal(
        &mut self,
        lexer_parts: Vec<crate::lexer::TemplatePart>,
        start_span: crate::span::Span,
    ) -> Result<Expression, ParserError> {
        self.advance(); // Consume the template string token

        let mut ast_parts = Vec::new();

        for lexer_part in lexer_parts {
            match lexer_part {
                crate::lexer::TemplatePart::String(s) => {
                    ast_parts.push(crate::ast::expression::TemplatePart::String(s));
                }
                crate::lexer::TemplatePart::Expression(tokens) => {
                    let handler = self.diagnostic_handler.clone();
                    let mut temp_parser = Parser::new(tokens, handler, self.interner, self.common);
                    let expr = temp_parser.parse_expression()?;
                    ast_parts.push(crate::ast::expression::TemplatePart::Expression(Box::new(
                        expr,
                    )));
                }
            }
        }

        Ok(Expression {
            kind: ExpressionKind::Template(TemplateLiteral {
                parts: ast_parts,
                span: start_span,
            }),
            span: start_span,
            ..Default::default()
        })
    }

    fn parse_argument_list(&mut self) -> Result<Vec<Argument>, ParserError> {
        let mut arguments = Vec::new();

        if self.check(&TokenKind::RightParen) {
            return Ok(arguments);
        }

        loop {
            let start_span = self.current_span();
            let is_spread = self.match_token(&[TokenKind::DotDotDot]);

            // If we have a spread operator but no expression (just `...`),
            // create a placeholder expression
            let (value, span) = if is_spread
                && (self.check(&TokenKind::RightParen) || self.check(&TokenKind::Comma))
            {
                let placeholder =
                    Expression::new(ExpressionKind::Literal(Literal::Nil), start_span);
                (placeholder, start_span)
            } else {
                let expr = self.parse_expression()?;
                let span = start_span.combine(&expr.span);
                (expr, span)
            };

            arguments.push(Argument {
                value,
                is_spread,
                span,
            });

            if !self.match_token(&[TokenKind::Comma]) {
                break;
            }
        }

        Ok(arguments)
    }

    // Operator matching helpers

    fn match_assignment_op(&mut self) -> Option<AssignmentOp> {
        let op = match &self.current().kind {
            TokenKind::Equal => Some(AssignmentOp::Assign),
            TokenKind::PlusEqual => Some(AssignmentOp::AddAssign),
            TokenKind::MinusEqual => Some(AssignmentOp::SubtractAssign),
            TokenKind::StarEqual => Some(AssignmentOp::MultiplyAssign),
            TokenKind::SlashEqual => Some(AssignmentOp::DivideAssign),
            TokenKind::PercentEqual => Some(AssignmentOp::ModuloAssign),
            TokenKind::CaretEqual => Some(AssignmentOp::PowerAssign),
            TokenKind::DotDotEqual => Some(AssignmentOp::ConcatenateAssign),
            TokenKind::AmpersandEqual => Some(AssignmentOp::BitwiseAndAssign),
            TokenKind::PipeEqual => Some(AssignmentOp::BitwiseOrAssign),
            TokenKind::SlashSlashEqual => Some(AssignmentOp::FloorDivideAssign),
            TokenKind::LessLessEqual => Some(AssignmentOp::LeftShiftAssign),
            TokenKind::GreaterGreaterEqual => Some(AssignmentOp::RightShiftAssign),
            _ => None,
        };

        if op.is_some() {
            self.advance();
        }

        op
    }

    fn match_equality_op(&mut self) -> Option<BinaryOp> {
        let op = match &self.current().kind {
            TokenKind::EqualEqual => Some(BinaryOp::Equal),
            TokenKind::BangEqual | TokenKind::TildeEqual => Some(BinaryOp::NotEqual),
            _ => None,
        };

        if op.is_some() {
            self.advance();
        }

        op
    }

    fn match_comparison_op(&mut self) -> Option<BinaryOp> {
        let op = match &self.current().kind {
            TokenKind::LessThan => Some(BinaryOp::LessThan),
            TokenKind::LessEqual => Some(BinaryOp::LessThanOrEqual),
            TokenKind::GreaterThan => Some(BinaryOp::GreaterThan),
            TokenKind::GreaterEqual => Some(BinaryOp::GreaterThanOrEqual),
            _ => None,
        };

        if op.is_some() {
            self.advance();
        }

        op
    }

    fn match_shift_op(&mut self) -> Option<BinaryOp> {
        let op = match &self.current().kind {
            TokenKind::LessLess => Some(BinaryOp::ShiftLeft),
            TokenKind::GreaterGreater => Some(BinaryOp::ShiftRight),
            _ => None,
        };

        if op.is_some() {
            self.advance();
        }

        op
    }

    fn match_additive_op(&mut self) -> Option<BinaryOp> {
        let op = match &self.current().kind {
            TokenKind::Plus => Some(BinaryOp::Add),
            TokenKind::Minus => Some(BinaryOp::Subtract),
            _ => None,
        };

        if op.is_some() {
            self.advance();
        }

        op
    }

    fn match_multiplicative_op(&mut self) -> Option<BinaryOp> {
        let op = match &self.current().kind {
            TokenKind::Star => Some(BinaryOp::Multiply),
            TokenKind::Slash => Some(BinaryOp::Divide),
            TokenKind::SlashSlash => Some(BinaryOp::IntegerDivide),
            TokenKind::Percent => Some(BinaryOp::Modulo),
            _ => None,
        };

        if op.is_some() {
            self.advance();
        }

        op
    }

    fn match_unary_op(&mut self) -> Option<UnaryOp> {
        let op = match &self.current().kind {
            TokenKind::Not => Some(UnaryOp::Not),
            TokenKind::Bang => Some(UnaryOp::Not),
            TokenKind::Minus => Some(UnaryOp::Negate),
            TokenKind::Hash => Some(UnaryOp::Length),
            TokenKind::Tilde => Some(UnaryOp::BitwiseNot),
            _ => None,
        };

        if op.is_some() {
            self.advance();
        }

        op
    }
}
