use super::{Parser, ParserError};
use crate::ast::expression::Literal;
use crate::ast::types::*;
use crate::ast::Spanned;
use crate::lexer::TokenKind;

pub trait TypeParser {
    fn parse_type(&mut self) -> Result<Type, ParserError>;
}

impl TypeParser for Parser<'_> {
    fn parse_type(&mut self) -> Result<Type, ParserError> {
        // Check for type predicate: identifier is Type
        if matches!(&self.current().kind, TokenKind::Identifier(_)) {
            let checkpoint = self.position;
            let start_span = self.current_span();

            // Try to parse identifier
            if let Ok(param_name) = self.parse_identifier() {
                // Check if next token is 'is'
                if self.check(&TokenKind::Is) {
                    self.advance(); // consume 'is'
                    let type_annotation = Box::new(self.parse_union_type()?);
                    let end_span = type_annotation.span;
                    return Ok(Type {
                        kind: TypeKind::TypePredicate(crate::ast::types::TypePredicate {
                            parameter_name: param_name,
                            type_annotation,
                            span: start_span.combine(&end_span),
                        }),
                        span: start_span.combine(&end_span),
                    });
                }
            }

            // Not a type predicate, rewind and parse as normal type
            self.position = checkpoint;
        }

        self.parse_union_type()
    }
}

impl Parser<'_> {
    fn parse_union_type(&mut self) -> Result<Type, ParserError> {
        let mut types = vec![self.parse_intersection_type()?];

        while self.match_token(&[TokenKind::Pipe]) {
            types.push(self.parse_intersection_type()?);
        }

        if types.len() == 1 {
            Ok(types.into_iter().next().unwrap())
        } else {
            let start_span = types.first().unwrap().span;
            let end_span = types.last().unwrap().span;
            Ok(Type {
                kind: TypeKind::Union(types),
                span: start_span.combine(&end_span),
            })
        }
    }

    fn parse_intersection_type(&mut self) -> Result<Type, ParserError> {
        let mut types = vec![self.parse_postfix_type()?];

        while self.match_token(&[TokenKind::Ampersand]) {
            types.push(self.parse_postfix_type()?);
        }

        if types.len() == 1 {
            Ok(types.into_iter().next().unwrap())
        } else {
            let start_span = types.first().unwrap().span;
            let end_span = types.last().unwrap().span;
            Ok(Type {
                kind: TypeKind::Intersection(types),
                span: start_span.combine(&end_span),
            })
        }
    }

    fn parse_postfix_type(&mut self) -> Result<Type, ParserError> {
        let mut ty = self.parse_primary_type()?;

        loop {
            match &self.current().kind {
                TokenKind::LeftBracket => {
                    self.advance();
                    if self.match_token(&[TokenKind::RightBracket]) {
                        // Array type: T[]
                        let end_span = self.current_span();
                        let start_span = ty.span;
                        ty = Type {
                            kind: TypeKind::Array(Box::new(ty)),
                            span: start_span.combine(&end_span),
                        };
                    } else {
                        // Index access type: T[K]
                        let index = self.parse_type()?;
                        self.consume(TokenKind::RightBracket, "Expected ']'")?;
                        let end_span = self.current_span();
                        let start_span = ty.span;
                        ty = Type {
                            kind: TypeKind::IndexAccess(Box::new(ty), Box::new(index)),
                            span: start_span.combine(&end_span),
                        };
                    }
                }
                TokenKind::Question => {
                    // Nullable type: T?
                    self.advance();
                    let end_span = self.current_span();
                    let start_span = ty.span;
                    ty = Type {
                        kind: TypeKind::Nullable(Box::new(ty)),
                        span: start_span.combine(&end_span),
                    };
                }
                _ => break,
            }
        }

        Ok(ty)
    }

    fn parse_primary_type(&mut self) -> Result<Type, ParserError> {
        let start_span = self.current_span();

        match &self.current().kind.clone() {
            // Primitive types
            TokenKind::Identifier(name) => {
                let primitive = match self.resolve(*name) {
                    s if s == "nil" => Some(PrimitiveType::Nil),
                    s if s == "boolean" => Some(PrimitiveType::Boolean),
                    s if s == "number" => Some(PrimitiveType::Number),
                    s if s == "integer" => Some(PrimitiveType::Integer),
                    s if s == "string" => Some(PrimitiveType::String),
                    s if s == "unknown" => Some(PrimitiveType::Unknown),
                    s if s == "never" => Some(PrimitiveType::Never),
                    s if s == "void" => Some(PrimitiveType::Void),
                    s if s == "table" => Some(PrimitiveType::Table),
                    s if s == "coroutine" => Some(PrimitiveType::Coroutine),
                    _ => None,
                };

                if let Some(prim) = primitive {
                    self.advance();
                    return Ok(Type {
                        kind: TypeKind::Primitive(prim),
                        span: start_span,
                    });
                }

                // Type reference
                let name_ident = Spanned::new(*name, start_span);
                self.advance();

                let type_arguments = if self.match_token(&[TokenKind::LessThan]) {
                    let args = self.parse_type_arguments()?;
                    self.consume(TokenKind::GreaterThan, "Expected '>' after type arguments")?;
                    Some(args)
                } else {
                    None
                };

                let end_span = self.current_span();

                Ok(Type {
                    kind: TypeKind::Reference(TypeReference {
                        name: name_ident,
                        type_arguments,
                        span: start_span.combine(&end_span),
                    }),
                    span: start_span.combine(&end_span),
                })
            }

            // Literal types
            TokenKind::Number(s) => {
                let num = s.parse::<f64>().map_err(|_| ParserError {
                    message: "Invalid number in type".to_string(),
                    span: start_span,
                })?;
                self.advance();
                Ok(Type {
                    kind: TypeKind::Literal(Literal::Number(num)),
                    span: start_span,
                })
            }
            TokenKind::String(s) => {
                let string = s.clone();
                self.advance();
                Ok(Type {
                    kind: TypeKind::Literal(Literal::String(string)),
                    span: start_span,
                })
            }
            TokenKind::True => {
                self.advance();
                Ok(Type {
                    kind: TypeKind::Literal(Literal::Boolean(true)),
                    span: start_span,
                })
            }
            TokenKind::False => {
                self.advance();
                Ok(Type {
                    kind: TypeKind::Literal(Literal::Boolean(false)),
                    span: start_span,
                })
            }
            TokenKind::Nil => {
                self.advance();
                Ok(Type {
                    kind: TypeKind::Literal(Literal::Nil),
                    span: start_span,
                })
            }

            // Object type: { ... }
            TokenKind::LeftBrace => self.parse_object_type(),

            // Tuple type: [T, U, V]
            TokenKind::LeftBracket => self.parse_tuple_type(),

            // Function type: (x: T) -> U
            TokenKind::LeftParen => self.parse_function_type(),

            // Parenthesized type: (T)
            _ => Err(ParserError {
                message: format!("Unexpected token in type: {:?}", self.current().kind),
                span: start_span,
            }),
        }
    }

    fn parse_object_type(&mut self) -> Result<Type, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::LeftBrace, "Expected '{'")?;

        let mut members = Vec::new();

        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            // Check for index signature
            if self.check(&TokenKind::LeftBracket) {
                members.push(ObjectTypeMember::Index(self.parse_index_signature()?));
            } else {
                let is_readonly = self.match_token(&[TokenKind::Readonly]);
                let name = self.parse_identifier()?;

                // Check if it's a method or property
                if self.check(&TokenKind::LeftParen) || self.check(&TokenKind::LessThan) {
                    // Method signature
                    let type_parameters = if self.match_token(&[TokenKind::LessThan]) {
                        Some(self.parse_type_parameters()?)
                    } else {
                        None
                    };

                    self.consume(TokenKind::LeftParen, "Expected '('")?;
                    let parameters = self.parse_parameter_list()?;
                    self.consume(TokenKind::RightParen, "Expected ')'")?;

                    self.consume(TokenKind::Colon, "Expected ':' after method parameters")?;
                    let return_type = self.parse_type()?;
                    let span = name.span.combine(&return_type.span);

                    members.push(ObjectTypeMember::Method(
                        crate::ast::statement::MethodSignature {
                            name,
                            type_parameters,
                            parameters,
                            return_type,
                            body: None,
                            span,
                        },
                    ));
                } else {
                    // Property signature
                    let is_optional = self.match_token(&[TokenKind::Question]);
                    self.consume(TokenKind::Colon, "Expected ':' after property name")?;
                    let type_annotation = self.parse_type()?;
                    let span = name.span.combine(&type_annotation.span);

                    members.push(ObjectTypeMember::Property(
                        crate::ast::statement::PropertySignature {
                            is_readonly,
                            name,
                            is_optional,
                            type_annotation,
                            span,
                        },
                    ));
                }

                // Optional comma or semicolon
                self.match_token(&[TokenKind::Comma, TokenKind::Semicolon]);
            }
        }

        let end_span = self.current_span();
        self.consume(TokenKind::RightBrace, "Expected '}' after object type")?;

        Ok(Type {
            kind: TypeKind::Object(ObjectType {
                members,
                span: start_span.combine(&end_span),
            }),
            span: start_span.combine(&end_span),
        })
    }

    fn parse_tuple_type(&mut self) -> Result<Type, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::LeftBracket, "Expected '['")?;

        let mut types = Vec::new();

        if !self.check(&TokenKind::RightBracket) {
            loop {
                types.push(self.parse_type()?);

                if !self.match_token(&[TokenKind::Comma]) {
                    break;
                }
            }
        }

        let end_span = self.current_span();
        self.consume(TokenKind::RightBracket, "Expected ']' after tuple type")?;

        Ok(Type {
            kind: TypeKind::Tuple(types),
            span: start_span.combine(&end_span),
        })
    }

    fn parse_function_type(&mut self) -> Result<Type, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::LeftParen, "Expected '('")?;

        let parameters = self.parse_parameter_list()?;

        self.consume(TokenKind::RightParen, "Expected ')'")?;
        self.consume(TokenKind::Arrow, "Expected '->' in function type")?;

        let return_type = Box::new(self.parse_type()?);
        let end_span = return_type.span;

        Ok(Type {
            kind: TypeKind::Function(FunctionType {
                type_parameters: None,
                parameters,
                return_type,
                throws: None,
                span: start_span.combine(&end_span),
            }),
            span: start_span.combine(&end_span),
        })
    }

    fn parse_type_arguments(&mut self) -> Result<Vec<Type>, ParserError> {
        let mut args = Vec::new();

        loop {
            args.push(self.parse_type()?);

            if !self.match_token(&[TokenKind::Comma]) {
                break;
            }
        }

        Ok(args)
    }
}
