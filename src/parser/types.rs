use super::{ExpressionParser, Parser, ParserError};
use crate::ast::expression::Literal;
use crate::ast::statement::TypeParameter;
use crate::ast::types::*;
use crate::ast::Spanned;
use crate::lexer::TokenKind;
use crate::span::Span;

pub trait TypeParser {
    fn parse_type(&mut self) -> Result<Type, ParserError>;
}

impl TypeParser for Parser<'_> {
    #[inline]
    fn parse_type(&mut self) -> Result<Type, ParserError> {
        if matches!(&self.current().kind, TokenKind::Identifier(_)) {
            let checkpoint = self.position;
            let start_span = self.current_span();

            if let Ok(param_name) = self.parse_identifier() {
                if self.check(&TokenKind::Is) {
                    self.advance();
                    let type_annotation = self.arena.alloc(self.parse_union_type()?);
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

            self.position = checkpoint;
        }

        self.parse_union_type()
    }
}

impl Parser<'_> {
    #[inline]
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
            let types = self.arena.alloc_slice_fill_iter(types.into_iter());
            Ok(Type {
                kind: TypeKind::Union(types),
                span: start_span.combine(&end_span),
            })
        }
    }

    #[inline]
    fn parse_intersection_type(&mut self) -> Result<Type, ParserError> {
        let mut types = vec![self.parse_conditional_type()?];

        while self.match_token(&[TokenKind::Ampersand]) {
            types.push(self.parse_conditional_type()?);
        }

        if types.len() == 1 {
            Ok(types.into_iter().next().unwrap())
        } else {
            let start_span = types.first().unwrap().span;
            let end_span = types.last().unwrap().span;
            let types = self.arena.alloc_slice_fill_iter(types.into_iter());
            Ok(Type {
                kind: TypeKind::Intersection(types),
                span: start_span.combine(&end_span),
            })
        }
    }

    #[inline]
    fn parse_conditional_type(&mut self) -> Result<Type, ParserError> {
        let check_type = self.parse_postfix_type()?;

        // Check for conditional type: T extends U ? X : Y
        if self.match_token(&[TokenKind::Extends]) {
            // Parse the extends type - use primary type only to avoid consuming '?'
            // which is the separator in conditional types, not a nullable suffix
            let extends_type = self.parse_primary_type()?;
            self.consume(TokenKind::Question, "Expected '?' in conditional type")?;
            let true_type = self.parse_type()?;
            self.consume(TokenKind::Colon, "Expected ':' in conditional type")?;
            let false_type = self.parse_type()?;

            let start_span = check_type.span;
            let end_span = false_type.span;

            return Ok(Type {
                kind: TypeKind::Conditional(ConditionalType {
                    check_type: self.arena.alloc(check_type),
                    extends_type: self.arena.alloc(extends_type),
                    true_type: self.arena.alloc(true_type),
                    false_type: self.arena.alloc(false_type),
                    span: start_span.combine(&end_span),
                }),
                span: start_span.combine(&end_span),
            });
        }

        Ok(check_type)
    }

    #[inline]
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
                            kind: TypeKind::Array(self.arena.alloc(ty)),
                            span: start_span.combine(&end_span),
                        };
                    } else {
                        // Index access type: T[K]
                        let index = self.parse_type()?;
                        self.consume(TokenKind::RightBracket, "Expected ']'")?;
                        let end_span = self.current_span();
                        let start_span = ty.span;
                        ty = Type {
                            kind: TypeKind::IndexAccess(self.arena.alloc(ty), self.arena.alloc(index)),
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
                        kind: TypeKind::Nullable(self.arena.alloc(ty)),
                        span: start_span.combine(&end_span),
                    };
                }
                _ => break,
            }
        }

        Ok(ty)
    }

    #[inline]
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
                    s if s == "thread" => Some(PrimitiveType::Thread),
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
                    self.consume_closing_angle_bracket()?;
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

            // Keyof type: keyof T
            TokenKind::Keyof => {
                self.advance();
                let operand = self.parse_primary_type()?;
                let end_span = operand.span;
                Ok(Type {
                    kind: TypeKind::KeyOf(self.arena.alloc(operand)),
                    span: start_span.combine(&end_span),
                })
            }

            // Typeof type: typeof(expression)
            TokenKind::Typeof => {
                self.advance();
                self.consume(TokenKind::LeftParen, "Expected '(' after 'typeof'")?;

                // Parse the expression inside typeof()
                let expr = self.parse_expression()?;

                self.consume(
                    TokenKind::RightParen,
                    "Expected ')' after typeof expression",
                )?;

                let end_span = self.current_span();
                Ok(Type {
                    kind: TypeKind::TypeQuery(self.arena.alloc(expr)),
                    span: start_span.combine(&end_span),
                })
            }
            // Template literal type: `hello ${T}`
            TokenKind::TemplateString(parts) => {
                self.advance();
                let parts_clone = parts.clone();
                let template_parts = self.parse_template_literal_type_parts(parts_clone)?;
                let end_span = self.current_span();
                Ok(Type {
                    kind: TypeKind::TemplateLiteral(TemplateLiteralType {
                        parts: template_parts,
                        span: start_span.combine(&end_span),
                    }),
                    span: start_span.combine(&end_span),
                })
            }

            // Object type: { ... }
            TokenKind::LeftBrace => self.parse_object_type(),

            // Tuple type: [T, U, V]
            TokenKind::LeftBracket => self.parse_tuple_type(),

            // Function type: (x: T) -> U or Tuple type: [T, U]
            // Note: Tuple types use square brackets, but we might encounter
            // parenthesized types or function types starting with '('
            TokenKind::LeftParen => {
                // Try to parse as function type first
                match self.try_parse_function_type() {
                    Ok(func_type) => Ok(func_type),
                    Err(_) => {
                        // If it fails, it might be a tuple type with parens
                        // or a parenthesized type. Let's parse it as a tuple.
                        self.parse_tuple_type_with_parens()
                    }
                }
            }

            // Variadic type: ...T or ...T[] (used for variadic returns)
            TokenKind::DotDotDot => {
                self.advance(); // consume '...'
                                // Parse the inner type, which could be a postfix type (e.g., string[])
                let inner_type = self.parse_postfix_type()?;
                let end_span = inner_type.span;
                Ok(Type {
                    kind: TypeKind::Variadic(self.arena.alloc(inner_type)),
                    span: start_span.combine(&end_span),
                })
            }

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

        // Check for mapped type: { [K in T]: V } or { readonly [K in T]?: V }
        // Look ahead to see if this is a mapped type
        let checkpoint = self.position;

        // Skip readonly modifier if present
        if self.check(&TokenKind::Readonly) {
            self.advance();
        }

        if self.check(&TokenKind::LeftBracket) {
            // Save position and try to parse as mapped type
            self.advance(); // consume '['

            // Check if next token is an identifier followed by 'in'
            if let TokenKind::Identifier(_) = self.current().kind {
                self.advance(); // consume identifier
                if self.check(&TokenKind::In) {
                    // This looks like a mapped type, rewind and parse it
                    self.position = checkpoint;
                    return self.parse_mapped_type(start_span);
                }
            }
        }
        // Not a mapped type, rewind and continue with object type parsing
        self.position = checkpoint;

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

        let members = self.arena.alloc_slice_fill_iter(members.into_iter());
        Ok(Type {
            kind: TypeKind::Object(ObjectType {
                members,
                span: start_span.combine(&end_span),
            }),
            span: start_span.combine(&end_span),
        })
    }

    fn parse_mapped_type(&mut self, start_span: Span) -> Result<Type, ParserError> {
        use crate::ast::types::{MappedType, MappedTypeModifier};

        // Check for readonly modifier before the bracket: readonly [K in T]
        // or with +/- prefix: +readonly [K in T] or -readonly [K in T]
        let readonly_modifier = if self.match_token(&[TokenKind::Plus]) {
            self.consume(TokenKind::Readonly, "Expected 'readonly' after '+'")?;
            MappedTypeModifier::Add
        } else if self.match_token(&[TokenKind::Minus]) {
            self.consume(TokenKind::Readonly, "Expected 'readonly' after '-'")?;
            MappedTypeModifier::Remove
        } else if self.match_token(&[TokenKind::Readonly]) {
            MappedTypeModifier::Add
        } else {
            MappedTypeModifier::None
        };

        self.consume(TokenKind::LeftBracket, "Expected '[' in mapped type")?;

        // Parse type parameter name (e.g., K in "[K in T]")
        let param_name = self.parse_identifier()?;
        let param_span = param_name.span;
        let type_parameter = TypeParameter {
            name: param_name,
            constraint: None,
            default: None,
            span: param_span,
        };

        self.consume(TokenKind::In, "Expected 'in' in mapped type")?;

        // Parse the 'in' type (e.g., keyof T)
        let in_type = self.parse_type()?;

        self.consume(
            TokenKind::RightBracket,
            "Expected ']' after mapped type parameter",
        )?;

        // Check for optional modifier: ?, +?, or -?
        let optional_modifier = if self.match_token(&[TokenKind::Question]) {
            MappedTypeModifier::Add
        } else if self.check(&TokenKind::Plus) || self.check(&TokenKind::Minus) {
            let checkpoint = self.position;
            if self.match_token(&[TokenKind::Plus]) {
                if self.match_token(&[TokenKind::Question]) {
                    MappedTypeModifier::Add
                } else {
                    self.position = checkpoint;
                    MappedTypeModifier::None
                }
            } else if self.match_token(&[TokenKind::Minus]) {
                if self.match_token(&[TokenKind::Question]) {
                    MappedTypeModifier::Remove
                } else {
                    self.position = checkpoint;
                    MappedTypeModifier::None
                }
            } else {
                MappedTypeModifier::None
            }
        } else {
            MappedTypeModifier::None
        };

        self.consume(TokenKind::Colon, "Expected ':' in mapped type")?;

        // Parse the value type
        let value_type = self.parse_type()?;

        // Optional semicolon
        self.match_token(&[TokenKind::Semicolon]);

        let end_span = self.current_span();
        self.consume(TokenKind::RightBrace, "Expected '}' after mapped type")?;

        Ok(Type {
            kind: TypeKind::Mapped(MappedType {
                readonly_modifier,
                type_parameter: self.arena.alloc(type_parameter),
                in_type: self.arena.alloc(in_type),
                optional_modifier,
                value_type: self.arena.alloc(value_type),
                span: start_span.combine(&end_span),
            }),
            span: start_span.combine(&end_span),
        })
    }

    fn parse_template_literal_type_parts(
        &mut self,
        lexer_parts: Vec<crate::lexer::TemplatePart>,
    ) -> Result<&'arena [TemplateLiteralTypePart<'arena>], ParserError> {
        use crate::ast::types::TemplateLiteralTypePart;

        let mut parts = Vec::new();

        for lexer_part in lexer_parts {
            match lexer_part {
                crate::lexer::TemplatePart::String(s) => {
                    parts.push(TemplateLiteralTypePart::String(s));
                }
                crate::lexer::TemplatePart::Expression(tokens) => {
                    // Parse the expression as a type
                    let handler = self.diagnostic_handler.clone();
                    let mut temp_parser = Parser::new(tokens, handler, self.interner, self.common);
                    let typ = temp_parser.parse_type()?;
                    parts.push(TemplateLiteralTypePart::Type(typ));
                }
            }
        }

        let parts = self.arena.alloc_slice_fill_iter(parts.into_iter());
        Ok(parts)
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

        let types = self.arena.alloc_slice_fill_iter(types.into_iter());
        Ok(Type {
            kind: TypeKind::Tuple(types),
            span: start_span.combine(&end_span),
        })
    }

    /// Try to parse a function type, but return an error if it's not a function type
    fn try_parse_function_type(&mut self) -> Result<Type, ParserError> {
        let checkpoint = self.position;
        let start_span = self.current_span();

        // Try to parse as function type, but rewind on any error
        let result = (|| -> Result<Type, ParserError> {
            self.consume(TokenKind::LeftParen, "Expected '('")?;
            let parameters = self.parse_parameter_list()?;
            self.consume(TokenKind::RightParen, "Expected ')'")?;

            // Check if this is actually a function type (should have -> or =>)
            if !self.check(&TokenKind::Arrow) && !self.check(&TokenKind::FatArrow) {
                return Err(ParserError {
                    message: "Not a function type".to_string(),
                    span: start_span,
                });
            }

            // Accept both -> and => for function type arrow
            if !self.match_token(&[TokenKind::Arrow]) {
                self.consume(TokenKind::FatArrow, "Expected '->' or '=>'")?;
            }
            let return_type = self.arena.alloc(self.parse_type()?);
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
        })();

        // If parsing failed, rewind to checkpoint
        match result {
            Ok(ty) => Ok(ty),
            Err(e) => {
                self.position = checkpoint;
                Err(e)
            }
        }
    }

    /// Parse a tuple type using parentheses: (T, U, V)
    fn parse_tuple_type_with_parens(&mut self) -> Result<Type, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::LeftParen, "Expected '('")?;

        let mut types = Vec::new();

        // Parse first type
        types.push(self.parse_type()?);

        // Parse additional types separated by commas
        while self.match_token(&[TokenKind::Comma]) {
            types.push(self.parse_type()?);
        }

        self.consume(TokenKind::RightParen, "Expected ')' after tuple type")?;
        let end_span = self.current_span();

        // If there's only one type and no comma, it's a parenthesized type, not a tuple
        if types.len() == 1 {
            Ok(Type {
                kind: TypeKind::Parenthesized(self.arena.alloc(types.into_iter().next().unwrap())),
                span: start_span.combine(&end_span),
            })
        } else {
            let types = self.arena.alloc_slice_fill_iter(types.into_iter());
            Ok(Type {
                kind: TypeKind::Tuple(types),
                span: start_span.combine(&end_span),
            })
        }
    }

    pub(crate) fn parse_type_arguments(&mut self) -> Result<&'arena [Type<'arena>], ParserError> {
        let mut args = Vec::new();

        loop {
            args.push(self.parse_type()?);

            if !self.match_token(&[TokenKind::Comma]) {
                break;
            }
        }

        let args = self.arena.alloc_slice_fill_iter(args.into_iter());
        Ok(args)
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

    fn parse_type(source: &str) -> Result<Type, ParserError> {
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common) = StringInterner::new_with_common_identifiers();
        let mut lexer = Lexer::new(source, handler.clone(), &interner);
        let tokens = lexer.tokenize().map_err(|e| ParserError {
            message: format!("Lexer error: {:?}", e),
            span: Span::default(),
        })?;
        let mut parser = Parser::new(tokens, handler, &interner, &common);
        parser.parse_type()
    }

    #[test]
    fn test_parse_primitive_nil() {
        let result = parse_type("nil");
        assert!(result.is_ok());
        // nil is parsed as a literal type, not a primitive
        match result.unwrap().kind {
            TypeKind::Literal(Literal::Nil) => {}
            _ => panic!("Expected nil literal type"),
        }
    }

    #[test]
    fn test_parse_primitive_boolean() {
        let result = parse_type("boolean");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Primitive(PrimitiveType::Boolean) => {}
            _ => panic!("Expected boolean primitive"),
        }
    }

    #[test]
    fn test_parse_primitive_number() {
        let result = parse_type("number");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Primitive(PrimitiveType::Number) => {}
            _ => panic!("Expected number primitive"),
        }
    }

    #[test]
    fn test_parse_primitive_integer() {
        let result = parse_type("integer");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Primitive(PrimitiveType::Integer) => {}
            _ => panic!("Expected integer primitive"),
        }
    }

    #[test]
    fn test_parse_primitive_string() {
        let result = parse_type("string");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Primitive(PrimitiveType::String) => {}
            _ => panic!("Expected string primitive"),
        }
    }

    #[test]
    fn test_parse_primitive_unknown() {
        let result = parse_type("unknown");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Primitive(PrimitiveType::Unknown) => {}
            _ => panic!("Expected unknown primitive"),
        }
    }

    #[test]
    fn test_parse_primitive_never() {
        let result = parse_type("never");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Primitive(PrimitiveType::Never) => {}
            _ => panic!("Expected never primitive"),
        }
    }

    #[test]
    fn test_parse_primitive_void() {
        let result = parse_type("void");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Primitive(PrimitiveType::Void) => {}
            _ => panic!("Expected void primitive"),
        }
    }

    #[test]
    fn test_parse_primitive_table() {
        let result = parse_type("table");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Primitive(PrimitiveType::Table) => {}
            _ => panic!("Expected table primitive"),
        }
    }

    #[test]
    fn test_parse_primitive_coroutine() {
        let result = parse_type("coroutine");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Primitive(PrimitiveType::Coroutine) => {}
            _ => panic!("Expected coroutine primitive"),
        }
    }

    #[test]
    fn test_parse_primitive_thread() {
        let result = parse_type("thread");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Primitive(PrimitiveType::Thread) => {}
            _ => panic!("Expected thread primitive"),
        }
    }

    #[test]
    fn test_parse_type_reference() {
        let result = parse_type("MyType");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Reference(_) => {}
            _ => panic!("Expected type reference"),
        }
    }

    #[test]
    fn test_parse_type_reference_with_args() {
        let result = parse_type("Array<number>");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Reference(ref_type) => {
                assert!(ref_type.type_arguments.is_some());
                assert_eq!(ref_type.type_arguments.unwrap().len(), 1);
            }
            _ => panic!("Expected type reference with args"),
        }
    }

    #[test]
    fn test_parse_type_reference_with_multiple_args() {
        let result = parse_type("Map<string, number>");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Reference(ref_type) => {
                assert!(ref_type.type_arguments.is_some());
                assert_eq!(ref_type.type_arguments.unwrap().len(), 2);
            }
            _ => panic!("Expected type reference with multiple args"),
        }
    }

    #[test]
    fn test_parse_union_type() {
        let result = parse_type("string | number");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Union(types) => {
                assert_eq!(types.len(), 2);
            }
            _ => panic!("Expected union type"),
        }
    }

    #[test]
    fn test_parse_union_type_multiple() {
        let result = parse_type("A | B | C");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Union(types) => {
                assert_eq!(types.len(), 3);
            }
            _ => panic!("Expected union type with multiple"),
        }
    }

    #[test]
    fn test_parse_intersection_type() {
        let result = parse_type("A & B");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Intersection(types) => {
                assert_eq!(types.len(), 2);
            }
            _ => panic!("Expected intersection type"),
        }
    }

    #[test]
    fn test_parse_intersection_type_multiple() {
        let result = parse_type("A & B & C");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Intersection(types) => {
                assert_eq!(types.len(), 3);
            }
            _ => panic!("Expected intersection type with multiple"),
        }
    }

    #[test]
    fn test_parse_array_type() {
        let result = parse_type("number[]");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Array(_) => {}
            _ => panic!("Expected array type"),
        }
    }

    #[test]
    fn test_parse_array_type_nested() {
        let result = parse_type("number[][]");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Array(inner) => match inner.kind {
                TypeKind::Array(_) => {}
                _ => panic!("Expected nested array type"),
            },
            _ => panic!("Expected array type"),
        }
    }

    #[test]
    fn test_parse_nullable_type() {
        let result = parse_type("number?");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Nullable(_) => {}
            _ => panic!("Expected nullable type"),
        }
    }

    #[test]
    fn test_parse_index_access_type() {
        let result = parse_type("T[K]");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::IndexAccess(_, _) => {}
            _ => panic!("Expected index access type"),
        }
    }

    #[test]
    fn test_parse_function_type() {
        let result = parse_type("() -> void");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Function(_) => {}
            _ => panic!("Expected function type"),
        }
    }

    #[test]
    fn test_parse_function_type_with_params() {
        let result = parse_type("(x: number, y: string) -> boolean");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Function(func_type) => {
                assert_eq!(func_type.parameters.len(), 2);
            }
            _ => panic!("Expected function type with params"),
        }
    }

    #[test]
    fn test_parse_tuple_type() {
        let result = parse_type("[number, string, boolean]");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Tuple(types) => {
                assert_eq!(types.len(), 3);
            }
            _ => panic!("Expected tuple type"),
        }
    }

    #[test]
    fn test_parse_empty_tuple_type() {
        let result = parse_type("[]");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Tuple(types) => {
                assert!(types.is_empty());
            }
            _ => panic!("Expected empty tuple type"),
        }
    }

    #[test]
    fn test_parse_tuple_type_with_parens() {
        let result = parse_type("(number, string)");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Tuple(types) => {
                assert_eq!(types.len(), 2);
            }
            _ => panic!("Expected tuple type with parens"),
        }
    }

    #[test]
    fn test_parse_object_type() {
        let result = parse_type("{ x: number, y: string }");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Object(obj_type) => {
                assert_eq!(obj_type.members.len(), 2);
            }
            _ => panic!("Expected object type"),
        }
    }

    #[test]
    fn test_parse_object_type_empty() {
        let result = parse_type("{}");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Object(obj_type) => {
                assert!(obj_type.members.is_empty());
            }
            _ => panic!("Expected empty object type"),
        }
    }

    #[test]
    fn test_parse_object_type_with_method() {
        let result = parse_type("{ method(): number }");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Object(obj_type) => {
                assert_eq!(obj_type.members.len(), 1);
            }
            _ => panic!("Expected object type with method"),
        }
    }

    #[test]
    fn test_parse_object_type_with_optional() {
        let result = parse_type("{ x?: number }");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Object(obj_type) => {
                assert_eq!(obj_type.members.len(), 1);
            }
            _ => panic!("Expected object type with optional"),
        }
    }

    #[test]
    fn test_parse_object_type_with_readonly() {
        let result = parse_type("{ readonly x: number }");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Object(obj_type) => {
                assert_eq!(obj_type.members.len(), 1);
            }
            _ => panic!("Expected object type with readonly"),
        }
    }

    #[test]
    fn test_parse_object_type_with_index_signature() {
        let result = parse_type("{ [key: string]: number }");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Object(obj_type) => {
                assert_eq!(obj_type.members.len(), 1);
            }
            _ => panic!("Expected object type with index signature"),
        }
    }

    #[test]
    fn test_parse_literal_number_type() {
        let result = parse_type("42");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Literal(Literal::Number(n)) => {
                assert_eq!(n, 42.0);
            }
            _ => panic!("Expected literal number type"),
        }
    }

    #[test]
    fn test_parse_literal_string_type() {
        let result = parse_type("\"hello\"");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Literal(Literal::String(s)) => {
                assert_eq!(s, "hello");
            }
            _ => panic!("Expected literal string type"),
        }
    }

    #[test]
    fn test_parse_literal_true_type() {
        let result = parse_type("true");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Literal(Literal::Boolean(true)) => {}
            _ => panic!("Expected literal true type"),
        }
    }

    #[test]
    fn test_parse_literal_false_type() {
        let result = parse_type("false");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Literal(Literal::Boolean(false)) => {}
            _ => panic!("Expected literal false type"),
        }
    }

    #[test]
    fn test_parse_literal_nil_type() {
        let result = parse_type("nil");
        assert!(result.is_ok());
        // nil token is parsed as a literal nil type
        let ty = result.unwrap();
        match ty.kind {
            TypeKind::Literal(Literal::Nil) => {}
            TypeKind::Primitive(PrimitiveType::Nil) => {}
            _ => panic!("Expected nil type, got {:?}", ty.kind),
        }
    }

    #[test]
    fn test_parse_variadic_type() {
        let result = parse_type("...string");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Variadic(_) => {}
            _ => panic!("Expected variadic type"),
        }
    }

    #[test]
    fn test_parse_variadic_array_type() {
        let result = parse_type("...string[]");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Variadic(inner) => match inner.kind {
                TypeKind::Array(_) => {}
                _ => panic!("Expected variadic array type"),
            },
            _ => panic!("Expected variadic type"),
        }
    }

    #[test]
    fn test_parse_type_predicate() {
        let result = parse_type("x is string");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::TypePredicate(_) => {}
            _ => panic!("Expected type predicate"),
        }
    }

    #[test]
    fn test_parse_complex_type() {
        let result = parse_type("Array<string | number> | null");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Union(types) => {
                assert_eq!(types.len(), 2);
            }
            _ => panic!("Expected complex union type"),
        }
    }

    #[test]
    fn test_parse_intersection_with_union() {
        let result = parse_type("A & (B | C)");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Intersection(types) => {
                assert_eq!(types.len(), 2);
            }
            _ => panic!("Expected intersection with union"),
        }
    }

    #[test]
    fn test_parse_function_type_returning_union() {
        let result = parse_type("() -> string | number");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Function(func_type) => match func_type.return_type.kind {
                TypeKind::Union(_) => {}
                _ => panic!("Expected union return type"),
            },
            _ => panic!("Expected function type"),
        }
    }

    #[test]
    fn test_parse_function_type_with_void_return() {
        let result = parse_type("() -> void");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Function(func_type) => match func_type.return_type.kind {
                TypeKind::Primitive(PrimitiveType::Void) => {}
                _ => panic!("Expected void return type"),
            },
            _ => panic!("Expected function type"),
        }
    }

    #[test]
    fn test_parse_nested_function_type() {
        let result = parse_type("(x: (y: number) -> string) -> void");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Function(func_type) => {
                assert_eq!(func_type.parameters.len(), 1);
            }
            _ => panic!("Expected nested function type"),
        }
    }

    #[test]
    fn test_parse_object_type_with_multiple_methods() {
        let result = parse_type("{ foo(): void, bar(x: number): string }");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Object(obj_type) => {
                assert_eq!(obj_type.members.len(), 2);
            }
            _ => panic!("Expected object type with multiple methods"),
        }
    }

    #[test]
    fn test_parse_object_type_mixed_members() {
        let result = parse_type("{ x: number, method(): void, readonly y: string }");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Object(obj_type) => {
                assert_eq!(obj_type.members.len(), 3);
            }
            _ => panic!("Expected object type with mixed members"),
        }
    }

    #[test]
    fn test_parse_complex_nested_type() {
        let result = parse_type("Array<{ x: number, y: string }>");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Reference(ref_type) => {
                assert!(ref_type.type_arguments.is_some());
                let args = ref_type.type_arguments.unwrap();
                assert_eq!(args.len(), 1);
                match &args[0].kind {
                    TypeKind::Object(_) => {}
                    _ => panic!("Expected object type argument"),
                }
            }
            _ => panic!("Expected reference type"),
        }
    }

    #[test]
    fn test_parse_union_of_function_types() {
        let result = parse_type("(() -> void) | (() -> string)");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Union(types) => {
                assert_eq!(types.len(), 2);
            }
            _ => panic!("Expected union of function types"),
        }
    }

    #[test]
    fn test_parse_intersection_of_object_types() {
        let result = parse_type("{ x: number } & { y: string }");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Intersection(types) => {
                assert_eq!(types.len(), 2);
            }
            _ => panic!("Expected intersection of object types"),
        }
    }

    #[test]
    fn test_parse_conditional_type() {
        let result = parse_type("T extends U ? X : Y");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Conditional(conditional) => {
                // Check that check_type is a reference (T)
                match &conditional.check_type.kind {
                    TypeKind::Reference(_) => {}
                    _ => panic!("Expected check_type to be a reference"),
                }
                // Check that extends_type is a reference (U)
                match &conditional.extends_type.kind {
                    TypeKind::Reference(_) => {}
                    _ => panic!("Expected extends_type to be a reference"),
                }
                // Check that true_type is a reference (X)
                match &conditional.true_type.kind {
                    TypeKind::Reference(_) => {}
                    _ => panic!("Expected true_type to be a reference"),
                }
                // Check that false_type is a reference (Y)
                match &conditional.false_type.kind {
                    TypeKind::Reference(_) => {}
                    _ => panic!("Expected false_type to be a reference"),
                }
            }
            _ => panic!("Expected conditional type"),
        }
    }

    #[test]
    fn test_parse_conditional_type_with_primitive() {
        let result = parse_type("T extends string ? number : boolean");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Conditional(conditional) => {
                // Check that extends_type is string
                match &conditional.extends_type.kind {
                    TypeKind::Primitive(PrimitiveType::String) => {}
                    _ => panic!("Expected extends_type to be string"),
                }
                // Check that true_type is number
                match &conditional.true_type.kind {
                    TypeKind::Primitive(PrimitiveType::Number) => {}
                    _ => panic!("Expected true_type to be number"),
                }
                // Check that false_type is boolean
                match &conditional.false_type.kind {
                    TypeKind::Primitive(PrimitiveType::Boolean) => {}
                    _ => panic!("Expected false_type to be boolean"),
                }
            }
            _ => panic!("Expected conditional type"),
        }
    }

    #[test]
    fn test_parse_nested_conditional_type() {
        let result = parse_type("T extends string ? (T extends number ? boolean : never) : void");
        if let Err(ref e) = result {
            eprintln!("Parse error: {}", e.message);
        }
        assert!(result.is_ok());
        let typ = result.unwrap();
        eprintln!("Parsed type kind: {:?}", typ.kind);
        match typ.kind {
            TypeKind::Conditional(conditional) => {
                // Check that true_type is a nested conditional
                eprintln!("True type kind: {:?}", conditional.true_type.kind);
                match &conditional.true_type.kind {
                    TypeKind::Conditional(_) => {}
                    TypeKind::Parenthesized(inner) => {
                        // The nested conditional might be wrapped in Parenthesized
                        match &inner.kind {
                            TypeKind::Conditional(_) => {}
                            _ => panic!(
                                "Expected nested conditional type in true branch, got: {:?}",
                                inner.kind
                            ),
                        }
                    }
                    _ => panic!(
                        "Expected nested conditional type in true branch, got: {:?}",
                        conditional.true_type.kind
                    ),
                }
            }
            _ => panic!("Expected conditional type, got: {:?}", typ.kind),
        }
    }

    #[test]
    fn test_parse_mapped_type_basic() {
        let result = parse_type("{ [K in T]: V }");
        if let Err(ref e) = result {
            eprintln!("Parse error: {}", e.message);
        }
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Mapped(mapped) => {
                assert_eq!(mapped.readonly_modifier, MappedTypeModifier::None);
                assert_eq!(mapped.optional_modifier, MappedTypeModifier::None);
            }
            _ => panic!("Expected mapped type"),
        }
    }

    #[test]
    fn test_parse_mapped_type_with_keyof() {
        let result = parse_type("{ [K in keyof T]: T[K] }");
        if let Err(ref e) = result {
            eprintln!("Parse error: {}", e.message);
        }
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Mapped(mapped) => {
                // Check that in_type is a KeyOf expression
                match &mapped.in_type.kind {
                    TypeKind::KeyOf(_) => {}
                    _ => panic!("Expected keyof expression in mapped type"),
                }
            }
            _ => panic!("Expected mapped type"),
        }
    }

    #[test]
    fn test_parse_mapped_type_with_readonly() {
        let result = parse_type("{ readonly [K in T]: V }");
        if let Err(ref e) = result {
            eprintln!("Parse error: {}", e.message);
        }
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Mapped(mapped) => {
                assert_eq!(mapped.readonly_modifier, MappedTypeModifier::Add);
            }
            _ => panic!("Expected mapped type with readonly"),
        }
    }

    #[test]
    fn test_parse_mapped_type_with_optional() {
        let result = parse_type("{ [K in T]?: V }");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Mapped(mapped) => {
                assert_eq!(mapped.optional_modifier, MappedTypeModifier::Add);
            }
            _ => panic!("Expected mapped type with optional"),
        }
    }

    #[test]
    fn test_parse_mapped_type_with_modifiers() {
        let result = parse_type("{ readonly [K in T]?: V }");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::Mapped(mapped) => {
                assert_eq!(mapped.readonly_modifier, MappedTypeModifier::Add);
                assert_eq!(mapped.optional_modifier, MappedTypeModifier::Add);
            }
            _ => panic!("Expected mapped type with modifiers"),
        }
    }

    #[test]
    fn test_parse_template_literal_type_basic() {
        let result = parse_type("`hello`");
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::TemplateLiteral(template) => {
                assert_eq!(template.parts.len(), 1);
                match &template.parts[0] {
                    TemplateLiteralTypePart::String(s) => assert_eq!(s, "hello"),
                    _ => panic!("Expected string part"),
                }
            }
            _ => panic!("Expected template literal type"),
        }
    }

    #[test]
    fn test_parse_template_literal_type_with_type() {
        let result = parse_type("`prefix_${T}`");
        if let Err(ref e) = result {
            eprintln!("Parse error: {}", e.message);
        }
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::TemplateLiteral(template) => {
                assert_eq!(template.parts.len(), 2);
                match &template.parts[0] {
                    TemplateLiteralTypePart::String(s) => assert_eq!(s, "prefix_"),
                    _ => panic!("Expected string part"),
                }
                match &template.parts[1] {
                    TemplateLiteralTypePart::Type(_) => {}
                    _ => panic!("Expected type part"),
                }
            }
            _ => panic!("Expected template literal type"),
        }
    }

    #[test]
    fn test_parse_template_literal_type_complex() {
        let result = parse_type("`${T}_${U}`");
        if let Err(ref e) = result {
            eprintln!("Parse error: {}", e.message);
        }
        assert!(result.is_ok());
        match result.unwrap().kind {
            TypeKind::TemplateLiteral(template) => {
                assert_eq!(template.parts.len(), 3);
                // T, _, U
            }
            _ => panic!("Expected template literal type"),
        }
    }
}
