use super::{ExpressionParser, Parser, ParserError, PatternParser, TypeParser};
use crate::ast::pattern::Pattern;
use crate::ast::statement::*;
use crate::ast::types::{PrimitiveType, Type, TypeKind};
use crate::ast::Ident;
use crate::ast::Spanned;
use crate::lexer::TokenKind;
use crate::span::Span;

pub trait StatementParser<'arena> {
    fn parse_statement(&mut self) -> Result<Statement<'arena>, ParserError>;
    fn parse_block(&mut self) -> Result<Block<'arena>, ParserError>;
}

impl<'a, 'arena> StatementParser<'arena> for Parser<'a, 'arena> {
    #[inline]
    fn parse_statement(&mut self) -> Result<Statement<'arena>, ParserError> {
        // Check for decorators first
        if self.check(&TokenKind::At) {
            return self.parse_class_declaration();
        }

        match &self.current().kind {
            TokenKind::Const | TokenKind::Local => self.parse_variable_declaration(),
            TokenKind::Function => self.parse_function_declaration(),
            TokenKind::If => self.parse_if_statement(),
            TokenKind::While => self.parse_while_statement(),
            TokenKind::For => self.parse_for_statement(),
            TokenKind::Repeat => self.parse_repeat_statement(),
            TokenKind::Return => self.parse_return_statement(),
            TokenKind::Break => {
                let span = self.current_span();
                self.advance();
                Ok(Statement::Break(span))
            }
            TokenKind::Continue => {
                let span = self.current_span();
                self.advance();
                Ok(Statement::Continue(span))
            }
            TokenKind::Interface => self.parse_interface_declaration(),
            TokenKind::Type => self.parse_type_alias_declaration(),
            TokenKind::Enum => self.parse_enum_declaration(),
            TokenKind::Import => self.parse_import_declaration(),
            TokenKind::Export => self.parse_export_declaration(),
            TokenKind::Abstract | TokenKind::Final | TokenKind::Class => {
                self.parse_class_declaration()
            }
            TokenKind::Declare => self.parse_declare_statement(),
            TokenKind::Throw => self.parse_throw_statement(),
            TokenKind::Try => self.parse_try_statement(),
            TokenKind::Rethrow => self.parse_rethrow_statement(),
            TokenKind::Namespace => self.parse_namespace_declaration(),
            TokenKind::Goto => self.parse_goto_statement(),
            TokenKind::ColonColon => self.parse_label_statement(),
            _ => {
                // Expression statement
                let expr = self.parse_expression()?;
                Ok(Statement::Expression(expr))
            }
        }
    }

    #[inline]
    fn parse_block(&mut self) -> Result<Block<'arena>, ParserError> {
        let start_span = self.current_span();
        let mut statements = Vec::with_capacity(4);

        while !self.is_at_end()
            && !matches!(
                &self.current().kind,
                TokenKind::End
                    | TokenKind::Else
                    | TokenKind::Elseif
                    | TokenKind::Until
                    | TokenKind::RightBrace
            )
        {
            statements.push(self.parse_statement()?);
        }

        let end_span = if !statements.is_empty() {
            statements.last().unwrap().span()
        } else {
            start_span
        };

        let statements = self.alloc_vec(statements);

        Ok(Block {
            statements,
            span: start_span.combine(&end_span),
        })
    }
}

// Helper trait for getting span from Statement
trait Spannable {
    fn span(&self) -> Span;
}

impl<'arena> Spannable for Statement<'arena> {
    fn span(&self) -> Span {
        match self {
            Statement::Variable(v) => v.span,
            Statement::Function(f) => f.span,
            Statement::Class(c) => c.span,
            Statement::Interface(i) => i.span,
            Statement::TypeAlias(t) => t.span,
            Statement::Enum(e) => e.span,
            Statement::Import(i) => i.span,
            Statement::Export(e) => e.span,
            Statement::If(i) => i.span,
            Statement::While(w) => w.span,
            Statement::For(f) => match f {
                ForStatement::Numeric(n) => n.span,
                ForStatement::Generic(g) => g.span,
            },
            Statement::Repeat(r) => r.span,
            Statement::Return(r) => r.span,
            Statement::Break(s) | Statement::Continue(s) => *s,
            Statement::Label(l) => l.span,
            Statement::Goto(g) => g.span,
            Statement::Expression(e) => e.span,
            Statement::Block(b) => b.span,
            Statement::DeclareFunction(f) => f.span,
            Statement::DeclareNamespace(n) => n.span,
            Statement::DeclareType(t) => t.span,
            Statement::DeclareInterface(i) => i.span,
            Statement::DeclareConst(c) => c.span,
            Statement::Throw(t) => t.span,
            Statement::Try(t) => t.span,
            Statement::Rethrow(s) => *s,
            Statement::Namespace(n) => n.span,
        }
    }
}

// Statement implementations
impl<'a, 'arena> Parser<'a, 'arena> {
    fn parse_label_statement(&mut self) -> Result<Statement<'arena>, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::ColonColon, "Expected '::'")?;
        let name = self.parse_identifier()?;
        self.consume(TokenKind::ColonColon, "Expected '::' after label name")?;
        let end_span = self.current_span();

        Ok(Statement::Label(LabelStatement {
            name,
            span: start_span.combine(&end_span),
        }))
    }

    fn parse_goto_statement(&mut self) -> Result<Statement<'arena>, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::Goto, "Expected 'goto'")?;
        let target = self.parse_identifier()?;
        let end_span = target.span;

        Ok(Statement::Goto(GotoStatement {
            target,
            span: start_span.combine(&end_span),
        }))
    }

    #[inline]
    fn parse_variable_declaration(&mut self) -> Result<Statement<'arena>, ParserError> {
        let start_span = self.current_span();
        let kind = if matches!(self.current().kind, TokenKind::Const) {
            VariableKind::Const
        } else {
            VariableKind::Local
        };
        self.advance();

        // Fast path for simple identifier patterns (most common case)
        let pattern = if matches!(self.current().kind, TokenKind::Identifier(_)) {
            // Optimized path for simple identifiers
            let ident_span = self.current_span();
            let id = match &self.current().kind {
                TokenKind::Identifier(id) => *id,
                _ => unreachable!(),
            };
            self.advance();
            Pattern::Identifier(Spanned::new(id, ident_span))
        } else {
            // Fall back to full pattern parsing for complex cases
            self.parse_pattern()?
        };

        let type_annotation = if self.match_token(&[TokenKind::Colon]) {
            Some(self.parse_type()?)
        } else {
            None
        };

        self.consume(TokenKind::Equal, "Expected '=' in variable declaration")?;

        let initializer = self.parse_expression()?;
        let end_span = initializer.span;

        Ok(Statement::Variable(VariableDeclaration {
            kind,
            pattern,
            type_annotation,
            initializer,
            span: start_span.combine(&end_span),
        }))
    }

    #[inline]
    fn parse_function_declaration(&mut self) -> Result<Statement<'arena>, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::Function, "Expected 'function'")?;

        let name = self.parse_identifier()?;

        let type_parameters = if self.match_token(&[TokenKind::LessThan]) {
            Some(self.parse_type_parameters()?)
        } else {
            None
        };

        self.consume(TokenKind::LeftParen, "Expected '(' after function name")?;
        let parameters = self.parse_parameter_list()?;
        self.consume(TokenKind::RightParen, "Expected ')' after parameters")?;

        let return_type = if self.match_token(&[TokenKind::Colon]) {
            Some(self.parse_type()?)
        } else {
            None
        };

        let throws = if self.match_token(&[TokenKind::Throws]) {
            let mut error_types = Vec::with_capacity(2);

            if self.check(&TokenKind::LeftParen) {
                self.consume(TokenKind::LeftParen, "Expected '(' after 'throws'")?;
                if !self.check(&TokenKind::RightParen) {
                    loop {
                        error_types.push(self.parse_type()?);
                        if !self.match_token(&[TokenKind::Comma]) {
                            break;
                        }
                    }
                }
                self.consume(TokenKind::RightParen, "Expected ')' after throws types")?;
            } else {
                error_types.push(self.parse_type()?);
            }
            let error_types = self.alloc_vec(error_types);
            Some(error_types)
        } else {
            None
        };

        let use_braces = self.check(&TokenKind::LeftBrace);
        if use_braces {
            self.consume(TokenKind::LeftBrace, "Expected '{'")?;
        }
        let body = self.parse_block()?;
        if use_braces {
            self.consume(TokenKind::RightBrace, "Expected '}' after function body")?;
        } else {
            self.consume(TokenKind::End, "Expected 'end' after function body")?;
        }
        let end_span = self.current_span();

        Ok(Statement::Function(FunctionDeclaration {
            name,
            type_parameters,
            parameters,
            return_type,
            throws,
            body,
            span: start_span.combine(&end_span),
        }))
    }

    fn parse_if_statement(&mut self) -> Result<Statement<'arena>, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::If, "Expected 'if'")?;

        let condition = self.parse_expression()?;
        self.consume(TokenKind::Then, "Expected 'then' after if condition")?;

        let then_block = self.parse_block()?;

        let mut else_ifs = Vec::new();
        while self.match_token(&[TokenKind::Elseif]) {
            let elseif_start = self.current_span();
            let elseif_condition = self.parse_expression()?;
            self.consume(TokenKind::Then, "Expected 'then' after elseif condition")?;
            let elseif_block = self.parse_block()?;
            let elseif_end = elseif_block.span;

            else_ifs.push(ElseIf {
                condition: elseif_condition,
                block: elseif_block,
                span: elseif_start.combine(&elseif_end),
            });
        }

        let else_block = if self.match_token(&[TokenKind::Else]) {
            Some(self.parse_block()?)
        } else {
            None
        };

        self.consume(TokenKind::End, "Expected 'end' after if statement")?;
        let end_span = self.current_span();

        let else_ifs = self.alloc_vec(else_ifs);

        Ok(Statement::If(IfStatement {
            condition,
            then_block,
            else_ifs,
            else_block,
            span: start_span.combine(&end_span),
        }))
    }

    fn parse_while_statement(&mut self) -> Result<Statement<'arena>, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::While, "Expected 'while'")?;

        let condition = self.parse_expression()?;
        self.consume(TokenKind::Do, "Expected 'do' after while condition")?;

        let body = self.parse_block()?;
        self.consume(TokenKind::End, "Expected 'end' after while body")?;
        let end_span = self.current_span();

        Ok(Statement::While(WhileStatement {
            condition,
            body,
            span: start_span.combine(&end_span),
        }))
    }

    fn parse_repeat_statement(&mut self) -> Result<Statement<'arena>, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::Repeat, "Expected 'repeat'")?;

        let body = self.parse_block()?;
        self.consume(TokenKind::Until, "Expected 'until' after repeat body")?;

        let until = self.parse_expression()?;
        let end_span = until.span;

        Ok(Statement::Repeat(RepeatStatement {
            body,
            until,
            span: start_span.combine(&end_span),
        }))
    }

    fn parse_for_statement(&mut self) -> Result<Statement<'arena>, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::For, "Expected 'for'")?;

        let first_var = self.parse_identifier()?;

        if self.match_token(&[TokenKind::Equal]) {
            // Numeric for
            let start = self.parse_expression()?;
            self.consume(TokenKind::Comma, "Expected ',' after for start value")?;
            let end = self.parse_expression()?;

            let step = if self.match_token(&[TokenKind::Comma]) {
                Some(self.parse_expression()?)
            } else {
                None
            };

            self.consume(TokenKind::Do, "Expected 'do' after for range")?;
            let body = self.parse_block()?;
            self.consume(TokenKind::End, "Expected 'end' after for body")?;
            let end_span = self.current_span();

            Ok(Statement::For(self.alloc(ForStatement::Numeric(self.alloc(
                ForNumeric {
                    variable: first_var,
                    start,
                    end,
                    step,
                    body,
                    span: start_span.combine(&end_span),
                },
            )))))
        } else {
            // Generic for: for k, v in iterator do
            let mut variables = vec![first_var];

            while self.match_token(&[TokenKind::Comma]) {
                variables.push(self.parse_identifier()?);
            }

            self.consume(TokenKind::In, "Expected 'in' in for loop")?;

            let mut iterators = vec![self.parse_expression()?];
            while self.match_token(&[TokenKind::Comma]) {
                iterators.push(self.parse_expression()?);
            }

            self.consume(TokenKind::Do, "Expected 'do' after for iterators")?;
            let body = self.parse_block()?;
            self.consume(TokenKind::End, "Expected 'end' after for body")?;
            let end_span = self.current_span();

            let variables = self.alloc_vec(variables);
            let iterators = self.alloc_vec(iterators);

            Ok(Statement::For(self.alloc(ForStatement::Generic(
                ForGeneric {
                    variables,
                    iterators,
                    body,
                    span: start_span.combine(&end_span),
                },
            ))))
        }
    }

    fn parse_return_statement(&mut self) -> Result<Statement<'arena>, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::Return, "Expected 'return'")?;

        let mut values = Vec::new();

        if !matches!(
            &self.current().kind,
            TokenKind::End
                | TokenKind::Else
                | TokenKind::Elseif
                | TokenKind::Until
                | TokenKind::Eof
        ) {
            values.push(self.parse_expression()?);

            while self.match_token(&[TokenKind::Comma]) {
                values.push(self.parse_expression()?);
            }
        }

        let end_span = if !values.is_empty() {
            values.last().unwrap().span
        } else {
            start_span
        };

        let values = self.alloc_vec(values);

        Ok(Statement::Return(ReturnStatement {
            values,
            span: start_span.combine(&end_span),
        }))
    }

    fn parse_interface_declaration(&mut self) -> Result<Statement<'arena>, ParserError> {
        self.consume(TokenKind::Interface, "Expected 'interface'")?;
        let interface = self.parse_interface_inner()?;
        Ok(Statement::Interface(interface))
    }

    fn parse_interface_inner(&mut self) -> Result<InterfaceDeclaration<'arena>, ParserError> {
        let start_span = self.current_span();
        let name = self.parse_identifier()?;

        let type_parameters = if self.match_token(&[TokenKind::LessThan]) {
            Some(self.parse_type_parameters()?)
        } else {
            None
        };

        let mut extends = Vec::new();
        if self.match_token(&[TokenKind::Extends]) {
            extends.push(self.parse_type()?);
            while self.match_token(&[TokenKind::Comma]) {
                extends.push(self.parse_type()?);
            }
        }

        // Support both { } braces and Lua-style end syntax
        let use_braces = self.check(&TokenKind::LeftBrace);

        if use_braces {
            self.consume(TokenKind::LeftBrace, "Expected '{' after interface header")?;
        }

        let members = self.parse_interface_members(use_braces)?;

        if use_braces {
            self.consume(TokenKind::RightBrace, "Expected '}' after interface body")?;
        } else {
            self.consume(TokenKind::End, "Expected 'end' after interface body")?;
        }
        let end_span = self.current_span();

        let extends = self.alloc_vec(extends);

        Ok(InterfaceDeclaration {
            name,
            type_parameters,
            extends,
            members,
            span: start_span.combine(&end_span),
        })
    }

    fn parse_interface_members(&mut self, use_braces: bool) -> Result<&'arena [InterfaceMember<'arena>], ParserError> {
        let mut members = Vec::new();

        let end_token = if use_braces {
            TokenKind::RightBrace
        } else {
            TokenKind::End
        };

        while !self.check(&end_token) && !self.is_at_end() {
            if self.check(&TokenKind::LeftBracket) {
                members.push(InterfaceMember::Index(self.parse_index_signature()?));
            } else {
                let is_readonly = self.match_token(&[TokenKind::Readonly]);
                // Allow keywords as property/method names in interfaces
                let name = self.parse_interface_member_name()?;

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

                    let body = if self.check(&TokenKind::LeftBrace) {
                        self.consume(TokenKind::LeftBrace, "Expected '{'")?;
                        let block = self.parse_block()?;
                        self.consume(TokenKind::RightBrace, "Expected '}' after method body")?;
                        Some(block)
                    } else {
                        None
                    };

                    members.push(InterfaceMember::Method(MethodSignature {
                        name,
                        type_parameters,
                        parameters,
                        return_type,
                        body,
                        span,
                    }));
                } else {
                    // Property signature
                    let is_optional = self.match_token(&[TokenKind::Question]);
                    self.consume(TokenKind::Colon, "Expected ':' after property name")?;
                    let type_annotation = self.parse_type()?;
                    let span = name.span.combine(&type_annotation.span);

                    members.push(InterfaceMember::Property(PropertySignature {
                        is_readonly,
                        name,
                        is_optional,
                        type_annotation,
                        span,
                    }));
                }

                self.match_token(&[TokenKind::Comma, TokenKind::Semicolon]);
            }
        }

        let members = self.alloc_vec(members);
        Ok(members)
    }

    pub(super) fn parse_index_signature(&mut self) -> Result<IndexSignature<'arena>, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::LeftBracket, "Expected '['")?;

        // Check if we have [string] or [number] directly (without key name)
        // or [key: string] or [key: number] (with key name)
        // or [K]: V where K is a type parameter
        let (key_name, key_type) = if let TokenKind::Identifier(s) = &self.current().kind {
            let name = self.resolve(*s);
            let s_copy = *s; // Copy the StringId before we advance
            if name == "string" || name == "number" {
                // Direct [string] or [number] syntax
                let key_type = if name == "string" {
                    self.advance();
                    IndexKeyType::String
                } else {
                    self.advance();
                    IndexKeyType::Number
                };
                // Create a dummy identifier for key_name
                let key_name = Spanned::new(s_copy, start_span);
                (key_name, key_type)
            } else {
                // Check if next token is ':' (indicating [key: type] syntax)
                // or ']' (indicating [Type] syntax with type parameter)
                let key_name = self.parse_identifier()?;

                if self.check(&TokenKind::Colon) {
                    // [key: type] syntax
                    self.consume(TokenKind::Colon, "Expected ':' after index key name")?;

                    let key_type = match &self.current().kind {
                        TokenKind::Identifier(s) if self.resolve(*s) == "string" => {
                            self.advance();
                            IndexKeyType::String
                        }
                        TokenKind::Identifier(s) if self.resolve(*s) == "number" => {
                            self.advance();
                            IndexKeyType::Number
                        }
                        _ => {
                            return Err(ParserError {
                                message: "Index signature key must be 'string' or 'number'"
                                    .to_string(),
                                span: self.current_span(),
                            })
                        }
                    };
                    (key_name, key_type)
                } else {
                    // [Type] syntax with type parameter (e.g., [K]: V)
                    // Treat it as a string index for now
                    (key_name, IndexKeyType::String)
                }
            }
        } else {
            return Err(ParserError {
                message: "Expected identifier in index signature".to_string(),
                span: self.current_span(),
            });
        };

        self.consume(TokenKind::RightBracket, "Expected ']'")?;
        self.consume(TokenKind::Colon, "Expected ':' after index signature key")?;

        let value_type = self.parse_type()?;
        let end_span = value_type.span;

        self.match_token(&[TokenKind::Comma, TokenKind::Semicolon]);

        Ok(IndexSignature {
            key_name,
            key_type,
            value_type,
            span: start_span.combine(&end_span),
        })
    }

    fn parse_type_alias_declaration(&mut self) -> Result<Statement<'arena>, ParserError> {
        self.consume(TokenKind::Type, "Expected 'type'")?;
        let type_alias = self.parse_type_alias_inner()?;
        Ok(Statement::TypeAlias(type_alias))
    }

    fn parse_type_alias_inner(&mut self) -> Result<TypeAliasDeclaration<'arena>, ParserError> {
        let start_span = self.current_span();
        let name = self.parse_identifier()?;

        let type_parameters = if self.match_token(&[TokenKind::LessThan]) {
            Some(self.parse_type_parameters()?)
        } else {
            None
        };

        self.consume(TokenKind::Equal, "Expected '=' in type alias")?;

        let type_annotation = self.parse_type()?;
        let end_span = type_annotation.span;

        Ok(TypeAliasDeclaration {
            name,
            type_parameters,
            type_annotation,
            span: start_span.combine(&end_span),
        })
    }

    fn parse_enum_declaration(&mut self) -> Result<Statement<'arena>, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::Enum, "Expected 'enum'")?;

        let name = self.parse_identifier()?;

        // Parse optional implements clause
        let mut implements = Vec::new();
        if self.match_token(&[TokenKind::Implements]) {
            loop {
                implements.push(self.parse_type()?);
                if !self.match_token(&[TokenKind::Comma]) {
                    break;
                }
            }
        }

        self.consume(TokenKind::LeftBrace, "Expected '{' after enum name")?;

        let mut members = Vec::new();
        let mut fields = Vec::new();
        let mut constructor = None;
        let mut methods = Vec::new();
        let mut is_rich_enum = false;

        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            let token = &self.current().kind;

            if matches!(token, TokenKind::Identifier(_)) {
                let member_start = self.current_span();
                let member_name = self.parse_identifier()?;

                if self.check(&TokenKind::Colon) {
                    self.consume(TokenKind::Colon, "Expected ':' in field declaration")?;
                    let field_type = self.parse_type()?;
                    let field_end = self.current_span();
                    fields.push(EnumField {
                        name: member_name,
                        type_annotation: field_type,
                        span: member_start.combine(&field_end),
                    });
                    is_rich_enum = true;
                } else if self.check(&TokenKind::LeftParen) {
                    // Disambiguate between method definition and member invocation.
                    // Method: `name(): void { ... }` or `name(param: type): void { ... }`
                    // Member: `name("value")` or `name(42)`
                    // Check: after `(`, if `)` is followed by `:` or `{`, or if first
                    // thing inside parens is `identifier :` (typed param), it's a method.
                    let is_method = {
                        // nth_token_kind(0) is `(`, nth_token_kind(1) is what follows
                        let after_paren = self.nth_token_kind(1);
                        match after_paren {
                            // Empty params: `name()` - check what follows `)`
                            Some(TokenKind::RightParen) => {
                                // After `)`, check for `:` (return type) or `{` (body)
                                matches!(
                                    self.nth_token_kind(2),
                                    Some(TokenKind::Colon) | Some(TokenKind::LeftBrace)
                                )
                            }
                            // Typed param: `name(param: type, ...)`
                            Some(TokenKind::Identifier(_)) => {
                                matches!(self.nth_token_kind(2), Some(TokenKind::Colon))
                            }
                            _ => false,
                        }
                    };

                    if is_method {
                        // Parse as method definition
                        self.consume(TokenKind::LeftParen, "Expected '('")?;
                        let params = self.parse_typed_parameter_list()?;
                        self.consume(
                            TokenKind::RightParen,
                            "Expected ')' after method parameters",
                        )?;
                        let return_type = if self.match_token(&[TokenKind::Colon]) {
                            Some(self.parse_type()?)
                        } else {
                            None
                        };
                        self.consume(TokenKind::LeftBrace, "Expected '{' after method signature")?;
                        let body = self.parse_block()?;
                        self.consume(TokenKind::RightBrace, "Expected '}' after method body")?;
                        let method_end = self.current_span();
                        methods.push(EnumMethod {
                            name: member_name,
                            parameters: params,
                            return_type,
                            body,
                            span: member_start.combine(&method_end),
                        });
                        is_rich_enum = true;
                    } else {
                        // Parse as member with arguments
                        self.consume(TokenKind::LeftParen, "Expected '(' after enum member name")?;
                        let mut arguments = Vec::new();
                        while !self.check(&TokenKind::RightParen) && !self.is_at_end() {
                            let arg = self.parse_expression()?;
                            arguments.push(arg);
                            if !self.check(&TokenKind::RightParen) {
                                self.consume(TokenKind::Comma, "Expected ',' between arguments")?;
                            }
                        }
                        self.consume(TokenKind::RightParen, "Expected ')' after arguments")?;
                        let member_end = self.current_span();

                        let arguments = self.alloc_vec(arguments);
                        members.push(EnumMember {
                            name: member_name,
                            arguments,
                            value: None,
                            span: member_start.combine(&member_end),
                        });
                        is_rich_enum = true;
                    }
                } else {
                    let value = if self.match_token(&[TokenKind::Equal]) {
                        match &self.current().kind {
                            TokenKind::Number(s) => {
                                let val = s.parse::<f64>().map_err(|_| ParserError {
                                    message: "Invalid number in enum value".to_string(),
                                    span: self.current_span(),
                                })?;
                                self.advance();
                                Some(EnumValue::Number(val))
                            }
                            TokenKind::String(s) => {
                                let val = s.clone();
                                self.advance();
                                Some(EnumValue::String(val))
                            }
                            _ => {
                                return Err(ParserError {
                                    message: "Enum value must be a number or string".to_string(),
                                    span: self.current_span(),
                                })
                            }
                        }
                    } else {
                        None
                    };

                    let member_end = self.current_span();

                    members.push(EnumMember {
                        name: member_name,
                        arguments: &[],
                        value,
                        span: member_start.combine(&member_end),
                    });
                }
            } else if matches!(token, TokenKind::Constructor) {
                let constructor_start = self.current_span();
                self.advance();
                self.consume(TokenKind::LeftParen, "Expected '(' after 'constructor'")?;
                let params = self.parse_typed_parameter_list()?;
                self.consume(
                    TokenKind::RightParen,
                    "Expected ')' after constructor parameters",
                )?;
                self.consume(
                    TokenKind::LeftBrace,
                    "Expected '{' after constructor parameters",
                )?;
                let body = self.parse_block()?;
                self.consume(TokenKind::RightBrace, "Expected '}' after constructor body")?;
                let constructor_end = self.current_span();
                constructor = Some(EnumConstructor {
                    parameters: params,
                    body,
                    span: constructor_start.combine(&constructor_end),
                });
                is_rich_enum = true;
            } else if matches!(token, TokenKind::Function) {
                let method_start = self.current_span();
                self.advance();
                let method_name = self.parse_identifier()?;
                self.consume(TokenKind::LeftParen, "Expected '(' after method name")?;
                let params = self.parse_typed_parameter_list()?;
                self.consume(
                    TokenKind::RightParen,
                    "Expected ')' after method parameters",
                )?;
                let return_type = if self.match_token(&[TokenKind::Colon]) {
                    Some(self.parse_type()?)
                } else {
                    None
                };
                self.consume(TokenKind::LeftBrace, "Expected '{' after method signature")?;
                let body = self.parse_block()?;
                self.consume(TokenKind::RightBrace, "Expected '}' after method body")?;
                let method_end = self.current_span();
                methods.push(EnumMethod {
                    name: method_name,
                    parameters: params,
                    return_type,
                    body,
                    span: method_start.combine(&method_end),
                });
                is_rich_enum = true;
            } else {
                return Err(ParserError {
                    message: format!("Unexpected token in enum body: {:?}", token),
                    span: self.current_span(),
                });
            }

            // Commas are optional separators in rich enums (like class member syntax).
            // For simple enums they're still expected between members.
            if !self.check(&TokenKind::RightBrace) {
                self.match_token(&[TokenKind::Comma]);
            }
        }

        self.consume(TokenKind::RightBrace, "Expected '}' after enum body")?;
        let end_span = self.current_span();

        let members = self.alloc_vec(members);
        let fields = if is_rich_enum {
            self.alloc_vec(fields)
        } else {
            &[]
        };
        let methods = if is_rich_enum {
            self.alloc_vec(methods)
        } else {
            &[]
        };
        let implements = self.alloc_vec(implements);

        Ok(Statement::Enum(EnumDeclaration {
            name,
            members,
            fields,
            constructor: if is_rich_enum { constructor } else { None },
            methods,
            implements,
            span: start_span.combine(&end_span),
        }))
    }

    fn parse_import_declaration(&mut self) -> Result<Statement<'arena>, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::Import, "Expected 'import'")?;

        let is_type_only = if self.check(&TokenKind::Type) {
            self.advance();
            true
        } else {
            false
        };

        let clause = if self.match_token(&[TokenKind::Star]) {
            self.consume(TokenKind::As, "Expected 'as' after '*'")?;
            let name = self.parse_identifier()?;
            ImportClause::Namespace(name)
        } else if self.check(&TokenKind::LeftBrace) {
            self.consume(TokenKind::LeftBrace, "Expected '{'")?;
            let specifiers = self.parse_import_specifiers()?;
            self.consume(TokenKind::RightBrace, "Expected '}'")?;
            if is_type_only {
                ImportClause::TypeOnly(specifiers)
            } else {
                ImportClause::Named(specifiers)
            }
        } else {
            if is_type_only {
                return Err(ParserError {
                    message: "Type-only imports must use named import syntax: import type { Name } from '...'".to_string(),
                    span: self.current_span(),
                });
            }
            let name = self.parse_identifier()?;

            // Check for alias: default as LocalName
            let local_name = if self.match_token(&[TokenKind::As]) {
                self.parse_identifier()?
            } else {
                name.clone()
            };

            // Check for mixed import: default, { named }
            if self.match_token(&[TokenKind::Comma]) {
                self.consume(
                    TokenKind::LeftBrace,
                    "Expected '{' after ',' in mixed import",
                )?;
                let specifiers = self.parse_import_specifiers()?;
                self.consume(TokenKind::RightBrace, "Expected '}'")?;
                ImportClause::Mixed {
                    default: local_name,
                    named: specifiers,
                }
            } else {
                ImportClause::Default(local_name)
            }
        };

        self.consume(TokenKind::From, "Expected 'from' in import")?;

        let source = match &self.current().kind {
            TokenKind::String(s) => {
                let src = s.clone();
                self.advance();
                src
            }
            _ => {
                return Err(ParserError {
                    message: "Expected string literal for import source".to_string(),
                    span: self.current_span(),
                })
            }
        };

        let end_span = self.current_span();

        Ok(Statement::Import(ImportDeclaration {
            clause,
            source,
            span: start_span.combine(&end_span),
        }))
    }

    fn parse_import_specifiers(&mut self) -> Result<&'arena [ImportSpecifier], ParserError> {
        let mut specifiers = Vec::new();

        loop {
            let imported = self.parse_identifier()?;
            let local = if self.match_token(&[TokenKind::As]) {
                Some(self.parse_identifier()?)
            } else {
                None
            };

            let span = if let Some(ref l) = local {
                imported.span.combine(&l.span)
            } else {
                imported.span
            };

            specifiers.push(ImportSpecifier {
                imported,
                local,
                span,
            });

            if !self.match_token(&[TokenKind::Comma]) {
                break;
            }
        }

        let specifiers = self.alloc_vec(specifiers);
        Ok(specifiers)
    }

    fn parse_export_declaration(&mut self) -> Result<Statement<'arena>, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::Export, "Expected 'export'")?;

        let is_default = match &self.current().kind {
            TokenKind::Identifier(s) if self.resolve(*s) == "default" => {
                self.advance();
                true
            }
            _ => false,
        };

        let kind = if is_default {
            // Check if this is a class or function declaration
            // If so, treat it as a declaration export, not an expression export
            if matches!(
                &self.current().kind,
                TokenKind::Class | TokenKind::Abstract | TokenKind::Final | TokenKind::Function
            ) {
                let decl = self.parse_statement()?;
                ExportKind::Declaration(self.alloc(decl))
            } else {
                let expr = self.parse_expression()?;
                ExportKind::Default(self.alloc(expr))
            }
        } else if self.check(&TokenKind::LeftBrace) {
            self.consume(TokenKind::LeftBrace, "Expected '{'")?;
            let specifiers = self.parse_export_specifiers()?;
            self.consume(TokenKind::RightBrace, "Expected '}'")?;

            let source = if self.match_token(&[TokenKind::From]) {
                match &self.current().kind {
                    TokenKind::String(s) => {
                        let source_string = s.clone();
                        self.advance();
                        Some(source_string)
                    }
                    _ => {
                        return Err(ParserError {
                            message: "Expected string literal after 'from'".to_string(),
                            span: self.current_span(),
                        });
                    }
                }
            } else {
                None
            };

            ExportKind::Named { specifiers, source }
        } else {
            let decl = self.parse_statement()?;
            ExportKind::Declaration(self.alloc(decl))
        };

        let end_span = self.current_span();

        Ok(Statement::Export(ExportDeclaration {
            kind,
            span: start_span.combine(&end_span),
        }))
    }

    fn parse_export_specifiers(&mut self) -> Result<&'arena [ExportSpecifier], ParserError> {
        let mut specifiers = Vec::new();

        loop {
            let local = self.parse_identifier()?;
            let exported = if self.match_token(&[TokenKind::As]) {
                Some(self.parse_identifier()?)
            } else {
                None
            };

            let span = if let Some(ref e) = exported {
                local.span.combine(&e.span)
            } else {
                local.span
            };

            specifiers.push(ExportSpecifier {
                local,
                exported,
                span,
            });

            if !self.match_token(&[TokenKind::Comma]) {
                break;
            }
        }

        let specifiers = self.alloc_vec(specifiers);
        Ok(specifiers)
    }

    fn parse_declare_statement(&mut self) -> Result<Statement<'arena>, ParserError> {
        let _start_span = self.current_span();
        self.consume(TokenKind::Declare, "Expected 'declare'")?;

        match &self.current().kind {
            TokenKind::Function => self.parse_declare_function(),
            TokenKind::Const => self.parse_declare_const(),
            TokenKind::Namespace => self.parse_declare_namespace(),
            TokenKind::Type => self.parse_declare_type(),
            TokenKind::Interface => self.parse_declare_interface(),
            _ => Err(ParserError {
                message: "Expected 'function', 'const', 'namespace', 'type', or 'interface' after 'declare'".to_string(),
                span: self.current_span(),
            }),
        }
    }

    fn parse_declare_function(&mut self) -> Result<Statement<'arena>, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::Function, "Expected 'function'")?;

        let name = self.parse_identifier_or_keyword()?;

        let type_parameters = if self.match_token(&[TokenKind::LessThan]) {
            Some(self.parse_type_parameters()?)
        } else {
            None
        };

        self.consume(TokenKind::LeftParen, "Expected '(' after function name")?;
        let parameters = self.parse_parameter_list()?;
        self.consume(TokenKind::RightParen, "Expected ')' after parameters")?;

        let return_type = if self.match_token(&[TokenKind::Colon]) {
            self.parse_type()?
        } else {
            Type::new(
                TypeKind::Primitive(PrimitiveType::Void),
                self.current_span(),
            )
        };

        let throws = if self.match_token(&[TokenKind::Throws]) {
            let mut error_types = Vec::new();

            if self.check(&TokenKind::LeftParen) {
                self.consume(TokenKind::LeftParen, "Expected '(' after 'throws'")?;
                if !self.check(&TokenKind::RightParen) {
                    loop {
                        error_types.push(self.parse_type()?);
                        if !self.match_token(&[TokenKind::Comma]) {
                            break;
                        }
                    }
                }
                self.consume(TokenKind::RightParen, "Expected ')' after throws types")?;
            } else {
                error_types.push(self.parse_type()?);
            }
            let error_types = self.alloc_vec(error_types);
            Some(error_types)
        } else {
            None
        };

        let end_span = self.current_span();

        Ok(Statement::DeclareFunction(DeclareFunctionStatement {
            name,
            type_parameters,
            parameters,
            return_type,
            throws,
            is_export: false,
            span: start_span.combine(&end_span),
        }))
    }

    fn parse_declare_const(&mut self) -> Result<Statement<'arena>, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::Const, "Expected 'const'")?;

        let name = self.parse_identifier()?;

        self.consume(TokenKind::Colon, "Expected ':' after const name")?;
        let type_annotation = self.parse_type()?;

        let end_span = self.current_span();

        Ok(Statement::DeclareConst(DeclareConstStatement {
            name,
            type_annotation,
            is_export: false,
            span: start_span.combine(&end_span),
        }))
    }

    fn parse_declare_namespace(&mut self) -> Result<Statement<'arena>, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::Namespace, "Expected 'namespace'")?;

        let name = self.parse_identifier()?;

        self.consume(TokenKind::LeftBrace, "Expected '{' after namespace name")?;

        let mut members = Vec::new();
        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            let is_export = self.match_token(&[TokenKind::Export]);

            let member = match &self.current().kind {
                TokenKind::Function => {
                    let mut func_stmt = self.parse_declare_function()?;
                    if let Statement::DeclareFunction(ref mut func) = func_stmt {
                        func.is_export = is_export;
                    }
                    func_stmt
                }
                TokenKind::Const => {
                    let mut const_stmt = self.parse_declare_const()?;
                    if let Statement::DeclareConst(ref mut const_decl) = const_stmt {
                        const_decl.is_export = is_export;
                    }
                    const_stmt
                }
                _ => {
                    return Err(ParserError {
                        message: format!(
                            "Expected 'function' or 'const' in namespace, found {:?}",
                            self.current().kind
                        ),
                        span: self.current_span(),
                    });
                }
            };

            members.push(member);
        }

        self.consume(TokenKind::RightBrace, "Expected '}' after namespace body")?;

        let end_span = self.current_span();

        let members = self.alloc_vec(members);

        Ok(Statement::DeclareNamespace(DeclareNamespaceStatement {
            name,
            members,
            span: start_span.combine(&end_span),
        }))
    }

    fn parse_declare_type(&mut self) -> Result<Statement<'arena>, ParserError> {
        self.consume(TokenKind::Type, "Expected 'type'")?;
        let type_alias = self.parse_type_alias_inner()?;
        Ok(Statement::DeclareType(type_alias))
    }

    fn parse_declare_interface(&mut self) -> Result<Statement<'arena>, ParserError> {
        self.consume(TokenKind::Interface, "Expected 'interface'")?;
        let interface = self.parse_interface_inner()?;
        Ok(Statement::DeclareInterface(interface))
    }

    fn parse_class_declaration(&mut self) -> Result<Statement<'arena>, ParserError> {
        let start_span = self.current_span();

        let decorators = self.parse_decorators()?;

        let mut is_abstract = false;
        let mut is_final = false;

        loop {
            if self.match_token(&[TokenKind::Abstract]) {
                is_abstract = true;
            } else if self.match_token(&[TokenKind::Final]) {
                is_final = true;
            } else {
                break;
            }
        }

        self.consume(TokenKind::Class, "Expected 'class'")?;

        let name = self.parse_identifier()?;

        let type_parameters = if self.match_token(&[TokenKind::LessThan]) {
            Some(self.parse_type_parameters()?)
        } else {
            None
        };

        let primary_constructor = if self.match_token(&[TokenKind::LeftParen]) {
            let params = self.parse_primary_constructor_parameters()?;
            self.consume(
                TokenKind::RightParen,
                "Expected ')' after primary constructor parameters",
            )?;
            Some(params)
        } else {
            None
        };

        let (extends, parent_constructor_args) = if self.match_token(&[TokenKind::Extends]) {
            let parent_type = self.parse_type()?;

            let parent_args = if self.match_token(&[TokenKind::LeftParen]) {
                let mut args = Vec::new();
                if !self.check(&TokenKind::RightParen) {
                    loop {
                        args.push(self.parse_expression()?);
                        if !self.match_token(&[TokenKind::Comma]) {
                            break;
                        }
                    }
                }
                self.consume(
                    TokenKind::RightParen,
                    "Expected ')' after parent constructor arguments",
                )?;
                let args = self.alloc_vec(args);
                Some(args)
            } else {
                None
            };

            (Some(parent_type), parent_args)
        } else {
            (None, None)
        };

        let mut implements = Vec::new();
        if self.match_token(&[TokenKind::Implements]) {
            implements.push(self.parse_type()?);
            while self.match_token(&[TokenKind::Comma]) {
                implements.push(self.parse_type()?);
            }
        }

        let use_braces = self.check(&TokenKind::LeftBrace);

        if use_braces {
            self.consume(TokenKind::LeftBrace, "Expected '{' after class header")?;
        }

        let mut members = Vec::new();
        if use_braces {
            while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
                members.push(self.parse_class_member()?);
            }
            self.consume(TokenKind::RightBrace, "Expected '}' after class body")?;
        } else {
            while !self.check(&TokenKind::End) && !self.is_at_end() {
                members.push(self.parse_class_member()?);
            }
            self.consume(TokenKind::End, "Expected 'end' after class body")?;
        }

        if primary_constructor.is_some() {
            let has_parameterized_constructor = members
                .iter()
                .any(|m| matches!(m, ClassMember::Constructor(c) if !c.parameters.is_empty()));

            if has_parameterized_constructor {
                return Err(ParserError {
                    message: "Cannot have both a primary constructor and a parameterized constructor in the same class".to_string(),
                    span: start_span,
                });
            }
        }

        let end_span = self.current_span();

        let implements = self.alloc_vec(implements);
        let members = self.alloc_vec(members);

        Ok(Statement::Class(ClassDeclaration {
            decorators,
            is_abstract,
            is_final,
            name,
            type_parameters,
            primary_constructor,
            extends,
            parent_constructor_args,
            implements,
            members,
            span: start_span.combine(&end_span),
        }))
    }

    fn parse_class_member(&mut self) -> Result<ClassMember<'arena>, ParserError> {
        let decorators = self.parse_decorators()?;

        let access = if self.match_token(&[TokenKind::Public]) {
            Some(AccessModifier::Public)
        } else if self.match_token(&[TokenKind::Private]) {
            Some(AccessModifier::Private)
        } else if self.match_token(&[TokenKind::Protected]) {
            Some(AccessModifier::Protected)
        } else {
            None
        };

        let is_static = self.match_token(&[TokenKind::Static]);
        let mut is_abstract = false;
        let mut is_final = false;

        loop {
            if self.match_token(&[TokenKind::Abstract]) {
                is_abstract = true;
            } else if self.match_token(&[TokenKind::Final]) {
                is_final = true;
            } else {
                break;
            }
        }
        let is_override = self.match_token(&[TokenKind::Override]);
        let is_readonly = self.match_token(&[TokenKind::Readonly]);

        // Disambiguate getter/setter syntax from methods named "get"/"set":
        // `get propName(): T { ... }` is a getter (get followed by identifier)
        // `get(): T { ... }` is a method named "get" (get followed by `(` or `<`)
        if self.check(&TokenKind::Get)
            && matches!(self.nth_token_kind(1), Some(TokenKind::Identifier(_)))
        {
            return self.parse_getter(decorators, access, is_static);
        }
        if self.check(&TokenKind::Set)
            && matches!(self.nth_token_kind(1), Some(TokenKind::Identifier(_)))
        {
            return self.parse_setter(decorators, access, is_static);
        }

        if self.check(&TokenKind::Constructor) {
            return self.parse_constructor(decorators);
        }

        if self.check(&TokenKind::Operator) {
            return self.parse_operator(decorators, access);
        }

        let start_span = self.current_span();
        // Accept `get`/`set` as member names when they aren't acting as getter/setter keywords
        let name = if self.check(&TokenKind::Get) || self.check(&TokenKind::Set) {
            let keyword_str = if self.check(&TokenKind::Get) {
                "get"
            } else {
                "set"
            };
            let span = self.current_span();
            let id = self.interner.intern(keyword_str);
            self.advance();
            Spanned::new(id, span)
        } else {
            self.parse_identifier()?
        };

        if self.check(&TokenKind::Colon) {
            // Property
            self.advance();
            let type_annotation = self.parse_type()?;

            let initializer = if self.match_token(&[TokenKind::Equal]) {
                Some(self.parse_expression()?)
            } else {
                None
            };

            self.match_token(&[TokenKind::Semicolon]);

            let end_span = self.current_span();

            Ok(ClassMember::Property(PropertyDeclaration {
                decorators,
                access,
                is_static,
                is_readonly,
                name,
                type_annotation,
                initializer,
                span: start_span.combine(&end_span),
            }))
        } else if self.check(&TokenKind::LessThan) || self.check(&TokenKind::LeftParen) {
            // Method (may have type parameters before the parameter list)
            let type_parameters = if self.match_token(&[TokenKind::LessThan]) {
                Some(self.parse_type_parameters()?)
            } else {
                None
            };

            self.consume(TokenKind::LeftParen, "Expected '('")?;
            let parameters = self.parse_parameter_list()?;
            self.consume(TokenKind::RightParen, "Expected ')'")?;

            let return_type = if self.match_token(&[TokenKind::Colon]) {
                Some(self.parse_type()?)
            } else {
                None
            };

            let body = if is_abstract {
                self.match_token(&[TokenKind::Semicolon]);
                None
            } else {
                let use_braces = self.check(&TokenKind::LeftBrace);
                if use_braces {
                    self.consume(TokenKind::LeftBrace, "Expected '{'")?;
                }
                let block = self.parse_block()?;
                if use_braces {
                    self.consume(TokenKind::RightBrace, "Expected '}' after method body")?;
                } else {
                    self.consume(TokenKind::End, "Expected 'end' after method body")?;
                }
                Some(block)
            };

            let end_span = self.current_span();

            Ok(ClassMember::Method(MethodDeclaration {
                decorators,
                access,
                is_static,
                is_abstract,
                is_override,
                is_final,
                name,
                type_parameters,
                parameters,
                return_type,
                body,
                span: start_span.combine(&end_span),
            }))
        } else {
            Err(ParserError {
                message: "Expected ':' for property or '(' for method".into(),
                span: self.current_span(),
            })
        }
    }

    fn parse_constructor(
        &mut self,
        decorators: &'arena [Decorator<'arena>],
    ) -> Result<ClassMember<'arena>, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::Constructor, "Expected 'constructor'")?;
        self.consume(TokenKind::LeftParen, "Expected '('")?;
        let parameters = self.parse_parameter_list()?;
        self.consume(TokenKind::RightParen, "Expected ')'")?;

        let use_braces = self.check(&TokenKind::LeftBrace);
        if use_braces {
            self.consume(TokenKind::LeftBrace, "Expected '{'")?;
        }
        let body = self.parse_block()?;
        if use_braces {
            self.consume(TokenKind::RightBrace, "Expected '}' after constructor body")?;
        } else {
            self.consume(TokenKind::End, "Expected 'end' after constructor body")?;
        }
        let end_span = self.current_span();

        Ok(ClassMember::Constructor(ConstructorDeclaration {
            decorators,
            parameters,
            body,
            span: start_span.combine(&end_span),
        }))
    }

    fn parse_getter(
        &mut self,
        decorators: &'arena [Decorator<'arena>],
        access: Option<AccessModifier>,
        is_static: bool,
    ) -> Result<ClassMember<'arena>, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::Get, "Expected 'get'")?;
        let name = self.parse_identifier()?;
        self.consume(TokenKind::LeftParen, "Expected '('")?;
        self.consume(TokenKind::RightParen, "Expected ')'")?;
        self.consume(TokenKind::Colon, "Expected ':' for getter return type")?;
        let return_type = self.parse_type()?;

        let use_braces = self.check(&TokenKind::LeftBrace);
        if use_braces {
            self.consume(TokenKind::LeftBrace, "Expected '{'")?;
        }
        let body = self.parse_block()?;
        if use_braces {
            self.consume(TokenKind::RightBrace, "Expected '}' after getter body")?;
        } else {
            self.consume(TokenKind::End, "Expected 'end' after getter body")?;
        }
        let end_span = self.current_span();

        Ok(ClassMember::Getter(GetterDeclaration {
            decorators,
            access,
            is_static,
            name,
            return_type,
            body,
            span: start_span.combine(&end_span),
        }))
    }

    fn parse_setter(
        &mut self,
        decorators: &'arena [Decorator<'arena>],
        access: Option<AccessModifier>,
        is_static: bool,
    ) -> Result<ClassMember<'arena>, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::Set, "Expected 'set'")?;
        let name = self.parse_identifier()?;
        self.consume(TokenKind::LeftParen, "Expected '('")?;

        let param_start = self.current_span();
        let param_pattern = self.parse_pattern()?;
        self.consume(TokenKind::Colon, "Expected ':' for setter parameter type")?;
        let param_type = self.parse_type()?;

        self.consume(TokenKind::RightParen, "Expected ')'")?;

        let use_braces = self.check(&TokenKind::LeftBrace);
        if use_braces {
            self.consume(TokenKind::LeftBrace, "Expected '{'")?;
        }
        let body = self.parse_block()?;
        if use_braces {
            self.consume(TokenKind::RightBrace, "Expected '}' after setter body")?;
        } else {
            self.consume(TokenKind::End, "Expected 'end' after setter body")?;
        }
        let end_span = self.current_span();

        let parameter = Parameter {
            pattern: param_pattern,
            type_annotation: Some(param_type),
            default: None,
            is_rest: false,
            is_optional: false,
            span: param_start,
        };

        Ok(ClassMember::Setter(SetterDeclaration {
            decorators,
            access,
            is_static,
            name,
            parameter,
            body,
            span: start_span.combine(&end_span),
        }))
    }

    fn parse_operator(
        &mut self,
        decorators: &'arena [Decorator<'arena>],
        access: Option<AccessModifier>,
    ) -> Result<ClassMember<'arena>, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::Operator, "Expected 'operator'")?;

        let operator_kind = self.parse_operator_kind()?;

        let mut operator = operator_kind;

        // Check for unary minus
        if operator == OperatorKind::Subtract
            && self.check(&TokenKind::LeftParen)
            && self.nth_token_kind(1) == Some(&TokenKind::RightParen)
        {
            self.advance();
            self.advance();
            operator = OperatorKind::UnaryMinus;
        }

        let parameters: &'arena [Parameter<'arena>] = if operator == OperatorKind::NewIndex {
            self.consume(TokenKind::LeftParen, "Expected '(' after operator")?;
            let params = self.parse_parameter_list()?;
            self.consume(TokenKind::RightParen, "Expected ')'")?;
            if params.len() != 2 {
                return Err(ParserError {
                    message: "operator []= requires exactly 2 parameters (index and value)".into(),
                    span: self.current_span(),
                });
            }
            params
        } else if self.check(&TokenKind::LeftParen) {
            self.consume(TokenKind::LeftParen, "Expected '(' after operator")?;
            if !self.check(&TokenKind::RightParen) {
                let params = self.parse_parameter_list()?;
                self.consume(TokenKind::RightParen, "Expected ')'")?;
                params
            } else {
                self.consume(TokenKind::RightParen, "Expected ')'")?;
                &[]
            }
        } else {
            &[]
        };

        let return_type = if self.match_token(&[TokenKind::Colon]) {
            Some(self.parse_type()?)
        } else {
            None
        };

        let use_braces = self.check(&TokenKind::LeftBrace);
        if use_braces {
            self.consume(TokenKind::LeftBrace, "Expected '{'")?;
        }
        let body = self.parse_block()?;
        if use_braces {
            self.consume(TokenKind::RightBrace, "Expected '}' after operator body")?;
        } else {
            self.consume(TokenKind::End, "Expected 'end' after operator body")?;
        }

        let end_span = self.current_span();

        Ok(ClassMember::Operator(OperatorDeclaration {
            decorators,
            access,
            operator,
            parameters,
            return_type,
            body,
            span: start_span.combine(&end_span),
        }))
    }

    fn parse_operator_kind(&mut self) -> Result<OperatorKind, ParserError> {
        let op = match &self.current().kind {
            TokenKind::Plus => {
                self.advance();
                OperatorKind::Add
            }
            TokenKind::Minus => {
                self.advance();
                OperatorKind::Subtract
            }
            TokenKind::Star => {
                self.advance();
                OperatorKind::Multiply
            }
            TokenKind::Slash => {
                self.advance();
                OperatorKind::Divide
            }
            TokenKind::Percent => {
                self.advance();
                OperatorKind::Modulo
            }
            TokenKind::Caret => {
                self.advance();
                OperatorKind::Power
            }
            TokenKind::DotDot => {
                self.advance();
                OperatorKind::Concatenate
            }
            TokenKind::SlashSlash => {
                self.advance();
                OperatorKind::FloorDivide
            }
            TokenKind::EqualEqual => {
                self.advance();
                OperatorKind::Equal
            }
            TokenKind::BangEqual | TokenKind::TildeEqual => {
                self.advance();
                OperatorKind::NotEqual
            }
            TokenKind::LessThan => {
                self.advance();
                OperatorKind::LessThan
            }
            TokenKind::LessEqual => {
                self.advance();
                OperatorKind::LessThanOrEqual
            }
            TokenKind::GreaterThan => {
                self.advance();
                OperatorKind::GreaterThan
            }
            TokenKind::GreaterEqual => {
                self.advance();
                OperatorKind::GreaterThanOrEqual
            }
            TokenKind::Ampersand => {
                self.advance();
                OperatorKind::BitwiseAnd
            }
            TokenKind::Pipe => {
                self.advance();
                OperatorKind::BitwiseOr
            }
            TokenKind::Tilde => {
                self.advance();
                OperatorKind::BitwiseXor
            }
            TokenKind::LessLess => {
                self.advance();
                OperatorKind::ShiftLeft
            }
            TokenKind::GreaterGreater => {
                self.advance();
                OperatorKind::ShiftRight
            }
            TokenKind::LeftBracket => {
                self.advance();
                if self.check(&TokenKind::RightBracket)
                    && self.nth_token_kind(1) == Some(&TokenKind::Equal)
                {
                    self.advance();
                    self.advance();
                    OperatorKind::NewIndex
                } else if self.match_token(&[TokenKind::RightBracket]) {
                    OperatorKind::Index
                } else {
                    return Err(ParserError {
                        message: "Expected ']' or '=' after '[' in operator definition".into(),
                        span: self.current_span(),
                    });
                }
            }
            TokenKind::LeftParen => {
                self.advance();
                self.consume(
                    TokenKind::RightParen,
                    "Expected ')' after '(' in operator()",
                )?;
                OperatorKind::Call
            }
            TokenKind::Hash => {
                self.advance();
                OperatorKind::Length
            }
            _ => {
                return Err(ParserError {
                    message: "Invalid operator symbol".to_string(),
                    span: self.current_span(),
                });
            }
        };
        Ok(op)
    }

    fn parse_throw_statement(&mut self) -> Result<Statement<'arena>, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::Throw, "Expected 'throw'")?;

        let expression = self.parse_expression()?;
        let end_span = expression.span;

        Ok(Statement::Throw(ThrowStatement {
            expression,
            span: start_span.combine(&end_span),
        }))
    }

    fn parse_rethrow_statement(&mut self) -> Result<Statement<'arena>, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::Rethrow, "Expected 'rethrow'")?;

        Ok(Statement::Rethrow(start_span))
    }

    fn parse_namespace_declaration(&mut self) -> Result<Statement<'arena>, ParserError> {
        let start_span = self.current_span();

        if self.has_namespace {
            return Err(ParserError {
                message: "Only one namespace declaration allowed per file".to_string(),
                span: start_span,
            });
        }

        if !self.is_first_statement {
            return Err(ParserError {
                message: "Namespace declaration must be the first statement".to_string(),
                span: start_span,
            });
        }

        self.consume(TokenKind::Namespace, "Expected 'namespace'")?;

        let mut path = Vec::new();

        loop {
            let name = self.parse_identifier()?;
            path.push(name);

            if self.match_token(&[TokenKind::Dot]) {
                continue;
            } else {
                break;
            }
        }

        self.consume(
            TokenKind::Semicolon,
            "Expected ';' after namespace declaration",
        )?;

        self.has_namespace = true;

        let end_span = self.current_span();

        Ok(Statement::Namespace(NamespaceDeclaration {
            path,
            span: start_span.combine(&end_span),
        }))
    }

    fn parse_try_statement(&mut self) -> Result<Statement<'arena>, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::Try, "Expected 'try'")?;

        let use_braces = self.check(&TokenKind::LeftBrace);
        if use_braces {
            self.consume(TokenKind::LeftBrace, "Expected '{'")?;
        }
        let try_block = self.parse_block()?;
        if use_braces {
            self.consume(TokenKind::RightBrace, "Expected '}' after try block")?;
        } else {
            self.consume(TokenKind::End, "Expected 'end' after try block")?;
        }

        let mut catch_clauses = Vec::new();
        while self.match_token(&[TokenKind::Catch]) {
            catch_clauses.push(self.parse_catch_clause()?);
        }

        let finally_block = if self.match_token(&[TokenKind::Finally]) {
            let use_braces = self.check(&TokenKind::LeftBrace);
            if use_braces {
                self.consume(TokenKind::LeftBrace, "Expected '{'")?;
            }
            let finally = self.parse_block()?;
            if use_braces {
                self.consume(TokenKind::RightBrace, "Expected '}' after finally block")?;
            } else {
                self.consume(TokenKind::End, "Expected 'end' after finally block")?;
            }
            Some(finally)
        } else {
            None
        };

        let end_span = if let Some(last_catch) = catch_clauses.last() {
            last_catch.span
        } else if let Some(ref fin) = finally_block {
            fin.span
        } else {
            try_block.span
        };

        let catch_clauses = self.alloc_vec(catch_clauses);

        Ok(Statement::Try(TryStatement {
            try_block,
            catch_clauses,
            finally_block,
            span: start_span.combine(&end_span),
        }))
    }

    fn parse_catch_clause(&mut self) -> Result<CatchClause<'arena>, ParserError> {
        let start_span = self.current_span();

        self.consume(TokenKind::LeftParen, "Expected '(' after 'catch'")?;

        let variable = self.parse_identifier()?;
        let span_after_var = self.current_span();

        let pattern = if self.match_token(&[TokenKind::Colon]) {
            let mut type_annotations = Vec::new();
            type_annotations.push(self.parse_type()?);

            while self.match_token(&[TokenKind::Pipe]) {
                type_annotations.push(self.parse_type()?);
            }

            if type_annotations.len() == 1 {
                let type_annotation = type_annotations.into_iter().next().unwrap();
                let span = type_annotation.span;
                CatchPattern::Typed {
                    variable,
                    type_annotation,
                    span: start_span.combine(&span),
                }
            } else {
                let type_annotations = self.alloc_vec(type_annotations);
                CatchPattern::MultiTyped {
                    variable,
                    type_annotations,
                    span: start_span.combine(&span_after_var),
                }
            }
        } else {
            CatchPattern::Untyped {
                variable,
                span: start_span.combine(&span_after_var),
            }
        };

        self.consume(TokenKind::RightParen, "Expected ')' after catch parameter")?;

        let use_braces = self.check(&TokenKind::LeftBrace);
        if use_braces {
            self.consume(TokenKind::LeftBrace, "Expected '{'")?;
        }
        let body = self.parse_block()?;
        if use_braces {
            self.consume(TokenKind::RightBrace, "Expected '}' after catch body")?;
        } else {
            self.consume(TokenKind::End, "Expected 'end' after catch body")?;
        }

        let end_span = body.span;

        Ok(CatchClause {
            pattern,
            body,
            span: start_span.combine(&end_span),
        })
    }

    // Helper methods

    fn parse_decorators(&mut self) -> Result<&'arena [Decorator<'arena>], ParserError> {
        let mut decorators = Vec::new();

        while self.check(&TokenKind::At) {
            decorators.push(self.parse_decorator()?);
        }

        let decorators = self.alloc_vec(decorators);
        Ok(decorators)
    }

    fn parse_decorator(&mut self) -> Result<Decorator<'arena>, ParserError> {
        let start_span = self.current_span();
        self.consume(TokenKind::At, "Expected '@'")?;

        let expression = self.parse_decorator_expression()?;
        let end_span = self.current_span();

        Ok(Decorator {
            expression,
            span: start_span.combine(&end_span),
        })
    }

    fn parse_decorator_expression(&mut self) -> Result<DecoratorExpression<'arena>, ParserError> {
        let start_span = self.current_span();
        let name = self.parse_identifier_or_keyword()?;

        let mut expr = DecoratorExpression::Identifier(name);

        loop {
            match &self.current().kind {
                TokenKind::Dot => {
                    self.advance();
                    let property = self.parse_identifier_or_keyword()?;
                    let span = start_span.combine(&property.span);
                    expr = DecoratorExpression::Member {
                        object: self.alloc(expr),
                        property,
                        span,
                    };
                }
                TokenKind::LeftParen => {
                    self.advance();
                    let mut arguments = Vec::new();

                    if !self.check(&TokenKind::RightParen) {
                        loop {
                            arguments.push(self.parse_expression()?);
                            if !self.match_token(&[TokenKind::Comma]) {
                                break;
                            }
                        }
                    }

                    let end_span = self.current_span();
                    self.consume(
                        TokenKind::RightParen,
                        "Expected ')' after decorator arguments",
                    )?;

                    let arguments = self.alloc_vec(arguments);
                    let span = start_span.combine(&end_span);
                    expr = DecoratorExpression::Call {
                        callee: self.alloc(expr),
                        arguments,
                        span,
                    };
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    pub(super) fn parse_identifier(&mut self) -> Result<Ident, ParserError> {
        match &self.current().kind {
            TokenKind::Identifier(name) => {
                let span = self.current_span();
                let ident = Spanned::new(*name, span);
                self.advance();
                Ok(ident)
            }
            _ => Err(ParserError {
                message: format!("Expected identifier, got {:?}", self.current().kind),
                span: self.current_span(),
            }),
        }
    }

    fn parse_identifier_or_keyword(&mut self) -> Result<Ident, ParserError> {
        let span = self.current_span();
        let name = match &self.current().kind {
            TokenKind::Identifier(name) => *name,
            kind if kind.is_keyword() => match kind.to_keyword_str() {
                Some(s) => self.interner.intern(s),
                None => {
                    return Err(ParserError {
                        message: format!(
                            "Internal error: keyword {:?} missing string representation",
                            kind
                        ),
                        span,
                    });
                }
            },
            _ => {
                return Err(ParserError {
                    message: format!(
                        "Expected identifier or keyword, got {:?}",
                        self.current().kind
                    ),
                    span,
                });
            }
        };

        self.advance();
        Ok(Spanned::new(name, span))
    }

    /// Parse an identifier or keyword for interface member names
    /// This allows keywords like 'type' to be used as property names
    fn parse_interface_member_name(&mut self) -> Result<Ident, ParserError> {
        let span = self.current_span();

        match &self.current().kind {
            TokenKind::Identifier(name) => {
                let id = *name;
                self.advance();
                Ok(Spanned::new(id, span))
            }
            kind if kind.is_keyword() => {
                // Allow keywords as member names in interfaces
                if let Some(s) = kind.to_keyword_str() {
                    let id = self.interner.intern(s);
                    self.advance();
                    Ok(Spanned::new(id, span))
                } else {
                    Err(ParserError {
                        message: format!(
                            "Internal error: keyword {:?} missing string representation",
                            kind
                        ),
                        span,
                    })
                }
            }
            _ => Err(ParserError {
                message: format!(
                    "Expected identifier or keyword, got {:?}",
                    self.current().kind
                ),
                span,
            }),
        }
    }

    pub(super) fn parse_type_parameters(&mut self) -> Result<&'arena [TypeParameter<'arena>], ParserError> {
        let mut params = Vec::new();

        loop {
            let param_start = self.current_span();
            let name = self.parse_identifier()?;

            let constraint = if self.match_token(&[TokenKind::Extends, TokenKind::Implements]) {
                let parsed_constraint = self.parse_type()?;
                Some(self.alloc(parsed_constraint))
            } else {
                None
            };

            let default = if self.match_token(&[TokenKind::Equal]) {
                let parsed_default = self.parse_type()?;
                Some(self.alloc(parsed_default))
            } else {
                None
            };

            let param_end = self.current_span();

            params.push(TypeParameter {
                name,
                constraint,
                default,
                span: param_start.combine(&param_end),
            });

            if !self.match_token(&[TokenKind::Comma]) {
                break;
            }
        }

        self.consume(TokenKind::GreaterThan, "Expected '>' after type parameters")?;

        let params = self.alloc_vec(params);
        Ok(params)
    }

    #[inline]
    pub(super) fn parse_parameter_list(&mut self) -> Result<&'arena [Parameter<'arena>], ParserError> {
        let mut params = Vec::with_capacity(4);

        if self.check(&TokenKind::RightParen) {
            return Ok(&[]);
        }

        loop {
            let param_start = self.current_span();
            let is_rest = self.match_token(&[TokenKind::DotDotDot]);

            let pattern =
                if is_rest && self.check(&TokenKind::RightParen) || self.check(&TokenKind::Comma) {
                    Pattern::Wildcard(param_start)
                } else {
                    self.parse_pattern()?
                };

            let is_optional = self.match_token(&[TokenKind::Question]);

            let type_annotation = if self.match_token(&[TokenKind::Colon]) {
                Some(self.parse_type()?)
            } else {
                None
            };

            let default = if self.match_token(&[TokenKind::Equal]) {
                Some(self.parse_expression()?)
            } else {
                None
            };

            let param_end = self.current_span();

            params.push(Parameter {
                pattern,
                type_annotation,
                default,
                is_rest,
                is_optional,
                span: param_start.combine(&param_end),
            });

            if !self.match_token(&[TokenKind::Comma]) {
                break;
            }
        }

        let params = self.alloc_vec(params);
        Ok(params)
    }

    fn parse_primary_constructor_parameters(
        &mut self,
    ) -> Result<&'arena [ConstructorParameter<'arena>], ParserError> {
        let mut params = Vec::new();

        if self.check(&TokenKind::RightParen) {
            return Ok(&[]);
        }

        loop {
            let param_start = self.current_span();

            // Parse decorators on constructor parameters (e.g., @readonly)
            let decorators = self.parse_decorators()?;

            let access = if self.match_token(&[TokenKind::Public]) {
                Some(AccessModifier::Public)
            } else if self.match_token(&[TokenKind::Private]) {
                Some(AccessModifier::Private)
            } else if self.match_token(&[TokenKind::Protected]) {
                Some(AccessModifier::Protected)
            } else {
                None
            };

            let mut is_readonly = self.match_token(&[TokenKind::Readonly]);

            // Check if @readonly decorator was used
            if !is_readonly {
                for dec in decorators {
                    if let crate::ast::statement::DecoratorExpression::Identifier(ident) =
                        &dec.expression
                    {
                        let name_str = self.interner.resolve(ident.node);
                        if name_str == "readonly" {
                            is_readonly = true;
                            break;
                        }
                    }
                }
            }

            let name = self.parse_identifier()?;

            self.consume(
                TokenKind::Colon,
                "Expected ':' after parameter name in primary constructor",
            )?;
            let type_annotation = self.parse_type()?;

            let default = if self.match_token(&[TokenKind::Equal]) {
                Some(self.parse_expression()?)
            } else {
                None
            };

            let param_end = self.current_span();

            params.push(ConstructorParameter {
                decorators,
                access,
                is_readonly,
                name,
                type_annotation,
                default,
                span: param_start.combine(&param_end),
            });

            if !self.match_token(&[TokenKind::Comma]) {
                break;
            }
        }

        let params = self.alloc_vec(params);
        Ok(params)
    }

    fn parse_typed_parameter_list(&mut self) -> Result<&'arena [Parameter<'arena>], ParserError> {
        let mut params = Vec::new();

        if self.check(&TokenKind::RightParen) {
            return Ok(&[]);
        }

        loop {
            let param_start = self.current_span();

            let name = self.parse_identifier()?;

            self.consume(TokenKind::Colon, "Expected ':' after parameter name")?;
            let type_annotation = self.parse_type()?;

            let param_end = self.current_span();

            let pattern = Pattern::Identifier(name.clone());

            params.push(Parameter {
                pattern,
                type_annotation: Some(type_annotation),
                default: None,
                is_rest: false,
                is_optional: false,
                span: param_start.combine(&param_end),
            });

            if !self.match_token(&[TokenKind::Comma]) {
                break;
            }
        }

        let params = self.alloc_vec(params);
        Ok(params)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Program;
    use crate::diagnostics::CollectingDiagnosticHandler;
    use crate::lexer::Lexer;
    use crate::span::Span;
    use crate::string_interner::StringInterner;
    use std::sync::Arc;

    fn parse_statement(source: &str) -> Result<Statement<'static>, ParserError> {
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
        parser.parse_statement()
    }

    fn parse_program(source: &str) -> Result<Program<'static>, ParserError> {
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
        parser.parse()
    }

    #[test]
    fn test_parse_local_variable_declaration() {
        let result = parse_statement("local x = 42");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Variable(var) => {
                assert!(matches!(var.kind, VariableKind::Local));
            }
            _ => panic!("Expected variable declaration"),
        }
    }

    #[test]
    fn test_parse_const_variable_declaration() {
        let result = parse_statement("const x = 42");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Variable(var) => {
                assert!(matches!(var.kind, VariableKind::Const));
            }
            _ => panic!("Expected const declaration"),
        }
    }

    #[test]
    fn test_parse_variable_with_type_annotation() {
        let result = parse_statement("local x: number = 42");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Variable(var) => {
                assert!(var.type_annotation.is_some());
            }
            _ => panic!("Expected variable declaration with type"),
        }
    }

    #[test]
    fn test_parse_function_declaration() {
        let result = parse_statement("function foo() end");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Function(_) => {}
            _ => panic!("Expected function declaration"),
        }
    }

    #[test]
    fn test_parse_function_with_params() {
        let result = parse_statement("function foo(x: number, y: string): number return x end");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Function(func) => {
                assert_eq!(func.parameters.len(), 2);
                assert!(func.return_type.is_some());
            }
            _ => panic!("Expected function declaration with params"),
        }
    }

    #[test]
    fn test_parse_function_with_braces() {
        let result = parse_statement("function foo() { return 42 }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Function(_) => {}
            _ => panic!("Expected function declaration with braces"),
        }
    }

    #[test]
    fn test_parse_if_statement() {
        let result = parse_statement("if true then end");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::If(_) => {}
            _ => panic!("Expected if statement"),
        }
    }

    #[test]
    fn test_parse_if_else_statement() {
        let result = parse_statement("if true then else end");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::If(if_stmt) => {
                assert!(if_stmt.else_block.is_some());
            }
            _ => panic!("Expected if-else statement"),
        }
    }

    #[test]
    fn test_parse_if_elseif_statement() {
        let result = parse_statement("if true then elseif false then end");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::If(if_stmt) => {
                assert_eq!(if_stmt.else_ifs.len(), 1);
            }
            _ => panic!("Expected if-elseif statement"),
        }
    }

    #[test]
    fn test_parse_while_statement() {
        let result = parse_statement("while true do end");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::While(_) => {}
            _ => panic!("Expected while statement"),
        }
    }

    #[test]
    fn test_parse_repeat_statement() {
        let result = parse_statement("repeat until true");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Repeat(_) => {}
            _ => panic!("Expected repeat statement"),
        }
    }

    #[test]
    fn test_parse_numeric_for() {
        let result = parse_statement("for i = 1, 10 do end");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::For(for_stmt) => match for_stmt {
                ForStatement::Numeric(_) => {}
                _ => panic!("Expected numeric for"),
            },
            _ => panic!("Expected for statement"),
        }
    }

    #[test]
    fn test_parse_numeric_for_with_step() {
        let result = parse_statement("for i = 1, 10, 2 do end");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::For(for_stmt) => match for_stmt {
                ForStatement::Numeric(num) => {
                    assert!(num.step.is_some());
                }
                _ => panic!("Expected numeric for with step"),
            },
            _ => panic!("Expected for statement"),
        }
    }

    #[test]
    fn test_parse_generic_for() {
        let result = parse_statement("for k, v in pairs(t) do end");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::For(for_stmt) => match for_stmt {
                ForStatement::Generic(gen) => {
                    assert_eq!(gen.variables.len(), 2);
                    assert_eq!(gen.iterators.len(), 1);
                }
                _ => panic!("Expected generic for"),
            },
            _ => panic!("Expected for statement"),
        }
    }

    #[test]
    fn test_parse_return_statement() {
        let result = parse_statement("return 42");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Return(ret) => {
                assert_eq!(ret.values.len(), 1);
            }
            _ => panic!("Expected return statement"),
        }
    }

    #[test]
    fn test_parse_return_multiple_values() {
        let result = parse_statement("return 1, 2, 3");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Return(ret) => {
                assert_eq!(ret.values.len(), 3);
            }
            _ => panic!("Expected return statement with multiple values"),
        }
    }

    #[test]
    fn test_parse_break_statement() {
        let result = parse_statement("break");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Break(_) => {}
            _ => panic!("Expected break statement"),
        }
    }

    #[test]
    fn test_parse_continue_statement() {
        let result = parse_statement("continue");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Continue(_) => {}
            _ => panic!("Expected continue statement"),
        }
    }

    #[test]
    fn test_parse_interface_declaration() {
        let result = parse_statement("interface Foo {}");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Interface(_) => {}
            _ => panic!("Expected interface declaration"),
        }
    }

    #[test]
    fn test_parse_interface_with_extends() {
        let result = parse_statement("interface Foo extends Bar, Baz {}");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Interface(iface) => {
                assert_eq!(iface.extends.len(), 2);
            }
            _ => panic!("Expected interface with extends"),
        }
    }

    #[test]
    fn test_parse_interface_with_members() {
        let result = parse_statement("interface Foo { x: number, y: string }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Interface(iface) => {
                assert_eq!(iface.members.len(), 2);
            }
            _ => panic!("Expected interface with members"),
        }
    }

    #[test]
    fn test_parse_type_alias() {
        let result = parse_statement("type MyType = number");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::TypeAlias(_) => {}
            _ => panic!("Expected type alias"),
        }
    }

    #[test]
    fn test_parse_type_alias_with_params() {
        let result = parse_statement("type Container<T> = { value: T }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::TypeAlias(type_alias) => {
                assert!(type_alias.type_parameters.is_some());
            }
            _ => panic!("Expected type alias with params"),
        }
    }

    #[test]
    fn test_parse_simple_enum() {
        let result = parse_statement("enum Color { Red, Green, Blue }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Enum(enm) => {
                assert_eq!(enm.members.len(), 3);
            }
            _ => panic!("Expected enum declaration"),
        }
    }

    #[test]
    fn test_parse_enum_with_values() {
        let result = parse_statement("enum Status { Ok = 200, Error = 500 }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Enum(enm) => {
                assert_eq!(enm.members.len(), 2);
            }
            _ => panic!("Expected enum with values"),
        }
    }

    #[test]
    fn test_parse_import_default() {
        let result = parse_statement("import foo from 'module'");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Import(_) => {}
            _ => panic!("Expected import statement"),
        }
    }

    #[test]
    fn test_parse_import_named() {
        let result = parse_statement("import { foo, bar } from 'module'");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Import(imp) => match imp.clause {
                ImportClause::Named(specs) => assert_eq!(specs.len(), 2),
                _ => panic!("Expected named import"),
            },
            _ => panic!("Expected import statement"),
        }
    }

    #[test]
    fn test_parse_import_namespace() {
        let result = parse_statement("import * as foo from 'module'");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Import(imp) => match imp.clause {
                ImportClause::Namespace(_) => {}
                _ => panic!("Expected namespace import"),
            },
            _ => panic!("Expected import statement"),
        }
    }

    #[test]
    fn test_parse_export_default() {
        let result = parse_statement("export default 42");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Export(_) => {}
            _ => panic!("Expected export statement"),
        }
    }

    #[test]
    fn test_parse_export_named() {
        let result = parse_statement("export { foo, bar }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Export(exp) => match exp.kind {
                ExportKind::Named { specifiers, .. } => assert_eq!(specifiers.len(), 2),
                _ => panic!("Expected named export"),
            },
            _ => panic!("Expected export statement"),
        }
    }

    #[test]
    fn test_parse_class_declaration() {
        let result = parse_statement("class Foo {}");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Class(_) => {}
            _ => panic!("Expected class declaration"),
        }
    }

    #[test]
    fn test_parse_class_with_extends() {
        let result = parse_statement("class Foo extends Bar {}");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Class(cls) => {
                assert!(cls.extends.is_some());
            }
            _ => panic!("Expected class with extends"),
        }
    }

    #[test]
    fn test_parse_class_with_implements() {
        let result = parse_statement("class Foo implements Bar, Baz {}");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Class(cls) => {
                assert_eq!(cls.implements.len(), 2);
            }
            _ => panic!("Expected class with implements"),
        }
    }

    #[test]
    fn test_parse_class_with_members() {
        let result = parse_statement("class Foo { x: number = 0 }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Class(cls) => {
                assert_eq!(cls.members.len(), 1);
            }
            _ => panic!("Expected class with members"),
        }
    }

    #[test]
    fn test_parse_class_with_method() {
        let result = parse_statement("class Foo { method(): number { return 42 } }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Class(cls) => {
                assert_eq!(cls.members.len(), 1);
            }
            _ => panic!("Expected class with method"),
        }
    }

    #[test]
    fn test_parse_class_with_constructor() {
        let result = parse_statement("class Foo { constructor() { } }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Class(cls) => {
                assert_eq!(cls.members.len(), 1);
            }
            _ => panic!("Expected class with constructor"),
        }
    }

    #[test]
    fn test_parse_abstract_class() {
        let result = parse_statement("abstract class Foo {}");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Class(cls) => {
                assert!(cls.is_abstract);
            }
            _ => panic!("Expected abstract class"),
        }
    }

    #[test]
    fn test_parse_final_class() {
        let result = parse_statement("final class Foo {}");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Class(cls) => {
                assert!(cls.is_final);
            }
            _ => panic!("Expected final class"),
        }
    }

    #[test]
    fn test_parse_throw_statement() {
        let result = parse_statement("throw error");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Throw(_) => {}
            _ => panic!("Expected throw statement"),
        }
    }

    #[test]
    fn test_parse_rethrow_statement() {
        let result = parse_statement("rethrow");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Rethrow(_) => {}
            _ => panic!("Expected rethrow statement"),
        }
    }

    #[test]
    fn test_parse_try_statement() {
        let result = parse_statement("try end");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Try(_) => {}
            _ => panic!("Expected try statement"),
        }
    }

    #[test]
    fn test_parse_try_catch() {
        let result = parse_statement("try end catch (e) end");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Try(try_stmt) => {
                assert_eq!(try_stmt.catch_clauses.len(), 1);
            }
            _ => panic!("Expected try-catch statement"),
        }
    }

    #[test]
    fn test_parse_try_catch_finally() {
        let result = parse_statement("try end catch (e) end finally end");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Try(try_stmt) => {
                assert_eq!(try_stmt.catch_clauses.len(), 1);
                assert!(try_stmt.finally_block.is_some());
            }
            _ => panic!("Expected try-catch-finally statement"),
        }
    }

    #[test]
    fn test_parse_namespace_declaration() {
        let result = parse_program("namespace Foo;");
        assert!(result.is_ok());
        let program = result.unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Namespace(_) => {}
            _ => panic!("Expected namespace declaration"),
        }
    }

    #[test]
    fn test_parse_namespace_with_path() {
        let result = parse_program("namespace Foo.Bar.Baz;");
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.statements[0] {
            Statement::Namespace(ns) => {
                assert_eq!(ns.path.len(), 3);
            }
            _ => panic!("Expected namespace with path"),
        }
    }

    #[test]
    fn test_parse_declare_function() {
        let result = parse_statement("declare function foo(): void");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::DeclareFunction(_) => {}
            _ => panic!("Expected declare function"),
        }
    }

    #[test]
    fn test_parse_declare_const() {
        let result = parse_statement("declare const FOO: number");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::DeclareConst(_) => {}
            _ => panic!("Expected declare const"),
        }
    }

    #[test]
    fn test_parse_expression_statement() {
        let result = parse_statement("foo()");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Expression(_) => {}
            _ => panic!("Expected expression statement"),
        }
    }

    #[test]
    fn test_parse_function_with_throws() {
        let result = parse_statement("function foo() throws Error end");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Function(func) => {
                assert!(func.throws.is_some());
            }
            _ => panic!("Expected function with throws"),
        }
    }

    #[test]
    fn test_parse_function_with_type_params() {
        let result = parse_statement("function foo<T>(x: T): T return x end");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Function(func) => {
                assert!(func.type_parameters.is_some());
            }
            _ => panic!("Expected function with type params"),
        }
    }

    #[test]
    fn test_parse_interface_with_type_params() {
        let result = parse_statement("interface Container<T> {}");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Interface(iface) => {
                assert!(iface.type_parameters.is_some());
            }
            _ => panic!("Expected interface with type params"),
        }
    }

    #[test]
    fn test_parse_interface_with_methods() {
        let result = parse_statement("interface Foo { method(): number }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Interface(iface) => {
                assert_eq!(iface.members.len(), 1);
            }
            _ => panic!("Expected interface with methods"),
        }
    }

    #[test]
    fn test_parse_interface_with_index_signature() {
        let result = parse_statement("interface Foo { [key: string]: number }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Interface(iface) => {
                assert_eq!(iface.members.len(), 1);
            }
            _ => panic!("Expected interface with index signature"),
        }
    }

    #[test]
    fn test_parse_class_with_primary_constructor() {
        let result = parse_statement("class Foo(x: number) { }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Class(cls) => {
                assert!(cls.primary_constructor.is_some());
            }
            _ => panic!("Expected class with primary constructor"),
        }
    }

    #[test]
    fn test_parse_class_with_parent_constructor_args() {
        let result = parse_statement("class Foo extends Bar(1, 2) { }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Class(cls) => {
                assert!(cls.extends.is_some());
                assert!(cls.parent_constructor_args.is_some());
            }
            _ => panic!("Expected class with parent constructor args"),
        }
    }

    #[test]
    fn test_parse_class_with_getter() {
        let result = parse_statement("class Foo { get value(): number { return 42 } }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Class(cls) => {
                assert_eq!(cls.members.len(), 1);
            }
            _ => panic!("Expected class with getter"),
        }
    }

    #[test]
    fn test_parse_class_with_setter() {
        let result = parse_statement("class Foo { set value(v: number) { } }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Class(cls) => {
                assert_eq!(cls.members.len(), 1);
            }
            _ => panic!("Expected class with setter"),
        }
    }

    #[test]
    fn test_parse_class_with_operator() {
        let result = parse_statement("class Foo { operator + (other: Foo): Foo { return self } }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Class(cls) => {
                assert_eq!(cls.members.len(), 1);
            }
            _ => panic!("Expected class with operator"),
        }
    }

    #[test]
    fn test_parse_class_with_access_modifiers() {
        // Test each access modifier separately
        let result = parse_statement("class Foo { public x: number }");
        assert!(result.is_ok());

        let result = parse_statement("class Foo { private x: number }");
        assert!(result.is_ok());

        let result = parse_statement("class Foo { protected x: number }");
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_class_with_readonly() {
        let result = parse_statement("class Foo { readonly x: number }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Class(cls) => {
                assert_eq!(cls.members.len(), 1);
            }
            _ => panic!("Expected class with readonly member"),
        }
    }

    #[test]
    fn test_parse_class_with_abstract_method() {
        let result = parse_statement("abstract class Foo { abstract method(): number }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Class(cls) => {
                assert!(cls.is_abstract);
                assert_eq!(cls.members.len(), 1);
            }
            _ => panic!("Expected class with abstract method"),
        }
    }

    #[test]
    fn test_parse_class_with_override() {
        let result = parse_statement("class Foo { override method(): number { return 42 } }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Class(cls) => {
                assert_eq!(cls.members.len(), 1);
            }
            _ => panic!("Expected class with override"),
        }
    }

    #[test]
    fn test_parse_class_with_decorator() {
        let result = parse_statement("class Foo { @decorator method(): void { } }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Class(cls) => {
                assert_eq!(cls.members.len(), 1);
            }
            _ => panic!("Expected class with decorator"),
        }
    }

    #[test]
    fn test_parse_enum_with_constructor() {
        let result = parse_statement("enum Foo { constructor() { } }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Enum(enm) => {
                assert!(enm.constructor.is_some());
            }
            _ => panic!("Expected enum with constructor"),
        }
    }

    #[test]
    fn test_parse_enum_with_method() {
        let result = parse_statement("enum Foo { function method(): number { return 42 } }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Enum(enm) => {
                assert_eq!(enm.methods.len(), 1);
            }
            _ => panic!("Expected enum with method"),
        }
    }

    #[test]
    fn test_parse_enum_with_field() {
        let result = parse_statement("enum Foo { x: number }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Enum(enm) => {
                assert_eq!(enm.fields.len(), 1);
            }
            _ => panic!("Expected enum with field"),
        }
    }

    #[test]
    fn test_parse_enum_implements() {
        let result = parse_statement("enum Foo implements Bar, Baz { }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Enum(enm) => {
                assert_eq!(enm.implements.len(), 2);
            }
            _ => panic!("Expected enum with implements"),
        }
    }

    #[test]
    fn test_parse_import_with_alias() {
        let result = parse_statement("import { foo as bar } from 'module'");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Import(imp) => match imp.clause {
                ImportClause::Named(specs) => {
                    assert_eq!(specs.len(), 1);
                    assert!(specs[0].local.is_some());
                }
                _ => panic!("Expected named import with alias"),
            },
            _ => panic!("Expected import statement"),
        }
    }

    #[test]
    fn test_parse_import_type_only() {
        let result = parse_statement("import type { Foo } from 'module'");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Import(imp) => match imp.clause {
                ImportClause::TypeOnly(_) => {}
                _ => panic!("Expected type-only import"),
            },
            _ => panic!("Expected import statement"),
        }
    }

    #[test]
    fn test_parse_export_declaration() {
        let result = parse_statement("export function foo() end");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Export(exp) => match exp.kind {
                ExportKind::Declaration(_) => {}
                _ => panic!("Expected export declaration"),
            },
            _ => panic!("Expected export statement"),
        }
    }

    #[test]
    fn test_parse_export_with_alias() {
        let result = parse_statement("export { foo as bar }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Export(exp) => match exp.kind {
                ExportKind::Named { specifiers, .. } => {
                    assert_eq!(specifiers.len(), 1);
                    assert!(specifiers[0].exported.is_some());
                }
                _ => panic!("Expected export with alias"),
            },
            _ => panic!("Expected export statement"),
        }
    }

    #[test]
    fn test_parse_export_from() {
        let result = parse_statement("export { foo } from 'module'");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Export(exp) => match exp.kind {
                ExportKind::Named { source, .. } => {
                    assert!(source.is_some());
                }
                _ => panic!("Expected export from"),
            },
            _ => panic!("Expected export statement"),
        }
    }

    #[test]
    fn test_parse_declare_namespace() {
        let result = parse_statement("declare namespace Foo { }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::DeclareNamespace(_) => {}
            _ => panic!("Expected declare namespace"),
        }
    }

    #[test]
    fn test_parse_declare_type() {
        let result = parse_statement("declare type Foo = number");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::DeclareType(_) => {}
            _ => panic!("Expected declare type"),
        }
    }

    #[test]
    fn test_parse_declare_interface() {
        let result = parse_statement("declare interface Foo { }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::DeclareInterface(_) => {}
            _ => panic!("Expected declare interface"),
        }
    }

    #[test]
    fn test_parse_try_catch_with_type() {
        let result = parse_statement("try end catch (e: Error) end");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Try(try_stmt) => {
                assert_eq!(try_stmt.catch_clauses.len(), 1);
            }
            _ => panic!("Expected try-catch with type"),
        }
    }

    #[test]
    fn test_parse_try_catch_with_union_type() {
        let result = parse_statement("try end catch (e: Error | OtherError) end");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Try(try_stmt) => {
                assert_eq!(try_stmt.catch_clauses.len(), 1);
            }
            _ => panic!("Expected try-catch with union type"),
        }
    }

    #[test]
    fn test_parse_multiple_catch_clauses() {
        let result = parse_statement("try end catch (e: Error) end catch (e: OtherError) end");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Try(try_stmt) => {
                assert_eq!(try_stmt.catch_clauses.len(), 2);
            }
            _ => panic!("Expected multiple catch clauses"),
        }
    }

    #[test]
    fn test_parse_try_with_braces() {
        let result = parse_statement("try { } catch (e) { }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Try(_) => {}
            _ => panic!("Expected try with braces"),
        }
    }

    #[test]
    fn test_parse_catch_with_braces() {
        let result = parse_statement("try { } catch (e) { }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Try(_) => {}
            _ => panic!("Expected catch with braces"),
        }
    }

    #[test]
    fn test_parse_complex_program() {
        let source = r#"
            local x: number = 42
            function foo(a: number): number
                return a * 2
            end
            if x > 0 then
                print("positive")
            end
        "#;
        let result = parse_program(source);
        assert!(result.is_ok());
        let program = result.unwrap();
        assert_eq!(program.statements.len(), 3);
    }

    #[test]
    fn test_parse_interface_with_readonly() {
        let result = parse_statement("interface Foo { readonly x: number }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Interface(iface) => {
                assert_eq!(iface.members.len(), 1);
            }
            _ => panic!("Expected interface with readonly"),
        }
    }

    #[test]
    fn test_parse_interface_with_optional() {
        let result = parse_statement("interface Foo { x?: number }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Interface(iface) => {
                assert_eq!(iface.members.len(), 1);
            }
            _ => panic!("Expected interface with optional"),
        }
    }

    #[test]
    fn test_parse_class_with_final_method() {
        let result = parse_statement("class Foo { final method(): void { } }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Class(cls) => {
                assert_eq!(cls.members.len(), 1);
            }
            _ => panic!("Expected class with final method"),
        }
    }

    #[test]
    fn test_parse_enum_member_with_arguments() {
        let result = parse_statement("enum Foo { Bar(1, 2, 3) }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Enum(enm) => {
                assert_eq!(enm.members.len(), 1);
            }
            _ => panic!("Expected enum with member arguments"),
        }
    }

    #[test]
    fn test_parse_empty_return() {
        let result = parse_statement("return");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Return(ret) => {
                assert!(ret.values.is_empty());
            }
            _ => panic!("Expected empty return"),
        }
    }

    #[test]
    fn test_parse_function_with_default_param() {
        let result = parse_statement("function foo(x: number = 42) end");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Function(func) => {
                assert_eq!(func.parameters.len(), 1);
            }
            _ => panic!("Expected function with default param"),
        }
    }

    #[test]
    fn test_parse_function_with_rest_param() {
        let result = parse_statement("function foo(...args) end");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Function(func) => {
                assert_eq!(func.parameters.len(), 1);
            }
            _ => panic!("Expected function with rest param"),
        }
    }

    #[test]
    fn test_parse_function_with_optional_param() {
        let result = parse_statement("function foo(x?: number) end");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Function(func) => {
                assert_eq!(func.parameters.len(), 1);
            }
            _ => panic!("Expected function with optional param"),
        }
    }

    #[test]
    fn test_parse_declare_function_with_throws() {
        let result = parse_statement("declare function foo(): void throws Error");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::DeclareFunction(func) => {
                assert!(func.throws.is_some());
            }
            _ => panic!("Expected declare function with throws"),
        }
    }

    #[test]
    fn test_parse_declare_function_exported() {
        let result = parse_statement("declare namespace Foo { export function bar(): void }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::DeclareNamespace(ns) => {
                assert_eq!(ns.members.len(), 1);
            }
            _ => panic!("Expected declare namespace with exported function"),
        }
    }

    #[test]
    fn test_parse_class_decorator() {
        let result = parse_statement("@decorator class Foo { }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Class(cls) => {
                assert_eq!(cls.decorators.len(), 1);
            }
            _ => panic!("Expected class with decorator"),
        }
    }

    #[test]
    fn test_parse_decorator_with_args() {
        let result = parse_statement("@decorator(1, 2) class Foo { }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Class(cls) => {
                assert_eq!(cls.decorators.len(), 1);
            }
            _ => panic!("Expected class with decorated args"),
        }
    }

    #[test]
    fn test_parse_decorator_chain() {
        let result = parse_statement("@foo @bar class Foo { }");
        assert!(result.is_ok());
        match result.unwrap() {
            Statement::Class(cls) => {
                assert_eq!(cls.decorators.len(), 2);
            }
            _ => panic!("Expected class with decorator chain"),
        }
    }

    #[test]
    fn test_parse_generic_class() {
        let result = parse_statement("class Box<T> {}");
        assert!(
            result.is_ok(),
            "Generic class should parse: {:?}",
            result.err()
        );
        match result.unwrap() {
            Statement::Class(cls) => {
                assert!(cls.type_parameters.is_some());
                assert_eq!(cls.type_parameters.unwrap().len(), 1);
            }
            other => panic!("Expected class, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_generic_class_with_field() {
        let result = parse_statement("class Box<T> { private value: T }");
        assert!(
            result.is_ok(),
            "Generic class with field should parse: {:?}",
            result.err()
        );
        match result.unwrap() {
            Statement::Class(cls) => {
                assert!(cls.type_parameters.is_some());
                assert_eq!(cls.members.len(), 1);
            }
            other => panic!("Expected class, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_generic_class_with_method() {
        let result = parse_statement("class Box<T> { getValue(): T { return self.value } }");
        assert!(
            result.is_ok(),
            "Generic class with method should parse: {:?}",
            result.err()
        );
        match result.unwrap() {
            Statement::Class(cls) => {
                assert!(cls.type_parameters.is_some());
                assert_eq!(cls.members.len(), 1);
            }
            other => panic!("Expected class, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_get_as_method_name() {
        // "get" followed by "(" should be a method named "get", not a getter
        let result = parse_statement("class Foo { get(): number { return 0 } }");
        assert!(
            result.is_ok(),
            "get() as method name should parse: {:?}",
            result.err()
        );
        match result.unwrap() {
            Statement::Class(cls) => {
                assert_eq!(cls.members.len(), 1);
                match &cls.members[0] {
                    ClassMember::Method(_) => {}
                    other => panic!("Expected method, got {:?}", other),
                }
            }
            other => panic!("Expected class, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_get_as_getter() {
        // "get" followed by identifier should still be a getter
        let result = parse_statement("class Foo { get value(): number { return 0 } }");
        assert!(
            result.is_ok(),
            "get as getter should parse: {:?}",
            result.err()
        );
        match result.unwrap() {
            Statement::Class(cls) => {
                assert_eq!(cls.members.len(), 1);
                match &cls.members[0] {
                    ClassMember::Getter(_) => {}
                    other => panic!("Expected getter, got {:?}", other),
                }
            }
            other => panic!("Expected class, got {:?}", other),
        }
    }
}
