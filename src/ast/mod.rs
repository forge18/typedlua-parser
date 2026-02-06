pub mod expression;
pub mod pattern;
pub mod statement;
pub mod types;

use crate::span::Span;
use crate::string_interner::StringId;
use serde::Serialize;

/// Wrapper for AST nodes with span information
#[derive(Debug, Clone, Serialize)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Spanned { node, span }
    }
}

/// Identifier
pub type Ident = Spanned<StringId>;

/// Top-level program
#[derive(Debug, Clone, Serialize)]
pub struct Program<'arena> {
    #[serde(borrow)]
    pub statements: &'arena [statement::Statement<'arena>],
    pub span: Span,
}

impl<'arena> Program<'arena> {
    pub fn new(statements: &'arena [statement::Statement<'arena>], span: Span) -> Self {
        Program { statements, span }
    }
}
