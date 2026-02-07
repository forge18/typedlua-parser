use super::{expression::Expression, expression::Literal, Ident};
use crate::span::Span;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub enum Pattern<'arena> {
    Identifier(Ident),
    Literal(Literal, Span),
    #[serde(borrow)]
    Array(ArrayPattern<'arena>),
    #[serde(borrow)]
    Object(ObjectPattern<'arena>),
    Wildcard(Span),
    #[serde(borrow)]
    Or(OrPattern<'arena>),
}

#[derive(Debug, Clone, Serialize)]
pub struct OrPattern<'arena> {
    #[serde(borrow)]
    pub alternatives: &'arena [Pattern<'arena>],
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct ArrayPattern<'arena> {
    #[serde(borrow)]
    pub elements: &'arena [ArrayPatternElement<'arena>],
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub enum ArrayPatternElement<'arena> {
    #[serde(borrow)]
    Pattern(PatternWithDefault<'arena>),
    Rest(Ident),
    Hole,
}

#[derive(Debug, Clone, Serialize)]
pub struct PatternWithDefault<'arena> {
    #[serde(borrow)]
    pub pattern: Pattern<'arena>,
    #[serde(borrow)]
    pub default: Option<Expression<'arena>>,
}

#[derive(Debug, Clone, Serialize)]
pub struct ObjectPattern<'arena> {
    #[serde(borrow)]
    pub properties: &'arena [ObjectPatternProperty<'arena>],
    pub rest: Option<Ident>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct ObjectPatternProperty<'arena> {
    pub key: Ident,
    #[serde(borrow)]
    pub computed_key: Option<Expression<'arena>>,
    #[serde(borrow)]
    pub value: Option<Pattern<'arena>>,
    #[serde(borrow)]
    pub default: Option<Expression<'arena>>,
    pub span: Span,
}

impl<'arena> Pattern<'arena> {
    pub fn span(&self) -> Span {
        match self {
            Pattern::Identifier(id) => id.span,
            Pattern::Literal(_, span) => *span,
            Pattern::Array(arr) => arr.span,
            Pattern::Object(obj) => obj.span,
            Pattern::Wildcard(span) => *span,
            Pattern::Or(or) => or.span,
        }
    }

    pub fn node_id(&self) -> Option<super::StringId> {
        match self {
            Pattern::Identifier(id) => Some(id.node),
            _ => None,
        }
    }
}
