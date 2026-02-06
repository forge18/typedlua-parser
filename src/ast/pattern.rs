use super::{expression::Expression, expression::Literal, Ident};
use crate::span::Span;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Pattern {
    Identifier(Ident),
    Literal(Literal, Span),
    Array(ArrayPattern),
    Object(ObjectPattern),
    Wildcard(Span),
    Or(OrPattern),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OrPattern {
    pub alternatives: Vec<Pattern>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ArrayPattern {
    pub elements: Vec<ArrayPatternElement>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ArrayPatternElement {
    Pattern(Pattern),
    Rest(Ident),
    Hole,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ObjectPattern {
    pub properties: Vec<ObjectPatternProperty>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ObjectPatternProperty {
    pub key: Ident,
    pub value: Option<Pattern>,
    pub default: Option<Expression>,
    pub span: Span,
}

impl Pattern {
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
