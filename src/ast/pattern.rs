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
