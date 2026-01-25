use super::{
    expression::Expression,
    expression::Literal,
    statement::{IndexSignature, MethodSignature, Parameter, PropertySignature, TypeParameter},
    Ident,
};
use crate::span::Span;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span,
}

impl Type {
    pub fn new(kind: TypeKind, span: Span) -> Self {
        Type { kind, span }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TypeKind {
    Primitive(PrimitiveType),
    Reference(TypeReference),
    Union(Vec<Type>),
    Intersection(Vec<Type>),
    Object(ObjectType),
    Array(Box<Type>),
    Tuple(Vec<Type>),
    Function(FunctionType),
    Literal(Literal),
    TypeQuery(Box<Expression>),
    KeyOf(Box<Type>),
    IndexAccess(Box<Type>, Box<Type>),
    Conditional(ConditionalType),
    Mapped(MappedType),
    TemplateLiteral(TemplateLiteralType),
    Nullable(Box<Type>),
    Parenthesized(Box<Type>),
    Infer(Ident),                 // infer R - captures type in conditional
    TypePredicate(TypePredicate), // x is T - type guard predicate
    Variadic(Box<Type>),          // ...T[] - variadic return type
    Namespace(Vec<String>),       // File-based namespace type
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum PrimitiveType {
    Nil,
    Boolean,
    Number,
    Integer,
    String,
    Unknown,
    Never,
    Void,
    Table,
    Coroutine,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeReference {
    pub name: Ident,
    pub type_arguments: Option<Vec<Type>>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ObjectType {
    pub members: Vec<ObjectTypeMember>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ObjectTypeMember {
    Property(PropertySignature),
    Method(MethodSignature),
    Index(IndexSignature),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionType {
    pub type_parameters: Option<Vec<TypeParameter>>,
    pub parameters: Vec<Parameter>,
    pub return_type: Box<Type>,
    pub throws: Option<Vec<Type>>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConditionalType {
    pub check_type: Box<Type>,
    pub extends_type: Box<Type>,
    pub true_type: Box<Type>,
    pub false_type: Box<Type>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MappedType {
    pub is_readonly: bool,
    pub type_parameter: Box<TypeParameter>,
    pub in_type: Box<Type>,
    pub is_optional: bool,
    pub value_type: Box<Type>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateLiteralType {
    pub parts: Vec<TemplateLiteralTypePart>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TemplateLiteralTypePart {
    String(String),
    Type(Type),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypePredicate {
    pub parameter_name: Ident,
    pub type_annotation: Box<Type>,
    pub span: Span,
}
