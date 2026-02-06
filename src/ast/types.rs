use super::{
    expression::Expression,
    expression::Literal,
    statement::{IndexSignature, MethodSignature, Parameter, PropertySignature, TypeParameter},
    Ident,
};
use crate::span::Span;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct Type<'arena> {
    #[serde(borrow)]
    pub kind: TypeKind<'arena>,
    pub span: Span,
}

impl<'arena> Type<'arena> {
    pub fn new(kind: TypeKind<'arena>, span: Span) -> Self {
        Type { kind, span }
    }
}

#[derive(Debug, Clone, Serialize)]
pub enum TypeKind<'arena> {
    Primitive(PrimitiveType),
    #[serde(borrow)]
    Reference(TypeReference<'arena>),
    #[serde(borrow)]
    Union(&'arena [Type<'arena>]),
    #[serde(borrow)]
    Intersection(&'arena [Type<'arena>]),
    #[serde(borrow)]
    Object(ObjectType<'arena>),
    #[serde(borrow)]
    Array(&'arena Type<'arena>),
    #[serde(borrow)]
    Tuple(&'arena [Type<'arena>]),
    #[serde(borrow)]
    Function(FunctionType<'arena>),
    Literal(Literal),
    #[serde(borrow)]
    TypeQuery(&'arena Expression<'arena>),
    #[serde(borrow)]
    KeyOf(&'arena Type<'arena>),
    IndexAccess(#[serde(borrow)] &'arena Type<'arena>, #[serde(borrow)] &'arena Type<'arena>),
    #[serde(borrow)]
    Conditional(ConditionalType<'arena>),
    #[serde(borrow)]
    Mapped(MappedType<'arena>),
    #[serde(borrow)]
    TemplateLiteral(TemplateLiteralType<'arena>),
    #[serde(borrow)]
    Nullable(&'arena Type<'arena>),
    #[serde(borrow)]
    Parenthesized(&'arena Type<'arena>),
    Infer(Ident),                 // infer R - captures type in conditional
    #[serde(borrow)]
    TypePredicate(TypePredicate<'arena>), // x is T - type guard predicate
    #[serde(borrow)]
    Variadic(&'arena Type<'arena>),          // ...T[] - variadic return type
    Namespace(Vec<String>),       // File-based namespace type
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
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
    Thread,
}

#[derive(Debug, Clone, Serialize)]
pub struct TypeReference<'arena> {
    pub name: Ident,
    #[serde(borrow)]
    pub type_arguments: Option<&'arena [Type<'arena>]>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct ObjectType<'arena> {
    #[serde(borrow)]
    pub members: &'arena [ObjectTypeMember<'arena>],
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub enum ObjectTypeMember<'arena> {
    #[serde(borrow)]
    Property(PropertySignature<'arena>),
    #[serde(borrow)]
    Method(MethodSignature<'arena>),
    #[serde(borrow)]
    Index(IndexSignature<'arena>),
}

#[derive(Debug, Clone, Serialize)]
pub struct FunctionType<'arena> {
    #[serde(borrow)]
    pub type_parameters: Option<&'arena [TypeParameter<'arena>]>,
    #[serde(borrow)]
    pub parameters: &'arena [Parameter<'arena>],
    #[serde(borrow)]
    pub return_type: &'arena Type<'arena>,
    #[serde(borrow)]
    pub throws: Option<&'arena [Type<'arena>]>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct ConditionalType<'arena> {
    #[serde(borrow)]
    pub check_type: &'arena Type<'arena>,
    #[serde(borrow)]
    pub extends_type: &'arena Type<'arena>,
    #[serde(borrow)]
    pub true_type: &'arena Type<'arena>,
    #[serde(borrow)]
    pub false_type: &'arena Type<'arena>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum MappedTypeModifier {
    Add,    // +readonly, +? (or just readonly, ?)
    Remove, // -readonly, -?
    None,   // no modifier
}

#[derive(Debug, Clone, Serialize)]
pub struct MappedType<'arena> {
    pub readonly_modifier: MappedTypeModifier,
    #[serde(borrow)]
    pub type_parameter: &'arena TypeParameter<'arena>,
    #[serde(borrow)]
    pub in_type: &'arena Type<'arena>,
    pub optional_modifier: MappedTypeModifier,
    #[serde(borrow)]
    pub value_type: &'arena Type<'arena>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct TemplateLiteralType<'arena> {
    #[serde(borrow)]
    pub parts: &'arena [TemplateLiteralTypePart<'arena>],
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub enum TemplateLiteralTypePart<'arena> {
    String(String),
    #[serde(borrow)]
    Type(Type<'arena>),
}

#[derive(Debug, Clone, Serialize)]
pub struct TypePredicate<'arena> {
    pub parameter_name: Ident,
    #[serde(borrow)]
    pub type_annotation: &'arena Type<'arena>,
    pub span: Span,
}
