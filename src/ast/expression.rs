use super::{pattern::Pattern, statement::TypeParameter, types::Type, Ident};
use crate::span::Span;
use crate::string_interner::StringId;
use serde::{Deserialize, Serialize};

use super::statement::{Block, Parameter};

#[derive(Debug, Clone, Default, Serialize)]
pub struct Expression<'arena> {
    pub kind: ExpressionKind<'arena>,
    pub span: Span,
    #[serde(borrow)]
    pub annotated_type: Option<Type<'arena>>,
    pub receiver_class: Option<ReceiverClassInfo>,
}

#[derive(Debug, Clone, Serialize)]
pub struct ReceiverClassInfo {
    pub class_name: StringId,
    pub is_static: bool,
}

impl<'arena> Expression<'arena> {
    pub fn new(kind: ExpressionKind<'arena>, span: Span) -> Self {
        Expression {
            kind,
            span,
            annotated_type: None,
            receiver_class: None,
        }
    }
}

#[derive(Debug, Clone, Default, Serialize)]
pub enum ExpressionKind<'arena> {
    Identifier(StringId),
    Literal(Literal),
    Binary(BinaryOp, &'arena Expression<'arena>, &'arena Expression<'arena>),
    Unary(UnaryOp, &'arena Expression<'arena>),
    Assignment(&'arena Expression<'arena>, AssignmentOp, &'arena Expression<'arena>),
    Member(&'arena Expression<'arena>, Ident),
    Index(&'arena Expression<'arena>, &'arena Expression<'arena>),
    Call(&'arena Expression<'arena>, &'arena [Argument<'arena>], Option<&'arena [Type<'arena>]>),
    MethodCall(&'arena Expression<'arena>, Ident, &'arena [Argument<'arena>], Option<&'arena [Type<'arena>]>),
    Array(&'arena [ArrayElement<'arena>]),
    Object(&'arena [ObjectProperty<'arena>]),
    Function(FunctionExpression<'arena>),
    Arrow(ArrowFunction<'arena>),
    Conditional(&'arena Expression<'arena>, &'arena Expression<'arena>, &'arena Expression<'arena>),
    Pipe(&'arena Expression<'arena>, &'arena Expression<'arena>),
    Match(MatchExpression<'arena>),
    Parenthesized(&'arena Expression<'arena>),
    #[default]
    SelfKeyword,
    SuperKeyword,
    Template(TemplateLiteral<'arena>),
    TypeAssertion(&'arena Expression<'arena>, Type<'arena>),
    New(&'arena Expression<'arena>, &'arena [Argument<'arena>], Option<&'arena [Type<'arena>]>),
    OptionalMember(&'arena Expression<'arena>, Ident),
    OptionalIndex(&'arena Expression<'arena>, &'arena Expression<'arena>),
    OptionalCall(&'arena Expression<'arena>, &'arena [Argument<'arena>], Option<&'arena [Type<'arena>]>),
    OptionalMethodCall(&'arena Expression<'arena>, Ident, &'arena [Argument<'arena>], Option<&'arena [Type<'arena>]>),
    Try(TryExpression<'arena>),
    ErrorChain(&'arena Expression<'arena>, &'arena Expression<'arena>),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Literal {
    Nil,
    Boolean(bool),
    Number(f64),
    Integer(i64),
    String(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    IntegerDivide,
    Power,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    And,
    Or,
    NullCoalesce,
    Concatenate,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,
    Instanceof,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum UnaryOp {
    Not,
    Negate,
    Length,
    BitwiseNot,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum AssignmentOp {
    Assign,            // =
    AddAssign,         // +=
    SubtractAssign,    // -=
    MultiplyAssign,    // *=
    DivideAssign,      // /=
    ModuloAssign,      // %=
    PowerAssign,       // ^=
    ConcatenateAssign, // ..=
    BitwiseAndAssign,  // &=
    BitwiseOrAssign,   // |=
    FloorDivideAssign, // //=
    LeftShiftAssign,   // <<=
    RightShiftAssign,  // >>=
}

#[derive(Debug, Clone, Serialize)]
pub struct Argument<'arena> {
    #[serde(borrow)]
    pub value: Expression<'arena>,
    pub is_spread: bool,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub enum ArrayElement<'arena> {
    #[serde(borrow)]
    Expression(Expression<'arena>),
    #[serde(borrow)]
    Spread(Expression<'arena>),
}

#[derive(Debug, Clone, Serialize)]
pub enum ObjectProperty<'arena> {
    Property {
        key: Ident,
        #[serde(borrow)]
        value: &'arena Expression<'arena>,
        span: Span,
    },
    Computed {
        #[serde(borrow)]
        key: &'arena Expression<'arena>,
        #[serde(borrow)]
        value: &'arena Expression<'arena>,
        span: Span,
    },
    Spread {
        #[serde(borrow)]
        value: &'arena Expression<'arena>,
        span: Span,
    },
}

#[derive(Debug, Clone, Serialize)]
pub struct FunctionExpression<'arena> {
    #[serde(borrow)]
    pub type_parameters: Option<&'arena [TypeParameter<'arena>]>,
    #[serde(borrow)]
    pub parameters: &'arena [Parameter<'arena>],
    #[serde(borrow)]
    pub return_type: Option<Type<'arena>>,
    #[serde(borrow)]
    pub body: Block<'arena>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct ArrowFunction<'arena> {
    #[serde(borrow)]
    pub parameters: &'arena [Parameter<'arena>],
    #[serde(borrow)]
    pub return_type: Option<Type<'arena>>,
    #[serde(borrow)]
    pub body: ArrowBody<'arena>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub enum ArrowBody<'arena> {
    #[serde(borrow)]
    Expression(&'arena Expression<'arena>),
    #[serde(borrow)]
    Block(Block<'arena>),
}

#[derive(Debug, Clone, Serialize)]
pub struct MatchExpression<'arena> {
    #[serde(borrow)]
    pub value: &'arena Expression<'arena>,
    #[serde(borrow)]
    pub arms: &'arena [MatchArm<'arena>],
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct MatchArm<'arena> {
    #[serde(borrow)]
    pub pattern: Pattern<'arena>,
    #[serde(borrow)]
    pub guard: Option<Expression<'arena>>,
    #[serde(borrow)]
    pub body: MatchArmBody<'arena>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub enum MatchArmBody<'arena> {
    #[serde(borrow)]
    Expression(&'arena Expression<'arena>),
    #[serde(borrow)]
    Block(Block<'arena>),
}

#[derive(Debug, Clone, Serialize)]
pub struct TemplateLiteral<'arena> {
    #[serde(borrow)]
    pub parts: &'arena [TemplatePart<'arena>],
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub enum TemplatePart<'arena> {
    String(String),
    #[serde(borrow)]
    Expression(&'arena Expression<'arena>),
}

#[derive(Debug, Clone, Serialize)]
pub struct TryExpression<'arena> {
    #[serde(borrow)]
    pub expression: &'arena Expression<'arena>,
    pub catch_variable: Ident,
    #[serde(borrow)]
    pub catch_expression: &'arena Expression<'arena>,
    pub span: Span,
}
