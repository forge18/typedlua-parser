use super::{pattern::Pattern, statement::TypeParameter, types::Type, Ident};
use crate::span::Span;
use crate::string_interner::StringId;
use serde::{Deserialize, Serialize};

use super::statement::{Block, Parameter};

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
    pub annotated_type: Option<Type>,
    pub receiver_class: Option<ReceiverClassInfo>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReceiverClassInfo {
    pub class_name: StringId,
    pub is_static: bool,
}

impl Expression {
    pub fn new(kind: ExpressionKind, span: Span) -> Self {
        Expression {
            kind,
            span,
            annotated_type: None,
            receiver_class: None,
        }
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub enum ExpressionKind {
    Identifier(StringId),
    Literal(Literal),
    Binary(BinaryOp, Box<Expression>, Box<Expression>),
    Unary(UnaryOp, Box<Expression>),
    Assignment(Box<Expression>, AssignmentOp, Box<Expression>),
    Member(Box<Expression>, Ident),
    Index(Box<Expression>, Box<Expression>),
    Call(Box<Expression>, Vec<Argument>, Option<Vec<Type>>),
    MethodCall(Box<Expression>, Ident, Vec<Argument>, Option<Vec<Type>>),
    Array(Vec<ArrayElement>),
    Object(Vec<ObjectProperty>),
    Function(FunctionExpression),
    Arrow(ArrowFunction),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
    Pipe(Box<Expression>, Box<Expression>),
    Match(MatchExpression),
    Parenthesized(Box<Expression>),
    #[default]
    SelfKeyword,
    SuperKeyword,
    Template(TemplateLiteral),
    TypeAssertion(Box<Expression>, Type),
    New(Box<Expression>, Vec<Argument>),
    OptionalMember(Box<Expression>, Ident),
    OptionalIndex(Box<Expression>, Box<Expression>),
    OptionalCall(Box<Expression>, Vec<Argument>, Option<Vec<Type>>),
    OptionalMethodCall(Box<Expression>, Ident, Vec<Argument>, Option<Vec<Type>>),
    Try(TryExpression),
    ErrorChain(Box<Expression>, Box<Expression>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Literal {
    Nil,
    Boolean(bool),
    Number(f64),
    Integer(i64),
    String(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnaryOp {
    Not,
    Negate,
    Length,
    BitwiseNot,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Argument {
    pub value: Expression,
    pub is_spread: bool,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ArrayElement {
    Expression(Expression),
    Spread(Expression),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ObjectProperty {
    Property {
        key: Ident,
        value: Box<Expression>,
        span: Span,
    },
    Computed {
        key: Box<Expression>,
        value: Box<Expression>,
        span: Span,
    },
    Spread {
        value: Box<Expression>,
        span: Span,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionExpression {
    pub type_parameters: Option<Vec<TypeParameter>>,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ArrowFunction {
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub body: ArrowBody,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ArrowBody {
    Expression(Box<Expression>),
    Block(Block),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MatchExpression {
    pub value: Box<Expression>,
    pub arms: Vec<MatchArm>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Expression>,
    pub body: MatchArmBody,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum MatchArmBody {
    Expression(Box<Expression>),
    Block(Block),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateLiteral {
    pub parts: Vec<TemplatePart>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TemplatePart {
    String(String),
    Expression(Box<Expression>),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TryExpression {
    pub expression: Box<Expression>,
    pub catch_variable: Ident,
    pub catch_expression: Box<Expression>,
    pub span: Span,
}
