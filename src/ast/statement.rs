use super::{expression::Expression, pattern::Pattern, types::Type, Ident};
use crate::span::Span;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize)]
pub enum Statement<'arena> {
    #[serde(borrow)]
    Variable(VariableDeclaration<'arena>),
    #[serde(borrow)]
    Function(FunctionDeclaration<'arena>),
    #[serde(borrow)]
    Class(ClassDeclaration<'arena>),
    #[serde(borrow)]
    Interface(InterfaceDeclaration<'arena>),
    #[serde(borrow)]
    TypeAlias(TypeAliasDeclaration<'arena>),
    #[serde(borrow)]
    Enum(EnumDeclaration<'arena>),
    #[serde(borrow)]
    Import(ImportDeclaration<'arena>),
    #[serde(borrow)]
    Export(ExportDeclaration<'arena>),
    #[serde(borrow)]
    If(IfStatement<'arena>),
    #[serde(borrow)]
    While(WhileStatement<'arena>),
    #[serde(borrow)]
    For(&'arena ForStatement<'arena>),
    #[serde(borrow)]
    Repeat(RepeatStatement<'arena>),
    #[serde(borrow)]
    Return(ReturnStatement<'arena>),
    Break(Span),
    Continue(Span),
    Label(LabelStatement),
    Goto(GotoStatement),
    #[serde(borrow)]
    Expression(Expression<'arena>),
    #[serde(borrow)]
    Block(Block<'arena>),
    // Exception handling statements
    #[serde(borrow)]
    Throw(ThrowStatement<'arena>),
    #[serde(borrow)]
    Try(TryStatement<'arena>),
    Rethrow(Span),

    // File-based namespace declaration
    Namespace(NamespaceDeclaration),

    // Declaration file statements
    #[serde(borrow)]
    DeclareFunction(DeclareFunctionStatement<'arena>),
    #[serde(borrow)]
    DeclareNamespace(DeclareNamespaceStatement<'arena>),
    #[serde(borrow)]
    DeclareType(TypeAliasDeclaration<'arena>),      // Same as TypeAlias
    #[serde(borrow)]
    DeclareInterface(InterfaceDeclaration<'arena>), // Same as Interface
    #[serde(borrow)]
    DeclareConst(DeclareConstStatement<'arena>),
}

#[derive(Debug, Clone, Serialize)]
pub struct VariableDeclaration<'arena> {
    pub kind: VariableKind,
    #[serde(borrow)]
    pub pattern: Pattern<'arena>,
    #[serde(borrow)]
    pub type_annotation: Option<Type<'arena>>,
    #[serde(borrow)]
    pub initializer: Expression<'arena>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum VariableKind {
    Const,
    Local,
}

#[derive(Debug, Clone, Serialize)]
pub struct FunctionDeclaration<'arena> {
    pub name: Ident,
    #[serde(borrow)]
    pub type_parameters: Option<&'arena [TypeParameter<'arena>]>,
    #[serde(borrow)]
    pub parameters: &'arena [Parameter<'arena>],
    #[serde(borrow)]
    pub return_type: Option<Type<'arena>>,
    #[serde(borrow)]
    pub throws: Option<&'arena [Type<'arena>]>,
    #[serde(borrow)]
    pub body: Block<'arena>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct ClassDeclaration<'arena> {
    #[serde(borrow)]
    pub decorators: &'arena [Decorator<'arena>],
    pub is_abstract: bool,
    pub is_final: bool,
    pub name: Ident,
    #[serde(borrow)]
    pub type_parameters: Option<&'arena [TypeParameter<'arena>]>,
    #[serde(borrow)]
    pub primary_constructor: Option<&'arena [ConstructorParameter<'arena>]>,
    #[serde(borrow)]
    pub extends: Option<Type<'arena>>,
    #[serde(borrow)]
    pub parent_constructor_args: Option<&'arena [Expression<'arena>]>,
    #[serde(borrow)]
    pub implements: &'arena [Type<'arena>],
    #[serde(borrow)]
    pub members: &'arena [ClassMember<'arena>],
    /// True if this is a forward declaration (no members, used for mutual references)
    #[serde(default)]
    pub is_forward_declaration: bool,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub enum ClassMember<'arena> {
    #[serde(borrow)]
    Property(PropertyDeclaration<'arena>),
    #[serde(borrow)]
    Constructor(ConstructorDeclaration<'arena>),
    #[serde(borrow)]
    Method(MethodDeclaration<'arena>),
    #[serde(borrow)]
    Getter(GetterDeclaration<'arena>),
    #[serde(borrow)]
    Setter(SetterDeclaration<'arena>),
    #[serde(borrow)]
    Operator(OperatorDeclaration<'arena>),
}

#[derive(Debug, Clone, Serialize)]
pub struct PropertyDeclaration<'arena> {
    #[serde(borrow)]
    pub decorators: &'arena [Decorator<'arena>],
    pub access: Option<AccessModifier>,
    pub is_static: bool,
    pub is_readonly: bool,
    pub name: Ident,
    #[serde(borrow)]
    pub type_annotation: Type<'arena>,
    #[serde(borrow)]
    pub initializer: Option<Expression<'arena>>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct ConstructorDeclaration<'arena> {
    #[serde(borrow)]
    pub decorators: &'arena [Decorator<'arena>],
    #[serde(borrow)]
    pub parameters: &'arena [Parameter<'arena>],
    #[serde(borrow)]
    pub body: Block<'arena>,
    pub span: Span,
}

/// Represents a parameter in a primary constructor (compact class syntax)
/// Example: `class Point(public x: number, private readonly y: number)`
#[derive(Debug, Clone, Serialize)]
pub struct ConstructorParameter<'arena> {
    #[serde(borrow)]
    pub decorators: &'arena [Decorator<'arena>],
    pub access: Option<AccessModifier>,
    pub is_readonly: bool,
    pub name: Ident,
    #[serde(borrow)]
    pub type_annotation: Type<'arena>,
    #[serde(borrow)]
    pub default: Option<Expression<'arena>>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct MethodDeclaration<'arena> {
    #[serde(borrow)]
    pub decorators: &'arena [Decorator<'arena>],
    pub access: Option<AccessModifier>,
    pub is_static: bool,
    pub is_abstract: bool,
    pub is_final: bool,
    pub is_override: bool,
    pub name: Ident,
    #[serde(borrow)]
    pub type_parameters: Option<&'arena [TypeParameter<'arena>]>,
    #[serde(borrow)]
    pub parameters: &'arena [Parameter<'arena>],
    #[serde(borrow)]
    pub return_type: Option<Type<'arena>>,
    #[serde(borrow)]
    pub body: Option<Block<'arena>>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct GetterDeclaration<'arena> {
    #[serde(borrow)]
    pub decorators: &'arena [Decorator<'arena>],
    pub access: Option<AccessModifier>,
    pub is_static: bool,
    pub name: Ident,
    #[serde(borrow)]
    pub return_type: Type<'arena>,
    #[serde(borrow)]
    pub body: Block<'arena>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct SetterDeclaration<'arena> {
    #[serde(borrow)]
    pub decorators: &'arena [Decorator<'arena>],
    pub access: Option<AccessModifier>,
    pub is_static: bool,
    pub name: Ident,
    #[serde(borrow)]
    pub parameter: Parameter<'arena>,
    #[serde(borrow)]
    pub body: Block<'arena>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub enum OperatorKind {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Power,
    Concatenate,
    FloorDivide,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,
    Index,
    NewIndex,
    Call,
    UnaryMinus,
    Length,
}

#[derive(Debug, Clone, Serialize)]
pub struct OperatorDeclaration<'arena> {
    #[serde(borrow)]
    pub decorators: &'arena [Decorator<'arena>],
    pub access: Option<AccessModifier>,
    pub operator: OperatorKind,
    #[serde(borrow)]
    pub parameters: &'arena [Parameter<'arena>],
    #[serde(borrow)]
    pub return_type: Option<Type<'arena>>,
    #[serde(borrow)]
    pub body: Block<'arena>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum AccessModifier {
    Public,
    Private,
    Protected,
}

#[derive(Debug, Clone, Serialize)]
pub struct InterfaceDeclaration<'arena> {
    pub name: Ident,
    #[serde(borrow)]
    pub type_parameters: Option<&'arena [TypeParameter<'arena>]>,
    #[serde(borrow)]
    pub extends: &'arena [Type<'arena>],
    #[serde(borrow)]
    pub members: &'arena [InterfaceMember<'arena>],
    /// True if this is a forward declaration (no members, used for mutual references)
    #[serde(default)]
    pub is_forward_declaration: bool,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub enum InterfaceMember<'arena> {
    #[serde(borrow)]
    Property(PropertySignature<'arena>),
    #[serde(borrow)]
    Method(MethodSignature<'arena>),
    #[serde(borrow)]
    Index(IndexSignature<'arena>),
}

#[derive(Debug, Clone, Serialize)]
pub struct PropertySignature<'arena> {
    pub is_readonly: bool,
    pub name: Ident,
    pub is_optional: bool,
    #[serde(borrow)]
    pub type_annotation: Type<'arena>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct MethodSignature<'arena> {
    pub name: Ident,
    #[serde(borrow)]
    pub type_parameters: Option<&'arena [TypeParameter<'arena>]>,
    #[serde(borrow)]
    pub parameters: &'arena [Parameter<'arena>],
    #[serde(borrow)]
    pub return_type: Type<'arena>,
    #[serde(borrow)]
    pub body: Option<Block<'arena>>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct IndexSignature<'arena> {
    pub key_name: Ident,
    pub key_type: IndexKeyType,
    #[serde(borrow)]
    pub value_type: Type<'arena>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum IndexKeyType {
    String,
    Number,
}

#[derive(Debug, Clone, Serialize)]
pub struct TypeAliasDeclaration<'arena> {
    pub name: Ident,
    #[serde(borrow)]
    pub type_parameters: Option<&'arena [TypeParameter<'arena>]>,
    #[serde(borrow)]
    pub type_annotation: Type<'arena>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct EnumDeclaration<'arena> {
    pub name: Ident,
    #[serde(borrow)]
    pub members: &'arena [EnumMember<'arena>],
    #[serde(borrow)]
    pub fields: &'arena [EnumField<'arena>],
    #[serde(borrow)]
    pub constructor: Option<EnumConstructor<'arena>>,
    #[serde(borrow)]
    pub methods: &'arena [EnumMethod<'arena>],
    #[serde(borrow)]
    pub implements: &'arena [Type<'arena>],
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct EnumField<'arena> {
    pub name: Ident,
    #[serde(borrow)]
    pub type_annotation: Type<'arena>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct EnumConstructor<'arena> {
    #[serde(borrow)]
    pub parameters: &'arena [Parameter<'arena>],
    #[serde(borrow)]
    pub body: Block<'arena>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct EnumMethod<'arena> {
    pub name: Ident,
    #[serde(borrow)]
    pub parameters: &'arena [Parameter<'arena>],
    #[serde(borrow)]
    pub return_type: Option<Type<'arena>>,
    #[serde(borrow)]
    pub body: Block<'arena>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct EnumMember<'arena> {
    pub name: Ident,
    #[serde(borrow)]
    pub arguments: &'arena [Expression<'arena>],
    pub value: Option<EnumValue>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub enum EnumValue {
    Number(f64),
    String(String),
}

#[derive(Debug, Clone, Serialize)]
pub struct ImportDeclaration<'arena> {
    #[serde(borrow)]
    pub clause: ImportClause<'arena>,
    pub source: String,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub enum ImportClause<'arena> {
    Default(Ident),
    #[serde(borrow)]
    Named(&'arena [ImportSpecifier]),
    Namespace(Ident),
    #[serde(borrow)]
    TypeOnly(&'arena [ImportSpecifier]),
    Mixed {
        default: Ident,
        #[serde(borrow)]
        named: &'arena [ImportSpecifier],
    },
}

#[derive(Debug, Clone, Serialize)]
pub struct ImportSpecifier {
    pub imported: Ident,
    pub local: Option<Ident>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct ExportDeclaration<'arena> {
    #[serde(borrow)]
    pub kind: ExportKind<'arena>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub enum ExportKind<'arena> {
    #[serde(borrow)]
    Declaration(&'arena Statement<'arena>),
    Named {
        #[serde(borrow)]
        specifiers: &'arena [ExportSpecifier],
        source: Option<String>,
        is_type_only: bool,
    },
    #[serde(borrow)]
    Default(&'arena Expression<'arena>),
    All {
        source: String,
        is_type_only: bool,
    },
}

#[derive(Debug, Clone, Serialize)]
pub struct ExportSpecifier {
    pub local: Ident,
    pub exported: Option<Ident>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct IfStatement<'arena> {
    #[serde(borrow)]
    pub condition: Expression<'arena>,
    #[serde(borrow)]
    pub then_block: Block<'arena>,
    #[serde(borrow)]
    pub else_ifs: &'arena [ElseIf<'arena>],
    #[serde(borrow)]
    pub else_block: Option<Block<'arena>>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct ElseIf<'arena> {
    #[serde(borrow)]
    pub condition: Expression<'arena>,
    #[serde(borrow)]
    pub block: Block<'arena>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct WhileStatement<'arena> {
    #[serde(borrow)]
    pub condition: Expression<'arena>,
    #[serde(borrow)]
    pub body: Block<'arena>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct RepeatStatement<'arena> {
    #[serde(borrow)]
    pub body: Block<'arena>,
    #[serde(borrow)]
    pub until: Expression<'arena>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub enum ForStatement<'arena> {
    #[serde(borrow)]
    Numeric(&'arena ForNumeric<'arena>),
    #[serde(borrow)]
    Generic(ForGeneric<'arena>),
}

#[derive(Debug, Clone, Serialize)]
pub struct ForNumeric<'arena> {
    pub variable: Ident,
    #[serde(borrow)]
    pub start: Expression<'arena>,
    #[serde(borrow)]
    pub end: Expression<'arena>,
    #[serde(borrow)]
    pub step: Option<Expression<'arena>>,
    #[serde(borrow)]
    pub body: Block<'arena>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct ForGeneric<'arena> {
    #[serde(borrow)]
    pub variables: &'arena [Ident],
    #[serde(borrow)]
    pub pattern: Option<super::pattern::Pattern<'arena>>,
    #[serde(borrow)]
    pub iterators: &'arena [Expression<'arena>],
    #[serde(borrow)]
    pub body: Block<'arena>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct ReturnStatement<'arena> {
    #[serde(borrow)]
    pub values: &'arena [Expression<'arena>],
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct Block<'arena> {
    #[serde(borrow)]
    pub statements: &'arena [Statement<'arena>],
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct TypeParameter<'arena> {
    pub name: Ident,
    #[serde(borrow)]
    pub constraint: Option<&'arena Type<'arena>>,
    #[serde(borrow)]
    pub default: Option<&'arena Type<'arena>>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct Parameter<'arena> {
    #[serde(borrow)]
    pub pattern: Pattern<'arena>,
    #[serde(borrow)]
    pub type_annotation: Option<Type<'arena>>,
    #[serde(borrow)]
    pub default: Option<Expression<'arena>>,
    pub is_rest: bool,
    pub is_optional: bool, // For optional parameters (parameter?: Type)
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct Decorator<'arena> {
    #[serde(borrow)]
    pub expression: DecoratorExpression<'arena>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub enum DecoratorExpression<'arena> {
    Identifier(Ident),
    Call {
        #[serde(borrow)]
        callee: &'arena DecoratorExpression<'arena>,
        #[serde(borrow)]
        arguments: &'arena [Expression<'arena>],
        span: Span,
    },
    Member {
        #[serde(borrow)]
        object: &'arena DecoratorExpression<'arena>,
        property: Ident,
        span: Span,
    },
}

// Declaration file-specific statements

#[derive(Debug, Clone, Serialize)]
pub struct DeclareFunctionStatement<'arena> {
    pub name: Ident,
    #[serde(borrow)]
    pub type_parameters: Option<&'arena [TypeParameter<'arena>]>,
    #[serde(borrow)]
    pub parameters: &'arena [Parameter<'arena>],
    #[serde(borrow)]
    pub return_type: Type<'arena>,
    #[serde(borrow)]
    pub throws: Option<&'arena [Type<'arena>]>,
    pub is_export: bool, // For `export function` inside namespaces
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct DeclareNamespaceStatement<'arena> {
    pub name: Ident,
    #[serde(borrow)]
    pub members: &'arena [Statement<'arena>], // Can contain export function, export const, etc.
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct DeclareConstStatement<'arena> {
    pub name: Ident,
    #[serde(borrow)]
    pub type_annotation: Type<'arena>,
    pub is_export: bool, // For `export const` inside namespaces
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct LabelStatement {
    pub name: Ident,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct GotoStatement {
    pub target: Ident,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct ThrowStatement<'arena> {
    #[serde(borrow)]
    pub expression: Expression<'arena>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct NamespaceDeclaration {
    pub path: Vec<Ident>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct TryStatement<'arena> {
    #[serde(borrow)]
    pub try_block: Block<'arena>,
    #[serde(borrow)]
    pub catch_clauses: &'arena [CatchClause<'arena>],
    #[serde(borrow)]
    pub finally_block: Option<Block<'arena>>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct CatchClause<'arena> {
    #[serde(borrow)]
    pub pattern: CatchPattern<'arena>,
    #[serde(borrow)]
    pub body: Block<'arena>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub enum CatchPattern<'arena> {
    Untyped {
        variable: Ident,
        span: Span,
    },
    Typed {
        variable: Ident,
        #[serde(borrow)]
        type_annotation: Type<'arena>,
        span: Span,
    },
    MultiTyped {
        variable: Ident,
        #[serde(borrow)]
        type_annotations: &'arena [Type<'arena>],
        span: Span,
    },
}
