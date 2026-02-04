# AST (Abstract Syntax Tree)

The AST module defines the structured representation of parsed TypedLua code.

## Overview

The AST is organized into four main modules:

- `statement.rs` - Statement nodes
- `expression.rs` - Expression nodes
- `types.rs` - Type system nodes
- `pattern.rs` - Pattern matching nodes

## Common Types

### Spanned

A wrapper that adds span information to any value:

```rust
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}
```

### Ident

A string identifier interned for efficiency:

```rust
pub type Ident = Spanned<StringId>;
```

### Program

The root node containing all statements:

```rust
pub struct Program {
    pub statements: Vec<Statement>,
    pub span: Span,
}
```

## Statements

### VariableDeclaration

```rust
pub struct VariableDeclaration {
    pub kind: VariableKind,  // Local or Const
    pub pattern: Pattern,
    pub type_annotation: Option<Type>,
    pub initializer: Expression,
    pub span: Span,
}

pub enum VariableKind {
    Local,
    Const,
}
```

### FunctionDeclaration

```rust
pub struct FunctionDeclaration {
    pub name: Ident,
    pub type_parameters: Option<Vec<TypeParameter>>,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub throws: Option<Vec<Type>>,  // Error types
    pub body: Block,
    pub span: Span,
}

pub struct Parameter {
    pub pattern: Pattern,
    pub type_annotation: Option<Type>,
    pub default: Option<Expression>,
    pub is_rest: bool,
    pub is_optional: bool,
    pub span: Span,
}
```

### ClassDeclaration

```rust
pub struct ClassDeclaration {
    pub name: Ident,
    pub type_parameters: Option<Vec<TypeParameter>>,
    pub extends: Option<ReferenceType>,
    pub implements: Vec<ReferenceType>,
    pub members: Vec<ClassMember>,
    pub span: Span,
}

pub enum ClassMember {
    Property(PropertyDeclaration),
    Method(MethodDeclaration),
    Constructor(ConstructorDeclaration),
}
```

### InterfaceDeclaration

```rust
pub struct InterfaceDeclaration {
    pub name: Ident,
    pub type_parameters: Option<Vec<TypeParameter>>,
    pub extends: Vec<ReferenceType>,
    pub signatures: Vec<InterfaceSignature>,
    pub span: Span,
}

pub enum InterfaceSignature {
    Property(PropertySignature),
    Method(MethodSignature),
}
```

### TypeAliasDeclaration

```rust
pub struct TypeAliasDeclaration {
    pub name: Ident,
    pub type_parameters: Option<Vec<TypeParameter>>,
    pub value: Type,
    pub span: Span,
}
```

### EnumDeclaration

```rust
pub struct EnumDeclaration {
    pub name: Ident,
    pub members: Vec<EnumMember>,
    pub span: Span,
}

pub struct EnumMember {
    pub name: Ident,
    pub value: Option<Expression>,
    pub span: Span,
}
```

### Control Flow Statements

```rust
pub enum Statement {
    If(IfStatement),
    While(WhileStatement),
    For(ForStatement),
    Repeat(RepeatStatement),
    Return(Option<Expression>),
    Break,
    Continue,
    // ... other variants
}
```

## Expressions

### Literal

```rust
pub enum Literal {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}
```

### Binary Operations

```rust
pub enum BinaryOp {
    Add, Subtract, Multiply, Divide, Modulo, Power,
    Concatenate, IntegerDivide,
    Equal, NotEqual, LessThan, LessThanOrEqual, GreaterThan, GreaterThanOrEqual,
    And, Or,
    BitwiseOr, BitwiseXor, BitwiseAnd, ShiftLeft, ShiftRight,
    NullCoalesce,
}

pub struct BinaryExpression {
    pub op: BinaryOp,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub span: Span,
}
```

### Unary Operations

```rust
pub enum UnaryOp {
    Not, Negate, Length, BitwiseNot,
}

pub struct UnaryExpression {
    pub op: UnaryOp,
    pub expr: Box<Expression>,
    pub span: Span,
}
```

### Function Expression

```rust
pub struct FunctionExpression {
    pub type_parameters: Option<Vec<TypeParameter>>,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub body: Block,
    pub span: Span,
}
```

### Arrow Function

```rust
pub enum ArrowBody {
    Block(Block),
    Expression(Box<Expression>),
}

pub struct ArrowFunction {
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub body: ArrowBody,
    pub span: Span,
}
```

### Call and Member Access

```rust
pub struct CallExpression {
    pub callee: Box<Expression>,
    pub arguments: Vec<Argument>,
    pub type_arguments: Option<Vec<Type>>,
    pub span: Span,
}

pub struct MethodCallExpression {
    pub object: Box<Expression>,
    pub method: Ident,
    pub arguments: Vec<Argument>,
    pub type_arguments: Option<Vec<Type>>,
    pub span: Span,
}

pub struct MemberExpression {
    pub object: Box<Expression>,
    pub member: Ident,
    pub span: Span,
}

pub struct IndexExpression {
    pub object: Box<Expression>,
    pub index: Box<Expression>,
    pub span: Span,
}
```

### Object and Array

```rust
pub enum ObjectProperty {
    Property { key: Ident, value: Box<Expression> },
    Computed { key: Box<Expression>, value: Box<Expression> },
    Spread { value: Box<Expression> },
}

pub struct ObjectExpression {
    pub properties: Vec<ObjectProperty>,
    pub span: Span,
}

pub enum ArrayElement {
    Expression(Expression),
    Spread(Expression),
}

pub struct ArrayExpression {
    pub elements: Vec<ArrayElement>,
    pub span: Span,
}
```

### Template String

```rust
pub enum TemplatePart {
    String(String),
    Expression(Box<Expression>),
}

pub struct TemplateLiteral {
    pub parts: Vec<TemplatePart>,
    pub span: Span,
}
```

### Other Expressions

```rust
pub enum Expression {
    Literal(Literal),
    Identifier(Ident),
    Binary(BinaryExpression),
    Unary(UnaryExpression),
    Call(CallExpression),
    MethodCall(MethodCallExpression),
    Member(MemberExpression),
    Index(IndexExpression),
    Function(FunctionExpression),
    Arrow(ArrowFunction),
    Object(ObjectExpression),
    Array(ArrayExpression),
    Template(TemplateLiteral),
    Parenthesized(Box<Expression>),
    Conditional(ConditionalExpression),
    New(CallExpression, Vec<Argument>),
    // ... other variants
}
```

## Types

### Primitive Types

```rust
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
```

### Type References

```rust
pub struct ReferenceType {
    pub name: Ident,
    pub type_arguments: Option<Vec<Type>>,
    pub span: Span,
}
```

### Union and Intersection

```rust
pub struct UnionType {
    pub types: Vec<Type>,
    pub span: Span,
}

pub struct IntersectionType {
    pub types: Vec<Type>,
    pub span: Span,
}
```

### Function Types

```rust
pub struct FunctionType {
    pub type_parameters: Option<Vec<TypeParameter>>,
    pub parameters: Vec<ParameterType>,
    pub return_type: Box<Type>,
    pub throws: Option<Vec<Type>>,
    pub span: Span,
}

pub struct ParameterType {
    pub pattern: Pattern,
    pub type_annotation: Option<Type>,
    pub is_optional: bool,
    pub is_rest: bool,
    pub span: Span,
}
```

### Complex Types

```rust
pub struct ArrayType {
    pub element: Box<Type>,
    pub span: Span,
}

pub struct TupleType {
    pub types: Vec<Type>,
    pub span: Span,
}

pub struct ObjectType {
    pub properties: Vec<ObjectTypeProperty>,
    pub index_signature: Option<IndexSignature>,
    pub span: Span,
}

pub struct MappedType {
    pub readonly_modifier: MappedTypeModifier,
    pub key_type: Box<Type>,
    pub value_type: Box<Type>,
    pub optional_modifier: MappedTypeModifier,
    pub span: Span,
}

pub struct ConditionalType {
    pub check_type: Box<Type>,
    pub extends_type: Box<Type>,
    pub true_type: Box<Type>,
    pub false_type: Box<Type>,
    pub span: Span,
}
```

### Type Parameters

```rust
pub struct TypeParameter {
    pub name: Ident,
    pub constraint: Option<Type>,
    pub default: Option<Type>,
    pub span: Span,
}
```

## Patterns

Patterns are used in destructuring and match expressions:

```rust
pub enum Pattern {
    Identifier(Ident),
    Wildcard(Span),
    Literal(Literal, Span),
    Array(ArrayPattern),
    Object(ObjectPattern),
    Or(OrPattern),
}

pub struct ArrayPattern {
    pub elements: Vec<ArrayPatternElement>,
    pub span: Span,
}

pub enum ArrayPatternElement {
    Pattern(Pattern),
    Rest(Ident),
    Hole,
}

pub struct ObjectPattern {
    pub properties: Vec<ObjectPatternProperty>,
    pub span: Span,
}

pub struct ObjectPatternProperty {
    pub key: Ident,
    pub value: Option<Pattern>,
    pub default: Option<Expression>,
    pub span: Span,
}

pub struct OrPattern {
    pub alternatives: Vec<Pattern>,
    pub span: Span,
}
```

## Serde Support

All AST nodes derive `Serialize` and `Deserialize` for serialization:

```rust
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Program {
    #[serde(default)]
    pub statements: Vec<Statement>,
    #[serde(default)]
    pub span: Span,
}
```

This enables:
- Saving parsed AST to files
- Sending AST over process boundaries
- Pretty-printing AST for debugging