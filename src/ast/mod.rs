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
    /// Statement boundaries for incremental parsing (index, span)
    #[serde(skip)]
    pub statement_ranges: Option<Vec<(usize, Span)>>,
}

impl<'arena> Program<'arena> {
    pub fn new(statements: &'arena [statement::Statement<'arena>], span: Span) -> Self {
        // Compute statement ranges for incremental parsing
        let statement_ranges = statements
            .iter()
            .enumerate()
            .map(|(idx, stmt)| (idx, get_statement_span(stmt)))
            .collect();

        Program {
            statements,
            span,
            statement_ranges: Some(statement_ranges),
        }
    }

    /// Get statement ranges for incremental parsing
    pub fn get_statement_ranges(&self) -> &[(usize, Span)] {
        self.statement_ranges.as_deref().unwrap_or(&[])
    }
}

/// Helper function to extract span from a Statement
/// This matches each variant and extracts its span field
fn get_statement_span(statement: &statement::Statement) -> Span {
    use statement::{ForStatement, Statement};

    match statement {
        Statement::Variable(decl) => decl.span,
        Statement::Function(decl) => decl.span,
        Statement::Class(decl) => decl.span,
        Statement::Interface(decl) => decl.span,
        Statement::TypeAlias(decl) => decl.span,
        Statement::Enum(decl) => decl.span,
        Statement::Import(decl) => decl.span,
        Statement::Export(decl) => decl.span,
        Statement::If(stmt) => stmt.span,
        Statement::While(stmt) => stmt.span,
        Statement::For(stmt) => match stmt {
            ForStatement::Numeric(for_num) => for_num.span,
            ForStatement::Generic(for_gen) => for_gen.span,
        },
        Statement::Repeat(stmt) => stmt.span,
        Statement::Return(stmt) => stmt.span,
        Statement::Break(span) => *span,
        Statement::Continue(span) => *span,
        Statement::Block(block) => block.span,
        Statement::Label(label) => label.span,
        Statement::Goto(goto) => goto.span,
        Statement::Expression(expr) => expr.span,
        Statement::Throw(stmt) => stmt.span,
        Statement::Try(stmt) => stmt.span,
        Statement::Rethrow(span) => *span,
        Statement::Namespace(decl) => decl.span,
        Statement::DeclareFunction(decl) => decl.span,
        Statement::DeclareNamespace(decl) => decl.span,
        Statement::DeclareType(decl) => decl.span,
        Statement::DeclareInterface(decl) => decl.span,
        Statement::DeclareConst(decl) => decl.span,
    }
}
