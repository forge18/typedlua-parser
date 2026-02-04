// Shared benchmark utilities for typedlua-parser

use criterion::{black_box, Criterion};
use std::sync::Arc;
use typedlua_parser::{lexer::Lexer, parser::Parser, string_interner::StringInterner};

/// Simple no-op diagnostic handler for benchmarks
struct NoOpDiagnosticHandler;

impl typedlua_parser::diagnostics::DiagnosticHandler for NoOpDiagnosticHandler {
    fn report(&self, _diagnostic: typedlua_parser::diagnostics::Diagnostic) {
        // Ignore diagnostics for benchmarking
    }

    fn has_errors(&self) -> bool {
        false
    }

    fn error_count(&self) -> usize {
        0
    }

    fn warning_count(&self) -> usize {
        0
    }

    fn get_diagnostics(&self) -> Vec<typedlua_parser::diagnostics::Diagnostic> {
        Vec::new()
    }
}

/// Generate synthetic TypedLua code with specified number of identifiers
pub fn generate_identifiers(n: usize) -> String {
    (0..n)
        .map(|i| format!("local x{i} = {i}"))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Generate synthetic TypedLua code with specified number of string literals
pub fn generate_strings(n: usize) -> String {
    (0..n)
        .map(|i| format!("local s{i} = \"string{i}\""))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Generate synthetic TypedLua code with template strings
pub fn generate_templates(n: usize) -> String {
    (0..n)
        .map(|i| format!("local t{i} = `template{i}`"))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Generate synthetic TypedLua code with numeric literals
pub fn generate_numbers(n: usize) -> String {
    (0..n)
        .map(|i| format!("local n{i} = {i}.{i}"))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Generate nested expression with specified depth
pub fn generate_nested_expr(depth: usize) -> String {
    let mut expr = "1".to_string();
    for _i in 0..depth {
        expr = format!("(1 + {})", expr);
    }
    format!("local x = {}", expr)
}

/// Generate chained method calls with specified length
pub fn generate_chained_calls(length: usize) -> String {
    let calls = (0..length)
        .map(|i| format!(".method{i}()"))
        .collect::<String>();
    format!("local x = obj{}", calls)
}

/// Generate variable declarations with specified count
pub fn generate_variables(n: usize) -> String {
    (0..n)
        .map(|i| format!("local x{i}: number = {i}"))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Generate function declarations with specified count
pub fn generate_functions(n: usize) -> String {
    (0..n)
        .map(|i| format!("function f{i}(): number {{\n    return {i};\n}}", i = i))
        .collect::<Vec<_>>()
        .join("\n\n")
}

/// Generate class declarations with specified count
pub fn generate_classes(n: usize) -> String {
    (0..n)
        .map(|i| {
            format!(
                "class C{i} {{\n    x: number;\n    constructor(x: number) {{\n        this.x = x;\n    }}\n}}",
                i = i
            )
        })
        .collect::<Vec<_>>()
        .join("\n\n")
}

/// Generate interface declarations with specified count
pub fn generate_interfaces(n: usize) -> String {
    (0..n)
        .map(|i| {
            format!(
                "interface I{i} {{\n    x: number;\n    y: string;\n}}",
                i = i
            )
        })
        .collect::<Vec<_>>()
        .join("\n\n")
}

/// Generate generic type declarations with specified count
pub fn generate_generics(n: usize) -> String {
    (0..n)
        .map(|i| format!("type T{i}<T> = {{\n    value: T;\n}};", i = i))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Benchmark lexer on given input
pub fn bench_lexer(c: &mut Criterion, name: &str, input: String) {
    let handler = Arc::new(NoOpDiagnosticHandler);
    let (interner, common) = StringInterner::new_with_common_identifiers();

    c.bench_function(name, |b| {
        b.iter(|| {
            let mut lexer = Lexer::new(
                black_box(&input),
                Arc::clone(&handler) as Arc<dyn typedlua_parser::diagnostics::DiagnosticHandler>,
                &interner,
            );
            let tokens = lexer.tokenize().unwrap_or_default();
            let mut parser = Parser::new(
                tokens,
                Arc::clone(&handler) as Arc<dyn typedlua_parser::diagnostics::DiagnosticHandler>,
                &interner,
                &common,
            );
            let _ = parser.parse();
        })
    });
}

/// Benchmark parser on given input
pub fn bench_parser(c: &mut Criterion, name: &str, input: String) {
    let handler = Arc::new(NoOpDiagnosticHandler);
    let (interner, common) = StringInterner::new_with_common_identifiers();

    c.bench_function(name, |b| {
        b.iter(|| {
            let mut lexer = Lexer::new(
                black_box(&input),
                Arc::clone(&handler) as Arc<dyn typedlua_parser::diagnostics::DiagnosticHandler>,
                &interner,
            );
            let tokens = lexer.tokenize().unwrap_or_default();
            let mut parser = Parser::new(
                tokens,
                Arc::clone(&handler) as Arc<dyn typedlua_parser::diagnostics::DiagnosticHandler>,
                &interner,
                &common,
            );
            let _ = parser.parse();
        })
    });
}

/// Benchmark full parse (lex + parse) on given input
pub fn bench_full(c: &mut Criterion, name: &str, input: String) {
    let handler = Arc::new(NoOpDiagnosticHandler);
    let (interner, common) = StringInterner::new_with_common_identifiers();

    c.bench_function(name, |b| {
        b.iter(|| {
            let mut lexer = Lexer::new(
                black_box(&input),
                Arc::clone(&handler) as Arc<dyn typedlua_parser::diagnostics::DiagnosticHandler>,
                &interner,
            );
            let tokens = lexer.tokenize().unwrap_or_default();
            let mut parser = Parser::new(
                tokens,
                Arc::clone(&handler) as Arc<dyn typedlua_parser::diagnostics::DiagnosticHandler>,
                &interner,
                &common,
            );
            let _ = parser.parse();
        })
    });
}
