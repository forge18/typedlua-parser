use std::sync::Arc;
use std::time::Instant;
use typedlua_parser::{lexer::Lexer, parser::Parser, string_interner::StringInterner};

struct NoOpDiagnosticHandler;

impl typedlua_parser::diagnostics::DiagnosticHandler for NoOpDiagnosticHandler {
    fn report(&self, _diagnostic: typedlua_parser::diagnostics::Diagnostic) {}
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

fn generate_functions(n: usize) -> String {
    (0..n)
        .map(|i| format!("function f{i}(): number {{\n    return {i};\n}}", i = i))
        .collect::<Vec<_>>()
        .join("\n\n")
}

fn generate_nested_expr(depth: usize) -> String {
    let mut expr = "1".to_string();
    for _ in 0..depth {
        expr = format!("(1 + {})", expr);
    }
    format!("local x = {}", expr)
}

fn generate_variables(n: usize) -> String {
    (0..n)
        .map(|i| format!("local x{i}: number = {i}"))
        .collect::<Vec<_>>()
        .join("\n")
}

fn generate_templates(n: usize) -> String {
    (0..n)
        .map(|i| format!("local t{i} = `template{i}`"))
        .collect::<Vec<_>>()
        .join("\n")
}

fn generate_templates_with_expr(n: usize) -> String {
    (0..n)
        .map(|i| format!("local t{i} = `result: ${{x + {i}}}`"))
        .collect::<Vec<_>>()
        .join("\n")
}

fn generate_classes(n: usize) -> String {
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

fn profile_lexer(name: &str, input: &str, iterations: usize) {
    let handler: Arc<dyn typedlua_parser::diagnostics::DiagnosticHandler> =
        Arc::new(NoOpDiagnosticHandler);
    let interner = StringInterner::new();

    for _ in 0..10 {
        let mut lexer = Lexer::new(input, Arc::clone(&handler), &interner);
        let _ = lexer.tokenize().unwrap();
    }

    let start = Instant::now();
    for _ in 0..iterations {
        let mut lexer = Lexer::new(input, Arc::clone(&handler), &interner);
        let _ = lexer.tokenize().unwrap();
    }
    let elapsed = start.elapsed();
    let per_iter = elapsed.as_secs_f64() / iterations as f64;
    let ns_per_iter = per_iter * 1_000_000_000.0;

    println!(
        "LEX {:30} {:>12.2} ns ({:.2} µs)",
        name,
        ns_per_iter,
        ns_per_iter / 1000.0
    );
}

fn profile_parser(name: &str, input: &str, iterations: usize) {
    let handler: Arc<dyn typedlua_parser::diagnostics::DiagnosticHandler> =
        Arc::new(NoOpDiagnosticHandler);
    let (interner, common) = StringInterner::new_with_common_identifiers();

    for _ in 0..10 {
        let mut lexer = Lexer::new(input, Arc::clone(&handler), &interner);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens, Arc::clone(&handler), &interner, &common);
        let _ = parser.parse();
    }

    let start = Instant::now();
    for _ in 0..iterations {
        let mut lexer = Lexer::new(input, Arc::clone(&handler), &interner);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens, Arc::clone(&handler), &interner, &common);
        let _ = parser.parse();
    }
    let elapsed = start.elapsed();
    let per_iter = elapsed.as_secs_f64() / iterations as f64;
    let ns_per_iter = per_iter * 1_000_000_000.0;

    println!(
        "PRS {:30} {:>12.2} ns ({:.2} µs)",
        name,
        ns_per_iter,
        ns_per_iter / 1000.0
    );
}

fn main() {
    println!("Detailed Performance Profiling\n");
    println!("{}", "=".repeat(70));

    println!("\n--- LEXER PERFORMANCE ---");
    profile_lexer("functions_10", &generate_functions(10), 2000);
    profile_lexer("functions_50", &generate_functions(50), 500);
    profile_lexer("nested_expr_5", &generate_nested_expr(5), 2000);
    profile_lexer("variables_50", &generate_variables(50), 1000);
    profile_lexer("templates_simple_50", &generate_templates(50), 1000);
    profile_lexer("templates_expr_50", &generate_templates_with_expr(50), 500);
    profile_lexer("classes_5", &generate_classes(5), 1000);
    profile_lexer("classes_20", &generate_classes(20), 200);

    println!("\n--- PARSER PERFORMANCE ---");
    profile_parser("functions_10", &generate_functions(10), 2000);
    profile_parser("functions_50", &generate_functions(50), 500);
    profile_parser("nested_expr_5", &generate_nested_expr(5), 2000);
    profile_parser("variables_50", &generate_variables(50), 1000);
    profile_parser("templates_simple_50", &generate_templates(50), 1000);
    profile_parser("templates_expr_50", &generate_templates_with_expr(50), 500);
    profile_parser("classes_5", &generate_classes(5), 1000);
    profile_parser("classes_20", &generate_classes(20), 200);

    println!("\n{}", "=".repeat(70));
    println!("Done!");
}
