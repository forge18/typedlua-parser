// Lexer benchmarks for typedlua-parser

use criterion::{criterion_group, criterion_main, Criterion};

mod lib;
use lib::*;

pub fn lexer_benchmarks(c: &mut Criterion) {
    // Small file with 100 identifiers
    bench_lexer(c, "lexer/small_identifiers", generate_identifiers(100));

    // Large file with 5000 identifiers
    bench_lexer(c, "lexer/large_identifiers", generate_identifiers(5000));

    // String literals benchmark
    bench_lexer(c, "lexer/strings", generate_strings(1000));

    // Template string benchmark
    bench_lexer(c, "lexer/templates", generate_templates(1000));

    // Numeric literals benchmark
    bench_lexer(c, "lexer/numbers", generate_numbers(1000));

    // Mixed content benchmark
    let mixed = format!(
        "{}\n{}\n{}\n{}\n{}",
        generate_identifiers(500),
        generate_strings(500),
        generate_templates(500),
        generate_numbers(500),
        generate_variables(500)
    );
    bench_lexer(c, "lexer/mixed", mixed);
}

criterion_group!(benches, lexer_benchmarks);
criterion_main!(benches);
