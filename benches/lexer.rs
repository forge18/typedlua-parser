// Lexer benchmarks for typedlua-parser

use criterion::{criterion_group, criterion_main, Criterion};

mod lib;
use lib::*;

pub fn lexer_benchmarks(c: &mut Criterion) {
    // Realistic small file (typical module size)
    bench_lexer(c, "lexer/realistic_small", generate_identifiers(50));

    // Realistic medium file
    bench_lexer(c, "lexer/realistic_medium", generate_identifiers(200));

    // String literals (realistic count)
    bench_lexer(c, "lexer/strings", generate_strings(100));

    // Template strings (realistic count)
    bench_lexer(c, "lexer/templates", generate_templates(50));

    // Numeric literals (realistic count)
    bench_lexer(c, "lexer/numbers", generate_numbers(100));

    // Mixed realistic content
    let mixed = format!(
        "{}\n{}\n{}\n{}\n{}",
        generate_identifiers(50),
        generate_strings(25),
        generate_templates(10),
        generate_numbers(25),
        generate_variables(25)
    );
    bench_lexer(c, "lexer/mixed_realistic", mixed);
}

criterion_group!(benches, lexer_benchmarks);
criterion_main!(benches);
