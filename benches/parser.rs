// Parser benchmarks for typedlua-parser

use criterion::{criterion_group, criterion_main, Criterion};

mod lib;
use lib::*;

pub fn parser_benchmarks(c: &mut Criterion) {
    // Simple expressions
    bench_parser(c, "parser/simple_expr", "local x = 1 + 2 * 3".to_string());

    // Nested expressions (depth 20)
    bench_parser(c, "parser/nested_expr", generate_nested_expr(20));

    // Chained method calls (length 50)
    bench_parser(c, "parser/chained_calls", generate_chained_calls(50));

    // Variable declarations
    bench_parser(c, "parser/variables", generate_variables(1000));

    // Function declarations
    bench_parser(c, "parser/functions", generate_functions(500));

    // Class declarations
    bench_parser(c, "parser/classes", generate_classes(100));

    // Interface declarations
    bench_parser(c, "parser/interfaces", generate_interfaces(100));

    // Generic types
    bench_parser(c, "parser/generics", generate_generics(100));
}

criterion_group!(benches, parser_benchmarks);
criterion_main!(benches);
