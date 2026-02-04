// Parser benchmarks for typedlua-parser

use criterion::{criterion_group, criterion_main, Criterion};

mod lib;
use lib::*;

pub fn parser_benchmarks(c: &mut Criterion) {
    // Simple expressions
    bench_parser(c, "parser/simple_expr", "local x = 1 + 2 * 3".to_string());

    // Nested expressions (realistic depth)
    bench_parser(c, "parser/nested_expr", generate_nested_expr(5));

    // Chained method calls (realistic length)
    bench_parser(c, "parser/chained_calls", generate_chained_calls(5));

    // Variable declarations (realistic count)
    bench_parser(c, "parser/variables", generate_variables(50));

    // Function declarations (realistic count)
    bench_parser(c, "parser/functions", generate_functions(10));

    // Class declarations (realistic count)
    bench_parser(c, "parser/classes", generate_classes(5));

    // Interface declarations (realistic count)
    bench_parser(c, "parser/interfaces", generate_interfaces(5));

    // Generic types (realistic count)
    bench_parser(c, "parser/generics", generate_generics(5));
}

criterion_group!(benches, parser_benchmarks);
criterion_main!(benches);
