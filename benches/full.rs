// Full parsing benchmarks for typedlua-parser

use criterion::{criterion_group, criterion_main, Criterion};

mod lib;
use lib::*;

pub fn full_benchmarks(c: &mut Criterion) {
    // Small complete program
    let small_program = format!(
        "{}\n{}\n{}\n{}",
        generate_variables(100),
        generate_functions(50),
        generate_classes(20),
        generate_interfaces(20)
    );
    bench_full(c, "full/small_program", small_program);

    // Medium complete program
    let medium_program = format!(
        "{}\n{}\n{}\n{}",
        generate_variables(500),
        generate_functions(200),
        generate_classes(50),
        generate_interfaces(50)
    );
    bench_full(c, "full/medium_program", medium_program);

    // Large complete program
    let large_program = format!(
        "{}\n{}\n{}\n{}",
        generate_variables(2000),
        generate_functions(500),
        generate_classes(100),
        generate_interfaces(100)
    );
    bench_full(c, "full/large_program", large_program);

    // Real-world sample (if available)
    if let Ok(content) = std::fs::read_to_string("test_data/sample.tl") {
        bench_full(c, "full/real_world", content);
    }
}

criterion_group!(benches, full_benchmarks);
criterion_main!(benches);
