// Full parsing benchmarks for typedlua-parser

use criterion::{criterion_group, criterion_main, Criterion};

mod lib;
use lib::*;

pub fn full_benchmarks(c: &mut Criterion) {
    // Small realistic program (single module)
    let small_program = format!(
        "{}\n{}\n{}\n{}",
        generate_variables(20),
        generate_functions(5),
        generate_classes(2),
        generate_interfaces(2)
    );
    bench_full(c, "full/small_program", small_program);

    // Medium realistic program (medium module)
    let medium_program = format!(
        "{}\n{}\n{}\n{}",
        generate_variables(50),
        generate_functions(10),
        generate_classes(5),
        generate_interfaces(5)
    );
    bench_full(c, "full/medium_program", medium_program);

    // Large realistic program (large module)
    let large_program = format!(
        "{}\n{}\n{}\n{}",
        generate_variables(100),
        generate_functions(20),
        generate_classes(10),
        generate_interfaces(10)
    );
    bench_full(c, "full/large_program", large_program);

    // Real-world sample (if available)
    if let Ok(content) = std::fs::read_to_string("test_data/sample.tl") {
        bench_full(c, "full/real_world", content);
    }
}

criterion_group!(benches, full_benchmarks);
criterion_main!(benches);
