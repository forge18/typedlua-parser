//! Performance benchmarks for incremental parsing
//!
//! These benchmarks validate the speedup claim for incremental parsing
//! on typical edit patterns by comparing incremental parse time against full reparse time.

use bumpalo::Bump;
use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use luanext_parser::diagnostics::CollectingDiagnosticHandler;
use luanext_parser::incremental::TextEdit;
use luanext_parser::lexer::Lexer;
use luanext_parser::parser::Parser;
use luanext_parser::string_interner::StringInterner;
use std::sync::Arc;

/// Generate a source file with N statements
fn generate_source(num_statements: usize) -> String {
    let mut source = String::new();
    for i in 0..num_statements {
        source.push_str(&format!("local var{} = {}\n", i, i));
    }
    source
}

/// Single character edit in the middle of a file
fn bench_single_char_edit(c: &mut Criterion) {
    let mut group = c.benchmark_group("single_char_edit");

    for size in [100, 500, 1000].iter() {
        let source = generate_source(*size);
        let (interner, common) = StringInterner::new_with_common_identifiers();
        let interner = Arc::new(interner);

        // Benchmark full reparse
        group.bench_with_input(BenchmarkId::new("full_parse", size), size, |b, _| {
            b.iter(|| {
                let arena = Bump::new();
                let handler = Arc::new(CollectingDiagnosticHandler::new());
                let mut lexer = Lexer::new(&source, handler.clone(), &interner);
                let tokens = lexer.tokenize().unwrap();
                let mut parser = Parser::new(tokens, handler.clone(), &interner, &common, &arena);
                black_box(parser.parse().unwrap());
            });
        });

        // Benchmark incremental parse (setup outside measured block)
        group.bench_with_input(BenchmarkId::new("incremental_parse", size), size, |b, _| {
            // Setup: do initial parse ONCE
            let arena1 = Box::leak(Box::new(Bump::new()));
            let handler = Arc::new(CollectingDiagnosticHandler::new());
            let mut lexer = Lexer::new(&source, handler.clone(), &interner);
            let tokens = lexer.tokenize().unwrap();
            let mut parser = Parser::new(tokens, handler.clone(), &interner, &common, arena1);
            let (_, tree) = parser.parse_incremental(None, &[], &source).unwrap();

            // Prepare edit
            let edit_pos = source.find("var50 = 50").unwrap() + 10;
            let mut edited_source = source.clone();
            edited_source.replace_range(edit_pos..edit_pos + 1, "1");
            let edits = vec![TextEdit {
                range: (edit_pos as u32, (edit_pos + 1) as u32),
                new_text: "1".to_string(),
            }];

            // Cast tree to 'static
            let static_tree: &'static luanext_parser::incremental::IncrementalParseTree<'static> = unsafe {
                std::mem::transmute(&tree)
            };

            // MEASURE ONLY THE INCREMENTAL PARSE
            b.iter(|| {
                let arena2 = Bump::new();
                // No full re-lex needed: incremental path lexes dirty regions internally
                let mut parser2 = Parser::new(vec![], handler.clone(), &interner, &common, &arena2);
                black_box(parser2.parse_incremental(Some(static_tree), &edits, &edited_source).unwrap());
            });
        });
    }

    group.finish();
}

/// Line deletion in the middle of a file
fn bench_line_deletion(c: &mut Criterion) {
    let mut group = c.benchmark_group("line_deletion");

    for size in [100, 500, 1000].iter() {
        let source = generate_source(*size);
        let (interner, common) = StringInterner::new_with_common_identifiers();
        let interner = Arc::new(interner);

        group.bench_with_input(BenchmarkId::new("full_parse", size), size, |b, _| {
            b.iter(|| {
                let arena = Bump::new();
                let handler = Arc::new(CollectingDiagnosticHandler::new());
                let mut lexer = Lexer::new(&source, handler.clone(), &interner);
                let tokens = lexer.tokenize().unwrap();
                let mut parser = Parser::new(tokens, handler.clone(), &interner, &common, &arena);
                black_box(parser.parse().unwrap());
            });
        });

        group.bench_with_input(BenchmarkId::new("incremental_parse", size), size, |b, _| {
            // Setup: initial parse
            let arena1 = Box::leak(Box::new(Bump::new()));
            let handler = Arc::new(CollectingDiagnosticHandler::new());
            let mut lexer = Lexer::new(&source, handler.clone(), &interner);
            let tokens = lexer.tokenize().unwrap();
            let mut parser = Parser::new(tokens, handler.clone(), &interner, &common, arena1);
            let (_, tree) = parser.parse_incremental(None, &[], &source).unwrap();

            // Prepare edit: delete line 50
            let line_start = source.lines().take(50).map(|l| l.len() + 1).sum::<usize>();
            let line_end = line_start + source.lines().nth(50).unwrap().len() + 1;
            let mut edited_source = source.clone();
            edited_source.replace_range(line_start..line_end, "");
            let edits = vec![TextEdit {
                range: (line_start as u32, line_end as u32),
                new_text: String::new(),
            }];

            let static_tree: &'static luanext_parser::incremental::IncrementalParseTree<'static> = unsafe {
                std::mem::transmute(&tree)
            };

            // MEASURE ONLY THE INCREMENTAL PARSE
            b.iter(|| {
                let arena2 = Bump::new();
                // No full re-lex needed: incremental path lexes dirty regions internally
                let mut parser2 = Parser::new(vec![], handler.clone(), &interner, &common, &arena2);
                black_box(parser2.parse_incremental(Some(static_tree), &edits, &edited_source).unwrap());
            });
        });
    }

    group.finish();
}

/// Multi-line paste in the middle of a file
fn bench_multiline_paste(c: &mut Criterion) {
    let mut group = c.benchmark_group("multiline_paste");

    for size in [100, 500, 1000].iter() {
        let source = generate_source(*size);
        let (interner, common) = StringInterner::new_with_common_identifiers();
        let interner = Arc::new(interner);
        let paste_text = "local new1 = 999\nlocal new2 = 999\nlocal new3 = 999\n";

        group.bench_with_input(BenchmarkId::new("full_parse", size), size, |b, _| {
            b.iter(|| {
                let arena = Bump::new();
                let handler = Arc::new(CollectingDiagnosticHandler::new());
                let mut lexer = Lexer::new(&source, handler.clone(), &interner);
                let tokens = lexer.tokenize().unwrap();
                let mut parser = Parser::new(tokens, handler.clone(), &interner, &common, &arena);
                black_box(parser.parse().unwrap());
            });
        });

        group.bench_with_input(BenchmarkId::new("incremental_parse", size), size, |b, _| {
            // Setup: initial parse
            let arena1 = Box::leak(Box::new(Bump::new()));
            let handler = Arc::new(CollectingDiagnosticHandler::new());
            let mut lexer = Lexer::new(&source, handler.clone(), &interner);
            let tokens = lexer.tokenize().unwrap();
            let mut parser = Parser::new(tokens, handler.clone(), &interner, &common, arena1);
            let (_, tree) = parser.parse_incremental(None, &[], &source).unwrap();

            // Prepare edit: paste 3 lines after line 50
            let insert_pos = source.lines().take(50).map(|l| l.len() + 1).sum::<usize>();
            let mut edited_source = source.clone();
            edited_source.insert_str(insert_pos, paste_text);
            let edits = vec![TextEdit {
                range: (insert_pos as u32, insert_pos as u32),
                new_text: paste_text.to_string(),
            }];

            let static_tree: &'static luanext_parser::incremental::IncrementalParseTree<'static> = unsafe {
                std::mem::transmute(&tree)
            };

            // MEASURE ONLY THE INCREMENTAL PARSE
            b.iter(|| {
                let arena2 = Bump::new();
                // No full re-lex needed: incremental path lexes dirty regions internally
                let mut parser2 = Parser::new(vec![], handler.clone(), &interner, &common, &arena2);
                black_box(parser2.parse_incremental(Some(static_tree), &edits, &edited_source).unwrap());
            });
        });
    }

    group.finish();
}

criterion_group!(benches, bench_single_char_edit, bench_line_deletion, bench_multiline_paste);
criterion_main!(benches);
