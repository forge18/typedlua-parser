//! Integration tests for incremental parsing

use bumpalo::Bump;
use luanext_parser::diagnostics::CollectingDiagnosticHandler;
use luanext_parser::incremental::TextEdit;
use luanext_parser::lexer::Lexer;
use luanext_parser::parser::Parser;
use luanext_parser::string_interner::StringInterner;
use std::sync::Arc;

#[test]
fn test_no_edits_fast_path() {
    let source = "local x = 1\nlocal y = 2";

    // First parse
    let (interner, common) = StringInterner::new_with_common_identifiers();
    let handler = Arc::new(CollectingDiagnosticHandler::new());
    let mut lexer = Lexer::new(source, &interner);
    let tokens = lexer.tokenize();

    let arena1 = Bump::new();
    let mut parser1 = Parser::new(tokens.clone(), Arc::clone(&handler), &interner, &common, &arena1);

    let (prog1, tree1) = parser1.parse_incremental(None, &[], source).expect("First parse should succeed");
    assert_eq!(prog1.statements.len(), 2);
    assert_eq!(tree1.statements.len(), 2);

    // Second parse with no edits - should reuse everything
    let arena2 = Bump::new();
    let mut parser2 = Parser::new(tokens, Arc::clone(&handler), &interner, &common, &arena2);

    // Cast tree1 to 'static for incremental parse
    let static_tree: &luanext_parser::incremental::IncrementalParseTree<'static> = unsafe {
        std::mem::transmute(&tree1)
    };

    let (prog2, tree2) = parser2.parse_incremental(Some(static_tree), &[], source).expect("Second parse should succeed");
    assert_eq!(prog2.statements.len(), 2);
    assert_eq!(tree2.statements.len(), 2);
    assert_eq!(tree2.version, 2);
}

#[test]
fn test_full_parse_fallback() {
    let source = "local x = 1\nlocal y = 2";

    // First parse
    let (interner, common) = StringInterner::new_with_common_identifiers();
    let handler = Arc::new(CollectingDiagnosticHandler::new());
    let mut lexer = Lexer::new(source, &interner);
    let tokens = lexer.tokenize();

    let arena1 = Bump::new();
    let mut parser1 = Parser::new(tokens.clone(), Arc::clone(&handler), &interner, &common, &arena1);

    let (prog1, tree1) = parser1.parse_incremental(None, &[], source).expect("First parse should succeed");
    assert_eq!(prog1.statements.len(), 2);

    // Second parse with different source - should do full parse
    let new_source = "local a = 10\nlocal b = 20\nlocal c = 30";
    let mut lexer2 = Lexer::new(new_source, &interner);
    let tokens2 = lexer2.tokenize();

    let arena2 = Bump::new();
    let mut parser2 = Parser::new(tokens2, Arc::clone(&handler), &interner, &common, &arena2);

    let static_tree: &luanext_parser::incremental::IncrementalParseTree<'static> = unsafe {
        std::mem::transmute(&tree1)
    };

    let edits = vec![TextEdit {
        range: (0, source.len() as u32),
        new_text: new_source.to_string(),
    }];

    let (prog2, tree2) = parser2.parse_incremental(Some(static_tree), &edits, new_source).expect("Second parse should succeed");
    assert_eq!(prog2.statements.len(), 3);
    assert_eq!(tree2.version, 2);
}

#[test]
fn test_first_parse_path() {
    let source = "local x = 1";

    let (interner, common) = StringInterner::new_with_common_identifiers();
    let handler = Arc::new(CollectingDiagnosticHandler::new());
    let mut lexer = Lexer::new(source, &interner);
    let tokens = lexer.tokenize();

    let arena = Bump::new();
    let mut parser = Parser::new(tokens, Arc::clone(&handler), &interner, &common, &arena);

    let (prog, tree) = parser.parse_incremental(None, &[], source).expect("Parse should succeed");
    assert_eq!(prog.statements.len(), 1);
    assert_eq!(tree.version, 1);
    assert_eq!(tree.arenas.len(), 1);
}

#[test]
fn test_arena_consolidation_trigger() {
    // This test verifies that arena consolidation gets triggered at >3 arenas
    let source1 = "local x = 1";
    let source2 = "local y = 2";
    let source3 = "local z = 3";
    let source4 = "local w = 4";

    let (interner, common) = StringInterner::new_with_common_identifiers();
    let handler = Arc::new(CollectingDiagnosticHandler::new());

    // Parse 1: Initial parse
    let mut lexer = Lexer::new(source1, &interner);
    let tokens = lexer.tokenize();
    let arena1 = Bump::new();
    let mut parser1 = Parser::new(tokens, Arc::clone(&handler), &interner, &common, &arena1);
    let (_, mut tree) = parser1.parse_incremental(None, &[], source1).expect("Parse should succeed");
    assert_eq!(tree.arenas.len(), 1);
    assert_eq!(tree.version, 1);

    // Simulate incremental parses that accumulate arenas
    // We'll manually add arenas to test consolidation
    use bumpalo::Bump;
    use luanext_parser::incremental::CachedStatement;

    // Add arenas to trigger consolidation (max 3, then consolidate)
    tree.arenas.push(Arc::new(Bump::new()));
    tree.arenas.push(Arc::new(Bump::new()));
    tree.arenas.push(Arc::new(Bump::new()));
    assert_eq!(tree.arenas.len(), 4); // Now we have 4 arenas

    // Trigger GC - should consolidate because > 3
    tree.collect_garbage();

    // After consolidation, should be back to 1 arena
    assert_eq!(tree.arenas.len(), 1, "Consolidation should reduce to 1 arena");
    assert_eq!(tree.statements.len(), 1, "Statement should be preserved");
}

#[test]
fn test_periodic_consolidation_every_10_parses() {
    // Verify consolidation triggers every 10 parses
    let source = "local x = 1";

    let (interner, common) = StringInterner::new_with_common_identifiers();
    let handler = Arc::new(CollectingDiagnosticHandler::new());

    let mut lexer = Lexer::new(source, &interner);
    let tokens = lexer.tokenize();
    let arena = Bump::new();
    let mut parser = Parser::new(tokens, Arc::clone(&handler), &interner, &common, &arena);
    let (_, mut tree) = parser.parse_incremental(None, &[], source).expect("Parse should succeed");

    // Simulate version 9 - should not consolidate
    tree.version = 9;
    tree.arenas.push(Arc::new(Bump::new())); // Add a second arena
    assert_eq!(tree.arenas.len(), 2);

    tree.collect_garbage();
    assert_eq!(tree.arenas.len(), 2, "Should not consolidate at version 9");

    // Simulate version 10 - should consolidate
    tree.version = 10;
    tree.collect_garbage();
    assert_eq!(tree.arenas.len(), 1, "Should consolidate at version 10");
}
