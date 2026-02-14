//! Realistic incremental parsing edit scenario tests
//!
//! These tests simulate real-world editing patterns to verify incremental
//! parsing produces the same results as full parsing.

use bumpalo::Bump;
use luanext_parser::diagnostics::CollectingDiagnosticHandler;
use luanext_parser::incremental::TextEdit;
use luanext_parser::lexer::Lexer;
use luanext_parser::parser::Parser;
use luanext_parser::string_interner::StringInterner;
use std::sync::Arc;

fn parse_and_compare(initial_source: &str, edits: &[TextEdit], final_source: &str) {
    let (interner, common) = StringInterner::new_with_common_identifiers();
    let handler = Arc::new(CollectingDiagnosticHandler::new());

    // First parse (initial source)
    let mut lexer1 = Lexer::new(initial_source, handler.clone(), &interner);
    let tokens1 = lexer1.tokenize().expect("Initial tokenize failed");
    let arena1 = Bump::new();
    let mut parser1 = Parser::new(tokens1, handler.clone(), &interner, &common, &arena1);
    let (_, tree1) = parser1
        .parse_incremental(None, &[], initial_source)
        .expect("Initial parse should succeed");

    // Incremental parse (with edits)
    let mut lexer2 = Lexer::new(final_source, handler.clone(), &interner);
    let tokens2 = lexer2.tokenize().expect("Final tokenize failed");
    let arena2 = Bump::new();
    let mut parser2 = Parser::new(tokens2.clone(), handler.clone(), &interner, &common, &arena2);

    let static_tree: &luanext_parser::incremental::IncrementalParseTree<'static> = unsafe {
        std::mem::transmute(&tree1)
    };

    let (inc_prog, _) = parser2
        .parse_incremental(Some(static_tree), edits, final_source)
        .expect("Incremental parse should succeed");

    // Full parse of final source (for comparison)
    let arena3 = Bump::new();
    let mut parser3 = Parser::new(tokens2, handler.clone(), &interner, &common, &arena3);
    let full_prog = parser3.parse().expect("Full parse should succeed");

    // Both should have same number of statements
    assert_eq!(
        inc_prog.statements.len(),
        full_prog.statements.len(),
        "Statement count mismatch: incremental={}, full={}",
        inc_prog.statements.len(),
        full_prog.statements.len()
    );
}

#[test]
fn test_type_single_character() {
    let initial = "local x = 1";
    let final_text = "local x = 12";

    let edits = vec![TextEdit {
        range: (11, 11),
        new_text: "2".to_string(),
    }];

    parse_and_compare(initial, &edits, final_text);
}

#[test]
fn test_delete_single_line() {
    let initial = "local x = 1\nlocal y = 2\nlocal z = 3";
    let final_text = "local x = 1\nlocal z = 3";

    let edits = vec![TextEdit {
        range: (12, 24),
        new_text: String::new(),
    }];

    parse_and_compare(initial, &edits, final_text);
}

#[test]
fn test_paste_multiline_code() {
    let initial = "local x = 1";
    let final_text = "local x = 1\nlocal y = 2\nlocal z = 3";

    let edits = vec![TextEdit {
        range: (11, 11),
        new_text: "\nlocal y = 2\nlocal z = 3".to_string(),
    }];

    parse_and_compare(initial, &edits, final_text);
}

#[test]
fn test_undo_redo_sequence() {
    let initial = "local x = 1";

    let edit1 = vec![TextEdit {
        range: (10, 11),
        new_text: "2".to_string(),
    }];

    let edit2 = vec![TextEdit {
        range: (10, 11),
        new_text: "1".to_string(),
    }];

    parse_and_compare(initial, &edit1, "local x = 2");
    parse_and_compare("local x = 2", &edit2, "local x = 1");
}

#[test]
fn test_format_document() {
    let initial = "local x=1\nlocal y=2";
    let final_text = "local x = 1\nlocal y = 2";

    let edits = vec![
        TextEdit {
            range: (7, 7),
            new_text: " ".to_string(),
        },
        TextEdit {
            range: (8, 8),
            new_text: " ".to_string(),
        },
        TextEdit {
            range: (17, 17),
            new_text: " ".to_string(),
        },
        TextEdit {
            range: (18, 18),
            new_text: " ".to_string(),
        },
    ];

    parse_and_compare(initial, &edits, final_text);
}

#[test]
fn test_incremental_typing_sequence() {
    let initial = "";

    let step1 = vec![TextEdit {
        range: (0, 0),
        new_text: "local ".to_string(),
    }];
    parse_and_compare(initial, &step1, "local ");

    let step2 = vec![TextEdit {
        range: (6, 6),
        new_text: "x".to_string(),
    }];
    parse_and_compare("local ", &step2, "local x");

    let step3 = vec![TextEdit {
        range: (7, 7),
        new_text: " = ".to_string(),
    }];
    parse_and_compare("local x", &step3, "local x = ");

    let step4 = vec![TextEdit {
        range: (10, 10),
        new_text: "1".to_string(),
    }];
    parse_and_compare("local x = ", &step4, "local x = 1");
}

#[test]
fn test_delete_entire_function() {
    let initial = "function foo(): void\n    return\nend\n\nfunction bar(): void\n    return\nend";
    let final_text = "function bar(): void\n    return\nend";

    let edits = vec![TextEdit {
        range: (0, 36),
        new_text: String::new(),
    }];

    parse_and_compare(initial, &edits, final_text);
}

#[test]
fn test_insert_new_function() {
    let initial = "function foo(): void\n    return\nend";
    let final_text = "function foo(): void\n    return\nend\n\nfunction bar(): void\n    return\nend";

    let edits = vec![TextEdit {
        range: (34, 34),
        new_text: "\n\nfunction bar(): void\n    return\nend".to_string(),
    }];

    parse_and_compare(initial, &edits, final_text);
}

#[test]
fn test_comment_out_code() {
    let initial = "local x = 1\nlocal y = 2";
    let final_text = "-- local x = 1\n-- local y = 2";

    let edits = vec![
        TextEdit {
            range: (0, 0),
            new_text: "-- ".to_string(),
        },
        TextEdit {
            range: (12, 12),
            new_text: "-- ".to_string(),
        },
    ];

    parse_and_compare(initial, &edits, final_text);
}

#[test]
fn test_uncomment_code() {
    let initial = "-- local x = 1\n-- local y = 2";
    let final_text = "local x = 1\nlocal y = 2";

    let edits = vec![
        TextEdit {
            range: (0, 3),
            new_text: String::new(),
        },
        TextEdit {
            range: (12, 15),
            new_text: String::new(),
        },
    ];

    parse_and_compare(initial, &edits, final_text);
}
