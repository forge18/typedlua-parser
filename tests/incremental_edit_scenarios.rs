//! Realistic incremental parsing edit scenario tests
//!
//! These tests simulate real-world editing patterns to verify incremental
//! parsing produces the same results as full parsing.

use luanext_parser::diagnostics::CollectingDiagnosticHandler;
use luanext_parser::incremental::TextEdit;
use luanext_parser::parser::Parser;

fn parse_and_compare(initial_source: &str, edits: &[TextEdit], final_source: &str) {
    // Parse with incremental
    let incremental_arena = bumpalo::Bump::new();
    let incremental_handler = CollectingDiagnosticHandler::new();
    let incremental_parser = Parser::new(initial_source, &incremental_handler, &incremental_arena);
    let incremental_result = incremental_parser.parse_incremental(edits);

    // Parse final source fresh (full parse)
    let full_arena = bumpalo::Bump::new();
    let full_handler = CollectingDiagnosticHandler::new();
    let full_parser = Parser::new(final_source, &full_handler, &full_arena);
    let full_result = full_parser.parse();

    // Both should succeed
    assert!(incremental_result.is_some(), "Incremental parse failed");
    assert!(full_result.is_some(), "Full parse failed");

    // Both should have same number of statements
    let inc_stmts = incremental_result.unwrap().statements.len();
    let full_stmts = full_result.unwrap().statements.len();
    assert_eq!(
        inc_stmts, full_stmts,
        "Statement count mismatch: incremental={}, full={}",
        inc_stmts, full_stmts
    );
}

#[test]
fn test_type_single_character() {
    let initial = "local x = 1";
    let final_text = "local x = 12";

    let edits = vec![TextEdit {
        range: (11, 11), // Insert after "1"
        new_text: "2".to_string(),
    }];

    parse_and_compare(initial, &edits, final_text);
}

#[test]
fn test_delete_single_line() {
    let initial = "local x = 1\nlocal y = 2\nlocal z = 3";
    let final_text = "local x = 1\nlocal z = 3";

    let edits = vec![TextEdit {
        range: (12, 24), // Delete "local y = 2\n"
        new_text: String::new(),
    }];

    parse_and_compare(initial, &edits, final_text);
}

#[test]
fn test_paste_multiline_code() {
    let initial = "local x = 1";
    let final_text = "local x = 1\nlocal y = 2\nlocal z = 3";

    let edits = vec![TextEdit {
        range: (11, 11), // Append after first line
        new_text: "\nlocal y = 2\nlocal z = 3".to_string(),
    }];

    parse_and_compare(initial, &edits, final_text);
}

#[test]
fn test_undo_redo_sequence() {
    // Initial: "local x = 1"
    // Edit 1: Change to "local x = 2"
    // Edit 2: Change back to "local x = 1" (undo)

    let initial = "local x = 1";

    // First edit: change 1 to 2
    let edit1 = vec![TextEdit {
        range: (10, 11),
        new_text: "2".to_string(),
    }];

    // Second edit: change 2 back to 1
    let edit2 = vec![TextEdit {
        range: (10, 11),
        new_text: "1".to_string(),
    }];

    // After edit1
    parse_and_compare(initial, &edit1, "local x = 2");

    // After edit2 (undo)
    parse_and_compare("local x = 2", &edit2, "local x = 1");
}

#[test]
fn test_format_document() {
    let initial = "local x=1\nlocal y=2";
    let final_text = "local x = 1\nlocal y = 2";

    // Add spaces around equals signs
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
    // Simulate typing "local x = 1" character by character
    let initial = "";

    // Type "local "
    let step1 = vec![TextEdit {
        range: (0, 0),
        new_text: "local ".to_string(),
    }];
    parse_and_compare(initial, &step1, "local ");

    // Type "x"
    let step2 = vec![TextEdit {
        range: (6, 6),
        new_text: "x".to_string(),
    }];
    parse_and_compare("local ", &step2, "local x");

    // Type " = "
    let step3 = vec![TextEdit {
        range: (7, 7),
        new_text: " = ".to_string(),
    }];
    parse_and_compare("local x", &step3, "local x = ");

    // Type "1"
    let step4 = vec![TextEdit {
        range: (10, 10),
        new_text: "1".to_string(),
    }];
    parse_and_compare("local x = ", &step4, "local x = 1");
}

#[test]
fn test_delete_entire_function() {
    let initial = r#"
function foo(): void
    return
end

function bar(): void
    return
end
"#;

    let final_text = r#"
function bar(): void
    return
end
"#;

    // Delete first function (approximate byte positions)
    let edits = vec![TextEdit {
        range: (1, 42), // Delete "function foo()...\nend\n"
        new_text: String::new(),
    }];

    parse_and_compare(initial, &edits, final_text);
}

#[test]
fn test_insert_new_function() {
    let initial = r#"function foo(): void
    return
end"#;

    let final_text = r#"function foo(): void
    return
end

function bar(): void
    return
end"#;

    let edits = vec![TextEdit {
        range: (35, 35), // Append after first function
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
            range: (0, 3), // Delete "-- "
            new_text: String::new(),
        },
        TextEdit {
            range: (12, 15), // Delete "-- " (adjusted for first deletion)
            new_text: String::new(),
        },
    ];

    parse_and_compare(initial, &edits, final_text);
}
