//! Integration tests for incremental lexing functionality

use luanext_parser::diagnostics::CollectingDiagnosticHandler;
use luanext_parser::errors::LexerError;
use luanext_parser::incremental::{TextEdit, adjust_token_offsets, merge_token_streams};
use luanext_parser::lexer::{Lexer, TokenKind};
use luanext_parser::string_interner::StringInterner;
use std::sync::Arc;

// Helper to create a lexer
fn make_lexer<'a>(source: &str, interner: &'a StringInterner) -> Lexer<'a> {
    Lexer::new(source, Arc::new(CollectingDiagnosticHandler::new()), interner)
}

#[test]
fn test_byte_to_char_index_ascii() {
    let interner = StringInterner::new();
    let source = "local x = 1";
    let mut lexer = make_lexer(source, &interner);

    // Test sync_position with ASCII text
    assert!(lexer.sync_position(0)); // Start
    assert!(lexer.sync_position(5)); // After "local"
    assert!(lexer.sync_position(11)); // End
    assert!(!lexer.sync_position(12)); // Out of bounds
}

#[test]
fn test_byte_to_char_index_utf8() {
    let interner = StringInterner::new();
    // "local 你好 = 1" - 你好 are 3 bytes each
    let source = "local 你好 = 1";
    let mut lexer = make_lexer(source, &interner);

    assert!(lexer.sync_position(0)); // Start
    assert!(lexer.sync_position(6)); // After "local "
    assert!(lexer.sync_position(12)); // After "你好" (6 + 3 + 3)

    // Mid-character offset should fail
    assert!(!lexer.sync_position(7)); // Middle of 你
    assert!(!lexer.sync_position(10)); // Middle of 好
}

#[test]
fn test_byte_to_char_index_emoji() {
    let interner = StringInterner::new();
    // "local 😀 = 1" - 😀 is 4 bytes
    let source = "local 😀 = 1";
    let mut lexer = make_lexer(source, &interner);

    assert!(lexer.sync_position(6)); // After "local "
    assert!(lexer.sync_position(10)); // After emoji (6 + 4)

    // Mid-emoji offset should fail
    assert!(!lexer.sync_position(7));
    assert!(!lexer.sync_position(8));
    assert!(!lexer.sync_position(9));
}

#[test]
fn test_sync_position_line_column() {
    let interner = StringInterner::new();
    let source = "local x = 1\nlocal y = 2\nlocal z = 3";
    let mut lexer = make_lexer(source, &interner);

    // Line 1, column 1
    assert!(lexer.sync_position(0));

    // Line 2, start
    assert!(lexer.sync_position(12));

    // Line 3, start
    assert!(lexer.sync_position(24));
}

#[test]
fn test_new_at_constructor() {
    let interner = StringInterner::new();
    let source = "local x = 1\nlocal y = 2";

    // Create lexer starting at second line
    let lexer_opt = Lexer::new_at(source, Arc::new(CollectingDiagnosticHandler::new()), &interner, 12, 2, 1);
    assert!(lexer_opt.is_some());

    // Invalid byte offset (mid-character in UTF-8)
    let source_utf8 = "你好";
    let lexer_opt = Lexer::new_at(source_utf8, Arc::new(CollectingDiagnosticHandler::new()), &interner, 1, 1, 1);
    assert!(lexer_opt.is_none());
}

#[test]
fn test_tokenize_from() {
    let interner = StringInterner::new();
    let source = "local x = 1\nlocal y = 2";
    let mut lexer = make_lexer(source, &interner);

    // Tokenize from second line
    let result = lexer.tokenize_from(12);
    assert!(result.is_ok());

    let tokens = result.unwrap();
    // Should have: local, y, =, 2, EOF
    assert_eq!(tokens.len(), 5);
    assert!(matches!(tokens[0].kind, TokenKind::Local));
}

#[test]
fn test_tokenize_from_invalid_offset() {
    let interner = StringInterner::new();
    let source = "local x = 1";
    let mut lexer = make_lexer(source, &interner);

    // Out of bounds
    let result = lexer.tokenize_from(100);
    assert!(matches!(result, Err(LexerError::InvalidByteOffset(100))));

    // Mid-character in UTF-8
    let source_utf8 = "你好";
    let mut lexer = Lexer::new(source_utf8, Arc::new(CollectingDiagnosticHandler::new()), &interner);
    let result = lexer.tokenize_from(1); // Middle of 你
    assert!(matches!(result, Err(LexerError::InvalidByteOffset(1))));
}

#[test]
fn test_tokenize_range() {
    let interner = StringInterner::new();
    let source = "local x = 1\nlocal y = 2\nlocal z = 3";
    let mut lexer = make_lexer(source, &interner);

    // Tokenize just the second line
    let result = lexer.tokenize_range(12, 24);
    assert!(result.is_ok());

    let tokens = result.unwrap();
    // Should have: local, y, =, 2 (no EOF since it's a range)
    assert!(tokens.len() >= 4);
    assert!(matches!(tokens[0].kind, TokenKind::Local));
}

#[test]
fn test_tokenize_range_invalid() {
    let interner = StringInterner::new();
    let source = "local x = 1";
    let mut lexer = make_lexer(source, &interner);

    // start > end
    let result = lexer.tokenize_range(10, 5);
    assert!(matches!(result, Err(LexerError::InvalidRange(10, 5))));
}

#[test]
fn test_tokenize_range_partial_token() {
    let interner = StringInterner::new();
    let source = "local function foo() end";
    let mut lexer = make_lexer(source, &interner);

    // Range that cuts off "function" keyword midway
    // tokenize_range should not include partial tokens
    let result = lexer.tokenize_range(0, 10);
    assert!(result.is_ok());

    let tokens = result.unwrap();
    // Should only include complete tokens
    for token in &tokens {
        assert!(token.span.end <= 10, "Token should not extend beyond range");
    }
}

#[test]
fn test_adjust_token_offsets_insertion() {
    let interner = StringInterner::new();
    let source = "local x = 1";
    let mut lexer = make_lexer(source, &interner);

    let tokens = lexer.tokenize().unwrap();

    // Insert "foo" at position 6 (after "local ")
    let edits = vec![TextEdit {
        range: (6, 6),
        new_text: "foo".to_string(),
    }];

    let adjusted = adjust_token_offsets(&tokens[..tokens.len() - 1], &edits); // Exclude EOF

    // Tokens after position 6 should be shifted by +3
    for token in &adjusted {
        if token.span.start >= 6 {
            // Verify shift happened
            assert!(token.span.start >= 9); // At least shifted
        }
    }
}

#[test]
fn test_adjust_token_offsets_deletion() {
    let interner = StringInterner::new();
    let source = "local xxx = 1";
    let mut lexer = make_lexer(source, &interner);

    let tokens = lexer.tokenize().unwrap();

    // Delete "xxx" at position 6-9
    let edits = vec![TextEdit {
        range: (6, 9),
        new_text: String::new(),
    }];

    let adjusted = adjust_token_offsets(&tokens[..tokens.len() - 1], &edits);

    // Tokens at 6-9 should be filtered out (overlapping)
    // Tokens after should be shifted by -3
    assert!(adjusted.len() < tokens.len() - 1); // Some tokens were filtered
}

#[test]
fn test_merge_token_streams_basic() {
    let interner = StringInterner::new();
    
    // Create some cached tokens
    let source1 = "local x = 1";
    let mut lexer1 = make_lexer(source1, &interner);
    let cached = lexer1.tokenize().unwrap();

    // Create new tokens from a dirty region
    let source2 = "local y = 2";
    let mut lexer2 = make_lexer(source2, &interner);
    let new_tokens = lexer2.tokenize().unwrap();

    // Merge with no overlap
    let dirty_ranges = vec![];
    let merged = merge_token_streams(&cached, new_tokens.clone(), &dirty_ranges);

    // Should have all tokens from both sources
    assert!(merged.len() >= cached.len());
}

#[test]
fn test_empty_source() {
    let interner = StringInterner::new();
    let source = "";
    let mut lexer = make_lexer(source, &interner);

    let result = lexer.tokenize_from(0);
    assert!(result.is_ok());

    let tokens = result.unwrap();
    // Should just have EOF
    assert_eq!(tokens.len(), 1);
    assert!(matches!(tokens[0].kind, TokenKind::Eof));
}

#[test]
fn test_tokenize_range_exact_boundary() {
    let interner = StringInterner::new();
    let source = "local x = 1";
    let mut lexer = make_lexer(source, &interner);

    // Tokenize exact source length
    let result = lexer.tokenize_range(0, source.len() as u32);
    assert!(result.is_ok());

    let tokens = result.unwrap();
    assert!(!tokens.is_empty());
}
