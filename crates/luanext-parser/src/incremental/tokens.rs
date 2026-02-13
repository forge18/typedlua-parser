//! Token stream caching and adjustment for incremental lexing

use crate::incremental::dirty::TextEdit;
use crate::incremental::offset::adjust_span;
use crate::lexer::Token;
use crate::span::Span;

/// Adjust token offsets after text edits
///
/// Returns adjusted tokens. Tokens that overlap with edits are filtered out (invalidated).
pub fn adjust_token_offsets(tokens: &[Token], edits: &[TextEdit]) -> Vec<Token> {
    tokens
        .iter()
        .filter_map(|token| {
            let adjusted_span = adjust_span(&token.span, edits)?;
            Some(Token {
                kind: token.kind.clone(),
                span: adjusted_span,
            })
        })
        .collect()
}

/// Merge cached tokens with newly lexed tokens
///
/// Algorithm:
/// 1. Filter cached tokens: exclude any that overlap with dirty regions
/// 2. Combine new tokens with valid cached tokens
/// 3. Sort by position and deduplicate
///
/// # Arguments
/// * `cached` - Tokens from cache (may have adjusted offsets)
/// * `new_tokens` - Freshly lexed tokens from dirty regions
/// * `dirty_ranges` - Byte ranges that were re-lexed [(start, end)]
///
/// # Returns
/// Merged token stream sorted by position
pub fn merge_token_streams(
    cached: &[Token],
    new_tokens: Vec<Token>,
    dirty_ranges: &[(u32, u32)],
) -> Vec<Token> {
    let mut result = Vec::with_capacity(cached.len() + new_tokens.len());

    // Filter cached tokens: exclude any that overlap with dirty regions
    let valid_cached: Vec<Token> = cached
        .iter()
        .filter(|token| {
            !dirty_ranges.iter().any(|(start, end)| {
                token.span.overlaps(&Span {
                    start: *start,
                    end: *end,
                    line: 0,
                    column: 0,
                })
            })
        })
        .cloned()
        .collect();

    // Merge: add all new tokens and non-overlapping cached tokens
    result.extend(new_tokens);
    result.extend(valid_cached);

    // Sort by position
    result.sort_by_key(|t| t.span.start);

    // Dedup: remove consecutive tokens at same position (shouldn't happen, but defensive)
    result.dedup_by_key(|t| t.span.start);

    result
}

/// Check if a token spans across a dirty region boundary
/// Such tokens must be invalidated and re-lexed
pub fn token_spans_boundary(token: &Token, dirty_start: u32, dirty_end: u32) -> bool {
    // Token starts before dirty region and ends inside it
    (token.span.start < dirty_start && token.span.end > dirty_start) ||
    // Token starts inside dirty region and ends after it
    (token.span.start < dirty_end && token.span.end > dirty_end)
}

/// Expand dirty region to include tokens that span boundaries
/// This ensures we re-lex complete tokens
pub fn expand_dirty_region(
    tokens: &[Token],
    dirty_start: u32,
    dirty_end: u32,
) -> (u32, u32) {
    let mut expanded_start = dirty_start;
    let mut expanded_end = dirty_end;

    for token in tokens {
        if token_spans_boundary(token, dirty_start, dirty_end) {
            expanded_start = expanded_start.min(token.span.start);
            expanded_end = expanded_end.max(token.span.end);
        }
    }

    (expanded_start, expanded_end)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::TokenKind;
    use crate::span::Span;
    use crate::string_interner::StringInterner;

    fn make_token(kind: TokenKind, start: u32, end: u32) -> Token {
        Token {
            kind,
            span: Span::new(start, end, 1, start),
        }
    }

    #[test]
    fn test_adjust_token_offsets_no_overlap() {
        let interner = StringInterner::new();
        let id = interner.intern("x");

        let tokens = vec![
            make_token(TokenKind::Const, 50, 55),
            make_token(TokenKind::Identifier(id), 56, 57),
        ];

        let edits = vec![TextEdit {
            range: (10, 15),
            new_text: "xyz".to_string(), // delta: -2
        }];

        let adjusted = adjust_token_offsets(&tokens, &edits);
        assert_eq!(adjusted.len(), 2);
        assert_eq!(adjusted[0].span.start, 48); // 50 - 2
        assert_eq!(adjusted[0].span.end, 53);   // 55 - 2
    }

    #[test]
    fn test_adjust_token_offsets_with_overlap() {
        let interner = StringInterner::new();
        let id = interner.intern("x");

        let tokens = vec![
            make_token(TokenKind::Const, 10, 15),
            make_token(TokenKind::Identifier(id), 20, 25),
        ];

        let edits = vec![TextEdit {
            range: (12, 22), // Overlaps both tokens
            new_text: "x".to_string(),
        }];

        let adjusted = adjust_token_offsets(&tokens, &edits);
        // Both tokens overlap edit - should be filtered out
        assert_eq!(adjusted.len(), 0);
    }

    #[test]
    fn test_merge_token_streams() {
        let interner = StringInterner::new();
        let id = interner.intern("x");

        let cached = vec![
            make_token(TokenKind::Const, 0, 5),
            make_token(TokenKind::Identifier(id), 6, 7),
            make_token(TokenKind::Local, 50, 55), // This will be in dirty region
        ];

        let new_tokens = vec![
            make_token(TokenKind::Function, 50, 58), // Replaces cached token
            make_token(TokenKind::End, 60, 63),
        ];

        let dirty_ranges = vec![(50, 65)];

        let merged = merge_token_streams(&cached, new_tokens, &dirty_ranges);

        // Should have: cached[0,1] + new[0,1] = 4 tokens
        assert_eq!(merged.len(), 4);
        assert_eq!(merged[0].span.start, 0);  // cached
        assert_eq!(merged[1].span.start, 6);  // cached
        assert_eq!(merged[2].span.start, 50); // new (replaced cached)
        assert_eq!(merged[3].span.start, 60); // new
    }

    #[test]
    fn test_token_spans_boundary() {
        let token = make_token(TokenKind::String("hello".to_string()), 10, 20);

        // Edit inside token
        assert!(token_spans_boundary(&token, 5, 15));
        assert!(token_spans_boundary(&token, 15, 25));

        // Edit completely before or after
        assert!(!token_spans_boundary(&token, 0, 5));
        assert!(!token_spans_boundary(&token, 25, 30));

        // Edit completely contains token
        assert!(!token_spans_boundary(&token, 5, 25));
    }

    #[test]
    fn test_expand_dirty_region() {
        let tokens = vec![
            make_token(TokenKind::Const, 0, 5),
            make_token(TokenKind::String("multi\nline".to_string()), 10, 25),
            make_token(TokenKind::End, 30, 33),
        ];

        // Edit in middle of multi-line string
        let (start, end) = expand_dirty_region(&tokens, 15, 20);
        assert_eq!(start, 10); // Expanded to include string token
        assert_eq!(end, 25);   // Expanded to end of string
    }
}
