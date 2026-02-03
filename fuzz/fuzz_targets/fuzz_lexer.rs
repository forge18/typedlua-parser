#![no_main]

use libfuzzer_sys::fuzz_target;
use typedlua_parser::lexer::Lexer;
use typedlua_parser::string_interner::StringInterner;

fuzz_target!(|data: &[u8]| {
    // Try to interpret the data as UTF-8
    if let Ok(input) = std::str::from_utf8(data) {
        let interner = StringInterner::new();
        let mut lexer = Lexer::new(input, &interner);

        // Lex all tokens - should not panic
        while let Ok(token) = lexer.next_token() {
            if token.kind == typedlua_parser::lexer::TokenKind::Eof {
                break;
            }
        }
    }
});
