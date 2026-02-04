#![no_main]

use libfuzzer_sys::fuzz_target;
use typedlua_parser::lexer::Lexer;
use typedlua_parser::parser::Parser;
use typedlua_parser::string_interner::StringInterner;

struct NoOpDiagnosticHandler;

impl typedlua_parser::diagnostic::DiagnosticHandler for NoOpDiagnosticHandler {
    fn report(&self, _diagnostic: typedlua_parser::diagnostic::Diagnostic) {
        // Ignore diagnostics for fuzzing
    }
}

fuzz_target!(|data: &[u8]| {
    // Try to interpret the data as UTF-8
    if let Ok(input) = std::str::from_utf8(data) {
        let interner = StringInterner::new();
        let lexer = Lexer::new(input, &interner);
        let handler = NoOpDiagnosticHandler;
        let mut parser = Parser::new(lexer, &handler, &interner);

        // Parse the input
        if let Ok(program) = parser.parse_program() {
            // Try to type check valid parses
            // Note: We're just checking it doesn't panic
            // A full type checker would need more setup
            let _ = program;
        }
    }
});
