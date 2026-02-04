mod lexeme;

pub use lexeme::{TemplatePart, Token, TokenKind};

use crate::diagnostics::{error_codes, DiagnosticHandler};
use crate::errors::LexerError;
use crate::span::Span;
use crate::string_interner::StringInterner;
use std::sync::Arc;

/// Lexer for TypedLua source code
pub struct Lexer<'a> {
    source: Vec<char>,
    position: u32,
    line: u32,
    column: u32,
    diagnostic_handler: Arc<dyn DiagnosticHandler>,
    interner: &'a StringInterner,
}

impl<'a> Lexer<'a> {
    pub fn new(
        source: &str,
        diagnostic_handler: Arc<dyn DiagnosticHandler>,
        interner: &'a StringInterner,
    ) -> Self {
        // Pre-allocate Vec<char> to reduce reallocation overhead
        // Most UTF-8 text averages ~1.5 bytes per char, so divide by 2 as estimate
        let estimated_capacity = source.len() / 2 + 10;
        let mut chars = Vec::with_capacity(estimated_capacity);
        chars.extend(source.chars());

        Self {
            source: chars,
            position: 0,
            line: 1,
            column: 1,
            diagnostic_handler,
            interner,
        }
    }

    /// Tokenize the entire source
    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexerError> {
        // Pre-allocate: estimate ~1 token per 5 characters
        let estimated_tokens = self.source.len() / 5 + 10;
        let mut tokens = Vec::with_capacity(estimated_tokens);

        while !self.is_at_end() {
            self.skip_whitespace();
            if self.is_at_end() {
                break;
            }

            // Try to skip comments
            if self.try_skip_comment() {
                continue;
            }

            let token = self.next_token()?;
            tokens.push(token);
        }

        tokens.push(Token::eof(self.position));
        Ok(tokens)
    }

    fn next_token(&mut self) -> Result<Token, LexerError> {
        let start = self.position;
        let start_line = self.line;
        let start_column = self.column;

        let ch = self.current();

        let kind = match ch {
            // Single-character tokens
            '(' => {
                self.advance();
                TokenKind::LeftParen
            }
            ')' => {
                self.advance();
                TokenKind::RightParen
            }
            '{' => {
                self.advance();
                TokenKind::LeftBrace
            }
            '}' => {
                self.advance();
                TokenKind::RightBrace
            }
            '[' => {
                self.advance();
                TokenKind::LeftBracket
            }
            ']' => {
                self.advance();
                TokenKind::RightBracket
            }
            ',' => {
                self.advance();
                TokenKind::Comma
            }
            ';' => {
                self.advance();
                TokenKind::Semicolon
            }
            '+' => {
                self.advance();
                if self.current() == '=' {
                    self.advance();
                    TokenKind::PlusEqual
                } else {
                    TokenKind::Plus
                }
            }
            '*' => {
                self.advance();
                if self.current() == '=' {
                    self.advance();
                    TokenKind::StarEqual
                } else {
                    TokenKind::Star
                }
            }
            '/' => {
                self.advance();
                if self.current() == '/' {
                    self.advance();
                    if self.current() == '=' {
                        self.advance();
                        TokenKind::SlashSlashEqual
                    } else {
                        TokenKind::SlashSlash
                    }
                } else if self.current() == '=' {
                    self.advance();
                    TokenKind::SlashEqual
                } else {
                    TokenKind::Slash
                }
            }
            '%' => {
                self.advance();
                if self.current() == '=' {
                    self.advance();
                    TokenKind::PercentEqual
                } else {
                    TokenKind::Percent
                }
            }
            '^' => {
                self.advance();
                if self.current() == '=' {
                    self.advance();
                    TokenKind::CaretEqual
                } else {
                    TokenKind::Caret
                }
            }
            '#' => {
                self.advance();
                TokenKind::Hash
            }
            '&' => {
                self.advance();
                if self.current() == '=' {
                    self.advance();
                    TokenKind::AmpersandEqual
                } else {
                    TokenKind::Ampersand
                }
            }
            '?' => {
                self.advance();
                if self.current() == '?' {
                    self.advance();
                    TokenKind::QuestionQuestion
                } else if self.current() == '.' {
                    self.advance();
                    TokenKind::QuestionDot
                } else {
                    TokenKind::Question
                }
            }
            '@' => {
                self.advance();
                TokenKind::At
            }

            // Multi-character operators
            '~' => {
                self.advance();
                if self.current() == '=' {
                    self.advance();
                    TokenKind::TildeEqual
                } else {
                    TokenKind::Tilde
                }
            }
            '<' => {
                self.advance();
                if self.current() == '=' {
                    self.advance();
                    TokenKind::LessEqual
                } else if self.current() == '<' {
                    self.advance();
                    if self.current() == '=' {
                        self.advance();
                        TokenKind::LessLessEqual
                    } else {
                        TokenKind::LessLess
                    }
                } else {
                    TokenKind::LessThan
                }
            }
            '>' => {
                self.advance();
                if self.current() == '=' {
                    self.advance();
                    TokenKind::GreaterEqual
                } else if self.current() == '>' {
                    self.advance();
                    if self.current() == '=' {
                        self.advance();
                        TokenKind::GreaterGreaterEqual
                    } else {
                        TokenKind::GreaterGreater
                    }
                } else {
                    TokenKind::GreaterThan
                }
            }
            '=' => {
                self.advance();
                if self.current() == '=' {
                    self.advance();
                    TokenKind::EqualEqual
                } else if self.current() == '>' {
                    self.advance();
                    TokenKind::FatArrow
                } else {
                    TokenKind::Equal
                }
            }
            '!' => {
                self.advance();
                if self.current() == '=' {
                    self.advance();
                    TokenKind::BangEqual
                } else if self.current() == '!' {
                    self.advance();
                    TokenKind::BangBang
                } else {
                    TokenKind::Bang
                }
            }
            '.' => {
                self.advance();
                if self.current() == '.' {
                    self.advance();
                    if self.current() == '.' {
                        self.advance();
                        TokenKind::DotDotDot
                    } else if self.current() == '=' {
                        self.advance();
                        TokenKind::DotDotEqual
                    } else {
                        TokenKind::DotDot
                    }
                } else {
                    TokenKind::Dot
                }
            }
            ':' => {
                self.advance();
                if self.current() == ':' {
                    self.advance();
                    TokenKind::ColonColon
                } else {
                    TokenKind::Colon
                }
            }
            '-' => {
                self.advance();
                if self.current() == '>' {
                    self.advance();
                    TokenKind::Arrow
                } else if self.current() == '=' {
                    self.advance();
                    TokenKind::MinusEqual
                } else {
                    TokenKind::Minus
                }
            }
            '|' => {
                self.advance();
                if self.current() == '>' {
                    self.advance();
                    TokenKind::PipeOp
                } else if self.current() == '=' {
                    self.advance();
                    TokenKind::PipeEqual
                } else {
                    TokenKind::Pipe
                }
            }

            // String literals
            '"' => self.read_string('"')?,
            '\'' => self.read_string('\'')?,
            '`' => return self.read_template_string(),

            // Numbers
            '0'..='9' => self.read_number()?,

            // Identifiers and keywords
            'a'..='z' | 'A'..='Z' | '_' => self.read_identifier(),

            _ => {
                self.advance();
                self.diagnostic_handler.report_error(
                    Span::new(start, self.position, start_line, start_column),
                    error_codes::UNEXPECTED_CHAR,
                    &format!("Unexpected character: '{}'", ch),
                );
                TokenKind::Unknown(ch)
            }
        };

        Ok(Token::new(
            kind,
            Span::new(start, self.position, start_line, start_column),
        ))
    }

    fn read_identifier(&mut self) -> TokenKind {
        // Pre-allocate: most identifiers are < 16 chars
        let mut ident = String::with_capacity(16);

        while !self.is_at_end() && (self.current().is_alphanumeric() || self.current() == '_') {
            ident.push(self.current());
            self.advance();
        }

        TokenKind::from_keyword(&ident)
            .unwrap_or_else(|| TokenKind::Identifier(self.interner.intern(&ident)))
    }

    fn read_number(&mut self) -> Result<TokenKind, LexerError> {
        // Pre-allocate: most numbers are < 16 chars
        let mut number = String::with_capacity(16);

        // Handle hex numbers (0x...)
        if self.current() == '0' && self.peek() == Some('x') {
            number.push(self.current());
            self.advance();
            number.push(self.current());
            self.advance();

            while !self.is_at_end() && self.current().is_ascii_hexdigit() {
                number.push(self.current());
                self.advance();
            }
            return Ok(TokenKind::Number(number));
        }

        // Handle binary numbers (0b...)
        if self.current() == '0' && self.peek() == Some('b') {
            number.push(self.current());
            self.advance();
            number.push(self.current());
            self.advance();

            while !self.is_at_end() && matches!(self.current(), '0' | '1') {
                number.push(self.current());
                self.advance();
            }
            return Ok(TokenKind::Number(number));
        }

        // Regular decimal number
        while !self.is_at_end() && self.current().is_ascii_digit() {
            number.push(self.current());
            self.advance();
        }

        // Handle decimal point
        if !self.is_at_end()
            && self.current() == '.'
            && self.peek().is_some_and(|c| c.is_ascii_digit())
        {
            number.push(self.current());
            self.advance();

            while !self.is_at_end() && self.current().is_ascii_digit() {
                number.push(self.current());
                self.advance();
            }
        }

        // Handle scientific notation (e.g., 1e10, 2.5e-3)
        if !self.is_at_end() && matches!(self.current(), 'e' | 'E') {
            number.push(self.current());
            self.advance();

            if !self.is_at_end() && matches!(self.current(), '+' | '-') {
                number.push(self.current());
                self.advance();
            }

            while !self.is_at_end() && self.current().is_ascii_digit() {
                number.push(self.current());
                self.advance();
            }
        }

        Ok(TokenKind::Number(number))
    }

    fn read_string(&mut self, quote: char) -> Result<TokenKind, LexerError> {
        self.advance(); // Skip opening quote

        // Pre-allocate: most strings are < 32 chars
        let mut string = String::with_capacity(32);

        while !self.is_at_end() && self.current() != quote {
            if self.current() == '\\' {
                self.advance();
                if self.is_at_end() {
                    return Err(LexerError::UnterminatedString);
                }

                let escaped = match self.current() {
                    'n' => '\n',
                    't' => '\t',
                    'r' => '\r',
                    '\\' => '\\',
                    '\'' => '\'',
                    '"' => '"',
                    '0' => '\0',
                    _ => self.current(),
                };

                string.push(escaped);
                self.advance();
            } else {
                string.push(self.current());
                self.advance();
            }
        }

        if self.is_at_end() {
            return Err(LexerError::UnterminatedString);
        }

        self.advance(); // Skip closing quote
        Ok(TokenKind::String(string))
    }

    fn read_template_string(&mut self) -> Result<Token, LexerError> {
        let start = self.position;
        let start_line = self.line;
        let start_column = self.column;

        self.advance(); // Skip opening `

        // Pre-allocate: most templates have 3-5 parts
        let mut parts = Vec::with_capacity(4);
        // Pre-allocate: most string parts are < 32 chars
        let mut current_string = String::with_capacity(32);

        while !self.is_at_end() && self.current() != '`' {
            if self.current() == '$' && self.peek() == Some('{') {
                // Save current string part
                if !current_string.is_empty() {
                    parts.push(TemplatePart::String(std::mem::take(&mut current_string)));
                }

                self.advance(); // Skip $
                self.advance(); // Skip {

                // Read expression tokens until }
                let mut expr_tokens = Vec::new();
                let mut brace_depth = 1;

                while !self.is_at_end() && brace_depth > 0 {
                    if self.current() == '{' {
                        brace_depth += 1;
                    } else if self.current() == '}' {
                        brace_depth -= 1;
                        if brace_depth == 0 {
                            break;
                        }
                    }

                    self.skip_whitespace();
                    if !self.is_at_end() && self.current() != '}' {
                        let token = self.next_token()?;
                        expr_tokens.push(token);
                    }
                }

                if self.is_at_end() {
                    return Err(LexerError::UnterminatedString);
                }

                self.advance(); // Skip }
                parts.push(TemplatePart::Expression(expr_tokens));
            } else if self.current() == '\\' {
                self.advance();
                if !self.is_at_end() {
                    let escaped = match self.current() {
                        'n' => '\n',
                        't' => '\t',
                        'r' => '\r',
                        '\\' => '\\',
                        '`' => '`',
                        _ => self.current(),
                    };
                    current_string.push(escaped);
                    self.advance();
                }
            } else {
                current_string.push(self.current());
                self.advance();
            }
        }

        if self.is_at_end() {
            return Err(LexerError::UnterminatedString);
        }

        // Add final string part
        if !current_string.is_empty() {
            parts.push(TemplatePart::String(current_string));
        }

        self.advance(); // Skip closing `

        Ok(Token::new(
            TokenKind::TemplateString(parts),
            Span::new(start, self.position, start_line, start_column),
        ))
    }

    fn try_skip_comment(&mut self) -> bool {
        // Single-line comment: --
        if self.current() == '-' && self.peek() == Some('-') {
            // Check if it's a multi-line comment: --[[
            if ((self.position + 2) as usize) < self.source.len()
                && self.source[(self.position + 2) as usize] == '['
                && ((self.position + 3) as usize) < self.source.len()
                && self.source[(self.position + 3) as usize] == '['
            {
                // Multi-line comment: --[[ ... ]]--
                self.advance(); // Skip first -
                self.advance(); // Skip second -
                self.advance(); // Skip first [
                self.advance(); // Skip second [

                while !self.is_at_end() {
                    if self.current() == ']' && self.peek() == Some(']') {
                        self.advance(); // Skip first ]
                        self.advance(); // Skip second ]
                                        // Optionally skip trailing --
                        if self.current() == '-' && self.peek() == Some('-') {
                            self.advance();
                            self.advance();
                        }
                        return true;
                    }
                    self.advance();
                }

                // Unterminated comment
                self.diagnostic_handler.report_error(
                    Span::new(self.position, self.position, self.line, self.column),
                    error_codes::UNTERMINATED_COMMENT,
                    "Unterminated multi-line comment",
                );
                return true;
            } else {
                // Single-line comment
                while !self.is_at_end() && self.current() != '\n' {
                    self.advance();
                }
                return true;
            }
        }

        false
    }

    #[inline]
    fn skip_whitespace(&mut self) {
        // Fast path for common ASCII whitespace characters
        while !self.is_at_end() {
            match self.current() {
                ' ' | '\t' | '\r' | '\n' => self.advance(),
                _ => break,
            }
        }
    }

    #[inline(always)]
    fn current(&self) -> char {
        self.source
            .get(self.position as usize)
            .copied()
            .unwrap_or('\0')
    }

    #[inline(always)]
    fn peek(&self) -> Option<char> {
        self.source.get((self.position + 1) as usize).copied()
    }

    #[inline]
    fn advance(&mut self) {
        if !self.is_at_end() {
            if self.current() == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
            self.position += 1;
        }
    }

    #[inline(always)]
    fn is_at_end(&self) -> bool {
        (self.position as usize) >= self.source.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::di::{DiContainer, ServiceLifetime};
    use crate::diagnostics::{CollectingDiagnosticHandler, DiagnosticHandler};
    use crate::string_interner::StringInterner;

    fn lex(source: &str) -> Vec<Token> {
        let mut container = DiContainer::new();
        container.register(
            |_| Arc::new(CollectingDiagnosticHandler::new()) as Arc<dyn DiagnosticHandler>,
            ServiceLifetime::Transient,
        );
        let diagnostic_handler = container.resolve::<Arc<dyn DiagnosticHandler>>().unwrap();
        let interner = StringInterner::new();
        let mut lexer = Lexer::new(source, diagnostic_handler, &interner);
        lexer.tokenize().unwrap()
    }

    fn lex_with_interner(source: &str) -> (Vec<Token>, StringInterner) {
        let mut container = DiContainer::new();
        container.register(
            |_| Arc::new(CollectingDiagnosticHandler::new()) as Arc<dyn DiagnosticHandler>,
            ServiceLifetime::Transient,
        );
        let diagnostic_handler = container.resolve::<Arc<dyn DiagnosticHandler>>().unwrap();
        let interner = StringInterner::new();
        let mut lexer = Lexer::new(source, diagnostic_handler, &interner);
        (lexer.tokenize().unwrap(), interner)
    }

    fn lex_with_handler(source: &str) -> (Vec<Token>, Arc<CollectingDiagnosticHandler>) {
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let interner = StringInterner::new();
        let mut lexer = Lexer::new(source, handler.clone(), &interner);
        (lexer.tokenize().unwrap(), handler)
    }

    #[test]
    fn test_keywords() {
        let tokens = lex("const local function return if then else end");
        assert_eq!(tokens[0].kind, TokenKind::Const);
        assert_eq!(tokens[1].kind, TokenKind::Local);
        assert_eq!(tokens[2].kind, TokenKind::Function);
        assert_eq!(tokens[3].kind, TokenKind::Return);
        assert_eq!(tokens[4].kind, TokenKind::If);
        assert_eq!(tokens[5].kind, TokenKind::Then);
        assert_eq!(tokens[6].kind, TokenKind::Else);
        assert_eq!(tokens[7].kind, TokenKind::End);
    }

    #[test]
    fn test_identifiers() {
        let tokens = lex("foo bar_baz _test Test123");
        assert!(matches!(tokens[0].kind, TokenKind::Identifier(_)));
        assert!(matches!(tokens[1].kind, TokenKind::Identifier(_)));
        assert!(matches!(tokens[2].kind, TokenKind::Identifier(_)));
        assert!(matches!(tokens[3].kind, TokenKind::Identifier(_)));
    }

    #[test]
    fn test_numbers() {
        let tokens = lex("123 45.67 0x1A 0b1010 1e10 2.5e-3");

        assert!(matches!(&tokens[0].kind, TokenKind::Number(n) if n == "123"));
        assert!(matches!(&tokens[1].kind, TokenKind::Number(n) if n == "45.67"));
        assert!(matches!(&tokens[2].kind, TokenKind::Number(n) if n == "0x1A"));
        assert!(matches!(&tokens[3].kind, TokenKind::Number(n) if n == "0b1010"));
        assert!(matches!(&tokens[4].kind, TokenKind::Number(n) if n == "1e10"));
        assert!(matches!(&tokens[5].kind, TokenKind::Number(n) if n == "2.5e-3"));
    }

    #[test]
    fn test_strings() {
        let tokens = lex(r#""hello" 'world' "escape\n\t\"" "#);

        assert!(matches!(&tokens[0].kind, TokenKind::String(s) if s == "hello"));
        assert!(matches!(&tokens[1].kind, TokenKind::String(s) if s == "world"));
        assert!(matches!(&tokens[2].kind, TokenKind::String(s) if s == "escape\n\t\""));
    }

    #[test]
    fn test_template_strings() {
        let tokens = lex("`hello ${name} world`");

        assert!(matches!(tokens[0].kind, TokenKind::TemplateString(_)));
        if let TokenKind::TemplateString(parts) = &tokens[0].kind {
            assert_eq!(parts.len(), 3);
            assert!(matches!(&parts[0], TemplatePart::String(s) if s == "hello "));
            assert!(matches!(&parts[1], TemplatePart::Expression(_)));
            assert!(matches!(&parts[2], TemplatePart::String(s) if s == " world"));
        }
    }

    #[test]
    fn test_operators() {
        let tokens = lex("+ - * / % ^ # & | ~");
        assert_eq!(tokens[0].kind, TokenKind::Plus);
        assert_eq!(tokens[1].kind, TokenKind::Minus);
        assert_eq!(tokens[2].kind, TokenKind::Star);
        assert_eq!(tokens[3].kind, TokenKind::Slash);
        assert_eq!(tokens[4].kind, TokenKind::Percent);
        assert_eq!(tokens[5].kind, TokenKind::Caret);
        assert_eq!(tokens[6].kind, TokenKind::Hash);
        assert_eq!(tokens[7].kind, TokenKind::Ampersand);
        assert_eq!(tokens[8].kind, TokenKind::Pipe);
        assert_eq!(tokens[9].kind, TokenKind::Tilde);
    }

    #[test]
    fn test_comparison_operators() {
        let tokens = lex("< <= > >= == != ~=");
        assert_eq!(tokens[0].kind, TokenKind::LessThan);
        assert_eq!(tokens[1].kind, TokenKind::LessEqual);
        assert_eq!(tokens[2].kind, TokenKind::GreaterThan);
        assert_eq!(tokens[3].kind, TokenKind::GreaterEqual);
        assert_eq!(tokens[4].kind, TokenKind::EqualEqual);
        assert_eq!(tokens[5].kind, TokenKind::BangEqual);
        assert_eq!(tokens[6].kind, TokenKind::TildeEqual);
    }

    #[test]
    fn test_arrows_and_pipes() {
        let tokens = lex("-> => |>");
        assert_eq!(tokens[0].kind, TokenKind::Arrow);
        assert_eq!(tokens[1].kind, TokenKind::FatArrow);
        assert_eq!(tokens[2].kind, TokenKind::PipeOp);
    }

    #[test]
    fn test_dots() {
        let tokens = lex(". .. ...");
        assert_eq!(tokens[0].kind, TokenKind::Dot);
        assert_eq!(tokens[1].kind, TokenKind::DotDot);
        assert_eq!(tokens[2].kind, TokenKind::DotDotDot);
    }

    #[test]
    fn test_delimiters() {
        let tokens = lex("( ) { } [ ] , ;");
        assert_eq!(tokens[0].kind, TokenKind::LeftParen);
        assert_eq!(tokens[1].kind, TokenKind::RightParen);
        assert_eq!(tokens[2].kind, TokenKind::LeftBrace);
        assert_eq!(tokens[3].kind, TokenKind::RightBrace);
        assert_eq!(tokens[4].kind, TokenKind::LeftBracket);
        assert_eq!(tokens[5].kind, TokenKind::RightBracket);
        assert_eq!(tokens[6].kind, TokenKind::Comma);
        assert_eq!(tokens[7].kind, TokenKind::Semicolon);
    }

    #[test]
    fn test_single_line_comment() {
        let (tokens, _) = lex_with_interner("const x = 5 -- this is a comment\nlocal y = 10");
        assert_eq!(tokens[0].kind, TokenKind::Const);
        let (tokens, interner) = lex_with_interner("const x = 5");
        if let TokenKind::Identifier(id) = tokens[1].kind {
            assert_eq!(interner.resolve(id), "x");
        } else {
            panic!("Expected Identifier, got {:?}", tokens[1].kind);
        }
        assert_eq!(tokens[2].kind, TokenKind::Equal);
        if let TokenKind::Number(n) = &tokens[3].kind {
            assert_eq!(n, "5");
        } else {
            panic!("Expected Number, got {:?}", tokens[3].kind);
        }
        assert_eq!(tokens[4].kind, TokenKind::Eof);
    }

    #[test]
    fn test_multi_line_comment() {
        let (tokens, interner) = lex_with_interner(
            "const x = 5 --[[ this is\n a multi-line\n comment ]]-- local y = 10",
        );
        assert_eq!(tokens[0].kind, TokenKind::Const);
        if let TokenKind::Identifier(id) = tokens[1].kind {
            assert_eq!(interner.resolve(id), "x");
        } else {
            panic!("Expected Identifier, got {:?}", tokens[1].kind);
        }
        assert_eq!(tokens[2].kind, TokenKind::Equal);
        if let TokenKind::Number(n) = &tokens[3].kind {
            assert_eq!(n, "5");
        } else {
            panic!("Expected Number, got {:?}", tokens[3].kind);
        }
        assert_eq!(tokens[4].kind, TokenKind::Local);
    }

    #[test]
    fn test_complex_program() {
        let source = r#"
            const PI: number = 3.14159

            interface Point {
                x: number,
                y: number
            }

            function distance(p1: Point, p2: Point): number
                const dx = p2.x - p1.x
                const dy = p2.y - p1.y
                return (dx * dx + dy * dy) -> math.sqrt
            end
        "#;

        let tokens = lex(source);

        // Should have tokenized successfully
        assert!(!tokens.is_empty());
        assert_eq!(tokens.last().unwrap().kind, TokenKind::Eof);

        // Check some specific tokens
        assert_eq!(tokens[0].kind, TokenKind::Const);
        // Find interface token
        let has_interface = tokens.iter().any(|t| t.kind == TokenKind::Interface);
        assert!(has_interface);
        // Find colon token
        let has_colon = tokens.iter().any(|t| t.kind == TokenKind::Colon);
        assert!(has_colon);
    }

    #[test]
    fn test_line_and_column_tracking() {
        let source = "const\nx";
        let tokens = lex(source);

        assert_eq!(tokens[0].span.line, 1);
        assert_eq!(tokens[0].span.column, 1);
        assert_eq!(tokens[1].span.line, 2);
        assert_eq!(tokens[1].span.column, 1);
    }

    #[test]
    fn test_unterminated_string() {
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let interner = StringInterner::new();
        let mut lexer = Lexer::new("\"hello", handler.clone(), &interner);
        // Should fail with unterminated string error
        let result = lexer.tokenize();
        assert!(result.is_err() || handler.has_errors());
    }

    #[test]
    fn test_unterminated_multi_line_comment() {
        let (_, handler) = lex_with_handler("--[[ unterminated comment");
        assert!(handler.has_errors());
    }

    #[test]
    fn test_escape_sequences() {
        let tokens = lex("\"hello\\nworld\\ttab\\rreturn\\\\backslash\\\"quote\\'apos\\0null\"");
        if let TokenKind::String(s) = &tokens[0].kind {
            assert!(s.contains('\n'));
            assert!(s.contains('\t'));
            assert!(s.contains('\r'));
            assert!(s.contains('\\'));
            assert!(s.contains('"'));
            assert!(s.contains('\''));
            assert!(s.contains('\0'));
        } else {
            panic!("Expected String token");
        }
    }

    #[test]
    fn test_unknown_escape_sequence() {
        let tokens = lex("\"hello\\zworld\"");
        if let TokenKind::String(s) = &tokens[0].kind {
            // Unknown escape sequences should be preserved as-is
            assert!(s.contains('z'));
        } else {
            panic!("Expected String token");
        }
    }

    #[test]
    fn test_template_string_empty_expression() {
        let tokens = lex("`${}`");
        assert!(matches!(&tokens[0].kind, TokenKind::TemplateString(_)));
    }

    #[test]
    fn test_template_string_complex() {
        let tokens = lex("`Result: ${a + b} and ${foo()} end`");
        assert!(matches!(&tokens[0].kind, TokenKind::TemplateString(_)));
    }

    #[test]
    fn test_all_keywords() {
        let keywords = [
            ("and", TokenKind::And),
            ("break", TokenKind::Break),
            ("const", TokenKind::Const),
            ("continue", TokenKind::Continue),
            ("do", TokenKind::Do),
            ("else", TokenKind::Else),
            ("elseif", TokenKind::Elseif),
            ("end", TokenKind::End),
            ("enum", TokenKind::Enum),
            ("export", TokenKind::Export),
            ("false", TokenKind::False),
            ("final", TokenKind::Final),
            ("for", TokenKind::For),
            ("function", TokenKind::Function),
            ("if", TokenKind::If),
            ("import", TokenKind::Import),
            ("in", TokenKind::In),
            ("interface", TokenKind::Interface),
            ("local", TokenKind::Local),
            ("match", TokenKind::Match),
            ("namespace", TokenKind::Namespace),
            ("new", TokenKind::New),
            ("nil", TokenKind::Nil),
            ("not", TokenKind::Not),
            ("or", TokenKind::Or),
            ("readonly", TokenKind::Readonly),
            ("repeat", TokenKind::Repeat),
            ("rethrow", TokenKind::Rethrow),
            ("return", TokenKind::Return),
            ("super", TokenKind::Super),
            ("then", TokenKind::Then),
            ("throw", TokenKind::Throw),
            ("true", TokenKind::True),
            ("try", TokenKind::Try),
            ("type", TokenKind::Type),
            ("until", TokenKind::Until),
            ("while", TokenKind::While),
        ];

        for (kw, expected) in keywords.iter() {
            let tokens = lex(kw);
            assert_eq!(
                tokens[0].kind, *expected,
                "Keyword '{}' should produce {:?}",
                kw, expected
            );
        }
    }

    #[test]
    fn test_hex_numbers() {
        let tokens = lex("0xFF 0x0 0xABC");
        assert!(matches!(&tokens[0].kind, TokenKind::Number(s) if s == "0xFF"));
        assert!(matches!(&tokens[1].kind, TokenKind::Number(s) if s == "0x0"));
        assert!(matches!(&tokens[2].kind, TokenKind::Number(s) if s == "0xABC"));
    }

    #[test]
    fn test_binary_numbers() {
        let tokens = lex("0b1010 0b0 0b1111");
        assert!(matches!(&tokens[0].kind, TokenKind::Number(s) if s == "0b1010"));
        assert!(matches!(&tokens[1].kind, TokenKind::Number(s) if s == "0b0"));
        assert!(matches!(&tokens[2].kind, TokenKind::Number(s) if s == "0b1111"));
    }

    #[test]
    fn test_scientific_notation() {
        let tokens = lex("1e10 1.5e-3 2E+5");
        assert!(matches!(&tokens[0].kind, TokenKind::Number(s) if s == "1e10"));
        assert!(matches!(&tokens[1].kind, TokenKind::Number(s) if s == "1.5e-3"));
        assert!(matches!(&tokens[2].kind, TokenKind::Number(s) if s == "2E+5"));
    }

    #[test]
    fn test_compound_assignment() {
        let tokens = lex("+= -= *= /= %= ^= ..=");
        assert_eq!(tokens[0].kind, TokenKind::PlusEqual);
        assert_eq!(tokens[1].kind, TokenKind::MinusEqual);
        assert_eq!(tokens[2].kind, TokenKind::StarEqual);
        assert_eq!(tokens[3].kind, TokenKind::SlashEqual);
        assert_eq!(tokens[4].kind, TokenKind::PercentEqual);
        assert_eq!(tokens[5].kind, TokenKind::CaretEqual);
        assert_eq!(tokens[6].kind, TokenKind::DotDotEqual);
    }

    #[test]
    fn test_shift_operators() {
        let tokens = lex("<< >>");
        assert_eq!(tokens[0].kind, TokenKind::LessLess);
        assert_eq!(tokens[1].kind, TokenKind::GreaterGreater);
    }

    #[test]
    fn test_floor_divide() {
        let tokens = lex("//");
        assert_eq!(tokens[0].kind, TokenKind::SlashSlash);
    }

    #[test]
    fn test_concatenate() {
        let tokens = lex("..");
        assert_eq!(tokens[0].kind, TokenKind::DotDot);
    }

    #[test]
    fn test_variadic() {
        let tokens = lex("...");
        assert_eq!(tokens[0].kind, TokenKind::DotDotDot);
    }

    #[test]
    fn test_colon_colon() {
        let tokens = lex("::");
        assert_eq!(tokens[0].kind, TokenKind::ColonColon);
    }

    #[test]
    fn test_question_dot() {
        let tokens = lex("?.");
        assert_eq!(tokens[0].kind, TokenKind::QuestionDot);
    }

    #[test]
    fn test_bang_bang() {
        let tokens = lex("!!");
        assert_eq!(tokens[0].kind, TokenKind::BangBang);
    }

    #[test]
    fn test_null_coalesce() {
        let tokens = lex("??");
        assert_eq!(tokens[0].kind, TokenKind::QuestionQuestion);
    }

    #[test]
    fn test_pipe() {
        let tokens = lex("|");
        assert_eq!(tokens[0].kind, TokenKind::Pipe);
    }

    #[test]
    fn test_question() {
        let tokens = lex("?");
        assert_eq!(tokens[0].kind, TokenKind::Question);
    }

    #[test]
    fn test_at() {
        let tokens = lex("@");
        assert_eq!(tokens[0].kind, TokenKind::At);
    }

    #[test]
    fn test_is_keyword() {
        let tokens = lex("is");
        assert_eq!(tokens[0].kind, TokenKind::Is);
    }

    #[test]
    fn test_extends_keyword() {
        let tokens = lex("extends");
        assert_eq!(tokens[0].kind, TokenKind::Extends);
    }

    #[test]
    fn test_implements_keyword() {
        let tokens = lex("implements");
        assert_eq!(tokens[0].kind, TokenKind::Implements);
    }

    #[test]
    fn test_abstract_keyword() {
        let tokens = lex("abstract");
        assert_eq!(tokens[0].kind, TokenKind::Abstract);
    }

    #[test]
    fn test_class_keyword() {
        let tokens = lex("class");
        assert_eq!(tokens[0].kind, TokenKind::Class);
    }

    #[test]
    fn test_constructor_keyword() {
        let tokens = lex("constructor");
        assert_eq!(tokens[0].kind, TokenKind::Constructor);
    }

    #[test]
    fn test_declare_keyword() {
        let tokens = lex("declare");
        assert_eq!(tokens[0].kind, TokenKind::Declare);
    }

    #[test]
    fn test_from_keyword() {
        let tokens = lex("from");
        assert_eq!(tokens[0].kind, TokenKind::From);
    }

    #[test]
    fn test_get_keyword() {
        let tokens = lex("get");
        assert_eq!(tokens[0].kind, TokenKind::Get);
    }

    #[test]
    fn test_instanceof_keyword() {
        let tokens = lex("instanceof");
        assert_eq!(tokens[0].kind, TokenKind::Instanceof);
    }

    #[test]
    fn test_operator_keyword() {
        let tokens = lex("operator");
        assert_eq!(tokens[0].kind, TokenKind::Operator);
    }

    #[test]
    fn test_override_keyword() {
        let tokens = lex("override");
        assert_eq!(tokens[0].kind, TokenKind::Override);
    }

    #[test]
    fn test_private_keyword() {
        let tokens = lex("private");
        assert_eq!(tokens[0].kind, TokenKind::Private);
    }

    #[test]
    fn test_protected_keyword() {
        let tokens = lex("protected");
        assert_eq!(tokens[0].kind, TokenKind::Protected);
    }

    #[test]
    fn test_public_keyword() {
        let tokens = lex("public");
        assert_eq!(tokens[0].kind, TokenKind::Public);
    }

    #[test]
    fn test_set_keyword() {
        let tokens = lex("set");
        assert_eq!(tokens[0].kind, TokenKind::Set);
    }

    #[test]
    fn test_static_keyword() {
        let tokens = lex("static");
        assert_eq!(tokens[0].kind, TokenKind::Static);
    }

    #[test]
    fn test_throws_keyword() {
        let tokens = lex("throws");
        assert_eq!(tokens[0].kind, TokenKind::Throws);
    }

    #[test]
    fn test_catch_keyword() {
        let tokens = lex("catch");
        assert_eq!(tokens[0].kind, TokenKind::Catch);
    }

    #[test]
    fn test_finally_keyword() {
        let tokens = lex("finally");
        assert_eq!(tokens[0].kind, TokenKind::Finally);
    }

    #[test]
    fn test_as_keyword() {
        let tokens = lex("as");
        assert_eq!(tokens[0].kind, TokenKind::As);
    }

    #[test]
    fn test_comment_at_end() {
        let tokens = lex("const x = 5 -- comment at end");
        assert_eq!(tokens[0].kind, TokenKind::Const);
    }

    #[test]
    fn test_empty_source() {
        let tokens = lex("");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::Eof);
    }

    #[test]
    fn test_whitespace_only() {
        let tokens = lex("   \n\t  ");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::Eof);
    }

    #[test]
    fn test_single_char_tokens() {
        let tokens = lex("= ( ) { } [ ] , ; : . + - * / % ^ # & | ~ < > ! @ ?");
        assert_eq!(tokens[0].kind, TokenKind::Equal);
        assert_eq!(tokens[1].kind, TokenKind::LeftParen);
        assert_eq!(tokens[2].kind, TokenKind::RightParen);
        assert_eq!(tokens[3].kind, TokenKind::LeftBrace);
        assert_eq!(tokens[4].kind, TokenKind::RightBrace);
        assert_eq!(tokens[5].kind, TokenKind::LeftBracket);
        assert_eq!(tokens[6].kind, TokenKind::RightBracket);
        assert_eq!(tokens[7].kind, TokenKind::Comma);
        assert_eq!(tokens[8].kind, TokenKind::Semicolon);
        assert_eq!(tokens[9].kind, TokenKind::Colon);
        assert_eq!(tokens[10].kind, TokenKind::Dot);
        assert_eq!(tokens[11].kind, TokenKind::Plus);
        assert_eq!(tokens[12].kind, TokenKind::Minus);
        assert_eq!(tokens[13].kind, TokenKind::Star);
        assert_eq!(tokens[14].kind, TokenKind::Slash);
        assert_eq!(tokens[15].kind, TokenKind::Percent);
        assert_eq!(tokens[16].kind, TokenKind::Caret);
        assert_eq!(tokens[17].kind, TokenKind::Hash);
        assert_eq!(tokens[18].kind, TokenKind::Ampersand);
        assert_eq!(tokens[19].kind, TokenKind::Pipe);
        assert_eq!(tokens[20].kind, TokenKind::Tilde);
        assert_eq!(tokens[21].kind, TokenKind::LessThan);
        assert_eq!(tokens[22].kind, TokenKind::GreaterThan);
        assert_eq!(tokens[23].kind, TokenKind::Bang);
        assert_eq!(tokens[24].kind, TokenKind::At);
        assert_eq!(tokens[25].kind, TokenKind::Question);
    }

    #[test]
    fn test_invalid_character() {
        let (_, handler) = lex_with_handler("$");
        // Invalid characters should be reported but tokenization continues
        assert!(handler.has_errors() || handler.get_diagnostics().len() > 0);
    }

    #[test]
    fn test_identifier_with_numbers() {
        let (tokens, interner) = lex_with_interner("abc123 _test x1_y2");
        assert_eq!(tokens.len(), 4); // 3 identifiers + EOF
        if let TokenKind::Identifier(id) = tokens[0].kind {
            assert_eq!(interner.resolve(id), "abc123");
        }
        if let TokenKind::Identifier(id) = tokens[1].kind {
            assert_eq!(interner.resolve(id), "_test");
        }
        if let TokenKind::Identifier(id) = tokens[2].kind {
            assert_eq!(interner.resolve(id), "x1_y2");
        }
    }

    #[test]
    fn test_negative_numbers() {
        let tokens = lex("-5 -10.5");
        assert_eq!(tokens[0].kind, TokenKind::Minus);
        assert!(matches!(&tokens[1].kind, TokenKind::Number(s) if s == "5"));
        assert_eq!(tokens[2].kind, TokenKind::Minus);
        assert!(matches!(&tokens[3].kind, TokenKind::Number(s) if s == "10.5"));
    }

    #[test]
    fn test_adjacent_operators() {
        let tokens = lex("a+b*c");
        assert_eq!(tokens.len(), 6); // a + b * c EOF
    }

    #[test]
    fn test_deeply_nested_parens() {
        let tokens = lex("((((x))))");
        // 4 open parens + x + 4 close parens + EOF = 10 tokens
        assert_eq!(tokens.len(), 10);
    }
}
