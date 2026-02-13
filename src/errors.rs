use thiserror::Error;

/// Errors that can occur during lexical analysis
#[derive(Debug, Error, Clone, PartialEq)]
pub enum LexerError {
    #[error("Unterminated string literal")]
    UnterminatedString,

    #[error("Unterminated comment")]
    UnterminatedComment,

    #[error("Invalid escape sequence: {0}")]
    InvalidEscapeSequence(char),

    #[error("Invalid number literal: {0}")]
    InvalidNumber(String),

    #[error("Unexpected character: {0}")]
    UnexpectedCharacter(char),

    #[error("Invalid byte offset: {0}")]
    InvalidByteOffset(u32),

    #[error("Invalid byte range: {0} to {1}")]
    InvalidRange(u32, u32),
}
