use crate::span::Span;
use crate::string_interner::StringId;

/// Token kind representing different types of lexical elements
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Keywords
    Const,
    Local,
    Function,
    Return,
    If,
    Elseif,
    Else,
    Then,
    End,
    While,
    Do,
    For,
    In,
    Break,
    Continue,
    Repeat,
    Until,
    And,
    Or,
    Not,
    True,
    False,
    Nil,
    Interface,
    Type,
    Enum,
    Export,
    Import,
    From,
    As,
    Match,
    When,
    Class,
    Extends,
    Implements,
    Public,
    Private,
    Protected,
    Static,
    Abstract,
    Readonly,
    Override,
    Final,
    Super,
    Keyof,
    Infer,
    Is,
    Instanceof,
    Declare,
    Namespace,
    Constructor,
    Get,
    Set,
    New,
    Operator,
    Throw,
    Try,
    Catch,
    Finally,
    Rethrow,
    Throws,

    // Identifiers and Literals
    Identifier(StringId),
    Number(String),
    String(String),
    TemplateString(Vec<TemplatePart>),

    // Operators
    Plus,                // +
    Minus,               // -
    Star,                // *
    Slash,               // /
    Percent,             // %
    Caret,               // ^
    Hash,                // #
    Ampersand,           // &
    Pipe,                // |
    Tilde,               // ~
    LessThan,            // <
    LessEqual,           // <=
    GreaterThan,         // >
    GreaterEqual,        // >=
    Equal,               // =
    EqualEqual,          // ==
    BangEqual,           // !=
    TildeEqual,          // ~=
    PlusEqual,           // +=
    MinusEqual,          // -=
    StarEqual,           // *=
    SlashEqual,          // /=
    PercentEqual,        // %=
    CaretEqual,          // ^=
    DotDotEqual,         // ..=
    AmpersandEqual,      // &=
    PipeEqual,           // |=
    SlashSlashEqual,     // //=
    LessLessEqual,       // <<=
    GreaterGreaterEqual, // >>=
    Dot,                 // .
    DotDot,              // ..
    DotDotDot,           // ...
    SlashSlash,          // //
    LessLess,            // <<
    GreaterGreater,      // >>
    Arrow,               // ->
    FatArrow,            // =>
    PipeOp,              // |>
    Question,            // ?
    QuestionQuestion,    // ??
    QuestionDot,         // ?.
    Colon,               // :
    ColonColon,          // ::
    Bang,                // !
    BangBang,            // !!
    At,                  // @

    // Delimiters
    LeftParen,    // (
    RightParen,   // )
    LeftBrace,    // {
    RightBrace,   // }
    LeftBracket,  // [
    RightBracket, // ]
    Comma,        // ,
    Semicolon,    // ;

    // Special
    Eof,
    Unknown(char),
}

/// Part of a template literal
#[derive(Debug, Clone, PartialEq)]
pub enum TemplatePart {
    String(String),
    Expression(Vec<Token>),
}

/// A token with its kind and location
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn eof(position: u32) -> Self {
        Self {
            kind: TokenKind::Eof,
            span: Span::new(position, position, 0, 0),
        }
    }
}

impl TokenKind {
    /// Check if this token kind is a keyword
    pub fn is_keyword(&self) -> bool {
        matches!(
            self,
            TokenKind::Const
                | TokenKind::Local
                | TokenKind::Function
                | TokenKind::Return
                | TokenKind::If
                | TokenKind::Elseif
                | TokenKind::Else
                | TokenKind::Then
                | TokenKind::End
                | TokenKind::While
                | TokenKind::Do
                | TokenKind::For
                | TokenKind::In
                | TokenKind::Break
                | TokenKind::Continue
                | TokenKind::Repeat
                | TokenKind::Until
                | TokenKind::And
                | TokenKind::Or
                | TokenKind::Not
                | TokenKind::True
                | TokenKind::False
                | TokenKind::Nil
                | TokenKind::Interface
                | TokenKind::Type
                | TokenKind::Enum
                | TokenKind::Export
                | TokenKind::Import
                | TokenKind::From
                | TokenKind::As
                | TokenKind::Match
                | TokenKind::When
                | TokenKind::Class
                | TokenKind::Extends
                | TokenKind::Implements
                | TokenKind::Public
                | TokenKind::Private
                | TokenKind::Protected
                | TokenKind::Static
                | TokenKind::Abstract
                | TokenKind::Readonly
                | TokenKind::Override
                | TokenKind::Final
                | TokenKind::Super
                | TokenKind::Keyof
                | TokenKind::Infer
                | TokenKind::Is
                | TokenKind::Instanceof
                | TokenKind::Declare
                | TokenKind::Namespace
                | TokenKind::Constructor
                | TokenKind::Get
                | TokenKind::Set
        )
    }

    /// Convert keyword TokenKind to its string representation
    pub fn to_keyword_str(&self) -> Option<&'static str> {
        match self {
            TokenKind::Const => Some("const"),
            TokenKind::Local => Some("local"),
            TokenKind::Function => Some("function"),
            TokenKind::Return => Some("return"),
            TokenKind::If => Some("if"),
            TokenKind::Elseif => Some("elseif"),
            TokenKind::Else => Some("else"),
            TokenKind::Then => Some("then"),
            TokenKind::End => Some("end"),
            TokenKind::While => Some("while"),
            TokenKind::Do => Some("do"),
            TokenKind::For => Some("for"),
            TokenKind::In => Some("in"),
            TokenKind::Break => Some("break"),
            TokenKind::Continue => Some("continue"),
            TokenKind::Repeat => Some("repeat"),
            TokenKind::Until => Some("until"),
            TokenKind::And => Some("and"),
            TokenKind::Or => Some("or"),
            TokenKind::Not => Some("not"),
            TokenKind::True => Some("true"),
            TokenKind::False => Some("false"),
            TokenKind::Nil => Some("nil"),
            TokenKind::Interface => Some("interface"),
            TokenKind::Type => Some("type"),
            TokenKind::Enum => Some("enum"),
            TokenKind::Export => Some("export"),
            TokenKind::Import => Some("import"),
            TokenKind::From => Some("from"),
            TokenKind::As => Some("as"),
            TokenKind::Match => Some("match"),
            TokenKind::When => Some("when"),
            TokenKind::Class => Some("class"),
            TokenKind::Extends => Some("extends"),
            TokenKind::Implements => Some("implements"),
            TokenKind::Public => Some("public"),
            TokenKind::Private => Some("private"),
            TokenKind::Protected => Some("protected"),
            TokenKind::Static => Some("static"),
            TokenKind::Abstract => Some("abstract"),
            TokenKind::Readonly => Some("readonly"),
            TokenKind::Override => Some("override"),
            TokenKind::Final => Some("final"),
            TokenKind::Super => Some("super"),
            TokenKind::Keyof => Some("keyof"),
            TokenKind::Infer => Some("infer"),
            TokenKind::Is => Some("is"),
            TokenKind::Instanceof => Some("instanceof"),
            TokenKind::Declare => Some("declare"),
            TokenKind::Namespace => Some("namespace"),
            TokenKind::Constructor => Some("constructor"),
            TokenKind::Get => Some("get"),
            TokenKind::Set => Some("set"),
            _ => None,
        }
    }

    /// Get keyword from string
    /// Uses length-based bucketing for O(1) length check + small match per bucket
    pub fn from_keyword(s: &str) -> Option<Self> {
        // Fast path: check length first to reduce string comparisons
        match s.len() {
            2 => match s {
                "do" => Some(TokenKind::Do),
                "if" => Some(TokenKind::If),
                "in" => Some(TokenKind::In),
                "or" => Some(TokenKind::Or),
                "as" => Some(TokenKind::As),
                "is" => Some(TokenKind::Is),
                _ => None,
            },
            3 => match s {
                "end" => Some(TokenKind::End),
                "for" => Some(TokenKind::For),
                "and" => Some(TokenKind::And),
                "not" => Some(TokenKind::Not),
                "nil" => Some(TokenKind::Nil),
                "get" => Some(TokenKind::Get),
                "set" => Some(TokenKind::Set),
                "new" => Some(TokenKind::New),
                "try" => Some(TokenKind::Try),
                _ => None,
            },
            4 => match s {
                "then" => Some(TokenKind::Then),
                "else" => Some(TokenKind::Else),
                "type" => Some(TokenKind::Type),
                "enum" => Some(TokenKind::Enum),
                "from" => Some(TokenKind::From),
                "when" => Some(TokenKind::When),
                "true" => Some(TokenKind::True),
                _ => None,
            },
            5 => match s {
                "const" => Some(TokenKind::Const),
                "local" => Some(TokenKind::Local),
                "while" => Some(TokenKind::While),
                "break" => Some(TokenKind::Break),
                "until" => Some(TokenKind::Until),
                "false" => Some(TokenKind::False),
                "match" => Some(TokenKind::Match),
                "class" => Some(TokenKind::Class),
                "super" => Some(TokenKind::Super),
                "keyof" => Some(TokenKind::Keyof),
                "infer" => Some(TokenKind::Infer),
                "final" => Some(TokenKind::Final),
                "throw" => Some(TokenKind::Throw),
                "catch" => Some(TokenKind::Catch),
                _ => None,
            },
            6 => match s {
                "return" => Some(TokenKind::Return),
                "elseif" => Some(TokenKind::Elseif),
                "repeat" => Some(TokenKind::Repeat),
                "import" => Some(TokenKind::Import),
                "export" => Some(TokenKind::Export),
                "public" => Some(TokenKind::Public),
                "static" => Some(TokenKind::Static),
                "throws" => Some(TokenKind::Throws),
                _ => None,
            },
            7 => match s {
                "private" => Some(TokenKind::Private),
                "extends" => Some(TokenKind::Extends),
                "declare" => Some(TokenKind::Declare),
                "finally" => Some(TokenKind::Finally),
                "rethrow" => Some(TokenKind::Rethrow),
                _ => None,
            },
            8 => match s {
                "function" => Some(TokenKind::Function),
                "continue" => Some(TokenKind::Continue),
                "abstract" => Some(TokenKind::Abstract),
                "operator" => Some(TokenKind::Operator),
                "readonly" => Some(TokenKind::Readonly),
                "override" => Some(TokenKind::Override),
                _ => None,
            },
            9 => match s {
                "interface" => Some(TokenKind::Interface),
                "protected" => Some(TokenKind::Protected),
                "namespace" => Some(TokenKind::Namespace),
                _ => None,
            },
            10 => match s {
                "implements" => Some(TokenKind::Implements),
                "instanceof" => Some(TokenKind::Instanceof),
                _ => None,
            },
            11 => match s {
                "constructor" => Some(TokenKind::Constructor),
                _ => None,
            },
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keyword_detection() {
        assert!(TokenKind::Const.is_keyword());
        assert!(TokenKind::Function.is_keyword());
        assert!(!TokenKind::Plus.is_keyword());
        assert!(!TokenKind::Identifier(StringId::from_u32(0)).is_keyword());
    }

    #[test]
    fn test_from_keyword() {
        assert_eq!(TokenKind::from_keyword("const"), Some(TokenKind::Const));
        assert_eq!(
            TokenKind::from_keyword("function"),
            Some(TokenKind::Function)
        );
        assert_eq!(
            TokenKind::from_keyword("interface"),
            Some(TokenKind::Interface)
        );
        assert_eq!(TokenKind::from_keyword("notakeyword"), None);
    }
}
