# Lexer

The lexer converts TypedLua source code into a stream of tokens with position information.

## Usage

```rust
use typedlua_parser::{Lexer, StringInterner, DiagnosticHandler, TokenKind};
use std::sync::Arc;

let source = r#"const greeting: string = "Hello""#;

struct NoOpHandler;
impl DiagnosticHandler for NoOpHandler {
    fn report(&self, _d: typedlua_parser::diagnostics::Diagnostic) {}
    fn has_errors(&self) -> bool { false }
    fn error_count(&self) -> usize { 0 }
    fn warning_count(&self) -> usize { 0 }
    fn get_diagnostics(&self) -> Vec<typedlua_parser::diagnostics::Diagnostic> { Vec::new() }
}

let handler: Arc<dyn DiagnosticHandler> = Arc::new(NoOpHandler);
let interner = StringInterner::new();

let mut lexer = Lexer::new(source, handler, &interner);
let tokens = lexer.tokenize().unwrap();

for token in &tokens {
    println!("{:?}", token.kind);
}
```

## Token Kinds

### Keywords

```lua
function local const nil true false if then else elseif
end do while for in repeat until return break continue
class interface type enum import export declare namespace
new try catch throws as is extends super
```

### Operators

```lua
-- Arithmetic
+ - * / % ^

-- Assignment
= += -= *= /= %= ^= ..= &= |= //= <<= >>=

-- Comparison
== ~= < > <= >=

-- Logical
and or not

-- Bitwise
& | ~ << >>

-- Other
.. : :: . -> => ? ?? ?. ???
```

### Literals

- **Numbers**: `42`, `3.14`, `0xFF`, `0b1010`
- **Strings**: `"hello"`, `'world'`
- **Template strings**: `` `hello ${name}` ``

### Identifiers

- Regular identifiers: `foo`, `bar123`
- Self reference: `self`, `this`

## Token Structure

```rust
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}
```

## Span Information

Every token includes a `Span` indicating its position in source:

```rust
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: u32,
    pub column: u32,
}
```

## Template Strings

Template strings are lexed specially, with `${...}` expressions extracted:

```rust
let source = "`hello ${name}!`";
let mut lexer = Lexer::new(source, handler, &interner);
let tokens = lexer.tokenize().unwrap();

// Results in a single TemplateString token with parsed parts
```

The template string token contains:
- Static string parts
- Expression parts (sub-tokens to be parsed separately)

## Error Handling

The lexer returns `LexerError` on invalid input:

```rust
pub enum LexerError {
    UnterminatedString,
    InvalidNumber,
    UnrecognizedToken,
    // ...
}
```

## Optimizations

### Pre-allocation

The lexer estimates token count to minimize reallocations:

```rust
let estimated_tokens = (self.source.len() / 4).saturating_add(4);
let mut tokens = Vec::with_capacity(estimated_tokens);
```

### Memory Management

At the end of tokenization, excess capacity is trimmed:

```rust
tokens.shrink_to_fit();
```

## Lexing Phases

1. **Whitespace skipping**: Handles spaces, tabs, newlines
2. **Comment skipping**: Handles `--` comments
3. **Token recognition**: Identify keywords, operators, literals
4. **String interning**: Deduplicate identifier strings
5. **Template processing**: Extract embedded expressions