# TypedLua Parser Architecture

## Overview

The TypedLua parser is a hand-written recursive descent parser for TypedLua, a typed superset of Lua with TypeScript-inspired features. It consists of three main components:

1. **Lexer** - Tokenizes source code into tokens with position information
2. **Parser** - Builds an AST from tokens using recursive descent
3. **AST** - Structured representation of the parsed code

## Directory Structure

```
src/
├── lib.rs           # Main entry point and public API
├── ast/             # Abstract Syntax Tree definitions
│   ├── mod.rs       # AST module exports
│   ├── statement.rs # Statement nodes
│   ├── expression.rs # Expression nodes
│   ├── types.rs     # Type system nodes
│   └── pattern.rs   # Pattern matching nodes
├── lexer/           # Lexical analysis
│   └── mod.rs       # Lexer implementation
├── parser/          # Parsing logic
│   ├── mod.rs       # Parser core
│   ├── statement.rs # Statement parsing
│   ├── expression.rs # Expression parsing
│   ├── types.rs     # Type parsing
│   └── pattern.rs   # Pattern parsing
├── diagnostics/     # Error reporting
├── string_interner/ # String deduplication
└── span/            # Source location tracking
```

## Lexer

The lexer (`src/lexer/mod.rs`) converts source code into tokens:

- **Token kinds**: Keywords, operators, literals, identifiers
- **Position tracking**: Every token includes span information
- **String interning**: Identifiers are deduplicated using a string interner
- **Template strings**: Special handling for `${...}` expressions

### Lexer Optimization

The lexer uses pre-allocation hints based on source length to minimize reallocations:

```rust
let estimated_tokens = (self.source.len() / 4).saturating_add(4);
let mut tokens = Vec::with_capacity(estimated_tokens);
```

## Parser

The parser (`src/parser/`) uses recursive descent with Pratt parsing for expressions:

### Parser Structure

```
Parser
├── Token management (position, check, advance, match)
├── Statement parsing (functions, classes, variables, control flow)
├── Expression parsing (Pratt parser with precedence levels)
├── Type parsing (unions, intersections, generics)
└── Pattern parsing (destructuring, match patterns)
```

### Expression Parsing

Expressions use a Pratt parser with 15 precedence levels:

```
parse_assignment -> parse_conditional -> parse_logical_or -> ... -> parse_primary
```

Each level handles operators at that precedence, with lower precedence functions calling higher precedence ones.

### Parser Optimization

Key functions are marked `#[inline]` for performance:

- `#[inline]` on hot path functions (parse_expression, parse_statement)
- Pre-allocated vectors with `Vec::with_capacity()`
- Fast paths for common patterns (simple identifiers, parenthesized expressions)

## AST

The AST is defined in `src/ast/` with three main node types:

### Statements (`statement.rs`)
- `VariableDeclaration` - local/const declarations
- `FunctionDeclaration` - function definitions
- `ClassDeclaration` - class definitions
- `InterfaceDeclaration` - interface definitions
- `TypeAliasDeclaration` - type aliases
- `EnumDeclaration` - enum definitions
- Control flow: `If`, `While`, `For`, `Repeat`, `Return`, etc.

### Expressions (`expression.rs`)
- `Literal` - nil, boolean, number, string
- `Identifier` - variable references
- `Binary` / `Unary` - operators
- `Call` / `MethodCall` - function calls
- `Member` / `Index` - property access
- `FunctionExpression` - anonymous functions
- `Arrow` - arrow functions
- `Object` / `Array` - collection literals
- `Template` - template strings

### Types (`types.rs`)
- `Primitive` - number, string, boolean, etc.
- `Reference` - type references
- `Union` / `Intersection` - composite types
- `Function` - function types
- `Generic` - generic type instantiations
- `Array` - array types
- `Object` - object type literals
- `Tuple` - tuple types

## String Interner

The `string_interner` module deduplicates string identifiers:

- Reduces memory usage for repeated identifiers
- Enables fast identity comparison
- Uses `rustc-hash` for fast hashing

## Diagnostics

Error reporting through the `DiagnosticHandler` trait:

- Collects parse errors with source spans
- Categorized error codes (E20xx series)
- Supports both collecting and streaming handlers

## Performance Considerations

### Benchmark Results (release mode)

| Operation | Time | Notes |
|-----------|------|-------|
| Function parse | ~1.1 µs | Per function with types |
| Variable parse | ~550 ns | Per variable with type |
| Nested expression | ~650 ns | Per nesting level |
| Template literal | ~1.1 µs | Per template with expressions |

### Optimization Strategies Applied

1. **Inlining**: `#[inline]` on hot path functions
2. **Pre-allocation**: `Vec::with_capacity()` for known sizes
3. **Fast paths**: Special handling for common patterns
4. **Minimal cloning**: Use references where possible
5. **Discriminant caching**: `std::mem::discriminant` comparisons

## Error Recovery

The parser implements error recovery through synchronization:

- Skips to statement boundaries on parse errors
- Continues parsing after non-fatal errors
- Reports all errors, not just the first

## Thread Safety

- `DiagnosticHandler` trait requires `Send + Sync`
- `DiContainer` supports singleton and transient lifetimes
- Reference counting (`Arc`) for shared state