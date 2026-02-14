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

## Incremental Parsing

The parser supports incremental parsing behind the `incremental-parsing` feature flag (enabled by default). When a document is edited, only the affected statements are re-parsed — unmodified statements are reused from the previous parse tree.

### Architecture

```
┌──────────────────────────────────────────────────────────┐
│  LSP textDocument/didChange                              │
│  (edits: [{range, newText}, ...])                        │
└────────────────┬─────────────────────────────────────────┘
                 │
                 v
┌──────────────────────────────────────────────────────────┐
│  ParseStrategyAnalyzer (heuristics.rs)                   │
│  Decides: FullParse | Incremental | AppendOptimized      │
└────────────────┬─────────────────────────────────────────┘
                 │
                 v
┌──────────────────────────────────────────────────────────┐
│  Parser::parse_incremental()                             │
│                                                          │
│  1. Classify statements: clean vs dirty                  │
│     (is_statement_clean() — overlap check)               │
│  2. Reuse clean statements from previous tree            │
│  3. Re-lex + re-parse dirty regions only                 │
│  4. Build new IncrementalParseTree                       │
│  5. Run arena GC (collect_garbage)                       │
└────────────────┬─────────────────────────────────────────┘
                 │
                 v
┌──────────────────────────────────────────────────────────┐
│  IncrementalParseTree                                    │
│  ├── CachedStatement[] (refs into arenas)                │
│  ├── arenas: Vec<Rc<Bump>> (max 3, then consolidate)     │
│  ├── source_hash: u64 (FxHasher)                         │
│  └── version: u64                                        │
└──────────────────────────────────────────────────────────┘
```

### Statement-Level Caching

Statements are the unit of caching. Each `CachedStatement` stores:

- A reference to the arena-allocated `Statement` AST node
- The byte range and source hash for validation
- The token stream for that statement (for re-lexing avoidance)
- The arena generation index

A statement is **clean** if its byte range does not overlap any edit. Overlap uses half-open interval semantics: `stmt.start < edit.end && stmt.end > edit.start`.

### Arena Management

Incremental parsing uses multiple `bumpalo::Bump` arenas:

- Clean statements remain in their original arenas
- Re-parsed statements go into a new arena
- Arenas are consolidated (all cloned to one fresh arena) when:
  - More than 3 arenas accumulate, OR
  - Every 10 parse versions (periodic compaction)
- Unreferenced arenas (no statements point to them) are dropped

### Performance

Benchmarked on a 100-statement file with single-line edits:

| Scenario              | Speedup   | Notes                     |
|-----------------------|-----------|---------------------------|
| No edits (cache hit)  | ~10x      | Pointer copy only         |
| Single statement edit | 2.7-3.3x  | Re-parse 1 of 100         |
| All statements dirty  | ~1.0x     | Falls back to full parse  |

### Debug Logging

Set `LUANEXT_DEBUG_INCREMENTAL=1` to see parse decisions on stderr:
```
[incremental] No previous tree, doing full parse (1234 chars)
[incremental] Incremental parse: 98 clean, 2 dirty out of 100 total
[incremental] GC: 2 arenas after collection
```

### File Map

```
src/incremental/
├── mod.rs        # Module exports, debug_incremental! macro
├── cache.rs      # IncrementalParseTree, CachedStatement, arena GC
├── dirty.rs      # TextEdit, DirtyRegionSet, dirty region calculation
├── adjustment.rs # is_statement_clean() overlap check
├── offset.rs     # adjust_span() for offset recalculation
└── state.rs      # IncrementalState for LSP integration
```

## Error Recovery

The parser implements error recovery through synchronization:

- Skips to statement boundaries on parse errors
- Continues parsing after non-fatal errors
- Reports all errors, not just the first

## Thread Safety

- `DiagnosticHandler` trait requires `Send + Sync`
- `DiContainer` supports singleton and transient lifetimes
- Reference counting (`Arc`) for shared state