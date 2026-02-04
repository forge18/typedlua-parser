# TypedLua Parser

A high-performance lexer, parser, and AST for TypedLua - a statically-typed superset of Lua with TypeScript-inspired features.

[![Crates.io](https://img.shields.io/crates/v/typedlua-parser)](https://crates.io/crates/typedlua-parser)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Features

- **Fast Lexer** - Optimized tokenization with pre-allocation and string interning
- **Full Parser** - Hand-written recursive descent parser for TypedLua syntax
- **Complete AST** - Serde-serializable abstract syntax tree
- **Type System** - Support for classes, interfaces, generics, unions, and more

### Supported Syntax

- **Declarations**: `local`, `const`, `function`, `class`, `interface`, `type`, `enum`
- **Types**: Primitives, generics, unions, intersections, function types
- **Expressions**: Arithmetic, comparison, logical, bitwise, ternary
- **Functions**: Named functions, anonymous functions, arrow functions
- **Classes**: Properties, methods, constructors, inheritance
- **Control Flow**: `if`, `while`, `for`, `repeat`, `return`, `break`, `continue`
- **Modern Features**: Optional chaining (`?.`), null coalescing (`??`), template strings

## Quick Start

### Add to Cargo.toml

```toml
[dependencies]
typedlua-parser = "0.1.0"
```

### Basic Usage

```rust
use typedlua_parser::{parse_with_container, DiContainer, ServiceLifetime};

let source = r#"
    function greet(name: string): string
        return "Hello, " .. name
    end

    const result = greet("World")
"#;

let mut container = DiContainer::new();
container.register(
    |_| std::sync::Arc::new(
        typedlua_parser::CollectingDiagnosticHandler::new()
    ) as std::sync::Arc<dyn typedlua_parser::DiagnosticHandler>,
    ServiceLifetime::Transient,
);

match parse_with_container(source, &mut container) {
    Ok(program) => println!("Parsed {} statements", program.statements.len()),
    Err(e) => eprintln!("Parse error: {}", e),
}
```

### Manual Lexing and Parsing

```rust
use typedlua_parser::{Lexer, Parser, StringInterner, CollectingDiagnosticHandler};
use std::sync::Arc;

let source = "const x: number = 42";

let handler: Arc<dyn typedlua_parser::DiagnosticHandler> =
    Arc::new(CollectingDiagnosticHandler::new());
let (interner, common) = StringInterner::new_with_common_identifiers();

let mut lexer = Lexer::new(source, handler.clone(), &interner);
let tokens = lexer.tokenize().unwrap();

let mut parser = Parser::new(tokens, handler, &interner, &common);
let program = parser.parse().unwrap();

println!("Parsed {} statements", program.statements.len());
```

## Performance

The parser is optimized for speed with:

- `#[inline]` hints on hot path functions
- Pre-allocated vectors with capacity hints
- Fast paths for common patterns
- String interning for identifier deduplication

### Benchmark Results (release mode)

| Operation | Time | Notes |
|-----------|------|-------|
| Function parse | ~1.1 µs | Per function with type annotations |
| Variable parse | ~550 ns | Per variable with type annotation |
| Nested expression | ~650 ns | Per nesting level |
| Template literal | ~1.1 µs | Per template with expressions |

Run benchmarks with:

```bash
cargo bench
```

## Documentation

- [Architecture](docs/architecture.md) - Overview of parser design
- [Lexer](docs/lexer.md) - Lexer usage and tokens
- [Parser](docs/parser.md) - Parser usage and syntax
- [AST](docs/ast.md) - AST node types and structure

## Example Programs

### Classes

```lua
class Point
    x: number
    y: number

    constructor(x: number, y: number)
        self.x = x
        self.y = y
    end

    distance(other: Point): number
        local dx = self.x - other.x
        local dy = self.y - other.y
        return math.sqrt(dx * dx + dy * dy)
    end
end
```

### Interfaces and Generics

```lua
interface Container<T>
    value: T

    get(): T
    set(value: T): void
end

class Box<T> implements Container<T>
    value: T

    constructor(value: T)
        self.value = value
    end

    get(): T
        return self.value
    end

    set(value: T): void
        self.value = value
    end
end
```

### Arrow Functions and Type Guards

```lua
const double = (x: number): number => x * 2

function isPoint(value: unknown): value is Point
    return type(value) == "table" and value.x and value.y
end

const maybePoint: unknown = { x = 1, y = 2 }
if isPoint(maybePoint) then
    -- TypeScript knows maybePoint is Point here
    print(maybePoint.x, maybePoint.y)
end
```

### Pattern Matching

```lua
local result = match value with
    0 => "zero"
    | 1 => "one"
    | n if n > 10 => "big: " .. tostring(n)
    | _ => "other"
end
```

### Template Strings

```lua
const name = "TypedLua"
const version = 1.0
const message = `Hello, ${name}!
Version: ${version}
Computed: ${1 + 2 + 3}`
```

## Error Handling

```rust
use typedlua_parser::{CollectingDiagnosticHandler, Diagnostic};

let source = "const x: number = not_a_number";

let handler = CollectingDiagnosticHandler::new();
let (interner, common) = StringInterner::new_with_common_identifiers();

let mut lexer = Lexer::new(source, Arc::new(handler.clone()), &interner);
let tokens = lexer.tokenize();

let diagnostics = handler.get_diagnostics();
for diag in &diagnostics {
    println!("Error at line {}: {}", diag.span.line, diag.message);
}
```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- Inspired by TypeScript's type system design
- Built on the Lua language syntax
- Uses `rustc-hash` for fast hashing
- Uses `serde` for serialization support