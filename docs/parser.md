# Parser

The parser builds an Abstract Syntax Tree (AST) from a token stream using recursive descent parsing.

## Usage

### Basic Parsing

```rust
use typedlua_parser::{Parser, StringInterner, DiagnosticHandler, ParserError};
use std::sync::Arc;

let source = r#"
    function greet(name: string): string
        return "Hello, " .. name
    end
"#;

struct NoOpHandler;
impl DiagnosticHandler for NoOpHandler {
    fn report(&self, _d: typedlua_parser::diagnostics::Diagnostic) {}
    fn has_errors(&self) -> bool { false }
    fn error_count(&self) -> usize { 0 }
    fn warning_count(&self) -> usize { 0 }
    fn get_diagnostics(&self) -> Vec<typedlua_parser::diagnostics::Diagnostic> { Vec::new() }
}

let handler: Arc<dyn DiagnosticHandler> = Arc::new(NoOpHandler);
let (interner, common) = StringInterner::new_with_common_identifiers();
let mut lexer = Lexer::new(source, handler.clone(), &interner);
let tokens = lexer.tokenize().unwrap();

let mut parser = Parser::new(tokens, handler, &interner, &common);
let program = parser.parse().unwrap();

println!("Parsed {} statements", program.statements.len());
```

### Using DiContainer

```rust
use typedlua_parser::{DiContainer, ServiceLifetime, parse_with_container};
use std::sync::Arc;

let source = "const x: number = 42";
let mut container = DiContainer::new();
container.register(
    |_| Arc::new(CollectingDiagnosticHandler::new()) as Arc<dyn DiagnosticHandler>,
    ServiceLifetime::Transient,
);

let program = parse_with_container(source, &mut container).unwrap();
```

## Parser Structure

### Token Stream Management

```rust
pub struct Parser<'a> {
    tokens: Vec<Token>,
    position: usize,
    diagnostic_handler: Arc<dyn DiagnosticHandler>,
    interner: &'a StringInterner,
    common: &'a CommonIdentifiers,
    has_namespace: bool,
    is_first_statement: bool,
}
```

### Core Methods

#### Token Access

```rust
// Get current token
fn current(&self) -> &Token

// Check if at end
fn is_at_end(&self) -> bool

// Check token kind
fn check(&self, kind: &TokenKind) -> bool

// Look ahead
fn nth_token_kind(&self, n: usize) -> Option<&TokenKind>
```

#### Token Consumption

```rust
// Advance to next token
fn advance(&mut self) -> &Token

// Match and consume token
fn match_token(&mut self, kinds: &[TokenKind]) -> bool

// Expect and consume token
fn consume(&mut self, kind: TokenKind, message: &str) -> Result<&Token, ParserError>
```

## Statement Parsing

### Variable Declarations

```lua
local x = 1
const y: number = 2
local a, b = 1, 2
```

### Function Declarations

```lua
function greet(name: string): string
    return "Hello"
end

function add(a: number, b: number): number
    return a + b
end
```

### Class Declarations

```lua
class Point
    x: number
    y: number

    constructor(x: number, y: number)
        self.x = x
        self.y = y
    end

    distance(other: Point): number
        return math.sqrt((self.x - other.x)^2 + (self.y - other.y)^2)
    end
end
```

### Interface Declarations

```lua
interface Point
    x: number
    y: number
end

interface Shape
    area(): number
end
```

### Type Aliases

```lua
type Point = { x: number, y: number }
type NumberCallback = (number) -> number
```

### Control Flow

```lua
if condition then
    -- then block
elseif other_condition then
    -- elseif block
else
    -- else block
end

while condition do
    -- body
end

for i = 1, 10 do
    -- body
end

repeat
    -- body
until condition
```

## Expression Parsing

### Precedence Levels

The parser uses Pratt parsing with these precedence levels (lowest to highest):

1. `parse_assignment` - Assignment and arrow functions
2. `parse_conditional` - Ternary conditional `? :`
3. `parse_logical_or` - `or`
4. `parse_logical_and` - `and`
5. `parse_bitwise_or` - `|`
6. `parse_bitwise_xor` - `~`
7. `parse_bitwise_and` - `&`
8. `parse_equality` - `==` `~=`
9. `parse_comparison` - `<` `>` `<=` `>=`
10. `parse_concatenation` - `..`
11. `parse_shift` - `<<` `>>`
12. `parse_additive` - `+` `-`
13. `parse_multiplicative` - `*` `/` `%` `//`
14. `parse_power` - `^`
15. `parse_unary` - `-` `not` `#` `~`
16. `parse_postfix` - Member access, call, index
17. `parse_primary` - Literals, identifiers, groups

### Literal Expressions

```lua
nil
true
false
42
3.14
"hello"
```

### Arithmetic

```lua
1 + 2
3 - 4
5 * 6
7 / 8
9 % 10
2 ^ 3  -- power
```

### Comparison

```lua
1 == 2
3 ~= 4
5 < 6
7 > 8
9 <= 10
11 >= 12
```

### Logical

```lua
true and false
true or false
not true
```

### Bitwise

```lua
1 | 2   -- or
3 & 4   -- and
5 ~ 6   -- xor
7 << 8  -- shift left
9 >> 10 -- shift right
```

### String Concatenation

```lua
"hello" .. " " .. "world"
```

### Function Calls

```lua
foo()
obj:method(arg1, arg2)
func(arg1)(arg2)  -- chained calls
```

### Member Access

```lua
obj.property
obj[index]
obj?.property  -- optional chaining
obj?[index]
```

### Arrow Functions

```lua
(x: number): number => x * 2
(x, y) => x + y
```

### Object and Array Literals

```lua
{ x = 1, y = 2 }
[1, 2, 3]
```

### Template Strings

```lua
`Hello, ${name}!`
`The sum is ${a + b}`
```

## Type Parsing

### Primitive Types

```lua
nil
boolean
number
integer
string
unknown
never
void
```

### Type References

```lua
Point
Array<number>
Map<string, number>
```

### Union and Intersection

```lua
number | string       -- union
A & B & C            -- intersection
```

### Function Types

```lua
(x: number) -> number
(a: number, b: number): number => a + b
```

### Generic Types

```lua
class Container<T>
    value: T
end

interface Mapper<T, R>
    map(value: T): R
end
```

### Object Types

```lua
{ x: number, y: number }
{ [K in keyof T]: T[K] }
```

### Type Predicates

```lua
function isPoint(value: unknown): value is Point
```

## Error Recovery

The parser implements error recovery through synchronization:

```rust
fn synchronize(&mut self) {
    self.advance();
    while !self.is_at_end() {
        match &self.current().kind {
            TokenKind::Function | TokenKind::Local | TokenKind::Const => return,
            TokenKind::End | TokenKind::Elseif | TokenKind::Else | TokenKind::Until => return,
            _ => self.advance(),
        }
    }
}
```

## Optimization

The parser uses several optimization strategies:

1. **`#[inline]` attributes** on hot path functions
2. **Pre-allocated vectors** with `Vec::with_capacity()`
3. **Fast paths** for common patterns (simple identifiers)
4. **Discriminant caching** for token comparisons