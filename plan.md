# ADAM Programming Language — Master Plan

> **RULE: This document is NEVER modified unless the user explicitly requests removal of an expectation that cannot be achieved. This is a sacred reference contract.**

---

## Table of Contents

- [Language Overview](#language-overview)
- [Phase 0: Project Scaffolding](#phase-0-project-scaffolding)
- [Phase 1: Lexer](#phase-1-lexer)
- [Phase 2: AST Definition](#phase-2-ast-definition)
- [Phase 3: Parser](#phase-3-parser)
- [Phase 4: Name Resolution](#phase-4-name-resolution)
- [Phase 5: Type System & Checker](#phase-5-type-system--checker)
- [Phase 6: Borrow Checker](#phase-6-borrow-checker)
- [Phase 7: Adam IR (Mid-Level IR)](#phase-7-adam-ir-mid-level-ir)
- [Phase 8: LLVM Code Generation](#phase-8-llvm-code-generation)
- [Phase 9: Concurrency Runtime](#phase-9-concurrency-runtime)
- [Phase 10: Standard Library](#phase-10-standard-library)
- [Phase 11: UI Framework](#phase-11-ui-framework)
- [Phase 12: Platform Bridges (iOS & Android)](#phase-12-platform-bridges-ios--android)
- [Phase 13: Tooling](#phase-13-tooling)
- [Phase 14: Testing Infrastructure](#phase-14-testing-infrastructure)
- [Phase 15: Documentation & Examples](#phase-15-documentation--examples)

---

## Language Overview

**Name:** Adam
**File Extension:** `.adam`
**Compiler Implementation Language:** Rust
**Compilation Backend:** LLVM
**Memory Model:** Ownership + Borrowing (Rust-inspired, simplified — no lifetime annotations)
**Concurrency Model:** Green threads + Channels (Go-inspired)
**UI Paradigm:** Declarative views (SwiftUI-inspired, built into the language)
**Target Platforms:** iOS (ARM64), Android (ARM64), macOS/Linux/Windows (development)

### Design Principles

1. Immutable by default
2. Borrow by default in function parameters
3. No lifetime annotations — compiler infers via region analysis
4. No semicolons — newline-terminated statements
5. No null — optional types (`?T`) instead
6. Expression-based — everything returns a value
7. UTF-8 strings only
8. 27 keywords total (Go: 25, Rust: 39)

### Keywords

```
fn  struct  enum  trait  impl  view
let  mut  own  pub  use  mod
if  else  match  for  while  loop  break  continue  return
spawn  select  after  chan
true  false  nil
self  Self
```

### Type Table

| Type | Description |
|------|-------------|
| `i8, i16, i32, i64` | Signed integers |
| `u8, u16, u32, u64` | Unsigned integers |
| `f32, f64` | Floating point |
| `bool` | Boolean |
| `char` | Unicode scalar value |
| `String` | Owned, heap-allocated UTF-8 string |
| `str` | Borrowed string slice |
| `[T]` | Dynamic array (like Vec) |
| `[T; N]` | Fixed-size array |
| `(T, U, ...)` | Tuple |
| `?T` | Optional (nil or T) |
| `T ! E` | Result (value T or error E) |
| `fn(T) -> U` | Function type |
| `chan[T]` | Channel type |
| `Map[K, V]` | Hash map |
| `Set[T]` | Hash set |
| `Box[T]` | Heap-allocated owned pointer |
| `Rc[T]` | Reference-counted pointer (single-thread) |
| `Arc[T]` | Atomic reference-counted pointer (multi-thread) |

### Syntax Reference

```adam
// --- Variables ---
x := 5                  // immutable, type inferred
mut count := 0          // mutable
name: String = "adam"   // explicit type

// --- Functions ---
fn add(a i32, b i32) -> i32 {
    a + b
}

// Params are BORROWED by default
fn greet(name String) {
    print("Hello, {name}")
}

// 'own' takes ownership
fn consume(own name String) {
    // name moved in, dropped at end
}

// 'mut' for mutable borrow
fn append(mut name String) {
    name.push(" world")
}

// Multiple returns
fn divide(a f64, b f64) -> (f64, ?Error) {
    if b == 0 {
        return 0, Error("division by zero")
    }
    return a / b, nil
}

// --- Structs ---
struct Point {
    x f64
    y f64
}

impl Point {
    fn new(x f64, y f64) -> Point {
        Point { x, y }
    }

    fn distance(self, other Point) -> f64 {
        ((self.x - other.x).pow(2) + (self.y - other.y).pow(2)).sqrt()
    }
}

// --- Enums (ADTs) ---
enum Shape {
    Circle(f64)
    Rect(f64, f64)
    Triangle(f64, f64, f64)
}

// --- Pattern Matching ---
fn area(shape Shape) -> f64 {
    match shape {
        Circle(r) => 3.14159 * r * r
        Rect(w, h) => w * h
        Triangle(a, b, c) => {
            s := (a + b + c) / 2
            (s * (s-a) * (s-b) * (s-c)).sqrt()
        }
    }
}

// --- Traits ---
trait Drawable {
    fn draw(self)
}

impl Drawable for Shape {
    fn draw(self) { /* ... */ }
}

// --- Generics ---
fn max[T: Comparable](a T, b T) -> T {
    if a > b { a } else { b }
}

// --- Error Handling ---
fn read_file(path String) -> String ! IoError {
    file := fs.open(path)?
    content := file.read_all()?
    content
}

match read_file("data.txt") {
    ok(data) => process(data)
    err(e) => print("Failed: {e}")
}

// --- Optionals ---
fn find(items [String], target String) -> ?String {
    for item in items {
        if item == target {
            return item
        }
    }
    nil
}

if val := find(names, "adam") {
    print("Found: {val}")
}

// --- Concurrency ---
spawn {
    heavy_computation()
}

ch := chan[i32]()
spawn { ch.send(42) }
val := ch.recv()

ch := chan[i32](100)   // buffered

select {
    val := ch1.recv() => process(val)
    ch2.send(result) => {}
    after 1.seconds => print("timeout")
}

group := spawn_group()
for url in urls {
    group.spawn { fetch(url) }
}
results := group.wait()

// --- UI ---
view Counter {
    @state count: i32 = 0

    body {
        Column {
            Text("Count: {count}")
                .font(.title)
                .color(.blue)

            Row(spacing: 12) {
                Button("−") { count -= 1 }
                Button("+") { count += 1 }
            }
        }
        .padding(16)
    }
}

view Greeting {
    @prop name: String
    @state visible: bool = true

    body {
        if visible {
            Text("Hello, {name}!")
                .on_tap { visible = false }
        }
    }
}

// --- Modules ---
use math.vector.Vec3
use std.io.{read, write}

pub struct Point {
    pub x f64
    y f64       // private
}
```

### Ownership Rules

1. Every value has exactly one owner
2. When the owner goes out of scope, the value is dropped (destructor runs)
3. Assignment moves ownership (for non-Copy types)
4. Function parameters borrow by default (shared, immutable borrow)
5. `own` keyword in params takes ownership (value moved into function)
6. `mut` keyword in params takes a mutable borrow
7. Copy types (all numerics, bool, char) are implicitly copied, never moved
8. No lifetime annotations — compiler infers regions automatically
9. Compiler rejects code where safety can't be proven (with helpful error messages)

### Concurrency Rules

1. Green threads spawned with `spawn { }`, multiplexed on OS threads
2. Work-stealing scheduler distributes work across cores
3. Channels are typed, unbuffered (sync) by default, optionally buffered
4. `select` multiplexes over multiple channel operations
5. `Send` trait: type can be moved to another thread
6. `Sync` trait: type can be shared between threads via reference
7. Compiler enforces Send/Sync at compile time — no data races possible
8. `spawn_group` for structured concurrency (fork-join pattern)

---

## Phase 0: Project Scaffolding

### P0.S1 — Initialize Rust Workspace

**Description:** Create the Cargo workspace with all planned crate stubs.

**Deliverables:**
- `/Cargo.toml` — workspace root with members list
- `/crates/adam_lexer/Cargo.toml` + `src/lib.rs`
- `/crates/adam_ast/Cargo.toml` + `src/lib.rs`
- `/crates/adam_parser/Cargo.toml` + `src/lib.rs`
- `/crates/adam_resolve/Cargo.toml` + `src/lib.rs`
- `/crates/adam_types/Cargo.toml` + `src/lib.rs`
- `/crates/adam_borrow/Cargo.toml` + `src/lib.rs`
- `/crates/adam_ir/Cargo.toml` + `src/lib.rs`
- `/crates/adam_codegen/Cargo.toml` + `src/lib.rs`
- `/crates/adam_runtime/Cargo.toml` + `src/lib.rs`
- `/crates/adam_std/Cargo.toml` + `src/lib.rs`
- `/crates/adam_ui/Cargo.toml` + `src/lib.rs`
- `/crates/adam_cli/Cargo.toml` + `src/main.rs`
- `/crates/adam_lsp/Cargo.toml` + `src/lib.rs`

**Acceptance Criteria:**
- `cargo build` succeeds with zero errors
- `cargo test` runs (no tests yet, but no failures)
- Each crate compiles independently

**Dependencies:** None

---

### P0.S2 — Directory Structure

**Description:** Create all supporting directories for tests, examples, standard library source, and runtime code.

**Deliverables:**
- `/tests/lexer/` — lexer test fixtures
- `/tests/parser/` — parser test fixtures
- `/tests/typecheck/` — type checker test fixtures
- `/tests/borrow/` — borrow checker test fixtures
- `/tests/codegen/` — codegen test fixtures
- `/tests/integration/` — end-to-end test fixtures
- `/examples/` — example `.adam` programs
- `/std/` — Adam standard library source (written in Adam, used later)
- `/runtime/` — runtime support code (Rust + C glue)

**Acceptance Criteria:**
- All directories exist
- `.gitkeep` files in empty directories

**Dependencies:** P0.S1

---

### P0.S3 — Git Repository

**Description:** Initialize git repo with proper `.gitignore`.

**Deliverables:**
- `/.gitignore` — ignores `/target`, `*.o`, `*.so`, `*.dylib`, `.DS_Store`
- Initial commit with workspace scaffold

**Acceptance Criteria:**
- `git status` is clean after initial commit
- `cargo build` artifacts are not tracked

**Dependencies:** P0.S1, P0.S2

---

### P0.S4 — Example Adam Source Files

**Description:** Create example `.adam` files that serve as the test corpus. These are the programs the compiler must eventually handle.

**Deliverables:**
- `/examples/hello.adam` — `fn main() { print("Hello, Adam!") }`
- `/examples/fibonacci.adam` — recursive + iterative fibonacci
- `/examples/structs.adam` — struct definition, methods, trait impl
- `/examples/enums.adam` — enum definition, pattern matching
- `/examples/ownership.adam` — move, borrow, mutable borrow examples
- `/examples/channels.adam` — spawn, channels, select
- `/examples/counter.adam` — UI counter app (view, @state)
- `/examples/error_handling.adam` — Result type, ? operator
- `/examples/generics.adam` — generic functions and structs
- `/examples/modules.adam` — multi-file module example

**Acceptance Criteria:**
- All files contain syntactically valid Adam code (per our spec)
- Each file demonstrates at least one distinct language feature
- Files are well-commented explaining what they demonstrate

**Dependencies:** None (can be done in parallel with P0.S1)

---

## Phase 1: Lexer

**Crate:** `adam_lexer`
**Goal:** Convert Adam source text into a stream of tokens.

---

### P1.S1 — Token Type Definition

**Description:** Define the `Token` enum and `TokenKind` covering every lexical element of Adam.

**Deliverables:**
- `crates/adam_lexer/src/token.rs`

**Specific types to define:**
```rust
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,        // byte offset start..end in source
    pub line: u32,
    pub column: u32,
}

pub struct Span {
    pub start: u32,
    pub end: u32,
}

pub enum TokenKind {
    // Literals
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),     // after interpolation parsing
    CharLiteral(char),
    BoolLiteral(bool),

    // Identifier
    Identifier(String),

    // Keywords (one variant per keyword)
    Fn, Struct, Enum, Trait, Impl, View,
    Let, Mut, Own, Pub, Use, Mod,
    If, Else, Match, For, While, Loop, Break, Continue, Return,
    Spawn, Select, After, Chan,
    True, False, Nil,
    SelfValue,  // self
    SelfType,   // Self

    // Operators
    Plus, Minus, Star, Slash, Percent,          // + - * / %
    Eq, NotEq, Lt, Gt, LtEq, GtEq,            // == != < > <= >=
    And, Or, Not,                                // && || !
    Ampersand,                                   // &
    Assign,                                      // =
    ColonAssign,                                 // :=
    Arrow,                                       // ->
    FatArrow,                                    // =>
    Dot,                                         // .
    DotDot,                                      // ..
    Question,                                    // ?
    Bang,                                        // ! (in T ! E)

    // Delimiters
    LParen, RParen,                              // ( )
    LBrace, RBrace,                              // { }
    LBracket, RBracket,                          // [ ]
    Comma,                                       // ,
    Colon,                                       // :
    Semicolon,                                   // ; (optional, for multi-statement lines)

    // Special
    Newline,                                     // significant whitespace
    At,                                          // @ (for @state, @prop, @binding)
    StringInterpolStart,                         // start of {expr} in string
    StringInterpolEnd,                           // end of {expr} in string

    // Meta
    Eof,
    Error(String),                               // lexer error with message
}
```

**Acceptance Criteria:**
- Every token from the syntax reference can be represented
- `Token` carries source location (line, column, byte span)
- `Display` impl for `TokenKind` for readable error messages

**Dependencies:** P0.S1

---

### P1.S2 — Lexer Core Implementation

**Description:** Implement the main lexer that scans source text character by character and produces tokens.

**Deliverables:**
- `crates/adam_lexer/src/lexer.rs`

**Specific functions:**
```rust
pub struct Lexer<'src> {
    source: &'src str,
    chars: Peekable<CharIndices<'src>>,
    current_line: u32,
    current_column: u32,
    tokens: Vec<Token>,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Self;
    pub fn tokenize(mut self) -> LexResult;

    fn advance(&mut self) -> Option<(usize, char)>;
    fn peek(&mut self) -> Option<char>;
    fn peek_next(&mut self) -> Option<char>;

    fn skip_whitespace(&mut self);           // spaces and tabs only (not newlines)
    fn skip_line_comment(&mut self);         // // ...
    fn skip_block_comment(&mut self);        // /* ... */ (nested)

    fn lex_identifier_or_keyword(&mut self) -> Token;
    fn lex_number(&mut self) -> Token;       // int and float
    fn lex_string(&mut self) -> Token;       // "..." with interpolation
    fn lex_char(&mut self) -> Token;         // 'x'
    fn lex_operator(&mut self) -> Token;
    fn lex_newline(&mut self) -> Token;
}

pub struct LexResult {
    pub tokens: Vec<Token>,
    pub errors: Vec<LexError>,
}

pub struct LexError {
    pub message: String,
    pub line: u32,
    pub column: u32,
    pub span: Span,
}
```

**Acceptance Criteria:**
- Tokenizes all example `.adam` files without crashing
- Produces correct tokens for every syntax element
- Reports errors with line and column numbers
- Handles edge cases: empty file, only comments, only whitespace
- Nested block comments `/* /* inner */ outer */` work correctly

**Dependencies:** P1.S1

---

### P1.S3 — Keyword Recognition

**Description:** Implement keyword lookup — distinguish identifiers from keywords.

**Deliverables:**
- Keyword lookup table (HashMap or match statement) in `lexer.rs`

**Specific behavior:**
- Input `fn` → `TokenKind::Fn`
- Input `fnord` → `TokenKind::Identifier("fnord")`
- Input `self` → `TokenKind::SelfValue`
- Input `Self` → `TokenKind::SelfType`
- All 27 keywords correctly recognized
- Case-sensitive (no case folding)

**Acceptance Criteria:**
- All 27 keywords produce the correct `TokenKind`
- Identifiers starting with keywords are not misidentified (`format` is not `for` + `mat`)
- Performance: O(1) lookup via perfect hash or match

**Dependencies:** P1.S1

---

### P1.S4 — Number Literal Lexing

**Description:** Lex integer and floating-point literals with various bases and separators.

**Supported formats:**
- Decimal integers: `42`, `1_000_000`
- Hex integers: `0xFF`, `0xff`, `0xFF_FF`
- Octal integers: `0o77`
- Binary integers: `0b1010_1010`
- Floating point: `3.14`, `1.0e10`, `1.0e-10`, `1_000.5`
- No leading dot (`.5` is not valid — must be `0.5`)
- No trailing dot (`5.` is not valid — must be `5.0`)

**Deliverables:**
- Number lexing logic in `lexer.rs`

**Acceptance Criteria:**
- All formats above tokenize correctly
- Underscores in numbers are accepted and ignored
- `0x`, `0o`, `0b` without digits after prefix → error
- `1.2.3` → error
- Overflow of i64 → error with message

**Dependencies:** P1.S2

---

### P1.S5 — String Literal Lexing with Interpolation

**Description:** Lex string literals including escape sequences and `{expr}` interpolation.

**Supported features:**
- Double-quoted strings: `"hello"`
- Escape sequences: `\n`, `\t`, `\r`, `\\`, `\"`, `\0`, `\{` (literal brace)
- Unicode escapes: `\u{1F600}`
- String interpolation: `"Hello, {name}"` → tokens: `StringLiteral("Hello, ")`, `StringInterpolStart`, `Identifier("name")`, `StringInterpolEnd`
- Nested interpolation: `"result: {format(x)}"` — the interpolation can contain any expression
- Multi-line strings (string continues across newlines)
- Raw strings (future): `r"no \escapes"` — deferred to later

**Deliverables:**
- String lexing logic in `lexer.rs`

**Acceptance Criteria:**
- Simple strings tokenize correctly
- All escape sequences handled
- Interpolation produces correct token sequence
- Nested braces in interpolation (`"{map[key]}"`) work correctly
- Unterminated string → error with position of opening quote
- Invalid escape → error with position of backslash

**Dependencies:** P1.S2

---

### P1.S6 — Newline Handling (Significant Whitespace)

**Description:** Implement Go-style automatic semicolon insertion via newline significance.

**Rules (same as Go):**
A newline is treated as a statement terminator (significant) when the token before the newline is one of:
- An identifier
- A literal (int, float, string, char, bool)
- `break`, `continue`, `return`
- `)`  `}`  `]`
- `?`

Otherwise, the newline is ignored (allows multi-line expressions).

**Deliverables:**
- Newline significance logic in `lexer.rs`
- Non-significant newlines are filtered out

**Acceptance Criteria:**
- `x := 5\nprint(x)` → newline is significant between `5` and `print`
- `x := 5 +\n  3` → newline is NOT significant after `+`
- `fn foo(\n  a i32,\n  b i32\n)` → newlines inside parens not significant
- `return\n` → newline after `return` is significant
- All example files tokenize with correct significant newlines

**Dependencies:** P1.S2

---

### P1.S7 — Lexer Error Recovery

**Description:** When the lexer encounters an invalid character or sequence, it should record the error and continue lexing.

**Behavior:**
- Unknown character → emit `TokenKind::Error`, advance past it, continue
- Unterminated string → emit error token covering the string start, continue after next newline
- Unterminated block comment → emit error, consume to end of file
- Collect ALL errors, don't stop at first

**Deliverables:**
- Error recovery logic in `lexer.rs`
- `LexResult` contains both `tokens` and `errors` vectors

**Acceptance Criteria:**
- File with multiple errors reports ALL of them
- Tokens after an error are still correctly lexed (where possible)
- Each error has a human-readable message and precise source location

**Dependencies:** P1.S2

---

### P1.S8 — Lexer Test Suite

**Description:** Comprehensive test coverage for the lexer.

**Deliverables:**
- `crates/adam_lexer/src/tests.rs` (unit tests)
- `/tests/lexer/` — test fixture `.adam` files with expected token output

**Specific tests:**
- `test_empty_file` — produces only `Eof`
- `test_single_tokens` — each token kind individually
- `test_all_keywords` — all 27 keywords
- `test_identifiers` — various valid identifiers
- `test_integer_literals` — decimal, hex, octal, binary, with underscores
- `test_float_literals` — basic, scientific notation, with underscores
- `test_string_simple` — basic string
- `test_string_escapes` — all escape sequences
- `test_string_interpolation` — single and nested interpolation
- `test_string_interpolation_nested_braces` — `"{map[key]}"`
- `test_operators` — all operators, including multi-char ones
- `test_newline_significance` — all rules from P1.S6
- `test_comments` — line comments, block comments, nested block comments
- `test_error_recovery` — multiple errors in one file
- `test_example_files` — tokenize every file in `/examples/`
- `test_source_locations` — verify line/column on every token
- `test_edge_cases` — empty string `""`, zero `0`, single char identifiers

**Acceptance Criteria:**
- All tests pass
- At least 50 individual test cases
- 100% branch coverage of `TokenKind` variants (every variant is produced by at least one test)
- Lexer fuzz test runs for 10 seconds without panic (stretch goal)

**Dependencies:** P1.S1 through P1.S7

---

## Phase 2: AST Definition

**Crate:** `adam_ast`
**Goal:** Define every node type the parser will produce. This crate has NO logic — it's purely data structures. Every other crate depends on it.

---

### P2.S1 — Common Types

**Description:** Define shared types used throughout the AST.

**Deliverables:**
- `crates/adam_ast/src/common.rs`

**Specific types:**
```rust
/// Source location attached to every AST node
pub struct Span {
    pub start: u32,     // byte offset
    pub end: u32,
    pub line: u32,
    pub column: u32,
}

/// Every AST node is wrapped in Spanned<T> to carry location
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

/// Unique ID for each AST node (for later passes to attach metadata)
pub type NodeId = u32;

/// Identifier with location
pub struct Ident {
    pub name: String,
    pub span: Span,
}

/// Visibility
pub enum Visibility {
    Private,
    Public,     // pub
}

/// Mutability
pub enum Mutability {
    Immutable,
    Mutable,    // mut
}
```

**Acceptance Criteria:**
- All types are `Clone`, `Debug`, `PartialEq`
- `Span` can be merged (union of two spans for compound nodes)
- Zero logic — only data

**Dependencies:** P0.S1

---

### P2.S2 — Top-Level Item Nodes

**Description:** AST nodes for top-level declarations.

**Deliverables:**
- `crates/adam_ast/src/item.rs`

**Specific types:**
```rust
/// A complete source file
pub struct SourceFile {
    pub items: Vec<Spanned<Item>>,
}

/// Top-level item
pub enum Item {
    Function(FnDef),
    Struct(StructDef),
    Enum(EnumDef),
    Trait(TraitDef),
    Impl(ImplBlock),
    View(ViewDef),
    Use(UseDecl),
    Mod(ModDecl),
}

pub struct FnDef {
    pub visibility: Visibility,
    pub name: Ident,
    pub generic_params: Vec<GenericParam>,
    pub params: Vec<Param>,
    pub return_type: Option<Spanned<Type>>,     // None = void
    pub body: Block,
}

pub struct Param {
    pub ownership: ParamOwnership,
    pub name: Ident,
    pub ty: Spanned<Type>,
}

pub enum ParamOwnership {
    Borrow,         // default (no keyword)
    Own,            // own keyword
    MutBorrow,      // mut keyword
    SelfBorrow,     // self
    SelfMutBorrow,  // mut self
    SelfOwn,        // own self
}

pub struct StructDef {
    pub visibility: Visibility,
    pub name: Ident,
    pub generic_params: Vec<GenericParam>,
    pub fields: Vec<FieldDef>,
}

pub struct FieldDef {
    pub visibility: Visibility,
    pub name: Ident,
    pub ty: Spanned<Type>,
}

pub struct EnumDef {
    pub visibility: Visibility,
    pub name: Ident,
    pub generic_params: Vec<GenericParam>,
    pub variants: Vec<EnumVariant>,
}

pub struct EnumVariant {
    pub name: Ident,
    pub fields: Vec<Spanned<Type>>,    // positional fields
}

pub struct TraitDef {
    pub visibility: Visibility,
    pub name: Ident,
    pub generic_params: Vec<GenericParam>,
    pub methods: Vec<Spanned<FnDef>>,  // may have no body (required)
}

pub struct ImplBlock {
    pub generic_params: Vec<GenericParam>,
    pub trait_name: Option<Ident>,      // None for inherent impl
    pub target_type: Spanned<Type>,
    pub methods: Vec<Spanned<FnDef>>,
}

pub struct ViewDef {
    pub visibility: Visibility,
    pub name: Ident,
    pub fields: Vec<ViewField>,
    pub body: Block,                    // the body { ... } block
}

pub struct ViewField {
    pub attribute: ViewFieldAttr,
    pub name: Ident,
    pub ty: Spanned<Type>,
    pub default: Option<Spanned<Expr>>,
}

pub enum ViewFieldAttr {
    State,      // @state
    Prop,       // @prop
    Binding,    // @binding
}

pub struct UseDecl {
    pub path: Vec<Ident>,
    pub items: UseItems,
}

pub enum UseItems {
    All,                            // use foo.bar.*
    Single,                         // use foo.bar.Baz (last path element)
    Multiple(Vec<Ident>),           // use foo.{bar, baz}
}

pub struct ModDecl {
    pub visibility: Visibility,
    pub name: Ident,
}
```

**Acceptance Criteria:**
- Every top-level construct from the syntax reference has a corresponding AST node
- All nodes carry `Span` information
- Nodes are recursive where needed (e.g., `FnDef` contains `Block` which contains `Expr`)

**Dependencies:** P2.S1

---

### P2.S3 — Expression Nodes

**Description:** AST nodes for all expressions.

**Deliverables:**
- `crates/adam_ast/src/expr.rs`

**Specific types:**
```rust
pub enum Expr {
    // Literals
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    StringInterpolation(Vec<StringPart>),
    CharLiteral(char),
    BoolLiteral(bool),
    NilLiteral,

    // Identifiers and paths
    Identifier(Ident),
    Path(Vec<Ident>),               // foo.bar.baz

    // Operators
    Binary(Box<BinaryExpr>),
    Unary(Box<UnaryExpr>),

    // Function call
    Call(Box<CallExpr>),

    // Method call: expr.method(args)
    MethodCall(Box<MethodCallExpr>),

    // Field access: expr.field
    FieldAccess(Box<FieldAccessExpr>),

    // Index: expr[index]
    Index(Box<IndexExpr>),

    // Struct literal: Point { x: 1, y: 2 }
    StructLiteral(Box<StructLiteralExpr>),

    // Array literal: [1, 2, 3]
    ArrayLiteral(Vec<Spanned<Expr>>),

    // Tuple literal: (1, "hello")
    TupleLiteral(Vec<Spanned<Expr>>),

    // Block expression: { stmts; expr }
    Block(Block),

    // If expression
    If(Box<IfExpr>),

    // Match expression
    Match(Box<MatchExpr>),

    // Closure: |a, b| { a + b }
    Closure(Box<ClosureExpr>),

    // Assignment: x = 5
    Assign(Box<AssignExpr>),

    // Range: 1..10
    Range(Box<RangeExpr>),

    // Try: expr?
    Try(Box<Spanned<Expr>>),

    // Spawn: spawn { ... }
    Spawn(Block),

    // Channel operations
    ChanSend(Box<ChanSendExpr>),    // ch.send(val)
    ChanRecv(Box<Spanned<Expr>>),   // ch.recv()

    // Select expression
    Select(Box<SelectExpr>),

    // Return (expression, since everything is expr-based)
    Return(Option<Box<Spanned<Expr>>>),
    Break(Option<Box<Spanned<Expr>>>),
    Continue,
}

pub struct BinaryExpr {
    pub left: Spanned<Expr>,
    pub op: BinaryOp,
    pub right: Spanned<Expr>,
}

pub enum BinaryOp {
    Add, Sub, Mul, Div, Mod,
    Eq, NotEq, Lt, Gt, LtEq, GtEq,
    And, Or,
}

pub struct UnaryExpr {
    pub op: UnaryOp,
    pub operand: Spanned<Expr>,
}

pub enum UnaryOp {
    Neg,        // -
    Not,        // !
    Ref,        // &
}

pub struct CallExpr {
    pub callee: Spanned<Expr>,
    pub generic_args: Vec<Spanned<Type>>,
    pub args: Vec<Spanned<Expr>>,
}

pub struct MethodCallExpr {
    pub receiver: Spanned<Expr>,
    pub method: Ident,
    pub generic_args: Vec<Spanned<Type>>,
    pub args: Vec<Spanned<Expr>>,
}

pub struct FieldAccessExpr {
    pub object: Spanned<Expr>,
    pub field: Ident,
}

pub struct IndexExpr {
    pub object: Spanned<Expr>,
    pub index: Spanned<Expr>,
}

pub struct StructLiteralExpr {
    pub name: Ident,
    pub fields: Vec<(Ident, Spanned<Expr>)>,
}

pub struct IfExpr {
    pub condition: Spanned<Expr>,
    pub then_block: Block,
    pub else_block: Option<Block>,      // else or else if (nested)
}

pub struct MatchExpr {
    pub scrutinee: Spanned<Expr>,
    pub arms: Vec<MatchArm>,
}

pub struct MatchArm {
    pub pattern: Spanned<Pattern>,
    pub guard: Option<Spanned<Expr>>,
    pub body: Spanned<Expr>,
}

pub struct ClosureExpr {
    pub params: Vec<ClosureParam>,
    pub body: Spanned<Expr>,
}

pub struct ClosureParam {
    pub name: Ident,
    pub ty: Option<Spanned<Type>>,
}

pub struct AssignExpr {
    pub target: Spanned<Expr>,
    pub value: Spanned<Expr>,
}

pub struct RangeExpr {
    pub start: Spanned<Expr>,
    pub end: Spanned<Expr>,
    pub inclusive: bool,
}

pub struct ChanSendExpr {
    pub channel: Spanned<Expr>,
    pub value: Spanned<Expr>,
}

pub struct SelectExpr {
    pub arms: Vec<SelectArm>,
}

pub struct SelectArm {
    pub kind: SelectArmKind,
    pub body: Block,
}

pub enum SelectArmKind {
    Recv { binding: Ident, channel: Spanned<Expr> },
    Send { channel: Spanned<Expr>, value: Spanned<Expr> },
    After(Spanned<Expr>),
}

pub enum StringPart {
    Literal(String),
    Interpolation(Spanned<Expr>),
}
```

**Acceptance Criteria:**
- Every expression form from the syntax reference has a node
- All expressions can be nested arbitrarily (`Box` used for recursive types)
- Operator precedence is NOT encoded in the AST — that's the parser's job

**Dependencies:** P2.S1

---

### P2.S4 — Statement Nodes

**Description:** AST nodes for statements.

**Deliverables:**
- `crates/adam_ast/src/stmt.rs`

**Specific types:**
```rust
pub struct Block {
    pub stmts: Vec<Spanned<Stmt>>,
}

pub enum Stmt {
    /// let binding: x := 5, mut y := 10, name: String = "adam"
    Let(LetStmt),

    /// Expression as statement (function call, assignment, etc.)
    Expr(Spanned<Expr>),

    /// For loop: for item in collection { ... }
    For(ForStmt),

    /// While loop: while condition { ... }
    While(WhileStmt),

    /// Infinite loop: loop { ... }
    Loop(Block),

    /// Item declaration (function, struct, etc. inside a block)
    Item(Spanned<Item>),
}

pub struct LetStmt {
    pub mutability: Mutability,
    pub name: Ident,
    pub ty: Option<Spanned<Type>>,
    pub value: Spanned<Expr>,
    pub is_colon_assign: bool,    // true for :=, false for : Type =
}

pub struct ForStmt {
    pub binding: Ident,
    pub iterable: Spanned<Expr>,
    pub body: Block,
}

pub struct WhileStmt {
    pub condition: Spanned<Expr>,
    pub body: Block,
}
```

**Acceptance Criteria:**
- All statement forms representable
- `Block` can contain mixed statements and expressions
- Last expression in block (without newline terminator) is the block's value

**Dependencies:** P2.S1, P2.S3

---

### P2.S5 — Type Nodes

**Description:** AST representation of type annotations.

**Deliverables:**
- `crates/adam_ast/src/types.rs`

**Specific types:**
```rust
pub enum Type {
    /// Named type: i32, String, Point, Vec[T]
    Named(TypePath),

    /// Reference: &T
    Reference(Box<Spanned<Type>>),

    /// Mutable reference: &mut T
    MutReference(Box<Spanned<Type>>),

    /// Array: [T] (dynamic), [T; N] (fixed)
    Array(Box<ArrayType>),

    /// Tuple: (T, U, V)
    Tuple(Vec<Spanned<Type>>),

    /// Optional: ?T
    Optional(Box<Spanned<Type>>),

    /// Result: T ! E
    Result(Box<ResultType>),

    /// Function: fn(T, U) -> V
    Function(Box<FnType>),

    /// Channel: chan[T]
    Channel(Box<Spanned<Type>>),

    /// Inferred (no annotation, compiler figures it out)
    Inferred,
}

pub struct TypePath {
    pub name: Ident,
    pub generic_args: Vec<Spanned<Type>>,
}

pub struct ArrayType {
    pub element: Spanned<Type>,
    pub size: Option<Spanned<Expr>>,    // None = dynamic, Some = fixed
}

pub struct ResultType {
    pub ok: Spanned<Type>,
    pub err: Spanned<Type>,
}

pub struct FnType {
    pub params: Vec<Spanned<Type>>,
    pub return_type: Spanned<Type>,
}

pub struct GenericParam {
    pub name: Ident,
    pub bounds: Vec<Ident>,    // trait bounds: T: Comparable + Drawable
}
```

**Acceptance Criteria:**
- All type forms from the type table can be represented
- Generic type arguments: `Vec[i32]`, `Map[String, i32]`
- Nested types: `?[String]` (optional array of strings), `Vec[?i32]`

**Dependencies:** P2.S1

---

### P2.S6 — Pattern Nodes

**Description:** AST nodes for patterns used in `match`, `let`, `for`.

**Deliverables:**
- `crates/adam_ast/src/pattern.rs`

**Specific types:**
```rust
pub enum Pattern {
    /// Wildcard: _
    Wildcard,

    /// Binding: x (binds matched value to name)
    Binding(Ident),

    /// Literal: 5, "hello", true
    Literal(Spanned<Expr>),

    /// Enum variant: Circle(r), Some(val)
    Variant(VariantPattern),

    /// Struct: Point { x, y }
    Struct(StructPattern),

    /// Tuple: (a, b, c)
    Tuple(Vec<Spanned<Pattern>>),

    /// Or pattern: A | B
    Or(Vec<Spanned<Pattern>>),

    /// Rest: .. (in arrays/tuples)
    Rest,
}

pub struct VariantPattern {
    pub name: Ident,
    pub fields: Vec<Spanned<Pattern>>,
}

pub struct StructPattern {
    pub name: Ident,
    pub fields: Vec<(Ident, Spanned<Pattern>)>,
    pub has_rest: bool,     // Point { x, .. }
}
```

**Acceptance Criteria:**
- All pattern forms from the match examples work
- Patterns can be nested: `Some(Circle(r))`
- Guard expressions attach to `MatchArm`, not `Pattern`

**Dependencies:** P2.S1

---

### P2.S7 — AST Pretty Printer

**Description:** Implement a debug printer that outputs the AST in a readable tree format.

**Deliverables:**
- `crates/adam_ast/src/pretty.rs`

**Output format example:**
```
SourceFile
├── FnDef "main"
│   ├── Params: (none)
│   ├── ReturnType: (none)
│   └── Body
│       └── Call "print"
│           └── StringLiteral "Hello, Adam!"
```

**Acceptance Criteria:**
- Every AST node type has a pretty-print implementation
- Output is indented tree format
- Spans are optionally printed (flag to enable/disable)
- Can round-trip: parse → pretty-print → manually verify against source

**Dependencies:** P2.S1 through P2.S6

---

## Phase 3: Parser

**Crate:** `adam_parser`
**Goal:** Transform token stream into AST. Handwritten recursive descent parser with Pratt parsing for expressions.

---

### P3.S1 — Parser Infrastructure

**Description:** Set up the parser struct, token cursor, error handling, and lookahead.

**Deliverables:**
- `crates/adam_parser/src/parser.rs`

**Specific implementation:**
```rust
pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    errors: Vec<ParseError>,
}

pub struct ParseError {
    pub message: String,
    pub span: Span,
    pub expected: Vec<TokenKind>,   // what was expected
    pub found: TokenKind,            // what was found
}

pub struct ParseResult {
    pub ast: SourceFile,
    pub errors: Vec<ParseError>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self;
    pub fn parse(mut self) -> ParseResult;

    // Token navigation
    fn current(&self) -> &Token;
    fn peek(&self) -> &Token;
    fn peek_nth(&self, n: usize) -> &Token;
    fn advance(&mut self) -> &Token;
    fn expect(&mut self, kind: TokenKind) -> Result<&Token, ParseError>;
    fn eat(&mut self, kind: TokenKind) -> bool;     // consume if matches
    fn at(&self, kind: TokenKind) -> bool;           // check without consuming

    // Newline handling
    fn skip_newlines(&mut self);
    fn expect_newline_or_eof(&mut self) -> Result<(), ParseError>;

    // Error recovery
    fn synchronize(&mut self);      // skip tokens until next statement boundary
}
```

**Acceptance Criteria:**
- Parser can navigate token stream with arbitrary lookahead
- `expect` produces clear error messages: "expected `{`, found `(`"
- `synchronize` skips to next `fn`, `struct`, `enum`, etc. for recovery
- Multiple parse errors can be collected in a single parse run

**Dependencies:** P1 (all), P2 (all)

---

### P3.S2 — Top-Level Parsing

**Description:** Parse top-level items: functions, structs, enums, traits, impl blocks, views, use declarations, mod declarations.

**Deliverables:**
- `crates/adam_parser/src/item.rs`

**Specific functions:**
```rust
fn parse_item(&mut self) -> Result<Spanned<Item>, ParseError>;
fn parse_fn_def(&mut self) -> Result<FnDef, ParseError>;
fn parse_struct_def(&mut self) -> Result<StructDef, ParseError>;
fn parse_enum_def(&mut self) -> Result<EnumDef, ParseError>;
fn parse_trait_def(&mut self) -> Result<TraitDef, ParseError>;
fn parse_impl_block(&mut self) -> Result<ImplBlock, ParseError>;
fn parse_view_def(&mut self) -> Result<ViewDef, ParseError>;
fn parse_use_decl(&mut self) -> Result<UseDecl, ParseError>;
fn parse_mod_decl(&mut self) -> Result<ModDecl, ParseError>;
fn parse_generic_params(&mut self) -> Result<Vec<GenericParam>, ParseError>;
fn parse_param_list(&mut self) -> Result<Vec<Param>, ParseError>;
fn parse_visibility(&mut self) -> Visibility;
```

**Acceptance Criteria:**
- All example files parse without errors
- `pub` visibility correctly attached
- Generic params `[T: Comparable]` parsed
- Empty structs, enums with no variants, traits with no methods all handled
- Error recovery: bad struct field doesn't prevent parsing next item

**Dependencies:** P3.S1

---

### P3.S3 — Expression Parsing (Pratt Parser)

**Description:** Implement Pratt parsing for all expressions with correct operator precedence.

**Deliverables:**
- `crates/adam_parser/src/expr.rs`

**Operator precedence (lowest to highest):**
1. `=` (assignment)
2. `||` (logical or)
3. `&&` (logical and)
4. `==`, `!=` (equality)
5. `<`, `>`, `<=`, `>=` (comparison)
6. `..` (range)
7. `+`, `-` (additive)
8. `*`, `/`, `%` (multiplicative)
9. `!`, `-` (unary prefix)
10. `?` (try, postfix)
11. `.` (field access, method call — postfix)
12. `()` (function call — postfix)
13. `[]` (index — postfix)

**Specific functions:**
```rust
fn parse_expr(&mut self) -> Result<Spanned<Expr>, ParseError>;
fn parse_expr_with_precedence(&mut self, min_prec: u8) -> Result<Spanned<Expr>, ParseError>;
fn parse_prefix(&mut self) -> Result<Spanned<Expr>, ParseError>;
fn parse_infix(&mut self, left: Spanned<Expr>, prec: u8) -> Result<Spanned<Expr>, ParseError>;
fn parse_postfix(&mut self, left: Spanned<Expr>) -> Result<Spanned<Expr>, ParseError>;

// Specific expression parsers
fn parse_if_expr(&mut self) -> Result<Spanned<Expr>, ParseError>;
fn parse_match_expr(&mut self) -> Result<Spanned<Expr>, ParseError>;
fn parse_block_expr(&mut self) -> Result<Block, ParseError>;
fn parse_closure(&mut self) -> Result<Spanned<Expr>, ParseError>;
fn parse_struct_literal(&mut self) -> Result<Spanned<Expr>, ParseError>;
fn parse_array_literal(&mut self) -> Result<Spanned<Expr>, ParseError>;
fn parse_tuple_literal(&mut self) -> Result<Spanned<Expr>, ParseError>;
fn parse_spawn(&mut self) -> Result<Spanned<Expr>, ParseError>;
fn parse_select(&mut self) -> Result<Spanned<Expr>, ParseError>;
fn parse_string_interpolation(&mut self) -> Result<Spanned<Expr>, ParseError>;
```

**Acceptance Criteria:**
- `1 + 2 * 3` parses as `1 + (2 * 3)` (precedence correct)
- `a.b.c()` parses as `(a.b).c()` (left-to-right chaining)
- `foo()?` parses as `(foo())?` (try is postfix)
- `!a && b` parses as `(!a) && b` (unary binds tighter)
- `if x > 0 { x } else { -x }` parses as a single expression
- Nested match, if, and block expressions work
- String interpolation `"hello {name}"` parses correctly
- All example files parse without precedence errors

**Dependencies:** P3.S1, P2.S3

---

### P3.S4 — Statement Parsing

**Description:** Parse statements: let bindings, for loops, while loops, loop, expression statements.

**Deliverables:**
- `crates/adam_parser/src/stmt.rs`

**Specific functions:**
```rust
fn parse_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError>;
fn parse_let_stmt(&mut self) -> Result<LetStmt, ParseError>;
fn parse_for_stmt(&mut self) -> Result<ForStmt, ParseError>;
fn parse_while_stmt(&mut self) -> Result<WhileStmt, ParseError>;
fn parse_loop_stmt(&mut self) -> Result<Block, ParseError>;
fn parse_block(&mut self) -> Result<Block, ParseError>;
```

**Parsing rules for let:**
- `x := expr` → `LetStmt { mutability: Immutable, is_colon_assign: true, ty: None }`
- `mut x := expr` → `LetStmt { mutability: Mutable, is_colon_assign: true, ty: None }`
- `x: Type = expr` → `LetStmt { mutability: Immutable, is_colon_assign: false, ty: Some(Type) }`
- `mut x: Type = expr` → `LetStmt { mutability: Mutable, is_colon_assign: false, ty: Some(Type) }`

**Acceptance Criteria:**
- `:=` syntax works for both mutable and immutable
- Explicit type annotation syntax works
- `for x in collection { }` parses
- `while condition { }` parses
- `loop { }` parses
- Blocks parse with correct last-expression-is-value semantics
- Newlines correctly terminate statements

**Dependencies:** P3.S1, P3.S3, P2.S4

---

### P3.S5 — Type Parsing

**Description:** Parse type annotations in all positions (params, return types, let bindings, struct fields).

**Deliverables:**
- `crates/adam_parser/src/types.rs`

**Specific functions:**
```rust
fn parse_type(&mut self) -> Result<Spanned<Type>, ParseError>;
fn parse_named_type(&mut self) -> Result<TypePath, ParseError>;
fn parse_array_type(&mut self) -> Result<ArrayType, ParseError>;
fn parse_tuple_type(&mut self) -> Result<Vec<Spanned<Type>>, ParseError>;
fn parse_fn_type(&mut self) -> Result<FnType, ParseError>;
fn parse_optional_type(&mut self) -> Result<Spanned<Type>, ParseError>;
fn parse_result_type(&mut self) -> Result<ResultType, ParseError>;
fn parse_generic_args(&mut self) -> Result<Vec<Spanned<Type>>, ParseError>;
```

**Tricky cases:**
- `?[i32]` → Optional of Array of i32
- `[?i32]` → Array of Optional i32
- `fn(i32) -> String ! Error` → function returning Result
- `Map[String, Vec[i32]]` → nested generics
- `chan[?String]` → channel of optional string

**Acceptance Criteria:**
- All type forms from the type table parse correctly
- Generic args with nested generics: `Map[String, Vec[i32]]`
- `?T ! E` parses correctly (optional of result? or result of optional? — define precedence)
- Function types in any position

**Dependencies:** P3.S1, P2.S5

---

### P3.S6 — Pattern Parsing

**Description:** Parse patterns in match arms, let bindings (future: for bindings with destructuring).

**Deliverables:**
- `crates/adam_parser/src/pattern.rs`

**Specific functions:**
```rust
fn parse_pattern(&mut self) -> Result<Spanned<Pattern>, ParseError>;
fn parse_variant_pattern(&mut self) -> Result<VariantPattern, ParseError>;
fn parse_struct_pattern(&mut self) -> Result<StructPattern, ParseError>;
fn parse_tuple_pattern(&mut self) -> Result<Vec<Spanned<Pattern>>, ParseError>;
```

**Examples:**
- `_` → Wildcard
- `x` → Binding("x")
- `42` → Literal(42)
- `Circle(r)` → Variant with binding
- `Point { x, y }` → Struct with bindings
- `(a, b)` → Tuple
- `Some(Circle(r))` → nested
- `A | B` → Or pattern
- `Circle(..)` → Variant with rest

**Acceptance Criteria:**
- All pattern forms from examples parse
- Nested patterns work to arbitrary depth
- Or patterns: `A | B | C` parse as flat list
- Match arms with guards: `Circle(r) if r > 0 =>` parse correctly
- Exhaustive match checking is NOT done here (that's the type checker)

**Dependencies:** P3.S1, P2.S6

---

### P3.S7 — View Parsing

**Description:** Parse the `view` keyword and its unique syntax (SwiftUI-like DSL).

**Deliverables:**
- `crates/adam_parser/src/view.rs`

**What makes views special:**
- `@state`, `@prop`, `@binding` field attributes
- `body { }` block with implicit view builder
- Trailing closure syntax: `Button("tap") { action() }`
- Modifier chains: `.font(.title).color(.blue).padding(16)`
- Conditional views: `if condition { View }` inside body
- List comprehension: `List(items) { item in Text(item) }`

**Parsing strategy:**
- `view Name { ... }` is parsed like a special struct + body
- Inside `body { }`, every expression is a view expression
- View expressions can be chained with `.modifier(args)`
- Trailing closures are parsed as the last argument

**Acceptance Criteria:**
- Counter example from syntax reference parses correctly
- Greeting example with @prop parses correctly
- TodoList example with List comprehension parses correctly
- Modifier chains parse as method calls
- Nested views (Column containing Row containing Button) parse correctly

**Dependencies:** P3.S1, P3.S3, P2.S2

---

### P3.S8 — Parser Error Recovery

**Description:** When parsing fails, recover and continue to find more errors.

**Recovery strategies:**
1. **Statement level:** On error in a statement, skip to next newline, try next statement
2. **Item level:** On error in a top-level item, skip to next `fn`/`struct`/`enum`/etc.
3. **Block level:** On mismatched `{`/`}`, try to find matching delimiter
4. **Expression level:** On error in expression, skip to next `,` or `)`

**Deliverables:**
- `synchronize()` method in parser
- Error recovery at each parsing level

**Acceptance Criteria:**
- File with 5 syntax errors reports all 5 (not just the first)
- Error messages include "expected X, found Y" with source location
- Recovery doesn't produce cascading false errors (or minimizes them)
- A single bad function doesn't prevent parsing the rest of the file

**Dependencies:** P3.S1 through P3.S7

---

### P3.S9 — Parser Test Suite

**Description:** Comprehensive test coverage for the parser.

**Deliverables:**
- `crates/adam_parser/src/tests.rs` (unit tests)
- `/tests/parser/` — test fixture files

**Specific test categories:**
- `test_parse_empty_file` — produces empty `SourceFile`
- `test_parse_fn_basic` — `fn foo() { }`
- `test_parse_fn_with_params` — various param ownership modes
- `test_parse_fn_with_return_type` — explicit return types
- `test_parse_fn_with_generics` — `fn max[T: Comparable](a T, b T) -> T`
- `test_parse_struct` — basic, with pub fields, with generics
- `test_parse_enum` — with variants, with data
- `test_parse_trait` — with methods, with default methods
- `test_parse_impl` — inherent and trait impl
- `test_parse_view` — full view with @state, @prop, body
- `test_parse_use` — single, multiple, star
- `test_parse_let_colon_assign` — `x := 5`
- `test_parse_let_typed` — `x: i32 = 5`
- `test_parse_let_mutable` — `mut x := 5`
- `test_parse_binary_ops` — all operators, precedence
- `test_parse_unary_ops` — negation, not, ref
- `test_parse_call` — function and method calls
- `test_parse_field_access` — dot notation
- `test_parse_index` — bracket notation
- `test_parse_if` — if, if-else, if-else if-else
- `test_parse_match` — basic, with guards, nested
- `test_parse_for` — for-in loop
- `test_parse_while` — while loop
- `test_parse_loop` — infinite loop
- `test_parse_closure` — inline closures
- `test_parse_string_interpolation` — `"hello {name}"`
- `test_parse_spawn` — spawn expression
- `test_parse_channel` — channel creation, send, recv
- `test_parse_select` — select with recv, send, after
- `test_parse_try_operator` — `expr?`
- `test_parse_optional_type` — `?T`
- `test_parse_result_type` — `T ! E`
- `test_parse_complex_types` — `Map[String, Vec[?i32]]`
- `test_parse_patterns` — all pattern forms
- `test_parse_all_examples` — parse every file in `/examples/`
- `test_parse_error_recovery` — file with multiple errors
- `test_parse_error_messages` — verify error message quality

**Acceptance Criteria:**
- All tests pass
- At least 80 individual test cases
- Every AST node type is produced by at least one test
- Example files all parse to valid ASTs
- Pretty-printed AST can be manually verified against source

**Dependencies:** P3.S1 through P3.S8

---

## Phase 4: Name Resolution

**Crate:** `adam_resolve`
**Goal:** Walk the AST and link every identifier to its declaration. Build scope tree. Detect undefined names, duplicate definitions, and import errors.

---

### P4.S1 — Scope Data Structures

**Description:** Define the scope tree — a hierarchy of scopes that maps names to declarations.

**Deliverables:**
- `crates/adam_resolve/src/scope.rs`

**Specific types:**
```rust
pub type ScopeId = u32;
pub type DeclId = u32;

pub struct ScopeTree {
    scopes: Vec<Scope>,
    root: ScopeId,
}

pub struct Scope {
    pub id: ScopeId,
    pub parent: Option<ScopeId>,
    pub kind: ScopeKind,
    pub declarations: HashMap<String, DeclId>,
}

pub enum ScopeKind {
    Module,         // file-level
    Function,       // fn body
    Block,          // { ... }
    Loop,           // for, while, loop
    Impl,           // impl block
    Trait,          // trait block
    View,           // view body
    Match,          // match arm
}

pub struct Declaration {
    pub id: DeclId,
    pub name: String,
    pub kind: DeclKind,
    pub span: Span,
    pub scope: ScopeId,
}

pub enum DeclKind {
    Variable { mutable: bool },
    Function,
    Struct,
    Enum,
    EnumVariant,
    Trait,
    TypeParam,
    Param { ownership: ParamOwnership },
    Field,
    Method,
    Import,
}
```

**Acceptance Criteria:**
- Scopes can be nested to arbitrary depth
- Name lookup walks parent scopes (lexical scoping)
- Shadowing is allowed (inner scope can redeclare name from outer scope)
- Each declaration has a unique `DeclId`

**Dependencies:** P2 (all)

---

### P4.S2 — Scope Builder

**Description:** Walk the AST top-down and build the scope tree, entering/exiting scopes as blocks are entered/exited.

**Deliverables:**
- `crates/adam_resolve/src/builder.rs`

**Specific functions:**
```rust
pub fn resolve(ast: &SourceFile) -> ResolveResult;

struct Resolver {
    scope_tree: ScopeTree,
    current_scope: ScopeId,
    declarations: Vec<Declaration>,
    errors: Vec<ResolveError>,
}

impl Resolver {
    fn enter_scope(&mut self, kind: ScopeKind) -> ScopeId;
    fn exit_scope(&mut self);
    fn declare(&mut self, name: &str, kind: DeclKind, span: Span) -> Result<DeclId, ResolveError>;
    fn lookup(&self, name: &str) -> Option<DeclId>;

    fn resolve_item(&mut self, item: &Item);
    fn resolve_fn(&mut self, func: &FnDef);
    fn resolve_struct(&mut self, struc: &StructDef);
    fn resolve_enum(&mut self, enu: &EnumDef);
    fn resolve_trait(&mut self, trai: &TraitDef);
    fn resolve_impl(&mut self, imp: &ImplBlock);
    fn resolve_view(&mut self, view: &ViewDef);
    fn resolve_block(&mut self, block: &Block);
    fn resolve_stmt(&mut self, stmt: &Stmt);
    fn resolve_expr(&mut self, expr: &Expr);
    fn resolve_pattern(&mut self, pattern: &Pattern);
    fn resolve_type(&mut self, ty: &Type);
}
```

**Acceptance Criteria:**
- Every identifier in the AST is linked to a `DeclId` (or an error is emitted)
- Function params are in scope within the function body
- Struct fields are in scope within impl methods (via `self`)
- Enum variants are in scope when pattern matching
- `for x in items` — `x` is in scope within the loop body
- Match arm bindings are in scope within the arm body
- View `@state` and `@prop` fields are in scope within `body`

**Dependencies:** P4.S1

---

### P4.S3 — Import Resolution

**Description:** Resolve `use` declarations — find the target module and link imported names.

**Deliverables:**
- `crates/adam_resolve/src/imports.rs`

**Specific behavior:**
- `use math.vector.Vec3` → find module `math/vector.adam`, find `Vec3` in it
- `use math.{vector, matrix}` → import both submodules
- `use std.io.*` → import all pub items from `std/io`
- Circular imports detected and reported as errors
- Import resolution happens before local name resolution

**Acceptance Criteria:**
- Single imports resolve correctly
- Multi-imports resolve correctly
- Star imports resolve correctly
- Missing module → clear error with path tried
- Missing item in module → error naming the available items
- Circular imports → error with cycle description

**Dependencies:** P4.S2

---

### P4.S4 — Duplicate and Undefined Name Detection

**Description:** Report errors for undefined names and duplicate definitions in the same scope.

**Specific errors:**
- `Undefined name 'foo' at line 5:3` — used name not in any scope
- `Duplicate definition of 'foo' at line 5:3, previously defined at line 2:3` — same name declared twice in same scope
- `Cannot find type 'Foo' at line 5:10` — type name not found
- `Cannot find trait 'Bar' at line 5:10` — trait name not found
- `Enum variant 'Baz' not found in enum 'Foo'` — pattern matching unknown variant

**Deliverables:**
- Error detection logic in `builder.rs`
- Error types in `crates/adam_resolve/src/errors.rs`

**Acceptance Criteria:**
- All undefined name uses produce errors
- All duplicate definitions produce errors
- Shadowing in nested scopes is NOT an error
- Error messages include both use-site and definition-site locations
- Suggestions: "Did you mean 'bar'?" for typos (Levenshtein distance <= 2)

**Dependencies:** P4.S2

---

### P4.S5 — Name Resolution Test Suite

**Description:** Comprehensive tests for name resolution.

**Deliverables:**
- `crates/adam_resolve/src/tests.rs`
- `/tests/resolve/` — test fixtures

**Specific tests:**
- `test_simple_variable` — `x := 5; print(x)` resolves
- `test_undefined_variable` — `print(x)` with no declaration → error
- `test_duplicate_definition` — two `fn foo()` in same scope → error
- `test_shadowing` — inner scope shadows outer, both resolve correctly
- `test_function_params_in_scope` — params accessible in body
- `test_struct_fields_via_self` — `self.x` resolves to struct field
- `test_enum_variants_in_match` — variant names resolve
- `test_trait_methods` — methods in impl resolve to trait
- `test_generic_params` — `T` in `fn foo[T]()` resolves
- `test_for_loop_binding` — `for x in items` → `x` accessible in body
- `test_match_arm_binding` — `Circle(r)` → `r` accessible in arm
- `test_view_state_fields` — `@state count` accessible in body
- `test_imports` — use declarations resolve
- `test_circular_import` — error detected
- `test_did_you_mean` — typo suggestion

**Acceptance Criteria:**
- All tests pass
- At least 30 test cases
- Every `DeclKind` variant tested

**Dependencies:** P4.S1 through P4.S4

---

## Phase 5: Type System & Checker

**Crate:** `adam_types`
**Goal:** Assign types to every expression, check type correctness, resolve generics, verify trait implementations.

---

### P5.S1 — Internal Type Representation

**Description:** Define how types are represented internally in the compiler (different from AST type nodes).

**Deliverables:**
- `crates/adam_types/src/ty.rs`

**Specific types:**
```rust
pub type TypeId = u32;

pub enum Ty {
    // Primitives
    I8, I16, I32, I64,
    U8, U16, U32, U64,
    F32, F64,
    Bool,
    Char,
    Unit,           // void / ()

    // Compound
    String,
    Str,            // borrowed string slice
    Array(TypeId, Option<u64>),     // element type, optional fixed size
    Tuple(Vec<TypeId>),
    Struct(StructId),
    Enum(EnumId),

    // Smart pointers
    Box(TypeId),
    Rc(TypeId),
    Arc(TypeId),

    // References
    Ref(TypeId),            // &T (shared borrow)
    MutRef(TypeId),         // &mut T (mutable borrow)

    // Optional and Result
    Optional(TypeId),       // ?T
    Result(TypeId, TypeId), // T ! E

    // Function
    Function(FnSig),

    // Channel
    Channel(TypeId),

    // Generics
    TypeVar(TypeVarId),     // unsolved type variable
    Generic(GenericId),     // declared generic param (T)

    // Error (used for error recovery — propagates without cascading errors)
    Error,

    // Never (return type of functions that don't return — panic, infinite loop)
    Never,
}

pub struct FnSig {
    pub params: Vec<TypeId>,
    pub return_type: TypeId,
}

pub struct TypeContext {
    types: Vec<Ty>,         // indexed by TypeId
    // ... interning, caching
}
```

**Acceptance Criteria:**
- All types from the type table can be represented
- Types are interned (same type → same TypeId)
- `Error` type propagates without causing cascading errors
- `Never` type can coerce to any type (for diverging expressions)

**Dependencies:** P2.S5, P4 (all)

---

### P5.S2 — Type Inference Engine

**Description:** Implement bidirectional type inference — propagate types both top-down (from annotations) and bottom-up (from expressions).

**Deliverables:**
- `crates/adam_types/src/infer.rs`

**Algorithm:**
1. For each expression, create a fresh type variable if type unknown
2. Generate type constraints from expressions:
   - `x := 5` → x has type ?T, 5 has type i32, constraint: ?T = i32
   - `a + b` → a and b must be same numeric type, result is same type
   - `fn call(arg)` → arg type must match param type
3. Solve constraints via unification
4. If a type variable remains unsolved → error: "cannot infer type"

**Specific functions:**
```rust
pub struct TypeChecker {
    ctx: TypeContext,
    constraints: Vec<Constraint>,
    substitutions: HashMap<TypeVarId, TypeId>,
    errors: Vec<TypeError>,
}

impl TypeChecker {
    fn infer_expr(&mut self, expr: &Expr) -> TypeId;
    fn check_expr(&mut self, expr: &Expr, expected: TypeId) -> TypeId;
    fn unify(&mut self, a: TypeId, b: TypeId) -> Result<TypeId, TypeError>;
    fn solve(&mut self);
    fn substitute(&self, ty: TypeId) -> TypeId;  // apply substitutions
}
```

**Acceptance Criteria:**
- `x := 5` infers `x: i32`
- `x := 5.0` infers `x: f64`
- `x := "hello"` infers `x: String`
- `x := [1, 2, 3]` infers `x: [i32]`
- `x := (1, "hi")` infers `x: (i32, String)`
- Return type inferred from function body
- Generic functions instantiated at call site
- Conflicting constraints → clear error message

**Dependencies:** P5.S1

---

### P5.S3 — Type Checking Rules

**Description:** Implement all type-checking rules for every expression and statement form.

**Deliverables:**
- `crates/adam_types/src/check.rs`

**Rules (non-exhaustive):**
- **Binary ops:** `+`, `-`, `*`, `/`, `%` require numeric operands of same type; result is same type
- **Comparison ops:** `==`, `!=` require operands of same type implementing `Eq`; `<`, `>`, `<=`, `>=` require `Comparable`
- **Logical ops:** `&&`, `||` require `bool` operands; result is `bool`
- **Unary ops:** `-` requires numeric; `!` requires `bool`; `&` produces `Ref(T)`
- **Assignment:** target must be `mut`; value type must match target type
- **Function call:** arg types must match param types; return type is function's return type
- **Method call:** receiver type must have method; arg types match
- **Field access:** type must be struct with that field
- **Index:** type must implement `Index` trait; result is element type
- **If:** condition must be `bool`; both branches must have same type (or unit)
- **Match:** scrutinee type matched against patterns; all arms must return same type
- **For:** iterable must implement `Iterable` trait; binding gets element type
- **Return:** must match function's declared return type
- **Try `?`:** operand must be `Result` or `Optional`; enclosing fn must return compatible error type
- **Spawn:** block type is unit; spawn returns a handle
- **Channel send:** value type must match channel's type parameter
- **Channel recv:** returns channel's type parameter

**Acceptance Criteria:**
- Type mismatches produce clear error: "expected i32, found String at line 5:10"
- All example files type-check correctly
- Wrong number of arguments → error with expected vs actual count
- Missing struct fields in literal → error listing missing fields
- Accessing private field from outside module → error

**Dependencies:** P5.S1, P5.S2

---

### P5.S4 — Generic Instantiation (Monomorphization)

**Description:** When a generic function or type is used with concrete type arguments, create a specialized version.

**Example:**
```adam
fn max[T: Comparable](a T, b T) -> T { ... }

max(1, 2)           // generates max_i32(a: i32, b: i32) -> i32
max("a", "b")       // generates max_String(a: String, b: String) -> String
```

**Deliverables:**
- `crates/adam_types/src/monomorphize.rs`

**Acceptance Criteria:**
- Each unique concrete instantiation produces a separate version
- Trait bounds checked: `max[T: Comparable]` rejects types without `Comparable`
- Recursive generics handled (e.g., `Vec[Vec[i32]]`)
- Unused generic instantiations not generated (dead code elimination)
- Error: "type X does not implement trait Y" when bounds not met

**Dependencies:** P5.S2, P5.S3

---

### P5.S5 — Trait Resolution

**Description:** When a trait method is called, determine which `impl` block provides the implementation.

**Deliverables:**
- `crates/adam_types/src/traits.rs`

**Specific behavior:**
- For `shape.draw()`: find `impl Drawable for Shape`, use its `draw` method
- Trait bounds on generics: `T: Comparable` means T's impl of `Comparable` is available
- Orphan rule: can only implement trait if you own the trait or the type (prevents conflicts)
- Default methods: if trait provides default impl, use it unless overridden
- Multiple trait bounds: `T: Comparable + Drawable` — both must be implemented

**Acceptance Criteria:**
- Direct trait impl lookup works
- Generic bounds with trait constraints work
- Default methods used when not overridden
- Conflicting impls detected: "conflicting implementations of trait X for type Y"
- Missing impl detected: "type X does not implement trait Y"

**Dependencies:** P5.S3

---

### P5.S6 — Exhaustiveness Checking

**Description:** In `match` expressions, verify that all possible cases are covered.

**Deliverables:**
- `crates/adam_types/src/exhaustive.rs`

**Algorithm:** usefulness checking (same as Rust/ML family)
- Check that the set of patterns covers all possible values of the scrutinee type
- Enum matching: every variant must be covered (or wildcard `_` used)
- Bool matching: both `true` and `false` covered
- Integer matching: wildcard required (can't enumerate all values)
- Nested patterns: recursively check usefulness

**Acceptance Criteria:**
- Missing enum variant → error: "non-exhaustive match: variant Circle not covered"
- Redundant pattern → warning: "pattern at line 10 is unreachable"
- Wildcard `_` makes any match exhaustive
- Nested enum patterns checked correctly

**Dependencies:** P5.S3

---

### P5.S7 — Type Checker Test Suite

**Description:** Comprehensive tests for the type system.

**Deliverables:**
- `crates/adam_types/src/tests.rs`
- `/tests/typecheck/` — test fixtures

**Specific tests:**
- `test_infer_integer` — `x := 5` → `i32`
- `test_infer_float` — `x := 5.0` → `f64`
- `test_infer_string` — `x := "hello"` → `String`
- `test_infer_array` — `x := [1, 2, 3]` → `[i32]`
- `test_infer_tuple` — `x := (1, "hi")` → `(i32, String)`
- `test_explicit_type` — `x: i64 = 5` → `i64`
- `test_binary_op_numeric` — `1 + 2` → `i32`
- `test_binary_op_mismatch` — `1 + "hi"` → error
- `test_comparison` — `1 < 2` → `bool`
- `test_logical` — `true && false` → `bool`
- `test_function_call` — correct args → return type
- `test_function_call_wrong_args` — wrong types → error
- `test_method_call` — `point.distance(other)` → `f64`
- `test_field_access` — `point.x` → `f64`
- `test_if_branches_same_type` — both branches → same type
- `test_if_branches_different_type` — error
- `test_match_exhaustive` — all variants covered → ok
- `test_match_non_exhaustive` — missing variant → error
- `test_generic_instantiation` — `max(1, 2)` → `i32`
- `test_generic_bounds_met` — type implements required trait → ok
- `test_generic_bounds_not_met` — type doesn't implement trait → error
- `test_trait_method_resolution` — correct impl found
- `test_try_operator_result` — `expr?` on Result → ok
- `test_try_operator_non_result` — `expr?` on i32 → error
- `test_optional_nil` — `nil` assignable to `?T`
- `test_result_type` — `T ! E` correctly typed
- `test_channel_type` — `chan[i32]` send/recv types correct
- `test_spawn_type` — `spawn { }` returns handle
- `test_view_state_types` — @state fields correctly typed
- `test_all_examples` — all example files type-check

**Acceptance Criteria:**
- All tests pass
- At least 50 test cases
- Every Ty variant is tested
- Error messages are human-readable with source locations

**Dependencies:** P5.S1 through P5.S6

---

## Phase 6: Borrow Checker

**Crate:** `adam_borrow`
**Goal:** Verify at compile time that all ownership and borrowing rules are satisfied. No use-after-move, no aliased mutation, no dangling references.

---

### P6.S1 — Ownership Tracking

**Description:** Track which variables own which values, and when ownership is transferred (moved).

**Deliverables:**
- `crates/adam_borrow/src/ownership.rs`

**Data structures:**
```rust
pub struct OwnershipTracker {
    /// For each variable, its current state
    states: HashMap<DeclId, VarState>,
}

pub enum VarState {
    /// Variable owns its value and it's available
    Owned,
    /// Value has been moved out — any use is an error
    Moved { moved_at: Span },
    /// Variable is currently borrowed (shared)
    Borrowed { borrow_count: u32 },
    /// Variable is currently mutably borrowed
    MutBorrowed { borrower: Span },
    /// Variable is partially moved (struct with some fields moved)
    PartiallyMoved { moved_fields: Vec<String> },
}
```

**Rules to enforce:**
1. After `consume(own x)` → x is `Moved`, any subsequent use of x → error
2. After `y := x` (where x is non-Copy) → x is `Moved`
3. `x` passed as default param (borrow) → x remains `Owned` after call returns
4. `mut x` borrow → while borrow is active, no other borrows of x
5. Multiple shared borrows allowed simultaneously
6. Copy types never move — they're always `Owned`

**Acceptance Criteria:**
- Use-after-move detected: `consume(x); print(x)` → error at `print(x)`
- Double mutable borrow detected: `fn f(mut a, mut b)` called as `f(x, x)` → error
- Shared + mutable borrow conflict detected
- Copy types (i32, bool, etc.) never trigger move errors
- Error messages: "value `x` used after move. Moved at line 5:3, used at line 7:3"

**Dependencies:** P4 (all), P5 (all)

---

### P6.S2 — Borrow Tracking

**Description:** Track active borrows (references) and ensure they don't outlive the owned value.

**Deliverables:**
- `crates/adam_borrow/src/borrows.rs`

**Key concepts:**
- A borrow is created when a value is passed to a function param (default mode)
- A borrow is also created explicitly with `&x`
- Borrow ends when the borrower's scope ends (function returns)
- While a shared borrow is active: value can't be mutated or moved
- While a mutable borrow is active: no other borrows allowed

**Region-based analysis:**
- Each borrow gets a region (lifetime) inferred by the compiler
- Regions form a lattice based on scope nesting
- Borrow valid if its region is contained within owner's region
- NO explicit lifetime annotations — all regions inferred

**Acceptance Criteria:**
- Returning a reference to a local variable → error: "cannot return reference to local"
- Borrowing a value and then moving it → error: "cannot move while borrowed"
- Nested borrows work: borrowing a field of a borrowed struct
- Reborrow works: creating a new borrow from an existing borrow
- Error messages explain the conflict with both borrow site and invalidation site

**Dependencies:** P6.S1

---

### P6.S3 — Control Flow Sensitivity

**Description:** Borrow checking must be flow-sensitive — a variable can be in different states on different code paths.

**Examples:**
```adam
x := create_string()
if condition {
    consume(own x)      // x moved on this path
}
print(x)                // ERROR only if condition path was taken
                        // Actually: ERROR because x MIGHT be moved
```

```adam
x := create_string()
if condition {
    consume(own x)
    return               // x moved, but we return
}
print(x)                // OK — x is NOT moved here because the move path returns
```

**Deliverables:**
- `crates/adam_borrow/src/flow.rs`
- Control flow graph (CFG) construction from AST
- Dataflow analysis on CFG

**Acceptance Criteria:**
- Conditional moves detected correctly
- Early returns allow moves on returned paths
- Loops: if moved in loop body, error on next iteration
- Match arms: each arm tracked independently
- After match: join states from all arms

**Dependencies:** P6.S1, P6.S2

---

### P6.S4 — Drop Insertion

**Description:** Determine where destructors (drop) must be called. Insert drop points at scope exits, before moves, and at end of temporaries.

**Deliverables:**
- `crates/adam_borrow/src/drop.rs`

**Rules:**
1. When a variable goes out of scope and is still `Owned` → insert drop
2. When a variable is moved → no drop at scope exit (already transferred)
3. Temporaries (intermediate expression results) → drop at end of statement
4. `if` branches: drop at end of branch if moved in one but not other
5. Loop variables: drop at end of each iteration

**Acceptance Criteria:**
- No double-frees (dropped exactly once)
- No leaks (every owned value is either dropped or moved)
- Drop order: variables dropped in reverse declaration order (like Rust/C++)
- Struct fields dropped when struct is dropped
- Enum variants: correct variant's fields dropped

**Dependencies:** P6.S1, P6.S3

---

### P6.S5 — Send/Sync Checking

**Description:** Ensure thread safety — values sent across threads must be `Send`, values shared across threads must be `Sync`.

**Rules:**
- `spawn { ... }` — any captured variable must be `Send` (moved into new thread)
- `Arc[T]` — T must be `Send + Sync` for Arc to be Send
- `Rc[T]` — NOT Send (single-thread only). Using Rc across spawn → error
- `chan[T].send(val)` — val must be `Send`
- Raw pointers (if ever added) — NOT Send, NOT Sync

**Auto-derivation:**
- A struct is `Send` if all its fields are `Send`
- A struct is `Sync` if all its fields are `Sync`
- Compiler automatically derives Send/Sync (no manual impl needed for safe types)

**Deliverables:**
- `crates/adam_borrow/src/thread_safety.rs`

**Acceptance Criteria:**
- `Rc[T]` captured in spawn → error: "Rc is not Send, use Arc for multi-threaded access"
- Sending non-Send type over channel → error
- All std types have correct Send/Sync status
- Custom structs auto-derive correctly

**Dependencies:** P6.S1, P5.S5

---

### P6.S6 — Borrow Checker Test Suite

**Description:** Comprehensive tests for the borrow checker.

**Deliverables:**
- `crates/adam_borrow/src/tests.rs`
- `/tests/borrow/` — test fixtures

**Specific tests:**
- `test_use_after_move` — move then use → error
- `test_move_copy_type` — i32 never moves → ok
- `test_borrow_then_use` — borrow returns, then use → ok
- `test_borrow_then_move` — borrow active, then move → error
- `test_double_mut_borrow` — two &mut at once → error
- `test_shared_and_mut_borrow` — & and &mut at once → error
- `test_multiple_shared_borrows` — multiple & at once → ok
- `test_return_local_ref` — return &local → error
- `test_conditional_move` — might-move on branch → error
- `test_conditional_move_with_return` — move on path that returns → ok
- `test_loop_move` — move in loop body → error
- `test_match_arm_moves` — independent arms → ok
- `test_drop_order` — reverse declaration order
- `test_partial_move` — move struct field, use other field → ok (if supported) or error
- `test_spawn_send` — Send type in spawn → ok
- `test_spawn_non_send` — Rc in spawn → error
- `test_channel_send_type` — correct type → ok
- `test_channel_send_wrong_type` — wrong type → error
- `test_reborrow` — borrow from borrow → ok
- `test_borrow_default_param` — fn(name String) borrows → ok
- `test_own_param` — fn(own name String) moves → ok
- `test_mut_param` — fn(mut name String) mut borrows → ok
- `test_all_examples` — ownership.adam example passes

**Acceptance Criteria:**
- All tests pass
- At least 40 test cases
- No false positives (valid code rejected)
- No false negatives (invalid code accepted)
- Error messages include move/borrow sites

**Dependencies:** P6.S1 through P6.S5

---

## Phase 7: Adam IR (Mid-Level IR)

**Crate:** `adam_ir`
**Goal:** Lower the typed, borrow-checked AST into a simpler intermediate representation (Adam IR / AIR). This IR is a control flow graph (CFG) of basic blocks — easier to optimize and lower to LLVM IR.

---

### P7.S1 — IR Data Structures

**Description:** Define the AIR instruction set and basic block representation.

**Deliverables:**
- `crates/adam_ir/src/ir.rs`

**Specific types:**
```rust
pub type BlockId = u32;
pub type VarId = u32;
pub type FnId = u32;

pub struct IrModule {
    pub functions: Vec<IrFunction>,
    pub globals: Vec<IrGlobal>,
    pub string_literals: Vec<String>,
}

pub struct IrFunction {
    pub id: FnId,
    pub name: String,
    pub params: Vec<IrParam>,
    pub return_type: IrType,
    pub blocks: Vec<BasicBlock>,
    pub entry: BlockId,
}

pub struct BasicBlock {
    pub id: BlockId,
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
}

pub enum Instruction {
    /// Assign a value to a variable
    Assign(VarId, RValue),
    /// Drop a value (call destructor)
    Drop(VarId),
    /// No-op (placeholder)
    Nop,
}

pub enum RValue {
    /// Use a variable: x
    Use(Operand),
    /// Binary operation: a + b
    BinaryOp(BinOp, Operand, Operand),
    /// Unary operation: -a, !b
    UnaryOp(UnOp, Operand),
    /// Function call
    Call(FnId, Vec<Operand>),
    /// Create struct: Point { x: 1, y: 2 }
    Aggregate(AggregateKind, Vec<Operand>),
    /// Access field: point.x
    Field(Operand, u32),  // field index
    /// Access array element: arr[i]
    Index(Operand, Operand),
    /// Create reference: &x
    Ref(VarId),
    /// Create mutable reference: &mut x
    MutRef(VarId),
    /// Dereference: *ptr
    Deref(Operand),
    /// Cast between types
    Cast(Operand, IrType),
    /// Constant value
    Constant(Constant),
    /// Allocate on heap
    HeapAlloc(IrType),
    /// Channel create
    ChanCreate(IrType, Option<u64>),  // type, buffer size
    /// Channel send
    ChanSend(Operand, Operand),
    /// Channel receive
    ChanRecv(Operand),
}

pub enum Operand {
    Var(VarId),
    Constant(Constant),
}

pub enum Constant {
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(u32),    // index into string_literals table
    Unit,
    Nil,
}

pub enum Terminator {
    /// Return from function
    Return(Option<Operand>),
    /// Unconditional jump
    Goto(BlockId),
    /// Conditional branch
    Branch(Operand, BlockId, BlockId),  // condition, then, else
    /// Multi-way branch (for match)
    Switch(Operand, Vec<(Constant, BlockId)>, BlockId),  // default
    /// Spawn green thread
    Spawn(BlockId),
    /// Select on channels
    Select(Vec<SelectBranch>),
    /// Unreachable (after diverging call like panic)
    Unreachable,
}
```

**Acceptance Criteria:**
- Every expression form can be lowered into AIR instructions
- CFG is well-formed: every block has exactly one terminator
- No high-level constructs remain (no `if`, `match`, `for` — all lowered to branches/jumps)
- SSA-like: each VarId assigned exactly once (or close to it)

**Dependencies:** P5 (all), P6 (all)

---

### P7.S2 — AST to IR Lowering

**Description:** Transform the typed AST into AIR basic blocks.

**Deliverables:**
- `crates/adam_ir/src/lower.rs`

**Lowering rules:**
- `if/else` → conditional branch to two blocks, join block after
- `match` → switch instruction or chain of branches
- `for` → loop header block (check iterator), body block, exit block
- `while` → loop header (check condition), body, exit
- `loop` → body block with unconditional back-edge
- `break` → goto loop exit block
- `continue` → goto loop header
- `return` → Return terminator
- Short-circuit `&&` / `||` → conditional branches (not simple binary ops)
- String interpolation → series of string concatenation calls
- Method calls → static function calls with receiver as first arg
- Closures → struct (captures) + function

**Specific functions:**
```rust
pub fn lower_module(ast: &SourceFile, types: &TypeContext) -> IrModule;

fn lower_function(func: &FnDef) -> IrFunction;
fn lower_block(block: &Block) -> Vec<BasicBlock>;
fn lower_stmt(stmt: &Stmt) -> Vec<Instruction>;
fn lower_expr(expr: &Expr) -> (Vec<Instruction>, Operand);
fn lower_pattern(pattern: &Pattern, scrutinee: Operand) -> Vec<BasicBlock>;
```

**Acceptance Criteria:**
- All example programs lower to valid AIR
- Control flow is correct: if/else, match, loops, break, continue, return
- Drop instructions inserted at correct points (from borrow checker output)
- Closures capture the right variables
- Generic functions fully monomorphized before lowering

**Dependencies:** P7.S1

---

### P7.S3 — IR Optimization Passes

**Description:** Run optimization passes on the AIR before lowering to LLVM.

**Deliverables:**
- `crates/adam_ir/src/opt/mod.rs`
- `crates/adam_ir/src/opt/dead_code.rs`
- `crates/adam_ir/src/opt/const_fold.rs`
- `crates/adam_ir/src/opt/inline.rs`
- `crates/adam_ir/src/opt/simplify_cfg.rs`

**Passes:**
1. **Dead code elimination** — remove unreachable blocks, unused variables
2. **Constant folding** — `1 + 2` → `3` at compile time
3. **Constant propagation** — `x := 5; y := x + 1` → `y := 6`
4. **Function inlining** — small functions inlined at call site (threshold: < 20 instructions)
5. **CFG simplification** — merge blocks with single predecessor/successor, remove empty blocks
6. **Copy propagation** — `x := y; use(x)` → `use(y)` if x not used elsewhere

**Acceptance Criteria:**
- `1 + 2 * 3` constant-folded to `7`
- Dead branches after constant folding removed
- Small functions inlined
- CFG has no empty blocks or unnecessary jumps
- Optimizations don't change program semantics (verified by running test programs before/after)

**Dependencies:** P7.S2

---

### P7.S4 — IR Printer and Verifier

**Description:** Debug tools for the IR — print human-readable IR and verify well-formedness.

**Deliverables:**
- `crates/adam_ir/src/print.rs` — textual IR printer
- `crates/adam_ir/src/verify.rs` — structural verifier

**IR printer output format:**
```
fn main() -> unit {
  bb0:
    %0 = const 5
    %1 = const "Hello, Adam!"
    call @print(%1)
    return ()
}
```

**Verifier checks:**
- Every block has a terminator
- No block is empty
- All variable uses are dominated by their definitions
- Jump targets exist
- Function call argument count matches
- Types of operands match expected types

**Acceptance Criteria:**
- IR can be printed to string and visually inspected
- Verifier catches common IR bugs
- `adam build --emit=ir` dumps IR for any program

**Dependencies:** P7.S1, P7.S2

---

### P7.S5 — IR Test Suite

**Description:** Tests for IR lowering and optimization.

**Deliverables:**
- `crates/adam_ir/src/tests.rs`
- `/tests/ir/` — test fixtures

**Specific tests:**
- `test_lower_hello_world` — basic function call
- `test_lower_arithmetic` — binary ops
- `test_lower_if_else` — branches
- `test_lower_match` — switch/branch chains
- `test_lower_for_loop` — iterator loop CFG
- `test_lower_while_loop` — condition loop CFG
- `test_lower_function_call` — call with args
- `test_lower_method_call` — becomes static call
- `test_lower_struct_create` — aggregate instruction
- `test_lower_field_access` — field instruction
- `test_lower_closure` — capture struct + function
- `test_lower_spawn` — spawn terminator
- `test_lower_channel` — chan create/send/recv
- `test_opt_const_fold` — 1+2 → 3
- `test_opt_dead_code` — unreachable block removed
- `test_opt_inline` — small fn inlined
- `test_verify_valid` — valid IR passes verifier
- `test_verify_invalid` — invalid IR caught by verifier

**Acceptance Criteria:**
- All tests pass
- Optimized IR produces same results as unoptimized
- Verifier catches all intentionally broken IR

**Dependencies:** P7.S1 through P7.S4

---

## Phase 8: LLVM Code Generation

**Crate:** `adam_codegen`
**Goal:** Lower Adam IR to LLVM IR, producing native machine code via LLVM's optimizer and backend.

**Rust dependency:** `inkwell` crate (safe Rust bindings to LLVM C API)

---

### P8.S1 — LLVM Module Setup

**Description:** Set up the LLVM context, module, and builder. Configure target triple and data layout for the host machine (initially) and ARM64 (for mobile later).

**Deliverables:**
- `crates/adam_codegen/src/context.rs`

**Specific implementation:**
```rust
pub struct CodeGen<'ctx> {
    context: &'ctx inkwell::context::Context,
    module: inkwell::module::Module<'ctx>,
    builder: inkwell::builder::Builder<'ctx>,
    target_machine: inkwell::targets::TargetMachine,

    // Mappings from Adam IR to LLVM values
    functions: HashMap<FnId, inkwell::values::FunctionValue<'ctx>>,
    variables: HashMap<VarId, inkwell::values::PointerValue<'ctx>>,
    blocks: HashMap<BlockId, inkwell::basic_block::BasicBlock<'ctx>>,
    strings: HashMap<u32, inkwell::values::GlobalValue<'ctx>>,
}
```

**Acceptance Criteria:**
- LLVM context initialized
- Module created with correct target triple (host for now)
- Can create function declarations and basic blocks
- `inkwell` compiles and links correctly in the workspace

**Dependencies:** P7 (all)

---

### P8.S2 — Type Mapping

**Description:** Map Adam types to LLVM types.

**Deliverables:**
- `crates/adam_codegen/src/types.rs`

**Mapping:**
| Adam Type | LLVM Type |
|-----------|-----------|
| `i8` | `i8` |
| `i16` | `i16` |
| `i32` | `i32` |
| `i64` | `i64` |
| `u8`-`u64` | same as signed (LLVM doesn't distinguish) |
| `f32` | `float` |
| `f64` | `double` |
| `bool` | `i1` |
| `char` | `i32` (Unicode scalar) |
| `Unit` | `void` |
| `String` | `{ i8*, i64, i64 }` (ptr, len, capacity) |
| `str` | `{ i8*, i64 }` (ptr, len) |
| `[T]` | `{ T*, i64, i64 }` (ptr, len, cap) |
| `[T; N]` | `[N x T]` |
| `(T, U)` | `{ T, U }` (struct) |
| `?T` | `{ i1, T }` (has_value, value) — or tagged union |
| `T ! E` | `{ i1, union { T, E } }` |
| `&T` | `T*` |
| `Box[T]` | `T*` (heap-allocated) |
| `struct` | named LLVM struct type |
| `enum` | tagged union: `{ i8, union { variant1, variant2, ... } }` |
| `fn(T)->U` | function pointer |
| `chan[T]` | opaque pointer to runtime channel struct |

**Acceptance Criteria:**
- Every Adam type maps to a valid LLVM type
- Struct layout matches C ABI (for FFI compatibility)
- Enum discriminant uses minimal bits (i8 for < 256 variants)
- Recursive types handled (via LLVM opaque struct + setBody)

**Dependencies:** P8.S1

---

### P8.S3 — Function Code Generation

**Description:** Generate LLVM IR for each function — translate AIR basic blocks to LLVM basic blocks.

**Deliverables:**
- `crates/adam_codegen/src/function.rs`

**Specific implementation:**
```rust
fn codegen_function(&mut self, func: &IrFunction);
fn codegen_block(&mut self, block: &BasicBlock);
fn codegen_instruction(&mut self, inst: &Instruction);
fn codegen_rvalue(&mut self, rvalue: &RValue) -> BasicValueEnum<'ctx>;
fn codegen_operand(&mut self, operand: &Operand) -> BasicValueEnum<'ctx>;
fn codegen_terminator(&mut self, term: &Terminator);
```

**What each instruction generates:**
- `Assign` → `alloca` + `store` (or just register value)
- `BinaryOp` → LLVM `add`, `sub`, `mul`, `sdiv`, `srem`, `fadd`, etc.
- `Call` → LLVM `call` instruction
- `Aggregate` → series of `insertvalue` instructions
- `Field` → `extractvalue` or `getelementptr`
- `Index` → `getelementptr` + `load`
- `Ref` → address of variable (already a pointer from `alloca`)
- `Drop` → call to drop function (destructor)
- `Return` → LLVM `ret`
- `Branch` → LLVM `br` (conditional)
- `Goto` → LLVM `br` (unconditional)
- `Switch` → LLVM `switch`

**Acceptance Criteria:**
- "Hello, Adam!" compiles and runs, printing to stdout
- Fibonacci (recursive) compiles and produces correct results
- Fibonacci (iterative with loop) compiles and produces correct results
- Struct creation and field access work
- Enum creation and pattern matching work
- Function calls with correct ABI

**Dependencies:** P8.S1, P8.S2

---

### P8.S4 — Memory Management Code Generation

**Description:** Generate code for allocations, deallocations, and drop calls.

**Deliverables:**
- `crates/adam_codegen/src/memory.rs`

**Specifics:**
- Stack allocation: `alloca` for local variables
- Heap allocation: call to `adam_runtime_alloc(size, align)` → returns pointer
- Deallocation: call to `adam_runtime_dealloc(ptr, size, align)` at drop points
- String: heap-allocated buffer with SSO (small string optimization) — strings <= 23 bytes stored inline
- Vec/Array: heap-allocated buffer with growth strategy (double capacity)
- Box: single heap allocation + deallocation on drop
- Rc: allocation with reference count header, increment on clone, decrement on drop, dealloc at zero
- Arc: same as Rc but with atomic operations

**Acceptance Criteria:**
- No memory leaks: every alloc paired with dealloc (verified with Valgrind/ASAN)
- Stack variables don't heap-allocate
- String "hello" uses stack (SSO), string "this is a longer string" uses heap
- Vec growing: push past capacity triggers realloc
- Drop order correct: reverse of declaration order

**Dependencies:** P8.S3

---

### P8.S5 — String and Print Runtime

**Description:** Implement the runtime support for strings and `print()` — the first thing users will need.

**Deliverables:**
- `runtime/src/string.rs` — String runtime (alloc, concat, interpolation, drop)
- `runtime/src/print.rs` — print/println implementation

**Specifics:**
- `print(msg)` → calls libc `write(1, ptr, len)` on Unix
- String interpolation: `"Hello, {name}"` → runtime concatenation of parts
- String formatting: integers, floats, bools auto-convert to string representation
- UTF-8 validation on construction

**Acceptance Criteria:**
- `print("Hello, Adam!")` outputs to stdout
- `print("Count: {count}")` with count=42 outputs "Count: 42"
- String concatenation works
- Non-ASCII strings (emoji, CJK) print correctly

**Dependencies:** P8.S3, P8.S4

---

### P8.S6 — Linking and Binary Output

**Description:** Link generated LLVM object files with the Adam runtime into a final executable.

**Deliverables:**
- `crates/adam_codegen/src/link.rs`
- Build script integration in `adam_cli`

**Process:**
1. LLVM generates object file (.o) from module
2. Link with Adam runtime static library
3. Link with system libc
4. Output executable binary

**For different targets:**
- macOS: `ld` or `clang` as linker, link with `-lSystem`
- Linux: `ld` or `gcc` as linker, link with `-lc -lpthread`
- iOS/Android: deferred to Phase 12

**Acceptance Criteria:**
- `adam build hello.adam` produces `./hello` executable
- `./hello` runs and prints output
- Binary is statically linked with runtime (no extra .so needed)
- Binary size for hello world < 1MB (without debug info)

**Dependencies:** P8.S3, P8.S5

---

### P8.S7 — Codegen Test Suite

**Description:** End-to-end tests — compile Adam programs and verify output.

**Deliverables:**
- `crates/adam_codegen/src/tests.rs`
- `/tests/codegen/` — test programs with expected output

**Specific tests:**
- `test_hello_world` — prints "Hello, Adam!"
- `test_arithmetic` — `print("{1 + 2 * 3}")` → "7"
- `test_variables` — let, mut, assignment
- `test_functions` — call, return, multiple returns
- `test_recursion` — fibonacci recursive
- `test_loops` — for, while, loop with break
- `test_structs` — create, access fields, methods
- `test_enums` — create, match, nested patterns
- `test_strings` — concat, interpolation, UTF-8
- `test_arrays` — create, index, push, iterate
- `test_tuples` — create, destructure
- `test_optionals` — Some, nil, if-let
- `test_results` — ok, err, ? operator
- `test_closures` — capture variables, call
- `test_generics` — generic function called with different types
- `test_traits` — trait method dispatch
- `test_ownership_move` — verify moved values are dropped, not leaked
- `test_memory_no_leaks` — run under ASAN, check no leaks

**Test strategy:** Each test is an `.adam` file with a `// expect: <output>` comment. The test runner compiles it, executes it, and compares stdout to expected.

**Acceptance Criteria:**
- All tests pass
- At least 30 end-to-end tests
- No memory leaks (ASAN clean)
- No undefined behavior (UBSAN clean)

**Dependencies:** P8.S1 through P8.S6

---

## Phase 9: Concurrency Runtime

**Crate:** `adam_runtime` (extends the runtime started in Phase 8)
**Goal:** Implement green threads, channels, and select — the Go-style concurrency primitives.

---

### P9.S1 — Green Thread Data Structures

**Description:** Define the core data structures for green threads (also called fibers/coroutines).

**Deliverables:**
- `runtime/src/thread.rs`

**Specific types:**
```rust
pub type ThreadId = u64;

pub struct GreenThread {
    pub id: ThreadId,
    pub state: ThreadState,
    pub stack: Stack,           // separate stack per green thread
    pub context: Context,       // saved CPU registers for switching
    pub result: Option<Box<dyn Any>>,
}

pub enum ThreadState {
    Ready,          // can be scheduled
    Running,        // currently executing
    Blocked,        // waiting on channel/select
    Completed,      // finished execution
}

pub struct Stack {
    pub ptr: *mut u8,
    pub size: usize,            // default 8KB, growable
    pub capacity: usize,
}

pub struct Context {
    // Platform-specific: saved registers (rsp, rbp, rip on x86_64; sp, lr on aarch64)
    registers: [u64; 16],
}
```

**Acceptance Criteria:**
- Green thread struct fits in ~256 bytes (excluding stack)
- Stack default 8KB (Go starts at 8KB too), growable to 1MB
- Context switch saves/restores minimal registers (platform-specific assembly)
- 100,000 green threads can be created without OOM (800MB for stacks at 8KB each)

**Dependencies:** P8.S6

---

### P9.S2 — Context Switching

**Description:** Implement the low-level context switch — save current thread's registers, restore next thread's registers.

**Deliverables:**
- `runtime/src/context_switch.rs` (Rust)
- `runtime/src/arch/x86_64.S` (assembly for x86_64)
- `runtime/src/arch/aarch64.S` (assembly for ARM64)

**What context switch does:**
1. Save callee-saved registers of current thread to its `Context`
2. Switch stack pointer to new thread's stack
3. Restore callee-saved registers of new thread from its `Context`
4. Return (which jumps to new thread's saved return address)

**Acceptance Criteria:**
- Switch takes < 100 nanoseconds (benchmark)
- Works on x86_64 (macOS, Linux) and aarch64 (macOS M-series, Linux ARM, iOS, Android)
- No register corruption — all callee-saved registers preserved
- Stack switching works — each green thread has independent stack

**Dependencies:** P9.S1

---

### P9.S3 — Work-Stealing Scheduler

**Description:** Implement the M:N scheduler that distributes green threads across OS threads.

**Deliverables:**
- `runtime/src/scheduler.rs`

**Architecture:**
- One OS thread per CPU core (detect at startup)
- Each OS thread has a local run queue (double-ended deque)
- When local queue is empty, steal from another thread's queue (work-stealing)
- Global run queue for overflow and newly spawned threads
- Spawning: push new green thread onto current thread's local queue

**Specific functions:**
```rust
pub struct Scheduler {
    workers: Vec<Worker>,
    global_queue: Mutex<VecDeque<GreenThread>>,
    thread_count: usize,
}

pub struct Worker {
    local_queue: Deque<GreenThread>,    // work-stealing deque
    os_thread: JoinHandle<()>,
    current: Option<GreenThread>,
}

impl Scheduler {
    pub fn new() -> Self;           // detect cores, spawn OS threads
    pub fn spawn(&self, f: impl FnOnce());
    fn schedule(&self, thread: GreenThread);
    fn run_worker(&self, worker_id: usize);     // main loop per OS thread
    fn steal(&self, thief: usize) -> Option<GreenThread>;
}
```

**Acceptance Criteria:**
- Spawning 1 million green threads works (memory permitting)
- All CPU cores utilized (verify with profiler)
- Work-stealing balances load across cores
- No deadlocks in scheduler
- Scheduler shuts down cleanly when main function returns

**Dependencies:** P9.S1, P9.S2

---

### P9.S4 — Channel Implementation

**Description:** Implement typed channels — the primary communication mechanism between green threads.

**Deliverables:**
- `runtime/src/channel.rs`

**Two variants:**
1. **Unbuffered channel** (synchronous): send blocks until a receiver is ready, recv blocks until a sender is ready
2. **Buffered channel**: send blocks only when buffer is full, recv blocks when buffer is empty

**Specific implementation:**
```rust
pub struct Channel<T> {
    buffer: Option<VecDeque<T>>,    // None = unbuffered, Some = buffered
    capacity: usize,                 // 0 = unbuffered
    senders_waiting: VecDeque<(ThreadId, T)>,
    receivers_waiting: VecDeque<ThreadId>,
    closed: bool,
    lock: Mutex<()>,
}

impl<T: Send> Channel<T> {
    pub fn new(capacity: usize) -> Self;
    pub fn send(&self, value: T);       // blocks if needed
    pub fn recv(&self) -> ?T;           // None if closed
    pub fn try_send(&self, value: T) -> bool;
    pub fn try_recv(&self) -> ?T;
    pub fn close(&self);
}
```

**Blocking mechanism:**
- When send/recv needs to block, the green thread is set to `Blocked` state
- The scheduler picks another ready thread to run
- When the complementary operation happens, the blocked thread is moved to `Ready`

**Acceptance Criteria:**
- Unbuffered: send blocks until recv (and vice versa)
- Buffered: send doesn't block until buffer full
- Closing a channel: recv returns nil, send panics
- No data races (lock-protected or lock-free)
- Ping-pong benchmark: 1 million messages < 1 second

**Dependencies:** P9.S3

---

### P9.S5 — Select Implementation

**Description:** Implement the `select` statement — wait on multiple channel operations simultaneously.

**Deliverables:**
- `runtime/src/select.rs`

**Behavior:**
```adam
select {
    val := ch1.recv() => process(val)
    ch2.send(result) => {}
    after 1.seconds => print("timeout")
}
```

- Evaluates all channel operations
- If one is immediately ready, execute that arm
- If multiple are ready, pick one at random (fairness)
- If none are ready, block until one becomes ready
- `after` arm: schedule a timer, wake if timeout fires first

**Acceptance Criteria:**
- Single ready channel: that arm fires
- Multiple ready: random selection (verified over many runs)
- No ready channels: blocks until one becomes ready
- `after` timeout fires correctly
- Nested select works
- Select on closed channel: recv arm fires with nil

**Dependencies:** P9.S4

---

### P9.S6 — Spawn Group (Structured Concurrency)

**Description:** Implement `spawn_group()` for fork-join concurrency patterns.

**Deliverables:**
- `runtime/src/spawn_group.rs`

**Behavior:**
```adam
group := spawn_group()
for url in urls {
    group.spawn {
        fetch(url)
    }
}
results := group.wait()     // blocks until all complete, returns results
```

**Specific implementation:**
```rust
pub struct SpawnGroup<T> {
    threads: Vec<ThreadId>,
    results: Vec<Option<T>>,
    remaining: AtomicU64,
    waiter: Option<ThreadId>,
}

impl<T: Send> SpawnGroup<T> {
    pub fn new() -> Self;
    pub fn spawn(&mut self, f: impl FnOnce() -> T);
    pub fn wait(self) -> Vec<T>;
}
```

**Acceptance Criteria:**
- All spawned tasks run to completion before wait returns
- Results returned in spawn order (not completion order)
- Panic in one task: all results collected, panicked task's result is error
- Empty group: wait returns immediately with empty vec

**Dependencies:** P9.S3, P9.S4

---

### P9.S7 — Concurrency Codegen Integration

**Description:** Generate LLVM code for spawn, channels, and select — bridge from compiled Adam code to the Rust runtime.

**Deliverables:**
- `crates/adam_codegen/src/concurrency.rs`

**Specifics:**
- `spawn { body }` → allocate green thread, set entry point to body function, call `scheduler.spawn()`
- `chan[T]()` → call `runtime_channel_create(capacity)`, returns pointer
- `ch.send(val)` → call `runtime_channel_send(ch_ptr, val_ptr)`
- `ch.recv()` → call `runtime_channel_recv(ch_ptr)`, returns val
- `select { ... }` → build array of channel ops, call `runtime_select(ops)`, branch on result
- These are all calls into the Rust runtime via C FFI

**Acceptance Criteria:**
- `spawn { print("hello from thread") }` works
- Channel ping-pong between two green threads works
- Select with timeout works
- All channel example programs compile and run correctly

**Dependencies:** P8 (all), P9.S1 through P9.S6

---

### P9.S8 — Concurrency Test Suite

**Description:** Tests for all concurrency features.

**Deliverables:**
- `runtime/src/tests.rs`
- `/tests/concurrency/` — test programs

**Specific tests:**
- `test_spawn_basic` — spawn a thread, it runs
- `test_spawn_many` — spawn 10,000 threads, all complete
- `test_spawn_100k` — stress test with 100,000 threads
- `test_channel_unbuffered` — send/recv synchronize
- `test_channel_buffered` — buffer fills and drains correctly
- `test_channel_close` — closed channel behavior
- `test_channel_multiple_senders` — many senders, one receiver
- `test_channel_multiple_receivers` — one sender, many receivers
- `test_select_single_ready` — one arm ready
- `test_select_multiple_ready` — random selection
- `test_select_timeout` — after arm fires
- `test_select_closed` — channel closed during select
- `test_spawn_group_basic` — fork-join pattern
- `test_spawn_group_empty` — empty group returns immediately
- `test_no_data_race` — concurrent mutations caught at compile time (borrow checker)
- `test_send_sync_enforcement` — Rc rejected in spawn

**Acceptance Criteria:**
- All tests pass
- No deadlocks (test timeout = 10 seconds)
- No data races (run under ThreadSanitizer)
- Stress tests pass reliably (no flaky tests)

**Dependencies:** P9.S1 through P9.S7

---

## Phase 10: Standard Library

**Crate:** `adam_std` (compiler-side support) + `/std/` (Adam source code)
**Goal:** Provide the essential built-in types and functions that every Adam program needs. Initially written in Rust (as runtime intrinsics), with the goal of rewriting parts in Adam once the language is self-sufficient.

---

### P10.S1 — Core Traits

**Description:** Define the fundamental traits that the type system and standard library depend on.

**Deliverables:**
- `std/core/traits.adam`

**Traits:**
```adam
trait Eq {
    fn eq(self, other Self) -> bool
    fn ne(self, other Self) -> bool {
        !self.eq(other)
    }
}

trait Comparable: Eq {
    fn cmp(self, other Self) -> Ordering
    fn lt(self, other Self) -> bool { self.cmp(other) == Less }
    fn gt(self, other Self) -> bool { self.cmp(other) == Greater }
    fn le(self, other Self) -> bool { !self.gt(other) }
    fn ge(self, other Self) -> bool { !self.lt(other) }
}

enum Ordering { Less, Equal, Greater }

trait Display {
    fn to_string(self) -> String
}

trait Debug {
    fn debug_string(self) -> String
}

trait Hash {
    fn hash(self) -> u64
}

trait Clone {
    fn clone(self) -> Self
}

trait Copy: Clone {}    // marker trait — compiler auto-copies

trait Default {
    fn default() -> Self
}

trait Drop {
    fn drop(mut self)
}

trait Iterable {
    type Item
    fn iter(self) -> Iterator[Self.Item]
}

trait Iterator {
    type Item
    fn next(mut self) -> ?Self.Item

    // Default methods
    fn map[U](self, f fn(Self.Item) -> U) -> MapIter[Self, U] { ... }
    fn filter(self, f fn(Self.Item) -> bool) -> FilterIter[Self] { ... }
    fn collect(self) -> [Self.Item] { ... }
    fn count(self) -> u64 { ... }
    fn any(self, f fn(Self.Item) -> bool) -> bool { ... }
    fn all(self, f fn(Self.Item) -> bool) -> bool { ... }
    fn fold[A](self, init A, f fn(A, Self.Item) -> A) -> A { ... }
    fn enumerate(self) -> EnumerateIter[Self] { ... }
    fn zip[O: Iterator](self, other O) -> ZipIter[Self, O] { ... }
    fn take(self, n u64) -> TakeIter[Self] { ... }
    fn skip(self, n u64) -> SkipIter[Self] { ... }
}

trait Send {}       // can be moved to another thread
trait Sync {}       // can be shared between threads via &

trait Index {
    type Input
    type Output
    fn index(self, idx Self.Input) -> Self.Output
}

trait Add { fn add(self, other Self) -> Self }
trait Sub { fn sub(self, other Self) -> Self }
trait Mul { fn mul(self, other Self) -> Self }
trait Div { fn div(self, other Self) -> Self }
trait Mod { fn mod(self, other Self) -> Self }
trait Neg { fn neg(self) -> Self }
```

**Acceptance Criteria:**
- All traits defined with correct signatures
- Default method implementations where applicable
- All primitive types implement applicable traits (i32 implements Eq, Comparable, Display, Hash, Clone, Copy, Add, Sub, etc.)
- Compiler recognizes these traits for operator overloading

**Dependencies:** P5 (all), P8 (all)

---

### P10.S2 — String Methods

**Description:** Implement comprehensive String and str methods.

**Deliverables:**
- `std/string.adam` (API definitions)
- `runtime/src/string.rs` (Rust implementation of intrinsics)

**Methods:**
```adam
impl String {
    fn new() -> String
    fn from(s str) -> String
    fn len(self) -> u64
    fn is_empty(self) -> bool
    fn contains(self, sub str) -> bool
    fn starts_with(self, prefix str) -> bool
    fn ends_with(self, suffix str) -> bool
    fn find(self, sub str) -> ?u64              // first index
    fn rfind(self, sub str) -> ?u64             // last index
    fn split(self, delimiter str) -> [String]
    fn join(parts [String], sep str) -> String  // static
    fn trim(self) -> str
    fn trim_start(self) -> str
    fn trim_end(self) -> str
    fn to_upper(self) -> String
    fn to_lower(self) -> String
    fn replace(self, from str, to str) -> String
    fn repeat(self, n u64) -> String
    fn chars(self) -> Iterator[char]
    fn bytes(self) -> Iterator[u8]
    fn slice(self, start u64, end u64) -> str
    fn push(mut self, s str)
    fn push_char(mut self, c char)
    fn pop(mut self) -> ?char
    fn insert(mut self, idx u64, s str)
    fn remove(mut self, idx u64) -> char
    fn clear(mut self)
    fn as_str(self) -> str
}
```

**Acceptance Criteria:**
- All methods work correctly with ASCII and Unicode
- UTF-8 boundary handling correct (slicing at non-char boundary → panic with message)
- Empty string edge cases handled
- Performance: string operations on 1MB string < 10ms

**Dependencies:** P8.S5, P10.S1

---

### P10.S3 — Collections: Vec, Map, Set

**Description:** Implement the core collection types.

**Deliverables:**
- `std/collections/vec.adam` + `runtime/src/vec.rs`
- `std/collections/map.adam` + `runtime/src/map.rs`
- `std/collections/set.adam` + `runtime/src/set.rs`

**Vec[T] methods:**
```adam
impl Vec[T] {
    fn new() -> Vec[T]
    fn with_capacity(cap u64) -> Vec[T]
    fn len(self) -> u64
    fn capacity(self) -> u64
    fn is_empty(self) -> bool
    fn push(mut self, val own T)
    fn pop(mut self) -> ?T
    fn insert(mut self, idx u64, val own T)
    fn remove(mut self, idx u64) -> T
    fn get(self, idx u64) -> ?T
    fn first(self) -> ?T
    fn last(self) -> ?T
    fn contains(self, val T) -> bool        // T: Eq
    fn sort(mut self)                        // T: Comparable
    fn reverse(mut self)
    fn iter(self) -> Iterator[T]
    fn clear(mut self)
    fn extend(mut self, other [T])
    fn map[U](self, f fn(T) -> U) -> Vec[U]
    fn filter(self, f fn(T) -> bool) -> Vec[T]
}
```

**Map[K, V] methods:**
```adam
impl Map[K: Hash + Eq, V] {
    fn new() -> Map[K, V]
    fn len(self) -> u64
    fn is_empty(self) -> bool
    fn get(self, key K) -> ?V
    fn insert(mut self, key own K, val own V) -> ?V  // returns old value
    fn remove(mut self, key K) -> ?V
    fn contains_key(self, key K) -> bool
    fn keys(self) -> Iterator[K]
    fn values(self) -> Iterator[V]
    fn iter(self) -> Iterator[(K, V)]
    fn clear(mut self)
}
```

**Set[T] methods:**
```adam
impl Set[T: Hash + Eq] {
    fn new() -> Set[T]
    fn len(self) -> u64
    fn is_empty(self) -> bool
    fn insert(mut self, val own T) -> bool     // true if new
    fn remove(mut self, val T) -> bool          // true if existed
    fn contains(self, val T) -> bool
    fn iter(self) -> Iterator[T]
    fn union(self, other Set[T]) -> Set[T]
    fn intersection(self, other Set[T]) -> Set[T]
    fn difference(self, other Set[T]) -> Set[T]
    fn clear(mut self)
}
```

**Implementation details:**
- Vec: contiguous heap buffer, growth factor 2x
- Map: Robin Hood hashing (like Rust's HashMap) or Swiss table
- Set: wrapper around Map[T, Unit]

**Acceptance Criteria:**
- Vec push/pop O(1) amortized
- Map get/insert/remove O(1) average
- Ownership correctly transferred on insert (own keyword)
- Borrowing works for read operations
- No memory leaks (ASAN clean)
- 1 million inserts/lookups < 1 second

**Dependencies:** P8.S4, P10.S1

---

### P10.S4 — Math Functions

**Description:** Mathematical functions and constants.

**Deliverables:**
- `std/math.adam`

**Functions:**
```adam
pub const PI: f64 = 3.14159265358979323846
pub const E: f64 = 2.71828182845904523536
pub const TAU: f64 = 6.28318530717958647692

pub fn abs(x f64) -> f64
pub fn floor(x f64) -> f64
pub fn ceil(x f64) -> f64
pub fn round(x f64) -> f64
pub fn min(a f64, b f64) -> f64
pub fn max(a f64, b f64) -> f64
pub fn clamp(val f64, lo f64, hi f64) -> f64
pub fn sqrt(x f64) -> f64
pub fn pow(base f64, exp f64) -> f64
pub fn log(x f64) -> f64       // natural log
pub fn log2(x f64) -> f64
pub fn log10(x f64) -> f64
pub fn sin(x f64) -> f64
pub fn cos(x f64) -> f64
pub fn tan(x f64) -> f64
pub fn asin(x f64) -> f64
pub fn acos(x f64) -> f64
pub fn atan(x f64) -> f64
pub fn atan2(y f64, x f64) -> f64
```

**Implementation:** Calls to libm functions via LLVM intrinsics.

**Acceptance Criteria:**
- All functions produce correct results (compared to libm reference)
- Special values handled: NaN, Infinity, -0.0
- `sqrt(-1)` returns NaN

**Dependencies:** P8.S3

---

### P10.S5 — I/O: File System and Stdio

**Description:** File reading/writing and standard I/O.

**Deliverables:**
- `std/io.adam`
- `std/fs.adam`
- `runtime/src/io.rs`

**Functions:**
```adam
// Standard I/O
pub fn print(msg String)
pub fn println(msg String)
pub fn eprint(msg String)           // stderr
pub fn eprintln(msg String)
pub fn read_line() -> String ! IoError

// File system
pub fn fs.read(path String) -> String ! IoError
pub fn fs.read_bytes(path String) -> [u8] ! IoError
pub fn fs.write(path String, content String) ! IoError
pub fn fs.write_bytes(path String, content [u8]) ! IoError
pub fn fs.append(path String, content String) ! IoError
pub fn fs.exists(path String) -> bool
pub fn fs.remove(path String) ! IoError
pub fn fs.mkdir(path String) ! IoError
pub fn fs.list_dir(path String) -> [String] ! IoError

// File handle (for streaming)
pub struct File { /* opaque */ }

impl File {
    pub fn open(path String) -> File ! IoError
    pub fn create(path String) -> File ! IoError
    pub fn read_all(self) -> String ! IoError
    pub fn read_line(self) -> ?String ! IoError
    pub fn write(mut self, data String) ! IoError
    pub fn close(mut self)
}
```

**Acceptance Criteria:**
- Read and write files correctly
- UTF-8 handling in file I/O
- IoError contains message and OS error code
- File handle closed on drop (RAII)
- Large file support (> 100MB)

**Dependencies:** P8.S5, P10.S1

---

### P10.S6 — Formatting and Conversion

**Description:** String formatting runtime and type conversion functions.

**Deliverables:**
- `std/fmt.adam`
- `std/convert.adam`
- `runtime/src/format.rs`

**Formatting:**
- `Display` trait auto-called in string interpolation
- Integer formatting: decimal, hex (`0x`), octal (`0o`), binary (`0b`)
- Float formatting: fixed, scientific, configurable precision
- Debug formatting: struct/enum name with field values

**Conversion:**
```adam
pub fn parse_int(s str) -> i64 ! ParseError
pub fn parse_float(s str) -> f64 ! ParseError
pub fn parse_bool(s str) -> bool ! ParseError
pub fn to_string[T: Display](val T) -> String
```

**Acceptance Criteria:**
- `"{42}"` → "42"
- `"{3.14159:.2}"` → "3.14" (if we support format specifiers)
- `parse_int("42")` → ok(42)
- `parse_int("abc")` → err(ParseError)
- All primitive types implement Display

**Dependencies:** P8.S5, P10.S1

---

### P10.S7 — Standard Library Test Suite

**Description:** Tests for every standard library function.

**Deliverables:**
- `tests/stdlib/` — test programs

**Coverage:**
- Every String method (at least 2 tests each: normal case + edge case)
- Every Vec method
- Every Map method
- Every Set method
- Every math function
- Every I/O function
- Every conversion function

**Acceptance Criteria:**
- At least 150 test cases
- 100% function coverage (every pub function tested)
- Edge cases: empty collections, nil values, Unicode strings, large inputs
- No memory leaks

**Dependencies:** P10.S1 through P10.S6

---

## Phase 11: UI Framework

**Crate:** `adam_ui`
**Goal:** Declarative, reactive UI framework built into the language. SwiftUI-inspired but compiled to native via Skia rendering.

---

### P11.S1 — View Protocol and Compiler Support

**Description:** Implement the `view` keyword in the compiler — transform view declarations into structs with reactive state.

**What the compiler does with a view:**
```adam
view Counter {
    @state count: i32 = 0
    body {
        Text("Count: {count}")
    }
}
```

**Compiler transforms this into (conceptually):**
```
struct Counter {
    _count: ReactiveCell<i32>,      // @state → reactive cell
}

impl View for Counter {
    fn body(&self) -> ViewNode {
        ViewNode::Text(format("Count: {}", self._count.get()))
    }
}

// When _count changes, runtime calls body() again to get new view tree
```

**Deliverables:**
- Compiler transform in `crates/adam_parser/src/view.rs` (extended)
- `crates/adam_ui/src/view.rs` — View trait
- `crates/adam_ui/src/state.rs` — ReactiveCell, reactive state tracking

**Acceptance Criteria:**
- `@state` fields generate reactive cells
- `@prop` fields generate immutable stored properties
- `@binding` fields generate two-way bindings (reference to parent's @state)
- Changing @state triggers re-render of the view
- View body is re-evaluated only when dependencies change (dependency tracking)

**Dependencies:** P3.S7, P5 (all), P8 (all)

---

### P11.S2 — Virtual View Tree and Diffing

**Description:** Build a virtual view tree from the body output, diff it against the previous tree, and produce a minimal set of mutations.

**Deliverables:**
- `crates/adam_ui/src/vtree.rs`

**View node types:**
```rust
pub enum ViewNode {
    Text { content: String, modifiers: Modifiers },
    Image { source: ImageSource, modifiers: Modifiers },
    Container { kind: ContainerKind, children: Vec<ViewNode>, modifiers: Modifiers },
    Button { label: String, action: ActionId, modifiers: Modifiers },
    TextInput { value: BindingId, placeholder: String, modifiers: Modifiers },
    List { items: Vec<ViewNode>, modifiers: Modifiers },
    Custom { view_id: ViewId, modifiers: Modifiers },
    Conditional { condition: bool, child: Box<ViewNode> },
    Empty,
}

pub enum ContainerKind {
    Column,     // vertical stack
    Row,        // horizontal stack
    Stack,      // z-axis stack (overlay)
    Scroll,     // scrollable
}

pub struct Modifiers {
    pub padding: Option<EdgeInsets>,
    pub margin: Option<EdgeInsets>,
    pub background: Option<Color>,
    pub foreground: Option<Color>,
    pub font: Option<Font>,
    pub corner_radius: Option<f32>,
    pub border: Option<Border>,
    pub shadow: Option<Shadow>,
    pub opacity: Option<f32>,
    pub size: Option<Size>,
    pub alignment: Option<Alignment>,
    pub on_tap: Option<ActionId>,
}
```

**Diffing algorithm:** O(n) tree diff (similar to React's reconciliation)
1. Compare old and new tree node by node
2. Same type → update properties, recurse into children
3. Different type → remove old, insert new
4. Lists → keyed diffing for efficient reorder

**Acceptance Criteria:**
- Changing @state only updates affected nodes
- Adding/removing children works correctly
- List reordering via keys is efficient
- Diff produces minimal mutations (no unnecessary updates)
- 1000-node tree diffs in < 1ms

**Dependencies:** P11.S1

---

### P11.S3 — Layout Engine

**Description:** Flexbox-style layout engine that computes position and size of every view node.

**Deliverables:**
- `crates/adam_ui/src/layout.rs`

**Layout model (Flexbox-inspired):**
- **Column:** children laid out vertically (main axis = vertical)
- **Row:** children laid out horizontally (main axis = horizontal)
- **Stack:** children overlaid (z-axis)
- Each node has: padding, margin, width/height (fixed, fill, wrap), alignment
- **Spacing:** between children in Column/Row
- **Flex:** children can have flex weights for proportional sizing

**Algorithm:** Two-pass layout (like CSS Flexbox)
1. **Measure pass (bottom-up):** each node reports its desired size
2. **Layout pass (top-down):** parent assigns final position and size to children

**Acceptance Criteria:**
- Column stacks children vertically with correct spacing
- Row stacks children horizontally
- Padding and margin applied correctly
- Text wrapping: multi-line text calculates height correctly
- Scroll views handle content larger than viewport
- Nested layouts work (Row inside Column inside Stack)
- Layout of 1000 nodes < 1ms

**Dependencies:** P11.S2

---

### P11.S4 — Skia Rendering Backend

**Description:** Render the laid-out view tree to screen using Skia graphics library.

**Deliverables:**
- `crates/adam_ui/src/render.rs`
- `crates/adam_ui/src/skia_backend.rs`

**Rust dependency:** `skia-safe` crate (Rust bindings to Skia)

**What gets rendered:**
- **Text:** Skia text shaping and rendering (handles Unicode, RTL, emoji)
- **Rectangles:** with fill color, border, corner radius, shadow
- **Images:** decoded and drawn via Skia
- **Clipping:** for scroll views and corner radius
- **Opacity:** alpha blending for transparent views

**Rendering loop:**
1. Wait for state change or input event
2. Re-evaluate view body (only changed views)
3. Diff view tree
4. Re-layout affected nodes
5. Re-render affected region to Skia surface
6. Present surface to screen (platform-specific)

**Target: 60fps** — total frame budget 16.6ms

**Acceptance Criteria:**
- Counter app renders on screen
- Text renders in correct position with correct font/color
- Buttons are tappable rectangles with label
- Background colors, corner radius, padding all render correctly
- 60fps maintained with simple UI (< 100 views)
- No visual artifacts or flickering

**Dependencies:** P11.S3

---

### P11.S5 — Gesture and Input Handling

**Description:** Handle touch/mouse/keyboard input and route it to the correct view.

**Deliverables:**
- `crates/adam_ui/src/input.rs`
- `crates/adam_ui/src/gesture.rs`

**Gesture types:**
- **Tap:** single touch/click on a view
- **Long press:** touch held for > 500ms
- **Drag:** touch moved while held
- **Swipe:** fast drag in a direction
- **Scroll:** scroll gesture (touch drag in scroll view, mouse wheel)
- **Keyboard:** text input for TextInput views

**Hit testing:**
- Given a touch point (x, y), walk the view tree to find the deepest view containing that point
- Views with `on_tap`, `on_long_press`, etc. handle the gesture
- Event bubbling: if a view doesn't handle the event, it propagates to parent

**Acceptance Criteria:**
- Tapping a button fires its action
- Tapping outside a button does nothing
- Scrolling a scroll view works
- Text input accepts keyboard input
- Long press fires after 500ms
- Overlapping views: topmost (z-order) gets the event

**Dependencies:** P11.S3, P11.S4

---

### P11.S6 — Animation System

**Description:** Support animations for property changes (position, size, opacity, color).

**Deliverables:**
- `crates/adam_ui/src/animation.rs`

**Animation types:**
- **Spring physics:** natural-feeling animations with damping and stiffness
- **Tween:** linear interpolation from A to B over duration
- **Keyframe:** multi-step animations

**API:**
```adam
// Implicit animation: any @state change can animate
Button("Toggle") {
    with_animation(.spring) {
        visible = !visible
    }
}

// Explicit animation
view Card {
    @state offset: f64 = 0

    body {
        Box()
            .offset(y: offset)
            .animation(.spring(damping: 0.7))
    }
}
```

**Implementation:**
- Animation ticker runs at display refresh rate (60/120fps)
- Each animation interpolates a value over time
- When animation value changes, triggers re-render of affected views

**Acceptance Criteria:**
- Spring animation looks natural (overshoot + settle)
- Tween animation is smooth (no jitter)
- Multiple simultaneous animations work
- Animation cancellation (new animation replaces old)
- No frame drops during animation

**Dependencies:** P11.S4

---

### P11.S7 — Built-in View Components

**Description:** Implement the core set of UI components that ship with Adam.

**Deliverables:**
- `std/ui/text.adam` — Text view
- `std/ui/button.adam` — Button view
- `std/ui/image.adam` — Image view
- `std/ui/text_input.adam` — Text input field
- `std/ui/toggle.adam` — Toggle/switch
- `std/ui/slider.adam` — Slider control
- `std/ui/list.adam` — Scrollable list
- `std/ui/scroll.adam` — Scroll view
- `std/ui/navigator.adam` — Navigation stack
- `std/ui/alert.adam` — Alert dialog
- `std/ui/sheet.adam` — Bottom sheet
- `std/ui/progress.adam` — Progress indicator
- `std/ui/spacer.adam` — Flexible spacer

**Each component needs:**
- Rendering implementation (how it draws via Skia)
- Layout behavior (how it sizes itself)
- Gesture handling (what inputs it responds to)
- Modifier support (all standard modifiers apply)
- Accessibility label

**Acceptance Criteria:**
- Each component renders correctly
- Each component handles its gestures
- Components compose correctly (Text inside Button inside Column)
- TodoList example app works with these components

**Dependencies:** P11.S1 through P11.S6

---

### P11.S8 — UI Test Suite

**Description:** Tests for the UI framework.

**Deliverables:**
- `tests/ui/` — test programs

**Specific tests:**
- `test_view_state_change` — @state change triggers body re-evaluation
- `test_view_prop_passing` — @prop passes data from parent
- `test_view_binding` — @binding provides two-way data flow
- `test_diff_add_child` — adding a view produces insert mutation
- `test_diff_remove_child` — removing a view produces remove mutation
- `test_diff_update_text` — changing text produces update mutation
- `test_layout_column` — vertical layout positions correct
- `test_layout_row` — horizontal layout positions correct
- `test_layout_padding` — padding offsets children
- `test_layout_nested` — nested containers
- `test_render_text` — text appears on surface
- `test_render_button` — button renders with label
- `test_hit_test_button` — tap on button fires action
- `test_hit_test_miss` — tap outside button does nothing
- `test_scroll_view` — scroll changes visible content
- `test_animation_spring` — spring animation interpolates
- `test_list_rendering` — list with 100 items renders correctly
- `test_counter_app` — full counter app works end-to-end
- `test_todo_app` — full todo app works end-to-end

**Acceptance Criteria:**
- All tests pass
- At least 30 UI tests
- Pixel tests where feasible (render to image, compare against reference)
- No memory leaks in view lifecycle (create, update, destroy)

**Dependencies:** P11.S1 through P11.S7

---

## Phase 12: Platform Bridges (iOS & Android)

**Goal:** Run Adam UI apps on real iOS and Android devices.

---

### P12.S1 — iOS Host App

**Description:** Create an iOS app shell that hosts the Adam runtime and Skia surface.

**Deliverables:**
- `platform/ios/AdamHost/` — Xcode project
- `platform/ios/AdamHost/AppDelegate.swift`
- `platform/ios/AdamHost/AdamView.swift` — UIView subclass with Metal-backed Skia surface
- `platform/ios/bridge.h` — C header for Adam runtime FFI

**Architecture:**
```
iOS App (Swift)
    └── AdamView (UIView)
        └── CAMetalLayer
            └── Skia Surface (draws via Metal)
                └── Adam UI Rendering
```

**What the host does:**
1. Creates a UIWindow with a single AdamView
2. AdamView creates a CAMetalLayer and Skia GPU surface
3. Forward touch events to Adam runtime
4. Forward display link (vsync) to Adam render loop
5. Forward app lifecycle events (background, foreground, terminate)

**Acceptance Criteria:**
- Empty Adam app launches on iOS Simulator
- Skia draws to screen via Metal
- Touch events reach Adam runtime
- App lifecycle handled (pause, resume, terminate)
- Builds with Xcode / `xcodebuild`

**Dependencies:** P11.S4, P8.S6

---

### P12.S2 — iOS Compilation Pipeline

**Description:** Compile Adam source to an iOS-compatible binary.

**Deliverables:**
- `crates/adam_codegen/src/targets/ios.rs`
- Build system integration

**Pipeline:**
1. Compile Adam → LLVM IR (same as desktop)
2. Target triple: `aarch64-apple-ios` (device) or `x86_64-apple-ios-simulator`
3. LLVM generates .o file
4. Link with Adam runtime (compiled for iOS)
5. Link with Skia (compiled for iOS)
6. Package into .framework or static library
7. Embed in iOS host app
8. Sign and package as .app / .ipa

**Acceptance Criteria:**
- `adam build --target=ios` produces a static library
- Library links into iOS host app without errors
- Counter app runs on iOS Simulator
- Counter app runs on physical iOS device (with signing)

**Dependencies:** P12.S1, P8.S6

---

### P12.S3 — iOS Platform APIs

**Description:** Bridge iOS platform APIs to Adam code via C FFI.

**Deliverables:**
- `platform/ios/apis/` — Obj-C/Swift wrappers exposing C interface
- `std/platform/ios.adam` — Adam declarations for platform APIs

**APIs to bridge (initial set):**
```adam
// Device info
pub fn platform.screen_width() -> f64
pub fn platform.screen_height() -> f64
pub fn platform.safe_area() -> EdgeInsets
pub fn platform.is_dark_mode() -> bool

// Navigation
pub fn platform.open_url(url String) ! Error
pub fn platform.share(text String)

// Storage
pub fn platform.store(key String, value String)
pub fn platform.load(key String) -> ?String

// Haptics
pub fn platform.haptic_light()
pub fn platform.haptic_medium()
pub fn platform.haptic_heavy()

// Status bar
pub fn platform.set_status_bar_style(style StatusBarStyle)

// Keyboard
pub fn platform.dismiss_keyboard()
```

**FFI mechanism:**
- Adam calls C function (generated by compiler)
- C function calls Obj-C method (via Obj-C runtime)
- Result passed back through C return value

**Acceptance Criteria:**
- `platform.screen_width()` returns actual screen width
- `platform.store("key", "value")` persists across app launches (UserDefaults)
- `platform.haptic_light()` triggers haptic feedback
- Dark mode detection works

**Dependencies:** P12.S1, P12.S2

---

### P12.S4 — Android Host App

**Description:** Create an Android app shell that hosts the Adam runtime and Skia surface.

**Deliverables:**
- `platform/android/app/` — Android project (Gradle)
- `platform/android/app/src/main/java/com/adam/runtime/AdamActivity.java`
- `platform/android/app/src/main/java/com/adam/runtime/AdamSurfaceView.java`
- `platform/android/jni/adam_jni.c` — JNI bridge

**Architecture:**
```
Android App (Kotlin/Java)
    └── AdamActivity
        └── AdamSurfaceView (SurfaceView)
            └── ANativeWindow
                └── Skia Surface (draws via OpenGL/Vulkan)
                    └── Adam UI Rendering
```

**What the host does:**
1. Creates Activity with AdamSurfaceView
2. SurfaceView provides a native window handle
3. Skia creates GPU surface on native window
4. Touch events forwarded via JNI to native Adam runtime
5. Choreographer vsync forwarded to render loop
6. Lifecycle events forwarded

**Acceptance Criteria:**
- Empty Adam app launches on Android Emulator
- Skia draws to screen via OpenGL ES / Vulkan
- Touch events reach Adam runtime
- Activity lifecycle handled
- Builds with Gradle + NDK

**Dependencies:** P11.S4, P8.S6

---

### P12.S5 — Android Compilation Pipeline

**Description:** Compile Adam source to an Android-compatible shared library.

**Deliverables:**
- `crates/adam_codegen/src/targets/android.rs`
- Build system integration with NDK

**Pipeline:**
1. Compile Adam → LLVM IR
2. Target triple: `aarch64-linux-android` (device) or `x86_64-linux-android` (emulator)
3. LLVM generates .o file
4. Link with Adam runtime and Skia (compiled for Android via NDK)
5. Output: `libadam_app.so`
6. Package into Android app (APK/AAB) via Gradle

**Acceptance Criteria:**
- `adam build --target=android` produces `libadam_app.so`
- Library loads in Android host app
- Counter app runs on Android Emulator
- Counter app runs on physical Android device

**Dependencies:** P12.S4, P8.S6

---

### P12.S6 — Android Platform APIs

**Description:** Bridge Android platform APIs to Adam code via JNI.

**Deliverables:**
- `platform/android/jni/apis.c` — JNI wrappers
- `std/platform/android.adam` — Adam declarations

**APIs (matching iOS set):**
```adam
// Same API surface as iOS where possible
pub fn platform.screen_width() -> f64
pub fn platform.screen_height() -> f64
pub fn platform.safe_area() -> EdgeInsets
pub fn platform.is_dark_mode() -> bool
pub fn platform.open_url(url String) ! Error
pub fn platform.share(text String)
pub fn platform.store(key String, value String)     // SharedPreferences
pub fn platform.load(key String) -> ?String
pub fn platform.haptic_light()
pub fn platform.haptic_medium()
pub fn platform.haptic_heavy()
pub fn platform.dismiss_keyboard()
```

**FFI mechanism:**
- Adam calls C function
- C function calls Java method via JNI (`FindClass`, `GetMethodID`, `CallVoidMethod`)
- Result passed back through C return value

**Acceptance Criteria:**
- Same API surface as iOS (cross-platform code works on both)
- `platform.store/load` uses SharedPreferences
- Screen dimensions correct
- Haptics work on supported devices

**Dependencies:** P12.S4, P12.S5

---

### P12.S7 — Cross-Platform Abstraction

**Description:** Ensure Adam code is truly cross-platform — same source code runs on both iOS and Android.

**Deliverables:**
- `std/platform/mod.adam` — unified platform API that dispatches to iOS/Android at compile time

**Mechanism:**
```adam
// Compile-time platform detection
#if platform == "ios"
    use platform.ios as platform_impl
#elif platform == "android"
    use platform.android as platform_impl
#endif

// Unified API
pub fn screen_width() -> f64 {
    platform_impl.screen_width()
}
```

**Acceptance Criteria:**
- Same `.adam` source file compiles for both iOS and Android
- Platform-specific code gated behind `#if platform` directives
- TodoList app runs on both platforms from single source

**Dependencies:** P12.S3, P12.S6

---

### P12.S8 — Platform Bridge Test Suite

**Description:** Tests for platform integration.

**Deliverables:**
- `tests/platform/` — test programs
- CI integration for iOS Simulator and Android Emulator

**Specific tests:**
- `test_ios_launch` — app launches on simulator
- `test_ios_render` — counter renders correctly
- `test_ios_touch` — button tap registers
- `test_ios_storage` — key-value store works
- `test_android_launch` — app launches on emulator
- `test_android_render` — counter renders correctly
- `test_android_touch` — button tap registers
- `test_android_storage` — key-value store works
- `test_cross_platform_app` — same source runs on both

**Acceptance Criteria:**
- All tests pass on simulators/emulators
- No crashes on app lifecycle events (backgrounding, rotation)
- Performance: 60fps maintained on real devices

**Dependencies:** P12.S1 through P12.S7

---

## Phase 13: Tooling

**Goal:** Developer experience tools — everything a programmer needs to be productive with Adam.

---

### P13.S1 — CLI Tool (`adam`)

**Description:** The main command-line interface for the Adam toolchain.

**Deliverables:**
- `crates/adam_cli/src/main.rs`

**Commands:**
```
adam new <name>              Create a new Adam project
adam build [file]            Compile the project/file
adam run [file]              Compile and execute
adam test [path]             Run tests
adam fmt [path]              Format source files
adam check [path]            Type-check without compiling (fast feedback)
adam pkg init                Initialize package manifest
adam pkg add <name>          Add a dependency
adam pkg remove <name>       Remove a dependency
adam pkg update              Update dependencies
adam pkg install             Install all dependencies
adam clean                   Remove build artifacts
```

**Flags:**
```
--target=<target>       ios, android, macos, linux, windows
--release               Optimize for release (LLVM -O2)
--debug                 Include debug info (default)
--emit=<stage>          tokens, ast, ir, llvm (dump intermediate output)
--verbose               Show compilation steps
```

**Acceptance Criteria:**
- `adam new myapp` creates a project with `adam.toml`, `src/main.adam`, `.gitignore`
- `adam build` compiles the project, reports errors or success
- `adam run` compiles and executes, showing program output
- `adam check` is faster than full build (skips codegen)
- `--emit=ast` dumps the AST for debugging
- Exit codes: 0 = success, 1 = compile error, 2 = runtime error

**Dependencies:** P1-P8 (all compiler phases)

---

### P13.S2 — Project Structure and Package Manifest

**Description:** Define the standard Adam project layout and `adam.toml` manifest format.

**Deliverables:**
- Manifest parser in CLI
- `adam new` template

**Standard layout:**
```
myapp/
├── adam.toml            Package manifest
├── src/
│   └── main.adam        Entry point
├── tests/
│   └── main_test.adam   Tests
└── .gitignore
```

**`adam.toml` format:**
```toml
[package]
name = "myapp"
version = "0.1.0"
adam = "0.1"             # minimum Adam version

[dependencies]
http = "1.0"
json = "2.3"

[dev-dependencies]
bench = "0.5"

[build]
target = "ios"           # default target (optional)
```

**Acceptance Criteria:**
- `adam.toml` parsed correctly
- Missing manifest → error: "no adam.toml found"
- Invalid manifest → error with line number
- `adam new` creates valid project that immediately compiles

**Dependencies:** P13.S1

---

### P13.S3 — Package Manager

**Description:** Dependency resolution and package registry.

**Deliverables:**
- `crates/adam_cli/src/pkg/` — package manager module

**Features:**
- Dependency resolution with semantic versioning (semver)
- Lock file (`adam.lock`) for reproducible builds
- Package registry (initially: Git URLs; later: central registry)
- Dependency download to `.adam/packages/`
- Diamond dependency resolution (if A depends on C@1.x and B depends on C@1.y, resolve to compatible version)

**Acceptance Criteria:**
- `adam pkg add http` resolves and downloads the package
- `adam.lock` generated with exact versions
- Second build uses lock file (deterministic)
- Version conflicts detected and reported
- Circular dependencies detected and reported

**Dependencies:** P13.S2

---

### P13.S4 — Formatter (`adam fmt`)

**Description:** Automatic source code formatter — one canonical style, no configuration (like `gofmt`).

**Deliverables:**
- `crates/adam_cli/src/fmt.rs`

**Formatting rules:**
- 4-space indentation (no tabs)
- Trailing newline
- No trailing whitespace
- One blank line between top-level items
- No blank line at start/end of blocks
- Opening brace on same line: `fn foo() {`
- Single space around binary operators: `a + b`
- No space inside parens: `foo(a, b)` not `foo( a, b )`
- Chained modifiers: one per line, indented
  ```adam
  Text("hello")
      .font(.title)
      .color(.blue)
  ```
- Line width: 100 characters (break at operators/commas if exceeded)

**Acceptance Criteria:**
- `adam fmt` is idempotent: formatting twice produces same output
- `adam fmt --check` returns exit code 1 if file needs formatting (for CI)
- Formatter never changes program semantics
- Handles comments correctly (preserves position relative to code)
- Formats all example files consistently

**Dependencies:** P1 (lexer), P3 (parser), P2.S7 (pretty printer)

---

### P13.S5 — Test Framework

**Description:** Built-in test runner for Adam programs.

**Deliverables:**
- `std/test.adam` — test assertion functions
- Test runner in CLI

**Test syntax:**
```adam
// In tests/math_test.adam
use std.test.{assert, assert_eq, assert_ne, assert_err}

test "addition works" {
    assert_eq(1 + 2, 3)
}

test "division by zero returns error" {
    result := divide(1, 0)
    assert_err(result)
}

test "string contains" {
    assert("hello world".contains("world"))
}
```

**Assertion functions:**
```adam
pub fn assert(condition bool)
pub fn assert_eq[T: Eq + Display](actual T, expected T)
pub fn assert_ne[T: Eq + Display](actual T, expected T)
pub fn assert_err[T, E](result T ! E)
pub fn assert_ok[T, E](result T ! E)
pub fn assert_nil[T](val ?T)
pub fn assert_some[T](val ?T)
```

**Test runner behavior:**
- Discovers all files matching `*_test.adam` or in `tests/` directory
- Runs each `test "name" { }` block in isolation
- Reports: passed, failed (with assertion details), panicked
- Summary: `12 passed, 1 failed, 0 panicked`

**Acceptance Criteria:**
- `adam test` discovers and runs all tests
- Failed assertion shows: expected vs actual, source location
- Panic in test is caught and reported (doesn't crash test runner)
- Test output is colored (green pass, red fail)
- `adam test --filter "addition"` runs only matching tests
- Exit code 0 if all pass, 1 if any fail

**Dependencies:** P13.S1, P10 (all)

---

### P13.S6 — Language Server (LSP)

**Description:** Implement Language Server Protocol for IDE integration (VS Code, Neovim, etc.).

**Deliverables:**
- `crates/adam_lsp/src/lib.rs`
- VS Code extension: `editor/vscode/adam-lang/`

**Features (priority order):**
1. **Diagnostics** — real-time error/warning display as you type
2. **Go to definition** — click on name, jump to declaration
3. **Hover** — show type on hover
4. **Completions** — autocomplete identifiers, keywords, methods
5. **Signature help** — show function parameter info while typing call
6. **Find references** — all usages of a symbol
7. **Rename** — rename symbol across all files
8. **Document symbols** — outline view of functions/structs/etc.
9. **Formatting** — format on save via `adam fmt`
10. **Semantic highlighting** — rich syntax coloring

**Acceptance Criteria:**
- LSP starts and connects to VS Code
- Errors appear as red underlines within 500ms of typing
- Go to definition works for functions, structs, traits, variables
- Hover shows type information
- Completions suggest relevant names
- Formatting on save works

**Dependencies:** P1-P6 (compiler frontend), P13.S4 (formatter)

---

### P13.S7 — Benchmarking Framework

**Description:** Built-in benchmarking for measuring performance.

**Deliverables:**
- `std/bench.adam` — benchmark utilities
- Benchmark runner in CLI

**Syntax:**
```adam
use std.bench

bench "fibonacci 30" {
    fibonacci(30)
}

bench "string concat 1000" {
    mut s := ""
    for i in 0..1000 {
        s.push("x")
    }
}
```

**Output:**
```
fibonacci 30        ... 2,450 ns/iter (±120 ns)
string concat 1000  ... 15,200 ns/iter (±800 ns)
```

**Acceptance Criteria:**
- `adam bench` discovers and runs benchmarks
- Warmup phase (discard first N iterations)
- Statistical analysis: mean, standard deviation
- Configurable iteration count
- Results comparable across runs

**Dependencies:** P13.S1, P10 (all)

---

### P13.S8 — Tooling Test Suite

**Description:** Tests for all CLI tools.

**Deliverables:**
- `tests/tooling/`

**Specific tests:**
- `test_cli_new` — project creation
- `test_cli_build` — compilation
- `test_cli_run` — execution
- `test_cli_check` — type checking only
- `test_cli_emit_tokens` — token dump
- `test_cli_emit_ast` — AST dump
- `test_cli_emit_ir` — IR dump
- `test_fmt_idempotent` — format twice = same
- `test_fmt_check` — detect unformatted code
- `test_test_runner` — tests discover and run
- `test_test_failure_output` — assertion failure details
- `test_pkg_add` — dependency addition
- `test_lsp_diagnostics` — error reporting
- `test_lsp_completion` — autocomplete

**Acceptance Criteria:**
- All tests pass
- CLI commands have consistent --help output
- Error messages are helpful (not just "error occurred")

**Dependencies:** P13.S1 through P13.S7

---

## Phase 14: Testing Infrastructure

**Goal:** Comprehensive testing at every level — from unit tests to end-to-end mobile app tests.

---

### P14.S1 — Test Runner Infrastructure

**Description:** Set up the master test infrastructure that runs all test suites.

**Deliverables:**
- `scripts/test_all.sh` — runs all test categories
- CI configuration (GitHub Actions)

**Test categories:**
1. `cargo test` — all Rust unit tests (lexer, parser, resolver, type checker, borrow checker, IR, codegen)
2. Adam integration tests — compile and run `.adam` test programs
3. Memory safety tests — run under AddressSanitizer (ASAN)
4. Thread safety tests — run under ThreadSanitizer (TSAN)
5. Fuzzing — fuzz the lexer and parser with random input
6. Performance benchmarks — track regressions
7. iOS simulator tests — build and run on simulator
8. Android emulator tests — build and run on emulator

**Acceptance Criteria:**
- `scripts/test_all.sh` runs all categories
- CI runs on every commit
- Test results clearly reported (pass/fail per category)
- Flaky tests retried automatically (max 3 retries)

**Dependencies:** All previous phases

---

### P14.S2 — Fuzzing Infrastructure

**Description:** Fuzz test the lexer and parser to find crashes and edge cases.

**Deliverables:**
- `fuzz/fuzz_lexer.rs` — fuzz target for lexer
- `fuzz/fuzz_parser.rs` — fuzz target for parser
- `fuzz/corpus/` — seed corpus of valid Adam files

**Tool:** `cargo-fuzz` (libFuzzer-based)

**What we fuzz:**
- Lexer: random byte sequences → should never panic, always produce tokens or errors
- Parser: random token sequences → should never panic, always produce AST or errors
- Type checker: randomly generated ASTs → should never panic

**Acceptance Criteria:**
- Lexer survives 1 hour of fuzzing with no panics
- Parser survives 1 hour of fuzzing with no panics
- All crashes found are fixed and added to regression test corpus

**Dependencies:** P1, P3

---

### P14.S3 — Performance Regression Tests

**Description:** Track compilation speed and runtime performance across commits.

**Deliverables:**
- `bench/compile_time.rs` — measure compilation time of standard programs
- `bench/runtime_perf.rs` — measure execution time of benchmark programs
- Performance tracking script

**What to track:**
- Compile time of hello.adam (should stay < 100ms)
- Compile time of large program (1000 functions) (should stay < 5s)
- Runtime: fibonacci(35) execution time
- Runtime: channel ping-pong 1M messages
- Runtime: spawn 100K green threads
- Memory: peak RSS during compilation
- Binary size: hello world binary size

**Acceptance Criteria:**
- Performance tracked per commit
- > 20% regression triggers CI failure
- Historical data stored for trend analysis

**Dependencies:** P8 (all), P9 (all)

---

### P14.S4 — Error Message Quality Tests

**Description:** Ensure every compiler error message is helpful, includes source location, and (where possible) suggests a fix.

**Deliverables:**
- `tests/errors/` — `.adam` files that are intentionally wrong
- Each file has `// error: <expected message>` comments

**Error categories to test:**
- Syntax errors (unexpected token, missing brace, etc.)
- Type errors (type mismatch, missing method, wrong arg count)
- Name errors (undefined variable, duplicate definition)
- Borrow errors (use after move, aliased mutation)
- Import errors (missing module, missing item)
- Pattern errors (non-exhaustive match)

**Each error test verifies:**
1. Error is reported (not silently accepted)
2. Error message describes the problem
3. Error includes file:line:column
4. Error includes relevant code snippet with underline
5. Error suggests fix where possible ("did you mean X?", "try adding `mut`")

**Expected error output format:**
```
error[E0304]: cannot use `x` after move
  --> src/main.adam:5:12
   |
3  |     consume(own x)
   |                 - value moved here
4  |
5  |     print(x)
   |           ^ value used after move
   |
help: consider borrowing instead of moving
   |
3  |     consume(x)      // borrows by default
   |
```

**Acceptance Criteria:**
- At least 50 error test cases
- Every error message tested follows the format above
- No error message is just "error" without context
- Suggestions are correct and actionable

**Dependencies:** P1-P6 (all frontend phases)

---

## Phase 15: Documentation & Examples

**Goal:** Documentation and example apps that serve as both learning material and compiler test corpus.

---

### P15.S1 — Language Reference

**Description:** Complete reference documentation for the Adam language.

**Deliverables:**
- `docs/reference/` — language reference (one file per topic)
- Topics: variables, functions, structs, enums, traits, generics, ownership, borrowing, concurrency, views, modules, error handling, pattern matching

**Each topic includes:**
- Syntax specification
- Semantic rules
- Examples
- Common mistakes and how to fix them

**Acceptance Criteria:**
- Every language feature documented
- Every example in docs compiles and runs
- Cross-referenced (links between related topics)

**Dependencies:** All language phases complete

---

### P15.S2 — Tutorial

**Description:** Step-by-step tutorial for learning Adam.

**Deliverables:**
- `docs/tutorial/` — ordered tutorial chapters

**Chapters:**
1. Hello World — first program, `adam run`
2. Variables and Types — let, mut, type inference
3. Functions — params, return types, multiple returns
4. Control Flow — if, match, for, while, loop
5. Structs and Methods — data modeling
6. Enums and Pattern Matching — algebraic data types
7. Error Handling — Result, ?, Option
8. Ownership and Borrowing — memory safety
9. Collections — Vec, Map, Set
10. Concurrency — spawn, channels, select
11. Building a Mobile App — views, state, navigation
12. Platform APIs — accessing device features

**Acceptance Criteria:**
- Every code example compiles and runs
- Tutorial is self-contained (no assumed knowledge beyond programming basics)
- Progressive complexity (each chapter builds on previous)

**Dependencies:** All phases complete

---

### P15.S3 — Example Applications

**Description:** Complete, real-world-ish example applications.

**Deliverables:**
- `examples/hello.adam` — Hello World
- `examples/fibonacci.adam` — Fibonacci (recursive + iterative)
- `examples/counter_app.adam` — Counter UI app
- `examples/todo_app.adam` — Todo list with add/remove/complete
- `examples/weather_app.adam` — Fetch weather API, display results
- `examples/chat_app.adam` — Real-time chat using channels
- `examples/calculator_app.adam` — Calculator with button grid
- `examples/notes_app.adam` — Note-taking app with storage
- `examples/concurrency_demo.adam` — Parallel web fetcher

**Each example includes:**
- Complete, working source code
- Comments explaining key concepts
- Both iOS and Android compatible

**Acceptance Criteria:**
- Every example compiles and runs on desktop
- UI examples run on iOS Simulator and Android Emulator
- No example exceeds 300 lines (keep them focused)

**Dependencies:** All phases complete

---

## Dependency Graph Summary

```
Phase 0 (Scaffolding)
    ↓
Phase 1 (Lexer) → Phase 2 (AST) → Phase 3 (Parser)
    ↓
Phase 4 (Name Resolution)
    ↓
Phase 5 (Type Checker)
    ↓
Phase 6 (Borrow Checker)
    ↓
Phase 7 (Adam IR)
    ↓
Phase 8 (LLVM Codegen) ←── First runnable programs ("Hello World")
    ↓
Phase 9 (Concurrency Runtime) ←── spawn, channels, select work
    ↓
Phase 10 (Standard Library) ←── Collections, I/O, strings work
    ↓
Phase 11 (UI Framework) ←── Declarative views render on screen
    ↓
Phase 12 (Platform Bridges) ←── Apps run on iOS + Android
    ↓
Phase 13 (Tooling) ←── Formatter, LSP, package manager
    ↓
Phase 14 (Testing) + Phase 15 (Docs)
```

**Milestone checkpoints:**
- After Phase 3: Can parse all Adam syntax
- After Phase 6: Can type-check and borrow-check programs
- After Phase 8: **First compiled binary runs** (hello world)
- After Phase 9: Concurrent programs work (goroutines + channels)
- After Phase 10: Useful standard library available
- After Phase 11: UI apps render on desktop
- After Phase 12: **Apps run on real phones**
- After Phase 13: Full developer toolchain

---

## Success Metrics

| Metric | Target |
|--------|--------|
| Compile time (hello world) | < 100ms |
| Compile time (1000-fn program) | < 5 seconds |
| Binary size (hello world) | < 1MB |
| Green thread spawn time | < 1μs |
| Context switch time | < 100ns |
| Channel send/recv | < 200ns |
| 100K green threads | < 500ms to spawn all |
| UI frame time | < 16.6ms (60fps) |
| Memory per green thread | ~8KB (stack) + ~256B (metadata) |
| Compiler test count | > 500 total |
| Standard library test count | > 150 |
| Error message quality | Every error has location + suggestion |

---

*This document is the single source of truth for the Adam programming language project. It is NEVER modified unless the user explicitly requests removal of an expectation.*
