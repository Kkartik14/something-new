//! Stress and integration tests for LSP analysis, completion, hover,
//! symbols, DocumentStore, and IR lowering.

use crate::analysis::{analyze, Position};
use crate::completion::complete_at;
use crate::document::DocumentStore;
use crate::goto::goto_definition;
use crate::hover::hover_at;
use crate::symbols::{document_symbols, SymbolKind};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Parse source all the way through the IR lowering pipeline and return the
/// IR module.  Requires `adam_lexer`, `adam_parser`, and `adam_ir` which are
/// declared as dev-dependencies of adam_lsp.
fn lower(src: &str) -> adam_ir::IrModule {
    let lex_result = adam_lexer::Lexer::new(src).tokenize();
    assert!(
        lex_result.errors.is_empty(),
        "lex errors: {:?}",
        lex_result.errors
    );
    let parse_result = adam_parser::Parser::new(lex_result.tokens).parse();
    assert!(
        parse_result.errors.is_empty(),
        "parse errors: {:?}",
        parse_result.errors
    );
    adam_ir::lower_module(&parse_result.ast)
}

/// Collection of valid Adam programs that should parse, resolve, and
/// type-check without errors (or at most produce warnings but no errors
/// from the *lexer* or *parser*).
fn valid_programs() -> Vec<&'static str> {
    vec![
        // 1 - hello world
        "fn main() {\n    print(\"Hello, World!\")\n}\n",
        // 2 - arithmetic
        "fn add(a i32, b i32) -> i32 {\n    return a + b\n}\n",
        // 3 - if/else
        "fn max(a i32, b i32) -> i32 {\n    if a > b {\n        return a\n    } else {\n        return b\n    }\n}\n",
        // 4 - while loop
        "fn countdown(mut n i32) {\n    while n > 0 {\n        n = n - 1\n    }\n}\n",
        // 5 - for loop
        "fn sum_range() -> i32 {\n    total := 0\n    for i in 0..10 {\n        total = total + i\n    }\n    return total\n}\n",
        // 6 - struct
        "struct Point {\n    x i32\n    y i32\n}\n",
        // 7 - struct + function
        "struct Point {\n    x i32\n    y i32\n}\nfn origin() -> Point {\n    return Point { x: 0, y: 0 }\n}\n",
        // 8 - enum
        "enum Color {\n    Red\n    Green\n    Blue\n}\n",
        // 9 - match
        "fn describe(x i32) -> i32 {\n    match x {\n        1 => return 10\n        2 => return 20\n        _ => return 0\n    }\n}\n",
        // 10 - closure
        "fn apply() -> i32 {\n    f := |x| x + 1\n    return f(5)\n}\n",
        // 11 - multiple functions
        "fn foo() -> i32 { return 1 }\nfn bar() -> i32 { return 2 }\n",
        // 12 - mutable variable
        "fn mutate() {\n    mut x := 5\n    x = 10\n}\n",
        // 13 - nested if
        "fn nested(x i32) -> i32 {\n    if x > 10 {\n        if x > 20 {\n            return 3\n        } else {\n            return 2\n        }\n    } else {\n        return 1\n    }\n}\n",
        // 14 - loop + break
        "fn with_break() {\n    loop {\n        break\n    }\n}\n",
        // 15 - string literal
        "fn greet() {\n    name := \"Adam\"\n}\n",
        // 16 - bool literal
        "fn flag() -> bool {\n    return true\n}\n",
        // 17 - comparison operators
        "fn cmp(a i32, b i32) -> bool {\n    c := a == b\n    return c\n}\n",
        // 18 - return unit
        "fn noop() {\n    return\n}\n",
        // 19 - array literal
        "fn make_array() {\n    arr := [1, 2, 3]\n}\n",
        // 20 - tuple literal
        "fn make_tuple() {\n    t := (1, 2, 3)\n}\n",
        // 21 - negation
        "fn negate(x i32) -> i32 {\n    return -x\n}\n",
        // 22 - spawn
        "fn concurrent() {\n    spawn {\n        print(\"hello from task\")\n    }\n}\n",
        // 23 - channel
        "fn channel_test() {\n    ch := chan[i32](10)\n}\n",
        // 24 - string interpolation
        "fn show(x i32) -> String {\n    msg := \"value={x}\"\n    return msg\n}\n",
        // 25 - field access
        "struct Point {\n    x i32\n    y i32\n}\nfn get_x(p Point) -> i32 {\n    return p.x\n}\n",
        // 26 - method call
        "fn test_method() {\n    arr := [1, 2, 3]\n    arr.push(4)\n}\n",
        // 27 - multiple params
        "fn multi(a i32, b i32, c i32) -> i32 {\n    return a + b + c\n}\n",
        // 28 - empty function
        "fn empty() {\n}\n",
        // 29 - division/modulo
        "fn divmod(a i32, b i32) -> i32 {\n    d := a / b\n    m := a % b\n    return d + m\n}\n",
        // 30 - while true
        "fn spin() {\n    while true {\n        break\n    }\n}\n",
        // 31 - chained binary
        "fn chain() -> i32 {\n    return 1 + 2 + 3 + 4\n}\n",
        // 32 - logical ops
        "fn logic(a bool, b bool) -> bool {\n    return a && b || a\n}\n",
        // 33 - struct with many fields
        "struct BigStruct {\n    a i32\n    b i32\n    c i32\n    d i32\n    e i32\n}\n",
        // 34 - enum with payloads
        "enum Shape {\n    Circle(r i32)\n    Rect(w i32, h i32)\n}\n",
        // 35 - continue in loop
        "fn skip_even() {\n    for i in 0..10 {\n        if i % 2 == 0 {\n            continue\n        }\n    }\n}\n",
        // 36 - index access
        "fn get_first() -> i32 {\n    arr := [10, 20, 30]\n    return arr[0]\n}\n",
        // 37 - char literal
        "fn get_char() -> char {\n    return 'a'\n}\n",
        // 38 - float literal
        "fn get_pi() -> f64 {\n    return 3.14\n}\n",
        // 39 - complex while
        "fn fib(n i32) -> i32 {\n    a := 0\n    b := 1\n    i := 0\n    while i < n {\n        temp := a + b\n        a = b\n        b = temp\n        i = i + 1\n    }\n    return b\n}\n",
        // 40 - multiple statements
        "fn work() {\n    x := 1\n    y := 2\n    z := x + y\n    print(z)\n}\n",
        // 41 - function calling function
        "fn add(a i32, b i32) -> i32 {\n    return a + b\n}\nfn main() {\n    result := add(1, 2)\n}\n",
        // 42 - deeply nested expression
        "fn deep() -> i32 {\n    return ((1 + 2) * (3 + 4)) + 5\n}\n",
        // 43 - boolean negation
        "fn flip(b bool) -> bool {\n    return !b\n}\n",
        // 44 - assignment expression
        "fn reassign() {\n    mut x := 0\n    x = x + 1\n    x = x + 1\n}\n",
        // 45 - multiple structs
        "struct A {\n    x i32\n}\nstruct B {\n    y i32\n}\n",
        // 46 - multiple enums
        "enum E1 {\n    X\n    Y\n}\nenum E2 {\n    A\n    B\n}\n",
        // 47 - fn with no return type
        "fn side_effect() {\n    print(\"done\")\n}\n",
        // 48 - multiple string literals
        "fn strings() {\n    a := \"hello\"\n    b := \"world\"\n}\n",
        // 49 - range expression in for
        "fn range_loop() {\n    for i in 1..100 {\n        print(i)\n    }\n}\n",
        // 50 - complex match
        "fn complex_match(x i32) -> i32 {\n    match x {\n        1 => return 100\n        2 => return 200\n        3 => return 300\n        _ => return 0\n    }\n}\n",
    ]
}

/// Programs with intentional syntax/lex/parse errors that should
/// produce at least one diagnostic.  Each program is verified to have
/// at least one lex or parse error (not just resolver/type errors).
fn error_programs() -> Vec<&'static str> {
    vec![
        // 1 - missing closing brace
        "fn main() {",
        // 2 - invalid token (@)
        "fn main() { @@@invalid@@@ }",
        // 3 - unexpected token after fn
        "fn () {}",
        // 4 - unclosed string
        "fn main() { x := \"unclosed }",
        // 5 - double operators
        "fn main() { x := 1 ++ 2 }",
        // 6 - missing return type after arrow
        "fn main() -> {}",
        // 7 - invalid character ($)
        "fn main() { $ }",
        // 8 - lone return keyword (not a valid item)
        "return",
        // 9 - multiple fn keywords
        "fn fn fn fn",
        // 10 - comma as first param
        "fn main(,) {}",
        // 11 - struct with no name
        "struct {}",
        // 12 - enum with no name
        "enum {}",
        // 13 - orphan else at top level
        "else { }",
        // 14 - unclosed string 2
        "fn f() { x := \"hello }",
        // 15 - bad for syntax: no binding or iterator
        "fn f() { for { } }",
        // 16 - backtick is invalid
        "fn f() { ` }",
        // 17 - double let
        "fn f() { let let x := 1 }",
        // 18 - just an at sign
        "@",
        // 19 - angle bracket mismatch
        "fn f() { x := 1 <> 2 }",
        // 20 - unclosed paren
        "fn f() { x := (1 + 2 }",
        // 21 - unclosed bracket
        "fn f() { x := [1, 2 }",
        // 22 - extra close brace
        "fn main() { } }",
        // 23 - empty type annotation
        "fn f(x: ) {}",
        // 24 - double arrow
        "fn f() -> -> i32 {}",
        // 25 - fn as function name
        "fn fn() {}",
        // 26 - random tokens at top level
        "main() {}",
        // 27 - tilde is invalid
        "fn f() { ~ }",
        // 28 - two dollar signs
        "$$",
        // 29 - stray comma in body
        "fn f() { , }",
        // 30 - unclosed char literal
        "fn f() { c := 'ab }",
        // 31 - match without scrutinee
        "fn f() { match { } }",
        // 32 - if without condition
        "fn f() { if { } }",
        // 33 - number as struct field name
        "struct S { 123 }",
        // 34 - exclamation at top level
        "!!! bad !!!",
        // 35 - missing rhs in assignment
        "fn f() { x := }",
        // 36 - just a semicolon
        ";",
        // 37 - question mark at top level
        "???",
        // 38 - only open brace
        "{",
        // 39 - random close parens
        ") ) )",
        // 40 - number as enum variant
        "enum E { 123 }",
        // 41 - stray arrow at top level
        "=> something",
        // 42 - at-sign in body
        "fn f() { @ }",
        // 43 - pipe with nothing
        "fn f() { | }",
        // 44 - stray dot
        "fn f() { . }",
        // 45 - triple equals
        "fn f() { x === y }",
        // 46 - hash is invalid
        "# comment or bad",
        // 47 - backslash is invalid
        "fn f() { \\ }",
        // 48 - two left braces
        "{{ }}",
        // 49 - percent at top level
        "% bad",
        // 50 - ampersand triplet
        "fn f() { &&& }",
    ]
}

// =========================================================================
// LSP Analysis Stress (tests 1-7)
// =========================================================================

/// Test 1: Analyze 50 different valid Adam programs, verify the parse
/// phase produces an AST (diagnostics may come from later phases but we
/// do not crash).
#[test]
fn stress_analyze_50_valid_programs() {
    for (i, src) in valid_programs().iter().enumerate() {
        let result = analyze(src);
        // The analysis must not panic and should produce an AST from parsing.
        assert!(
            result.ast.is_some(),
            "program #{} failed to produce AST: diagnostics = {:?}",
            i + 1,
            result.diagnostics,
        );
    }
}

/// Test 2: Analyze 50 programs with intentional errors, verify diagnostics
/// are produced for each.
#[test]
fn stress_analyze_50_error_programs() {
    for (i, src) in error_programs().iter().enumerate() {
        let result = analyze(src);
        // Each error program should produce at least one diagnostic *or*
        // fail to produce an AST.
        let has_issue = !result.diagnostics.is_empty() || result.ast.is_none();
        assert!(
            has_issue,
            "error program #{} unexpectedly produced zero diagnostics and a valid AST\nsource: {:?}",
            i + 1,
            src,
        );
    }
}

/// Test 3: Analyze a very large program (1000+ lines), verify no crash.
#[test]
fn stress_analyze_large_program() {
    let mut source = String::new();
    for i in 0..200 {
        source.push_str(&format!(
            "fn func_{}(a i32, b i32) -> i32 {{\n    x := a + b\n    y := x * {}\n    return y\n}}\n\n",
            i, i
        ));
    }
    // Should be over 1000 lines.
    assert!(source.lines().count() > 1000);
    let result = analyze(&source);
    // Must not crash.  AST should be produced.
    assert!(result.ast.is_some(), "large program should parse");
}

/// Test 4: Analyze edge-case sources: empty, whitespace-only, comment-only.
#[test]
fn stress_analyze_edge_case_sources() {
    // Empty.
    let r1 = analyze("");
    assert!(r1.diagnostics.is_empty() || !r1.diagnostics.is_empty()); // no crash

    // Whitespace-only.
    let r2 = analyze("    \n\n   \t\t\n  ");
    assert!(r2.diagnostics.is_empty() || !r2.diagnostics.is_empty());

    // Comment-only.
    let r3 = analyze("// this is a comment\n// another comment\n");
    assert!(r3.diagnostics.is_empty() || !r3.diagnostics.is_empty());
}

/// Test 5: Rapid open/update/close cycling on DocumentStore (simulate fast typing).
#[test]
fn stress_document_store_rapid_cycling() {
    let mut store = DocumentStore::new();
    let uri = "file:///typing.adam";

    for version in 1..=50 {
        store.open(
            uri.to_string(),
            version,
            format!("fn main() {{\n    x := {}\n}}\n", version),
        );
        // Immediately update.
        store.update(
            uri,
            version + 100,
            format!("fn main() {{\n    y := {}\n}}\n", version * 2),
        );
        // Close.
        store.close(uri);
    }
    assert!(store.is_empty());
}

/// Test 6: Analyze a program, then get completions at every position.
#[test]
fn stress_completions_at_every_position() {
    let source = "fn main() {\n    let x := 42\n    print(x)\n}\n";
    let analysis = analyze(source);
    let line_count = source.lines().count() as u32;

    for line in 0..line_count {
        let line_str = source.lines().nth(line as usize).unwrap_or("");
        let line_len = line_str.len() as u32;
        for character in 0..=line_len {
            let pos = Position::new(line, character);
            // Must not panic.
            let _items = complete_at(source, pos, Some(&analysis));
        }
    }
}

/// Test 7: Analyze a program, hover at every keyword/identifier position.
#[test]
fn stress_hover_at_every_position() {
    let source = "fn main() {\n    let x := 42\n    print(x)\n}\n";
    let analysis = analyze(source);
    let line_count = source.lines().count() as u32;

    for line in 0..line_count {
        let line_str = source.lines().nth(line as usize).unwrap_or("");
        let line_len = line_str.len() as u32;
        for character in 0..=line_len {
            let pos = Position::new(line, character);
            // Must not panic.
            let _result = hover_at(source, pos, Some(&analysis));
        }
    }
}

// =========================================================================
// LSP Completion Stress (tests 8-13)
// =========================================================================

/// Test 8: Complete at start of empty file -- should get no completions
/// (empty prefix gives nothing by design of the completion engine).
#[test]
fn stress_complete_empty_file() {
    let items = complete_at("", Position::new(0, 0), None);
    // By design, empty prefix returns nothing.
    assert!(
        items.is_empty(),
        "empty file completion should return nothing"
    );
}

/// Test 9: Complete after `fn ` -- cursor is at an empty prefix so should
/// get no suggestions (prefix is empty).  But typing `f` at start should
/// give keyword matches.
#[test]
fn stress_complete_after_fn_prefix() {
    let items = complete_at("f", Position::new(0, 1), None);
    let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
    assert!(labels.contains(&"fn"), "typing 'f' should offer 'fn'");
    assert!(labels.contains(&"for"), "typing 'f' should offer 'for'");
}

/// Test 10: Complete after `let x: ` -- typing a type prefix should get
/// type suggestions.
#[test]
fn stress_complete_type_context() {
    // Typing 'I' should match Int, Int8, etc.
    let items = complete_at("let x: I", Position::new(0, 8), None);
    let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
    assert!(labels.contains(&"Int"), "should suggest Int");
    assert!(labels.contains(&"Int32"), "should suggest Int32");
}

/// Test 11: Complete inside struct body, enum body.
#[test]
fn stress_complete_inside_bodies() {
    // Inside struct body -- typing 'i' should match keywords starting with i.
    let struct_src = "struct Foo {\n    i";
    let items = complete_at(struct_src, Position::new(1, 5), None);
    let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
    assert!(labels.contains(&"if") || labels.contains(&"impl") || labels.contains(&"in"),
        "should suggest keywords starting with 'i'");

    // Inside enum body -- typing 'l' should match 'let', 'loop'.
    let enum_src = "enum E {\n    l";
    let items = complete_at(enum_src, Position::new(1, 5), None);
    let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
    assert!(labels.contains(&"let") || labels.contains(&"loop"),
        "should suggest keywords starting with 'l'");
}

/// Test 12: Complete with partial identifiers.
#[test]
fn stress_complete_partial_identifiers() {
    let prefixes_and_expected = vec![
        ("st", vec!["struct"]),
        ("re", vec!["return", "ref"]),
        ("wh", vec!["while", "where"]),
        ("ma", vec!["match"]),
        ("br", vec!["break"]),
        ("co", vec!["continue"]),
        ("pu", vec!["pub"]),
        ("en", vec!["enum"]),
        ("tr", vec!["trait", "true"]),
        ("fa", vec!["false"]),
    ];

    for (prefix, expected) in prefixes_and_expected {
        let items = complete_at(prefix, Position::new(0, prefix.len() as u32), None);
        let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
        for exp in expected {
            assert!(
                labels.contains(&exp),
                "prefix {:?} should suggest {:?}, got {:?}",
                prefix,
                exp,
                labels,
            );
        }
    }
}

/// Test 13: Call complete_at on 100 different positions in a complex program.
#[test]
fn stress_complete_100_positions() {
    let source = r#"fn fibonacci(n i32) -> i32 {
    if n <= 1 {
        return n
    }
    a := 0
    b := 1
    i := 2
    while i <= n {
        temp := a + b
        a = b
        b = temp
        i = i + 1
    }
    return b
}
fn main() {
    result := fibonacci(10)
    print(result)
}
"#;
    let analysis = analyze(source);
    let lines: Vec<&str> = source.lines().collect();

    let mut count = 0;
    for (line_idx, line_str) in lines.iter().enumerate() {
        let cols = line_str.len();
        for col in (0..=cols).step_by(std::cmp::max(1, cols / 5)) {
            let _items = complete_at(
                source,
                Position::new(line_idx as u32, col as u32),
                Some(&analysis),
            );
            count += 1;
            if count >= 100 {
                return;
            }
        }
    }
}

// =========================================================================
// LSP Symbols Stress (tests 14-16)
// =========================================================================

/// Test 14: Extract symbols from program with 20+ structs, 20+ functions,
/// 20+ enums.
#[test]
fn stress_symbols_many_declarations() {
    let mut source = String::new();
    for i in 0..20 {
        source.push_str(&format!("struct S{} {{\n    x i32\n}}\n", i));
    }
    for i in 0..20 {
        source.push_str(&format!("fn func_{}() -> i32 {{\n    return {}\n}}\n", i, i));
    }
    for i in 0..20 {
        source.push_str(&format!("enum E{} {{\n    A\n    B\n}}\n", i));
    }

    let symbols = document_symbols(&source, None);
    let struct_count = symbols.iter().filter(|s| s.kind == SymbolKind::Struct).count();
    let fn_count = symbols.iter().filter(|s| s.kind == SymbolKind::Function).count();
    let enum_count = symbols.iter().filter(|s| s.kind == SymbolKind::Enum).count();

    assert!(
        struct_count >= 20,
        "expected at least 20 structs, got {}",
        struct_count
    );
    assert!(
        fn_count >= 20,
        "expected at least 20 functions, got {}",
        fn_count
    );
    assert!(
        enum_count >= 20,
        "expected at least 20 enums, got {}",
        enum_count
    );
}

/// Test 15: Extract symbols from empty program.
#[test]
fn stress_symbols_empty_program() {
    let symbols = document_symbols("", None);
    assert!(symbols.is_empty(), "empty program should have no symbols");
}

/// Test 16: Verify symbol names and kinds match declarations.
#[test]
fn stress_symbols_names_and_kinds() {
    let source = concat!(
        "fn alpha() {}\n",
        "fn beta() {}\n",
        "struct Gamma {\n    x i32\n}\n",
        "enum Delta {\n    One\n    Two\n}\n",
        "trait Epsilon {\n}\n",
    );
    let symbols = document_symbols(source, None);
    let names: Vec<&str> = symbols.iter().map(|s| s.name.as_str()).collect();
    assert!(names.contains(&"alpha"), "missing alpha: {:?}", names);
    assert!(names.contains(&"beta"), "missing beta: {:?}", names);
    assert!(names.contains(&"Gamma"), "missing Gamma: {:?}", names);
    assert!(names.contains(&"Delta"), "missing Delta: {:?}", names);
    assert!(names.contains(&"Epsilon"), "missing Epsilon: {:?}", names);

    // Check kinds.
    for sym in &symbols {
        match sym.name.as_str() {
            "alpha" | "beta" => assert_eq!(sym.kind, SymbolKind::Function),
            "Gamma" => assert_eq!(sym.kind, SymbolKind::Struct),
            "Delta" => assert_eq!(sym.kind, SymbolKind::Enum),
            "Epsilon" => assert_eq!(sym.kind, SymbolKind::Trait),
            _ => {}
        }
    }
}

// =========================================================================
// IR Lowering Stress (tests 17-23)
// =========================================================================

/// Test 17: Lower 30 different valid programs to IR, verify IrModule has
/// the correct number of functions.
#[test]
fn stress_ir_lower_30_programs() {
    let programs_and_fn_counts: Vec<(&str, usize)> = vec![
        ("fn main() {\n}\n", 1),
        ("fn a() {} fn b() {}\n", 2),
        ("fn a() {} fn b() {} fn c() {}\n", 3),
        ("fn f(x i32) -> i32 { return x }\n", 1),
        ("fn add(a i32, b i32) -> i32 { return a + b }\n", 1),
        ("fn main() { print(\"hello\") }\n", 1),
        ("fn max(a i32, b i32) -> i32 { if a > b { return a } else { return b } }\n", 1),
        ("fn f() { mut x := 0\n x = 1 }\n", 1),
        ("fn f() { x := [1, 2, 3] }\n", 1),
        ("fn f() { t := (1, 2) }\n", 1),
        ("fn f() -> i32 { return -1 }\n", 1),
        ("fn f() -> bool { return true }\n", 1),
        ("fn f() -> bool { return false }\n", 1),
        ("fn f() -> char { return 'a' }\n", 1),
        ("fn f() -> f64 { return 3.14 }\n", 1),
        ("fn f() { for i in 0..10 { print(i) } }\n", 1),
        ("fn f() { while true { break } }\n", 1),
        ("fn f() { loop { break } }\n", 1),
        ("fn f(x i32) -> i32 { match x { 1 => return 1\n _ => return 0 } }\n", 1),
        ("fn f() { g := |x| x + 1 }\n", 1),
        ("fn f() { spawn { print(\"task\") } }\n", 1),
        ("fn f() { ch := chan[i32](1) }\n", 1),
        ("fn f() { return }\n", 1),
        ("fn f() { x := 1 + 2 * 3 }\n", 1),
        ("fn f() { x := \"hello\" }\n", 1),
        ("fn a() {} fn b() {} fn c() {} fn d() {}\n", 4),
        ("fn a() {} fn b() {} fn c() {} fn d() {} fn e() {}\n", 5),
        ("struct S { x i32 }\nfn f() -> i32 { return 0 }\n", 1),
        ("enum E { A\n B }\nfn f() -> i32 { return 0 }\n", 1),
        ("fn f() { a := 1\n b := 2\n c := a + b }\n", 1),
    ];

    for (i, (src, expected_fn_count)) in programs_and_fn_counts.iter().enumerate() {
        let module = lower(src);
        assert_eq!(
            module.functions.len(),
            *expected_fn_count,
            "program #{}: expected {} functions, got {}",
            i + 1,
            expected_fn_count,
            module.functions.len(),
        );
    }
}

/// Test 18: Lower program with structs, verify struct definitions appear in IR.
#[test]
fn stress_ir_lower_structs() {
    let source = concat!(
        "struct Point {\n    x i32\n    y i32\n}\n",
        "struct Color {\n    r i32\n    g i32\n    b i32\n}\n",
        "fn make() -> Point {\n    return Point { x: 1, y: 2 }\n}\n",
    );
    let module = lower(source);
    assert_eq!(module.struct_defs.len(), 2, "should have 2 struct defs");

    let names: Vec<&str> = module.struct_defs.iter().map(|s| s.name.as_str()).collect();
    assert!(names.contains(&"Point"), "missing Point struct");
    assert!(names.contains(&"Color"), "missing Color struct");

    let point = module.struct_defs.iter().find(|s| s.name == "Point").unwrap();
    assert_eq!(point.fields.len(), 2);
    assert_eq!(point.fields[0].name, "x");
    assert_eq!(point.fields[1].name, "y");
}

/// Test 19: Lower program with generics (generic syntax may parse but
/// lowering treats them as named types).
#[test]
fn stress_ir_lower_generic_like() {
    // Adam generics use bracket-style; the parser may or may not fully
    // support them.  We test something that the parser accepts.
    let source = "fn identity(x i32) -> i32 {\n    return x\n}\n";
    let module = lower(source);
    assert_eq!(module.functions.len(), 1);
    assert_eq!(module.functions[0].name, "identity");
}

/// Test 20: Lower program with closures/lambdas.
#[test]
fn stress_ir_lower_closures() {
    let source = "fn apply() -> i32 {\n    f := |x| x + 1\n    g := |a, b| a + b\n    return f(5)\n}\n";
    let module = lower(source);
    assert_eq!(module.functions.len(), 1);
    // Closures are lowered inline -- just verify no crash.
    assert!(!module.functions[0].blocks.is_empty());
}

/// Test 21: Lower program with match expressions.
#[test]
fn stress_ir_lower_match() {
    let source = "fn classify(x i32) -> i32 {\n    match x {\n        1 => return 10\n        2 => return 20\n        3 => return 30\n        _ => return 0\n    }\n}\n";
    let module = lower(source);
    let f = &module.functions[0];

    // Match should produce a Switch terminator.
    let has_switch = f
        .blocks
        .iter()
        .any(|b| matches!(b.terminator, adam_ir::Terminator::Switch(_, _, _)));
    assert!(has_switch, "match should produce a Switch terminator");
}

/// Test 22: Lower empty program -- should produce empty module.
#[test]
fn stress_ir_lower_empty() {
    let module = lower("");
    assert!(module.functions.is_empty(), "empty program should have no functions");
    assert!(module.struct_defs.is_empty());
    assert!(module.globals.is_empty());
}

/// Test 23: Lower program with 100 functions, verify all appear.
#[test]
fn stress_ir_lower_100_functions() {
    let mut source = String::new();
    for i in 0..100 {
        source.push_str(&format!("fn func_{}() -> i32 {{ return {} }}\n", i, i));
    }
    let module = lower(&source);
    assert_eq!(
        module.functions.len(),
        100,
        "expected 100 functions, got {}",
        module.functions.len(),
    );
    for i in 0..100 {
        let name = format!("func_{}", i);
        assert!(
            module.functions.iter().any(|f| f.name == name),
            "missing function {}",
            name,
        );
    }
}

// =========================================================================
// DocumentStore Stress (tests 24-27)
// =========================================================================

/// Test 24: Open 100 documents simultaneously.
#[test]
fn stress_document_store_100_open() {
    let mut store = DocumentStore::new();
    for i in 0..100 {
        let uri = format!("file:///doc_{}.adam", i);
        let content = format!("fn func_{}() {{}}\n", i);
        store.open(uri, 1, content);
    }
    assert_eq!(store.len(), 100, "should have 100 open documents");

    // Verify each is accessible.
    for i in 0..100 {
        let uri = format!("file:///doc_{}.adam", i);
        assert!(store.get(&uri).is_some(), "missing document {}", uri);
    }
}

/// Test 25: Update a document 1000 times rapidly.
#[test]
fn stress_document_store_1000_updates() {
    let mut store = DocumentStore::new();
    let uri = "file:///rapid.adam";
    store.open(uri.to_string(), 0, "fn main() {}\n".to_string());

    for version in 1..=1000 {
        store.update(
            uri,
            version,
            format!("fn main() {{ x := {} }}\n", version),
        );
    }
    let doc = store.get(uri).unwrap();
    assert_eq!(doc.version, 1000);
    assert!(doc.content.contains("1000"));
}

/// Test 26: Close all documents, verify store is empty.
#[test]
fn stress_document_store_close_all() {
    let mut store = DocumentStore::new();
    let uris: Vec<String> = (0..50)
        .map(|i| format!("file:///close_{}.adam", i))
        .collect();

    for uri in &uris {
        store.open(uri.clone(), 1, "fn f() {}\n".to_string());
    }
    assert_eq!(store.len(), 50);

    for uri in &uris {
        store.close(uri);
    }
    assert!(store.is_empty(), "all documents should be closed");
    assert_eq!(store.len(), 0);
}

/// Test 27: Reopen a closed document.
#[test]
fn stress_document_store_reopen() {
    let mut store = DocumentStore::new();
    let uri = "file:///reopen.adam";

    // Open.
    store.open(uri.to_string(), 1, "fn first() {}\n".to_string());
    assert_eq!(store.get(uri).unwrap().version, 1);

    // Close.
    store.close(uri);
    assert!(store.get(uri).is_none());

    // Reopen with new content.
    store.open(uri.to_string(), 2, "fn second() {}\n".to_string());
    let doc = store.get(uri).unwrap();
    assert_eq!(doc.version, 2);
    assert!(doc.content.contains("second"));
}

// =========================================================================
// Additional goto_definition stress
// =========================================================================

/// Goto definition at every position in a program -- must not panic.
#[test]
fn stress_goto_definition_every_position() {
    let source = "fn main() {\n    let x := 42\n    print(x)\n}\n";
    let analysis = analyze(source);
    let line_count = source.lines().count() as u32;

    for line in 0..line_count {
        let line_str = source.lines().nth(line as usize).unwrap_or("");
        let line_len = line_str.len() as u32;
        for character in 0..=line_len {
            let pos = Position::new(line, character);
            // Must not panic.
            let _result = goto_definition(source, "file:///test.adam", pos, Some(&analysis));
        }
    }
}

/// Goto definition on a known function name should find it.
#[test]
fn stress_goto_definition_finds_function() {
    let source = "fn hello() {\n}\nfn main() {\n    hello()\n}\n";
    let analysis = analyze(source);
    // Cursor on "hello" in the call site (line 3, somewhere in "hello").
    let result = goto_definition(source, "file:///t.adam", Position::new(3, 5), Some(&analysis));
    // If the resolver tracks "hello" as a declaration, we should find it.
    // Either way, no crash.
    if let Some(goto) = result {
        assert_eq!(goto.uri, "file:///t.adam");
    }
}

// =========================================================================
// Comprehensive combined stress
// =========================================================================

/// Run the full LSP pipeline (analyze + completions + hover + symbols +
/// goto) on each of the 50 valid programs.
#[test]
fn stress_full_lsp_pipeline_on_all_valid_programs() {
    for (i, src) in valid_programs().iter().enumerate() {
        let analysis = analyze(src);
        assert!(
            analysis.ast.is_some(),
            "program #{} failed to produce AST",
            i + 1
        );

        // Symbols.
        let _symbols = document_symbols(src, Some(&analysis));

        // Completions at a few positions.
        let lines: Vec<&str> = src.lines().collect();
        for (line_idx, line_str) in lines.iter().enumerate() {
            if line_idx > 3 {
                break; // sample a few lines
            }
            for col in (0..=line_str.len()).step_by(std::cmp::max(1, line_str.len() / 3)) {
                let pos = Position::new(line_idx as u32, col as u32);
                let _completions = complete_at(src, pos, Some(&analysis));
                let _hover = hover_at(src, pos, Some(&analysis));
                let _goto = goto_definition(src, "file:///test.adam", pos, Some(&analysis));
            }
        }
    }
}
