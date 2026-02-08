//! Fuzz-style tests for the Adam compiler frontend.
//!
//! Generates hundreds of random/pathological inputs and feeds them through
//! the pipeline: lex -> parse -> resolve -> typecheck.
//! Errors are fine; panics are bugs.

use crate::checker::TypeChecker;
use adam_lexer::Lexer;
use adam_parser::Parser;
use adam_resolve::resolve;

// ============================================================
// Deterministic PRNG (LCG)
// ============================================================

struct Rng {
    state: u64,
}

impl Rng {
    fn new(seed: u64) -> Self {
        Self { state: seed }
    }

    fn next(&mut self) -> u64 {
        // LCG parameters from Numerical Recipes
        self.state = self
            .state
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        self.state
    }

    fn next_usize(&mut self, max: usize) -> usize {
        if max == 0 {
            return 0;
        }
        (self.next() % max as u64) as usize
    }

    fn next_range(&mut self, min: usize, max: usize) -> usize {
        if min >= max {
            return min;
        }
        min + self.next_usize(max - min)
    }

    fn next_bool(&mut self) -> bool {
        self.next() % 2 == 0
    }

    fn choose<'a>(&mut self, items: &'a [&str]) -> &'a str {
        items[self.next_usize(items.len())]
    }

    fn next_printable_ascii(&mut self) -> u8 {
        (32 + self.next() % 95) as u8
    }
}

// ============================================================
// Core fuzz helper
// ============================================================

/// Run the full frontend pipeline on arbitrary source.
/// Errors are fine; panics are bugs.
fn fuzz_source(source: &str) {
    // Step 1: Lex
    let lex_result = Lexer::new(source).tokenize();
    // We don't assert errors are empty -- errors are fine.

    // Step 2: Parse (even if there were lex errors, the tokens are still produced)
    let parse_result = Parser::new(lex_result.tokens).parse();

    // Step 3: Resolve
    let _resolve_result = resolve(&parse_result.ast);

    // Step 4: Type check
    let _tc_result = TypeChecker::new().check(&parse_result.ast);
}

/// Like fuzz_source but catches panics and reports them.
fn fuzz_source_catch_panic(source: &str, label: &str, index: usize) {
    let result = std::panic::catch_unwind(|| {
        fuzz_source(source);
    });
    if let Err(e) = result {
        let msg = if let Some(s) = e.downcast_ref::<String>() {
            s.clone()
        } else if let Some(s) = e.downcast_ref::<&str>() {
            s.to_string()
        } else {
            "unknown panic".to_string()
        };
        panic!(
            "PANIC in fuzz test [{label}] #{index}:\n\
             Panic message: {msg}\n\
             Source (first 500 chars): {}",
            &source[..source.len().min(500)]
        );
    }
}

// ============================================================
// Test 1: Random ASCII strings
// ============================================================

#[test]
fn fuzz_random_ascii() {
    let mut rng = Rng::new(12345);
    for i in 0..100 {
        let len = rng.next_range(1, 500);
        let source: String = (0..len)
            .map(|_| rng.next_printable_ascii() as char)
            .collect();
        fuzz_source_catch_panic(&source, "random_ascii", i);
    }
}

// ============================================================
// Test 2: Random Unicode strings
// ============================================================

#[test]
fn fuzz_random_unicode() {
    let unicode_samples: &[&str] = &[
        "\u{1F600}",
        "\u{1F4A9}",
        "\u{2764}",
        "\u{1F680}", // emoji
        "\u{4E16}\u{754C}",
        "\u{3053}\u{3093}\u{306B}\u{3061}\u{306F}", // CJK/Japanese
        "\u{0627}\u{0644}\u{0639}\u{0631}\u{0628}\u{064A}\u{0629}", // Arabic RTL
        "\u{0301}\u{0302}\u{0303}\u{0304}\u{0305}", // combining chars
        "\u{200F}\u{200E}\u{200B}\u{FEFF}",         // bidi / zero-width
        "\u{0041}\u{030A}",                         // A + combining ring above
        "\u{D7FF}",                                 // edge of BMP
        "\u{FFFF}",                                 // max BMP
        "\u{10FFFF}",                               // max Unicode
        "\u{FDD0}\u{FFFE}",                         // noncharacters
    ];
    let mut rng = Rng::new(67890);
    for i in 0..100 {
        let len = rng.next_range(1, 50);
        let mut source = String::new();
        for _ in 0..len {
            if rng.next_bool() {
                source.push_str(unicode_samples[rng.next_usize(unicode_samples.len())]);
            } else {
                // Mix in some Adam keywords
                let kw = [
                    "fn", "let", "struct", "enum", "if", "match", " ", "\n", "=", "(", ")",
                ];
                source.push_str(kw[rng.next_usize(kw.len())]);
            }
        }
        fuzz_source_catch_panic(&source, "random_unicode", i);
    }
}

// ============================================================
// Test 3: Keyword soup
// ============================================================

#[test]
fn fuzz_keyword_soup() {
    let keywords: &[&str] = &[
        "fn", "struct", "enum", "trait", "impl", "view", "let", "mut", "own", "pub", "use", "mod",
        "if", "else", "match", "for", "while", "loop", "break", "continue", "return", "spawn",
        "select", "after", "chan", "true", "false", "nil", "self", "Self",
    ];
    let mut rng = Rng::new(11111);
    for i in 0..100 {
        let count = rng.next_range(1, 50);
        let source: String = (0..count)
            .map(|_| {
                let sep = if rng.next_bool() { " " } else { "\n" };
                format!("{}{}", rng.choose(keywords), sep)
            })
            .collect();
        fuzz_source_catch_panic(&source, "keyword_soup", i);
    }
}

// ============================================================
// Test 4: Bracket/brace torture
// ============================================================

#[test]
fn fuzz_bracket_torture() {
    let brackets: &[&str] = &["(", ")", "{", "}", "[", "]", "<", ">"];
    let mut rng = Rng::new(22222);
    for i in 0..100 {
        let count = rng.next_range(1, 200);
        let source: String = (0..count)
            .map(|_| rng.choose(brackets).to_string())
            .collect();
        fuzz_source_catch_panic(&source, "bracket_torture", i);
    }
}

// ============================================================
// Test 5: Number edge cases
// ============================================================

#[test]
fn fuzz_number_edge_cases() {
    let numbers: &[&str] = &[
        "0",
        "1",
        "999999999999999",
        "9999999999999999999",
        "0.0",
        "0.0000001",
        "1.0",
        "99999999.99999999",
        "1e10",
        "1e999",
        "1e-999",
        "0e0",
        "0x",
        "0xff",
        "0xFFFFFFFFFFFFFFFF",
        "0x0",
        "0b",
        "0b0",
        "0b1",
        "0b1111111111111111",
        "0o",
        "0o0",
        "0o7",
        "0o77777777777",
        "1_000",
        "1_000_000",
        "0x_FF",
        "0b_1010",
        "0.",
        ".0",
        "1.",
        ".1",
        "1.2.3",
        "1e",
        "1e+",
        "1e-",
        "-0",
        "-1",
        "--1",
        "++1",
        "0xGG",
        "0b22",
        "0o89",
    ];
    for (i, num) in numbers.iter().enumerate() {
        fuzz_source_catch_panic(num, "number_edge_case", i);
        // Also try in expression context
        let expr = format!("let x = {}", num);
        fuzz_source_catch_panic(&expr, "number_in_let", i);
    }
}

// ============================================================
// Test 6: String edge cases
// ============================================================

#[test]
fn fuzz_string_edge_cases() {
    let strings: &[&str] = &[
        r#"""#,                     // unterminated
        r#""hello"#,                // unterminated with content
        r#""hello world""#,         // normal
        r#""hello\nworld""#,        // escape
        r#""hello\""#,              // escaped quote then unterminated
        r#""hello\"world""#,        // escaped quote
        r#""\n\t\r\\\0""#,          // all escapes
        r#""\x41""#,                // hex escape
        r#""\u{1F600}""#,           // unicode escape
        r#""hello {x} world""#,     // interpolation
        r#""hello {} world""#,      // empty interpolation
        r#""hello {1 + 2} world""#, // expr interpolation
        r#""{{{}}}"#,               // nested braces in interpolation
        r#""""""#,                  // three quotes (two empty strings + unterminated?)
        "\"hello\nworld\"",         // literal newline in string
        "\"hello\0world\"",         // null byte in string
        r#""\""#,                   // just escaped quote, unterminated
        "\"\\",                     // backslash then EOF
    ];
    for (i, s) in strings.iter().enumerate() {
        fuzz_source_catch_panic(s, "string_edge_case", i);
    }
}

// ============================================================
// Test 7: Operator combinations
// ============================================================

#[test]
fn fuzz_operator_combinations() {
    let ops: &[&str] = &[
        "+", "-", "*", "/", "%", "=", "==", "!=", "<", ">", "<=", ">=", "&&", "||", "!", "&", "->",
        "=>", ".", "..", "?", "+=", "-=", "*=", "/=", "%=", ":=", ":", ",", ";", "|", "@",
    ];
    let mut rng = Rng::new(33333);
    // Every pair
    for (i, a) in ops.iter().enumerate() {
        for (j, b) in ops.iter().enumerate() {
            let source = format!("{}{}", a, b);
            fuzz_source_catch_panic(&source, "op_pair", i * ops.len() + j);
        }
    }
    // Random triples
    for i in 0..100 {
        let source = format!("{}{}{}", rng.choose(ops), rng.choose(ops), rng.choose(ops));
        fuzz_source_catch_panic(&source, "op_triple", i);
    }
}

// ============================================================
// Test 8: Huge identifiers
// ============================================================

#[test]
fn fuzz_huge_identifiers() {
    let ident_1k = "a".repeat(1000);
    fuzz_source_catch_panic(&ident_1k, "huge_ident", 0);
    fuzz_source_catch_panic(&format!("let {} = 1", ident_1k), "huge_ident_let", 0);

    let ident_10k = "b".repeat(10000);
    fuzz_source_catch_panic(&ident_10k, "huge_ident_10k", 0);
    fuzz_source_catch_panic(&format!("let {} = 1", ident_10k), "huge_ident_10k_let", 0);

    // Identifier with lots of underscores
    let ident_under = "a_".repeat(500);
    fuzz_source_catch_panic(&ident_under, "ident_underscores", 0);

    // Identifier starting with underscore
    let ident_leading = format!("_{}", "x".repeat(999));
    fuzz_source_catch_panic(&ident_leading, "ident_leading_under", 0);
}

// ============================================================
// Test 9: Repetitive patterns
// ============================================================

#[test]
fn fuzz_repetitive_patterns() {
    // 1000 let bindings
    let lets: String = (0..1000).map(|i| format!("let x{} = {}\n", i, i)).collect();
    fuzz_source_catch_panic(&lets, "1000_lets", 0);

    // 1000 function definitions
    let fns: String = (0..1000).map(|i| format!("fn f{}() {{}}\n", i)).collect();
    fuzz_source_catch_panic(&fns, "1000_fns", 0);

    // 500 struct definitions
    let structs: String = (0..500)
        .map(|i| format!("struct S{} {{ x: i32 }}\n", i))
        .collect();
    fuzz_source_catch_panic(&structs, "500_structs", 0);

    // 200 enum definitions
    let enums: String = (0..200)
        .map(|i| format!("enum E{} {{ A{}, B{} }}\n", i, i, i))
        .collect();
    fuzz_source_catch_panic(&enums, "200_enums", 0);
}

// ============================================================
// Test 10: Control characters
// ============================================================

#[test]
fn fuzz_control_characters() {
    // Null bytes
    fuzz_source_catch_panic("let x\0= 1", "null_byte", 0);
    fuzz_source_catch_panic("\0\0\0", "null_bytes", 1);
    fuzz_source_catch_panic("fn f\0() {}", "null_in_fn", 2);

    // Tabs
    fuzz_source_catch_panic("let\tx\t=\t1", "tabs", 3);

    // Carriage returns
    fuzz_source_catch_panic("let x = 1\r\nlet y = 2\r\n", "crlf", 4);
    fuzz_source_catch_panic("let x = 1\rlet y = 2\r", "cr_only", 5);

    // Vertical tabs and form feeds
    fuzz_source_catch_panic("let\x0Bx = 1", "vtab", 6);
    fuzz_source_catch_panic("let\x0Cx = 1", "formfeed", 7);

    // Mix everything
    fuzz_source_catch_panic("fn\x00f\x0B(\x0C)\x0D{\x09}\r\n", "ctrl_mix", 8);

    // Bell, backspace, escape
    fuzz_source_catch_panic("\x07\x08\x1B", "bell_bs_esc", 9);

    // Control chars mixed with valid code
    let mut rng = Rng::new(44444);
    let ctrl_chars = [0u8, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 27, 127];
    for i in 0..50 {
        let len = rng.next_range(1, 100);
        let source: String = (0..len)
            .map(|_| {
                if rng.next_bool() {
                    ctrl_chars[rng.next_usize(ctrl_chars.len())] as char
                } else {
                    rng.next_printable_ascii() as char
                }
            })
            .collect();
        fuzz_source_catch_panic(&source, "random_ctrl", i);
    }
}

// ============================================================
// Test 11: Comment edge cases
// ============================================================

#[test]
fn fuzz_comment_edge_cases() {
    let cases: &[&str] = &[
        "// unterminated line comment",
        "// line comment\nlet x = 1",
        "/* block comment */",
        "/* unterminated block comment",
        "/* nested /* comment */ */",
        "/* deeply /* nested /* comment */ */ */",
        "/* no close",
        "*/",  // close without open
        "/ *", // space between / and *
        "// /* mixed */",
        "/* // mixed */",
        "let x = 1 // trailing",
        "let x = 1 /* inline */ + 2",
        "/**/",   // empty block comment
        "/****/", // block with stars
        "/*/*/",  // confusing nesting
        "/* \"string inside comment\" */",
        "\"/* string that looks like comment */\"",
        "// \x00 null in comment",
        "/* \x00 null in block comment */",
    ];
    for (i, case) in cases.iter().enumerate() {
        fuzz_source_catch_panic(case, "comment_edge", i);
    }
}

// ============================================================
// Test 12: Type annotation soup
// ============================================================

#[test]
fn fuzz_type_annotation_soup() {
    let _types: &[&str] = &[
        "i32", "i64", "f32", "f64", "bool", "String", "char", "Vec", "Map", "Option", "Result",
        "Chan",
    ];
    let mut rng = Rng::new(55555);
    for i in 0..100 {
        let ty = gen_random_type(&mut rng, 0);
        let source = format!("let x: {} = 0", ty);
        fuzz_source_catch_panic(&source, "type_soup", i);
    }
}

fn gen_random_type(rng: &mut Rng, depth: usize) -> String {
    if depth > 5 {
        return "i32".to_string();
    }
    let base_types = ["i32", "i64", "f32", "f64", "bool", "String", "char"];
    let generic_types = ["Vec", "Option", "Result", "Map", "Chan"];

    match rng.next_usize(4) {
        0 => rng.choose(&base_types).to_string(),
        1 => {
            let base = rng.choose(&generic_types);
            let inner = gen_random_type(rng, depth + 1);
            format!("{}[{}]", base, inner)
        }
        2 => {
            let base = rng.choose(&generic_types);
            let a = gen_random_type(rng, depth + 1);
            let b = gen_random_type(rng, depth + 1);
            format!("{}[{}, {}]", base, a, b)
        }
        _ => {
            // Tuple-like or function type
            let a = gen_random_type(rng, depth + 1);
            format!("&{}", a)
        }
    }
}

// ============================================================
// Test 13: Expression depth bomb
// ============================================================

#[test]
fn fuzz_expression_depth() {
    // Moderate depth (50 levels) - should be safe
    let mut expr = "1".to_string();
    for i in 2..=50 {
        expr = format!("({} + {})", expr, i);
    }
    let source = format!("let x = {}", expr);
    fuzz_source_catch_panic(&source, "expr_depth_50", 0);

    // Parenthesized depth (50 levels)
    let mut paren = "1".to_string();
    for _ in 0..50 {
        paren = format!("({})", paren);
    }
    let source2 = format!("let x = {}", paren);
    fuzz_source_catch_panic(&source2, "paren_depth_50", 0);

    // Left-recursive chain
    let chain: String = std::iter::repeat("1 + ").take(100).collect::<String>() + "1";
    let source3 = format!("let x = {}", chain);
    fuzz_source_catch_panic(&source3, "add_chain_100", 0);
}

// ============================================================
// Test 14: Pattern matching chaos
// ============================================================

#[test]
fn fuzz_pattern_matching() {
    // Match with many arms
    let mut arms: String = (0..100)
        .map(|i| format!("    {} => {}\n", i, i * 2))
        .collect();
    arms.push_str("    _ => 0\n");
    let source = format!("fn f(x: i32) -> i32 {{\n    match x {{\n{}}}\n}}", arms);
    fuzz_source_catch_panic(&source, "match_100_arms", 0);

    // Nested match
    let source2 = r#"
fn f(x: i32, y: i32) -> i32 {
    match x {
        1 => match y {
            1 => 1
            2 => 2
            _ => 0
        }
        2 => match y {
            1 => 10
            _ => 20
        }
        _ => 0
    }
}
"#;
    fuzz_source_catch_panic(source2, "nested_match", 0);

    // Wildcard patterns
    let source3 = r#"
fn f(x: i32) -> i32 {
    match x {
        _ => 0
    }
}
"#;
    fuzz_source_catch_panic(source3, "wildcard_only", 0);

    // Random pattern-like things
    let mut rng = Rng::new(66666);
    let patterns = ["_", "0", "1", "true", "false", "nil", "x", "Foo", "Bar(x)"];
    for i in 0..50 {
        let n_arms = rng.next_range(1, 20);
        let arms: String = (0..n_arms)
            .map(|_| format!("    {} => 0\n", rng.choose(&patterns)))
            .collect();
        let source = format!("fn f(x: i32) -> i32 {{\n    match x {{\n{}}}\n}}", arms);
        fuzz_source_catch_panic(&source, "random_match", i);
    }
}

// ============================================================
// Test 15: Trait/impl soup
// ============================================================

#[test]
fn fuzz_trait_impl_soup() {
    let mut source = String::new();

    // 50 traits each with methods
    for i in 0..50 {
        source.push_str(&format!("trait T{} {{\n", i));
        for j in 0..5 {
            source.push_str(&format!("    fn m{}(self) -> i32\n", j));
        }
        source.push_str("}\n\n");
    }

    // 50 structs
    for i in 0..50 {
        source.push_str(&format!("struct S{} {{ val: i32 }}\n\n", i));
    }

    // 50 impls
    for i in 0..50 {
        source.push_str(&format!("impl T{} for S{} {{\n", i, i));
        for j in 0..5 {
            source.push_str(&format!("    fn m{}(self) -> i32 {{ self.val }}\n", j));
        }
        source.push_str("}\n\n");
    }

    fuzz_source_catch_panic(&source, "trait_impl_soup", 0);
}

// ============================================================
// Test 16: Barely valid programs
// ============================================================

#[test]
fn fuzz_barely_valid() {
    let cases: &[&str] = &[
        // Empty
        "",
        // Just newlines
        "\n\n\n",
        // Just spaces
        "   ",
        // Minimal function
        "fn f() {}",
        // Function with return
        "fn f() -> i32 { 0 }",
        // Minimal struct
        "struct S {}",
        // Struct with one field
        "struct S { x: i32 }",
        // Minimal enum
        "enum E { A }",
        // Minimal trait
        "trait T {}",
        // Minimal impl
        "struct S {}\nimpl S {}",
        // Let with no type annotation
        "fn f() { let x = 1 }",
        // Nested blocks
        "fn f() { { { { 1 } } } }",
        // If without else
        "fn f(x: bool) { if x { 1 } }",
        // If with else
        "fn f(x: bool) -> i32 { if x { 1 } else { 2 } }",
        // While loop
        "fn f() { while true { break } }",
        // For loop
        "fn f() { for x in items { } }",
        // Closure
        "fn f() { let c = |x: i32| -> i32 { x + 1 } }",
    ];
    for (i, case) in cases.iter().enumerate() {
        fuzz_source_catch_panic(case, "barely_valid", i);
    }
}

// ============================================================
// Test 17: Programs mixing every feature
// ============================================================

#[test]
fn fuzz_kitchen_sink() {
    let source = r#"
struct Point { x: f64, y: f64 }

enum Shape {
    Circle(f64),
    Rect(f64, f64),
}

trait Drawable {
    fn draw(self) -> String
}

impl Drawable for Shape {
    fn draw(self) -> String {
        match self {
            Shape::Circle(r) => "circle"
            Shape::Rect(w, h) => "rect"
        }
    }
}

fn distance(p1: Point, p2: Point) -> f64 {
    let dx = p2.x - p1.x
    let dy = p2.y - p1.y
    dx * dx + dy * dy
}

fn main() {
    let p = Point { x: 1.0, y: 2.0 }
    let s = Shape::Circle(3.14)
    let msg = s.draw()
    let items = [1, 2, 3, 4, 5]
    for item in items {
        let doubled = item * 2
    }
    let result = if true { 1 } else { 2 }
    let c = |x: i32| -> i32 { x + 1 }
}
"#;
    fuzz_source_catch_panic(source, "kitchen_sink", 0);
}

// ============================================================
// Test 18: Binary/garbage data
// ============================================================

#[test]
fn fuzz_binary_garbage() {
    let mut rng = Rng::new(77777);
    for i in 0..100 {
        let len = rng.next_range(1, 200);
        // Generate random bytes, ensure they're valid UTF-8 by using char range
        let source: String = (0..len)
            .map(|_| {
                let byte = (rng.next() % 256) as u8;
                // Convert to valid UTF-8 by replacing invalid sequences
                if byte < 128 {
                    byte as char
                } else {
                    // Use a safe Unicode char instead
                    char::from_u32((rng.next() % 0x7FF) as u32 + 0x80).unwrap_or('?')
                }
            })
            .collect();
        fuzz_source_catch_panic(&source, "binary_garbage", i);
    }
}

// ============================================================
// Test 19: Extremely long lines
// ============================================================

#[test]
fn fuzz_long_lines() {
    // 100K of spaces
    let spaces = " ".repeat(100_000);
    fuzz_source_catch_panic(&spaces, "long_spaces", 0);

    // 100K of 'a'
    let ident = "a".repeat(100_000);
    fuzz_source_catch_panic(&ident, "long_ident", 0);

    // Long expression on one line
    let long_expr: String = (0..10_000).map(|i| format!("{} + ", i)).collect::<String>() + "0";
    let source = format!("let x = {}", long_expr);
    fuzz_source_catch_panic(&source, "long_expr", 0);

    // Long string literal
    let long_str = format!("\"{}\"", "x".repeat(100_000));
    fuzz_source_catch_panic(&long_str, "long_string", 0);
}

// ============================================================
// Test 20: Alternating valid/invalid
// ============================================================

#[test]
fn fuzz_alternating_valid_invalid() {
    let valid_fragments = [
        "fn f() {}\n",
        "struct S { x: i32 }\n",
        "let x = 1\n",
        "enum E { A, B }\n",
        "trait T { fn m(self) -> i32 }\n",
    ];
    let invalid_fragments = [
        "@@##$$%%^^\n",
        "}}}{{{\n",
        "let let let\n",
        "fn fn fn\n",
        "=== !== <<<\n",
        "\x00\x01\x02\n",
    ];
    let mut rng = Rng::new(88888);
    for i in 0..50 {
        let mut source = String::new();
        let n_fragments = rng.next_range(2, 20);
        for _ in 0..n_fragments {
            if rng.next_bool() {
                source.push_str(valid_fragments[rng.next_usize(valid_fragments.len())]);
            } else {
                source.push_str(invalid_fragments[rng.next_usize(invalid_fragments.len())]);
            }
        }
        fuzz_source_catch_panic(&source, "alternating", i);
    }
}

// ============================================================
// Additional edge cases
// ============================================================

#[test]
fn fuzz_empty_and_whitespace() {
    fuzz_source_catch_panic("", "empty", 0);
    fuzz_source_catch_panic(" ", "space", 0);
    fuzz_source_catch_panic("\n", "newline", 0);
    fuzz_source_catch_panic("\t", "tab", 0);
    fuzz_source_catch_panic("\r\n", "crlf", 0);
    fuzz_source_catch_panic("\r", "cr", 0);
    fuzz_source_catch_panic("\n\n\n\n\n", "many_newlines", 0);
    fuzz_source_catch_panic("   \n   \n   ", "spaces_newlines", 0);
}

#[test]
fn fuzz_single_tokens() {
    // Every single character that could be meaningful
    let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_+-*/%=!<>&|.,:;?@(){}[]\"'\\#~`^$";
    for (i, c) in chars.chars().enumerate() {
        fuzz_source_catch_panic(&c.to_string(), "single_char", i);
    }
}

#[test]
fn fuzz_repeated_single_tokens() {
    let tokens = [
        "(", ")", "{", "}", "[", "]", "+", "-", "*", "/", "=", "!", "<", ">", "&", "|", ".", ",",
        ":", ";", "?", "@", "\\", "#", "~", "`", "^", "$",
    ];
    for (i, tok) in tokens.iter().enumerate() {
        let repeated = tok.repeat(100);
        fuzz_source_catch_panic(&repeated, "repeated_token", i);
    }
}

#[test]
fn fuzz_all_keyword_pairs() {
    let keywords: &[&str] = &[
        "fn", "struct", "enum", "trait", "impl", "let", "mut", "if", "else", "match", "for",
        "while", "return", "break", "continue",
    ];
    for (i, a) in keywords.iter().enumerate() {
        for (j, b) in keywords.iter().enumerate() {
            let source = format!("{} {}", a, b);
            fuzz_source_catch_panic(&source, "keyword_pair", i * keywords.len() + j);
        }
    }
}

#[test]
fn fuzz_generics_stress() {
    // Deeply nested generics
    let mut ty = "i32".to_string();
    for _ in 0..20 {
        ty = format!("Vec[{}]", ty);
    }
    let source = format!("let x: {} = nil", ty);
    fuzz_source_catch_panic(&source, "nested_generics_20", 0);

    // Multiple generic params
    let source2 = "let x: Map[Vec[Option[i32]], Result[String, Vec[Map[i32, bool]]]] = nil";
    fuzz_source_catch_panic(source2, "complex_generic", 0);

    // Random generic expressions
    let mut rng = Rng::new(99999);
    for i in 0..50 {
        let ty = gen_random_type(&mut rng, 0);
        let source = format!("fn f(x: {}) -> {} {{ x }}", ty, ty);
        fuzz_source_catch_panic(&source, "generic_fn", i);
    }
}

#[test]
fn fuzz_string_interpolation_stress() {
    let cases: &[&str] = &[
        r#""hello {world}""#,
        r#""{x} {y} {z}""#,
        r#""{1 + 2}""#,
        r#""{"nested"}""#,
        r#""{if true { 1 } else { 2 }}""#,
        r#""{{""#,          // open brace pair
        r#""}}""#,          // close brace pair
        r#""{""#,           // unterminated interpolation
        r#""}""#,           // stray close brace in string
        r#""{ { { } } }""#, // nested braces
    ];
    for (i, case) in cases.iter().enumerate() {
        fuzz_source_catch_panic(case, "interpolation_stress", i);
    }
}

#[test]
fn fuzz_assignment_variations() {
    let lhs = ["x", "x.y", "x.y.z", "arr[0]", "arr[i]", "*ptr"];
    let ops = ["=", "+=", "-=", "*=", "/=", "%=", ":="];
    let rhs = ["1", "x + y", "f()", "true", "\"hello\"", "[1, 2, 3]"];

    let mut count = 0;
    for l in &lhs {
        for op in &ops {
            for r in &rhs {
                let source = format!("fn f() {{ {} {} {} }}", l, op, r);
                fuzz_source_catch_panic(&source, "assignment", count);
                count += 1;
            }
        }
    }
}

#[test]
fn fuzz_closure_variations() {
    let cases: &[&str] = &[
        "let f = || { 1 }",
        "let f = |x: i32| { x }",
        "let f = |x: i32, y: i32| { x + y }",
        "let f = |x: i32| -> i32 { x + 1 }",
        "let f = || { || { || { 1 } } }", // nested closures
        "let f = |x: i32| { let g = |y: i32| { x + y } }",
    ];
    for (i, case) in cases.iter().enumerate() {
        let source = format!("fn main() {{ {} }}", case);
        fuzz_source_catch_panic(&source, "closure", i);
    }
}

#[test]
fn fuzz_spawn_channel_stress() {
    let cases: &[&str] = &[
        "fn f() { spawn { 1 } }",
        "fn f() { let ch = chan[i32]() }",
        "fn f() { spawn { spawn { spawn { 1 } } } }",
        "fn f() {\n    select {\n    }\n}",
    ];
    for (i, case) in cases.iter().enumerate() {
        fuzz_source_catch_panic(case, "spawn_channel", i);
    }
}
