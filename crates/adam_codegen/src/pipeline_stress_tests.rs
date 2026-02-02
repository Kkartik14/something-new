//! Complete compiler pipeline end-to-end stress tests.
//!
//! These tests feed real Adam programs through the ENTIRE pipeline:
//!   lex -> parse -> resolve -> typecheck -> borrow check -> IR lowering
//! and verify that:
//!   1. Valid programs produce zero errors at every stage
//!   2. Invalid programs produce the RIGHT errors at the right stage
//!   3. Edge case programs don't panic or crash
//!
//! Adam syntax reminders:
//!   - Function params: `name Type` (no colon)
//!   - Struct fields: `name Type` (no colon)
//!   - Let bindings: `x := expr` or `x: Type = expr`
//!   - Mutable bindings: `mut x := expr`
//!   - Return type: `-> Type`

use adam_lexer::Lexer;
use adam_parser::Parser;
use adam_resolve::resolve;
use adam_types::TypeChecker;
use adam_borrow::BorrowChecker;
use adam_ir::lower_module;

// ================================================================
// Pipeline helper
// ================================================================

/// Collects errors from every stage of the pipeline.
#[derive(Debug)]
struct PipelineResult {
    lex_errors: Vec<String>,
    parse_errors: Vec<String>,
    resolve_errors: Vec<String>,
    type_errors: Vec<String>,
    borrow_errors: Vec<String>,
    /// Whether IR lowering completed without panicking.
    ir_ok: bool,
    /// Total number of errors across all stages.
    total_errors: usize,
}

impl PipelineResult {
    fn is_clean(&self) -> bool {
        self.total_errors == 0 && self.ir_ok
    }

    fn has_lex_errors(&self) -> bool {
        !self.lex_errors.is_empty()
    }

    fn has_parse_errors(&self) -> bool {
        !self.parse_errors.is_empty()
    }

    fn has_resolve_errors(&self) -> bool {
        !self.resolve_errors.is_empty()
    }

    fn has_type_errors(&self) -> bool {
        !self.type_errors.is_empty()
    }

    fn has_borrow_errors(&self) -> bool {
        !self.borrow_errors.is_empty()
    }

    fn all_errors(&self) -> Vec<String> {
        let mut all = Vec::new();
        all.extend(self.lex_errors.iter().map(|e| format!("[lex] {}", e)));
        all.extend(self.parse_errors.iter().map(|e| format!("[parse] {}", e)));
        all.extend(self.resolve_errors.iter().map(|e| format!("[resolve] {}", e)));
        all.extend(self.type_errors.iter().map(|e| format!("[type] {}", e)));
        all.extend(self.borrow_errors.iter().map(|e| format!("[borrow] {}", e)));
        all
    }
}

/// Run the full compiler pipeline on source code, collecting all errors.
fn run_pipeline(source: &str) -> PipelineResult {
    // Stage 1: Lex
    let lex_result = Lexer::new(source).tokenize();
    let lex_errors: Vec<String> = lex_result.errors.iter().map(|e| e.to_string()).collect();

    // Stage 2: Parse
    let parse_result = Parser::new(lex_result.tokens).parse();
    let parse_errors: Vec<String> = parse_result.errors.iter().map(|e| e.to_string()).collect();

    // Stage 3: Resolve
    let resolve_result = resolve(&parse_result.ast);
    let resolve_errors: Vec<String> = resolve_result.errors.iter().map(|e| e.to_string()).collect();

    // Stage 4: Type check
    let type_result = TypeChecker::new().check(&parse_result.ast);
    let type_errors: Vec<String> = type_result.errors.iter().map(|e| e.to_string()).collect();

    // Stage 5: Borrow check
    let borrow_result = BorrowChecker::new().check(
        &parse_result.ast,
        Some(&resolve_result),
        Some(&type_result),
    );
    let borrow_errors: Vec<String> = borrow_result.errors.iter().map(|e| e.to_string()).collect();

    // Stage 6: IR lowering (catch panics)
    let ir_ok = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let _ir_module = lower_module(&parse_result.ast);
    }))
    .is_ok();

    let total_errors =
        lex_errors.len() + parse_errors.len() + resolve_errors.len() + type_errors.len() + borrow_errors.len();

    PipelineResult {
        lex_errors,
        parse_errors,
        resolve_errors,
        type_errors,
        borrow_errors,
        ir_ok,
        total_errors,
    }
}

/// Run the pipeline and assert no errors at any stage.
fn assert_pipeline_clean(source: &str) {
    let result = run_pipeline(source);
    assert!(
        result.is_clean(),
        "Expected clean pipeline, but got errors:\n{}\nir_ok: {}",
        result.all_errors().join("\n"),
        result.ir_ok,
    );
}

/// Run the pipeline and assert it does not panic (errors are okay).
fn assert_no_panic(source: &str) {
    let _result = run_pipeline(source);
    // If we got here, no panic occurred.
}

// ================================================================
// 1. Valid programs produce zero errors at every stage
// ================================================================

#[test]
fn pipeline_empty_program() {
    assert_pipeline_clean("");
}

#[test]
fn pipeline_hello_world() {
    assert_pipeline_clean(
        "fn main() {\n    msg := \"Hello, world!\"\n}",
    );
}

#[test]
fn pipeline_simple_function() {
    assert_pipeline_clean(
        "fn add(x i32, y i32) -> i32 {\n    return x + y\n}",
    );
}

#[test]
fn pipeline_function_with_borrow_param() {
    assert_pipeline_clean(
        "fn length(s String) -> i32 {\n    return 42\n}",
    );
}

#[test]
fn pipeline_function_with_mut_param() {
    assert_pipeline_clean(
        "fn increment(mut x i32) -> i32 {\n    return x + 1\n}",
    );
}

#[test]
fn pipeline_function_with_own_param() {
    assert_pipeline_clean(
        "fn consume(own s String) {\n    x := s\n}",
    );
}

#[test]
fn pipeline_multiple_functions() {
    assert_pipeline_clean(
        "fn add(a i32, b i32) -> i32 {\n    return a + b\n}\n\nfn mul(a i32, b i32) -> i32 {\n    return a * b\n}\n\nfn main() {\n    x := add(1, 2)\n    y := mul(3, 4)\n}",
    );
}

#[test]
fn pipeline_struct_definition() {
    assert_pipeline_clean(
        "struct Point {\n    x i32\n    y i32\n}",
    );
}

#[test]
fn pipeline_struct_with_method() {
    assert_pipeline_clean(
        "struct Point {\n    x i32\n    y i32\n}\n\nimpl Point {\n    fn origin() -> Point {\n        return Point { x: 0, y: 0 }\n    }\n}",
    );
}

#[test]
fn pipeline_enum_basic() {
    assert_pipeline_clean(
        "enum Color {\n    Red\n    Green\n    Blue\n}",
    );
}

#[test]
fn pipeline_enum_with_fields() {
    assert_pipeline_clean(
        "enum Shape {\n    Circle(f64)\n    Rectangle(f64, f64)\n}",
    );
}

#[test]
fn pipeline_pattern_matching() {
    assert_pipeline_clean(
        "enum Color {\n    Red\n    Green\n    Blue\n}\n\nfn describe(c Color) -> i32 {\n    match c {\n        Red => 0\n        Green => 1\n        Blue => 2\n    }\n}",
    );
}

#[test]
fn pipeline_if_else() {
    assert_pipeline_clean(
        "fn max(a i32, b i32) -> i32 {\n    if a > b {\n        return a\n    } else {\n        return b\n    }\n}",
    );
}

#[test]
fn pipeline_while_loop() {
    assert_pipeline_clean(
        "fn count_to_ten() -> i32 {\n    mut i := 0\n    while i < 10 {\n        i = i + 1\n    }\n    return i\n}",
    );
}

#[test]
fn pipeline_let_bindings() {
    assert_pipeline_clean(
        "fn main() {\n    x := 42\n    y := x + 1\n    z := y * 2\n}",
    );
}

#[test]
fn pipeline_mutable_variable() {
    assert_pipeline_clean(
        "fn main() {\n    mut x := 0\n    x = 10\n    x = x + 5\n}",
    );
}

#[test]
fn pipeline_nested_if() {
    assert_pipeline_clean(
        "fn classify(n i32) -> i32 {\n    if n > 0 {\n        if n > 100 {\n            return 2\n        }\n        return 1\n    }\n    return 0\n}",
    );
}

#[test]
fn pipeline_boolean_expressions() {
    assert_pipeline_clean(
        "fn logic(a bool, b bool) -> bool {\n    return a && b || !a\n}",
    );
}

#[test]
fn pipeline_string_literal() {
    assert_pipeline_clean(
        "fn greet() {\n    name := \"Adam\"\n}",
    );
}

#[test]
fn pipeline_array_literal() {
    assert_pipeline_clean(
        "fn main() {\n    arr := [1, 2, 3]\n}",
    );
}

#[test]
fn pipeline_tuple_literal() {
    assert_pipeline_clean(
        "fn main() {\n    t := (1, 2, 3)\n}",
    );
}

#[test]
fn pipeline_recursive_function() {
    assert_pipeline_clean(
        "fn fib(n i32) -> i32 {\n    if n <= 1 {\n        return n\n    }\n    return fib(n - 1) + fib(n - 2)\n}",
    );
}

#[test]
fn pipeline_mutual_recursion() {
    assert_pipeline_clean(
        "fn is_even(n i32) -> bool {\n    if n == 0 {\n        return true\n    }\n    return is_odd(n - 1)\n}\n\nfn is_odd(n i32) -> bool {\n    if n == 0 {\n        return false\n    }\n    return is_even(n - 1)\n}",
    );
}

#[test]
fn pipeline_trait_definition() {
    assert_pipeline_clean(
        "trait Printable {\n    fn to_string(self) -> String\n}",
    );
}

#[test]
fn pipeline_trait_impl() {
    assert_pipeline_clean(
        "struct Point {\n    x i32\n    y i32\n}\n\ntrait Describable {\n    fn describe(self) -> i32\n}\n\nimpl Describable for Point {\n    fn describe(self) -> i32 {\n        return self.x + self.y\n    }\n}",
    );
}

#[test]
fn pipeline_closure() {
    assert_pipeline_clean(
        "fn main() {\n    add := |a i32, b i32| a + b\n}",
    );
}

#[test]
fn pipeline_break_continue() {
    assert_pipeline_clean(
        "fn main() {\n    mut i := 0\n    loop {\n        if i >= 10 {\n            break\n        }\n        i = i + 1\n    }\n}",
    );
}

#[test]
fn pipeline_nested_scopes() {
    assert_pipeline_clean(
        "fn main() {\n    x := 1\n    {\n        y := x + 1\n        {\n            z := y + 1\n        }\n    }\n}",
    );
}

#[test]
fn pipeline_float_arithmetic() {
    assert_pipeline_clean(
        "fn main() {\n    a := 3.14\n    b := 2.71\n    c := a + b\n}",
    );
}

#[test]
fn pipeline_char_literal() {
    assert_pipeline_clean(
        "fn main() {\n    c := 'a'\n}",
    );
}

#[test]
fn pipeline_comparison_chain() {
    assert_pipeline_clean(
        "fn clamp(x i32, lo i32, hi i32) -> i32 {\n    if x < lo {\n        return lo\n    }\n    if x > hi {\n        return hi\n    }\n    return x\n}",
    );
}

#[test]
fn pipeline_struct_literal_and_field_access() {
    assert_pipeline_clean(
        "struct Point {\n    x i32\n    y i32\n}\n\nfn main() {\n    p := Point { x: 3, y: 4 }\n    sum := p.x + p.y\n}",
    );
}

// ================================================================
// 2. Invalid programs produce the right errors at the right stage
// ================================================================

// --- Syntax errors (parser should catch) ---

#[test]
fn pipeline_syntax_error_missing_brace() {
    let result = run_pipeline("fn main() {\n    x := 42\n");
    assert!(
        result.has_parse_errors(),
        "Expected parse errors for missing closing brace, got: {:?}",
        result.all_errors()
    );
    assert!(result.ir_ok, "IR lowering should not panic even with parse errors");
}

#[test]
fn pipeline_syntax_error_missing_paren() {
    let result = run_pipeline("fn main( {}");
    assert!(
        result.has_parse_errors(),
        "Expected parse errors for malformed function signature, got: {:?}",
        result.all_errors()
    );
}

#[test]
fn pipeline_syntax_error_unexpected_token() {
    let result = run_pipeline("fn 123() {}");
    assert!(
        result.has_parse_errors(),
        "Expected parse errors for invalid function name, got: {:?}",
        result.all_errors()
    );
}

#[test]
fn pipeline_syntax_error_double_operator() {
    // Should not panic regardless.
    assert_no_panic("fn main() {\n    x := 1 + + 2\n}");
}

// --- Undefined variable errors (resolver should catch) ---

#[test]
fn pipeline_undefined_variable() {
    let result = run_pipeline("fn main() {\n    x := y\n}");
    assert!(
        result.has_resolve_errors(),
        "Expected resolve errors for undefined variable 'y', got: {:?}",
        result.all_errors()
    );
}

#[test]
fn pipeline_undefined_function() {
    let result = run_pipeline("fn main() {\n    x := unknown_func(42)\n}");
    assert!(
        result.has_resolve_errors(),
        "Expected resolve errors for undefined function, got: {:?}",
        result.all_errors()
    );
}

#[test]
fn pipeline_undefined_type() {
    let result = run_pipeline("fn main() {\n    x: UnknownType = 42\n}");
    assert!(
        result.has_resolve_errors(),
        "Expected resolve errors for undefined type, got: {:?}",
        result.all_errors()
    );
}

// --- Type errors (type checker should catch) ---

#[test]
fn pipeline_type_error_arithmetic_on_bool() {
    let result = run_pipeline("fn main() {\n    x: bool = true\n    y := x + 1\n}");
    assert!(
        result.has_type_errors(),
        "Expected type errors for bool + int, got: {:?}",
        result.all_errors()
    );
}

#[test]
fn pipeline_type_error_wrong_arg_count() {
    let result = run_pipeline("fn add(a i32, b i32) -> i32 {\n    return a + b\n}\n\nfn main() {\n    x := add(1, 2, 3)\n}");
    assert!(
        result.has_type_errors(),
        "Expected type errors for wrong argument count, got: {:?}",
        result.all_errors()
    );
}

#[test]
fn pipeline_type_error_return_type_mismatch() {
    let result = run_pipeline("fn get_int() -> i32 {\n    return \"hello\"\n}");
    assert!(
        result.has_type_errors(),
        "Expected type errors for return type mismatch, got: {:?}",
        result.all_errors()
    );
}

#[test]
fn pipeline_type_error_condition_not_bool() {
    let result = run_pipeline("fn main() {\n    if 42 {\n        x := 1\n    }\n}");
    assert!(
        result.has_type_errors(),
        "Expected type errors for non-bool condition, got: {:?}",
        result.all_errors()
    );
}

#[test]
fn pipeline_type_error_wrong_field_type() {
    let result = run_pipeline("struct Point {\n    x i32\n    y i32\n}\n\nfn main() {\n    p := Point { x: \"hello\", y: 2 }\n}");
    assert!(
        result.has_type_errors(),
        "Expected type errors for wrong field type, got: {:?}",
        result.all_errors()
    );
}

#[test]
fn pipeline_type_error_missing_struct_field() {
    let result = run_pipeline("struct Point {\n    x i32\n    y i32\n}\n\nfn main() {\n    p := Point { x: 1 }\n}");
    assert!(
        result.has_type_errors(),
        "Expected type errors for missing struct field, got: {:?}",
        result.all_errors()
    );
}

// --- Borrow errors (borrow checker should catch) ---

#[test]
fn pipeline_borrow_error_use_after_move() {
    let result = run_pipeline("fn consume(own s String) {}\n\nfn main() {\n    s := \"hello\"\n    consume(s)\n    consume(s)\n}");
    assert!(
        result.has_borrow_errors(),
        "Expected borrow errors for use-after-move, got: {:?}",
        result.all_errors()
    );
}

#[test]
fn pipeline_borrow_error_assign_to_immutable() {
    let result = run_pipeline("fn main() {\n    x := 42\n    x = 10\n}");
    assert!(
        result.has_borrow_errors(),
        "Expected borrow errors for assigning to immutable variable, got: {:?}",
        result.all_errors()
    );
}

// ================================================================
// 3. Edge case programs don't panic or crash
// ================================================================

#[test]
fn pipeline_edge_empty_function_body() {
    assert_no_panic("fn main() {}");
}

#[test]
fn pipeline_edge_only_comments() {
    assert_no_panic("// This is a comment\n/* Block comment */");
}

#[test]
fn pipeline_edge_empty_struct() {
    assert_no_panic("struct Empty {}");
}

#[test]
fn pipeline_edge_empty_enum() {
    assert_no_panic("enum Nothing {}");
}

#[test]
fn pipeline_edge_deeply_nested_blocks() {
    let mut source = String::from("fn main() {\n");
    for _ in 0..20 {
        source.push_str("{\n");
    }
    source.push_str("x := 1\n");
    for _ in 0..20 {
        source.push_str("}\n");
    }
    source.push_str("}\n");
    assert_no_panic(&source);
}

#[test]
fn pipeline_edge_deeply_nested_if() {
    let mut source = String::from("fn main() {\n");
    for _ in 0..15 {
        source.push_str("if true {\n");
    }
    source.push_str("x := 42\n");
    for _ in 0..15 {
        source.push_str("}\n");
    }
    source.push_str("}\n");
    assert_no_panic(&source);
}

#[test]
fn pipeline_edge_deeply_nested_expressions() {
    // Build: ((((((1 + 1) + 1) + 1) ...)))
    let mut expr = String::from("1");
    for _ in 0..30 {
        expr = format!("({} + 1)", expr);
    }
    let source = format!("fn main() {{\n    x := {}\n}}\n", expr);
    assert_no_panic(&source);
}

#[test]
fn pipeline_edge_many_parameters() {
    let mut params = String::new();
    for i in 0..20 {
        if i > 0 {
            params.push_str(", ");
        }
        params.push_str(&format!("p{} i32", i));
    }
    let source = format!("fn many_params({}) {{}}\n", params);
    assert_no_panic(&source);
}

#[test]
fn pipeline_edge_many_let_bindings() {
    let mut source = String::from("fn main() {\n");
    for i in 0..50 {
        source.push_str(&format!("    x{} := {}\n", i, i));
    }
    source.push_str("}\n");
    assert_no_panic(&source);
}

#[test]
fn pipeline_edge_unicode_identifier_in_string() {
    assert_no_panic("fn main() {\n    greeting := \"Hello, world!\"\n}");
}

#[test]
fn pipeline_edge_empty_string() {
    assert_no_panic("fn main() {\n    s := \"\"\n}");
}

#[test]
fn pipeline_edge_multiple_returns() {
    assert_no_panic(
        "fn sign(x i32) -> i32 {\n    if x > 0 {\n        return 1\n    }\n    if x < 0 {\n        return -1\n    }\n    return 0\n}",
    );
}

#[test]
fn pipeline_edge_single_line_function() {
    assert_no_panic("fn id(x i32) -> i32 { return x }");
}

#[test]
fn pipeline_edge_expression_statement() {
    assert_no_panic("fn main() {\n    42\n    true\n    \"hello\"\n}");
}

#[test]
fn pipeline_edge_negation() {
    assert_no_panic("fn main() {\n    x := -42\n    y := !true\n}");
}

#[test]
fn pipeline_edge_complex_boolean() {
    assert_no_panic("fn check(a bool, b bool, c bool) -> bool {\n    return (a && b) || (!a && c)\n}");
}

// ================================================================
// 4. Large programs (stress tests)
// ================================================================

#[test]
fn pipeline_large_program_50_functions() {
    let mut source = String::new();
    for i in 0..50 {
        source.push_str(&format!(
            "fn func_{}(x i32) -> i32 {{\n    return x + {}\n}}\n\n",
            i, i
        ));
    }
    // Add a main that calls a few of them.
    source.push_str("fn main() {\n");
    source.push_str("    a := func_0(1)\n");
    source.push_str("    b := func_25(2)\n");
    source.push_str("    c := func_49(3)\n");
    source.push_str("}\n");

    let result = run_pipeline(&source);
    assert!(result.ir_ok, "IR lowering should succeed for large program");
    assert!(result.lex_errors.is_empty(), "Lex errors: {:?}", result.lex_errors);
    assert!(result.parse_errors.is_empty(), "Parse errors: {:?}", result.parse_errors);
}

#[test]
fn pipeline_large_program_many_structs() {
    let mut source = String::new();
    for i in 0..20 {
        source.push_str(&format!(
            "struct S{} {{\n    field_a i32\n    field_b i32\n}}\n\n",
            i
        ));
    }
    let result = run_pipeline(&source);
    assert!(result.ir_ok, "IR lowering panicked for many structs");
    assert!(result.lex_errors.is_empty());
    assert!(result.parse_errors.is_empty());
}

#[test]
fn pipeline_large_program_many_enums() {
    let mut source = String::new();
    for i in 0..10 {
        source.push_str(&format!(
            "enum E{i} {{\n    A{i}\n    B{i}\n    C{i}\n}}\n\n",
        ));
    }
    let result = run_pipeline(&source);
    assert!(result.ir_ok, "IR lowering panicked for many enums");
    assert!(result.lex_errors.is_empty());
    assert!(result.parse_errors.is_empty());
}

#[test]
fn pipeline_large_program_call_chain() {
    let mut source = String::new();
    for i in 0..20 {
        if i < 19 {
            source.push_str(&format!(
                "fn f{}(x i32) -> i32 {{\n    return f{}(x + 1)\n}}\n\n",
                i, i + 1
            ));
        } else {
            source.push_str(&format!(
                "fn f{}(x i32) -> i32 {{\n    return x\n}}\n\n",
                i
            ));
        }
    }
    source.push_str("fn main() {\n    result := f0(0)\n}\n");

    let result = run_pipeline(&source);
    assert!(result.ir_ok, "IR lowering panicked for call chain");
    assert!(result.lex_errors.is_empty());
    assert!(result.parse_errors.is_empty());
}

// ================================================================
// 5. Specific language feature coverage
// ================================================================

#[test]
fn pipeline_shadowing_in_nested_scopes() {
    let result = run_pipeline(
        "fn main() {\n    x := 1\n    {\n        x := 2\n        {\n            x := 3\n        }\n    }\n}",
    );
    // Shadowing is legal; this should at least not panic.
    assert!(result.ir_ok);
}

#[test]
fn pipeline_match_with_wildcard() {
    assert_pipeline_clean(
        "fn describe(x i32) -> i32 {\n    match x {\n        0 => 0\n        1 => 1\n        _ => 2\n    }\n}",
    );
}

#[test]
fn pipeline_match_with_binding() {
    assert_pipeline_clean(
        "fn echo(x i32) -> i32 {\n    match x {\n        n => n\n    }\n}",
    );
}

#[test]
fn pipeline_enum_pattern_with_fields() {
    assert_pipeline_clean(
        "enum Shape {\n    Circle(f64)\n    Rect(f64, f64)\n}\n\nfn area(s Shape) -> f64 {\n    match s {\n        Circle(r) => r\n        Rect(w, h) => w\n    }\n}",
    );
}

#[test]
fn pipeline_loop_with_break() {
    assert_pipeline_clean(
        "fn find_limit() -> i32 {\n    mut n := 0\n    loop {\n        if n >= 100 {\n            break\n        }\n        n = n + 1\n    }\n    return n\n}",
    );
}

#[test]
fn pipeline_for_loop_over_range() {
    assert_no_panic("fn main() {\n    for i in 0..10 {\n        x := i\n    }\n}");
}

#[test]
fn pipeline_struct_as_function_param() {
    assert_pipeline_clean(
        "struct Rect {\n    w i32\n    h i32\n}\n\nfn area(r Rect) -> i32 {\n    return r.w * r.h\n}\n\nfn main() {\n    r := Rect { w: 5, h: 3 }\n    a := area(r)\n}",
    );
}

#[test]
fn pipeline_multiple_structs_interacting() {
    assert_pipeline_clean(
        "struct Point {\n    x i32\n    y i32\n}\n\nstruct Rect {\n    origin Point\n    width i32\n    height i32\n}",
    );
}

#[test]
fn pipeline_arithmetic_all_ops() {
    assert_pipeline_clean(
        "fn all_ops(a i32, b i32) -> i32 {\n    sum := a + b\n    diff := a - b\n    prod := a * b\n    quot := a / b\n    rem := a % b\n    return sum + diff + prod + quot + rem\n}",
    );
}

#[test]
fn pipeline_comparison_all_ops() {
    assert_pipeline_clean(
        "fn all_comparisons(a i32, b i32) -> bool {\n    eq := a == b\n    ne := a != b\n    lt := a < b\n    gt := a > b\n    le := a <= b\n    ge := a >= b\n    return eq\n}",
    );
}

#[test]
fn pipeline_complex_struct_and_impl() {
    assert_pipeline_clean(
        "struct Counter {\n    value i32\n}\n\nimpl Counter {\n    fn new() -> Counter {\n        return Counter { value: 0 }\n    }\n\n    fn get(self) -> i32 {\n        return self.value\n    }\n}",
    );
}

#[test]
fn pipeline_enum_with_match_all_variants() {
    assert_pipeline_clean(
        "enum Direction {\n    North\n    South\n    East\n    West\n}\n\nfn to_int(d Direction) -> i32 {\n    match d {\n        North => 0\n        South => 1\n        East => 2\n        West => 3\n    }\n}",
    );
}

#[test]
fn pipeline_nested_function_calls() {
    assert_pipeline_clean(
        "fn inc(x i32) -> i32 {\n    return x + 1\n}\n\nfn main() {\n    result := inc(inc(inc(inc(inc(0)))))\n}",
    );
}

#[test]
fn pipeline_while_loop_with_complex_condition() {
    assert_pipeline_clean(
        "fn main() {\n    mut i := 0\n    mut j := 100\n    while i < 50 && j > 50 {\n        i = i + 1\n        j = j - 1\n    }\n}",
    );
}

#[test]
fn pipeline_multiple_let_types() {
    assert_pipeline_clean(
        "fn main() {\n    a: i32 = 1\n    b: f64 = 2.0\n    c: bool = true\n    d: String = \"hello\"\n    e: char = 'x'\n}",
    );
}

#[test]
fn pipeline_factorial_iterative() {
    assert_pipeline_clean(
        "fn factorial(n i32) -> i32 {\n    mut result := 1\n    mut i := 2\n    while i <= n {\n        result = result * i\n        i = i + 1\n    }\n    return result\n}",
    );
}

#[test]
fn pipeline_gcd() {
    assert_pipeline_clean(
        "fn gcd(a i32, b i32) -> i32 {\n    if b == 0 {\n        return a\n    }\n    return gcd(b, a % b)\n}",
    );
}

#[test]
fn pipeline_power_function() {
    assert_pipeline_clean(
        "fn power(base i32, exp i32) -> i32 {\n    if exp == 0 {\n        return 1\n    }\n    return base * power(base, exp - 1)\n}",
    );
}

// ================================================================
// 6. Error detection accuracy tests
// ================================================================

#[test]
fn pipeline_error_only_at_resolve_stage() {
    let result = run_pipeline("fn main() {\n    x := undefined_var\n}");
    // Lex and parse should be clean.
    assert!(!result.has_lex_errors(), "Unexpected lex errors: {:?}", result.lex_errors);
    assert!(!result.has_parse_errors(), "Unexpected parse errors: {:?}", result.parse_errors);
    // Resolve should catch the undefined variable.
    assert!(result.has_resolve_errors(), "Expected resolve errors for undefined variable");
    // IR should still work (on the malformed AST).
    assert!(result.ir_ok, "IR lowering should not panic");
}

#[test]
fn pipeline_error_only_at_type_stage() {
    let result = run_pipeline("fn add(a i32, b i32) -> i32 {\n    return a + b\n}\n\nfn main() {\n    x := add(1)\n}");
    assert!(!result.has_lex_errors());
    assert!(!result.has_parse_errors());
    // Type checker should catch wrong number of arguments.
    assert!(result.has_type_errors(), "Expected type errors for wrong arg count");
}

#[test]
fn pipeline_error_only_at_borrow_stage() {
    let result = run_pipeline("fn take(own s String) {}\n\nfn main() {\n    s := \"hello\"\n    take(s)\n    take(s)\n}");
    assert!(!result.has_lex_errors());
    assert!(!result.has_parse_errors());
    // Borrow checker should catch use-after-move.
    assert!(
        result.has_borrow_errors(),
        "Expected borrow errors for double move, got: {:?}",
        result.all_errors()
    );
}

#[test]
fn pipeline_error_cascading_does_not_panic() {
    // Program with errors at multiple stages.
    let result = run_pipeline("fn main() {\n    x := unknown\n    y: bool = x + 1\n    y = 42\n}");
    // Should have resolve errors for 'unknown'.
    assert!(result.has_resolve_errors());
    // Should not panic.
    assert!(result.ir_ok);
}

// ================================================================
// 7. Additional robustness tests
// ================================================================

#[test]
fn pipeline_all_literals() {
    assert_no_panic(
        "fn main() {\n    a := 42\n    b := 3.14\n    c := true\n    d := false\n    e := 'z'\n    f := \"hello\"\n    g := nil\n}",
    );
}

#[test]
fn pipeline_assignment_operators() {
    assert_no_panic("fn main() {\n    mut x := 10\n    x = x + 5\n    x = x - 3\n    x = x * 2\n}");
}

#[test]
fn pipeline_empty_match() {
    // An empty match may be an error but should not crash.
    assert_no_panic("fn main() {\n    x := 42\n    match x {}\n}");
}

#[test]
fn pipeline_match_on_bool() {
    assert_no_panic("fn main() {\n    b := true\n    match b {\n        true => 1\n        false => 0\n    }\n}");
}

#[test]
fn pipeline_many_nested_function_calls() {
    // Build deeply nested function call: f(f(f(f(...f(0)...))))
    let mut expr = String::from("0");
    for _ in 0..20 {
        expr = format!("inc({})", expr);
    }
    let source = format!(
        "fn inc(x i32) -> i32 {{ return x + 1 }}\nfn main() {{\n    result := {}\n}}\n",
        expr
    );
    assert_no_panic(&source);
}

#[test]
fn pipeline_only_struct_no_functions() {
    assert_no_panic("struct A {\n    x i32\n}\nstruct B {\n    y f64\n}");
}

#[test]
fn pipeline_only_enums_no_functions() {
    assert_no_panic(
        "enum Fruit {\n    Apple\n    Banana\n    Cherry\n}\n\nenum Veggie {\n    Carrot\n    Pea\n}",
    );
}

#[test]
fn pipeline_struct_with_all_basic_types() {
    assert_no_panic(
        "struct Everything {\n    a i32\n    b i64\n    c f32\n    d f64\n    e bool\n    f char\n    g String\n}",
    );
}

#[test]
fn pipeline_function_returning_struct() {
    assert_pipeline_clean(
        "struct Pair {\n    first i32\n    second i32\n}\n\nfn make_pair(a i32, b i32) -> Pair {\n    return Pair { first: a, second: b }\n}",
    );
}

#[test]
fn pipeline_multiple_impl_blocks() {
    assert_pipeline_clean(
        "struct Point {\n    x i32\n    y i32\n}\n\nimpl Point {\n    fn zero() -> Point {\n        return Point { x: 0, y: 0 }\n    }\n}\n\nimpl Point {\n    fn unit() -> Point {\n        return Point { x: 1, y: 1 }\n    }\n}",
    );
}

#[test]
fn pipeline_semicolons_as_terminators() {
    assert_no_panic("fn main() { x := 1; y := 2; }");
}

#[test]
fn pipeline_completely_garbage_input() {
    assert_no_panic("@#$%^&*");
}

#[test]
fn pipeline_only_whitespace() {
    assert_no_panic("   \n\n\t\t  \n  ");
}

#[test]
fn pipeline_very_long_identifier() {
    let long_name: String = (0..200).map(|_| 'a').collect();
    let source = format!("fn {}() {{}}\n", long_name);
    assert_no_panic(&source);
}

#[test]
fn pipeline_unterminated_string() {
    let result = run_pipeline("fn main() { s := \"unterminated }");
    assert!(
        result.has_lex_errors() || result.has_parse_errors(),
        "Expected errors for unterminated string, got: {:?}",
        result.all_errors()
    );
}

#[test]
fn pipeline_unterminated_block_comment() {
    let result = run_pipeline("/* this comment never ends");
    assert!(
        result.has_lex_errors(),
        "Expected lex errors for unterminated block comment, got: {:?}",
        result.all_errors()
    );
}

#[test]
fn pipeline_mixed_valid_and_invalid() {
    let result = run_pipeline("fn good() -> i32 {\n    return 42\n}\n\nfn bad() -> i32 {\n    return undefined\n}");
    assert!(result.has_resolve_errors());
    assert!(result.ir_ok);
}

#[test]
fn pipeline_recursive_struct_reference() {
    assert_no_panic("struct Node {\n    value i32\n    next ?Node\n}");
}

#[test]
fn pipeline_trait_with_multiple_methods() {
    assert_pipeline_clean(
        "trait Animal {\n    fn name(self) -> String\n    fn sound(self) -> String\n    fn legs(self) -> i32\n}",
    );
}

#[test]
fn pipeline_impl_with_self_methods() {
    assert_pipeline_clean(
        "struct Vec2 {\n    x f64\n    y f64\n}\n\nimpl Vec2 {\n    fn length(self) -> f64 {\n        return self.x + self.y\n    }\n\n    fn zero() -> Vec2 {\n        return Vec2 { x: 0.0, y: 0.0 }\n    }\n}",
    );
}

#[test]
fn pipeline_complex_program_combined() {
    assert_no_panic(
        "struct Point {\n    x i32\n    y i32\n}\n\nenum Shape {\n    Circle(i32)\n    Rectangle(i32, i32)\n}\n\ntrait HasArea {\n    fn area(self) -> i32\n}\n\nfn distance(a Point, b Point) -> i32 {\n    dx := a.x - b.x\n    dy := a.y - b.y\n    return dx * dx + dy * dy\n}\n\nfn main() {\n    origin := Point { x: 0, y: 0 }\n    p := Point { x: 3, y: 4 }\n    d := distance(origin, p)\n\n    mut sum := 0\n    mut i := 0\n    while i < 10 {\n        sum = sum + i\n        i = i + 1\n    }\n\n    shape := Circle(5)\n    match shape {\n        Circle(r) => r\n        Rectangle(w, h) => w\n    }\n}",
    );
}

#[test]
fn pipeline_fibonacci_both_versions() {
    assert_pipeline_clean(
        "fn fib_recursive(n i32) -> i32 {\n    if n <= 1 {\n        return n\n    }\n    return fib_recursive(n - 1) + fib_recursive(n - 2)\n}\n\nfn fib_iterative(n i32) -> i32 {\n    if n <= 1 { return n }\n    mut a := 0\n    mut b := 1\n    mut i := 2\n    while i <= n {\n        c := a + b\n        a = b\n        b = c\n        i = i + 1\n    }\n    return b\n}",
    );
}

#[test]
fn pipeline_multiple_errors_in_one_program() {
    let result = run_pipeline("fn main() {\n    x := undefined1\n    y := undefined2\n    z := undefined3\n}");
    assert!(
        result.resolve_errors.len() >= 3,
        "Expected at least 3 resolve errors, got {}: {:?}",
        result.resolve_errors.len(),
        result.resolve_errors
    );
}

#[test]
fn pipeline_stress_many_variables() {
    let mut source = String::from("fn main() {\n");
    for i in 0..100 {
        source.push_str(&format!("    var_{} := {}\n", i, i));
    }
    // Use the last few variables.
    source.push_str("    sum := var_97 + var_98 + var_99\n");
    source.push_str("}\n");
    assert_no_panic(&source);
}

#[test]
fn pipeline_stress_many_if_branches() {
    let mut source = String::from("fn classify(x i32) -> i32 {\n");
    for i in 0..20 {
        if i == 0 {
            source.push_str(&format!("    if x == {} {{\n        return {}\n    }}", i, i));
        } else {
            source.push_str(&format!(" else if x == {} {{\n        return {}\n    }}", i, i));
        }
    }
    source.push_str("\n    return -1\n}\n");
    assert_no_panic(&source);
}

#[test]
fn pipeline_stress_large_match() {
    let mut source = String::from("fn classify(x i32) -> i32 {\n    match x {\n");
    for i in 0..20 {
        source.push_str(&format!("        {} => {}\n", i, i * 10));
    }
    source.push_str("        _ => -1\n");
    source.push_str("    }\n}\n");
    assert_no_panic(&source);
}
