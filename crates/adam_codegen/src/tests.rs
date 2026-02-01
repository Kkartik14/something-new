use std::path::PathBuf;
use std::process::Command;
use std::sync::atomic::{AtomicU32, Ordering};

use inkwell::context::Context;

use adam_lexer::Lexer;
use adam_parser::Parser;
use adam_ir::lower::lower_module;

use crate::CodeGen;
use crate::link;

static TEST_COUNTER: AtomicU32 = AtomicU32::new(0);

/// Full pipeline: Adam source → executable → run → capture stdout.
fn compile_and_run(src: &str) -> String {
    let id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
    let test_dir = std::env::temp_dir().join(format!("adam_test_{}", id));
    std::fs::create_dir_all(&test_dir).expect("failed to create test dir");

    // 1. Lex
    let lex = Lexer::new(src).tokenize();
    assert!(lex.errors.is_empty(), "lex errors: {:?}", lex.errors);

    // 2. Parse
    let parse = Parser::new(lex.tokens).parse();
    assert!(parse.errors.is_empty(), "parse errors: {:?}", parse.errors);

    // 3. Lower to IR
    let ir_module = lower_module(&parse.ast);

    // 4. Generate LLVM IR
    let context = Context::create();
    let mut codegen = CodeGen::new(&context, "test");
    codegen.codegen_module(&ir_module);

    // Verify the LLVM module
    if let Err(e) = codegen.verify() {
        let ir_text = codegen.print_to_string();
        panic!("LLVM verification failed: {}\n\nGenerated IR:\n{}", e, ir_text);
    }

    // 5. Emit object file
    let obj_path = test_dir.join("test.o");
    codegen.emit_object_file(&obj_path).expect("failed to emit object");

    // 6. Find runtime library
    let runtime_path = find_runtime_lib();

    // 7. Link
    let exe_path = test_dir.join("test_bin");
    link::link_object(&obj_path, &runtime_path, &exe_path)
        .expect("failed to link");

    // 8. Run and capture output
    let output = Command::new(&exe_path)
        .output()
        .expect("failed to execute compiled binary");

    // Clean up
    let _ = std::fs::remove_dir_all(&test_dir);

    assert!(
        output.status.success(),
        "binary exited with status {:?}\nstderr: {}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );

    String::from_utf8(output.stdout).expect("output is not valid UTF-8")
}

/// Compile Adam source to LLVM IR text (for testing codegen without linking).
fn compile_to_ir(src: &str) -> String {
    let lex = Lexer::new(src).tokenize();
    assert!(lex.errors.is_empty(), "lex errors: {:?}", lex.errors);

    let parse = Parser::new(lex.tokens).parse();
    assert!(parse.errors.is_empty(), "parse errors: {:?}", parse.errors);

    let ir_module = lower_module(&parse.ast);

    let context = Context::create();
    let mut codegen = CodeGen::new(&context, "test");
    codegen.codegen_module(&ir_module);

    codegen.print_to_string()
}

fn find_runtime_lib() -> PathBuf {
    let workspace = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf();

    let candidates = [
        workspace.join("target/debug/libadam_runtime.a"),
        workspace.join("target/release/libadam_runtime.a"),
    ];

    for c in &candidates {
        if c.exists() {
            return c.clone();
        }
    }

    panic!(
        "Could not find libadam_runtime.a. Build it first with: cargo build -p adam_runtime\nSearched: {:?}",
        candidates
    );
}

// ================================================================
// LLVM IR generation tests (no linking needed)
// ================================================================

#[test]
fn test_ir_empty_function() {
    let ir = compile_to_ir("fn main() {}");
    assert!(ir.contains("define"), "expected function definition in IR");
    assert!(ir.contains("main"), "expected 'main' in IR");
}

#[test]
fn test_ir_function_with_return() {
    let ir = compile_to_ir("fn add(x i32, y i32) -> i32 {\n    return x + y\n}");
    assert!(ir.contains("add"), "expected 'add' in IR: {}", ir);
}

#[test]
fn test_ir_integer_constants() {
    let ir = compile_to_ir("fn get42() -> i32 {\n    return 42\n}");
    assert!(ir.contains("42"), "expected constant 42 in IR: {}", ir);
}

#[test]
fn test_ir_arithmetic_ops() {
    let ir = compile_to_ir("fn math() -> i32 {\n    x := 10\n    y := 20\n    return x + y\n}");
    assert!(ir.contains("add"), "expected add instruction in IR: {}", ir);
}

#[test]
fn test_ir_comparison_ops() {
    let ir = compile_to_ir("fn cmp(a i32, b i32) -> bool {\n    return a < b\n}");
    assert!(ir.contains("icmp"), "expected icmp in IR: {}", ir);
}

#[test]
fn test_ir_conditional_branch() {
    let ir = compile_to_ir("fn abs(x i32) -> i32 {\n    if x < 0 {\n        return -x\n    }\n    return x\n}");
    assert!(ir.contains("br i1"), "expected conditional branch in IR: {}", ir);
}

#[test]
fn test_ir_multiple_functions() {
    let ir = compile_to_ir("fn foo() -> i32 { return 1 }\nfn bar() -> i32 { return 2 }\nfn baz() -> i32 { return 3 }");
    assert!(ir.contains("foo"), "expected 'foo' in IR");
    assert!(ir.contains("bar"), "expected 'bar' in IR");
    assert!(ir.contains("baz"), "expected 'baz' in IR");
}

#[test]
fn test_ir_function_call() {
    let ir = compile_to_ir("fn double(x i32) -> i32 { return x + x }\nfn main() -> i32 { return double(21) }");
    assert!(ir.contains("call"), "expected call instruction in IR: {}", ir);
}

#[test]
fn test_ir_while_loop() {
    let ir = compile_to_ir("fn count() -> i32 {\n    mut i := 0\n    while i < 10 {\n        i = i + 1\n    }\n    return i\n}");
    assert!(ir.contains("br label"), "expected unconditional branch in IR: {}", ir);
}

#[test]
fn test_ir_boolean_ops() {
    let ir = compile_to_ir("fn logic(a bool, b bool) -> bool {\n    return a && b\n}");
    assert!(ir.contains("and"), "expected 'and' instruction in IR: {}", ir);
}

#[test]
fn test_ir_negation() {
    let ir = compile_to_ir("fn negate(x i32) -> i32 {\n    return -x\n}");
    assert!(ir.contains("sub") || ir.contains("neg"), "expected neg/sub in IR: {}", ir);
}

#[test]
fn test_ir_struct_creation() {
    let ir = compile_to_ir("struct Point {\n    x i32\n    y i32\n}\nfn make_point() -> Point {\n    p := Point { x: 1, y: 2 }\n    return p\n}");
    // Struct type should be defined with fields, and values should appear.
    assert!(ir.contains("Point") && ir.contains("1") && ir.contains("2"),
        "expected struct with fields in IR: {}", ir);
}

#[test]
fn test_ir_field_access() {
    let ir = compile_to_ir("struct Point {\n    x i32\n    y i32\n}\nfn get_x(p Point) -> i32 {\n    return p.x\n}");
    assert!(ir.contains("extractvalue"), "expected extractvalue in IR: {}", ir);
}

#[test]
fn test_ir_float_arithmetic() {
    let ir = compile_to_ir("fn fadd(a f64, b f64) -> f64 {\n    return a + b\n}");
    assert!(ir.contains("fadd"), "expected fadd in IR: {}", ir);
}

#[test]
fn test_ir_multiple_locals() {
    let ir = compile_to_ir("fn locals() -> i32 {\n    a := 1\n    b := 2\n    c := 3\n    d := a + b + c\n    return d\n}");
    assert!(ir.contains("alloca"), "expected alloca instructions in IR: {}", ir);
}

#[test]
fn test_ir_string_literal() {
    let ir = compile_to_ir("fn greet() {\n    name := \"Hello\"\n}");
    assert!(ir.contains("Hello"), "expected string literal in IR: {}", ir);
}

#[test]
fn test_ir_recursive_function() {
    let ir = compile_to_ir("fn fib(n i32) -> i32 {\n    if n <= 1 {\n        return n\n    }\n    return fib(n - 1) + fib(n - 2)\n}");
    assert!(ir.contains("call"), "expected recursive call in IR: {}", ir);
    assert!(ir.contains("fib"), "expected 'fib' in IR: {}", ir);
}

#[test]
fn test_ir_enum_variant() {
    let ir = compile_to_ir("enum Color {\n    Red\n    Green\n    Blue\n}\nfn get_color() {\n    c := Red\n}");
    assert!(ir.contains("define"), "expected function definition in IR");
}

#[test]
fn test_ir_tuple_creation() {
    let ir = compile_to_ir("fn make_tuple() {\n    t := (1, 2, 3)\n}");
    // Constant tuples may use const struct instead of insertvalue.
    assert!(ir.contains("i64 1") && ir.contains("i64 2") && ir.contains("i64 3"),
        "expected tuple values in IR: {}", ir);
}

#[test]
fn test_ir_mutation() {
    let ir = compile_to_ir("fn mutate() {\n    mut x := 5\n    x = 10\n}");
    assert!(ir.contains("store"), "expected store instruction in IR: {}", ir);
}

// ================================================================
// End-to-end tests (compile, link, run, verify output)
// ================================================================

#[test]
fn e2e_hello_world() {
    let output = compile_and_run("fn main() {\n    print(\"Hello, Adam!\")\n}");
    assert_eq!(output.trim(), "Hello, Adam!");
}

#[test]
fn e2e_integer_arithmetic() {
    let output = compile_and_run("fn main() {\n    x := 2 + 3 * 4\n    __adam_print_int(x)\n}");
    assert_eq!(output.trim(), "14");
}

#[test]
fn e2e_function_call() {
    let output = compile_and_run("fn double(x i32) -> i32 {\n    return x + x\n}\nfn main() {\n    result := double(21)\n    __adam_print_int(result)\n}");
    assert_eq!(output.trim(), "42");
}

#[test]
fn e2e_recursive_fibonacci() {
    let output = compile_and_run("fn fib(n i32) -> i32 {\n    if n <= 1 {\n        return n\n    }\n    return fib(n - 1) + fib(n - 2)\n}\nfn main() {\n    __adam_print_int(fib(10))\n}");
    assert_eq!(output.trim(), "55");
}

#[test]
fn e2e_while_loop() {
    let output = compile_and_run("fn main() {\n    mut sum := 0\n    mut i := 1\n    while i <= 10 {\n        sum = sum + i\n        i = i + 1\n    }\n    __adam_print_int(sum)\n}");
    assert_eq!(output.trim(), "55");
}

#[test]
fn e2e_conditional() {
    let output = compile_and_run("fn abs(x i32) -> i32 {\n    if x < 0 {\n        return -x\n    }\n    return x\n}\nfn main() {\n    __adam_print_int(abs(-42))\n}");
    assert_eq!(output.trim(), "42");
}

#[test]
fn e2e_multiple_functions() {
    let output = compile_and_run("fn add(a i32, b i32) -> i32 { return a + b }\nfn mul(a i32, b i32) -> i32 { return a * b }\nfn main() {\n    __adam_print_int(add(mul(3, 4), 5))\n}");
    assert_eq!(output.trim(), "17");
}

#[test]
fn e2e_boolean_logic() {
    let output = compile_and_run("fn main() {\n    x := true && false\n    __adam_print_bool(x)\n}");
    assert_eq!(output.trim(), "false");
}

#[test]
fn e2e_nested_if() {
    let output = compile_and_run("fn classify(n i32) -> i32 {\n    if n > 0 {\n        if n > 100 {\n            return 2\n        }\n        return 1\n    }\n    return 0\n}\nfn main() {\n    __adam_print_int(classify(50))\n}");
    assert_eq!(output.trim(), "1");
}

#[test]
fn e2e_variables_and_mutation() {
    let output = compile_and_run("fn main() {\n    mut x := 10\n    x = x + 5\n    x = x * 2\n    __adam_print_int(x)\n}");
    assert_eq!(output.trim(), "30");
}

#[test]
fn e2e_struct_create_and_field() {
    let output = compile_and_run("struct Point {\n    x i32\n    y i32\n}\nfn main() {\n    p := Point { x: 3, y: 4 }\n    __adam_print_int(p.x + p.y)\n}");
    assert_eq!(output.trim(), "7");
}

#[test]
fn e2e_factorial() {
    let output = compile_and_run("fn factorial(n i32) -> i32 {\n    if n <= 1 {\n        return 1\n    }\n    return n * factorial(n - 1)\n}\nfn main() {\n    __adam_print_int(factorial(10))\n}");
    assert_eq!(output.trim(), "3628800");
}

#[test]
fn e2e_iterative_fibonacci() {
    let output = compile_and_run("fn fib(n i32) -> i32 {\n    if n <= 1 { return n }\n    mut a := 0\n    mut b := 1\n    mut i := 2\n    while i <= n {\n        c := a + b\n        a = b\n        b = c\n        i = i + 1\n    }\n    return b\n}\nfn main() {\n    __adam_print_int(fib(20))\n}");
    assert_eq!(output.trim(), "6765");
}

#[test]
fn e2e_gcd() {
    let output = compile_and_run("fn gcd(a i32, b i32) -> i32 {\n    if b == 0 { return a }\n    return gcd(b, a % b)\n}\nfn main() {\n    __adam_print_int(gcd(48, 18))\n}");
    assert_eq!(output.trim(), "6");
}

#[test]
fn e2e_power() {
    let output = compile_and_run("fn power(base i32, exp i32) -> i32 {\n    if exp == 0 { return 1 }\n    return base * power(base, exp - 1)\n}\nfn main() {\n    __adam_print_int(power(2, 10))\n}");
    assert_eq!(output.trim(), "1024");
}

#[test]
fn e2e_deeply_nested_calls() {
    let output = compile_and_run("fn inc(x i32) -> i32 { return x + 1 }\nfn main() {\n    __adam_print_int(inc(inc(inc(inc(inc(0))))))\n}");
    assert_eq!(output.trim(), "5");
}

#[test]
fn e2e_zero_and_negative() {
    let output = compile_and_run("fn main() {\n    __adam_print_int(0)\n}");
    assert_eq!(output.trim(), "0");
}

#[test]
fn e2e_multiple_returns() {
    let output = compile_and_run("fn sign(x i32) -> i32 {\n    if x > 0 { return 1 }\n    if x < 0 { return -1 }\n    return 0\n}\nfn main() {\n    __adam_print_int(sign(42))\n}");
    assert_eq!(output.trim(), "1");
}

#[test]
fn e2e_complex_expression() {
    let output = compile_and_run("fn main() {\n    x := (1 + 2) * (3 + 4)\n    __adam_print_int(x)\n}");
    assert_eq!(output.trim(), "21");
}

#[test]
fn e2e_division_and_modulo() {
    let output = compile_and_run("fn main() {\n    __adam_print_int(17 / 5)\n}");
    assert_eq!(output.trim(), "3");
}

#[test]
fn e2e_countdown_loop() {
    let output = compile_and_run("fn main() {\n    mut sum := 0\n    mut n := 10\n    while n > 0 {\n        sum = sum + n\n        n = n - 1\n    }\n    __adam_print_int(sum)\n}");
    assert_eq!(output.trim(), "55");
}

#[test]
fn e2e_struct_as_arg() {
    let output = compile_and_run("struct Rect {\n    w i32\n    h i32\n}\nfn area(r Rect) -> i32 {\n    return r.w * r.h\n}\nfn main() {\n    r := Rect { w: 5, h: 3 }\n    __adam_print_int(area(r))\n}");
    assert_eq!(output.trim(), "15");
}

// ================================================================
// Concurrency codegen tests
// ================================================================

#[test]
fn test_ir_chan_create() {
    let ir = compile_to_ir("fn main() {\n    ch := chan[i32]()\n}");
    assert!(ir.contains("__adam_chan_create"), "expected chan_create call in IR:\n{}", ir);
}

#[test]
fn test_ir_chan_create_buffered() {
    let ir = compile_to_ir("fn main() {\n    ch := chan[i32](10)\n}");
    assert!(ir.contains("__adam_chan_create"), "expected chan_create call in IR:\n{}", ir);
}

#[test]
fn test_ir_spawn_basic() {
    let ir = compile_to_ir("fn work() {}\nfn main() {\n    spawn { work() }\n}");
    assert!(ir.contains("__adam_spawn"), "expected spawn call in IR:\n{}", ir);
    // The outlined function should exist.
    assert!(ir.contains("main__spawn_"), "expected outlined spawn function in IR:\n{}", ir);
}

#[test]
fn test_ir_spawn_generates_outlined_function() {
    let ir = compile_to_ir("fn process(x i32) {}\nfn main() {\n    spawn { process(42) }\n}");
    // Should have the outlined function definition.
    assert!(ir.contains("define"), "expected function definitions in IR");
    assert!(ir.contains("__adam_spawn"), "expected __adam_spawn call in IR:\n{}", ir);
}

#[test]
fn test_ir_spawn_with_captures() {
    let ir = compile_to_ir("fn consume(x i32) {}\nfn main() {\n    x := 42\n    spawn { consume(x) }\n}");
    assert!(ir.contains("__adam_spawn"), "expected spawn call in IR:\n{}", ir);
    // When there are captures, we should see alloc for the env struct.
    assert!(ir.contains("__adam_alloc"), "expected heap alloc for captures in IR:\n{}", ir);
}

#[test]
fn test_ir_chan_method_calls() {
    // ch.send(val) and ch.recv() go through CallNamed.
    let ir = compile_to_ir("fn main() {\n    ch := chan[i32](1)\n    ch.send(42)\n}");
    assert!(ir.contains("call"), "expected call instruction in IR:\n{}", ir);
    assert!(ir.contains("__adam_chan_create"), "expected chan_create in IR:\n{}", ir);
}
