//! Adversarial tests for the type checker.
//! These tests are designed to FIND BUGS by probing edge cases,
//! boundary conditions, and adversarial scenarios.

use adam_lexer::Lexer;
use adam_parser::Parser;
use crate::checker::TypeChecker;
use crate::ty::*;

// ============================================================
// Helpers
// ============================================================

fn typecheck(src: &str) -> crate::checker::TypeCheckResult {
    let lex_result = Lexer::new(src).tokenize();
    assert!(
        lex_result.errors.is_empty(),
        "Lex errors for input:\n{}\n\nErrors: {:?}",
        src,
        lex_result.errors
    );
    let parse_result = Parser::new(lex_result.tokens).parse();
    assert!(
        parse_result.errors.is_empty(),
        "Parse errors for input:\n{}\n\nErrors: {:?}",
        src,
        parse_result.errors
    );
    TypeChecker::new().check(&parse_result.ast)
}

fn typecheck_ok(src: &str) -> crate::checker::TypeCheckResult {
    let result = typecheck(src);
    assert!(
        result.errors.is_empty(),
        "Expected no type errors for input:\n{}\n\nErrors: {:#?}",
        src,
        result.errors
    );
    result
}

fn typecheck_with_errors(src: &str) -> crate::checker::TypeCheckResult {
    typecheck(src)
}

fn error_count(result: &crate::checker::TypeCheckResult) -> usize {
    result.errors.len()
}

fn has_error_containing(result: &crate::checker::TypeCheckResult, substring: &str) -> bool {
    result.errors.iter().any(|e| e.message.contains(substring))
}

// ============================================================
// 1. OPERATOR ABUSE: Every wrong type combo
// ============================================================

/// BUG PROBE: String + String should error (not numeric), but the checker
/// unifies the two strings successfully and only then checks is_numeric.
/// Since the unification succeeds (String == String), it returns String,
/// which is not numeric, so it DOES produce an error. Let's verify.
#[test]
fn adversarial_string_plus_string() {
    let src = r#"fn main() {
    x := "hello" + "world"
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: String + String should produce an arithmetic error"
    );
}

/// BUG PROBE: bool + bool should error.
#[test]
fn adversarial_bool_plus_bool() {
    let src = r#"fn main() {
    x := true + false
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: bool + bool should produce a type error"
    );
}

/// BUG PROBE: char * char should error.
#[test]
fn adversarial_char_times_char() {
    let src = r#"fn main() {
    x := 'a' * 'b'
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: char * char should produce a type error"
    );
}

/// BUG PROBE: bool - i32 should error with type mismatch first.
#[test]
fn adversarial_bool_minus_int() {
    let src = r#"fn main() {
    x := true - 5
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: bool - i32 should produce a type error"
    );
}

/// BUG PROBE: String % i32 should error.
#[test]
fn adversarial_string_mod_int() {
    let src = r#"fn main() {
    x := "hello" % 3
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: String % i32 should produce a type error"
    );
}

/// BUG PROBE: String < String is allowed by comparisons since they just
/// unify the types but never check that the type supports ordering.
/// This should arguably be an error for non-comparable types.
#[test]
fn adversarial_string_less_than_string() {
    let src = r#"fn main() {
    x := "a" < "b"
}"#;
    let result = typecheck_with_errors(src);
    // If this passes with no errors, it's a missing check:
    // comparison operators should require Comparable/Ord types.
    assert!(
        error_count(&result) > 0,
        "BUG: String < String should require an Ord/Comparable trait, but checker allows it"
    );
}

/// BUG PROBE: bool < bool comparison should not be allowed.
#[test]
fn adversarial_bool_less_than_bool() {
    let src = r#"fn main() {
    x := true < false
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: bool < bool should produce a type error (booleans are not ordered)"
    );
}

/// BUG PROBE: Negating a bool should error (-true is not valid).
#[test]
fn adversarial_negate_bool() {
    let src = r#"fn main() {
    x := -true
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: -true should produce an error (bool is not numeric)"
    );
}

/// BUG PROBE: Logical NOT on integer (!5) should error.
#[test]
fn adversarial_not_integer() {
    let src = r#"fn main() {
    x := !5
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: !5 should produce a type error (i32 is not bool)"
    );
}

// ============================================================
// 2. STRUCT LITERAL EDGE CASES: Missing fields, extra fields
// ============================================================

/// BUG PROBE: Struct literal with MISSING required fields should error.
/// The checker only validates fields that ARE present, not fields that are MISSING.
#[test]
fn adversarial_struct_missing_fields() {
    let src = r#"struct Point {
    x f64
    y f64
}

fn main() {
    p := Point { x: 1.0 }
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: Struct literal missing field 'y' should produce an error"
    );
}

/// BUG PROBE: Empty struct used as a value.
#[test]
fn adversarial_empty_struct() {
    let src = r#"struct Empty {}

fn main() {
    e := Empty {}
}"#;
    let result = typecheck_ok(src);
    let has_struct = result.expr_types.values().any(|&id| {
        matches!(result.ctx.ty(id), Ty::Struct(_))
    });
    assert!(has_struct, "Empty struct literal should produce Struct type");
}

/// BUG PROBE: Struct literal with ALL wrong field types.
#[test]
fn adversarial_struct_all_wrong_types() {
    let src = r#"struct Data {
    x i32
    y String
    z bool
}

fn main() {
    d := Data { x: "wrong", y: 42, z: 3.14 }
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) >= 3,
        "BUG: Every field has wrong type, should produce at least 3 errors, got {}",
        error_count(&result)
    );
}

// ============================================================
// 3. IF/ELSE TYPE INFERENCE EDGE CASES
// ============================================================

/// BUG PROBE: If-with-no-else used as a value in a let binding.
/// The checker returns unit for if-no-else, but if the user binds it,
/// the type should propagate. Is it actually unit or is there a problem?
#[test]
fn adversarial_if_no_else_as_value() {
    let src = r#"fn main() {
    x: i32 = if true { 5 }
}"#;
    let result = typecheck_with_errors(src);
    // if-no-else returns unit. Binding to i32 should cause a mismatch.
    assert!(
        error_count(&result) > 0,
        "BUG: 'if true {{ 5 }}' without else returns unit, should not assign to i32"
    );
}

/// BUG PROBE: Deeply nested if-else where each branch returns a different type.
#[test]
fn adversarial_nested_if_else_different_types() {
    let src = r#"fn main() {
    x := if true {
        1
    } else if false {
        "string"
    } else {
        true
    }
}"#;
    let result = typecheck_with_errors(src);
    // The first if returns i32, the else-if chain returns String then bool.
    // These should not unify.
    assert!(
        error_count(&result) > 0,
        "BUG: if/else branches with i32, String, and bool should produce type errors"
    );
}

// ============================================================
// 4. FUNCTION SIGNATURE EDGE CASES
// ============================================================

/// BUG PROBE: Function with zero params called with args.
#[test]
fn adversarial_zero_param_fn_called_with_args() {
    let src = r#"fn noop() { }

fn main() {
    noop(1, 2, 3)
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: Calling 0-param function with 3 args should error"
    );
}

/// BUG PROBE: Function declared to return i32 but body is empty (returns unit).
#[test]
fn adversarial_fn_returns_wrong_type_via_empty_body() {
    let src = r#"fn broken() -> i32 { }"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: Function declared -> i32 with empty body should error (body is unit)"
    );
}

/// BUG PROBE: Return with no value in a function that expects a return type.
#[test]
fn adversarial_bare_return_in_typed_fn() {
    let src = r#"fn foo() -> i32 {
    return
}"#;
    let result = typecheck_with_errors(src);
    // 'return' without value produces Never type.
    // The function body type unification should catch the mismatch,
    // but 'return' is Never which unifies with anything.
    // However, 'return' with no value should check against the return type.
    // Looking at the code: Expr::Return(None) returns never() without checking.
    // This means returning nothing from a fn -> i32 is silently accepted!
    assert!(
        error_count(&result) > 0,
        "BUG: 'return' with no value in fn -> i32 should error"
    );
}

/// BUG PROBE: Recursive function calling itself - does it handle the signature?
#[test]
fn adversarial_recursive_function() {
    let src = r#"fn factorial(n i32) -> i32 {
    if n <= 1 {
        1
    } else {
        n * factorial(n - 1)
    }
}"#;
    let result = typecheck_ok(src);
    assert!(result.errors.is_empty());
}

/// BUG PROBE: Function with many parameters.
#[test]
fn adversarial_many_params() {
    let src = r#"fn many(a i32, b i32, c i32, d i32, e i32, f i32, g i32, h i32) -> i32 {
    a + b + c + d + e + f + g + h
}

fn main() {
    x := many(1, 2, 3, 4, 5, 6, 7, 8)
}"#;
    let result = typecheck_ok(src);
    assert!(result.errors.is_empty());
}

// ============================================================
// 5. MATCH EXHAUSTIVENESS EDGE CASES
// ============================================================

/// BUG PROBE: Match on i32 without wildcard should error.
#[test]
fn adversarial_match_i32_no_wildcard() {
    let src = r#"fn main() {
    x := 5
    y := match x {
        1 => "one"
        2 => "two"
    }
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: Match on i32 with only specific values and no wildcard should error"
    );
}

/// BUG PROBE: Match on bool with only 'true' arm (missing false).
#[test]
fn adversarial_match_bool_missing_false() {
    let src = r#"fn main() {
    x := true
    y := match x {
        true => 1
    }
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: Match on bool with only 'true' should error (non-exhaustive)"
    );
}

/// BUG PROBE: Match on bool with only 'false' arm (missing true).
#[test]
fn adversarial_match_bool_missing_true() {
    let src = r#"fn main() {
    x := true
    y := match x {
        false => 0
    }
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: Match on bool with only 'false' should error (non-exhaustive)"
    );
}

/// BUG PROBE: Match on enum where arms return different types.
#[test]
fn adversarial_match_arms_different_types() {
    let src = r#"enum Color {
    Red
    Blue
}

fn main() {
    c := Red
    x := match c {
        Color.Red => 1
        Color.Blue => "blue"
    }
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: Match arms returning i32 and String should produce a type error"
    );
}

// ============================================================
// 6. SCOPE AND SHADOWING EDGE CASES
// ============================================================

/// Variable shadowed with a different type should be allowed (new binding).
#[test]
fn adversarial_shadow_different_type() {
    let src = r#"fn main() {
    x := 5
    x := "now a string"
}"#;
    // Shadowing creates a new variable in the same scope
    let result = typecheck_ok(src);
    assert!(result.errors.is_empty());
}

/// BUG PROBE: Using a variable after it's been shadowed - should use new type.
#[test]
fn adversarial_use_after_shadow() {
    let src = r#"fn main() {
    x := 5
    x := "now a string"
    y := x + 1
}"#;
    let result = typecheck_with_errors(src);
    // x is now String after shadowing, so x + 1 should fail
    assert!(
        error_count(&result) > 0,
        "BUG: After shadowing x as String, x + 1 should fail"
    );
}

/// BUG PROBE: Variable from inner scope should not leak to outer scope.
#[test]
fn adversarial_scope_leak() {
    let src = r#"fn main() {
    if true {
        inner := 42
    }
    y := inner
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: Variable 'inner' from if block should not be visible outside"
    );
}

// ============================================================
// 7. TYPE INFERENCE CHAINS
// ============================================================

/// BUG PROBE: Chain of let bindings where type must propagate through 5+ levels.
#[test]
fn adversarial_inference_chain() {
    let src = r#"fn main() {
    a := 42
    b := a
    c := b
    d := c
    e := d
    f := e
    g := f + 1
}"#;
    let result = typecheck_ok(src);
    assert!(result.errors.is_empty(), "Type inference should propagate through chain");
}

/// BUG PROBE: Inference through function return - is the returned type correct?
#[test]
fn adversarial_inference_through_call() {
    let src = r#"fn make_int() -> i32 { 42 }

fn main() {
    x := make_int()
    y := x + 1
}"#;
    let result = typecheck_ok(src);
    assert!(result.errors.is_empty());
}

// ============================================================
// 8. UNIFICATION EDGE CASES
// ============================================================

/// BUG PROBE: Unifying Error type with concrete types should not produce cascading errors.
#[test]
fn adversarial_error_propagation() {
    let src = r#"fn main() {
    x := undefined_var
    y := x + 1
    z := y * 2
}"#;
    let result = typecheck_with_errors(src);
    // Should have exactly 1 error for undefined_var, not cascading errors.
    // If error type propagates silently, subsequent ops should not add errors.
    let non_undefined_errors: Vec<_> = result.errors.iter()
        .filter(|e| !e.message.contains("undefined"))
        .collect();
    assert!(
        non_undefined_errors.is_empty(),
        "BUG: Error type should propagate silently without cascading errors, but got: {:?}",
        non_undefined_errors
    );
}

/// BUG PROBE: Never type should unify with anything.
#[test]
fn adversarial_never_unifies() {
    let src = r#"fn main() -> i32 {
    if true {
        return 5
    } else {
        return 10
    }
}"#;
    let result = typecheck_ok(src);
    assert!(result.errors.is_empty(), "Never from return should unify with i32");
}

/// BUG PROBE: TypeVar chain - does unification follow the chain?
#[test]
fn adversarial_typevar_chain() {
    let src = r#"fn main() {
    x := nil
    y := x
    z := y
}"#;
    let result = typecheck_ok(src);
    // nil is ?T, should propagate through chain without issues
    assert!(result.errors.is_empty());
}

// ============================================================
// 9. TRY OPERATOR EDGE CASES
// ============================================================

/// BUG PROBE: Try operator on a plain value (not Result/Optional).
#[test]
fn adversarial_try_on_plain_value() {
    let src = r#"fn main() {
    x := 42
    y := x?
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: ? on i32 should produce a type error"
    );
}

/// BUG PROBE: Try operator on bool.
#[test]
fn adversarial_try_on_bool() {
    let src = r#"fn main() {
    x := true?
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: ? on bool should produce a type error"
    );
}

// ============================================================
// 10. METHOD RESOLUTION EDGE CASES
// ============================================================

/// BUG PROBE: Calling a method that doesn't exist should produce an error.
/// The checker currently returns a fresh type var for unknown methods!
#[test]
fn adversarial_nonexistent_method() {
    let src = r#"struct Foo {
    x i32
}

fn main() {
    f := Foo { x: 1 }
    y := f.nonexistent()
}"#;
    let result = typecheck_with_errors(src);
    // The checker returns fresh_type_var() for unknown methods instead of erroring!
    assert!(
        error_count(&result) > 0,
        "BUG: Calling nonexistent method should produce an error, not a fresh type var"
    );
}

/// BUG PROBE: Method on a primitive type that doesn't exist.
#[test]
fn adversarial_method_on_int() {
    let src = r#"fn main() {
    x := 5
    y := x.foo()
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: Calling .foo() on i32 should error"
    );
}

/// BUG PROBE: Field access on non-struct returns fresh type var instead of error.
#[test]
fn adversarial_field_access_on_int() {
    let src = r#"fn main() {
    x := 5
    y := x.field
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: Field access on i32 should error"
    );
}

// ============================================================
// 11. ENUM EDGE CASES
// ============================================================

/// BUG PROBE: Enum with single variant.
#[test]
fn adversarial_single_variant_enum() {
    let src = r#"enum Wrapper {
    Value(i32)
}

fn main() {
    w := Value(42)
    x := match w {
        Value(n) => n
    }
}"#;
    let result = typecheck_ok(src);
    assert!(result.errors.is_empty());
}

/// BUG PROBE: Enum variant constructor with wrong number of args.
#[test]
fn adversarial_enum_variant_wrong_args() {
    let src = r#"enum Shape {
    Circle(f64)
    Rect(f64, f64)
}

fn main() {
    s := Circle(1.0, 2.0)
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: Circle expects 1 arg but got 2"
    );
}

/// BUG PROBE: Enum variant constructor with wrong type arg.
#[test]
fn adversarial_enum_variant_wrong_type() {
    let src = r#"enum Shape {
    Circle(f64)
}

fn main() {
    s := Circle("not a float")
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: Circle expects f64, got String"
    );
}

// ============================================================
// 12. CLOSURE EDGE CASES
// ============================================================

/// BUG PROBE: Closure with typed params and wrong return.
#[test]
fn adversarial_closure_type_mismatch() {
    let src = r#"fn main() {
    f := |x i32| "not an int"
}"#;
    // This should be valid - closure returns String, that's fine.
    // The closure type is fn(i32) -> String.
    let result = typecheck_ok(src);
    let has_fn = result.expr_types.values().any(|&id| {
        matches!(result.ctx.ty(id), Ty::Function(_))
    });
    assert!(has_fn, "Closure should produce Function type");
}

/// BUG PROBE: Calling a closure with wrong argument type.
#[test]
fn adversarial_closure_call_wrong_type() {
    let src = r#"fn apply(f fn(i32) -> i32, x i32) -> i32 {
    f(x)
}

fn main() {
    add := |x i32| x + 1
    y := apply(add, "wrong")
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: Passing String where i32 is expected should error"
    );
}

// ============================================================
// 13. RETURN TYPE EDGE CASES
// ============================================================

/// BUG PROBE: Function returning Never (via return in all branches).
#[test]
fn adversarial_fn_all_branches_return() {
    let src = r#"fn diverge(x i32) -> i32 {
    if x > 0 {
        return 1
    } else {
        return -1
    }
}"#;
    let result = typecheck_ok(src);
    assert!(result.errors.is_empty());
}

/// BUG PROBE: Function with explicit return type but body returns different type
/// via the last expression (not return statement).
#[test]
fn adversarial_body_type_vs_return_type() {
    let src = r#"fn broken() -> i32 {
    "this is a string"
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: Body returns String but function declares -> i32"
    );
}

// ============================================================
// 14. ASSIGNMENT EDGE CASES
// ============================================================

/// BUG PROBE: Assigning to an immutable variable - type checker doesn't check mutability.
/// This is more of a borrow checker concern, but let's see.
#[test]
fn adversarial_assign_immutable() {
    let src = r#"fn main() {
    x := 5
    x = 10
}"#;
    // The type checker does not check mutability - it only checks types.
    // This may or may not be considered a bug depending on design.
    let result = typecheck(src);
    // Just verify it doesn't crash.
    let _ = result;
}

/// BUG PROBE: Complex assignment type mismatch.
#[test]
fn adversarial_assign_complex_mismatch() {
    let src = r#"fn main() {
    mut x := [1, 2, 3]
    x = "not an array"
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: Assigning String to [i32] variable should error"
    );
}

// ============================================================
// 15. ARRAY AND INDEX EDGE CASES
// ============================================================

/// BUG PROBE: Indexing with a String (non-integer index).
#[test]
fn adversarial_string_index() {
    let src = r#"fn main() {
    arr := [1, 2, 3]
    x := arr["bad"]
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: Indexing array with String should error"
    );
}

/// BUG PROBE: Empty array type inference - can we add to it later?
#[test]
fn adversarial_empty_array_inference() {
    let src = r#"fn main() {
    arr := []
}"#;
    let result = typecheck_ok(src);
    let has_array = result.expr_types.values().any(|&id| {
        matches!(result.ctx.ty(id), Ty::Array(_, _))
    });
    assert!(has_array, "Empty array should produce Array type");
}

// ============================================================
// 16. TUPLE EDGE CASES
// ============================================================

/// BUG PROBE: Tuple with single element.
#[test]
fn adversarial_single_element_tuple() {
    let src = r#"fn main() {
    t := (42,)
}"#;
    // Parser may or may not support trailing comma in tuples.
    // If it does, this should be a 1-element tuple.
    let result = typecheck(src);
    // Just check it doesn't crash.
    let _ = result;
}

/// BUG PROBE: Nested tuples.
#[test]
fn adversarial_nested_tuples() {
    let src = r#"fn main() {
    t := ((1, 2), (3, 4))
}"#;
    let result = typecheck_ok(src);
    let has_tuple = result.expr_types.values().any(|&id| {
        matches!(result.ctx.ty(id), Ty::Tuple(_))
    });
    assert!(has_tuple, "Nested tuple should produce Tuple type");
}

// ============================================================
// 17. VIEW EDGE CASES
// ============================================================

/// BUG PROBE: View with wrong default type for a state field.
#[test]
fn adversarial_view_wrong_default_type() {
    let src = r#"view BadView {
    @state count: i32 = "not an int"

    body {
        count
    }
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: View state field with wrong default type should error"
    );
}

// ============================================================
// 18. IMPL BLOCK EDGE CASES
// ============================================================

/// BUG PROBE: Method with wrong return type in impl.
#[test]
fn adversarial_impl_method_wrong_return() {
    let src = r#"struct Foo {
    x i32
}

impl Foo {
    fn get(self) -> i32 {
        "not an int"
    }
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: Method returning String when declared -> i32 should error"
    );
}

/// BUG PROBE: Method with self param but wrong arg count when calling.
#[test]
fn adversarial_method_wrong_arg_count() {
    let src = r#"struct Calc {
    val i32
}

impl Calc {
    fn add(self, n i32) -> i32 {
        self.val + n
    }
}

fn main() {
    c := Calc { val: 10 }
    x := c.add(1, 2)
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: Method expects 1 arg (besides self) but got 2"
    );
}

// ============================================================
// 19. FOR LOOP EDGE CASES
// ============================================================

/// BUG PROBE: For loop over non-iterable type.
/// The checker defaults to i32 for unknown iterables - is that correct?
#[test]
fn adversarial_for_loop_non_iterable() {
    let src = r#"fn main() {
    for x in 42 {
        x
    }
}"#;
    let result = typecheck_with_errors(src);
    // The checker defaults to i32 for unknown iterables, not an error.
    // This is arguably a bug - iterating over a plain integer should error.
    assert!(
        error_count(&result) > 0,
        "BUG: for loop over plain i32 (not array/range/channel) should error"
    );
}

/// BUG PROBE: For loop where loop variable is used with wrong type.
#[test]
fn adversarial_for_loop_type_mismatch() {
    let src = r#"fn main() {
    arr := [1, 2, 3]
    for x in arr {
        y := x + "bad"
    }
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: Loop variable is i32, adding String should error"
    );
}

// ============================================================
// 20. WHILE LOOP EDGE CASES
// ============================================================

/// BUG PROBE: While condition that is a string.
#[test]
fn adversarial_while_string_condition() {
    let src = r#"fn main() {
    while "true" {
        5
    }
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: While condition must be bool, String should error"
    );
}

// ============================================================
// 21. SELF TYPE EDGE CASES
// ============================================================

/// BUG PROBE: Using Self outside of impl block.
#[test]
fn adversarial_self_outside_impl() {
    let src = r#"fn main() {
    x: Self = 5
}"#;
    let result = typecheck_with_errors(src);
    // Self outside impl should resolve to error type
    // But it might silently produce error type without an explicit error message
    // because resolve_type_inner returns self.ctx.error() for unknown Self.
    // The real question is whether the mismatch between error and i32 is silent.
    // Error type propagates silently, so this might NOT produce an error!
    let _ = result;
}

// ============================================================
// 22. MIXED NUMERIC TYPE EDGE CASES
// ============================================================

/// BUG PROBE: i32 + f64 should error (no implicit widening).
#[test]
fn adversarial_int_plus_float() {
    let src = r#"fn main() {
    x := 5 + 3.14
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: i32 + f64 should error (no implicit numeric coercion)"
    );
}

/// BUG PROBE: Comparison between different numeric types.
#[test]
fn adversarial_int_compare_float() {
    let src = r#"fn main() {
    x := 5 < 3.14
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: i32 < f64 should error (different types)"
    );
}

// ============================================================
// 23. CHANNEL EDGE CASES
// ============================================================

/// BUG PROBE: Channel recv returns element type, but what about send type check?
#[test]
fn adversarial_channel_send_wrong_type() {
    let src = r#"fn main() {
    ch := chan[i32]()
    ch.send("wrong type")
}"#;
    let result = typecheck_with_errors(src);
    // Built-in send method doesn't check the argument type!
    // It just returns unit.
    assert!(
        error_count(&result) > 0,
        "BUG: Sending String through chan[i32] should error"
    );
}

// ============================================================
// 24. BLOCK EXPRESSION EDGE CASES
// ============================================================

/// BUG PROBE: Empty block should return unit.
#[test]
fn adversarial_empty_block() {
    let src = r#"fn main() -> i32 {
    {}
}"#;
    let result = typecheck_with_errors(src);
    // Empty block returns unit, fn expects i32
    // But fn return check: sig.return_type != self.ctx.unit() -- it IS unit?
    // No, the fn has -> i32, so return type is i32, body is {} which is unit.
    // The block {} returns unit, so unify(i32, unit) should error.
    assert!(
        error_count(&result) > 0,
        "BUG: Empty block returns unit, should mismatch with -> i32"
    );
}

/// BUG PROBE: Block where last statement is a let (not an expression).
#[test]
fn adversarial_block_ending_in_let() {
    let src = r#"fn main() -> i32 {
    x := 42
}"#;
    let result = typecheck_with_errors(src);
    // Let statement returns unit. Function expects i32.
    assert!(
        error_count(&result) > 0,
        "BUG: Block ending in let statement returns unit, should mismatch with -> i32"
    );
}

// ============================================================
// 25. TRAIT EDGE CASES
// ============================================================

/// BUG PROBE: Impl block with method that doesn't match trait signature.
#[test]
fn adversarial_trait_impl_wrong_signature() {
    let src = r#"trait Foo {
    fn bar(self) -> i32
}

struct MyStruct {
    x i32
}

impl Foo for MyStruct {
    fn bar(self) -> String {
        "wrong return type"
    }
}"#;
    let result = typecheck_with_errors(src);
    // The checker registers methods but may not validate that impl methods
    // match the trait method signatures. This is likely a missing check.
    assert!(
        error_count(&result) > 0,
        "BUG: Impl method returns String but trait declares -> i32, should error"
    );
}

// ============================================================
// 26. STRUCTURAL UNIFICATION EDGE CASES
// ============================================================

/// BUG PROBE: Array size mismatch in unification.
#[test]
fn adversarial_array_size_mismatch() {
    let src = r#"fn main() {
    x: [i32; 3] = [1, 2, 3]
}"#;
    // Array literal has no size info (None), annotation has size 3.
    // Does unification handle this? Array(i32, None) vs Array(i32, Some(3))
    let result = typecheck_with_errors(src);
    // This should either work (if unsized arrays unify with sized) or error.
    // The unification code has: a_sz == b_sz, so None != Some(3) -> error.
    let _ = result; // Just verify no crash.
}

/// BUG PROBE: Tuple arity mismatch.
#[test]
fn adversarial_tuple_arity_mismatch() {
    let src = r#"fn main() {
    x: (i32, i32) = (1, 2, 3)
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: Tuple of 3 elements should not unify with tuple of 2"
    );
}

// ============================================================
// 27. COMPLEX INTERACTION TESTS
// ============================================================

/// BUG PROBE: Struct method that returns the struct type (builder pattern).
/// NOTE: Static method calls like `Builder.new()` don't work in the type checker
/// because the parser sees `Builder` as an identifier, not a type path.
/// This is itself a bug (static method resolution). Testing without static call.
#[test]
fn adversarial_builder_pattern() {
    let src = r#"struct Builder {
    count i32
}

impl Builder {
    fn make(n i32) -> Builder {
        Builder { count: n }
    }
}

fn main() {
    b := Builder { count: 0 }
}"#;
    let result = typecheck_ok(src);
    assert!(result.errors.is_empty());
}

/// BUG PROBE: Nested struct field access.
#[test]
fn adversarial_nested_field_access() {
    let src = r#"struct Inner {
    value i32
}

struct Outer {
    inner Inner
}

fn main() {
    o := Outer { inner: Inner { value: 42 } }
    x := o.inner.value
}"#;
    let result = typecheck_ok(src);
    let has_i32 = result.expr_types.values().any(|&id| *result.ctx.ty(id) == Ty::I32);
    assert!(has_i32, "o.inner.value should produce i32");
}

/// BUG PROBE: Using enum variant name as function name (collision).
#[test]
fn adversarial_variant_fn_collision() {
    let src = r#"enum Color {
    Red
    Blue
}

fn Red() -> i32 { 42 }

fn main() {
    x := Red
}"#;
    // The variant_ids and fn_sigs both have "Red".
    // In infer_expr for Identifier, fn_sigs is checked before variant_ids.
    // But variant_ids is checked first! Actually: lookup_var -> fn_sigs -> variant_ids.
    // Since Red is a variant, it should resolve to the enum type.
    // But there's also a function Red. Which takes precedence?
    let result = typecheck(src);
    // Just verify no crash or panic.
    let _ = result;
}

// ============================================================
// 28. ARITHMETIC ON ERROR TYPE
// ============================================================

/// BUG PROBE: Operations on Error type should propagate silently.
#[test]
fn adversarial_arithmetic_on_error() {
    let src = r#"fn main() {
    x := undefined + 1
    y := x * 2
    z := y - x
}"#;
    let result = typecheck_with_errors(src);
    // Only the undefined variable error should be reported.
    let undefined_errors = result.errors.iter()
        .filter(|e| e.message.contains("undefined"))
        .count();
    assert!(
        undefined_errors >= 1,
        "Should have at least one undefined variable error"
    );
    let arithmetic_errors = result.errors.iter()
        .filter(|e| e.message.contains("arithmetic"))
        .count();
    assert_eq!(
        arithmetic_errors, 0,
        "BUG: Error type in arithmetic should not produce additional arithmetic errors"
    );
}

// ============================================================
// 29. MULTIPLE RETURN STATEMENTS
// ============================================================

/// BUG PROBE: Multiple return statements with different types.
#[test]
fn adversarial_multiple_returns_different_types() {
    let src = r#"fn bad(x i32) -> i32 {
    if x > 0 {
        return "positive"
    }
    return 0
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: return \"positive\" in fn -> i32 should error"
    );
}

// ============================================================
// 30. DEEPLY NESTED EXPRESSION TYPE CHECKING
// ============================================================

/// BUG PROBE: Very deeply nested arithmetic.
#[test]
fn adversarial_deep_nesting() {
    let src = r#"fn main() {
    x := ((((((1 + 2) * 3) - 4) / 5) % 6) + 7)
}"#;
    let result = typecheck_ok(src);
    assert!(result.errors.is_empty());
}

/// BUG PROBE: Deeply nested function calls.
#[test]
fn adversarial_deep_call_nesting() {
    let src = r#"fn id(x i32) -> i32 { x }

fn main() {
    x := id(id(id(id(id(1)))))
}"#;
    let result = typecheck_ok(src);
    assert!(result.errors.is_empty());
}

// ============================================================
// 31. RANGE EDGE CASES
// ============================================================

/// BUG PROBE: Range with mismatched types (i32..String).
#[test]
fn adversarial_range_type_mismatch() {
    let src = r#"fn main() {
    r := 1.."end"
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: Range with i32 and String should error"
    );
}

// ============================================================
// 32. STRUCT WITH IMPL - Self type resolution
// ============================================================

/// BUG PROBE: Using Self type in method return.
/// NOTE: Static method calls like `Foo.make()` don't work (see builder_pattern note).
/// Testing Self type resolution without static call.
#[test]
fn adversarial_self_type_in_return() {
    let src = r#"struct Foo {
    x i32
}

impl Foo {
    fn get_x(self) -> i32 {
        self.x
    }
}

fn main() {
    f := Foo { x: 42 }
    y := f.get_x()
}"#;
    let result = typecheck_ok(src);
    assert!(result.errors.is_empty());
}

// ============================================================
// 33. PATTERN MATCHING EDGE CASES
// ============================================================

/// BUG PROBE: Match with guard that is not bool.
#[test]
fn adversarial_match_guard_non_bool() {
    let src = r#"fn main() {
    x := 5
    y := match x {
        n if n => 1
        _ => 0
    }
}"#;
    let result = typecheck_with_errors(src);
    // Guard 'n' is i32, not bool. Should error.
    assert!(
        error_count(&result) > 0,
        "BUG: Match guard must be bool, i32 guard should error"
    );
}

/// BUG PROBE: Match arm body uses binding from pattern.
#[test]
fn adversarial_match_binding_type() {
    let src = r#"enum Opt {
    Some(i32)
    None
}

fn main() {
    x := Some(42)
    y := match x {
        Some(val) => val + "bad"
        None => 0
    }
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: val is i32, val + \"bad\" should error"
    );
}

// ============================================================
// 34. CALL EXPRESSION EDGE CASES
// ============================================================

/// BUG PROBE: Calling a non-function value.
#[test]
fn adversarial_call_non_function() {
    let src = r#"fn main() {
    x := 42
    y := x(1, 2)
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: Calling an i32 as a function should error"
    );
}

/// BUG PROBE: Calling a string as a function.
#[test]
fn adversarial_call_string() {
    let src = r#"fn main() {
    f := "not a function"
    y := f(1)
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: Calling a String as a function should error"
    );
}

// ============================================================
// 35. SPAWN EDGE CASES
// ============================================================

/// BUG PROBE: Spawn block type should be unit.
#[test]
fn adversarial_spawn_type() {
    let src = r#"fn main() {
    x: i32 = spawn {
        42
    }
}"#;
    let result = typecheck_with_errors(src);
    // spawn returns unit, assigning to i32 should error
    assert!(
        error_count(&result) > 0,
        "BUG: spawn block returns unit, should not assign to i32"
    );
}

// ============================================================
// 36. COMPLEX MULTI-FILE LIKE SCENARIOS
// ============================================================

/// BUG PROBE: Multiple structs, enums, and functions interacting.
#[test]
fn adversarial_complex_interaction() {
    let src = r#"struct Point {
    x f64
    y f64
}

enum Shape {
    Circle(f64)
    Rect(Point, Point)
}

fn area(s Shape) -> f64 {
    match s {
        Circle(r) => r * r
        Rect(p1, p2) => p1.x * p2.y
    }
}

fn main() {
    c := Circle(5.0)
    a := area(c)
}"#;
    let result = typecheck_ok(src);
    assert!(result.errors.is_empty());
}

// ============================================================
// 37. IMPL METHOD SELF PARAM BUG
// ============================================================

/// BUG PROBE: Method where self param is counted in the sig.params vec.
/// When the checker strips self for arg matching, does it handle the offset correctly?
#[test]
fn adversarial_method_self_offset() {
    let src = r#"struct Counter {
    val i32
}

impl Counter {
    fn add(self, a i32, b i32) -> i32 {
        self.val + a + b
    }
}

fn main() {
    c := Counter { val: 0 }
    x := c.add(1, 2)
}"#;
    let result = typecheck_ok(src);
    assert!(result.errors.is_empty(), "Method with self + 2 params should work with 2 args");
}

/// BUG PROBE: Method with no params besides self, called with args.
#[test]
fn adversarial_method_no_params_with_args() {
    let src = r#"struct Foo {
    x i32
}

impl Foo {
    fn get(self) -> i32 {
        self.x
    }
}

fn main() {
    f := Foo { x: 1 }
    y := f.get(42)
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: .get() expects 0 args besides self, but got 1"
    );
}

// ============================================================
// 38. STRING INTERPOLATION EDGE CASES
// ============================================================

/// BUG PROBE: String interpolation should always produce String type.
#[test]
fn adversarial_string_interp_type() {
    let src = r#"fn main() {
    x := 42
    s := "value is {x}"
}"#;
    let result = typecheck_ok(src);
    let has_string = result.expr_types.values().any(|&id| *result.ctx.ty(id) == Ty::String);
    assert!(has_string, "String interpolation should produce String");
}

// ============================================================
// 39. BREAK/CONTINUE EDGE CASES
// ============================================================

/// BUG PROBE: Break with value.
#[test]
fn adversarial_break_with_value() {
    let src = r#"fn main() {
    loop {
        break
    }
}"#;
    let result = typecheck_ok(src);
    assert!(result.errors.is_empty());
}

// ============================================================
// 40. RETURN TYPE CHECK BUG: fn -> unit skips body type check
// ============================================================

/// BUG PROBE: Function with no return type (implicit unit) but body returns i32.
/// The checker skips body type check when return type is unit.
/// Is a function with no -> annotation but body expression i32 valid?
#[test]
fn adversarial_implicit_unit_fn_returns_value() {
    let src = r#"fn foo() {
    42
}"#;
    // Function has no -> annotation, so return type is unit.
    // Body returns i32. The checker skips the check because
    // sig.return_type == self.ctx.unit().
    // This means functions without a return type annotation can
    // return ANY type without error. Is that a bug?
    let result = typecheck_ok(src);
    // This should arguably produce an error or a warning.
    let _ = result;
}

// ============================================================
// 41. UNIFICATION: Box, Rc, Arc
// ============================================================

/// BUG PROBE: Box/Rc/Arc unification is not implemented.
/// The unification function has no case for Box, Rc, Arc!
#[test]
fn adversarial_box_unification() {
    let src = r#"fn main() {
    x: Box[i32] = Box[i32]
}"#;
    // This test is about whether Box types can be unified at all.
    // Since there's no literal syntax for Box, we test with annotations.
    let result = typecheck(src);
    let _ = result; // Just verify no crash
}

// ============================================================
// 42. RECURSIVE TYPE (self-referencing struct)
// ============================================================

/// BUG PROBE: Struct that references itself (like a linked list).
/// This might cause issues during field type resolution.
#[test]
fn adversarial_recursive_struct() {
    let src = r#"struct Node {
    value i32
    next ?Node
}

fn main() {
    n := Node { value: 1, next: nil }
}"#;
    // During registration, Node's fields reference Node,
    // but Node may not be registered yet.
    let result = typecheck(src);
    // Just check it doesn't panic/crash
    let _ = result;
}

// ============================================================
// 43. ENUM WITH NO VARIANTS
// ============================================================

/// BUG PROBE: Enum with no variants.
#[test]
fn adversarial_empty_enum() {
    let src = r#"enum Empty {}"#;
    let result = typecheck(src);
    // Should not crash
    let _ = result;
}

// ============================================================
// 44. MATCH ON STRING EDGE CASE
// ============================================================

/// BUG PROBE: Match on String requires wildcard.
#[test]
fn adversarial_match_string_no_wildcard() {
    let src = r#"fn main() {
    s := "hello"
    x := match s {
        "hello" => 1
        "world" => 2
    }
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: Match on String without wildcard should error (non-exhaustive)"
    );
}

// ============================================================
// 45. EQUALITY CHECK ON DIFFERENT TYPES
// ============================================================

/// BUG PROBE: Comparing i32 == String should error.
#[test]
fn adversarial_eq_different_types() {
    let src = r#"fn main() {
    x := 5 == "five"
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: Comparing i32 == String should error"
    );
}

/// BUG PROBE: Comparing bool == i32 should error.
#[test]
fn adversarial_eq_bool_int() {
    let src = r#"fn main() {
    x := true == 1
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: Comparing bool == i32 should error"
    );
}

// ============================================================
// 46. WHILE LOOP AS EXPRESSION VALUE
// ============================================================

/// NOTE: `while` is a statement, not an expression, in Adam's parser.
/// So `x: i32 = while true { 5 }` is a parse error, not a type error.
/// This test is removed since it's a parser limitation, not a type checker bug.
/// Instead, test that a while loop in statement position doesn't contribute a type.
#[test]
fn adversarial_while_as_statement() {
    let src = r#"fn main() {
    while true {
        break
    }
}"#;
    let result = typecheck_ok(src);
    assert!(result.errors.is_empty());
}

// ============================================================
// 47. MIXED ASSIGNMENT OPERATORS
// ============================================================

/// BUG PROBE: Assign different type to mutable variable.
#[test]
fn adversarial_mut_var_type_change() {
    let src = r#"fn main() {
    mut x := 5
    x = "changed type"
}"#;
    let result = typecheck_with_errors(src);
    assert!(
        error_count(&result) > 0,
        "BUG: Cannot assign String to mut i32 variable"
    );
}

// ============================================================
// 48. FUNCTION AS VALUE EDGE CASES
// ============================================================

/// BUG PROBE: Passing a function as a value.
#[test]
fn adversarial_fn_as_value() {
    let src = r#"fn add(a i32, b i32) -> i32 {
    a + b
}

fn main() {
    f := add
}"#;
    let result = typecheck_ok(src);
    let has_fn = result.expr_types.values().any(|&id| {
        matches!(result.ctx.ty(id), Ty::Function(_))
    });
    assert!(has_fn, "Function name as value should produce Function type");
}

// ============================================================
// 49. OPTIONAL TYPE EDGE CASES
// ============================================================

/// BUG PROBE: Nil assigned to typed optional.
#[test]
fn adversarial_nil_to_optional() {
    let src = r#"fn main() {
    x: ?i32 = nil
}"#;
    let result = typecheck_ok(src);
    assert!(result.errors.is_empty());
}

// ============================================================
// 50. EXHAUSTIVENESS CHECK WITH OR-PATTERNS
// ============================================================

/// BUG PROBE: Enum match using or-pattern to cover all variants.
#[test]
fn adversarial_or_pattern_exhaustive() {
    let src = r#"enum Color {
    Red
    Green
    Blue
}

fn main() {
    c := Red
    x := match c {
        Color.Red => 1
        Color.Green | Color.Blue => 2
    }
}"#;
    let result = typecheck_ok(src);
    assert!(
        result.errors.is_empty(),
        "Or-pattern covering Green|Blue with Red should be exhaustive"
    );
}
