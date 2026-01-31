//! Borrow checker tests.
//!
//! Each test lexes + parses Adam source, optionally type-checks it,
//! then runs the borrow checker and asserts on the result.

use adam_lexer::Lexer;
use adam_parser::Parser;
use adam_resolve::resolve;
use adam_types::TypeChecker;

use crate::checker::{BorrowCheckResult, BorrowChecker};

// ---- Helpers ---------------------------------------------------------------

/// Parse, resolve, type-check, then borrow-check the source.
/// Panics on lex/parse errors. Returns the borrow-check result.
fn borrow_check(src: &str) -> BorrowCheckResult {
    let lex_result = Lexer::new(src).tokenize();
    assert!(
        lex_result.errors.is_empty(),
        "lex errors: {:?}",
        lex_result.errors
    );

    let parse_result = Parser::new(lex_result.tokens).parse();
    assert!(
        parse_result.errors.is_empty(),
        "parse errors: {:?}",
        parse_result.errors
    );

    let resolve_result = resolve(&parse_result.ast);
    let type_result = TypeChecker::new().check(&parse_result.ast);

    BorrowChecker::new().check(
        &parse_result.ast,
        Some(&resolve_result),
        Some(&type_result),
    )
}

/// Assert that borrow checking produces no errors.
fn assert_no_errors(src: &str) {
    let result = borrow_check(src);
    if !result.errors.is_empty() {
        let msgs: Vec<_> = result.errors.iter().map(|e| e.to_string()).collect();
        panic!("expected no borrow errors, got:\n{}", msgs.join("\n"));
    }
}

/// Assert that borrow checking produces at least one error.
fn assert_has_errors(src: &str) -> BorrowCheckResult {
    let result = borrow_check(src);
    assert!(
        !result.errors.is_empty(),
        "expected borrow errors, but got none"
    );
    result
}

/// Assert that borrow checking produces exactly `n` errors.
fn assert_error_count(src: &str, n: usize) -> BorrowCheckResult {
    let result = borrow_check(src);
    assert_eq!(
        result.errors.len(),
        n,
        "expected {} error(s), got {}: {:?}",
        n,
        result.errors.len(),
        result
            .errors
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
    );
    result
}

/// Assert that at least one error message contains the given substring.
fn assert_error_contains(result: &BorrowCheckResult, substr: &str) {
    let has = result
        .errors
        .iter()
        .any(|e| e.message.contains(substr));
    assert!(
        has,
        "no error message contains '{}'; errors: {:?}",
        substr,
        result
            .errors
            .iter()
            .map(|e| e.message.clone())
            .collect::<Vec<_>>()
    );
}

// ===========================================================================
// S1: Basic usage — no errors expected
// ===========================================================================

#[test]
fn test_simple_use() {
    assert_no_errors(
        "fn main() {
            x := 5
        }",
    );
}

#[test]
fn test_simple_use_and_read() {
    assert_no_errors(
        "fn main() {
            x := 42
            y := x + 1
        }",
    );
}

#[test]
fn test_multiple_let_bindings() {
    assert_no_errors(
        "fn main() {
            a := 1
            b := 2
            c := a + b
        }",
    );
}

#[test]
fn test_function_with_params() {
    assert_no_errors(
        "fn add(a i32, b i32) -> i32 {
            return a + b
        }",
    );
}

#[test]
fn test_empty_function() {
    assert_no_errors("fn nothing() {}");
}

#[test]
fn test_bool_literal() {
    assert_no_errors(
        "fn main() {
            x := true
            y := false
        }",
    );
}

// ===========================================================================
// S2: Copy types — no move errors
// ===========================================================================

#[test]
fn test_copy_type_i32_no_move() {
    // i32 is Copy, so passing it to an `own` param should not move it.
    assert_no_errors(
        "fn consume(own x i32) {}
        fn main() {
            a := 42
            consume(a)
            b := a + 1
        }",
    );
}

#[test]
fn test_copy_type_bool_no_move() {
    assert_no_errors(
        "fn take(own b bool) {}
        fn main() {
            flag := true
            take(flag)
            x := flag
        }",
    );
}

#[test]
fn test_copy_type_f64_no_move() {
    assert_no_errors(
        "fn take(own x f64) {}
        fn main() {
            pi := 3.14
            take(pi)
            y := pi
        }",
    );
}

#[test]
fn test_copy_type_char_no_move() {
    assert_no_errors(
        "fn take(own c char) {}
        fn main() {
            letter := 'A'
            take(letter)
            x := letter
        }",
    );
}

#[test]
fn test_copy_type_reassign() {
    assert_no_errors(
        "fn main() {
            x := 10
            y := x
            z := x
        }",
    );
}

// ===========================================================================
// S3: Use-after-move errors
// ===========================================================================

#[test]
fn test_use_after_move() {
    let result = assert_has_errors(
        "fn consume(own s String) {}
        fn main() {
            name: String = \"hello\"
            consume(name)
            x := name
        }",
    );
    assert_error_contains(&result, "moved");
}

#[test]
fn test_use_after_move_in_let() {
    let result = assert_has_errors(
        "fn main() {
            a: String = \"hello\"
            b := a
            c := a
        }",
    );
    assert_error_contains(&result, "moved");
}

#[test]
fn test_double_move() {
    let result = assert_has_errors(
        "fn consume(own s String) {}
        fn main() {
            s: String = \"data\"
            consume(s)
            consume(s)
        }",
    );
    assert_error_contains(&result, "moved");
}

// ===========================================================================
// S4: Borrowing — shared borrows
// ===========================================================================

#[test]
fn test_borrow_then_use() {
    assert_no_errors(
        "fn read(s String) {}
        fn main() {
            name: String = \"hello\"
            read(name)
            x := name
        }",
    );
}

#[test]
fn test_multiple_shared_borrows_ok() {
    assert_no_errors(
        "fn read(s String) {}
        fn main() {
            name: String = \"hello\"
            read(name)
            read(name)
            read(name)
        }",
    );
}

#[test]
fn test_borrow_then_move_error() {
    let result = assert_has_errors(
        "fn read(s String) {}
        fn consume(own s String) {}
        fn main() {
            name: String = \"hello\"
            consume(name)
            read(name)
        }",
    );
    assert_error_contains(&result, "moved");
}

// ===========================================================================
// S5: Mutable borrowing
// ===========================================================================

#[test]
fn test_single_mut_borrow_ok() {
    assert_no_errors(
        "fn modify(mut s String) {}
        fn main() {
            name: String = \"hello\"
            modify(name)
        }",
    );
}

#[test]
fn test_double_mut_borrow_error() {
    let result = assert_has_errors(
        "fn modify(mut s String) {}
        fn main() {
            name: String = \"hello\"
            modify(name)
            modify(name)
        }",
    );
    assert_error_contains(&result, "mutably borrowed");
}

#[test]
fn test_shared_and_mut_borrow_error() {
    let result = assert_has_errors(
        "fn read(s String) {}
        fn modify(mut s String) {}
        fn main() {
            name: String = \"hello\"
            modify(name)
            read(name)
        }",
    );
    assert_error_contains(&result, "mutably borrowed");
}

#[test]
fn test_mut_then_shared_error() {
    let result = assert_has_errors(
        "fn modify(mut s String) {}
        fn read(s String) {}
        fn main() {
            data: String = \"test\"
            modify(data)
            read(data)
        }",
    );
    assert_error_contains(&result, "mutably borrowed");
}

// ===========================================================================
// S6: Own parameter semantics
// ===========================================================================

#[test]
fn test_own_param_moves() {
    let result = assert_has_errors(
        "fn consume(own s String) {}
        fn main() {
            s: String = \"value\"
            consume(s)
            consume(s)
        }",
    );
    assert_error_contains(&result, "moved");
}

#[test]
fn test_own_param_i32_does_not_move() {
    assert_no_errors(
        "fn take(own n i32) {}
        fn main() {
            x := 5
            take(x)
            take(x)
        }",
    );
}

// ===========================================================================
// S7: Mut parameter semantics
// ===========================================================================

#[test]
fn test_mut_param_borrows() {
    assert_no_errors(
        "fn modify(mut s String) {}
        fn main() {
            s: String = \"value\"
            modify(s)
        }",
    );
}

// ===========================================================================
// S8: Default (borrow) parameter semantics
// ===========================================================================

#[test]
fn test_default_param_borrows() {
    assert_no_errors(
        "fn read(s String) {}
        fn main() {
            s: String = \"value\"
            read(s)
            read(s)
        }",
    );
}

#[test]
fn test_default_param_does_not_move() {
    assert_no_errors(
        "fn read(s String) {}
        fn main() {
            s: String = \"value\"
            read(s)
            x := s
        }",
    );
}

// ===========================================================================
// S9: Control flow — if/else
// ===========================================================================

#[test]
fn test_conditional_move_error() {
    // If we move in one branch, the variable is considered moved after.
    let result = assert_has_errors(
        "fn consume(own s String) {}
        fn main() {
            s: String = \"hello\"
            if true {
                consume(s)
            }
            x := s
        }",
    );
    assert_error_contains(&result, "moved");
}

#[test]
fn test_both_branches_move_ok_no_use_after() {
    // Moving in both branches is fine if we never use it after.
    assert_no_errors(
        "fn consume(own s String) {}
        fn report(own s String) {}
        fn main() {
            s: String = \"hello\"
            if true {
                consume(s)
            } else {
                report(s)
            }
        }",
    );
}

#[test]
fn test_if_no_else_move_error() {
    let result = assert_has_errors(
        "fn consume(own s String) {}
        fn main() {
            s: String = \"data\"
            if true {
                consume(s)
            }
            consume(s)
        }",
    );
    assert_error_contains(&result, "moved");
}

// ===========================================================================
// S10: Loops — move inside loop
// ===========================================================================

#[test]
fn test_loop_move_error() {
    let result = assert_has_errors(
        "fn consume(own s String) {}
        fn main() {
            s: String = \"data\"
            while true {
                consume(s)
            }
        }",
    );
    assert_error_contains(&result, "moved");
}

#[test]
fn test_for_loop_move_error() {
    let result = assert_has_errors(
        "fn consume(own s String) {}
        fn main() {
            s: String = \"data\"
            for i in 0..10 {
                consume(s)
            }
        }",
    );
    assert_error_contains(&result, "moved");
}

#[test]
fn test_loop_copy_type_ok() {
    assert_no_errors(
        "fn take(own n i32) {}
        fn main() {
            x := 42
            while true {
                take(x)
            }
        }",
    );
}

// ===========================================================================
// S11: Match arms are independent
// ===========================================================================

#[test]
fn test_match_arm_independent() {
    // Each match arm is its own scope; moving in one does not affect others.
    assert_no_errors(
        "fn consume(own s String) {}
        fn main() {
            s: String = \"data\"
            x := 1
            match x {
                1 => consume(s)
                _ => consume(s)
            }
        }",
    );
}

// ===========================================================================
// S12: Assignment to immutable / mutable
// ===========================================================================

#[test]
fn test_assignment_to_immutable_error() {
    let result = assert_has_errors(
        "fn main() {
            x := 5
            x = 10
        }",
    );
    assert_error_contains(&result, "not declared as mutable");
}

#[test]
fn test_assignment_to_mutable_ok() {
    assert_no_errors(
        "fn main() {
            mut x := 5
            x = 10
        }",
    );
}

#[test]
fn test_mutable_reassignment_multiple() {
    assert_no_errors(
        "fn main() {
            mut counter := 0
            counter = 1
            counter = 2
            counter = 3
        }",
    );
}

// ===========================================================================
// S13: Spawn and channel
// ===========================================================================

#[test]
fn test_spawn_captures() {
    // When a non-Copy variable is captured in spawn, it is moved.
    let result = assert_has_errors(
        "fn main() {
            s: String = \"hello\"
            spawn {
                x := s
            }
            y := s
        }",
    );
    assert_error_contains(&result, "moved");
}

#[test]
fn test_spawn_copy_type_ok() {
    assert_no_errors(
        "fn main() {
            n := 42
            spawn {
                x := n
            }
            y := n
        }",
    );
}

#[test]
fn test_channel_send() {
    // Sending a non-Copy value through a channel moves it.
    let result = assert_has_errors(
        "fn main() {
            s: String = \"data\"
            ch := chan[String]()
            ch.send(s)
            x := s
        }",
    );
    assert_error_contains(&result, "moved");
}

#[test]
fn test_channel_send_copy_ok() {
    assert_no_errors(
        "fn main() {
            n := 42
            ch := chan[i32]()
            ch.send(n)
            x := n
        }",
    );
}

// ===========================================================================
// S14: Return reference to local
// ===========================================================================

#[test]
fn test_return_local_ref_error() {
    let result = assert_has_errors(
        "fn bad() -> &i32 {
            x := 42
            return &x
        }",
    );
    assert_error_contains(&result, "cannot return reference to local");
}

// ===========================================================================
// S15: Struct literal field moves
// ===========================================================================

#[test]
fn test_struct_literal_moves_field() {
    let result = assert_has_errors(
        "struct Wrapper {
            value String
        }
        fn main() {
            s: String = \"hello\"
            w := Wrapper { value: s }
            x := s
        }",
    );
    assert_error_contains(&result, "moved");
}

#[test]
fn test_struct_literal_copy_field_ok() {
    assert_no_errors(
        "struct Point {
            x i32
            y i32
        }
        fn main() {
            a := 1
            b := 2
            p := Point { x: a, y: b }
            c := a + b
        }",
    );
}

// ===========================================================================
// S16: Multiple functions
// ===========================================================================

#[test]
fn test_multiple_functions_independent() {
    assert_no_errors(
        "fn foo() {
            x := 5
        }
        fn bar() {
            y := 10
        }",
    );
}

#[test]
fn test_function_call_chain() {
    assert_no_errors(
        "fn identity(x i32) -> i32 {
            return x
        }
        fn main() {
            a := 5
            b := identity(a)
            c := identity(a)
        }",
    );
}

// ===========================================================================
// S17: Edge cases
// ===========================================================================

#[test]
fn test_unused_variable() {
    // Unused variables are fine from a borrow-check perspective.
    assert_no_errors(
        "fn main() {
            x := 5
        }",
    );
}

#[test]
fn test_shadowing_not_confused_with_move() {
    // A new `let` with the same name shadows but does not move the original.
    assert_no_errors(
        "fn main() {
            x := 5
            x := 10
        }",
    );
}

#[test]
fn test_nested_if_move() {
    let result = assert_has_errors(
        "fn consume(own s String) {}
        fn main() {
            s: String = \"hello\"
            if true {
                if true {
                    consume(s)
                }
            }
            x := s
        }",
    );
    assert_error_contains(&result, "moved");
}

#[test]
fn test_return_owned_value_ok() {
    assert_no_errors(
        "fn make() -> i32 {
            x := 42
            return x
        }",
    );
}

#[test]
fn test_binary_expr_of_copies() {
    assert_no_errors(
        "fn main() {
            a := 1
            b := 2
            c := a + b
            d := a * b
        }",
    );
}

#[test]
fn test_while_condition_no_move() {
    assert_no_errors(
        "fn main() {
            mut x := 10
            while x > 0 {
                x = x - 1
            }
        }",
    );
}

#[test]
fn test_closure_captures_copy() {
    assert_no_errors(
        "fn main() {
            x := 42
            f := |a i32| { a + x }
            y := x
        }",
    );
}

#[test]
fn test_multiple_params_different_ownership() {
    assert_no_errors(
        "fn process(own a String, b String, mut c String) {}
        fn main() {
            a: String = \"a\"
            b: String = \"b\"
            c: String = \"c\"
            process(a, b, c)
        }",
    );
}

#[test]
fn test_own_param_then_borrow_different_var() {
    assert_no_errors(
        "fn consume(own s String) {}
        fn read(s String) {}
        fn main() {
            a: String = \"hello\"
            b: String = \"world\"
            consume(a)
            read(b)
        }",
    );
}

#[test]
fn test_no_functions() {
    // An empty source file should produce no errors.
    let result = borrow_check("");
    assert!(result.errors.is_empty());
}

#[test]
fn test_impl_method_checked() {
    assert_no_errors(
        "struct Counter {
            value i32
        }
        impl Counter {
            fn get(self) -> i32 {
                return self.value
            }
        }",
    );
}

// ===========================================================================
// S18: Additional coverage
// ===========================================================================

#[test]
fn test_nested_function_calls() {
    assert_no_errors(
        "fn id(x i32) -> i32 { return x }
        fn main() {
            a := 5
            b := id(id(a))
        }",
    );
}

#[test]
fn test_string_move_then_use_in_different_fn() {
    // Each function has its own scope.
    assert_no_errors(
        "fn first(own s String) {}
        fn second(own s String) {}
        fn main() {
            a: String = \"x\"
            first(a)
        }
        fn other() {
            b: String = \"y\"
            second(b)
        }",
    );
}

#[test]
fn test_move_then_rebind_ok() {
    // After moving, if we rebind the same name, the new binding is valid.
    assert_no_errors(
        "fn consume(own s String) {}
        fn main() {
            mut s: String = \"first\"
            consume(s)
            s = \"second\"
            consume(s)
        }",
    );
}

#[test]
fn test_error_count_double_move() {
    // Two uses after move: each generates both a "use of moved value"
    // and a "cannot move" error (4 total).
    let result = assert_error_count(
        "fn consume(own s String) {}
        fn main() {
            s: String = \"data\"
            consume(s)
            consume(s)
            consume(s)
        }",
        4,
    );
    assert_error_contains(&result, "moved");
}

#[test]
fn test_loop_block_move_error() {
    let result = assert_has_errors(
        "fn consume(own s String) {}
        fn main() {
            s: String = \"data\"
            loop {
                consume(s)
            }
        }",
    );
    assert_error_contains(&result, "moved");
}

#[test]
fn test_if_else_both_borrow_ok() {
    assert_no_errors(
        "fn read(s String) {}
        fn main() {
            s: String = \"data\"
            if true {
                read(s)
            } else {
                read(s)
            }
            read(s)
        }",
    );
}

// ===========================================================================
// Regression tests — guards against reintroduction of fixed bugs
// ===========================================================================

#[test]
fn regression_closure_captures_move_non_copy() {
    // Bug: closures that captured non-Copy variables didn't mark them as moved,
    // allowing use-after-move.
    let result = assert_has_errors(
        "fn test() {\n    s := \"hello\"\n    f := |x| s\n    print(s)\n}"
    );
    assert_error_contains(&result, "moved");
}

#[test]
fn regression_move_after_borrow() {
    // Bug: moving a variable that was already borrowed was not detected.
    // The Own parameter path didn't check borrow state before moving.
    let result = assert_has_errors(
        "fn borrow(s String) {}\nfn consume(own s String) {}\nfn test() {\n    s := \"hello\"\n    borrow(s)\n    consume(s)\n}"
    );
    assert_error_contains(&result, "borrow");
}

#[test]
fn regression_unknown_fn_defaults_to_own() {
    // Bug: calling an unknown function defaulted to Borrow semantics,
    // meaning non-Copy args were never consumed. Now defaults to Own (conservative).
    let result = assert_has_errors(
        "fn test() {\n    s := \"hello\"\n    unknown_function(s)\n    print(s)\n}"
    );
    assert_error_contains(&result, "moved");
}

#[test]
fn regression_own_self_method_moves_receiver() {
    // Bug: method calls with `own self` semantics didn't move the receiver.
    let result = assert_has_errors(
        "fn consume(own s String) {}\nfn test() {\n    s := \"hello\"\n    s.consume()\n    print(s)\n}"
    );
    assert_error_contains(&result, "moved");
}

#[test]
fn regression_field_assign_on_immutable() {
    // Bug: assigning to a field of an immutable variable (e.g., p.x = 5)
    // was not caught.
    let result = assert_has_errors(
        "struct Point { x i32 }\nfn test() {\n    p := Point { x: 1 }\n    p.x = 5\n}"
    );
    assert_error_contains(&result, "mutable");
}

#[test]
fn regression_loop_reassign_before_move_no_false_positive() {
    // Bug: moving a variable in a loop body that was reassigned before
    // the move caused a false positive on the second iteration.
    assert_no_errors(
        "fn take(own s String) {}\nfn test() {\n    mut s := \"hello\"\n    mut i := 0\n    while i < 3 {\n        s = \"new\"\n        take(s)\n        i = i + 1\n    }\n}"
    );
}

#[test]
fn regression_move_in_while_condition() {
    // Bug: moving a variable in a while loop condition wasn't caught because
    // the snapshot was taken after evaluating the condition, not before.
    let result = assert_has_errors(
        "fn consume(own s String) -> bool { return true }\nfn test() {\n    s := \"hello\"\n    while consume(s) {\n        x := 1\n    }\n}"
    );
    assert_error_contains(&result, "moved");
}

#[test]
fn regression_for_loop_iterable_consumed() {
    // Bug: the iterable in a for-loop wasn't marked as moved/consumed,
    // allowing use-after-move.
    let result = assert_has_errors(
        "fn test() {\n    arr := \"hello\"\n    for c in arr {\n        x := c\n    }\n    print(arr)\n}"
    );
    assert_error_contains(&result, "moved");
}

#[test]
fn regression_borrow_and_move_in_same_call() {
    // Bug: passing a borrowed variable and then moving it in the same
    // function call args was not detected.
    let result = assert_has_errors(
        "fn two_args(a String, own b String) {}\nfn test() {\n    s := \"hello\"\n    two_args(s, s)\n}"
    );
    assert_error_contains(&result, "borrow");
}
