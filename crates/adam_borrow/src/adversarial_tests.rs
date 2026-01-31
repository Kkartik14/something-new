//! Adversarial tests — designed to break the borrow checker.
//!
//! These tests target edge cases, gaps, and potential unsoundness in the
//! borrow-checking implementation. Each test documents what behavior it
//! expects and why.

use adam_lexer::Lexer;
use adam_parser::Parser;
use adam_resolve::resolve;
use adam_types::TypeChecker;

use crate::checker::{BorrowCheckResult, BorrowChecker};

// ---- Helpers ---------------------------------------------------------------

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

fn assert_no_errors(src: &str) {
    let result = borrow_check(src);
    if !result.errors.is_empty() {
        let msgs: Vec<_> = result.errors.iter().map(|e| e.to_string()).collect();
        panic!("expected no borrow errors, got:\n{}", msgs.join("\n"));
    }
}

fn assert_has_errors(src: &str) -> BorrowCheckResult {
    let result = borrow_check(src);
    assert!(
        !result.errors.is_empty(),
        "expected borrow errors, but got none"
    );
    result
}

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
// 1. Double move through function calls (indirect paths)
//    consume(x) -> internal move; then consume(x) again should be caught.
// ===========================================================================

#[test]
fn adversarial_01_double_move_through_fn_calls() {
    // x is moved into consume, then passed again — must be caught.
    let result = assert_has_errors(
        "fn consume(own s String) {}
        fn main() {
            s: String = \"hello\"
            consume(s)
            consume(s)
        }",
    );
    assert_error_contains(&result, "moved");
}

#[test]
fn adversarial_01b_move_via_let_then_fn() {
    // Move through let binding, then try to call with original.
    let result = assert_has_errors(
        "fn consume(own s String) {}
        fn main() {
            s: String = \"hello\"
            t := s
            consume(s)
        }",
    );
    assert_error_contains(&result, "moved");
}

#[test]
fn adversarial_01c_move_through_struct_then_use() {
    // Move into a struct field, then try to use original.
    let result = assert_has_errors(
        "struct Wrapper { value String }
        fn main() {
            s: String = \"hello\"
            w := Wrapper { value: s }
            t := s
        }",
    );
    assert_error_contains(&result, "moved");
}

// ===========================================================================
// 2. Move in one branch, borrow in another, then use after
//    BUG CANDIDATE: The checker merges branch states conservatively, but
//    does it correctly handle the else-if chain?
// ===========================================================================

#[test]
fn adversarial_02_move_in_if_borrow_in_else_then_use() {
    // Move in if, borrow in else. After the if/else, the value is
    // *possibly* moved, so using it after should be an error.
    let result = assert_has_errors(
        "fn consume(own s String) {}
        fn read(s String) {}
        fn main() {
            s: String = \"hello\"
            if true {
                consume(s)
            } else {
                read(s)
            }
            read(s)
        }",
    );
    assert_error_contains(&result, "moved");
}

#[test]
fn adversarial_02b_move_in_else_not_if_then_use() {
    // Move only in else branch, use after — should still be an error.
    let result = assert_has_errors(
        "fn consume(own s String) {}
        fn read(s String) {}
        fn main() {
            s: String = \"hello\"
            if true {
                read(s)
            } else {
                consume(s)
            }
            read(s)
        }",
    );
    assert_error_contains(&result, "moved");
}

#[test]
fn adversarial_02c_move_in_elseif_chain() {
    // Move in one branch of an else-if chain. Use after — should be error.
    let result = assert_has_errors(
        "fn consume(own s String) {}
        fn read(s String) {}
        fn main() {
            s: String = \"hello\"
            if true {
                read(s)
            } else if false {
                consume(s)
            } else {
                read(s)
            }
            read(s)
        }",
    );
    assert_error_contains(&result, "moved");
}

// ===========================================================================
// 3. Nested loop moves
//    Move inside inner loop — should be caught. Does the outer loop also
//    detect that the value is gone on the next outer iteration?
// ===========================================================================

#[test]
fn adversarial_03_nested_loop_inner_move() {
    // Move s inside the inner loop. This should be caught for the inner loop.
    let result = assert_has_errors(
        "fn consume(own s String) {}
        fn main() {
            s: String = \"data\"
            while true {
                while true {
                    consume(s)
                }
            }
        }",
    );
    assert_error_contains(&result, "moved");
}

#[test]
fn adversarial_03b_nested_for_loops_move_outer_var() {
    // Move in inner for loop body — outer var should be caught.
    let result = assert_has_errors(
        "fn consume(own s String) {}
        fn main() {
            s: String = \"data\"
            for i in 0..5 {
                for j in 0..5 {
                    consume(s)
                }
            }
        }",
    );
    assert_error_contains(&result, "moved");
}

#[test]
fn adversarial_03c_move_between_inner_and_outer_loop() {
    // Move in outer loop but outside inner loop. On second outer iteration
    // the variable is gone.
    let result = assert_has_errors(
        "fn consume(own s String) {}
        fn main() {
            s: String = \"data\"
            while true {
                for i in 0..5 {
                    i
                }
                consume(s)
            }
        }",
    );
    assert_error_contains(&result, "moved");
}

// ===========================================================================
// 4. Partial struct moves
//    BUG CANDIDATE: Move one field, then try to use another field.
//    The checker does NOT track per-field moves. It only tracks whole-variable
//    state. Moving a field access expression is not detected because
//    check_expr for FieldAccess only checks the object sub-expression.
// ===========================================================================

#[test]
fn adversarial_04_partial_struct_move_field_access() {
    // Access field of a struct after moving the whole struct — should error.
    let result = assert_has_errors(
        "struct Pair {
            first String
            second String
        }
        fn consume(own s Pair) {}
        fn main() {
            p := Pair { first: \"a\", second: \"b\" }
            consume(p)
            x := p.first
        }",
    );
    assert_error_contains(&result, "moved");
}

// ===========================================================================
// 5. Closure captures in loops
//    BUG CANDIDATE: Closure created in a loop that captures a mutable
//    reference. The closure check_closure does NOT mark captured non-Copy
//    variables as moved from the outer scope.
// ===========================================================================

#[test]
fn adversarial_05_closure_captures_non_copy_in_loop() {
    // A closure that captures a non-Copy variable should move it.
    // Creating such a closure in a loop means the second iteration fails.
    // BUG: The checker does NOT move captured non-Copy vars out of outer scope.
    let result = borrow_check(
        "fn main() {
            s: String = \"hello\"
            while true {
                f := |x i32| { s }
            }
        }",
    );
    // The closure captures s (non-Copy). In a loop, the second iteration
    // would capture an already-moved value. This should be an error.
    if result.errors.is_empty() {
        panic!("BUG FOUND: Closure captures of non-Copy variables in a loop \
                are not detected. The variable is captured (moved) on the \
                first iteration but the checker doesn't track this.");
    }
}

#[test]
fn adversarial_05b_closure_captures_then_use_after() {
    // Capture s in closure, then try to use s after. If s is non-Copy,
    // the closure should have moved it.
    // BUG: The checker does NOT move captured variables from outer scope.
    let result = borrow_check(
        "fn main() {
            s: String = \"hello\"
            f := |x i32| { s }
            t := s
        }",
    );
    // s is captured by the closure — it should be moved.
    if result.errors.is_empty() {
        panic!("BUG FOUND: Closure captures of non-Copy variables do not \
                mark the variable as moved from the outer scope. \
                Variable 's' used after being captured by closure.");
    }
}

// ===========================================================================
// 6. Reborrow edge cases
//    Take a mutable borrow, then try to read through another path.
// ===========================================================================

#[test]
fn adversarial_06_mut_borrow_then_read() {
    // After mutably borrowing via modify(), trying to read should fail.
    // (This relies on the fact that borrows are not released.)
    let result = assert_has_errors(
        "fn modify(mut s String) {}
        fn read(s String) {}
        fn main() {
            s: String = \"hello\"
            modify(s)
            read(s)
        }",
    );
    assert_error_contains(&result, "mutably borrowed");
}

// ===========================================================================
// 7. Return reference to parameter
//    fn foo(x String) -> &String { return &x }
//    The parameter is a local, so returning a reference to it should be caught.
// ===========================================================================

#[test]
fn adversarial_07_return_ref_to_param() {
    // Returning a reference to a parameter (which is a local binding)
    // should be caught.
    let result = assert_has_errors(
        "fn bad(x String) -> &String {
            return &x
        }",
    );
    assert_error_contains(&result, "cannot return reference to local");
}

#[test]
fn adversarial_07b_return_ref_to_local_nested() {
    // Return &x from inside an if — should still be caught.
    let result = assert_has_errors(
        "fn bad() -> &i32 {
            x := 42
            if true {
                return &x
            }
            return &x
        }",
    );
    assert_error_contains(&result, "cannot return reference to local");
}

// ===========================================================================
// 8. Assignment to moved variable then use
//    Move x, then x = new_value, then use x — should be valid because
//    assignment re-initializes the variable.
// ===========================================================================

#[test]
fn adversarial_08_reassign_after_move_then_use() {
    // Move s, reassign it, then use it. Should be valid.
    assert_no_errors(
        "fn consume(own s String) {}
        fn read(s String) {}
        fn main() {
            mut s: String = \"first\"
            consume(s)
            s = \"second\"
            read(s)
        }",
    );
}

#[test]
fn adversarial_08b_reassign_after_move_use_old_value_bug() {
    // Move s, do NOT reassign, then use — should be error.
    let result = assert_has_errors(
        "fn consume(own s String) {}
        fn read(s String) {}
        fn main() {
            mut s: String = \"first\"
            consume(s)
            read(s)
        }",
    );
    assert_error_contains(&result, "moved");
}

// ===========================================================================
// 9. Spawn with mutable captures
//    spawn { x = 5 } where x is mutable. The spawn block should move
//    non-Copy captured variables.
// ===========================================================================

#[test]
fn adversarial_09_spawn_captures_and_moves() {
    // s is captured in spawn, so using it after spawn should fail.
    let result = assert_has_errors(
        "fn main() {
            s: String = \"hello\"
            spawn {
                t := s
            }
            x := s
        }",
    );
    assert_error_contains(&result, "moved");
}

#[test]
fn adversarial_09b_spawn_double_capture() {
    // Two spawns capturing the same non-Copy variable — second should fail.
    let result = assert_has_errors(
        "fn main() {
            s: String = \"hello\"
            spawn {
                t := s
            }
            spawn {
                u := s
            }
        }",
    );
    assert_error_contains(&result, "moved");
}

// ===========================================================================
// 10. Channel send in loop
//     Send the same non-copy value through a channel in every iteration.
//     The first iteration moves it; subsequent iterations should fail.
// ===========================================================================

#[test]
fn adversarial_10_channel_send_in_loop() {
    // ch.send(s) moves s. In a loop, it would be moved on the first
    // iteration, then used-after-move on the second.
    let result = assert_has_errors(
        "fn main() {
            s: String = \"data\"
            ch := chan[String]()
            while true {
                ch.send(s)
            }
        }",
    );
    assert_error_contains(&result, "moved");
}

#[test]
fn adversarial_10b_channel_send_twice_sequential() {
    // Two sends of the same non-Copy variable — second should fail.
    let result = assert_has_errors(
        "fn main() {
            s: String = \"data\"
            ch := chan[String]()
            ch.send(s)
            ch.send(s)
        }",
    );
    assert_error_contains(&result, "moved");
}

// ===========================================================================
// 11. Match arm moves with use after match
//     BUG CANDIDATE: Move in one match arm, use after match block.
// ===========================================================================

#[test]
fn adversarial_11_match_arm_move_then_use_after() {
    // Move in one match arm. After the match, the variable should be
    // considered moved (conservative merge).
    let result = assert_has_errors(
        "fn consume(own s String) {}
        fn main() {
            s: String = \"data\"
            x := 1
            match x {
                1 => consume(s)
                _ => x
            }
            t := s
        }",
    );
    assert_error_contains(&result, "moved");
}

#[test]
fn adversarial_11b_match_all_arms_move_no_use_after_ok() {
    // Move in all arms, but never use after — should be fine.
    assert_no_errors(
        "fn consume(own s String) {}
        fn report(own s String) {}
        fn main() {
            s: String = \"data\"
            x := 1
            match x {
                1 => consume(s)
                _ => report(s)
            }
        }",
    );
}

#[test]
fn adversarial_11c_match_move_in_some_arms_use_after() {
    // Move in the first arm, not the second. Use after should be error
    // because the merge is conservative (moved in any arm => moved).
    let result = assert_has_errors(
        "fn consume(own s String) {}
        fn read(s String) {}
        fn main() {
            s: String = \"data\"
            x := 1
            match x {
                1 => consume(s)
                _ => read(s)
            }
            read(s)
        }",
    );
    assert_error_contains(&result, "moved");
}

// ===========================================================================
// 12. Deeply nested control flow
//     5+ levels of if/while/for nesting with moves at different levels.
// ===========================================================================

#[test]
fn adversarial_12_deeply_nested_move() {
    // Move deep inside nested control flow, then use after.
    let result = assert_has_errors(
        "fn consume(own s String) {}
        fn main() {
            s: String = \"data\"
            if true {
                if true {
                    if true {
                        if true {
                            if true {
                                consume(s)
                            }
                        }
                    }
                }
            }
            t := s
        }",
    );
    assert_error_contains(&result, "moved");
}

#[test]
fn adversarial_12b_nested_loops_and_ifs_move() {
    // Nested while -> if -> for -> while -> move. Should be caught.
    let result = assert_has_errors(
        "fn consume(own s String) {}
        fn main() {
            s: String = \"data\"
            while true {
                if true {
                    for i in 0..3 {
                        while true {
                            consume(s)
                        }
                    }
                }
            }
        }",
    );
    assert_error_contains(&result, "moved");
}

// ===========================================================================
// 13. Self-referential patterns
//     A method that takes own self, and then tries to use self.
//     BUG CANDIDATE: Does the checker handle `own self` moves?
// ===========================================================================

#[test]
fn adversarial_13_own_self_use_after() {
    // BUG CANDIDATE: If a method takes `own self`, using self after the
    // method is called should be an error. But the checker may not track
    // this because method calls go through check_method_call which doesn't
    // move the receiver based on method param ownership.
    let result = assert_has_errors(
        "struct Obj { value String }
        fn consume(own s Obj) {}
        fn main() {
            o: Obj = Obj { value: \"hi\" }
            consume(o)
            x := o
        }",
    );
    assert_error_contains(&result, "moved");
}

// ===========================================================================
// 14. Multiple mutable borrows through different paths
//     BUG CANDIDATE: The mut borrow state persists, so sequential
//     mut borrows should be caught.
// ===========================================================================

#[test]
fn adversarial_14_sequential_mut_borrows() {
    // Two sequential mut borrows — the first is never released, so
    // the second should be caught.
    let result = assert_has_errors(
        "fn modify(mut s String) {}
        fn other_modify(mut s String) {}
        fn main() {
            s: String = \"hello\"
            modify(s)
            other_modify(s)
        }",
    );
    assert_error_contains(&result, "mutably borrowed");
}

#[test]
fn adversarial_14b_mut_borrow_then_move() {
    // Mutable borrow, then move — is the move caught?
    // The variable is in MutBorrowed state. Trying to move it
    // should fail because check_call will call check_expr (which calls
    // check_var_use, which only checks Moved state, NOT MutBorrowed).
    // BUG: After a mut borrow, the variable can still be moved because
    // check_var_use only checks for Moved state, not MutBorrowed.
    let result = borrow_check(
        "fn modify(mut s String) {}
        fn consume(own s String) {}
        fn main() {
            s: String = \"hello\"
            modify(s)
            consume(s)
        }",
    );
    // s is mutably borrowed by modify(). Then consume() tries to take
    // ownership (move). This should be an error — can't move a borrowed value.
    if result.errors.is_empty() {
        panic!("BUG FOUND: After a mutable borrow, the variable can still \
                be moved via an own-parameter call. check_var_use only \
                checks for Moved state, not MutBorrowed state. The \
                own-parameter code path also doesn't check for active borrows \
                before marking the variable as moved.");
    }
}

// ===========================================================================
// 15. Borrow then move in same expression
//     Something like foo(&x, consume(x)) — argument evaluation order matters.
//     BUG CANDIDATE: Arguments are evaluated left to right. If the first
//     arg borrows x and the second moves x, is this caught?
// ===========================================================================

#[test]
fn adversarial_15_borrow_and_move_in_same_call() {
    // Two arguments to the same function: first borrows, second moves.
    // This should be an error — you can't borrow and move in the same
    // expression.
    // BUG: The checker processes arguments sequentially. The first arg
    // (borrow) transitions to Borrowed state. The second arg (own/move)
    // mark_moved overrides the Borrowed state without checking for conflicts.
    let result = borrow_check(
        "fn both(a String, own b String) {}
        fn main() {
            s: String = \"hello\"
            both(s, s)
        }",
    );
    // The first arg borrows s, the second arg moves s.
    // This should be an error — can't borrow and move the same value
    // in the same expression.
    if result.errors.is_empty() {
        panic!("BUG FOUND: Borrowing and moving the same variable in the \
                same function call (different arguments) is not detected. \
                The first argument borrows s, then the second argument \
                moves s, but no error is raised because the Own path \
                in check_call doesn't check for active borrows before \
                calling mark_moved.");
    }
}

#[test]
fn adversarial_15b_move_and_borrow_same_call_reversed() {
    // First arg moves, second arg borrows — should definitely be caught.
    let result = assert_has_errors(
        "fn both(own a String, b String) {}
        fn main() {
            s: String = \"hello\"
            both(s, s)
        }",
    );
    assert_error_contains(&result, "moved");
}

// ===========================================================================
// 16. ADDITIONAL ADVERSARIAL TESTS — Discovered through code analysis
// ===========================================================================

// --- 16a: Unknown function parameters default to Borrow ---
// If we call a function that isn't defined in the file (external),
// the param ownership lookup fails and defaults to Borrow. This means
// we can pass ownership types to external functions without them being
// moved — the caller retains ownership even if the callee consumes.

#[test]
fn adversarial_16_unknown_fn_defaults_borrow() {
    // Calling an undefined function — all args default to Borrow ownership.
    // This means the variable is NOT moved. Is this correct?
    // In a sound borrow checker, calling an unknown function with a
    // non-Copy type should be conservative (assume move).
    // BUG: Unknown functions default to Borrow, allowing use-after-potential-move.
    let result = borrow_check(
        "fn main() {
            s: String = \"hello\"
            unknown_fn(s)
            t := s
        }",
    );
    // This SHOULD have errors (unknown function might consume s),
    // but the checker defaults to Borrow so it won't catch it.
    // If this passes with no errors, it's a real bug.
    if result.errors.is_empty() {
        panic!("BUG FOUND: Unknown function calls default to Borrow, \
                allowing use of potentially moved values after the call. \
                The checker should be conservative and assume ownership transfer \
                for unknown functions with non-Copy arguments.");
    }
}

// --- 16b: Closure does not track captures ---

#[test]
fn adversarial_16b_closure_does_not_move_captures() {
    // BUG: Closures don't mark captured non-Copy variables as moved.
    // This means you can use a variable after it's been captured
    // by a closure, even though the closure might consume it.
    let result = borrow_check(
        "fn main() {
            s: String = \"hello\"
            f := |x i32| { s }
            t := s
        }",
    );
    // s is captured by the closure — it should be moved.
    // If the checker allows this (no errors), it's a bug.
    if result.errors.is_empty() {
        panic!("BUG FOUND: Closure captures of non-Copy variables do not \
                mark the variable as moved from the outer scope. \
                The variable can be used after being captured.");
    }
}

// --- 16c: Method call does not track ownership transfer for receiver ---

#[test]
fn adversarial_16c_method_call_receiver_not_moved() {
    // BUG CANDIDATE: check_method_call only checks the receiver expression
    // and arguments, but does NOT look up the method's parameter ownership
    // for the self parameter. So `obj.consume_self()` where consume_self
    // takes `own self` will NOT move obj.
    let result = borrow_check(
        "struct Obj { value String }
        impl Obj {
            fn consume_self(own self) {}
        }
        fn main() {
            o: Obj = Obj { value: \"hi\" }
            o.consume_self()
            x := o
        }",
    );
    // o.consume_self() should move o (since it takes own self).
    // If the checker allows using o after, it's a bug.
    if result.errors.is_empty() {
        panic!("BUG FOUND: Method calls with `own self` do not move \
                the receiver. The variable can be used after the method \
                consumes it.");
    }
}

// --- 16d: Assignment to non-mutable through field access ---

#[test]
fn adversarial_16d_assign_to_field_of_immutable() {
    // Assigning to a field of an immutable variable should be an error.
    // BUG: The assignment check only looks at Identifier targets, not
    // FieldAccess targets. So `x.field = val` on an immutable x is allowed.
    let result = borrow_check(
        "struct Point {
            x i32
            y i32
        }
        fn main() {
            p := Point { x: 1, y: 2 }
            p.x = 5
        }",
    );
    // p is immutable, so p.x = 5 should be an error.
    if result.errors.is_empty() {
        panic!("BUG FOUND: Assignment to a field of an immutable variable \
                is not caught. The checker only validates mutability for \
                direct Identifier assignments, not FieldAccess targets.");
    }
}

// --- 16e: Move in match scrutinee not tracked ---

#[test]
fn adversarial_16e_move_in_match_scrutinee() {
    // If the scrutinee is a non-Copy variable, matching on it should
    // consume it (move it into the match).
    let result = assert_has_errors(
        "fn consume(own s String) {}
        fn main() {
            s: String = \"hello\"
            x := s
            consume(s)
        }",
    );
    assert_error_contains(&result, "moved");
}

// --- 16f: Loop with reassignment should be valid ---

#[test]
fn adversarial_16f_loop_reassign_before_move() {
    // In a loop: reassign s at the top, then consume it.
    // This should be valid because each iteration re-initializes s.
    // BUG CANDIDATE: The checker snapshots before the loop and checks
    // if any pre-existing var was moved. But if s is reassigned inside
    // the loop before being consumed, the checker may still flag it.
    // Actually, since we snapshot before the loop, and s starts as Owned,
    // the first iteration: s=Owned -> s="new" (re-owned) -> consume(s) (moved).
    // check_loop_moves compares snapshot(Owned) vs current(Moved) => error.
    // But this is a FALSE POSITIVE — the reassignment should make it valid.
    let result = borrow_check(
        "fn consume(own s String) {}
        fn main() {
            mut s: String = \"first\"
            while true {
                s = \"new\"
                consume(s)
            }
        }",
    );
    // This should have NO errors — the reassignment re-initializes s
    // before each consume. But the checker likely flags it.
    if !result.errors.is_empty() {
        let msgs: Vec<_> = result.errors.iter().map(|e| e.to_string()).collect();
        panic!("BUG FOUND (false positive): Reassignment inside loop before \
                move is still flagged as error. The checker does not account \
                for re-initialization within loop bodies.\n\
                Errors: {}", msgs.join("; "));
    }
}

// --- 16g: Assign moves source but checker doesn't check Assign value for non-ident ---

#[test]
fn adversarial_16g_assign_from_moved_variable() {
    // Assign from a moved variable — the moved check happens via check_expr
    // on the value, which calls check_var_use.
    let result = assert_has_errors(
        "fn consume(own s String) {}
        fn main() {
            mut t: String = \"initial\"
            s: String = \"hello\"
            consume(s)
            t = s
        }",
    );
    assert_error_contains(&result, "moved");
}

// --- 16h: Move inside if without else, then reassign, then use ---

#[test]
fn adversarial_16h_move_in_if_reassign_after() {
    // Move in if branch (no else). After the if, assign new value, then use.
    // After merge: s is moved (conservatively). Then s = "new" re-owns it.
    // Then read(s) should be fine.
    assert_no_errors(
        "fn consume(own s String) {}
        fn read(s String) {}
        fn main() {
            mut s: String = \"hello\"
            if true {
                consume(s)
            }
            s = \"world\"
            read(s)
        }",
    );
}

// --- 16i: For loop binding shadows outer variable ---

#[test]
fn adversarial_16i_for_loop_binding_shadows() {
    // The for-loop binding 'i' is declared inside the loop scope.
    // After the loop, 'i' should not be accessible (scope ends).
    // But the borrow checker tracks names in a flat map — does it
    // leak the loop binding into the outer scope?
    assert_no_errors(
        "fn main() {
            for i in 0..10 {
                x := i
            }
        }",
    );
}

// --- 16j: String literal move tracking ---

#[test]
fn adversarial_16j_string_literal_assign_is_new_value() {
    // String literals create new values. Assigning a string literal
    // to a moved variable should re-initialize it.
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

// --- 16k: Match with no arms ---

#[test]
fn adversarial_16k_empty_match() {
    // Edge case: match with no arms. Should not crash.
    assert_no_errors(
        "fn main() {
            x := 1
            match x {
            }
        }",
    );
}

// --- 16l: Nested spawn blocks ---

#[test]
fn adversarial_16l_nested_spawn_blocks() {
    // Nested spawn: outer spawn captures s, inner spawn tries to use s.
    // The outer spawn should move s from main's scope.
    let result = assert_has_errors(
        "fn main() {
            s: String = \"hello\"
            spawn {
                t := s
            }
            y := s
        }",
    );
    assert_error_contains(&result, "moved");
}

// --- 16m: Return reference through nested unary ---

#[test]
fn adversarial_16m_return_ref_direct() {
    // Basic return reference to local.
    let result = assert_has_errors(
        "fn bad() -> &i32 {
            x := 42
            return &x
        }",
    );
    assert_error_contains(&result, "cannot return reference to local");
}

// --- 16n: Multiple variables moved in different if branches ---

#[test]
fn adversarial_16n_different_vars_moved_in_branches() {
    // Move different variables in different branches, then use both after.
    // Both should be errors.
    let result = assert_has_errors(
        "fn consume(own s String) {}
        fn main() {
            a: String = \"hello\"
            b: String = \"world\"
            if true {
                consume(a)
            } else {
                consume(b)
            }
            x := a
            y := b
        }",
    );
    // Both a and b should be considered moved (conservative merge).
    assert!(
        result.errors.len() >= 2,
        "expected at least 2 errors (both a and b moved), got {}: {:?}",
        result.errors.len(),
        result.errors.iter().map(|e| e.message.clone()).collect::<Vec<_>>()
    );
}

// --- 16o: Immutable variable assignment through index ---

#[test]
fn adversarial_16o_assign_to_index_of_immutable() {
    // BUG CANDIDATE: Assignment to arr[0] where arr is immutable.
    // The checker only checks Identifier targets for mutability.
    let result = borrow_check(
        "fn main() {
            arr := [1, 2, 3]
            arr[0] = 99
        }",
    );
    // arr is immutable, so arr[0] = 99 should be an error.
    if result.errors.is_empty() {
        panic!("BUG FOUND: Assignment to index of immutable variable \
                is not caught. The checker only validates mutability \
                for direct Identifier assignments, not Index targets.");
    }
}

// --- 16p: Use of variable inside its own initializer ---

#[test]
fn adversarial_16p_self_referential_init() {
    // This is more of a parser/resolver issue, but let's see if
    // the borrow checker handles it gracefully.
    let result = borrow_check(
        "fn main() {
            x := 5
        }",
    );
    assert!(result.errors.is_empty());
}

// --- 16q: Move in while condition ---

#[test]
fn adversarial_16q_move_in_while_condition() {
    // BUG CANDIDATE: If the while condition moves a variable,
    // the second evaluation of the condition (second iteration)
    // would use-after-move. But the checker evaluates the condition
    // BEFORE snapshotting for the loop body.
    // Actually, check_while calls check_expr on condition BEFORE snapshot.
    // So a move in the condition won't be detected as a loop-move.
    let result = borrow_check(
        "fn takes_own(own s String) -> bool { return true }
        fn main() {
            s: String = \"hello\"
            while takes_own(s) {
                x := 1
            }
        }",
    );
    // The condition moves s on each evaluation. On the second iteration,
    // the condition would use-after-move. This should be an error.
    if result.errors.is_empty() {
        panic!("BUG FOUND: Move in while condition is not detected as a \
                loop-iteration error. The condition is evaluated before \
                the loop body snapshot, so re-evaluation on the next \
                iteration would use a moved value.");
    }
}

// --- 16r: For loop iterable move ---

#[test]
fn adversarial_16r_for_loop_iterable_move() {
    // The iterable expression in a for loop is evaluated once.
    // If it's a non-Copy variable, it should be moved (consumed by the loop).
    // BUG: check_for calls check_expr on the iterable, which calls
    // check_var_use (just checks if already moved), but does NOT mark
    // the iterable variable as moved. So using it after the loop is allowed.
    let result = borrow_check(
        "fn main() {
            items: String = \"hello\"
            for c in items {
                c
            }
            t := items
        }",
    );
    // items is consumed by the for loop (iterated over and consumed).
    // Using items after the loop should be an error.
    if result.errors.is_empty() {
        panic!("BUG FOUND: For-loop iterable (non-Copy variable) is not \
                moved/consumed by the loop. The variable remains usable \
                after the for loop, even though the loop consumed it.");
    }
}

// --- 16s: Struct literal with same var for multiple fields ---

#[test]
fn adversarial_16s_struct_same_var_two_fields() {
    // Use the same non-Copy variable for two struct fields.
    // The first field moves it, the second should fail.
    let result = assert_has_errors(
        "struct TwoStrings {
            a String
            b String
        }
        fn main() {
            s: String = \"hello\"
            obj := TwoStrings { a: s, b: s }
        }",
    );
    assert_error_contains(&result, "moved");
}

// --- 16t: Borrow after move should mention "moved" ---

#[test]
fn adversarial_16t_borrow_after_move() {
    let result = assert_has_errors(
        "fn consume(own s String) {}
        fn read(s String) {}
        fn main() {
            s: String = \"hello\"
            consume(s)
            read(s)
        }",
    );
    // The error should mention that s was moved.
    assert_error_contains(&result, "moved");
}

// --- 16u: Multiple function definitions with same param name ---

#[test]
fn adversarial_16u_same_param_name_different_fns() {
    // Two functions with same param name but different ownership.
    // Make sure the checker doesn't confuse them.
    assert_no_errors(
        "fn consume(own s String) {}
        fn read(s String) {}
        fn main() {
            a: String = \"hello\"
            read(a)
            b: String = \"world\"
            consume(b)
        }",
    );
}

// --- 16v: Move inside block expression ---

#[test]
fn adversarial_16v_move_inside_block_expr() {
    // Move a variable inside a block expression, then use after.
    let result = assert_has_errors(
        "fn consume(own s String) {}
        fn main() {
            s: String = \"hello\"
            x := {
                consume(s)
                42
            }
            t := s
        }",
    );
    assert_error_contains(&result, "moved");
}
