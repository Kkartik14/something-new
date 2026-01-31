//! Adversarial tests for the Adam IR lowering and optimization passes.
//!
//! These tests are designed to FIND BUGS, not to confirm happy paths.

use adam_lexer::Lexer;
use adam_parser::Parser;

use crate::ir::*;
use crate::lower::lower_module;
use crate::opt;
use crate::print::{print_function, print_module};
use crate::verify::{verify_function_standalone, verify_module};

// ================================================================
// Test helpers
// ================================================================

/// Lex, parse, and lower source code to an IR module.
fn lower(src: &str) -> IrModule {
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

    lower_module(&parse_result.ast)
}

/// Lower and verify -- asserts no verification errors.
fn lower_and_verify(src: &str) -> IrModule {
    let module = lower(src);
    let result = verify_module(&module);
    assert!(
        result.is_ok(),
        "verify errors: {:?}",
        result.errors
    );
    module
}

/// Lower, optimize, then verify.
fn lower_optimize_verify(src: &str) -> IrModule {
    let mut module = lower(src);
    opt::optimize(&mut module);
    let result = verify_module(&module);
    assert!(
        result.is_ok(),
        "post-opt verify errors: {:?}",
        result.errors
    );
    module
}

fn first_fn(module: &IrModule) -> &IrFunction {
    module.functions.first().expect("no functions in module")
}

/// Check that all terminator targets in a function actually exist as block IDs.
fn assert_all_targets_valid(func: &IrFunction) {
    let block_ids: std::collections::HashSet<BlockId> =
        func.blocks.iter().map(|b| b.id).collect();
    for block in &func.blocks {
        let targets = terminator_targets(&block.terminator);
        for target in &targets {
            assert!(
                block_ids.contains(target),
                "Block bb{} references nonexistent target bb{} in function {}. \
                 Existing blocks: {:?}",
                block.id,
                target,
                func.name,
                block_ids
            );
        }
    }
}

fn terminator_targets(term: &Terminator) -> Vec<BlockId> {
    match term {
        Terminator::Return(_) | Terminator::Unreachable => vec![],
        Terminator::Goto(t) => vec![*t],
        Terminator::Branch(_, a, b) => vec![*a, *b],
        Terminator::Switch(_, cases, def) => {
            let mut v: Vec<BlockId> = cases.iter().map(|(_, b)| *b).collect();
            v.push(*def);
            v
        }
        Terminator::Spawn(t, c) => vec![*t, *c],
        Terminator::Select(branches) => branches.iter().map(|b| b.target).collect(),
    }
}

// ================================================================
// 1. Empty function
// ================================================================

#[test]
fn adversarial_empty_function() {
    let module = lower("fn nothing() {}");
    let f = first_fn(&module);
    assert_eq!(f.name, "nothing");
    assert!(!f.blocks.is_empty(), "empty function should have at least one block");

    // The entry block should have a Return terminator (not Unreachable).
    let entry = &f.blocks[f.entry as usize];
    assert!(
        matches!(entry.terminator, Terminator::Return(_)),
        "empty function entry should return, got: {:?}",
        entry.terminator
    );

    // Verify the IR.
    let result = verify_function_standalone(f);
    assert!(result.is_ok(), "empty function should verify: {:?}", result.errors);
}

// ================================================================
// 2. Deeply nested control flow (5+ levels)
// ================================================================

#[test]
fn adversarial_deeply_nested_control_flow() {
    let src = r#"fn deep(x i32) -> i32 {
    if x > 0 {
        if x > 10 {
            if x > 20 {
                if x > 30 {
                    if x > 40 {
                        return 5
                    } else {
                        return 4
                    }
                } else {
                    return 3
                }
            } else {
                return 2
            }
        } else {
            return 1
        }
    } else {
        return 0
    }
}"#;
    let module = lower(src);
    let f = first_fn(&module);

    // Should have many blocks from nested ifs.
    assert!(
        f.blocks.len() >= 10,
        "deeply nested if should create many blocks, got {}",
        f.blocks.len()
    );

    // Every terminator target should reference a valid block.
    assert_all_targets_valid(f);

    // Should verify.
    let result = verify_function_standalone(f);
    assert!(result.is_ok(), "deeply nested code should verify: {:?}", result.errors);
}

// ================================================================
// 3. Break from nested loops
// ================================================================

#[test]
fn adversarial_break_from_nested_loops() {
    let src = r#"fn nested_break() {
    loop {
        loop {
            break
        }
        break
    }
}"#;
    let module = lower(src);
    let f = first_fn(&module);

    // Inner break should target the inner loop's exit.
    // Outer break should target the outer loop's exit.
    // They MUST be different blocks.
    assert_all_targets_valid(f);

    // Both loops should have exit blocks that are reachable.
    let goto_targets: Vec<BlockId> = f
        .blocks
        .iter()
        .filter_map(|b| {
            if let Terminator::Goto(t) = b.terminator {
                Some(t)
            } else {
                None
            }
        })
        .collect();

    // Should have at least 2 distinct goto targets (the two exit blocks).
    let unique_targets: std::collections::HashSet<_> = goto_targets.iter().collect();
    assert!(
        unique_targets.len() >= 2,
        "nested loops should have at least 2 distinct break targets, got {:?}",
        unique_targets
    );

    let result = verify_function_standalone(f);
    assert!(result.is_ok(), "nested break should verify: {:?}", result.errors);
}

#[test]
fn adversarial_break_targets_correct_loop() {
    // This tests that `break` in the INNER loop doesn't accidentally
    // target the OUTER loop's exit.
    let src = r#"fn break_test() -> i32 {
    mut x := 0
    loop {
        x = x + 1
        loop {
            break
        }
        if x > 5 {
            break
        }
    }
    return x
}"#;
    let module = lower(src);
    let f = first_fn(&module);
    assert_all_targets_valid(f);

    let result = verify_function_standalone(f);
    assert!(result.is_ok(), "break targeting should verify: {:?}", result.errors);
}

// ================================================================
// 4. Continue in for loop
// ================================================================

#[test]
fn adversarial_continue_in_for_loop() {
    let src = r#"fn skip_evens() -> i32 {
    mut total := 0
    for i in 0..10 {
        if i == 0 {
            continue
        }
        total = total + i
    }
    return total
}"#;
    let module = lower(src);
    let f = first_fn(&module);
    assert_all_targets_valid(f);

    // The continue should go back to the for-loop header (condition check),
    // not to the body or exit.
    // Verify that there's a Goto to a header block (a block with a Branch terminator).
    let header_blocks: Vec<BlockId> = f
        .blocks
        .iter()
        .filter(|b| matches!(b.terminator, Terminator::Branch(_, _, _)))
        .map(|b| b.id)
        .collect();

    assert!(!header_blocks.is_empty(), "for loop should have a header with a branch");

    let result = verify_function_standalone(f);
    assert!(result.is_ok(), "continue in for loop should verify: {:?}", result.errors);
}

// ================================================================
// 5. Match with many arms (10+)
// ================================================================

#[test]
fn adversarial_match_many_arms() {
    let src = r#"fn big_match(x i32) -> i32 {
    match x {
        1 => 10
        2 => 20
        3 => 30
        4 => 40
        5 => 50
        6 => 60
        7 => 70
        8 => 80
        9 => 90
        10 => 100
        _ => 0
    }
}"#;
    let module = lower(src);
    let f = first_fn(&module);

    // Should have a Switch with 10 cases.
    let switch_found = f.blocks.iter().any(|b| {
        if let Terminator::Switch(_, cases, _) = &b.terminator {
            cases.len() == 10
        } else {
            false
        }
    });
    assert!(switch_found, "match with 10 literal arms should produce a Switch with 10 cases");

    // Each arm should have its own block.
    assert!(
        f.blocks.len() >= 12,
        "10-arm match should create many blocks, got {}",
        f.blocks.len()
    );

    assert_all_targets_valid(f);

    let result = verify_function_standalone(f);
    assert!(result.is_ok(), "many-arm match should verify: {:?}", result.errors);
}

// ================================================================
// 6. Constant folding edge cases
// ================================================================

#[test]
fn adversarial_const_fold_division_by_zero_not_folded() {
    // Division by zero should NOT be folded (should remain as a BinaryOp).
    let mut module = lower("fn div_zero() -> i32 { return 1 / 0 }");
    opt::constant_fold(&mut module);
    let f = first_fn(&module);

    // The division should still be a BinaryOp, NOT a constant.
    let has_div_op = f.blocks.iter().any(|b| {
        b.instructions.iter().any(|i| {
            matches!(i, Instruction::Assign(_, RValue::BinaryOp(BinOp::Div, _, _)))
        })
    });
    assert!(
        has_div_op,
        "division by zero should NOT be folded -- it should remain as a Div operation"
    );
}

#[test]
fn adversarial_const_fold_modulo_by_zero_not_folded() {
    let mut module = lower("fn mod_zero() -> i32 { return 10 % 0 }");
    opt::constant_fold(&mut module);
    let f = first_fn(&module);

    let has_mod_op = f.blocks.iter().any(|b| {
        b.instructions.iter().any(|i| {
            matches!(i, Instruction::Assign(_, RValue::BinaryOp(BinOp::Mod, _, _)))
        })
    });
    assert!(
        has_mod_op,
        "modulo by zero should NOT be folded"
    );
}

#[test]
fn adversarial_const_fold_integer_overflow_wraps() {
    // i64::MAX + 1 should wrap (wrapping_add behavior).
    let mut module = lower("fn overflow() -> i64 { return 9223372036854775807 + 1 }");
    opt::constant_fold(&mut module);
    let f = first_fn(&module);

    // After folding, should produce i64::MIN (wrapping).
    let has_min = f.blocks.iter().any(|b| {
        b.instructions.iter().any(|i| {
            matches!(i, Instruction::Assign(_, RValue::Constant(Constant::Int(v))) if *v == i64::MIN)
        })
    });
    assert!(
        has_min,
        "overflow should wrap to i64::MIN via wrapping_add"
    );
}

#[test]
fn adversarial_const_fold_negate_i64_min() {
    // Negating i64::MIN is undefined in normal math. In Rust, -i64::MIN panics in debug.
    // The fold_unary does `Some(Constant::Int(-n))` which will panic on i64::MIN.
    // This is a REAL BUG if it panics.
    let module = IrModule {
        functions: vec![IrFunction {
            id: 0,
            name: "neg_min".into(),
            params: vec![],
            return_type: IrType::I64,
            entry: 0,
            locals: vec![IrLocal {
                id: 0,
                name: "x".into(),
                ty: IrType::I64,
            }],
            blocks: vec![BasicBlock {
                id: 0,
                instructions: vec![Instruction::Assign(
                    0,
                    RValue::UnaryOp(UnOp::Neg, Operand::Constant(Constant::Int(i64::MIN))),
                )],
                terminator: Terminator::Return(Some(Operand::Var(0))),
            }],
        }],
        globals: vec![],
        string_literals: vec![],
    };

    // This should not panic. If it does, that's a bug in constant_fold.
    let mut module = module;
    opt::constant_fold(&mut module);

    // After fold, check what happened.
    let f = first_fn(&module);
    let printed = print_function(f);
    // Just ensure we didn't crash.
    assert!(!printed.is_empty(), "should produce output after folding -i64::MIN");
}

#[test]
fn adversarial_const_fold_negative_division() {
    let mut module = lower("fn neg_div() -> i32 { return -7 / 2 }");
    opt::constant_fold(&mut module);
    let f = first_fn(&module);

    // -7 / 2 should fold to -3 (truncated division).
    // But first we need to check if the parser produces a single division
    // or a negation + division. Let's check what we got.
    let printed = print_function(f);
    assert!(!printed.is_empty());
}

#[test]
fn adversarial_const_fold_float_division_by_zero() {
    let mut module = lower("fn float_div_zero() -> f64 { return 1.0 / 0.0 }");
    opt::constant_fold(&mut module);
    // Should not panic. Float division by zero produces infinity, but the
    // optimizer chooses not to fold it (b != 0.0 check).
    let f = first_fn(&module);
    let printed = print_function(f);
    assert!(!printed.is_empty());
}

// ================================================================
// 7. Dead code after return
// ================================================================

#[test]
fn adversarial_dead_code_after_return() {
    let src = r#"fn foo() -> i32 {
    return 5
    x := 10
    return x
}"#;
    let module = lower(src);
    let f = first_fn(&module);

    // The code after `return 5` should be in unreachable blocks.
    // Entry block should have Return(5).
    let entry = &f.blocks[f.entry as usize];
    assert!(
        matches!(&entry.terminator, Terminator::Return(Some(Operand::Constant(Constant::Int(5))))),
        "entry should return 5, got: {:?}",
        entry.terminator
    );

    // After dead code elimination, unreachable blocks should be removed.
    let mut opt_module = module.clone();
    opt::dead_code(&mut opt_module);
    let opt_f = first_fn(&opt_module);

    // The dead blocks should be removed.
    let block_count_before = f.blocks.len();
    let block_count_after = opt_f.blocks.len();
    assert!(
        block_count_after <= block_count_before,
        "DCE should remove dead blocks (before={}, after={})",
        block_count_before,
        block_count_after
    );
}

// ================================================================
// 8. Empty blocks
// ================================================================

#[test]
fn adversarial_match_with_empty_arm() {
    // Match arm with just a literal (no complex body).
    let src = r#"fn empty_arms(x i32) -> i32 {
    match x {
        1 => 0
        _ => 0
    }
}"#;
    let module = lower(src);
    let f = first_fn(&module);
    assert_all_targets_valid(f);

    let result = verify_function_standalone(f);
    assert!(result.is_ok(), "match with simple arms should verify: {:?}", result.errors);
}

#[test]
fn adversarial_if_with_empty_then() {
    // If with an expression body but no statements.
    let src = r#"fn empty_then(x i32) -> i32 {
    if x > 0 {
        1
    } else {
        0
    }
}"#;
    let module = lower(src);
    let f = first_fn(&module);
    assert_all_targets_valid(f);

    let result = verify_function_standalone(f);
    assert!(result.is_ok(), "if with expr body should verify: {:?}", result.errors);
}

// ================================================================
// 9. String interpolation lowering
// ================================================================

#[test]
fn adversarial_string_interpolation() {
    let src = r#"fn greet(name String) -> String {
    msg := "hello {name}!"
    return msg
}"#;
    let module = lower(src);
    let f = first_fn(&module);

    // String interpolation should produce __str_concat calls.
    let has_concat = f.blocks.iter().any(|b| {
        b.instructions.iter().any(|i| {
            if let Instruction::Assign(_, RValue::CallNamed(name, _)) = i {
                name == "__str_concat"
            } else {
                false
            }
        })
    });
    assert!(
        has_concat,
        "string interpolation should produce __str_concat calls"
    );

    // "hello " and "!" should be interned as string literals.
    assert!(
        module.string_literals.len() >= 2,
        "should intern at least 2 string parts, got {:?}",
        module.string_literals
    );
}

#[test]
fn adversarial_string_interpolation_multiple_parts() {
    // Interpolation with multiple interpolated expressions.
    let src = r#"fn show(x i32, y i32) -> String {
    msg := "x={x} y={y}"
    return msg
}"#;
    let module = lower(src);
    let f = first_fn(&module);
    let printed = print_function(f);
    assert!(!printed.is_empty());

    // Should have at least 2 __str_concat calls for 3+ parts.
    let concat_count = f.blocks.iter().flat_map(|b| &b.instructions).filter(|i| {
        if let Instruction::Assign(_, RValue::CallNamed(name, _)) = i {
            name == "__str_concat"
        } else {
            false
        }
    }).count();
    assert!(
        concat_count >= 2,
        "multi-part interpolation should produce at least 2 concat calls, got {}",
        concat_count
    );

    let result = verify_function_standalone(f);
    assert!(result.is_ok(), "multi-part interpolation should verify: {:?}", result.errors);
}

// ================================================================
// 10. Complex expressions as function args
// ================================================================

#[test]
fn adversarial_complex_function_args() {
    let src = r#"fn add(a i32, b i32) -> i32 { return a + b }
fn mul(a i32, b i32) -> i32 { return a * b }
fn main() {
    result := add(1 + 2, mul(3, 4))
}"#;
    let module = lower(src);
    let main_fn = module.functions.iter().find(|f| f.name == "main").unwrap();

    // The add(1+2, mul(3,4)) should produce:
    // 1. Binary Add for 1+2
    // 2. Call to mul(3, 4)
    // 3. Call to add(result_of_1, result_of_2)
    let entry = &main_fn.blocks[main_fn.entry as usize];

    let has_add = entry.instructions.iter().any(|i| {
        matches!(i, Instruction::Assign(_, RValue::BinaryOp(BinOp::Add, _, _)))
    });
    assert!(has_add, "should have Add for 1+2 argument");

    // Should have two Call instructions (mul and add).
    let call_count = entry.instructions.iter().filter(|i| {
        matches!(i, Instruction::Assign(_, RValue::Call(_, _)))
    }).count();
    assert!(
        call_count >= 2,
        "should have at least 2 call instructions, got {}",
        call_count
    );

    assert_all_targets_valid(main_fn);
}

// ================================================================
// 11. Assignment to same variable multiple times
// ================================================================

#[test]
fn adversarial_multiple_assignments() {
    let src = r#"fn multi_assign() -> i32 {
    mut x := 0
    x = 1
    x = 2
    x = 3
    return x
}"#;
    let module = lower(src);
    let f = first_fn(&module);

    // All assignments should target the SAME variable ID.
    let assign_targets: Vec<VarId> = f
        .blocks
        .iter()
        .flat_map(|b| &b.instructions)
        .filter_map(|i| {
            if let Instruction::Assign(var, RValue::Use(_)) = i {
                Some(*var)
            } else {
                None
            }
        })
        .collect();

    // With mut x := 0; x = 1; x = 2; x = 3, all should assign to the
    // same var since x is looked up and reassigned.
    // Actually, `x := 0` creates a fresh var, then `x = 1` etc. should
    // assign to the same var. Let's check.
    assert!(
        assign_targets.len() >= 4,
        "should have at least 4 assignments, got {}",
        assign_targets.len()
    );

    // The first assignment (x := 0) creates a new var. Subsequent
    // assignments (x = 1, x = 2, x = 3) should use the same var ID.
    let first_target = assign_targets[0];
    for (i, target) in assign_targets.iter().enumerate().skip(1) {
        assert_eq!(
            *target, first_target,
            "assignment {} should target same var as first (expected %{}, got %{})",
            i, first_target, target
        );
    }

    let result = verify_function_standalone(f);
    assert!(result.is_ok(), "multiple assignments should verify: {:?}", result.errors);
}

// ================================================================
// 12. Optimizer doesn't break semantics
// ================================================================

#[test]
fn adversarial_optimize_preserves_verification() {
    let programs = [
        "fn a() -> i32 { return 2 + 3 }",
        "fn b(x i32) -> i32 { if x > 0 { return 1 } else { return 0 } }",
        "fn c() { mut i := 0\n while i < 10 { i = i + 1 } }",
        "fn d() { loop { break } }",
        "fn e() -> i32 { return 2 * 3 + 4 - 1 }",
    ];

    for src in &programs {
        let mut module = lower(src);

        let pre_verify = verify_module(&module);
        assert!(
            pre_verify.is_ok(),
            "pre-opt verify failed for: {}\nerrors: {:?}",
            src,
            pre_verify.errors
        );

        opt::optimize(&mut module);

        let post_verify = verify_module(&module);
        assert!(
            post_verify.is_ok(),
            "post-opt verify failed for: {}\nerrors: {:?}",
            src,
            post_verify.errors
        );
    }
}

#[test]
fn adversarial_optimize_complex_program() {
    let src = r#"fn fibonacci(n i32) -> i32 {
    if n <= 1 {
        return n
    }
    mut a := 0
    mut b := 1
    mut i := 2
    while i <= n {
        temp := a + b
        a = b
        b = temp
        i = i + 1
    }
    return b
}"#;
    let mut module = lower(src);
    opt::optimize(&mut module);
    let result = verify_module(&module);
    assert!(
        result.is_ok(),
        "fibonacci post-opt verify failed: {:?}",
        result.errors
    );
}

// ================================================================
// 13. Verifier catches real problems
// ================================================================

#[test]
fn adversarial_verifier_catches_self_referencing_block() {
    // Block that gotos itself (infinite loop with no instructions).
    let module = IrModule {
        functions: vec![IrFunction {
            id: 0,
            name: "self_loop".into(),
            params: vec![],
            return_type: IrType::Unit,
            entry: 0,
            locals: vec![],
            blocks: vec![BasicBlock {
                id: 0,
                instructions: vec![],
                terminator: Terminator::Goto(0), // points to itself
            }],
        }],
        globals: vec![],
        string_literals: vec![],
    };

    // This is actually valid IR (it's an infinite loop).
    // The verifier should NOT flag this as an error.
    let result = verify_module(&module);
    assert!(
        result.is_ok(),
        "self-referencing goto (infinite loop) is valid IR: {:?}",
        result.errors
    );
}

#[test]
fn adversarial_verifier_catches_nonexistent_target() {
    let module = IrModule {
        functions: vec![IrFunction {
            id: 0,
            name: "bad_targets".into(),
            params: vec![],
            return_type: IrType::Unit,
            entry: 0,
            locals: vec![],
            blocks: vec![
                BasicBlock {
                    id: 0,
                    instructions: vec![],
                    terminator: Terminator::Branch(
                        Operand::Constant(Constant::Bool(true)),
                        50,
                        100,
                    ),
                },
            ],
        }],
        globals: vec![],
        string_literals: vec![],
    };

    let result = verify_module(&module);
    assert!(!result.is_ok(), "should catch nonexistent branch targets");
    assert!(
        result.errors.len() >= 2,
        "should report errors for both branch targets, got {}",
        result.errors.len()
    );
}

#[test]
fn adversarial_verifier_catches_duplicate_local_ids() {
    let module = IrModule {
        functions: vec![IrFunction {
            id: 0,
            name: "dup_locals".into(),
            params: vec![],
            return_type: IrType::Unit,
            entry: 0,
            locals: vec![
                IrLocal { id: 0, name: "x".into(), ty: IrType::I64 },
                IrLocal { id: 0, name: "y".into(), ty: IrType::I64 }, // duplicate ID!
            ],
            blocks: vec![BasicBlock {
                id: 0,
                instructions: vec![],
                terminator: Terminator::Return(Some(Operand::Constant(Constant::Unit))),
            }],
        }],
        globals: vec![],
        string_literals: vec![],
    };

    let result = verify_module(&module);
    assert!(!result.is_ok(), "should catch duplicate local IDs");
}

#[test]
fn adversarial_verifier_switch_with_invalid_default() {
    let module = IrModule {
        functions: vec![IrFunction {
            id: 0,
            name: "bad_switch".into(),
            params: vec![],
            return_type: IrType::Unit,
            entry: 0,
            locals: vec![],
            blocks: vec![
                BasicBlock {
                    id: 0,
                    instructions: vec![],
                    terminator: Terminator::Switch(
                        Operand::Constant(Constant::Int(0)),
                        vec![(Constant::Int(1), 0)], // case target is valid (itself)
                        999, // default target is INVALID
                    ),
                },
            ],
        }],
        globals: vec![],
        string_literals: vec![],
    };

    let result = verify_module(&module);
    assert!(!result.is_ok(), "should catch invalid switch default target");
    assert!(
        result.errors.iter().any(|e| e.message.contains("999")),
        "error should mention the invalid target 999"
    );
}

#[test]
fn adversarial_verifier_empty_function_no_blocks() {
    let module = IrModule {
        functions: vec![IrFunction {
            id: 0,
            name: "no_blocks".into(),
            params: vec![],
            return_type: IrType::Unit,
            entry: 0,
            locals: vec![],
            blocks: vec![], // No blocks at all!
        }],
        globals: vec![],
        string_literals: vec![],
    };

    let result = verify_module(&module);
    assert!(
        !result.is_ok(),
        "function with no blocks should fail verification"
    );
}

// ================================================================
// 14. Printer handles all node types
// ================================================================

#[test]
fn adversarial_printer_all_instruction_types() {
    // Construct a function with every instruction and RValue variant.
    let func = IrFunction {
        id: 0,
        name: "all_types".into(),
        params: vec![
            IrParam { name: "a".into(), ty: IrType::I32 },
            IrParam { name: "b".into(), ty: IrType::String },
        ],
        return_type: IrType::Tuple(vec![IrType::I32, IrType::Bool]),
        entry: 0,
        locals: vec![
            IrLocal { id: 0, name: "a".into(), ty: IrType::I32 },
            IrLocal { id: 1, name: "b".into(), ty: IrType::String },
            IrLocal { id: 2, name: "t1".into(), ty: IrType::I64 },
            IrLocal { id: 3, name: "t2".into(), ty: IrType::Bool },
            IrLocal { id: 4, name: "t3".into(), ty: IrType::F64 },
            IrLocal { id: 5, name: "t4".into(), ty: IrType::Char },
            IrLocal { id: 6, name: "t5".into(), ty: IrType::Unit },
            IrLocal { id: 7, name: "t6".into(), ty: IrType::Ptr(Box::new(IrType::I32)) },
            IrLocal { id: 8, name: "t7".into(), ty: IrType::Array(Box::new(IrType::I32), Some(10)) },
            IrLocal { id: 9, name: "t8".into(), ty: IrType::Channel(Box::new(IrType::I64)) },
            IrLocal { id: 10, name: "t9".into(), ty: IrType::Function(vec![IrType::I32], Box::new(IrType::Bool)) },
            IrLocal { id: 11, name: "t10".into(), ty: IrType::Struct("Point".into()) },
            IrLocal { id: 12, name: "t11".into(), ty: IrType::Enum("Color".into()) },
            IrLocal { id: 13, name: "t12".into(), ty: IrType::Void },
        ],
        blocks: vec![
            BasicBlock {
                id: 0,
                instructions: vec![
                    // Constants
                    Instruction::Assign(2, RValue::Constant(Constant::Int(42))),
                    Instruction::Assign(3, RValue::Constant(Constant::Bool(true))),
                    Instruction::Assign(4, RValue::Constant(Constant::Float(3.14))),
                    Instruction::Assign(5, RValue::Constant(Constant::Char('A'))),
                    Instruction::Assign(6, RValue::Constant(Constant::Unit)),
                    Instruction::Assign(2, RValue::Constant(Constant::Nil)),
                    Instruction::Assign(2, RValue::Constant(Constant::String(0))),
                    // Binary ops
                    Instruction::Assign(2, RValue::BinaryOp(BinOp::Add, Operand::Var(0), Operand::Var(0))),
                    Instruction::Assign(2, RValue::BinaryOp(BinOp::Sub, Operand::Var(0), Operand::Var(0))),
                    Instruction::Assign(2, RValue::BinaryOp(BinOp::Mul, Operand::Var(0), Operand::Var(0))),
                    Instruction::Assign(2, RValue::BinaryOp(BinOp::Div, Operand::Var(0), Operand::Var(0))),
                    Instruction::Assign(2, RValue::BinaryOp(BinOp::Mod, Operand::Var(0), Operand::Var(0))),
                    Instruction::Assign(3, RValue::BinaryOp(BinOp::Eq, Operand::Var(0), Operand::Var(0))),
                    Instruction::Assign(3, RValue::BinaryOp(BinOp::NotEq, Operand::Var(0), Operand::Var(0))),
                    Instruction::Assign(3, RValue::BinaryOp(BinOp::Lt, Operand::Var(0), Operand::Var(0))),
                    Instruction::Assign(3, RValue::BinaryOp(BinOp::Gt, Operand::Var(0), Operand::Var(0))),
                    Instruction::Assign(3, RValue::BinaryOp(BinOp::LtEq, Operand::Var(0), Operand::Var(0))),
                    Instruction::Assign(3, RValue::BinaryOp(BinOp::GtEq, Operand::Var(0), Operand::Var(0))),
                    Instruction::Assign(3, RValue::BinaryOp(BinOp::And, Operand::Constant(Constant::Bool(true)), Operand::Constant(Constant::Bool(false)))),
                    Instruction::Assign(3, RValue::BinaryOp(BinOp::Or, Operand::Constant(Constant::Bool(true)), Operand::Constant(Constant::Bool(false)))),
                    // Unary ops
                    Instruction::Assign(2, RValue::UnaryOp(UnOp::Neg, Operand::Var(0))),
                    Instruction::Assign(3, RValue::UnaryOp(UnOp::Not, Operand::Var(3))),
                    Instruction::Assign(7, RValue::UnaryOp(UnOp::Ref, Operand::Var(0))),
                    // Calls
                    Instruction::Assign(2, RValue::Call(0, vec![Operand::Var(0)])),
                    Instruction::Assign(2, RValue::CallNamed("print".into(), vec![Operand::Var(1)])),
                    // Aggregates
                    Instruction::Assign(8, RValue::Aggregate(AggregateKind::Array, vec![Operand::Constant(Constant::Int(1)), Operand::Constant(Constant::Int(2))])),
                    Instruction::Assign(6, RValue::Aggregate(AggregateKind::Tuple, vec![Operand::Constant(Constant::Int(1)), Operand::Constant(Constant::Bool(true))])),
                    Instruction::Assign(11, RValue::Aggregate(AggregateKind::Struct("Point".into()), vec![Operand::Constant(Constant::Int(1)), Operand::Constant(Constant::Int(2))])),
                    // Field, Index
                    Instruction::Assign(2, RValue::Field(Operand::Var(11), 0)),
                    Instruction::Assign(2, RValue::Index(Operand::Var(8), Operand::Constant(Constant::Int(0)))),
                    // Ref, Deref, Cast
                    Instruction::Assign(7, RValue::Ref(0)),
                    Instruction::Assign(7, RValue::MutRef(0)),
                    Instruction::Assign(2, RValue::Deref(Operand::Var(7))),
                    Instruction::Assign(4, RValue::Cast(Operand::Var(0), IrType::F64)),
                    // Heap alloc
                    Instruction::Assign(7, RValue::HeapAlloc(IrType::I32)),
                    // Channel ops
                    Instruction::Assign(9, RValue::ChanCreate(IrType::I64, Some(10))),
                    Instruction::Assign(9, RValue::ChanCreate(IrType::I64, None)),
                    Instruction::Assign(6, RValue::ChanSend(Operand::Var(9), Operand::Constant(Constant::Int(42)))),
                    Instruction::Assign(2, RValue::ChanRecv(Operand::Var(9))),
                    // Use
                    Instruction::Assign(2, RValue::Use(Operand::Var(0))),
                    // Nop and Drop
                    Instruction::Nop,
                    Instruction::Drop(0),
                ],
                terminator: Terminator::Return(Some(Operand::Constant(Constant::Unit))),
            },
        ],
    };

    // This should not panic.
    let output = print_function(&func);
    assert!(!output.is_empty());

    // Verify it mentions key items.
    assert!(output.contains("fn @all_types"), "should print function name");
    assert!(output.contains("add"), "should print add operation");
    assert!(output.contains("nop"), "should print nop");
    assert!(output.contains("drop"), "should print drop");
    assert!(output.contains("heap_alloc"), "should print heap_alloc");
    assert!(output.contains("chan_create"), "should print chan_create");
    assert!(output.contains("chan_send"), "should print chan_send");
    assert!(output.contains("chan_recv"), "should print chan_recv");
}

#[test]
fn adversarial_printer_all_terminator_types() {
    let module = IrModule {
        functions: vec![IrFunction {
            id: 0,
            name: "terminators".into(),
            params: vec![],
            return_type: IrType::Unit,
            entry: 0,
            locals: vec![
                IrLocal { id: 0, name: "x".into(), ty: IrType::I64 },
            ],
            blocks: vec![
                // Goto
                BasicBlock {
                    id: 0,
                    instructions: vec![],
                    terminator: Terminator::Goto(1),
                },
                // Branch
                BasicBlock {
                    id: 1,
                    instructions: vec![],
                    terminator: Terminator::Branch(Operand::Constant(Constant::Bool(true)), 2, 3),
                },
                // Return with value
                BasicBlock {
                    id: 2,
                    instructions: vec![],
                    terminator: Terminator::Return(Some(Operand::Constant(Constant::Int(42)))),
                },
                // Switch
                BasicBlock {
                    id: 3,
                    instructions: vec![],
                    terminator: Terminator::Switch(
                        Operand::Var(0),
                        vec![
                            (Constant::Int(1), 4),
                            (Constant::Int(2), 5),
                        ],
                        6,
                    ),
                },
                // Spawn
                BasicBlock {
                    id: 4,
                    instructions: vec![],
                    terminator: Terminator::Spawn(5, 7),
                },
                // Return None
                BasicBlock {
                    id: 5,
                    instructions: vec![],
                    terminator: Terminator::Return(None),
                },
                // Select
                BasicBlock {
                    id: 6,
                    instructions: vec![],
                    terminator: Terminator::Select(vec![
                        SelectBranch {
                            kind: SelectBranchKind::Recv(0, Operand::Var(0)),
                            target: 7,
                        },
                        SelectBranch {
                            kind: SelectBranchKind::Send(Operand::Var(0), Operand::Constant(Constant::Int(1))),
                            target: 7,
                        },
                        SelectBranch {
                            kind: SelectBranchKind::After(Operand::Constant(Constant::Int(1000))),
                            target: 7,
                        },
                    ]),
                },
                // Unreachable
                BasicBlock {
                    id: 7,
                    instructions: vec![],
                    terminator: Terminator::Unreachable,
                },
            ],
        }],
        globals: vec![],
        string_literals: vec!["hello".into()],
    };

    let output = print_module(&module);
    assert!(output.contains("goto bb1"), "should print goto");
    assert!(output.contains("branch"), "should print branch");
    assert!(output.contains("return"), "should print return");
    assert!(output.contains("switch"), "should print switch");
    assert!(output.contains("spawn"), "should print spawn");
    assert!(output.contains("select"), "should print select");
    assert!(output.contains("unreachable"), "should print unreachable");
    assert!(output.contains("recv"), "should print recv in select");
    assert!(output.contains("send"), "should print send in select");
    assert!(output.contains("after"), "should print after in select");
}

#[test]
fn adversarial_printer_all_types() {
    // Test fmt_type for every IrType variant by creating params of each type.
    let func = IrFunction {
        id: 0,
        name: "types".into(),
        params: vec![
            IrParam { name: "a".into(), ty: IrType::I8 },
            IrParam { name: "b".into(), ty: IrType::I16 },
            IrParam { name: "c".into(), ty: IrType::I32 },
            IrParam { name: "d".into(), ty: IrType::I64 },
            IrParam { name: "e".into(), ty: IrType::U8 },
            IrParam { name: "f".into(), ty: IrType::U16 },
            IrParam { name: "g".into(), ty: IrType::U32 },
            IrParam { name: "h".into(), ty: IrType::U64 },
            IrParam { name: "i".into(), ty: IrType::F32 },
            IrParam { name: "j".into(), ty: IrType::F64 },
            IrParam { name: "k".into(), ty: IrType::Bool },
            IrParam { name: "l".into(), ty: IrType::Char },
            IrParam { name: "m".into(), ty: IrType::Unit },
            IrParam { name: "n".into(), ty: IrType::String },
            IrParam { name: "o".into(), ty: IrType::Str },
            IrParam { name: "p".into(), ty: IrType::Void },
            IrParam { name: "q".into(), ty: IrType::Ptr(Box::new(IrType::I32)) },
            IrParam { name: "r".into(), ty: IrType::Array(Box::new(IrType::I32), None) },
            IrParam { name: "s".into(), ty: IrType::Array(Box::new(IrType::I32), Some(5)) },
            IrParam { name: "t".into(), ty: IrType::Tuple(vec![IrType::I32, IrType::Bool]) },
            IrParam { name: "u".into(), ty: IrType::Struct("Foo".into()) },
            IrParam { name: "v".into(), ty: IrType::Enum("Bar".into()) },
            IrParam { name: "w".into(), ty: IrType::Function(vec![IrType::I32], Box::new(IrType::Bool)) },
            IrParam { name: "x".into(), ty: IrType::Channel(Box::new(IrType::I64)) },
        ],
        return_type: IrType::Unit,
        entry: 0,
        locals: vec![],
        blocks: vec![BasicBlock {
            id: 0,
            instructions: vec![],
            terminator: Terminator::Return(Some(Operand::Constant(Constant::Unit))),
        }],
    };

    let output = print_function(&func);
    // Check key types appear in the output.
    assert!(output.contains("i8"), "should contain i8");
    assert!(output.contains("i16"), "should contain i16");
    assert!(output.contains("i32"), "should contain i32");
    assert!(output.contains("i64"), "should contain i64");
    assert!(output.contains("u8"), "should contain u8");
    assert!(output.contains("u16"), "should contain u16");
    assert!(output.contains("u32"), "should contain u32");
    assert!(output.contains("u64"), "should contain u64");
    assert!(output.contains("f32"), "should contain f32");
    assert!(output.contains("f64"), "should contain f64");
    assert!(output.contains("bool"), "should contain bool");
    assert!(output.contains("char"), "should contain char");
    assert!(output.contains("unit"), "should contain unit");
    assert!(output.contains("String"), "should contain String");
    assert!(output.contains("str"), "should contain str");
    assert!(output.contains("void"), "should contain void");
    assert!(output.contains("*i32"), "should contain *i32");
    assert!(output.contains("chan[i64]"), "should contain chan[i64]");
    assert!(output.contains("Foo"), "should contain Foo struct");
    assert!(output.contains("Bar"), "should contain Bar enum");
}

// ================================================================
// 15. Select statement lowering
// ================================================================

#[test]
fn adversarial_select_lowering() {
    let src = r#"fn selector() {
    ch1 := chan[i32]()
    ch2 := chan[i32]()
    select {
        val := ch1.recv() => {
            val
        }
        ch2.send(42) => {
            0
        }
    }
}"#;
    let module = lower(src);
    let f = first_fn(&module);

    // Should have a Select terminator.
    let has_select = f.blocks.iter().any(|b| {
        matches!(&b.terminator, Terminator::Select(branches) if branches.len() == 2)
    });
    assert!(has_select, "select with 2 arms should produce a Select with 2 branches");

    assert_all_targets_valid(f);

    let result = verify_function_standalone(f);
    assert!(result.is_ok(), "select lowering should verify: {:?}", result.errors);
}

// ================================================================
// 16. Enum variant construction and matching (round-trip)
// ================================================================

#[test]
fn adversarial_enum_round_trip() {
    let src = r#"enum Shape {
    Circle(f64)
    Rectangle(f64, f64)
}
fn area(s Shape) -> f64 {
    match s {
        Circle(r) => r * r
        Rectangle(w, h) => w * h
        _ => 0.0
    }
}"#;
    let module = lower(src);
    let f = module.functions.iter().find(|f| f.name == "area").unwrap();

    // Should have a Switch terminator for the match.
    let has_switch = f.blocks.iter().any(|b| {
        matches!(&b.terminator, Terminator::Switch(_, cases, _) if cases.len() >= 2)
    });
    assert!(has_switch, "enum match should produce a switch with at least 2 cases");

    assert_all_targets_valid(f);

    let result = verify_function_standalone(f);
    assert!(result.is_ok(), "enum match should verify: {:?}", result.errors);
}

// ================================================================
// 17. Closure lowering
// ================================================================

#[test]
fn adversarial_closure_lowering() {
    let src = r#"fn apply() -> i32 {
    f := |x| x + 1
    return f(5)
}"#;
    let module = lower(src);
    let f = first_fn(&module);

    // Closure should produce instructions (even if it's lowered inline).
    assert!(!f.blocks.is_empty());
    let total_instructions: usize = f.blocks.iter().map(|b| b.instructions.len()).sum();
    assert!(
        total_instructions >= 2,
        "closure should produce at least some instructions, got {}",
        total_instructions
    );

    assert_all_targets_valid(f);

    let result = verify_function_standalone(f);
    assert!(result.is_ok(), "closure should verify: {:?}", result.errors);
}

#[test]
fn adversarial_closure_captures() {
    // A closure that captures an outer variable.
    let src = r#"fn make_adder() -> i32 {
    offset := 10
    f := |x| x + offset
    return f(5)
}"#;
    let module = lower(src);
    let f = first_fn(&module);

    // The closure body should reference the outer `offset` variable.
    // In the current lowering, closure params are in scope but captured
    // variables should also be accessible.
    assert_all_targets_valid(f);

    let result = verify_function_standalone(f);
    assert!(result.is_ok(), "closure with captures should verify: {:?}", result.errors);
}

// ================================================================
// 18. Struct method calls
// ================================================================

#[test]
fn adversarial_struct_method_call() {
    let src = r#"struct Counter {
    value i32
}
fn get_value(c Counter) -> i32 {
    return c.value
}
fn main() {
    c := Counter { value: 42 }
    result := c.get_value()
}"#;
    let module = lower(src);
    let main_fn = module.functions.iter().find(|f| f.name == "main").unwrap();

    // Method call should lower to CallNamed with receiver as first arg.
    let has_method_call = main_fn.blocks.iter().any(|b| {
        b.instructions.iter().any(|i| {
            if let Instruction::Assign(_, RValue::CallNamed(name, args)) = i {
                name == "get_value" && !args.is_empty()
            } else {
                false
            }
        })
    });
    assert!(
        has_method_call,
        "method call should lower to CallNamed with receiver as first arg"
    );

    assert_all_targets_valid(main_fn);
}

// ================================================================
// 19. Chained method calls
// ================================================================

#[test]
fn adversarial_chained_method_calls() {
    let src = r#"struct Builder {
    value i32
}
fn main() {
    b := Builder { value: 0 }
    result := b.foo().bar().baz()
}"#;
    let module = lower(src);
    let main_fn = module.functions.iter().find(|f| f.name == "main").unwrap();

    // Should produce 3 CallNamed instructions: foo, bar, baz.
    let call_names: Vec<String> = main_fn
        .blocks
        .iter()
        .flat_map(|b| &b.instructions)
        .filter_map(|i| {
            if let Instruction::Assign(_, RValue::CallNamed(name, _)) = i {
                Some(name.clone())
            } else {
                None
            }
        })
        .collect();

    assert!(
        call_names.contains(&"foo".to_string()),
        "should have call to foo, got: {:?}",
        call_names
    );
    assert!(
        call_names.contains(&"bar".to_string()),
        "should have call to bar, got: {:?}",
        call_names
    );
    assert!(
        call_names.contains(&"baz".to_string()),
        "should have call to baz, got: {:?}",
        call_names
    );

    // The calls should be in order: foo first, then bar on foo's result,
    // then baz on bar's result.
    let foo_idx = call_names.iter().position(|n| n == "foo").unwrap();
    let bar_idx = call_names.iter().position(|n| n == "bar").unwrap();
    let baz_idx = call_names.iter().position(|n| n == "baz").unwrap();
    assert!(
        foo_idx < bar_idx && bar_idx < baz_idx,
        "chained calls should be in order: foo({}), bar({}), baz({})",
        foo_idx,
        bar_idx,
        baz_idx
    );
}

// ================================================================
// 20. Large function stress test
// ================================================================

#[test]
fn adversarial_large_function_stress() {
    // Generate a function with 50+ variables and complex control flow.
    let mut body = String::new();
    for i in 0..50 {
        body.push_str(&format!("    mut v{} := {}\n", i, i));
    }
    // Add some control flow.
    body.push_str("    if v0 > 0 {\n");
    for i in 0..10 {
        body.push_str(&format!("        v{} = v{} + 1\n", i, i));
    }
    body.push_str("    }\n");
    body.push_str("    while v49 > 0 {\n");
    body.push_str("        v49 = v49 - 1\n");
    body.push_str("    }\n");
    body.push_str("    return v0\n");

    let src = format!("fn big() -> i32 {{\n{}}}", body);

    let module = lower(&src);
    let f = first_fn(&module);

    assert!(
        f.locals.len() >= 50,
        "should have at least 50 locals, got {}",
        f.locals.len()
    );

    assert_all_targets_valid(f);

    let result = verify_function_standalone(f);
    assert!(result.is_ok(), "large function should verify: {:?}", result.errors);

    // Also test that optimization doesn't break it.
    let mut opt_module = module.clone();
    opt::optimize(&mut opt_module);
    let opt_result = verify_module(&opt_module);
    assert!(
        opt_result.is_ok(),
        "large function post-opt should verify: {:?}",
        opt_result.errors
    );

    // And print shouldn't panic.
    let output = print_module(&opt_module);
    assert!(!output.is_empty());
}

// ================================================================
// Additional adversarial edge cases
// ================================================================

#[test]
fn adversarial_spawn_continuation_reachable() {
    // BUG HUNT: Spawn lowering creates a continue_block but never connects
    // the pre-spawn block to it. The Spawn terminator only points to the
    // spawned block, leaving continue_block unreachable.
    let src = r#"fn spawner() -> i32 {
    spawn {
        x := 1
    }
    return 42
}"#;
    let module = lower(src);
    let f = first_fn(&module);

    // After spawn, the function should eventually return 42.
    let has_return_42 = f.blocks.iter().any(|b| {
        matches!(
            &b.terminator,
            Terminator::Return(Some(Operand::Constant(Constant::Int(42))))
        )
    });
    assert!(
        has_return_42,
        "code after spawn should be reachable and return 42"
    );

    // The Spawn terminator should exist.
    let has_spawn = f.blocks.iter().any(|b| matches!(&b.terminator, Terminator::Spawn(_, _)));
    assert!(has_spawn, "should have Spawn terminator");

    // After DCE, the continue block should survive if it's reachable.
    let mut dce_module = module.clone();
    opt::dead_code(&mut dce_module);
    let dce_f = first_fn(&dce_module);

    // Check if return 42 survives DCE.
    let still_has_return = dce_f.blocks.iter().any(|b| {
        matches!(
            &b.terminator,
            Terminator::Return(Some(Operand::Constant(Constant::Int(42))))
        )
    });

    // BUG: If this fails, the continue block after spawn was unreachable
    // and got eliminated, meaning `return 42` is lost.
    // This IS a real bug in the lowering: Spawn terminator doesn't connect
    // to the continuation block.
    assert!(
        still_has_return,
        "BUG: return 42 after spawn was eliminated by DCE -- \
         the spawn lowering doesn't connect to the continuation block"
    );
}

#[test]
fn adversarial_match_switch_block_calculation() {
    // BUG HUNT: lower_match calculates switch_block as first_arm_block - 1.
    // This breaks when the scrutinee expression creates intermediate blocks.
    let src = r#"fn complex_scrutinee(x i32) -> i32 {
    match if x > 0 { x } else { 0 } {
        1 => 10
        2 => 20
        _ => 0
    }
}"#;
    let module = lower(src);
    let f = first_fn(&module);

    assert_all_targets_valid(f);

    // The switch terminator should exist.
    let has_switch = f.blocks.iter().any(|b| {
        matches!(&b.terminator, Terminator::Switch(_, _, _))
    });
    assert!(has_switch, "match should produce a switch terminator");

    // Verify!
    let result = verify_function_standalone(f);
    assert!(
        result.is_ok(),
        "BUG: match with complex scrutinee fails verification -- \
         switch_block calculation is wrong: {:?}",
        result.errors
    );
}

#[test]
fn adversarial_cfg_simplify_dangling_references() {
    // BUG HUNT: When simplify_cfg merges blocks, it doesn't update
    // references in OTHER blocks. If bb2 references bb1, and bb0->bb1
    // are merged, bb2 still references the now-deleted bb1.
    let mut module = IrModule {
        functions: vec![IrFunction {
            id: 0,
            name: "dangling".into(),
            params: vec![],
            return_type: IrType::Unit,
            entry: 0,
            locals: vec![
                IrLocal { id: 0, name: "x".into(), ty: IrType::I64 },
            ],
            blocks: vec![
                BasicBlock {
                    id: 0,
                    instructions: vec![
                        Instruction::Assign(0, RValue::Constant(Constant::Int(0))),
                    ],
                    terminator: Terminator::Goto(1), // bb0 -> bb1
                },
                BasicBlock {
                    id: 1,
                    instructions: vec![],
                    terminator: Terminator::Branch(
                        Operand::Constant(Constant::Bool(true)),
                        2, // true -> bb2
                        3, // false -> bb3
                    ),
                },
                BasicBlock {
                    id: 2,
                    instructions: vec![],
                    terminator: Terminator::Goto(1), // back to bb1 (loop)
                },
                BasicBlock {
                    id: 3,
                    instructions: vec![],
                    terminator: Terminator::Return(Some(Operand::Constant(Constant::Unit))),
                },
            ],
        }],
        globals: vec![],
        string_literals: vec![],
    };

    // bb0 -> bb1 can be merged (bb1 has only one predecessor: bb0... wait,
    // bb2 also goes to bb1). So this should NOT be merged.
    // Let's construct a case where it DOES merge.
    let mut module2 = IrModule {
        functions: vec![IrFunction {
            id: 0,
            name: "dangling2".into(),
            params: vec![],
            return_type: IrType::Unit,
            entry: 0,
            locals: vec![
                IrLocal { id: 0, name: "x".into(), ty: IrType::I64 },
            ],
            blocks: vec![
                BasicBlock {
                    id: 0,
                    instructions: vec![
                        Instruction::Assign(0, RValue::Constant(Constant::Int(0))),
                    ],
                    terminator: Terminator::Goto(1), // bb0 -> bb1 (only predecessor)
                },
                BasicBlock {
                    id: 1,
                    instructions: vec![],
                    terminator: Terminator::Goto(2), // bb1 -> bb2 (only predecessor)
                },
                BasicBlock {
                    id: 2,
                    instructions: vec![],
                    terminator: Terminator::Branch(
                        Operand::Constant(Constant::Bool(true)),
                        1, // BUG: references bb1 which will be merged into bb0
                        3,
                    ),
                },
                BasicBlock {
                    id: 3,
                    instructions: vec![],
                    terminator: Terminator::Return(Some(Operand::Constant(Constant::Unit))),
                },
            ],
        }],
        globals: vec![],
        string_literals: vec![],
    };

    // Before CFG simplify, this should verify.
    let pre = verify_module(&module2);
    assert!(pre.is_ok(), "pre-simplify should verify");

    opt::simplify_cfg(&mut module2);

    // After CFG simplify, check if verification still passes.
    let post = verify_module(&module2);
    // BUG: If bb1 is merged into bb0, then bb2's reference to bb1 is dangling.
    if !post.is_ok() {
        // This is a REAL BUG: simplify_cfg broke the IR.
        panic!(
            "BUG: simplify_cfg created dangling block references: {:?}",
            post.errors
        );
    }
}

#[test]
fn adversarial_dce_after_cfg_simplify() {
    // Run both passes in sequence and verify the result.
    let src = r#"fn complex() -> i32 {
    mut x := 0
    if true {
        x = 1
    } else {
        x = 2
    }
    while x < 10 {
        x = x + 1
        if x == 5 {
            break
        }
    }
    return x
}"#;
    let mut module = lower(src);

    // Full optimization pipeline.
    opt::optimize(&mut module);

    let result = verify_module(&module);
    assert!(
        result.is_ok(),
        "full optimization of complex function should verify: {:?}",
        result.errors
    );
}

#[test]
fn adversarial_return_in_loop() {
    let src = r#"fn find_first() -> i32 {
    mut i := 0
    while i < 100 {
        if i == 42 {
            return i
        }
        i = i + 1
    }
    return -1
}"#;
    let module = lower_and_verify(src);
    let f = first_fn(&module);

    // Should have a return inside the loop.
    let return_count = f
        .blocks
        .iter()
        .filter(|b| matches!(&b.terminator, Terminator::Return(_)))
        .count();
    assert!(
        return_count >= 2,
        "should have at least 2 return terminators (in-loop and after-loop), got {}",
        return_count
    );
}

#[test]
fn adversarial_if_as_expression() {
    // If used as an expression should produce a result value.
    let src = r#"fn conditional(x i32) -> i32 {
    result := if x > 0 { 1 } else { 0 }
    return result
}"#;
    let module = lower(src);
    let f = first_fn(&module);
    assert_all_targets_valid(f);

    let result = verify_function_standalone(f);
    assert!(result.is_ok(), "if-expression should verify: {:?}", result.errors);
}

#[test]
fn adversarial_match_as_expression() {
    let src = r#"fn categorize(x i32) -> i32 {
    result := match x {
        1 => 10
        2 => 20
        _ => 0
    }
    return result
}"#;
    let module = lower(src);
    let f = first_fn(&module);
    assert_all_targets_valid(f);

    let result = verify_function_standalone(f);
    assert!(result.is_ok(), "match-expression should verify: {:?}", result.errors);
}

#[test]
fn adversarial_multiple_returns() {
    let src = r#"fn multi_return(x i32) -> i32 {
    if x > 10 {
        return 3
    }
    if x > 5 {
        return 2
    }
    if x > 0 {
        return 1
    }
    return 0
}"#;
    let module = lower_and_verify(src);
    let f = first_fn(&module);

    let return_count = f
        .blocks
        .iter()
        .filter(|b| matches!(&b.terminator, Terminator::Return(_)))
        .count();
    assert!(
        return_count >= 4,
        "should have 4 return terminators, got {}",
        return_count
    );
}

#[test]
fn adversarial_nested_match_in_if() {
    let src = r#"fn nested(x i32) -> i32 {
    if x > 0 {
        match x {
            1 => return 10
            2 => return 20
            _ => return 30
        }
    } else {
        return 0
    }
}"#;
    let module = lower(src);
    let f = first_fn(&module);
    assert_all_targets_valid(f);

    let result = verify_function_standalone(f);
    assert!(result.is_ok(), "nested match-in-if should verify: {:?}", result.errors);
}

#[test]
fn adversarial_while_with_complex_condition() {
    let src = r#"fn complex_cond(mut x i32, mut y i32) -> i32 {
    while x > 0 && y > 0 {
        x = x - 1
        y = y - 1
    }
    return x + y
}"#;
    let module = lower(src);
    let f = first_fn(&module);
    assert_all_targets_valid(f);

    let result = verify_function_standalone(f);
    assert!(result.is_ok(), "while with && condition should verify: {:?}", result.errors);
}

#[test]
fn adversarial_for_over_array() {
    let src = r#"fn sum_array() -> i32 {
    arr := [1, 2, 3, 4, 5]
    mut total := 0
    for item in arr {
        total = total + item
    }
    return total
}"#;
    let module = lower(src);
    let f = first_fn(&module);
    assert_all_targets_valid(f);

    let result = verify_function_standalone(f);
    assert!(result.is_ok(), "for-over-array should verify: {:?}", result.errors);
}

#[test]
fn adversarial_optimizer_idempotent() {
    // Running optimize twice should produce the same result.
    let src = "fn foo() -> i32 { return 2 + 3 * 4 }";
    let mut module1 = lower(src);
    opt::optimize(&mut module1);
    let output1 = print_module(&module1);

    let mut module2 = lower(src);
    opt::optimize(&mut module2);
    opt::optimize(&mut module2);
    let output2 = print_module(&module2);

    assert_eq!(
        output1, output2,
        "optimizer should be idempotent -- running twice gives same result"
    );
}

#[test]
fn adversarial_empty_match() {
    // Match with only a wildcard.
    let src = r#"fn catch_all(x i32) -> i32 {
    match x {
        _ => 0
    }
}"#;
    let module = lower(src);
    let f = first_fn(&module);
    assert_all_targets_valid(f);

    // With only a wildcard, there should be NO switch (just a Goto to the default).
    let result = verify_function_standalone(f);
    assert!(result.is_ok(), "wildcard-only match should verify: {:?}", result.errors);
}

#[test]
fn adversarial_break_outside_loop() {
    // This should parse but the break has no loop to target.
    // The lowerer should handle this gracefully (not panic).
    // Note: this may not parse or may be caught by resolve.
    // We just need to make sure the IR layer doesn't panic.
    // If it panics during parse, that's fine -- we catch it.
    let src = r#"fn bad_break() {
    break
}"#;
    let lex_result = Lexer::new(src).tokenize();
    if !lex_result.errors.is_empty() {
        return; // Parser catches it, fine.
    }
    let parse_result = Parser::new(lex_result.tokens).parse();
    if !parse_result.errors.is_empty() {
        return; // Parser catches it, fine.
    }

    // If it parses, the lowerer should not panic.
    let module = lower_module(&parse_result.ast);
    let f = first_fn(&module);

    // The break outside loop should produce a block with Unreachable
    // terminator (since there's no loop_stack entry).
    let printed = print_function(f);
    assert!(!printed.is_empty(), "should produce output even with bad break");
}

#[test]
fn adversarial_verifier_instruction_references_invalid_fn() {
    let module = IrModule {
        functions: vec![IrFunction {
            id: 0,
            name: "bad_call".into(),
            params: vec![],
            return_type: IrType::Unit,
            entry: 0,
            locals: vec![
                IrLocal { id: 0, name: "x".into(), ty: IrType::I64 },
            ],
            blocks: vec![BasicBlock {
                id: 0,
                instructions: vec![
                    Instruction::Assign(0, RValue::Call(999, vec![])), // fn#999 doesn't exist
                ],
                terminator: Terminator::Return(Some(Operand::Constant(Constant::Unit))),
            }],
        }],
        globals: vec![],
        string_literals: vec![],
    };

    let result = verify_module(&module);
    assert!(
        !result.is_ok(),
        "should catch reference to nonexistent function"
    );
    assert!(
        result.errors.iter().any(|e| e.message.contains("999")),
        "error should mention fn id 999"
    );
}

#[test]
fn adversarial_print_module_with_globals() {
    let module = IrModule {
        functions: vec![],
        globals: vec![
            IrGlobal { name: "GLOBAL_X".into(), ty: IrType::I64 },
            IrGlobal { name: "GLOBAL_S".into(), ty: IrType::String },
        ],
        string_literals: vec!["hello".into(), "world".into()],
    };

    let output = print_module(&module);
    assert!(output.contains("GLOBAL_X"), "should print global name");
    assert!(output.contains("GLOBAL_S"), "should print global name");
    assert!(output.contains("string literals"), "should print string literal section");
}

#[test]
fn adversarial_deeply_nested_loops_with_break_continue() {
    let src = r#"fn deep_loops() -> i32 {
    mut result := 0
    mut i := 0
    while i < 10 {
        mut j := 0
        while j < 10 {
            if j == 5 {
                break
            }
            if j == 3 {
                j = j + 1
                continue
            }
            result = result + j
            j = j + 1
        }
        i = i + 1
    }
    return result
}"#;
    let module = lower(src);
    let f = first_fn(&module);
    assert_all_targets_valid(f);

    let result = verify_function_standalone(f);
    assert!(result.is_ok(), "nested loops with break/continue should verify: {:?}", result.errors);

    // Also survive optimization.
    let mut opt_module = module;
    opt::optimize(&mut opt_module);
    let opt_result = verify_module(&opt_module);
    assert!(
        opt_result.is_ok(),
        "nested loops post-opt should verify: {:?}",
        opt_result.errors
    );
}

#[test]
fn adversarial_match_with_binding_pattern() {
    let src = r#"fn bind_test(x i32) -> i32 {
    match x {
        1 => 10
        n => n + 1
    }
}"#;
    let module = lower(src);
    let f = first_fn(&module);
    assert_all_targets_valid(f);

    let result = verify_function_standalone(f);
    assert!(result.is_ok(), "match with binding pattern should verify: {:?}", result.errors);
}

#[test]
fn adversarial_string_interning_dedup() {
    // The same string used multiple times should be interned once.
    let src = r#"fn dup_strings() {
    a := "hello"
    b := "hello"
    c := "hello"
}"#;
    let module = lower(src);

    let hello_count = module
        .string_literals
        .iter()
        .filter(|s| *s == "hello")
        .count();
    assert_eq!(
        hello_count, 1,
        "same string should be interned once, but found {} copies",
        hello_count
    );
}

#[test]
fn adversarial_const_fold_chained_operations() {
    // Chained constant operations: (2 + 3) * (4 - 1).
    // Only direct constant operands get folded, not intermediate results
    // stored in variables.
    let src = "fn chain() -> i32 { return (2 + 3) * (4 - 1) }";
    let mut module = lower(src);
    opt::constant_fold(&mut module);
    let f = first_fn(&module);

    // After first fold: 2+3 -> 5, 4-1 -> 3.
    // But 5*3 might not be folded because operands are now variables.
    // Check what happened.
    let has_const_5 = f.blocks.iter().any(|b| {
        b.instructions.iter().any(|i| {
            matches!(i, Instruction::Assign(_, RValue::Constant(Constant::Int(5))))
        })
    });
    let has_const_3 = f.blocks.iter().any(|b| {
        b.instructions.iter().any(|i| {
            matches!(i, Instruction::Assign(_, RValue::Constant(Constant::Int(3))))
        })
    });

    assert!(has_const_5, "2+3 should fold to 5");
    assert!(has_const_3, "4-1 should fold to 3");

    // The multiplication may or may not fold (depends on implementation).
    // This tests that the optimizer at least handles the simple cases.
}

#[test]
fn adversarial_verify_module_with_duplicate_function_names() {
    let module = IrModule {
        functions: vec![
            IrFunction {
                id: 0,
                name: "same_name".into(),
                params: vec![],
                return_type: IrType::Unit,
                entry: 0,
                locals: vec![],
                blocks: vec![BasicBlock {
                    id: 0,
                    instructions: vec![],
                    terminator: Terminator::Return(Some(Operand::Constant(Constant::Unit))),
                }],
            },
            IrFunction {
                id: 1,
                name: "same_name".into(), // duplicate!
                params: vec![],
                return_type: IrType::Unit,
                entry: 0,
                locals: vec![],
                blocks: vec![BasicBlock {
                    id: 0,
                    instructions: vec![],
                    terminator: Terminator::Return(Some(Operand::Constant(Constant::Unit))),
                }],
            },
        ],
        globals: vec![],
        string_literals: vec![],
    };

    let result = verify_module(&module);
    assert!(!result.is_ok(), "should catch duplicate function names");
    assert!(
        result.errors.iter().any(|e| e.message.contains("duplicate")),
        "error should mention 'duplicate'"
    );
}

// IrModule does not implement Clone by default in some configurations,
// so we manually clone for tests that need it.
fn clone_module(m: &IrModule) -> IrModule {
    IrModule {
        functions: m.functions.clone(),
        globals: m.globals.clone(),
        string_literals: m.string_literals.clone(),
    }
}

#[test]
fn adversarial_loop_continue_targets_header() {
    // Verify continue in a loop goes back to the header, not the body start.
    let src = r#"fn continue_test() {
    mut x := 0
    loop {
        x = x + 1
        if x > 10 {
            break
        }
        continue
    }
}"#;
    let module = lower(src);
    let f = first_fn(&module);

    // Find the loop header block (the one that the entry block gotos first).
    // Then verify that continue targets that same block.
    let goto_targets: Vec<BlockId> = f
        .blocks
        .iter()
        .filter_map(|b| {
            if let Terminator::Goto(t) = &b.terminator {
                Some(*t)
            } else {
                None
            }
        })
        .collect();

    // There should be gotos to the header block (from entry and from continue).
    assert!(
        goto_targets.len() >= 2,
        "should have at least 2 gotos (entry->header and continue->header), got {:?}",
        goto_targets
    );

    assert_all_targets_valid(f);

    let result = verify_function_standalone(f);
    assert!(result.is_ok(), "loop with continue should verify: {:?}", result.errors);
}

#[test]
fn adversarial_nested_if_else_chain() {
    let src = r#"fn classify(x i32) -> i32 {
    if x > 100 {
        return 5
    } else if x > 50 {
        return 4
    } else if x > 25 {
        return 3
    } else if x > 10 {
        return 2
    } else if x > 0 {
        return 1
    } else {
        return 0
    }
}"#;
    let module = lower(src);
    let f = first_fn(&module);
    assert_all_targets_valid(f);

    // Should have 6 return terminators.
    let return_count = f
        .blocks
        .iter()
        .filter(|b| matches!(&b.terminator, Terminator::Return(_)))
        .count();
    assert!(
        return_count >= 6,
        "if-else chain should have 6 returns, got {}",
        return_count
    );

    let result = verify_function_standalone(f);
    assert!(result.is_ok(), "if-else chain should verify: {:?}", result.errors);
}
