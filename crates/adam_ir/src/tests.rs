//! Tests for the Adam IR crate.

use adam_lexer::Lexer;
use adam_parser::Parser;

use crate::ir::*;
use crate::lower::lower_module;
use crate::print::{print_module, print_function};
use crate::verify::{verify_module, verify_function_standalone};
use crate::opt;

// ================================================================
// Test helpers
// ================================================================

/// Lex, parse, and lower source code to an IR module.
fn lower(src: &str) -> IrModule {
    let lex_result = Lexer::new(src).tokenize();
    assert!(lex_result.errors.is_empty(), "lex errors: {:?}", lex_result.errors);

    let parse_result = Parser::new(lex_result.tokens).parse();
    assert!(parse_result.errors.is_empty(), "parse errors: {:?}", parse_result.errors);

    lower_module(&parse_result.ast)
}

/// Lower and verify — asserts no verification errors.
fn lower_and_verify(src: &str) -> IrModule {
    let module = lower(src);
    let result = verify_module(&module);
    assert!(result.is_ok(), "verify errors: {:?}", result.errors);
    module
}

/// Get the first function in the lowered module.
fn first_fn(module: &IrModule) -> &IrFunction {
    module.functions.first().expect("no functions in module")
}

// ================================================================
// Basic lowering tests
// ================================================================

#[test]
fn lower_hello_world() {
    let module = lower(r#"fn main() {
    print("Hello, World!")
}"#);
    assert_eq!(module.functions.len(), 1);
    let f = first_fn(&module);
    assert_eq!(f.name, "main");
    assert!(!f.blocks.is_empty());
    assert!(!module.string_literals.is_empty());
    assert!(module.string_literals.contains(&"Hello, World!".to_string()));
}

#[test]
fn lower_arithmetic() {
    let module = lower("fn add() -> i32 {\n    x := 1 + 2\n    y := x * 3\n    return y\n}");
    let f = first_fn(&module);
    assert_eq!(f.name, "add");
    assert_eq!(f.return_type, IrType::I32);
    // Should have at least one block with arithmetic instructions.
    let entry = &f.blocks[0];
    assert!(!entry.instructions.is_empty());

    // Verify there's an Add and a Mul operation.
    let has_add = entry.instructions.iter().any(|i| matches!(
        i, Instruction::Assign(_, RValue::BinaryOp(BinOp::Add, _, _))
    ));
    let has_mul = entry.instructions.iter().any(|i| matches!(
        i, Instruction::Assign(_, RValue::BinaryOp(BinOp::Mul, _, _))
    ));
    assert!(has_add, "should have an add instruction");
    assert!(has_mul, "should have a mul instruction");
}

#[test]
fn lower_if_else() {
    let module = lower("fn check(x i32) -> i32 {\n    if x > 0 {\n        return 1\n    } else {\n        return -1\n    }\n}");
    let f = first_fn(&module);

    // Should have branch terminator.
    let has_branch = f.blocks.iter().any(|b| matches!(b.terminator, Terminator::Branch(_, _, _)));
    assert!(has_branch, "if/else should produce a branch terminator");

    // Should have at least 4 blocks: entry, then, else, merge.
    assert!(f.blocks.len() >= 4, "if/else should create at least 4 blocks, got {}", f.blocks.len());
}

#[test]
fn lower_match() {
    let module = lower("fn describe(x i32) -> i32 {\n    match x {\n        1 => return 10\n        2 => return 20\n        _ => return 0\n    }\n}");
    let f = first_fn(&module);

    // Should have a switch terminator.
    let has_switch = f.blocks.iter().any(|b| matches!(b.terminator, Terminator::Switch(_, _, _)));
    assert!(has_switch, "match should produce a switch terminator");
}

#[test]
fn lower_for_loop() {
    let module = lower("fn sum_range() -> i32 {\n    total := 0\n    for i in 0..10 {\n        total = total + i\n    }\n    return total\n}");
    let f = first_fn(&module);

    // For loop should produce multiple blocks with back-edge.
    assert!(f.blocks.len() >= 3, "for loop should create header/body/exit blocks");

    // Check there's a goto back to the header (back-edge).
    let has_backedge = f.blocks.iter().any(|b| {
        if let Terminator::Goto(target) = &b.terminator {
            // A goto to an earlier block is a back-edge.
            f.blocks.iter().any(|earlier| earlier.id == *target && earlier.id < b.id)
        } else {
            false
        }
    });
    assert!(has_backedge, "for loop should have a back-edge");
}

#[test]
fn lower_while_loop() {
    let module = lower("fn countdown(mut n i32) {\n    while n > 0 {\n        n = n - 1\n    }\n}");
    let f = first_fn(&module);

    // While loop should have a branch in the header.
    let has_branch = f.blocks.iter().any(|b| matches!(b.terminator, Terminator::Branch(_, _, _)));
    assert!(has_branch, "while loop should have a conditional branch");
}

#[test]
fn lower_loop() {
    let module = lower("fn infinite() {\n    loop {\n        break\n    }\n}");
    let f = first_fn(&module);

    // Loop should have a goto as a back-edge.
    assert!(f.blocks.len() >= 2, "loop should create header and exit blocks");
}

#[test]
fn lower_function_call() {
    let module = lower("fn add(a i32, b i32) -> i32 {\n    return a + b\n}\nfn main() {\n    result := add(1, 2)\n}");
    assert_eq!(module.functions.len(), 2);

    let main_fn = module.functions.iter().find(|f| f.name == "main").unwrap();
    // Should have a Call instruction.
    let has_call = main_fn.blocks.iter().any(|b| {
        b.instructions.iter().any(|i| matches!(
            i, Instruction::Assign(_, RValue::Call(_, _))
        ))
    });
    assert!(has_call, "should have a call instruction for add()");
}

#[test]
fn lower_struct_create() {
    let module = lower("struct Point {\n    x i32\n    y i32\n}\nfn make_point() -> Point {\n    p := Point { x: 10, y: 20 }\n    return p\n}");
    let f = module.functions.iter().find(|f| f.name == "make_point").unwrap();

    // Should have an Aggregate instruction.
    let has_aggregate = f.blocks.iter().any(|b| {
        b.instructions.iter().any(|i| matches!(
            i, Instruction::Assign(_, RValue::Aggregate(AggregateKind::Struct(_), _))
        ))
    });
    assert!(has_aggregate, "struct literal should produce an Aggregate instruction");
}

#[test]
fn lower_field_access() {
    let module = lower("struct Point {\n    x i32\n    y i32\n}\nfn get_x(p Point) -> i32 {\n    return p.x\n}");
    let f = module.functions.iter().find(|f| f.name == "get_x").unwrap();

    // Should have a Field instruction.
    let has_field = f.blocks.iter().any(|b| {
        b.instructions.iter().any(|i| matches!(
            i, Instruction::Assign(_, RValue::Field(_, _))
        ))
    });
    assert!(has_field, "field access should produce a Field instruction");
}

#[test]
fn lower_closure_basic() {
    let module = lower("fn apply() -> i32 {\n    f := |x| x + 1\n    return f(5)\n}");
    let f = first_fn(&module);
    // Should produce some instructions (closure body lowered inline for now).
    assert!(!f.blocks.is_empty());
}

#[test]
fn lower_spawn() {
    let module = lower("fn concurrent() {\n    spawn {\n        print(\"hello from task\")\n    }\n}");
    let f = first_fn(&module);

    // Should have a spawn terminator.
    let has_spawn = f.blocks.iter().any(|b| matches!(b.terminator, Terminator::Spawn(_, _)));
    assert!(has_spawn, "spawn should produce a Spawn terminator");
}

#[test]
fn lower_channel() {
    let module = lower("fn channel_test() {\n    ch := chan[i32](10)\n}");
    let f = first_fn(&module);

    // Should have a ChanCreate instruction.
    let has_chan = f.blocks.iter().any(|b| {
        b.instructions.iter().any(|i| matches!(
            i, Instruction::Assign(_, RValue::ChanCreate(_, _))
        ))
    });
    assert!(has_chan, "chan[] should produce a ChanCreate instruction");
}

#[test]
fn lower_string_literal() {
    let module = lower("fn greet() {\n    name := \"Adam\"\n}");
    assert!(module.string_literals.contains(&"Adam".to_string()));
}

#[test]
fn lower_bool_literal() {
    let module = lower("fn flag() -> bool {\n    return true\n}");
    let f = first_fn(&module);
    let has_bool = f.blocks.iter().any(|b| {
        matches!(&b.terminator, Terminator::Return(Some(Operand::Constant(Constant::Bool(true)))))
    });
    assert!(has_bool, "should return a bool constant true");
}

#[test]
fn lower_unary_neg() {
    let module = lower("fn negate(x i32) -> i32 {\n    return -x\n}");
    let f = first_fn(&module);
    let has_neg = f.blocks.iter().any(|b| {
        b.instructions.iter().any(|i| matches!(
            i, Instruction::Assign(_, RValue::UnaryOp(UnOp::Neg, _))
        ))
    });
    assert!(has_neg, "should have a Neg unary operation");
}

#[test]
fn lower_array_literal() {
    let module = lower("fn make_array() {\n    arr := [1, 2, 3]\n}");
    let f = first_fn(&module);
    let has_array = f.blocks.iter().any(|b| {
        b.instructions.iter().any(|i| matches!(
            i, Instruction::Assign(_, RValue::Aggregate(AggregateKind::Array, _))
        ))
    });
    assert!(has_array, "array literal should produce an Array aggregate");
}

#[test]
fn lower_tuple_literal() {
    let module = lower("fn make_tuple() {\n    t := (1, 2, 3)\n}");
    let f = first_fn(&module);
    let has_tuple = f.blocks.iter().any(|b| {
        b.instructions.iter().any(|i| matches!(
            i, Instruction::Assign(_, RValue::Aggregate(AggregateKind::Tuple, _))
        ))
    });
    assert!(has_tuple, "tuple literal should produce a Tuple aggregate");
}

#[test]
fn lower_multiple_functions() {
    let module = lower("fn foo() -> i32 { return 1 }\nfn bar() -> i32 { return 2 }\nfn baz() -> i32 { return 3 }");
    assert_eq!(module.functions.len(), 3);
    assert_eq!(module.functions[0].name, "foo");
    assert_eq!(module.functions[1].name, "bar");
    assert_eq!(module.functions[2].name, "baz");
}

#[test]
fn lower_nested_if() {
    let module = lower("fn nested(x i32) -> i32 {\n    if x > 10 {\n        if x > 20 {\n            return 3\n        } else {\n            return 2\n        }\n    } else {\n        return 1\n    }\n}");
    let f = first_fn(&module);
    // Nested if should create many blocks.
    assert!(f.blocks.len() >= 6, "nested if should create many blocks, got {}", f.blocks.len());
}

#[test]
fn lower_break_continue() {
    let module = lower("fn with_break() {\n    x := 0\n    while true {\n        if x > 5 {\n            break\n        }\n        x = x + 1\n        continue\n    }\n}");
    let f = first_fn(&module);
    // break and continue should produce Goto terminators.
    let goto_count = f.blocks.iter().filter(|b| matches!(b.terminator, Terminator::Goto(_))).count();
    assert!(goto_count >= 2, "break/continue should produce goto terminators");
}

#[test]
fn lower_return_unit() {
    let module = lower("fn noop() {\n    return\n}");
    let f = first_fn(&module);
    let has_return = f.blocks.iter().any(|b| matches!(b.terminator, Terminator::Return(_)));
    assert!(has_return, "should have a return terminator");
}

#[test]
fn lower_comparison_ops() {
    let module = lower("fn cmp(a i32, b i32) -> bool {\n    c1 := a == b\n    c2 := a != b\n    c3 := a < b\n    c4 := a > b\n    c5 := a <= b\n    c6 := a >= b\n    return c1\n}");
    let f = first_fn(&module);
    let entry = &f.blocks[0];

    let ops: Vec<BinOp> = entry.instructions.iter().filter_map(|i| {
        if let Instruction::Assign(_, RValue::BinaryOp(op, _, _)) = i {
            Some(*op)
        } else {
            None
        }
    }).collect();

    assert!(ops.contains(&BinOp::Eq));
    assert!(ops.contains(&BinOp::NotEq));
    assert!(ops.contains(&BinOp::Lt));
    assert!(ops.contains(&BinOp::Gt));
    assert!(ops.contains(&BinOp::LtEq));
    assert!(ops.contains(&BinOp::GtEq));
}

#[test]
fn lower_assign_existing_var() {
    let module = lower("fn mutate() {\n    mut x := 5\n    x = 10\n}");
    let f = first_fn(&module);
    // The variable should be assigned twice.
    let assign_count = f.blocks.iter().flat_map(|b| &b.instructions).filter(|i| {
        matches!(i, Instruction::Assign(_, _))
    }).count();
    assert!(assign_count >= 2, "should have at least 2 assignments");
}

// ================================================================
// Optimization tests
// ================================================================

#[test]
fn opt_const_fold_add() {
    let mut module = lower("fn add() -> i32 {\n    return 2 + 3\n}");
    opt::constant_fold(&mut module);
    let f = first_fn(&module);

    // After folding, the add should become a constant 5.
    let has_const_5 = f.blocks.iter().any(|b| {
        b.instructions.iter().any(|i| matches!(
            i, Instruction::Assign(_, RValue::Constant(Constant::Int(5)))
        ))
    });
    assert!(has_const_5, "constant fold should produce const 5 from 2+3");
}

#[test]
fn opt_const_fold_mul() {
    let mut module = lower("fn mul() -> i32 {\n    return 3 * 7\n}");
    opt::constant_fold(&mut module);
    let f = first_fn(&module);

    let has_const_21 = f.blocks.iter().any(|b| {
        b.instructions.iter().any(|i| matches!(
            i, Instruction::Assign(_, RValue::Constant(Constant::Int(21)))
        ))
    });
    assert!(has_const_21, "constant fold should produce const 21 from 3*7");
}

#[test]
fn opt_const_fold_bool() {
    let mut module = lower("fn logic() -> bool {\n    return true && false\n}");
    opt::constant_fold(&mut module);
    let f = first_fn(&module);

    let has_const_false = f.blocks.iter().any(|b| {
        b.instructions.iter().any(|i| matches!(
            i, Instruction::Assign(_, RValue::Constant(Constant::Bool(false)))
        ))
    });
    assert!(has_const_false, "constant fold should produce false from true && false");
}

#[test]
fn opt_const_fold_comparison() {
    let mut module = lower("fn cmp() -> bool {\n    return 5 > 3\n}");
    opt::constant_fold(&mut module);
    let f = first_fn(&module);

    let has_const_true = f.blocks.iter().any(|b| {
        b.instructions.iter().any(|i| matches!(
            i, Instruction::Assign(_, RValue::Constant(Constant::Bool(true)))
        ))
    });
    assert!(has_const_true, "constant fold should produce true from 5 > 3");
}

#[test]
fn opt_dead_code_basic() {
    // Create a module with an unreachable block manually.
    let mut module = IrModule {
        functions: vec![IrFunction {
            id: 0,
            name: "test".into(),
            params: vec![],
            return_type: IrType::Unit,
            entry: 0,
            locals: vec![],
            blocks: vec![
                BasicBlock {
                    id: 0,
                    instructions: vec![],
                    terminator: Terminator::Return(Some(Operand::Constant(Constant::Unit))),
                },
                BasicBlock {
                    id: 1,
                    instructions: vec![Instruction::Nop],
                    terminator: Terminator::Unreachable,
                },
            ],
        }],
        globals: vec![],
        string_literals: vec![],
        struct_defs: vec![],
    };

    assert_eq!(module.functions[0].blocks.len(), 2);
    opt::dead_code(&mut module);
    assert_eq!(module.functions[0].blocks.len(), 1, "dead block should be removed");
    assert_eq!(module.functions[0].blocks[0].id, 0);
}

#[test]
fn opt_simplify_cfg_merge() {
    // bb0 -> goto bb1, bb1 has only one predecessor.
    let mut module = IrModule {
        functions: vec![IrFunction {
            id: 0,
            name: "test".into(),
            params: vec![],
            return_type: IrType::Unit,
            entry: 0,
            locals: vec![
                IrLocal { id: 0, name: "x".into(), ty: IrType::I64 },
                IrLocal { id: 1, name: "y".into(), ty: IrType::I64 },
            ],
            blocks: vec![
                BasicBlock {
                    id: 0,
                    instructions: vec![
                        Instruction::Assign(0, RValue::Constant(Constant::Int(1))),
                    ],
                    terminator: Terminator::Goto(1),
                },
                BasicBlock {
                    id: 1,
                    instructions: vec![
                        Instruction::Assign(1, RValue::Constant(Constant::Int(2))),
                    ],
                    terminator: Terminator::Return(Some(Operand::Var(1))),
                },
            ],
        }],
        globals: vec![],
        string_literals: vec![],
        struct_defs: vec![],
    };

    assert_eq!(module.functions[0].blocks.len(), 2);
    opt::simplify_cfg(&mut module);
    assert_eq!(module.functions[0].blocks.len(), 1, "blocks should be merged");
    assert_eq!(module.functions[0].blocks[0].instructions.len(), 2);
}

#[test]
fn opt_remove_nops() {
    let mut module = IrModule {
        functions: vec![IrFunction {
            id: 0,
            name: "test".into(),
            params: vec![],
            return_type: IrType::Unit,
            entry: 0,
            locals: vec![],
            blocks: vec![BasicBlock {
                id: 0,
                instructions: vec![Instruction::Nop, Instruction::Nop, Instruction::Nop],
                terminator: Terminator::Return(Some(Operand::Constant(Constant::Unit))),
            }],
        }],
        globals: vec![],
        string_literals: vec![],
        struct_defs: vec![],
    };

    assert_eq!(module.functions[0].blocks[0].instructions.len(), 3);
    opt::remove_nops(&mut module);
    assert_eq!(module.functions[0].blocks[0].instructions.len(), 0, "nops should be removed");
}

#[test]
fn opt_full_pipeline() {
    let mut module = lower("fn calculate() -> i32 {\n    x := 2 + 3\n    y := 10 * 2\n    return x\n}");
    opt::optimize(&mut module);
    let f = first_fn(&module);
    // After optimization, constant operations should be folded.
    let has_const = f.blocks.iter().any(|b| {
        b.instructions.iter().any(|i| matches!(
            i, Instruction::Assign(_, RValue::Constant(Constant::Int(_)))
        ))
    });
    assert!(has_const, "optimization pipeline should fold constants");
}

// ================================================================
// Copy propagation tests
// ================================================================

#[test]
fn opt_copy_prop_simple() {
    // x = 5; y = x; return y  →  return should use x's var (or the constant after const prop)
    let mut module = IrModule {
        functions: vec![IrFunction {
            id: 0,
            name: "test".into(),
            params: vec![],
            return_type: IrType::I64,
            entry: 0,
            locals: vec![
                IrLocal { id: 0, name: "x".into(), ty: IrType::I64 },
                IrLocal { id: 1, name: "y".into(), ty: IrType::I64 },
            ],
            blocks: vec![BasicBlock {
                id: 0,
                instructions: vec![
                    Instruction::Assign(0, RValue::Constant(Constant::Int(5))),
                    Instruction::Assign(1, RValue::Use(Operand::Var(0))), // y = x
                ],
                terminator: Terminator::Return(Some(Operand::Var(1))), // return y
            }],
        }],
        globals: vec![],
        string_literals: vec![],
        struct_defs: vec![],
    };

    opt::copy_propagation(&mut module);

    // After copy prop, return should use var 0 (x) instead of var 1 (y).
    let f = &module.functions[0];
    if let Terminator::Return(Some(Operand::Var(v))) = &f.blocks[0].terminator {
        assert_eq!(*v, 0, "copy prop should replace y with x in return");
    } else {
        panic!("expected return with var operand");
    }
}

#[test]
fn opt_copy_prop_no_prop_after_reassign() {
    // x = 5; y = x; x = 10; return y  →  y should NOT be replaced (x was reassigned)
    let mut module = IrModule {
        functions: vec![IrFunction {
            id: 0,
            name: "test".into(),
            params: vec![],
            return_type: IrType::I64,
            entry: 0,
            locals: vec![
                IrLocal { id: 0, name: "x".into(), ty: IrType::I64 },
                IrLocal { id: 1, name: "y".into(), ty: IrType::I64 },
            ],
            blocks: vec![BasicBlock {
                id: 0,
                instructions: vec![
                    Instruction::Assign(0, RValue::Constant(Constant::Int(5))),
                    Instruction::Assign(1, RValue::Use(Operand::Var(0))), // y = x
                    Instruction::Assign(0, RValue::Constant(Constant::Int(10))), // x = 10
                ],
                terminator: Terminator::Return(Some(Operand::Var(1))), // return y
            }],
        }],
        globals: vec![],
        string_literals: vec![],
        struct_defs: vec![],
    };

    opt::copy_propagation(&mut module);

    // y should still refer to var 1 since the copy y=x was invalidated by x=10.
    // But wait — the copy y=x happened before x was reassigned. Copy prop records
    // copies at the point of assignment. When x is reassigned, the copy y->x is
    // invalidated. But y already got assigned. The return uses var 1 (y).
    // Actually, copies maps dst->src. After y=x: copies = {1->0}.
    // After x=10: x is reassigned, so we remove entries where src==0. copies = {}.
    // Return uses var 1, which is not in copies, so no replacement. Correct!
    let f = &module.functions[0];
    if let Terminator::Return(Some(Operand::Var(v))) = &f.blocks[0].terminator {
        assert_eq!(*v, 1, "should NOT propagate copy after source reassignment");
    } else {
        panic!("expected return with var operand");
    }
}

// ================================================================
// Constant propagation tests
// ================================================================

#[test]
fn opt_const_prop_simple() {
    // x = 5; y = x + 1  →  y = 5 + 1
    let mut module = IrModule {
        functions: vec![IrFunction {
            id: 0,
            name: "test".into(),
            params: vec![],
            return_type: IrType::I64,
            entry: 0,
            locals: vec![
                IrLocal { id: 0, name: "x".into(), ty: IrType::I64 },
                IrLocal { id: 1, name: "y".into(), ty: IrType::I64 },
            ],
            blocks: vec![BasicBlock {
                id: 0,
                instructions: vec![
                    Instruction::Assign(0, RValue::Constant(Constant::Int(5))),
                    Instruction::Assign(
                        1,
                        RValue::BinaryOp(
                            BinOp::Add,
                            Operand::Var(0),       // x
                            Operand::Constant(Constant::Int(1)),
                        ),
                    ),
                ],
                terminator: Terminator::Return(Some(Operand::Var(1))),
            }],
        }],
        globals: vec![],
        string_literals: vec![],
        struct_defs: vec![],
    };

    opt::constant_propagation(&mut module);

    // After const prop, the BinaryOp should have Constant(5) instead of Var(0).
    let f = &module.functions[0];
    if let Instruction::Assign(_, RValue::BinaryOp(BinOp::Add, lhs, _)) = &f.blocks[0].instructions[1] {
        assert!(
            matches!(lhs, Operand::Constant(Constant::Int(5))),
            "const prop should replace Var(0) with Constant(5), got {:?}",
            lhs
        );
    } else {
        panic!("expected BinaryOp instruction");
    }
}

#[test]
fn opt_const_prop_then_fold() {
    // x = 5; y = x + 1  →  after const prop + fold: y = 6
    let mut module = IrModule {
        functions: vec![IrFunction {
            id: 0,
            name: "test".into(),
            params: vec![],
            return_type: IrType::I64,
            entry: 0,
            locals: vec![
                IrLocal { id: 0, name: "x".into(), ty: IrType::I64 },
                IrLocal { id: 1, name: "y".into(), ty: IrType::I64 },
            ],
            blocks: vec![BasicBlock {
                id: 0,
                instructions: vec![
                    Instruction::Assign(0, RValue::Constant(Constant::Int(5))),
                    Instruction::Assign(
                        1,
                        RValue::BinaryOp(
                            BinOp::Add,
                            Operand::Var(0),
                            Operand::Constant(Constant::Int(1)),
                        ),
                    ),
                ],
                terminator: Terminator::Return(Some(Operand::Var(1))),
            }],
        }],
        globals: vec![],
        string_literals: vec![],
        struct_defs: vec![],
    };

    // Run const prop then fold — the full optimize pipeline does this.
    opt::constant_propagation(&mut module);
    opt::constant_fold(&mut module);

    let f = &module.functions[0];
    let has_6 = f.blocks[0].instructions.iter().any(|i| {
        matches!(i, Instruction::Assign(1, RValue::Constant(Constant::Int(6))))
    });
    assert!(has_6, "const prop + fold should produce y = 6");
}

// ================================================================
// Verification tests
// ================================================================

#[test]
fn verify_valid_module() {
    let module = lower("fn main() {\n    x := 5\n    print(x)\n}");
    let result = verify_module(&module);
    assert!(result.is_ok(), "simple valid module should verify: {:?}", result.errors);
}

#[test]
fn verify_invalid_goto_target() {
    let module = IrModule {
        functions: vec![IrFunction {
            id: 0,
            name: "bad".into(),
            params: vec![],
            return_type: IrType::Unit,
            entry: 0,
            locals: vec![],
            blocks: vec![BasicBlock {
                id: 0,
                instructions: vec![],
                terminator: Terminator::Goto(999), // nonexistent target
            }],
        }],
        globals: vec![],
        string_literals: vec![],
        struct_defs: vec![],
    };
    let result = verify_module(&module);
    assert!(!result.is_ok(), "invalid goto target should fail verification");
    assert!(result.errors[0].message.contains("999"));
}

#[test]
fn verify_invalid_branch_target() {
    let module = IrModule {
        functions: vec![IrFunction {
            id: 0,
            name: "bad_branch".into(),
            params: vec![],
            return_type: IrType::Unit,
            entry: 0,
            locals: vec![],
            blocks: vec![BasicBlock {
                id: 0,
                instructions: vec![],
                terminator: Terminator::Branch(
                    Operand::Constant(Constant::Bool(true)),
                    100, // nonexistent
                    200, // nonexistent
                ),
            }],
        }],
        globals: vec![],
        string_literals: vec![],
        struct_defs: vec![],
    };
    let result = verify_module(&module);
    assert!(!result.is_ok());
    assert!(result.errors.len() >= 2, "should report errors for both targets");
}

#[test]
fn verify_duplicate_block_id() {
    let module = IrModule {
        functions: vec![IrFunction {
            id: 0,
            name: "dup_blocks".into(),
            params: vec![],
            return_type: IrType::Unit,
            entry: 0,
            locals: vec![],
            blocks: vec![
                BasicBlock {
                    id: 0,
                    instructions: vec![],
                    terminator: Terminator::Return(Some(Operand::Constant(Constant::Unit))),
                },
                BasicBlock {
                    id: 0, // duplicate!
                    instructions: vec![],
                    terminator: Terminator::Return(Some(Operand::Constant(Constant::Unit))),
                },
            ],
        }],
        globals: vec![],
        string_literals: vec![],
        struct_defs: vec![],
    };
    let result = verify_module(&module);
    assert!(!result.is_ok());
    assert!(result.errors.iter().any(|e| e.message.contains("duplicate block")));
}

#[test]
fn verify_missing_entry() {
    let module = IrModule {
        functions: vec![IrFunction {
            id: 0,
            name: "no_entry".into(),
            params: vec![],
            return_type: IrType::Unit,
            entry: 42, // doesn't exist
            locals: vec![],
            blocks: vec![BasicBlock {
                id: 0,
                instructions: vec![],
                terminator: Terminator::Return(Some(Operand::Constant(Constant::Unit))),
            }],
        }],
        globals: vec![],
        string_literals: vec![],
        struct_defs: vec![],
    };
    let result = verify_module(&module);
    assert!(!result.is_ok());
    assert!(result.errors.iter().any(|e| e.message.contains("entry block")));
}

#[test]
fn verify_function_standalone_api() {
    let func = IrFunction {
        id: 0,
        name: "standalone".into(),
        params: vec![],
        return_type: IrType::Unit,
        entry: 0,
        locals: vec![],
        blocks: vec![BasicBlock {
            id: 0,
            instructions: vec![],
            terminator: Terminator::Return(Some(Operand::Constant(Constant::Unit))),
        }],
    };
    let result = verify_function_standalone(&func);
    assert!(result.is_ok());
}

// ================================================================
// Printer tests
// ================================================================

#[test]
fn print_basic_function() {
    let module = lower("fn main() {\n    x := 5\n}");
    let output = print_module(&module);
    assert!(output.contains("fn @main"), "output should contain function name");
    assert!(output.contains("bb0:"), "output should contain block label");
    assert!(output.contains("return"), "output should contain return");
}

#[test]
fn print_function_with_params() {
    let module = lower("fn add(a i32, b i32) -> i32 {\n    return a + b\n}");
    let output = print_module(&module);
    assert!(output.contains("fn @add("));
    assert!(output.contains("a: i32"));
    assert!(output.contains("b: i32"));
    assert!(output.contains("-> i32"));
}

#[test]
fn print_string_literals() {
    let module = lower("fn greet() {\n    msg := \"Hello\"\n}");
    let output = print_module(&module);
    assert!(output.contains("string literals"), "should show string literal table");
    assert!(output.contains("Hello"), "should contain the string literal");
}

#[test]
fn print_single_function() {
    let func = IrFunction {
        id: 0,
        name: "test".into(),
        params: vec![],
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
    assert!(output.contains("fn @test"));
    assert!(output.contains("return const ()"));
}

// ================================================================
// Integration: lower + verify + optimize + print
// ================================================================

#[test]
fn integration_full_pipeline() {
    let src = "fn fibonacci(n i32) -> i32 {\n    if n <= 1 {\n        return n\n    }\n    a := 0\n    b := 1\n    i := 2\n    while i <= n {\n        temp := a + b\n        a = b\n        b = temp\n        i = i + 1\n    }\n    return b\n}";

    let mut module = lower(src);

    // Verify before optimization.
    let result = verify_module(&module);
    assert!(result.is_ok(), "pre-opt verify failed: {:?}", result.errors);

    // Optimize.
    opt::optimize(&mut module);

    // Verify after optimization.
    let result = verify_module(&module);
    assert!(result.is_ok(), "post-opt verify failed: {:?}", result.errors);

    // Print.
    let output = print_module(&module);
    assert!(output.contains("fn @fibonacci"));
    assert!(!output.is_empty());
}

#[test]
fn lower_and_verify_complex() {
    let module = lower_and_verify("fn process() -> i32 {\n    result := 0\n    for item in 0..10 {\n        if item > 0 {\n            result = result + item\n        }\n    }\n    return result\n}");
    assert!(!module.functions.is_empty());
}

// ================================================================
// Regression tests — guards against reintroduction of fixed bugs
// ================================================================

#[test]
fn regression_const_fold_negate_i64_min_no_panic() {
    // Bug: constant folding negation of i64::MIN panicked because
    // -i64::MIN overflows. Fix: use checked_neg().
    let mut module = IrModule {
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
        struct_defs: vec![],
    };

    // Must not panic.
    opt::constant_fold(&mut module);

    // The negation should NOT be folded (since result would overflow).
    // It should remain as a UnaryOp.
    let f = first_fn(&module);
    let still_unary = f.blocks[0].instructions.iter().any(|i| {
        matches!(i, Instruction::Assign(_, RValue::UnaryOp(UnOp::Neg, _)))
    });
    assert!(
        still_unary,
        "negation of i64::MIN should not be folded (overflow)"
    );
}

#[test]
fn regression_spawn_continuation_survives_dce() {
    // Bug: Spawn terminator only pointed to the spawned block, not the
    // continuation block. Code after spawn was unreachable and eliminated
    // by dead code elimination.
    let src = r#"fn spawner() -> i32 {
    spawn {
        x := 1
    }
    return 42
}"#;
    let mut module = lower(src);

    // The spawn should produce Spawn(target, continuation).
    let f = first_fn(&module);
    let has_spawn = f.blocks.iter().any(|b| matches!(&b.terminator, Terminator::Spawn(_, _)));
    assert!(has_spawn, "should have Spawn terminator with two targets");

    // After DCE, the return 42 block must survive.
    opt::dead_code(&mut module);
    let f = first_fn(&module);
    let has_return_42 = f.blocks.iter().any(|b| {
        matches!(&b.terminator, Terminator::Return(Some(Operand::Constant(Constant::Int(42)))))
    });
    assert!(
        has_return_42,
        "return 42 after spawn must survive DCE — continuation block must be reachable"
    );
}

#[test]
fn regression_multi_part_string_interpolation_parses() {
    // Bug: multi-part string interpolation like "x={x} y={y}" failed to
    // parse because the lexer's pending token queue (Option<Token>) could
    // only hold one token, losing the second StringInterpolStart.
    // Fix: changed pending from Option<Token> to Vec<Token>.
    let src = r#"fn show(x i32, y i32) -> String {
    msg := "x={x} y={y}"
    return msg
}"#;
    let module = lower(src);
    let f = first_fn(&module);

    // Should have at least 2 __str_concat calls (for 4 parts: "x=", x, " y=", y).
    let concat_count = f
        .blocks
        .iter()
        .flat_map(|b| &b.instructions)
        .filter(|i| {
            matches!(i, Instruction::Assign(_, RValue::CallNamed(name, _)) if name == "__str_concat")
        })
        .count();
    assert!(
        concat_count >= 2,
        "multi-part interpolation must produce at least 2 concat calls, got {}",
        concat_count
    );

    // Must verify cleanly.
    let result = verify_function_standalone(f);
    assert!(
        result.is_ok(),
        "multi-part interpolation IR must verify: {:?}",
        result.errors
    );
}

// ================================================================
// Drop insertion tests
// ================================================================

/// Helper: count the number of Drop instructions across all blocks of a function.
fn count_drops(func: &IrFunction) -> usize {
    func.blocks
        .iter()
        .flat_map(|b| &b.instructions)
        .filter(|i| matches!(i, Instruction::Drop(_)))
        .count()
}

#[test]
fn drop_emitted_for_string_variable() {
    let module = lower(r#"fn main() {
    s := "hello"
}"#);
    let f = first_fn(&module);
    let drops = count_drops(f);
    assert!(drops > 0, "expected Drop instructions for String variable, found {}", drops);
}

#[test]
fn no_drop_for_integer_variable() {
    let module = lower(r#"fn main() {
    x := 42
}"#);
    let f = first_fn(&module);
    let drops = count_drops(f);
    assert_eq!(drops, 0, "integer variables should not produce Drop instructions");
}

#[test]
fn drop_emitted_in_nested_block() {
    let module = lower(r#"fn main() {
    if true {
        s := "inner"
    }
}"#);
    let f = first_fn(&module);
    let drops = count_drops(f);
    assert!(drops > 0, "expected Drop for String in nested block");
}

#[test]
fn multiple_drops_for_multiple_strings() {
    let module = lower(r#"fn main() {
    a := "one"
    b := "two"
    c := "three"
}"#);
    let f = first_fn(&module);
    let drops = count_drops(f);
    assert!(drops >= 3, "expected at least 3 Drops for 3 String variables, found {}", drops);
}

// ================================================================
// String interpolation with type conversion
// ================================================================

#[test]
fn lower_string_interpolation_with_int() {
    let module = lower(r#"fn main() {
    x := 42
    msg := "value is {x}"
}"#);
    let f = first_fn(&module);

    // Should have a __int_to_string call for the int interpolation.
    let has_int_to_str = f.blocks.iter().flat_map(|b| &b.instructions).any(|i| {
        matches!(i, Instruction::Assign(_, RValue::CallNamed(name, _)) if name == "__int_to_string")
    });
    assert!(has_int_to_str, "int interpolation should emit __int_to_string call");

    // Should also have __str_concat calls.
    let has_concat = f.blocks.iter().flat_map(|b| &b.instructions).any(|i| {
        matches!(i, Instruction::Assign(_, RValue::CallNamed(name, _)) if name == "__str_concat")
    });
    assert!(has_concat, "string interpolation should emit __str_concat calls");
}

#[test]
fn lower_string_interpolation_with_bool() {
    let module = lower(r#"fn main() {
    b := true
    msg := "flag is {b}"
}"#);
    let f = first_fn(&module);

    let has_bool_to_str = f.blocks.iter().flat_map(|b| &b.instructions).any(|i| {
        matches!(i, Instruction::Assign(_, RValue::CallNamed(name, _)) if name == "__bool_to_string")
    });
    assert!(has_bool_to_str, "bool interpolation should emit __bool_to_string call");
}

#[test]
fn lower_string_interpolation_string_no_conversion() {
    let module = lower(r#"fn main() {
    name := "world"
    msg := "hello {name}"
}"#);
    let f = first_fn(&module);

    // String interpolation of a string var should NOT emit __int_to_string etc.
    let has_int_to_str = f.blocks.iter().flat_map(|b| &b.instructions).any(|i| {
        matches!(i, Instruction::Assign(_, RValue::CallNamed(name, _)) if name == "__int_to_string")
    });
    assert!(!has_int_to_str, "string interpolation of String var should not emit __int_to_string");

    // Should still have __str_concat.
    let has_concat = f.blocks.iter().flat_map(|b| &b.instructions).any(|i| {
        matches!(i, Instruction::Assign(_, RValue::CallNamed(name, _)) if name == "__str_concat")
    });
    assert!(has_concat, "string interpolation should emit __str_concat calls");
}

#[test]
fn lower_string_interpolation_multi_type() {
    let module = lower(r#"fn main() {
    x := 1
    y := 2
    msg := "x={x} y={y}"
}"#);
    let f = first_fn(&module);

    // Should have at least 2 __int_to_string calls (one for x, one for y).
    let int_to_str_count = f.blocks.iter().flat_map(|b| &b.instructions).filter(|i| {
        matches!(i, Instruction::Assign(_, RValue::CallNamed(name, _)) if name == "__int_to_string")
    }).count();
    assert!(int_to_str_count >= 2, "multi-int interpolation should emit at least 2 __int_to_string calls, got {}", int_to_str_count);
}

// ================================================================
// Function inlining tests
// ================================================================

#[test]
fn opt_inline_simple_function() {
    // add(a, b) = a + b; main calls add(3, 4) → should inline the add body
    let mut module = IrModule {
        functions: vec![
            IrFunction {
                id: 0,
                name: "add".into(),
                params: vec![
                    IrParam { name: "a".into(), ty: IrType::I64 },
                    IrParam { name: "b".into(), ty: IrType::I64 },
                ],
                return_type: IrType::I64,
                entry: 0,
                locals: vec![
                    IrLocal { id: 0, name: "a".into(), ty: IrType::I64 },
                    IrLocal { id: 1, name: "b".into(), ty: IrType::I64 },
                    IrLocal { id: 2, name: "result".into(), ty: IrType::I64 },
                ],
                blocks: vec![BasicBlock {
                    id: 0,
                    instructions: vec![
                        Instruction::Assign(2, RValue::BinaryOp(
                            BinOp::Add,
                            Operand::Var(0),
                            Operand::Var(1),
                        )),
                    ],
                    terminator: Terminator::Return(Some(Operand::Var(2))),
                }],
            },
            IrFunction {
                id: 1,
                name: "main".into(),
                params: vec![],
                return_type: IrType::I64,
                entry: 0,
                locals: vec![
                    IrLocal { id: 0, name: "res".into(), ty: IrType::I64 },
                ],
                blocks: vec![BasicBlock {
                    id: 0,
                    instructions: vec![
                        Instruction::Assign(0, RValue::CallNamed(
                            "add".into(),
                            vec![Operand::Constant(Constant::Int(3)), Operand::Constant(Constant::Int(4))],
                        )),
                    ],
                    terminator: Terminator::Return(Some(Operand::Var(0))),
                }],
            },
        ],
        globals: vec![],
        string_literals: vec![],
        struct_defs: vec![],
    };

    opt::function_inline(&mut module);

    // The main function should no longer have a CallNamed to "add".
    let main_fn = &module.functions[1];
    let has_call = main_fn.blocks.iter().flat_map(|b| &b.instructions).any(|i| {
        matches!(i, Instruction::Assign(_, RValue::CallNamed(name, _)) if name == "add")
    });
    assert!(!has_call, "add() call should have been inlined");

    // Should have a BinaryOp(Add, ...) instruction in main instead.
    let has_add = main_fn.blocks.iter().flat_map(|b| &b.instructions).any(|i| {
        matches!(i, Instruction::Assign(_, RValue::BinaryOp(BinOp::Add, _, _)))
    });
    assert!(has_add, "inlined add body should contain BinaryOp::Add");
}

#[test]
fn opt_inline_recursive_not_inlined() {
    // factorial(n) calls factorial(n-1) — should NOT be inlined
    let mut module = IrModule {
        functions: vec![
            IrFunction {
                id: 0,
                name: "factorial".into(),
                params: vec![IrParam { name: "n".into(), ty: IrType::I64 }],
                return_type: IrType::I64,
                entry: 0,
                locals: vec![
                    IrLocal { id: 0, name: "n".into(), ty: IrType::I64 },
                    IrLocal { id: 1, name: "tmp".into(), ty: IrType::I64 },
                    IrLocal { id: 2, name: "rec".into(), ty: IrType::I64 },
                ],
                blocks: vec![BasicBlock {
                    id: 0,
                    instructions: vec![
                        Instruction::Assign(1, RValue::BinaryOp(
                            BinOp::Sub, Operand::Var(0), Operand::Constant(Constant::Int(1)),
                        )),
                        Instruction::Assign(2, RValue::CallNamed(
                            "factorial".into(), vec![Operand::Var(1)],
                        )),
                    ],
                    terminator: Terminator::Return(Some(Operand::Var(2))),
                }],
            },
            IrFunction {
                id: 1,
                name: "main".into(),
                params: vec![],
                return_type: IrType::I64,
                entry: 0,
                locals: vec![
                    IrLocal { id: 0, name: "res".into(), ty: IrType::I64 },
                ],
                blocks: vec![BasicBlock {
                    id: 0,
                    instructions: vec![
                        Instruction::Assign(0, RValue::CallNamed(
                            "factorial".into(), vec![Operand::Constant(Constant::Int(5))],
                        )),
                    ],
                    terminator: Terminator::Return(Some(Operand::Var(0))),
                }],
            },
        ],
        globals: vec![],
        string_literals: vec![],
        struct_defs: vec![],
    };

    opt::function_inline(&mut module);

    // factorial is recursive, so it should NOT be inlineable.
    // The call in main should still be there.
    let main_fn = &module.functions[1];
    let has_call = main_fn.blocks.iter().flat_map(|b| &b.instructions).any(|i| {
        matches!(i, Instruction::Assign(_, RValue::CallNamed(name, _)) if name == "factorial")
    });
    assert!(has_call, "recursive function should NOT be inlined");
}

#[test]
fn opt_inline_large_function_not_inlined() {
    // A function with >= 20 instructions should NOT be inlined.
    let many_instructions: Vec<Instruction> = (0..21u32)
        .map(|i| Instruction::Assign(i + 1, RValue::Constant(Constant::Int(i as i64))))
        .collect();

    let mut locals: Vec<IrLocal> = vec![IrLocal { id: 0, name: "x".into(), ty: IrType::I64 }];
    for i in 0..21u32 {
        locals.push(IrLocal { id: i + 1, name: format!("t{}", i), ty: IrType::I64 });
    }

    let mut module = IrModule {
        functions: vec![
            IrFunction {
                id: 0,
                name: "big".into(),
                params: vec![IrParam { name: "x".into(), ty: IrType::I64 }],
                return_type: IrType::I64,
                entry: 0,
                locals,
                blocks: vec![BasicBlock {
                    id: 0,
                    instructions: many_instructions,
                    terminator: Terminator::Return(Some(Operand::Var(0))),
                }],
            },
            IrFunction {
                id: 1,
                name: "main".into(),
                params: vec![],
                return_type: IrType::I64,
                entry: 0,
                locals: vec![
                    IrLocal { id: 0, name: "res".into(), ty: IrType::I64 },
                ],
                blocks: vec![BasicBlock {
                    id: 0,
                    instructions: vec![
                        Instruction::Assign(0, RValue::CallNamed(
                            "big".into(), vec![Operand::Constant(Constant::Int(1))],
                        )),
                    ],
                    terminator: Terminator::Return(Some(Operand::Var(0))),
                }],
            },
        ],
        globals: vec![],
        string_literals: vec![],
        struct_defs: vec![],
    };

    opt::function_inline(&mut module);

    // big() has >= 20 instructions, should NOT be inlined.
    let main_fn = &module.functions[1];
    let has_call = main_fn.blocks.iter().flat_map(|b| &b.instructions).any(|i| {
        matches!(i, Instruction::Assign(_, RValue::CallNamed(name, _)) if name == "big")
    });
    assert!(has_call, "large function should NOT be inlined");
}

#[test]
fn opt_inline_param_substitution() {
    // identity(x) = x; main calls identity(42) → result should use constant 42
    let mut module = IrModule {
        functions: vec![
            IrFunction {
                id: 0,
                name: "identity".into(),
                params: vec![IrParam { name: "x".into(), ty: IrType::I64 }],
                return_type: IrType::I64,
                entry: 0,
                locals: vec![
                    IrLocal { id: 0, name: "x".into(), ty: IrType::I64 },
                ],
                blocks: vec![BasicBlock {
                    id: 0,
                    instructions: vec![],
                    terminator: Terminator::Return(Some(Operand::Var(0))),
                }],
            },
            IrFunction {
                id: 1,
                name: "main".into(),
                params: vec![],
                return_type: IrType::I64,
                entry: 0,
                locals: vec![
                    IrLocal { id: 0, name: "res".into(), ty: IrType::I64 },
                ],
                blocks: vec![BasicBlock {
                    id: 0,
                    instructions: vec![
                        Instruction::Assign(0, RValue::CallNamed(
                            "identity".into(), vec![Operand::Constant(Constant::Int(42))],
                        )),
                    ],
                    terminator: Terminator::Return(Some(Operand::Var(0))),
                }],
            },
        ],
        globals: vec![],
        string_literals: vec![],
        struct_defs: vec![],
    };

    opt::function_inline(&mut module);

    // After inlining identity(42), res should be assigned Use(Constant(42))
    let main_fn = &module.functions[1];
    let has_const_42 = main_fn.blocks.iter().flat_map(|b| &b.instructions).any(|i| {
        matches!(i, Instruction::Assign(0, RValue::Use(Operand::Constant(Constant::Int(42)))))
    });
    assert!(has_const_42, "inlined identity(42) should produce Assign(res, Use(Constant(42)))");

    // Should NOT have a CallNamed anymore.
    let has_call = main_fn.blocks.iter().flat_map(|b| &b.instructions).any(|i| {
        matches!(i, Instruction::Assign(_, RValue::CallNamed(..)))
    });
    assert!(!has_call, "identity call should be fully inlined");
}
