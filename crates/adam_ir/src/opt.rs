//! Basic optimization passes on the IR.
//!
//! These are simple, safe transformations that improve code quality
//! without requiring complex analysis.

use std::collections::HashSet;

use crate::ir::*;

// ================================================================
// Constant folding
// ================================================================

/// Evaluate constant binary operations at compile time.
pub fn constant_fold(module: &mut IrModule) {
    for func in &mut module.functions {
        constant_fold_function(func);
    }
}

fn constant_fold_function(func: &mut IrFunction) {
    for block in &mut func.blocks {
        for instr in &mut block.instructions {
            if let Instruction::Assign(_, rvalue) = instr {
                if let Some(folded) = try_fold_rvalue(rvalue) {
                    *rvalue = RValue::Constant(folded);
                }
            }
        }
    }
}

fn try_fold_rvalue(rv: &RValue) -> Option<Constant> {
    match rv {
        RValue::BinaryOp(op, Operand::Constant(left), Operand::Constant(right)) => {
            fold_binary(*op, left, right)
        }
        RValue::UnaryOp(op, Operand::Constant(operand)) => fold_unary(*op, operand),
        _ => None,
    }
}

fn fold_binary(op: BinOp, left: &Constant, right: &Constant) -> Option<Constant> {
    match (left, right) {
        // Integer operations.
        (Constant::Int(a), Constant::Int(b)) => {
            match op {
                BinOp::Add => Some(Constant::Int(a.wrapping_add(*b))),
                BinOp::Sub => Some(Constant::Int(a.wrapping_sub(*b))),
                BinOp::Mul => Some(Constant::Int(a.wrapping_mul(*b))),
                BinOp::Div => {
                    if *b != 0 {
                        Some(Constant::Int(a.wrapping_div(*b)))
                    } else {
                        None // don't fold division by zero
                    }
                }
                BinOp::Mod => {
                    if *b != 0 {
                        Some(Constant::Int(a.wrapping_rem(*b)))
                    } else {
                        None
                    }
                }
                BinOp::Eq => Some(Constant::Bool(a == b)),
                BinOp::NotEq => Some(Constant::Bool(a != b)),
                BinOp::Lt => Some(Constant::Bool(a < b)),
                BinOp::Gt => Some(Constant::Bool(a > b)),
                BinOp::LtEq => Some(Constant::Bool(a <= b)),
                BinOp::GtEq => Some(Constant::Bool(a >= b)),
                _ => None,
            }
        }

        // Float operations.
        (Constant::Float(a), Constant::Float(b)) => {
            match op {
                BinOp::Add => Some(Constant::Float(a + b)),
                BinOp::Sub => Some(Constant::Float(a - b)),
                BinOp::Mul => Some(Constant::Float(a * b)),
                BinOp::Div => {
                    if *b != 0.0 {
                        Some(Constant::Float(a / b))
                    } else {
                        None
                    }
                }
                BinOp::Eq => Some(Constant::Bool(a == b)),
                BinOp::NotEq => Some(Constant::Bool(a != b)),
                BinOp::Lt => Some(Constant::Bool(a < b)),
                BinOp::Gt => Some(Constant::Bool(a > b)),
                BinOp::LtEq => Some(Constant::Bool(a <= b)),
                BinOp::GtEq => Some(Constant::Bool(a >= b)),
                _ => None,
            }
        }

        // Boolean operations.
        (Constant::Bool(a), Constant::Bool(b)) => {
            match op {
                BinOp::And => Some(Constant::Bool(*a && *b)),
                BinOp::Or => Some(Constant::Bool(*a || *b)),
                BinOp::Eq => Some(Constant::Bool(a == b)),
                BinOp::NotEq => Some(Constant::Bool(a != b)),
                _ => None,
            }
        }

        _ => None,
    }
}

fn fold_unary(op: UnOp, operand: &Constant) -> Option<Constant> {
    match (op, operand) {
        (UnOp::Neg, Constant::Int(n)) => n.checked_neg().map(Constant::Int),
        (UnOp::Neg, Constant::Float(f)) => Some(Constant::Float(-f)),
        (UnOp::Not, Constant::Bool(b)) => Some(Constant::Bool(!b)),
        _ => None,
    }
}

// ================================================================
// Dead code elimination
// ================================================================

/// Remove unreachable blocks — blocks that are not the entry block
/// and have no incoming edges from any reachable block.
pub fn dead_code(module: &mut IrModule) {
    for func in &mut module.functions {
        dead_code_function(func);
    }
}

fn dead_code_function(func: &mut IrFunction) {
    // BFS/DFS from entry block to find reachable blocks.
    let reachable = find_reachable_blocks(func);

    // Remove unreachable blocks.
    func.blocks.retain(|b| reachable.contains(&b.id));
}

fn find_reachable_blocks(func: &IrFunction) -> HashSet<BlockId> {
    let mut reachable = HashSet::new();
    let mut worklist = vec![func.entry];

    while let Some(block_id) = worklist.pop() {
        if !reachable.insert(block_id) {
            continue; // already visited
        }

        // Find the block and its successors.
        if let Some(block) = func.blocks.iter().find(|b| b.id == block_id) {
            for succ in terminator_successors(&block.terminator) {
                if !reachable.contains(&succ) {
                    worklist.push(succ);
                }
            }
        }
    }

    reachable
}

fn terminator_successors(term: &Terminator) -> Vec<BlockId> {
    match term {
        Terminator::Return(_) | Terminator::Unreachable => vec![],
        Terminator::Goto(target) => vec![*target],
        Terminator::Branch(_, then_bb, else_bb) => vec![*then_bb, *else_bb],
        Terminator::Switch(_, cases, default) => {
            let mut succs: Vec<BlockId> = cases.iter().map(|(_, bb)| *bb).collect();
            succs.push(*default);
            succs
        }
        Terminator::Spawn(target, cont) => vec![*target, *cont],
        Terminator::Select(branches) => branches.iter().map(|b| b.target).collect(),
    }
}

// ================================================================
// CFG simplification
// ================================================================

/// Merge blocks with a single predecessor and single successor
/// when the predecessor's terminator is an unconditional goto.
pub fn simplify_cfg(module: &mut IrModule) {
    for func in &mut module.functions {
        simplify_cfg_function(func);
    }
}

fn simplify_cfg_function(func: &mut IrFunction) {
    // Repeat until no more merges are possible.
    loop {
        let mut merged = false;

        // Build predecessor count map.
        let mut pred_count = std::collections::HashMap::<BlockId, usize>::new();
        for block in &func.blocks {
            // Ensure every block appears in the map.
            pred_count.entry(block.id).or_insert(0);
            for succ in terminator_successors(&block.terminator) {
                *pred_count.entry(succ).or_insert(0) += 1;
            }
        }

        // Find a block that goes unconditionally to a successor
        // that has exactly one predecessor.
        let mut merge_pair: Option<(BlockId, BlockId)> = None;
        for block in &func.blocks {
            if let Terminator::Goto(target) = &block.terminator {
                let target_id = *target;
                // Don't merge a block with itself (infinite loop).
                if target_id != block.id {
                    if pred_count.get(&target_id) == Some(&1) {
                        // Don't merge if the target is the entry block.
                        if target_id != func.entry {
                            merge_pair = Some((block.id, target_id));
                            break;
                        }
                    }
                }
            }
        }

        if let Some((pred_id, succ_id)) = merge_pair {
            // Find the successor block and take its contents.
            let succ_idx = func.blocks.iter().position(|b| b.id == succ_id).unwrap();
            let succ_block = func.blocks.remove(succ_idx);

            // Find the predecessor block and append successor's contents.
            let pred_idx = func.blocks.iter().position(|b| b.id == pred_id).unwrap();
            func.blocks[pred_idx]
                .instructions
                .extend(succ_block.instructions);
            func.blocks[pred_idx].terminator = succ_block.terminator;

            merged = true;
        }

        if !merged {
            break;
        }
    }
}

/// Remove Nop instructions from all blocks.
pub fn remove_nops(module: &mut IrModule) {
    for func in &mut module.functions {
        for block in &mut func.blocks {
            block.instructions.retain(|i| !matches!(i, Instruction::Nop));
        }
    }
}

/// Run all optimization passes in sequence.
pub fn optimize(module: &mut IrModule) {
    constant_fold(module);
    dead_code(module);
    simplify_cfg(module);
    remove_nops(module);
}
