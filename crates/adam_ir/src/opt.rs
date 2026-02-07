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

// ================================================================
// Copy propagation
// ================================================================

/// For `Assign(dst, Use(Var(src)))`, replace subsequent uses of `dst`
/// with `src` if `src` is not reassigned before those uses.
pub fn copy_propagation(module: &mut IrModule) {
    for func in &mut module.functions {
        copy_propagation_function(func);
    }
}

fn copy_propagation_function(func: &mut IrFunction) {
    use std::collections::HashMap;

    // Only propagate within a single basic block (local copy prop).
    for block in &mut func.blocks {
        // Map: dst -> src (valid copies).
        let mut copies: HashMap<VarId, VarId> = HashMap::new();

        for instr in &mut block.instructions {
            // First, apply existing copy mappings to this instruction's operands.
            replace_operands_in_instruction(instr, &copies);

            match instr {
                Instruction::Assign(dst, RValue::Use(Operand::Var(src))) => {
                    // New copy: dst = src. Record it.
                    let resolved_src = *copies.get(src).unwrap_or(src);
                    copies.insert(*dst, resolved_src);
                }
                Instruction::Assign(dst, _) => {
                    // dst is reassigned to something else — invalidate it as a copy.
                    copies.remove(dst);
                    // Also invalidate any copies that used dst as their source.
                    copies.retain(|_, src| *src != *dst);
                }
                _ => {}
            }
        }

        // Also apply to the terminator operands.
        replace_operands_in_terminator(&mut block.terminator, &copies);
    }
}

fn replace_operands_in_instruction(
    instr: &mut Instruction,
    copies: &std::collections::HashMap<VarId, VarId>,
) {
    match instr {
        Instruction::Assign(_, rvalue) => {
            replace_operands_in_rvalue(rvalue, copies);
        }
        Instruction::Drop(_) | Instruction::Nop => {}
    }
}

fn replace_operands_in_rvalue(
    rvalue: &mut RValue,
    copies: &std::collections::HashMap<VarId, VarId>,
) {
    match rvalue {
        RValue::Use(op) => replace_operand(op, copies),
        RValue::BinaryOp(_, a, b) => {
            replace_operand(a, copies);
            replace_operand(b, copies);
        }
        RValue::UnaryOp(_, op) => replace_operand(op, copies),
        RValue::Call(_, args) | RValue::CallNamed(_, args) | RValue::Aggregate(_, args) => {
            for arg in args {
                replace_operand(arg, copies);
            }
        }
        RValue::Field(op, _) => replace_operand(op, copies),
        RValue::Index(a, b) => {
            replace_operand(a, copies);
            replace_operand(b, copies);
        }
        RValue::Deref(op) | RValue::Cast(op, _) => replace_operand(op, copies),
        RValue::ChanSend(a, b) => {
            replace_operand(a, copies);
            replace_operand(b, copies);
        }
        RValue::ChanRecv(op) => replace_operand(op, copies),
        RValue::Constant(_) | RValue::HeapAlloc(_) | RValue::ChanCreate(_, _)
        | RValue::Ref(_) | RValue::MutRef(_) => {}
    }
}

fn replace_operand(op: &mut Operand, copies: &std::collections::HashMap<VarId, VarId>) {
    if let Operand::Var(var) = op {
        if let Some(&src) = copies.get(var) {
            *var = src;
        }
    }
}

fn replace_operands_in_terminator(
    term: &mut Terminator,
    copies: &std::collections::HashMap<VarId, VarId>,
) {
    match term {
        Terminator::Return(Some(op)) => replace_operand(op, copies),
        Terminator::Branch(op, _, _) => replace_operand(op, copies),
        Terminator::Switch(op, _, _) => replace_operand(op, copies),
        _ => {}
    }
}

// ================================================================
// Constant propagation
// ================================================================

/// For `Assign(var, Constant(c))`, replace subsequent uses of `var`
/// with the constant `c` if `var` is not reassigned.
pub fn constant_propagation(module: &mut IrModule) {
    for func in &mut module.functions {
        constant_propagation_function(func);
    }
}

fn constant_propagation_function(func: &mut IrFunction) {
    use std::collections::HashMap;

    // Local constant prop within each basic block.
    for block in &mut func.blocks {
        let mut constants: HashMap<VarId, Constant> = HashMap::new();

        for instr in &mut block.instructions {
            // Apply known constants to operands.
            replace_const_operands_in_instruction(instr, &constants);

            match instr {
                Instruction::Assign(dst, RValue::Constant(c)) => {
                    constants.insert(*dst, c.clone());
                }
                Instruction::Assign(dst, _) => {
                    // dst is reassigned — no longer a known constant.
                    constants.remove(dst);
                }
                _ => {}
            }
        }

        // Apply to terminator.
        replace_const_operands_in_terminator(&mut block.terminator, &constants);
    }
}

fn replace_const_operands_in_instruction(
    instr: &mut Instruction,
    constants: &std::collections::HashMap<VarId, Constant>,
) {
    if let Instruction::Assign(_, rvalue) = instr {
        replace_const_operands_in_rvalue(rvalue, constants);
    }
}

fn replace_const_operands_in_rvalue(
    rvalue: &mut RValue,
    constants: &std::collections::HashMap<VarId, Constant>,
) {
    match rvalue {
        RValue::Use(op) => replace_const_operand(op, constants),
        RValue::BinaryOp(_, a, b) => {
            replace_const_operand(a, constants);
            replace_const_operand(b, constants);
        }
        RValue::UnaryOp(_, op) => replace_const_operand(op, constants),
        RValue::Call(_, args) | RValue::CallNamed(_, args) | RValue::Aggregate(_, args) => {
            for arg in args {
                replace_const_operand(arg, constants);
            }
        }
        RValue::Field(op, _) => replace_const_operand(op, constants),
        RValue::Index(a, b) => {
            replace_const_operand(a, constants);
            replace_const_operand(b, constants);
        }
        RValue::Deref(op) | RValue::Cast(op, _) => replace_const_operand(op, constants),
        RValue::ChanSend(a, b) => {
            replace_const_operand(a, constants);
            replace_const_operand(b, constants);
        }
        RValue::ChanRecv(op) => replace_const_operand(op, constants),
        RValue::Constant(_) | RValue::HeapAlloc(_) | RValue::ChanCreate(_, _)
        | RValue::Ref(_) | RValue::MutRef(_) => {}
    }
}

fn replace_const_operand(
    op: &mut Operand,
    constants: &std::collections::HashMap<VarId, Constant>,
) {
    if let Operand::Var(var) = op {
        if let Some(c) = constants.get(var) {
            *op = Operand::Constant(c.clone());
        }
    }
}

fn replace_const_operands_in_terminator(
    term: &mut Terminator,
    constants: &std::collections::HashMap<VarId, Constant>,
) {
    match term {
        Terminator::Return(Some(op)) => replace_const_operand(op, constants),
        Terminator::Branch(op, _, _) => replace_const_operand(op, constants),
        Terminator::Switch(op, _, _) => replace_const_operand(op, constants),
        _ => {}
    }
}

// ================================================================
// Function inlining
// ================================================================

/// Inline small, non-recursive functions at their call sites.
/// Only inlines single-block functions with < 20 instructions.
pub fn function_inline(module: &mut IrModule) {
    use std::collections::HashMap;

    // Phase 1: Identify inlineable functions.
    let mut inlineable: HashMap<String, usize> = HashMap::new(); // name -> fn index
    for (idx, func) in module.functions.iter().enumerate() {
        // Single block, < 20 instructions, ends with Return.
        if func.blocks.len() == 1
            && func.blocks[0].instructions.len() < 20
            && matches!(func.blocks[0].terminator, Terminator::Return(_))
        {
            // Check it's not recursive (doesn't call itself).
            let is_recursive = func.blocks[0].instructions.iter().any(|i| {
                matches!(i, Instruction::Assign(_, RValue::CallNamed(name, _)) if name == &func.name)
            });
            if !is_recursive {
                inlineable.insert(func.name.clone(), idx);
            }
        }
    }

    // Phase 2: For each function, inline calls to inlineable functions.
    // We need to collect changes first, then apply (to avoid borrow issues).
    let callee_data: HashMap<String, (Vec<IrParam>, Vec<Instruction>, Option<Operand>, Vec<IrLocal>)> =
        inlineable.iter().map(|(name, &idx)| {
            let func = &module.functions[idx];
            let ret_operand = if let Terminator::Return(op) = &func.blocks[0].terminator {
                op.clone()
            } else {
                None
            };
            (
                name.clone(),
                (
                    func.params.clone(),
                    func.blocks[0].instructions.clone(),
                    ret_operand,
                    func.locals.clone(),
                ),
            )
        }).collect();

    for func in &mut module.functions {
        let mut max_var_id = func.locals.iter().map(|l| l.id).max().unwrap_or(0);

        for block in &mut func.blocks {
            let mut new_instructions = Vec::new();

            for instr in block.instructions.drain(..) {
                if let Instruction::Assign(dst, RValue::CallNamed(ref callee_name, ref args)) = instr {
                    if let Some((params, callee_instrs, ret_op, callee_locals)) = callee_data.get(callee_name) {
                        // Build var remapping: param vars -> argument operands, local vars -> fresh vars.
                        let var_offset = max_var_id + 1;

                        // Map callee param indices to argument operands.
                        let param_to_arg: HashMap<VarId, Operand> = callee_locals.iter()
                            .enumerate()
                            .filter(|(i, _)| *i < params.len())
                            .map(|(i, local)| (local.id, args.get(i).cloned().unwrap_or(Operand::Constant(Constant::Unit))))
                            .collect();

                        // Map callee local vars to fresh vars (offset by var_offset).
                        let var_remap: HashMap<VarId, VarId> = callee_locals.iter()
                            .filter(|l| !param_to_arg.contains_key(&l.id))
                            .map(|l| (l.id, l.id + var_offset))
                            .collect();

                        max_var_id += callee_locals.len() as u32 + 1;

                        // Clone and remap instructions.
                        for callee_instr in callee_instrs {
                            let remapped = remap_instruction(callee_instr, &param_to_arg, &var_remap);
                            new_instructions.push(remapped);
                        }

                        // Map the return value to the destination.
                        if let Some(ret) = ret_op {
                            let remapped_ret = remap_operand(ret, &param_to_arg, &var_remap);
                            new_instructions.push(Instruction::Assign(dst, RValue::Use(remapped_ret)));
                        }

                        continue; // Skip original call instruction.
                    }
                }
                new_instructions.push(instr);
            }

            block.instructions = new_instructions;
        }
    }
}

fn remap_instruction(
    instr: &Instruction,
    param_to_arg: &std::collections::HashMap<VarId, Operand>,
    var_remap: &std::collections::HashMap<VarId, VarId>,
) -> Instruction {
    match instr {
        Instruction::Assign(dst, rvalue) => {
            let new_dst = var_remap.get(dst).copied().unwrap_or(*dst);
            let new_rvalue = remap_rvalue(rvalue, param_to_arg, var_remap);
            Instruction::Assign(new_dst, new_rvalue)
        }
        Instruction::Drop(v) => {
            let new_v = var_remap.get(v).copied().unwrap_or(*v);
            Instruction::Drop(new_v)
        }
        Instruction::Nop => Instruction::Nop,
    }
}

fn remap_rvalue(
    rvalue: &RValue,
    param_to_arg: &std::collections::HashMap<VarId, Operand>,
    var_remap: &std::collections::HashMap<VarId, VarId>,
) -> RValue {
    match rvalue {
        RValue::Use(op) => RValue::Use(remap_operand(op, param_to_arg, var_remap)),
        RValue::BinaryOp(op, a, b) => RValue::BinaryOp(
            *op,
            remap_operand(a, param_to_arg, var_remap),
            remap_operand(b, param_to_arg, var_remap),
        ),
        RValue::UnaryOp(op, a) => RValue::UnaryOp(*op, remap_operand(a, param_to_arg, var_remap)),
        RValue::Constant(c) => RValue::Constant(c.clone()),
        RValue::Call(id, args) => RValue::Call(
            *id,
            args.iter().map(|a| remap_operand(a, param_to_arg, var_remap)).collect(),
        ),
        RValue::CallNamed(name, args) => RValue::CallNamed(
            name.clone(),
            args.iter().map(|a| remap_operand(a, param_to_arg, var_remap)).collect(),
        ),
        RValue::Aggregate(kind, args) => RValue::Aggregate(
            kind.clone(),
            args.iter().map(|a| remap_operand(a, param_to_arg, var_remap)).collect(),
        ),
        RValue::Field(op, idx) => RValue::Field(remap_operand(op, param_to_arg, var_remap), *idx),
        RValue::Index(a, b) => RValue::Index(
            remap_operand(a, param_to_arg, var_remap),
            remap_operand(b, param_to_arg, var_remap),
        ),
        RValue::Ref(v) => RValue::Ref(var_remap.get(v).copied().unwrap_or(*v)),
        RValue::MutRef(v) => RValue::MutRef(var_remap.get(v).copied().unwrap_or(*v)),
        RValue::Deref(op) => RValue::Deref(remap_operand(op, param_to_arg, var_remap)),
        RValue::Cast(op, ty) => RValue::Cast(remap_operand(op, param_to_arg, var_remap), ty.clone()),
        RValue::HeapAlloc(ty) => RValue::HeapAlloc(ty.clone()),
        RValue::ChanCreate(ty, cap) => RValue::ChanCreate(ty.clone(), *cap),
        RValue::ChanSend(a, b) => RValue::ChanSend(
            remap_operand(a, param_to_arg, var_remap),
            remap_operand(b, param_to_arg, var_remap),
        ),
        RValue::ChanRecv(op) => RValue::ChanRecv(remap_operand(op, param_to_arg, var_remap)),
    }
}

fn remap_operand(
    op: &Operand,
    param_to_arg: &std::collections::HashMap<VarId, Operand>,
    var_remap: &std::collections::HashMap<VarId, VarId>,
) -> Operand {
    match op {
        Operand::Var(v) => {
            // First check if it's a parameter that maps to an argument.
            if let Some(arg) = param_to_arg.get(v) {
                return arg.clone();
            }
            // Then check if it's a local that needs remapping.
            if let Some(&new_v) = var_remap.get(v) {
                return Operand::Var(new_v);
            }
            Operand::Var(*v)
        }
        Operand::Constant(c) => Operand::Constant(c.clone()),
    }
}

/// Run all optimization passes in a fixed-point loop until stable.
pub fn optimize(module: &mut IrModule) {
    function_inline(module);
    // Run passes in a loop to reach a fixed point.
    for _ in 0..4 {
        constant_fold(module);
        copy_propagation(module);
        constant_propagation(module);
        constant_fold(module);
    }
    dead_code(module);
    simplify_cfg(module);
    remove_nops(module);
}
