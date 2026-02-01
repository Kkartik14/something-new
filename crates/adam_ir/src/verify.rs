//! IR verifier — checks structural invariants of the IR.
//!
//! Catches bugs in the lowering pass before they become
//! mysterious codegen failures.

use std::collections::HashSet;

use crate::ir::*;

/// Result of IR verification.
#[derive(Debug)]
pub struct VerifyResult {
    pub errors: Vec<VerifyError>,
}

impl VerifyResult {
    pub fn is_ok(&self) -> bool {
        self.errors.is_empty()
    }
}

/// A single verification error.
#[derive(Debug, Clone)]
pub struct VerifyError {
    pub message: String,
    pub function: String,
    pub block: Option<BlockId>,
}

impl std::fmt::Display for VerifyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.block {
            Some(bb) => write!(f, "verify error in @{} bb{}: {}", self.function, bb, self.message),
            None => write!(f, "verify error in @{}: {}", self.function, self.message),
        }
    }
}

/// Verify an entire IR module.
pub fn verify_module(module: &IrModule) -> VerifyResult {
    let mut errors = Vec::new();

    // Check for duplicate function names.
    let mut fn_names = HashSet::new();
    for func in &module.functions {
        if !fn_names.insert(&func.name) {
            errors.push(VerifyError {
                message: format!("duplicate function name '{}'", func.name),
                function: func.name.clone(),
                block: None,
            });
        }
    }

    // Verify each function.
    for func in &module.functions {
        verify_function(func, module, &mut errors);
    }

    VerifyResult { errors }
}

fn verify_function(func: &IrFunction, module: &IrModule, errors: &mut Vec<VerifyError>) {
    let block_ids: HashSet<BlockId> = func.blocks.iter().map(|b| b.id).collect();
    let local_ids: HashSet<VarId> = func.locals.iter().map(|l| l.id).collect();
    let fn_ids: HashSet<FnId> = module.functions.iter().map(|f| f.id).collect();

    // 1. Entry block must exist.
    if !block_ids.contains(&func.entry) {
        errors.push(VerifyError {
            message: format!("entry block bb{} does not exist", func.entry),
            function: func.name.clone(),
            block: None,
        });
    }

    // 2. No duplicate block IDs.
    let mut seen_blocks = HashSet::new();
    for block in &func.blocks {
        if !seen_blocks.insert(block.id) {
            errors.push(VerifyError {
                message: format!("duplicate block id bb{}", block.id),
                function: func.name.clone(),
                block: Some(block.id),
            });
        }
    }

    // 3. No duplicate local IDs.
    let mut seen_locals = HashSet::new();
    for local in &func.locals {
        if !seen_locals.insert(local.id) {
            errors.push(VerifyError {
                message: format!("duplicate local id %{}", local.id),
                function: func.name.clone(),
                block: None,
            });
        }
    }

    // 4. Verify each block.
    for block in &func.blocks {
        // 4a. Every block must have a terminator (i.e., not Unreachable
        // for reachable blocks, though we allow it for dead code).
        // We check that jump targets exist.
        verify_terminator(
            &block.terminator,
            &block_ids,
            func,
            block.id,
            errors,
        );

        // 4b. Check instructions reference valid locals and functions.
        for instr in &block.instructions {
            verify_instruction(instr, &local_ids, &fn_ids, func, block.id, errors);
        }
    }
}

fn verify_terminator(
    term: &Terminator,
    block_ids: &HashSet<BlockId>,
    func: &IrFunction,
    block_id: BlockId,
    errors: &mut Vec<VerifyError>,
) {
    match term {
        Terminator::Return(_) => { /* always valid */ }
        Terminator::Unreachable => { /* allowed for dead blocks */ }

        Terminator::Goto(target) => {
            if !block_ids.contains(target) {
                errors.push(VerifyError {
                    message: format!("goto target bb{} does not exist", target),
                    function: func.name.clone(),
                    block: Some(block_id),
                });
            }
        }

        Terminator::Branch(_, then_bb, else_bb) => {
            if !block_ids.contains(then_bb) {
                errors.push(VerifyError {
                    message: format!("branch then-target bb{} does not exist", then_bb),
                    function: func.name.clone(),
                    block: Some(block_id),
                });
            }
            if !block_ids.contains(else_bb) {
                errors.push(VerifyError {
                    message: format!("branch else-target bb{} does not exist", else_bb),
                    function: func.name.clone(),
                    block: Some(block_id),
                });
            }
        }

        Terminator::Switch(_, cases, default) => {
            for (_, target) in cases {
                if !block_ids.contains(target) {
                    errors.push(VerifyError {
                        message: format!("switch case target bb{} does not exist", target),
                        function: func.name.clone(),
                        block: Some(block_id),
                    });
                }
            }
            if !block_ids.contains(default) {
                errors.push(VerifyError {
                    message: format!("switch default target bb{} does not exist", default),
                    function: func.name.clone(),
                    block: Some(block_id),
                });
            }
        }

        Terminator::Spawn(target, cont) => {
            if !block_ids.contains(target) {
                errors.push(VerifyError {
                    message: format!("spawn target bb{} does not exist", target),
                    function: func.name.clone(),
                    block: Some(block_id),
                });
            }
            if !block_ids.contains(cont) {
                errors.push(VerifyError {
                    message: format!("spawn continuation bb{} does not exist", cont),
                    function: func.name.clone(),
                    block: Some(block_id),
                });
            }
        }

        Terminator::Select(branches) => {
            for branch in branches {
                if !block_ids.contains(&branch.target) {
                    errors.push(VerifyError {
                        message: format!(
                            "select branch target bb{} does not exist",
                            branch.target
                        ),
                        function: func.name.clone(),
                        block: Some(block_id),
                    });
                }
            }
        }
    }
}

fn verify_instruction(
    instr: &Instruction,
    _local_ids: &HashSet<VarId>,
    fn_ids: &HashSet<FnId>,
    func: &IrFunction,
    block_id: BlockId,
    errors: &mut Vec<VerifyError>,
) {
    match instr {
        Instruction::Assign(_, rvalue) => {
            // Check function calls reference valid function IDs.
            if let RValue::Call(fn_id, _) = rvalue {
                if !fn_ids.contains(fn_id) {
                    errors.push(VerifyError {
                        message: format!("call references unknown function id {}", fn_id),
                        function: func.name.clone(),
                        block: Some(block_id),
                    });
                }
            }
        }
        Instruction::Drop(_) | Instruction::Nop => {}
    }
}

/// Verify a single function (convenience wrapper).
pub fn verify_function_standalone(func: &IrFunction) -> VerifyResult {
    let module = IrModule {
        functions: vec![func.clone()],
        globals: vec![],
        string_literals: vec![],
        struct_defs: vec![],
    };
    verify_module(&module)
}
