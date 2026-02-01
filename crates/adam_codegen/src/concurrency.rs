//! Concurrency codegen — spawn, select, and channel operations.
//!
//! Generates LLVM IR for Adam's concurrency primitives:
//! - Channel send/recv with correct FFI signatures
//! - `spawn { ... }` → outlines block + calls `__adam_spawn`
//! - `select { ... }` → builds cases + calls `__adam_select`

use std::collections::HashMap;
use std::collections::HashSet;

use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValueEnum, FunctionValue};
use inkwell::AddressSpace;

use adam_ir::ir::*;

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    // ================================================================
    // Channel send
    // ================================================================

    /// Generate code for ChanSend with correct FFI signature.
    ///
    /// Runtime: `__adam_chan_send(ch: ptr, val_ptr: ptr, val_size: usize)`
    pub(crate) fn codegen_chan_send(
        &mut self,
        chan: &Operand,
        val: &Operand,
        local_types: &HashMap<VarId, IrType>,
    ) -> BasicValueEnum<'ctx> {
        let chan_ptr = self.codegen_operand(chan, local_types);
        let send_val = self.codegen_operand(val, local_types);

        // Store value on stack, pass pointer and size.
        let alloca = self.builder.build_alloca(send_val.get_type(), "send_tmp")
            .expect("alloca");
        self.builder.build_store(alloca, send_val).expect("store");

        // Compute value size from the IR type.
        let val_ty = self.operand_ir_type(val, local_types);
        let val_size = self.size_of_type(&val_ty);
        let size_val = self.context.i64_type().const_int(val_size, false);

        let send_fn = self.get_or_declare_void_fn(
            "__adam_chan_send",
            &[
                self.context.ptr_type(AddressSpace::default()).into(),
                self.context.ptr_type(AddressSpace::default()).into(),
                self.context.i64_type().into(),
            ],
        );

        self.builder.build_call(
            send_fn,
            &[chan_ptr.into(), alloca.into(), size_val.into()],
            "",
        ).expect("chan_send call");

        // Return unit.
        self.context.struct_type(&[], false).const_zero().into()
    }

    // ================================================================
    // Channel recv
    // ================================================================

    /// Generate code for ChanRecv with correct FFI signature.
    ///
    /// Runtime: `__adam_chan_recv(ch: ptr, val_ptr: ptr, val_size: usize) -> bool`
    pub(crate) fn codegen_chan_recv(
        &mut self,
        chan: &Operand,
        local_types: &HashMap<VarId, IrType>,
    ) -> BasicValueEnum<'ctx> {
        let chan_ptr = self.codegen_operand(chan, local_types);

        // Determine the element type from the channel type.
        let chan_ty = self.operand_ir_type(chan, local_types);
        let elem_ty = match &chan_ty {
            IrType::Channel(inner) => inner.as_ref().clone(),
            _ => IrType::I64,
        };

        let elem_llvm = self.llvm_type(&elem_ty);
        let val_size = self.size_of_type(&elem_ty);

        // Allocate space for the received value.
        let alloca = self.builder.build_alloca(elem_llvm, "recv_tmp")
            .expect("alloca");
        let size_val = self.context.i64_type().const_int(val_size, false);

        let recv_fn = self.get_or_declare_chan_recv_fn();

        let _result = self.builder.build_call(
            recv_fn,
            &[chan_ptr.into(), alloca.into(), size_val.into()],
            "recv_ok",
        ).expect("chan_recv call");

        // Load the received value from the alloca.
        self.builder.build_load(elem_llvm, alloca, "recv_val")
            .expect("load recv")
    }

    // ================================================================
    // Spawn terminator
    // ================================================================

    /// Generate code for the Spawn terminator.
    ///
    /// Outlines the target block into a separate LLVM function, packages
    /// captured variables into a heap-allocated environment struct, and
    /// calls `__adam_spawn(outlined_fn, env_ptr)`.
    pub(crate) fn codegen_spawn_terminator(
        &mut self,
        target: &BlockId,
        cont: &BlockId,
        func: &IrFunction,
        local_types: &HashMap<VarId, IrType>,
        _llvm_fn: FunctionValue<'ctx>,
    ) {
        // Find the target block.
        let target_block = func.blocks.iter().find(|b| b.id == *target)
            .expect("spawn target block not found");

        // Collect variables read by the target block (these are captures).
        let read_vars = collect_block_read_vars(target_block);
        // Collect variables written by the target block (these need allocas).
        let write_vars = collect_block_write_vars(target_block);

        // All vars that need allocas in the outlined function.
        let all_vars: HashSet<VarId> = read_vars.union(&write_vars).copied().collect();

        // Captures: variables that are read and defined in the parent function.
        let mut captures: Vec<VarId> = read_vars.iter()
            .filter(|v| local_types.contains_key(v))
            .copied()
            .collect();
        captures.sort();

        // Build capture struct type.
        let capture_types: Vec<BasicTypeEnum<'ctx>> = captures.iter()
            .map(|v| self.llvm_type(&local_types[v]))
            .collect();
        let capture_struct_ty = self.context.struct_type(&capture_types, false);

        // Compute capture struct size.
        let capture_size: u64 = captures.iter()
            .map(|v| self.size_of_type(&local_types[v]))
            .sum::<u64>()
            .max(1);

        // Create the outlined function: extern "C" fn(usize)
        let outlined_name = format!("{}__spawn_{}", func.name, target);
        let outlined_fn_ty = self.context.void_type().fn_type(
            &[self.context.i64_type().into()],
            false,
        );
        let outlined_fn = self.module.add_function(
            &outlined_name,
            outlined_fn_ty,
            None,
        );

        // Save current builder position and variable state.
        let saved_block = self.builder.get_insert_block();
        let saved_variables = self.variables.clone();
        let saved_var_types = self.var_value_types.clone();

        // ---- Build the outlined function body ----
        let entry = self.context.append_basic_block(outlined_fn, "entry");
        self.builder.position_at_end(entry);

        let arg = outlined_fn.get_nth_param(0).unwrap().into_int_value();

        // Cast arg to capture struct pointer (if we have captures).
        let env_ptr = if !captures.is_empty() {
            Some(self.builder.build_int_to_ptr(
                arg,
                self.context.ptr_type(AddressSpace::default()),
                "env_ptr",
            ).expect("int_to_ptr"))
        } else {
            None
        };

        // Create allocas for all referenced variables.
        let mut outlined_vars = HashMap::new();
        let mut outlined_var_types = HashMap::new();

        for var_id in &all_vars {
            if let Some(ir_ty) = local_types.get(var_id) {
                let llvm_ty = self.llvm_type(ir_ty);
                let alloca = self.builder.build_alloca(llvm_ty, &format!("v{}", var_id))
                    .expect("alloca");
                outlined_vars.insert(*var_id, alloca);
                outlined_var_types.insert(*var_id, llvm_ty);
            }
        }

        // Load captured variables from the env struct.
        if let Some(env_ptr) = env_ptr {
            for (i, var_id) in captures.iter().enumerate() {
                let var_ty = self.llvm_type(&local_types[var_id]);
                // GEP into the capture struct field.
                let zero = self.context.i32_type().const_zero();
                let idx = self.context.i32_type().const_int(i as u64, false);
                let field_ptr = unsafe {
                    self.builder.build_gep(
                        capture_struct_ty,
                        env_ptr,
                        &[zero, idx],
                        &format!("cap_{}", var_id),
                    ).expect("gep")
                };
                let val = self.builder.build_load(
                    var_ty, field_ptr, &format!("load_cap_{}", var_id),
                ).expect("load");
                self.builder.build_store(outlined_vars[var_id], val)
                    .expect("store");
            }
        }

        // Set up the outlined function's variable environment.
        self.variables = outlined_vars;
        self.var_value_types = outlined_var_types;

        // Emit the target block's instructions.
        for inst in &target_block.instructions {
            self.codegen_instruction(inst, local_types);
        }

        // Free the environment struct (if we had captures).
        if let Some(env_ptr) = env_ptr {
            let dealloc_fn = self.get_or_declare_void_fn(
                "__adam_dealloc",
                &[
                    self.context.ptr_type(AddressSpace::default()).into(),
                    self.context.i64_type().into(),
                    self.context.i64_type().into(),
                ],
            );
            let size_val = self.context.i64_type().const_int(capture_size, false);
            let align_val = self.context.i64_type().const_int(8, false);
            self.builder.build_call(
                dealloc_fn,
                &[env_ptr.into(), size_val.into(), align_val.into()],
                "",
            ).expect("dealloc");
        }

        // Return void (spawned thread will exit).
        self.builder.build_return(None).expect("ret");

        // ---- Restore state ----
        self.variables = saved_variables;
        self.var_value_types = saved_var_types;
        if let Some(bb) = saved_block {
            self.builder.position_at_end(bb);
        }

        // ---- In the parent function: spawn the outlined function ----
        let spawn_fn = self.get_or_declare_spawn_fn();
        let fn_ptr = outlined_fn.as_global_value().as_pointer_value();

        if captures.is_empty() {
            // No captures: pass 0 as arg.
            let arg_val = self.context.i64_type().const_zero();
            self.builder.build_call(
                spawn_fn,
                &[fn_ptr.into(), arg_val.into()],
                "",
            ).expect("spawn call");
        } else {
            // Heap-allocate the capture struct.
            let alloc_fn = self.get_or_declare_runtime_fn(
                "__adam_alloc",
                &[self.context.i64_type().into(), self.context.i64_type().into()],
                self.context.ptr_type(AddressSpace::default()).into(),
            );
            let size_val = self.context.i64_type().const_int(capture_size, false);
            let align_val = self.context.i64_type().const_int(8, false);
            let heap_ptr = {
                let result = self.builder.build_call(
                    alloc_fn,
                    &[size_val.into(), align_val.into()],
                    "env_heap",
                ).expect("alloc");
                match result.try_as_basic_value() {
                    inkwell::values::ValueKind::Basic(val) => val.into_pointer_value(),
                    inkwell::values::ValueKind::Instruction(_) => {
                        self.context.ptr_type(AddressSpace::default()).const_null()
                    }
                }
            };

            // Store captured variables into the heap struct.
            for (i, var_id) in captures.iter().enumerate() {
                let var_ptr = self.variables[var_id];
                let var_ty = self.llvm_type(&local_types[var_id]);
                let val = self.builder.build_load(var_ty, var_ptr, &format!("cap_val_{}", var_id))
                    .expect("load");
                let zero = self.context.i32_type().const_zero();
                let idx = self.context.i32_type().const_int(i as u64, false);
                let field_ptr = unsafe {
                    self.builder.build_gep(
                        capture_struct_ty,
                        heap_ptr,
                        &[zero, idx],
                        &format!("env_field_{}", i),
                    ).expect("gep")
                };
                self.builder.build_store(field_ptr, val).expect("store");
            }

            // Convert pointer to usize for __adam_spawn arg.
            let arg_val = self.builder.build_ptr_to_int(
                heap_ptr,
                self.context.i64_type(),
                "env_arg",
            ).expect("ptr_to_int");

            self.builder.build_call(
                spawn_fn,
                &[fn_ptr.into(), arg_val.into()],
                "",
            ).expect("spawn call");
        }

        // Branch to continuation.
        let cont_bb = self.blocks[cont];
        self.builder.build_unconditional_branch(cont_bb).expect("br");
    }

    // ================================================================
    // Select terminator
    // ================================================================

    /// Generate code for the Select terminator.
    ///
    /// Builds SelectCase pointers, calls `__adam_select`, and switches
    /// on the result to branch to the correct target block.
    pub(crate) fn codegen_select_terminator(
        &mut self,
        branches: &[SelectBranch],
        local_types: &HashMap<VarId, IrType>,
        llvm_fn: FunctionValue<'ctx>,
    ) {
        let ptr_type = self.context.ptr_type(AddressSpace::default());

        // Separate branches into recv/send cases vs timeout.
        let mut cases: Vec<(usize, &SelectBranch)> = Vec::new();
        let mut timeout_ms: u64 = 0;
        let mut timeout_target: Option<BlockId> = None;

        for (i, branch) in branches.iter().enumerate() {
            match &branch.kind {
                SelectBranchKind::Recv(_, _) | SelectBranchKind::Send(_, _) => {
                    cases.push((i, branch));
                }
                SelectBranchKind::After(operand) => {
                    if let Operand::Constant(Constant::Int(ms)) = operand {
                        timeout_ms = *ms as u64;
                    }
                    timeout_target = Some(branch.target);
                }
            }
        }

        let num_cases = cases.len();

        // Determine the max recv value size for the recv buffer.
        let mut max_recv_size: u64 = 0;
        for (_, branch) in &cases {
            if let SelectBranchKind::Recv(_, chan_op) = &branch.kind {
                let chan_ty = self.operand_ir_type(chan_op, local_types);
                if let IrType::Channel(inner) = &chan_ty {
                    let size = self.size_of_type(inner);
                    max_recv_size = max_recv_size.max(size);
                }
            }
        }
        max_recv_size = max_recv_size.max(1);

        // Allocate recv buffer on stack.
        let recv_buf_ty = self.context.i8_type().array_type(max_recv_size as u32);
        let recv_buf = self.builder.build_alloca(recv_buf_ty, "recv_buf")
            .expect("alloca");

        // Build cases array (array of pointers to SelectCase).
        let cases_arr_ty = ptr_type.array_type(num_cases.max(1) as u32);
        let cases_arr = self.builder.build_alloca(cases_arr_ty, "select_cases")
            .expect("alloca");

        // Populate each case.
        for (case_idx, (_, branch)) in cases.iter().enumerate() {
            let case_ptr = match &branch.kind {
                SelectBranchKind::Recv(_, chan_op) => {
                    let chan_ptr = self.codegen_operand(chan_op, local_types);
                    let select_recv_fn = self.get_or_declare_runtime_fn(
                        "__adam_select_recv",
                        &[ptr_type.into()],
                        ptr_type.into(),
                    );
                    let result = self.builder.build_call(
                        select_recv_fn,
                        &[chan_ptr.into()],
                        "select_case",
                    ).expect("call");
                    match result.try_as_basic_value() {
                        inkwell::values::ValueKind::Basic(val) => val,
                        inkwell::values::ValueKind::Instruction(_) => {
                            ptr_type.const_null().into()
                        }
                    }
                }
                SelectBranchKind::Send(chan_op, val_op) => {
                    let chan_ptr = self.codegen_operand(chan_op, local_types);
                    let send_val = self.codegen_operand(val_op, local_types);

                    let val_alloca = self.builder.build_alloca(send_val.get_type(), "sel_send_val")
                        .expect("alloca");
                    self.builder.build_store(val_alloca, send_val).expect("store");

                    let val_ty = self.operand_ir_type(val_op, local_types);
                    let val_size = self.size_of_type(&val_ty);
                    let size_val = self.context.i64_type().const_int(val_size, false);

                    let select_send_fn = self.get_or_declare_runtime_fn(
                        "__adam_select_send",
                        &[ptr_type.into(), ptr_type.into(), self.context.i64_type().into()],
                        ptr_type.into(),
                    );
                    let result = self.builder.build_call(
                        select_send_fn,
                        &[chan_ptr.into(), val_alloca.into(), size_val.into()],
                        "select_case",
                    ).expect("call");
                    match result.try_as_basic_value() {
                        inkwell::values::ValueKind::Basic(val) => val,
                        inkwell::values::ValueKind::Instruction(_) => {
                            ptr_type.const_null().into()
                        }
                    }
                }
                _ => unreachable!(),
            };

            // Store case pointer in the array.
            let zero = self.context.i32_type().const_zero();
            let idx_val = self.context.i32_type().const_int(case_idx as u64, false);
            let slot = unsafe {
                self.builder.build_gep(
                    cases_arr_ty,
                    cases_arr,
                    &[zero, idx_val],
                    "case_slot",
                ).expect("gep")
            };
            self.builder.build_store(slot, case_ptr).expect("store");
        }

        // Get pointer to first element of cases array.
        let zero = self.context.i32_type().const_zero();
        let cases_ptr = unsafe {
            self.builder.build_gep(
                cases_arr_ty,
                cases_arr,
                &[zero, zero],
                "cases_ptr",
            ).expect("gep")
        };

        // Get pointer to recv buffer.
        let recv_buf_ptr = unsafe {
            self.builder.build_gep(
                recv_buf_ty,
                recv_buf,
                &[zero, zero],
                "recv_buf_ptr",
            ).expect("gep")
        };

        // Call __adam_select.
        let select_fn = self.get_or_declare_runtime_fn(
            "__adam_select",
            &[
                ptr_type.into(),                      // cases_ptr
                self.context.i64_type().into(),       // num_cases
                self.context.i64_type().into(),       // timeout_ms
                ptr_type.into(),                      // recv_buf
                self.context.i64_type().into(),       // recv_buf_size
            ],
            self.context.i64_type().into(),
        );

        let num_cases_val = self.context.i64_type().const_int(num_cases as u64, false);
        let timeout_val = self.context.i64_type().const_int(timeout_ms, false);
        let recv_size_val = self.context.i64_type().const_int(max_recv_size, false);

        let selected_idx = {
            let result = self.builder.build_call(
                select_fn,
                &[
                    cases_ptr.into(),
                    num_cases_val.into(),
                    timeout_val.into(),
                    recv_buf_ptr.into(),
                    recv_size_val.into(),
                ],
                "selected",
            ).expect("select call");
            match result.try_as_basic_value() {
                inkwell::values::ValueKind::Basic(val) => val.into_int_value(),
                inkwell::values::ValueKind::Instruction(_) => {
                    self.context.i64_type().const_zero()
                }
            }
        };

        // Default block: timeout target or unreachable.
        let default_bb = if let Some(target) = timeout_target {
            self.blocks[&target]
        } else {
            let unreach_bb = self.context.append_basic_block(llvm_fn, "select_unreach");
            let saved = self.builder.get_insert_block();
            self.builder.position_at_end(unreach_bb);
            self.builder.build_unreachable().expect("unreachable");
            if let Some(bb) = saved {
                self.builder.position_at_end(bb);
            }
            unreach_bb
        };

        // Build switch cases with intermediate blocks for recv.
        let mut switch_cases: Vec<(inkwell::values::IntValue<'ctx>, inkwell::basic_block::BasicBlock<'ctx>)> = Vec::new();

        for (case_idx, (_, branch)) in cases.iter().enumerate() {
            match &branch.kind {
                SelectBranchKind::Recv(var_id, chan_op) => {
                    // Create intermediate block to load received value.
                    let case_bb = self.context.append_basic_block(
                        llvm_fn,
                        &format!("select_recv_{}", case_idx),
                    );
                    let saved = self.builder.get_insert_block();
                    self.builder.position_at_end(case_bb);

                    // Load received value from recv_buf.
                    let chan_ty = self.operand_ir_type(chan_op, local_types);
                    let elem_ty = match &chan_ty {
                        IrType::Channel(inner) => inner.as_ref().clone(),
                        _ => IrType::I64,
                    };
                    let elem_llvm = self.llvm_type(&elem_ty);
                    let val = self.builder.build_load(elem_llvm, recv_buf_ptr, "recv_val")
                        .expect("load");

                    // Store into the destination variable.
                    if let Some(var_ptr) = self.variables.get(var_id) {
                        self.builder.build_store(*var_ptr, val).expect("store");
                        self.var_value_types.insert(*var_id, val.get_type());
                    }

                    let target_bb = self.blocks[&branch.target];
                    self.builder.build_unconditional_branch(target_bb).expect("br");

                    if let Some(bb) = saved {
                        self.builder.position_at_end(bb);
                    }
                    switch_cases.push((
                        self.context.i64_type().const_int(case_idx as u64, false),
                        case_bb,
                    ));
                }
                SelectBranchKind::Send(_, _) => {
                    let target_bb = self.blocks[&branch.target];
                    switch_cases.push((
                        self.context.i64_type().const_int(case_idx as u64, false),
                        target_bb,
                    ));
                }
                _ => unreachable!(),
            }
        }

        self.builder.build_switch(selected_idx, default_bb, &switch_cases)
            .expect("switch");
    }

    // ================================================================
    // Helper: declare runtime functions
    // ================================================================

    fn get_or_declare_spawn_fn(&self) -> FunctionValue<'ctx> {
        let name = "__adam_spawn";
        if let Some(f) = self.module.get_function(name) {
            return f;
        }
        let fn_type = self.context.void_type().fn_type(
            &[
                self.context.ptr_type(AddressSpace::default()).into(),
                self.context.i64_type().into(),
            ],
            false,
        );
        self.module.add_function(name, fn_type, Some(inkwell::module::Linkage::External))
    }

    fn get_or_declare_chan_recv_fn(&self) -> FunctionValue<'ctx> {
        let name = "__adam_chan_recv";
        if let Some(f) = self.module.get_function(name) {
            return f;
        }
        let fn_type = self.context.bool_type().fn_type(
            &[
                self.context.ptr_type(AddressSpace::default()).into(),
                self.context.ptr_type(AddressSpace::default()).into(),
                self.context.i64_type().into(),
            ],
            false,
        );
        self.module.add_function(name, fn_type, Some(inkwell::module::Linkage::External))
    }

    /// Get or declare a runtime function with explicit param and return types.
    pub(crate) fn get_or_declare_runtime_fn(
        &self,
        name: &str,
        param_types: &[inkwell::types::BasicMetadataTypeEnum<'ctx>],
        ret_type: BasicTypeEnum<'ctx>,
    ) -> FunctionValue<'ctx> {
        if let Some(f) = self.module.get_function(name) {
            return f;
        }
        let fn_type = ret_type.fn_type(param_types, false);
        self.module.add_function(name, fn_type, Some(inkwell::module::Linkage::External))
    }
}

// ================================================================
// Variable collection helpers (free functions)
// ================================================================

/// Collect all VarIds that are READ (appear as operands) in a block.
fn collect_block_read_vars(block: &adam_ir::ir::BasicBlock) -> HashSet<VarId> {
    let mut vars = HashSet::new();
    for inst in &block.instructions {
        match inst {
            Instruction::Assign(_, rvalue) => {
                collect_rvalue_read_vars(rvalue, &mut vars);
            }
            Instruction::Drop(v) => { vars.insert(*v); }
            Instruction::Nop => {}
        }
    }
    collect_terminator_read_vars(&block.terminator, &mut vars);
    vars
}

/// Collect all VarIds that are WRITTEN (appear as Assign targets) in a block.
fn collect_block_write_vars(block: &adam_ir::ir::BasicBlock) -> HashSet<VarId> {
    let mut vars = HashSet::new();
    for inst in &block.instructions {
        if let Instruction::Assign(v, _) = inst {
            vars.insert(*v);
        }
    }
    vars
}

fn collect_rvalue_read_vars(rvalue: &RValue, vars: &mut HashSet<VarId>) {
    match rvalue {
        RValue::Use(op) => collect_operand_vars(op, vars),
        RValue::BinaryOp(_, l, r) => {
            collect_operand_vars(l, vars);
            collect_operand_vars(r, vars);
        }
        RValue::UnaryOp(_, op) => collect_operand_vars(op, vars),
        RValue::Call(_, args) | RValue::CallNamed(_, args) => {
            for a in args { collect_operand_vars(a, vars); }
        }
        RValue::Aggregate(_, fields) => {
            for f in fields { collect_operand_vars(f, vars); }
        }
        RValue::Field(op, _) => collect_operand_vars(op, vars),
        RValue::Index(base, idx) => {
            collect_operand_vars(base, vars);
            collect_operand_vars(idx, vars);
        }
        RValue::Ref(v) | RValue::MutRef(v) => { vars.insert(*v); }
        RValue::Deref(op) => collect_operand_vars(op, vars),
        RValue::Cast(op, _) => collect_operand_vars(op, vars),
        RValue::ChanSend(ch, val) => {
            collect_operand_vars(ch, vars);
            collect_operand_vars(val, vars);
        }
        RValue::ChanRecv(ch) => collect_operand_vars(ch, vars),
        RValue::Constant(_) | RValue::HeapAlloc(_) | RValue::ChanCreate(_, _) => {}
    }
}

fn collect_operand_vars(operand: &Operand, vars: &mut HashSet<VarId>) {
    if let Operand::Var(v) = operand {
        vars.insert(*v);
    }
}

fn collect_terminator_read_vars(term: &Terminator, vars: &mut HashSet<VarId>) {
    match term {
        Terminator::Return(Some(op)) => collect_operand_vars(op, vars),
        Terminator::Branch(cond, _, _) => collect_operand_vars(cond, vars),
        Terminator::Switch(op, _, _) => collect_operand_vars(op, vars),
        Terminator::Select(branches) => {
            for branch in branches {
                match &branch.kind {
                    SelectBranchKind::Recv(_, ch) => collect_operand_vars(ch, vars),
                    SelectBranchKind::Send(ch, val) => {
                        collect_operand_vars(ch, vars);
                        collect_operand_vars(val, vars);
                    }
                    SelectBranchKind::After(op) => collect_operand_vars(op, vars),
                }
            }
        }
        _ => {}
    }
}
