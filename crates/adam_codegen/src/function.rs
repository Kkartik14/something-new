use std::collections::HashMap;

use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, FloatValue};
use inkwell::IntPredicate;
use inkwell::FloatPredicate;
use inkwell::AddressSpace;

use adam_ir::ir::*;

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    // ================================================================
    // Module-level codegen
    // ================================================================

    /// Generate LLVM IR for an entire Adam IR module.
    pub fn codegen_module(&mut self, module: &IrModule) {
        // 1. Emit string literals as global constants.
        for (idx, s) in module.string_literals.iter().enumerate() {
            // Create a null-terminated string constant as a global.
            let bytes = s.as_bytes();
            let i8_type = self.context.i8_type();
            let str_len = (bytes.len() + 1) as u32; // +1 for null terminator
            let arr_type = i8_type.array_type(str_len);

            let mut values: Vec<inkwell::values::IntValue<'ctx>> = bytes
                .iter()
                .map(|&b| i8_type.const_int(b as u64, false))
                .collect();
            values.push(i8_type.const_zero()); // null terminator

            let const_str = i8_type.const_array(&values);
            let global = self.module.add_global(arr_type, None, &format!(".str.{}", idx));
            global.set_initializer(&const_str);
            global.set_constant(true);
            global.set_unnamed_addr(true);

            self.strings.insert(idx as u32, global.as_pointer_value());
        }

        // 2. Define named struct types from the module's struct definitions.
        for struct_def in &module.struct_defs {
            let field_types: Vec<BasicTypeEnum<'ctx>> = struct_def.fields
                .iter()
                .map(|f| self.llvm_type(&f.ty))
                .collect();
            let st = self.context.opaque_struct_type(&struct_def.name);
            st.set_body(&field_types, false);
            self.struct_types.insert(struct_def.name.clone(), st);
        }

        // 3. Forward-declare all functions so calls can resolve.
        for func in &module.functions {
            let fn_type = if func.name == "main" && matches!(func.return_type, IrType::Unit | IrType::Void) {
                // main must return i32 for the OS.
                self.context.i32_type().fn_type(&[], false)
            } else {
                self.llvm_fn_type(
                    &func.params.iter().map(|p| p.ty.clone()).collect::<Vec<_>>(),
                    &func.return_type,
                )
            };
            let llvm_fn = self.module.add_function(&func.name, fn_type, None);
            self.functions.insert(func.id, llvm_fn);
        }

        // 3. Generate each function body.
        for func in &module.functions {
            self.codegen_function(func);
        }
    }

    // ================================================================
    // Function-level codegen
    // ================================================================

    fn codegen_function(&mut self, func: &IrFunction) {
        let llvm_fn = self.functions[&func.id];

        // Clear per-function state.
        self.variables.clear();
        self.blocks.clear();
        self.var_value_types.clear();

        // Build a type lookup for locals.
        let local_types: HashMap<VarId, IrType> = func
            .locals
            .iter()
            .map(|l| (l.id, l.ty.clone()))
            .collect();

        // Create all LLVM basic blocks first (forward references for branches).
        for block in &func.blocks {
            let bb = self.context.append_basic_block(llvm_fn, &format!("bb{}", block.id));
            self.blocks.insert(block.id, bb);
        }

        // Emit allocas in the entry block for all locals.
        let entry_bb = self.blocks[&func.entry];
        self.builder.position_at_end(entry_bb);

        for local in &func.locals {
            let ty = self.llvm_type(&local.ty);
            let alloca = self.builder.build_alloca(ty, &local.name)
                .expect("failed to build alloca");
            self.variables.insert(local.id, alloca);
        }

        // Store function parameters into their allocas and track their types.
        for (i, _param) in func.params.iter().enumerate() {
            let var_id = i as VarId;
            let arg_val = llvm_fn.get_nth_param(i as u32)
                .expect("missing function parameter");
            let alloca = self.variables[&var_id];
            self.builder.build_store(alloca, arg_val)
                .expect("failed to store param");
            self.var_value_types.insert(var_id, arg_val.get_type());
        }

        // For main(), we override the return type to I32 so that the process returns 0.
        let effective_return_type = if func.name == "main" && matches!(func.return_type, IrType::Unit | IrType::Void) {
            IrType::I32
        } else {
            func.return_type.clone()
        };

        // Generate each block.
        for block in &func.blocks {
            self.codegen_block(block, &local_types, llvm_fn, &effective_return_type, func);
        }
    }

    // ================================================================
    // Block-level codegen
    // ================================================================

    fn codegen_block(
        &mut self,
        block: &adam_ir::ir::BasicBlock,
        local_types: &HashMap<VarId, IrType>,
        llvm_fn: FunctionValue<'ctx>,
        return_type: &IrType,
        func: &IrFunction,
    ) {
        let bb = self.blocks[&block.id];
        self.builder.position_at_end(bb);

        for inst in &block.instructions {
            self.codegen_instruction(inst, local_types);
        }

        self.codegen_terminator(&block.terminator, local_types, llvm_fn, return_type, func);
    }

    // ================================================================
    // Instruction codegen
    // ================================================================

    pub(crate) fn codegen_instruction(
        &mut self,
        inst: &Instruction,
        local_types: &HashMap<VarId, IrType>,
    ) {
        match inst {
            Instruction::Assign(var_id, rvalue) => {
                let val = self.codegen_rvalue(rvalue, local_types);
                let ptr = self.variables[var_id];
                self.builder.build_store(ptr, val).expect("failed to store");
                // Track the actual LLVM type stored, so loads use the right type.
                self.var_value_types.insert(*var_id, val.get_type());
            }
            Instruction::Drop(var_id) => {
                if let (Some(&ptr), Some(ty)) =
                    (self.variables.get(var_id), local_types.get(var_id))
                {
                    self.codegen_drop(ptr, ty);
                }
            }
            Instruction::Nop => {}
        }
    }

    // ================================================================
    // RValue codegen
    // ================================================================

    fn codegen_rvalue(
        &mut self,
        rvalue: &RValue,
        local_types: &HashMap<VarId, IrType>,
    ) -> BasicValueEnum<'ctx> {
        match rvalue {
            RValue::Use(operand) => self.codegen_operand(operand, local_types),

            RValue::BinaryOp(op, lhs, rhs) => {
                let l = self.codegen_operand(lhs, local_types);
                let r = self.codegen_operand(rhs, local_types);
                let lhs_ty = self.operand_ir_type(lhs, local_types);
                self.codegen_binop(*op, l, r, &lhs_ty)
            }

            RValue::UnaryOp(op, operand) => {
                let val = self.codegen_operand(operand, local_types);
                let op_ty = self.operand_ir_type(operand, local_types);
                self.codegen_unop(*op, val, &op_ty)
            }

            RValue::Call(fn_id, args) => {
                let callee = self.functions[fn_id];
                let arg_vals: Vec<BasicValueEnum<'ctx>> = args
                    .iter()
                    .map(|a| self.codegen_operand(a, local_types))
                    .collect();
                let arg_vals = self.coerce_call_args(callee, arg_vals);
                let args_meta: Vec<inkwell::values::BasicMetadataValueEnum<'ctx>> =
                    arg_vals.iter().map(|v| (*v).into()).collect();
                let result = self.builder
                    .build_call(callee, &args_meta, "call")
                    .expect("failed to build call");
                match result.try_as_basic_value() {
                    inkwell::values::ValueKind::Basic(val) => val,
                    inkwell::values::ValueKind::Instruction(_) => {
                        self.context.struct_type(&[], false).const_zero().into()
                    }
                }
            }

            RValue::CallNamed(name, args) => {
                self.codegen_call_named(name, args, local_types)
            }

            RValue::Aggregate(kind, fields) => {
                let field_vals: Vec<BasicValueEnum<'ctx>> = fields
                    .iter()
                    .map(|f| self.codegen_operand(f, local_types))
                    .collect();
                self.codegen_aggregate(kind, &field_vals)
            }

            RValue::Field(operand, idx) => {
                let agg = self.codegen_operand(operand, local_types);
                self.builder
                    .build_extract_value(agg.into_struct_value(), *idx, "field")
                    .expect("failed to extract field")
            }

            RValue::Index(base, index) => {
                let base_val = self.codegen_operand(base, local_types);
                let idx_val = self.codegen_operand(index, local_types);
                // For arrays: GEP into the array
                let base_ty = self.operand_ir_type(base, local_types);
                match &base_ty {
                    IrType::Array(elem_ty, Some(_)) => {
                        // Fixed-size array — use extractvalue if index is constant
                        let elem_llvm = self.llvm_type(elem_ty);
                        let arr = base_val;
                        // Alloca the array, GEP into it, load element
                        let arr_alloca = self.builder.build_alloca(arr.get_type(), "arr_tmp")
                            .expect("alloca");
                        self.builder.build_store(arr_alloca, arr).expect("store");
                        let zero = self.context.i64_type().const_zero();
                        let gep = unsafe {
                            self.builder.build_gep(
                                elem_llvm.array_type(0),
                                arr_alloca,
                                &[zero, idx_val.into_int_value().into()],
                                "idx",
                            ).expect("gep")
                        };
                        self.builder.build_load(elem_llvm, gep, "elem").expect("load")
                    }
                    _ => {
                        // Dynamic array/slice: load ptr, GEP, load
                        // ptr is field 0 of the {ptr, len, cap} struct
                        let ptr = self.builder
                            .build_extract_value(base_val.into_struct_value(), 0, "arr_ptr")
                            .expect("extract ptr");
                        let elem_ty_llvm = self.context.i8_type(); // fallback
                        let gep = unsafe {
                            self.builder.build_gep(
                                elem_ty_llvm,
                                ptr.into_pointer_value(),
                                &[idx_val.into_int_value().into()],
                                "idx",
                            ).expect("gep")
                        };
                        self.builder.build_load(elem_ty_llvm, gep, "elem").expect("load")
                    }
                }
            }

            RValue::Ref(var_id) | RValue::MutRef(var_id) => {
                // The variable already lives as an alloca (pointer), just return its address.
                self.variables[var_id].into()
            }

            RValue::Deref(operand) => {
                let ptr = self.codegen_operand(operand, local_types);
                let op_ty = self.operand_ir_type(operand, local_types);
                let pointee_ty = match &op_ty {
                    IrType::Ptr(inner) => self.llvm_type(inner),
                    _ => self.context.i64_type().into(),
                };
                self.builder
                    .build_load(pointee_ty, ptr.into_pointer_value(), "deref")
                    .expect("failed to deref")
            }

            RValue::Cast(operand, target_ty) => {
                let val = self.codegen_operand(operand, local_types);
                let src_ty = self.operand_ir_type(operand, local_types);
                self.codegen_cast(val, &src_ty, target_ty)
            }

            RValue::Constant(c) => self.codegen_constant(c),

            RValue::HeapAlloc(ty) => {
                // Call runtime: adam_runtime_alloc(size, align) -> ptr
                let size = self.size_of_type(ty);
                let align = self.align_of_type(ty);
                self.codegen_runtime_call(
                    "__adam_alloc",
                    &[
                        self.context.i64_type().const_int(size, false).into(),
                        self.context.i64_type().const_int(align, false).into(),
                    ],
                    self.context.ptr_type(AddressSpace::default()).into(),
                )
            }

            RValue::ChanCreate(_, capacity) => {
                let cap = capacity.unwrap_or(0);
                self.codegen_runtime_call(
                    "__adam_chan_create",
                    &[self.context.i64_type().const_int(cap, false).into()],
                    self.context.ptr_type(AddressSpace::default()).into(),
                )
            }

            RValue::ChanSend(chan, val) => {
                self.codegen_chan_send(chan, val, local_types)
            }

            RValue::ChanRecv(chan) => {
                self.codegen_chan_recv(chan, local_types)
            }
        }
    }

    // ================================================================
    // Operand codegen
    // ================================================================

    pub(crate) fn codegen_operand(
        &self,
        operand: &Operand,
        local_types: &HashMap<VarId, IrType>,
    ) -> BasicValueEnum<'ctx> {
        match operand {
            Operand::Var(var_id) => {
                let ptr = self.variables[var_id];
                // Prefer the actual stored type; fall back to declared type.
                let llvm_ty = self.var_value_types.get(var_id)
                    .copied()
                    .unwrap_or_else(|| self.llvm_type(&local_types[var_id]));
                self.builder.build_load(llvm_ty, ptr, "load")
                    .expect("failed to load variable")
            }
            Operand::Constant(c) => self.codegen_constant(c),
        }
    }

    fn codegen_constant(&self, c: &Constant) -> BasicValueEnum<'ctx> {
        match c {
            Constant::Int(val) => {
                self.context.i64_type().const_int(*val as u64, true).into()
            }
            Constant::Float(val) => {
                self.context.f64_type().const_float(*val).into()
            }
            Constant::Bool(val) => {
                self.context.bool_type().const_int(*val as u64, false).into()
            }
            Constant::Char(val) => {
                self.context.i32_type().const_int(*val as u64, false).into()
            }
            Constant::String(idx) => {
                // Return the global string pointer as a value.
                if let Some(ptr) = self.strings.get(idx) {
                    (*ptr).into()
                } else {
                    self.context.ptr_type(AddressSpace::default())
                        .const_null()
                        .into()
                }
            }
            Constant::Unit => {
                self.context.struct_type(&[], false).const_zero().into()
            }
            Constant::Nil => {
                self.context.ptr_type(AddressSpace::default())
                    .const_null()
                    .into()
            }
        }
    }

    // ================================================================
    // Binary operation codegen
    // ================================================================

    fn codegen_binop(
        &self,
        op: BinOp,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
        lhs_ty: &IrType,
    ) -> BasicValueEnum<'ctx> {
        if is_float_type(lhs_ty) {
            let l = lhs.into_float_value();
            let r = rhs.into_float_value();
            self.codegen_float_binop(op, l, r)
        } else {
            let mut l = lhs.into_int_value();
            let mut r = rhs.into_int_value();
            // Ensure both operands have the same bit width.
            let l_bits = l.get_type().get_bit_width();
            let r_bits = r.get_type().get_bit_width();
            if l_bits != r_bits {
                let wider = self.context.custom_width_int_type(l_bits.max(r_bits));
                let signed = is_signed_type(lhs_ty);
                if l_bits < r_bits {
                    l = if signed {
                        self.builder.build_int_s_extend(l, wider, "sext").expect("sext")
                    } else {
                        self.builder.build_int_z_extend(l, wider, "zext").expect("zext")
                    };
                } else {
                    r = if signed {
                        self.builder.build_int_s_extend(r, wider, "sext").expect("sext")
                    } else {
                        self.builder.build_int_z_extend(r, wider, "zext").expect("zext")
                    };
                }
            }
            let signed = is_signed_type(lhs_ty);
            self.codegen_int_binop(op, l, r, signed)
        }
    }

    fn codegen_int_binop(
        &self,
        op: BinOp,
        l: IntValue<'ctx>,
        r: IntValue<'ctx>,
        signed: bool,
    ) -> BasicValueEnum<'ctx> {
        match op {
            BinOp::Add => self.builder.build_int_add(l, r, "add").expect("add").into(),
            BinOp::Sub => self.builder.build_int_sub(l, r, "sub").expect("sub").into(),
            BinOp::Mul => self.builder.build_int_mul(l, r, "mul").expect("mul").into(),
            BinOp::Div => {
                if signed {
                    self.builder.build_int_signed_div(l, r, "sdiv").expect("sdiv").into()
                } else {
                    self.builder.build_int_unsigned_div(l, r, "udiv").expect("udiv").into()
                }
            }
            BinOp::Mod => {
                if signed {
                    self.builder.build_int_signed_rem(l, r, "srem").expect("srem").into()
                } else {
                    self.builder.build_int_unsigned_rem(l, r, "urem").expect("urem").into()
                }
            }
            BinOp::Eq => self.builder.build_int_compare(IntPredicate::EQ, l, r, "eq").expect("eq").into(),
            BinOp::NotEq => self.builder.build_int_compare(IntPredicate::NE, l, r, "ne").expect("ne").into(),
            BinOp::Lt => {
                let pred = if signed { IntPredicate::SLT } else { IntPredicate::ULT };
                self.builder.build_int_compare(pred, l, r, "lt").expect("lt").into()
            }
            BinOp::Gt => {
                let pred = if signed { IntPredicate::SGT } else { IntPredicate::UGT };
                self.builder.build_int_compare(pred, l, r, "gt").expect("gt").into()
            }
            BinOp::LtEq => {
                let pred = if signed { IntPredicate::SLE } else { IntPredicate::ULE };
                self.builder.build_int_compare(pred, l, r, "le").expect("le").into()
            }
            BinOp::GtEq => {
                let pred = if signed { IntPredicate::SGE } else { IntPredicate::UGE };
                self.builder.build_int_compare(pred, l, r, "ge").expect("ge").into()
            }
            BinOp::And => self.builder.build_and(l, r, "and").expect("and").into(),
            BinOp::Or => self.builder.build_or(l, r, "or").expect("or").into(),
        }
    }

    fn codegen_float_binop(
        &self,
        op: BinOp,
        l: FloatValue<'ctx>,
        r: FloatValue<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        match op {
            BinOp::Add => self.builder.build_float_add(l, r, "fadd").expect("fadd").into(),
            BinOp::Sub => self.builder.build_float_sub(l, r, "fsub").expect("fsub").into(),
            BinOp::Mul => self.builder.build_float_mul(l, r, "fmul").expect("fmul").into(),
            BinOp::Div => self.builder.build_float_div(l, r, "fdiv").expect("fdiv").into(),
            BinOp::Mod => self.builder.build_float_rem(l, r, "frem").expect("frem").into(),
            BinOp::Eq => self.builder.build_float_compare(FloatPredicate::OEQ, l, r, "feq").expect("feq").into(),
            BinOp::NotEq => self.builder.build_float_compare(FloatPredicate::UNE, l, r, "fne").expect("fne").into(),
            BinOp::Lt => self.builder.build_float_compare(FloatPredicate::OLT, l, r, "flt").expect("flt").into(),
            BinOp::Gt => self.builder.build_float_compare(FloatPredicate::OGT, l, r, "fgt").expect("fgt").into(),
            BinOp::LtEq => self.builder.build_float_compare(FloatPredicate::OLE, l, r, "fle").expect("fle").into(),
            BinOp::GtEq => self.builder.build_float_compare(FloatPredicate::OGE, l, r, "fge").expect("fge").into(),
            BinOp::And | BinOp::Or => {
                // Logical And/Or on floats is meaningless, treat as zero
                self.context.bool_type().const_zero().into()
            }
        }
    }

    // ================================================================
    // Unary operation codegen
    // ================================================================

    fn codegen_unop(
        &self,
        op: UnOp,
        val: BasicValueEnum<'ctx>,
        ty: &IrType,
    ) -> BasicValueEnum<'ctx> {
        match op {
            UnOp::Neg => {
                if is_float_type(ty) {
                    self.builder
                        .build_float_neg(val.into_float_value(), "fneg")
                        .expect("fneg")
                        .into()
                } else {
                    self.builder
                        .build_int_neg(val.into_int_value(), "neg")
                        .expect("neg")
                        .into()
                }
            }
            UnOp::Not => {
                self.builder
                    .build_not(val.into_int_value(), "not")
                    .expect("not")
                    .into()
            }
            UnOp::Ref => {
                // Ref as a unary op: put value on stack, return pointer.
                let alloca = self.builder.build_alloca(val.get_type(), "ref_tmp")
                    .expect("alloca");
                self.builder.build_store(alloca, val).expect("store");
                alloca.into()
            }
        }
    }

    // ================================================================
    // Terminator codegen
    // ================================================================

    fn codegen_terminator(
        &mut self,
        term: &Terminator,
        local_types: &HashMap<VarId, IrType>,
        llvm_fn: FunctionValue<'ctx>,
        return_type: &IrType,
        func: &IrFunction,
    ) {
        match term {
            Terminator::Return(None) => {
                if matches!(return_type, IrType::Unit | IrType::Void) {
                    self.builder.build_return(None).expect("ret void");
                } else {
                    // Unreachable block in a non-void function: return a zero value.
                    let ret_ty = self.llvm_type(return_type);
                    let zero = self.zero_for_type(ret_ty);
                    self.builder.build_return(Some(&zero)).expect("ret default");
                }
            }
            Terminator::Return(Some(operand)) => {
                if matches!(return_type, IrType::Unit | IrType::Void) {
                    self.builder.build_return(None).expect("ret void");
                } else {
                    let val = self.codegen_operand(operand, local_types);
                    let ret_ty = self.llvm_type(return_type);
                    let coerced = self.coerce_value(val, ret_ty);
                    self.builder.build_return(Some(&coerced)).expect("ret");
                }
            }
            Terminator::Goto(target) => {
                let bb = self.blocks[target];
                self.builder.build_unconditional_branch(bb).expect("br");
            }
            Terminator::Branch(cond, then_bb, else_bb) => {
                let cond_val = self.codegen_operand(cond, local_types);
                let then_block = self.blocks[then_bb];
                let else_block = self.blocks[else_bb];
                let cond_i1 = self.ensure_i1(cond_val.into_int_value());
                self.builder
                    .build_conditional_branch(
                        cond_i1,
                        then_block,
                        else_block,
                    )
                    .expect("cond br");
            }
            Terminator::Switch(operand, cases, default) => {
                let val = self.codegen_operand(operand, local_types);
                let default_bb = self.blocks[default];
                let cases_llvm: Vec<(IntValue<'ctx>, inkwell::basic_block::BasicBlock<'ctx>)> =
                    cases
                        .iter()
                        .map(|(c, bb_id)| {
                            let case_val = match c {
                                Constant::Int(v) => {
                                    self.context.i64_type().const_int(*v as u64, true)
                                }
                                Constant::Bool(v) => {
                                    self.context.bool_type().const_int(*v as u64, false)
                                }
                                Constant::Char(v) => {
                                    self.context.i32_type().const_int(*v as u64, false)
                                }
                                _ => self.context.i64_type().const_zero(),
                            };
                            (case_val, self.blocks[bb_id])
                        })
                        .collect();
                self.builder
                    .build_switch(val.into_int_value(), default_bb, &cases_llvm)
                    .expect("switch");
            }
            Terminator::Spawn(target, cont) => {
                self.codegen_spawn_terminator(target, cont, func, local_types, llvm_fn);
            }
            Terminator::Select(branches) => {
                self.codegen_select_terminator(branches, local_types, llvm_fn);
            }
            Terminator::Unreachable => {
                self.builder.build_unreachable().expect("unreachable");
            }
        }
    }

    // ================================================================
    // Aggregate codegen
    // ================================================================

    fn codegen_aggregate(
        &self,
        kind: &AggregateKind,
        fields: &[BasicValueEnum<'ctx>],
    ) -> BasicValueEnum<'ctx> {
        match kind {
            AggregateKind::Struct(name) => {
                if let Some(struct_ty) = self.struct_types.get(name) {
                    // Use the named struct type and coerce fields to match.
                    let field_types_vec = struct_ty.get_field_types();
                    let mut agg = struct_ty.get_undef();
                    for (i, val) in fields.iter().enumerate() {
                        let coerced = if i < field_types_vec.len() {
                            self.coerce_value(*val, field_types_vec[i])
                        } else {
                            *val
                        };
                        agg = self.builder
                            .build_insert_value(agg, coerced, i as u32, "ins")
                            .expect("insert value")
                            .into_struct_value();
                    }
                    agg.into()
                } else {
                    // Fallback: anonymous struct.
                    let field_types: Vec<inkwell::types::BasicTypeEnum<'ctx>> =
                        fields.iter().map(|f| f.get_type()).collect();
                    let struct_ty = self.context.struct_type(&field_types, false);
                    let mut agg = struct_ty.get_undef();
                    for (i, val) in fields.iter().enumerate() {
                        agg = self.builder
                            .build_insert_value(agg, *val, i as u32, "ins")
                            .expect("insert value")
                            .into_struct_value();
                    }
                    agg.into()
                }
            }
            AggregateKind::Tuple => {
                let field_types: Vec<inkwell::types::BasicTypeEnum<'ctx>> =
                    fields.iter().map(|f| f.get_type()).collect();
                let struct_ty = self.context.struct_type(&field_types, false);
                let mut agg = struct_ty.get_undef();
                for (i, val) in fields.iter().enumerate() {
                    agg = self.builder
                        .build_insert_value(agg, *val, i as u32, "ins")
                        .expect("insert value")
                        .into_struct_value();
                }
                agg.into()
            }
            AggregateKind::Array => {
                if fields.is_empty() {
                    return self.context.struct_type(&[], false).const_zero().into();
                }
                let elem_ty = fields[0].get_type();
                let arr_ty = elem_ty.array_type(fields.len() as u32);
                let mut agg = arr_ty.get_undef();
                for (i, val) in fields.iter().enumerate() {
                    agg = self.builder
                        .build_insert_value(agg, *val, i as u32, "ins")
                        .expect("insert value")
                        .into_array_value();
                }
                agg.into()
            }
        }
    }

    // ================================================================
    // Cast codegen
    // ================================================================

    fn codegen_cast(
        &self,
        val: BasicValueEnum<'ctx>,
        src_ty: &IrType,
        dst_ty: &IrType,
    ) -> BasicValueEnum<'ctx> {
        let dst_llvm = self.llvm_type(dst_ty);

        match (src_ty, dst_ty) {
            // Int to wider int (sign extend or zero extend)
            (s, d) if is_int_type(s) && is_int_type(d) => {
                let src_int = val.into_int_value();
                let dst_int_ty = dst_llvm.into_int_type();
                let src_bits = src_int.get_type().get_bit_width();
                let dst_bits = dst_int_ty.get_bit_width();
                if src_bits == dst_bits {
                    val
                } else if src_bits < dst_bits {
                    if is_signed_type(s) {
                        self.builder
                            .build_int_s_extend(src_int, dst_int_ty, "sext")
                            .expect("sext")
                            .into()
                    } else {
                        self.builder
                            .build_int_z_extend(src_int, dst_int_ty, "zext")
                            .expect("zext")
                            .into()
                    }
                } else {
                    self.builder
                        .build_int_truncate(src_int, dst_int_ty, "trunc")
                        .expect("trunc")
                        .into()
                }
            }

            // Int to float
            (s, _) if is_int_type(s) && is_float_type(dst_ty) => {
                let dst_float_ty = dst_llvm.into_float_type();
                if is_signed_type(s) {
                    self.builder
                        .build_signed_int_to_float(val.into_int_value(), dst_float_ty, "sitofp")
                        .expect("sitofp")
                        .into()
                } else {
                    self.builder
                        .build_unsigned_int_to_float(val.into_int_value(), dst_float_ty, "uitofp")
                        .expect("uitofp")
                        .into()
                }
            }

            // Float to int
            (_, d) if is_float_type(src_ty) && is_int_type(d) => {
                let dst_int_ty = dst_llvm.into_int_type();
                if is_signed_type(d) {
                    self.builder
                        .build_float_to_signed_int(val.into_float_value(), dst_int_ty, "fptosi")
                        .expect("fptosi")
                        .into()
                } else {
                    self.builder
                        .build_float_to_unsigned_int(val.into_float_value(), dst_int_ty, "fptoui")
                        .expect("fptoui")
                        .into()
                }
            }

            // Float to float (f32 <-> f64)
            _ if is_float_type(src_ty) && is_float_type(dst_ty) => {
                let dst_float_ty = dst_llvm.into_float_type();
                self.builder
                    .build_float_cast(val.into_float_value(), dst_float_ty, "fpcast")
                    .expect("fpcast")
                    .into()
            }

            // Pointer casts — bitcast
            (IrType::Ptr(_), IrType::Ptr(_)) => val,

            // Fallback — return value unchanged
            _ => val,
        }
    }

    // ================================================================
    // Named call codegen (runtime functions)
    // ================================================================

    fn codegen_call_named(
        &mut self,
        name: &str,
        args: &[Operand],
        local_types: &HashMap<VarId, IrType>,
    ) -> BasicValueEnum<'ctx> {
        let arg_vals: Vec<BasicValueEnum<'ctx>> = args
            .iter()
            .map(|a| self.codegen_operand(a, local_types))
            .collect();

        // Get or declare the function.
        let callee = self.get_or_declare_external(name, &arg_vals);

        // Coerce arguments to match the declared function's parameter types.
        let arg_vals = self.coerce_call_args(callee, arg_vals);
        let args_meta: Vec<inkwell::values::BasicMetadataValueEnum<'ctx>> =
            arg_vals.iter().map(|v| (*v).into()).collect();

        let result = self.builder
            .build_call(callee, &args_meta, "call")
            .expect("failed to build named call");

        match result.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => val,
            inkwell::values::ValueKind::Instruction(_) => {
                self.context.struct_type(&[], false).const_zero().into()
            }
        }
    }

    /// Get or declare an external function with a type inferred from its arguments.
    fn get_or_declare_external(
        &self,
        name: &str,
        args: &[BasicValueEnum<'ctx>],
    ) -> FunctionValue<'ctx> {
        if let Some(f) = self.module.get_function(name) {
            return f;
        }

        // Infer function type based on known runtime functions.
        let param_types: Vec<inkwell::types::BasicMetadataTypeEnum<'ctx>> =
            args.iter().map(|a| a.get_type().into()).collect();

        // Most runtime functions return void or a pointer.
        let fn_type = match name {
            "print" | "println" | "__adam_dealloc" => {
                self.context.void_type().fn_type(&param_types, false)
            }
            "__adam_chan_send" => {
                // Fixed signature: (ptr, ptr, i64) -> void
                let p: Vec<inkwell::types::BasicMetadataTypeEnum<'ctx>> = vec![
                    self.context.ptr_type(AddressSpace::default()).into(),
                    self.context.ptr_type(AddressSpace::default()).into(),
                    self.context.i64_type().into(),
                ];
                self.context.void_type().fn_type(&p, false)
            }
            "__adam_print_int" => {
                let p: inkwell::types::BasicMetadataTypeEnum = self.context.i64_type().into();
                self.context.void_type().fn_type(&[p], false)
            }
            "__adam_print_float" => {
                let p: inkwell::types::BasicMetadataTypeEnum = self.context.f64_type().into();
                self.context.void_type().fn_type(&[p], false)
            }
            "__adam_print_bool" => {
                // Runtime uses i8 for bool, not i1
                let p: inkwell::types::BasicMetadataTypeEnum = self.context.i8_type().into();
                self.context.void_type().fn_type(&[p], false)
            }
            "__adam_println_str" | "__adam_print_str" | "__adam_print_cstr" | "__adam_println_cstr" => {
                self.context.void_type().fn_type(&param_types, false)
            }
            "__adam_alloc" | "__adam_chan_create" => {
                self.context
                    .ptr_type(AddressSpace::default())
                    .fn_type(&param_types, false)
            }
            "__adam_chan_recv" => {
                // Fixed signature: (ptr, ptr, i64) -> bool
                let p: Vec<inkwell::types::BasicMetadataTypeEnum<'ctx>> = vec![
                    self.context.ptr_type(AddressSpace::default()).into(),
                    self.context.ptr_type(AddressSpace::default()).into(),
                    self.context.i64_type().into(),
                ];
                self.context.bool_type().fn_type(&p, false)
            }
            "__str_concat" => {
                // Returns a string struct
                self.string_type().fn_type(&param_types, false)
            }
            "__int_to_string" | "__float_to_string" | "__bool_to_string" | "__char_to_string" => {
                self.string_type().fn_type(&param_types, false)
            }
            _ => {
                // Default: return i64
                self.context.i64_type().fn_type(&param_types, false)
            }
        };

        self.module.add_function(name, fn_type, Some(inkwell::module::Linkage::External))
    }

    // ================================================================
    // Runtime call helper
    // ================================================================

    fn codegen_runtime_call(
        &self,
        name: &str,
        args: &[BasicValueEnum<'ctx>],
        _ret_ty: BasicTypeEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let callee = self.get_or_declare_external(name, args);
        let args_meta: Vec<inkwell::values::BasicMetadataValueEnum<'ctx>> =
            args.iter().map(|v| (*v).into()).collect();
        let result = self.builder
            .build_call(callee, &args_meta, "rt_call")
            .expect("failed to build runtime call");
        match result.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => val,
            inkwell::values::ValueKind::Instruction(_) => {
                self.context.struct_type(&[], false).const_zero().into()
            }
        }
    }

    // ================================================================
    // Type helpers
    // ================================================================

    pub(crate) fn operand_ir_type(&self, operand: &Operand, local_types: &HashMap<VarId, IrType>) -> IrType {
        match operand {
            Operand::Var(var_id) => {
                local_types.get(var_id).cloned().unwrap_or(IrType::I64)
            }
            Operand::Constant(c) => match c {
                Constant::Int(_) => IrType::I64,
                Constant::Float(_) => IrType::F64,
                Constant::Bool(_) => IrType::Bool,
                Constant::Char(_) => IrType::Char,
                Constant::String(_) => IrType::String,
                Constant::Unit => IrType::Unit,
                Constant::Nil => IrType::Ptr(Box::new(IrType::Void)),
            },
        }
    }

    /// Approximate size of a type in bytes (for heap allocation).
    pub(crate) fn size_of_type(&self, ty: &IrType) -> u64 {
        match ty {
            IrType::I8 | IrType::U8 | IrType::Bool => 1,
            IrType::I16 | IrType::U16 => 2,
            IrType::I32 | IrType::U32 | IrType::Char | IrType::F32 => 4,
            IrType::I64 | IrType::U64 | IrType::F64 => 8,
            IrType::Ptr(_) | IrType::Function(_, _) | IrType::Channel(_) => 8,
            IrType::String => 24, // ptr + len + cap
            IrType::Str => 16,   // ptr + len
            IrType::Array(elem, Some(n)) => self.size_of_type(elem) * n,
            IrType::Array(_, None) => 24, // ptr + len + cap
            IrType::Tuple(fields) => fields.iter().map(|f| self.size_of_type(f)).sum(),
            IrType::Unit | IrType::Void => 0,
            IrType::Struct(_) | IrType::Enum(_) => 8, // conservative default
        }
    }

    // ================================================================
    // Value coercion helpers
    // ================================================================

    /// Coerce a value to match a target LLVM type, inserting casts as needed.
    fn coerce_value(&self, val: BasicValueEnum<'ctx>, target_ty: BasicTypeEnum<'ctx>) -> BasicValueEnum<'ctx> {
        if val.get_type() == target_ty {
            return val;
        }
        // Int-to-int coercion
        if val.is_int_value() && target_ty.is_int_type() {
            let src_int = val.into_int_value();
            let dst_int_ty = target_ty.into_int_type();
            let src_bits = src_int.get_type().get_bit_width();
            let dst_bits = dst_int_ty.get_bit_width();
            if src_bits == dst_bits {
                return val;
            } else if src_bits < dst_bits {
                return self.builder
                    .build_int_s_extend(src_int, dst_int_ty, "sext")
                    .expect("sext")
                    .into();
            } else {
                return self.builder
                    .build_int_truncate(src_int, dst_int_ty, "trunc")
                    .expect("trunc")
                    .into();
            }
        }
        // Float-to-float coercion
        if val.is_float_value() && target_ty.is_float_type() {
            return self.builder
                .build_float_cast(val.into_float_value(), target_ty.into_float_type(), "fpcast")
                .expect("fpcast")
                .into();
        }
        // Int-to-float
        if val.is_int_value() && target_ty.is_float_type() {
            return self.builder
                .build_signed_int_to_float(val.into_int_value(), target_ty.into_float_type(), "sitofp")
                .expect("sitofp")
                .into();
        }
        // Float-to-int
        if val.is_float_value() && target_ty.is_int_type() {
            return self.builder
                .build_float_to_signed_int(val.into_float_value(), target_ty.into_int_type(), "fptosi")
                .expect("fptosi")
                .into();
        }
        // Struct-to-struct coercion (e.g. anonymous {i64, i64} to named %Rect{i32, i32})
        if val.is_struct_value() && target_ty.is_struct_type() {
            let src_struct = val.into_struct_value();
            let dst_struct_ty = target_ty.into_struct_type();
            let dst_field_types = dst_struct_ty.get_field_types();
            let src_field_count = src_struct.get_type().count_fields();
            // Only coerce field-by-field if source has enough fields.
            if src_field_count >= dst_field_types.len() as u32 && !dst_field_types.is_empty() {
                let mut agg = dst_struct_ty.get_undef();
                for (i, dst_ft) in dst_field_types.iter().enumerate() {
                    let field_val = self.builder
                        .build_extract_value(src_struct, i as u32, "f")
                        .expect("extract");
                    let coerced = self.coerce_value(field_val, *dst_ft);
                    agg = self.builder
                        .build_insert_value(agg, coerced, i as u32, "ins")
                        .expect("insert")
                        .into_struct_value();
                }
                return agg.into();
            }
            // Source is empty/smaller — return zero of target type.
            return self.zero_for_type(target_ty);
        }
        // Empty struct (Unit) to non-struct — return zero
        if val.is_struct_value() && !target_ty.is_struct_type() {
            return self.zero_for_type(target_ty);
        }
        val
    }

    /// Ensure an integer value is i1 (for branch conditions).
    fn ensure_i1(&self, val: IntValue<'ctx>) -> IntValue<'ctx> {
        if val.get_type().get_bit_width() == 1 {
            return val;
        }
        // Compare != 0 to get i1
        let zero = val.get_type().const_zero();
        self.builder
            .build_int_compare(IntPredicate::NE, val, zero, "to_bool")
            .expect("cmp to i1")
    }

    /// Coerce call arguments to match the callee's parameter types.
    fn coerce_call_args(&self, callee: FunctionValue<'ctx>, args: Vec<BasicValueEnum<'ctx>>) -> Vec<BasicValueEnum<'ctx>> {
        let param_types = callee.get_type().get_param_types();
        args.into_iter().enumerate().map(|(i, arg)| {
            if let Some(param_meta_ty) = param_types.get(i) {
                if let Ok(basic_ty) = BasicTypeEnum::try_from(*param_meta_ty) {
                    self.coerce_value(arg, basic_ty)
                } else {
                    arg
                }
            } else {
                arg
            }
        }).collect()
    }

    /// Create a zero/default value for a given LLVM type.
    fn zero_for_type(&self, ty: BasicTypeEnum<'ctx>) -> BasicValueEnum<'ctx> {
        match ty {
            BasicTypeEnum::IntType(t) => t.const_zero().into(),
            BasicTypeEnum::FloatType(t) => t.const_zero().into(),
            BasicTypeEnum::PointerType(t) => t.const_null().into(),
            BasicTypeEnum::StructType(t) => t.const_zero().into(),
            BasicTypeEnum::ArrayType(t) => t.const_zero().into(),
            BasicTypeEnum::VectorType(t) => t.const_zero().into(),
            BasicTypeEnum::ScalableVectorType(_) => {
                // Should never happen for Adam types
                self.context.i64_type().const_zero().into()
            }
        }
    }

    pub(crate) fn align_of_type(&self, ty: &IrType) -> u64 {
        match ty {
            IrType::I8 | IrType::U8 | IrType::Bool => 1,
            IrType::I16 | IrType::U16 => 2,
            IrType::I32 | IrType::U32 | IrType::Char | IrType::F32 => 4,
            _ => 8,
        }
    }
}

// ================================================================
// Free functions — type classification
// ================================================================

fn is_int_type(ty: &IrType) -> bool {
    matches!(
        ty,
        IrType::I8 | IrType::I16 | IrType::I32 | IrType::I64
            | IrType::U8 | IrType::U16 | IrType::U32 | IrType::U64
            | IrType::Bool | IrType::Char
    )
}

fn is_float_type(ty: &IrType) -> bool {
    matches!(ty, IrType::F32 | IrType::F64)
}

fn is_signed_type(ty: &IrType) -> bool {
    matches!(ty, IrType::I8 | IrType::I16 | IrType::I32 | IrType::I64)
}
