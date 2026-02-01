//! AST-to-IR lowering — transforms the typed AST into IR basic blocks.
//!
//! The `Lowerer` walks the AST and produces a CFG of `BasicBlock`s
//! connected by terminators (goto, branch, switch, return).

use std::collections::HashMap;

use adam_ast::expr::*;
use adam_ast::item::*;
use adam_ast::pattern::Pattern;
use adam_ast::stmt::*;
use adam_ast::types::Type;

use crate::ir::*;

/// Lower an entire source file into an IR module.
pub fn lower_module(ast: &SourceFile) -> IrModule {
    let mut lowerer = Lowerer::new();

    // First pass: register all function names so cross-references work.
    for (idx, item) in ast.items.iter().enumerate() {
        if let Item::Function(func) = &item.node {
            lowerer.fn_ids.insert(func.name.name.clone(), idx as FnId);
        }
    }

    // Second pass: lower each function.
    for item in &ast.items {
        match &item.node {
            Item::Function(func) => {
                let ir_func = lowerer.lower_function(func);
                lowerer.functions.push(ir_func);
            }
            Item::Struct(s) => {
                // Register struct as a global type (no code generated).
                lowerer.struct_names.push(s.name.name.clone());
                lowerer.struct_defs.push(IrStructDef {
                    name: s.name.name.clone(),
                    fields: s.fields.iter().map(|f| IrStructField {
                        name: f.name.name.clone(),
                        ty: lowerer.lower_type(&f.ty.node),
                    }).collect(),
                });
            }
            Item::Enum(e) => {
                lowerer.enum_names.push(e.name.name.clone());
            }
            _ => {
                // Other items (trait, impl, use, mod) are handled elsewhere.
            }
        }
    }

    IrModule {
        functions: lowerer.functions,
        globals: lowerer.globals,
        string_literals: lowerer.string_literals,
        struct_defs: lowerer.struct_defs,
    }
}

/// The lowering context that accumulates IR while walking the AST.
struct Lowerer {
    // ---- Module-level state ----
    functions: Vec<IrFunction>,
    globals: Vec<IrGlobal>,
    string_literals: Vec<String>,
    struct_defs: Vec<IrStructDef>,
    fn_ids: HashMap<String, FnId>,
    struct_names: Vec<String>,
    enum_names: Vec<String>,
    next_fn_id: FnId,

    // ---- Per-function state ----
    next_var: VarId,
    next_block: BlockId,
    blocks: Vec<BasicBlock>,
    current_block: BlockId,
    locals: Vec<IrLocal>,
    /// Map from variable name to VarId (scoped via a stack of frames).
    var_env: Vec<HashMap<String, VarId>>,
    /// Stack of (header_block, exit_block) for break/continue.
    loop_stack: Vec<(BlockId, BlockId)>,
}

impl Lowerer {
    fn new() -> Self {
        Self {
            functions: Vec::new(),
            globals: Vec::new(),
            string_literals: Vec::new(),
            struct_defs: Vec::new(),
            fn_ids: HashMap::new(),
            struct_names: Vec::new(),
            enum_names: Vec::new(),
            next_fn_id: 0,

            next_var: 0,
            next_block: 0,
            blocks: Vec::new(),
            current_block: 0,
            locals: Vec::new(),
            var_env: vec![HashMap::new()],
            loop_stack: Vec::new(),
        }
    }

    // ================================================================
    // Variable / block allocation helpers
    // ================================================================

    fn fresh_var(&mut self, name: impl Into<String>, ty: IrType) -> VarId {
        let id = self.next_var;
        self.next_var += 1;
        self.locals.push(IrLocal {
            id,
            name: name.into(),
            ty,
        });
        id
    }

    fn fresh_tmp(&mut self) -> VarId {
        self.fresh_var(format!("_t{}", self.next_var), IrType::I64)
    }

    fn new_block(&mut self) -> BlockId {
        let id = self.next_block;
        self.next_block += 1;
        self.blocks.push(BasicBlock {
            id,
            instructions: Vec::new(),
            terminator: Terminator::Unreachable,
        });
        id
    }

    fn emit(&mut self, instr: Instruction) {
        let blk = self.current_block as usize;
        if blk < self.blocks.len() {
            self.blocks[blk].instructions.push(instr);
        }
    }

    fn set_terminator(&mut self, term: Terminator) {
        let blk = self.current_block as usize;
        if blk < self.blocks.len() {
            self.blocks[blk].terminator = term;
        }
    }

    fn switch_to(&mut self, block: BlockId) {
        self.current_block = block;
    }

    // ---- Scope helpers ----

    fn push_scope(&mut self) {
        self.var_env.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.var_env.pop();
    }

    fn define_var(&mut self, name: &str, var: VarId) {
        if let Some(scope) = self.var_env.last_mut() {
            scope.insert(name.to_string(), var);
        }
    }

    fn lookup_var(&self, name: &str) -> Option<VarId> {
        for scope in self.var_env.iter().rev() {
            if let Some(&id) = scope.get(name) {
                return Some(id);
            }
        }
        None
    }

    // ---- String interning ----

    fn intern_string(&mut self, s: &str) -> u32 {
        for (i, existing) in self.string_literals.iter().enumerate() {
            if existing == s {
                return i as u32;
            }
        }
        let idx = self.string_literals.len() as u32;
        self.string_literals.push(s.to_string());
        idx
    }

    // ================================================================
    // Reset per-function state
    // ================================================================

    fn reset_function_state(&mut self) {
        self.next_var = 0;
        self.next_block = 0;
        self.blocks = Vec::new();
        self.current_block = 0;
        self.locals = Vec::new();
        self.var_env = vec![HashMap::new()];
        self.loop_stack = Vec::new();
    }

    // ================================================================
    // Functions
    // ================================================================

    fn lower_function(&mut self, func: &FnDef) -> IrFunction {
        self.reset_function_state();

        let fn_id = self.fn_ids.get(&func.name.name).copied().unwrap_or_else(|| {
            let id = self.next_fn_id;
            self.next_fn_id += 1;
            id
        });

        // Create entry block.
        let entry = self.new_block();
        self.switch_to(entry);

        // Lower parameters — declare them as local variables.
        let mut ir_params = Vec::new();
        for param in &func.params {
            let ty = self.lower_type(&param.ty.node);
            let var = self.fresh_var(&param.name.name, ty.clone());
            self.define_var(&param.name.name, var);
            ir_params.push(IrParam {
                name: param.name.name.clone(),
                ty,
            });
        }

        let return_type = func
            .return_type
            .as_ref()
            .map(|t| self.lower_type(&t.node))
            .unwrap_or(IrType::Unit);

        // Lower body.
        if let Some(body) = &func.body {
            self.lower_block(body);

            // If the current block has no terminator yet, add a default return.
            let blk = self.current_block as usize;
            if blk < self.blocks.len() {
                if matches!(self.blocks[blk].terminator, Terminator::Unreachable) {
                    self.set_terminator(Terminator::Return(Some(Operand::Constant(Constant::Unit))));
                }
            }
        } else {
            // Extern / abstract function — just return unit.
            self.set_terminator(Terminator::Return(Some(Operand::Constant(Constant::Unit))));
        }

        IrFunction {
            id: fn_id,
            name: func.name.name.clone(),
            params: ir_params,
            return_type,
            blocks: std::mem::take(&mut self.blocks),
            entry,
            locals: std::mem::take(&mut self.locals),
        }
    }

    // ================================================================
    // Blocks and statements
    // ================================================================

    fn lower_block(&mut self, block: &Block) {
        self.push_scope();
        for stmt in &block.stmts {
            self.lower_stmt(&stmt.node);
        }
        self.pop_scope();
    }

    fn lower_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let(let_stmt) => {
                let operand = self.lower_expr(&let_stmt.value.node);
                let ty = let_stmt
                    .ty
                    .as_ref()
                    .map(|t| self.lower_type(&t.node))
                    .unwrap_or(IrType::I64);
                let var = self.fresh_var(&let_stmt.name.name, ty);
                self.emit(Instruction::Assign(var, RValue::Use(operand)));
                self.define_var(&let_stmt.name.name, var);
            }

            Stmt::Expr(expr) => {
                self.lower_expr(&expr.node);
            }

            Stmt::For(for_stmt) => {
                self.lower_for(for_stmt);
            }

            Stmt::While(while_stmt) => {
                self.lower_while(while_stmt);
            }

            Stmt::Loop(body) => {
                self.lower_loop(body);
            }

            Stmt::Item(item) => {
                // Nested function definitions etc.
                if let Item::Function(func) = &item.node {
                    let id = self.fn_ids.len() as FnId;
                    self.fn_ids.insert(func.name.name.clone(), id);
                }
            }
        }
    }

    // ================================================================
    // Control flow lowering
    // ================================================================

    fn lower_if(&mut self, if_expr: &IfExpr) -> Operand {
        let cond = self.lower_expr(&if_expr.condition.node);

        let then_block = self.new_block();
        let else_block = self.new_block();
        let merge_block = self.new_block();

        self.set_terminator(Terminator::Branch(cond, then_block, else_block));

        // Then branch.
        self.switch_to(then_block);
        let then_result = self.lower_block_as_expr(&if_expr.then_block);
        let then_exit = self.current_block;

        // Else branch.
        self.switch_to(else_block);
        let else_result = if let Some(else_blk) = &if_expr.else_block {
            match else_blk {
                ElseBlock::Else(block) => self.lower_block_as_expr(block),
                ElseBlock::ElseIf(elif) => self.lower_if(&elif.node),
            }
        } else {
            Operand::Constant(Constant::Unit)
        };
        let else_exit = self.current_block;

        // Connect both branches to merge.
        let result_var = self.fresh_tmp();

        // Patch then exit.
        self.switch_to(then_exit);
        self.emit(Instruction::Assign(result_var, RValue::Use(then_result)));
        if matches!(self.blocks[then_exit as usize].terminator, Terminator::Unreachable) {
            self.set_terminator(Terminator::Goto(merge_block));
        }

        // Patch else exit.
        self.switch_to(else_exit);
        self.emit(Instruction::Assign(result_var, RValue::Use(else_result)));
        if matches!(self.blocks[else_exit as usize].terminator, Terminator::Unreachable) {
            self.set_terminator(Terminator::Goto(merge_block));
        }

        self.switch_to(merge_block);
        Operand::Var(result_var)
    }

    fn lower_match(&mut self, match_expr: &MatchExpr) -> Operand {
        let scrutinee = self.lower_expr(&match_expr.scrutinee.node);
        let merge_block = self.new_block();
        let result_var = self.fresh_tmp();

        let mut cases: Vec<(Constant, BlockId)> = Vec::new();
        let mut default_block = merge_block;

        for arm in &match_expr.arms {
            let arm_block = self.new_block();

            match &arm.pattern.node {
                Pattern::Literal(lit) => {
                    let constant = self.expr_to_constant(&lit.node);
                    cases.push((constant, arm_block));
                }
                Pattern::Wildcard | Pattern::Binding(_) => {
                    // Default arm.
                    default_block = arm_block;
                }
                Pattern::Variant(vp) => {
                    // For enums, use a simple integer tag match.
                    // For now, use the variant name hash as the constant.
                    let tag = self.variant_name_to_tag(&vp.name.name);
                    cases.push((Constant::Int(tag), arm_block));

                    // Bind fields inside the arm.
                    self.switch_to(arm_block);
                    self.push_scope();
                    for (i, field_pat) in vp.fields.iter().enumerate() {
                        if let Pattern::Binding(ident) = &field_pat.node {
                            let field_var = self.fresh_var(&ident.name, IrType::I64);
                            self.emit(Instruction::Assign(
                                field_var,
                                RValue::Field(scrutinee.clone(), i as u32),
                            ));
                            self.define_var(&ident.name, field_var);
                        }
                    }
                }
                _ => {
                    default_block = arm_block;
                }
            }

            self.switch_to(arm_block);
            if !matches!(&arm.pattern.node, Pattern::Variant(_)) {
                self.push_scope();
                if let Pattern::Binding(ident) = &arm.pattern.node {
                    let bind_var = self.fresh_var(&ident.name, IrType::I64);
                    self.emit(Instruction::Assign(bind_var, RValue::Use(scrutinee.clone())));
                    self.define_var(&ident.name, bind_var);
                }
            }
            let arm_result = self.lower_expr(&arm.body.node);
            self.emit(Instruction::Assign(result_var, RValue::Use(arm_result)));
            self.pop_scope();

            if matches!(self.blocks[self.current_block as usize].terminator, Terminator::Unreachable) {
                self.set_terminator(Terminator::Goto(merge_block));
            }
        }

        // Emit switch terminator on the original block.
        // We need to find the block that had the scrutinee.
        // The switch is in the block just before the first arm block.
        let switch_block = cases.first().map(|(_, b)| b - 1).unwrap_or(self.current_block);
        self.switch_to(switch_block);
        if !cases.is_empty() {
            self.set_terminator(Terminator::Switch(scrutinee, cases, default_block));
        } else {
            self.set_terminator(Terminator::Goto(default_block));
        }

        self.switch_to(merge_block);
        Operand::Var(result_var)
    }

    fn lower_for(&mut self, for_stmt: &ForStmt) {
        // Lower iterable (e.g., a range).
        let iterable = self.lower_expr(&for_stmt.iterable.node);

        // Create blocks: header (condition), body, exit.
        let header_block = self.new_block();
        let body_block = self.new_block();
        let exit_block = self.new_block();

        // For simplicity we model for-in as:
        //   _iter = iterable
        //   _idx = 0
        //   goto header
        // header:
        //   branch (_idx < len(_iter)) body exit
        // body:
        //   binding = _iter[_idx]
        //   ... body stmts ...
        //   _idx = _idx + 1
        //   goto header
        // exit:

        let iter_var = self.fresh_var("_iter", IrType::I64);
        self.emit(Instruction::Assign(iter_var, RValue::Use(iterable.clone())));

        let idx_var = self.fresh_var("_idx", IrType::I64);
        self.emit(Instruction::Assign(idx_var, RValue::Constant(Constant::Int(0))));

        self.set_terminator(Terminator::Goto(header_block));

        // Header block: check condition.
        self.switch_to(header_block);
        let cond_var = self.fresh_tmp();
        // Simplified: we create a comparison that the codegen will handle.
        self.emit(Instruction::Assign(
            cond_var,
            RValue::BinaryOp(
                BinOp::Lt,
                Operand::Var(idx_var),
                iterable.clone(),
            ),
        ));
        self.set_terminator(Terminator::Branch(Operand::Var(cond_var), body_block, exit_block));

        // Body block.
        self.switch_to(body_block);
        self.push_scope();
        self.loop_stack.push((header_block, exit_block));

        // Bind the loop variable.
        let binding_var = self.fresh_var(&for_stmt.binding.name, IrType::I64);
        self.emit(Instruction::Assign(
            binding_var,
            RValue::Index(Operand::Var(iter_var), Operand::Var(idx_var)),
        ));
        self.define_var(&for_stmt.binding.name, binding_var);

        self.lower_block_stmts(&for_stmt.body);

        // Increment index.
        let inc_var = self.fresh_tmp();
        self.emit(Instruction::Assign(
            inc_var,
            RValue::BinaryOp(BinOp::Add, Operand::Var(idx_var), Operand::Constant(Constant::Int(1))),
        ));
        self.emit(Instruction::Assign(idx_var, RValue::Use(Operand::Var(inc_var))));

        self.loop_stack.pop();
        self.pop_scope();

        if matches!(self.blocks[self.current_block as usize].terminator, Terminator::Unreachable) {
            self.set_terminator(Terminator::Goto(header_block));
        }

        self.switch_to(exit_block);
    }

    fn lower_while(&mut self, while_stmt: &WhileStmt) {
        let header_block = self.new_block();
        let body_block = self.new_block();
        let exit_block = self.new_block();

        self.set_terminator(Terminator::Goto(header_block));

        // Header: evaluate condition.
        self.switch_to(header_block);
        let cond = self.lower_expr(&while_stmt.condition.node);
        self.set_terminator(Terminator::Branch(cond, body_block, exit_block));

        // Body.
        self.switch_to(body_block);
        self.push_scope();
        self.loop_stack.push((header_block, exit_block));
        self.lower_block_stmts(&while_stmt.body);
        self.loop_stack.pop();
        self.pop_scope();

        if matches!(self.blocks[self.current_block as usize].terminator, Terminator::Unreachable) {
            self.set_terminator(Terminator::Goto(header_block));
        }

        self.switch_to(exit_block);
    }

    fn lower_loop(&mut self, body: &Block) {
        let header_block = self.new_block();
        let exit_block = self.new_block();

        self.set_terminator(Terminator::Goto(header_block));

        self.switch_to(header_block);
        self.push_scope();
        self.loop_stack.push((header_block, exit_block));
        self.lower_block_stmts(body);
        self.loop_stack.pop();
        self.pop_scope();

        if matches!(self.blocks[self.current_block as usize].terminator, Terminator::Unreachable) {
            self.set_terminator(Terminator::Goto(header_block));
        }

        self.switch_to(exit_block);
    }

    // ================================================================
    // Expression lowering
    // ================================================================

    fn lower_expr(&mut self, expr: &Expr) -> Operand {
        match expr {
            // ---- Literals ----
            Expr::IntLiteral(n) => Operand::Constant(Constant::Int(*n)),
            Expr::FloatLiteral(f) => Operand::Constant(Constant::Float(*f)),
            Expr::BoolLiteral(b) => Operand::Constant(Constant::Bool(*b)),
            Expr::CharLiteral(c) => Operand::Constant(Constant::Char(*c)),
            Expr::NilLiteral => Operand::Constant(Constant::Nil),

            Expr::StringLiteral(s) => {
                let idx = self.intern_string(s);
                Operand::Constant(Constant::String(idx))
            }

            Expr::StringInterpolation(parts) => {
                // Lower each part, concatenate via calls.
                // For now, produce a simplified representation.
                let mut result = None;
                for part in parts {
                    let operand = match part {
                        StringPart::Literal(s) => {
                            let idx = self.intern_string(s);
                            Operand::Constant(Constant::String(idx))
                        }
                        StringPart::Interpolation(expr) => {
                            self.lower_expr(&expr.node)
                        }
                    };
                    result = Some(match result {
                        None => operand,
                        Some(prev) => {
                            let tmp = self.fresh_tmp();
                            self.emit(Instruction::Assign(
                                tmp,
                                RValue::CallNamed("__str_concat".to_string(), vec![prev, operand]),
                            ));
                            Operand::Var(tmp)
                        }
                    });
                }
                result.unwrap_or(Operand::Constant(Constant::Unit))
            }

            // ---- Identifiers ----
            Expr::Identifier(ident) => {
                if let Some(var) = self.lookup_var(&ident.name) {
                    Operand::Var(var)
                } else {
                    // Could be a function name or unresolved — emit as-is.
                    // For function references, create a temporary.
                    let tmp = self.fresh_tmp();
                    self.emit(Instruction::Assign(
                        tmp,
                        RValue::CallNamed(ident.name.clone(), vec![]),
                    ));
                    Operand::Var(tmp)
                }
            }

            Expr::Path(segments) => {
                // For qualified paths like Foo.bar, resolve the first segment.
                if let Some(first) = segments.first() {
                    if let Some(var) = self.lookup_var(&first.name) {
                        // Field access chain.
                        let mut current = Operand::Var(var);
                        for (i, seg) in segments.iter().enumerate().skip(1) {
                            let tmp = self.fresh_tmp();
                            self.emit(Instruction::Assign(
                                tmp,
                                RValue::Field(current, i as u32 - 1),
                            ));
                            let _ = seg; // name used for debugging
                            current = Operand::Var(tmp);
                        }
                        current
                    } else {
                        // Qualified name (e.g., enum variant).
                        let full_name = segments
                            .iter()
                            .map(|s| s.name.as_str())
                            .collect::<Vec<_>>()
                            .join(".");
                        let tmp = self.fresh_tmp();
                        self.emit(Instruction::Assign(
                            tmp,
                            RValue::CallNamed(full_name, vec![]),
                        ));
                        Operand::Var(tmp)
                    }
                } else {
                    Operand::Constant(Constant::Unit)
                }
            }

            // ---- Binary operators ----
            Expr::Binary(bin) => {
                let left = self.lower_expr(&bin.left.node);
                let right = self.lower_expr(&bin.right.node);
                let op = match bin.op {
                    BinaryOp::Add => BinOp::Add,
                    BinaryOp::Sub => BinOp::Sub,
                    BinaryOp::Mul => BinOp::Mul,
                    BinaryOp::Div => BinOp::Div,
                    BinaryOp::Mod => BinOp::Mod,
                    BinaryOp::Eq => BinOp::Eq,
                    BinaryOp::NotEq => BinOp::NotEq,
                    BinaryOp::Lt => BinOp::Lt,
                    BinaryOp::Gt => BinOp::Gt,
                    BinaryOp::LtEq => BinOp::LtEq,
                    BinaryOp::GtEq => BinOp::GtEq,
                    BinaryOp::And => BinOp::And,
                    BinaryOp::Or => BinOp::Or,
                };
                let tmp = self.fresh_tmp();
                self.emit(Instruction::Assign(tmp, RValue::BinaryOp(op, left, right)));
                Operand::Var(tmp)
            }

            // ---- Unary operators ----
            Expr::Unary(unary) => {
                let operand = self.lower_expr(&unary.operand.node);
                let op = match unary.op {
                    UnaryOp::Neg => UnOp::Neg,
                    UnaryOp::Not => UnOp::Not,
                    UnaryOp::Ref => UnOp::Ref,
                };
                let tmp = self.fresh_tmp();
                self.emit(Instruction::Assign(tmp, RValue::UnaryOp(op, operand)));
                Operand::Var(tmp)
            }

            // ---- Function call ----
            Expr::Call(call) => {
                let args: Vec<Operand> = call.args.iter().map(|a| self.lower_expr(&a.node)).collect();

                // Determine the callee.
                let tmp = self.fresh_tmp();
                match &call.callee.node {
                    Expr::Identifier(ident) => {
                        if let Some(&fn_id) = self.fn_ids.get(&ident.name) {
                            self.emit(Instruction::Assign(tmp, RValue::Call(fn_id, args)));
                        } else {
                            self.emit(Instruction::Assign(
                                tmp,
                                RValue::CallNamed(ident.name.clone(), args),
                            ));
                        }
                    }
                    Expr::Path(segments) => {
                        let name = segments
                            .iter()
                            .map(|s| s.name.as_str())
                            .collect::<Vec<_>>()
                            .join(".");
                        self.emit(Instruction::Assign(
                            tmp,
                            RValue::CallNamed(name, args),
                        ));
                    }
                    _ => {
                        let callee = self.lower_expr(&call.callee.node);
                        self.emit(Instruction::Assign(
                            tmp,
                            RValue::CallNamed(format!("__indirect_call"), vec![callee].into_iter().chain(args).collect()),
                        ));
                    }
                }
                Operand::Var(tmp)
            }

            // ---- Method call ----
            Expr::MethodCall(mc) => {
                let receiver = self.lower_expr(&mc.receiver.node);
                let mut args = vec![receiver];
                for arg in &mc.args {
                    args.push(self.lower_expr(&arg.node));
                }
                let tmp = self.fresh_tmp();
                self.emit(Instruction::Assign(
                    tmp,
                    RValue::CallNamed(mc.method.name.clone(), args),
                ));
                Operand::Var(tmp)
            }

            // ---- Field access ----
            Expr::FieldAccess(fa) => {
                let object = self.lower_expr(&fa.object.node);
                let tmp = self.fresh_tmp();
                // Use field index 0 as placeholder — real index comes from type info.
                let field_index = self.field_name_to_index(&fa.field.name);
                self.emit(Instruction::Assign(tmp, RValue::Field(object, field_index)));
                Operand::Var(tmp)
            }

            // ---- Index ----
            Expr::Index(idx) => {
                let object = self.lower_expr(&idx.object.node);
                let index = self.lower_expr(&idx.index.node);
                let tmp = self.fresh_tmp();
                self.emit(Instruction::Assign(tmp, RValue::Index(object, index)));
                Operand::Var(tmp)
            }

            // ---- Struct literal ----
            Expr::StructLiteral(sl) => {
                let fields: Vec<Operand> = sl
                    .fields
                    .iter()
                    .map(|(_, value)| self.lower_expr(&value.node))
                    .collect();
                let tmp = self.fresh_tmp();
                self.emit(Instruction::Assign(
                    tmp,
                    RValue::Aggregate(AggregateKind::Struct(sl.name.name.clone()), fields),
                ));
                Operand::Var(tmp)
            }

            // ---- Array literal ----
            Expr::ArrayLiteral(elems) => {
                let operands: Vec<Operand> = elems.iter().map(|e| self.lower_expr(&e.node)).collect();
                let tmp = self.fresh_tmp();
                self.emit(Instruction::Assign(
                    tmp,
                    RValue::Aggregate(AggregateKind::Array, operands),
                ));
                Operand::Var(tmp)
            }

            // ---- Tuple literal ----
            Expr::TupleLiteral(elems) => {
                let operands: Vec<Operand> = elems.iter().map(|e| self.lower_expr(&e.node)).collect();
                let tmp = self.fresh_tmp();
                self.emit(Instruction::Assign(
                    tmp,
                    RValue::Aggregate(AggregateKind::Tuple, operands),
                ));
                Operand::Var(tmp)
            }

            // ---- Block expression ----
            Expr::Block(block) => {
                self.lower_block_as_expr(block)
            }

            // ---- If expression ----
            Expr::If(if_expr) => {
                self.lower_if(if_expr)
            }

            // ---- Match expression ----
            Expr::Match(match_expr) => {
                self.lower_match(match_expr)
            }

            // ---- Closure ----
            Expr::Closure(closure) => {
                // Lower closure as an anonymous function reference.
                let tmp = self.fresh_tmp();
                let closure_name = format!("__closure_{}", tmp);

                // For now, store closure as a named reference.
                // Full closure lowering (capture analysis) would happen in a
                // dedicated pass.
                self.push_scope();
                for param in &closure.params {
                    let var = self.fresh_var(&param.name.name, IrType::I64);
                    self.define_var(&param.name.name, var);
                }
                let body_result = self.lower_expr(&closure.body.node);
                self.pop_scope();

                self.emit(Instruction::Assign(
                    tmp,
                    RValue::CallNamed(closure_name, vec![body_result]),
                ));
                Operand::Var(tmp)
            }

            // ---- Assignment ----
            Expr::Assign(assign) => {
                let value = self.lower_expr(&assign.value.node);
                match &assign.target.node {
                    Expr::Identifier(ident) => {
                        if let Some(var) = self.lookup_var(&ident.name) {
                            self.emit(Instruction::Assign(var, RValue::Use(value)));
                            Operand::Var(var)
                        } else {
                            let var = self.fresh_var(&ident.name, IrType::I64);
                            self.emit(Instruction::Assign(var, RValue::Use(value)));
                            self.define_var(&ident.name, var);
                            Operand::Var(var)
                        }
                    }
                    Expr::FieldAccess(fa) => {
                        let obj = self.lower_expr(&fa.object.node);
                        let field_idx = self.field_name_to_index(&fa.field.name);
                        let tmp = self.fresh_tmp();
                        // Model field write as a special call.
                        self.emit(Instruction::Assign(
                            tmp,
                            RValue::CallNamed(
                                "__field_set".to_string(),
                                vec![obj, Operand::Constant(Constant::Int(field_idx as i64)), value],
                            ),
                        ));
                        Operand::Var(tmp)
                    }
                    Expr::Index(idx) => {
                        let obj = self.lower_expr(&idx.object.node);
                        let index = self.lower_expr(&idx.index.node);
                        let tmp = self.fresh_tmp();
                        self.emit(Instruction::Assign(
                            tmp,
                            RValue::CallNamed(
                                "__index_set".to_string(),
                                vec![obj, index, value],
                            ),
                        ));
                        Operand::Var(tmp)
                    }
                    _ => {
                        // Unsupported assign target — just evaluate and discard.
                        value
                    }
                }
            }

            // ---- Range ----
            Expr::Range(range) => {
                let start = self.lower_expr(&range.start.node);
                let end = self.lower_expr(&range.end.node);
                let tmp = self.fresh_tmp();
                self.emit(Instruction::Assign(
                    tmp,
                    RValue::Aggregate(AggregateKind::Tuple, vec![start, end]),
                ));
                Operand::Var(tmp)
            }

            // ---- Try ----
            Expr::Try(inner) => {
                let val = self.lower_expr(&inner.node);
                // In a full implementation, this would check for error and
                // early-return. For now, pass through.
                val
            }

            // ---- Spawn ----
            Expr::Spawn(block) => {
                let spawn_block = self.new_block();
                let continue_block = self.new_block();

                self.set_terminator(Terminator::Spawn(spawn_block, continue_block));

                // Spawned block.
                self.switch_to(spawn_block);
                self.lower_block(block);
                self.set_terminator(Terminator::Return(None));

                // Continue in the parent.
                self.switch_to(continue_block);
                Operand::Constant(Constant::Unit)
            }

            // ---- Channel create ----
            Expr::ChanCreate(cc) => {
                let elem_ty = self.lower_type(&cc.element_type.node);
                let capacity = cc.capacity.as_ref().map(|cap| {
                    if let Expr::IntLiteral(n) = &cap.node {
                        *n as u64
                    } else {
                        0
                    }
                });
                let tmp = self.fresh_tmp();
                self.emit(Instruction::Assign(
                    tmp,
                    RValue::ChanCreate(elem_ty, capacity),
                ));
                Operand::Var(tmp)
            }

            // ---- Select ----
            Expr::Select(sel) => {
                self.lower_select(sel)
            }

            // ---- Return ----
            Expr::Return(val) => {
                let op = val
                    .as_ref()
                    .map(|v| self.lower_expr(&v.node))
                    .unwrap_or(Operand::Constant(Constant::Unit));
                self.set_terminator(Terminator::Return(Some(op)));
                // Create unreachable continuation block.
                let dead = self.new_block();
                self.switch_to(dead);
                Operand::Constant(Constant::Unit)
            }

            // ---- Break ----
            Expr::Break(val) => {
                let _val = val
                    .as_ref()
                    .map(|v| self.lower_expr(&v.node));
                if let Some(&(_, exit_block)) = self.loop_stack.last() {
                    self.set_terminator(Terminator::Goto(exit_block));
                }
                let dead = self.new_block();
                self.switch_to(dead);
                Operand::Constant(Constant::Unit)
            }

            // ---- Continue ----
            Expr::Continue => {
                if let Some(&(header_block, _)) = self.loop_stack.last() {
                    self.set_terminator(Terminator::Goto(header_block));
                }
                let dead = self.new_block();
                self.switch_to(dead);
                Operand::Constant(Constant::Unit)
            }
        }
    }

    // ================================================================
    // Select lowering
    // ================================================================

    fn lower_select(&mut self, sel: &SelectExpr) -> Operand {
        let merge_block = self.new_block();
        let result_var = self.fresh_tmp();
        let mut branches = Vec::new();

        for arm in &sel.arms {
            let arm_block = self.new_block();

            let kind = match &arm.kind {
                SelectArmKind::Recv { binding, channel } => {
                    let chan = self.lower_expr(&channel.node);
                    let var = self.fresh_var(&binding.name, IrType::I64);
                    self.define_var(&binding.name, var);
                    SelectBranchKind::Recv(var, chan)
                }
                SelectArmKind::Send { channel, value } => {
                    let chan = self.lower_expr(&channel.node);
                    let val = self.lower_expr(&value.node);
                    SelectBranchKind::Send(chan, val)
                }
                SelectArmKind::After(dur) => {
                    let d = self.lower_expr(&dur.node);
                    SelectBranchKind::After(d)
                }
            };

            branches.push(SelectBranch {
                kind,
                target: arm_block,
            });

            self.switch_to(arm_block);
            self.push_scope();
            let arm_result = self.lower_block_as_expr(&arm.body);
            self.emit(Instruction::Assign(result_var, RValue::Use(arm_result)));
            self.pop_scope();

            if matches!(self.blocks[self.current_block as usize].terminator, Terminator::Unreachable) {
                self.set_terminator(Terminator::Goto(merge_block));
            }
        }

        // Set select terminator on the predecessor block.
        let select_block = branches.first().map(|b| b.target - 1).unwrap_or(self.current_block);
        self.switch_to(select_block);
        self.set_terminator(Terminator::Select(branches));

        self.switch_to(merge_block);
        Operand::Var(result_var)
    }

    // ================================================================
    // Helpers
    // ================================================================

    /// Lower a block and return the last expression's operand (or unit).
    fn lower_block_as_expr(&mut self, block: &Block) -> Operand {
        self.push_scope();
        let mut last = Operand::Constant(Constant::Unit);
        for stmt in &block.stmts {
            match &stmt.node {
                Stmt::Expr(expr) => {
                    last = self.lower_expr(&expr.node);
                }
                other => {
                    self.lower_stmt(other);
                    last = Operand::Constant(Constant::Unit);
                }
            }
        }
        self.pop_scope();
        last
    }

    /// Lower block statements without creating a scope (used when scope
    /// is already managed by the caller, e.g., in loops).
    fn lower_block_stmts(&mut self, block: &Block) {
        for stmt in &block.stmts {
            self.lower_stmt(&stmt.node);
        }
    }

    /// Convert a literal expression to a constant (for match patterns).
    fn expr_to_constant(&self, expr: &Expr) -> Constant {
        match expr {
            Expr::IntLiteral(n) => Constant::Int(*n),
            Expr::FloatLiteral(f) => Constant::Float(*f),
            Expr::BoolLiteral(b) => Constant::Bool(*b),
            Expr::CharLiteral(c) => Constant::Char(*c),
            Expr::NilLiteral => Constant::Nil,
            _ => Constant::Int(0), // fallback
        }
    }

    /// Simple hash for enum variant name -> integer tag.
    fn variant_name_to_tag(&self, name: &str) -> i64 {
        let mut hash: i64 = 0;
        for b in name.bytes() {
            hash = hash.wrapping_mul(31).wrapping_add(b as i64);
        }
        hash
    }

    /// Look up a field name's index in the struct definitions.
    fn field_name_to_index(&self, name: &str) -> u32 {
        for sdef in &self.struct_defs {
            for (idx, field) in sdef.fields.iter().enumerate() {
                if field.name == name {
                    return idx as u32;
                }
            }
        }
        0 // fallback
    }

    // ================================================================
    // Type lowering
    // ================================================================

    fn lower_type(&self, ty: &Type) -> IrType {
        match ty {
            Type::Named(path) => {
                match path.name.name.as_str() {
                    "i8" => IrType::I8,
                    "i16" => IrType::I16,
                    "i32" => IrType::I32,
                    "i64" => IrType::I64,
                    "u8" => IrType::U8,
                    "u16" => IrType::U16,
                    "u32" => IrType::U32,
                    "u64" => IrType::U64,
                    "f32" => IrType::F32,
                    "f64" => IrType::F64,
                    "bool" => IrType::Bool,
                    "char" => IrType::Char,
                    "String" => IrType::String,
                    "str" => IrType::Str,
                    name => {
                        // Check if it's a known struct or enum.
                        if self.struct_names.contains(&name.to_string()) {
                            IrType::Struct(name.to_string())
                        } else if self.enum_names.contains(&name.to_string()) {
                            IrType::Enum(name.to_string())
                        } else {
                            IrType::Struct(name.to_string())
                        }
                    }
                }
            }
            Type::Reference(inner) | Type::MutReference(inner) => {
                IrType::Ptr(Box::new(self.lower_type(&inner.node)))
            }
            Type::Array(arr) => {
                let elem = self.lower_type(&arr.element.node);
                let size = arr.size.as_ref().and_then(|s| {
                    if let Expr::IntLiteral(n) = &s.node {
                        Some(*n as u64)
                    } else {
                        None
                    }
                });
                IrType::Array(Box::new(elem), size)
            }
            Type::Tuple(types) => {
                let inner: Vec<IrType> = types.iter().map(|t| self.lower_type(&t.node)).collect();
                IrType::Tuple(inner)
            }
            Type::Optional(inner) => {
                // Model optionals as a pointer for now.
                IrType::Ptr(Box::new(self.lower_type(&inner.node)))
            }
            Type::Result(res) => {
                // Model Result as a tuple (ok, err).
                let ok = self.lower_type(&res.ok.node);
                let err = self.lower_type(&res.err.node);
                IrType::Tuple(vec![ok, err])
            }
            Type::Function(ft) => {
                let params: Vec<IrType> = ft.params.iter().map(|p| self.lower_type(&p.node)).collect();
                let ret = self.lower_type(&ft.return_type.node);
                IrType::Function(params, Box::new(ret))
            }
            Type::Channel(inner) => {
                IrType::Channel(Box::new(self.lower_type(&inner.node)))
            }
            Type::Inferred => IrType::I64, // default to i64 when type is inferred
        }
    }
}
