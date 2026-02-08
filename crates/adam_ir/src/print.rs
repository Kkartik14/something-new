//! IR printer — human-readable textual representation of the IR.
//!
//! Produces output like:
//! ```text
//! fn main() -> unit {
//!   bb0:
//!     %0 = const 5
//!     %1 = const "Hello"
//!     call @print(%1)
//!     return ()
//! }
//! ```

use crate::ir::*;

/// Print an entire IR module to a string.
pub fn print_module(module: &IrModule) -> String {
    let mut out = String::new();

    // String literals table.
    if !module.string_literals.is_empty() {
        out.push_str("; string literals\n");
        for (i, s) in module.string_literals.iter().enumerate() {
            out.push_str(&format!(";   str{} = {:?}\n", i, s));
        }
        out.push('\n');
    }

    // Globals.
    if !module.globals.is_empty() {
        for global in &module.globals {
            out.push_str(&format!(
                "global @{}: {}\n",
                global.name,
                fmt_type(&global.ty)
            ));
        }
        out.push('\n');
    }

    // Functions.
    for func in &module.functions {
        out.push_str(&print_function(func));
        out.push('\n');
    }

    out
}

/// Print a single IR function to a string.
pub fn print_function(func: &IrFunction) -> String {
    let mut out = String::new();

    // Function signature.
    let params: Vec<String> = func
        .params
        .iter()
        .map(|p| format!("{}: {}", p.name, fmt_type(&p.ty)))
        .collect();
    out.push_str(&format!(
        "fn @{}({}) -> {} {{\n",
        func.name,
        params.join(", "),
        fmt_type(&func.return_type),
    ));

    // Locals.
    if !func.locals.is_empty() {
        out.push_str("  ; locals:\n");
        for local in &func.locals {
            out.push_str(&format!(
                "  ;   %{} = {} : {}\n",
                local.id,
                local.name,
                fmt_type(&local.ty)
            ));
        }
        out.push('\n');
    }

    // Basic blocks.
    for block in &func.blocks {
        out.push_str(&format!("  bb{}:\n", block.id));

        for instr in &block.instructions {
            out.push_str(&format!("    {}\n", fmt_instruction(instr)));
        }

        out.push_str(&format!("    {}\n", fmt_terminator(&block.terminator)));
        out.push('\n');
    }

    out.push_str("}\n");
    out
}

// ================================================================
// Formatting helpers
// ================================================================

fn fmt_type(ty: &IrType) -> String {
    match ty {
        IrType::I8 => "i8".into(),
        IrType::I16 => "i16".into(),
        IrType::I32 => "i32".into(),
        IrType::I64 => "i64".into(),
        IrType::U8 => "u8".into(),
        IrType::U16 => "u16".into(),
        IrType::U32 => "u32".into(),
        IrType::U64 => "u64".into(),
        IrType::F32 => "f32".into(),
        IrType::F64 => "f64".into(),
        IrType::Bool => "bool".into(),
        IrType::Char => "char".into(),
        IrType::Unit => "unit".into(),
        IrType::String => "String".into(),
        IrType::Str => "str".into(),
        IrType::Ptr(inner) => format!("*{}", fmt_type(inner)),
        IrType::Array(elem, None) => format!("[{}]", fmt_type(elem)),
        IrType::Array(elem, Some(n)) => format!("[{}; {}]", fmt_type(elem), n),
        IrType::Tuple(elems) => {
            let parts: Vec<String> = elems.iter().map(|e| fmt_type(e)).collect();
            format!("({})", parts.join(", "))
        }
        IrType::Struct(name) => name.clone(),
        IrType::Enum(name) => name.clone(),
        IrType::Function(params, ret) => {
            let ps: Vec<String> = params.iter().map(|p| fmt_type(p)).collect();
            format!("fn({}) -> {}", ps.join(", "), fmt_type(ret))
        }
        IrType::Channel(inner) => format!("chan[{}]", fmt_type(inner)),
        IrType::Void => "void".into(),
    }
}

fn fmt_instruction(instr: &Instruction) -> String {
    match instr {
        Instruction::Assign(var, rvalue) => {
            format!("%{} = {}", var, fmt_rvalue(rvalue))
        }
        Instruction::Drop(var) => format!("drop %{}", var),
        Instruction::Nop => "nop".into(),
    }
}

fn fmt_rvalue(rv: &RValue) -> String {
    match rv {
        RValue::Use(op) => fmt_operand(op),
        RValue::BinaryOp(op, left, right) => {
            format!(
                "{} {} {}",
                fmt_binop(op),
                fmt_operand(left),
                fmt_operand(right)
            )
        }
        RValue::UnaryOp(op, operand) => {
            format!("{} {}", fmt_unop(op), fmt_operand(operand))
        }
        RValue::Call(fn_id, args) => {
            let args_str: Vec<String> = args.iter().map(|a| fmt_operand(a)).collect();
            format!("call fn#{}({})", fn_id, args_str.join(", "))
        }
        RValue::CallNamed(name, args) => {
            let args_str: Vec<String> = args.iter().map(|a| fmt_operand(a)).collect();
            format!("call @{}({})", name, args_str.join(", "))
        }
        RValue::Aggregate(kind, fields) => {
            let fields_str: Vec<String> = fields.iter().map(|f| fmt_operand(f)).collect();
            match kind {
                AggregateKind::Array => format!("[{}]", fields_str.join(", ")),
                AggregateKind::Tuple => format!("({})", fields_str.join(", ")),
                AggregateKind::Struct(name) => {
                    format!("{} {{ {} }}", name, fields_str.join(", "))
                }
            }
        }
        RValue::Field(op, idx) => format!("{}.{}", fmt_operand(op), idx),
        RValue::Index(obj, idx) => format!("{}[{}]", fmt_operand(obj), fmt_operand(idx)),
        RValue::Ref(var) => format!("&%{}", var),
        RValue::MutRef(var) => format!("&mut %{}", var),
        RValue::Deref(op) => format!("*{}", fmt_operand(op)),
        RValue::Cast(op, ty) => format!("{} as {}", fmt_operand(op), fmt_type(ty)),
        RValue::Constant(c) => fmt_constant(c),
        RValue::HeapAlloc(ty) => format!("heap_alloc {}", fmt_type(ty)),
        RValue::ChanCreate(ty, cap) => match cap {
            Some(n) => format!("chan_create[{}]({})", fmt_type(ty), n),
            None => format!("chan_create[{}]()", fmt_type(ty)),
        },
        RValue::ChanSend(chan, val) => {
            format!("chan_send {} <- {}", fmt_operand(chan), fmt_operand(val))
        }
        RValue::ChanRecv(chan) => format!("chan_recv {}", fmt_operand(chan)),
    }
}

fn fmt_operand(op: &Operand) -> String {
    match op {
        Operand::Var(id) => format!("%{}", id),
        Operand::Constant(c) => fmt_constant(c),
    }
}

fn fmt_constant(c: &Constant) -> String {
    match c {
        Constant::Int(n) => format!("const {}", n),
        Constant::Float(f) => format!("const {:.1}", f),
        Constant::Bool(b) => format!("const {}", b),
        Constant::Char(c) => format!("const '{}'", c),
        Constant::String(idx) => format!("const str#{}", idx),
        Constant::Unit => "const ()".into(),
        Constant::Nil => "const nil".into(),
    }
}

fn fmt_binop(op: &BinOp) -> &'static str {
    match op {
        BinOp::Add => "add",
        BinOp::Sub => "sub",
        BinOp::Mul => "mul",
        BinOp::Div => "div",
        BinOp::Mod => "mod",
        BinOp::Eq => "eq",
        BinOp::NotEq => "neq",
        BinOp::Lt => "lt",
        BinOp::Gt => "gt",
        BinOp::LtEq => "lte",
        BinOp::GtEq => "gte",
        BinOp::And => "and",
        BinOp::Or => "or",
    }
}

fn fmt_unop(op: &UnOp) -> &'static str {
    match op {
        UnOp::Neg => "neg",
        UnOp::Not => "not",
        UnOp::Ref => "ref",
    }
}

fn fmt_terminator(term: &Terminator) -> String {
    match term {
        Terminator::Return(None) => "return".into(),
        Terminator::Return(Some(op)) => format!("return {}", fmt_operand(op)),
        Terminator::Goto(target) => format!("goto bb{}", target),
        Terminator::Branch(cond, then_bb, else_bb) => {
            format!("branch {} bb{} bb{}", fmt_operand(cond), then_bb, else_bb)
        }
        Terminator::Switch(scrutinee, cases, default) => {
            let cases_str: Vec<String> = cases
                .iter()
                .map(|(c, bb)| format!("{} => bb{}", fmt_constant(c), bb))
                .collect();
            format!(
                "switch {} [{}] default bb{}",
                fmt_operand(scrutinee),
                cases_str.join(", "),
                default
            )
        }
        Terminator::Spawn(target, cont) => format!("spawn bb{}, continue bb{}", target, cont),
        Terminator::Select(branches) => {
            let parts: Vec<String> = branches
                .iter()
                .map(|b| {
                    let kind_str = match &b.kind {
                        SelectBranchKind::Recv(var, chan) => {
                            format!("recv %{} <- {}", var, fmt_operand(chan))
                        }
                        SelectBranchKind::Send(chan, val) => {
                            format!("send {} <- {}", fmt_operand(chan), fmt_operand(val))
                        }
                        SelectBranchKind::After(dur) => {
                            format!("after {}", fmt_operand(dur))
                        }
                    };
                    format!("{} => bb{}", kind_str, b.target)
                })
                .collect();
            format!("select [{}]", parts.join(", "))
        }
        Terminator::Unreachable => "unreachable".into(),
    }
}
