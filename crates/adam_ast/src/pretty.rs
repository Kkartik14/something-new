//! AST pretty printer — outputs a readable tree format for debugging.
//!
//! Prints AST nodes as an indented tree. Example output:
//! ```text
//! SourceFile
//! ├── FnDef "main"
//! │   ├── Params: (none)
//! │   ├── ReturnType: (none)
//! │   └── Body
//! │       └── Call "print"
//! │           └── StringLiteral "Hello, Adam!"
//! ```

use std::fmt;

use crate::common::*;
use crate::expr::*;
use crate::item::*;
use crate::pattern::*;
use crate::stmt::*;
use crate::types::*;

/// Pretty-prints an AST to a string.
pub struct PrettyPrinter {
    output: String,
    indent: usize,
    show_spans: bool,
}

impl PrettyPrinter {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            indent: 0,
            show_spans: false,
        }
    }

    pub fn with_spans(mut self) -> Self {
        self.show_spans = true;
        self
    }

    pub fn print_source_file(&mut self, file: &SourceFile) -> &str {
        self.line("SourceFile");
        self.indent += 1;
        for (i, item) in file.items.iter().enumerate() {
            self.prefix(i == file.items.len() - 1);
            self.print_item(&item.node);
        }
        self.indent -= 1;
        &self.output
    }

    fn print_item(&mut self, item: &Item) {
        match item {
            Item::Function(f) => self.print_fn_def(f),
            Item::Struct(s) => self.print_struct_def(s),
            Item::Enum(e) => self.print_enum_def(e),
            Item::Trait(t) => self.print_trait_def(t),
            Item::Impl(i) => self.print_impl_block(i),
            Item::View(v) => self.print_view_def(v),
            Item::Use(u) => self.print_use_decl(u),
            Item::Mod(m) => self.print_mod_decl(m),
        }
    }

    fn print_fn_def(&mut self, f: &FnDef) {
        let vis = vis_str(f.visibility);
        let generics = generics_str(&f.generic_params);
        self.line(&format!("{vis}FnDef \"{}\"{generics}", f.name.name));
        self.indent += 1;

        // Params
        if f.params.is_empty() {
            self.child(false, "Params: (none)");
        } else {
            self.child_start(false, "Params");
            self.indent += 1;
            for (i, p) in f.params.iter().enumerate() {
                let own = ownership_str(p.ownership);
                self.prefix(i == f.params.len() - 1);
                self.line(&format!("{own}{}: {}", p.name.name, type_str(&p.ty.node)));
            }
            self.indent -= 1;
        }

        // Return type
        match &f.return_type {
            Some(ty) => self.child(false, &format!("ReturnType: {}", type_str(&ty.node))),
            None => self.child(false, "ReturnType: (none)"),
        }

        // Body
        let is_last = true;
        match &f.body {
            Some(block) => {
                self.prefix(is_last);
                self.line("Body");
                self.indent += 1;
                self.print_block(block);
                self.indent -= 1;
            }
            None => self.child(is_last, "Body: (none)"),
        }

        self.indent -= 1;
    }

    fn print_struct_def(&mut self, s: &StructDef) {
        let vis = vis_str(s.visibility);
        let generics = generics_str(&s.generic_params);
        self.line(&format!("{vis}StructDef \"{}\"{generics}", s.name.name));
        self.indent += 1;
        for (i, field) in s.fields.iter().enumerate() {
            let fvis = vis_str(field.visibility);
            self.prefix(i == s.fields.len() - 1);
            self.line(&format!(
                "{fvis}{}: {}",
                field.name.name,
                type_str(&field.ty.node)
            ));
        }
        self.indent -= 1;
    }

    fn print_enum_def(&mut self, e: &EnumDef) {
        let vis = vis_str(e.visibility);
        let generics = generics_str(&e.generic_params);
        self.line(&format!("{vis}EnumDef \"{}\"{generics}", e.name.name));
        self.indent += 1;
        for (i, variant) in e.variants.iter().enumerate() {
            self.prefix(i == e.variants.len() - 1);
            if variant.fields.is_empty() {
                self.line(&variant.name.name);
            } else {
                let fields: Vec<String> =
                    variant.fields.iter().map(|f| type_str(&f.node)).collect();
                self.line(&format!("{}({})", variant.name.name, fields.join(", ")));
            }
        }
        self.indent -= 1;
    }

    fn print_trait_def(&mut self, t: &TraitDef) {
        let vis = vis_str(t.visibility);
        let generics = generics_str(&t.generic_params);
        self.line(&format!("{vis}TraitDef \"{}\"{generics}", t.name.name));
        self.indent += 1;
        for (i, method) in t.methods.iter().enumerate() {
            self.prefix(i == t.methods.len() - 1);
            self.print_fn_def(&method.node);
        }
        self.indent -= 1;
    }

    fn print_impl_block(&mut self, imp: &ImplBlock) {
        let generics = generics_str(&imp.generic_params);
        let target = type_str(&imp.target_type.node);
        match &imp.trait_name {
            Some(t) => self.line(&format!("Impl{generics} {} for {target}", t.name)),
            None => self.line(&format!("Impl{generics} {target}")),
        }
        self.indent += 1;
        for (i, method) in imp.methods.iter().enumerate() {
            self.prefix(i == imp.methods.len() - 1);
            self.print_fn_def(&method.node);
        }
        self.indent -= 1;
    }

    fn print_view_def(&mut self, v: &ViewDef) {
        let vis = vis_str(v.visibility);
        self.line(&format!("{vis}ViewDef \"{}\"", v.name.name));
        self.indent += 1;
        for field in &v.fields {
            let attr = match field.attribute {
                ViewFieldAttr::State => "@state",
                ViewFieldAttr::Prop => "@prop",
                ViewFieldAttr::Binding => "@binding",
            };
            self.prefix(false);
            self.line(&format!(
                "{attr} {}: {}",
                field.name.name,
                type_str(&field.ty.node)
            ));
        }
        self.prefix(true);
        self.line("Body");
        self.indent += 1;
        self.print_block(&v.body);
        self.indent -= 1;
        self.indent -= 1;
    }

    fn print_use_decl(&mut self, u: &UseDecl) {
        let path: Vec<&str> = u.path.iter().map(|i| i.name.as_str()).collect();
        let items = match &u.items {
            UseItems::All => ".*".to_string(),
            UseItems::Single => String::new(),
            UseItems::Multiple(ids) => {
                let names: Vec<&str> = ids.iter().map(|i| i.name.as_str()).collect();
                format!(".{{{}}}", names.join(", "))
            }
        };
        self.line(&format!("Use {}{items}", path.join(".")));
    }

    fn print_mod_decl(&mut self, m: &ModDecl) {
        let vis = vis_str(m.visibility);
        self.line(&format!("{vis}Mod \"{}\"", m.name.name));
    }

    // === Statements ===

    fn print_block(&mut self, block: &Block) {
        for (i, stmt) in block.stmts.iter().enumerate() {
            self.prefix(i == block.stmts.len() - 1);
            self.print_stmt(&stmt.node);
        }
    }

    fn print_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let(l) => {
                let m = match l.mutability {
                    Mutability::Mutable => "mut ",
                    Mutability::Immutable => "",
                };
                let ty = match &l.ty {
                    Some(t) => format!(": {}", type_str(&t.node)),
                    None => String::new(),
                };
                let op = if l.is_colon_assign { ":=" } else { "=" };
                self.line(&format!("Let {m}{}{ty} {op}", l.name.name));
                self.indent += 1;
                self.prefix(true);
                self.print_expr(&l.value.node);
                self.indent -= 1;
            }
            Stmt::Expr(e) => self.print_expr(&e.node),
            Stmt::For(f) => {
                self.line(&format!("For {} in", f.binding.name));
                self.indent += 1;
                self.prefix(false);
                self.print_expr(&f.iterable.node);
                self.prefix(true);
                self.line("Body");
                self.indent += 1;
                self.print_block(&f.body);
                self.indent -= 1;
                self.indent -= 1;
            }
            Stmt::While(w) => {
                self.line("While");
                self.indent += 1;
                self.prefix(false);
                self.line("Condition");
                self.indent += 1;
                self.prefix(true);
                self.print_expr(&w.condition.node);
                self.indent -= 1;
                self.prefix(true);
                self.line("Body");
                self.indent += 1;
                self.print_block(&w.body);
                self.indent -= 1;
                self.indent -= 1;
            }
            Stmt::Loop(block) => {
                self.line("Loop");
                self.indent += 1;
                self.print_block(block);
                self.indent -= 1;
            }
            Stmt::Item(item) => self.print_item(&item.node),
        }
    }

    // === Expressions ===

    fn print_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::IntLiteral(n) => self.line(&format!("Int({n})")),
            Expr::FloatLiteral(n) => self.line(&format!("Float({n})")),
            Expr::StringLiteral(s) => self.line(&format!("String({s:?})")),
            Expr::StringInterpolation(parts) => {
                self.line("StringInterpolation");
                self.indent += 1;
                for (i, part) in parts.iter().enumerate() {
                    self.prefix(i == parts.len() - 1);
                    match part {
                        StringPart::Literal(s) => self.line(&format!("Literal({s:?})")),
                        StringPart::Interpolation(e) => {
                            self.line("Interpolation");
                            self.indent += 1;
                            self.prefix(true);
                            self.print_expr(&e.node);
                            self.indent -= 1;
                        }
                    }
                }
                self.indent -= 1;
            }
            Expr::CharLiteral(c) => self.line(&format!("Char({c:?})")),
            Expr::BoolLiteral(b) => self.line(&format!("Bool({b})")),
            Expr::NilLiteral => self.line("Nil"),
            Expr::Identifier(id) => self.line(&format!("Ident({})", id.name)),
            Expr::Path(parts) => {
                let path: Vec<&str> = parts.iter().map(|i| i.name.as_str()).collect();
                self.line(&format!("Path({})", path.join(".")));
            }
            Expr::Binary(b) => {
                self.line(&format!("Binary({:?})", b.op));
                self.indent += 1;
                self.prefix(false);
                self.print_expr(&b.left.node);
                self.prefix(true);
                self.print_expr(&b.right.node);
                self.indent -= 1;
            }
            Expr::Unary(u) => {
                self.line(&format!("Unary({:?})", u.op));
                self.indent += 1;
                self.prefix(true);
                self.print_expr(&u.operand.node);
                self.indent -= 1;
            }
            Expr::Call(c) => {
                self.line("Call");
                self.indent += 1;
                self.prefix(false);
                self.line("Callee");
                self.indent += 1;
                self.prefix(true);
                self.print_expr(&c.callee.node);
                self.indent -= 1;
                if !c.generic_args.is_empty() {
                    self.prefix(false);
                    let args: Vec<String> =
                        c.generic_args.iter().map(|a| type_str(&a.node)).collect();
                    self.line(&format!("GenericArgs: [{}]", args.join(", ")));
                }
                self.prefix(true);
                self.line("Args");
                self.indent += 1;
                for (i, arg) in c.args.iter().enumerate() {
                    self.prefix(i == c.args.len() - 1);
                    self.print_expr(&arg.node);
                }
                self.indent -= 1;
                self.indent -= 1;
            }
            Expr::MethodCall(m) => {
                self.line(&format!("MethodCall .{}", m.method.name));
                self.indent += 1;
                self.prefix(false);
                self.line("Receiver");
                self.indent += 1;
                self.prefix(true);
                self.print_expr(&m.receiver.node);
                self.indent -= 1;
                self.prefix(true);
                self.line("Args");
                self.indent += 1;
                for (i, arg) in m.args.iter().enumerate() {
                    self.prefix(i == m.args.len() - 1);
                    self.print_expr(&arg.node);
                }
                self.indent -= 1;
                self.indent -= 1;
            }
            Expr::FieldAccess(f) => {
                self.line(&format!("FieldAccess .{}", f.field.name));
                self.indent += 1;
                self.prefix(true);
                self.print_expr(&f.object.node);
                self.indent -= 1;
            }
            Expr::Index(idx) => {
                self.line("Index");
                self.indent += 1;
                self.prefix(false);
                self.print_expr(&idx.object.node);
                self.prefix(true);
                self.print_expr(&idx.index.node);
                self.indent -= 1;
            }
            Expr::StructLiteral(s) => {
                self.line(&format!("StructLiteral {}", s.name.name));
                self.indent += 1;
                for (i, (name, val)) in s.fields.iter().enumerate() {
                    self.prefix(i == s.fields.len() - 1);
                    self.line(&format!("{}: ", name.name));
                    self.indent += 1;
                    self.prefix(true);
                    self.print_expr(&val.node);
                    self.indent -= 1;
                }
                self.indent -= 1;
            }
            Expr::ArrayLiteral(elems) => {
                self.line(&format!("ArrayLiteral [{}]", elems.len()));
                self.indent += 1;
                for (i, elem) in elems.iter().enumerate() {
                    self.prefix(i == elems.len() - 1);
                    self.print_expr(&elem.node);
                }
                self.indent -= 1;
            }
            Expr::TupleLiteral(elems) => {
                self.line(&format!("TupleLiteral ({})", elems.len()));
                self.indent += 1;
                for (i, elem) in elems.iter().enumerate() {
                    self.prefix(i == elems.len() - 1);
                    self.print_expr(&elem.node);
                }
                self.indent -= 1;
            }
            Expr::Block(block) => {
                self.line("Block");
                self.indent += 1;
                self.print_block(block);
                self.indent -= 1;
            }
            Expr::If(if_expr) => {
                self.line("If");
                self.indent += 1;
                self.prefix(false);
                self.line("Condition");
                self.indent += 1;
                self.prefix(true);
                self.print_expr(&if_expr.condition.node);
                self.indent -= 1;
                let has_else = if_expr.else_block.is_some();
                self.prefix(!has_else);
                self.line("Then");
                self.indent += 1;
                self.print_block(&if_expr.then_block);
                self.indent -= 1;
                if let Some(else_block) = &if_expr.else_block {
                    self.prefix(true);
                    match else_block {
                        ElseBlock::Else(block) => {
                            self.line("Else");
                            self.indent += 1;
                            self.print_block(block);
                            self.indent -= 1;
                        }
                        ElseBlock::ElseIf(elif) => {
                            self.line("ElseIf");
                            self.indent += 1;
                            self.prefix(true);
                            self.print_expr(&Expr::If(Box::new(elif.node.clone())));
                            self.indent -= 1;
                        }
                    }
                }
                self.indent -= 1;
            }
            Expr::Match(m) => {
                self.line("Match");
                self.indent += 1;
                self.prefix(false);
                self.line("Scrutinee");
                self.indent += 1;
                self.prefix(true);
                self.print_expr(&m.scrutinee.node);
                self.indent -= 1;
                for (i, arm) in m.arms.iter().enumerate() {
                    self.prefix(i == m.arms.len() - 1);
                    self.line("Arm");
                    self.indent += 1;
                    self.prefix(false);
                    self.print_pattern(&arm.pattern.node);
                    if let Some(guard) = &arm.guard {
                        self.prefix(false);
                        self.line("Guard");
                        self.indent += 1;
                        self.prefix(true);
                        self.print_expr(&guard.node);
                        self.indent -= 1;
                    }
                    self.prefix(true);
                    self.print_expr(&arm.body.node);
                    self.indent -= 1;
                }
                self.indent -= 1;
            }
            Expr::Closure(c) => {
                let params: Vec<String> = c
                    .params
                    .iter()
                    .map(|p| match &p.ty {
                        Some(t) => format!("{}: {}", p.name.name, type_str(&t.node)),
                        None => p.name.name.clone(),
                    })
                    .collect();
                self.line(&format!("Closure |{}|", params.join(", ")));
                self.indent += 1;
                self.prefix(true);
                self.print_expr(&c.body.node);
                self.indent -= 1;
            }
            Expr::Assign(a) => {
                self.line("Assign");
                self.indent += 1;
                self.prefix(false);
                self.print_expr(&a.target.node);
                self.prefix(true);
                self.print_expr(&a.value.node);
                self.indent -= 1;
            }
            Expr::Range(r) => {
                let op = if r.inclusive { "..=" } else { ".." };
                self.line(&format!("Range({op})"));
                self.indent += 1;
                self.prefix(false);
                self.print_expr(&r.start.node);
                self.prefix(true);
                self.print_expr(&r.end.node);
                self.indent -= 1;
            }
            Expr::Try(e) => {
                self.line("Try(?)");
                self.indent += 1;
                self.prefix(true);
                self.print_expr(&e.node);
                self.indent -= 1;
            }
            Expr::Spawn(block) => {
                self.line("Spawn");
                self.indent += 1;
                self.print_block(block);
                self.indent -= 1;
            }
            Expr::ChanCreate(c) => {
                let cap = match &c.capacity {
                    Some(_) => "buffered",
                    None => "unbuffered",
                };
                self.line(&format!(
                    "ChanCreate({cap}) chan[{}]",
                    type_str(&c.element_type.node)
                ));
            }
            Expr::Select(s) => {
                self.line("Select");
                self.indent += 1;
                for (i, arm) in s.arms.iter().enumerate() {
                    self.prefix(i == s.arms.len() - 1);
                    match &arm.kind {
                        SelectArmKind::Recv { binding, .. } => {
                            self.line(&format!("Recv -> {}", binding.name));
                        }
                        SelectArmKind::Send { .. } => {
                            self.line("Send");
                        }
                        SelectArmKind::After(_) => {
                            self.line("After");
                        }
                    }
                    self.indent += 1;
                    self.print_block(&arm.body);
                    self.indent -= 1;
                }
                self.indent -= 1;
            }
            Expr::Return(val) => match val {
                Some(e) => {
                    self.line("Return");
                    self.indent += 1;
                    self.prefix(true);
                    self.print_expr(&e.node);
                    self.indent -= 1;
                }
                None => self.line("Return (void)"),
            },
            Expr::Break(val) => match val {
                Some(e) => {
                    self.line("Break");
                    self.indent += 1;
                    self.prefix(true);
                    self.print_expr(&e.node);
                    self.indent -= 1;
                }
                None => self.line("Break"),
            },
            Expr::Continue => self.line("Continue"),
        }
    }

    // === Patterns ===

    fn print_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Wildcard => self.line("Pattern::_"),
            Pattern::Binding(id) => self.line(&format!("Pattern::Bind({})", id.name)),
            Pattern::Literal(e) => {
                self.line("Pattern::Literal");
                self.indent += 1;
                self.prefix(true);
                self.print_expr(&e.node);
                self.indent -= 1;
            }
            Pattern::Variant(v) => {
                self.line(&format!("Pattern::Variant({})", v.name.name));
                self.indent += 1;
                for (i, field) in v.fields.iter().enumerate() {
                    self.prefix(i == v.fields.len() - 1);
                    self.print_pattern(&field.node);
                }
                self.indent -= 1;
            }
            Pattern::Struct(s) => {
                let rest = if s.has_rest { ", .." } else { "" };
                self.line(&format!("Pattern::Struct({}{})", s.name.name, rest));
                self.indent += 1;
                for (i, (name, pat)) in s.fields.iter().enumerate() {
                    self.prefix(i == s.fields.len() - 1);
                    self.line(&format!("{}: ", name.name));
                    self.indent += 1;
                    self.prefix(true);
                    self.print_pattern(&pat.node);
                    self.indent -= 1;
                }
                self.indent -= 1;
            }
            Pattern::Tuple(elems) => {
                self.line(&format!("Pattern::Tuple({})", elems.len()));
                self.indent += 1;
                for (i, elem) in elems.iter().enumerate() {
                    self.prefix(i == elems.len() - 1);
                    self.print_pattern(&elem.node);
                }
                self.indent -= 1;
            }
            Pattern::Or(alts) => {
                self.line("Pattern::Or");
                self.indent += 1;
                for (i, alt) in alts.iter().enumerate() {
                    self.prefix(i == alts.len() - 1);
                    self.print_pattern(&alt.node);
                }
                self.indent -= 1;
            }
            Pattern::Rest => self.line("Pattern::.."),
        }
    }

    // === Output helpers ===

    fn line(&mut self, text: &str) {
        self.output.push_str(text);
        self.output.push('\n');
    }

    fn prefix(&mut self, is_last: bool) {
        for i in 0..self.indent {
            if i == self.indent - 1 {
                if is_last {
                    self.output.push_str("└── ");
                } else {
                    self.output.push_str("├── ");
                }
            } else {
                self.output.push_str("│   ");
            }
        }
    }

    fn child(&mut self, is_last: bool, text: &str) {
        self.prefix(is_last);
        self.line(text);
    }

    fn child_start(&mut self, is_last: bool, text: &str) {
        self.prefix(is_last);
        self.line(text);
    }
}

impl Default for PrettyPrinter {
    fn default() -> Self {
        Self::new()
    }
}

/// Convenience function to pretty-print a source file.
pub fn pretty_print(file: &SourceFile) -> String {
    let mut pp = PrettyPrinter::new();
    pp.print_source_file(file);
    pp.output
}

// === Helper functions ===

fn vis_str(vis: Visibility) -> &'static str {
    match vis {
        Visibility::Public => "pub ",
        Visibility::Private => "",
    }
}

fn generics_str(params: &[GenericParam]) -> String {
    if params.is_empty() {
        return String::new();
    }
    let parts: Vec<String> = params
        .iter()
        .map(|p| {
            if p.bounds.is_empty() {
                p.name.name.clone()
            } else {
                let bounds: Vec<&str> = p.bounds.iter().map(|b| b.name.as_str()).collect();
                format!("{}: {}", p.name.name, bounds.join(" + "))
            }
        })
        .collect();
    format!("[{}]", parts.join(", "))
}

fn ownership_str(ownership: ParamOwnership) -> &'static str {
    match ownership {
        ParamOwnership::Borrow => "",
        ParamOwnership::Own => "own ",
        ParamOwnership::MutBorrow => "mut ",
        ParamOwnership::SelfBorrow => "self",
        ParamOwnership::SelfMutBorrow => "mut self",
        ParamOwnership::SelfOwn => "own self",
    }
}

fn type_str(ty: &Type) -> String {
    match ty {
        Type::Named(path) => {
            let mut s = path.name.name.clone();
            if !path.generic_args.is_empty() {
                let args: Vec<String> = path
                    .generic_args
                    .iter()
                    .map(|a| type_str(&a.node))
                    .collect();
                s.push_str(&format!("[{}]", args.join(", ")));
            }
            s
        }
        Type::Reference(inner) => format!("&{}", type_str(&inner.node)),
        Type::MutReference(inner) => format!("&mut {}", type_str(&inner.node)),
        Type::Array(arr) => match &arr.size {
            Some(_) => format!("[{}; N]", type_str(&arr.element.node)),
            None => format!("[{}]", type_str(&arr.element.node)),
        },
        Type::Tuple(elems) => {
            let parts: Vec<String> = elems.iter().map(|e| type_str(&e.node)).collect();
            format!("({})", parts.join(", "))
        }
        Type::Optional(inner) => format!("?{}", type_str(&inner.node)),
        Type::Result(r) => format!("{} ! {}", type_str(&r.ok.node), type_str(&r.err.node)),
        Type::Function(f) => {
            let params: Vec<String> = f.params.iter().map(|p| type_str(&p.node)).collect();
            format!(
                "fn({}) -> {}",
                params.join(", "),
                type_str(&f.return_type.node)
            )
        }
        Type::Channel(inner) => format!("chan[{}]", type_str(&inner.node)),
        Type::Inferred => "_".to_string(),
    }
}

/// Format a type for display (public API).
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", type_str(self))
    }
}
