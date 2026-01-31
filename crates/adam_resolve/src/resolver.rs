//! Scope builder — walks the AST and resolves names.

use adam_ast::common::*;
use adam_ast::expr::*;
use adam_ast::item::*;
use adam_ast::pattern::Pattern;
use adam_ast::stmt::*;
use adam_ast::types::Type;

use crate::errors::{suggest, ResolveError, ResolveErrorKind};
use crate::scope::*;

/// Result of name resolution.
#[derive(Debug)]
pub struct ResolveResult {
    pub scope_tree: ScopeTree,
    pub declarations: Vec<Declaration>,
    pub errors: Vec<ResolveError>,
}

/// Resolve all names in a source file.
pub fn resolve(ast: &SourceFile) -> ResolveResult {
    let mut resolver = Resolver::new();
    resolver.resolve_source_file(ast);
    ResolveResult {
        scope_tree: resolver.scope_tree,
        declarations: resolver.declarations,
        errors: resolver.errors,
    }
}

/// The resolver walks the AST and builds the scope tree.
struct Resolver {
    scope_tree: ScopeTree,
    current_scope: ScopeId,
    declarations: Vec<Declaration>,
    errors: Vec<ResolveError>,
}

impl Resolver {
    fn new() -> Self {
        let scope_tree = ScopeTree::new();
        let root = scope_tree.root();
        Self {
            scope_tree,
            current_scope: root,
            declarations: vec![],
            errors: vec![],
        }
    }

    // ---- Scope management ----

    fn enter_scope(&mut self, kind: ScopeKind) -> ScopeId {
        let id = self.scope_tree.add_scope(self.current_scope, kind);
        self.current_scope = id;
        id
    }

    fn exit_scope(&mut self) {
        let parent = self
            .scope_tree
            .scope(self.current_scope)
            .parent
            .expect("cannot exit root scope");
        self.current_scope = parent;
    }

    // ---- Declaration ----

    fn declare(&mut self, name: &str, kind: DeclKind, span: Span) -> DeclId {
        let id = self.declarations.len() as DeclId;
        let decl = Declaration {
            id,
            name: name.to_string(),
            kind,
            span,
            scope: self.current_scope,
        };
        self.declarations.push(decl);

        if let Err(existing_id) =
            self.scope_tree
                .declare(self.current_scope, name.to_string(), id)
        {
            let prev_span = self.declarations[existing_id as usize].span;
            self.errors.push(ResolveError::new(
                ResolveErrorKind::DuplicateDefinition {
                    name: name.to_string(),
                    previous_span: prev_span,
                },
                span,
            ));
        }

        id
    }

    // ---- Lookup ----

    fn lookup(&self, name: &str) -> Option<DeclId> {
        self.scope_tree.lookup(self.current_scope, name)
    }

    fn check_name(&mut self, name: &str, span: Span) {
        if self.lookup(name).is_none() {
            let candidates = self.scope_tree.names_in_scope(self.current_scope);
            let suggestion = suggest(name, &candidates);
            self.errors.push(ResolveError::new(
                ResolveErrorKind::UndefinedName {
                    name: name.to_string(),
                    suggestion,
                },
                span,
            ));
        }
    }

    fn check_type_name(&mut self, name: &str, span: Span) {
        // Built-in types are always valid.
        if is_builtin_type(name) {
            return;
        }
        if self.lookup(name).is_none() {
            let candidates = self.scope_tree.names_in_scope(self.current_scope);
            let suggestion = suggest(name, &candidates);
            self.errors.push(ResolveError::new(
                ResolveErrorKind::UndefinedType {
                    name: name.to_string(),
                    suggestion,
                },
                span,
            ));
        }
    }

    // ---- Source file ----

    fn resolve_source_file(&mut self, file: &SourceFile) {
        // First pass: declare all top-level names so order doesn't matter.
        for item in &file.items {
            self.declare_item_name(&item.node);
        }

        // Second pass: resolve bodies.
        for item in &file.items {
            self.resolve_item(&item.node);
        }
    }

    /// Declare the name introduced by a top-level item (first pass).
    fn declare_item_name(&mut self, item: &Item) {
        match item {
            Item::Function(f) => {
                self.declare(&f.name.name, DeclKind::Function, f.name.span);
            }
            Item::Struct(s) => {
                self.declare(&s.name.name, DeclKind::Struct, s.name.span);
            }
            Item::Enum(e) => {
                self.declare(&e.name.name, DeclKind::Enum, e.name.span);
                // Also declare variant names at module level.
                for variant in &e.variants {
                    self.declare(
                        &variant.name.name,
                        DeclKind::EnumVariant,
                        variant.name.span,
                    );
                }
            }
            Item::Trait(t) => {
                self.declare(&t.name.name, DeclKind::Trait, t.name.span);
            }
            Item::Impl(_) => {
                // impl blocks don't introduce a name at module scope.
            }
            Item::View(v) => {
                self.declare(&v.name.name, DeclKind::Struct, v.name.span);
            }
            Item::Use(u) => {
                self.resolve_use_decl(u);
            }
            Item::Mod(m) => {
                self.declare(&m.name.name, DeclKind::Module, m.name.span);
            }
        }
    }

    // ---- Items ----

    fn resolve_item(&mut self, item: &Item) {
        match item {
            Item::Function(f) => self.resolve_fn(f),
            Item::Struct(s) => self.resolve_struct(s),
            Item::Enum(e) => self.resolve_enum(e),
            Item::Trait(t) => self.resolve_trait(t),
            Item::Impl(i) => self.resolve_impl(i),
            Item::View(v) => self.resolve_view(v),
            Item::Use(_) => { /* already handled in first pass */ }
            Item::Mod(_) => { /* just a declaration */ }
        }
    }

    fn resolve_fn(&mut self, func: &FnDef) {
        self.enter_scope(ScopeKind::Function);

        // Declare generic params.
        for gp in &func.generic_params {
            self.declare(&gp.name.name, DeclKind::TypeParam, gp.name.span);
            for bound in &gp.bounds {
                self.check_type_name(&bound.name, bound.span);
            }
        }

        // Declare params.
        for param in &func.params {
            let ownership = param.ownership;
            self.declare(
                &param.name.name,
                DeclKind::Param { ownership },
                param.name.span,
            );
            self.resolve_type(&param.ty.node);
        }

        // Return type.
        if let Some(ret) = &func.return_type {
            self.resolve_type(&ret.node);
        }

        // Body.
        if let Some(body) = &func.body {
            self.resolve_block_stmts(body);
        }

        self.exit_scope();
    }

    fn resolve_struct(&mut self, struc: &StructDef) {
        if !struc.generic_params.is_empty() {
            self.enter_scope(ScopeKind::Block);
            for gp in &struc.generic_params {
                self.declare(&gp.name.name, DeclKind::TypeParam, gp.name.span);
                for bound in &gp.bounds {
                    self.check_type_name(&bound.name, bound.span);
                }
            }
            for field in &struc.fields {
                self.resolve_type(&field.ty.node);
            }
            self.exit_scope();
        } else {
            for field in &struc.fields {
                self.resolve_type(&field.ty.node);
            }
        }
    }

    fn resolve_enum(&mut self, enu: &EnumDef) {
        if !enu.generic_params.is_empty() {
            self.enter_scope(ScopeKind::Block);
            for gp in &enu.generic_params {
                self.declare(&gp.name.name, DeclKind::TypeParam, gp.name.span);
                for bound in &gp.bounds {
                    self.check_type_name(&bound.name, bound.span);
                }
            }
            for variant in &enu.variants {
                for field_ty in &variant.fields {
                    self.resolve_type(&field_ty.node);
                }
            }
            self.exit_scope();
        } else {
            for variant in &enu.variants {
                for field_ty in &variant.fields {
                    self.resolve_type(&field_ty.node);
                }
            }
        }
    }

    fn resolve_trait(&mut self, trai: &TraitDef) {
        self.enter_scope(ScopeKind::Trait);

        for gp in &trai.generic_params {
            self.declare(&gp.name.name, DeclKind::TypeParam, gp.name.span);
        }

        // Declare Self type.
        self.declare("Self", DeclKind::TypeParam, trai.name.span);

        for method in &trai.methods {
            self.declare(
                &method.node.name.name,
                DeclKind::Method,
                method.node.name.span,
            );
            self.resolve_fn(&method.node);
        }

        self.exit_scope();
    }

    fn resolve_impl(&mut self, imp: &ImplBlock) {
        self.enter_scope(ScopeKind::Impl);

        for gp in &imp.generic_params {
            self.declare(&gp.name.name, DeclKind::TypeParam, gp.name.span);
        }

        // Declare Self type.
        if let Type::Named(path) = &imp.target_type.node {
            self.declare("Self", DeclKind::TypeParam, path.name.span);
        }

        // Check trait name.
        if let Some(trait_name) = &imp.trait_name {
            self.check_type_name(&trait_name.name, trait_name.span);
        }

        // Resolve target type.
        self.resolve_type(&imp.target_type.node);

        // First pass: declare method names.
        for method in &imp.methods {
            self.declare(
                &method.node.name.name,
                DeclKind::Method,
                method.node.name.span,
            );
        }

        // Second pass: resolve method bodies.
        for method in &imp.methods {
            self.resolve_fn(&method.node);
        }

        self.exit_scope();
    }

    fn resolve_view(&mut self, view: &ViewDef) {
        self.enter_scope(ScopeKind::View);

        // Declare view fields (@state, @prop, @binding).
        for field in &view.fields {
            self.declare(
                &field.name.name,
                DeclKind::ViewField,
                field.name.span,
            );
            self.resolve_type(&field.ty.node);
            if let Some(default) = &field.default {
                self.resolve_expr(&default.node);
            }
        }

        // Resolve body.
        self.resolve_block_stmts(&view.body);

        self.exit_scope();
    }

    fn resolve_use_decl(&mut self, use_decl: &UseDecl) {
        // For now, just declare the imported names. Full cross-file
        // resolution comes when the module system is wired up.
        match &use_decl.items {
            UseItems::Single => {
                // Last path segment is the imported name.
                if let Some(last) = use_decl.path.last() {
                    self.declare(&last.name, DeclKind::Import, last.span);
                }
            }
            UseItems::All => {
                // Wildcard import — nothing to declare until we know the module's exports.
            }
            UseItems::Multiple(names) => {
                for name in names {
                    self.declare(&name.name, DeclKind::Import, name.span);
                }
            }
        }
    }

    // ---- Blocks and statements ----

    /// Resolve statements in a block without creating a new scope
    /// (caller creates the scope).
    fn resolve_block_stmts(&mut self, block: &Block) {
        for stmt in &block.stmts {
            self.resolve_stmt(&stmt.node);
        }
    }

    /// Resolve statements in a block with a new Block scope.
    fn resolve_block(&mut self, block: &Block) {
        self.enter_scope(ScopeKind::Block);
        self.resolve_block_stmts(block);
        self.exit_scope();
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let(let_stmt) => {
                // Resolve value first (before declaring the name).
                self.resolve_expr(&let_stmt.value.node);
                if let Some(ty) = &let_stmt.ty {
                    self.resolve_type(&ty.node);
                }
                let mutable = let_stmt.mutability == Mutability::Mutable;
                self.declare(
                    &let_stmt.name.name,
                    DeclKind::Variable { mutable },
                    let_stmt.name.span,
                );
            }
            Stmt::Expr(expr) => {
                self.resolve_expr(&expr.node);
            }
            Stmt::For(for_stmt) => {
                // Resolve iterable in the outer scope.
                self.resolve_expr(&for_stmt.iterable.node);

                // Loop body gets its own scope with the binding.
                self.enter_scope(ScopeKind::Loop);
                self.declare(
                    &for_stmt.binding.name,
                    DeclKind::Variable { mutable: false },
                    for_stmt.binding.span,
                );
                self.resolve_block_stmts(&for_stmt.body);
                self.exit_scope();
            }
            Stmt::While(while_stmt) => {
                self.resolve_expr(&while_stmt.condition.node);
                self.enter_scope(ScopeKind::Loop);
                self.resolve_block_stmts(&while_stmt.body);
                self.exit_scope();
            }
            Stmt::Loop(block) => {
                self.enter_scope(ScopeKind::Loop);
                self.resolve_block_stmts(block);
                self.exit_scope();
            }
            Stmt::Item(item) => {
                // Items in blocks — declare then resolve.
                self.declare_item_name(&item.node);
                self.resolve_item(&item.node);
            }
        }
    }

    // ---- Expressions ----

    fn resolve_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::IntLiteral(_)
            | Expr::FloatLiteral(_)
            | Expr::StringLiteral(_)
            | Expr::CharLiteral(_)
            | Expr::BoolLiteral(_)
            | Expr::NilLiteral
            | Expr::Continue => {}

            Expr::Identifier(ident) => {
                self.check_name(&ident.name, ident.span);
            }

            Expr::Path(segments) => {
                // Check the first segment (the rest are field/method lookups).
                if let Some(first) = segments.first() {
                    self.check_name(&first.name, first.span);
                }
            }

            Expr::StringInterpolation(parts) => {
                for part in parts {
                    if let StringPart::Interpolation(expr) = part {
                        self.resolve_expr(&expr.node);
                    }
                }
            }

            Expr::Binary(bin) => {
                self.resolve_expr(&bin.left.node);
                self.resolve_expr(&bin.right.node);
            }

            Expr::Unary(unary) => {
                self.resolve_expr(&unary.operand.node);
            }

            Expr::Call(call) => {
                self.resolve_expr(&call.callee.node);
                for arg in &call.args {
                    self.resolve_expr(&arg.node);
                }
                for ga in &call.generic_args {
                    self.resolve_type(&ga.node);
                }
            }

            Expr::MethodCall(mc) => {
                self.resolve_expr(&mc.receiver.node);
                // Method name is resolved by type checker, not here.
                for arg in &mc.args {
                    self.resolve_expr(&arg.node);
                }
                for ga in &mc.generic_args {
                    self.resolve_type(&ga.node);
                }
            }

            Expr::FieldAccess(fa) => {
                self.resolve_expr(&fa.object.node);
                // Field name resolved by type checker.
            }

            Expr::Index(idx) => {
                self.resolve_expr(&idx.object.node);
                self.resolve_expr(&idx.index.node);
            }

            Expr::StructLiteral(sl) => {
                self.check_type_name(&sl.name.name, sl.name.span);
                for (_, value) in &sl.fields {
                    self.resolve_expr(&value.node);
                }
            }

            Expr::ArrayLiteral(elems) => {
                for elem in elems {
                    self.resolve_expr(&elem.node);
                }
            }

            Expr::TupleLiteral(elems) => {
                for elem in elems {
                    self.resolve_expr(&elem.node);
                }
            }

            Expr::Block(block) => {
                self.resolve_block(block);
            }

            Expr::If(if_expr) => {
                self.resolve_if(if_expr);
            }

            Expr::Match(match_expr) => {
                self.resolve_expr(&match_expr.scrutinee.node);
                for arm in &match_expr.arms {
                    self.resolve_match_arm(arm);
                }
            }

            Expr::Closure(closure) => {
                self.enter_scope(ScopeKind::Closure);
                for param in &closure.params {
                    self.declare(
                        &param.name.name,
                        DeclKind::Param {
                            ownership: ParamOwnership::Borrow,
                        },
                        param.name.span,
                    );
                    if let Some(ty) = &param.ty {
                        self.resolve_type(&ty.node);
                    }
                }
                self.resolve_expr(&closure.body.node);
                self.exit_scope();
            }

            Expr::Assign(assign) => {
                self.resolve_expr(&assign.target.node);
                self.resolve_expr(&assign.value.node);
            }

            Expr::Range(range) => {
                self.resolve_expr(&range.start.node);
                self.resolve_expr(&range.end.node);
            }

            Expr::Try(inner) => {
                self.resolve_expr(&inner.node);
            }

            Expr::Spawn(block) => {
                self.resolve_block(block);
            }

            Expr::ChanCreate(cc) => {
                self.resolve_type(&cc.element_type.node);
                if let Some(cap) = &cc.capacity {
                    self.resolve_expr(&cap.node);
                }
            }

            Expr::Select(sel) => {
                for arm in &sel.arms {
                    self.resolve_select_arm(arm);
                }
            }

            Expr::Return(val) => {
                if let Some(v) = val {
                    self.resolve_expr(&v.node);
                }
            }

            Expr::Break(val) => {
                if let Some(v) = val {
                    self.resolve_expr(&v.node);
                }
            }
        }
    }

    fn resolve_if(&mut self, if_expr: &IfExpr) {
        self.resolve_expr(&if_expr.condition.node);
        self.resolve_block(&if_expr.then_block);
        if let Some(else_block) = &if_expr.else_block {
            match else_block {
                ElseBlock::Else(block) => self.resolve_block(block),
                ElseBlock::ElseIf(elif) => self.resolve_if(&elif.node),
            }
        }
    }

    fn resolve_match_arm(&mut self, arm: &MatchArm) {
        self.enter_scope(ScopeKind::Match);
        self.resolve_pattern(&arm.pattern.node);
        if let Some(guard) = &arm.guard {
            self.resolve_expr(&guard.node);
        }
        self.resolve_expr(&arm.body.node);
        self.exit_scope();
    }

    fn resolve_select_arm(&mut self, arm: &SelectArm) {
        match &arm.kind {
            SelectArmKind::Recv { binding, channel } => {
                self.resolve_expr(&channel.node);
                self.enter_scope(ScopeKind::Block);
                self.declare(
                    &binding.name,
                    DeclKind::Variable { mutable: false },
                    binding.span,
                );
                self.resolve_block_stmts(&arm.body);
                self.exit_scope();
            }
            SelectArmKind::Send { channel, value } => {
                self.resolve_expr(&channel.node);
                self.resolve_expr(&value.node);
                self.resolve_block(&arm.body);
            }
            SelectArmKind::After(dur) => {
                self.resolve_expr(&dur.node);
                self.resolve_block(&arm.body);
            }
        }
    }

    // ---- Patterns ----

    fn resolve_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Wildcard | Pattern::Rest => {}

            Pattern::Binding(ident) => {
                self.declare(
                    &ident.name,
                    DeclKind::Variable { mutable: false },
                    ident.span,
                );
            }

            Pattern::Literal(_) => {}

            Pattern::Variant(vp) => {
                // Check that the variant name (or qualified name) exists.
                // For qualified names like "Shape.Circle", we check "Shape".
                let name = &vp.name.name;
                if let Some(dot_pos) = name.find('.') {
                    let enum_name = &name[..dot_pos];
                    self.check_name(enum_name, vp.name.span);
                } else {
                    self.check_name(name, vp.name.span);
                }

                for field in &vp.fields {
                    self.resolve_pattern(&field.node);
                }
            }

            Pattern::Struct(sp) => {
                self.check_type_name(&sp.name.name, sp.name.span);
                for (_, pat) in &sp.fields {
                    self.resolve_pattern(&pat.node);
                }
            }

            Pattern::Tuple(pats) => {
                for pat in pats {
                    self.resolve_pattern(&pat.node);
                }
            }

            Pattern::Or(pats) => {
                for pat in pats {
                    self.resolve_pattern(&pat.node);
                }
            }
        }
    }

    // ---- Types ----

    fn resolve_type(&mut self, ty: &Type) {
        match ty {
            Type::Named(path) => {
                self.check_type_name(&path.name.name, path.name.span);
                for arg in &path.generic_args {
                    self.resolve_type(&arg.node);
                }
            }
            Type::Reference(inner) | Type::MutReference(inner) | Type::Optional(inner) => {
                self.resolve_type(&inner.node);
            }
            Type::Array(arr) => {
                self.resolve_type(&arr.element.node);
                if let Some(size) = &arr.size {
                    self.resolve_expr(&size.node);
                }
            }
            Type::Tuple(types) => {
                for t in types {
                    self.resolve_type(&t.node);
                }
            }
            Type::Result(res) => {
                self.resolve_type(&res.ok.node);
                self.resolve_type(&res.err.node);
            }
            Type::Function(ft) => {
                for param in &ft.params {
                    self.resolve_type(&param.node);
                }
                self.resolve_type(&ft.return_type.node);
            }
            Type::Channel(inner) => {
                self.resolve_type(&inner.node);
            }
            Type::Inferred => {}
        }
    }
}

/// Check if a type name is a built-in primitive type.
fn is_builtin_type(name: &str) -> bool {
    matches!(
        name,
        "i8" | "i16"
            | "i32"
            | "i64"
            | "u8"
            | "u16"
            | "u32"
            | "u64"
            | "f32"
            | "f64"
            | "bool"
            | "char"
            | "String"
            | "str"
            | "Self"
            | "Map"
            | "Set"
            | "Box"
            | "Rc"
            | "Arc"
    )
}
