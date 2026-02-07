//! Borrow checker — walks the AST enforcing ownership and borrowing rules.
//!
//! The borrow checker runs *after* name resolution and type checking.
//! It uses the resolved declarations and inferred types to determine
//! which operations constitute moves, borrows, or mutable borrows,
//! and ensures no value is used after it has been moved, no mutable
//! borrow overlaps with any other borrow, etc.

use std::collections::HashMap;

use adam_ast::common::*;
use adam_ast::expr::*;
use adam_ast::item::*;
use adam_ast::stmt::*;
use adam_ast::types::Type;
use adam_resolve::ResolveResult;
use adam_types::TypeCheckResult;

use crate::ownership::{BorrowOrigin, OwnershipTracker, VarState};

// ---------------------------------------------------------------------------
// Error types
// ---------------------------------------------------------------------------

/// A borrow-check error.
#[derive(Debug, Clone)]
pub struct BorrowError {
    pub message: String,
    pub span: Span,
}

impl BorrowError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }
}

impl std::fmt::Display for BorrowError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}..{}] {}", self.span.start, self.span.end, self.message)
    }
}

/// Result of borrow checking.
#[derive(Debug)]
pub struct BorrowCheckResult {
    pub errors: Vec<BorrowError>,
}

// ---------------------------------------------------------------------------
// The borrow checker
// ---------------------------------------------------------------------------

/// Walks the AST and enforces ownership / borrowing rules.
pub struct BorrowChecker {
    /// Ownership tracker (stack of scopes is managed manually).
    tracker: OwnershipTracker,
    /// Collected errors.
    errors: Vec<BorrowError>,
    /// Known function parameter ownerships: fn_name -> vec of ParamOwnership.
    fn_param_ownerships: HashMap<String, Vec<ParamOwnership>>,
    /// Known type annotations for let bindings: var_name -> type_name string.
    var_types: HashMap<String, String>,
    /// Declarations from the resolver for mutability lookup.
    decl_mutability: HashMap<String, bool>,
    /// Current scope depth (0 = function body).
    scope_depth: usize,
    /// Variable name -> scope depth at which it was declared.
    var_scopes: HashMap<String, usize>,
    /// Borrow origins: ref_var_name -> what it points to.
    borrow_origins: HashMap<String, BorrowOrigin>,
    /// Names of function parameters (always outlive locals).
    fn_params: Vec<String>,
}

impl BorrowChecker {
    pub fn new() -> Self {
        Self {
            tracker: OwnershipTracker::new(),
            errors: vec![],
            fn_param_ownerships: HashMap::new(),
            var_types: HashMap::new(),
            decl_mutability: HashMap::new(),
            scope_depth: 0,
            var_scopes: HashMap::new(),
            borrow_origins: HashMap::new(),
            fn_params: Vec::new(),
        }
    }

    /// Run the borrow checker on a source file.
    /// Accepts optional results from name resolution and type checking
    /// for richer analysis, but works standalone too.
    pub fn check(
        mut self,
        ast: &SourceFile,
        _resolve: Option<&ResolveResult>,
        _types: Option<&TypeCheckResult>,
    ) -> BorrowCheckResult {
        self.check_source_file(ast);
        BorrowCheckResult {
            errors: self.errors,
        }
    }

    // ---- Helpers ----------------------------------------------------------

    fn error(&mut self, message: impl Into<String>, span: Span) {
        self.errors.push(BorrowError::new(message, span));
    }

    /// Determine the type name string for an expression (best effort).
    fn expr_type_name(&self, expr: &Expr) -> Option<String> {
        match expr {
            Expr::IntLiteral(_) => Some("i32".into()),
            Expr::FloatLiteral(_) => Some("f64".into()),
            Expr::BoolLiteral(_) => Some("bool".into()),
            Expr::CharLiteral(_) => Some("char".into()),
            Expr::StringLiteral(_) | Expr::StringInterpolation(_) => Some("String".into()),
            Expr::Identifier(ident) => self.var_types.get(&ident.name).cloned(),
            _ => None,
        }
    }

    /// Check if an expression is a Copy type (never moves).
    fn is_copy_expr(&self, expr: &Expr) -> bool {
        match expr {
            Expr::IntLiteral(_) => true,
            Expr::FloatLiteral(_) => true,
            Expr::BoolLiteral(_) => true,
            Expr::CharLiteral(_) => true,
            Expr::NilLiteral => true,
            Expr::Identifier(ident) => {
                if let Some(ty) = self.var_types.get(&ident.name) {
                    OwnershipTracker::is_copy_type(ty)
                } else {
                    false
                }
            }
            Expr::Binary(_) => {
                // Arithmetic and comparison results are typically Copy.
                true
            }
            Expr::Unary(u) => {
                matches!(u.op, UnaryOp::Neg | UnaryOp::Not)
            }
            _ => false,
        }
    }

    /// Extract the variable name from an expression if it is a simple identifier.
    fn expr_var_name(expr: &Expr) -> Option<&str> {
        match expr {
            Expr::Identifier(ident) => Some(&ident.name),
            _ => None,
        }
    }

    // ---- Source file -------------------------------------------------------

    fn check_source_file(&mut self, file: &SourceFile) {
        // First pass: collect function parameter ownerships.
        for item in &file.items {
            if let Item::Function(func) = &item.node {
                let ownerships: Vec<ParamOwnership> =
                    func.params.iter().map(|p| p.ownership).collect();
                self.fn_param_ownerships
                    .insert(func.name.name.clone(), ownerships);
            }
            if let Item::Impl(imp) = &item.node {
                for method in &imp.methods {
                    let ownerships: Vec<ParamOwnership> =
                        method.node.params.iter().map(|p| p.ownership).collect();
                    self.fn_param_ownerships
                        .insert(method.node.name.name.clone(), ownerships);
                }
            }
        }

        // Second pass: check each item.
        for item in &file.items {
            self.check_item(&item.node);
        }
    }

    fn check_item(&mut self, item: &Item) {
        match item {
            Item::Function(func) => self.check_fn(func),
            Item::Impl(imp) => self.check_impl(imp),
            _ => {}
        }
    }

    fn check_impl(&mut self, imp: &ImplBlock) {
        for method in &imp.methods {
            self.check_fn(&method.node);
        }
    }

    // ---- Functions ---------------------------------------------------------

    fn check_fn(&mut self, func: &FnDef) {
        // Save the outer state and create a fresh tracker for this function.
        let outer_tracker = std::mem::replace(&mut self.tracker, OwnershipTracker::new());
        let outer_var_types = self.var_types.clone();
        let outer_decl_mut = self.decl_mutability.clone();
        let outer_scope_depth = self.scope_depth;
        let outer_var_scopes = self.var_scopes.clone();
        let outer_borrow_origins = self.borrow_origins.clone();
        let outer_fn_params = self.fn_params.clone();

        self.scope_depth = 0;
        self.var_scopes.clear();
        self.borrow_origins.clear();
        self.fn_params.clear();

        // Register parameters.
        for param in &func.params {
            let mutable = param.ownership == ParamOwnership::MutBorrow
                || param.ownership == ParamOwnership::SelfMutBorrow;
            self.tracker.mark_owned(&param.name.name, mutable);

            // Record the type name for Copy checks.
            if let Some(ty_name) = self.type_to_name(&param.ty.node) {
                self.var_types.insert(param.name.name.clone(), ty_name);
            }
            self.decl_mutability.insert(param.name.name.clone(), mutable);

            // Register as function parameter for borrow origin analysis.
            self.fn_params.push(param.name.name.clone());
            self.var_scopes.insert(param.name.name.clone(), 0);
        }

        // Check the body.
        if let Some(body) = &func.body {
            self.check_block(body);
        }

        // Restore the outer state.
        self.tracker = outer_tracker;
        self.var_types = outer_var_types;
        self.decl_mutability = outer_decl_mut;
        self.scope_depth = outer_scope_depth;
        self.var_scopes = outer_var_scopes;
        self.borrow_origins = outer_borrow_origins;
        self.fn_params = outer_fn_params;
    }

    /// Extract a simple type name string from an AST Type node.
    fn type_to_name(&self, ty: &Type) -> Option<String> {
        match ty {
            Type::Named(path) => Some(path.name.name.clone()),
            Type::Reference(_) => Some("&ref".into()),
            Type::MutReference(_) => Some("&mut_ref".into()),
            Type::Optional(inner) => self.type_to_name(&inner.node).map(|n| format!("?{}", n)),
            Type::Channel(_) => Some("chan".into()),
            Type::Array(_) => Some("Array".into()),
            Type::Tuple(_) => Some("Tuple".into()),
            Type::Inferred => None,
            _ => None,
        }
    }

    // ---- Blocks ------------------------------------------------------------

    fn check_block(&mut self, block: &Block) {
        self.scope_depth += 1;
        for stmt in &block.stmts {
            self.check_stmt(&stmt.node);
        }
        self.check_scope_exit();
        self.scope_depth -= 1;
    }

    /// On scope exit, check if any reference in an outer scope points to a
    /// local variable that is about to be destroyed.
    fn check_scope_exit(&mut self) {
        let dying_depth = self.scope_depth;
        let mut errors_to_add = Vec::new();

        for (ref_var, origin) in &self.borrow_origins {
            if let BorrowOrigin::Local { name: src_name, scope_depth: src_depth } = origin {
                // The ref_var's own scope depth.
                let ref_depth = self.var_scopes.get(ref_var).copied().unwrap_or(0);

                // If the source variable lives at the dying depth and the
                // reference lives in an outer (smaller) scope, the reference
                // will outlive its source.
                if *src_depth == dying_depth && ref_depth < dying_depth {
                    errors_to_add.push(format!(
                        "reference '{}' would outlive the value '{}' it borrows",
                        ref_var, src_name
                    ));
                }
            }
        }

        for msg in errors_to_add {
            self.errors.push(BorrowError::new(msg, Span { start: 0, end: 0 }));
        }

        // Clean up borrow origins for variables going out of scope.
        self.borrow_origins.retain(|name, _| {
            self.var_scopes.get(name).copied().unwrap_or(0) < dying_depth
        });
        self.var_scopes.retain(|_, depth| *depth < dying_depth);
    }

    // ---- Statements --------------------------------------------------------

    fn check_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let(let_stmt) => self.check_let(let_stmt),
            Stmt::Expr(expr) => {
                self.check_expr(&expr.node, expr.span);
            }
            Stmt::For(for_stmt) => self.check_for(for_stmt),
            Stmt::While(while_stmt) => self.check_while(while_stmt),
            Stmt::Loop(block) => self.check_loop(block),
            Stmt::Item(item) => self.check_item(&item.node),
        }
    }

    fn check_let(&mut self, let_stmt: &LetStmt) {
        // Check the initializer expression.
        self.check_expr(&let_stmt.value.node, let_stmt.value.span);

        // If the initializer is a variable, it gets moved into this binding
        // (unless it is a Copy type).
        if let Some(src_name) = Self::expr_var_name(&let_stmt.value.node) {
            if !self.is_copy_expr(&let_stmt.value.node) {
                self.tracker.mark_moved(src_name, let_stmt.value.span);
            }
        }

        let mutable = let_stmt.mutability == Mutability::Mutable;
        self.tracker.mark_owned(&let_stmt.name.name, mutable);
        self.decl_mutability
            .insert(let_stmt.name.name.clone(), mutable);

        // Record the type for Copy checks.
        if let Some(ty) = &let_stmt.ty {
            if let Some(ty_name) = self.type_to_name(&ty.node) {
                self.var_types.insert(let_stmt.name.name.clone(), ty_name);
            }
        } else {
            // Infer Copy-ness from the initializer.
            if let Some(ty_name) = self.expr_type_name(&let_stmt.value.node) {
                self.var_types.insert(let_stmt.name.name.clone(), ty_name);
            }
        }

        // Track scope depth for this variable.
        self.var_scopes.insert(let_stmt.name.name.clone(), self.scope_depth);

        // Track borrow origins: if RHS is &expr, record what it points to.
        if let Some(origin) = self.extract_borrow_origin(&let_stmt.value.node) {
            self.borrow_origins.insert(let_stmt.name.name.clone(), origin);
        }
    }

    fn check_for(&mut self, for_stmt: &ForStmt) {
        self.check_expr(&for_stmt.iterable.node, for_stmt.iterable.span);

        // The for-loop consumes the iterable — mark it as moved.
        if let Some(iterable_name) = Self::expr_var_name(&for_stmt.iterable.node) {
            if !self.is_copy_expr(&for_stmt.iterable.node) {
                self.tracker.mark_moved(iterable_name, for_stmt.iterable.span);
            }
        }

        // The loop body may execute multiple times, so moving a variable
        // inside it is an error (it would be moved on the second iteration).
        let snapshot = self.tracker.snapshot();

        // Declare the loop binding.
        self.tracker.mark_owned(&for_stmt.binding.name, false);
        self.check_block(&for_stmt.body);

        // Check if anything that existed *before* the loop was moved inside.
        self.check_loop_moves(&snapshot, &for_stmt.body);

        self.tracker.restore(snapshot);
    }

    fn check_while(&mut self, while_stmt: &WhileStmt) {
        // Take snapshot BEFORE evaluating the condition, since the condition
        // is re-evaluated on every iteration — moves in the condition are loop-moves.
        let snapshot = self.tracker.snapshot();
        self.check_expr(&while_stmt.condition.node, while_stmt.condition.span);
        self.check_block(&while_stmt.body);
        self.check_loop_moves(&snapshot, &while_stmt.body);
        self.tracker.restore(snapshot);
    }

    fn check_loop(&mut self, block: &Block) {
        let snapshot = self.tracker.snapshot();
        self.check_block(block);
        self.check_loop_moves(&snapshot, block);
        self.tracker.restore(snapshot);
    }

    /// After running the loop body once, check if any pre-existing variable
    /// was moved. If so, the second iteration would use-after-move.
    /// However, if the variable was reassigned within the loop body before
    /// the move, that's OK — it gets a fresh value each iteration.
    fn check_loop_moves(&mut self, before: &HashMap<String, VarState>, block: &Block) {
        // Collect variables that are reassigned in the loop body.
        let reassigned = self.collect_reassigned_vars(block);

        for name in self.tracker.tracked_names() {
            let was_owned = matches!(
                before.get(&name),
                Some(VarState::Owned) | Some(VarState::Borrowed { .. })
            );
            if was_owned {
                if let Some(VarState::Moved { moved_at }) = self.tracker.get_state(&name) {
                    // If the variable is reassigned in the loop, skip the error —
                    // each iteration provides a fresh value.
                    if reassigned.contains(&name) {
                        continue;
                    }
                    let moved_at = *moved_at;
                    self.error(
                        format!(
                            "value '{}' is moved inside a loop; it would be unavailable on the next iteration",
                            name
                        ),
                        moved_at,
                    );
                }
            }
        }
    }

    /// Collect variable names that are reassigned (via `=`) in a block.
    fn collect_reassigned_vars(&self, block: &Block) -> Vec<String> {
        let mut names = Vec::new();
        for stmt in &block.stmts {
            if let Stmt::Expr(expr) = &stmt.node {
                if let Expr::Assign(assign) = &expr.node {
                    if let Expr::Identifier(ident) = &assign.target.node {
                        names.push(ident.name.clone());
                    }
                }
            }
        }
        names
    }

    // ---- Expressions -------------------------------------------------------

    fn check_expr(&mut self, expr: &Expr, span: Span) {
        match expr {
            Expr::Identifier(ident) => {
                self.check_var_use(ident);
            }

            Expr::Binary(bin) => {
                self.check_expr(&bin.left.node, bin.left.span);
                self.check_expr(&bin.right.node, bin.right.span);
            }

            Expr::Unary(unary) => {
                self.check_expr(&unary.operand.node, unary.operand.span);
            }

            Expr::Call(call) => {
                self.check_call(call, span);
            }

            Expr::MethodCall(mc) => {
                self.check_method_call(mc, span);
            }

            Expr::FieldAccess(fa) => {
                self.check_expr(&fa.object.node, fa.object.span);
            }

            Expr::Index(idx) => {
                self.check_expr(&idx.object.node, idx.object.span);
                self.check_expr(&idx.index.node, idx.index.span);
            }

            Expr::StructLiteral(sl) => {
                for (_, value) in &sl.fields {
                    self.check_expr(&value.node, value.span);
                    // Each field value expression that is a variable gets moved in.
                    if let Some(src) = Self::expr_var_name(&value.node) {
                        if !self.is_copy_expr(&value.node) {
                            self.tracker.mark_moved(src, value.span);
                        }
                    }
                }
            }

            Expr::ArrayLiteral(elems) => {
                for elem in elems {
                    self.check_expr(&elem.node, elem.span);
                }
            }

            Expr::TupleLiteral(elems) => {
                for elem in elems {
                    self.check_expr(&elem.node, elem.span);
                }
            }

            Expr::Block(block) => {
                self.check_block(block);
            }

            Expr::If(if_expr) => {
                self.check_if(if_expr);
            }

            Expr::Match(match_expr) => {
                self.check_match(match_expr);
            }

            Expr::Closure(closure) => {
                self.check_closure(closure);
            }

            Expr::Assign(assign) => {
                self.check_assign(assign, span);
            }

            Expr::Range(range) => {
                self.check_expr(&range.start.node, range.start.span);
                self.check_expr(&range.end.node, range.end.span);
            }

            Expr::Try(inner) => {
                self.check_expr(&inner.node, inner.span);
            }

            Expr::Spawn(block) => {
                self.check_spawn(block, span);
            }

            Expr::ChanCreate(_) => {
                // Channel creation is fine — no ownership issues.
            }

            Expr::Select(sel) => {
                for arm in &sel.arms {
                    self.check_block(&arm.body);
                }
            }

            Expr::Return(val) => {
                if let Some(v) = val {
                    self.check_expr(&v.node, v.span);
                    self.check_return_ref(&v.node, v.span);
                }
            }

            Expr::Break(val) => {
                if let Some(v) = val {
                    self.check_expr(&v.node, v.span);
                }
            }

            Expr::Path(segments) => {
                // Check the first segment as a variable use.
                if let Some(first) = segments.first() {
                    self.check_var_use(first);
                }
            }

            // Literals — no ownership effects.
            Expr::IntLiteral(_)
            | Expr::FloatLiteral(_)
            | Expr::StringLiteral(_)
            | Expr::StringInterpolation(_)
            | Expr::CharLiteral(_)
            | Expr::BoolLiteral(_)
            | Expr::NilLiteral
            | Expr::Continue => {}
        }
    }

    /// Check that a variable is usable (not moved).
    fn check_var_use(&mut self, ident: &Ident) {
        if let Some(VarState::Moved { moved_at }) = self.tracker.get_state(&ident.name) {
            let moved_at = *moved_at;
            self.error(
                format!(
                    "use of moved value '{}' (moved at {}..{})",
                    ident.name, moved_at.start, moved_at.end
                ),
                ident.span,
            );
        }
    }

    /// Check a function call, handling ownership transfer for arguments.
    fn check_call(&mut self, call: &CallExpr, call_span: Span) {
        // Check the callee itself.
        self.check_expr(&call.callee.node, call.callee.span);

        // Determine the function name (for looking up param ownerships).
        let fn_name = match &call.callee.node {
            Expr::Identifier(ident) => Some(ident.name.clone()),
            Expr::Path(segments) => segments.last().map(|s| s.name.clone()),
            _ => None,
        };

        let param_ownerships = fn_name
            .as_deref()
            .and_then(|name| self.fn_param_ownerships.get(name))
            .cloned();

        for (i, arg) in call.args.iter().enumerate() {
            self.check_expr(&arg.node, arg.span);

            // Determine the ownership mode for this argument.
            // Default to Own for unknown functions (conservative — assumes move).
            let ownership = param_ownerships
                .as_ref()
                .and_then(|ownerships| ownerships.get(i))
                .copied()
                .unwrap_or(ParamOwnership::Own);

            if let Some(var_name) = Self::expr_var_name(&arg.node) {
                match ownership {
                    ParamOwnership::Own | ParamOwnership::SelfOwn => {
                        // Ownership transfer (move), unless Copy.
                        if !self.is_copy_expr(&arg.node) {
                            // Check for active borrows before moving.
                            if let Some(state) = self.tracker.check_mut_borrowable(var_name) {
                                match state {
                                    VarState::Borrowed { count } => {
                                        let count = *count;
                                        self.error(
                                            format!(
                                                "cannot move '{}': it is currently borrowed ({} active borrows)",
                                                var_name, count
                                            ),
                                            arg.span,
                                        );
                                    }
                                    VarState::MutBorrowed { span: prev_span } => {
                                        let prev_span = *prev_span;
                                        self.error(
                                            format!(
                                                "cannot move '{}': it is currently mutably borrowed at {}..{}",
                                                var_name, prev_span.start, prev_span.end
                                            ),
                                            arg.span,
                                        );
                                    }
                                    VarState::Moved { moved_at } => {
                                        let moved_at = *moved_at;
                                        self.error(
                                            format!(
                                                "cannot move '{}': value was already moved at {}..{}",
                                                var_name, moved_at.start, moved_at.end
                                            ),
                                            arg.span,
                                        );
                                    }
                                    _ => {}
                                }
                            }
                            self.tracker.mark_moved(var_name, arg.span);
                        }
                    }
                    ParamOwnership::MutBorrow | ParamOwnership::SelfMutBorrow => {
                        // Mutable borrow — check for conflicts.
                        if let Some(state) = self.tracker.check_mut_borrowable(var_name) {
                            match state {
                                VarState::Moved { moved_at } => {
                                    let moved_at = *moved_at;
                                    self.error(
                                        format!(
                                            "cannot mutably borrow '{}': value was moved at {}..{}",
                                            var_name, moved_at.start, moved_at.end
                                        ),
                                        arg.span,
                                    );
                                }
                                VarState::Borrowed { count } => {
                                    let count = *count;
                                    self.error(
                                        format!(
                                            "cannot mutably borrow '{}': it is already shared-borrowed ({} active borrows)",
                                            var_name, count
                                        ),
                                        arg.span,
                                    );
                                }
                                VarState::MutBorrowed { span: prev_span } => {
                                    let prev_span = *prev_span;
                                    self.error(
                                        format!(
                                            "cannot mutably borrow '{}': it is already mutably borrowed at {}..{}",
                                            var_name, prev_span.start, prev_span.end
                                        ),
                                        arg.span,
                                    );
                                }
                                _ => {}
                            }
                        }
                        self.tracker.mark_mut_borrowed(var_name, call_span);
                    }
                    ParamOwnership::Borrow | ParamOwnership::SelfBorrow => {
                        // Shared borrow — check for conflicts with mut borrows.
                        if let Some(state) = self.tracker.check_borrowable(var_name) {
                            match state {
                                VarState::Moved { moved_at } => {
                                    let moved_at = *moved_at;
                                    self.error(
                                        format!(
                                            "cannot borrow '{}': value was moved at {}..{}",
                                            var_name, moved_at.start, moved_at.end
                                        ),
                                        arg.span,
                                    );
                                }
                                VarState::MutBorrowed { span: prev_span } => {
                                    let prev_span = *prev_span;
                                    self.error(
                                        format!(
                                            "cannot shared-borrow '{}': it is already mutably borrowed at {}..{}",
                                            var_name, prev_span.start, prev_span.end
                                        ),
                                        arg.span,
                                    );
                                }
                                _ => {}
                            }
                        }
                        self.tracker.mark_borrowed(var_name);
                    }
                }
            }
        }
    }

    /// Check a method call.
    fn check_method_call(&mut self, mc: &MethodCallExpr, _span: Span) {
        self.check_expr(&mc.receiver.node, mc.receiver.span);
        for arg in &mc.args {
            self.check_expr(&arg.node, arg.span);
        }

        // Check if the method's self parameter takes ownership.
        if let Some(receiver_name) = Self::expr_var_name(&mc.receiver.node) {
            if let Some(ownerships) = self.fn_param_ownerships.get(&mc.method.name) {
                if let Some(&self_ownership) = ownerships.first() {
                    match self_ownership {
                        ParamOwnership::SelfOwn | ParamOwnership::Own => {
                            if !self.is_copy_expr(&mc.receiver.node) {
                                self.tracker.mark_moved(receiver_name, mc.receiver.span);
                            }
                        }
                        ParamOwnership::SelfMutBorrow | ParamOwnership::MutBorrow => {
                            self.tracker.mark_mut_borrowed(receiver_name, mc.receiver.span);
                        }
                        _ => {
                            self.tracker.mark_borrowed(receiver_name);
                        }
                    }
                }
            }
        }

        // If the method name is "send", treat the first argument as moved.
        if mc.method.name == "send" {
            for arg in &mc.args {
                if let Some(var_name) = Self::expr_var_name(&arg.node) {
                    if !self.is_copy_expr(&arg.node) {
                        self.tracker.mark_moved(var_name, arg.span);
                    }
                }
            }
        }
    }

    /// Check an if expression, merging branch states.
    fn check_if(&mut self, if_expr: &IfExpr) {
        self.check_expr(&if_expr.condition.node, if_expr.condition.span);

        let before = self.tracker.snapshot();

        // Then branch.
        self.check_block(&if_expr.then_block);
        let then_state = self.tracker.snapshot();

        // Else branch (if present).
        self.tracker.restore(before.clone());
        if let Some(else_block) = &if_expr.else_block {
            match else_block {
                ElseBlock::Else(block) => self.check_block(block),
                ElseBlock::ElseIf(elif) => self.check_if(&elif.node),
            }
        }
        let else_state = self.tracker.snapshot();

        // Merge: conservative — if moved in either branch, considered moved.
        self.tracker
            .merge_branches(&before, &then_state, &else_state);
    }

    /// Check a match expression: each arm is independent but we merge afterward.
    fn check_match(&mut self, match_expr: &MatchExpr) {
        self.check_expr(&match_expr.scrutinee.node, match_expr.scrutinee.span);

        let before = self.tracker.snapshot();
        let mut arm_states: Vec<HashMap<String, VarState>> = Vec::new();

        for arm in &match_expr.arms {
            self.tracker.restore(before.clone());
            self.check_expr(&arm.body.node, arm.body.span);
            arm_states.push(self.tracker.snapshot());
        }

        // Merge all arms: if moved in *any* arm, considered moved.
        self.tracker.restore(before.clone());
        if let Some(first) = arm_states.first() {
            let mut merged = first.clone();
            for arm_state in arm_states.iter().skip(1) {
                // Conservative merge: moved in any arm => moved.
                for (name, state) in arm_state {
                    if let VarState::Moved { moved_at } = state {
                        merged.insert(name.clone(), VarState::Moved { moved_at: *moved_at });
                    }
                }
            }
            self.tracker.restore(merged);
        }
    }

    /// Check a closure — captures are validated.
    fn check_closure(&mut self, closure: &ClosureExpr) {
        // Collect free variables used inside the closure body.
        let captured_vars = self.collect_expr_free_vars(&closure.body.node, closure.body.span, closure);

        // Non-Copy captured variables are moved into the closure.
        for (name, var_span) in &captured_vars {
            if self.tracker.get_state(name).is_some() && !self.is_var_copy(name) {
                // Check it hasn't already been moved.
                if let Some(VarState::Moved { moved_at }) = self.tracker.get_state(name) {
                    let moved_at = *moved_at;
                    self.error(
                        format!(
                            "cannot capture '{}' in closure: value was moved at {}..{}",
                            name, moved_at.start, moved_at.end
                        ),
                        *var_span,
                    );
                }
                self.tracker.mark_moved(name, *var_span);
            }
        }

        // Create a sub-scope for the closure parameters.
        for param in &closure.params {
            self.tracker.mark_owned(&param.name.name, false);
        }
        self.check_expr(&closure.body.node, closure.body.span);
    }

    /// Collect free variables from a closure body (variables not declared as params).
    fn collect_expr_free_vars(
        &self,
        expr: &Expr,
        span: Span,
        closure: &ClosureExpr,
    ) -> Vec<(String, Span)> {
        let param_names: Vec<&str> = closure.params.iter().map(|p| p.name.name.as_str()).collect();
        let mut vars = Vec::new();
        self.collect_expr_vars(expr, span, &mut vars);
        // Filter out closure parameters — those aren't captures.
        vars.retain(|(name, _)| !param_names.contains(&name.as_str()));
        vars
    }

    /// Check an assignment expression.
    fn check_assign(&mut self, assign: &AssignExpr, span: Span) {
        // Check the value expression first.
        self.check_expr(&assign.value.node, assign.value.span);

        // Check that the target is mutable.
        match &assign.target.node {
            Expr::Identifier(ident) => {
                let is_mut = self.decl_mutability.get(&ident.name).copied().unwrap_or(false);
                if !is_mut {
                    self.error(
                        format!("cannot assign to '{}': variable is not declared as mutable", ident.name),
                        span,
                    );
                }

                // If the source is a variable, it gets moved (unless Copy).
                if let Some(src) = Self::expr_var_name(&assign.value.node) {
                    if !self.is_copy_expr(&assign.value.node) {
                        self.tracker.mark_moved(src, assign.value.span);
                    }
                }

                // The target is now re-owned (assignment restores owned state).
                self.tracker.mark_owned(&ident.name, is_mut);

                // Track borrow origins for assignments: x = &y.
                if let Some(origin) = self.extract_borrow_origin(&assign.value.node) {
                    self.borrow_origins.insert(ident.name.clone(), origin);
                }
            }
            Expr::FieldAccess(fa) => {
                // Check the root object is mutable.
                if let Some(root_name) = Self::extract_root_var(&assign.target.node) {
                    let is_mut = self.decl_mutability.get(root_name).copied().unwrap_or(false);
                    if !is_mut {
                        self.error(
                            format!(
                                "cannot assign to field '{}' of '{}': variable is not declared as mutable",
                                fa.field.name, root_name
                            ),
                            span,
                        );
                    }
                }
            }
            Expr::Index(_) => {
                // Check the root object is mutable.
                if let Some(root_name) = Self::extract_root_var(&assign.target.node) {
                    let is_mut = self.decl_mutability.get(root_name).copied().unwrap_or(false);
                    if !is_mut {
                        self.error(
                            format!(
                                "cannot assign to index of '{}': variable is not declared as mutable",
                                root_name
                            ),
                            span,
                        );
                    }
                }
            }
            _ => {}
        }

        // Check the target expression for field access etc.
        self.check_expr(&assign.target.node, assign.target.span);
    }

    /// Extract the root variable name from a potentially nested field/index expression.
    fn extract_root_var(expr: &Expr) -> Option<&str> {
        match expr {
            Expr::Identifier(ident) => Some(&ident.name),
            Expr::FieldAccess(fa) => Self::extract_root_var(&fa.object.node),
            Expr::Index(idx) => Self::extract_root_var(&idx.object.node),
            _ => None,
        }
    }

    /// Check a spawn block — captured variables must be Send.
    fn check_spawn(&mut self, block: &Block, span: Span) {
        // Variables used inside the spawn block are implicitly moved into it.
        // We collect all identifiers used inside the block and mark them moved.
        let used_vars = self.collect_block_vars(block);
        for (name, var_span) in &used_vars {
            // Check if the variable exists in the current scope.
            if self.tracker.get_state(name).is_some() {
                // Enforce Send: captured variables must be safe to transfer across threads.
                if !self.is_var_send(name) {
                    self.error(
                        format!(
                            "cannot capture '{}' in spawn: type '{}' does not implement Send",
                            name,
                            self.var_types.get(name).cloned().unwrap_or_else(|| "unknown".into())
                        ),
                        *var_span,
                    );
                }

                if !self.is_var_copy(name) {
                    // Check it is usable first.
                    if let Some(VarState::Moved { moved_at }) = self.tracker.get_state(name) {
                        let moved_at = *moved_at;
                        self.error(
                            format!(
                                "cannot capture '{}' in spawn: value was moved at {}..{}",
                                name, moved_at.start, moved_at.end
                            ),
                            *var_span,
                        );
                    }
                    self.tracker.mark_moved(name, span);
                }
            }
        }

        self.check_block(block);
    }

    /// Check that a return expression does not return a reference to a local.
    fn check_return_ref(&mut self, expr: &Expr, span: Span) {
        if let Some(local_name) = self.references_local(expr) {
            self.error(
                format!("cannot return reference to local variable '{}'", local_name),
                span,
            );
        }
    }

    /// Recursively check if an expression is a reference to a local variable.
    /// Returns the name of the referenced local if found.
    fn references_local(&self, expr: &Expr) -> Option<String> {
        match expr {
            // Direct reference: &x
            Expr::Unary(u) if u.op == UnaryOp::Ref => {
                self.expr_references_local_target(&u.operand.node)
            }
            // If expression: check both branches
            Expr::If(if_expr) => {
                // Check then branch (last expr in block)
                if let Some(last) = if_expr.then_block.stmts.last() {
                    if let Stmt::Expr(e) = &last.node {
                        if let Some(name) = self.references_local(&e.node) {
                            return Some(name);
                        }
                    }
                }
                // Check else branch
                match &if_expr.else_block {
                    Some(ElseBlock::Else(block)) => {
                        if let Some(last) = block.stmts.last() {
                            if let Stmt::Expr(e) = &last.node {
                                return self.references_local(&e.node);
                            }
                        }
                    }
                    Some(ElseBlock::ElseIf(elif)) => {
                        return self.references_local(&Expr::If(Box::new(elif.node.clone())));
                    }
                    None => {}
                }
                None
            }
            // Block expression: check last expression
            Expr::Block(block) => {
                if let Some(last) = block.stmts.last() {
                    if let Stmt::Expr(e) = &last.node {
                        return self.references_local(&e.node);
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Check if a ref target expression (the operand of &) refers to a local.
    fn expr_references_local_target(&self, expr: &Expr) -> Option<String> {
        match expr {
            Expr::Identifier(ident) => {
                if self.tracker.get_state(&ident.name).is_some() {
                    Some(ident.name.clone())
                } else {
                    None
                }
            }
            // &x.field — field of a local
            Expr::FieldAccess(fa) => {
                self.expr_references_local_target(&fa.object.node)
            }
            // &arr[0] — index into a local
            Expr::Index(idx) => {
                self.expr_references_local_target(&idx.object.node)
            }
            _ => None,
        }
    }

    /// Check if a variable is a Copy type.
    fn is_var_copy(&self, name: &str) -> bool {
        self.var_types
            .get(name)
            .map(|ty| OwnershipTracker::is_copy_type(ty))
            .unwrap_or(false)
    }

    /// Extract a borrow origin from an expression, if it's a reference.
    fn extract_borrow_origin(&self, expr: &Expr) -> Option<BorrowOrigin> {
        if let Expr::Unary(u) = expr {
            if u.op == UnaryOp::Ref {
                // Get the target variable name.
                if let Some(name) = self.extract_ref_target_name(&u.operand.node) {
                    if self.fn_params.contains(&name) {
                        return Some(BorrowOrigin::Param { name });
                    }
                    let depth = self.var_scopes.get(&name).copied().unwrap_or(0);
                    return Some(BorrowOrigin::Local { name, scope_depth: depth });
                }
            }
        }
        None
    }

    /// Extract the root variable name from a ref target (handles field access and indexing).
    fn extract_ref_target_name(&self, expr: &Expr) -> Option<String> {
        match expr {
            Expr::Identifier(ident) => Some(ident.name.clone()),
            Expr::FieldAccess(fa) => self.extract_ref_target_name(&fa.object.node),
            Expr::Index(idx) => self.extract_ref_target_name(&idx.object.node),
            _ => None,
        }
    }

    /// Check if a variable's type implements Send (safe to transfer across threads).
    fn is_var_send(&self, name: &str) -> bool {
        self.var_types
            .get(name)
            .map(|ty| Self::is_send_type(ty))
            .unwrap_or(true) // unknown types assumed Send
    }

    /// Check if a type name is Send. Primitives, String, and channels are Send.
    /// Rc and raw pointers are not Send.
    fn is_send_type(type_name: &str) -> bool {
        // Non-Send types
        if type_name == "Rc" || type_name.starts_with("Rc[") {
            return false;
        }
        if type_name == "UnsafeCell" || type_name.starts_with("UnsafeCell[") {
            return false;
        }
        // All primitives, String, channels, and user-defined types are Send by default
        true
    }

    /// Collect all variable identifiers used in a block (shallow — not inside nested fns).
    fn collect_block_vars(&self, block: &Block) -> Vec<(String, Span)> {
        let mut vars = Vec::new();
        for stmt in &block.stmts {
            self.collect_stmt_vars(&stmt.node, &mut vars);
        }
        vars
    }

    fn collect_stmt_vars(&self, stmt: &Stmt, out: &mut Vec<(String, Span)>) {
        match stmt {
            Stmt::Let(ls) => {
                self.collect_expr_vars(&ls.value.node, ls.value.span, out);
            }
            Stmt::Expr(expr) => {
                self.collect_expr_vars(&expr.node, expr.span, out);
            }
            Stmt::For(fs) => {
                self.collect_expr_vars(&fs.iterable.node, fs.iterable.span, out);
                for s in &fs.body.stmts {
                    self.collect_stmt_vars(&s.node, out);
                }
            }
            Stmt::While(ws) => {
                self.collect_expr_vars(&ws.condition.node, ws.condition.span, out);
                for s in &ws.body.stmts {
                    self.collect_stmt_vars(&s.node, out);
                }
            }
            Stmt::Loop(block) => {
                for s in &block.stmts {
                    self.collect_stmt_vars(&s.node, out);
                }
            }
            Stmt::Item(_) => {}
        }
    }

    fn collect_expr_vars(&self, expr: &Expr, span: Span, out: &mut Vec<(String, Span)>) {
        match expr {
            Expr::Identifier(ident) => {
                out.push((ident.name.clone(), ident.span));
            }
            Expr::Binary(bin) => {
                self.collect_expr_vars(&bin.left.node, bin.left.span, out);
                self.collect_expr_vars(&bin.right.node, bin.right.span, out);
            }
            Expr::Unary(u) => {
                self.collect_expr_vars(&u.operand.node, u.operand.span, out);
            }
            Expr::Call(call) => {
                self.collect_expr_vars(&call.callee.node, call.callee.span, out);
                for arg in &call.args {
                    self.collect_expr_vars(&arg.node, arg.span, out);
                }
            }
            Expr::MethodCall(mc) => {
                self.collect_expr_vars(&mc.receiver.node, mc.receiver.span, out);
                for arg in &mc.args {
                    self.collect_expr_vars(&arg.node, arg.span, out);
                }
            }
            Expr::FieldAccess(fa) => {
                self.collect_expr_vars(&fa.object.node, fa.object.span, out);
            }
            Expr::Index(idx) => {
                self.collect_expr_vars(&idx.object.node, idx.object.span, out);
                self.collect_expr_vars(&idx.index.node, idx.index.span, out);
            }
            Expr::Assign(assign) => {
                self.collect_expr_vars(&assign.target.node, assign.target.span, out);
                self.collect_expr_vars(&assign.value.node, assign.value.span, out);
            }
            Expr::Block(block) => {
                for s in &block.stmts {
                    self.collect_stmt_vars(&s.node, out);
                }
            }
            Expr::If(if_expr) => {
                self.collect_expr_vars(&if_expr.condition.node, if_expr.condition.span, out);
                for s in &if_expr.then_block.stmts {
                    self.collect_stmt_vars(&s.node, out);
                }
                if let Some(else_block) = &if_expr.else_block {
                    match else_block {
                        ElseBlock::Else(block) => {
                            for s in &block.stmts {
                                self.collect_stmt_vars(&s.node, out);
                            }
                        }
                        ElseBlock::ElseIf(elif) => {
                            self.collect_expr_vars(
                                &Expr::If(Box::new(elif.node.clone())),
                                span,
                                out,
                            );
                        }
                    }
                }
            }
            _ => {}
        }
    }
}

impl Default for BorrowChecker {
    fn default() -> Self {
        Self::new()
    }
}
