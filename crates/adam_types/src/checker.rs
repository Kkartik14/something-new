//! Type checker — inference, checking rules, trait resolution.
//!
//! Walks the AST, assigns types to every expression, checks
//! correctness, and resolves generics.

use std::collections::HashMap;

use adam_ast::common::*;
use adam_ast::expr::*;
use adam_ast::item::*;
use adam_ast::pattern::Pattern;
use adam_ast::stmt::*;
use adam_ast::types::Type;

use crate::ty::*;

/// A type error.
#[derive(Debug, Clone)]
pub struct TypeError {
    pub message: String,
    pub span: Span,
}

impl TypeError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}..{}] {}",
            self.span.start, self.span.end, self.message
        )
    }
}

/// Result of type checking.
#[derive(Debug)]
pub struct TypeCheckResult {
    pub ctx: TypeContext,
    pub expr_types: HashMap<u32, TypeId>,
    pub errors: Vec<TypeError>,
}

/// The type checker.
pub struct TypeChecker {
    pub ctx: TypeContext,
    /// Type of each expression (keyed by span start for simplicity).
    expr_types: HashMap<u32, TypeId>,
    /// Variable name -> TypeId in current scope.
    env: Vec<HashMap<String, TypeId>>,
    /// Function name -> FnSig.
    fn_sigs: HashMap<String, FnSig>,
    /// Struct name -> (StructId, TypeId).
    struct_ids: HashMap<String, (StructId, TypeId)>,
    /// Enum name -> (EnumId, TypeId).
    enum_ids: HashMap<String, (EnumId, TypeId)>,
    /// Enum variant name -> (EnumId, variant index, TypeId of the enum).
    variant_ids: HashMap<String, (EnumId, usize, TypeId)>,
    /// Trait name -> TraitId.
    trait_ids: HashMap<String, TraitId>,
    /// Method name -> (receiver TypeId, FnSig) for impl methods.
    method_sigs: HashMap<(String, String), FnSig>,
    /// Substitutions for type variables.
    substitutions: HashMap<TypeVarId, TypeId>,
    /// Current function return type (for checking return statements).
    current_return_type: Option<TypeId>,
    /// Errors collected.
    errors: Vec<TypeError>,
    /// Generic function name -> TypeVarIds of its generic params.
    fn_generic_params: HashMap<String, Vec<TypeVarId>>,
    /// Generic function name -> Vec of trait bound names per generic param.
    fn_generic_bounds: HashMap<String, Vec<Vec<String>>>,
}

impl TypeChecker {
    pub fn new() -> Self {
        let ctx = TypeContext::new();

        // Seed built-in function signatures.
        let mut fn_sigs = HashMap::new();
        let print_sig = FnSig {
            params: vec![ctx.string()],
            return_type: ctx.unit(),
        };
        fn_sigs.insert("print".to_string(), print_sig.clone());
        fn_sigs.insert("println".to_string(), print_sig);

        Self {
            ctx,
            expr_types: HashMap::new(),
            env: vec![HashMap::new()],
            fn_sigs,
            struct_ids: HashMap::new(),
            enum_ids: HashMap::new(),
            variant_ids: HashMap::new(),
            trait_ids: HashMap::new(),
            method_sigs: HashMap::new(),
            substitutions: HashMap::new(),
            current_return_type: None,
            errors: vec![],
            fn_generic_params: HashMap::new(),
            fn_generic_bounds: HashMap::new(),
        }
    }

    pub fn check(mut self, ast: &SourceFile) -> TypeCheckResult {
        self.check_source_file(ast);
        TypeCheckResult {
            ctx: self.ctx,
            expr_types: self.expr_types,
            errors: self.errors,
        }
    }

    // ---- Environment (scope) management ----

    fn push_scope(&mut self) {
        self.env.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.env.pop();
    }

    fn define(&mut self, name: &str, ty: TypeId) {
        if let Some(scope) = self.env.last_mut() {
            scope.insert(name.to_string(), ty);
        }
    }

    fn lookup_var(&self, name: &str) -> Option<TypeId> {
        for scope in self.env.iter().rev() {
            if let Some(&ty) = scope.get(name) {
                return Some(ty);
            }
        }
        None
    }

    fn record_expr_type(&mut self, span_start: u32, ty: TypeId) {
        self.expr_types.insert(span_start, ty);
    }

    fn err(&mut self, message: impl Into<String>, span: Span) {
        self.errors.push(TypeError::new(message, span));
    }

    /// Suggest a similar name from scope for typo diagnostics.
    fn suggest_similar_name(&self, name: &str) -> Option<String> {
        let mut best: Option<(usize, String)> = None;
        for scope in self.env.iter().rev() {
            for var_name in scope.keys() {
                let dist = Self::edit_distance(name, var_name);
                if dist <= 2 && dist < name.len() {
                    if best.as_ref().map_or(true, |(d, _)| dist < *d) {
                        best = Some((dist, var_name.clone()));
                    }
                }
            }
        }
        // Also check function names.
        for fn_name in self.fn_sigs.keys() {
            let dist = Self::edit_distance(name, fn_name);
            if dist <= 2 && dist < name.len() {
                if best.as_ref().map_or(true, |(d, _)| dist < *d) {
                    best = Some((dist, fn_name.clone()));
                }
            }
        }
        best.map(|(_, n)| n)
    }

    /// Simple edit distance (Levenshtein).
    fn edit_distance(a: &str, b: &str) -> usize {
        let a: Vec<char> = a.chars().collect();
        let b: Vec<char> = b.chars().collect();
        let (m, n) = (a.len(), b.len());
        let mut dp = vec![vec![0usize; n + 1]; m + 1];
        for i in 0..=m {
            dp[i][0] = i;
        }
        for j in 0..=n {
            dp[0][j] = j;
        }
        for i in 1..=m {
            for j in 1..=n {
                let cost = if a[i - 1] == b[j - 1] { 0 } else { 1 };
                dp[i][j] = (dp[i - 1][j] + 1)
                    .min(dp[i][j - 1] + 1)
                    .min(dp[i - 1][j - 1] + cost);
            }
        }
        dp[m][n]
    }

    // ---- Resolve AST type to internal TypeId ----

    fn resolve_ast_type(&mut self, ty: &Spanned<Type>) -> TypeId {
        self.resolve_type_inner(&ty.node)
    }

    fn resolve_type_inner(&mut self, ty: &Type) -> TypeId {
        match ty {
            Type::Named(path) => {
                let name = &path.name.name;
                match name.as_str() {
                    "i8" => self.ctx.i8(),
                    "i16" => self.ctx.i16(),
                    "i32" => self.ctx.i32(),
                    "i64" => self.ctx.i64(),
                    "u8" => self.ctx.u8(),
                    "u16" => self.ctx.u16(),
                    "u32" => self.ctx.u32(),
                    "u64" => self.ctx.u64(),
                    "f32" => self.ctx.f32(),
                    "f64" => self.ctx.f64(),
                    "bool" => self.ctx.bool(),
                    "char" => self.ctx.char(),
                    "String" => self.ctx.string(),
                    "str" => self.ctx.str(),
                    "Self" => {
                        // Look up Self in env
                        self.lookup_var("Self").unwrap_or(self.ctx.error())
                    }
                    "Box" => {
                        if let Some(first) = path.generic_args.first() {
                            let inner = self.resolve_ast_type(first);
                            self.ctx.intern(Ty::Box(inner))
                        } else {
                            self.ctx.error()
                        }
                    }
                    "Rc" => {
                        if let Some(first) = path.generic_args.first() {
                            let inner = self.resolve_ast_type(first);
                            self.ctx.intern(Ty::Rc(inner))
                        } else {
                            self.ctx.error()
                        }
                    }
                    "Arc" => {
                        if let Some(first) = path.generic_args.first() {
                            let inner = self.resolve_ast_type(first);
                            self.ctx.intern(Ty::Arc(inner))
                        } else {
                            self.ctx.error()
                        }
                    }
                    "Map" | "Set" => {
                        // Collections — for now treat as opaque struct
                        self.ctx.error()
                    }
                    _ => {
                        // User-defined struct or enum
                        if let Some(&(_, ty_id)) = self.struct_ids.get(name.as_str()) {
                            ty_id
                        } else if let Some(&(_, ty_id)) = self.enum_ids.get(name.as_str()) {
                            ty_id
                        } else if let Some(ty_id) = self.lookup_var(name) {
                            // Could be a generic parameter
                            ty_id
                        } else {
                            self.ctx.error()
                        }
                    }
                }
            }
            Type::Reference(inner) => {
                let inner_id = self.resolve_ast_type(inner);
                self.ctx.intern(Ty::Ref(inner_id))
            }
            Type::MutReference(inner) => {
                let inner_id = self.resolve_ast_type(inner);
                self.ctx.intern(Ty::MutRef(inner_id))
            }
            Type::Array(arr) => {
                let elem = self.resolve_ast_type(&arr.element);
                let size = arr.size.as_ref().and_then(|s| {
                    if let Expr::IntLiteral(n) = &s.node {
                        Some(*n as u64)
                    } else {
                        None
                    }
                });
                self.ctx.intern(Ty::Array(elem, size))
            }
            Type::Tuple(types) => {
                let elems: Vec<_> = types.iter().map(|t| self.resolve_ast_type(t)).collect();
                self.ctx.intern(Ty::Tuple(elems))
            }
            Type::Optional(inner) => {
                let inner_id = self.resolve_ast_type(inner);
                self.ctx.intern(Ty::Optional(inner_id))
            }
            Type::Result(res) => {
                let ok = self.resolve_ast_type(&res.ok);
                let err = self.resolve_ast_type(&res.err);
                self.ctx.intern(Ty::Result(ok, err))
            }
            Type::Function(ft) => {
                let params: Vec<_> = ft.params.iter().map(|p| self.resolve_ast_type(p)).collect();
                let ret = self.resolve_ast_type(&ft.return_type);
                self.ctx.intern(Ty::Function(FnSig {
                    params,
                    return_type: ret,
                }))
            }
            Type::Channel(inner) => {
                let inner_id = self.resolve_ast_type(inner);
                self.ctx.intern(Ty::Channel(inner_id))
            }
            Type::Inferred => self.ctx.fresh_type_var(),
        }
    }

    // ---- Unification ----

    fn unify(&mut self, a: TypeId, b: TypeId, span: Span) -> TypeId {
        let a = self.apply_subs(a);
        let b = self.apply_subs(b);

        if a == b {
            return a;
        }

        let ta = self.ctx.ty(a).clone();
        let tb = self.ctx.ty(b).clone();

        // Error type propagates silently.
        if ta == Ty::Error || tb == Ty::Error {
            return self.ctx.error();
        }

        // Never coerces to anything.
        if ta == Ty::Never {
            return b;
        }
        if tb == Ty::Never {
            return a;
        }

        // Type variables unify.
        if let Ty::TypeVar(var) = ta {
            self.substitutions.insert(var, b);
            return b;
        }
        if let Ty::TypeVar(var) = tb {
            self.substitutions.insert(var, a);
            return a;
        }

        // Structural unification.
        match (&ta, &tb) {
            (Ty::Optional(a_inner), Ty::Optional(b_inner)) => {
                let inner = self.unify(*a_inner, *b_inner, span);
                self.ctx.intern(Ty::Optional(inner))
            }
            (Ty::Result(a_ok, a_err), Ty::Result(b_ok, b_err)) => {
                let ok = self.unify(*a_ok, *b_ok, span);
                let err = self.unify(*a_err, *b_err, span);
                self.ctx.intern(Ty::Result(ok, err))
            }
            (Ty::Array(a_elem, a_sz), Ty::Array(b_elem, b_sz)) if a_sz == b_sz => {
                let elem = self.unify(*a_elem, *b_elem, span);
                self.ctx.intern(Ty::Array(elem, *a_sz))
            }
            (Ty::Tuple(a_elems), Ty::Tuple(b_elems)) if a_elems.len() == b_elems.len() => {
                let elems: Vec<_> = a_elems
                    .iter()
                    .zip(b_elems.iter())
                    .map(|(&ae, &be)| self.unify(ae, be, span))
                    .collect();
                self.ctx.intern(Ty::Tuple(elems))
            }
            (Ty::Ref(a_inner), Ty::Ref(b_inner)) => {
                let inner = self.unify(*a_inner, *b_inner, span);
                self.ctx.intern(Ty::Ref(inner))
            }
            (Ty::MutRef(a_inner), Ty::MutRef(b_inner)) => {
                let inner = self.unify(*a_inner, *b_inner, span);
                self.ctx.intern(Ty::MutRef(inner))
            }
            (Ty::Channel(a_inner), Ty::Channel(b_inner)) => {
                let inner = self.unify(*a_inner, *b_inner, span);
                self.ctx.intern(Ty::Channel(inner))
            }
            (Ty::Function(a_sig), Ty::Function(b_sig))
                if a_sig.params.len() == b_sig.params.len() =>
            {
                let params: Vec<_> = a_sig
                    .params
                    .iter()
                    .zip(b_sig.params.iter())
                    .map(|(&ap, &bp)| self.unify(ap, bp, span))
                    .collect();
                let ret = self.unify(a_sig.return_type, b_sig.return_type, span);
                self.ctx.intern(Ty::Function(FnSig {
                    params,
                    return_type: ret,
                }))
            }
            _ => {
                self.err(
                    format!(
                        "type mismatch: expected `{}`, found `{}`",
                        self.ctx.display(a),
                        self.ctx.display(b)
                    ),
                    span,
                );
                self.ctx.error()
            }
        }
    }

    fn apply_subs(&self, id: TypeId) -> TypeId {
        self.ctx.resolve(id, &self.substitutions)
    }

    /// Substitute TypeVars in a type according to a mapping.
    fn substitute_type(&mut self, ty_id: TypeId, subs: &HashMap<TypeVarId, TypeId>) -> TypeId {
        if subs.is_empty() {
            return ty_id;
        }
        let ty = self.ctx.ty(ty_id).clone();
        match ty {
            Ty::TypeVar(var) => {
                if let Some(&replacement) = subs.get(&var) {
                    replacement
                } else {
                    ty_id
                }
            }
            Ty::Array(elem, size) => {
                let new_elem = self.substitute_type(elem, subs);
                if new_elem == elem {
                    ty_id
                } else {
                    self.ctx.intern(Ty::Array(new_elem, size))
                }
            }
            Ty::Tuple(elems) => {
                let new_elems: Vec<_> = elems
                    .iter()
                    .map(|&e| self.substitute_type(e, subs))
                    .collect();
                if new_elems == elems {
                    ty_id
                } else {
                    self.ctx.intern(Ty::Tuple(new_elems))
                }
            }
            Ty::Optional(inner) => {
                let new_inner = self.substitute_type(inner, subs);
                if new_inner == inner {
                    ty_id
                } else {
                    self.ctx.intern(Ty::Optional(new_inner))
                }
            }
            Ty::Result(ok, err) => {
                let new_ok = self.substitute_type(ok, subs);
                let new_err = self.substitute_type(err, subs);
                if new_ok == ok && new_err == err {
                    ty_id
                } else {
                    self.ctx.intern(Ty::Result(new_ok, new_err))
                }
            }
            Ty::Ref(inner) => {
                let new_inner = self.substitute_type(inner, subs);
                if new_inner == inner {
                    ty_id
                } else {
                    self.ctx.intern(Ty::Ref(new_inner))
                }
            }
            Ty::MutRef(inner) => {
                let new_inner = self.substitute_type(inner, subs);
                if new_inner == inner {
                    ty_id
                } else {
                    self.ctx.intern(Ty::MutRef(new_inner))
                }
            }
            Ty::Box(inner) => {
                let new_inner = self.substitute_type(inner, subs);
                if new_inner == inner {
                    ty_id
                } else {
                    self.ctx.intern(Ty::Box(new_inner))
                }
            }
            Ty::Rc(inner) => {
                let new_inner = self.substitute_type(inner, subs);
                if new_inner == inner {
                    ty_id
                } else {
                    self.ctx.intern(Ty::Rc(new_inner))
                }
            }
            Ty::Arc(inner) => {
                let new_inner = self.substitute_type(inner, subs);
                if new_inner == inner {
                    ty_id
                } else {
                    self.ctx.intern(Ty::Arc(new_inner))
                }
            }
            Ty::Channel(inner) => {
                let new_inner = self.substitute_type(inner, subs);
                if new_inner == inner {
                    ty_id
                } else {
                    self.ctx.intern(Ty::Channel(new_inner))
                }
            }
            Ty::Function(sig) => {
                let new_params: Vec<_> = sig
                    .params
                    .iter()
                    .map(|&p| self.substitute_type(p, subs))
                    .collect();
                let new_ret = self.substitute_type(sig.return_type, subs);
                if new_params == sig.params && new_ret == sig.return_type {
                    ty_id
                } else {
                    self.ctx.intern(Ty::Function(FnSig {
                        params: new_params,
                        return_type: new_ret,
                    }))
                }
            }
            // Primitives, structs, enums, Generic, Error, Never — no TypeVars inside.
            _ => ty_id,
        }
    }

    /// Create a fresh instantiation of a generic function's signature.
    /// Each call site gets its own TypeVars so they can infer independently.
    /// Returns the instantiated sig and the fresh TypeIds for bound checking.
    fn instantiate_fn_sig(&mut self, fn_name: &str, sig: &FnSig) -> (FnSig, Vec<TypeId>) {
        if let Some(generic_tvars) = self.fn_generic_params.get(fn_name).cloned() {
            let mut sub_map = HashMap::new();
            let mut fresh_tvars = Vec::new();
            for old_var in &generic_tvars {
                let fresh = self.ctx.fresh_type_var();
                sub_map.insert(*old_var, fresh);
                fresh_tvars.push(fresh);
            }
            let params: Vec<_> = sig
                .params
                .iter()
                .map(|&p| self.substitute_type(p, &sub_map))
                .collect();
            let return_type = self.substitute_type(sig.return_type, &sub_map);
            (
                FnSig {
                    params,
                    return_type,
                },
                fresh_tvars,
            )
        } else {
            (sig.clone(), vec![])
        }
    }

    /// Check that concrete types inferred for generic params satisfy trait bounds.
    fn check_generic_bounds(&mut self, fn_name: &str, fresh_tvars: &[TypeId], span: Span) {
        let bounds = match self.fn_generic_bounds.get(fn_name) {
            Some(b) => b.clone(),
            None => return,
        };
        for (i, &fresh_ty) in fresh_tvars.iter().enumerate() {
            let concrete = self.apply_subs(fresh_ty);
            let concrete_ty = self.ctx.ty(concrete).clone();
            // Skip unresolved TypeVars and error types.
            if concrete_ty == Ty::Error || matches!(concrete_ty, Ty::TypeVar(_)) {
                continue;
            }
            if let Some(param_bounds) = bounds.get(i) {
                let type_name = self.ctx.display(concrete);
                for trait_name in param_bounds {
                    if !self.type_implements_trait(concrete, trait_name) {
                        self.err(
                            format!(
                                "type `{}` does not implement trait `{}`",
                                type_name, trait_name
                            ),
                            span,
                        );
                    }
                }
            }
        }
    }

    /// Check if a concrete type has a trait impl registered.
    fn type_implements_trait(&self, type_id: TypeId, trait_name: &str) -> bool {
        if let Some(&trait_id) = self.trait_ids.get(trait_name) {
            self.ctx
                .trait_impls
                .iter()
                .any(|imp| imp.trait_id == trait_id && imp.target_type == type_id)
        } else {
            false
        }
    }

    // ---- Source file ----

    fn check_source_file(&mut self, file: &SourceFile) {
        // First pass: register all type/function declarations.
        for item in &file.items {
            self.register_item(&item.node);
        }

        // Second pass: check function bodies.
        for item in &file.items {
            self.check_item(&item.node);
        }
    }

    fn register_item(&mut self, item: &Item) {
        match item {
            Item::Function(f) => {
                let sig = self.build_fn_sig(f);
                self.fn_sigs.insert(f.name.name.clone(), sig);
            }
            Item::Struct(s) => {
                let fields: Vec<FieldInfo> = s
                    .fields
                    .iter()
                    .map(|f| FieldInfo {
                        name: f.name.name.clone(),
                        ty: self.resolve_ast_type(&f.ty),
                        is_public: f.visibility == Visibility::Public,
                    })
                    .collect();
                let struct_id = self.ctx.add_struct(StructInfo {
                    name: s.name.name.clone(),
                    fields,
                    generic_params: vec![],
                });
                let ty_id = self.ctx.intern(Ty::Struct(struct_id));
                self.struct_ids
                    .insert(s.name.name.clone(), (struct_id, ty_id));
            }
            Item::Enum(e) => {
                let variants: Vec<VariantInfo> = e
                    .variants
                    .iter()
                    .map(|v| VariantInfo {
                        name: v.name.name.clone(),
                        fields: v.fields.iter().map(|f| self.resolve_ast_type(f)).collect(),
                    })
                    .collect();
                let enum_id = self.ctx.add_enum(EnumInfo {
                    name: e.name.name.clone(),
                    variants,
                    generic_params: vec![],
                });
                let ty_id = self.ctx.intern(Ty::Enum(enum_id));
                self.enum_ids.insert(e.name.name.clone(), (enum_id, ty_id));
                // Register variants.
                for (i, variant) in e.variants.iter().enumerate() {
                    self.variant_ids
                        .insert(variant.name.name.clone(), (enum_id, i, ty_id));
                }
            }
            Item::Trait(t) => {
                let methods: Vec<TraitMethodInfo> = t
                    .methods
                    .iter()
                    .map(|m| TraitMethodInfo {
                        name: m.node.name.name.clone(),
                        sig: self.build_fn_sig(&m.node),
                        has_default: m.node.body.is_some(),
                    })
                    .collect();
                let trait_id = self.ctx.add_trait(TraitInfo {
                    name: t.name.name.clone(),
                    methods,
                });
                self.trait_ids.insert(t.name.name.clone(), trait_id);
            }
            Item::Impl(imp) => {
                let target_ty = self.resolve_type_inner(&imp.target_type.node);
                let type_name = self.ctx.display(target_ty);
                // Define Self
                self.define("Self", target_ty);

                for method in &imp.methods {
                    let sig = self.build_fn_sig(&method.node);
                    self.method_sigs.insert(
                        (type_name.clone(), method.node.name.name.clone()),
                        sig.clone(),
                    );
                    // Also register as a plain function for Type.method() calls
                    let qualified = format!("{}.{}", type_name, method.node.name.name);
                    self.fn_sigs.insert(qualified, sig);
                }

                if let Some(trait_name) = &imp.trait_name {
                    if let Some(&trait_id) = self.trait_ids.get(&trait_name.name) {
                        let methods: Vec<_> = imp
                            .methods
                            .iter()
                            .map(|m| (m.node.name.name.clone(), self.build_fn_sig(&m.node)))
                            .collect();

                        // Validate impl method signatures against trait declarations.
                        let trait_info = self.ctx.traits[trait_id as usize].clone();
                        for (impl_name, impl_sig) in &methods {
                            if let Some(trait_method) =
                                trait_info.methods.iter().find(|m| m.name == *impl_name)
                            {
                                // Check return type matches.
                                if self.apply_subs(impl_sig.return_type)
                                    != self.apply_subs(trait_method.sig.return_type)
                                    && trait_method.sig.return_type != self.ctx.error()
                                    && impl_sig.return_type != self.ctx.error()
                                {
                                    // Find the span of this method in the impl block.
                                    let method_span = imp
                                        .methods
                                        .iter()
                                        .find(|m| m.node.name.name == *impl_name)
                                        .map(|m| m.node.name.span)
                                        .unwrap_or(imp.target_type.span);
                                    self.err(
                                        format!(
                                            "impl method `{}` has return type `{}` but trait `{}` declares `{}`",
                                            impl_name,
                                            self.ctx.display(impl_sig.return_type),
                                            trait_name.name,
                                            self.ctx.display(trait_method.sig.return_type)
                                        ),
                                        method_span,
                                    );
                                }
                                // Check param count matches (excluding self).
                                let impl_non_self = if impl_sig.params.is_empty() {
                                    0
                                } else {
                                    impl_sig.params.len() - 1
                                };
                                let trait_non_self = if trait_method.sig.params.is_empty() {
                                    0
                                } else {
                                    trait_method.sig.params.len() - 1
                                };
                                if impl_non_self != trait_non_self {
                                    let method_span = imp
                                        .methods
                                        .iter()
                                        .find(|m| m.node.name.name == *impl_name)
                                        .map(|m| m.node.name.span)
                                        .unwrap_or(imp.target_type.span);
                                    self.err(
                                        format!(
                                            "impl method `{}` has {} parameter(s) but trait `{}` declares {}",
                                            impl_name, impl_non_self, trait_name.name, trait_non_self
                                        ),
                                        method_span,
                                    );
                                }
                            }
                        }

                        self.ctx.add_trait_impl(TraitImpl {
                            trait_id,
                            target_type: target_ty,
                            methods,
                        });
                    }
                }
            }
            Item::View(v) => {
                // Views are similar to structs for type-checking purposes.
                let fields: Vec<FieldInfo> = v
                    .fields
                    .iter()
                    .map(|f| FieldInfo {
                        name: f.name.name.clone(),
                        ty: self.resolve_ast_type(&f.ty),
                        is_public: true,
                    })
                    .collect();
                let struct_id = self.ctx.add_struct(StructInfo {
                    name: v.name.name.clone(),
                    fields,
                    generic_params: vec![],
                });
                let ty_id = self.ctx.intern(Ty::Struct(struct_id));
                self.struct_ids
                    .insert(v.name.name.clone(), (struct_id, ty_id));
            }
            Item::Use(_) | Item::Mod(_) => {}
        }
    }

    fn build_fn_sig(&mut self, f: &FnDef) -> FnSig {
        // If the function has generic params, create TypeVars for them.
        let mut generic_tvars = Vec::new();
        if !f.generic_params.is_empty() {
            self.push_scope();
            for gp in &f.generic_params {
                let tv = self.ctx.fresh_type_var();
                if let Ty::TypeVar(id) = self.ctx.ty(tv).clone() {
                    generic_tvars.push(id);
                }
                self.define(&gp.name.name, tv);
            }
        }

        let params: Vec<_> = f
            .params
            .iter()
            .map(|p| self.resolve_ast_type(&p.ty))
            .collect();
        let return_type = f
            .return_type
            .as_ref()
            .map(|t| self.resolve_ast_type(t))
            .unwrap_or(self.ctx.unit());

        if !f.generic_params.is_empty() {
            self.pop_scope();
            // Store bounds for each generic param.
            let bounds: Vec<Vec<String>> = f
                .generic_params
                .iter()
                .map(|gp| gp.bounds.iter().map(|b| b.name.clone()).collect())
                .collect();
            self.fn_generic_params
                .insert(f.name.name.clone(), generic_tvars);
            if bounds.iter().any(|b| !b.is_empty()) {
                self.fn_generic_bounds.insert(f.name.name.clone(), bounds);
            }
        }

        FnSig {
            params,
            return_type,
        }
    }

    // ---- Items ----

    fn check_item(&mut self, item: &Item) {
        match item {
            Item::Function(f) => self.check_fn(f),
            Item::Impl(imp) => self.check_impl(imp),
            Item::View(v) => self.check_view(v),
            Item::Struct(_) | Item::Enum(_) | Item::Trait(_) | Item::Use(_) | Item::Mod(_) => {}
        }
    }

    fn check_fn(&mut self, f: &FnDef) {
        if let Some(body) = &f.body {
            self.push_scope();

            let sig = self
                .fn_sigs
                .get(&f.name.name)
                .cloned()
                .unwrap_or_else(|| self.build_fn_sig(f));
            self.current_return_type = Some(sig.return_type);

            // Bind generic param names so `T` resolves inside the body.
            if let Some(generic_tvars) = self.fn_generic_params.get(&f.name.name).cloned() {
                for (gp, &tvar_id) in f.generic_params.iter().zip(generic_tvars.iter()) {
                    let tv_ty_id = self.ctx.intern(Ty::TypeVar(tvar_id));
                    self.define(&gp.name.name, tv_ty_id);
                }
            }

            // Bind params.
            for (i, param) in f.params.iter().enumerate() {
                let ty = if i < sig.params.len() {
                    sig.params[i]
                } else {
                    self.ctx.error()
                };
                self.define(&param.name.name, ty);
            }

            let body_ty = self.check_block(body);

            // Check return type.
            if sig.return_type != self.ctx.unit() {
                self.unify(sig.return_type, body_ty, f.name.span);
            }

            self.current_return_type = None;
            self.pop_scope();
        }
    }

    fn check_impl(&mut self, imp: &ImplBlock) {
        let target_ty = self.resolve_type_inner(&imp.target_type.node);
        self.push_scope();
        self.define("Self", target_ty);

        for method in &imp.methods {
            let type_name = self.ctx.display(target_ty);
            let key = (type_name.clone(), method.node.name.name.clone());
            let sig = self.method_sigs.get(&key).cloned();

            if let Some(body) = &method.node.body {
                self.push_scope();

                let sig = sig.unwrap_or_else(|| self.build_fn_sig(&method.node));
                self.current_return_type = Some(sig.return_type);

                // Bind self if it's a method with self param.
                for (i, param) in method.node.params.iter().enumerate() {
                    let ty = if param.name.name == "self" {
                        target_ty
                    } else if i < sig.params.len() {
                        sig.params[i]
                    } else {
                        self.ctx.error()
                    };
                    self.define(&param.name.name, ty);
                }

                let body_ty = self.check_block(body);
                if sig.return_type != self.ctx.unit() {
                    self.unify(sig.return_type, body_ty, method.node.name.span);
                }

                self.current_return_type = None;
                self.pop_scope();
            }
        }

        self.pop_scope();
    }

    fn check_view(&mut self, v: &ViewDef) {
        self.push_scope();
        for field in &v.fields {
            let ty = self.resolve_ast_type(&field.ty);
            self.define(&field.name.name, ty);
            if let Some(default) = &field.default {
                let default_ty = self.infer_expr(default);
                self.unify(ty, default_ty, default.span);
            }
        }
        self.check_block(&v.body);
        self.pop_scope();
    }

    // ---- Blocks and statements ----

    fn check_block(&mut self, block: &Block) -> TypeId {
        self.push_scope();
        let mut last_ty = self.ctx.unit();
        for (i, stmt) in block.stmts.iter().enumerate() {
            let ty = self.check_stmt(&stmt.node);
            if i == block.stmts.len() - 1 {
                last_ty = ty;
            }
        }
        self.pop_scope();
        last_ty
    }

    fn check_stmt(&mut self, stmt: &Stmt) -> TypeId {
        match stmt {
            Stmt::Let(let_stmt) => {
                let value_ty = self.infer_expr(&let_stmt.value);
                let declared_ty = if let Some(ty) = &let_stmt.ty {
                    let t = self.resolve_ast_type(ty);
                    self.unify(t, value_ty, let_stmt.value.span);
                    t
                } else {
                    value_ty
                };
                self.define(&let_stmt.name.name, declared_ty);
                self.ctx.unit()
            }
            Stmt::Expr(expr) => self.infer_expr(expr),
            Stmt::For(for_stmt) => {
                let iterable_ty = self.infer_expr(&for_stmt.iterable);
                // Infer element type from iterable.
                let resolved_iter = self.apply_subs(iterable_ty);
                let elem_ty = match self.ctx.ty(resolved_iter).clone() {
                    Ty::Array(elem, _) => elem,
                    Ty::Channel(elem) => elem,
                    Ty::Error => self.ctx.error(),
                    _ => {
                        self.err(
                            format!(
                                "cannot iterate over type `{}`; expected array or channel",
                                self.ctx.display(resolved_iter)
                            ),
                            for_stmt.iterable.span,
                        );
                        self.ctx.error()
                    }
                };
                self.push_scope();
                self.define(&for_stmt.binding.name, elem_ty);
                self.check_block_stmts(&for_stmt.body);
                self.pop_scope();
                self.ctx.unit()
            }
            Stmt::While(while_stmt) => {
                let cond_ty = self.infer_expr(&while_stmt.condition);
                self.unify(self.ctx.bool(), cond_ty, while_stmt.condition.span);
                self.push_scope();
                self.check_block_stmts(&while_stmt.body);
                self.pop_scope();
                self.ctx.unit()
            }
            Stmt::Loop(block) => {
                self.push_scope();
                self.check_block_stmts(block);
                self.pop_scope();
                self.ctx.unit()
            }
            Stmt::Item(item) => {
                self.register_item(&item.node);
                self.check_item(&item.node);
                self.ctx.unit()
            }
        }
    }

    fn check_block_stmts(&mut self, block: &Block) {
        for stmt in &block.stmts {
            self.check_stmt(&stmt.node);
        }
    }

    // ---- Expression inference ----

    pub fn infer_expr(&mut self, expr: &Spanned<Expr>) -> TypeId {
        let ty = self.infer_expr_inner(&expr.node, expr.span);
        self.record_expr_type(expr.span.start, ty);
        ty
    }

    fn infer_expr_inner(&mut self, expr: &Expr, span: Span) -> TypeId {
        match expr {
            Expr::IntLiteral(_) => self.ctx.i32(),
            Expr::FloatLiteral(_) => self.ctx.f64(),
            Expr::StringLiteral(_) => self.ctx.string(),
            Expr::StringInterpolation(parts) => {
                for part in parts {
                    if let StringPart::Interpolation(expr) = part {
                        self.infer_expr(expr);
                    }
                }
                self.ctx.string()
            }
            Expr::CharLiteral(_) => self.ctx.char(),
            Expr::BoolLiteral(_) => self.ctx.bool(),
            Expr::NilLiteral => {
                // nil has type ?T for some fresh T.
                let inner = self.ctx.fresh_type_var();
                self.ctx.intern(Ty::Optional(inner))
            }

            Expr::Identifier(ident) => {
                if let Some(ty) = self.lookup_var(&ident.name) {
                    ty
                } else if self.fn_sigs.contains_key(&ident.name) {
                    // It's a function name used as value.
                    let sig = self.fn_sigs[&ident.name].clone();
                    self.ctx.intern(Ty::Function(sig))
                } else if self.variant_ids.contains_key(&ident.name) {
                    let &(_, _, enum_ty) = &self.variant_ids[&ident.name];
                    enum_ty
                } else {
                    let suggestion = self.suggest_similar_name(&ident.name);
                    let msg = if let Some(similar) = suggestion {
                        format!(
                            "undefined variable `{}`; did you mean `{}`?",
                            ident.name, similar
                        )
                    } else {
                        format!("undefined variable `{}`", ident.name)
                    };
                    self.err(msg, ident.span);
                    self.ctx.error()
                }
            }

            Expr::Path(segments) => {
                // e.g. Type.method or Enum.Variant
                if segments.len() >= 2 {
                    let type_name = &segments[0].name;
                    let member = &segments[1].name;

                    // Check for Enum.Variant
                    if let Some(&(_, enum_ty)) = self.enum_ids.get(type_name.as_str()) {
                        return enum_ty;
                    }

                    // Check for Type.method (returns the function)
                    let key = format!("{}.{}", type_name, member);
                    if let Some(sig) = self.fn_sigs.get(&key).cloned() {
                        return self.ctx.intern(Ty::Function(sig));
                    }
                }
                self.ctx.error()
            }

            Expr::Binary(bin) => {
                let left_ty = self.infer_expr(&bin.left);
                let right_ty = self.infer_expr(&bin.right);

                match bin.op {
                    BinaryOp::Add
                    | BinaryOp::Sub
                    | BinaryOp::Mul
                    | BinaryOp::Div
                    | BinaryOp::Mod => {
                        let result = self.unify(left_ty, right_ty, span);
                        let resolved = self.apply_subs(result);
                        if resolved != self.ctx.error() && !self.ctx.is_numeric(resolved) {
                            self.err(
                                format!(
                                    "arithmetic operator requires numeric type, found `{}`",
                                    self.ctx.display(resolved)
                                ),
                                span,
                            );
                        }
                        result
                    }
                    BinaryOp::Eq | BinaryOp::NotEq => {
                        self.unify(left_ty, right_ty, span);
                        self.ctx.bool()
                    }
                    BinaryOp::Lt | BinaryOp::Gt | BinaryOp::LtEq | BinaryOp::GtEq => {
                        let result = self.unify(left_ty, right_ty, span);
                        let resolved = self.apply_subs(result);
                        if resolved != self.ctx.error()
                            && !self.ctx.is_numeric(resolved)
                            && resolved != self.ctx.char()
                        {
                            self.err(
                                format!(
                                    "comparison operator requires numeric or char type, found `{}`",
                                    self.ctx.display(resolved)
                                ),
                                span,
                            );
                        }
                        self.ctx.bool()
                    }
                    BinaryOp::And | BinaryOp::Or => {
                        self.unify(self.ctx.bool(), left_ty, bin.left.span);
                        self.unify(self.ctx.bool(), right_ty, bin.right.span);
                        self.ctx.bool()
                    }
                }
            }

            Expr::Unary(unary) => {
                let operand_ty = self.infer_expr(&unary.operand);
                match unary.op {
                    UnaryOp::Neg => {
                        let resolved = self.apply_subs(operand_ty);
                        if resolved != self.ctx.error() && !self.ctx.is_numeric(resolved) {
                            self.err(
                                format!(
                                    "unary `-` requires numeric type, found `{}`",
                                    self.ctx.display(resolved)
                                ),
                                span,
                            );
                        }
                        operand_ty
                    }
                    UnaryOp::Not => {
                        self.unify(self.ctx.bool(), operand_ty, unary.operand.span);
                        self.ctx.bool()
                    }
                    UnaryOp::Ref => self.ctx.intern(Ty::Ref(operand_ty)),
                }
            }

            Expr::Call(call) => {
                // Extract callee function name for generic instantiation.
                let callee_name = match &call.callee.node {
                    Expr::Identifier(ident) => Some(ident.name.clone()),
                    _ => None,
                };

                let callee_ty = self.infer_expr(&call.callee);
                let callee_resolved = self.apply_subs(callee_ty);
                let arg_types: Vec<_> = call.args.iter().map(|a| self.infer_expr(a)).collect();

                match self.ctx.ty(callee_resolved).clone() {
                    Ty::Function(sig) => {
                        // If this is a generic function, instantiate with fresh TypeVars
                        // so each call site infers types independently.
                        let (sig, fresh_tvars) = if let Some(ref name) = callee_name {
                            self.instantiate_fn_sig(name, &sig)
                        } else {
                            (sig, vec![])
                        };

                        if arg_types.len() != sig.params.len() {
                            self.err(
                                format!(
                                    "expected {} arguments in call to `{}`, found {}",
                                    sig.params.len(),
                                    callee_name.as_deref().unwrap_or("<closure>"),
                                    arg_types.len()
                                ),
                                span,
                            );
                        } else {
                            for (i, (&param, &arg)) in
                                sig.params.iter().zip(arg_types.iter()).enumerate()
                            {
                                self.unify(param, arg, call.args[i].span);
                            }
                        }

                        // After unification, check that inferred types satisfy trait bounds.
                        if let Some(ref name) = callee_name {
                            if !fresh_tvars.is_empty() {
                                self.check_generic_bounds(name, &fresh_tvars, span);
                            }
                        }

                        sig.return_type
                    }
                    Ty::Error => self.ctx.error(),
                    _ => {
                        // Could be a variant constructor.
                        if let Expr::Identifier(ident) = &call.callee.node {
                            if let Some(&(enum_id, variant_idx, enum_ty)) =
                                self.variant_ids.get(&ident.name)
                            {
                                let variant =
                                    &self.ctx.enums[enum_id as usize].variants[variant_idx].clone();
                                if arg_types.len() != variant.fields.len() {
                                    self.err(
                                        format!(
                                            "variant `{}` expects {} fields, found {}",
                                            variant.name,
                                            variant.fields.len(),
                                            arg_types.len()
                                        ),
                                        span,
                                    );
                                } else {
                                    for (i, (&expected, &actual)) in
                                        variant.fields.iter().zip(arg_types.iter()).enumerate()
                                    {
                                        self.unify(expected, actual, call.args[i].span);
                                    }
                                }
                                return enum_ty;
                            }
                        }
                        self.err(
                            format!(
                                "cannot call non-function type `{}`",
                                self.ctx.display(callee_resolved)
                            ),
                            span,
                        );
                        self.ctx.error()
                    }
                }
            }

            Expr::MethodCall(mc) => {
                let receiver_ty = self.infer_expr(&mc.receiver);
                let receiver_resolved = self.apply_subs(receiver_ty);
                let arg_types: Vec<_> = mc.args.iter().map(|a| self.infer_expr(a)).collect();

                let type_name = self.ctx.display(receiver_resolved);
                let key = (type_name.clone(), mc.method.name.clone());

                if let Some(sig) = self.method_sigs.get(&key).cloned() {
                    // Skip self param in sig for arg matching.
                    let non_self_params: Vec<_> = if !sig.params.is_empty() {
                        sig.params[1..].to_vec()
                    } else {
                        vec![]
                    };
                    if arg_types.len() != non_self_params.len() {
                        self.err(
                            format!(
                                "method `{}` expects {} arguments, found {}",
                                mc.method.name,
                                non_self_params.len(),
                                arg_types.len()
                            ),
                            span,
                        );
                    } else {
                        for (i, (&param, &arg)) in
                            non_self_params.iter().zip(arg_types.iter()).enumerate()
                        {
                            self.unify(param, arg, mc.args[i].span);
                        }
                    }
                    sig.return_type
                } else {
                    // Built-in methods (e.g. channel send/recv).
                    match self.ctx.ty(receiver_resolved).clone() {
                        Ty::Channel(elem) if mc.method.name == "recv" => elem,
                        Ty::Channel(elem) if mc.method.name == "send" => {
                            // Validate the argument type matches the channel element type.
                            if arg_types.len() != 1 {
                                self.err(
                                    format!(
                                        "channel `send` expects 1 argument, found {}",
                                        arg_types.len()
                                    ),
                                    span,
                                );
                            } else {
                                self.unify(elem, arg_types[0], mc.args[0].span);
                            }
                            self.ctx.unit()
                        }
                        Ty::Array(_, _) if mc.method.name == "len" => self.ctx.i64(),
                        Ty::String if mc.method.name == "len" => self.ctx.i64(),
                        Ty::Error => self.ctx.error(),
                        _ => {
                            self.err(
                                format!(
                                    "no method `{}` found on type `{}`",
                                    mc.method.name,
                                    self.ctx.display(receiver_resolved)
                                ),
                                mc.method.span,
                            );
                            self.ctx.error()
                        }
                    }
                }
            }

            Expr::FieldAccess(fa) => {
                let object_ty = self.infer_expr(&fa.object);
                let resolved = self.apply_subs(object_ty);

                match self.ctx.ty(resolved).clone() {
                    Ty::Struct(sid) => {
                        let struct_info = self.ctx.structs[sid as usize].clone();
                        if let Some(field) =
                            struct_info.fields.iter().find(|f| f.name == fa.field.name)
                        {
                            field.ty
                        } else {
                            self.err(
                                format!(
                                    "no field `{}` on type `{}`",
                                    fa.field.name, struct_info.name
                                ),
                                fa.field.span,
                            );
                            self.ctx.error()
                        }
                    }
                    Ty::Tuple(elems) => {
                        // Tuple field access: t.0, t.1, etc.
                        if let Ok(idx) = fa.field.name.parse::<usize>() {
                            if idx < elems.len() {
                                elems[idx]
                            } else {
                                self.err(
                                    format!(
                                        "tuple index {} out of bounds (len {})",
                                        idx,
                                        elems.len()
                                    ),
                                    fa.field.span,
                                );
                                self.ctx.error()
                            }
                        } else {
                            self.err(
                                format!("no field `{}` on tuple type", fa.field.name),
                                fa.field.span,
                            );
                            self.ctx.error()
                        }
                    }
                    Ty::Error => self.ctx.error(),
                    _ => {
                        self.err(
                            format!(
                                "no field `{}` on type `{}`",
                                fa.field.name,
                                self.ctx.display(resolved)
                            ),
                            fa.field.span,
                        );
                        self.ctx.error()
                    }
                }
            }

            Expr::Index(idx) => {
                let object_ty = self.infer_expr(&idx.object);
                let index_ty = self.infer_expr(&idx.index);
                let resolved = self.apply_subs(object_ty);

                match self.ctx.ty(resolved).clone() {
                    Ty::Array(elem, _) => {
                        // Index should be integer.
                        if !self.ctx.is_integer(self.apply_subs(index_ty)) {
                            self.unify(self.ctx.i64(), index_ty, idx.index.span);
                        }
                        elem
                    }
                    Ty::Error => self.ctx.error(),
                    _ => self.ctx.fresh_type_var(),
                }
            }

            Expr::StructLiteral(sl) => {
                if let Some(&(struct_id, ty_id)) = self.struct_ids.get(&sl.name.name) {
                    let struct_info = self.ctx.structs[struct_id as usize].clone();
                    let provided_names: Vec<&str> =
                        sl.fields.iter().map(|(n, _)| n.name.as_str()).collect();
                    for (field_name, field_value) in &sl.fields {
                        let value_ty = self.infer_expr(field_value);
                        if let Some(field_info) = struct_info
                            .fields
                            .iter()
                            .find(|f| f.name == field_name.name)
                        {
                            self.unify(field_info.ty, value_ty, field_value.span);
                        } else {
                            self.err(
                                format!(
                                    "unknown field `{}` in struct `{}`",
                                    field_name.name, sl.name.name
                                ),
                                field_name.span,
                            );
                        }
                    }
                    // Check for missing fields.
                    let missing: Vec<_> = struct_info
                        .fields
                        .iter()
                        .filter(|f| !provided_names.contains(&f.name.as_str()))
                        .map(|f| f.name.clone())
                        .collect();
                    if !missing.is_empty() {
                        self.err(
                            format!(
                                "missing field(s) in struct `{}`: {}",
                                sl.name.name,
                                missing.join(", ")
                            ),
                            sl.name.span,
                        );
                    }
                    ty_id
                } else {
                    self.err(
                        format!("unknown struct type `{}`", sl.name.name),
                        sl.name.span,
                    );
                    self.ctx.error()
                }
            }

            Expr::ArrayLiteral(elems) => {
                if elems.is_empty() {
                    let elem_ty = self.ctx.fresh_type_var();
                    self.ctx.intern(Ty::Array(elem_ty, None))
                } else {
                    let first_ty = self.infer_expr(&elems[0]);
                    for elem in &elems[1..] {
                        let ty = self.infer_expr(elem);
                        self.unify(first_ty, ty, elem.span);
                    }
                    self.ctx.intern(Ty::Array(first_ty, None))
                }
            }

            Expr::TupleLiteral(elems) => {
                let elem_types: Vec<_> = elems.iter().map(|e| self.infer_expr(e)).collect();
                self.ctx.intern(Ty::Tuple(elem_types))
            }

            Expr::Block(block) => self.check_block(block),

            Expr::If(if_expr) => self.check_if(if_expr, span),

            Expr::Match(match_expr) => self.check_match(match_expr, span),

            Expr::Closure(closure) => {
                self.push_scope();
                let param_types: Vec<_> = closure
                    .params
                    .iter()
                    .map(|p| {
                        let ty =
                            p.ty.as_ref()
                                .map(|t| self.resolve_ast_type(t))
                                .unwrap_or_else(|| self.ctx.fresh_type_var());
                        self.define(&p.name.name, ty);
                        ty
                    })
                    .collect();
                let body_ty = self.infer_expr(&closure.body);
                self.pop_scope();
                self.ctx.intern(Ty::Function(FnSig {
                    params: param_types,
                    return_type: body_ty,
                }))
            }

            Expr::Assign(assign) => {
                let target_ty = self.infer_expr(&assign.target);
                let value_ty = self.infer_expr(&assign.value);
                self.unify(target_ty, value_ty, assign.value.span);
                self.ctx.unit()
            }

            Expr::Range(range) => {
                let start_ty = self.infer_expr(&range.start);
                let end_ty = self.infer_expr(&range.end);
                self.unify(start_ty, end_ty, span);
                // Range type is an array of the element type.
                self.ctx.intern(Ty::Array(start_ty, None))
            }

            Expr::Try(inner) => {
                let inner_ty = self.infer_expr(inner);
                let resolved = self.apply_subs(inner_ty);
                match self.ctx.ty(resolved).clone() {
                    Ty::Optional(inner) => inner,
                    Ty::Result(ok, _err) => ok,
                    Ty::Error => self.ctx.error(),
                    _ => {
                        self.err(
                            format!(
                                "try operator `?` requires `?T` or `T ! E`, found `{}`",
                                self.ctx.display(resolved)
                            ),
                            span,
                        );
                        self.ctx.error()
                    }
                }
            }

            Expr::Spawn(block) => {
                self.push_scope();
                self.check_block_stmts(block);
                self.pop_scope();
                self.ctx.unit()
            }

            Expr::ChanCreate(cc) => {
                let elem_ty = self.resolve_ast_type(&cc.element_type);
                if let Some(cap) = &cc.capacity {
                    self.infer_expr(cap);
                }
                self.ctx.intern(Ty::Channel(elem_ty))
            }

            Expr::Select(sel) => {
                for arm in &sel.arms {
                    self.check_select_arm(arm);
                }
                self.ctx.unit()
            }

            Expr::Return(val) => {
                if let Some(v) = val {
                    let val_ty = self.infer_expr(v);
                    if let Some(ret_ty) = self.current_return_type {
                        self.unify(ret_ty, val_ty, v.span);
                    }
                } else {
                    // Bare `return` with no value — must be in a unit-returning function.
                    if let Some(ret_ty) = self.current_return_type {
                        if ret_ty != self.ctx.unit() {
                            self.err(
                                format!(
                                    "bare `return` in function returning `{}` — did you mean to return a value?",
                                    self.ctx.display(ret_ty)
                                ),
                                span,
                            );
                        }
                    }
                }
                self.ctx.never()
            }

            Expr::Break(_) | Expr::Continue => self.ctx.never(),
        }
    }

    fn check_if(&mut self, if_expr: &IfExpr, span: Span) -> TypeId {
        let cond_ty = self.infer_expr(&if_expr.condition);
        self.unify(self.ctx.bool(), cond_ty, if_expr.condition.span);

        let then_ty = self.check_block(&if_expr.then_block);

        if let Some(else_block) = &if_expr.else_block {
            let else_ty = match else_block {
                ElseBlock::Else(block) => self.check_block(block),
                ElseBlock::ElseIf(elif) => self.check_if(&elif.node, elif.span),
            };
            self.unify(then_ty, else_ty, span)
        } else {
            // if without else returns unit.
            self.ctx.unit()
        }
    }

    fn check_match(&mut self, match_expr: &MatchExpr, span: Span) -> TypeId {
        let scrutinee_ty = self.infer_expr(&match_expr.scrutinee);
        let mut result_ty: Option<TypeId> = None;

        for arm in &match_expr.arms {
            self.push_scope();
            self.check_pattern(&arm.pattern.node, scrutinee_ty);

            if let Some(guard) = &arm.guard {
                let guard_ty = self.infer_expr(guard);
                self.unify(self.ctx.bool(), guard_ty, guard.span);
            }

            let body_ty = self.infer_expr(&arm.body);
            if let Some(prev) = result_ty {
                result_ty = Some(self.unify(prev, body_ty, arm.body.span));
            } else {
                result_ty = Some(body_ty);
            }
            self.pop_scope();
        }

        // Exhaustiveness check.
        self.check_exhaustiveness(&match_expr.arms, scrutinee_ty, span);

        result_ty.unwrap_or(self.ctx.unit())
    }

    fn check_pattern(&mut self, pattern: &Pattern, expected_ty: TypeId) {
        match pattern {
            Pattern::Wildcard | Pattern::Rest => {}
            Pattern::Binding(ident) => {
                self.define(&ident.name, expected_ty);
            }
            Pattern::Literal(lit) => {
                let lit_ty = self.infer_expr(lit);
                self.unify(expected_ty, lit_ty, lit.span);
            }
            Pattern::Variant(vp) => {
                // Look up variant.
                let name = &vp.name.name;
                // Handle dot-qualified names like "Shape.Circle"
                let variant_name = if let Some(dot_pos) = name.find('.') {
                    &name[dot_pos + 1..]
                } else {
                    name.as_str()
                };
                if let Some(&(enum_id, variant_idx, _)) = self.variant_ids.get(variant_name) {
                    let variant = self.ctx.enums[enum_id as usize].variants[variant_idx].clone();
                    for (i, field_pat) in vp.fields.iter().enumerate() {
                        let field_ty = if i < variant.fields.len() {
                            variant.fields[i]
                        } else {
                            self.ctx.error()
                        };
                        self.check_pattern(&field_pat.node, field_ty);
                    }
                }
            }
            Pattern::Struct(sp) => {
                if let Some(&(struct_id, _)) = self.struct_ids.get(&sp.name.name) {
                    let struct_info = self.ctx.structs[struct_id as usize].clone();
                    for (field_name, field_pat) in &sp.fields {
                        let field_ty = struct_info
                            .fields
                            .iter()
                            .find(|f| f.name == field_name.name)
                            .map(|f| f.ty)
                            .unwrap_or(self.ctx.error());
                        self.check_pattern(&field_pat.node, field_ty);
                    }
                }
            }
            Pattern::Tuple(pats) => {
                let resolved = self.apply_subs(expected_ty);
                if let Ty::Tuple(elems) = self.ctx.ty(resolved).clone() {
                    for (i, pat) in pats.iter().enumerate() {
                        let elem_ty = if i < elems.len() {
                            elems[i]
                        } else {
                            self.ctx.error()
                        };
                        self.check_pattern(&pat.node, elem_ty);
                    }
                }
            }
            Pattern::Or(pats) => {
                for pat in pats {
                    self.check_pattern(&pat.node, expected_ty);
                }
            }
        }
    }

    fn check_exhaustiveness(&mut self, arms: &[MatchArm], scrutinee_ty: TypeId, span: Span) {
        let patterns: Vec<&Pattern> = arms.iter().map(|a| &a.pattern.node).collect();
        let missing = self.find_missing_patterns(&patterns, scrutinee_ty);
        if !missing.is_empty() {
            let resolved = self.apply_subs(scrutinee_ty);
            match self.ctx.ty(resolved).clone() {
                Ty::Enum(_) => {
                    self.err(
                        format!(
                            "non-exhaustive match: missing variant(s): {}",
                            missing.join(", ")
                        ),
                        span,
                    );
                }
                Ty::Bool => {
                    self.err(
                        "non-exhaustive match on bool: add a wildcard `_` pattern",
                        span,
                    );
                }
                _ => {
                    self.err(
                        format!(
                            "non-exhaustive match on type `{}`: add a wildcard `_` pattern",
                            self.ctx.display(resolved)
                        ),
                        span,
                    );
                }
            }
        }
    }

    /// Recursively find missing patterns for a type given a set of patterns.
    /// Returns descriptions of missing pattern(s), or empty if exhaustive.
    fn find_missing_patterns(&self, patterns: &[&Pattern], ty: TypeId) -> Vec<String> {
        // Flatten Or patterns into the pattern list.
        let mut flat: Vec<&Pattern> = Vec::new();
        for &p in patterns {
            if let Pattern::Or(subs) = p {
                for sub in subs {
                    flat.push(&sub.node);
                }
            } else {
                flat.push(p);
            }
        }

        // If any pattern is wildcard or binding, everything is covered.
        if flat
            .iter()
            .any(|p| matches!(p, Pattern::Wildcard | Pattern::Binding(_)))
        {
            return vec![];
        }

        let resolved = self.apply_subs(ty);
        match self.ctx.ty(resolved).clone() {
            Ty::Enum(enum_id) => {
                let enum_info = self.ctx.enums[enum_id as usize].clone();
                let mut missing = Vec::new();

                for variant in &enum_info.variants {
                    // Collect sub-pattern lists from arms matching this variant.
                    let sub_pattern_sets: Vec<Vec<&Pattern>> = flat
                        .iter()
                        .filter_map(|&p| {
                            if let Pattern::Variant(vp) = p {
                                let name = &vp.name.name;
                                let variant_name = if let Some(dot_pos) = name.find('.') {
                                    &name[dot_pos + 1..]
                                } else {
                                    name.as_str()
                                };
                                if variant_name == variant.name {
                                    Some(vp.fields.iter().map(|f| &f.node).collect())
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        })
                        .collect();

                    if sub_pattern_sets.is_empty() {
                        // No arm matches this variant at all.
                        missing.push(variant.name.clone());
                        continue;
                    }

                    // Variant is matched. If it has no fields, it's fully covered.
                    if variant.fields.is_empty() {
                        continue;
                    }

                    // Check each field position recursively.
                    for (field_idx, field_ty) in variant.fields.iter().enumerate() {
                        let field_pats: Vec<&Pattern> = sub_pattern_sets
                            .iter()
                            .filter_map(|sp| sp.get(field_idx).copied())
                            .collect();
                        let sub_missing = self.find_missing_patterns(&field_pats, *field_ty);
                        for m in &sub_missing {
                            missing.push(format!("{}({})", variant.name, m));
                        }
                    }
                }

                missing
            }
            Ty::Bool => {
                let has_true = flat.iter().any(|p| {
                    matches!(p, Pattern::Literal(lit) if matches!(lit.node, Expr::BoolLiteral(true)))
                });
                let has_false = flat.iter().any(|p| {
                    matches!(p, Pattern::Literal(lit) if matches!(lit.node, Expr::BoolLiteral(false)))
                });
                let mut missing = Vec::new();
                if !has_true {
                    missing.push("true".to_string());
                }
                if !has_false {
                    missing.push("false".to_string());
                }
                missing
            }
            _ => {
                // For integers, strings, etc. — require a wildcard.
                vec![format!("`_` for {}", self.ctx.display(resolved))]
            }
        }
    }

    fn check_select_arm(&mut self, arm: &SelectArm) {
        match &arm.kind {
            SelectArmKind::Recv { binding, channel } => {
                let chan_ty = self.infer_expr(channel);
                let resolved = self.apply_subs(chan_ty);
                let elem_ty = match self.ctx.ty(resolved).clone() {
                    Ty::Channel(elem) => elem,
                    _ => self.ctx.fresh_type_var(),
                };
                self.push_scope();
                self.define(&binding.name, elem_ty);
                self.check_block_stmts(&arm.body);
                self.pop_scope();
            }
            SelectArmKind::Send { channel, value } => {
                self.infer_expr(channel);
                self.infer_expr(value);
                self.push_scope();
                self.check_block_stmts(&arm.body);
                self.pop_scope();
            }
            SelectArmKind::After(dur) => {
                self.infer_expr(dur);
                self.push_scope();
                self.check_block_stmts(&arm.body);
                self.pop_scope();
            }
        }
    }
}
