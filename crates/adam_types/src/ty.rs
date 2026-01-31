//! Internal type representation — different from AST type nodes.
//! Types are interned: each unique type gets a `TypeId`, and
//! identical types always map to the same `TypeId`.

use std::collections::HashMap;

/// Unique identifier for an interned type.
pub type TypeId = u32;

/// Unique identifier for a type variable (unsolved inference variable).
pub type TypeVarId = u32;

/// Unique identifier for a generic type parameter.
pub type GenericId = u32;

/// Unique identifier for a struct definition.
pub type StructId = u32;

/// Unique identifier for an enum definition.
pub type EnumId = u32;

/// Unique identifier for a trait definition.
pub type TraitId = u32;

/// Internal type representation.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ty {
    // Primitives
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Bool,
    Char,
    Unit,

    // Compound
    String,
    Str,
    Array(TypeId, Option<u64>),
    Tuple(Vec<TypeId>),
    Struct(StructId),
    Enum(EnumId),

    // Smart pointers
    Box(TypeId),
    Rc(TypeId),
    Arc(TypeId),

    // References
    Ref(TypeId),
    MutRef(TypeId),

    // Optional and Result
    Optional(TypeId),
    Result(TypeId, TypeId),

    // Function
    Function(FnSig),

    // Channel
    Channel(TypeId),

    // Generics
    TypeVar(TypeVarId),
    Generic(GenericId),

    // Error (propagates without cascading errors)
    Error,

    // Never (diverging: panic, infinite loop, return)
    Never,
}

/// Function signature.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnSig {
    pub params: Vec<TypeId>,
    pub return_type: TypeId,
}

/// Information about a struct type.
#[derive(Debug, Clone)]
pub struct StructInfo {
    pub name: std::string::String,
    pub fields: Vec<FieldInfo>,
    pub generic_params: Vec<GenericId>,
}

/// Struct field info.
#[derive(Debug, Clone)]
pub struct FieldInfo {
    pub name: std::string::String,
    pub ty: TypeId,
    pub is_public: bool,
}

/// Information about an enum type.
#[derive(Debug, Clone)]
pub struct EnumInfo {
    pub name: std::string::String,
    pub variants: Vec<VariantInfo>,
    pub generic_params: Vec<GenericId>,
}

/// Enum variant info.
#[derive(Debug, Clone)]
pub struct VariantInfo {
    pub name: std::string::String,
    pub fields: Vec<TypeId>,
}

/// Information about a trait.
#[derive(Debug, Clone)]
pub struct TraitInfo {
    pub name: std::string::String,
    pub methods: Vec<TraitMethodInfo>,
}

/// Trait method info.
#[derive(Debug, Clone)]
pub struct TraitMethodInfo {
    pub name: std::string::String,
    pub sig: FnSig,
    pub has_default: bool,
}

/// A trait implementation.
#[derive(Debug, Clone)]
pub struct TraitImpl {
    pub trait_id: TraitId,
    pub target_type: TypeId,
    pub methods: Vec<(std::string::String, FnSig)>,
}

/// Type context — interning and storage for all types.
#[derive(Debug)]
pub struct TypeContext {
    /// All interned types, indexed by TypeId.
    types: Vec<Ty>,
    /// Reverse map for interning.
    intern_map: HashMap<Ty, TypeId>,
    /// Next type variable id.
    next_type_var: TypeVarId,
    /// Next generic id.
    next_generic: GenericId,
    /// Struct definitions.
    pub structs: Vec<StructInfo>,
    /// Enum definitions.
    pub enums: Vec<EnumInfo>,
    /// Trait definitions.
    pub traits: Vec<TraitInfo>,
    /// Trait implementations.
    pub trait_impls: Vec<TraitImpl>,
}

impl TypeContext {
    pub fn new() -> Self {
        let mut ctx = Self {
            types: vec![],
            intern_map: HashMap::new(),
            next_type_var: 0,
            next_generic: 0,
            structs: vec![],
            enums: vec![],
            traits: vec![],
            trait_impls: vec![],
        };
        // Pre-intern common types so they have predictable IDs.
        ctx.intern(Ty::I8);     // 0
        ctx.intern(Ty::I16);    // 1
        ctx.intern(Ty::I32);    // 2
        ctx.intern(Ty::I64);    // 3
        ctx.intern(Ty::U8);     // 4
        ctx.intern(Ty::U16);    // 5
        ctx.intern(Ty::U32);    // 6
        ctx.intern(Ty::U64);    // 7
        ctx.intern(Ty::F32);    // 8
        ctx.intern(Ty::F64);    // 9
        ctx.intern(Ty::Bool);   // 10
        ctx.intern(Ty::Char);   // 11
        ctx.intern(Ty::Unit);   // 12
        ctx.intern(Ty::String); // 13
        ctx.intern(Ty::Str);    // 14
        ctx.intern(Ty::Error);  // 15
        ctx.intern(Ty::Never);  // 16
        ctx
    }

    /// Intern a type, returning its unique TypeId.
    pub fn intern(&mut self, ty: Ty) -> TypeId {
        if let Some(&id) = self.intern_map.get(&ty) {
            return id;
        }
        let id = self.types.len() as TypeId;
        self.intern_map.insert(ty.clone(), id);
        self.types.push(ty);
        id
    }

    /// Get the type for a TypeId.
    pub fn ty(&self, id: TypeId) -> &Ty {
        &self.types[id as usize]
    }

    /// Create a fresh type variable.
    pub fn fresh_type_var(&mut self) -> TypeId {
        let var_id = self.next_type_var;
        self.next_type_var += 1;
        self.intern(Ty::TypeVar(var_id))
    }

    /// Create a fresh generic parameter.
    pub fn fresh_generic(&mut self) -> (GenericId, TypeId) {
        let gen_id = self.next_generic;
        self.next_generic += 1;
        let ty_id = self.intern(Ty::Generic(gen_id));
        (gen_id, ty_id)
    }

    /// Register a struct and return its StructId.
    pub fn add_struct(&mut self, info: StructInfo) -> StructId {
        let id = self.structs.len() as StructId;
        self.structs.push(info);
        id
    }

    /// Register an enum and return its EnumId.
    pub fn add_enum(&mut self, info: EnumInfo) -> EnumId {
        let id = self.enums.len() as EnumId;
        self.enums.push(info);
        id
    }

    /// Register a trait and return its TraitId.
    pub fn add_trait(&mut self, info: TraitInfo) -> TraitId {
        let id = self.traits.len() as TraitId;
        self.traits.push(info);
        id
    }

    /// Register a trait impl.
    pub fn add_trait_impl(&mut self, imp: TraitImpl) {
        self.trait_impls.push(imp);
    }

    // ---- Well-known type IDs ----

    pub fn i8(&self) -> TypeId { 0 }
    pub fn i16(&self) -> TypeId { 1 }
    pub fn i32(&self) -> TypeId { 2 }
    pub fn i64(&self) -> TypeId { 3 }
    pub fn u8(&self) -> TypeId { 4 }
    pub fn u16(&self) -> TypeId { 5 }
    pub fn u32(&self) -> TypeId { 6 }
    pub fn u64(&self) -> TypeId { 7 }
    pub fn f32(&self) -> TypeId { 8 }
    pub fn f64(&self) -> TypeId { 9 }
    pub fn bool(&self) -> TypeId { 10 }
    pub fn char(&self) -> TypeId { 11 }
    pub fn unit(&self) -> TypeId { 12 }
    pub fn string(&self) -> TypeId { 13 }
    pub fn str(&self) -> TypeId { 14 }
    pub fn error(&self) -> TypeId { 15 }
    pub fn never(&self) -> TypeId { 16 }

    /// Check if a type is numeric (integer or float).
    pub fn is_numeric(&self, id: TypeId) -> bool {
        matches!(
            self.ty(id),
            Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64
                | Ty::U8 | Ty::U16 | Ty::U32 | Ty::U64
                | Ty::F32 | Ty::F64
        )
    }

    /// Check if a type is an integer.
    pub fn is_integer(&self, id: TypeId) -> bool {
        matches!(
            self.ty(id),
            Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64
                | Ty::U8 | Ty::U16 | Ty::U32 | Ty::U64
        )
    }

    /// Check if a type is a float.
    pub fn is_float(&self, id: TypeId) -> bool {
        matches!(self.ty(id), Ty::F32 | Ty::F64)
    }

    /// Check if a type is a Copy type (never moves).
    pub fn is_copy(&self, id: TypeId) -> bool {
        matches!(
            self.ty(id),
            Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64
                | Ty::U8 | Ty::U16 | Ty::U32 | Ty::U64
                | Ty::F32 | Ty::F64
                | Ty::Bool | Ty::Char | Ty::Unit
                | Ty::Never | Ty::Error
        )
    }

    /// Check if a type is Send (safe to move across threads).
    pub fn is_send(&self, id: TypeId) -> bool {
        match self.ty(id) {
            Ty::Rc(_) => false,  // Rc is not Send
            Ty::Ref(_) | Ty::MutRef(_) => false,  // references not Send by default
            Ty::Error | Ty::Never => true,
            _ => true,  // most types are Send
        }
    }

    /// Resolve a type, following TypeVar substitutions.
    pub fn resolve(&self, id: TypeId, subs: &HashMap<TypeVarId, TypeId>) -> TypeId {
        match self.ty(id) {
            Ty::TypeVar(var) => {
                if let Some(&resolved) = subs.get(var) {
                    self.resolve(resolved, subs)
                } else {
                    id
                }
            }
            _ => id,
        }
    }

    /// Display a type as a human-readable string.
    pub fn display(&self, id: TypeId) -> std::string::String {
        match self.ty(id).clone() {
            Ty::I8 => "i8".into(),
            Ty::I16 => "i16".into(),
            Ty::I32 => "i32".into(),
            Ty::I64 => "i64".into(),
            Ty::U8 => "u8".into(),
            Ty::U16 => "u16".into(),
            Ty::U32 => "u32".into(),
            Ty::U64 => "u64".into(),
            Ty::F32 => "f32".into(),
            Ty::F64 => "f64".into(),
            Ty::Bool => "bool".into(),
            Ty::Char => "char".into(),
            Ty::Unit => "()".into(),
            Ty::String => "String".into(),
            Ty::Str => "str".into(),
            Ty::Array(elem, None) => format!("[{}]", self.display(elem)),
            Ty::Array(elem, Some(n)) => format!("[{}; {}]", self.display(elem), n),
            Ty::Tuple(elems) => {
                let parts: Vec<_> = elems.iter().map(|e| self.display(*e)).collect();
                format!("({})", parts.join(", "))
            }
            Ty::Struct(sid) => self.structs[sid as usize].name.clone(),
            Ty::Enum(eid) => self.enums[eid as usize].name.clone(),
            Ty::Box(inner) => format!("Box[{}]", self.display(inner)),
            Ty::Rc(inner) => format!("Rc[{}]", self.display(inner)),
            Ty::Arc(inner) => format!("Arc[{}]", self.display(inner)),
            Ty::Ref(inner) => format!("&{}", self.display(inner)),
            Ty::MutRef(inner) => format!("&mut {}", self.display(inner)),
            Ty::Optional(inner) => format!("?{}", self.display(inner)),
            Ty::Result(ok, err) => format!("{} ! {}", self.display(ok), self.display(err)),
            Ty::Function(sig) => {
                let params: Vec<_> = sig.params.iter().map(|p| self.display(*p)).collect();
                format!("fn({}) -> {}", params.join(", "), self.display(sig.return_type))
            }
            Ty::Channel(inner) => format!("chan[{}]", self.display(inner)),
            Ty::TypeVar(v) => format!("?T{}", v),
            Ty::Generic(g) => format!("G{}", g),
            Ty::Error => "<error>".into(),
            Ty::Never => "!".into(),
        }
    }

    /// Total number of interned types.
    pub fn type_count(&self) -> usize {
        self.types.len()
    }
}
