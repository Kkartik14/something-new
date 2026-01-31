//! Scope tree — a hierarchy of scopes mapping names to declarations.

use std::collections::HashMap;

use adam_ast::common::Span;
use adam_ast::item::ParamOwnership;

/// Unique identifier for a scope.
pub type ScopeId = u32;

/// Unique identifier for a declaration.
pub type DeclId = u32;

/// A tree of nested scopes.
#[derive(Debug)]
pub struct ScopeTree {
    scopes: Vec<Scope>,
}

impl ScopeTree {
    pub fn new() -> Self {
        let root = Scope {
            id: 0,
            parent: None,
            kind: ScopeKind::Module,
            declarations: HashMap::new(),
            children: vec![],
        };
        Self {
            scopes: vec![root],
        }
    }

    pub fn root(&self) -> ScopeId {
        0
    }

    /// Create a new child scope and return its id.
    pub fn add_scope(&mut self, parent: ScopeId, kind: ScopeKind) -> ScopeId {
        let id = self.scopes.len() as ScopeId;
        let scope = Scope {
            id,
            parent: Some(parent),
            kind,
            declarations: HashMap::new(),
            children: vec![],
        };
        self.scopes.push(scope);
        self.scopes[parent as usize].children.push(id);
        id
    }

    pub fn scope(&self, id: ScopeId) -> &Scope {
        &self.scopes[id as usize]
    }

    pub fn scope_mut(&mut self, id: ScopeId) -> &mut Scope {
        &mut self.scopes[id as usize]
    }

    /// Insert a declaration into a scope. Returns `Err` with the existing
    /// `DeclId` if the name is already declared in this scope.
    pub fn declare(
        &mut self,
        scope: ScopeId,
        name: String,
        decl_id: DeclId,
    ) -> Result<(), DeclId> {
        let s = &mut self.scopes[scope as usize];
        if let Some(&existing) = s.declarations.get(&name) {
            Err(existing)
        } else {
            s.declarations.insert(name, decl_id);
            Ok(())
        }
    }

    /// Look up a name starting from `scope`, walking up parent scopes.
    pub fn lookup(&self, scope: ScopeId, name: &str) -> Option<DeclId> {
        let s = &self.scopes[scope as usize];
        if let Some(&id) = s.declarations.get(name) {
            return Some(id);
        }
        if let Some(parent) = s.parent {
            self.lookup(parent, name)
        } else {
            None
        }
    }

    /// Look up a name in exactly one scope (no parent walking).
    pub fn lookup_local(&self, scope: ScopeId, name: &str) -> Option<DeclId> {
        self.scopes[scope as usize].declarations.get(name).copied()
    }

    /// Collect all names declared in a scope (for "did you mean" suggestions).
    pub fn names_in_scope(&self, scope: ScopeId) -> Vec<&str> {
        let mut names = vec![];
        let mut current = Some(scope);
        while let Some(id) = current {
            let s = &self.scopes[id as usize];
            for name in s.declarations.keys() {
                names.push(name.as_str());
            }
            current = s.parent;
        }
        names
    }

    pub fn scope_count(&self) -> usize {
        self.scopes.len()
    }
}

/// A single scope.
#[derive(Debug)]
pub struct Scope {
    pub id: ScopeId,
    pub parent: Option<ScopeId>,
    pub kind: ScopeKind,
    pub declarations: HashMap<String, DeclId>,
    pub children: Vec<ScopeId>,
}

/// What kind of scope this is.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeKind {
    /// File-level module scope.
    Module,
    /// Function body.
    Function,
    /// Generic `{ ... }` block.
    Block,
    /// Loop body (for, while, loop).
    Loop,
    /// Impl block.
    Impl,
    /// Trait block.
    Trait,
    /// View body.
    View,
    /// Match arm.
    Match,
    /// Closure body.
    Closure,
}

/// A resolved declaration.
#[derive(Debug, Clone)]
pub struct Declaration {
    pub id: DeclId,
    pub name: String,
    pub kind: DeclKind,
    pub span: Span,
    pub scope: ScopeId,
}

/// What kind of declaration this is.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DeclKind {
    Variable { mutable: bool },
    Function,
    Struct,
    Enum,
    EnumVariant,
    Trait,
    TypeParam,
    Param { ownership: ParamOwnership },
    Field,
    Method,
    Import,
    ViewField,
    Module,
}
