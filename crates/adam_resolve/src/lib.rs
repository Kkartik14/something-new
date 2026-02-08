//! Adam Name Resolution — links identifiers to declarations.
//!
//! Walks the AST, builds a scope tree, and resolves every
//! name to its declaration. Detects undefined and duplicate names.

pub mod errors;
pub mod resolver;
pub mod scope;

#[cfg(test)]
mod adversarial_tests;
#[cfg(test)]
mod tests;

pub use errors::{ResolveError, ResolveErrorKind};
pub use resolver::{resolve, resolve_multi, ResolveResult};
pub use scope::{DeclId, DeclKind, Declaration, ScopeId, ScopeKind, ScopeTree};
