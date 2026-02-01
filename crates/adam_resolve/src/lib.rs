//! Adam Name Resolution — links identifiers to declarations.
//!
//! Walks the AST, builds a scope tree, and resolves every
//! name to its declaration. Detects undefined and duplicate names.

pub mod scope;
pub mod errors;
pub mod resolver;

#[cfg(test)]
mod tests;
#[cfg(test)]
mod adversarial_tests;

pub use resolver::{resolve, ResolveResult};
pub use scope::{ScopeTree, ScopeId, DeclId, Declaration, DeclKind, ScopeKind};
pub use errors::{ResolveError, ResolveErrorKind};
