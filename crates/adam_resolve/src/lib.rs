//! Adam Name Resolution — links identifiers to declarations.
//!
//! Walks the AST, builds a scope tree, and resolves every
//! name to its declaration. Detects undefined and duplicate names.
