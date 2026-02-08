//! Adam AST — Abstract Syntax Tree node definitions.
//!
//! This crate defines every node type that the parser produces.
//! It contains NO logic — purely data structures.
//! Every other compiler crate depends on this one.

pub mod common;
pub mod expr;
pub mod item;
pub mod pattern;
pub mod pretty;
pub mod stmt;
pub mod types;
