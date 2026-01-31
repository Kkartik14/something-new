//! Adam IR — mid-level intermediate representation (control flow graph).
//!
//! The Adam IR (AIR) is a CFG-based intermediate representation that
//! sits between the typed AST and the final code generator. It
//! organizes code into functions, basic blocks, instructions, and
//! terminators.
//!
//! # Architecture
//!
//! - **`ir`** — Core data structures (IrModule, IrFunction, BasicBlock, etc.)
//! - **`lower`** — AST-to-IR lowering pass
//! - **`print`** — Human-readable textual printer
//! - **`verify`** — Structural verification pass
//! - **`opt`** — Basic optimization passes (constant folding, DCE, CFG simplify)

pub mod ir;
pub mod lower;
pub mod print;
pub mod verify;
pub mod opt;

#[cfg(test)]
mod tests;
#[cfg(test)]
mod adversarial_tests;

pub use ir::*;
pub use lower::lower_module;
