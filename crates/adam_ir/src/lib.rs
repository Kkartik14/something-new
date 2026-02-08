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
pub mod opt;
pub mod print;
pub mod verify;

#[cfg(test)]
mod adversarial_tests;
#[cfg(test)]
mod tests;

pub use ir::*;
pub use lower::lower_module;
