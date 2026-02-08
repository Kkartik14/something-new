//! Adam Borrow Checker — verifies ownership and borrowing rules at compile time.
//!
//! This crate walks the AST after name resolution and type checking,
//! tracking the ownership state of every variable. It enforces:
//!
//! - No use-after-move (unless the type is Copy).
//! - No simultaneous mutable and shared borrows.
//! - No double mutable borrows.
//! - No returning references to local variables.
//! - Move-in-loop detection.
//! - Assignment only to mutable variables.
//!
//! Copy types (all numeric primitives, `bool`, `char`) are never moved
//! and can be freely duplicated.
//!
//! Parameter ownership annotations control how arguments are passed:
//! - (default) — shared, immutable borrow.
//! - `own` — takes ownership (move).
//! - `mut` — mutable borrow.

pub mod checker;
pub mod ownership;

#[cfg(test)]
mod adversarial_tests;
#[cfg(test)]
mod tests;

pub use checker::{BorrowCheckResult, BorrowChecker, BorrowError};
pub use ownership::{OwnershipTracker, VarState};
