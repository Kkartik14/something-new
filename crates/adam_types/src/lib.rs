//! Adam Type System — type checking, inference, and generic instantiation.

pub mod ty;
pub mod checker;

#[cfg(test)]
mod tests;

#[cfg(test)]
mod adversarial_tests;

#[cfg(test)]
mod fuzz_tests;

pub use ty::{TypeContext, TypeId, Ty, FnSig, StructInfo, EnumInfo, TraitInfo};
pub use checker::{TypeChecker, TypeCheckResult, TypeError};
