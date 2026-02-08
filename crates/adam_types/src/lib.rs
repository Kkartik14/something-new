//! Adam Type System — type checking, inference, and generic instantiation.

pub mod checker;
pub mod ty;

#[cfg(test)]
mod tests;

#[cfg(test)]
mod adversarial_tests;

#[cfg(test)]
mod fuzz_tests;

pub use checker::{TypeCheckResult, TypeChecker, TypeError};
pub use ty::{EnumInfo, FnSig, StructInfo, TraitInfo, Ty, TypeContext, TypeId};
