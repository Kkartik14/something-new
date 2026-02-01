//! Adam Standard Library -- compiler-side support for built-in types and functions.
//!
//! This crate does **not** contain Adam source code (the compiler cannot yet
//! compile `.adam` files).  Instead it defines Rust metadata that the
//! compiler's type checker and codegen consult to know which traits exist,
//! which primitive types implement them, and how operators map to traits.

pub mod traits;
pub mod primitives;
pub mod operator;

// Re-export the most commonly used items at the crate root for convenience.
pub use traits::{CoreTrait, Ordering, TraitMethod, core_trait_methods};
pub use primitives::{primitive_traits, primitive_has_trait};
pub use operator::{OpKind, op_to_trait, parse_op};
