//! Adam Standard Library -- compiler-side support for built-in types and functions.
//!
//! This crate does **not** contain Adam source code (the compiler cannot yet
//! compile `.adam` files).  Instead it defines Rust metadata that the
//! compiler's type checker and codegen consult to know which traits exist,
//! which primitive types implement them, and how operators map to traits.

pub mod operator;
pub mod primitives;
pub mod traits;

// Re-export the most commonly used items at the crate root for convenience.
pub use operator::{op_to_trait, parse_op, OpKind};
pub use primitives::{primitive_has_trait, primitive_traits};
pub use traits::{core_trait_methods, CoreTrait, Ordering, TraitMethod};
