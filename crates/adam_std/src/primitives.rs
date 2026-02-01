//! Mapping from Adam primitive type names to the core traits they implement.
//!
//! The compiler's type checker calls [`primitive_traits`] to discover which
//! operations are valid on a given primitive without consulting any
//! user-written `impl` blocks.

use crate::traits::CoreTrait;

/// Return the set of [`CoreTrait`]s that the named primitive type implements.
///
/// Accepted `type_name` values: `"i8"`, `"i16"`, `"i32"`, `"i64"`,
/// `"u8"`, `"u16"`, `"u32"`, `"u64"`, `"f32"`, `"f64"`, `"bool"`,
/// `"char"`, `"String"`.
///
/// Returns an empty `Vec` for unknown type names.
pub fn primitive_traits(type_name: &str) -> Vec<CoreTrait> {
    match type_name {
        "i8" | "i16" | "i32" | "i64" => signed_integer_traits(),
        "u8" | "u16" | "u32" | "u64" => unsigned_integer_traits(),
        "f32" | "f64" => float_traits(),
        "bool" => bool_traits(),
        "char" => char_traits(),
        "String" => string_traits(),
        _ => Vec::new(),
    }
}

/// Check whether a primitive type implements a specific trait.
pub fn primitive_has_trait(type_name: &str, t: CoreTrait) -> bool {
    primitive_traits(type_name).contains(&t)
}

// -- Private helpers --------------------------------------------------------

fn signed_integer_traits() -> Vec<CoreTrait> {
    vec![
        CoreTrait::Eq,
        CoreTrait::Comparable,
        CoreTrait::Display,
        CoreTrait::Debug,
        CoreTrait::Hash,
        CoreTrait::Clone,
        CoreTrait::Copy,
        CoreTrait::Default,
        CoreTrait::Add,
        CoreTrait::Sub,
        CoreTrait::Mul,
        CoreTrait::Div,
        CoreTrait::Mod,
        CoreTrait::Neg,
        CoreTrait::Send,
        CoreTrait::Sync,
    ]
}

fn unsigned_integer_traits() -> Vec<CoreTrait> {
    vec![
        CoreTrait::Eq,
        CoreTrait::Comparable,
        CoreTrait::Display,
        CoreTrait::Debug,
        CoreTrait::Hash,
        CoreTrait::Clone,
        CoreTrait::Copy,
        CoreTrait::Default,
        CoreTrait::Add,
        CoreTrait::Sub,
        CoreTrait::Mul,
        CoreTrait::Div,
        CoreTrait::Mod,
        // No Neg — unsigned types cannot negate.
        CoreTrait::Send,
        CoreTrait::Sync,
    ]
}

fn float_traits() -> Vec<CoreTrait> {
    vec![
        CoreTrait::Eq,
        CoreTrait::Comparable,
        CoreTrait::Display,
        CoreTrait::Debug,
        // No Hash — floating-point values cannot be reliably hashed.
        CoreTrait::Clone,
        CoreTrait::Copy,
        CoreTrait::Default,
        CoreTrait::Add,
        CoreTrait::Sub,
        CoreTrait::Mul,
        CoreTrait::Div,
        CoreTrait::Mod,
        CoreTrait::Neg,
        CoreTrait::Send,
        CoreTrait::Sync,
    ]
}

fn bool_traits() -> Vec<CoreTrait> {
    vec![
        CoreTrait::Eq,
        CoreTrait::Display,
        CoreTrait::Debug,
        CoreTrait::Hash,
        CoreTrait::Clone,
        CoreTrait::Copy,
        CoreTrait::Default,
        CoreTrait::Send,
        CoreTrait::Sync,
    ]
}

fn char_traits() -> Vec<CoreTrait> {
    vec![
        CoreTrait::Eq,
        CoreTrait::Comparable,
        CoreTrait::Display,
        CoreTrait::Debug,
        CoreTrait::Hash,
        CoreTrait::Clone,
        CoreTrait::Copy,
        CoreTrait::Default,
        CoreTrait::Send,
        CoreTrait::Sync,
    ]
}

fn string_traits() -> Vec<CoreTrait> {
    vec![
        CoreTrait::Eq,
        CoreTrait::Comparable,
        CoreTrait::Display,
        CoreTrait::Debug,
        CoreTrait::Hash,
        CoreTrait::Clone,
        CoreTrait::Default,
        CoreTrait::Drop,
        CoreTrait::Iterable,
        CoreTrait::Index,
        CoreTrait::Add,
        CoreTrait::Send,
        CoreTrait::Sync,
    ]
}

// =========================================================================
// Tests
// =========================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // -- i32 trait set -------------------------------------------------------

    #[test]
    fn i32_implements_arithmetic() {
        let traits = primitive_traits("i32");
        assert!(traits.contains(&CoreTrait::Add));
        assert!(traits.contains(&CoreTrait::Sub));
        assert!(traits.contains(&CoreTrait::Mul));
        assert!(traits.contains(&CoreTrait::Div));
        assert!(traits.contains(&CoreTrait::Mod));
        assert!(traits.contains(&CoreTrait::Neg));
    }

    #[test]
    fn i32_implements_eq_and_comparable() {
        let traits = primitive_traits("i32");
        assert!(traits.contains(&CoreTrait::Eq));
        assert!(traits.contains(&CoreTrait::Comparable));
    }

    #[test]
    fn i32_implements_hash() {
        assert!(primitive_has_trait("i32", CoreTrait::Hash));
    }

    #[test]
    fn i32_does_not_implement_drop() {
        assert!(!primitive_has_trait("i32", CoreTrait::Drop));
    }

    #[test]
    fn i32_is_copy() {
        assert!(primitive_has_trait("i32", CoreTrait::Copy));
    }

    // -- unsigned integers ---------------------------------------------------

    #[test]
    fn u64_does_not_implement_neg() {
        assert!(!primitive_has_trait("u64", CoreTrait::Neg));
    }

    #[test]
    fn u8_implements_add() {
        assert!(primitive_has_trait("u8", CoreTrait::Add));
    }

    // -- floats --------------------------------------------------------------

    #[test]
    fn f64_does_not_implement_hash() {
        assert!(!primitive_has_trait("f64", CoreTrait::Hash));
    }

    #[test]
    fn f32_does_not_implement_hash() {
        assert!(!primitive_has_trait("f32", CoreTrait::Hash));
    }

    #[test]
    fn f64_implements_neg() {
        assert!(primitive_has_trait("f64", CoreTrait::Neg));
    }

    #[test]
    fn f64_implements_eq_and_comparable() {
        assert!(primitive_has_trait("f64", CoreTrait::Eq));
        assert!(primitive_has_trait("f64", CoreTrait::Comparable));
    }

    // -- bool ----------------------------------------------------------------

    #[test]
    fn bool_does_not_implement_add() {
        assert!(!primitive_has_trait("bool", CoreTrait::Add));
    }

    #[test]
    fn bool_does_not_implement_comparable() {
        assert!(!primitive_has_trait("bool", CoreTrait::Comparable));
    }

    #[test]
    fn bool_implements_eq_and_hash() {
        assert!(primitive_has_trait("bool", CoreTrait::Eq));
        assert!(primitive_has_trait("bool", CoreTrait::Hash));
    }

    // -- char ----------------------------------------------------------------

    #[test]
    fn char_implements_comparable() {
        assert!(primitive_has_trait("char", CoreTrait::Comparable));
    }

    #[test]
    fn char_does_not_implement_add() {
        assert!(!primitive_has_trait("char", CoreTrait::Add));
    }

    // -- String --------------------------------------------------------------

    #[test]
    fn string_implements_drop() {
        assert!(primitive_has_trait("String", CoreTrait::Drop));
    }

    #[test]
    fn string_does_not_implement_copy() {
        assert!(!primitive_has_trait("String", CoreTrait::Copy));
    }

    #[test]
    fn string_implements_iterable_and_index() {
        assert!(primitive_has_trait("String", CoreTrait::Iterable));
        assert!(primitive_has_trait("String", CoreTrait::Index));
    }

    #[test]
    fn string_implements_add_for_concatenation() {
        assert!(primitive_has_trait("String", CoreTrait::Add));
    }

    // -- unknown types -------------------------------------------------------

    #[test]
    fn unknown_type_returns_empty() {
        assert!(primitive_traits("FooBar").is_empty());
    }

    // -- all signed integers share the same trait set -------------------------

    #[test]
    fn all_signed_integers_same_traits() {
        let i8t = primitive_traits("i8");
        let i16t = primitive_traits("i16");
        let i32t = primitive_traits("i32");
        let i64t = primitive_traits("i64");
        assert_eq!(i8t, i16t);
        assert_eq!(i16t, i32t);
        assert_eq!(i32t, i64t);
    }

    // -- all unsigned integers share the same trait set ------------------------

    #[test]
    fn all_unsigned_integers_same_traits() {
        let u8t = primitive_traits("u8");
        let u16t = primitive_traits("u16");
        let u32t = primitive_traits("u32");
        let u64t = primitive_traits("u64");
        assert_eq!(u8t, u16t);
        assert_eq!(u16t, u32t);
        assert_eq!(u32t, u64t);
    }
}
