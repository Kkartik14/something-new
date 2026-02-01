//! Core trait definitions for the Adam language.
//!
//! These are Rust-side metadata structures that the compiler's type checker
//! and codegen reference when resolving trait implementations, operator
//! overloading, and auto-derived capabilities for primitive types.

use std::fmt;

/// Every built-in trait in the Adam language.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CoreTrait {
    // Equality & comparison
    Eq,
    Comparable,

    // Formatting
    Display,
    Debug,

    // Hashing
    Hash,

    // Value semantics
    Clone,
    Copy,
    Default,
    Drop,

    // Iteration
    Iterable,
    Iterator,

    // Concurrency safety
    Send,
    Sync,

    // Indexing
    Index,

    // Arithmetic operators
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Neg,
}

impl CoreTrait {
    /// The canonical Adam-source name of this trait (e.g. "Eq", "Comparable").
    pub fn name(self) -> &'static str {
        match self {
            CoreTrait::Eq => "Eq",
            CoreTrait::Comparable => "Comparable",
            CoreTrait::Display => "Display",
            CoreTrait::Debug => "Debug",
            CoreTrait::Hash => "Hash",
            CoreTrait::Clone => "Clone",
            CoreTrait::Copy => "Copy",
            CoreTrait::Default => "Default",
            CoreTrait::Drop => "Drop",
            CoreTrait::Iterable => "Iterable",
            CoreTrait::Iterator => "Iterator",
            CoreTrait::Send => "Send",
            CoreTrait::Sync => "Sync",
            CoreTrait::Index => "Index",
            CoreTrait::Add => "Add",
            CoreTrait::Sub => "Sub",
            CoreTrait::Mul => "Mul",
            CoreTrait::Div => "Div",
            CoreTrait::Mod => "Mod",
            CoreTrait::Neg => "Neg",
        }
    }

    /// Return an array of every `CoreTrait` variant, in declaration order.
    pub fn all() -> &'static [CoreTrait] {
        &[
            CoreTrait::Eq,
            CoreTrait::Comparable,
            CoreTrait::Display,
            CoreTrait::Debug,
            CoreTrait::Hash,
            CoreTrait::Clone,
            CoreTrait::Copy,
            CoreTrait::Default,
            CoreTrait::Drop,
            CoreTrait::Iterable,
            CoreTrait::Iterator,
            CoreTrait::Send,
            CoreTrait::Sync,
            CoreTrait::Index,
            CoreTrait::Add,
            CoreTrait::Sub,
            CoreTrait::Mul,
            CoreTrait::Div,
            CoreTrait::Mod,
            CoreTrait::Neg,
        ]
    }
}

impl fmt::Display for CoreTrait {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

// ---------------------------------------------------------------------------
// Ordering — the result type for Comparable::cmp
// ---------------------------------------------------------------------------

/// Result of a three-way comparison, returned by `Comparable::cmp`.
///
/// Uses `repr(i8)` so it can be passed across the FFI boundary to
/// `__adam_*` runtime functions unchanged.
#[repr(i8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Ordering {
    Less = -1,
    Equal = 0,
    Greater = 1,
}

impl fmt::Display for Ordering {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ordering::Less => f.write_str("Less"),
            Ordering::Equal => f.write_str("Equal"),
            Ordering::Greater => f.write_str("Greater"),
        }
    }
}

// ---------------------------------------------------------------------------
// TraitMethod — describes a single method inside a trait
// ---------------------------------------------------------------------------

/// Description of a single method that belongs to a trait.
///
/// Parameter and return types are given as string names (e.g. `"Self"`,
/// `"bool"`, `"Ordering"`) because the compiler's type-checker will resolve
/// them against its own `TypeContext`.  Keeping them as strings here avoids
/// a circular dependency between `adam_std` and `adam_types`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraitMethod {
    /// Method name, e.g. `"eq"`, `"cmp"`, `"add"`.
    pub name: &'static str,
    /// Parameter type names **excluding** the implicit `self` receiver.
    /// For a binary operator like `add(other: Self) -> Self` this is `["Self"]`.
    pub params: &'static [&'static str],
    /// Return type name, e.g. `"bool"`, `"Self"`, `"Ordering"`.
    pub return_type: &'static str,
}

// ---------------------------------------------------------------------------
// CORE_TRAIT_METHODS — canonical method signatures for every core trait
// ---------------------------------------------------------------------------

/// Return the method signatures for a given `CoreTrait`.
///
/// Traits that are *marker traits* (no methods) return an empty slice.
pub fn core_trait_methods(t: CoreTrait) -> &'static [TraitMethod] {
    match t {
        CoreTrait::Eq => &[
            TraitMethod { name: "eq", params: &["Self"], return_type: "bool" },
            TraitMethod { name: "ne", params: &["Self"], return_type: "bool" },
        ],
        CoreTrait::Comparable => &[
            TraitMethod { name: "cmp", params: &["Self"], return_type: "Ordering" },
            TraitMethod { name: "lt", params: &["Self"], return_type: "bool" },
            TraitMethod { name: "le", params: &["Self"], return_type: "bool" },
            TraitMethod { name: "gt", params: &["Self"], return_type: "bool" },
            TraitMethod { name: "ge", params: &["Self"], return_type: "bool" },
        ],
        CoreTrait::Display => &[
            TraitMethod { name: "to_string", params: &[], return_type: "String" },
        ],
        CoreTrait::Debug => &[
            TraitMethod { name: "debug_string", params: &[], return_type: "String" },
        ],
        CoreTrait::Hash => &[
            TraitMethod { name: "hash", params: &[], return_type: "u64" },
        ],
        CoreTrait::Clone => &[
            TraitMethod { name: "clone", params: &[], return_type: "Self" },
        ],
        CoreTrait::Default => &[
            TraitMethod { name: "default", params: &[], return_type: "Self" },
        ],
        CoreTrait::Drop => &[
            TraitMethod { name: "drop", params: &[], return_type: "()" },
        ],
        CoreTrait::Iterable => &[
            TraitMethod { name: "iter", params: &[], return_type: "Iterator" },
        ],
        CoreTrait::Iterator => &[
            TraitMethod { name: "next", params: &[], return_type: "?Self::Item" },
        ],
        CoreTrait::Index => &[
            TraitMethod { name: "index", params: &["Self::Index"], return_type: "Self::Output" },
        ],
        CoreTrait::Add => &[
            TraitMethod { name: "add", params: &["Self"], return_type: "Self" },
        ],
        CoreTrait::Sub => &[
            TraitMethod { name: "sub", params: &["Self"], return_type: "Self" },
        ],
        CoreTrait::Mul => &[
            TraitMethod { name: "mul", params: &["Self"], return_type: "Self" },
        ],
        CoreTrait::Div => &[
            TraitMethod { name: "div", params: &["Self"], return_type: "Self" },
        ],
        CoreTrait::Mod => &[
            TraitMethod { name: "mod_", params: &["Self"], return_type: "Self" },
        ],
        CoreTrait::Neg => &[
            TraitMethod { name: "neg", params: &[], return_type: "Self" },
        ],
        // Marker traits — no methods required
        CoreTrait::Copy => &[],
        CoreTrait::Send => &[],
        CoreTrait::Sync => &[],
    }
}

// =========================================================================
// Tests
// =========================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ordering_repr_values() {
        assert_eq!(Ordering::Less as i8, -1);
        assert_eq!(Ordering::Equal as i8, 0);
        assert_eq!(Ordering::Greater as i8, 1);
    }

    #[test]
    fn ordering_display() {
        assert_eq!(format!("{}", Ordering::Less), "Less");
        assert_eq!(format!("{}", Ordering::Equal), "Equal");
        assert_eq!(format!("{}", Ordering::Greater), "Greater");
    }

    #[test]
    fn core_trait_name_round_trip() {
        for &t in CoreTrait::all() {
            // Display and name() must agree
            assert_eq!(format!("{}", t), t.name());
        }
    }

    #[test]
    fn core_trait_all_contains_every_variant() {
        // We know there are exactly 20 variants.
        assert_eq!(CoreTrait::all().len(), 20);
    }

    #[test]
    fn eq_trait_has_eq_and_ne_methods() {
        let methods = core_trait_methods(CoreTrait::Eq);
        assert_eq!(methods.len(), 2);
        assert_eq!(methods[0].name, "eq");
        assert_eq!(methods[0].return_type, "bool");
        assert_eq!(methods[1].name, "ne");
    }

    #[test]
    fn comparable_trait_has_cmp_method_returning_ordering() {
        let methods = core_trait_methods(CoreTrait::Comparable);
        let cmp = &methods[0];
        assert_eq!(cmp.name, "cmp");
        assert_eq!(cmp.return_type, "Ordering");
        assert_eq!(cmp.params, &["Self"]);
    }

    #[test]
    fn marker_traits_have_no_methods() {
        assert!(core_trait_methods(CoreTrait::Copy).is_empty());
        assert!(core_trait_methods(CoreTrait::Send).is_empty());
        assert!(core_trait_methods(CoreTrait::Sync).is_empty());
    }

    #[test]
    fn add_trait_method_signature() {
        let methods = core_trait_methods(CoreTrait::Add);
        assert_eq!(methods.len(), 1);
        assert_eq!(methods[0].name, "add");
        assert_eq!(methods[0].params, &["Self"]);
        assert_eq!(methods[0].return_type, "Self");
    }

    #[test]
    fn neg_trait_is_unary() {
        let methods = core_trait_methods(CoreTrait::Neg);
        assert_eq!(methods.len(), 1);
        assert_eq!(methods[0].params.len(), 0, "Neg takes no extra params (unary)");
    }

    #[test]
    fn drop_trait_returns_unit() {
        let methods = core_trait_methods(CoreTrait::Drop);
        assert_eq!(methods.len(), 1);
        assert_eq!(methods[0].name, "drop");
        assert_eq!(methods[0].return_type, "()");
    }

    #[test]
    fn ordering_equality() {
        assert_ne!(Ordering::Less, Ordering::Equal);
        assert_ne!(Ordering::Equal, Ordering::Greater);
        assert_eq!(Ordering::Less, Ordering::Less);
    }
}
