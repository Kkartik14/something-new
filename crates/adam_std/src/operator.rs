//! Mapping from Adam operator tokens to the core trait that must be
//! implemented in order for the operator to be valid on a given type.
//!
//! The compiler's type checker uses [`op_to_trait`] (and the [`OpKind`] enum)
//! when it encounters a binary or unary expression and needs to verify that
//! the operand type supports the operation.

use crate::traits::CoreTrait;
use std::fmt;

/// Every operator that maps to a core trait.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OpKind {
    /// `+`  (binary addition / string concatenation)
    Add,
    /// `-`  (binary subtraction)
    Sub,
    /// `*`  (multiplication)
    Mul,
    /// `/`  (division)
    Div,
    /// `%`  (modulo / remainder)
    Mod,
    /// `-`  (unary negation)
    Neg,
    /// `==` (equality)
    EqEq,
    /// `!=` (inequality)
    NotEq,
    /// `<`
    Lt,
    /// `>`
    Gt,
    /// `<=`
    Le,
    /// `>=`
    Ge,
    /// `[]` (index)
    Index,
}

impl OpKind {
    /// The source-level token(s) for this operator.
    pub fn symbol(self) -> &'static str {
        match self {
            OpKind::Add => "+",
            OpKind::Sub => "-",
            OpKind::Mul => "*",
            OpKind::Div => "/",
            OpKind::Mod => "%",
            OpKind::Neg => "-",
            OpKind::EqEq => "==",
            OpKind::NotEq => "!=",
            OpKind::Lt => "<",
            OpKind::Gt => ">",
            OpKind::Le => "<=",
            OpKind::Ge => ">=",
            OpKind::Index => "[]",
        }
    }
}

impl fmt::Display for OpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.symbol())
    }
}

/// Return the [`CoreTrait`] that must be implemented for `op` to be valid.
///
/// # Examples
/// ```ignore
/// assert_eq!(op_to_trait(OpKind::Add), CoreTrait::Add);
/// assert_eq!(op_to_trait(OpKind::EqEq), CoreTrait::Eq);
/// ```
pub fn op_to_trait(op: OpKind) -> CoreTrait {
    match op {
        OpKind::Add => CoreTrait::Add,
        OpKind::Sub => CoreTrait::Sub,
        OpKind::Mul => CoreTrait::Mul,
        OpKind::Div => CoreTrait::Div,
        OpKind::Mod => CoreTrait::Mod,
        OpKind::Neg => CoreTrait::Neg,
        OpKind::EqEq | OpKind::NotEq => CoreTrait::Eq,
        OpKind::Lt | OpKind::Gt | OpKind::Le | OpKind::Ge => CoreTrait::Comparable,
        OpKind::Index => CoreTrait::Index,
    }
}

/// Try to parse a source-level operator token into an [`OpKind`].
///
/// Returns `None` for tokens that do not map to a trait-backed operator
/// (e.g. `&&`, `||`, `!`).
pub fn parse_op(token: &str) -> Option<OpKind> {
    match token {
        "+" => Some(OpKind::Add),
        "-" => Some(OpKind::Sub),
        "*" => Some(OpKind::Mul),
        "/" => Some(OpKind::Div),
        "%" => Some(OpKind::Mod),
        "==" => Some(OpKind::EqEq),
        "!=" => Some(OpKind::NotEq),
        "<" => Some(OpKind::Lt),
        ">" => Some(OpKind::Gt),
        "<=" => Some(OpKind::Le),
        ">=" => Some(OpKind::Ge),
        "[]" => Some(OpKind::Index),
        _ => None,
    }
}

// =========================================================================
// Tests
// =========================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // -- op_to_trait ---------------------------------------------------------

    #[test]
    fn add_maps_to_add_trait() {
        assert_eq!(op_to_trait(OpKind::Add), CoreTrait::Add);
    }

    #[test]
    fn sub_maps_to_sub_trait() {
        assert_eq!(op_to_trait(OpKind::Sub), CoreTrait::Sub);
    }

    #[test]
    fn mul_maps_to_mul_trait() {
        assert_eq!(op_to_trait(OpKind::Mul), CoreTrait::Mul);
    }

    #[test]
    fn div_maps_to_div_trait() {
        assert_eq!(op_to_trait(OpKind::Div), CoreTrait::Div);
    }

    #[test]
    fn mod_maps_to_mod_trait() {
        assert_eq!(op_to_trait(OpKind::Mod), CoreTrait::Mod);
    }

    #[test]
    fn neg_maps_to_neg_trait() {
        assert_eq!(op_to_trait(OpKind::Neg), CoreTrait::Neg);
    }

    #[test]
    fn eq_eq_maps_to_eq_trait() {
        assert_eq!(op_to_trait(OpKind::EqEq), CoreTrait::Eq);
    }

    #[test]
    fn not_eq_maps_to_eq_trait() {
        assert_eq!(op_to_trait(OpKind::NotEq), CoreTrait::Eq);
    }

    #[test]
    fn comparison_operators_map_to_comparable() {
        assert_eq!(op_to_trait(OpKind::Lt), CoreTrait::Comparable);
        assert_eq!(op_to_trait(OpKind::Gt), CoreTrait::Comparable);
        assert_eq!(op_to_trait(OpKind::Le), CoreTrait::Comparable);
        assert_eq!(op_to_trait(OpKind::Ge), CoreTrait::Comparable);
    }

    #[test]
    fn index_maps_to_index_trait() {
        assert_eq!(op_to_trait(OpKind::Index), CoreTrait::Index);
    }

    // -- parse_op ------------------------------------------------------------

    #[test]
    fn parse_arithmetic_operators() {
        assert_eq!(parse_op("+"), Some(OpKind::Add));
        assert_eq!(parse_op("-"), Some(OpKind::Sub));
        assert_eq!(parse_op("*"), Some(OpKind::Mul));
        assert_eq!(parse_op("/"), Some(OpKind::Div));
        assert_eq!(parse_op("%"), Some(OpKind::Mod));
    }

    #[test]
    fn parse_comparison_operators() {
        assert_eq!(parse_op("=="), Some(OpKind::EqEq));
        assert_eq!(parse_op("!="), Some(OpKind::NotEq));
        assert_eq!(parse_op("<"), Some(OpKind::Lt));
        assert_eq!(parse_op(">"), Some(OpKind::Gt));
        assert_eq!(parse_op("<="), Some(OpKind::Le));
        assert_eq!(parse_op(">="), Some(OpKind::Ge));
    }

    #[test]
    fn parse_index_operator() {
        assert_eq!(parse_op("[]"), Some(OpKind::Index));
    }

    #[test]
    fn parse_unknown_operator_returns_none() {
        assert_eq!(parse_op("&&"), None);
        assert_eq!(parse_op("||"), None);
        assert_eq!(parse_op("!"), None);
        assert_eq!(parse_op("??"), None);
    }

    #[test]
    fn op_kind_display() {
        assert_eq!(format!("{}", OpKind::Add), "+");
        assert_eq!(format!("{}", OpKind::EqEq), "==");
        assert_eq!(format!("{}", OpKind::Le), "<=");
        assert_eq!(format!("{}", OpKind::Index), "[]");
    }

    // -- round-trip: parse -> symbol -----------------------------------------

    #[test]
    fn parse_then_symbol_round_trip() {
        let tokens = ["+", "-", "*", "/", "%", "==", "!=", "<", ">", "<=", ">=", "[]"];
        for tok in tokens {
            let op = parse_op(tok).unwrap_or_else(|| panic!("failed to parse '{}'", tok));
            assert_eq!(op.symbol(), tok, "symbol() should return the original token");
        }
    }
}
