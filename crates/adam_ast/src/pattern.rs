//! Pattern AST nodes — used in match arms, let bindings, for loops.

use crate::common::*;
use crate::expr::Expr;

/// Pattern.
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    /// Wildcard: `_`
    Wildcard,

    /// Binding: `x` (binds matched value to name)
    Binding(Ident),

    /// Literal: `5`, `"hello"`, `true`
    Literal(Spanned<Expr>),

    /// Enum variant: `Circle(r)`, `Some(val)`
    Variant(VariantPattern),

    /// Struct: `Point { x, y }`
    Struct(StructPattern),

    /// Tuple: `(a, b, c)`
    Tuple(Vec<Spanned<Pattern>>),

    /// Or pattern: `A | B`
    Or(Vec<Spanned<Pattern>>),

    /// Rest: `..` (in arrays/tuples)
    Rest,
}

/// Enum variant pattern.
#[derive(Debug, Clone, PartialEq)]
pub struct VariantPattern {
    pub name: Ident,
    pub fields: Vec<Spanned<Pattern>>,
}

/// Struct pattern.
#[derive(Debug, Clone, PartialEq)]
pub struct StructPattern {
    pub name: Ident,
    pub fields: Vec<(Ident, Spanned<Pattern>)>,
    pub has_rest: bool,
}
