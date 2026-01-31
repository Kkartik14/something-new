//! Type AST nodes.

use crate::common::*;
use crate::expr::Expr;

/// Type annotation.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// Named type: `i32`, `String`, `Point`, `Vec[T]`
    Named(TypePath),

    /// Reference: `&T`
    Reference(Box<Spanned<Type>>),

    /// Mutable reference: `&mut T`
    MutReference(Box<Spanned<Type>>),

    /// Array: `[T]` (dynamic), `[T; N]` (fixed)
    Array(Box<ArrayType>),

    /// Tuple: `(T, U, V)`
    Tuple(Vec<Spanned<Type>>),

    /// Optional: `?T`
    Optional(Box<Spanned<Type>>),

    /// Result: `T ! E`
    Result(Box<ResultType>),

    /// Function type: `fn(T, U) -> V`
    Function(Box<FnType>),

    /// Channel: `chan[T]`
    Channel(Box<Spanned<Type>>),

    /// Inferred (no annotation — compiler figures it out).
    Inferred,
}

/// Named type with optional generic arguments.
#[derive(Debug, Clone, PartialEq)]
pub struct TypePath {
    pub name: Ident,
    pub generic_args: Vec<Spanned<Type>>,
}

/// Array type.
#[derive(Debug, Clone, PartialEq)]
pub struct ArrayType {
    pub element: Spanned<Type>,
    /// `None` = dynamic (`[T]`), `Some` = fixed size (`[T; N]`).
    pub size: Option<Spanned<Expr>>,
}

/// Result type: `T ! E`.
#[derive(Debug, Clone, PartialEq)]
pub struct ResultType {
    pub ok: Spanned<Type>,
    pub err: Spanned<Type>,
}

/// Function type: `fn(params) -> return_type`.
#[derive(Debug, Clone, PartialEq)]
pub struct FnType {
    pub params: Vec<Spanned<Type>>,
    pub return_type: Spanned<Type>,
}

/// Generic parameter: `T`, `T: Comparable`, `T: Comparable + Drawable`.
#[derive(Debug, Clone, PartialEq)]
pub struct GenericParam {
    pub name: Ident,
    pub bounds: Vec<Ident>,
}
