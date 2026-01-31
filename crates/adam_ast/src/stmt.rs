//! Statement AST nodes.

use crate::common::*;
use crate::expr::Expr;
use crate::item::Item;
use crate::types::Type;

/// A block of statements.
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<Spanned<Stmt>>,
}

/// Statement.
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    /// Variable binding: `x := 5`, `mut y := 10`, `name: String = "adam"`
    Let(LetStmt),

    /// Expression as statement.
    Expr(Spanned<Expr>),

    /// For loop: `for item in collection { ... }`
    For(ForStmt),

    /// While loop: `while condition { ... }`
    While(WhileStmt),

    /// Infinite loop: `loop { ... }`
    Loop(Block),

    /// Item declaration inside a block.
    Item(Spanned<Item>),
}

/// Let binding.
#[derive(Debug, Clone, PartialEq)]
pub struct LetStmt {
    pub mutability: Mutability,
    pub name: Ident,
    pub ty: Option<Spanned<Type>>,
    pub value: Spanned<Expr>,
    /// `true` for `:=` syntax, `false` for `: Type =` syntax.
    pub is_colon_assign: bool,
}

/// For-in loop.
#[derive(Debug, Clone, PartialEq)]
pub struct ForStmt {
    pub binding: Ident,
    pub iterable: Spanned<Expr>,
    pub body: Block,
}

/// While loop.
#[derive(Debug, Clone, PartialEq)]
pub struct WhileStmt {
    pub condition: Spanned<Expr>,
    pub body: Block,
}
