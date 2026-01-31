//! Expression AST nodes.

use crate::common::*;
use crate::pattern::Pattern;
use crate::stmt::Block;
use crate::types::Type;

/// Expression node.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    // Literals
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    StringInterpolation(Vec<StringPart>),
    CharLiteral(char),
    BoolLiteral(bool),
    NilLiteral,

    // Identifiers and paths
    Identifier(Ident),
    Path(Vec<Ident>),

    // Operators
    Binary(Box<BinaryExpr>),
    Unary(Box<UnaryExpr>),

    // Function call
    Call(Box<CallExpr>),

    // Method call: expr.method(args)
    MethodCall(Box<MethodCallExpr>),

    // Field access: expr.field
    FieldAccess(Box<FieldAccessExpr>),

    // Index: expr[index]
    Index(Box<IndexExpr>),

    // Struct literal: Point { x: 1, y: 2 }
    StructLiteral(Box<StructLiteralExpr>),

    // Array literal: [1, 2, 3]
    ArrayLiteral(Vec<Spanned<Expr>>),

    // Tuple literal: (1, "hello")
    TupleLiteral(Vec<Spanned<Expr>>),

    // Block expression: { stmts; expr }
    Block(Block),

    // If expression
    If(Box<IfExpr>),

    // Match expression
    Match(Box<MatchExpr>),

    // Closure: |a, b| { a + b }
    Closure(Box<ClosureExpr>),

    // Assignment: x = 5
    Assign(Box<AssignExpr>),

    // Range: 1..10
    Range(Box<RangeExpr>),

    // Try: expr?
    Try(Box<Spanned<Expr>>),

    // Spawn: spawn { ... }
    Spawn(Block),

    // Channel create: chan[T]() or chan[T](cap)
    ChanCreate(Box<ChanCreateExpr>),

    // Select expression
    Select(Box<SelectExpr>),

    // Return
    Return(Option<Box<Spanned<Expr>>>),
    Break(Option<Box<Spanned<Expr>>>),
    Continue,
}

/// String interpolation part.
#[derive(Debug, Clone, PartialEq)]
pub enum StringPart {
    Literal(String),
    Interpolation(Spanned<Expr>),
}

/// Binary expression: `left op right`
#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    pub left: Spanned<Expr>,
    pub op: BinaryOp,
    pub right: Spanned<Expr>,
}

/// Binary operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    NotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    And,
    Or,
}

/// Unary expression: `op operand`
#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub operand: Spanned<Expr>,
}

/// Unary operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
    Ref,
}

/// Function call: `callee(args)`
#[derive(Debug, Clone, PartialEq)]
pub struct CallExpr {
    pub callee: Spanned<Expr>,
    pub generic_args: Vec<Spanned<Type>>,
    pub args: Vec<Spanned<Expr>>,
}

/// Method call: `receiver.method(args)`
#[derive(Debug, Clone, PartialEq)]
pub struct MethodCallExpr {
    pub receiver: Spanned<Expr>,
    pub method: Ident,
    pub generic_args: Vec<Spanned<Type>>,
    pub args: Vec<Spanned<Expr>>,
}

/// Field access: `object.field`
#[derive(Debug, Clone, PartialEq)]
pub struct FieldAccessExpr {
    pub object: Spanned<Expr>,
    pub field: Ident,
}

/// Index expression: `object[index]`
#[derive(Debug, Clone, PartialEq)]
pub struct IndexExpr {
    pub object: Spanned<Expr>,
    pub index: Spanned<Expr>,
}

/// Struct literal: `Name { field: value, ... }`
#[derive(Debug, Clone, PartialEq)]
pub struct StructLiteralExpr {
    pub name: Ident,
    pub fields: Vec<(Ident, Spanned<Expr>)>,
}

/// If expression.
#[derive(Debug, Clone, PartialEq)]
pub struct IfExpr {
    pub condition: Spanned<Expr>,
    pub then_block: Block,
    pub else_block: Option<ElseBlock>,
}

/// Else branch — either `else { }` or `else if { }`.
#[derive(Debug, Clone, PartialEq)]
pub enum ElseBlock {
    Else(Block),
    ElseIf(Box<Spanned<IfExpr>>),
}

/// Match expression.
#[derive(Debug, Clone, PartialEq)]
pub struct MatchExpr {
    pub scrutinee: Spanned<Expr>,
    pub arms: Vec<MatchArm>,
}

/// Match arm.
#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Spanned<Pattern>,
    pub guard: Option<Spanned<Expr>>,
    pub body: Spanned<Expr>,
}

/// Closure: `|params| body`
#[derive(Debug, Clone, PartialEq)]
pub struct ClosureExpr {
    pub params: Vec<ClosureParam>,
    pub body: Spanned<Expr>,
}

/// Closure parameter.
#[derive(Debug, Clone, PartialEq)]
pub struct ClosureParam {
    pub name: Ident,
    pub ty: Option<Spanned<Type>>,
}

/// Assignment: `target = value`
#[derive(Debug, Clone, PartialEq)]
pub struct AssignExpr {
    pub target: Spanned<Expr>,
    pub value: Spanned<Expr>,
}

/// Range: `start..end`
#[derive(Debug, Clone, PartialEq)]
pub struct RangeExpr {
    pub start: Spanned<Expr>,
    pub end: Spanned<Expr>,
    pub inclusive: bool,
}

/// Channel create: `chan[T]()` or `chan[T](capacity)`
#[derive(Debug, Clone, PartialEq)]
pub struct ChanCreateExpr {
    pub element_type: Spanned<Type>,
    pub capacity: Option<Spanned<Expr>>,
}

/// Select expression.
#[derive(Debug, Clone, PartialEq)]
pub struct SelectExpr {
    pub arms: Vec<SelectArm>,
}

/// Select arm.
#[derive(Debug, Clone, PartialEq)]
pub struct SelectArm {
    pub kind: SelectArmKind,
    pub body: Block,
}

/// Select arm kind.
#[derive(Debug, Clone, PartialEq)]
pub enum SelectArmKind {
    Recv {
        binding: Ident,
        channel: Spanned<Expr>,
    },
    Send {
        channel: Spanned<Expr>,
        value: Spanned<Expr>,
    },
    After(Spanned<Expr>),
}
