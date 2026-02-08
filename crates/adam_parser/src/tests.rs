//! Parser test suite — comprehensive tests for every AST node type.

use crate::parser::Parser;
use adam_ast::common::*;
use adam_ast::expr::*;
use adam_ast::item::*;
use adam_ast::pattern::*;
use adam_ast::stmt::*;
use adam_ast::types::*;
use adam_lexer::Lexer;

/// Helper: lex and parse source code, assert no errors.
fn parse_ok(src: &str) -> SourceFile {
    let lex = Lexer::new(src);
    let result = lex.tokenize();
    assert!(result.errors.is_empty(), "Lex errors: {:?}", result.errors);
    let parser = Parser::new(result.tokens);
    let result = parser.parse();
    assert!(
        result.errors.is_empty(),
        "Parse errors for input:\n{}\n\nErrors: {:?}",
        src,
        result.errors
    );
    result.ast
}

/// Helper: lex and parse, expect parse errors.
fn parse_with_errors(src: &str) -> (SourceFile, Vec<crate::parser::ParseError>) {
    let lex = Lexer::new(src);
    let result = lex.tokenize();
    let parser = Parser::new(result.tokens);
    let result = parser.parse();
    (result.ast, result.errors)
}

/// Helper: get the first (and usually only) item.
fn first_item(ast: &SourceFile) -> &Item {
    &ast.items[0].node
}

/// Helper: extract FnDef from Item.
fn as_fn(item: &Item) -> &FnDef {
    match item {
        Item::Function(f) => f,
        _ => panic!("expected function, got {:?}", item),
    }
}

/// Helper: extract first statement of a function's body.
fn first_stmt_of_fn(item: &Item) -> &Stmt {
    let f = as_fn(item);
    &f.body.as_ref().unwrap().stmts[0].node
}

/// Helper: extract expression from Stmt::Expr.
fn as_expr_stmt(stmt: &Stmt) -> &Expr {
    match stmt {
        Stmt::Expr(e) => &e.node,
        _ => panic!("expected expression statement, got {:?}", stmt),
    }
}

// ============================================================
// Empty / Trivial
// ============================================================

#[test]
fn test_parse_empty_file() {
    let ast = parse_ok("");
    assert!(ast.items.is_empty());
}

#[test]
fn test_parse_only_newlines() {
    let ast = parse_ok("\n\n\n");
    assert!(ast.items.is_empty());
}

#[test]
fn test_parse_only_comments() {
    let ast = parse_ok("// a comment\n/* block */\n");
    assert!(ast.items.is_empty());
}

// ============================================================
// Functions
// ============================================================

#[test]
fn test_parse_fn_basic() {
    let ast = parse_ok("fn foo() { }");
    let f = as_fn(first_item(&ast));
    assert_eq!(f.name.name, "foo");
    assert!(f.params.is_empty());
    assert!(f.return_type.is_none());
    assert!(f.body.is_some());
    assert_eq!(f.visibility, Visibility::Private);
}

#[test]
fn test_parse_fn_pub() {
    let ast = parse_ok("pub fn bar() { }");
    let f = as_fn(first_item(&ast));
    assert_eq!(f.visibility, Visibility::Public);
    assert_eq!(f.name.name, "bar");
}

#[test]
fn test_parse_fn_with_params() {
    let ast = parse_ok("fn add(x i32, y i32) -> i32 { x }");
    let f = as_fn(first_item(&ast));
    assert_eq!(f.params.len(), 2);
    assert_eq!(f.params[0].name.name, "x");
    assert_eq!(f.params[0].ownership, ParamOwnership::Borrow);
    assert_eq!(f.params[1].name.name, "y");
    assert!(f.return_type.is_some());
}

#[test]
fn test_parse_fn_ownership_params() {
    let ast = parse_ok("fn consume(own name String) { }");
    let f = as_fn(first_item(&ast));
    assert_eq!(f.params[0].ownership, ParamOwnership::Own);
    assert_eq!(f.params[0].name.name, "name");
}

#[test]
fn test_parse_fn_mut_param() {
    let ast = parse_ok("fn modify(mut val String) { }");
    let f = as_fn(first_item(&ast));
    assert_eq!(f.params[0].ownership, ParamOwnership::MutBorrow);
}

#[test]
fn test_parse_fn_self_param() {
    let ast = parse_ok("fn method(self) { }");
    let f = as_fn(first_item(&ast));
    assert_eq!(f.params[0].ownership, ParamOwnership::SelfBorrow);
    assert_eq!(f.params[0].name.name, "self");
}

#[test]
fn test_parse_fn_mut_self() {
    let ast = parse_ok("fn method(mut self) { }");
    let f = as_fn(first_item(&ast));
    assert_eq!(f.params[0].ownership, ParamOwnership::SelfMutBorrow);
}

#[test]
fn test_parse_fn_own_self() {
    let ast = parse_ok("fn method(own self) { }");
    let f = as_fn(first_item(&ast));
    assert_eq!(f.params[0].ownership, ParamOwnership::SelfOwn);
}

#[test]
fn test_parse_fn_with_generics() {
    let ast = parse_ok("fn max[T: Comparable](a T, b T) -> T { a }");
    let f = as_fn(first_item(&ast));
    assert_eq!(f.generic_params.len(), 1);
    assert_eq!(f.generic_params[0].name.name, "T");
    assert_eq!(f.generic_params[0].bounds.len(), 1);
    assert_eq!(f.generic_params[0].bounds[0].name, "Comparable");
}

#[test]
fn test_parse_fn_no_body() {
    // Trait method signature
    let ast = parse_ok("trait Foo {\n    fn bar(self) -> i32\n}");
    if let Item::Trait(t) = first_item(&ast) {
        assert!(t.methods[0].node.body.is_none());
    } else {
        panic!("expected trait");
    }
}

// ============================================================
// Structs
// ============================================================

#[test]
fn test_parse_struct_basic() {
    let ast = parse_ok("struct Point {\n    x f64\n    y f64\n}");
    if let Item::Struct(s) = first_item(&ast) {
        assert_eq!(s.name.name, "Point");
        assert_eq!(s.fields.len(), 2);
        assert_eq!(s.fields[0].name.name, "x");
        assert_eq!(s.fields[1].name.name, "y");
    } else {
        panic!("expected struct");
    }
}

#[test]
fn test_parse_struct_pub_fields() {
    let ast = parse_ok("struct Config {\n    pub host String\n    pub port i32\n}");
    if let Item::Struct(s) = first_item(&ast) {
        assert_eq!(s.fields[0].visibility, Visibility::Public);
        assert_eq!(s.fields[1].visibility, Visibility::Public);
    } else {
        panic!("expected struct");
    }
}

#[test]
fn test_parse_struct_with_generics() {
    let ast = parse_ok("struct Pair[T, U] {\n    first T\n    second U\n}");
    if let Item::Struct(s) = first_item(&ast) {
        assert_eq!(s.generic_params.len(), 2);
        assert_eq!(s.generic_params[0].name.name, "T");
        assert_eq!(s.generic_params[1].name.name, "U");
    } else {
        panic!("expected struct");
    }
}

#[test]
fn test_parse_struct_empty() {
    let ast = parse_ok("struct Empty { }");
    if let Item::Struct(s) = first_item(&ast) {
        assert!(s.fields.is_empty());
    } else {
        panic!("expected struct");
    }
}

// ============================================================
// Enums
// ============================================================

#[test]
fn test_parse_enum_basic() {
    let ast = parse_ok("enum Shape {\n    Circle(f64)\n    Rect(f64, f64)\n}");
    if let Item::Enum(e) = first_item(&ast) {
        assert_eq!(e.name.name, "Shape");
        assert_eq!(e.variants.len(), 2);
        assert_eq!(e.variants[0].name.name, "Circle");
        assert_eq!(e.variants[0].fields.len(), 1);
        assert_eq!(e.variants[1].name.name, "Rect");
        assert_eq!(e.variants[1].fields.len(), 2);
    } else {
        panic!("expected enum");
    }
}

#[test]
fn test_parse_enum_no_data() {
    let ast = parse_ok("enum Color {\n    Red\n    Green\n    Blue\n}");
    if let Item::Enum(e) = first_item(&ast) {
        assert_eq!(e.variants.len(), 3);
        assert!(e.variants[0].fields.is_empty());
    } else {
        panic!("expected enum");
    }
}

#[test]
fn test_parse_enum_with_generics() {
    let ast = parse_ok("enum Option[T] {\n    Some(T)\n    None\n}");
    if let Item::Enum(e) = first_item(&ast) {
        assert_eq!(e.generic_params.len(), 1);
        assert_eq!(e.variants.len(), 2);
    } else {
        panic!("expected enum");
    }
}

// ============================================================
// Traits
// ============================================================

#[test]
fn test_parse_trait_basic() {
    let ast = parse_ok("trait Display {\n    fn to_string(self) -> String\n}");
    if let Item::Trait(t) = first_item(&ast) {
        assert_eq!(t.name.name, "Display");
        assert_eq!(t.methods.len(), 1);
        assert_eq!(t.methods[0].node.name.name, "to_string");
        assert!(t.methods[0].node.body.is_none());
    } else {
        panic!("expected trait");
    }
}

#[test]
fn test_parse_trait_with_default() {
    let ast =
        parse_ok("trait Greet {\n    fn hello(self) -> String {\n        \"hello\"\n    }\n}");
    if let Item::Trait(t) = first_item(&ast) {
        assert!(t.methods[0].node.body.is_some());
    } else {
        panic!("expected trait");
    }
}

// ============================================================
// Impl blocks
// ============================================================

#[test]
fn test_parse_impl_inherent() {
    let ast = parse_ok(
        "impl Point {\n    fn new(x f64, y f64) -> Point {\n        Point { x, y }\n    }\n}",
    );
    if let Item::Impl(imp) = first_item(&ast) {
        assert!(imp.trait_name.is_none());
        assert_eq!(imp.methods.len(), 1);
        assert_eq!(imp.methods[0].node.name.name, "new");
    } else {
        panic!("expected impl");
    }
}

#[test]
fn test_parse_impl_trait() {
    let ast = parse_ok(
        "impl Display for Point {\n    fn to_string(self) -> String {\n        \"point\"\n    }\n}",
    );
    if let Item::Impl(imp) = first_item(&ast) {
        assert_eq!(imp.trait_name.as_ref().unwrap().name, "Display");
    } else {
        panic!("expected impl");
    }
}

// ============================================================
// Views
// ============================================================

#[test]
fn test_parse_view_basic() {
    let src = "view Counter {\n    @state count: i32 = 0\n\n    body {\n        count\n    }\n}";
    let ast = parse_ok(src);
    if let Item::View(v) = first_item(&ast) {
        assert_eq!(v.name.name, "Counter");
        assert_eq!(v.fields.len(), 1);
        assert_eq!(v.fields[0].attribute, ViewFieldAttr::State);
        assert_eq!(v.fields[0].name.name, "count");
        assert!(v.fields[0].default.is_some());
    } else {
        panic!("expected view");
    }
}

#[test]
fn test_parse_view_with_prop() {
    let src = "view Greeting {\n    @prop name: String\n\n    body {\n        name\n    }\n}";
    let ast = parse_ok(src);
    if let Item::View(v) = first_item(&ast) {
        assert_eq!(v.fields[0].attribute, ViewFieldAttr::Prop);
        assert!(v.fields[0].default.is_none());
    } else {
        panic!("expected view");
    }
}

// ============================================================
// Use and Mod
// ============================================================

#[test]
fn test_parse_use_single() {
    let ast = parse_ok("use std.io");
    if let Item::Use(u) = first_item(&ast) {
        assert_eq!(u.path.len(), 2);
        assert_eq!(u.path[0].name, "std");
        assert_eq!(u.path[1].name, "io");
        assert_eq!(u.items, UseItems::Single);
    } else {
        panic!("expected use");
    }
}

#[test]
fn test_parse_use_wildcard() {
    let ast = parse_ok("use std.io.*");
    if let Item::Use(u) = first_item(&ast) {
        assert_eq!(u.items, UseItems::All);
    } else {
        panic!("expected use");
    }
}

#[test]
fn test_parse_use_multiple() {
    let ast = parse_ok("use std.{io, fs}");
    if let Item::Use(u) = first_item(&ast) {
        if let UseItems::Multiple(names) = &u.items {
            assert_eq!(names.len(), 2);
            assert_eq!(names[0].name, "io");
            assert_eq!(names[1].name, "fs");
        } else {
            panic!("expected multiple");
        }
    } else {
        panic!("expected use");
    }
}

#[test]
fn test_parse_mod() {
    let ast = parse_ok("pub mod networking");
    if let Item::Mod(m) = first_item(&ast) {
        assert_eq!(m.name.name, "networking");
        assert_eq!(m.visibility, Visibility::Public);
    } else {
        panic!("expected mod");
    }
}

// ============================================================
// Let bindings
// ============================================================

#[test]
fn test_parse_let_colon_assign() {
    let ast = parse_ok("fn main() {\n    x := 5\n}");
    let stmt = first_stmt_of_fn(first_item(&ast));
    if let Stmt::Let(l) = stmt {
        assert_eq!(l.name.name, "x");
        assert_eq!(l.mutability, Mutability::Immutable);
        assert!(l.is_colon_assign);
        assert!(l.ty.is_none());
        if let Expr::IntLiteral(5) = &l.value.node {
        } else {
            panic!("expected 5");
        }
    } else {
        panic!("expected let");
    }
}

#[test]
fn test_parse_let_mutable() {
    let ast = parse_ok("fn main() {\n    mut x := 10\n}");
    let stmt = first_stmt_of_fn(first_item(&ast));
    if let Stmt::Let(l) = stmt {
        assert_eq!(l.mutability, Mutability::Mutable);
    } else {
        panic!("expected let");
    }
}

#[test]
fn test_parse_let_typed() {
    let ast = parse_ok("fn main() {\n    x: i32 = 42\n}");
    let stmt = first_stmt_of_fn(first_item(&ast));
    if let Stmt::Let(l) = stmt {
        assert!(!l.is_colon_assign);
        assert!(l.ty.is_some());
        if let Type::Named(path) = &l.ty.as_ref().unwrap().node {
            assert_eq!(path.name.name, "i32");
        } else {
            panic!("expected named type");
        }
    } else {
        panic!("expected let");
    }
}

#[test]
fn test_parse_let_with_keyword() {
    let ast = parse_ok("fn main() {\n    let x := 5\n}");
    let stmt = first_stmt_of_fn(first_item(&ast));
    if let Stmt::Let(l) = stmt {
        assert_eq!(l.name.name, "x");
    } else {
        panic!("expected let");
    }
}

#[test]
fn test_parse_let_mut_typed() {
    let ast = parse_ok("fn main() {\n    mut name: String = \"adam\"\n}");
    let stmt = first_stmt_of_fn(first_item(&ast));
    if let Stmt::Let(l) = stmt {
        assert_eq!(l.mutability, Mutability::Mutable);
        assert!(l.ty.is_some());
    } else {
        panic!("expected let");
    }
}

// ============================================================
// Binary operators and precedence
// ============================================================

#[test]
fn test_parse_binary_add() {
    let ast = parse_ok("fn f() { 1 + 2 }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::Binary(b) = expr {
        assert_eq!(b.op, BinaryOp::Add);
    } else {
        panic!("expected binary");
    }
}

#[test]
fn test_parse_precedence_mul_over_add() {
    // 1 + 2 * 3 should be 1 + (2 * 3)
    let ast = parse_ok("fn f() { 1 + 2 * 3 }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::Binary(b) = expr {
        assert_eq!(b.op, BinaryOp::Add);
        // right should be 2 * 3
        if let Expr::Binary(r) = &b.right.node {
            assert_eq!(r.op, BinaryOp::Mul);
        } else {
            panic!("expected right to be binary mul");
        }
    } else {
        panic!("expected binary");
    }
}

#[test]
fn test_parse_left_associativity() {
    // a - b - c should be (a - b) - c
    let ast = parse_ok("fn f() { 1 - 2 - 3 }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::Binary(b) = expr {
        assert_eq!(b.op, BinaryOp::Sub);
        // left should be 1 - 2
        if let Expr::Binary(l) = &b.left.node {
            assert_eq!(l.op, BinaryOp::Sub);
        } else {
            panic!("expected left to be binary sub");
        }
    } else {
        panic!("expected binary");
    }
}

#[test]
fn test_parse_comparison_operators() {
    let ast = parse_ok("fn f() { x > 0 }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::Binary(b) = expr {
        assert_eq!(b.op, BinaryOp::Gt);
    } else {
        panic!("expected binary");
    }
}

#[test]
fn test_parse_logical_operators() {
    // a && b || c should be (a && b) || c
    let ast = parse_ok("fn f() { a && b || c }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::Binary(b) = expr {
        assert_eq!(b.op, BinaryOp::Or);
        if let Expr::Binary(l) = &b.left.node {
            assert_eq!(l.op, BinaryOp::And);
        } else {
            panic!("expected left to be &&");
        }
    } else {
        panic!("expected binary");
    }
}

#[test]
fn test_parse_equality() {
    let ast = parse_ok("fn f() { x == y }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::Binary(b) = expr {
        assert_eq!(b.op, BinaryOp::Eq);
    } else {
        panic!("expected binary");
    }
}

#[test]
fn test_parse_all_binary_ops() {
    let ops = vec![
        ("+", BinaryOp::Add),
        ("-", BinaryOp::Sub),
        ("*", BinaryOp::Mul),
        ("/", BinaryOp::Div),
        ("%", BinaryOp::Mod),
        ("==", BinaryOp::Eq),
        ("!=", BinaryOp::NotEq),
        ("<", BinaryOp::Lt),
        (">", BinaryOp::Gt),
        ("<=", BinaryOp::LtEq),
        (">=", BinaryOp::GtEq),
        ("&&", BinaryOp::And),
        ("||", BinaryOp::Or),
    ];
    for (sym, expected_op) in ops {
        let src = format!("fn f() {{ a {} b }}", sym);
        let ast = parse_ok(&src);
        let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
        if let Expr::Binary(b) = expr {
            assert_eq!(b.op, expected_op, "failed for operator {}", sym);
        } else {
            panic!("expected binary for operator {}", sym);
        }
    }
}

// ============================================================
// Unary operators
// ============================================================

#[test]
fn test_parse_unary_neg() {
    let ast = parse_ok("fn f() { -x }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::Unary(u) = expr {
        assert_eq!(u.op, UnaryOp::Neg);
    } else {
        panic!("expected unary");
    }
}

#[test]
fn test_parse_unary_not() {
    let ast = parse_ok("fn f() { !flag }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::Unary(u) = expr {
        assert_eq!(u.op, UnaryOp::Not);
    } else {
        panic!("expected unary");
    }
}

#[test]
fn test_parse_unary_ref() {
    let ast = parse_ok("fn f() { &x }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::Unary(u) = expr {
        assert_eq!(u.op, UnaryOp::Ref);
    } else {
        panic!("expected unary");
    }
}

#[test]
fn test_parse_unary_binds_tighter_than_binary() {
    // !a && b should be (!a) && b
    let ast = parse_ok("fn f() { !a && b }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::Binary(b) = expr {
        assert_eq!(b.op, BinaryOp::And);
        if let Expr::Unary(u) = &b.left.node {
            assert_eq!(u.op, UnaryOp::Not);
        } else {
            panic!("expected left to be unary not");
        }
    } else {
        panic!("expected binary");
    }
}

// ============================================================
// Function calls and method calls
// ============================================================

#[test]
fn test_parse_call() {
    let ast = parse_ok("fn f() { print(\"hello\") }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::Call(c) = expr {
        assert_eq!(c.args.len(), 1);
        if let Expr::Identifier(name) = &c.callee.node {
            assert_eq!(name.name, "print");
        } else {
            panic!("expected identifier callee");
        }
    } else {
        panic!("expected call");
    }
}

#[test]
fn test_parse_call_multiple_args() {
    let ast = parse_ok("fn f() { add(1, 2, 3) }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::Call(c) = expr {
        assert_eq!(c.args.len(), 3);
    } else {
        panic!("expected call");
    }
}

#[test]
fn test_parse_method_call() {
    let ast = parse_ok("fn f() { obj.method(42) }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::MethodCall(m) = expr {
        assert_eq!(m.method.name, "method");
        assert_eq!(m.args.len(), 1);
    } else {
        panic!("expected method call");
    }
}

#[test]
fn test_parse_method_chain() {
    // a.b().c()
    let ast = parse_ok("fn f() { a.b().c() }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::MethodCall(m) = expr {
        assert_eq!(m.method.name, "c");
        if let Expr::MethodCall(inner) = &m.receiver.node {
            assert_eq!(inner.method.name, "b");
        } else {
            panic!("expected inner method call");
        }
    } else {
        panic!("expected method call");
    }
}

#[test]
fn test_parse_chained_call() {
    // f()()
    let ast = parse_ok("fn f() { g()() }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::Call(c) = expr {
        if let Expr::Call(_) = &c.callee.node {
            // OK
        } else {
            panic!("expected call of call");
        }
    } else {
        panic!("expected call");
    }
}

// ============================================================
// Field access and index
// ============================================================

#[test]
fn test_parse_field_access() {
    let ast = parse_ok("fn f() { obj.field }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::FieldAccess(fa) = expr {
        assert_eq!(fa.field.name, "field");
    } else {
        panic!("expected field access");
    }
}

#[test]
fn test_parse_index() {
    let ast = parse_ok("fn f() { arr[0] }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::Index(idx) = expr {
        if let Expr::IntLiteral(0) = &idx.index.node {
        } else {
            panic!("expected 0");
        }
    } else {
        panic!("expected index");
    }
}

// ============================================================
// Literals
// ============================================================

#[test]
fn test_parse_int_literal() {
    let ast = parse_ok("fn f() { 42 }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    assert!(matches!(expr, Expr::IntLiteral(42)));
}

#[test]
fn test_parse_float_literal() {
    let ast = parse_ok("fn f() { 3.14 }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::FloatLiteral(n) = expr {
        assert!((n - 3.14).abs() < 1e-10);
    } else {
        panic!("expected float");
    }
}

#[test]
fn test_parse_string_literal() {
    let ast = parse_ok("fn f() { \"hello\" }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::StringLiteral(s) = expr {
        assert_eq!(s, "hello");
    } else {
        panic!("expected string");
    }
}

#[test]
fn test_parse_char_literal() {
    let ast = parse_ok("fn f() { 'a' }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    assert!(matches!(expr, Expr::CharLiteral('a')));
}

#[test]
fn test_parse_bool_literal() {
    let ast = parse_ok("fn f() { true }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    assert!(matches!(expr, Expr::BoolLiteral(true)));

    let ast = parse_ok("fn f() { false }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    assert!(matches!(expr, Expr::BoolLiteral(false)));
}

#[test]
fn test_parse_nil_literal() {
    let ast = parse_ok("fn f() { nil }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    assert!(matches!(expr, Expr::NilLiteral));
}

// ============================================================
// Array and tuple literals
// ============================================================

#[test]
fn test_parse_array_literal() {
    let ast = parse_ok("fn f() { [1, 2, 3] }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::ArrayLiteral(elements) = expr {
        assert_eq!(elements.len(), 3);
    } else {
        panic!("expected array literal");
    }
}

#[test]
fn test_parse_empty_array() {
    let ast = parse_ok("fn f() { [] }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::ArrayLiteral(elements) = expr {
        assert!(elements.is_empty());
    } else {
        panic!("expected array literal");
    }
}

#[test]
fn test_parse_tuple_literal() {
    let ast = parse_ok("fn f() { (1, 2) }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::TupleLiteral(elements) = expr {
        assert_eq!(elements.len(), 2);
    } else {
        panic!("expected tuple literal");
    }
}

// ============================================================
// Struct literals
// ============================================================

#[test]
fn test_parse_struct_literal() {
    let ast = parse_ok("fn f() { Point { x: 1, y: 2 } }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::StructLiteral(s) = expr {
        assert_eq!(s.name.name, "Point");
        assert_eq!(s.fields.len(), 2);
        assert_eq!(s.fields[0].0.name, "x");
        assert_eq!(s.fields[1].0.name, "y");
    } else {
        panic!("expected struct literal");
    }
}

#[test]
fn test_parse_struct_literal_shorthand() {
    let ast = parse_ok("fn f() { Point { x, y } }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::StructLiteral(s) = expr {
        assert_eq!(s.fields.len(), 2);
        // Shorthand: field value should be Identifier with same name
        if let Expr::Identifier(i) = &s.fields[0].1.node {
            assert_eq!(i.name, "x");
        } else {
            panic!("expected shorthand identifier");
        }
    } else {
        panic!("expected struct literal");
    }
}

// ============================================================
// If expression
// ============================================================

#[test]
fn test_parse_if_basic() {
    let ast = parse_ok("fn f() { if x > 0 { x } }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::If(i) = expr {
        assert!(i.else_block.is_none());
    } else {
        panic!("expected if");
    }
}

#[test]
fn test_parse_if_else() {
    let ast = parse_ok("fn f() { if x > 0 { x } else { -x } }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::If(i) = expr {
        assert!(i.else_block.is_some());
        assert!(matches!(i.else_block.as_ref().unwrap(), ElseBlock::Else(_)));
    } else {
        panic!("expected if");
    }
}

#[test]
fn test_parse_if_else_if() {
    let ast = parse_ok("fn f() { if a { 1 } else if b { 2 } else { 3 } }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::If(i) = expr {
        assert!(matches!(
            i.else_block.as_ref().unwrap(),
            ElseBlock::ElseIf(_)
        ));
    } else {
        panic!("expected if");
    }
}

// ============================================================
// Match expression
// ============================================================

#[test]
fn test_parse_match_basic() {
    let src = "fn f() {\n    match x {\n        1 => true\n        2 => false\n    }\n}";
    let ast = parse_ok(src);
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::Match(m) = expr {
        assert_eq!(m.arms.len(), 2);
    } else {
        panic!("expected match");
    }
}

#[test]
fn test_parse_match_with_guard() {
    let src = "fn f() {\n    match x {\n        n if n > 0 => true\n        _ => false\n    }\n}";
    let ast = parse_ok(src);
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::Match(m) = expr {
        assert!(m.arms[0].guard.is_some());
        assert!(m.arms[1].guard.is_none());
    } else {
        panic!("expected match");
    }
}

#[test]
fn test_parse_match_variant_pattern() {
    let src =
        "fn f() {\n    match shape {\n        Circle(r) => r\n        Rect(w, h) => w\n    }\n}";
    let ast = parse_ok(src);
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::Match(m) = expr {
        if let Pattern::Variant(v) = &m.arms[0].pattern.node {
            assert_eq!(v.name.name, "Circle");
            assert_eq!(v.fields.len(), 1);
        } else {
            panic!("expected variant pattern");
        }
    } else {
        panic!("expected match");
    }
}

#[test]
fn test_parse_match_wildcard_pattern() {
    let src = "fn f() {\n    match x {\n        _ => 0\n    }\n}";
    let ast = parse_ok(src);
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::Match(m) = expr {
        assert!(matches!(m.arms[0].pattern.node, Pattern::Wildcard));
    } else {
        panic!("expected match");
    }
}

// ============================================================
// For, While, Loop
// ============================================================

#[test]
fn test_parse_for_loop() {
    let ast = parse_ok("fn f() {\n    for i in items {\n        i\n    }\n}");
    let stmt = first_stmt_of_fn(first_item(&ast));
    if let Stmt::For(f) = stmt {
        assert_eq!(f.binding.name, "i");
    } else {
        panic!("expected for");
    }
}

#[test]
fn test_parse_for_range() {
    let ast = parse_ok("fn f() {\n    for i in 0..10 {\n        i\n    }\n}");
    let stmt = first_stmt_of_fn(first_item(&ast));
    if let Stmt::For(f) = stmt {
        assert!(matches!(f.iterable.node, Expr::Range(_)));
    } else {
        panic!("expected for");
    }
}

#[test]
fn test_parse_while_loop() {
    let ast = parse_ok("fn f() {\n    while x > 0 {\n        x\n    }\n}");
    let stmt = first_stmt_of_fn(first_item(&ast));
    assert!(matches!(stmt, Stmt::While(_)));
}

#[test]
fn test_parse_loop() {
    let ast = parse_ok("fn f() {\n    loop {\n        break\n    }\n}");
    let stmt = first_stmt_of_fn(first_item(&ast));
    assert!(matches!(stmt, Stmt::Loop(_)));
}

// ============================================================
// Closures
// ============================================================

#[test]
fn test_parse_closure() {
    let ast = parse_ok("fn f() { |x, y| x + y }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::Closure(c) = expr {
        assert_eq!(c.params.len(), 2);
        assert_eq!(c.params[0].name.name, "x");
        assert!(c.params[0].ty.is_none());
    } else {
        panic!("expected closure");
    }
}

#[test]
fn test_parse_closure_with_types() {
    let ast = parse_ok("fn f() { |x i32, y i32| x + y }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::Closure(c) = expr {
        assert!(c.params[0].ty.is_some());
    } else {
        panic!("expected closure");
    }
}

// ============================================================
// Try operator
// ============================================================

#[test]
fn test_parse_try_operator() {
    let ast = parse_ok("fn f() { foo()? }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::Try(inner) = expr {
        assert!(matches!(inner.node, Expr::Call(_)));
    } else {
        panic!("expected try");
    }
}

#[test]
fn test_parse_try_binds_tight() {
    // foo()? should be (foo())?
    let ast = parse_ok("fn f() { a.b()? }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::Try(inner) = expr {
        assert!(matches!(inner.node, Expr::MethodCall(_)));
    } else {
        panic!("expected try");
    }
}

// ============================================================
// Assignment
// ============================================================

#[test]
fn test_parse_assignment() {
    let ast = parse_ok("fn f() { x = 5 }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::Assign(a) = expr {
        if let Expr::Identifier(i) = &a.target.node {
            assert_eq!(i.name, "x");
        } else {
            panic!("expected identifier target");
        }
    } else {
        panic!("expected assignment");
    }
}

#[test]
fn test_parse_compound_assignment() {
    let ast = parse_ok("fn f() { x += 1 }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    // Compound assignment desugars to x = x + 1
    if let Expr::Assign(a) = expr {
        if let Expr::Binary(b) = &a.value.node {
            assert_eq!(b.op, BinaryOp::Add);
        } else {
            panic!("expected binary in desugared compound assignment");
        }
    } else {
        panic!("expected assignment");
    }
}

// ============================================================
// Range
// ============================================================

#[test]
fn test_parse_range() {
    let ast = parse_ok("fn f() { 0..10 }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::Range(r) = expr {
        assert!(!r.inclusive);
    } else {
        panic!("expected range");
    }
}

// ============================================================
// Spawn and channels
// ============================================================

#[test]
fn test_parse_spawn() {
    let ast = parse_ok("fn f() { spawn { 42 } }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    assert!(matches!(expr, Expr::Spawn(_)));
}

#[test]
fn test_parse_chan_create() {
    let ast = parse_ok("fn f() {\n    ch := chan[i32]()\n}");
    let stmt = first_stmt_of_fn(first_item(&ast));
    if let Stmt::Let(l) = stmt {
        if let Expr::ChanCreate(c) = &l.value.node {
            assert!(c.capacity.is_none());
        } else {
            panic!("expected chan create");
        }
    } else {
        panic!("expected let");
    }
}

#[test]
fn test_parse_chan_buffered() {
    let ast = parse_ok("fn f() {\n    ch := chan[String](10)\n}");
    let stmt = first_stmt_of_fn(first_item(&ast));
    if let Stmt::Let(l) = stmt {
        if let Expr::ChanCreate(c) = &l.value.node {
            assert!(c.capacity.is_some());
        } else {
            panic!("expected chan create");
        }
    } else {
        panic!("expected let");
    }
}

// ============================================================
// Select
// ============================================================

#[test]
fn test_parse_select() {
    let src = "fn f() {\n    select {\n        msg := ch.recv() => msg\n        after timeout => nil\n    }\n}";
    let ast = parse_ok(src);
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::Select(s) = expr {
        assert_eq!(s.arms.len(), 2);
        assert!(matches!(s.arms[0].kind, SelectArmKind::Recv { .. }));
        assert!(matches!(s.arms[1].kind, SelectArmKind::After(_)));
    } else {
        panic!("expected select");
    }
}

// ============================================================
// Return, Break, Continue
// ============================================================

#[test]
fn test_parse_return() {
    let ast = parse_ok("fn f() { return 42 }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::Return(Some(_)) = expr {
        // OK
    } else {
        panic!("expected return with value");
    }
}

#[test]
fn test_parse_return_no_value() {
    let ast = parse_ok("fn f() {\n    return\n}");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::Return(None) = expr {
        // OK
    } else {
        panic!("expected return without value");
    }
}

#[test]
fn test_parse_break_continue() {
    let ast = parse_ok("fn f() {\n    loop {\n        break\n    }\n}");
    let stmt = first_stmt_of_fn(first_item(&ast));
    if let Stmt::Loop(block) = stmt {
        if let Stmt::Expr(e) = &block.stmts[0].node {
            assert!(matches!(e.node, Expr::Break(None)));
        } else {
            panic!("expected expr stmt");
        }
    } else {
        panic!("expected loop");
    }
}

// ============================================================
// Types
// ============================================================

#[test]
fn test_parse_return_type_result() {
    let ast = parse_ok("fn read_file(path String) -> String ! IoError { path }");
    let f = as_fn(first_item(&ast));
    if let Type::Result(r) = &f.return_type.as_ref().unwrap().node {
        if let Type::Named(ok) = &r.ok.node {
            assert_eq!(ok.name.name, "String");
        } else {
            panic!("expected named ok type");
        }
        if let Type::Named(err) = &r.err.node {
            assert_eq!(err.name.name, "IoError");
        } else {
            panic!("expected named err type");
        }
    } else {
        panic!("expected result type");
    }
}

#[test]
fn test_parse_optional_type() {
    let ast = parse_ok("fn find() -> ?String { nil }");
    let f = as_fn(first_item(&ast));
    if let Type::Optional(inner) = &f.return_type.as_ref().unwrap().node {
        if let Type::Named(path) = &inner.node {
            assert_eq!(path.name.name, "String");
        } else {
            panic!("expected named type");
        }
    } else {
        panic!("expected optional type");
    }
}

#[test]
fn test_parse_array_type() {
    let ast = parse_ok("fn process(items [i32]) { }");
    let f = as_fn(first_item(&ast));
    if let Type::Array(a) = &f.params[0].ty.node {
        assert!(a.size.is_none());
    } else {
        panic!("expected array type");
    }
}

#[test]
fn test_parse_generic_type() {
    let ast = parse_ok("fn get_map() -> Map[String, i32] { Map { } }");
    let f = as_fn(first_item(&ast));
    if let Type::Named(path) = &f.return_type.as_ref().unwrap().node {
        assert_eq!(path.name.name, "Map");
        assert_eq!(path.generic_args.len(), 2);
    } else {
        panic!("expected named type with generics");
    }
}

// ============================================================
// Patterns
// ============================================================

#[test]
fn test_parse_or_pattern() {
    let src = "fn f() {\n    match x {\n        1 | 2 | 3 => true\n        _ => false\n    }\n}";
    let ast = parse_ok(src);
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::Match(m) = expr {
        if let Pattern::Or(pats) = &m.arms[0].pattern.node {
            assert_eq!(pats.len(), 3);
        } else {
            panic!("expected or pattern");
        }
    } else {
        panic!("expected match");
    }
}

#[test]
fn test_parse_tuple_pattern() {
    let src = "fn f() {\n    match pair {\n        (a, b) => a\n    }\n}";
    let ast = parse_ok(src);
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::Match(m) = expr {
        if let Pattern::Tuple(pats) = &m.arms[0].pattern.node {
            assert_eq!(pats.len(), 2);
        } else {
            panic!("expected tuple pattern");
        }
    } else {
        panic!("expected match");
    }
}

// ============================================================
// String interpolation
// ============================================================

#[test]
fn test_parse_string_interpolation() {
    let ast = parse_ok("fn f() { \"hello {name}\" }");
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::StringInterpolation(parts) = expr {
        assert!(parts.len() >= 2); // at least literal and interpolation
    } else {
        panic!("expected string interpolation, got {:?}", expr);
    }
}

// ============================================================
// Multiple items
// ============================================================

#[test]
fn test_parse_multiple_functions() {
    let src = "fn foo() { }\n\nfn bar() { }\n\nfn baz() { }";
    let ast = parse_ok(src);
    assert_eq!(ast.items.len(), 3);
}

#[test]
fn test_parse_mixed_items() {
    let src = "struct S { }\n\nenum E {\n    A\n}\n\nfn f() { }";
    let ast = parse_ok(src);
    assert_eq!(ast.items.len(), 3);
    assert!(matches!(ast.items[0].node, Item::Struct(_)));
    assert!(matches!(ast.items[1].node, Item::Enum(_)));
    assert!(matches!(ast.items[2].node, Item::Function(_)));
}

// ============================================================
// Error recovery
// ============================================================

#[test]
fn test_parse_error_recovery_multiple_items() {
    // First fn has a syntax error, second should still parse
    let src = "fn bad( { }\n\nfn good() { }";
    let (ast, errors) = parse_with_errors(src);
    assert!(!errors.is_empty(), "should have errors");
    // Should have parsed at least one item (the good function)
    assert!(
        !ast.items.is_empty(),
        "should have recovered and parsed some items"
    );
}

#[test]
fn test_parse_error_message_quality() {
    let src = "fn f(";
    let (_, errors) = parse_with_errors(src);
    assert!(!errors.is_empty());
    let msg = &errors[0].message;
    assert!(
        msg.contains("expected"),
        "error should contain 'expected', got: {}",
        msg
    );
}

// ============================================================
// Complex / Integration
// ============================================================

#[test]
fn test_parse_function_with_body() {
    let src = r#"fn distance(self, other Point) -> f64 {
    dx := self.x - other.x
    dy := self.y - other.y
    (dx * dx + dy * dy).sqrt()
}"#;
    let ast = parse_ok(src);
    let f = as_fn(first_item(&ast));
    assert_eq!(f.name.name, "distance");
    assert_eq!(f.body.as_ref().unwrap().stmts.len(), 3);
}

#[test]
fn test_parse_nested_if_in_fn() {
    let src = r#"fn classify(n i32) -> String {
    if n > 0 {
        "positive"
    } else if n < 0 {
        "negative"
    } else {
        "zero"
    }
}"#;
    let ast = parse_ok(src);
    let f = as_fn(first_item(&ast));
    assert_eq!(f.body.as_ref().unwrap().stmts.len(), 1);
}

#[test]
fn test_parse_match_with_block_body() {
    let src = r#"fn f() {
    match shape {
        Circle(r) => 3.14 * r * r
        Triangle(a, b, c) => {
            s := (a + b + c) / 2.0
            s
        }
    }
}"#;
    let ast = parse_ok(src);
    let expr = as_expr_stmt(first_stmt_of_fn(first_item(&ast)));
    if let Expr::Match(m) = expr {
        assert_eq!(m.arms.len(), 2);
        // Second arm body should be a block expression
        assert!(matches!(m.arms[1].body.node, Expr::Block(_)));
    } else {
        panic!("expected match");
    }
}

// ============================================================
// Associated types
// ============================================================

#[test]
fn parse_trait_with_associated_type() {
    let ast = parse_ok(
        r#"trait Iterator {
    type Item
    fn next(self) -> i32
}"#,
    );
    if let Item::Trait(t) = &ast.items[0].node {
        assert_eq!(t.name.name, "Iterator");
        assert_eq!(t.associated_types.len(), 1);
        assert_eq!(t.associated_types[0].name.name, "Item");
        assert_eq!(t.methods.len(), 1);
    } else {
        panic!("expected trait");
    }
}

#[test]
fn parse_impl_with_associated_type_binding() {
    let ast = parse_ok(
        r#"impl Iterator for Vec {
    type Item = i32
    fn next(self) -> i32 { 0 }
}"#,
    );
    if let Item::Impl(imp) = &ast.items[0].node {
        assert_eq!(imp.associated_type_bindings.len(), 1);
        assert_eq!(imp.associated_type_bindings[0].0.name, "Item");
        assert!(matches!(
            imp.associated_type_bindings[0].1.node,
            Type::Named(_)
        ));
        assert_eq!(imp.methods.len(), 1);
    } else {
        panic!("expected impl");
    }
}

#[test]
fn parse_trait_multiple_associated_types() {
    let ast = parse_ok(
        r#"trait Map {
    type Key
    type Value
    fn get(self, k i32) -> i32
}"#,
    );
    if let Item::Trait(t) = &ast.items[0].node {
        assert_eq!(t.associated_types.len(), 2);
        assert_eq!(t.associated_types[0].name.name, "Key");
        assert_eq!(t.associated_types[1].name.name, "Value");
    } else {
        panic!("expected trait");
    }
}
