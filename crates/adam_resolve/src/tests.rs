//! Name resolution tests.

use adam_lexer::Lexer;
use adam_parser::Parser;

use crate::errors::ResolveErrorKind;
use crate::resolver::resolve;
use crate::scope::{DeclKind, ScopeKind};

// ---- Helpers ----

fn resolve_ok(src: &str) -> crate::resolver::ResolveResult {
    let lexer = Lexer::new(src);
    let lex_result = lexer.tokenize();
    assert!(lex_result.errors.is_empty(), "lex errors: {:?}", lex_result.errors);
    let parse = Parser::new(lex_result.tokens).parse();
    assert!(parse.errors.is_empty(), "parse errors: {:?}", parse.errors);
    let result = resolve(&parse.ast);
    if !result.errors.is_empty() {
        panic!(
            "expected no resolve errors, got: {:?}",
            result.errors.iter().map(|e| e.to_string()).collect::<Vec<_>>()
        );
    }
    result
}

fn resolve_with_errors(src: &str) -> crate::resolver::ResolveResult {
    let lexer = Lexer::new(src);
    let lex_result = lexer.tokenize();
    assert!(lex_result.errors.is_empty(), "lex errors: {:?}", lex_result.errors);
    let parse = Parser::new(lex_result.tokens).parse();
    assert!(parse.errors.is_empty(), "parse errors: {:?}", parse.errors);
    resolve(&parse.ast)
}

fn has_decl(result: &crate::resolver::ResolveResult, name: &str, kind: &DeclKind) -> bool {
    result
        .declarations
        .iter()
        .any(|d| d.name == name && &d.kind == kind)
}

fn error_count(result: &crate::resolver::ResolveResult) -> usize {
    result.errors.len()
}

fn first_error_kind(result: &crate::resolver::ResolveResult) -> &ResolveErrorKind {
    &result.errors[0].kind
}

// ---- P4.S1: Scope structure tests ----

#[test]
fn test_scope_tree_root() {
    let result = resolve_ok("fn main() {}");
    assert!(result.scope_tree.scope_count() >= 2); // root + function scope
    assert_eq!(result.scope_tree.scope(0).kind, ScopeKind::Module);
}

#[test]
fn test_nested_scopes() {
    let result = resolve_ok(
        "fn main() {
            if true {
                x := 1
            }
        }",
    );
    // root (Module) -> function -> block (if)
    assert!(result.scope_tree.scope_count() >= 3);
}

#[test]
fn test_scope_lookup_walks_parents() {
    let result = resolve_ok(
        "fn main() {
            x := 5
            if true {
                y := x
            }
        }",
    );
    assert!(has_decl(&result, "x", &DeclKind::Variable { mutable: false }));
}

// ---- P4.S2: Basic declarations ----

#[test]
fn test_simple_variable() {
    let result = resolve_ok(
        "fn main() {
            x := 5
        }",
    );
    assert!(has_decl(&result, "x", &DeclKind::Variable { mutable: false }));
}

#[test]
fn test_mutable_variable() {
    let result = resolve_ok(
        "fn main() {
            mut count := 0
        }",
    );
    assert!(has_decl(&result, "count", &DeclKind::Variable { mutable: true }));
}

#[test]
fn test_typed_variable() {
    let result = resolve_ok(
        "fn main() {
            name: String = \"adam\"
        }",
    );
    assert!(has_decl(&result, "name", &DeclKind::Variable { mutable: false }));
}

#[test]
fn test_function_declared() {
    let result = resolve_ok("fn foo() {}");
    assert!(has_decl(&result, "foo", &DeclKind::Function));
}

#[test]
fn test_struct_declared() {
    let result = resolve_ok(
        "struct Point {
            x f64
            y f64
        }",
    );
    assert!(has_decl(&result, "Point", &DeclKind::Struct));
}

#[test]
fn test_enum_declared() {
    let result = resolve_ok(
        "enum Shape {
            Circle(f64)
            Rectangle(f64, f64)
        }",
    );
    assert!(has_decl(&result, "Shape", &DeclKind::Enum));
    assert!(has_decl(&result, "Circle", &DeclKind::EnumVariant));
    assert!(has_decl(&result, "Rectangle", &DeclKind::EnumVariant));
}

#[test]
fn test_trait_declared() {
    let result = resolve_ok(
        "trait Drawable {
            fn draw(self)
        }",
    );
    assert!(has_decl(&result, "Drawable", &DeclKind::Trait));
}

#[test]
fn test_module_declared() {
    let result = resolve_ok("mod utils");
    assert!(has_decl(&result, "utils", &DeclKind::Module));
}

// ---- Function params ----

#[test]
fn test_function_params_in_scope() {
    let result = resolve_ok(
        "fn add(a i32, b i32) -> i32 {
            a + b
        }",
    );
    assert!(has_decl(
        &result,
        "a",
        &DeclKind::Param {
            ownership: adam_ast::item::ParamOwnership::Borrow
        }
    ));
    assert!(has_decl(
        &result,
        "b",
        &DeclKind::Param {
            ownership: adam_ast::item::ParamOwnership::Borrow
        }
    ));
}

#[test]
fn test_self_param() {
    let result = resolve_ok(
        "struct Foo {
            x i32
        }
        impl Foo {
            fn get(self) -> i32 {
                self.x
            }
        }",
    );
    assert!(has_decl(
        &result,
        "self",
        &DeclKind::Param {
            ownership: adam_ast::item::ParamOwnership::SelfBorrow
        }
    ));
}

#[test]
fn test_own_param() {
    let result = resolve_ok(
        "fn consume(own val String) {
            val
        }",
    );
    assert!(has_decl(
        &result,
        "val",
        &DeclKind::Param {
            ownership: adam_ast::item::ParamOwnership::Own
        }
    ));
}

// ---- Generics ----

#[test]
fn test_generic_params() {
    let result = resolve_ok(
        "fn identity[T](x T) -> T {
            x
        }",
    );
    assert!(has_decl(&result, "T", &DeclKind::TypeParam));
}

#[test]
fn test_generic_with_bounds() {
    let result = resolve_ok(
        "trait Comparable {}
        fn sort[T: Comparable](items [T]) {}",
    );
    assert!(has_decl(&result, "T", &DeclKind::TypeParam));
}

// ---- Loops ----

#[test]
fn test_for_loop_binding() {
    let result = resolve_ok(
        "fn main() {
            items := [1, 2, 3]
            for item in items {
                item
            }
        }",
    );
    assert!(has_decl(&result, "item", &DeclKind::Variable { mutable: false }));
}

#[test]
fn test_while_loop() {
    let result = resolve_ok(
        "fn main() {
            mut x := 10
            while x > 0 {
                x = x - 1
            }
        }",
    );
    assert!(has_decl(&result, "x", &DeclKind::Variable { mutable: true }));
}

#[test]
fn test_loop() {
    let result = resolve_ok(
        "fn main() {
            loop {
                break
            }
        }",
    );
    assert!(error_count(&result) == 0);
}

// ---- Match ----

#[test]
fn test_match_arm_binding() {
    let result = resolve_ok(
        "enum Shape {
            Circle(f64)
            Rectangle(f64, f64)
        }
        fn area(s Shape) -> f64 {
            match s {
                Circle(r) => r * r
                Rectangle(w, h) => w * h
            }
        }",
    );
    assert!(has_decl(&result, "r", &DeclKind::Variable { mutable: false }));
    assert!(has_decl(&result, "w", &DeclKind::Variable { mutable: false }));
    assert!(has_decl(&result, "h", &DeclKind::Variable { mutable: false }));
}

#[test]
fn test_match_wildcard() {
    let result = resolve_ok(
        "fn main() {
            x := 5
            match x {
                1 => 10
                _ => 0
            }
        }",
    );
    assert!(error_count(&result) == 0);
}

// ---- Closures ----

#[test]
fn test_closure_params() {
    let result = resolve_ok(
        "fn main() {
            add := |a, b| a + b
        }",
    );
    assert!(has_decl(
        &result,
        "a",
        &DeclKind::Param {
            ownership: adam_ast::item::ParamOwnership::Borrow
        }
    ));
}

// ---- Shadowing ----

#[test]
fn test_shadowing_allowed() {
    let result = resolve_ok(
        "fn main() {
            x := 5
            if true {
                x := 10
            }
        }",
    );
    // Both declarations exist, no error.
    let x_decls: Vec<_> = result
        .declarations
        .iter()
        .filter(|d| d.name == "x")
        .collect();
    assert_eq!(x_decls.len(), 2);
}

// ---- Impl ----

#[test]
fn test_impl_methods() {
    let result = resolve_ok(
        "struct Counter {
            value i32
        }
        impl Counter {
            fn new() -> Counter {
                Counter { value: 0 }
            }
            fn get(self) -> i32 {
                self.value
            }
        }",
    );
    assert!(has_decl(&result, "new", &DeclKind::Method));
    assert!(has_decl(&result, "get", &DeclKind::Method));
}

#[test]
fn test_trait_impl() {
    let result = resolve_ok(
        "trait Drawable {
            fn draw(self)
        }
        struct Circle {
            radius f64
        }
        impl Drawable for Circle {
            fn draw(self) {
                self.radius
            }
        }",
    );
    assert!(has_decl(&result, "Drawable", &DeclKind::Trait));
    assert!(has_decl(&result, "Circle", &DeclKind::Struct));
}

// ---- Views ----

#[test]
fn test_view_state_fields() {
    let result = resolve_ok(
        "view Counter {
            @state count: i32 = 0
            body {
                count
            }
        }",
    );
    assert!(has_decl(&result, "count", &DeclKind::ViewField));
}

#[test]
fn test_view_prop_field() {
    let result = resolve_ok(
        "view Greeting {
            @prop name: String
            body {
                name
            }
        }",
    );
    assert!(has_decl(&result, "name", &DeclKind::ViewField));
}

// ---- Imports ----

#[test]
fn test_single_import() {
    let result = resolve_ok("use std.io.println");
    assert!(has_decl(&result, "println", &DeclKind::Import));
}

#[test]
fn test_multi_import() {
    let result = resolve_ok("use std.{io, fs}");
    assert!(has_decl(&result, "io", &DeclKind::Import));
    assert!(has_decl(&result, "fs", &DeclKind::Import));
}

#[test]
fn test_wildcard_import() {
    // Wildcard imports don't declare any specific names.
    let result = resolve_ok("use std.io.*");
    assert!(error_count(&result) == 0);
}

// ---- Items in blocks ----

#[test]
fn test_item_in_block() {
    let result = resolve_ok(
        "fn main() {
            fn helper() -> i32 {
                42
            }
            helper()
        }",
    );
    // helper is declared as a function inside main's scope
    let helpers: Vec<_> = result
        .declarations
        .iter()
        .filter(|d| d.name == "helper" && d.kind == DeclKind::Function)
        .collect();
    assert_eq!(helpers.len(), 1);
}

// ---- Forward references (top-level) ----

#[test]
fn test_forward_reference() {
    // Functions can reference other functions defined later.
    let result = resolve_ok(
        "fn main() {
            helper()
        }
        fn helper() -> i32 {
            42
        }",
    );
    assert!(error_count(&result) == 0);
}

#[test]
fn test_struct_forward_reference() {
    let result = resolve_ok(
        "fn make() -> Point {
            Point { x: 0.0, y: 0.0 }
        }
        struct Point {
            x f64
            y f64
        }",
    );
    assert!(error_count(&result) == 0);
}

// ---- Error detection ----

#[test]
fn test_undefined_variable() {
    let result = resolve_with_errors(
        "fn main() {
            y
        }",
    );
    assert_eq!(error_count(&result), 1);
    match first_error_kind(&result) {
        ResolveErrorKind::UndefinedName { name, .. } => assert_eq!(name, "y"),
        other => panic!("expected UndefinedName, got {:?}", other),
    }
}

#[test]
fn test_undefined_in_expression() {
    let result = resolve_with_errors(
        "fn main() {
            x := unknown_var + 1
        }",
    );
    assert_eq!(error_count(&result), 1);
    match first_error_kind(&result) {
        ResolveErrorKind::UndefinedName { name, .. } => assert_eq!(name, "unknown_var"),
        other => panic!("expected UndefinedName, got {:?}", other),
    }
}

#[test]
fn test_duplicate_definition() {
    let result = resolve_with_errors(
        "fn foo() {}
        fn foo() {}",
    );
    assert_eq!(error_count(&result), 1);
    match first_error_kind(&result) {
        ResolveErrorKind::DuplicateDefinition { name, .. } => assert_eq!(name, "foo"),
        other => panic!("expected DuplicateDefinition, got {:?}", other),
    }
}

#[test]
fn test_duplicate_variable_in_same_scope() {
    let result = resolve_with_errors(
        "fn main() {
            x := 1
            x := 2
        }",
    );
    assert_eq!(error_count(&result), 1);
    match first_error_kind(&result) {
        ResolveErrorKind::DuplicateDefinition { name, .. } => assert_eq!(name, "x"),
        other => panic!("expected DuplicateDefinition, got {:?}", other),
    }
}

#[test]
fn test_undefined_type() {
    let result = resolve_with_errors(
        "fn main() {
            x: UnknownType = 5
        }",
    );
    assert_eq!(error_count(&result), 1);
    match first_error_kind(&result) {
        ResolveErrorKind::UndefinedType { name, .. } => assert_eq!(name, "UnknownType"),
        other => panic!("expected UndefinedType, got {:?}", other),
    }
}

#[test]
fn test_undefined_struct_literal() {
    let result = resolve_with_errors(
        "fn main() {
            p := Unknown { x: 1 }
        }",
    );
    assert_eq!(error_count(&result), 1);
    match first_error_kind(&result) {
        ResolveErrorKind::UndefinedType { name, .. } => assert_eq!(name, "Unknown"),
        other => panic!("expected UndefinedType, got {:?}", other),
    }
}

#[test]
fn test_undefined_trait_in_impl() {
    let result = resolve_with_errors(
        "struct Foo {}
        impl UnknownTrait for Foo {}",
    );
    assert_eq!(error_count(&result), 1);
    match first_error_kind(&result) {
        ResolveErrorKind::UndefinedType { name, .. } => assert_eq!(name, "UnknownTrait"),
        other => panic!("expected UndefinedType, got {:?}", other),
    }
}

// ---- "Did you mean" suggestions ----

#[test]
fn test_did_you_mean() {
    let result = resolve_with_errors(
        "fn main() {
            count := 5
            coutn
        }",
    );
    assert_eq!(error_count(&result), 1);
    match first_error_kind(&result) {
        ResolveErrorKind::UndefinedName {
            name, suggestion, ..
        } => {
            assert_eq!(name, "coutn");
            assert_eq!(suggestion.as_deref(), Some("count"));
        }
        other => panic!("expected UndefinedName with suggestion, got {:?}", other),
    }
}

#[test]
fn test_did_you_mean_type() {
    let result = resolve_with_errors(
        "struct Point {
            x f64
            y f64
        }
        fn main() {
            p: Poitn = Point { x: 0.0, y: 0.0 }
        }",
    );
    // Should have UndefinedType with suggestion "Point"
    let type_errors: Vec<_> = result
        .errors
        .iter()
        .filter(|e| matches!(&e.kind, ResolveErrorKind::UndefinedType { .. }))
        .collect();
    assert!(!type_errors.is_empty());
    match &type_errors[0].kind {
        ResolveErrorKind::UndefinedType { name, suggestion } => {
            assert_eq!(name, "Poitn");
            assert_eq!(suggestion.as_deref(), Some("Point"));
        }
        _ => unreachable!(),
    }
}

// ---- Levenshtein distance ----

#[test]
fn test_levenshtein_identical() {
    assert_eq!(crate::errors::levenshtein("hello", "hello"), 0);
}

#[test]
fn test_levenshtein_one_off() {
    assert_eq!(crate::errors::levenshtein("hello", "helo"), 1);
}

#[test]
fn test_levenshtein_distant() {
    assert_eq!(crate::errors::levenshtein("abc", "xyz"), 3);
}

// ---- Built-in types don't trigger errors ----

#[test]
fn test_builtin_types_resolve() {
    let result = resolve_ok(
        "fn main() {
            a: i32 = 1
            b: f64 = 2.0
            c: bool = true
            d: String = \"hi\"
            e: char = 'a'
        }",
    );
    assert!(error_count(&result) == 0);
}

#[test]
fn test_optional_type() {
    let result = resolve_ok(
        "fn main() {
            x: ?i32 = nil
        }",
    );
    assert!(error_count(&result) == 0);
}

#[test]
fn test_array_type() {
    let result = resolve_ok(
        "fn main() {
            items: [i32] = [1, 2, 3]
        }",
    );
    assert!(error_count(&result) == 0);
}

#[test]
fn test_result_type() {
    let result = resolve_ok(
        "fn read() -> String ! String {
            \"ok\"
        }",
    );
    assert!(error_count(&result) == 0);
}

// ---- Complex integration tests ----

#[test]
fn test_struct_with_impl_and_usage() {
    let result = resolve_ok(
        "struct Point {
            x f64
            y f64
        }
        impl Point {
            fn new(x f64, y f64) -> Point {
                Point { x: x, y: y }
            }
            fn distance(self) -> f64 {
                self.x + self.y
            }
        }
        fn main() {
            p := Point.new(1.0, 2.0)
            p.distance()
        }",
    );
    assert!(has_decl(&result, "Point", &DeclKind::Struct));
    assert!(has_decl(&result, "new", &DeclKind::Method));
    assert!(has_decl(&result, "distance", &DeclKind::Method));
    assert!(has_decl(&result, "p", &DeclKind::Variable { mutable: false }));
}

#[test]
fn test_enum_match_integration() {
    let result = resolve_ok(
        "enum Option {
            Some(i32)
            None
        }
        fn unwrap(opt Option) -> i32 {
            match opt {
                Some(val) => val
                None => 0
            }
        }",
    );
    assert!(has_decl(&result, "Option", &DeclKind::Enum));
    assert!(has_decl(&result, "Some", &DeclKind::EnumVariant));
    assert!(has_decl(&result, "None", &DeclKind::EnumVariant));
    assert!(has_decl(&result, "val", &DeclKind::Variable { mutable: false }));
}

#[test]
fn test_trait_with_impl_integration() {
    let result = resolve_ok(
        "trait Printable {
            fn to_string(self) -> String
        }
        struct Name {
            value String
        }
        impl Printable for Name {
            fn to_string(self) -> String {
                self.value
            }
        }",
    );
    assert!(has_decl(&result, "Printable", &DeclKind::Trait));
    assert!(has_decl(&result, "Name", &DeclKind::Struct));
}

#[test]
fn test_select_expression() {
    let result = resolve_ok(
        "fn main() {
            ch := chan[i32]()
            select {
                val := ch.recv() => {
                    val
                }
            }
        }",
    );
    assert!(has_decl(&result, "ch", &DeclKind::Variable { mutable: false }));
    assert!(has_decl(&result, "val", &DeclKind::Variable { mutable: false }));
}

#[test]
fn test_spawn_block() {
    let result = resolve_ok(
        "fn main() {
            x := 5
            spawn {
                x
            }
        }",
    );
    assert!(error_count(&result) == 0);
}

#[test]
fn test_multiple_errors() {
    let result = resolve_with_errors(
        "fn main() {
            unknown1
            unknown2
        }",
    );
    assert_eq!(error_count(&result), 2);
}

#[test]
fn test_return_expression() {
    let result = resolve_ok(
        "fn main() -> i32 {
            x := 42
            return x
        }",
    );
    assert!(error_count(&result) == 0);
}

#[test]
fn test_if_else() {
    let result = resolve_ok(
        "fn main() {
            x := 5
            if x > 0 {
                x
            } else {
                0
            }
        }",
    );
    assert!(error_count(&result) == 0);
}

#[test]
fn test_generic_struct() {
    let result = resolve_ok(
        "struct Pair[T] {
            first T
            second T
        }",
    );
    assert!(has_decl(&result, "Pair", &DeclKind::Struct));
}

#[test]
fn test_fn_type_param() {
    let result = resolve_ok(
        "fn apply(f fn(i32) -> i32, x i32) -> i32 {
            f(x)
        }",
    );
    assert!(has_decl(
        &result,
        "f",
        &DeclKind::Param {
            ownership: adam_ast::item::ParamOwnership::Borrow
        }
    ));
}

#[test]
fn test_string_interpolation() {
    let result = resolve_ok(
        "fn main() {
            name := \"Adam\"
            msg := \"Hello, {name}!\"
        }",
    );
    assert!(error_count(&result) == 0);
}

#[test]
fn test_range_expr() {
    let result = resolve_ok(
        "fn main() {
            r := 1..10
        }",
    );
    assert!(error_count(&result) == 0);
}

#[test]
fn test_scope_count() {
    let result = resolve_ok(
        "fn main() {
            if true {
                x := 1
            }
            if false {
                y := 2
            }
        }",
    );
    // Module(1) + main fn(1) + if block(1) + if block(1) = at least 4
    assert!(result.scope_tree.scope_count() >= 4);
}

#[test]
fn test_empty_file() {
    let result = resolve_ok("");
    assert!(error_count(&result) == 0);
    assert_eq!(result.scope_tree.scope_count(), 1); // just root
}

#[test]
fn test_self_type_in_impl() {
    let result = resolve_ok(
        "struct Foo {}
        impl Foo {
            fn make() -> Self {
                Foo {}
            }
        }",
    );
    // Self should resolve as a type param in impl scope.
    assert!(has_decl(&result, "Self", &DeclKind::TypeParam));
}
