//! Adversarial tests for name resolution — shadowing, scoping edge cases, cycles.

#[cfg(test)]
mod tests {
    use crate::resolver::resolve;
    use adam_lexer::Lexer;
    use adam_parser::Parser;

    fn resolve_source(source: &str) -> crate::resolver::ResolveResult {
        let tokens = Lexer::new(source).tokenize().tokens;
        let ast = Parser::new(tokens).parse().ast;
        resolve(&ast)
    }

    // -----------------------------------------------------------------------
    // Shadowing
    // -----------------------------------------------------------------------

    #[test]
    fn test_variable_shadowing() {
        let source = r#"
fn main() {
    let x = 1
    let x = 2
    let x = 3
}
"#;
        let result = resolve_source(source);
        // Shadowing should be allowed (or produce a specific diagnostic).
        let _ = result;
    }

    #[test]
    fn test_deep_shadowing() {
        let mut source = String::from("fn main() {\n");
        for i in 0..100 {
            source.push_str(&format!("    let x = {}\n", i));
        }
        source.push_str("}\n");
        let result = resolve_source(&source);
        let _ = result;
    }

    #[test]
    fn test_parameter_shadows_outer() {
        let source = r#"
fn outer() {
    let x = 1
    fn inner(x: Int) {
        let y = x
    }
}
"#;
        let result = resolve_source(source);
        let _ = result;
    }

    // -----------------------------------------------------------------------
    // Scope edges
    // -----------------------------------------------------------------------

    #[test]
    fn test_use_before_declaration() {
        let source = r#"
fn main() {
    let y = x
    let x = 42
}
"#;
        let result = resolve_source(source);
        // Resolver may or may not flag use-before-decl within the same scope.
        // Either way, should not panic.
        let _ = result;
    }

    #[test]
    fn test_cross_function_reference() {
        let source = r#"
fn a() {
    b()
}

fn b() {
    a()
}
"#;
        let result = resolve_source(source);
        // Mutual recursion — should resolve since functions are hoisted.
        let _ = result;
    }

    #[test]
    fn test_many_nested_scopes() {
        let depth = 100;
        let mut source = String::from("fn main() {\n");
        for i in 0..depth {
            source.push_str(&format!("    let x{} = {}\n    {{\n", i, i));
        }
        source.push_str("        let inner = 0\n");
        for _ in 0..depth {
            source.push_str("    }\n");
        }
        source.push_str("}\n");
        let result = resolve_source(&source);
        let _ = result;
    }

    // -----------------------------------------------------------------------
    // Duplicate definitions
    // -----------------------------------------------------------------------

    #[test]
    fn test_duplicate_function_names() {
        let source = r#"
fn foo() {}
fn foo() {}
"#;
        let result = resolve_source(source);
        // Should report duplicate definition.
        assert!(!result.errors.is_empty());
    }

    #[test]
    fn test_duplicate_struct_names() {
        let source = r#"
struct Foo {}
struct Foo {}
"#;
        let result = resolve_source(source);
        assert!(!result.errors.is_empty());
    }

    // -----------------------------------------------------------------------
    // Large-scale resolution
    // -----------------------------------------------------------------------

    #[test]
    fn test_many_declarations() {
        let decls: Vec<String> = (0..500).map(|i| format!("fn f{}() {{}}", i)).collect();
        let source = decls.join("\n");
        let result = resolve_source(&source);
        assert!(result.declarations.len() >= 500);
    }

    #[test]
    fn test_many_variables_in_function() {
        let vars: Vec<String> = (0..500)
            .map(|i| format!("    let v{} = {}", i, i))
            .collect();
        let source = format!("fn main() {{\n{}\n}}", vars.join("\n"));
        let result = resolve_source(&source);
        let _ = result;
    }

    // -----------------------------------------------------------------------
    // Empty and degenerate inputs
    // -----------------------------------------------------------------------

    #[test]
    fn test_empty_source() {
        let result = resolve_source("");
        assert!(result.errors.is_empty());
    }

    #[test]
    fn test_only_comments() {
        let result = resolve_source("// just a comment\n");
        assert!(result.errors.is_empty());
    }

    #[test]
    fn test_empty_function() {
        let result = resolve_source("fn empty() {}");
        assert!(result.errors.is_empty());
    }

    // -----------------------------------------------------------------------
    // Import edge cases
    // -----------------------------------------------------------------------

    #[test]
    fn test_undefined_import() {
        let source = "use nonexistent.module\n";
        let result = resolve_source(source);
        // Should produce an error for undefined module.
        let _ = result;
    }

    #[test]
    fn test_wildcard_import() {
        let source = "use std.*\n";
        let result = resolve_source(source);
        let _ = result;
    }
}
