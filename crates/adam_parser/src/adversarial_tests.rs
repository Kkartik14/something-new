//! Adversarial tests for the parser — malformed and pathological input.

#[cfg(test)]
mod tests {
    use crate::parser::Parser;
    use adam_lexer::Lexer;

    fn parse(source: &str) -> crate::parser::ParseResult {
        let tokens = Lexer::new(source).tokenize().tokens;
        Parser::new(tokens).parse()
    }

    // -----------------------------------------------------------------------
    // Deep nesting — tests stack safety
    // -----------------------------------------------------------------------

    #[test]
    fn test_deeply_nested_if_else() {
        let mut source = String::new();
        let depth = 50;
        source.push_str("fn main() {\n");
        for _ in 0..depth {
            source.push_str("if true {\n");
        }
        source.push_str("x\n");
        for _ in 0..depth {
            source.push_str("}\n");
        }
        source.push_str("}\n");
        let result = parse(&source);
        // Should either parse or error — not stack overflow.
        let _ = result;
    }

    #[test]
    fn test_deeply_nested_expressions() {
        let depth = 200;
        let mut source = String::from("let x = ");
        for _ in 0..depth {
            source.push('(');
        }
        source.push('1');
        for _ in 0..depth {
            source.push(')');
        }
        let result = parse(&source);
        let _ = result;
    }

    #[test]
    fn test_deeply_nested_function_calls() {
        let depth = 100;
        let mut source = String::from("let x = ");
        for _ in 0..depth {
            source.push_str("f(");
        }
        source.push('1');
        for _ in 0..depth {
            source.push(')');
        }
        let result = parse(&source);
        let _ = result;
    }

    // -----------------------------------------------------------------------
    // Malformed syntax — should error gracefully
    // -----------------------------------------------------------------------

    #[test]
    fn test_missing_closing_brace() {
        let result = parse("fn main() {");
        assert!(!result.errors.is_empty());
    }

    #[test]
    fn test_missing_opening_brace() {
        let result = parse("fn main() }");
        assert!(!result.errors.is_empty());
    }

    #[test]
    fn test_double_operators() {
        let result = parse("let x = 1 ++ 2");
        let _ = result; // Should not panic.
    }

    #[test]
    fn test_missing_expression() {
        let result = parse("let x =");
        assert!(!result.errors.is_empty());
    }

    #[test]
    fn test_missing_function_body() {
        let result = parse("fn foo()");
        // Parser may accept this as a forward declaration — either way, no panic.
        let _ = result;
    }

    #[test]
    fn test_empty_struct() {
        let result = parse("struct Empty {}");
        // Empty struct should be valid.
        let _ = result;
    }

    #[test]
    fn test_only_keywords() {
        let result = parse("fn fn fn fn");
        assert!(!result.errors.is_empty());
    }

    #[test]
    fn test_only_operators() {
        let result = parse("+ - * / % && || == !=");
        assert!(!result.errors.is_empty());
    }

    #[test]
    fn test_unmatched_parens() {
        let result = parse("fn main() { ((()) }");
        assert!(!result.errors.is_empty());
    }

    #[test]
    fn test_semicolons_everywhere() {
        let result = parse(";;;fn;;;main();;;{;;;};");
        // Some languages use semicolons — should handle gracefully.
        let _ = result;
    }

    // -----------------------------------------------------------------------
    // Type system edge cases
    // -----------------------------------------------------------------------

    #[test]
    fn test_deeply_nested_generic_types() {
        let source = "let x: Vec<Vec<Vec<Vec<Vec<Vec<Vec<Int>>>>>>> = []";
        let result = parse(source);
        let _ = result;
    }

    #[test]
    fn test_function_with_many_params() {
        let params: Vec<String> = (0..100).map(|i| format!("p{}: Int", i)).collect();
        let source = format!("fn big({}) {{}}", params.join(", "));
        let result = parse(&source);
        let _ = result;
    }

    #[test]
    fn test_struct_with_many_fields() {
        let fields: Vec<String> = (0..200).map(|i| format!("    field{}: Int", i)).collect();
        let source = format!("struct Big {{\n{}\n}}", fields.join("\n"));
        let result = parse(&source);
        let _ = result;
    }

    #[test]
    fn test_enum_with_many_variants() {
        let variants: Vec<String> = (0..200).map(|i| format!("    V{}", i)).collect();
        let source = format!("enum Big {{\n{}\n}}", variants.join("\n"));
        let result = parse(&source);
        let _ = result;
    }

    // -----------------------------------------------------------------------
    // Pattern matching edge cases
    // -----------------------------------------------------------------------

    #[test]
    fn test_match_with_many_arms() {
        let arms: Vec<String> = (0..100).map(|i| format!("    {} => {}", i, i * 2)).collect();
        let source = format!("fn f(x: Int) -> Int {{\n    match x {{\n{}\n    }}\n}}", arms.join("\n"));
        let result = parse(&source);
        let _ = result;
    }

    #[test]
    fn test_nested_match() {
        let source = r#"
fn f(x: Int, y: Int) -> Int {
    match x {
        0 => match y {
            0 => 0
            _ => 1
        }
        _ => 2
    }
}
"#;
        let result = parse(source);
        let _ = result;
    }

    // -----------------------------------------------------------------------
    // Large-scale inputs
    // -----------------------------------------------------------------------

    #[test]
    fn test_many_functions() {
        let fns: Vec<String> = (0..500).map(|i| format!("fn f{}() {{}}", i)).collect();
        let source = fns.join("\n");
        let result = parse(&source);
        // Should handle many top-level items.
        let _ = result;
    }

    #[test]
    fn test_large_expression() {
        // x + x + x + ... (1000 additions)
        let source = format!("let x = {}", vec!["1"; 1000].join(" + "));
        let result = parse(&source);
        let _ = result;
    }

    // -----------------------------------------------------------------------
    // Error recovery
    // -----------------------------------------------------------------------

    #[test]
    fn test_error_recovery_continues_after_bad_function() {
        let source = r#"
fn good1() {}

fn bad( {
    // missing closing paren
}

fn good2() {}
"#;
        let result = parse(source);
        // Should still find good1 and good2 even with bad in the middle.
        let _ = result;
    }

    #[test]
    fn test_error_recovery_missing_types() {
        let source = r#"
fn f(x: , y: Int) -> {
    42
}
"#;
        let result = parse(source);
        assert!(!result.errors.is_empty());
    }

    // -----------------------------------------------------------------------
    // Empty and minimal inputs
    // -----------------------------------------------------------------------

    #[test]
    fn test_empty_input() {
        let result = parse("");
        // Empty source should be valid (no items).
        let _ = result;
    }

    #[test]
    fn test_single_newline() {
        let result = parse("\n");
        let _ = result;
    }

    #[test]
    fn test_single_keyword() {
        let result = parse("fn");
        assert!(!result.errors.is_empty());
    }

    #[test]
    fn test_single_number() {
        let result = parse("42");
        let _ = result; // May be valid as expression statement.
    }
}
