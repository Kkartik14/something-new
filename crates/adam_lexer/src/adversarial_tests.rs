//! Adversarial tests for the lexer — input that should break, crash, or confuse.

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;

    // -----------------------------------------------------------------------
    // Null bytes and control characters
    // -----------------------------------------------------------------------

    #[test]
    fn test_null_byte_in_source() {
        let source = "fn main\0() {}";
        let result = Lexer::new(source).tokenize();
        // Should not panic — either produce an error or skip the null.
        let _ = result;
    }

    #[test]
    fn test_all_control_characters() {
        for byte in 0u8..32 {
            if byte == b'\n' || byte == b'\r' || byte == b'\t' {
                continue; // These are valid whitespace.
            }
            let source = format!("fn x{}", byte as char);
            let result = Lexer::new(&source).tokenize();
            let _ = result; // Should not panic.
        }
    }

    // -----------------------------------------------------------------------
    // Extremely long inputs
    // -----------------------------------------------------------------------

    #[test]
    fn test_very_long_identifier() {
        let name: String = "a".repeat(100_000);
        let source = format!("let {} = 42", name);
        let result = Lexer::new(&source).tokenize();
        assert!(result.errors.is_empty(), "long identifier should lex");
    }

    #[test]
    fn test_very_long_string_literal() {
        let content: String = "x".repeat(100_000);
        let source = format!("\"{}\"", content);
        let result = Lexer::new(&source).tokenize();
        assert!(result.errors.is_empty(), "long string should lex");
    }

    #[test]
    fn test_very_long_number() {
        let num: String = "9".repeat(100_000);
        let source = num;
        let result = Lexer::new(&source).tokenize();
        // Should either succeed or produce a "number too large" error — not panic.
        let _ = result;
    }

    #[test]
    fn test_many_tokens() {
        // 100,000 simple tokens.
        let source: String = (0..100_000).map(|_| "x ").collect();
        let result = Lexer::new(&source).tokenize();
        assert!(result.tokens.len() >= 100_000);
    }

    // -----------------------------------------------------------------------
    // Unicode edge cases
    // -----------------------------------------------------------------------

    #[test]
    fn test_unicode_identifiers() {
        let source = "let café = 42";
        let result = Lexer::new(source).tokenize();
        let _ = result; // Should not panic.
    }

    #[test]
    fn test_emoji_in_string() {
        let source = r#""hello 🌍🔥💯""#;
        let result = Lexer::new(source).tokenize();
        assert!(result.errors.is_empty(), "emoji in string should be valid");
    }

    #[test]
    fn test_zero_width_characters() {
        let source = "let x\u{200B}y = 42"; // Zero-width space.
        let result = Lexer::new(source).tokenize();
        let _ = result; // Should not panic.
    }

    #[test]
    fn test_bom_at_start() {
        let source = "\u{FEFF}fn main() {}"; // BOM.
        let result = Lexer::new(source).tokenize();
        let _ = result; // Should handle gracefully.
    }

    #[test]
    fn test_mixed_line_endings() {
        let source = "fn a() {}\r\nfn b() {}\rfn c() {}\n";
        let result = Lexer::new(source).tokenize();
        let _ = result; // Should handle all line ending styles.
    }

    // -----------------------------------------------------------------------
    // String edge cases
    // -----------------------------------------------------------------------

    #[test]
    fn test_unterminated_string() {
        let source = "\"hello";
        let result = Lexer::new(source).tokenize();
        assert!(!result.errors.is_empty(), "unterminated string should error");
    }

    #[test]
    fn test_string_with_newline() {
        let source = "\"hello\nworld\"";
        let result = Lexer::new(source).tokenize();
        // Depends on language spec: multi-line strings or error.
        let _ = result;
    }

    #[test]
    fn test_nested_string_interpolation() {
        let source = r#""outer \(inner \(deep))" "#;
        let result = Lexer::new(source).tokenize();
        let _ = result; // Should not panic on nested interpolation.
    }

    #[test]
    fn test_escape_sequences() {
        let source = r#""\n\t\r\\\"\0\x41\u{1F600}""#;
        let result = Lexer::new(source).tokenize();
        let _ = result;
    }

    #[test]
    fn test_invalid_escape_sequence() {
        let source = r#""\q""#;
        let result = Lexer::new(source).tokenize();
        // Should produce an error for invalid escape.
        let _ = result;
    }

    // -----------------------------------------------------------------------
    // Comment edge cases
    // -----------------------------------------------------------------------

    #[test]
    fn test_deeply_nested_block_comments() {
        let source = "/* /* /* /* /* deep */ */ */ */ */";
        let result = Lexer::new(source).tokenize();
        let _ = result; // Should handle nested comments.
    }

    #[test]
    fn test_unterminated_block_comment() {
        let source = "/* this never ends";
        let result = Lexer::new(source).tokenize();
        assert!(!result.errors.is_empty(), "unterminated comment should error");
    }

    #[test]
    fn test_comment_with_string_inside() {
        let source = "// this has a \"string\" in a comment\nlet x = 42";
        let result = Lexer::new(source).tokenize();
        assert!(result.errors.is_empty());
    }

    // -----------------------------------------------------------------------
    // Number edge cases
    // -----------------------------------------------------------------------

    #[test]
    fn test_number_overflow_i64() {
        let source = "99999999999999999999999999999999999999999999";
        let result = Lexer::new(source).tokenize();
        let _ = result; // Should not panic.
    }

    #[test]
    fn test_hex_literal() {
        let source = "0xFF 0x00 0xDEADBEEF";
        let result = Lexer::new(source).tokenize();
        let _ = result;
    }

    #[test]
    fn test_binary_literal() {
        let source = "0b1010 0b0000 0b1111_0000";
        let result = Lexer::new(source).tokenize();
        let _ = result;
    }

    #[test]
    fn test_float_edge_cases() {
        let source = "0.0 1e10 1.5e-3 .5 1. 1e999";
        let result = Lexer::new(source).tokenize();
        let _ = result; // Should not panic on any of these.
    }

    // -----------------------------------------------------------------------
    // Operator edge cases
    // -----------------------------------------------------------------------

    #[test]
    fn test_many_operators_in_sequence() {
        let source = "+-*/%&&||!==!=<<=>>=...";
        let result = Lexer::new(source).tokenize();
        let _ = result; // Should tokenize without panic.
    }

    #[test]
    fn test_only_symbols() {
        let source = "{}()[]<>,.;:=+-*/&|^~!?@#$";
        let result = Lexer::new(source).tokenize();
        let _ = result;
    }

    // -----------------------------------------------------------------------
    // Degenerate inputs
    // -----------------------------------------------------------------------

    #[test]
    fn test_empty_input() {
        let result = Lexer::new("").tokenize();
        assert!(result.errors.is_empty());
    }

    #[test]
    fn test_only_whitespace() {
        let result = Lexer::new("   \n\n\t\t  \n").tokenize();
        assert!(result.errors.is_empty());
    }

    #[test]
    fn test_only_newlines() {
        let source: String = "\n".repeat(100_000);
        let result = Lexer::new(&source).tokenize();
        assert!(result.errors.is_empty());
    }

    #[test]
    fn test_alternating_spaces_and_tokens() {
        let source: String = (0..10_000).map(|_| " x").collect();
        let result = Lexer::new(&source).tokenize();
        assert!(result.tokens.len() >= 10_000);
    }

    // -----------------------------------------------------------------------
    // Pathological patterns
    // -----------------------------------------------------------------------

    #[test]
    fn test_deeply_nested_parens() {
        let depth = 1000;
        let opens: String = "(".repeat(depth);
        let closes: String = ")".repeat(depth);
        let source = format!("{}x{}", opens, closes);
        let result = Lexer::new(&source).tokenize();
        // Lexer shouldn't care about nesting — just tokenizes.
        assert!(result.tokens.len() >= depth * 2 + 1);
    }

    #[test]
    fn test_many_keywords_in_sequence() {
        let keywords = vec!["fn", "let", "if", "else", "for", "while", "return"];
        let source: String = keywords.iter().cycle().take(10_000)
            .map(|k| format!("{} ", k)).collect();
        let result = Lexer::new(&source).tokenize();
        assert!(result.tokens.len() >= 10_000);
    }

    #[test]
    fn test_source_at_exactly_power_of_2_boundary() {
        // Test at buffer boundaries that might trigger off-by-one errors.
        for power in [256, 512, 1024, 4096, 8192] {
            let source: String = "x".repeat(power);
            let result = Lexer::new(&source).tokenize();
            let _ = result;
        }
    }
}
