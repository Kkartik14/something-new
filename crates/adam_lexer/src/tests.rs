//! Comprehensive lexer test suite.

use crate::token::*;
use crate::lexer::Lexer;

/// Helper: tokenize source and return token kinds (excluding Eof).
fn kinds(source: &str) -> Vec<TokenKind> {
    let result = Lexer::new(source).tokenize();
    result
        .tokens
        .into_iter()
        .filter(|t| t.kind != TokenKind::Eof)
        .map(|t| t.kind)
        .collect()
}

/// Helper: tokenize and return (kind, line, column) tuples (excluding Eof).
fn kinds_with_loc(source: &str) -> Vec<(TokenKind, u32, u32)> {
    let result = Lexer::new(source).tokenize();
    result
        .tokens
        .into_iter()
        .filter(|t| t.kind != TokenKind::Eof)
        .map(|t| (t.kind, t.line, t.column))
        .collect()
}

/// Helper: tokenize and return errors.
fn errors(source: &str) -> Vec<LexError> {
    Lexer::new(source).tokenize().errors
}

// ========================================================
// Empty and trivial inputs
// ========================================================

#[test]
fn test_empty_file() {
    let result = Lexer::new("").tokenize();
    assert_eq!(result.tokens.len(), 1);
    assert_eq!(result.tokens[0].kind, TokenKind::Eof);
    assert!(result.errors.is_empty());
}

#[test]
fn test_only_whitespace() {
    let result = Lexer::new("   \t  \t  ").tokenize();
    assert_eq!(result.tokens.len(), 1);
    assert_eq!(result.tokens[0].kind, TokenKind::Eof);
}

#[test]
fn test_only_newlines() {
    let result = Lexer::new("\n\n\n").tokenize();
    // No tokens end statements, so newlines are not significant
    assert_eq!(result.tokens.len(), 1);
    assert_eq!(result.tokens[0].kind, TokenKind::Eof);
}

#[test]
fn test_only_comments() {
    let result = Lexer::new("// this is a comment\n// another").tokenize();
    assert_eq!(result.tokens.len(), 1);
    assert_eq!(result.tokens[0].kind, TokenKind::Eof);
}

// ========================================================
// Keywords
// ========================================================

#[test]
fn test_all_keywords() {
    let source = "fn struct enum trait impl view let mut own pub use mod if else match for while loop break continue return spawn select after chan true false nil self Self";
    let k = kinds(source);
    assert_eq!(k, vec![
        TokenKind::Fn, TokenKind::Struct, TokenKind::Enum, TokenKind::Trait,
        TokenKind::Impl, TokenKind::View, TokenKind::Let, TokenKind::Mut,
        TokenKind::Own, TokenKind::Pub, TokenKind::Use, TokenKind::Mod,
        TokenKind::If, TokenKind::Else, TokenKind::Match, TokenKind::For,
        TokenKind::While, TokenKind::Loop, TokenKind::Break, TokenKind::Continue,
        TokenKind::Return, TokenKind::Spawn, TokenKind::Select, TokenKind::After,
        TokenKind::Chan, TokenKind::True, TokenKind::False, TokenKind::Nil,
        TokenKind::SelfValue, TokenKind::SelfType,
        TokenKind::Newline,
    ]);
}

#[test]
fn test_keyword_prefix_is_identifier() {
    // "fnord" should be identifier, not fn + ord
    let k = kinds("fnord");
    assert_eq!(k, vec![TokenKind::Identifier("fnord".to_string()), TokenKind::Newline]);
}

#[test]
fn test_keyword_suffix_is_identifier() {
    let k = kinds("format");
    assert_eq!(k, vec![TokenKind::Identifier("format".to_string()), TokenKind::Newline]);
}

#[test]
fn test_keyword_case_sensitive() {
    // "Fn" is an identifier, not keyword fn
    let k = kinds("Fn Struct IF");
    assert_eq!(k, vec![
        TokenKind::Identifier("Fn".to_string()),
        TokenKind::Identifier("Struct".to_string()),
        TokenKind::Identifier("IF".to_string()),
        TokenKind::Newline,
    ]);
}

// ========================================================
// Identifiers
// ========================================================

#[test]
fn test_simple_identifiers() {
    let k = kinds("foo bar baz");
    assert_eq!(k, vec![
        TokenKind::Identifier("foo".to_string()),
        TokenKind::Identifier("bar".to_string()),
        TokenKind::Identifier("baz".to_string()),
        TokenKind::Newline,
    ]);
}

#[test]
fn test_underscore_identifiers() {
    let k = kinds("_ _foo foo_bar foo_");
    assert_eq!(k, vec![
        TokenKind::Identifier("_".to_string()),
        TokenKind::Identifier("_foo".to_string()),
        TokenKind::Identifier("foo_bar".to_string()),
        TokenKind::Identifier("foo_".to_string()),
        TokenKind::Newline,
    ]);
}

#[test]
fn test_identifier_with_numbers() {
    let k = kinds("x1 vec3 point2d");
    assert_eq!(k, vec![
        TokenKind::Identifier("x1".to_string()),
        TokenKind::Identifier("vec3".to_string()),
        TokenKind::Identifier("point2d".to_string()),
        TokenKind::Newline,
    ]);
}

#[test]
fn test_single_char_identifier() {
    let k = kinds("x");
    assert_eq!(k, vec![TokenKind::Identifier("x".to_string()), TokenKind::Newline]);
}

// ========================================================
// Integer literals
// ========================================================

#[test]
fn test_decimal_integers() {
    let k = kinds("0 1 42 1000 999999");
    assert_eq!(k, vec![
        TokenKind::IntLiteral(0), TokenKind::IntLiteral(1),
        TokenKind::IntLiteral(42), TokenKind::IntLiteral(1000),
        TokenKind::IntLiteral(999999), TokenKind::Newline,
    ]);
}

#[test]
fn test_integer_with_underscores() {
    let k = kinds("1_000_000 1_0");
    assert_eq!(k, vec![
        TokenKind::IntLiteral(1_000_000),
        TokenKind::IntLiteral(10),
        TokenKind::Newline,
    ]);
}

#[test]
fn test_hex_integers() {
    let k = kinds("0xFF 0x00 0xDEAD_BEEF");
    assert_eq!(k, vec![
        TokenKind::IntLiteral(0xFF),
        TokenKind::IntLiteral(0x00),
        TokenKind::IntLiteral(0xDEAD_BEEF),
        TokenKind::Newline,
    ]);
}

#[test]
fn test_octal_integers() {
    let k = kinds("0o77 0o0 0o755");
    assert_eq!(k, vec![
        TokenKind::IntLiteral(0o77),
        TokenKind::IntLiteral(0o0),
        TokenKind::IntLiteral(0o755),
        TokenKind::Newline,
    ]);
}

#[test]
fn test_binary_integers() {
    let k = kinds("0b1010 0b0 0b1111_0000");
    assert_eq!(k, vec![
        TokenKind::IntLiteral(0b1010),
        TokenKind::IntLiteral(0b0),
        TokenKind::IntLiteral(0b1111_0000),
        TokenKind::Newline,
    ]);
}

#[test]
fn test_hex_no_digits_error() {
    let e = errors("0x");
    assert!(!e.is_empty());
    assert!(e[0].message.contains("hex digits"));
}

#[test]
fn test_octal_no_digits_error() {
    let e = errors("0o");
    assert!(!e.is_empty());
    assert!(e[0].message.contains("octal digits"));
}

#[test]
fn test_binary_no_digits_error() {
    let e = errors("0b");
    assert!(!e.is_empty());
    assert!(e[0].message.contains("binary digits"));
}

// ========================================================
// Float literals
// ========================================================

#[test]
fn test_simple_floats() {
    let k = kinds("3.14 0.5 100.0");
    assert_eq!(k, vec![
        TokenKind::FloatLiteral(3.14),
        TokenKind::FloatLiteral(0.5),
        TokenKind::FloatLiteral(100.0),
        TokenKind::Newline,
    ]);
}

#[test]
fn test_float_with_underscores() {
    let k = kinds("1_000.5");
    assert_eq!(k, vec![TokenKind::FloatLiteral(1000.5), TokenKind::Newline]);
}

#[test]
fn test_float_scientific() {
    let k = kinds("1.0e10 1.5e-3 2.0E+5");
    assert_eq!(k, vec![
        TokenKind::FloatLiteral(1.0e10),
        TokenKind::FloatLiteral(1.5e-3),
        TokenKind::FloatLiteral(2.0e5),
        TokenKind::Newline,
    ]);
}

#[test]
fn test_integer_scientific_is_float() {
    let k = kinds("1e10");
    assert_eq!(k, vec![TokenKind::FloatLiteral(1e10), TokenKind::Newline]);
}

#[test]
fn test_dot_after_int_is_not_float() {
    // "5.method" should be int 5, dot, identifier method
    let k = kinds("5.method");
    assert_eq!(k, vec![
        TokenKind::IntLiteral(5),
        TokenKind::Dot,
        TokenKind::Identifier("method".to_string()),
        TokenKind::Newline,
    ]);
}

#[test]
fn test_zero() {
    let k = kinds("0");
    assert_eq!(k, vec![TokenKind::IntLiteral(0), TokenKind::Newline]);
}

// ========================================================
// String literals
// ========================================================

#[test]
fn test_simple_string() {
    let k = kinds(r#""hello""#);
    assert_eq!(k, vec![TokenKind::StringLiteral("hello".to_string()), TokenKind::Newline]);
}

#[test]
fn test_empty_string() {
    let k = kinds(r#""""#);
    assert_eq!(k, vec![TokenKind::StringLiteral("".to_string()), TokenKind::Newline]);
}

#[test]
fn test_string_escapes() {
    let k = kinds(r#""\n\t\r\\\"\0""#);
    assert_eq!(k, vec![
        TokenKind::StringLiteral("\n\t\r\\\"\0".to_string()),
        TokenKind::Newline,
    ]);
}

#[test]
fn test_string_escaped_brace() {
    let k = kinds(r#""\{not interpolation\}""#);
    assert_eq!(k, vec![
        TokenKind::StringLiteral("{not interpolation}".to_string()),
        TokenKind::Newline,
    ]);
}

#[test]
fn test_string_unicode_escape() {
    let k = kinds(r#""\u{1F600}""#);
    assert_eq!(k, vec![
        TokenKind::StringLiteral("😀".to_string()),
        TokenKind::Newline,
    ]);
}

#[test]
fn test_string_interpolation_simple() {
    // "hello {name}" should produce:
    // StringLiteral("hello "), StringInterpolStart, Identifier("name"), StringInterpolEnd, StringLiteral(""), ...
    // Actually our lexer does: StringLiteral("hello ") then on next call { triggers interp start
    let result = Lexer::new(r#""hello {name}""#).tokenize();
    let k: Vec<TokenKind> = result.tokens.iter().map(|t| t.kind.clone()).collect();

    // Check that we get the interpolation tokens
    assert!(k.contains(&TokenKind::StringInterpolStart));
    assert!(k.contains(&TokenKind::StringInterpolEnd));
    assert!(k.contains(&TokenKind::Identifier("name".to_string())));
}

#[test]
fn test_string_interpolation_at_start() {
    // "{x}" — interpolation at very start of string
    let result = Lexer::new(r#""{x}""#).tokenize();
    let k: Vec<TokenKind> = result.tokens.iter().map(|t| t.kind.clone()).collect();
    assert!(k.contains(&TokenKind::StringInterpolStart));
    assert!(k.contains(&TokenKind::Identifier("x".to_string())));
    assert!(k.contains(&TokenKind::StringInterpolEnd));
}

#[test]
fn test_unterminated_string() {
    let e = errors(r#""hello"#);
    assert!(!e.is_empty());
    assert!(e[0].message.contains("unterminated"));
}

#[test]
fn test_invalid_escape() {
    let e = errors(r#""\q""#);
    assert!(!e.is_empty());
    assert!(e[0].message.contains("invalid escape"));
}

// ========================================================
// Char literals
// ========================================================

#[test]
fn test_simple_char() {
    let k = kinds("'a'");
    assert_eq!(k, vec![TokenKind::CharLiteral('a'), TokenKind::Newline]);
}

#[test]
fn test_char_escape() {
    let k = kinds(r"'\n'");
    assert_eq!(k, vec![TokenKind::CharLiteral('\n'), TokenKind::Newline]);
}

#[test]
fn test_char_backslash() {
    let k = kinds(r"'\\'");
    assert_eq!(k, vec![TokenKind::CharLiteral('\\'), TokenKind::Newline]);
}

#[test]
fn test_char_single_quote() {
    let k = kinds(r"'\''");
    assert_eq!(k, vec![TokenKind::CharLiteral('\''), TokenKind::Newline]);
}

#[test]
fn test_char_unicode_escape() {
    let k = kinds(r"'\u{41}'");
    assert_eq!(k, vec![TokenKind::CharLiteral('A'), TokenKind::Newline]);
}

#[test]
fn test_unterminated_char() {
    let e = errors("'a");
    assert!(!e.is_empty());
}

// ========================================================
// Operators
// ========================================================

#[test]
fn test_single_char_operators() {
    // @ doesn't end a statement, so no trailing Newline
    let k = kinds("+ - * / % < > = ! & . ? @");
    assert_eq!(k, vec![
        TokenKind::Plus, TokenKind::Minus, TokenKind::Star, TokenKind::Slash,
        TokenKind::Percent, TokenKind::Lt, TokenKind::Gt, TokenKind::Assign,
        TokenKind::Not, TokenKind::Ampersand, TokenKind::Dot, TokenKind::Question,
        TokenKind::At,
    ]);
}

#[test]
fn test_double_char_operators() {
    let k = kinds("== != <= >= && || -> => := ..");
    assert_eq!(k, vec![
        TokenKind::Eq, TokenKind::NotEq, TokenKind::LtEq, TokenKind::GtEq,
        TokenKind::And, TokenKind::Or, TokenKind::Arrow, TokenKind::FatArrow,
        TokenKind::ColonAssign, TokenKind::DotDot,
    ]);
}

#[test]
fn test_compound_assignment() {
    let k = kinds("+= -= *= /= %=");
    assert_eq!(k, vec![
        TokenKind::PlusAssign, TokenKind::MinusAssign, TokenKind::StarAssign,
        TokenKind::SlashAssign, TokenKind::PercentAssign,
    ]);
}

#[test]
fn test_pipe_operator() {
    let k = kinds("| ||");
    assert_eq!(k, vec![TokenKind::Pipe, TokenKind::Or]);
}

// ========================================================
// Delimiters
// ========================================================

#[test]
fn test_delimiters() {
    let k = kinds("( ) { } [ ] , : ;");
    assert_eq!(k, vec![
        TokenKind::LParen, TokenKind::RParen,
        TokenKind::LBrace, TokenKind::RBrace,
        TokenKind::LBracket, TokenKind::RBracket,
        TokenKind::Comma, TokenKind::Colon, TokenKind::Semicolon,
    ]);
}

// ========================================================
// Significant newlines (Go-style rules)
// ========================================================

#[test]
fn test_newline_after_identifier() {
    let k = kinds("foo\nbar");
    assert!(k.contains(&TokenKind::Newline));
}

#[test]
fn test_newline_after_literal() {
    let k = kinds("42\n\"hello\"");
    assert!(k.contains(&TokenKind::Newline));
}

#[test]
fn test_newline_after_rparen() {
    let k = kinds(")\nfoo");
    assert!(k.contains(&TokenKind::Newline));
}

#[test]
fn test_newline_after_rbrace() {
    let k = kinds("}\nfoo");
    assert!(k.contains(&TokenKind::Newline));
}

#[test]
fn test_newline_after_rbracket() {
    let k = kinds("]\nfoo");
    assert!(k.contains(&TokenKind::Newline));
}

#[test]
fn test_no_newline_after_operator() {
    // Newline after + is not significant
    let k = kinds("a +\nb");
    assert_eq!(k, vec![
        TokenKind::Identifier("a".to_string()),
        TokenKind::Plus,
        TokenKind::Identifier("b".to_string()),
        TokenKind::Newline,
    ]);
}

#[test]
fn test_no_newline_after_comma() {
    let k = kinds("a,\nb");
    assert_eq!(k, vec![
        TokenKind::Identifier("a".to_string()),
        TokenKind::Comma,
        TokenKind::Identifier("b".to_string()),
        TokenKind::Newline,
    ]);
}

#[test]
fn test_no_newline_after_lparen() {
    let k = kinds("(\na");
    assert_eq!(k, vec![
        TokenKind::LParen,
        TokenKind::Identifier("a".to_string()),
        TokenKind::Newline,
    ]);
}

#[test]
fn test_no_newline_after_lbrace() {
    let k = kinds("{\na");
    assert_eq!(k, vec![
        TokenKind::LBrace,
        TokenKind::Identifier("a".to_string()),
        TokenKind::Newline,
    ]);
}

#[test]
fn test_newline_after_return() {
    let k = kinds("return\nfoo");
    assert!(k.contains(&TokenKind::Newline));
    // Should be: return, newline, foo, newline
    assert_eq!(k[0], TokenKind::Return);
    assert_eq!(k[1], TokenKind::Newline);
}

#[test]
fn test_newline_after_break() {
    let k = kinds("break\ncontinue");
    assert_eq!(k[0], TokenKind::Break);
    assert_eq!(k[1], TokenKind::Newline);
    assert_eq!(k[2], TokenKind::Continue);
}

#[test]
fn test_newline_after_true_false_nil() {
    let k = kinds("true\nfalse\nnil");
    assert!(k.contains(&TokenKind::True));
    assert!(k.contains(&TokenKind::False));
    assert!(k.contains(&TokenKind::Nil));
    // Should have newlines between them
    let newline_count = k.iter().filter(|t| **t == TokenKind::Newline).count();
    assert_eq!(newline_count, 3); // after true, after false, after nil
}

#[test]
fn test_multiple_newlines_collapsed() {
    // Multiple newlines after a statement-ending token = just one
    let k = kinds("foo\n\n\nbar");
    let newline_count = k.iter().filter(|t| **t == TokenKind::Newline).count();
    assert_eq!(newline_count, 2); // after foo, after bar
}

// ========================================================
// Comments
// ========================================================

#[test]
fn test_line_comment() {
    let k = kinds("foo // this is a comment\nbar");
    assert_eq!(k, vec![
        TokenKind::Identifier("foo".to_string()),
        TokenKind::Newline,
        TokenKind::Identifier("bar".to_string()),
        TokenKind::Newline,
    ]);
}

#[test]
fn test_block_comment() {
    let k = kinds("foo /* comment */ bar");
    assert_eq!(k, vec![
        TokenKind::Identifier("foo".to_string()),
        TokenKind::Identifier("bar".to_string()),
        TokenKind::Newline,
    ]);
}

#[test]
fn test_nested_block_comment() {
    let k = kinds("foo /* outer /* inner */ still comment */ bar");
    assert_eq!(k, vec![
        TokenKind::Identifier("foo".to_string()),
        TokenKind::Identifier("bar".to_string()),
        TokenKind::Newline,
    ]);
}

#[test]
fn test_unterminated_block_comment() {
    let e = errors("foo /* never closed");
    assert!(!e.is_empty());
    assert!(e[0].message.contains("unterminated block comment"));
}

// ========================================================
// Error recovery
// ========================================================

#[test]
fn test_unknown_character_recovery() {
    // Unknown character `~` should produce error but continue
    let result = Lexer::new("foo ~ bar").tokenize();
    assert!(!result.errors.is_empty());
    // Should still get foo and bar tokens
    let idents: Vec<_> = result.tokens.iter().filter(|t| matches!(&t.kind, TokenKind::Identifier(_))).collect();
    assert_eq!(idents.len(), 2);
}

#[test]
fn test_multiple_errors() {
    let result = Lexer::new("~ ^ `").tokenize();
    // Should have 3 errors, one for each unknown character
    assert_eq!(result.errors.len(), 3);
}

// ========================================================
// Source locations
// ========================================================

#[test]
fn test_first_token_location() {
    let result = Lexer::new("foo").tokenize();
    let tok = &result.tokens[0];
    assert_eq!(tok.line, 1);
    assert_eq!(tok.column, 1);
    assert_eq!(tok.span.start, 0);
    assert_eq!(tok.span.end, 3);
}

#[test]
fn test_second_line_location() {
    let k = kinds_with_loc("foo\nbar");
    // bar should be on line 2, column 1
    let bar = k.iter().find(|(kind, _, _)| *kind == TokenKind::Identifier("bar".to_string())).unwrap();
    assert_eq!(bar.1, 2); // line
    assert_eq!(bar.2, 1); // column
}

#[test]
fn test_column_tracking() {
    let k = kinds_with_loc("a b c");
    assert_eq!(k[0].2, 1); // a at column 1
    assert_eq!(k[1].2, 3); // b at column 3
    assert_eq!(k[2].2, 5); // c at column 5
}

// ========================================================
// Complex expressions
// ========================================================

#[test]
fn test_function_declaration() {
    let k = kinds("fn add(a i32, b i32) -> i32 {");
    assert_eq!(k, vec![
        TokenKind::Fn,
        TokenKind::Identifier("add".to_string()),
        TokenKind::LParen,
        TokenKind::Identifier("a".to_string()),
        TokenKind::Identifier("i32".to_string()),
        TokenKind::Comma,
        TokenKind::Identifier("b".to_string()),
        TokenKind::Identifier("i32".to_string()),
        TokenKind::RParen,
        TokenKind::Arrow,
        TokenKind::Identifier("i32".to_string()),
        TokenKind::LBrace,
    ]);
}

#[test]
fn test_variable_declaration() {
    let k = kinds("x := 42");
    assert_eq!(k, vec![
        TokenKind::Identifier("x".to_string()),
        TokenKind::ColonAssign,
        TokenKind::IntLiteral(42),
        TokenKind::Newline,
    ]);
}

#[test]
fn test_mutable_variable() {
    let k = kinds("mut count := 0");
    assert_eq!(k, vec![
        TokenKind::Mut,
        TokenKind::Identifier("count".to_string()),
        TokenKind::ColonAssign,
        TokenKind::IntLiteral(0),
        TokenKind::Newline,
    ]);
}

#[test]
fn test_typed_variable() {
    let k = kinds("name: String = \"adam\"");
    assert_eq!(k, vec![
        TokenKind::Identifier("name".to_string()),
        TokenKind::Colon,
        TokenKind::Identifier("String".to_string()),
        TokenKind::Assign,
        TokenKind::StringLiteral("adam".to_string()),
        TokenKind::Newline,
    ]);
}

#[test]
fn test_method_chain() {
    let k = kinds("x.foo().bar(1, 2)");
    assert_eq!(k, vec![
        TokenKind::Identifier("x".to_string()),
        TokenKind::Dot,
        TokenKind::Identifier("foo".to_string()),
        TokenKind::LParen,
        TokenKind::RParen,
        TokenKind::Dot,
        TokenKind::Identifier("bar".to_string()),
        TokenKind::LParen,
        TokenKind::IntLiteral(1),
        TokenKind::Comma,
        TokenKind::IntLiteral(2),
        TokenKind::RParen,
        TokenKind::Newline,
    ]);
}

#[test]
fn test_struct_definition() {
    let source = "struct Point {\n    x f64\n    y f64\n}";
    let k = kinds(source);
    assert_eq!(k, vec![
        TokenKind::Struct,
        TokenKind::Identifier("Point".to_string()),
        TokenKind::LBrace,
        TokenKind::Identifier("x".to_string()),
        TokenKind::Identifier("f64".to_string()),
        TokenKind::Newline,
        TokenKind::Identifier("y".to_string()),
        TokenKind::Identifier("f64".to_string()),
        TokenKind::Newline,
        TokenKind::RBrace,
        TokenKind::Newline,
    ]);
}

#[test]
fn test_match_expression() {
    let source = "match shape {\n    Circle(r) => r * r\n}";
    let k = kinds(source);
    assert!(k.contains(&TokenKind::Match));
    assert!(k.contains(&TokenKind::FatArrow));
    assert!(k.contains(&TokenKind::Star));
}

#[test]
fn test_spawn_channel() {
    let k = kinds("spawn { ch.send(42) }");
    assert!(k.contains(&TokenKind::Spawn));
    assert!(k.contains(&TokenKind::IntLiteral(42)));
}

#[test]
fn test_view_syntax() {
    let source = "view Counter {\n    @state count: i32 = 0\n}";
    let k = kinds(source);
    assert!(k.contains(&TokenKind::View));
    assert!(k.contains(&TokenKind::At));
    assert!(k.contains(&TokenKind::Identifier("state".to_string())));
    assert!(k.contains(&TokenKind::Identifier("count".to_string())));
}

#[test]
fn test_generic_syntax() {
    let k = kinds("fn max[T: Comparable](a T, b T) -> T");
    assert!(k.contains(&TokenKind::LBracket));
    assert!(k.contains(&TokenKind::RBracket));
    assert!(k.contains(&TokenKind::Colon));
}

#[test]
fn test_result_type_syntax() {
    // T ! E — bang is lexed as Not
    let k = kinds("String ! IoError");
    assert!(k.contains(&TokenKind::Not));
}

#[test]
fn test_optional_type() {
    let k = kinds("?String");
    assert_eq!(k[0], TokenKind::Question);
    assert_eq!(k[1], TokenKind::Identifier("String".to_string()));
}

#[test]
fn test_try_operator() {
    let k = kinds("foo()?");
    assert!(k.contains(&TokenKind::Question));
}

#[test]
fn test_range_operator() {
    let k = kinds("0..10");
    assert!(k.contains(&TokenKind::DotDot));
    assert!(k.contains(&TokenKind::IntLiteral(0)));
    assert!(k.contains(&TokenKind::IntLiteral(10)));
}

#[test]
fn test_colon_assign_vs_colon() {
    let k = kinds("x := 5");
    assert!(k.contains(&TokenKind::ColonAssign));

    let k2 = kinds("x: i32");
    assert!(k2.contains(&TokenKind::Colon));
}

// ========================================================
// TokenKind methods
// ========================================================

#[test]
fn test_ends_statement() {
    assert!(TokenKind::Identifier("x".to_string()).ends_statement());
    assert!(TokenKind::IntLiteral(5).ends_statement());
    assert!(TokenKind::RParen.ends_statement());
    assert!(TokenKind::RBrace.ends_statement());
    assert!(TokenKind::Return.ends_statement());
    assert!(TokenKind::Break.ends_statement());
    assert!(TokenKind::Continue.ends_statement());
    assert!(TokenKind::True.ends_statement());
    assert!(TokenKind::Question.ends_statement());

    assert!(!TokenKind::Plus.ends_statement());
    assert!(!TokenKind::LParen.ends_statement());
    assert!(!TokenKind::Comma.ends_statement());
    assert!(!TokenKind::LBrace.ends_statement());
    assert!(!TokenKind::Fn.ends_statement());
}

#[test]
fn test_is_keyword() {
    assert!(TokenKind::Fn.is_keyword());
    assert!(TokenKind::Struct.is_keyword());
    assert!(TokenKind::True.is_keyword());
    assert!(TokenKind::Nil.is_keyword());
    assert!(TokenKind::SelfValue.is_keyword());

    assert!(!TokenKind::Plus.is_keyword());
    assert!(!TokenKind::Identifier("foo".to_string()).is_keyword());
    assert!(!TokenKind::IntLiteral(5).is_keyword());
}

#[test]
fn test_token_display() {
    assert_eq!(format!("{}", TokenKind::Fn), "`fn`");
    assert_eq!(format!("{}", TokenKind::Plus), "`+`");
    assert_eq!(format!("{}", TokenKind::Eof), "end of file");
    assert_eq!(format!("{}", TokenKind::Identifier("foo".to_string())), "identifier `foo`");
    assert_eq!(format!("{}", TokenKind::IntLiteral(42)), "integer `42`");
}

// ========================================================
// Edge cases
// ========================================================

#[test]
fn test_adjacent_operators() {
    let k = kinds("a+b");
    assert_eq!(k, vec![
        TokenKind::Identifier("a".to_string()),
        TokenKind::Plus,
        TokenKind::Identifier("b".to_string()),
        TokenKind::Newline,
    ]);
}

#[test]
fn test_negative_number_is_two_tokens() {
    // -5 is Minus then IntLiteral, not a negative literal
    let k = kinds("-5");
    assert_eq!(k, vec![TokenKind::Minus, TokenKind::IntLiteral(5), TokenKind::Newline]);
}

#[test]
fn test_arrow_vs_minus_gt() {
    let k = kinds("->");
    assert_eq!(k, vec![TokenKind::Arrow]);
}

#[test]
fn test_fat_arrow() {
    let k = kinds("=>");
    assert_eq!(k, vec![TokenKind::FatArrow]);
}

#[test]
fn test_eq_vs_assign() {
    let k = kinds("= ==");
    assert_eq!(k, vec![TokenKind::Assign, TokenKind::Eq]);
}

#[test]
fn test_not_vs_not_eq() {
    let k = kinds("! !=");
    assert_eq!(k, vec![TokenKind::Not, TokenKind::NotEq]);
}

#[test]
fn test_and_vs_ampersand() {
    let k = kinds("& &&");
    assert_eq!(k, vec![TokenKind::Ampersand, TokenKind::And]);
}

// ========================================================
// Regression tests — guards against reintroduction of fixed bugs
// ========================================================

#[test]
fn regression_multi_part_string_interpolation_tokens() {
    // Bug: the lexer used Option<Token> for pending tokens. When a string
    // continuation (after first interpolation closes) hit another '{',
    // lex_string_continuation set pending to StringInterpolStart, but the
    // closing-brace handler then overwrote it with the StringLiteral
    // continuation. The second StringInterpolStart was lost.
    // Fix: changed pending from Option<Token> to Vec<Token>.
    let toks = kinds(r#""x={x} y={y}""#);

    // Expected token sequence:
    // StringLiteral("x="), StringInterpolStart, Identifier("x"),
    // StringInterpolEnd, StringLiteral(" y="), StringInterpolStart,
    // Identifier("y"), StringInterpolEnd
    assert!(
        toks.len() >= 8,
        "multi-part interpolation should produce at least 8 tokens, got {}: {:?}",
        toks.len(),
        toks
    );

    // Verify the critical token that was previously lost.
    let interp_start_count = toks
        .iter()
        .filter(|t| matches!(t, TokenKind::StringInterpolStart))
        .count();
    assert_eq!(
        interp_start_count, 2,
        "must have exactly 2 StringInterpolStart tokens (one per interpolation), got {}",
        interp_start_count
    );

    let interp_end_count = toks
        .iter()
        .filter(|t| matches!(t, TokenKind::StringInterpolEnd))
        .count();
    assert_eq!(
        interp_end_count, 2,
        "must have exactly 2 StringInterpolEnd tokens, got {}",
        interp_end_count
    );

    // Verify no lex errors.
    let errs = errors(r#""x={x} y={y}""#);
    assert!(
        errs.is_empty(),
        "multi-part interpolation should produce no lex errors, got: {:?}",
        errs
    );
}

#[test]
fn regression_three_part_string_interpolation_tokens() {
    // Stress test: three interpolation parts to ensure the Vec<Token> queue
    // handles arbitrary depth.
    let toks = kinds(r#""a={a} b={b} c={c}""#);

    let interp_start_count = toks
        .iter()
        .filter(|t| matches!(t, TokenKind::StringInterpolStart))
        .count();
    assert_eq!(
        interp_start_count, 3,
        "must have 3 StringInterpolStart tokens, got {}",
        interp_start_count
    );

    let interp_end_count = toks
        .iter()
        .filter(|t| matches!(t, TokenKind::StringInterpolEnd))
        .count();
    assert_eq!(
        interp_end_count, 3,
        "must have 3 StringInterpolEnd tokens, got {}",
        interp_end_count
    );

    let errs = errors(r#""a={a} b={b} c={c}""#);
    assert!(errs.is_empty(), "no lex errors: {:?}", errs);
}
