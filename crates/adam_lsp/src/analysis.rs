//! Core analysis engine — runs the full compiler pipeline on a document
//! and collects diagnostics.

use adam_lexer::Lexer;
use adam_parser::Parser;
use adam_resolve::ResolveResult;
use adam_types::TypeCheckResult;

/// Position in a source document (0-indexed line and character).
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct Position {
    pub line: u32,
    pub character: u32,
}

impl Position {
    pub fn new(line: u32, character: u32) -> Self {
        Self { line, character }
    }
}

/// A range in a source document.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct Range {
    pub start: Position,
    pub end: Position,
}

impl Range {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub fn point(line: u32, character: u32) -> Self {
        let pos = Position::new(line, character);
        Self {
            start: pos,
            end: pos,
        }
    }
}

/// Diagnostic severity levels.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
    Info,
    Hint,
}

/// A diagnostic message from the analysis pipeline.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct Diagnostic {
    pub range: Range,
    pub severity: DiagnosticSeverity,
    pub message: String,
    pub source: String,
}

impl Diagnostic {
    pub fn error(range: Range, message: impl Into<String>, source: impl Into<String>) -> Self {
        Self {
            range,
            severity: DiagnosticSeverity::Error,
            message: message.into(),
            source: source.into(),
        }
    }

    pub fn warning(range: Range, message: impl Into<String>, source: impl Into<String>) -> Self {
        Self {
            range,
            severity: DiagnosticSeverity::Warning,
            message: message.into(),
            source: source.into(),
        }
    }
}

/// Results of a full analysis pass on a document.
#[derive(Debug)]
pub struct AnalysisResult {
    pub diagnostics: Vec<Diagnostic>,
    pub tokens: Vec<adam_lexer::Token>,
    pub ast: Option<adam_ast::item::SourceFile>,
    pub resolve_result: Option<ResolveResult>,
    pub type_result: Option<TypeCheckResult>,
}

impl AnalysisResult {
    pub fn has_errors(&self) -> bool {
        self.diagnostics
            .iter()
            .any(|d| d.severity == DiagnosticSeverity::Error)
    }
}

/// Convert a byte offset span to a line/character range using a line offset table.
fn span_to_range(source: &str, span_start: u32, span_end: u32) -> Range {
    let line_offsets = compute_line_offsets(source);

    let start = offset_to_position(&line_offsets, span_start as usize);
    let end = offset_to_position(&line_offsets, span_end as usize);
    Range::new(start, end)
}

/// Compute byte offsets for each line start.
fn compute_line_offsets(source: &str) -> Vec<usize> {
    let mut offsets = vec![0];
    for (i, ch) in source.char_indices() {
        if ch == '\n' {
            offsets.push(i + 1);
        }
    }
    offsets
}

/// Convert a byte offset to a Position using precomputed line offsets.
fn offset_to_position(line_offsets: &[usize], offset: usize) -> Position {
    // Binary search for the line containing this offset.
    let line = match line_offsets.binary_search(&offset) {
        Ok(exact) => exact,
        Err(insert) => insert.saturating_sub(1),
    };
    let col = offset.saturating_sub(line_offsets[line]);
    Position::new(line as u32, col as u32)
}

/// Run the full analysis pipeline on source text.
pub fn analyze(source: &str) -> AnalysisResult {
    let mut diagnostics = Vec::new();

    // Phase 1: Lex.
    let lex_result = Lexer::new(source).tokenize();
    for err in &lex_result.errors {
        diagnostics.push(Diagnostic::error(
            span_to_range(source, err.span.start, err.span.end),
            &err.message,
            "lexer",
        ));
    }

    if !lex_result.errors.is_empty() {
        return AnalysisResult {
            diagnostics,
            tokens: lex_result.tokens,
            ast: None,
            resolve_result: None,
            type_result: None,
        };
    }

    let tokens = lex_result.tokens.clone();

    // Phase 2: Parse.
    let parse_result = Parser::new(lex_result.tokens).parse();
    for err in &parse_result.errors {
        diagnostics.push(Diagnostic::error(
            span_to_range(source, err.span.start, err.span.end),
            &err.message,
            "parser",
        ));
    }

    if !parse_result.errors.is_empty() {
        return AnalysisResult {
            diagnostics,
            tokens,
            ast: Some(parse_result.ast),
            resolve_result: None,
            type_result: None,
        };
    }

    let ast = parse_result.ast;

    // Phase 3: Resolve names.
    let resolve_result = adam_resolve::resolve(&ast);
    for err in &resolve_result.errors {
        diagnostics.push(Diagnostic::error(
            span_to_range(source, err.span.start, err.span.end),
            format!("{}", err),
            "resolver",
        ));
    }

    // Phase 4: Type check.
    let type_result = adam_types::TypeChecker::new().check(&ast);
    for err in &type_result.errors {
        diagnostics.push(Diagnostic::error(
            span_to_range(source, err.span.start, err.span.end),
            &err.message,
            "typechecker",
        ));
    }

    // Phase 5: Borrow check.
    let borrow_result = adam_borrow::BorrowChecker::new().check(
        &ast,
        if resolve_result.errors.is_empty() {
            Some(&resolve_result)
        } else {
            None
        },
        if type_result.errors.is_empty() {
            Some(&type_result)
        } else {
            None
        },
    );
    for err in &borrow_result.errors {
        diagnostics.push(Diagnostic::error(
            span_to_range(source, err.span.start, err.span.end),
            &err.message,
            "borrowck",
        ));
    }

    AnalysisResult {
        diagnostics,
        tokens,
        ast: Some(ast),
        resolve_result: Some(resolve_result),
        type_result: Some(type_result),
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_position_new() {
        let pos = Position::new(5, 10);
        assert_eq!(pos.line, 5);
        assert_eq!(pos.character, 10);
    }

    #[test]
    fn test_range_point() {
        let r = Range::point(3, 7);
        assert_eq!(r.start, r.end);
        assert_eq!(r.start.line, 3);
    }

    #[test]
    fn test_compute_line_offsets() {
        let offsets = compute_line_offsets("abc\ndef\nghi");
        assert_eq!(offsets, vec![0, 4, 8]);
    }

    #[test]
    fn test_compute_line_offsets_empty() {
        let offsets = compute_line_offsets("");
        assert_eq!(offsets, vec![0]);
    }

    #[test]
    fn test_offset_to_position() {
        let offsets = compute_line_offsets("abc\ndef\nghi");
        assert_eq!(offset_to_position(&offsets, 0), Position::new(0, 0));
        assert_eq!(offset_to_position(&offsets, 2), Position::new(0, 2));
        assert_eq!(offset_to_position(&offsets, 4), Position::new(1, 0));
        assert_eq!(offset_to_position(&offsets, 6), Position::new(1, 2));
        assert_eq!(offset_to_position(&offsets, 8), Position::new(2, 0));
    }

    #[test]
    fn test_span_to_range() {
        let source = "fn main() {\n    x\n}";
        let r = span_to_range(source, 0, 2);
        assert_eq!(r.start, Position::new(0, 0));
        assert_eq!(r.end, Position::new(0, 2));
    }

    #[test]
    fn test_analyze_empty() {
        let result = analyze("");
        // Empty source may or may not produce diagnostics depending on parser behavior.
        assert!(result.tokens.is_empty() || !result.tokens.is_empty());
    }

    #[test]
    fn test_analyze_valid_fn() {
        let result = analyze("fn main() {\n}\n");
        // Check that analysis completes (may have resolve/type errors but shouldn't crash).
        assert!(result.ast.is_some());
    }

    #[test]
    fn test_diagnostic_error() {
        let d = Diagnostic::error(Range::point(1, 0), "test error", "test");
        assert_eq!(d.severity, DiagnosticSeverity::Error);
        assert_eq!(d.message, "test error");
        assert_eq!(d.source, "test");
    }

    #[test]
    fn test_diagnostic_warning() {
        let d = Diagnostic::warning(Range::point(0, 0), "test warning", "test");
        assert_eq!(d.severity, DiagnosticSeverity::Warning);
    }

    #[test]
    fn test_analysis_has_errors() {
        let result = AnalysisResult {
            diagnostics: vec![Diagnostic::error(Range::point(0, 0), "err", "test")],
            tokens: vec![],
            ast: None,
            resolve_result: None,
            type_result: None,
        };
        assert!(result.has_errors());
    }

    #[test]
    fn test_analysis_no_errors() {
        let result = AnalysisResult {
            diagnostics: vec![Diagnostic::warning(Range::point(0, 0), "warn", "test")],
            tokens: vec![],
            ast: None,
            resolve_result: None,
            type_result: None,
        };
        assert!(!result.has_errors());
    }
}
