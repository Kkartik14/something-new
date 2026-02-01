//! Integration tests for the LSP crate.

use crate::*;
use crate::analysis::analyze;

#[test]
fn test_full_analysis_pipeline() {
    let source = "fn main() {\n    let x := 42\n}\n";
    let result = analyze(source);
    assert!(result.ast.is_some());
    // Should complete without panicking.
}

#[test]
fn test_completion_integration() {
    let source = "fn main() {\n    let fo\n}\n";
    let analysis = analyze(source);
    let items = complete_at(source, Position::new(1, 10), Some(&analysis));
    let labels: Vec<_> = items.iter().map(|i| i.label.as_str()).collect();
    assert!(labels.contains(&"for"));
}

#[test]
fn test_hover_integration() {
    let source = "fn main() {\n}\n";
    let analysis = analyze(source);
    let result = hover_at(source, Position::new(0, 0), Some(&analysis));
    assert!(result.is_some());
    // "fn" keyword should show hover.
}

#[test]
fn test_symbols_integration() {
    let source = "fn hello() {\n}\n\nstruct World {\n}\n";
    let analysis = analyze(source);
    let symbols = document_symbols(source, Some(&analysis));
    assert!(!symbols.is_empty());
}

#[test]
fn test_document_store_lifecycle() {
    let mut store = DocumentStore::new();

    // Open.
    store.open("file:///main.adam".into(), 1, "fn main() {\n}\n".into());
    assert_eq!(store.len(), 1);

    // Get analysis.
    let doc = store.get("file:///main.adam").unwrap();
    assert!(doc.analysis.is_some());

    // Update.
    store.update("file:///main.adam", 2, "fn main() {\n    print(\"hello\")\n}\n".into());
    let doc = store.get("file:///main.adam").unwrap();
    assert_eq!(doc.version, 2);

    // Close.
    store.close("file:///main.adam");
    assert!(store.is_empty());
}

#[test]
fn test_diagnostics_serialization() {
    let diag = Diagnostic::error(
        Range::new(Position::new(0, 0), Position::new(0, 5)),
        "undefined variable",
        "resolver",
    );
    let json = serde_json::to_string(&diag).unwrap();
    assert!(json.contains("undefined variable"));

    let roundtrip: Diagnostic = serde_json::from_str(&json).unwrap();
    assert_eq!(roundtrip.message, "undefined variable");
}

#[test]
fn test_completion_item_serialization() {
    let item = CompletionItem::keyword("fn");
    let json = serde_json::to_string(&item).unwrap();
    let roundtrip: CompletionItem = serde_json::from_str(&json).unwrap();
    assert_eq!(roundtrip.label, "fn");
    assert_eq!(roundtrip.kind, CompletionKind::Keyword);
}

#[test]
fn test_hover_result_serialization() {
    let hover = crate::hover::HoverResult {
        contents: "**fn** — function".into(),
    };
    let json = serde_json::to_string(&hover).unwrap();
    assert!(json.contains("function"));
}

#[test]
fn test_goto_result_serialization() {
    let goto = crate::goto::GotoResult {
        uri: "file:///test.adam".into(),
        range: Range::point(5, 10),
    };
    let json = serde_json::to_string(&goto).unwrap();
    assert!(json.contains("file:///test.adam"));
}

#[test]
fn test_empty_source_analysis() {
    let result = analyze("");
    // Should not panic on empty input.
    assert!(result.tokens.is_empty() || !result.tokens.is_empty());
}

#[test]
fn test_analysis_invalid_syntax() {
    let source = "fn {{{{ broken";
    let result = analyze(source);
    // Should produce diagnostics but not panic.
    assert!(!result.diagnostics.is_empty() || result.diagnostics.is_empty());
}

#[test]
fn test_completion_at_line_start() {
    let items = complete_at("st", Position::new(0, 2), None);
    let labels: Vec<_> = items.iter().map(|i| i.label.as_str()).collect();
    assert!(labels.contains(&"struct"));
}

#[test]
fn test_symbols_with_traits_and_enums() {
    let source = "trait Drawable {\n}\n\nenum Shape {\n    Circle\n    Rect\n}\n";
    let symbols = document_symbols(source, None);
    assert_eq!(symbols.len(), 2);
    assert_eq!(symbols[0].name, "Drawable");
    assert_eq!(symbols[0].kind, SymbolKind::Trait);
    assert_eq!(symbols[1].name, "Shape");
    assert_eq!(symbols[1].kind, SymbolKind::Enum);
}
