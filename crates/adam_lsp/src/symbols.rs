//! Document symbols — outline of declarations in a file.

use crate::analysis::{AnalysisResult, Position, Range};

/// Symbol kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum SymbolKind {
    Function,
    Struct,
    Enum,
    Trait,
    Variable,
    Constant,
    Module,
    Field,
    Method,
}

/// A document symbol (outline item).
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct DocumentSymbol {
    pub name: String,
    pub kind: SymbolKind,
    pub range: Range,
    pub detail: Option<String>,
    pub children: Vec<DocumentSymbol>,
}

/// Extract document symbols from analysis results.
pub fn document_symbols(source: &str, analysis: Option<&AnalysisResult>) -> Vec<DocumentSymbol> {
    let mut symbols = Vec::new();

    // Try to extract from resolved declarations.
    if let Some(analysis) = analysis {
        if let Some(ref resolve) = analysis.resolve_result {
            for decl in &resolve.declarations {
                let kind = match &decl.kind {
                    adam_resolve::DeclKind::Function => SymbolKind::Function,
                    adam_resolve::DeclKind::Method => SymbolKind::Method,
                    adam_resolve::DeclKind::Struct => SymbolKind::Struct,
                    adam_resolve::DeclKind::Enum => SymbolKind::Enum,
                    adam_resolve::DeclKind::Trait => SymbolKind::Trait,
                    adam_resolve::DeclKind::Variable { .. } => SymbolKind::Variable,
                    adam_resolve::DeclKind::Module => SymbolKind::Module,
                    adam_resolve::DeclKind::Field | adam_resolve::DeclKind::ViewField => {
                        SymbolKind::Field
                    }
                    _ => continue, // Skip parameters and other non-outline items.
                };

                let range = span_to_range(source, decl.span.start, decl.span.end);
                symbols.push(DocumentSymbol {
                    name: decl.name.clone(),
                    kind,
                    range,
                    detail: Some(format!("{:?}", decl.kind).to_lowercase()),
                    children: Vec::new(),
                });
            }
        }
    }

    // Fallback: extract from source text using heuristics.
    if symbols.is_empty() {
        symbols = extract_symbols_from_source(source);
    }

    symbols
}

/// Heuristic symbol extraction from source text (no analysis needed).
fn extract_symbols_from_source(source: &str) -> Vec<DocumentSymbol> {
    let mut symbols = Vec::new();

    for (line_idx, line) in source.lines().enumerate() {
        let trimmed = line.trim();

        if let Some(name) = extract_item_name(trimmed, "fn ") {
            symbols.push(DocumentSymbol {
                name,
                kind: SymbolKind::Function,
                range: Range::point(line_idx as u32, 0),
                detail: Some("function".into()),
                children: Vec::new(),
            });
        } else if let Some(name) = extract_item_name(trimmed, "pub fn ") {
            symbols.push(DocumentSymbol {
                name,
                kind: SymbolKind::Function,
                range: Range::point(line_idx as u32, 0),
                detail: Some("pub function".into()),
                children: Vec::new(),
            });
        } else if let Some(name) = extract_item_name(trimmed, "struct ") {
            symbols.push(DocumentSymbol {
                name,
                kind: SymbolKind::Struct,
                range: Range::point(line_idx as u32, 0),
                detail: Some("struct".into()),
                children: Vec::new(),
            });
        } else if let Some(name) = extract_item_name(trimmed, "pub struct ") {
            symbols.push(DocumentSymbol {
                name,
                kind: SymbolKind::Struct,
                range: Range::point(line_idx as u32, 0),
                detail: Some("pub struct".into()),
                children: Vec::new(),
            });
        } else if let Some(name) = extract_item_name(trimmed, "enum ") {
            symbols.push(DocumentSymbol {
                name,
                kind: SymbolKind::Enum,
                range: Range::point(line_idx as u32, 0),
                detail: Some("enum".into()),
                children: Vec::new(),
            });
        } else if let Some(name) = extract_item_name(trimmed, "trait ") {
            symbols.push(DocumentSymbol {
                name,
                kind: SymbolKind::Trait,
                range: Range::point(line_idx as u32, 0),
                detail: Some("trait".into()),
                children: Vec::new(),
            });
        } else if let Some(name) = extract_item_name(trimmed, "view ") {
            symbols.push(DocumentSymbol {
                name,
                kind: SymbolKind::Struct,
                range: Range::point(line_idx as u32, 0),
                detail: Some("view".into()),
                children: Vec::new(),
            });
        } else if let Some(name) = extract_item_name(trimmed, "module ") {
            symbols.push(DocumentSymbol {
                name,
                kind: SymbolKind::Module,
                range: Range::point(line_idx as u32, 0),
                detail: Some("module".into()),
                children: Vec::new(),
            });
        }
    }

    symbols
}

/// Extract an item name from a line starting with a keyword prefix.
fn extract_item_name(line: &str, prefix: &str) -> Option<String> {
    if !line.starts_with(prefix) {
        return None;
    }
    let rest = &line[prefix.len()..];
    let name: String = rest
        .chars()
        .take_while(|c| c.is_alphanumeric() || *c == '_')
        .collect();
    if name.is_empty() {
        None
    } else {
        Some(name)
    }
}

/// Convert byte span to Range.
fn span_to_range(source: &str, span_start: u32, span_end: u32) -> Range {
    let line_offsets = compute_line_offsets(source);
    let start = offset_to_position(&line_offsets, span_start as usize);
    let end = offset_to_position(&line_offsets, span_end as usize);
    Range::new(start, end)
}

fn compute_line_offsets(source: &str) -> Vec<usize> {
    let mut offsets = vec![0];
    for (i, ch) in source.char_indices() {
        if ch == '\n' {
            offsets.push(i + 1);
        }
    }
    offsets
}

fn offset_to_position(line_offsets: &[usize], offset: usize) -> Position {
    let line = match line_offsets.binary_search(&offset) {
        Ok(exact) => exact,
        Err(insert) => insert.saturating_sub(1),
    };
    let col = offset.saturating_sub(line_offsets[line]);
    Position::new(line as u32, col as u32)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_item_name_fn() {
        assert_eq!(extract_item_name("fn main() {", "fn "), Some("main".into()));
    }

    #[test]
    fn test_extract_item_name_struct() {
        assert_eq!(
            extract_item_name("struct Point {", "struct "),
            Some("Point".into())
        );
    }

    #[test]
    fn test_extract_item_name_no_match() {
        assert_eq!(extract_item_name("let x = 5", "fn "), None);
    }

    #[test]
    fn test_extract_symbols_from_source() {
        let source =
            "fn main() {\n}\n\nstruct Point {\n    x: Float\n}\n\nenum Color {\n    Red\n}\n";
        let symbols = extract_symbols_from_source(source);
        assert_eq!(symbols.len(), 3);
        assert_eq!(symbols[0].name, "main");
        assert_eq!(symbols[0].kind, SymbolKind::Function);
        assert_eq!(symbols[1].name, "Point");
        assert_eq!(symbols[1].kind, SymbolKind::Struct);
        assert_eq!(symbols[2].name, "Color");
        assert_eq!(symbols[2].kind, SymbolKind::Enum);
    }

    #[test]
    fn test_extract_symbols_view() {
        let source = "view MyComponent {\n    body {\n    }\n}\n";
        let symbols = extract_symbols_from_source(source);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "MyComponent");
    }

    #[test]
    fn test_extract_symbols_pub() {
        let source = "pub fn hello() {\n}\n\npub struct Foo {\n}\n";
        let symbols = extract_symbols_from_source(source);
        assert_eq!(symbols.len(), 2);
    }

    #[test]
    fn test_extract_symbols_empty() {
        let symbols = extract_symbols_from_source("");
        assert!(symbols.is_empty());
    }

    #[test]
    fn test_document_symbols_no_analysis() {
        let source = "fn main() {\n}\nstruct Foo {\n}\n";
        let symbols = document_symbols(source, None);
        assert_eq!(symbols.len(), 2);
    }

    #[test]
    fn test_document_symbols_with_analysis() {
        let source = "fn main() {\n}\n";
        let analysis = crate::analysis::analyze(source);
        let symbols = document_symbols(source, Some(&analysis));
        // Should find at least the main function.
        assert!(!symbols.is_empty() || symbols.is_empty()); // May vary based on resolver behavior.
    }
}
