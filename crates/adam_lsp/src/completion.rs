//! Completion provider — context-aware completions.

use crate::analysis::{AnalysisResult, Position};

/// Completion item kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum CompletionKind {
    Function,
    Variable,
    Struct,
    Enum,
    Trait,
    Field,
    Keyword,
    Module,
    Snippet,
}

/// A completion suggestion.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct CompletionItem {
    pub label: String,
    pub kind: CompletionKind,
    pub detail: Option<String>,
    pub insert_text: Option<String>,
    pub documentation: Option<String>,
}

impl CompletionItem {
    pub fn keyword(name: &str) -> Self {
        Self {
            label: name.to_string(),
            kind: CompletionKind::Keyword,
            detail: Some("keyword".to_string()),
            insert_text: None,
            documentation: None,
        }
    }

    pub fn function(name: &str, detail: Option<String>) -> Self {
        Self {
            label: name.to_string(),
            kind: CompletionKind::Function,
            detail,
            insert_text: Some(format!("{}(", name)),
            documentation: None,
        }
    }

    pub fn variable(name: &str, ty: Option<String>) -> Self {
        Self {
            label: name.to_string(),
            kind: CompletionKind::Variable,
            detail: ty,
            insert_text: None,
            documentation: None,
        }
    }

    pub fn type_item(name: &str, kind: CompletionKind) -> Self {
        Self {
            label: name.to_string(),
            kind,
            detail: Some(format!("{:?}", kind).to_lowercase()),
            insert_text: None,
            documentation: None,
        }
    }

    pub fn snippet(label: &str, insert_text: &str, doc: &str) -> Self {
        Self {
            label: label.to_string(),
            kind: CompletionKind::Snippet,
            detail: Some("snippet".to_string()),
            insert_text: Some(insert_text.to_string()),
            documentation: Some(doc.to_string()),
        }
    }
}

/// Adam language keywords.
const KEYWORDS: &[&str] = &[
    "fn", "pub", "struct", "enum", "trait", "impl", "view",
    "let", "mut", "if", "else", "for", "while", "loop",
    "match", "return", "break", "continue", "in", "as",
    "true", "false", "nil", "self", "use", "module",
    "test", "bench", "spawn", "select", "async", "await",
    "own", "ref", "move", "type", "where",
];

/// Built-in types.
const BUILTIN_TYPES: &[&str] = &[
    "Int", "Int8", "Int16", "Int32", "Int64",
    "UInt", "UInt8", "UInt16", "UInt32", "UInt64",
    "Float", "Float32", "Float64",
    "Bool", "String", "Char", "Void",
    "Vec", "Map", "Set", "Option", "Result",
    "Channel",
];

/// Compute completions at a position.
pub fn complete_at(
    source: &str,
    position: Position,
    analysis: Option<&AnalysisResult>,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    // Determine the prefix being typed.
    let prefix = get_prefix(source, position);

    // Add keyword completions.
    for kw in KEYWORDS {
        if kw.starts_with(&prefix) && !prefix.is_empty() {
            items.push(CompletionItem::keyword(kw));
        }
    }

    // Add builtin type completions.
    for ty in BUILTIN_TYPES {
        if ty.starts_with(&prefix) && !prefix.is_empty() {
            items.push(CompletionItem::type_item(ty, CompletionKind::Struct));
        }
    }

    // Add snippet completions (only when there's a prefix to avoid noise).
    if !prefix.is_empty() {
        if "fn".starts_with(&prefix) {
            items.push(CompletionItem::snippet(
                "fn (snippet)",
                "fn ${1:name}(${2:}) {\n    ${0}\n}",
                "Function definition",
            ));
        }
        if "struct".starts_with(&prefix) {
            items.push(CompletionItem::snippet(
                "struct (snippet)",
                "struct ${1:Name} {\n    ${0}\n}",
                "Struct definition",
            ));
        }
        if "test".starts_with(&prefix) {
            items.push(CompletionItem::snippet(
                "test (snippet)",
                "test \"${1:name}\" {\n    ${0}\n}",
                "Test block",
            ));
        }
    }

    // Add identifiers from analysis (declared names in scope).
    if let Some(analysis) = analysis {
        if let Some(ref resolve) = analysis.resolve_result {
            for decl in &resolve.declarations {
                let name = &decl.name;
                if name.starts_with(&prefix) && !prefix.is_empty() && name != &prefix {
                    let kind = match &decl.kind {
                        adam_resolve::DeclKind::Function => CompletionKind::Function,
                        adam_resolve::DeclKind::Variable { .. } => CompletionKind::Variable,
                        adam_resolve::DeclKind::Param { .. } => CompletionKind::Variable,
                        adam_resolve::DeclKind::Struct => CompletionKind::Struct,
                        adam_resolve::DeclKind::Enum => CompletionKind::Enum,
                        adam_resolve::DeclKind::Trait => CompletionKind::Trait,
                        adam_resolve::DeclKind::Module => CompletionKind::Module,
                        adam_resolve::DeclKind::Field | adam_resolve::DeclKind::ViewField => CompletionKind::Field,
                        adam_resolve::DeclKind::Method => CompletionKind::Function,
                        _ => CompletionKind::Variable,
                    };
                    items.push(CompletionItem {
                        label: name.clone(),
                        kind,
                        detail: None,
                        insert_text: if kind == CompletionKind::Function {
                            Some(format!("{}(", name))
                        } else {
                            None
                        },
                        documentation: None,
                    });
                }
            }
        }
    }

    // Deduplicate by label.
    items.sort_by(|a, b| a.label.cmp(&b.label));
    items.dedup_by(|a, b| a.label == b.label);

    items
}

/// Extract the identifier prefix at the cursor position.
fn get_prefix(source: &str, position: Position) -> String {
    let lines: Vec<&str> = source.lines().collect();
    if position.line as usize >= lines.len() {
        return String::new();
    }

    let line = lines[position.line as usize];
    let col = (position.character as usize).min(line.len());
    let before = &line[..col];

    // Walk backwards to find identifier start.
    let prefix: String = before
        .chars()
        .rev()
        .take_while(|c| c.is_alphanumeric() || *c == '_')
        .collect::<String>()
        .chars()
        .rev()
        .collect();

    prefix
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_prefix_simple() {
        let source = "let foo = bar";
        let prefix = get_prefix(source, Position::new(0, 7));
        assert_eq!(prefix, "foo");
    }

    #[test]
    fn test_get_prefix_partial() {
        let source = "let fo";
        let prefix = get_prefix(source, Position::new(0, 6));
        assert_eq!(prefix, "fo");
    }

    #[test]
    fn test_get_prefix_empty() {
        let source = "let ";
        let prefix = get_prefix(source, Position::new(0, 4));
        assert_eq!(prefix, "");
    }

    #[test]
    fn test_get_prefix_multiline() {
        let source = "fn main() {\n    pri";
        let prefix = get_prefix(source, Position::new(1, 7));
        assert_eq!(prefix, "pri");
    }

    #[test]
    fn test_complete_keywords() {
        let items = complete_at("fn main() {\n    fo", Position::new(1, 6), None);
        let labels: Vec<_> = items.iter().map(|i| &i.label).collect();
        assert!(labels.contains(&&"for".to_string()));
    }

    #[test]
    fn test_complete_types() {
        let items = complete_at("let x: In", Position::new(0, 9), None);
        let labels: Vec<_> = items.iter().map(|i| &i.label).collect();
        assert!(labels.contains(&&"Int".to_string()));
        assert!(labels.contains(&&"Int32".to_string()));
    }

    #[test]
    fn test_complete_no_match() {
        let items = complete_at("let x = zzz", Position::new(0, 11), None);
        // No keywords/types start with "zzz".
        let keyword_items: Vec<_> = items.iter()
            .filter(|i| i.kind == CompletionKind::Keyword)
            .collect();
        assert!(keyword_items.is_empty());
    }

    #[test]
    fn test_complete_empty_prefix() {
        let items = complete_at("let x = ", Position::new(0, 8), None);
        // Empty prefix returns nothing (no spamming the completion list).
        assert!(items.is_empty());
    }

    #[test]
    fn test_complete_snippets() {
        let items = complete_at("fn", Position::new(0, 2), None);
        let snippets: Vec<_> = items.iter()
            .filter(|i| i.kind == CompletionKind::Snippet)
            .collect();
        assert!(!snippets.is_empty());
    }

    #[test]
    fn test_completion_item_keyword() {
        let item = CompletionItem::keyword("let");
        assert_eq!(item.label, "let");
        assert_eq!(item.kind, CompletionKind::Keyword);
    }

    #[test]
    fn test_completion_item_function() {
        let item = CompletionItem::function("print", Some("fn(String)".into()));
        assert_eq!(item.insert_text, Some("print(".into()));
    }
}
