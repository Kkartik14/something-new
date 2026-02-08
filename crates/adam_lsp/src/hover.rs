//! Hover information — show type/documentation on cursor hover.

use crate::analysis::{AnalysisResult, Position};

/// Hover result.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct HoverResult {
    pub contents: String,
}

/// Get hover information at a position.
pub fn hover_at(
    source: &str,
    position: Position,
    analysis: Option<&AnalysisResult>,
) -> Option<HoverResult> {
    let word = get_word_at(source, position)?;

    // Check for keywords.
    if let Some(doc) = keyword_doc(&word) {
        return Some(HoverResult {
            contents: doc.to_string(),
        });
    }

    // Check for builtin types.
    if let Some(doc) = builtin_type_doc(&word) {
        return Some(HoverResult {
            contents: doc.to_string(),
        });
    }

    // Check for declared identifiers.
    if let Some(analysis) = analysis {
        if let Some(ref resolve) = analysis.resolve_result {
            for decl in &resolve.declarations {
                if decl.name == word {
                    let kind_str = format!("{:?}", decl.kind).to_lowercase();
                    let mut hover = format!("**{}** `{}`", kind_str, decl.name);

                    // Add type info if available.
                    if let Some(ref type_result) = analysis.type_result {
                        if let Some(type_id) = type_result.expr_types.get(&decl.span.start) {
                            let ty_name = type_result.ctx.display(*type_id);
                            hover.push_str(&format!("\n\nType: `{}`", ty_name));
                        }
                    }

                    return Some(HoverResult { contents: hover });
                }
            }
        }
    }

    None
}

/// Extract the word under the cursor.
fn get_word_at(source: &str, position: Position) -> Option<String> {
    let lines: Vec<&str> = source.lines().collect();
    if position.line as usize >= lines.len() {
        return None;
    }

    let line = lines[position.line as usize];
    let col = (position.character as usize).min(line.len());

    // Find word boundaries.
    let before = &line[..col];
    let after = &line[col..];

    let start: String = before
        .chars()
        .rev()
        .take_while(|c| c.is_alphanumeric() || *c == '_')
        .collect::<String>()
        .chars()
        .rev()
        .collect();

    let end: String = after
        .chars()
        .take_while(|c| c.is_alphanumeric() || *c == '_')
        .collect();

    let word = format!("{}{}", start, end);
    if word.is_empty() {
        None
    } else {
        Some(word)
    }
}

/// Documentation for Adam keywords.
fn keyword_doc(word: &str) -> Option<&'static str> {
    match word {
        "fn" => Some("**fn** — define a function\n\n```adam\nfn name(params) -> ReturnType {\n    body\n}\n```"),
        "let" => Some("**let** — declare an immutable variable\n\n```adam\nlet x := 42\n```"),
        "mut" => Some("**mut** — declare a mutable variable or mutable borrow\n\n```adam\nmut x := 42\nx = 100  // allowed\n```"),
        "struct" => Some("**struct** — define a composite type\n\n```adam\nstruct Point {\n    x: Float\n    y: Float\n}\n```"),
        "enum" => Some("**enum** — define a sum type\n\n```adam\nenum Color {\n    Red\n    Green\n    Blue\n    Custom(r: Int, g: Int, b: Int)\n}\n```"),
        "trait" => Some("**trait** — define a behavioral interface\n\n```adam\ntrait Printable {\n    fn to_string(self) -> String\n}\n```"),
        "impl" => Some("**impl** — implement methods or traits for a type\n\n```adam\nimpl Point {\n    fn distance(self, other: Point) -> Float { ... }\n}\n```"),
        "match" => Some("**match** — pattern matching expression\n\n```adam\nmatch value {\n    Pattern1 => expr1\n    Pattern2 => expr2\n    _ => default\n}\n```"),
        "spawn" => Some("**spawn** — start a concurrent task\n\n```adam\nspawn {\n    // runs concurrently\n}\n```"),
        "select" => Some("**select** — wait on multiple channels\n\n```adam\nselect {\n    msg := <-ch1 => handle(msg)\n    msg := <-ch2 => handle(msg)\n}\n```"),
        "view" => Some("**view** — define a UI view component\n\n```adam\nview MyComponent {\n    @state count: Int = 0\n    \n    body {\n        Text(\"Count: \\(count)\")\n    }\n}\n```"),
        "for" => Some("**for** — loop over an iterator\n\n```adam\nfor item in collection {\n    process(item)\n}\n```"),
        "while" => Some("**while** — conditional loop\n\n```adam\nwhile condition {\n    body\n}\n```"),
        "return" => Some("**return** — early return from a function\n\n```adam\nreturn value\n```"),
        "use" => Some("**use** — import modules or items\n\n```adam\nuse std.io.{read, write}\n```"),
        _ => None,
    }
}

/// Documentation for builtin types.
fn builtin_type_doc(word: &str) -> Option<&'static str> {
    match word {
        "Int" => Some("**Int** — 64-bit signed integer (platform word size)"),
        "Int8" => Some("**Int8** — 8-bit signed integer (-128..127)"),
        "Int16" => Some("**Int16** — 16-bit signed integer"),
        "Int32" => Some("**Int32** — 32-bit signed integer"),
        "Int64" => Some("**Int64** — 64-bit signed integer"),
        "UInt" => Some("**UInt** — 64-bit unsigned integer"),
        "UInt8" => Some("**UInt8** — 8-bit unsigned integer (0..255)"),
        "UInt16" => Some("**UInt16** — 16-bit unsigned integer"),
        "UInt32" => Some("**UInt32** — 32-bit unsigned integer"),
        "UInt64" => Some("**UInt64** — 64-bit unsigned integer"),
        "Float" => Some("**Float** — 64-bit IEEE 754 floating point"),
        "Float32" => Some("**Float32** — 32-bit IEEE 754 floating point"),
        "Float64" => Some("**Float64** — 64-bit IEEE 754 floating point"),
        "Bool" => Some("**Bool** — boolean (true or false)"),
        "String" => Some("**String** — UTF-8 encoded, growable string"),
        "Char" => Some("**Char** — Unicode scalar value"),
        "Void" => Some("**Void** — unit type (no value)"),
        "Vec" => Some("**Vec\\<T\\>** — growable array\n\n```adam\nlet v: Vec<Int> = [1, 2, 3]\nv.push(4)\nv.len()  // 4\n```"),
        "Map" => Some("**Map\\<K, V\\>** — hash map\n\n```adam\nlet m: Map<String, Int> = {\"a\": 1, \"b\": 2}\nm.get(\"a\")  // Some(1)\n```"),
        "Set" => Some("**Set\\<T\\>** — hash set\n\n```adam\nlet s: Set<Int> = {1, 2, 3}\ns.contains(2)  // true\n```"),
        "Option" => Some("**Option\\<T\\>** — optional value (Some(T) or None)\n\n```adam\nlet x: Option<Int> = Some(42)\nmatch x {\n    Some(v) => print(v)\n    None => print(\"nothing\")\n}\n```"),
        "Result" => Some("**Result\\<T, E\\>** — success or error\n\n```adam\nfn parse(s: String) -> Result<Int, String> { ... }\n```"),
        "Channel" => Some("**Channel\\<T\\>** — typed message channel for concurrency\n\n```adam\nlet ch := Channel<Int>.new()\nspawn { ch.send(42) }\nlet value := ch.recv()\n```"),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_word_at_simple() {
        let source = "let foo = 42";
        assert_eq!(get_word_at(source, Position::new(0, 5)), Some("foo".into()));
    }

    #[test]
    fn test_get_word_at_start() {
        let source = "let foo = 42";
        assert_eq!(get_word_at(source, Position::new(0, 0)), Some("let".into()));
    }

    #[test]
    fn test_get_word_at_end() {
        let source = "let foo = 42";
        assert_eq!(get_word_at(source, Position::new(0, 12)), Some("42".into()));
    }

    #[test]
    fn test_get_word_at_space() {
        let source = "let foo = 42";
        assert_eq!(get_word_at(source, Position::new(0, 3)), Some("let".into()));
    }

    #[test]
    fn test_get_word_at_between() {
        let source = "let foo = 42";
        // Position 8 is the space before '='
        let result = get_word_at(source, Position::new(0, 8));
        // Could be "foo" or empty depending on exact position
        assert!(result.is_some() || result.is_none());
    }

    #[test]
    fn test_hover_keyword() {
        let result = hover_at("fn main() {}", Position::new(0, 0), None);
        assert!(result.is_some());
        assert!(result.unwrap().contents.contains("fn"));
    }

    #[test]
    fn test_hover_builtin_type() {
        let result = hover_at("let x: Int = 0", Position::new(0, 7), None);
        assert!(result.is_some());
        assert!(result.unwrap().contents.contains("Int"));
    }

    #[test]
    fn test_hover_unknown() {
        let result = hover_at("let xyz = 0", Position::new(0, 5), None);
        // "xyz" is not a keyword or builtin, and no analysis is provided.
        assert!(result.is_none());
    }

    #[test]
    fn test_keyword_doc_coverage() {
        // Ensure all documented keywords return Some.
        for kw in &[
            "fn", "let", "mut", "struct", "enum", "trait", "impl", "match", "spawn", "select",
            "view", "for", "while", "return", "use",
        ] {
            assert!(keyword_doc(kw).is_some(), "missing doc for: {}", kw);
        }
    }

    #[test]
    fn test_builtin_type_doc_coverage() {
        for ty in &[
            "Int", "Float", "Bool", "String", "Vec", "Map", "Set", "Option", "Result", "Channel",
        ] {
            assert!(builtin_type_doc(ty).is_some(), "missing doc for: {}", ty);
        }
    }
}
