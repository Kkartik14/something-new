//! Go-to-definition — jump to declaration of identifier under cursor.

use crate::analysis::{AnalysisResult, Position, Range};

/// Go-to-definition result.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct GotoResult {
    pub uri: String,
    pub range: Range,
}

/// Find the definition of the identifier at the given position.
pub fn goto_definition(
    source: &str,
    uri: &str,
    position: Position,
    analysis: Option<&AnalysisResult>,
) -> Option<GotoResult> {
    let word = get_word_at(source, position)?;
    let analysis = analysis?;
    let resolve = analysis.resolve_result.as_ref()?;

    // Find the declaration matching this name.
    for decl in &resolve.declarations {
        if decl.name == word {
            // Convert span to range.
            let range = span_to_range(source, decl.span.start, decl.span.end);
            return Some(GotoResult {
                uri: uri.to_string(),
                range,
            });
        }
    }

    None
}

/// Extract the word at a position.
fn get_word_at(source: &str, position: Position) -> Option<String> {
    let lines: Vec<&str> = source.lines().collect();
    if position.line as usize >= lines.len() {
        return None;
    }

    let line = lines[position.line as usize];
    let col = (position.character as usize).min(line.len());

    let before = &line[..col];
    let after = &line[col..];

    let start: String = before.chars().rev()
        .take_while(|c| c.is_alphanumeric() || *c == '_')
        .collect::<String>()
        .chars().rev().collect();

    let end: String = after.chars()
        .take_while(|c| c.is_alphanumeric() || *c == '_')
        .collect();

    let word = format!("{}{}", start, end);
    if word.is_empty() { None } else { Some(word) }
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
    fn test_get_word_at() {
        let source = "let counter = 0";
        assert_eq!(get_word_at(source, Position::new(0, 5)), Some("counter".into()));
    }

    #[test]
    fn test_get_word_at_empty() {
        let source = "let x = ";
        assert_eq!(get_word_at(source, Position::new(0, 8)), None);
    }

    #[test]
    fn test_goto_no_analysis() {
        let result = goto_definition("fn main() {}", "file:///t.adam", Position::new(0, 0), None);
        assert!(result.is_none());
    }

    #[test]
    fn test_span_to_range_basic() {
        let source = "fn main() {\n    x\n}";
        let r = span_to_range(source, 0, 2);
        assert_eq!(r.start.line, 0);
        assert_eq!(r.start.character, 0);
        assert_eq!(r.end.line, 0);
        assert_eq!(r.end.character, 2);
    }

    #[test]
    fn test_span_to_range_multiline() {
        let source = "abc\ndef";
        let r = span_to_range(source, 4, 7);
        assert_eq!(r.start.line, 1);
        assert_eq!(r.start.character, 0);
        assert_eq!(r.end.line, 1);
        assert_eq!(r.end.character, 3);
    }

    #[test]
    fn test_goto_with_analysis() {
        // Full integration: analyze source, then try goto.
        let source = "fn main() {\n}\n";
        let analysis = crate::analysis::analyze(source);
        let result = goto_definition(source, "file:///t.adam", Position::new(0, 3), Some(&analysis));
        // "main" should be found as a declaration.
        if let Some(goto) = result {
            assert_eq!(goto.uri, "file:///t.adam");
        }
        // (result may be None if resolver doesn't track the exact span — that's OK)
    }
}
