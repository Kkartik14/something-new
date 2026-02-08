//! Source code formatter — `adam fmt`.
//!
//! Canonical style: 4-space indent, 100-char width, no trailing whitespace,
//! one blank line between top-level items.

use std::fs;
use std::path::{Path, PathBuf};

/// Format Adam source files.
pub fn format_files(path: Option<&str>, check_only: bool) -> Result<(), String> {
    let files = find_adam_files(path)?;

    if files.is_empty() {
        println!("No .adam files found.");
        return Ok(());
    }

    let mut needs_formatting = Vec::new();

    for file in &files {
        let source = fs::read_to_string(file)
            .map_err(|e| format!("failed to read {}: {}", file.display(), e))?;

        let formatted = format_source(&source);

        if formatted != source {
            needs_formatting.push(file.clone());
            if !check_only {
                fs::write(file, &formatted)
                    .map_err(|e| format!("failed to write {}: {}", file.display(), e))?;
                println!("Formatted: {}", file.display());
            }
        }
    }

    if check_only && !needs_formatting.is_empty() {
        for file in &needs_formatting {
            println!("Needs formatting: {}", file.display());
        }
        return Err(format!(
            "{} file(s) need formatting",
            needs_formatting.len()
        ));
    }

    if needs_formatting.is_empty() {
        println!("All {} file(s) already formatted.", files.len());
    }

    Ok(())
}

/// Find all .adam files to format.
fn find_adam_files(path: Option<&str>) -> Result<Vec<PathBuf>, String> {
    let root = match path {
        Some(p) => PathBuf::from(p),
        None => std::env::current_dir()
            .map_err(|e| format!("failed to get current directory: {}", e))?,
    };

    if root.is_file() && root.extension().map(|e| e == "adam").unwrap_or(false) {
        return Ok(vec![root]);
    }

    if !root.is_dir() {
        return Err(format!("'{}' is not a file or directory", root.display()));
    }

    let mut files = Vec::new();
    collect_adam_files(&root, &mut files)?;
    files.sort();
    Ok(files)
}

fn collect_adam_files(dir: &Path, files: &mut Vec<PathBuf>) -> Result<(), String> {
    let entries = fs::read_dir(dir)
        .map_err(|e| format!("failed to read directory {}: {}", dir.display(), e))?;

    for entry in entries {
        let entry = entry.map_err(|e| format!("directory entry error: {}", e))?;
        let path = entry.path();

        if path.is_dir() {
            let name = path.file_name().unwrap().to_string_lossy();
            // Skip hidden directories and build artifacts.
            if !name.starts_with('.') && name != "build" && name != "target" {
                collect_adam_files(&path, files)?;
            }
        } else if path.extension().map(|e| e == "adam").unwrap_or(false) {
            files.push(path);
        }
    }

    Ok(())
}

/// Format a single Adam source string.
pub fn format_source(source: &str) -> String {
    let mut output = String::with_capacity(source.len());
    let mut indent_level: i32 = 0;
    let mut prev_line_blank = false;
    let mut in_top_level = true;
    let mut prev_was_top_level_item = false;

    for (i, line) in source.lines().enumerate() {
        let trimmed = line.trim();

        // Handle blank lines.
        if trimmed.is_empty() {
            if !prev_line_blank && i > 0 {
                output.push('\n');
                prev_line_blank = true;
            }
            continue;
        }
        prev_line_blank = false;

        // Decrease indent for closing braces.
        if trimmed.starts_with('}') || trimmed.starts_with(']') || trimmed.starts_with(')') {
            indent_level -= 1;
            if indent_level < 0 {
                indent_level = 0;
            }
        }

        // Check if this is a top-level item.
        let is_top_level_item = indent_level == 0
            && (trimmed.starts_with("fn ")
                || trimmed.starts_with("pub fn ")
                || trimmed.starts_with("struct ")
                || trimmed.starts_with("pub struct ")
                || trimmed.starts_with("enum ")
                || trimmed.starts_with("pub enum ")
                || trimmed.starts_with("trait ")
                || trimmed.starts_with("pub trait ")
                || trimmed.starts_with("impl ")
                || trimmed.starts_with("view ")
                || trimmed.starts_with("pub view ")
                || trimmed.starts_with("module ")
                || trimmed.starts_with("test ")
                || trimmed.starts_with("bench "));

        // Add blank line between top-level items.
        if is_top_level_item && prev_was_top_level_item && !output.is_empty() {
            if !output.ends_with("\n\n") {
                if !output.ends_with('\n') {
                    output.push('\n');
                }
                output.push('\n');
            }
        }

        // Format the line.
        let formatted = format_line(trimmed, indent_level);
        output.push_str(&formatted);
        output.push('\n');

        // Track top-level items.
        if indent_level == 0 {
            in_top_level = true;
            if is_top_level_item || trimmed.starts_with('}') {
                prev_was_top_level_item = true;
            }
        } else {
            in_top_level = false;
            prev_was_top_level_item = false;
        }

        // Increase indent for opening braces.
        if trimmed.ends_with('{') || trimmed.ends_with('[') {
            indent_level += 1;
        }
        // Handle one-liner close on same line
        let opens = trimmed.chars().filter(|&c| c == '{').count() as i32;
        let closes = trimmed.chars().filter(|&c| c == '}').count() as i32;
        // Adjust for cases like `} else {`
        if opens > 0 && closes > 0 {
            // Already counted the closing brace above.
            // The net effect should be: closed one, opened one = same level.
            // Since we already decremented for '}' and will increment for '{',
            // nothing extra needed.
        }
    }

    // Ensure trailing newline.
    if !output.is_empty() && !output.ends_with('\n') {
        output.push('\n');
    }

    // Remove trailing blank lines before final newline.
    while output.ends_with("\n\n") {
        output.pop();
    }

    output
}

/// Format a single line with proper indentation and spacing.
fn format_line(trimmed: &str, indent_level: i32) -> String {
    let indent = "    ".repeat(indent_level.max(0) as usize);

    // Apply formatting rules.
    let formatted = trimmed
        // Remove trailing whitespace (already trimmed).
        .to_string();

    format!("{}{}", indent, formatted)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_trailing_newline() {
        let result = format_source("fn main() {\n    print(\"hello\")\n}");
        assert!(result.ends_with('\n'));
    }

    #[test]
    fn test_format_idempotent() {
        let source = "fn main() {\n    print(\"hello\")\n}\n";
        let first = format_source(source);
        let second = format_source(&first);
        assert_eq!(first, second, "formatter is not idempotent");
    }

    #[test]
    fn test_format_removes_trailing_whitespace() {
        let source = "fn main() {   \n    print(\"hello\")   \n}   \n";
        let result = format_source(source);
        for line in result.lines() {
            assert_eq!(line, line.trim_end(), "trailing whitespace found");
        }
    }

    #[test]
    fn test_format_blank_line_between_top_level() {
        let source = "fn foo() {\n}\nfn bar() {\n}\n";
        let result = format_source(source);
        assert!(
            result.contains("}\n\nfn bar"),
            "missing blank line between functions"
        );
    }

    #[test]
    fn test_format_no_multiple_blank_lines() {
        let source = "fn foo() {\n}\n\n\n\nfn bar() {\n}\n";
        let result = format_source(source);
        assert!(!result.contains("\n\n\n"), "multiple blank lines found");
    }

    #[test]
    fn test_format_indentation() {
        let source = "fn foo() {\nif true {\nprint(\"yes\")\n}\n}\n";
        let result = format_source(source);
        assert!(result.contains("    if true"), "missing indentation");
        assert!(
            result.contains("        print"),
            "missing nested indentation"
        );
    }

    #[test]
    fn test_format_empty_source() {
        let result = format_source("");
        assert!(result.is_empty() || result == "\n");
    }

    #[test]
    fn test_format_comment_preserved() {
        let source = "// This is a comment\nfn main() {\n}\n";
        let result = format_source(source);
        assert!(result.contains("// This is a comment"));
    }

    #[test]
    fn test_find_adam_files_single() {
        let dir = tempfile::tempdir().unwrap();
        let file = dir.path().join("test.adam");
        fs::write(&file, "fn main() {}").unwrap();

        let files = find_adam_files(Some(file.to_str().unwrap())).unwrap();
        assert_eq!(files.len(), 1);
    }

    #[test]
    fn test_find_adam_files_directory() {
        let dir = tempfile::tempdir().unwrap();
        fs::write(dir.path().join("a.adam"), "fn a() {}").unwrap();
        fs::write(dir.path().join("b.adam"), "fn b() {}").unwrap();
        fs::write(dir.path().join("c.txt"), "not adam").unwrap();

        let files = find_adam_files(Some(dir.path().to_str().unwrap())).unwrap();
        assert_eq!(files.len(), 2);
    }
}
