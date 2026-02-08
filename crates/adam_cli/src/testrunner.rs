//! Test runner — `adam test`.
//!
//! Discovers test files (*_test.adam and tests/*.adam), parses test blocks,
//! compiles and runs each test, reports results.

use std::fs;
use std::path::{Path, PathBuf};
use std::time::Instant;

/// Test result for a single test case.
#[derive(Debug)]
pub struct TestResult {
    pub name: String,
    pub file: PathBuf,
    pub passed: bool,
    pub message: Option<String>,
    pub duration_ms: f64,
}

/// Run all tests, optionally filtered by name.
pub fn run_tests(filter: Option<&str>) -> Result<(), String> {
    let files = discover_test_files()?;

    if files.is_empty() {
        println!("No test files found.");
        return Ok(());
    }

    let mut tests = Vec::new();
    for file in &files {
        let source = fs::read_to_string(file)
            .map_err(|e| format!("failed to read {}: {}", file.display(), e))?;
        let file_tests = extract_test_names(&source);
        for name in file_tests {
            if let Some(f) = filter {
                if !name.contains(f) {
                    continue;
                }
            }
            tests.push((file.clone(), name));
        }
    }

    if tests.is_empty() {
        println!("No tests matched filter.");
        return Ok(());
    }

    println!("Running {} test(s)...\n", tests.len());

    let mut results = Vec::new();
    let mut passed = 0;
    let mut failed = 0;

    for (file, name) in &tests {
        let start = Instant::now();

        // Compile and run the test.
        let result = run_single_test(file, name);
        let elapsed = start.elapsed().as_secs_f64() * 1000.0;

        let test_result = TestResult {
            name: name.clone(),
            file: file.clone(),
            passed: result.is_ok(),
            message: result.err(),
            duration_ms: elapsed,
        };

        if test_result.passed {
            println!("  PASS  {} ({:.1}ms)", name, elapsed);
            passed += 1;
        } else {
            println!("  FAIL  {} ({:.1}ms)", name, elapsed);
            if let Some(ref msg) = test_result.message {
                println!("        {}", msg);
            }
            failed += 1;
        }

        results.push(test_result);
    }

    println!();
    println!("{} passed, {} failed", passed, failed);

    if failed > 0 {
        Err(format!("{} test(s) failed", failed))
    } else {
        Ok(())
    }
}

/// Discover test files in the project.
fn discover_test_files() -> Result<Vec<PathBuf>, String> {
    let mut files = Vec::new();

    // Look in tests/ directory.
    let tests_dir = Path::new("tests");
    if tests_dir.exists() {
        collect_test_files(tests_dir, &mut files)?;
    }

    // Look for *_test.adam files in src/.
    let src_dir = Path::new("src");
    if src_dir.exists() {
        collect_test_files_pattern(src_dir, "_test.adam", &mut files)?;
    }

    files.sort();
    Ok(files)
}

fn collect_test_files(dir: &Path, files: &mut Vec<PathBuf>) -> Result<(), String> {
    let entries =
        fs::read_dir(dir).map_err(|e| format!("failed to read {}: {}", dir.display(), e))?;
    for entry in entries {
        let path = entry.map_err(|e| format!("{}", e))?.path();
        if path.is_file() && path.extension().map(|e| e == "adam").unwrap_or(false) {
            files.push(path);
        } else if path.is_dir() {
            collect_test_files(&path, files)?;
        }
    }
    Ok(())
}

fn collect_test_files_pattern(
    dir: &Path,
    pattern: &str,
    files: &mut Vec<PathBuf>,
) -> Result<(), String> {
    let entries =
        fs::read_dir(dir).map_err(|e| format!("failed to read {}: {}", dir.display(), e))?;
    for entry in entries {
        let path = entry.map_err(|e| format!("{}", e))?.path();
        if path.is_file() {
            if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                if name.ends_with(pattern) {
                    files.push(path);
                }
            }
        } else if path.is_dir() {
            collect_test_files_pattern(&path, pattern, files)?;
        }
    }
    Ok(())
}

/// Extract test names from source code.
/// Looks for `test "name" {` blocks.
pub fn extract_test_names(source: &str) -> Vec<String> {
    let mut names = Vec::new();
    for line in source.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("test ") {
            // Extract name between quotes.
            if let Some(start) = trimmed.find('"') {
                if let Some(end) = trimmed[start + 1..].find('"') {
                    names.push(trimmed[start + 1..start + 1 + end].to_string());
                }
            }
        }
    }
    names
}

/// Run a single test by name.
fn run_single_test(file: &Path, _name: &str) -> Result<(), String> {
    // For now, compile the test file and check for errors.
    let source = fs::read_to_string(file).map_err(|e| format!("failed to read: {}", e))?;

    // Lex.
    let lex_result = adam_lexer::Lexer::new(&source).tokenize();
    if !lex_result.errors.is_empty() {
        return Err(lex_result
            .errors
            .iter()
            .map(|e| format!("lex error [{}:{}]: {}", e.line, e.column, e.message))
            .collect::<Vec<_>>()
            .join("\n"));
    }

    // Parse.
    let parse_result = adam_parser::Parser::new(lex_result.tokens).parse();
    if !parse_result.errors.is_empty() {
        return Err(parse_result
            .errors
            .iter()
            .map(|e| format!("parse error: {}", e.message))
            .collect::<Vec<_>>()
            .join("\n"));
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_test_names() {
        let source = r#"
test "addition works" {
    assert_eq(1 + 2, 3)
}

test "string contains" {
    assert("hello".contains("ell"))
}

fn helper() {
    // not a test
}
"#;
        let names = extract_test_names(source);
        assert_eq!(names, vec!["addition works", "string contains"]);
    }

    #[test]
    fn test_extract_test_names_empty() {
        let names = extract_test_names("fn main() { }");
        assert!(names.is_empty());
    }

    #[test]
    fn test_extract_test_names_single() {
        let names = extract_test_names("test \"my test\" {\n}");
        assert_eq!(names, vec!["my test"]);
    }

    #[test]
    fn test_discover_files_empty_dir() {
        // Test collect_test_files on an empty dir (avoids cwd race).
        let dir = tempfile::tempdir().unwrap();
        let mut files = Vec::new();
        collect_test_files(dir.path(), &mut files).unwrap();
        assert!(files.is_empty());
    }

    #[test]
    fn test_discover_files_with_tests_dir() {
        // Test collect_test_files directly to avoid cwd race in parallel tests.
        let dir = tempfile::tempdir().unwrap();
        let tests_dir = dir.path().join("tests");
        fs::create_dir(&tests_dir).unwrap();
        fs::write(tests_dir.join("math_test.adam"), "test \"x\" {}").unwrap();

        let mut files = Vec::new();
        collect_test_files(&tests_dir, &mut files).unwrap();
        assert_eq!(files.len(), 1);
    }
}
