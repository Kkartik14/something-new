//! Compilation pipeline — `adam build`, `adam run`, `adam check`.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

/// What intermediate representation to emit.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EmitStage {
    Tokens,
    Ast,
    Ir,
    Llvm,
}

/// Compilation options parsed from CLI arguments.
#[derive(Debug, Clone)]
pub struct CompileOpts {
    pub file: Option<PathBuf>,
    pub target: Option<String>,
    pub release: bool,
    pub emit: Option<EmitStage>,
    pub verbose: bool,
}

impl CompileOpts {
    pub fn parse(args: &[String]) -> Result<Self, i32> {
        let mut opts = CompileOpts {
            file: None,
            target: None,
            release: false,
            emit: None,
            verbose: false,
        };

        for arg in args {
            if arg.starts_with("--target=") {
                opts.target = Some(arg["--target=".len()..].to_string());
            } else if arg == "--release" {
                opts.release = true;
            } else if arg.starts_with("--emit=") {
                let stage = &arg["--emit=".len()..];
                opts.emit = Some(match stage {
                    "tokens" => EmitStage::Tokens,
                    "ast" => EmitStage::Ast,
                    "ir" => EmitStage::Ir,
                    "llvm" => EmitStage::Llvm,
                    _ => {
                        eprintln!("error: unknown emit stage '{}' (expected: tokens, ast, ir, llvm)", stage);
                        return Err(1);
                    }
                });
            } else if arg == "--verbose" || arg == "-v" {
                opts.verbose = true;
            } else if !arg.starts_with('-') {
                opts.file = Some(PathBuf::from(arg));
            } else {
                eprintln!("error: unknown option '{}'", arg);
                return Err(1);
            }
        }

        Ok(opts)
    }
}

/// Find the source file to compile.
fn find_source(opts: &CompileOpts) -> Result<PathBuf, String> {
    if let Some(ref file) = opts.file {
        if file.exists() {
            return Ok(file.clone());
        }
        return Err(format!("file not found: {}", file.display()));
    }

    // Look for adam.toml project.
    if Path::new("adam.toml").exists() {
        let main = Path::new("src").join("main.adam");
        if main.exists() {
            return Ok(main);
        }
        return Err("src/main.adam not found".to_string());
    }

    Err("no file specified and no adam.toml found".to_string())
}

/// Helper: lex source, returning tokens or formatted errors.
fn lex_source(source: &str) -> Result<Vec<adam_lexer::Token>, String> {
    let lex_result = adam_lexer::Lexer::new(source).tokenize();
    if !lex_result.errors.is_empty() {
        return Err(lex_result.errors.iter()
            .map(|e| format!("lex error [{}:{}]: {}", e.line, e.column, e.message))
            .collect::<Vec<_>>()
            .join("\n"));
    }
    Ok(lex_result.tokens)
}

/// Helper: parse tokens, returning AST or formatted errors.
fn parse_tokens(tokens: Vec<adam_lexer::Token>) -> Result<adam_ast::item::SourceFile, String> {
    let parse_result = adam_parser::Parser::new(tokens).parse();
    if !parse_result.errors.is_empty() {
        return Err(parse_result.errors.iter()
            .map(|e| format!("parse error: {}", e.message))
            .collect::<Vec<_>>()
            .join("\n"));
    }
    Ok(parse_result.ast)
}

/// Helper: run name resolution.
fn resolve_ast(ast: &adam_ast::item::SourceFile) -> Result<adam_resolve::ResolveResult, String> {
    let result = adam_resolve::resolve(ast);
    if !result.errors.is_empty() {
        return Err(result.errors.iter()
            .map(|e| format!("resolve error: {}", e))
            .collect::<Vec<_>>()
            .join("\n"));
    }
    Ok(result)
}

/// Helper: run type checking.
fn typecheck_ast(ast: &adam_ast::item::SourceFile) -> Result<adam_types::TypeCheckResult, String> {
    let result = adam_types::TypeChecker::new().check(ast);
    if !result.errors.is_empty() {
        return Err(result.errors.iter()
            .map(|e| format!("type error: {}", e.message))
            .collect::<Vec<_>>()
            .join("\n"));
    }
    Ok(result)
}

/// Helper: run borrow checking.
fn borrowcheck_ast(
    ast: &adam_ast::item::SourceFile,
    resolve: Option<&adam_resolve::ResolveResult>,
    types: Option<&adam_types::TypeCheckResult>,
) -> Result<(), String> {
    let result = adam_borrow::BorrowChecker::new().check(ast, resolve, types);
    if !result.errors.is_empty() {
        return Err(result.errors.iter()
            .map(|e| format!("borrow error: {}", e.message))
            .collect::<Vec<_>>()
            .join("\n"));
    }
    Ok(())
}

/// Full compilation pipeline: source -> tokens -> AST -> resolved -> typed -> borrow-checked -> IR -> LLVM -> object -> executable.
pub fn build(opts: &CompileOpts) -> Result<(), String> {
    let source_path = find_source(opts)?;
    let source = fs::read_to_string(&source_path)
        .map_err(|e| format!("failed to read {}: {}", source_path.display(), e))?;

    if opts.verbose {
        println!("[1/7] Lexing {}", source_path.display());
    }

    // Step 1: Lex.
    let tokens = lex_source(&source)?;

    if opts.emit == Some(EmitStage::Tokens) {
        for tok in &tokens {
            println!("{:?}", tok);
        }
        return Ok(());
    }

    // Step 2: Parse.
    if opts.verbose {
        println!("[2/7] Parsing");
    }
    let ast = parse_tokens(tokens)?;

    if opts.emit == Some(EmitStage::Ast) {
        println!("{:#?}", ast);
        return Ok(());
    }

    // Step 3: Resolve names.
    if opts.verbose {
        println!("[3/7] Resolving names");
    }
    let resolved = resolve_ast(&ast)?;

    // Step 4: Type check.
    if opts.verbose {
        println!("[4/7] Type checking");
    }
    let typed = typecheck_ast(&ast)?;

    // Step 5: Borrow check.
    if opts.verbose {
        println!("[5/7] Borrow checking");
    }
    borrowcheck_ast(&ast, Some(&resolved), Some(&typed))?;

    // Step 6: Lower to IR.
    if opts.verbose {
        println!("[6/7] Lowering to IR");
    }
    let ir_module = adam_ir::lower_module(&ast);

    if opts.emit == Some(EmitStage::Ir) {
        println!("{:#?}", ir_module);
        return Ok(());
    }

    // Step 7: Code generation.
    if opts.verbose {
        println!("[7/7] Generating code");
    }
    let context = inkwell::context::Context::create();
    let module_name = source_path.file_stem()
        .map(|s| s.to_string_lossy().into_owned())
        .unwrap_or_else(|| "main".to_string());

    let mut codegen = if let Some(ref target) = opts.target {
        let target_config = adam_codegen::targets::Platform::from_cli_name(target)
            .ok_or_else(|| format!("unknown target: {}", target))?;
        let triple = match target_config {
            adam_codegen::targets::Platform::IOS => "aarch64-apple-ios",
            adam_codegen::targets::Platform::IOSSimulator => {
                if cfg!(target_arch = "aarch64") {
                    "aarch64-apple-ios-simulator"
                } else {
                    "x86_64-apple-ios-simulator"
                }
            }
            adam_codegen::targets::Platform::Android => "aarch64-linux-android24",
            _ => return Err(format!("unsupported cross-compilation target: {}", target)),
        };
        adam_codegen::CodeGen::with_triple(&context, &module_name, triple)
    } else {
        adam_codegen::CodeGen::new(&context, &module_name)
    };

    codegen.codegen_module(&ir_module);

    if opts.emit == Some(EmitStage::Llvm) {
        println!("{}", codegen.print_to_string());
        return Ok(());
    }

    // Emit object file.
    let build_dir = PathBuf::from("build");
    fs::create_dir_all(&build_dir)
        .map_err(|e| format!("failed to create build dir: {}", e))?;

    let object_path = build_dir.join(format!("{}.o", module_name));
    codegen.emit_object_file(&object_path)
        .map_err(|e| format!("failed to emit object file: {}", e))?;

    // Link.
    let runtime_lib = adam_codegen::find_runtime_library(None)
        .map_err(|e| format!("{}", e))?;
    let exe_path = build_dir.join(&module_name);
    adam_codegen::link_object(&object_path, &runtime_lib, &exe_path)
        .map_err(|e| format!("link error: {}", e))?;

    if opts.verbose {
        println!("Built: {}", exe_path.display());
    } else {
        println!("{}", exe_path.display());
    }

    Ok(())
}

/// Compile and execute.
pub fn run(opts: &CompileOpts) -> Result<(), String> {
    build(opts)?;

    let source_path = find_source(opts)?;
    let module_name = source_path.file_stem()
        .map(|s| s.to_string_lossy().into_owned())
        .unwrap_or_else(|| "main".to_string());
    let exe_path = PathBuf::from("build").join(&module_name);

    let status = Command::new(&exe_path)
        .status()
        .map_err(|e| format!("failed to execute {}: {}", exe_path.display(), e))?;

    if !status.success() {
        return Err(format!("runtime error: process exited with {}", status));
    }

    Ok(())
}

/// Type-check only (skip codegen for fast feedback).
pub fn check(opts: &CompileOpts) -> Result<(), String> {
    let source_path = find_source(opts)?;
    let source = fs::read_to_string(&source_path)
        .map_err(|e| format!("failed to read {}: {}", source_path.display(), e))?;

    // Lex.
    let tokens = lex_source(&source)?;

    // Parse.
    let ast = parse_tokens(tokens)?;

    // Resolve.
    let resolved = resolve_ast(&ast)?;

    // Type check.
    let typed = typecheck_ast(&ast)?;

    // Borrow check.
    borrowcheck_ast(&ast, Some(&resolved), Some(&typed))?;

    println!("OK — no errors found in {}", source_path.display());
    Ok(())
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_compile_opts_empty() {
        let opts = CompileOpts::parse(&[]).unwrap();
        assert!(opts.file.is_none());
        assert!(opts.target.is_none());
        assert!(!opts.release);
        assert!(opts.emit.is_none());
        assert!(!opts.verbose);
    }

    #[test]
    fn test_parse_compile_opts_full() {
        let args: Vec<String> = vec![
            "src/main.adam".to_string(),
            "--target=ios".to_string(),
            "--release".to_string(),
            "--emit=ast".to_string(),
            "--verbose".to_string(),
        ];
        let opts = CompileOpts::parse(&args).unwrap();
        assert_eq!(opts.file, Some(PathBuf::from("src/main.adam")));
        assert_eq!(opts.target, Some("ios".to_string()));
        assert!(opts.release);
        assert_eq!(opts.emit, Some(EmitStage::Ast));
        assert!(opts.verbose);
    }

    #[test]
    fn test_parse_compile_opts_emit_stages() {
        for (name, stage) in &[
            ("tokens", EmitStage::Tokens),
            ("ast", EmitStage::Ast),
            ("ir", EmitStage::Ir),
            ("llvm", EmitStage::Llvm),
        ] {
            let args = vec![format!("--emit={}", name)];
            let opts = CompileOpts::parse(&args).unwrap();
            assert_eq!(opts.emit, Some(*stage));
        }
    }

    #[test]
    fn test_parse_compile_opts_unknown_emit() {
        let args = vec!["--emit=wasm".to_string()];
        let result = CompileOpts::parse(&args);
        assert!(result.is_err());
    }

    #[test]
    fn test_find_source_nonexistent() {
        let opts = CompileOpts {
            file: Some(PathBuf::from("nonexistent.adam")),
            target: None,
            release: false,
            emit: None,
            verbose: false,
        };
        let result = find_source(&opts);
        assert!(result.is_err());
    }
}
