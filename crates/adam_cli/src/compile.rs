//! Compilation pipeline — `adam build`, `adam run`, `adam check`.

use std::collections::HashMap;
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

/// Resolve a dotted module path to a file path relative to a source directory.
///
/// Tries `<src_dir>/<seg1>/<seg2>.adam` first, then `<src_dir>/<seg1>/<seg2>/mod.adam`.
fn resolve_module_path(src_dir: &Path, segments: &[String]) -> Option<PathBuf> {
    if segments.is_empty() {
        return None;
    }
    // Build the directory path from all segments except the last.
    let mut dir = src_dir.to_path_buf();
    for seg in &segments[..segments.len() - 1] {
        dir.push(seg);
    }
    let last = &segments[segments.len() - 1];

    // Try <dir>/<last>.adam
    let file_path = dir.join(format!("{}.adam", last));
    if file_path.exists() {
        return Some(file_path);
    }

    // Try <dir>/<last>/mod.adam
    let mod_path = dir.join(last).join("mod.adam");
    if mod_path.exists() {
        return Some(mod_path);
    }

    None
}

/// Collect all `use` paths from a source file AST.
fn collect_use_paths(ast: &adam_ast::item::SourceFile) -> Vec<Vec<String>> {
    let mut paths = Vec::new();
    for item in &ast.items {
        if let adam_ast::item::Item::Use(use_decl) = &item.node {
            let segments: Vec<String> = use_decl.path.iter().map(|s| s.name.clone()).collect();
            if segments.len() >= 2 {
                // Module path is everything except the last segment for Single imports.
                match &use_decl.items {
                    adam_ast::item::UseItems::Single => {
                        paths.push(segments[..segments.len() - 1].to_vec());
                    }
                    _ => {
                        paths.push(segments);
                    }
                }
            }
        }
    }
    paths
}

/// Load and parse all modules referenced by use declarations.
fn load_modules(
    src_dir: &Path,
    ast: &adam_ast::item::SourceFile,
) -> Result<HashMap<String, adam_ast::item::SourceFile>, String> {
    let mut modules = HashMap::new();
    let use_paths = collect_use_paths(ast);

    for mod_segments in use_paths {
        let mod_key = mod_segments.join(".");
        if modules.contains_key(&mod_key) {
            continue;
        }
        if let Some(file_path) = resolve_module_path(src_dir, &mod_segments) {
            let source = fs::read_to_string(&file_path)
                .map_err(|e| format!("failed to read module {}: {}", mod_key, e))?;
            let tokens = lex_source(&source)?;
            let mod_ast = parse_tokens(tokens)?;
            modules.insert(mod_key, mod_ast);
        }
        // If module file not found, skip silently — the resolver will handle undefined names.
    }

    Ok(modules)
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

    // Step 3: Resolve names (with cross-file module support).
    if opts.verbose {
        println!("[3/7] Resolving names");
    }
    let src_dir = source_path.parent().unwrap_or(Path::new("."));
    let modules = load_modules(src_dir, &ast)?;
    let resolved = if modules.is_empty() {
        resolve_ast(&ast)?
    } else {
        if opts.verbose {
            println!("  loaded {} module(s): {}", modules.len(),
                modules.keys().cloned().collect::<Vec<_>>().join(", "));
        }
        let result = adam_resolve::resolve_multi(&ast, &modules);
        if !result.errors.is_empty() {
            return Err(result.errors.iter()
                .map(|e| format!("resolve error: {}", e))
                .collect::<Vec<_>>()
                .join("\n"));
        }
        result
    };

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

    // Resolve target configuration.
    let target_config = if let Some(ref target) = opts.target {
        let platform = adam_codegen::targets::Platform::from_cli_name(target)
            .ok_or_else(|| format!("unknown target '{}' (expected: ios, ios-simulator, android, android-emulator, macos, linux)", target))?;
        Some(match platform {
            adam_codegen::targets::Platform::IOS => adam_codegen::targets::ios::ios_device_target(),
            adam_codegen::targets::Platform::IOSSimulator => adam_codegen::targets::ios::ios_simulator_target(),
            adam_codegen::targets::Platform::Android => adam_codegen::targets::android::android_device_target(),
            adam_codegen::targets::Platform::AndroidEmulator => adam_codegen::targets::android::android_emulator_target(),
            adam_codegen::targets::Platform::MacOS | adam_codegen::targets::Platform::Linux => {
                adam_codegen::TargetConfig::host()
            }
        })
    } else {
        None
    };

    let mut codegen = if let Some(ref config) = target_config {
        adam_codegen::CodeGen::with_triple(&context, &module_name, &config.triple)
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

    if let Some(ref config) = target_config {
        // Cross-compilation: use target-specific output name and linker.
        let output_name = config.output_filename(&module_name);
        let output_path = build_dir.join(&output_name);
        adam_codegen::link_with_target_config(&object_path, &runtime_lib, &output_path, config)
            .map_err(|e| format!("link error: {}", e))?;
        if opts.verbose {
            println!("Built: {} (target: {})", output_path.display(), config.platform);
        } else {
            println!("{}", output_path.display());
        }
    } else {
        // Native build: use standard linker.
        let exe_path = build_dir.join(&module_name);
        adam_codegen::link_object(&object_path, &runtime_lib, &exe_path)
            .map_err(|e| format!("link error: {}", e))?;
        if opts.verbose {
            println!("Built: {}", exe_path.display());
        } else {
            println!("{}", exe_path.display());
        }
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

    // ============================================================
    // End-to-end pipeline tests (source → IR, no LLVM linking)
    // ============================================================

    /// Run the pipeline from source through IR lowering, returning the IR module.
    fn pipeline_to_ir(src: &str) -> adam_ir::ir::IrModule {
        let tokens = lex_source(src).expect("lex failed");
        let ast = parse_tokens(tokens).expect("parse failed");
        let resolved = resolve_ast(&ast).expect("resolve failed");
        let typed = typecheck_ast(&ast).expect("typecheck failed");
        borrowcheck_ast(&ast, Some(&resolved), Some(&typed)).expect("borrowcheck failed");
        adam_ir::lower_module(&ast)
    }

    #[test]
    fn e2e_hello_world() {
        let ir = pipeline_to_ir("fn main() {\n    print(\"Hello, Adam!\")\n}");
        assert!(
            ir.functions.iter().any(|f| f.name == "main"),
            "IR should contain a 'main' function"
        );
    }

    #[test]
    fn e2e_fibonacci() {
        let ir = pipeline_to_ir(
            "fn fib(n i32) -> i32 {\n    if n <= 1 {\n        n\n    } else {\n        fib(n - 1) + fib(n - 2)\n    }\n}\nfn main() {\n    result := fib(10)\n    print(\"done\")\n}"
        );
        assert!(ir.functions.iter().any(|f| f.name == "fib"), "IR should contain 'fib'");
        assert!(ir.functions.iter().any(|f| f.name == "main"), "IR should contain 'main'");
    }

    #[test]
    fn e2e_generics() {
        let ir = pipeline_to_ir(
            "fn id[T](x T) -> T {\n    x\n}\nfn main() {\n    a := id(42)\n    b := id(\"hello\")\n}"
        );
        assert!(ir.functions.iter().any(|f| f.name == "main"), "IR should contain 'main'");
    }

    #[test]
    fn e2e_structs_and_methods() {
        let ir = pipeline_to_ir(
            "struct Point {\n    x i32\n    y i32\n}\nimpl Point {\n    fn sum(self) -> i32 {\n        self.x + self.y\n    }\n}\nfn main() {\n    p := Point { x: 3, y: 4 }\n}"
        );
        assert!(ir.functions.iter().any(|f| f.name == "main"), "IR should contain 'main'");
    }

    #[test]
    fn e2e_enums_and_match() {
        let ir = pipeline_to_ir(
            "enum Color {\n    Red\n    Green\n    Blue\n}\nfn describe(c Color) -> i32 {\n    match c {\n        Color.Red => 0\n        Color.Green => 1\n        Color.Blue => 2\n    }\n}\nfn main() {\n    c := Red\n}"
        );
        assert!(ir.functions.iter().any(|f| f.name == "describe"), "IR should contain 'describe'");
    }

    #[test]
    fn e2e_ownership_and_borrowing() {
        // Tests that the borrow checker accepts valid ownership patterns.
        let ir = pipeline_to_ir(
            "fn consume(own s String) {\n    print(s)\n}\nfn main() {\n    s := \"hello\"\n    consume(s)\n}"
        );
        assert!(ir.functions.iter().any(|f| f.name == "main"), "IR should contain 'main'");
    }

    #[test]
    fn e2e_trait_with_impl() {
        let ir = pipeline_to_ir(
            "trait Greet {\n    fn greet(self) -> String\n}\nstruct Bot {\n    name String\n}\nimpl Greet for Bot {\n    fn greet(self) -> String {\n        self.name\n    }\n}\nfn main() {\n    b := Bot { name: \"Ada\" }\n}"
        );
        assert!(ir.functions.iter().any(|f| f.name == "main"), "IR should contain 'main'");
    }

    #[test]
    fn e2e_closures() {
        let ir = pipeline_to_ir(
            "fn apply(f fn(i32) -> i32, x i32) -> i32 {\n    f(x)\n}\nfn main() {\n    double := |x i32| x * 2\n    result := apply(double, 21)\n}"
        );
        assert!(ir.functions.iter().any(|f| f.name == "main"), "IR should contain 'main'");
    }

    // ============================================================
    // Cross-compilation target resolution tests
    // ============================================================

    #[test]
    fn test_target_ios_resolves() {
        let platform = adam_codegen::targets::Platform::from_cli_name("ios").unwrap();
        assert_eq!(platform, adam_codegen::targets::Platform::IOS);
        let config = adam_codegen::targets::ios::ios_device_target();
        assert_eq!(config.triple, "aarch64-apple-ios");
        assert!(config.frameworks.contains(&"UIKit".to_string()));
    }

    #[test]
    fn test_target_ios_simulator_resolves() {
        let platform = adam_codegen::targets::Platform::from_cli_name("ios-simulator").unwrap();
        assert_eq!(platform, adam_codegen::targets::Platform::IOSSimulator);
        let config = adam_codegen::targets::ios::ios_simulator_target();
        assert!(config.triple.contains("simulator"));
    }

    #[test]
    fn test_target_android_resolves() {
        let platform = adam_codegen::targets::Platform::from_cli_name("android").unwrap();
        assert_eq!(platform, adam_codegen::targets::Platform::Android);
        let config = adam_codegen::targets::android::android_device_target();
        assert!(config.triple.starts_with("aarch64-linux-android"));
        assert!(config.system_libs.contains(&"android".to_string()));
    }

    #[test]
    fn test_target_android_emulator_resolves() {
        let platform = adam_codegen::targets::Platform::from_cli_name("android-emulator").unwrap();
        assert_eq!(platform, adam_codegen::targets::Platform::AndroidEmulator);
        let config = adam_codegen::targets::android::android_emulator_target();
        assert!(config.triple.starts_with("x86_64-linux-android"));
    }

    #[test]
    fn test_target_unknown_fails() {
        assert!(adam_codegen::targets::Platform::from_cli_name("wasm").is_none());
        assert!(adam_codegen::targets::Platform::from_cli_name("windows").is_none());
    }

    #[test]
    fn test_target_output_filename() {
        let ios_config = adam_codegen::targets::ios::ios_device_target();
        assert_eq!(ios_config.output_filename("app"), "libapp.a");

        let android_config = adam_codegen::targets::android::android_device_target();
        assert_eq!(android_config.output_filename("app"), "libapp.so");
    }

    #[test]
    fn test_host_target_config() {
        let host = adam_codegen::TargetConfig::host();
        #[cfg(target_os = "macos")]
        assert_eq!(host.platform, adam_codegen::targets::Platform::MacOS);
        #[cfg(target_os = "linux")]
        assert_eq!(host.platform, adam_codegen::targets::Platform::Linux);
        assert!(!host.triple.is_empty());
    }
}
