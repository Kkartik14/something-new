//! Adam CLI — the main command-line interface for the Adam toolchain.
//!
//! Commands:
//!   adam new <name>          Create a new Adam project
//!   adam build [file]        Compile the project/file
//!   adam run [file]          Compile and execute
//!   adam check [file]        Type-check without codegen
//!   adam test [path]         Run tests
//!   adam fmt [path]          Format source files
//!   adam bench [path]        Run benchmarks
//!   adam pkg <subcmd>        Package management
//!   adam clean               Remove build artifacts

mod bench;
mod compile;
mod format;
mod manifest;
mod project;
mod testrunner;

use std::env;
use std::process;

const VERSION: &str = "0.1.0";

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        print_usage();
        process::exit(0);
    }

    let result = match args[1].as_str() {
        "new" => cmd_new(&args[2..]),
        "build" => cmd_build(&args[2..]),
        "run" => cmd_run(&args[2..]),
        "check" => cmd_check(&args[2..]),
        "test" => cmd_test(&args[2..]),
        "fmt" => cmd_fmt(&args[2..]),
        "bench" => cmd_bench(&args[2..]),
        "pkg" => cmd_pkg(&args[2..]),
        "clean" => cmd_clean(&args[2..]),
        "--version" | "-V" => {
            println!("adam {}", VERSION);
            Ok(())
        }
        "--help" | "-h" | "help" => {
            print_usage();
            Ok(())
        }
        cmd => {
            eprintln!("error: unknown command '{}'", cmd);
            eprintln!("Run 'adam --help' for usage.");
            Err(1)
        }
    };

    match result {
        Ok(()) => process::exit(0),
        Err(code) => process::exit(code),
    }
}

fn print_usage() {
    println!("adam {} — the Adam programming language", VERSION);
    println!();
    println!("USAGE:");
    println!("    adam <command> [options] [arguments]");
    println!();
    println!("COMMANDS:");
    println!("    new <name>          Create a new Adam project");
    println!("    build [file]        Compile the project or a single file");
    println!("    run [file]          Compile and execute");
    println!("    check [file]        Type-check without code generation (fast)");
    println!("    test [path]         Run tests");
    println!("    fmt [path]          Format source files");
    println!("    bench [path]        Run benchmarks");
    println!("    pkg <subcommand>    Package management");
    println!("    clean               Remove build artifacts");
    println!();
    println!("OPTIONS:");
    println!("    --target=<target>   Target platform (ios, android, macos, linux)");
    println!("    --release           Optimize for release");
    println!("    --emit=<stage>      Dump intermediate output (tokens, ast, ir, llvm)");
    println!("    --verbose           Show compilation steps");
    println!("    --help, -h          Show this help");
    println!("    --version, -V       Show version");
}

// ---------------------------------------------------------------------------
// Command implementations
// ---------------------------------------------------------------------------

fn cmd_new(args: &[String]) -> Result<(), i32> {
    if args.is_empty() {
        eprintln!("error: missing project name");
        eprintln!("Usage: adam new <name>");
        return Err(1);
    }
    let name = &args[0];
    project::create_project(name).map_err(|e| {
        eprintln!("error: {}", e);
        1
    })
}

fn cmd_build(args: &[String]) -> Result<(), i32> {
    let opts = compile::CompileOpts::parse(args)?;
    compile::build(&opts).map_err(|e| {
        eprintln!("{}", e);
        1
    })
}

fn cmd_run(args: &[String]) -> Result<(), i32> {
    let opts = compile::CompileOpts::parse(args)?;
    compile::run(&opts).map_err(|e| {
        eprintln!("{}", e);
        match e.as_str() {
            s if s.contains("runtime error") => 2,
            _ => 1,
        }
    })
}

fn cmd_check(args: &[String]) -> Result<(), i32> {
    let opts = compile::CompileOpts::parse(args)?;
    compile::check(&opts).map_err(|e| {
        eprintln!("{}", e);
        1
    })
}

fn cmd_test(args: &[String]) -> Result<(), i32> {
    let filter = args.first().map(|s| s.as_str());
    testrunner::run_tests(filter).map_err(|e| {
        eprintln!("{}", e);
        1
    })
}

fn cmd_fmt(args: &[String]) -> Result<(), i32> {
    let check_only = args.iter().any(|a| a == "--check");
    let path = args
        .iter()
        .find(|a| !a.starts_with('-'))
        .map(|s| s.as_str());
    format::format_files(path, check_only).map_err(|e| {
        eprintln!("{}", e);
        1
    })
}

fn cmd_bench(args: &[String]) -> Result<(), i32> {
    let filter = args.first().map(|s| s.as_str());
    bench::run_benchmarks(filter).map_err(|e| {
        eprintln!("{}", e);
        1
    })
}

fn cmd_pkg(args: &[String]) -> Result<(), i32> {
    if args.is_empty() {
        eprintln!("error: missing pkg subcommand");
        eprintln!("Usage: adam pkg <init|add|remove|update|install>");
        return Err(1);
    }
    match args[0].as_str() {
        "init" => manifest::pkg_init(),
        "add" => {
            if args.len() < 2 {
                eprintln!("error: missing package name");
                return Err(1);
            }
            manifest::pkg_add(&args[1], args.get(2).map(|s| s.as_str()))
        }
        "remove" => {
            if args.len() < 2 {
                eprintln!("error: missing package name");
                return Err(1);
            }
            manifest::pkg_remove(&args[1])
        }
        "update" => manifest::pkg_update(),
        "install" => manifest::pkg_install(),
        sub => {
            eprintln!("error: unknown pkg subcommand '{}'", sub);
            Err(1)
        }
    }
}

fn cmd_clean(_args: &[String]) -> Result<(), i32> {
    let build_dir = std::path::Path::new("build");
    if build_dir.exists() {
        std::fs::remove_dir_all(build_dir).map_err(|e| {
            eprintln!("error: failed to remove build directory: {}", e);
            1
        })?;
        println!("Cleaned build artifacts.");
    } else {
        println!("Nothing to clean.");
    }
    Ok(())
}
