use std::env;

fn main() {
    let arch = env::var("CARGO_CFG_TARGET_ARCH").unwrap();

    let asm_file = match arch.as_str() {
        "aarch64" => "src/arch/aarch64.S",
        "x86_64" => "src/arch/x86_64.S",
        _ => {
            println!(
                "cargo:warning=Unsupported architecture for context switching: {}",
                arch
            );
            return;
        }
    };

    cc::Build::new()
        .file(asm_file)
        .compile("adam_context_switch");

    println!("cargo:rerun-if-changed={}", asm_file);
    println!("cargo:rerun-if-changed=build.rs");
}
