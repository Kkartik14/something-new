use std::path::{Path, PathBuf};
use std::process::Command;

use crate::CodeGen;

/// Output from the compilation pipeline.
pub struct CompileOutput {
    pub object_path: PathBuf,
    pub executable_path: PathBuf,
}

impl<'ctx> CodeGen<'ctx> {
    /// Compile and link an Adam module into an executable.
    ///
    /// Steps:
    /// 1. Emit LLVM module as object file (.o)
    /// 2. Link with Adam runtime static library
    /// 3. Link with system libraries
    /// 4. Produce executable binary
    pub fn compile_and_link(
        &self,
        output_name: &str,
        runtime_lib_path: &Path,
        output_dir: &Path,
    ) -> Result<CompileOutput, String> {
        // 1. Emit object file.
        let object_path = output_dir.join(format!("{}.o", output_name));
        self.emit_object_file(&object_path)?;

        // 2. Link into executable.
        let executable_path = output_dir.join(output_name);
        link_object(&object_path, runtime_lib_path, &executable_path)?;

        Ok(CompileOutput {
            object_path,
            executable_path,
        })
    }
}

/// Link an object file with the Adam runtime into an executable.
pub fn link_object(
    object_path: &Path,
    runtime_lib_path: &Path,
    output_path: &Path,
) -> Result<(), String> {
    // Use clang as the linker — it handles platform-specific linking details.
    let mut cmd = Command::new("clang");

    cmd.arg(object_path);
    cmd.arg(runtime_lib_path);
    cmd.arg("-o");
    cmd.arg(output_path);

    // Platform-specific flags.
    #[cfg(target_os = "macos")]
    {
        cmd.arg("-lSystem");
        cmd.arg("-lresolv");
    }

    #[cfg(target_os = "linux")]
    {
        cmd.arg("-lc");
        cmd.arg("-lpthread");
        cmd.arg("-ldl");
        cmd.arg("-lm");
    }

    let output = cmd
        .output()
        .map_err(|e| format!("failed to run linker: {}", e))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!("linker failed: {}", stderr));
    }

    Ok(())
}

/// Link an object file using a TargetConfig for cross-compilation.
pub fn link_with_target_config(
    object_path: &Path,
    runtime_lib_path: &Path,
    output_path: &Path,
    config: &crate::targets::TargetConfig,
) -> Result<(), String> {
    let mut cmd = Command::new(&config.linker);
    let args = config.linker_command(object_path, runtime_lib_path, output_path);
    for arg in &args {
        cmd.arg(arg);
    }

    let output = cmd
        .output()
        .map_err(|e| format!("failed to run linker '{}': {}", config.linker, e))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!(
            "linker failed for target {}: {}",
            config.platform, stderr
        ));
    }

    Ok(())
}

/// Find the Adam runtime static library.
///
/// Searches for `libadam_runtime.a` in:
/// 1. The provided directory
/// 2. The cargo target directory
/// 3. Common build output locations
pub fn find_runtime_library(search_dir: Option<&Path>) -> Result<PathBuf, String> {
    let candidates = vec![
        search_dir.map(|d| d.join("libadam_runtime.a")),
        // Cargo debug build output
        Some(PathBuf::from("target/debug/libadam_runtime.a")),
        // Cargo release build output
        Some(PathBuf::from("target/release/libadam_runtime.a")),
    ];

    for candidate in candidates.into_iter().flatten() {
        if candidate.exists() {
            return Ok(candidate);
        }
    }

    Err("could not find libadam_runtime.a — build adam_runtime first".to_string())
}
