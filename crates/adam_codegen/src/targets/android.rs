//! Android cross-compilation target configuration.
//!
//! Supports:
//! - `aarch64-linux-android` — physical Android devices (ARM64)
//! - `x86_64-linux-android` — Android Emulator (x86_64)
//! - `armv7-linux-androideabi` — older 32-bit devices

use std::env;
use std::path::PathBuf;

use super::{Arch, OutputFormat, Platform, TargetConfig};

/// Minimum Android API level supported by Adam.
pub const MIN_ANDROID_API: u32 = 24;

/// Create a target config for Android device (aarch64).
pub fn android_device_target() -> TargetConfig {
    android_target_with_api(Arch::Aarch64, false, MIN_ANDROID_API)
}

/// Create a target config for Android Emulator (x86_64).
pub fn android_emulator_target() -> TargetConfig {
    android_target_with_api(Arch::X86_64, true, MIN_ANDROID_API)
}

/// Create a target config for a specific Android architecture and API level.
pub fn android_target_with_api(arch: Arch, emulator: bool, api_level: u32) -> TargetConfig {
    let (triple, _ndk_arch) = match (arch, emulator) {
        (Arch::Aarch64, false) => ("aarch64-linux-android", "aarch64"),
        (Arch::Aarch64, true) => ("aarch64-linux-android", "aarch64"),
        (Arch::X86_64, _) => ("x86_64-linux-android", "x86_64"),
    };

    let platform = if emulator {
        Platform::AndroidEmulator
    } else {
        Platform::Android
    };

    let ndk_path = find_ndk_path();
    let toolchain = ndk_path.as_ref().map(|p| find_ndk_toolchain(p, arch));

    TargetConfig {
        platform,
        arch,
        triple: format!("{}{}", triple, api_level),
        output_format: OutputFormat::SharedLib,
        min_os_version: Some(api_level.to_string()),
        sdk_path: ndk_path.clone(),
        linker: toolchain
            .as_ref()
            .and_then(|t| t.clang.as_ref())
            .map(|p| p.to_string_lossy().into_owned())
            .unwrap_or_else(|| format!("{}{}-clang", triple, api_level)),
        linker_flags: vec![
            format!("--target={}{}", triple, api_level),
            "-fPIC".to_string(),
            "-shared".to_string(),
        ],
        system_libs: vec![
            "c".to_string(),
            "m".to_string(),
            "log".to_string(),
            "android".to_string(),
            "EGL".to_string(),
            "GLESv3".to_string(),
        ],
        frameworks: Vec::new(), // Android doesn't use frameworks.
        pic: true,
        opt_level: 2,
    }
}

/// Create target configs for all common Android architectures.
pub fn all_android_targets() -> Vec<TargetConfig> {
    vec![
        android_target_with_api(Arch::Aarch64, false, MIN_ANDROID_API),
        android_target_with_api(Arch::X86_64, true, MIN_ANDROID_API),
    ]
}

// ---------------------------------------------------------------------------
// NDK discovery
// ---------------------------------------------------------------------------

/// NDK toolchain paths for a specific architecture.
#[derive(Debug, Clone)]
pub struct NdkToolchain {
    /// Path to the NDK-provided clang.
    pub clang: Option<PathBuf>,
    /// Path to the NDK-provided clang++.
    pub clangpp: Option<PathBuf>,
    /// Path to the NDK-provided ar.
    pub ar: Option<PathBuf>,
    /// Path to the sysroot.
    pub sysroot: Option<PathBuf>,
}

/// Find the Android NDK path.
///
/// Searches in order:
/// 1. `ANDROID_NDK_HOME` environment variable
/// 2. `ANDROID_NDK_ROOT` environment variable
/// 3. `ANDROID_HOME/ndk/<version>` (latest version)
/// 4. Common installation paths
pub fn find_ndk_path() -> Option<PathBuf> {
    // 1. ANDROID_NDK_HOME
    if let Ok(path) = env::var("ANDROID_NDK_HOME") {
        let p = PathBuf::from(&path);
        if p.exists() {
            return Some(p);
        }
    }

    // 2. ANDROID_NDK_ROOT
    if let Ok(path) = env::var("ANDROID_NDK_ROOT") {
        let p = PathBuf::from(&path);
        if p.exists() {
            return Some(p);
        }
    }

    // 3. ANDROID_HOME/ndk/<version>
    if let Ok(home) = env::var("ANDROID_HOME") {
        let ndk_dir = PathBuf::from(&home).join("ndk");
        if ndk_dir.exists() {
            if let Some(latest) = find_latest_ndk_version(&ndk_dir) {
                return Some(latest);
            }
        }
    }

    // 4. Common paths.
    let common_paths = [
        dirs_home().join("Android/Sdk/ndk"),
        dirs_home().join("Library/Android/sdk/ndk"),
        PathBuf::from("/usr/local/lib/android/sdk/ndk"),
    ];

    for base in &common_paths {
        if base.exists() {
            if let Some(latest) = find_latest_ndk_version(base) {
                return Some(latest);
            }
        }
    }

    None
}

/// Find the latest NDK version in a directory containing versioned NDK installs.
fn find_latest_ndk_version(ndk_dir: &PathBuf) -> Option<PathBuf> {
    let mut versions: Vec<_> = std::fs::read_dir(ndk_dir)
        .ok()?
        .filter_map(|e| e.ok())
        .filter(|e| e.path().is_dir())
        .map(|e| e.path())
        .collect();
    versions.sort();
    versions.last().cloned()
}

/// Find NDK toolchain binaries for a given architecture.
pub fn find_ndk_toolchain(ndk_path: &PathBuf, arch: Arch) -> NdkToolchain {
    let host_tag = if cfg!(target_os = "macos") {
        if cfg!(target_arch = "aarch64") {
            "darwin-x86_64" // NDK uses x86_64 even on Apple Silicon (Rosetta)
        } else {
            "darwin-x86_64"
        }
    } else {
        "linux-x86_64"
    };

    let toolchain_bin = ndk_path
        .join("toolchains")
        .join("llvm")
        .join("prebuilt")
        .join(host_tag)
        .join("bin");

    let _ndk_arch_name = match arch {
        Arch::Aarch64 => "aarch64-linux-android",
        Arch::X86_64 => "x86_64-linux-android",
    };

    let clang = toolchain_bin.join("clang");
    let clangpp = toolchain_bin.join("clang++");
    let ar = toolchain_bin.join("llvm-ar");
    let sysroot = ndk_path
        .join("toolchains")
        .join("llvm")
        .join("prebuilt")
        .join(host_tag)
        .join("sysroot");

    NdkToolchain {
        clang: if clang.exists() { Some(clang) } else { None },
        clangpp: if clangpp.exists() {
            Some(clangpp)
        } else {
            None
        },
        ar: if ar.exists() { Some(ar) } else { None },
        sysroot: if sysroot.exists() {
            Some(sysroot)
        } else {
            None
        },
    }
}

/// Validate that the Android development environment is set up.
pub fn validate_android_toolchain() -> Result<(), Vec<String>> {
    let mut errors = Vec::new();

    if find_ndk_path().is_none() {
        errors.push(
            "Android NDK not found — set ANDROID_NDK_HOME or install via Android Studio"
                .to_string(),
        );
    }

    if let Some(ndk) = find_ndk_path() {
        let tc = find_ndk_toolchain(&ndk, Arch::Aarch64);
        if tc.clang.is_none() {
            errors.push("NDK clang not found".to_string());
        }
        if tc.sysroot.is_none() {
            errors.push("NDK sysroot not found".to_string());
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

/// Android-specific compilation settings.
pub struct AndroidCompileSettings {
    /// Android API level.
    pub api_level: u32,
    /// Whether to strip debug symbols.
    pub strip_debug: bool,
    /// Application ID (e.g. "com.example.myapp").
    pub application_id: String,
    /// Version code (integer, incremented for each release).
    pub version_code: u32,
    /// Version name (human-readable, e.g. "1.0.0").
    pub version_name: String,
}

impl Default for AndroidCompileSettings {
    fn default() -> Self {
        Self {
            api_level: MIN_ANDROID_API,
            strip_debug: false,
            application_id: "com.adam.app".to_string(),
            version_code: 1,
            version_name: "0.1.0".to_string(),
        }
    }
}

/// Get the user's home directory.
fn dirs_home() -> PathBuf {
    env::var("HOME")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("/"))
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_android_device_target() {
        let config = android_device_target();
        assert_eq!(config.platform, Platform::Android);
        assert_eq!(config.arch, Arch::Aarch64);
        assert!(config.triple.starts_with("aarch64-linux-android"));
        assert_eq!(config.output_format, OutputFormat::SharedLib);
        assert!(config.pic);
        assert!(config.system_libs.contains(&"log".to_string()));
        assert!(config.system_libs.contains(&"android".to_string()));
    }

    #[test]
    fn test_android_emulator_target() {
        let config = android_emulator_target();
        assert_eq!(config.platform, Platform::AndroidEmulator);
        assert_eq!(config.arch, Arch::X86_64);
        assert!(config.triple.starts_with("x86_64-linux-android"));
        assert_eq!(config.output_format, OutputFormat::SharedLib);
    }

    #[test]
    fn test_android_target_with_api() {
        let config = android_target_with_api(Arch::Aarch64, false, 30);
        assert!(config.triple.contains("30"));
        assert_eq!(config.min_os_version, Some("30".to_string()));
    }

    #[test]
    fn test_all_android_targets() {
        let targets = all_android_targets();
        assert_eq!(targets.len(), 2);
        assert_eq!(targets[0].arch, Arch::Aarch64);
        assert_eq!(targets[1].arch, Arch::X86_64);
    }

    #[test]
    fn test_android_linker_flags() {
        let config = android_device_target();
        let args = config.linker_command(
            std::path::Path::new("app.o"),
            std::path::Path::new("libadam_runtime.a"),
            std::path::Path::new("libadam_app.so"),
        );
        assert!(args.contains(&"-target".to_string()));
        assert!(args.iter().any(|a| a.contains("aarch64-linux-android")));
        assert!(args.contains(&"-landroid".to_string()));
        assert!(args.contains(&"-llog".to_string()));
    }

    #[test]
    fn test_android_shared_lib_output() {
        let config = android_device_target();
        assert_eq!(config.output_filename("adam_app"), "libadam_app.so");
    }

    #[test]
    fn test_android_compile_settings_default() {
        let settings = AndroidCompileSettings::default();
        assert_eq!(settings.api_level, MIN_ANDROID_API);
        assert!(!settings.strip_debug);
        assert_eq!(settings.application_id, "com.adam.app");
        assert_eq!(settings.version_code, 1);
    }

    #[test]
    fn test_find_ndk_does_not_panic() {
        let _ = find_ndk_path();
    }

    #[test]
    fn test_validate_android_toolchain_does_not_panic() {
        let _ = validate_android_toolchain();
    }

    #[test]
    fn test_dirs_home() {
        let home = dirs_home();
        // Should return something — either HOME env or "/"
        assert!(!home.to_string_lossy().is_empty());
    }
}
