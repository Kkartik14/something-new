//! iOS cross-compilation target configuration.
//!
//! Supports:
//! - `aarch64-apple-ios` — physical iOS devices (iPhone, iPad)
//! - `x86_64-apple-ios-simulator` — iOS Simulator on Intel Macs
//! - `aarch64-apple-ios-simulator` — iOS Simulator on Apple Silicon Macs

use std::path::PathBuf;
use std::process::Command;

use super::{Arch, OutputFormat, Platform, TargetConfig};

/// Minimum iOS version supported by Adam.
pub const MIN_IOS_VERSION: &str = "15.0";

/// Create a target config for iOS device (aarch64).
pub fn ios_device_target() -> TargetConfig {
    TargetConfig {
        platform: Platform::IOS,
        arch: Arch::Aarch64,
        triple: "aarch64-apple-ios".to_string(),
        output_format: OutputFormat::StaticLib,
        min_os_version: Some(MIN_IOS_VERSION.to_string()),
        sdk_path: find_ios_sdk(),
        linker: "clang".to_string(),
        linker_flags: vec!["-arch".to_string(), "arm64".to_string()],
        system_libs: vec!["System".to_string()],
        frameworks: vec![
            "UIKit".to_string(),
            "Foundation".to_string(),
            "Metal".to_string(),
            "QuartzCore".to_string(),
        ],
        pic: true,
        opt_level: 2,
    }
}

/// Create a target config for iOS Simulator.
///
/// Automatically selects the right architecture based on the host.
pub fn ios_simulator_target() -> TargetConfig {
    let (arch, triple, arch_flag) = if cfg!(target_arch = "aarch64") {
        (Arch::Aarch64, "aarch64-apple-ios-simulator", "arm64")
    } else {
        (Arch::X86_64, "x86_64-apple-ios-simulator", "x86_64")
    };

    TargetConfig {
        platform: Platform::IOSSimulator,
        arch,
        triple: triple.to_string(),
        output_format: OutputFormat::StaticLib,
        min_os_version: Some(MIN_IOS_VERSION.to_string()),
        sdk_path: find_ios_simulator_sdk(),
        linker: "clang".to_string(),
        linker_flags: vec!["-arch".to_string(), arch_flag.to_string()],
        system_libs: vec!["System".to_string()],
        frameworks: vec![
            "UIKit".to_string(),
            "Foundation".to_string(),
            "Metal".to_string(),
            "QuartzCore".to_string(),
        ],
        pic: true,
        opt_level: 2,
    }
}

/// Create a target config for a specific iOS architecture.
pub fn ios_target(arch: Arch, simulator: bool) -> TargetConfig {
    if simulator {
        let mut config = ios_simulator_target();
        config.arch = arch;
        config.triple = match arch {
            Arch::Aarch64 => "aarch64-apple-ios-simulator".to_string(),
            Arch::X86_64 => "x86_64-apple-ios-simulator".to_string(),
        };
        config.linker_flags = vec![
            "-arch".to_string(),
            match arch {
                Arch::Aarch64 => "arm64".to_string(),
                Arch::X86_64 => "x86_64".to_string(),
            },
        ];
        config
    } else {
        let mut config = ios_device_target();
        config.arch = arch;
        config
    }
}

/// Find the iOS SDK path using `xcrun`.
pub fn find_ios_sdk() -> Option<PathBuf> {
    find_sdk("iphoneos")
}

/// Find the iOS Simulator SDK path using `xcrun`.
pub fn find_ios_simulator_sdk() -> Option<PathBuf> {
    find_sdk("iphonesimulator")
}

/// Find an Apple SDK path using `xcrun --sdk <name> --show-sdk-path`.
fn find_sdk(sdk_name: &str) -> Option<PathBuf> {
    let output = Command::new("xcrun")
        .args(["--sdk", sdk_name, "--show-sdk-path"])
        .output()
        .ok()?;

    if output.status.success() {
        let path = String::from_utf8_lossy(&output.stdout).trim().to_string();
        if !path.is_empty() {
            return Some(PathBuf::from(path));
        }
    }
    None
}

/// Find the path to `clang` for iOS cross-compilation.
pub fn find_ios_clang() -> Option<PathBuf> {
    let output = Command::new("xcrun")
        .args(["--sdk", "iphoneos", "--find", "clang"])
        .output()
        .ok()?;

    if output.status.success() {
        let path = String::from_utf8_lossy(&output.stdout).trim().to_string();
        if !path.is_empty() {
            return Some(PathBuf::from(path));
        }
    }
    None
}

/// Find the path to `ar` for creating static libraries.
pub fn find_ios_ar() -> Option<PathBuf> {
    let output = Command::new("xcrun")
        .args(["--sdk", "iphoneos", "--find", "ar"])
        .output()
        .ok()?;

    if output.status.success() {
        let path = String::from_utf8_lossy(&output.stdout).trim().to_string();
        if !path.is_empty() {
            return Some(PathBuf::from(path));
        }
    }
    None
}

/// Validate that the iOS development environment is set up.
pub fn validate_ios_toolchain() -> Result<(), Vec<String>> {
    let mut errors = Vec::new();

    // Check for Xcode command-line tools.
    if Command::new("xcrun").arg("--version").output().is_err() {
        errors.push("xcrun not found — install Xcode Command Line Tools".to_string());
    }

    // Check for iOS SDK.
    if find_ios_sdk().is_none() {
        errors.push("iOS SDK not found — install Xcode with iOS support".to_string());
    }

    // Check for iOS Simulator SDK.
    if find_ios_simulator_sdk().is_none() {
        errors.push("iOS Simulator SDK not found".to_string());
    }

    // Check for clang.
    if find_ios_clang().is_none() {
        errors.push("clang for iOS not found".to_string());
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

/// iOS-specific compilation settings for the Adam runtime.
pub struct IosCompileSettings {
    /// Whether to embed bitcode (required for App Store submission).
    pub embed_bitcode: bool,
    /// Whether to strip debug symbols for release builds.
    pub strip_debug: bool,
    /// Code signing identity (e.g. "Apple Development: ...").
    pub code_sign_identity: Option<String>,
    /// Provisioning profile UUID.
    pub provisioning_profile: Option<String>,
    /// Bundle identifier (e.g. "com.example.myapp").
    pub bundle_id: String,
}

impl Default for IosCompileSettings {
    fn default() -> Self {
        Self {
            embed_bitcode: false,
            strip_debug: false,
            code_sign_identity: None,
            provisioning_profile: None,
            bundle_id: "com.adam.app".to_string(),
        }
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ios_device_target() {
        let config = ios_device_target();
        assert_eq!(config.platform, Platform::IOS);
        assert_eq!(config.arch, Arch::Aarch64);
        assert_eq!(config.triple, "aarch64-apple-ios");
        assert_eq!(config.output_format, OutputFormat::StaticLib);
        assert_eq!(config.min_os_version, Some(MIN_IOS_VERSION.to_string()));
        assert!(config.pic);
        assert!(config.frameworks.contains(&"UIKit".to_string()));
        assert!(config.frameworks.contains(&"Metal".to_string()));
    }

    #[test]
    fn test_ios_simulator_target() {
        let config = ios_simulator_target();
        assert_eq!(config.platform, Platform::IOSSimulator);
        assert!(config.triple.contains("simulator"));
        assert_eq!(config.output_format, OutputFormat::StaticLib);
        assert!(config.pic);
    }

    #[test]
    fn test_ios_target_architectures() {
        let device_arm = ios_target(Arch::Aarch64, false);
        assert_eq!(device_arm.triple, "aarch64-apple-ios");
        assert_eq!(device_arm.platform, Platform::IOS);

        let sim_arm = ios_target(Arch::Aarch64, true);
        assert_eq!(sim_arm.triple, "aarch64-apple-ios-simulator");
        assert_eq!(sim_arm.platform, Platform::IOSSimulator);

        let sim_x86 = ios_target(Arch::X86_64, true);
        assert_eq!(sim_x86.triple, "x86_64-apple-ios-simulator");
        assert_eq!(sim_x86.platform, Platform::IOSSimulator);
    }

    #[test]
    fn test_ios_linker_command() {
        let config = ios_device_target();
        let args = config.linker_command(
            std::path::Path::new("app.o"),
            std::path::Path::new("libadam_runtime.a"),
            std::path::Path::new("libadam_app.a"),
        );
        assert!(args.contains(&"-target".to_string()));
        assert!(args.contains(&"aarch64-apple-ios".to_string()));
        assert!(args.contains(&"-framework".to_string()));
        assert!(args.contains(&"UIKit".to_string()));
        assert!(args.contains(&"-arch".to_string()));
        assert!(args.contains(&"arm64".to_string()));
    }

    #[test]
    fn test_ios_min_version() {
        let config = ios_device_target();
        assert_eq!(config.min_os_version.as_deref(), Some(MIN_IOS_VERSION));

        let args = config.linker_command(
            std::path::Path::new("a.o"),
            std::path::Path::new("rt.a"),
            std::path::Path::new("out"),
        );
        let version_flag = args.iter().find(|a| a.starts_with("-mios-version-min="));
        assert!(version_flag.is_some());
        assert!(version_flag.unwrap().contains(MIN_IOS_VERSION));
    }

    #[test]
    fn test_ios_compile_settings_default() {
        let settings = IosCompileSettings::default();
        assert!(!settings.embed_bitcode);
        assert!(!settings.strip_debug);
        assert!(settings.code_sign_identity.is_none());
        assert_eq!(settings.bundle_id, "com.adam.app");
    }

    #[test]
    fn test_find_ios_sdk_on_macos() {
        // This test runs on macOS CI / dev machines.
        // On non-macOS, the SDK won't be found, which is fine.
        if cfg!(target_os = "macos") {
            // Just verify it doesn't panic.
            let _ = find_ios_sdk();
            let _ = find_ios_simulator_sdk();
        }
    }

    #[test]
    fn test_validate_toolchain_does_not_panic() {
        // Should not panic regardless of platform.
        let _ = validate_ios_toolchain();
    }
}
