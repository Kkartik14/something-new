//! Cross-compilation target definitions for Adam.
//!
//! Each target specifies the LLVM triple, linker flags, output format,
//! and platform-specific configuration needed to compile Adam code for
//! a given platform.

pub mod android;
pub mod ios;

use std::path::{Path, PathBuf};

// ---------------------------------------------------------------------------
// Target architecture
// ---------------------------------------------------------------------------

/// CPU architecture for cross-compilation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Arch {
    Aarch64,
    X86_64,
}

impl Arch {
    pub fn as_str(&self) -> &'static str {
        match self {
            Arch::Aarch64 => "aarch64",
            Arch::X86_64 => "x86_64",
        }
    }
}

impl std::fmt::Display for Arch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

// ---------------------------------------------------------------------------
// Output format
// ---------------------------------------------------------------------------

/// The output format for the compilation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputFormat {
    /// Static library (.a)
    StaticLib,
    /// Shared library (.so / .dylib)
    SharedLib,
    /// Object file (.o)
    ObjectFile,
    /// Framework bundle (iOS)
    Framework,
}

impl OutputFormat {
    pub fn extension(&self) -> &'static str {
        match self {
            OutputFormat::StaticLib => "a",
            OutputFormat::SharedLib => "so",
            OutputFormat::ObjectFile => "o",
            OutputFormat::Framework => "framework",
        }
    }
}

// ---------------------------------------------------------------------------
// Platform
// ---------------------------------------------------------------------------

/// Target platform.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Platform {
    /// macOS desktop (native)
    MacOS,
    /// iOS device
    IOS,
    /// iOS Simulator
    IOSSimulator,
    /// Android device
    Android,
    /// Android Emulator
    AndroidEmulator,
    /// Linux desktop
    Linux,
}

impl Platform {
    /// Whether this is a mobile platform.
    pub fn is_mobile(&self) -> bool {
        matches!(
            self,
            Platform::IOS | Platform::IOSSimulator | Platform::Android | Platform::AndroidEmulator
        )
    }

    /// Whether this is an Apple platform.
    pub fn is_apple(&self) -> bool {
        matches!(
            self,
            Platform::MacOS | Platform::IOS | Platform::IOSSimulator
        )
    }

    /// Whether this is an Android platform.
    pub fn is_android(&self) -> bool {
        matches!(self, Platform::Android | Platform::AndroidEmulator)
    }

    /// The short name used in CLI flags (e.g. `--target=ios`).
    pub fn cli_name(&self) -> &'static str {
        match self {
            Platform::MacOS => "macos",
            Platform::IOS => "ios",
            Platform::IOSSimulator => "ios-simulator",
            Platform::Android => "android",
            Platform::AndroidEmulator => "android-emulator",
            Platform::Linux => "linux",
        }
    }

    /// Parse a CLI target name.
    pub fn from_cli_name(name: &str) -> Option<Self> {
        match name {
            "macos" => Some(Platform::MacOS),
            "ios" => Some(Platform::IOS),
            "ios-simulator" | "ios-sim" => Some(Platform::IOSSimulator),
            "android" => Some(Platform::Android),
            "android-emulator" | "android-emu" => Some(Platform::AndroidEmulator),
            "linux" => Some(Platform::Linux),
            _ => None,
        }
    }
}

impl std::fmt::Display for Platform {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.cli_name())
    }
}

// ---------------------------------------------------------------------------
// Target configuration
// ---------------------------------------------------------------------------

/// Complete target configuration for cross-compilation.
#[derive(Debug, Clone)]
pub struct TargetConfig {
    /// Target platform.
    pub platform: Platform,
    /// CPU architecture.
    pub arch: Arch,
    /// LLVM target triple (e.g. "aarch64-apple-ios").
    pub triple: String,
    /// Output format.
    pub output_format: OutputFormat,
    /// Minimum OS version (for Apple platforms).
    pub min_os_version: Option<String>,
    /// SDK path (for cross-compilation).
    pub sdk_path: Option<PathBuf>,
    /// Linker to use (defaults to "clang").
    pub linker: String,
    /// Additional linker flags.
    pub linker_flags: Vec<String>,
    /// System libraries to link.
    pub system_libs: Vec<String>,
    /// Frameworks to link (Apple platforms).
    pub frameworks: Vec<String>,
    /// Whether to produce position-independent code.
    pub pic: bool,
    /// Optimization level: 0=none, 1=basic, 2=default, 3=aggressive.
    pub opt_level: u32,
}

impl TargetConfig {
    /// Get the LLVM target triple.
    pub fn llvm_triple(&self) -> &str {
        &self.triple
    }

    /// Build the complete linker command for linking an object file.
    pub fn linker_command(
        &self,
        object_path: &Path,
        runtime_lib_path: &Path,
        output_path: &Path,
    ) -> Vec<String> {
        let mut args = Vec::new();

        // Object file.
        args.push(object_path.to_string_lossy().into_owned());

        // Runtime library.
        args.push(runtime_lib_path.to_string_lossy().into_owned());

        // Output path.
        args.push("-o".to_string());
        args.push(output_path.to_string_lossy().into_owned());

        // SDK path (sysroot).
        if let Some(ref sdk) = self.sdk_path {
            args.push("-isysroot".to_string());
            args.push(sdk.to_string_lossy().into_owned());
        }

        // Target triple.
        args.push("-target".to_string());
        args.push(self.triple.clone());

        // Min OS version for Apple platforms.
        if let Some(ref version) = self.min_os_version {
            if self.platform.is_apple() {
                let flag = match self.platform {
                    Platform::IOS => format!("-mios-version-min={}", version),
                    Platform::IOSSimulator => format!("-mios-simulator-version-min={}", version),
                    Platform::MacOS => format!("-mmacosx-version-min={}", version),
                    _ => String::new(),
                };
                if !flag.is_empty() {
                    args.push(flag);
                }
            }
        }

        // PIC flag.
        if self.pic {
            args.push("-fPIC".to_string());
        }

        // Output format flags.
        match self.output_format {
            OutputFormat::StaticLib => {
                // Static lib is created with `ar`, not the linker.
                // This path shouldn't be used for static libs.
            }
            OutputFormat::SharedLib => {
                args.push("-shared".to_string());
            }
            OutputFormat::ObjectFile => {
                args.push("-c".to_string());
            }
            OutputFormat::Framework => {
                args.push("-dynamiclib".to_string());
            }
        }

        // Frameworks (Apple).
        for framework in &self.frameworks {
            args.push("-framework".to_string());
            args.push(framework.clone());
        }

        // System libraries.
        for lib in &self.system_libs {
            args.push(format!("-l{}", lib));
        }

        // Additional linker flags.
        args.extend(self.linker_flags.clone());

        args
    }

    /// Get the output file name for a given module name.
    pub fn output_filename(&self, module_name: &str) -> String {
        match self.output_format {
            OutputFormat::StaticLib => format!("lib{}.a", module_name),
            OutputFormat::SharedLib => {
                if self.platform.is_apple() {
                    format!("lib{}.dylib", module_name)
                } else {
                    format!("lib{}.so", module_name)
                }
            }
            OutputFormat::ObjectFile => format!("{}.o", module_name),
            OutputFormat::Framework => format!("{}.framework", module_name),
        }
    }

    /// Detect the host platform and return its target config.
    pub fn host() -> Self {
        #[cfg(target_os = "macos")]
        {
            Self {
                platform: Platform::MacOS,
                #[cfg(target_arch = "aarch64")]
                arch: Arch::Aarch64,
                #[cfg(target_arch = "x86_64")]
                arch: Arch::X86_64,
                #[cfg(target_arch = "aarch64")]
                triple: "aarch64-apple-macosx".to_string(),
                #[cfg(target_arch = "x86_64")]
                triple: "x86_64-apple-macosx".to_string(),
                output_format: OutputFormat::ObjectFile,
                min_os_version: Some("13.0".to_string()),
                sdk_path: None,
                linker: "clang".to_string(),
                linker_flags: Vec::new(),
                system_libs: vec!["System".to_string(), "resolv".to_string()],
                frameworks: Vec::new(),
                pic: false,
                opt_level: 2,
            }
        }
        #[cfg(target_os = "linux")]
        {
            Self {
                platform: Platform::Linux,
                #[cfg(target_arch = "aarch64")]
                arch: Arch::Aarch64,
                #[cfg(target_arch = "x86_64")]
                arch: Arch::X86_64,
                #[cfg(target_arch = "aarch64")]
                triple: "aarch64-unknown-linux-gnu".to_string(),
                #[cfg(target_arch = "x86_64")]
                triple: "x86_64-unknown-linux-gnu".to_string(),
                output_format: OutputFormat::ObjectFile,
                min_os_version: None,
                sdk_path: None,
                linker: "clang".to_string(),
                linker_flags: Vec::new(),
                system_libs: vec![
                    "c".to_string(),
                    "pthread".to_string(),
                    "dl".to_string(),
                    "m".to_string(),
                ],
                frameworks: Vec::new(),
                pic: false,
                opt_level: 2,
            }
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
    fn test_platform_cli_names() {
        assert_eq!(Platform::from_cli_name("ios"), Some(Platform::IOS));
        assert_eq!(
            Platform::from_cli_name("ios-simulator"),
            Some(Platform::IOSSimulator)
        );
        assert_eq!(
            Platform::from_cli_name("ios-sim"),
            Some(Platform::IOSSimulator)
        );
        assert_eq!(Platform::from_cli_name("android"), Some(Platform::Android));
        assert_eq!(
            Platform::from_cli_name("android-emulator"),
            Some(Platform::AndroidEmulator)
        );
        assert_eq!(
            Platform::from_cli_name("android-emu"),
            Some(Platform::AndroidEmulator)
        );
        assert_eq!(Platform::from_cli_name("macos"), Some(Platform::MacOS));
        assert_eq!(Platform::from_cli_name("linux"), Some(Platform::Linux));
        assert_eq!(Platform::from_cli_name("windows"), None);
    }

    #[test]
    fn test_platform_properties() {
        assert!(Platform::IOS.is_mobile());
        assert!(Platform::IOSSimulator.is_mobile());
        assert!(Platform::Android.is_mobile());
        assert!(!Platform::MacOS.is_mobile());
        assert!(!Platform::Linux.is_mobile());

        assert!(Platform::IOS.is_apple());
        assert!(Platform::IOSSimulator.is_apple());
        assert!(Platform::MacOS.is_apple());
        assert!(!Platform::Android.is_apple());

        assert!(Platform::Android.is_android());
        assert!(Platform::AndroidEmulator.is_android());
        assert!(!Platform::IOS.is_android());
    }

    #[test]
    fn test_arch_display() {
        assert_eq!(Arch::Aarch64.as_str(), "aarch64");
        assert_eq!(Arch::X86_64.as_str(), "x86_64");
    }

    #[test]
    fn test_output_format_extension() {
        assert_eq!(OutputFormat::StaticLib.extension(), "a");
        assert_eq!(OutputFormat::SharedLib.extension(), "so");
        assert_eq!(OutputFormat::ObjectFile.extension(), "o");
        assert_eq!(OutputFormat::Framework.extension(), "framework");
    }

    #[test]
    fn test_output_filename() {
        let config = ios::ios_device_target();
        assert_eq!(config.output_filename("adam_app"), "libadam_app.a");

        let mut shared_config = config.clone();
        shared_config.output_format = OutputFormat::SharedLib;
        assert_eq!(
            shared_config.output_filename("adam_app"),
            "libadam_app.dylib"
        );

        let android_config = android::android_device_target();
        let mut android_shared = android_config.clone();
        android_shared.output_format = OutputFormat::SharedLib;
        assert_eq!(android_shared.output_filename("adam_app"), "libadam_app.so");
    }

    #[test]
    fn test_host_config() {
        let host = TargetConfig::host();
        // Should match the current platform.
        #[cfg(target_os = "macos")]
        assert_eq!(host.platform, Platform::MacOS);
        #[cfg(target_os = "linux")]
        assert_eq!(host.platform, Platform::Linux);
    }

    #[test]
    fn test_linker_command_basic() {
        let config = TargetConfig::host();
        let args = config.linker_command(
            Path::new("test.o"),
            Path::new("libadam_runtime.a"),
            Path::new("test"),
        );
        assert!(args.contains(&"test.o".to_string()));
        assert!(args.contains(&"libadam_runtime.a".to_string()));
        assert!(args.contains(&"-o".to_string()));
        assert!(args.contains(&"-target".to_string()));
    }
}
