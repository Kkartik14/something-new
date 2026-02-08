//! FFI function table for platform API dispatch.
//!
//! The compiler uses this table to map Adam `platform.*` calls to the
//! correct C function names for each target platform. This enables
//! the same Adam source code to work on both iOS and Android.

use crate::platform::PlatformKind;

// ---------------------------------------------------------------------------
// FFI function entry
// ---------------------------------------------------------------------------

/// A single FFI function mapping.
#[derive(Debug, Clone)]
pub struct FfiFunction {
    /// Adam-side name (e.g. "platform.screen_width").
    pub adam_name: &'static str,
    /// C function name on iOS (e.g. "adam_ios_screen_width").
    pub ios_c_name: &'static str,
    /// C function name on Android (e.g. "adam_platform_screen_width").
    pub android_c_name: &'static str,
    /// Return type signature (for codegen).
    pub return_type: &'static str,
    /// Parameter types (for codegen).
    pub param_types: &'static [&'static str],
}

// ---------------------------------------------------------------------------
// Platform FFI table
// ---------------------------------------------------------------------------

/// The complete table of platform FFI functions.
pub struct PlatformFfiTable {
    platform: PlatformKind,
    functions: Vec<FfiFunction>,
}

impl PlatformFfiTable {
    /// Create the default FFI table with all platform functions.
    pub fn new(platform: PlatformKind) -> Self {
        Self {
            platform,
            functions: all_platform_functions(),
        }
    }

    /// Create a table for a specific platform.
    pub fn for_platform(platform: PlatformKind) -> Self {
        Self::new(platform)
    }

    /// Get the number of registered functions.
    pub fn function_count(&self) -> usize {
        self.functions.len()
    }

    /// Get all Adam-side function names.
    pub fn function_names(&self) -> Vec<&'static str> {
        self.functions.iter().map(|f| f.adam_name).collect()
    }

    /// Look up the C function name for a given Adam function name.
    pub fn c_function_name(&self, adam_name: &str) -> Option<&'static str> {
        let func = self.functions.iter().find(|f| f.adam_name == adam_name)?;
        match self.platform {
            PlatformKind::IOS | PlatformKind::IOSSimulator => Some(func.ios_c_name),
            PlatformKind::Android => Some(func.android_c_name),
            PlatformKind::Desktop => None, // Desktop uses mock; no real C functions.
        }
    }

    /// Look up a function by its Adam name.
    pub fn get_function(&self, adam_name: &str) -> Option<&FfiFunction> {
        self.functions.iter().find(|f| f.adam_name == adam_name)
    }

    /// Get all functions.
    pub fn functions(&self) -> &[FfiFunction] {
        &self.functions
    }

    /// Get the platform kind.
    pub fn platform(&self) -> PlatformKind {
        self.platform
    }
}

impl Default for PlatformFfiTable {
    fn default() -> Self {
        Self::new(PlatformKind::current())
    }
}

// ---------------------------------------------------------------------------
// Function definitions
// ---------------------------------------------------------------------------

/// All platform API functions with their per-platform C names.
fn all_platform_functions() -> Vec<FfiFunction> {
    vec![
        FfiFunction {
            adam_name: "platform.screen_width",
            ios_c_name: "adam_ios_screen_width",
            android_c_name: "adam_platform_screen_width",
            return_type: "f64",
            param_types: &[],
        },
        FfiFunction {
            adam_name: "platform.screen_height",
            ios_c_name: "adam_ios_screen_height",
            android_c_name: "adam_platform_screen_height",
            return_type: "f64",
            param_types: &[],
        },
        FfiFunction {
            adam_name: "platform.screen_scale",
            ios_c_name: "adam_ios_screen_scale",
            android_c_name: "adam_platform_screen_scale",
            return_type: "f64",
            param_types: &[],
        },
        FfiFunction {
            adam_name: "platform.safe_area",
            ios_c_name: "adam_ios_safe_area",
            android_c_name: "adam_platform_safe_area",
            return_type: "EdgeInsets",
            param_types: &[],
        },
        FfiFunction {
            adam_name: "platform.is_dark_mode",
            ios_c_name: "adam_ios_is_dark_mode",
            android_c_name: "adam_platform_is_dark_mode",
            return_type: "bool",
            param_types: &[],
        },
        FfiFunction {
            adam_name: "platform.open_url",
            ios_c_name: "adam_ios_open_url",
            android_c_name: "adam_platform_open_url_c",
            return_type: "bool",
            param_types: &["String"],
        },
        FfiFunction {
            adam_name: "platform.share",
            ios_c_name: "adam_ios_share",
            android_c_name: "adam_platform_share_c",
            return_type: "void",
            param_types: &["String"],
        },
        FfiFunction {
            adam_name: "platform.store",
            ios_c_name: "adam_ios_store",
            android_c_name: "adam_platform_store_c",
            return_type: "void",
            param_types: &["String", "String"],
        },
        FfiFunction {
            adam_name: "platform.load",
            ios_c_name: "adam_ios_load",
            android_c_name: "adam_platform_load_c",
            return_type: "?String",
            param_types: &["String"],
        },
        FfiFunction {
            adam_name: "platform.haptic_light",
            ios_c_name: "adam_ios_haptic_light",
            android_c_name: "adam_platform_haptic_light",
            return_type: "void",
            param_types: &[],
        },
        FfiFunction {
            adam_name: "platform.haptic_medium",
            ios_c_name: "adam_ios_haptic_medium",
            android_c_name: "adam_platform_haptic_medium",
            return_type: "void",
            param_types: &[],
        },
        FfiFunction {
            adam_name: "platform.haptic_heavy",
            ios_c_name: "adam_ios_haptic_heavy",
            android_c_name: "adam_platform_haptic_heavy",
            return_type: "void",
            param_types: &[],
        },
    ]
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_all_functions_have_unique_names() {
        let funcs = all_platform_functions();
        let names: Vec<_> = funcs.iter().map(|f| f.adam_name).collect();
        let mut unique = names.clone();
        unique.sort();
        unique.dedup();
        assert_eq!(names.len(), unique.len(), "duplicate Adam function names");
    }

    #[test]
    fn test_all_functions_have_unique_ios_c_names() {
        let funcs = all_platform_functions();
        let names: Vec<_> = funcs.iter().map(|f| f.ios_c_name).collect();
        let mut unique = names.clone();
        unique.sort();
        unique.dedup();
        assert_eq!(names.len(), unique.len(), "duplicate iOS C function names");
    }

    #[test]
    fn test_all_functions_have_unique_android_c_names() {
        let funcs = all_platform_functions();
        let names: Vec<_> = funcs.iter().map(|f| f.android_c_name).collect();
        let mut unique = names.clone();
        unique.sort();
        unique.dedup();
        assert_eq!(
            names.len(),
            unique.len(),
            "duplicate Android C function names"
        );
    }

    #[test]
    fn test_get_function_returns_correct_types() {
        let table = PlatformFfiTable::for_platform(PlatformKind::IOS);

        let f = table.get_function("platform.screen_width").unwrap();
        assert_eq!(f.return_type, "f64");
        assert!(f.param_types.is_empty());

        let f = table.get_function("platform.open_url").unwrap();
        assert_eq!(f.return_type, "bool");
        assert_eq!(f.param_types, &["String"]);

        let f = table.get_function("platform.store").unwrap();
        assert_eq!(f.return_type, "void");
        assert_eq!(f.param_types, &["String", "String"]);
    }

    #[test]
    fn test_ios_simulator_uses_ios_c_names() {
        let table = PlatformFfiTable::for_platform(PlatformKind::IOSSimulator);
        let name = table.c_function_name("platform.screen_width");
        assert_eq!(name, Some("adam_ios_screen_width"));
    }

    #[test]
    fn test_nonexistent_function() {
        let table = PlatformFfiTable::for_platform(PlatformKind::IOS);
        assert!(table.get_function("platform.nonexistent").is_none());
        assert!(table.c_function_name("platform.nonexistent").is_none());
    }
}
