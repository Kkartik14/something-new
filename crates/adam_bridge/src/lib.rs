//! Adam Bridge — cross-platform abstraction layer.
//!
//! Provides a unified API surface that works across iOS and Android.
//! At compile time, the correct platform implementation is selected
//! based on the target triple. At runtime, calls dispatch to the
//! platform-specific C FFI functions.

pub mod platform;
pub mod ffi;

pub use platform::{
    PlatformApi, PlatformKind, ScreenMetrics, EdgeInsets, HapticStyle, StatusBarStyle,
};

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_platform_kind_detection() {
        let kind = PlatformKind::current();
        // On macOS (development), should be Desktop.
        #[cfg(target_os = "macos")]
        assert_eq!(kind, PlatformKind::Desktop);
        #[cfg(target_os = "linux")]
        assert_eq!(kind, PlatformKind::Desktop);
    }

    #[test]
    fn test_platform_kind_from_triple() {
        assert_eq!(PlatformKind::from_triple("aarch64-apple-ios"), PlatformKind::IOS);
        assert_eq!(PlatformKind::from_triple("aarch64-apple-ios-simulator"), PlatformKind::IOSSimulator);
        assert_eq!(PlatformKind::from_triple("x86_64-apple-ios-simulator"), PlatformKind::IOSSimulator);
        assert_eq!(PlatformKind::from_triple("aarch64-linux-android24"), PlatformKind::Android);
        assert_eq!(PlatformKind::from_triple("x86_64-linux-android24"), PlatformKind::Android);
        assert_eq!(PlatformKind::from_triple("aarch64-apple-macosx"), PlatformKind::Desktop);
        assert_eq!(PlatformKind::from_triple("x86_64-unknown-linux-gnu"), PlatformKind::Desktop);
    }

    #[test]
    fn test_screen_metrics_default() {
        let m = ScreenMetrics::default();
        assert_eq!(m.width, 0.0);
        assert_eq!(m.height, 0.0);
        assert_eq!(m.scale, 1.0);
    }

    #[test]
    fn test_edge_insets_default() {
        let i = EdgeInsets::default();
        assert_eq!(i.top, 0.0);
        assert_eq!(i.bottom, 0.0);
        assert_eq!(i.left, 0.0);
        assert_eq!(i.right, 0.0);
    }

    #[test]
    fn test_edge_insets_zero() {
        let i = EdgeInsets::zero();
        assert_eq!(i.top, 0.0);
        assert_eq!(i.right, 0.0);
    }

    #[test]
    fn test_haptic_style_values() {
        assert_eq!(HapticStyle::Light as u8, 0);
        assert_eq!(HapticStyle::Medium as u8, 1);
        assert_eq!(HapticStyle::Heavy as u8, 2);
    }

    #[test]
    fn test_status_bar_style_values() {
        assert_eq!(StatusBarStyle::Default as u8, 0);
        assert_eq!(StatusBarStyle::Light as u8, 1);
        assert_eq!(StatusBarStyle::Dark as u8, 2);
    }

    #[test]
    fn test_mock_platform_api() {
        let api = platform::MockPlatformApi::new(375.0, 812.0, 3.0);
        assert_eq!(api.screen_width(), 375.0);
        assert_eq!(api.screen_height(), 812.0);
        assert_eq!(api.screen_scale(), 3.0);
        assert!(!api.is_dark_mode());
    }

    #[test]
    fn test_mock_platform_dark_mode() {
        let mut api = platform::MockPlatformApi::new(375.0, 812.0, 3.0);
        assert!(!api.is_dark_mode());
        api.set_dark_mode(true);
        assert!(api.is_dark_mode());
    }

    #[test]
    fn test_mock_platform_storage() {
        let mut api = platform::MockPlatformApi::new(375.0, 812.0, 3.0);
        assert_eq!(api.load("key"), None);
        api.store("key", "value");
        assert_eq!(api.load("key"), Some("value".to_string()));
    }

    #[test]
    fn test_mock_platform_safe_area() {
        let mut api = platform::MockPlatformApi::new(375.0, 812.0, 3.0);
        let insets = api.safe_area();
        assert_eq!(insets.top, 0.0);

        api.set_safe_area(EdgeInsets {
            top: 47.0,
            bottom: 34.0,
            left: 0.0,
            right: 0.0,
        });
        let insets = api.safe_area();
        assert_eq!(insets.top, 47.0);
        assert_eq!(insets.bottom, 34.0);
    }

    #[test]
    fn test_mock_platform_haptic_count() {
        let mut api = platform::MockPlatformApi::new(375.0, 812.0, 3.0);
        assert_eq!(api.haptic_count(), 0);
        api.haptic(HapticStyle::Light);
        api.haptic(HapticStyle::Medium);
        api.haptic(HapticStyle::Heavy);
        assert_eq!(api.haptic_count(), 3);
    }

    #[test]
    fn test_ffi_function_declarations() {
        // Verify the FFI function table has all expected entries.
        let table = ffi::PlatformFfiTable::default();
        assert_eq!(table.function_count(), 12);
    }

    #[test]
    fn test_ffi_function_names() {
        let table = ffi::PlatformFfiTable::default();
        let names = table.function_names();
        assert!(names.contains(&"platform.screen_width"));
        assert!(names.contains(&"platform.screen_height"));
        assert!(names.contains(&"platform.screen_scale"));
        assert!(names.contains(&"platform.safe_area"));
        assert!(names.contains(&"platform.is_dark_mode"));
        assert!(names.contains(&"platform.open_url"));
        assert!(names.contains(&"platform.share"));
        assert!(names.contains(&"platform.store"));
        assert!(names.contains(&"platform.load"));
        assert!(names.contains(&"platform.haptic_light"));
        assert!(names.contains(&"platform.haptic_medium"));
        assert!(names.contains(&"platform.haptic_heavy"));
    }

    #[test]
    fn test_ffi_table_c_names() {
        let table = ffi::PlatformFfiTable::for_platform(PlatformKind::IOS);
        let c_name = table.c_function_name("platform.screen_width");
        assert_eq!(c_name, Some("adam_ios_screen_width"));

        let table = ffi::PlatformFfiTable::for_platform(PlatformKind::Android);
        let c_name = table.c_function_name("platform.screen_width");
        assert_eq!(c_name, Some("adam_platform_screen_width"));
    }

    #[test]
    fn test_ffi_table_desktop_has_no_c_names() {
        let table = ffi::PlatformFfiTable::for_platform(PlatformKind::Desktop);
        let c_name = table.c_function_name("platform.screen_width");
        assert_eq!(c_name, None);
    }
}
