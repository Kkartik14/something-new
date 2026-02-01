//! Cross-platform API trait and mock implementation.
//!
//! The `PlatformApi` trait defines the complete set of platform operations
//! that Adam code can call. Each target platform implements this trait
//! with its native calls.

use std::collections::HashMap;

// ---------------------------------------------------------------------------
// Platform kind
// ---------------------------------------------------------------------------

/// Identifies the target platform at compile/runtime.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PlatformKind {
    /// iOS device
    IOS,
    /// iOS Simulator
    IOSSimulator,
    /// Android device or emulator
    Android,
    /// Desktop (macOS, Linux) — used for development/testing
    Desktop,
}

impl PlatformKind {
    /// Detect the current platform at compile time.
    pub fn current() -> Self {
        #[cfg(target_os = "ios")]
        return PlatformKind::IOS;

        #[cfg(target_os = "android")]
        return PlatformKind::Android;

        #[cfg(not(any(target_os = "ios", target_os = "android")))]
        return PlatformKind::Desktop;
    }

    /// Determine the platform kind from an LLVM target triple.
    pub fn from_triple(triple: &str) -> Self {
        if triple.contains("apple-ios") {
            if triple.contains("simulator") {
                PlatformKind::IOSSimulator
            } else {
                PlatformKind::IOS
            }
        } else if triple.contains("linux-android") {
            PlatformKind::Android
        } else {
            PlatformKind::Desktop
        }
    }

    /// Whether this platform supports haptic feedback.
    pub fn supports_haptics(&self) -> bool {
        matches!(self, PlatformKind::IOS | PlatformKind::Android)
    }

    /// Whether this platform has a safe area (notch, home indicator).
    pub fn has_safe_area(&self) -> bool {
        matches!(self, PlatformKind::IOS | PlatformKind::IOSSimulator | PlatformKind::Android)
    }

    /// Whether this is a real device (not simulator/emulator/desktop).
    pub fn is_device(&self) -> bool {
        matches!(self, PlatformKind::IOS | PlatformKind::Android)
    }
}

// ---------------------------------------------------------------------------
// Data types
// ---------------------------------------------------------------------------

/// Screen metrics.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ScreenMetrics {
    pub width: f64,
    pub height: f64,
    pub scale: f64,
    pub safe_area: EdgeInsets,
}

impl Default for ScreenMetrics {
    fn default() -> Self {
        Self {
            width: 0.0,
            height: 0.0,
            scale: 1.0,
            safe_area: EdgeInsets::zero(),
        }
    }
}

/// Edge insets (safe area, padding, etc.).
#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(C)]
pub struct EdgeInsets {
    pub top: f64,
    pub bottom: f64,
    pub left: f64,
    pub right: f64,
}

impl EdgeInsets {
    pub fn zero() -> Self {
        Self { top: 0.0, bottom: 0.0, left: 0.0, right: 0.0 }
    }

    pub fn uniform(value: f64) -> Self {
        Self { top: value, bottom: value, left: value, right: value }
    }

    pub fn horizontal(&self) -> f64 {
        self.left + self.right
    }

    pub fn vertical(&self) -> f64 {
        self.top + self.bottom
    }
}

impl Default for EdgeInsets {
    fn default() -> Self {
        Self::zero()
    }
}

/// Haptic feedback intensity.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum HapticStyle {
    Light = 0,
    Medium = 1,
    Heavy = 2,
}

/// Status bar appearance.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum StatusBarStyle {
    Default = 0,
    Light = 1,
    Dark = 2,
}

// ---------------------------------------------------------------------------
// Platform API trait
// ---------------------------------------------------------------------------

/// The unified platform API that Adam code calls.
///
/// Each target platform provides an implementation of this trait.
/// During development and testing, `MockPlatformApi` can be used.
pub trait PlatformApi {
    // -- Screen metrics --
    fn screen_width(&self) -> f64;
    fn screen_height(&self) -> f64;
    fn screen_scale(&self) -> f64;
    fn safe_area(&self) -> EdgeInsets;

    // -- Appearance --
    fn is_dark_mode(&self) -> bool;

    // -- Navigation --
    fn open_url(&self, url: &str) -> bool;
    fn share(&self, text: &str);

    // -- Storage --
    fn store(&mut self, key: &str, value: &str);
    fn load(&self, key: &str) -> Option<String>;

    // -- Haptics --
    fn haptic(&mut self, style: HapticStyle);

    // -- Keyboard --
    fn dismiss_keyboard(&self);
}

// ---------------------------------------------------------------------------
// Mock platform API (for testing)
// ---------------------------------------------------------------------------

/// Mock platform API for unit tests and desktop development.
pub struct MockPlatformApi {
    width: f64,
    height: f64,
    scale: f64,
    safe_area: EdgeInsets,
    dark_mode: bool,
    storage: HashMap<String, String>,
    haptic_events: Vec<HapticStyle>,
    opened_urls: Vec<String>,
    shared_texts: Vec<String>,
}

impl MockPlatformApi {
    pub fn new(width: f64, height: f64, scale: f64) -> Self {
        Self {
            width,
            height,
            scale,
            safe_area: EdgeInsets::zero(),
            dark_mode: false,
            storage: HashMap::new(),
            haptic_events: Vec::new(),
            opened_urls: Vec::new(),
            shared_texts: Vec::new(),
        }
    }

    /// Create a mock simulating an iPhone 14 Pro.
    pub fn iphone_14_pro() -> Self {
        let mut api = Self::new(393.0, 852.0, 3.0);
        api.safe_area = EdgeInsets {
            top: 59.0,
            bottom: 34.0,
            left: 0.0,
            right: 0.0,
        };
        api
    }

    /// Create a mock simulating a Pixel 7.
    pub fn pixel_7() -> Self {
        let mut api = Self::new(411.0, 915.0, 2.75);
        api.safe_area = EdgeInsets {
            top: 24.0,
            bottom: 0.0,
            left: 0.0,
            right: 0.0,
        };
        api
    }

    pub fn set_dark_mode(&mut self, dark: bool) {
        self.dark_mode = dark;
    }

    pub fn set_safe_area(&mut self, insets: EdgeInsets) {
        self.safe_area = insets;
    }

    pub fn haptic_count(&self) -> usize {
        self.haptic_events.len()
    }

    pub fn last_haptic(&self) -> Option<HapticStyle> {
        self.haptic_events.last().copied()
    }

    pub fn opened_urls(&self) -> &[String] {
        &self.opened_urls
    }

    pub fn shared_texts(&self) -> &[String] {
        &self.shared_texts
    }
}

impl PlatformApi for MockPlatformApi {
    fn screen_width(&self) -> f64 { self.width }
    fn screen_height(&self) -> f64 { self.height }
    fn screen_scale(&self) -> f64 { self.scale }
    fn safe_area(&self) -> EdgeInsets { self.safe_area }
    fn is_dark_mode(&self) -> bool { self.dark_mode }

    fn open_url(&self, _url: &str) -> bool {
        true
    }

    fn share(&self, _text: &str) {
    }

    fn store(&mut self, key: &str, value: &str) {
        self.storage.insert(key.to_string(), value.to_string());
    }

    fn load(&self, key: &str) -> Option<String> {
        self.storage.get(key).cloned()
    }

    fn haptic(&mut self, style: HapticStyle) {
        self.haptic_events.push(style);
    }

    fn dismiss_keyboard(&self) {
        // No-op in mock.
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_platform_kind_supports_haptics() {
        assert!(PlatformKind::IOS.supports_haptics());
        assert!(PlatformKind::Android.supports_haptics());
        assert!(!PlatformKind::Desktop.supports_haptics());
        assert!(!PlatformKind::IOSSimulator.supports_haptics());
    }

    #[test]
    fn test_platform_kind_has_safe_area() {
        assert!(PlatformKind::IOS.has_safe_area());
        assert!(PlatformKind::IOSSimulator.has_safe_area());
        assert!(PlatformKind::Android.has_safe_area());
        assert!(!PlatformKind::Desktop.has_safe_area());
    }

    #[test]
    fn test_platform_kind_is_device() {
        assert!(PlatformKind::IOS.is_device());
        assert!(PlatformKind::Android.is_device());
        assert!(!PlatformKind::IOSSimulator.is_device());
        assert!(!PlatformKind::Desktop.is_device());
    }

    #[test]
    fn test_edge_insets_uniform() {
        let i = EdgeInsets::uniform(16.0);
        assert_eq!(i.top, 16.0);
        assert_eq!(i.bottom, 16.0);
        assert_eq!(i.left, 16.0);
        assert_eq!(i.right, 16.0);
        assert_eq!(i.horizontal(), 32.0);
        assert_eq!(i.vertical(), 32.0);
    }

    #[test]
    fn test_screen_metrics_default() {
        let m = ScreenMetrics::default();
        assert_eq!(m.scale, 1.0);
    }

    #[test]
    fn test_mock_iphone_14_pro() {
        let api = MockPlatformApi::iphone_14_pro();
        assert_eq!(api.screen_width(), 393.0);
        assert_eq!(api.screen_height(), 852.0);
        assert_eq!(api.screen_scale(), 3.0);
        assert_eq!(api.safe_area().top, 59.0);
        assert_eq!(api.safe_area().bottom, 34.0);
    }

    #[test]
    fn test_mock_pixel_7() {
        let api = MockPlatformApi::pixel_7();
        assert_eq!(api.screen_width(), 411.0);
        assert_eq!(api.screen_height(), 915.0);
        assert_eq!(api.screen_scale(), 2.75);
    }

    #[test]
    fn test_mock_storage_overwrite() {
        let mut api = MockPlatformApi::new(100.0, 200.0, 1.0);
        api.store("k", "v1");
        assert_eq!(api.load("k"), Some("v1".to_string()));
        api.store("k", "v2");
        assert_eq!(api.load("k"), Some("v2".to_string()));
    }

    #[test]
    fn test_mock_open_url() {
        let api = MockPlatformApi::new(100.0, 200.0, 1.0);
        assert!(api.open_url("https://example.com"));
    }
}
