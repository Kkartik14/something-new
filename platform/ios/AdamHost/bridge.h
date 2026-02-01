// bridge.h — C interface between iOS host app and Adam runtime.
//
// This header declares the functions that the Swift host app calls to
// communicate with the compiled Adam binary and its runtime.

#ifndef ADAM_BRIDGE_H
#define ADAM_BRIDGE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

// ---------------------------------------------------------------------------
// App lifecycle
// ---------------------------------------------------------------------------

/// Initialize the Adam runtime. Call once from application:didFinishLaunching.
/// Returns 0 on success, non-zero on failure.
int adam_runtime_init(void);

/// Shut down the Adam runtime. Call from applicationWillTerminate.
void adam_runtime_shutdown(void);

/// Notify the runtime that the app is entering the background.
void adam_app_did_enter_background(void);

/// Notify the runtime that the app is entering the foreground.
void adam_app_will_enter_foreground(void);

/// Notify the runtime of a memory warning.
void adam_app_did_receive_memory_warning(void);

// ---------------------------------------------------------------------------
// Render loop
// ---------------------------------------------------------------------------

/// Called each frame (driven by CADisplayLink / vsync).
/// `width` and `height` are the drawable size in pixels.
/// `timestamp` is the frame time in seconds since boot.
void adam_render_frame(uint32_t width, uint32_t height, double timestamp);

/// Set the screen scale factor (e.g. 2.0 for Retina, 3.0 for Plus models).
void adam_set_screen_scale(float scale);

// ---------------------------------------------------------------------------
// Touch / pointer input
// ---------------------------------------------------------------------------

/// Touch phase constants matching UITouch.Phase.
typedef enum {
    ADAM_TOUCH_BEGAN   = 0,
    ADAM_TOUCH_MOVED   = 1,
    ADAM_TOUCH_ENDED   = 2,
    ADAM_TOUCH_CANCELLED = 3,
} AdamTouchPhase;

/// Forward a touch event to the Adam runtime.
/// Coordinates are in points (not pixels).
void adam_touch_event(
    int64_t touch_id,
    float x,
    float y,
    AdamTouchPhase phase,
    double timestamp
);

// ---------------------------------------------------------------------------
// Keyboard input
// ---------------------------------------------------------------------------

/// Forward a text input event (from UITextField / UITextView).
void adam_text_input(const char *text, uint32_t length);

/// Forward a key event.
void adam_key_event(uint32_t key_code, bool is_down, uint32_t modifiers);

// ---------------------------------------------------------------------------
// Skia surface management
// ---------------------------------------------------------------------------

/// Opaque handle to a Skia GPU surface.
typedef void *AdamSkiaSurface;

/// Create a Skia surface backed by the given Metal texture.
/// `metal_texture` is a id<MTLTexture> cast to void*.
/// `width` and `height` are the texture dimensions in pixels.
AdamSkiaSurface adam_skia_surface_create(
    void *metal_device,
    void *metal_queue,
    void *metal_texture,
    uint32_t width,
    uint32_t height
);

/// Destroy a Skia surface.
void adam_skia_surface_destroy(AdamSkiaSurface surface);

/// Flush pending Skia draw commands to the Metal command buffer.
void adam_skia_surface_flush(AdamSkiaSurface surface);

// ---------------------------------------------------------------------------
// Platform API callbacks (iOS -> Adam)
// ---------------------------------------------------------------------------

/// Screen metrics that the host reports to the runtime.
typedef struct {
    float width;           // points
    float height;          // points
    float scale;           // pixel scale
    float safe_area_top;
    float safe_area_bottom;
    float safe_area_left;
    float safe_area_right;
} AdamScreenMetrics;

/// Update screen metrics (call on launch and on rotation).
void adam_update_screen_metrics(AdamScreenMetrics metrics);

/// Notify the runtime of a dark mode change.
void adam_set_dark_mode(bool is_dark);

// ---------------------------------------------------------------------------
// Platform API functions (Adam -> iOS, implemented in Swift/ObjC)
// ---------------------------------------------------------------------------

/// Open a URL. Returns true on success.
bool adam_platform_open_url(const char *url, uint32_t url_len);

/// Share text via the system share sheet.
void adam_platform_share(const char *text, uint32_t text_len);

/// Store a key-value pair (UserDefaults).
void adam_platform_store(
    const char *key, uint32_t key_len,
    const char *value, uint32_t value_len
);

/// Load a value by key (UserDefaults).
/// Returns the value length, or 0 if not found.
/// `out_buf` must be at least `out_buf_cap` bytes.
uint32_t adam_platform_load(
    const char *key, uint32_t key_len,
    char *out_buf, uint32_t out_buf_cap
);

/// Trigger haptic feedback.
typedef enum {
    ADAM_HAPTIC_LIGHT  = 0,
    ADAM_HAPTIC_MEDIUM = 1,
    ADAM_HAPTIC_HEAVY  = 2,
} AdamHapticStyle;

void adam_platform_haptic(AdamHapticStyle style);

/// Set the status bar style.
typedef enum {
    ADAM_STATUS_BAR_DEFAULT = 0,
    ADAM_STATUS_BAR_LIGHT   = 1,
    ADAM_STATUS_BAR_DARK    = 2,
} AdamStatusBarStyle;

void adam_platform_set_status_bar_style(AdamStatusBarStyle style);

/// Dismiss the keyboard.
void adam_platform_dismiss_keyboard(void);

#ifdef __cplusplus
}
#endif

#endif // ADAM_BRIDGE_H
