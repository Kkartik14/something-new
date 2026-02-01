// apis.c — Android platform API implementations via JNI.
//
// These functions provide the platform API surface that the Adam runtime
// calls. On Android, they dispatch to Java via JNI (through AdamJNI/
// AdamPlatformAPIs). On iOS, the equivalent functions call Obj-C directly.

#include <jni.h>
#include <string.h>

// Forward declarations of the JNI callback functions from adam_jni.c.
extern int adam_android_open_url(const char *url, unsigned int url_len);
extern void adam_android_share(const char *text, unsigned int text_len);
extern void adam_android_store(const char *key, unsigned int key_len,
                               const char *value, unsigned int value_len);
extern unsigned int adam_android_load(const char *key, unsigned int key_len,
                                      char *out_buf, unsigned int out_buf_cap);
extern void adam_android_haptic(int style);
extern void adam_android_dismiss_keyboard(void);

// ---------------------------------------------------------------------------
// Screen metrics (provided by the host activity)
// ---------------------------------------------------------------------------

static float g_screen_width = 0;
static float g_screen_height = 0;
static float g_screen_scale = 1.0f;
static float g_safe_area_top = 0;
static float g_safe_area_bottom = 0;
static float g_safe_area_left = 0;
static float g_safe_area_right = 0;
static int   g_is_dark_mode = 0;

// Called from adam_jni.c when metrics are updated.
void adam_android_update_metrics(float w, float h, float scale,
                                 float st, float sb, float sl, float sr) {
    g_screen_width = w;
    g_screen_height = h;
    g_screen_scale = scale;
    g_safe_area_top = st;
    g_safe_area_bottom = sb;
    g_safe_area_left = sl;
    g_safe_area_right = sr;
}

void adam_android_set_dark_mode(int is_dark) {
    g_is_dark_mode = is_dark;
}

// ---------------------------------------------------------------------------
// Unified platform API functions (called by Adam runtime)
//
// These are the C functions that the compiler generates calls to when
// Adam code uses `platform.screen_width()`, etc.
// ---------------------------------------------------------------------------

double adam_platform_screen_width(void) {
    return (double)g_screen_width;
}

double adam_platform_screen_height(void) {
    return (double)g_screen_height;
}

double adam_platform_screen_scale(void) {
    return (double)g_screen_scale;
}

typedef struct {
    double top;
    double bottom;
    double left;
    double right;
} AdamEdgeInsetsC;

AdamEdgeInsetsC adam_platform_safe_area(void) {
    AdamEdgeInsetsC insets;
    insets.top = g_safe_area_top;
    insets.bottom = g_safe_area_bottom;
    insets.left = g_safe_area_left;
    insets.right = g_safe_area_right;
    return insets;
}

int adam_platform_is_dark_mode(void) {
    return g_is_dark_mode;
}

int adam_platform_open_url_c(const char *url, unsigned int url_len) {
    return adam_android_open_url(url, url_len);
}

void adam_platform_share_c(const char *text, unsigned int text_len) {
    adam_android_share(text, text_len);
}

void adam_platform_store_c(const char *key, unsigned int key_len,
                           const char *value, unsigned int value_len) {
    adam_android_store(key, key_len, value, value_len);
}

unsigned int adam_platform_load_c(const char *key, unsigned int key_len,
                                  char *out_buf, unsigned int out_buf_cap) {
    return adam_android_load(key, key_len, out_buf, out_buf_cap);
}

void adam_platform_haptic_light(void)  { adam_android_haptic(0); }
void adam_platform_haptic_medium(void) { adam_android_haptic(1); }
void adam_platform_haptic_heavy(void)  { adam_android_haptic(2); }

void adam_platform_dismiss_keyboard_c(void) {
    adam_android_dismiss_keyboard();
}
