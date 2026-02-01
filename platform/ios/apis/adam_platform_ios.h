// adam_platform_ios.h — C declarations for iOS platform APIs.
//
// These are the Obj-C-implemented functions that the Adam runtime
// calls through C FFI for iOS-specific functionality.

#ifndef ADAM_PLATFORM_IOS_H
#define ADAM_PLATFORM_IOS_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// Screen metrics
double adam_ios_screen_width(void);
double adam_ios_screen_height(void);
double adam_ios_screen_scale(void);

typedef struct {
    double top;
    double bottom;
    double left;
    double right;
} AdamEdgeInsets;

AdamEdgeInsets adam_ios_safe_area(void);

// Dark mode
int adam_ios_is_dark_mode(void);

// Navigation
int adam_ios_open_url(const char *url, uint32_t url_len);
void adam_ios_share(const char *text, uint32_t text_len);

// Storage
void adam_ios_store(const char *key, uint32_t key_len,
                   const char *value, uint32_t value_len);
uint32_t adam_ios_load(const char *key, uint32_t key_len,
                       char *out_buf, uint32_t out_buf_cap);

// Haptics
void adam_ios_haptic_light(void);
void adam_ios_haptic_medium(void);
void adam_ios_haptic_heavy(void);

// Status bar
void adam_ios_set_status_bar_style(int style);

// Keyboard
void adam_ios_dismiss_keyboard(void);

#ifdef __cplusplus
}
#endif

#endif // ADAM_PLATFORM_IOS_H
