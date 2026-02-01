// adam_platform_ios.m — Obj-C implementations of Adam platform APIs for iOS.
//
// These functions are called from the Adam runtime (via C FFI) and dispatch
// to the appropriate iOS SDK calls.

#import <UIKit/UIKit.h>
#import <Foundation/Foundation.h>

// ---------------------------------------------------------------------------
// Screen metrics
// ---------------------------------------------------------------------------

double adam_ios_screen_width(void) {
    return (double)UIScreen.mainScreen.bounds.size.width;
}

double adam_ios_screen_height(void) {
    return (double)UIScreen.mainScreen.bounds.size.height;
}

double adam_ios_screen_scale(void) {
    return (double)UIScreen.mainScreen.scale;
}

typedef struct {
    double top;
    double bottom;
    double left;
    double right;
} AdamEdgeInsets;

AdamEdgeInsets adam_ios_safe_area(void) {
    AdamEdgeInsets insets = {0, 0, 0, 0};
    if (@available(iOS 11.0, *)) {
        UIWindow *window = UIApplication.sharedApplication.windows.firstObject;
        if (window) {
            UIEdgeInsets safe = window.safeAreaInsets;
            insets.top = safe.top;
            insets.bottom = safe.bottom;
            insets.left = safe.left;
            insets.right = safe.right;
        }
    }
    return insets;
}

// ---------------------------------------------------------------------------
// Dark mode
// ---------------------------------------------------------------------------

int adam_ios_is_dark_mode(void) {
    if (@available(iOS 13.0, *)) {
        UITraitCollection *tc = UITraitCollection.currentTraitCollection;
        return tc.userInterfaceStyle == UIUserInterfaceStyleDark ? 1 : 0;
    }
    return 0;
}

// ---------------------------------------------------------------------------
// Navigation
// ---------------------------------------------------------------------------

int adam_ios_open_url(const char *url, uint32_t url_len) {
    NSString *urlStr = [[NSString alloc] initWithBytes:url
                                                length:url_len
                                              encoding:NSUTF8StringEncoding];
    if (!urlStr) return 0;

    NSURL *nsUrl = [NSURL URLWithString:urlStr];
    if (!nsUrl) return 0;

    dispatch_async(dispatch_get_main_queue(), ^{
        [UIApplication.sharedApplication openURL:nsUrl
                                         options:@{}
                               completionHandler:nil];
    });
    return 1;
}

void adam_ios_share(const char *text, uint32_t text_len) {
    NSString *str = [[NSString alloc] initWithBytes:text
                                             length:text_len
                                           encoding:NSUTF8StringEncoding];
    if (!str) return;

    dispatch_async(dispatch_get_main_queue(), ^{
        UIActivityViewController *vc =
            [[UIActivityViewController alloc] initWithActivityItems:@[str]
                                             applicationActivities:nil];
        UIWindow *window = UIApplication.sharedApplication.windows.firstObject;
        [window.rootViewController presentViewController:vc animated:YES completion:nil];
    });
}

// ---------------------------------------------------------------------------
// Storage (UserDefaults)
// ---------------------------------------------------------------------------

void adam_ios_store(const char *key, uint32_t key_len,
                   const char *value, uint32_t value_len) {
    NSString *keyStr = [[NSString alloc] initWithBytes:key
                                                length:key_len
                                              encoding:NSUTF8StringEncoding];
    NSString *valStr = [[NSString alloc] initWithBytes:value
                                                length:value_len
                                              encoding:NSUTF8StringEncoding];
    if (!keyStr || !valStr) return;

    NSString *prefixedKey = [NSString stringWithFormat:@"adam.%@", keyStr];
    [NSUserDefaults.standardUserDefaults setObject:valStr forKey:prefixedKey];
}

uint32_t adam_ios_load(const char *key, uint32_t key_len,
                       char *out_buf, uint32_t out_buf_cap) {
    NSString *keyStr = [[NSString alloc] initWithBytes:key
                                                length:key_len
                                              encoding:NSUTF8StringEncoding];
    if (!keyStr) return 0;

    NSString *prefixedKey = [NSString stringWithFormat:@"adam.%@", keyStr];
    NSString *value = [NSUserDefaults.standardUserDefaults stringForKey:prefixedKey];
    if (!value) return 0;

    NSData *utf8 = [value dataUsingEncoding:NSUTF8StringEncoding];
    uint32_t len = (uint32_t)utf8.length;
    uint32_t copy_len = len < out_buf_cap ? len : out_buf_cap;
    memcpy(out_buf, utf8.bytes, copy_len);
    return copy_len;
}

// ---------------------------------------------------------------------------
// Haptics
// ---------------------------------------------------------------------------

void adam_ios_haptic_light(void) {
    dispatch_async(dispatch_get_main_queue(), ^{
        UIImpactFeedbackGenerator *gen =
            [[UIImpactFeedbackGenerator alloc] initWithStyle:UIImpactFeedbackStyleLight];
        [gen prepare];
        [gen impactOccurred];
    });
}

void adam_ios_haptic_medium(void) {
    dispatch_async(dispatch_get_main_queue(), ^{
        UIImpactFeedbackGenerator *gen =
            [[UIImpactFeedbackGenerator alloc] initWithStyle:UIImpactFeedbackStyleMedium];
        [gen prepare];
        [gen impactOccurred];
    });
}

void adam_ios_haptic_heavy(void) {
    dispatch_async(dispatch_get_main_queue(), ^{
        UIImpactFeedbackGenerator *gen =
            [[UIImpactFeedbackGenerator alloc] initWithStyle:UIImpactFeedbackStyleHeavy];
        [gen prepare];
        [gen impactOccurred];
    });
}

// ---------------------------------------------------------------------------
// Status bar
// ---------------------------------------------------------------------------

void adam_ios_set_status_bar_style(int style) {
    // 0 = default, 1 = light, 2 = dark
    // The actual update is driven by the ViewController in Swift.
    // This stores the preference for the VC to read.
}

// ---------------------------------------------------------------------------
// Keyboard
// ---------------------------------------------------------------------------

void adam_ios_dismiss_keyboard(void) {
    dispatch_async(dispatch_get_main_queue(), ^{
        [UIApplication.sharedApplication sendAction:@selector(resignFirstResponder)
                                                 to:nil
                                               from:nil
                                           forEvent:nil];
    });
}
