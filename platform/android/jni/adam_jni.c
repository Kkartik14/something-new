// adam_jni.c — JNI bridge between Android host app and Adam runtime.
//
// Implements the native methods declared in AdamJNI.java.
// Each function follows the JNI naming convention:
//   Java_com_adam_runtime_AdamJNI_<methodName>

#include <jni.h>
#include <android/log.h>
#include <android/native_window.h>
#include <android/native_window_jni.h>

#define LOG_TAG "AdamJNI"
#define LOGI(...) __android_log_print(ANDROID_LOG_INFO,  LOG_TAG, __VA_ARGS__)
#define LOGE(...) __android_log_print(ANDROID_LOG_ERROR, LOG_TAG, __VA_ARGS__)

// ---------------------------------------------------------------------------
// Forward declarations of Adam runtime C functions.
// These are defined in the compiled Adam runtime static library.
// ---------------------------------------------------------------------------

extern int  adam_runtime_init(void);
extern void adam_runtime_shutdown(void);
extern void adam_app_did_enter_background(void);
extern void adam_app_will_enter_foreground(void);
extern void adam_app_did_receive_memory_warning(void);
extern void adam_render_frame(unsigned int width, unsigned int height, double timestamp);
extern void adam_set_screen_scale(float scale);
extern void adam_touch_event(long long touch_id, float x, float y, int phase, double timestamp);
extern void adam_text_input(const char *text, unsigned int length);
extern void adam_key_event(unsigned int key_code, int is_down, unsigned int modifiers);
extern void adam_update_screen_metrics(float width, float height, float scale,
                                        float safe_top, float safe_bottom,
                                        float safe_left, float safe_right);
extern void adam_set_dark_mode(int is_dark);

// Skia surface management (Android uses ANativeWindow).
extern void *adam_skia_surface_create_gl(void *native_window,
                                          unsigned int width, unsigned int height);
extern void  adam_skia_surface_destroy(void *surface);
extern void  adam_skia_surface_flush(void *surface);

// ---------------------------------------------------------------------------
// Global state
// ---------------------------------------------------------------------------

static JavaVM   *g_jvm = NULL;
static jobject   g_activity = NULL;
static ANativeWindow *g_native_window = NULL;
static void     *g_skia_surface = NULL;

// Cache JNI class and method IDs for callbacks from native to Java.
static jclass    g_adam_jni_class = NULL;
static jmethodID g_open_url_method = NULL;
static jmethodID g_share_method = NULL;
static jmethodID g_store_method = NULL;
static jmethodID g_load_method = NULL;
static jmethodID g_haptic_method = NULL;
static jmethodID g_dismiss_keyboard_method = NULL;

// ---------------------------------------------------------------------------
// JNI_OnLoad — called when the native library is loaded.
// ---------------------------------------------------------------------------

JNIEXPORT jint JNI_OnLoad(JavaVM *vm, void *reserved) {
    g_jvm = vm;
    LOGI("Adam JNI library loaded");
    return JNI_VERSION_1_6;
}

// Helper to get JNIEnv for the current thread.
static JNIEnv *get_env(void) {
    JNIEnv *env = NULL;
    if (g_jvm) {
        (*g_jvm)->GetEnv(g_jvm, (void **)&env, JNI_VERSION_1_6);
    }
    return env;
}

// Cache method IDs for platform API callbacks.
static void cache_method_ids(JNIEnv *env) {
    jclass cls = (*env)->FindClass(env, "com/adam/runtime/AdamJNI");
    if (!cls) {
        LOGE("Failed to find AdamJNI class");
        return;
    }
    g_adam_jni_class = (jclass)(*env)->NewGlobalRef(env, cls);

    g_open_url_method = (*env)->GetStaticMethodID(
            env, g_adam_jni_class, "openUrl", "(Ljava/lang/String;)Z");
    g_share_method = (*env)->GetStaticMethodID(
            env, g_adam_jni_class, "share", "(Ljava/lang/String;)V");
    g_store_method = (*env)->GetStaticMethodID(
            env, g_adam_jni_class, "store",
            "(Ljava/lang/String;Ljava/lang/String;)V");
    g_load_method = (*env)->GetStaticMethodID(
            env, g_adam_jni_class, "load",
            "(Ljava/lang/String;)Ljava/lang/String;");
    g_haptic_method = (*env)->GetStaticMethodID(
            env, g_adam_jni_class, "haptic", "(I)V");
    g_dismiss_keyboard_method = (*env)->GetStaticMethodID(
            env, g_adam_jni_class, "dismissKeyboard", "()V");
}

// ---------------------------------------------------------------------------
// Lifecycle
// ---------------------------------------------------------------------------

JNIEXPORT jint JNICALL
Java_com_adam_runtime_AdamJNI_runtimeInit(JNIEnv *env, jclass cls) {
    LOGI("Adam runtime init");
    cache_method_ids(env);
    return (jint)adam_runtime_init();
}

JNIEXPORT void JNICALL
Java_com_adam_runtime_AdamJNI_runtimeShutdown(JNIEnv *env, jclass cls) {
    LOGI("Adam runtime shutdown");
    adam_runtime_shutdown();

    if (g_adam_jni_class) {
        (*env)->DeleteGlobalRef(env, g_adam_jni_class);
        g_adam_jni_class = NULL;
    }
}

JNIEXPORT void JNICALL
Java_com_adam_runtime_AdamJNI_appDidEnterBackground(JNIEnv *env, jclass cls) {
    adam_app_did_enter_background();
}

JNIEXPORT void JNICALL
Java_com_adam_runtime_AdamJNI_appWillEnterForeground(JNIEnv *env, jclass cls) {
    adam_app_will_enter_foreground();
}

JNIEXPORT void JNICALL
Java_com_adam_runtime_AdamJNI_appDidReceiveMemoryWarning(JNIEnv *env, jclass cls) {
    adam_app_did_receive_memory_warning();
}

// ---------------------------------------------------------------------------
// Rendering
// ---------------------------------------------------------------------------

JNIEXPORT void JNICALL
Java_com_adam_runtime_AdamJNI_renderFrame(JNIEnv *env, jclass cls,
                                          jint width, jint height, jdouble timestamp) {
    adam_render_frame((unsigned int)width, (unsigned int)height, timestamp);

    if (g_skia_surface) {
        adam_skia_surface_flush(g_skia_surface);
    }
}

JNIEXPORT void JNICALL
Java_com_adam_runtime_AdamJNI_setScreenScale(JNIEnv *env, jclass cls, jfloat scale) {
    adam_set_screen_scale(scale);
}

JNIEXPORT void JNICALL
Java_com_adam_runtime_AdamJNI_surfaceCreated(JNIEnv *env, jclass cls, jobject surface) {
    if (g_native_window) {
        ANativeWindow_release(g_native_window);
    }
    g_native_window = ANativeWindow_fromSurface(env, surface);
    LOGI("Surface created: %p", g_native_window);
}

JNIEXPORT void JNICALL
Java_com_adam_runtime_AdamJNI_surfaceChanged(JNIEnv *env, jclass cls,
                                              jint width, jint height) {
    LOGI("Surface changed: %dx%d", width, height);

    // Destroy old Skia surface and create new one.
    if (g_skia_surface) {
        adam_skia_surface_destroy(g_skia_surface);
        g_skia_surface = NULL;
    }

    if (g_native_window) {
        g_skia_surface = adam_skia_surface_create_gl(
                g_native_window, (unsigned int)width, (unsigned int)height);
    }
}

JNIEXPORT void JNICALL
Java_com_adam_runtime_AdamJNI_surfaceDestroyed(JNIEnv *env, jclass cls) {
    LOGI("Surface destroyed");

    if (g_skia_surface) {
        adam_skia_surface_destroy(g_skia_surface);
        g_skia_surface = NULL;
    }

    if (g_native_window) {
        ANativeWindow_release(g_native_window);
        g_native_window = NULL;
    }
}

// ---------------------------------------------------------------------------
// Input
// ---------------------------------------------------------------------------

JNIEXPORT void JNICALL
Java_com_adam_runtime_AdamJNI_touchEvent(JNIEnv *env, jclass cls,
                                         jint touchId, jfloat x, jfloat y,
                                         jint phase, jdouble timestamp) {
    adam_touch_event((long long)touchId, x, y, phase, timestamp);
}

JNIEXPORT void JNICALL
Java_com_adam_runtime_AdamJNI_textInput(JNIEnv *env, jclass cls, jstring text) {
    const char *utf8 = (*env)->GetStringUTFChars(env, text, NULL);
    if (utf8) {
        unsigned int len = (unsigned int)(*env)->GetStringUTFLength(env, text);
        adam_text_input(utf8, len);
        (*env)->ReleaseStringUTFChars(env, text, utf8);
    }
}

JNIEXPORT void JNICALL
Java_com_adam_runtime_AdamJNI_keyEvent(JNIEnv *env, jclass cls,
                                       jint keyCode, jboolean isDown, jint modifiers) {
    adam_key_event((unsigned int)keyCode, isDown ? 1 : 0, (unsigned int)modifiers);
}

// ---------------------------------------------------------------------------
// Screen metrics
// ---------------------------------------------------------------------------

JNIEXPORT void JNICALL
Java_com_adam_runtime_AdamJNI_updateScreenMetrics(
        JNIEnv *env, jclass cls,
        jfloat width, jfloat height, jfloat scale,
        jfloat safeTop, jfloat safeBottom, jfloat safeLeft, jfloat safeRight) {
    adam_update_screen_metrics(width, height, scale,
                               safeTop, safeBottom, safeLeft, safeRight);
}

JNIEXPORT void JNICALL
Java_com_adam_runtime_AdamJNI_setDarkMode(JNIEnv *env, jclass cls, jboolean isDark) {
    adam_set_dark_mode(isDark ? 1 : 0);
}

// ---------------------------------------------------------------------------
// Platform API callbacks (native -> Java)
//
// These are called from the Adam runtime when it needs platform services.
// They use JNI to call back into AdamJNI static methods.
// ---------------------------------------------------------------------------

int adam_android_open_url(const char *url, unsigned int url_len) {
    JNIEnv *env = get_env();
    if (!env || !g_adam_jni_class || !g_open_url_method) return 0;

    jstring jurl = (*env)->NewStringUTF(env, url);
    jboolean result = (*env)->CallStaticBooleanMethod(
            env, g_adam_jni_class, g_open_url_method, jurl);
    (*env)->DeleteLocalRef(env, jurl);
    return result ? 1 : 0;
}

void adam_android_share(const char *text, unsigned int text_len) {
    JNIEnv *env = get_env();
    if (!env || !g_adam_jni_class || !g_share_method) return;

    jstring jtext = (*env)->NewStringUTF(env, text);
    (*env)->CallStaticVoidMethod(env, g_adam_jni_class, g_share_method, jtext);
    (*env)->DeleteLocalRef(env, jtext);
}

void adam_android_store(const char *key, unsigned int key_len,
                        const char *value, unsigned int value_len) {
    JNIEnv *env = get_env();
    if (!env || !g_adam_jni_class || !g_store_method) return;

    jstring jkey = (*env)->NewStringUTF(env, key);
    jstring jval = (*env)->NewStringUTF(env, value);
    (*env)->CallStaticVoidMethod(env, g_adam_jni_class, g_store_method, jkey, jval);
    (*env)->DeleteLocalRef(env, jkey);
    (*env)->DeleteLocalRef(env, jval);
}

unsigned int adam_android_load(const char *key, unsigned int key_len,
                                char *out_buf, unsigned int out_buf_cap) {
    JNIEnv *env = get_env();
    if (!env || !g_adam_jni_class || !g_load_method) return 0;

    jstring jkey = (*env)->NewStringUTF(env, key);
    jstring result = (jstring)(*env)->CallStaticObjectMethod(
            env, g_adam_jni_class, g_load_method, jkey);
    (*env)->DeleteLocalRef(env, jkey);

    if (!result) return 0;

    const char *utf8 = (*env)->GetStringUTFChars(env, result, NULL);
    unsigned int len = (unsigned int)(*env)->GetStringUTFLength(env, result);
    unsigned int copy_len = len < out_buf_cap ? len : out_buf_cap;

    if (utf8) {
        memcpy(out_buf, utf8, copy_len);
        (*env)->ReleaseStringUTFChars(env, result, utf8);
    }
    (*env)->DeleteLocalRef(env, result);
    return copy_len;
}

void adam_android_haptic(int style) {
    JNIEnv *env = get_env();
    if (!env || !g_adam_jni_class || !g_haptic_method) return;
    (*env)->CallStaticVoidMethod(env, g_adam_jni_class, g_haptic_method, (jint)style);
}

void adam_android_dismiss_keyboard(void) {
    JNIEnv *env = get_env();
    if (!env || !g_adam_jni_class || !g_dismiss_keyboard_method) return;
    (*env)->CallStaticVoidMethod(env, g_adam_jni_class, g_dismiss_keyboard_method);
}
