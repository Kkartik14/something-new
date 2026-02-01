package com.adam.runtime;

import android.view.Surface;

/**
 * JNI bridge class — declares native methods implemented in adam_jni.c.
 *
 * These methods are the Java-to-native bridge. Each one maps to a C
 * function with the JNI naming convention:
 *   Java_com_adam_runtime_AdamJNI_<methodName>
 */
public class AdamJNI {

    // -----------------------------------------------------------------------
    // App lifecycle
    // -----------------------------------------------------------------------

    /** Initialize the Adam runtime. Returns 0 on success. */
    public static native int runtimeInit();

    /** Shut down the Adam runtime. */
    public static native void runtimeShutdown();

    /** App is entering the background. */
    public static native void appDidEnterBackground();

    /** App is entering the foreground. */
    public static native void appWillEnterForeground();

    /** System is low on memory. */
    public static native void appDidReceiveMemoryWarning();

    // -----------------------------------------------------------------------
    // Rendering
    // -----------------------------------------------------------------------

    /** Called each frame by Choreographer. */
    public static native void renderFrame(int width, int height, double timestamp);

    /** Set the screen scale (pixel density). */
    public static native void setScreenScale(float scale);

    /** Surface created — pass the ANativeWindow. */
    public static native void surfaceCreated(Surface surface);

    /** Surface dimensions changed. */
    public static native void surfaceChanged(int width, int height);

    /** Surface destroyed. */
    public static native void surfaceDestroyed();

    // -----------------------------------------------------------------------
    // Input
    // -----------------------------------------------------------------------

    /** Forward a touch event. */
    public static native void touchEvent(
            int touchId, float x, float y, int phase, double timestamp);

    /** Forward a text input event. */
    public static native void textInput(String text);

    /** Forward a key event. */
    public static native void keyEvent(int keyCode, boolean isDown, int modifiers);

    // -----------------------------------------------------------------------
    // Screen metrics
    // -----------------------------------------------------------------------

    /** Update screen metrics (size, density, safe area). */
    public static native void updateScreenMetrics(
            float width, float height, float scale,
            float safeTop, float safeBottom, float safeLeft, float safeRight);

    /** Notify of dark mode change. */
    public static native void setDarkMode(boolean isDark);

    // -----------------------------------------------------------------------
    // Platform APIs (called from native into Java)
    //
    // These are NOT native methods — they are called from native code via
    // JNI CallStaticVoidMethod. Declared here for documentation.
    // -----------------------------------------------------------------------

    /** Called from native to open a URL. */
    public static boolean openUrl(String url) {
        // Dispatched to the Activity via a handler.
        return AdamPlatformAPIs.openUrl(url);
    }

    /** Called from native to share text. */
    public static void share(String text) {
        AdamPlatformAPIs.share(text);
    }

    /** Called from native to store a key-value pair. */
    public static void store(String key, String value) {
        AdamPlatformAPIs.store(key, value);
    }

    /** Called from native to load a stored value. */
    public static String load(String key) {
        return AdamPlatformAPIs.load(key);
    }

    /** Called from native to trigger haptic feedback. */
    public static void haptic(int style) {
        AdamPlatformAPIs.haptic(style);
    }

    /** Called from native to dismiss the keyboard. */
    public static void dismissKeyboard() {
        AdamPlatformAPIs.dismissKeyboard();
    }
}
