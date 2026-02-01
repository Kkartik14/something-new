package com.adam.runtime;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.net.Uri;
import android.os.Build;
import android.os.VibrationEffect;
import android.os.Vibrator;
import android.os.VibratorManager;
import android.view.inputmethod.InputMethodManager;

/**
 * Android implementations of the platform APIs that the Adam runtime
 * calls via JNI. These provide the same functionality as the iOS
 * equivalents for cross-platform parity.
 */
public class AdamPlatformAPIs {

    private static Activity currentActivity;

    /** Set the current activity reference (called from AdamActivity.onCreate). */
    public static void setActivity(Activity activity) {
        currentActivity = activity;
    }

    // -----------------------------------------------------------------------
    // URL handling
    // -----------------------------------------------------------------------

    public static boolean openUrl(String url) {
        if (currentActivity == null) return false;
        try {
            Intent intent = new Intent(Intent.ACTION_VIEW, Uri.parse(url));
            currentActivity.startActivity(intent);
            return true;
        } catch (Exception e) {
            return false;
        }
    }

    // -----------------------------------------------------------------------
    // Share
    // -----------------------------------------------------------------------

    public static void share(String text) {
        if (currentActivity == null) return;
        Intent intent = new Intent(Intent.ACTION_SEND);
        intent.setType("text/plain");
        intent.putExtra(Intent.EXTRA_TEXT, text);
        currentActivity.startActivity(Intent.createChooser(intent, "Share"));
    }

    // -----------------------------------------------------------------------
    // Storage (SharedPreferences)
    // -----------------------------------------------------------------------

    private static SharedPreferences getPrefs() {
        if (currentActivity == null) return null;
        return currentActivity.getSharedPreferences("adam_storage", Context.MODE_PRIVATE);
    }

    public static void store(String key, String value) {
        SharedPreferences prefs = getPrefs();
        if (prefs == null) return;
        prefs.edit().putString(key, value).apply();
    }

    public static String load(String key) {
        SharedPreferences prefs = getPrefs();
        if (prefs == null) return null;
        return prefs.getString(key, null);
    }

    // -----------------------------------------------------------------------
    // Haptics
    // -----------------------------------------------------------------------

    public static void haptic(int style) {
        if (currentActivity == null) return;

        Vibrator vibrator;
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            VibratorManager vm = (VibratorManager) currentActivity
                    .getSystemService(Context.VIBRATOR_MANAGER_SERVICE);
            vibrator = vm.getDefaultVibrator();
        } else {
            vibrator = (Vibrator) currentActivity.getSystemService(Context.VIBRATOR_SERVICE);
        }

        if (vibrator == null || !vibrator.hasVibrator()) return;

        // Map style to duration: 0=light(10ms), 1=medium(20ms), 2=heavy(40ms).
        long durationMs;
        int amplitude;
        switch (style) {
            case 0:  durationMs = 10; amplitude = 50;  break;
            case 1:  durationMs = 20; amplitude = 128; break;
            case 2:  durationMs = 40; amplitude = 255; break;
            default: durationMs = 20; amplitude = 128; break;
        }

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            vibrator.vibrate(VibrationEffect.createOneShot(durationMs, amplitude));
        } else {
            vibrator.vibrate(durationMs);
        }
    }

    // -----------------------------------------------------------------------
    // Keyboard
    // -----------------------------------------------------------------------

    public static void dismissKeyboard() {
        if (currentActivity == null) return;
        InputMethodManager imm = (InputMethodManager) currentActivity
                .getSystemService(Context.INPUT_METHOD_SERVICE);
        if (imm != null && currentActivity.getCurrentFocus() != null) {
            imm.hideSoftInputFromWindow(
                    currentActivity.getCurrentFocus().getWindowToken(), 0);
        }
    }
}
