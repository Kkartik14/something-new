package com.adam.runtime;

import android.app.Activity;
import android.content.res.Configuration;
import android.os.Bundle;
import android.view.MotionEvent;
import android.view.View;
import android.view.WindowManager;

/**
 * Main activity for Adam apps. Hosts the AdamSurfaceView which provides
 * the native rendering surface for Skia.
 *
 * Architecture:
 *   AdamActivity
 *     └── AdamSurfaceView (SurfaceView)
 *         └── ANativeWindow
 *             └── Skia Surface (draws via OpenGL ES / Vulkan)
 *                 └── Adam UI Rendering
 */
public class AdamActivity extends Activity {

    private AdamSurfaceView surfaceView;

    // Load the native library containing the Adam runtime and app code.
    static {
        System.loadLibrary("adam_jni");
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        // Go full-screen, drawing behind the system bars.
        getWindow().addFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);

        // Initialize the Adam runtime.
        int result = AdamJNI.runtimeInit();
        if (result != 0) {
            throw new RuntimeException("Adam runtime init failed (code " + result + ")");
        }

        // Report screen metrics.
        updateScreenMetrics();

        // Report initial dark mode state.
        boolean isDark = (getResources().getConfiguration().uiMode
                & Configuration.UI_MODE_NIGHT_MASK) == Configuration.UI_MODE_NIGHT_YES;
        AdamJNI.setDarkMode(isDark);

        // Create the rendering surface.
        surfaceView = new AdamSurfaceView(this);
        setContentView(surfaceView);
    }

    @Override
    protected void onResume() {
        super.onResume();
        AdamJNI.appWillEnterForeground();
        if (surfaceView != null) {
            surfaceView.onResume();
        }
    }

    @Override
    protected void onPause() {
        super.onPause();
        AdamJNI.appDidEnterBackground();
        if (surfaceView != null) {
            surfaceView.onPause();
        }
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        AdamJNI.runtimeShutdown();
    }

    @Override
    public void onLowMemory() {
        super.onLowMemory();
        AdamJNI.appDidReceiveMemoryWarning();
    }

    @Override
    public void onConfigurationChanged(Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
        updateScreenMetrics();
        boolean isDark = (newConfig.uiMode & Configuration.UI_MODE_NIGHT_MASK)
                == Configuration.UI_MODE_NIGHT_YES;
        AdamJNI.setDarkMode(isDark);
    }

    @Override
    public boolean onTouchEvent(MotionEvent event) {
        // Forward touch events to the native runtime.
        int action = event.getActionMasked();
        int pointerIndex = event.getActionIndex();
        int pointerId = event.getPointerId(pointerIndex);
        float x = event.getX(pointerIndex);
        float y = event.getY(pointerIndex);
        double timestamp = (double) event.getEventTime() / 1000.0;

        int phase;
        switch (action) {
            case MotionEvent.ACTION_DOWN:
            case MotionEvent.ACTION_POINTER_DOWN:
                phase = 0; // BEGAN
                break;
            case MotionEvent.ACTION_MOVE:
                phase = 1; // MOVED
                break;
            case MotionEvent.ACTION_UP:
            case MotionEvent.ACTION_POINTER_UP:
                phase = 2; // ENDED
                break;
            case MotionEvent.ACTION_CANCEL:
                phase = 3; // CANCELLED
                break;
            default:
                return super.onTouchEvent(event);
        }

        AdamJNI.touchEvent(pointerId, x, y, phase, timestamp);
        return true;
    }

    private void updateScreenMetrics() {
        View decorView = getWindow().getDecorView();
        float density = getResources().getDisplayMetrics().density;
        float width = decorView.getWidth() / density;
        float height = decorView.getHeight() / density;

        if (width == 0 || height == 0) {
            // Fallback to display metrics before layout.
            width = getResources().getDisplayMetrics().widthPixels / density;
            height = getResources().getDisplayMetrics().heightPixels / density;
        }

        AdamJNI.updateScreenMetrics(
                width, height, density,
                0, 0, 0, 0 // safe area — computed after layout
        );
        AdamJNI.setScreenScale(density);
    }
}
