package com.adam.runtime;

import android.content.Context;
import android.view.Choreographer;
import android.view.Surface;
import android.view.SurfaceHolder;
import android.view.SurfaceView;

/**
 * SurfaceView that provides a native window for Skia rendering.
 *
 * The Choreographer drives the render loop at the display's refresh rate.
 * Each frame:
 *   1. Choreographer fires vsync callback
 *   2. We call AdamJNI.renderFrame() which invokes the Adam render pipeline
 *   3. Adam draws via Skia to the ANativeWindow
 *   4. SurfaceFlinger composites the result to screen
 */
public class AdamSurfaceView extends SurfaceView
        implements SurfaceHolder.Callback, Choreographer.FrameCallback {

    private boolean isRunning = false;
    private boolean hasSurface = false;
    private int surfaceWidth = 0;
    private int surfaceHeight = 0;

    public AdamSurfaceView(Context context) {
        super(context);
        getHolder().addCallback(this);
    }

    // -----------------------------------------------------------------------
    // SurfaceHolder.Callback
    // -----------------------------------------------------------------------

    @Override
    public void surfaceCreated(SurfaceHolder holder) {
        Surface surface = holder.getSurface();
        if (surface != null && surface.isValid()) {
            AdamJNI.surfaceCreated(surface);
            hasSurface = true;
            startRenderLoop();
        }
    }

    @Override
    public void surfaceChanged(SurfaceHolder holder, int format, int width, int height) {
        surfaceWidth = width;
        surfaceHeight = height;
        AdamJNI.surfaceChanged(width, height);
    }

    @Override
    public void surfaceDestroyed(SurfaceHolder holder) {
        hasSurface = false;
        stopRenderLoop();
        AdamJNI.surfaceDestroyed();
    }

    // -----------------------------------------------------------------------
    // Choreographer.FrameCallback (render loop)
    // -----------------------------------------------------------------------

    @Override
    public void doFrame(long frameTimeNanos) {
        if (isRunning && hasSurface) {
            double timestamp = (double) frameTimeNanos / 1_000_000_000.0;
            AdamJNI.renderFrame(surfaceWidth, surfaceHeight, timestamp);

            // Request next frame.
            Choreographer.getInstance().postFrameCallback(this);
        }
    }

    // -----------------------------------------------------------------------
    // Lifecycle
    // -----------------------------------------------------------------------

    public void onResume() {
        isRunning = true;
        if (hasSurface) {
            startRenderLoop();
        }
    }

    public void onPause() {
        isRunning = false;
        stopRenderLoop();
    }

    private void startRenderLoop() {
        if (isRunning && hasSurface) {
            Choreographer.getInstance().postFrameCallback(this);
        }
    }

    private void stopRenderLoop() {
        Choreographer.getInstance().removeFrameCallback(this);
    }
}
