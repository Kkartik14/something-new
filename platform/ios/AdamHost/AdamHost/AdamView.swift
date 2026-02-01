// AdamView.swift — UIView subclass with Metal-backed Skia rendering surface.
//
// Architecture:
//   AdamView (UIView)
//     └── CAMetalLayer
//         └── Skia GPU Surface (draws via Metal)
//             └── Adam UI Rendering

import UIKit
import Metal
import QuartzCore

/// The main rendering view for Adam apps. Uses CAMetalLayer + Metal for
/// GPU-accelerated Skia rendering.
class AdamView: UIView {

    // MARK: - Metal state

    private var metalDevice: MTLDevice!
    private var commandQueue: MTLCommandQueue!
    private var metalLayer: CAMetalLayer!

    // MARK: - Display link

    private var displayLink: CADisplayLink?

    // MARK: - Skia surface

    private var skiaSurface: AdamSkiaSurface?

    // MARK: - Layer class

    override class var layerClass: AnyClass {
        return CAMetalLayer.self
    }

    // MARK: - Init

    override init(frame: CGRect) {
        super.init(frame: frame)
        setup()
    }

    required init?(coder: NSCoder) {
        super.init(coder: coder)
        setup()
    }

    private func setup() {
        // Get the Metal device.
        guard let device = MTLCreateSystemDefaultDevice() else {
            fatalError("Metal is not supported on this device")
        }
        metalDevice = device
        commandQueue = device.makeCommandQueue()!

        // Configure the Metal layer.
        metalLayer = self.layer as? CAMetalLayer
        metalLayer.device = metalDevice
        metalLayer.pixelFormat = .bgra8Unorm
        metalLayer.framebufferOnly = true
        metalLayer.contentsScale = UIScreen.main.scale

        // Enable multi-touch.
        self.isMultipleTouchEnabled = true
    }

    // MARK: - Render loop

    /// Start the CADisplayLink-driven render loop.
    func startRenderLoop() {
        guard displayLink == nil else { return }
        displayLink = CADisplayLink(target: self, selector: #selector(renderFrame(_:)))
        displayLink?.add(to: .main, forMode: .common)
    }

    /// Stop the render loop.
    func stopRenderLoop() {
        displayLink?.invalidate()
        displayLink = nil
        destroySkiaSurface()
    }

    @objc private func renderFrame(_ link: CADisplayLink) {
        guard let drawable = metalLayer.nextDrawable() else { return }

        let width = UInt32(metalLayer.drawableSize.width)
        let height = UInt32(metalLayer.drawableSize.height)

        // Ensure Skia surface matches current drawable size.
        ensureSkiaSurface(
            texture: drawable.texture,
            width: width,
            height: height
        )

        // Drive the Adam render loop.
        adam_render_frame(width, height, link.timestamp)

        // Flush Skia commands and present.
        if let surface = skiaSurface {
            adam_skia_surface_flush(surface)
        }

        guard let commandBuffer = commandQueue.makeCommandBuffer() else { return }
        commandBuffer.present(drawable)
        commandBuffer.commit()
    }

    override func layoutSubviews() {
        super.layoutSubviews()
        metalLayer.drawableSize = CGSize(
            width: bounds.width * metalLayer.contentsScale,
            height: bounds.height * metalLayer.contentsScale
        )
        // Recreate Skia surface on next frame.
        destroySkiaSurface()
    }

    // MARK: - Skia surface management

    private func ensureSkiaSurface(texture: MTLTexture, width: UInt32, height: UInt32) {
        if skiaSurface == nil {
            skiaSurface = adam_skia_surface_create(
                Unmanaged.passUnretained(metalDevice).toOpaque(),
                Unmanaged.passUnretained(commandQueue).toOpaque(),
                Unmanaged.passUnretained(texture).toOpaque(),
                width,
                height
            )
        }
    }

    private func destroySkiaSurface() {
        if let surface = skiaSurface {
            adam_skia_surface_destroy(surface)
            skiaSurface = nil
        }
    }

    deinit {
        stopRenderLoop()
    }

    // MARK: - Touch handling

    override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?) {
        forwardTouches(touches, phase: ADAM_TOUCH_BEGAN)
    }

    override func touchesMoved(_ touches: Set<UITouch>, with event: UIEvent?) {
        forwardTouches(touches, phase: ADAM_TOUCH_MOVED)
    }

    override func touchesEnded(_ touches: Set<UITouch>, with event: UIEvent?) {
        forwardTouches(touches, phase: ADAM_TOUCH_ENDED)
    }

    override func touchesCancelled(_ touches: Set<UITouch>, with event: UIEvent?) {
        forwardTouches(touches, phase: ADAM_TOUCH_CANCELLED)
    }

    private func forwardTouches(_ touches: Set<UITouch>, phase: AdamTouchPhase) {
        for touch in touches {
            let location = touch.location(in: self)
            let touchId = Int64(touch.hash)
            let timestamp = touch.timestamp

            adam_touch_event(
                touchId,
                Float(location.x),
                Float(location.y),
                phase,
                timestamp
            )
        }
    }
}
