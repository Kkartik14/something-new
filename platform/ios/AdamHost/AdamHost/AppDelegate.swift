// AppDelegate.swift — Adam iOS host app entry point.
//
// Sets up the window, initializes the Adam runtime, and forwards
// lifecycle events to the native bridge.

import UIKit

@main
class AppDelegate: UIResponder, UIApplicationDelegate {

    var window: UIWindow?

    func application(
        _ application: UIApplication,
        didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey: Any]?
    ) -> Bool {
        // Initialize the Adam runtime.
        let result = adam_runtime_init()
        if result != 0 {
            fatalError("Adam runtime failed to initialize (code \(result))")
        }

        // Report initial screen metrics.
        updateScreenMetrics()

        // Report initial dark mode state.
        let isDark = UITraitCollection.current.userInterfaceStyle == .dark
        adam_set_dark_mode(isDark)

        // Create the window with AdamView as the root.
        let window = UIWindow(frame: UIScreen.main.bounds)
        let viewController = AdamViewController()
        window.rootViewController = viewController
        window.makeKeyAndVisible()
        self.window = window

        return true
    }

    func applicationDidEnterBackground(_ application: UIApplication) {
        adam_app_did_enter_background()
    }

    func applicationWillEnterForeground(_ application: UIApplication) {
        adam_app_will_enter_foreground()
    }

    func applicationWillTerminate(_ application: UIApplication) {
        adam_runtime_shutdown()
    }

    func applicationDidReceiveMemoryWarning(_ application: UIApplication) {
        adam_app_did_receive_memory_warning()
    }

    // MARK: - Helpers

    private func updateScreenMetrics() {
        let screen = UIScreen.main
        let bounds = screen.bounds
        let scale = screen.scale
        let safeArea = window?.safeAreaInsets ?? .zero

        var metrics = AdamScreenMetrics()
        metrics.width = Float(bounds.width)
        metrics.height = Float(bounds.height)
        metrics.scale = Float(scale)
        metrics.safe_area_top = Float(safeArea.top)
        metrics.safe_area_bottom = Float(safeArea.bottom)
        metrics.safe_area_left = Float(safeArea.left)
        metrics.safe_area_right = Float(safeArea.right)

        adam_update_screen_metrics(metrics)
        adam_set_screen_scale(Float(scale))
    }
}

// MARK: - AdamViewController

/// Root view controller that hosts the AdamView and handles trait changes.
class AdamViewController: UIViewController {

    private var adamView: AdamView!

    override func loadView() {
        adamView = AdamView(frame: UIScreen.main.bounds)
        self.view = adamView
    }

    override func viewDidLoad() {
        super.viewDidLoad()
        adamView.startRenderLoop()
    }

    override func viewWillDisappear(_ animated: Bool) {
        super.viewWillDisappear(animated)
        adamView.stopRenderLoop()
    }

    override func traitCollectionDidChange(_ previousTraitCollection: UITraitCollection?) {
        super.traitCollectionDidChange(previousTraitCollection)
        if traitCollection.userInterfaceStyle != previousTraitCollection?.userInterfaceStyle {
            let isDark = traitCollection.userInterfaceStyle == .dark
            adam_set_dark_mode(isDark)
        }
    }

    override var prefersStatusBarHidden: Bool {
        return false
    }

    override var preferredStatusBarStyle: UIStatusBarStyle {
        // TODO: Drive this from Adam runtime state.
        return .default
    }
}
