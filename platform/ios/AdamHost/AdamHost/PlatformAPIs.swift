// PlatformAPIs.swift — iOS implementations of platform API callbacks.
//
// These are the Swift-side implementations of the C functions declared
// in bridge.h that the Adam runtime calls for platform-specific operations.

import UIKit

// MARK: - URL handling

@_cdecl("adam_platform_open_url")
func adamPlatformOpenUrl(_ url: UnsafePointer<CChar>, _ urlLen: UInt32) -> Bool {
    guard let string = String(bytes: UnsafeBufferPointer(start: url, count: Int(urlLen)),
                              encoding: .utf8),
          let parsedUrl = URL(string: string) else {
        return false
    }
    // UIApplication.open is async; we fire and forget.
    DispatchQueue.main.async {
        UIApplication.shared.open(parsedUrl, options: [:], completionHandler: nil)
    }
    return true
}

// MARK: - Share sheet

@_cdecl("adam_platform_share")
func adamPlatformShare(_ text: UnsafePointer<CChar>, _ textLen: UInt32) {
    guard let string = String(bytes: UnsafeBufferPointer(start: text, count: Int(textLen)),
                              encoding: .utf8) else {
        return
    }
    DispatchQueue.main.async {
        let activityVC = UIActivityViewController(
            activityItems: [string],
            applicationActivities: nil
        )
        if let rootVC = UIApplication.shared.windows.first?.rootViewController {
            rootVC.present(activityVC, animated: true)
        }
    }
}

// MARK: - Storage (UserDefaults)

@_cdecl("adam_platform_store")
func adamPlatformStore(
    _ key: UnsafePointer<CChar>, _ keyLen: UInt32,
    _ value: UnsafePointer<CChar>, _ valueLen: UInt32
) {
    guard let keyStr = String(bytes: UnsafeBufferPointer(start: key, count: Int(keyLen)),
                              encoding: .utf8),
          let valueStr = String(bytes: UnsafeBufferPointer(start: value, count: Int(valueLen)),
                                encoding: .utf8) else {
        return
    }
    UserDefaults.standard.set(valueStr, forKey: "adam.\(keyStr)")
}

@_cdecl("adam_platform_load")
func adamPlatformLoad(
    _ key: UnsafePointer<CChar>, _ keyLen: UInt32,
    _ outBuf: UnsafeMutablePointer<CChar>, _ outBufCap: UInt32
) -> UInt32 {
    guard let keyStr = String(bytes: UnsafeBufferPointer(start: key, count: Int(keyLen)),
                              encoding: .utf8),
          let value = UserDefaults.standard.string(forKey: "adam.\(keyStr)") else {
        return 0
    }
    let utf8 = Array(value.utf8)
    let copyLen = min(utf8.count, Int(outBufCap))
    utf8.withUnsafeBufferPointer { buf in
        outBuf.update(from: UnsafeRawPointer(buf.baseAddress!).assumingMemoryBound(to: CChar.self),
                       count: copyLen)
    }
    return UInt32(copyLen)
}

// MARK: - Haptics

@_cdecl("adam_platform_haptic")
func adamPlatformHaptic(_ style: AdamHapticStyle) {
    DispatchQueue.main.async {
        let feedbackStyle: UIImpactFeedbackGenerator.FeedbackStyle
        switch style {
        case ADAM_HAPTIC_LIGHT:  feedbackStyle = .light
        case ADAM_HAPTIC_MEDIUM: feedbackStyle = .medium
        case ADAM_HAPTIC_HEAVY:  feedbackStyle = .heavy
        default:                feedbackStyle = .medium
        }
        let generator = UIImpactFeedbackGenerator(style: feedbackStyle)
        generator.prepare()
        generator.impactOccurred()
    }
}

// MARK: - Status bar

// Global status bar style state, read by the view controller.
var adamStatusBarStyleOverride: UIStatusBarStyle = .default

@_cdecl("adam_platform_set_status_bar_style")
func adamPlatformSetStatusBarStyle(_ style: AdamStatusBarStyle) {
    DispatchQueue.main.async {
        switch style {
        case ADAM_STATUS_BAR_LIGHT: adamStatusBarStyleOverride = .lightContent
        case ADAM_STATUS_BAR_DARK:  adamStatusBarStyleOverride = .darkContent
        default:                    adamStatusBarStyleOverride = .default
        }
        // Tell the root VC to update.
        UIApplication.shared.windows.first?.rootViewController?.setNeedsStatusBarAppearanceUpdate()
    }
}

// MARK: - Keyboard

@_cdecl("adam_platform_dismiss_keyboard")
func adamPlatformDismissKeyboard() {
    DispatchQueue.main.async {
        UIApplication.shared.sendAction(
            #selector(UIResponder.resignFirstResponder),
            to: nil, from: nil, for: nil
        )
    }
}
