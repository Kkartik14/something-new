//! Input event handling for Adam UI.
//!
//! Handles touch, mouse, and keyboard events. Routes events to the correct
//! view via hit testing on the layout tree.

use crate::layout::{LayoutNode, LayoutRect};
use crate::vtree::{ActionId, ViewNode};

// ---------------------------------------------------------------------------
// Input events
// ---------------------------------------------------------------------------

/// Raw input event from the platform.
#[derive(Debug, Clone, PartialEq)]
pub enum InputEvent {
    /// Touch or mouse down.
    PointerDown { x: f32, y: f32, pointer_id: u32 },
    /// Touch or mouse move.
    PointerMove { x: f32, y: f32, pointer_id: u32 },
    /// Touch or mouse up.
    PointerUp { x: f32, y: f32, pointer_id: u32 },
    /// Mouse scroll / trackpad scroll.
    Scroll {
        x: f32,
        y: f32,
        delta_x: f32,
        delta_y: f32,
    },
    /// Key pressed.
    KeyDown {
        key: KeyCode,
        modifiers: KeyModifiers,
    },
    /// Key released.
    KeyUp {
        key: KeyCode,
        modifiers: KeyModifiers,
    },
    /// Text input (after IME processing).
    TextInput { text: String },
}

/// Key codes for keyboard events.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum KeyCode {
    A,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
    I,
    J,
    K,
    L,
    M,
    N,
    O,
    P,
    Q,
    R,
    S,
    T,
    U,
    V,
    W,
    X,
    Y,
    Z,
    Num0,
    Num1,
    Num2,
    Num3,
    Num4,
    Num5,
    Num6,
    Num7,
    Num8,
    Num9,
    Enter,
    Escape,
    Backspace,
    Tab,
    Space,
    Left,
    Right,
    Up,
    Down,
    Home,
    End,
    PageUp,
    PageDown,
    Delete,
    Insert,
    F1,
    F2,
    F3,
    F4,
    F5,
    F6,
    F7,
    F8,
    F9,
    F10,
    F11,
    F12,
    Unknown,
}

/// Key modifier state.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct KeyModifiers {
    pub shift: bool,
    pub ctrl: bool,
    pub alt: bool,
    pub meta: bool,
}

// ---------------------------------------------------------------------------
// UI events (processed from input events)
// ---------------------------------------------------------------------------

/// Processed UI event routed to a specific view node.
#[derive(Debug, Clone, PartialEq)]
pub enum UIEvent {
    /// Tap on a view.
    Tap { action: ActionId, x: f32, y: f32 },
    /// Long press on a view (> 500ms).
    LongPress { action: ActionId, x: f32, y: f32 },
    /// Scroll event in a scroll view.
    ScrollEvent { delta_x: f32, delta_y: f32 },
    /// Text input for a text field.
    TextChanged { text: String },
    /// Key event for focused view.
    KeyEvent {
        key: KeyCode,
        modifiers: KeyModifiers,
    },
}

/// Result of a hit test: the path to the node and its layout rect.
#[derive(Debug, Clone)]
pub struct HitTestResult {
    pub path: Vec<usize>,
    pub rect: LayoutRect,
    pub action: Option<ActionId>,
}

// ---------------------------------------------------------------------------
// Hit testing
// ---------------------------------------------------------------------------

/// Find the deepest node at point (x, y) in the layout tree.
/// Returns the hit path and the node's action if it has one.
pub fn hit_test(
    view_tree: &ViewNode,
    layout_tree: &LayoutNode,
    x: f32,
    y: f32,
) -> Option<HitTestResult> {
    let mut path = Vec::new();
    let mut best: Option<HitTestResult> = None;
    hit_test_recursive(view_tree, layout_tree, x, y, &mut path, &mut best);
    best
}

fn hit_test_recursive(
    view_node: &ViewNode,
    layout_node: &LayoutNode,
    x: f32,
    y: f32,
    path: &mut Vec<usize>,
    best: &mut Option<HitTestResult>,
) {
    if !layout_node.rect.contains_point(x, y) {
        return;
    }

    // This node contains the point — it's a candidate
    let action = node_action(view_node);
    *best = Some(HitTestResult {
        path: path.clone(),
        rect: layout_node.rect,
        action,
    });

    // Check children (last child = topmost in z-order for Stack)
    let children = node_children(view_node);
    if let Some(children) = children {
        for (i, (child_view, child_layout)) in
            children.iter().zip(layout_node.children.iter()).enumerate()
        {
            path.push(i);
            hit_test_recursive(child_view, child_layout, x, y, path, best);
            path.pop();
        }
    }
}

fn node_action(node: &ViewNode) -> Option<ActionId> {
    match node {
        ViewNode::Button { action, .. } => Some(*action),
        ViewNode::Text { modifiers, .. }
        | ViewNode::Image { modifiers, .. }
        | ViewNode::Container { modifiers, .. }
        | ViewNode::TextInput { modifiers, .. }
        | ViewNode::Toggle { modifiers, .. }
        | ViewNode::Slider { modifiers, .. }
        | ViewNode::List { modifiers, .. }
        | ViewNode::Custom { modifiers, .. }
        | ViewNode::Progress { modifiers, .. }
        | ViewNode::Spacer { modifiers, .. } => modifiers.on_tap,
        ViewNode::Conditional { child, .. } => node_action(child),
        ViewNode::Empty => None,
    }
}

fn node_children(node: &ViewNode) -> Option<&[ViewNode]> {
    match node {
        ViewNode::Container { children, .. } => Some(children),
        ViewNode::List { items, .. } => Some(items),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Event processor
// ---------------------------------------------------------------------------

/// Processes raw input events into UI events.
pub struct EventProcessor {
    /// Currently pressed pointer and its position
    active_pointer: Option<PointerState>,
    /// Time when pointer went down (for long press detection)
    pointer_down_time_ms: u64,
    /// Long press threshold in milliseconds
    pub long_press_threshold_ms: u64,
}

struct PointerState {
    pointer_id: u32,
    start_x: f32,
    start_y: f32,
    current_x: f32,
    current_y: f32,
}

impl EventProcessor {
    pub fn new() -> Self {
        Self {
            active_pointer: None,
            pointer_down_time_ms: 0,
            long_press_threshold_ms: 500,
        }
    }

    /// Process an input event and return any UI events produced.
    /// `current_time_ms` is the current timestamp in milliseconds.
    pub fn process(
        &mut self,
        event: &InputEvent,
        view_tree: &ViewNode,
        layout_tree: &LayoutNode,
        current_time_ms: u64,
    ) -> Vec<UIEvent> {
        let mut ui_events = Vec::new();

        match event {
            InputEvent::PointerDown { x, y, pointer_id } => {
                self.active_pointer = Some(PointerState {
                    pointer_id: *pointer_id,
                    start_x: *x,
                    start_y: *y,
                    current_x: *x,
                    current_y: *y,
                });
                self.pointer_down_time_ms = current_time_ms;
            }

            InputEvent::PointerMove { x, y, pointer_id } => {
                if let Some(ref mut state) = self.active_pointer {
                    if state.pointer_id == *pointer_id {
                        state.current_x = *x;
                        state.current_y = *y;
                    }
                }
            }

            InputEvent::PointerUp { x, y, pointer_id } => {
                if let Some(ref state) = self.active_pointer {
                    if state.pointer_id == *pointer_id {
                        let dx = *x - state.start_x;
                        let dy = *y - state.start_y;
                        let distance = (dx * dx + dy * dy).sqrt();
                        let duration = current_time_ms - self.pointer_down_time_ms;

                        if distance < 10.0 {
                            // Tap or long press (not a drag)
                            if let Some(hit) = hit_test(view_tree, layout_tree, *x, *y) {
                                if let Some(action) = hit.action {
                                    if duration >= self.long_press_threshold_ms {
                                        ui_events.push(UIEvent::LongPress {
                                            action,
                                            x: *x,
                                            y: *y,
                                        });
                                    } else {
                                        ui_events.push(UIEvent::Tap {
                                            action,
                                            x: *x,
                                            y: *y,
                                        });
                                    }
                                }
                            }
                        }
                    }
                }
                self.active_pointer = None;
            }

            InputEvent::Scroll {
                delta_x, delta_y, ..
            } => {
                ui_events.push(UIEvent::ScrollEvent {
                    delta_x: *delta_x,
                    delta_y: *delta_y,
                });
            }

            InputEvent::TextInput { text } => {
                ui_events.push(UIEvent::TextChanged { text: text.clone() });
            }

            InputEvent::KeyDown { key, modifiers } => {
                ui_events.push(UIEvent::KeyEvent {
                    key: *key,
                    modifiers: *modifiers,
                });
            }

            InputEvent::KeyUp { .. } => {
                // Key up events don't produce UI events by default
            }
        }

        ui_events
    }
}

impl Default for EventProcessor {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::layout::*;
    use crate::vtree::*;

    fn text_node(s: &str) -> ViewNode {
        ViewNode::Text {
            content: s.to_string(),
            modifiers: Default::default(),
        }
    }

    fn button_node(label: &str, action: u64) -> ViewNode {
        ViewNode::Button {
            label: label.to_string(),
            action: ActionId(action),
            modifiers: Default::default(),
        }
    }

    fn make_layout(view: &ViewNode) -> LayoutNode {
        let tm = EstimatedTextMeasure::default();
        layout(view, Constraints::loose(400.0, 800.0), &tm)
    }

    #[test]
    fn test_hit_test_button() {
        let view = ViewNode::Container {
            kind: ContainerKind::Column,
            children: vec![text_node("header"), button_node("Click Me", 42)],
            modifiers: Default::default(),
        };
        let layout_tree = make_layout(&view);

        // Hit the button area (below header)
        let btn_rect = layout_tree.children[1].rect;
        let result = hit_test(&view, &layout_tree, btn_rect.x + 5.0, btn_rect.y + 5.0);
        assert!(result.is_some());
        let hit = result.unwrap();
        assert_eq!(hit.action, Some(ActionId(42)));
    }

    #[test]
    fn test_hit_test_miss() {
        let view = button_node("btn", 1);
        let layout_tree = make_layout(&view);

        // Hit outside the button
        let result = hit_test(&view, &layout_tree, 999.0, 999.0);
        assert!(result.is_none());
    }

    #[test]
    fn test_hit_test_text_no_action() {
        let view = text_node("no action");
        let layout_tree = make_layout(&view);

        let result = hit_test(&view, &layout_tree, 5.0, 5.0);
        assert!(result.is_some());
        assert_eq!(result.unwrap().action, None);
    }

    #[test]
    fn test_event_processor_tap() {
        let view = button_node("btn", 1);
        let layout_tree = make_layout(&view);
        let mut processor = EventProcessor::new();

        let events = processor.process(
            &InputEvent::PointerDown {
                x: 5.0,
                y: 5.0,
                pointer_id: 0,
            },
            &view,
            &layout_tree,
            0,
        );
        assert!(events.is_empty());

        let events = processor.process(
            &InputEvent::PointerUp {
                x: 5.0,
                y: 5.0,
                pointer_id: 0,
            },
            &view,
            &layout_tree,
            100, // 100ms later — well under long press threshold
        );
        assert_eq!(events.len(), 1);
        assert!(matches!(
            &events[0],
            UIEvent::Tap {
                action: ActionId(1),
                ..
            }
        ));
    }

    #[test]
    fn test_event_processor_long_press() {
        let view = button_node("btn", 1);
        let layout_tree = make_layout(&view);
        let mut processor = EventProcessor::new();

        processor.process(
            &InputEvent::PointerDown {
                x: 5.0,
                y: 5.0,
                pointer_id: 0,
            },
            &view,
            &layout_tree,
            0,
        );

        let events = processor.process(
            &InputEvent::PointerUp {
                x: 5.0,
                y: 5.0,
                pointer_id: 0,
            },
            &view,
            &layout_tree,
            600, // 600ms — over long press threshold
        );
        assert_eq!(events.len(), 1);
        assert!(matches!(
            &events[0],
            UIEvent::LongPress {
                action: ActionId(1),
                ..
            }
        ));
    }

    #[test]
    fn test_event_processor_scroll() {
        let view = text_node("x");
        let layout_tree = make_layout(&view);
        let mut processor = EventProcessor::new();

        let events = processor.process(
            &InputEvent::Scroll {
                x: 10.0,
                y: 10.0,
                delta_x: 0.0,
                delta_y: -50.0,
            },
            &view,
            &layout_tree,
            0,
        );
        assert_eq!(events.len(), 1);
        assert!(matches!(&events[0], UIEvent::ScrollEvent { delta_y, .. } if *delta_y == -50.0));
    }

    #[test]
    fn test_event_processor_text_input() {
        let view = text_node("x");
        let layout_tree = make_layout(&view);
        let mut processor = EventProcessor::new();

        let events = processor.process(
            &InputEvent::TextInput {
                text: "hello".to_string(),
            },
            &view,
            &layout_tree,
            0,
        );
        assert_eq!(events.len(), 1);
        assert!(matches!(&events[0], UIEvent::TextChanged { text } if text == "hello"));
    }

    #[test]
    fn test_event_processor_drag_not_tap() {
        let view = button_node("btn", 1);
        let layout_tree = make_layout(&view);
        let mut processor = EventProcessor::new();

        processor.process(
            &InputEvent::PointerDown {
                x: 5.0,
                y: 5.0,
                pointer_id: 0,
            },
            &view,
            &layout_tree,
            0,
        );

        // Move far away (drag)
        processor.process(
            &InputEvent::PointerMove {
                x: 200.0,
                y: 200.0,
                pointer_id: 0,
            },
            &view,
            &layout_tree,
            50,
        );

        // Release far from start — should NOT produce tap
        let events = processor.process(
            &InputEvent::PointerUp {
                x: 200.0,
                y: 200.0,
                pointer_id: 0,
            },
            &view,
            &layout_tree,
            100,
        );
        assert!(events.is_empty(), "drag should not produce tap");
    }

    #[test]
    fn test_hit_test_overlapping_stack() {
        let view = ViewNode::Container {
            kind: ContainerKind::Stack,
            children: vec![button_node("back", 1), button_node("front", 2)],
            modifiers: Default::default(),
        };
        let layout_tree = make_layout(&view);

        let result = hit_test(&view, &layout_tree, 5.0, 5.0);
        assert!(result.is_some());
        // Front button (index 1) should be the deepest hit (topmost in z-order)
        let hit = result.unwrap();
        assert_eq!(hit.action, Some(ActionId(2)));
        assert_eq!(hit.path, vec![1]);
    }
}
