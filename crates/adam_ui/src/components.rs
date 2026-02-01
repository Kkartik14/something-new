//! Built-in view components for Adam UI.
//!
//! These are convenience constructors for creating common UI nodes.
//! In the Adam language, these are the standard library UI types.

use crate::vtree::*;

// ---------------------------------------------------------------------------
// Text
// ---------------------------------------------------------------------------

/// Create a text view.
pub fn text(content: impl Into<String>) -> ViewNode {
    ViewNode::Text {
        content: content.into(),
        modifiers: Default::default(),
    }
}

/// Create a styled text view.
pub fn styled_text(content: impl Into<String>, font_size: f32, color: Color) -> ViewNode {
    ViewNode::Text {
        content: content.into(),
        modifiers: Modifiers {
            font: Some(Font {
                size: font_size,
                ..Default::default()
            }),
            foreground: Some(color),
            ..Default::default()
        },
    }
}

// ---------------------------------------------------------------------------
// Button
// ---------------------------------------------------------------------------

/// Create a button.
pub fn button(label: impl Into<String>, action: ActionId) -> ViewNode {
    ViewNode::Button {
        label: label.into(),
        action,
        modifiers: Default::default(),
    }
}

/// Create a styled button.
pub fn styled_button(
    label: impl Into<String>,
    action: ActionId,
    background: Color,
    foreground: Color,
    corner_radius: f32,
) -> ViewNode {
    ViewNode::Button {
        label: label.into(),
        action,
        modifiers: Modifiers {
            background: Some(background),
            foreground: Some(foreground),
            corner_radius: Some(corner_radius),
            ..Default::default()
        },
    }
}

// ---------------------------------------------------------------------------
// Image
// ---------------------------------------------------------------------------

/// Create an image from an asset path.
pub fn image_asset(path: impl Into<String>) -> ViewNode {
    ViewNode::Image {
        source: ImageSource::Asset(path.into()),
        modifiers: Default::default(),
    }
}

/// Create an image from a URL.
pub fn image_url(url: impl Into<String>) -> ViewNode {
    ViewNode::Image {
        source: ImageSource::Url(url.into()),
        modifiers: Default::default(),
    }
}

// ---------------------------------------------------------------------------
// TextInput
// ---------------------------------------------------------------------------

/// Create a text input field.
pub fn text_input(
    value: impl Into<String>,
    placeholder: impl Into<String>,
    binding: BindingId,
) -> ViewNode {
    ViewNode::TextInput {
        value: value.into(),
        binding,
        placeholder: placeholder.into(),
        modifiers: Default::default(),
    }
}

// ---------------------------------------------------------------------------
// Toggle
// ---------------------------------------------------------------------------

/// Create a toggle switch.
pub fn toggle(is_on: bool, binding: BindingId) -> ViewNode {
    ViewNode::Toggle {
        is_on,
        binding,
        modifiers: Default::default(),
    }
}

// ---------------------------------------------------------------------------
// Slider
// ---------------------------------------------------------------------------

/// Create a slider.
pub fn slider(value: f64, min: f64, max: f64, binding: BindingId) -> ViewNode {
    ViewNode::Slider {
        value,
        min,
        max,
        binding,
        modifiers: Default::default(),
    }
}

// ---------------------------------------------------------------------------
// Containers
// ---------------------------------------------------------------------------

/// Create a vertical stack (Column).
pub fn column(children: Vec<ViewNode>) -> ViewNode {
    ViewNode::Container {
        kind: ContainerKind::Column,
        children,
        modifiers: Default::default(),
    }
}

/// Create a vertical stack with spacing.
pub fn column_spaced(spacing: f32, children: Vec<ViewNode>) -> ViewNode {
    ViewNode::Container {
        kind: ContainerKind::Column,
        children,
        modifiers: Modifiers {
            spacing: Some(spacing),
            ..Default::default()
        },
    }
}

/// Create a horizontal stack (Row).
pub fn row(children: Vec<ViewNode>) -> ViewNode {
    ViewNode::Container {
        kind: ContainerKind::Row,
        children,
        modifiers: Default::default(),
    }
}

/// Create a horizontal stack with spacing.
pub fn row_spaced(spacing: f32, children: Vec<ViewNode>) -> ViewNode {
    ViewNode::Container {
        kind: ContainerKind::Row,
        children,
        modifiers: Modifiers {
            spacing: Some(spacing),
            ..Default::default()
        },
    }
}

/// Create a z-axis overlay stack.
pub fn stack(children: Vec<ViewNode>) -> ViewNode {
    ViewNode::Container {
        kind: ContainerKind::Stack,
        children,
        modifiers: Default::default(),
    }
}

/// Create a scrollable container.
pub fn scroll(children: Vec<ViewNode>) -> ViewNode {
    ViewNode::Container {
        kind: ContainerKind::Scroll,
        children,
        modifiers: Default::default(),
    }
}

// ---------------------------------------------------------------------------
// List
// ---------------------------------------------------------------------------

/// Create a list view.
pub fn list(items: Vec<ViewNode>) -> ViewNode {
    ViewNode::List {
        items,
        modifiers: Default::default(),
    }
}

/// Create a list view with keyed items.
pub fn keyed_list(items: Vec<(Key, ViewNode)>) -> ViewNode {
    let nodes: Vec<ViewNode> = items
        .into_iter()
        .map(|(key, mut node)| {
            set_key(&mut node, key);
            node
        })
        .collect();
    ViewNode::List {
        items: nodes,
        modifiers: Default::default(),
    }
}

fn set_key(node: &mut ViewNode, key: Key) {
    match node {
        ViewNode::Text { modifiers, .. }
        | ViewNode::Image { modifiers, .. }
        | ViewNode::Container { modifiers, .. }
        | ViewNode::Button { modifiers, .. }
        | ViewNode::TextInput { modifiers, .. }
        | ViewNode::Toggle { modifiers, .. }
        | ViewNode::Slider { modifiers, .. }
        | ViewNode::List { modifiers, .. }
        | ViewNode::Custom { modifiers, .. }
        | ViewNode::Progress { modifiers, .. }
        | ViewNode::Spacer { modifiers, .. } => {
            modifiers.key = Some(key);
        }
        ViewNode::Conditional { child, .. } => set_key(child, key),
        ViewNode::Empty => {}
    }
}

// ---------------------------------------------------------------------------
// Progress
// ---------------------------------------------------------------------------

/// Create a determinate progress indicator.
pub fn progress(value: f64) -> ViewNode {
    ViewNode::Progress {
        value: Some(value),
        modifiers: Default::default(),
    }
}

/// Create an indeterminate progress indicator.
pub fn indeterminate_progress() -> ViewNode {
    ViewNode::Progress {
        value: None,
        modifiers: Default::default(),
    }
}

// ---------------------------------------------------------------------------
// Spacer
// ---------------------------------------------------------------------------

/// Create a flexible spacer.
pub fn spacer() -> ViewNode {
    ViewNode::Spacer {
        modifiers: Default::default(),
    }
}

/// Create a spacer with a specific flex weight.
pub fn weighted_spacer(flex: f32) -> ViewNode {
    ViewNode::Spacer {
        modifiers: Modifiers {
            flex: Some(flex),
            ..Default::default()
        },
    }
}

// ---------------------------------------------------------------------------
// Conditional
// ---------------------------------------------------------------------------

/// Conditionally show a view.
pub fn show_if(condition: bool, child: ViewNode) -> ViewNode {
    ViewNode::Conditional {
        condition,
        child: Box::new(child),
    }
}

// ---------------------------------------------------------------------------
// Modifier helpers (builder pattern)
// ---------------------------------------------------------------------------

/// Extension trait for applying modifiers to view nodes.
pub trait ViewModifiers {
    fn padding(self, insets: EdgeInsets) -> Self;
    fn background(self, color: Color) -> Self;
    fn foreground(self, color: Color) -> Self;
    fn corner_radius(self, radius: f32) -> Self;
    fn border(self, width: f32, color: Color) -> Self;
    fn opacity(self, opacity: f32) -> Self;
    fn on_tap(self, action: ActionId) -> Self;
    fn fixed_size(self, width: f32, height: f32) -> Self;
    fn accessibility(self, label: impl Into<String>) -> Self;
}

impl ViewModifiers for ViewNode {
    fn padding(mut self, insets: EdgeInsets) -> Self {
        if let Some(m) = get_modifiers_mut(&mut self) {
            m.padding = Some(insets);
        }
        self
    }

    fn background(mut self, color: Color) -> Self {
        if let Some(m) = get_modifiers_mut(&mut self) {
            m.background = Some(color);
        }
        self
    }

    fn foreground(mut self, color: Color) -> Self {
        if let Some(m) = get_modifiers_mut(&mut self) {
            m.foreground = Some(color);
        }
        self
    }

    fn corner_radius(mut self, radius: f32) -> Self {
        if let Some(m) = get_modifiers_mut(&mut self) {
            m.corner_radius = Some(radius);
        }
        self
    }

    fn border(mut self, width: f32, color: Color) -> Self {
        if let Some(m) = get_modifiers_mut(&mut self) {
            m.border = Some(Border { width, color });
        }
        self
    }

    fn opacity(mut self, opacity: f32) -> Self {
        if let Some(m) = get_modifiers_mut(&mut self) {
            m.opacity = Some(opacity);
        }
        self
    }

    fn on_tap(mut self, action: ActionId) -> Self {
        if let Some(m) = get_modifiers_mut(&mut self) {
            m.on_tap = Some(action);
        }
        self
    }

    fn fixed_size(mut self, width: f32, height: f32) -> Self {
        if let Some(m) = get_modifiers_mut(&mut self) {
            m.size = Some(Size {
                width: Dimension::Fixed(width),
                height: Dimension::Fixed(height),
            });
        }
        self
    }

    fn accessibility(mut self, label: impl Into<String>) -> Self {
        if let Some(m) = get_modifiers_mut(&mut self) {
            m.accessibility_label = Some(label.into());
        }
        self
    }
}

fn get_modifiers_mut(node: &mut ViewNode) -> Option<&mut Modifiers> {
    match node {
        ViewNode::Text { modifiers, .. }
        | ViewNode::Image { modifiers, .. }
        | ViewNode::Container { modifiers, .. }
        | ViewNode::Button { modifiers, .. }
        | ViewNode::TextInput { modifiers, .. }
        | ViewNode::Toggle { modifiers, .. }
        | ViewNode::Slider { modifiers, .. }
        | ViewNode::List { modifiers, .. }
        | ViewNode::Custom { modifiers, .. }
        | ViewNode::Progress { modifiers, .. }
        | ViewNode::Spacer { modifiers, .. } => Some(modifiers),
        ViewNode::Conditional { .. } => None,
        ViewNode::Empty => None,
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_text_component() {
        let node = text("Hello");
        match &node {
            ViewNode::Text { content, .. } => assert_eq!(content, "Hello"),
            _ => panic!("expected text"),
        }
    }

    #[test]
    fn test_button_component() {
        let node = button("Click", ActionId(1));
        match &node {
            ViewNode::Button { label, action, .. } => {
                assert_eq!(label, "Click");
                assert_eq!(*action, ActionId(1));
            }
            _ => panic!("expected button"),
        }
    }

    #[test]
    fn test_column_component() {
        let node = column(vec![text("a"), text("b")]);
        match &node {
            ViewNode::Container { kind, children, .. } => {
                assert_eq!(*kind, ContainerKind::Column);
                assert_eq!(children.len(), 2);
            }
            _ => panic!("expected container"),
        }
    }

    #[test]
    fn test_row_spaced_component() {
        let node = row_spaced(10.0, vec![text("a")]);
        match &node {
            ViewNode::Container { modifiers, .. } => {
                assert_eq!(modifiers.spacing, Some(10.0));
            }
            _ => panic!("expected container"),
        }
    }

    #[test]
    fn test_toggle_component() {
        let node = toggle(true, BindingId(1));
        match &node {
            ViewNode::Toggle { is_on, .. } => assert!(*is_on),
            _ => panic!("expected toggle"),
        }
    }

    #[test]
    fn test_slider_component() {
        let node = slider(50.0, 0.0, 100.0, BindingId(1));
        match &node {
            ViewNode::Slider { value, min, max, .. } => {
                assert_eq!(*value, 50.0);
                assert_eq!(*min, 0.0);
                assert_eq!(*max, 100.0);
            }
            _ => panic!("expected slider"),
        }
    }

    #[test]
    fn test_spacer_component() {
        let node = spacer();
        assert!(matches!(node, ViewNode::Spacer { .. }));
    }

    #[test]
    fn test_progress_component() {
        let node = progress(0.75);
        match &node {
            ViewNode::Progress { value, .. } => assert_eq!(*value, Some(0.75)),
            _ => panic!("expected progress"),
        }
    }

    #[test]
    fn test_show_if_true() {
        let node = show_if(true, text("visible"));
        match &node {
            ViewNode::Conditional { condition, .. } => assert!(*condition),
            _ => panic!("expected conditional"),
        }
    }

    #[test]
    fn test_modifier_chain() {
        let node = text("styled")
            .padding(EdgeInsets::all(10.0))
            .background(Color::RED)
            .corner_radius(8.0)
            .opacity(0.9);

        match &node {
            ViewNode::Text { modifiers, .. } => {
                assert_eq!(modifiers.padding, Some(EdgeInsets::all(10.0)));
                assert_eq!(modifiers.background, Some(Color::RED));
                assert_eq!(modifiers.corner_radius, Some(8.0));
                assert_eq!(modifiers.opacity, Some(0.9));
            }
            _ => panic!("expected text"),
        }
    }

    #[test]
    fn test_keyed_list() {
        let node = keyed_list(vec![
            (Key::Int(1), text("first")),
            (Key::Int(2), text("second")),
        ]);
        match &node {
            ViewNode::List { items, .. } => {
                assert_eq!(items.len(), 2);
                // Items should have keys set
                match &items[0] {
                    ViewNode::Text { modifiers, .. } => {
                        assert_eq!(modifiers.key, Some(Key::Int(1)));
                    }
                    _ => panic!("expected text"),
                }
            }
            _ => panic!("expected list"),
        }
    }

    #[test]
    fn test_styled_text() {
        let node = styled_text("Big Red", 24.0, Color::RED);
        match &node {
            ViewNode::Text { modifiers, .. } => {
                assert_eq!(modifiers.font.as_ref().unwrap().size, 24.0);
                assert_eq!(modifiers.foreground, Some(Color::RED));
            }
            _ => panic!("expected text"),
        }
    }

    #[test]
    fn test_styled_button() {
        let node = styled_button("OK", ActionId(1), Color::GREEN, Color::WHITE, 12.0);
        match &node {
            ViewNode::Button { modifiers, .. } => {
                assert_eq!(modifiers.background, Some(Color::GREEN));
                assert_eq!(modifiers.foreground, Some(Color::WHITE));
                assert_eq!(modifiers.corner_radius, Some(12.0));
            }
            _ => panic!("expected button"),
        }
    }

    #[test]
    fn test_text_input_component() {
        let node = text_input("", "Type here...", BindingId(1));
        match &node {
            ViewNode::TextInput { placeholder, .. } => {
                assert_eq!(placeholder, "Type here...");
            }
            _ => panic!("expected text input"),
        }
    }

    #[test]
    fn test_image_asset() {
        let node = image_asset("icons/home.png");
        match &node {
            ViewNode::Image { source, .. } => {
                assert_eq!(*source, ImageSource::Asset("icons/home.png".to_string()));
            }
            _ => panic!("expected image"),
        }
    }

    #[test]
    fn test_scroll_component() {
        let node = scroll(vec![text("a"), text("b")]);
        match &node {
            ViewNode::Container { kind, .. } => {
                assert_eq!(*kind, ContainerKind::Scroll);
            }
            _ => panic!("expected scroll"),
        }
    }

    #[test]
    fn test_accessibility_label() {
        let node = button("X", ActionId(1))
            .accessibility("Close button");
        match &node {
            ViewNode::Button { modifiers, .. } => {
                assert_eq!(
                    modifiers.accessibility_label,
                    Some("Close button".to_string())
                );
            }
            _ => panic!("expected button"),
        }
    }

    #[test]
    fn test_counter_app() {
        // Build a counter app UI tree
        let count = 5;
        let tree = column_spaced(12.0, vec![
            styled_text(
                &format!("Count: {}", count),
                24.0,
                Color::BLACK,
            ),
            row_spaced(8.0, vec![
                button("-", ActionId(1))
                    .background(Color::RED)
                    .foreground(Color::WHITE)
                    .corner_radius(8.0),
                button("+", ActionId(2))
                    .background(Color::GREEN)
                    .foreground(Color::WHITE)
                    .corner_radius(8.0),
            ]),
        ]);

        match &tree {
            ViewNode::Container { kind, children, .. } => {
                assert_eq!(*kind, ContainerKind::Column);
                assert_eq!(children.len(), 2);
            }
            _ => panic!("expected column"),
        }
    }

    #[test]
    fn test_todo_app() {
        // Build a todo app UI tree
        let todos = vec!["Buy groceries", "Walk the dog", "Write code"];
        let items: Vec<ViewNode> = todos
            .iter()
            .enumerate()
            .map(|(i, todo)| {
                row_spaced(8.0, vec![
                    toggle(false, BindingId(i as u64)),
                    text(*todo),
                    spacer(),
                    button("X", ActionId(100 + i as u64))
                        .background(Color::RED)
                        .foreground(Color::WHITE),
                ])
            })
            .collect();

        let tree = column_spaced(8.0, vec![
            styled_text("Todo List", 28.0, Color::BLACK),
            text_input("", "Add a todo...", BindingId(99)),
            list(items),
        ]);

        match &tree {
            ViewNode::Container { children, .. } => {
                assert_eq!(children.len(), 3);
            }
            _ => panic!("expected column"),
        }
    }
}
