//! Virtual View Tree — the core data structure for Adam UI.
//!
//! Views produce `ViewNode` trees. The diffing algorithm compares old and new
//! trees to produce a minimal set of `Mutation`s for the rendering backend.

use crate::state::ViewId;

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

/// Unique identifier for actions (button taps, gesture callbacks).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ActionId(pub u64);

/// Unique identifier for bindings (two-way data flow).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BindingId(pub u64);

/// A key for list item identity (enables efficient reorder diffing).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Key {
    Int(i64),
    Str(String),
}

// ---------------------------------------------------------------------------
// Color and styling types
// ---------------------------------------------------------------------------

/// RGBA color (0-255 per channel).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
}

impl Color {
    pub const fn rgba(r: u8, g: u8, b: u8, a: u8) -> Self {
        Self { r, g, b, a }
    }
    pub const fn rgb(r: u8, g: u8, b: u8) -> Self {
        Self { r, g, b, a: 255 }
    }
    pub const BLACK: Color = Color::rgb(0, 0, 0);
    pub const WHITE: Color = Color::rgb(255, 255, 255);
    pub const RED: Color = Color::rgb(255, 0, 0);
    pub const GREEN: Color = Color::rgb(0, 255, 0);
    pub const BLUE: Color = Color::rgb(0, 0, 255);
    pub const TRANSPARENT: Color = Color::rgba(0, 0, 0, 0);
}

impl Default for Color {
    fn default() -> Self {
        Color::BLACK
    }
}

/// Edge insets (padding/margin).
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct EdgeInsets {
    pub top: f32,
    pub right: f32,
    pub bottom: f32,
    pub left: f32,
}

impl EdgeInsets {
    pub const fn all(v: f32) -> Self {
        Self {
            top: v,
            right: v,
            bottom: v,
            left: v,
        }
    }
    pub const fn symmetric(horizontal: f32, vertical: f32) -> Self {
        Self {
            top: vertical,
            right: horizontal,
            bottom: vertical,
            left: horizontal,
        }
    }
    pub const ZERO: EdgeInsets = EdgeInsets::all(0.0);
}

/// Size constraint.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Size {
    pub width: Dimension,
    pub height: Dimension,
}

/// A single dimension value.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Dimension {
    /// Fixed size in logical pixels.
    Fixed(f32),
    /// Fill available space.
    Fill,
    /// Wrap content (minimum size).
    Wrap,
    /// Proportional flex weight.
    Flex(f32),
}

impl Default for Size {
    fn default() -> Self {
        Self {
            width: Dimension::Wrap,
            height: Dimension::Wrap,
        }
    }
}

/// Alignment within a container.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Alignment {
    Start,
    Center,
    End,
    Stretch,
}

impl Default for Alignment {
    fn default() -> Self {
        Alignment::Start
    }
}

/// Font specification.
#[derive(Debug, Clone, PartialEq)]
pub struct Font {
    pub family: String,
    pub size: f32,
    pub weight: FontWeight,
    pub italic: bool,
}

impl Default for Font {
    fn default() -> Self {
        Self {
            family: String::from("system"),
            size: 16.0,
            weight: FontWeight::Regular,
            italic: false,
        }
    }
}

/// Font weight.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FontWeight {
    Thin,
    Light,
    Regular,
    Medium,
    SemiBold,
    Bold,
    Black,
}

/// Border specification.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Border {
    pub width: f32,
    pub color: Color,
}

/// Shadow specification.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Shadow {
    pub color: Color,
    pub offset_x: f32,
    pub offset_y: f32,
    pub blur_radius: f32,
}

/// Image source.
#[derive(Debug, Clone, PartialEq)]
pub enum ImageSource {
    Asset(String),
    Url(String),
    Data(Vec<u8>),
}

// ---------------------------------------------------------------------------
// Modifiers
// ---------------------------------------------------------------------------

/// Visual modifiers that can be applied to any view node.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Modifiers {
    pub padding: Option<EdgeInsets>,
    pub margin: Option<EdgeInsets>,
    pub background: Option<Color>,
    pub foreground: Option<Color>,
    pub font: Option<Font>,
    pub corner_radius: Option<f32>,
    pub border: Option<Border>,
    pub shadow: Option<Shadow>,
    pub opacity: Option<f32>,
    pub size: Option<Size>,
    pub alignment: Option<Alignment>,
    pub on_tap: Option<ActionId>,
    pub flex: Option<f32>,
    pub spacing: Option<f32>,
    pub key: Option<Key>,
    pub accessibility_label: Option<String>,
}

// ---------------------------------------------------------------------------
// ViewNode
// ---------------------------------------------------------------------------

/// A node in the virtual view tree.
#[derive(Debug, Clone, PartialEq)]
pub enum ViewNode {
    /// Text display.
    Text {
        content: String,
        modifiers: Modifiers,
    },
    /// Image display.
    Image {
        source: ImageSource,
        modifiers: Modifiers,
    },
    /// Container with children (Column, Row, Stack, Scroll).
    Container {
        kind: ContainerKind,
        children: Vec<ViewNode>,
        modifiers: Modifiers,
    },
    /// Tappable button.
    Button {
        label: String,
        action: ActionId,
        modifiers: Modifiers,
    },
    /// Text input field.
    TextInput {
        value: String,
        binding: BindingId,
        placeholder: String,
        modifiers: Modifiers,
    },
    /// Toggle/switch.
    Toggle {
        is_on: bool,
        binding: BindingId,
        modifiers: Modifiers,
    },
    /// Slider control.
    Slider {
        value: f64,
        min: f64,
        max: f64,
        binding: BindingId,
        modifiers: Modifiers,
    },
    /// List of items.
    List {
        items: Vec<ViewNode>,
        modifiers: Modifiers,
    },
    /// Custom child view (references another View by ID).
    Custom {
        view_id: ViewId,
        modifiers: Modifiers,
    },
    /// Conditional child (renders child only if condition is true).
    Conditional {
        condition: bool,
        child: Box<ViewNode>,
    },
    /// Progress indicator.
    Progress {
        value: Option<f64>,
        modifiers: Modifiers,
    },
    /// Flexible spacer.
    Spacer { modifiers: Modifiers },
    /// Empty node (placeholder / removed).
    Empty,
}

/// Container layout direction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ContainerKind {
    /// Vertical stack.
    Column,
    /// Horizontal stack.
    Row,
    /// Z-axis stack (overlay).
    Stack,
    /// Scrollable container.
    Scroll,
}

// ---------------------------------------------------------------------------
// Mutations — output of diffing
// ---------------------------------------------------------------------------

/// A path to a node in the tree (list of child indices).
pub type NodePath = Vec<usize>;

/// A mutation to apply to the rendered view tree.
#[derive(Debug, Clone, PartialEq)]
pub enum Mutation {
    /// Insert a new node at a path.
    Insert {
        path: NodePath,
        index: usize,
        node: ViewNode,
    },
    /// Remove a node at a path.
    Remove { path: NodePath, index: usize },
    /// Replace a node with a different one.
    Replace { path: NodePath, node: ViewNode },
    /// Update text content.
    UpdateText { path: NodePath, new_content: String },
    /// Update modifiers.
    UpdateModifiers {
        path: NodePath,
        new_modifiers: Modifiers,
    },
    /// Move a child node (for keyed list reordering).
    Move {
        path: NodePath,
        from_index: usize,
        to_index: usize,
    },
}

// ---------------------------------------------------------------------------
// Diffing algorithm
// ---------------------------------------------------------------------------

/// Diff two view trees and produce a minimal set of mutations.
///
/// Uses an O(n) reconciliation algorithm similar to React:
/// 1. Same type → update properties, recurse into children
/// 2. Different type → remove old, insert new
/// 3. Lists → keyed diffing for efficient reorder
pub fn diff(old: &ViewNode, new: &ViewNode) -> Vec<Mutation> {
    let mut mutations = Vec::new();
    diff_node(old, new, &mut Vec::new(), &mut mutations);
    mutations
}

fn diff_node(old: &ViewNode, new: &ViewNode, path: &mut NodePath, mutations: &mut Vec<Mutation>) {
    // If nodes are identical, nothing to do
    if old == new {
        return;
    }

    match (old, new) {
        // Same type: Text
        (
            ViewNode::Text {
                content: old_content,
                modifiers: old_mods,
            },
            ViewNode::Text {
                content: new_content,
                modifiers: new_mods,
            },
        ) => {
            if old_content != new_content {
                mutations.push(Mutation::UpdateText {
                    path: path.clone(),
                    new_content: new_content.clone(),
                });
            }
            if old_mods != new_mods {
                mutations.push(Mutation::UpdateModifiers {
                    path: path.clone(),
                    new_modifiers: new_mods.clone(),
                });
            }
        }

        // Same type: Container (same kind)
        (
            ViewNode::Container {
                kind: old_kind,
                children: old_children,
                modifiers: old_mods,
            },
            ViewNode::Container {
                kind: new_kind,
                children: new_children,
                modifiers: new_mods,
            },
        ) if old_kind == new_kind => {
            if old_mods != new_mods {
                mutations.push(Mutation::UpdateModifiers {
                    path: path.clone(),
                    new_modifiers: new_mods.clone(),
                });
            }
            diff_children(old_children, new_children, path, mutations);
        }

        // Same type: Button
        (
            ViewNode::Button {
                label: old_label,
                action: old_act,
                modifiers: old_mods,
            },
            ViewNode::Button {
                label: new_label,
                action: new_act,
                modifiers: new_mods,
            },
        ) => {
            if old_label != new_label || old_act != new_act {
                mutations.push(Mutation::UpdateText {
                    path: path.clone(),
                    new_content: new_label.clone(),
                });
            }
            if old_mods != new_mods {
                mutations.push(Mutation::UpdateModifiers {
                    path: path.clone(),
                    new_modifiers: new_mods.clone(),
                });
            }
        }

        // Same type: TextInput
        (
            ViewNode::TextInput {
                value: old_val,
                modifiers: old_mods,
                ..
            },
            ViewNode::TextInput {
                value: new_val,
                modifiers: new_mods,
                ..
            },
        ) => {
            if old_val != new_val {
                mutations.push(Mutation::UpdateText {
                    path: path.clone(),
                    new_content: new_val.clone(),
                });
            }
            if old_mods != new_mods {
                mutations.push(Mutation::UpdateModifiers {
                    path: path.clone(),
                    new_modifiers: new_mods.clone(),
                });
            }
        }

        // Same type: List
        (
            ViewNode::List {
                items: old_items,
                modifiers: old_mods,
            },
            ViewNode::List {
                items: new_items,
                modifiers: new_mods,
            },
        ) => {
            if old_mods != new_mods {
                mutations.push(Mutation::UpdateModifiers {
                    path: path.clone(),
                    new_modifiers: new_mods.clone(),
                });
            }
            diff_keyed_children(old_items, new_items, path, mutations);
        }

        // Same type: Conditional
        (
            ViewNode::Conditional {
                condition: old_cond,
                child: old_child,
            },
            ViewNode::Conditional {
                condition: new_cond,
                child: new_child,
            },
        ) => {
            match (*old_cond, *new_cond) {
                (true, true) => {
                    // Both visible, diff children
                    diff_node(old_child, new_child, path, mutations);
                }
                (false, true) => {
                    // Was hidden, now visible — insert
                    mutations.push(Mutation::Insert {
                        path: path.clone(),
                        index: 0,
                        node: *new_child.clone(),
                    });
                }
                (true, false) => {
                    // Was visible, now hidden — remove
                    mutations.push(Mutation::Remove {
                        path: path.clone(),
                        index: 0,
                    });
                }
                (false, false) => {
                    // Both hidden, nothing to do
                }
            }
        }

        // Same type: Progress
        (
            ViewNode::Progress {
                value: old_val,
                modifiers: old_mods,
            },
            ViewNode::Progress {
                value: new_val,
                modifiers: new_mods,
            },
        ) => {
            if old_val != new_val || old_mods != new_mods {
                mutations.push(Mutation::Replace {
                    path: path.clone(),
                    node: new.clone(),
                });
            }
        }

        // Same type: Toggle
        (
            ViewNode::Toggle {
                is_on: old_on,
                modifiers: old_mods,
                ..
            },
            ViewNode::Toggle {
                is_on: new_on,
                modifiers: new_mods,
                ..
            },
        ) => {
            if old_on != new_on || old_mods != new_mods {
                mutations.push(Mutation::Replace {
                    path: path.clone(),
                    node: new.clone(),
                });
            }
        }

        // Same type: Slider
        (
            ViewNode::Slider {
                value: old_val,
                modifiers: old_mods,
                ..
            },
            ViewNode::Slider {
                value: new_val,
                modifiers: new_mods,
                ..
            },
        ) => {
            if old_val != new_val || old_mods != new_mods {
                mutations.push(Mutation::Replace {
                    path: path.clone(),
                    node: new.clone(),
                });
            }
        }

        // Empty to Empty — no change
        (ViewNode::Empty, ViewNode::Empty) => {}

        // Different types — replace entirely
        _ => {
            mutations.push(Mutation::Replace {
                path: path.clone(),
                node: new.clone(),
            });
        }
    }
}

/// Diff unkeyed children (for Container nodes).
fn diff_children(
    old: &[ViewNode],
    new: &[ViewNode],
    path: &mut NodePath,
    mutations: &mut Vec<Mutation>,
) {
    let min_len = old.len().min(new.len());

    // Diff common prefix
    for i in 0..min_len {
        path.push(i);
        diff_node(&old[i], &new[i], path, mutations);
        path.pop();
    }

    // Handle removals (old has more children)
    for i in (min_len..old.len()).rev() {
        mutations.push(Mutation::Remove {
            path: path.clone(),
            index: i,
        });
    }

    // Handle insertions (new has more children)
    for i in min_len..new.len() {
        mutations.push(Mutation::Insert {
            path: path.clone(),
            index: i,
            node: new[i].clone(),
        });
    }
}

/// Diff keyed children (for List nodes). Uses keys for efficient reordering.
fn diff_keyed_children(
    old: &[ViewNode],
    new: &[ViewNode],
    path: &mut NodePath,
    mutations: &mut Vec<Mutation>,
) {
    // Extract keys
    let old_keys: Vec<Option<&Key>> = old.iter().map(|n| node_key(n)).collect();
    let new_keys: Vec<Option<&Key>> = new.iter().map(|n| node_key(n)).collect();

    // If no keys, fall back to positional diffing
    if old_keys.iter().all(|k| k.is_none()) && new_keys.iter().all(|k| k.is_none()) {
        diff_children(old, new, path, mutations);
        return;
    }

    // Build old key → index map
    let mut old_key_map = std::collections::HashMap::new();
    for (i, key) in old_keys.iter().enumerate() {
        if let Some(k) = key {
            old_key_map.insert(*k, i);
        }
    }

    // Track which old indices are used
    let mut used_old = vec![false; old.len()];

    // For each new item, find matching old item or insert
    for (new_idx, new_node) in new.iter().enumerate() {
        if let Some(key) = node_key(new_node) {
            if let Some(&old_idx) = old_key_map.get(key) {
                used_old[old_idx] = true;
                // Diff the matching nodes
                path.push(new_idx);
                diff_node(&old[old_idx], new_node, path, mutations);
                path.pop();

                // If position changed, emit a move
                if old_idx != new_idx {
                    mutations.push(Mutation::Move {
                        path: path.clone(),
                        from_index: old_idx,
                        to_index: new_idx,
                    });
                }
            } else {
                // New key — insert
                mutations.push(Mutation::Insert {
                    path: path.clone(),
                    index: new_idx,
                    node: new_node.clone(),
                });
            }
        } else {
            // No key — positional match
            if new_idx < old.len() {
                used_old[new_idx] = true;
                path.push(new_idx);
                diff_node(&old[new_idx], new_node, path, mutations);
                path.pop();
            } else {
                mutations.push(Mutation::Insert {
                    path: path.clone(),
                    index: new_idx,
                    node: new_node.clone(),
                });
            }
        }
    }

    // Remove old items that weren't matched
    for (i, used) in used_old.iter().enumerate().rev() {
        if !used {
            mutations.push(Mutation::Remove {
                path: path.clone(),
                index: i,
            });
        }
    }
}

/// Extract the key from a node's modifiers.
fn node_key(node: &ViewNode) -> Option<&Key> {
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
        | ViewNode::Spacer { modifiers, .. } => modifiers.key.as_ref(),
        ViewNode::Conditional { child, .. } => node_key(child),
        ViewNode::Empty => None,
    }
}

/// Count total nodes in a tree (for benchmarking/testing).
pub fn tree_node_count(node: &ViewNode) -> usize {
    match node {
        ViewNode::Container { children, .. } => {
            1 + children.iter().map(tree_node_count).sum::<usize>()
        }
        ViewNode::List { items, .. } => 1 + items.iter().map(tree_node_count).sum::<usize>(),
        ViewNode::Conditional { child, condition } => {
            if *condition {
                1 + tree_node_count(child)
            } else {
                1
            }
        }
        _ => 1,
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn text(s: &str) -> ViewNode {
        ViewNode::Text {
            content: s.to_string(),
            modifiers: Default::default(),
        }
    }

    fn text_with_key(s: &str, key: Key) -> ViewNode {
        ViewNode::Text {
            content: s.to_string(),
            modifiers: Modifiers {
                key: Some(key),
                ..Default::default()
            },
        }
    }

    fn column(children: Vec<ViewNode>) -> ViewNode {
        ViewNode::Container {
            kind: ContainerKind::Column,
            children,
            modifiers: Default::default(),
        }
    }

    fn button(label: &str, action: u64) -> ViewNode {
        ViewNode::Button {
            label: label.to_string(),
            action: ActionId(action),
            modifiers: Default::default(),
        }
    }

    #[test]
    fn test_diff_identical_trees() {
        let tree = text("hello");
        let mutations = diff(&tree, &tree);
        assert!(
            mutations.is_empty(),
            "identical trees should produce no mutations"
        );
    }

    #[test]
    fn test_diff_text_change() {
        let old = text("hello");
        let new = text("world");
        let mutations = diff(&old, &new);
        assert_eq!(mutations.len(), 1);
        match &mutations[0] {
            Mutation::UpdateText { new_content, .. } => {
                assert_eq!(new_content, "world");
            }
            other => panic!("expected UpdateText, got {:?}", other),
        }
    }

    #[test]
    fn test_diff_add_child() {
        let old = column(vec![text("a")]);
        let new = column(vec![text("a"), text("b")]);
        let mutations = diff(&old, &new);
        assert_eq!(mutations.len(), 1);
        match &mutations[0] {
            Mutation::Insert { index, node, .. } => {
                assert_eq!(*index, 1);
                match node {
                    ViewNode::Text { content, .. } => assert_eq!(content, "b"),
                    _ => panic!("expected text node"),
                }
            }
            other => panic!("expected Insert, got {:?}", other),
        }
    }

    #[test]
    fn test_diff_remove_child() {
        let old = column(vec![text("a"), text("b"), text("c")]);
        let new = column(vec![text("a")]);
        let mutations = diff(&old, &new);
        // Should remove index 2 and 1 (in reverse order)
        assert_eq!(mutations.len(), 2);
        match &mutations[0] {
            Mutation::Remove { index, .. } => assert_eq!(*index, 2),
            other => panic!("expected Remove, got {:?}", other),
        }
        match &mutations[1] {
            Mutation::Remove { index, .. } => assert_eq!(*index, 1),
            other => panic!("expected Remove, got {:?}", other),
        }
    }

    #[test]
    fn test_diff_update_text_in_container() {
        let old = column(vec![text("hello"), text("world")]);
        let new = column(vec![text("hello"), text("earth")]);
        let mutations = diff(&old, &new);
        assert_eq!(mutations.len(), 1);
        match &mutations[0] {
            Mutation::UpdateText { path, new_content } => {
                assert_eq!(path, &[1]);
                assert_eq!(new_content, "earth");
            }
            other => panic!("expected UpdateText, got {:?}", other),
        }
    }

    #[test]
    fn test_diff_type_change() {
        let old = text("hello");
        let new = button("click", 1);
        let mutations = diff(&old, &new);
        assert_eq!(mutations.len(), 1);
        assert!(matches!(&mutations[0], Mutation::Replace { .. }));
    }

    #[test]
    fn test_diff_conditional_show() {
        let old = ViewNode::Conditional {
            condition: false,
            child: Box::new(text("hidden")),
        };
        let new = ViewNode::Conditional {
            condition: true,
            child: Box::new(text("visible")),
        };
        let mutations = diff(&old, &new);
        assert_eq!(mutations.len(), 1);
        assert!(matches!(&mutations[0], Mutation::Insert { .. }));
    }

    #[test]
    fn test_diff_conditional_hide() {
        let old = ViewNode::Conditional {
            condition: true,
            child: Box::new(text("visible")),
        };
        let new = ViewNode::Conditional {
            condition: false,
            child: Box::new(text("hidden")),
        };
        let mutations = diff(&old, &new);
        assert_eq!(mutations.len(), 1);
        assert!(matches!(&mutations[0], Mutation::Remove { .. }));
    }

    #[test]
    fn test_diff_keyed_reorder() {
        let old = ViewNode::List {
            items: vec![
                text_with_key("a", Key::Int(1)),
                text_with_key("b", Key::Int(2)),
                text_with_key("c", Key::Int(3)),
            ],
            modifiers: Default::default(),
        };
        let new = ViewNode::List {
            items: vec![
                text_with_key("c", Key::Int(3)),
                text_with_key("a", Key::Int(1)),
                text_with_key("b", Key::Int(2)),
            ],
            modifiers: Default::default(),
        };
        let mutations = diff(&old, &new);
        // Should have Move mutations
        let moves: Vec<_> = mutations
            .iter()
            .filter(|m| matches!(m, Mutation::Move { .. }))
            .collect();
        assert!(
            !moves.is_empty(),
            "keyed reorder should produce Move mutations"
        );
    }

    #[test]
    fn test_diff_keyed_insert() {
        let old = ViewNode::List {
            items: vec![
                text_with_key("a", Key::Int(1)),
                text_with_key("b", Key::Int(2)),
            ],
            modifiers: Default::default(),
        };
        let new = ViewNode::List {
            items: vec![
                text_with_key("a", Key::Int(1)),
                text_with_key("x", Key::Int(99)),
                text_with_key("b", Key::Int(2)),
            ],
            modifiers: Default::default(),
        };
        let mutations = diff(&old, &new);
        let inserts: Vec<_> = mutations
            .iter()
            .filter(|m| matches!(m, Mutation::Insert { .. }))
            .collect();
        assert!(
            !inserts.is_empty(),
            "keyed insert should produce Insert mutations"
        );
    }

    #[test]
    fn test_diff_modifiers_change() {
        let old = ViewNode::Text {
            content: "hello".to_string(),
            modifiers: Modifiers {
                background: Some(Color::RED),
                ..Default::default()
            },
        };
        let new = ViewNode::Text {
            content: "hello".to_string(),
            modifiers: Modifiers {
                background: Some(Color::BLUE),
                ..Default::default()
            },
        };
        let mutations = diff(&old, &new);
        assert_eq!(mutations.len(), 1);
        assert!(matches!(&mutations[0], Mutation::UpdateModifiers { .. }));
    }

    #[test]
    fn test_diff_empty_to_empty() {
        let mutations = diff(&ViewNode::Empty, &ViewNode::Empty);
        assert!(mutations.is_empty());
    }

    #[test]
    fn test_tree_node_count() {
        let tree = column(vec![
            text("a"),
            text("b"),
            column(vec![text("c"), text("d")]),
        ]);
        assert_eq!(tree_node_count(&tree), 6);
    }

    #[test]
    fn test_diff_large_tree_performance() {
        // Build a 1000-node tree
        let items: Vec<ViewNode> = (0..1000).map(|i| text(&format!("item {}", i))).collect();
        let old = column(items);

        let items_new: Vec<ViewNode> = (0..1000)
            .map(|i| {
                if i == 500 {
                    text("changed")
                } else {
                    text(&format!("item {}", i))
                }
            })
            .collect();
        let new = column(items_new);

        let start = std::time::Instant::now();
        let mutations = diff(&old, &new);
        let elapsed = start.elapsed();

        // Only one node changed
        assert_eq!(mutations.len(), 1);
        // Should complete in < 10ms (well within 1ms on modern hardware)
        assert!(elapsed.as_millis() < 10, "diff took {:?}", elapsed);
    }

    #[test]
    fn test_diff_nested_containers() {
        let old = column(vec![ViewNode::Container {
            kind: ContainerKind::Row,
            children: vec![text("a"), text("b")],
            modifiers: Default::default(),
        }]);
        let new = column(vec![ViewNode::Container {
            kind: ContainerKind::Row,
            children: vec![text("a"), text("c")],
            modifiers: Default::default(),
        }]);
        let mutations = diff(&old, &new);
        assert_eq!(mutations.len(), 1);
        match &mutations[0] {
            Mutation::UpdateText { path, new_content } => {
                assert_eq!(path, &[0, 1]); // depth 2
                assert_eq!(new_content, "c");
            }
            other => panic!("expected UpdateText, got {:?}", other),
        }
    }

    #[test]
    fn test_color_constants() {
        assert_eq!(Color::BLACK, Color::rgb(0, 0, 0));
        assert_eq!(Color::WHITE, Color::rgb(255, 255, 255));
        assert_eq!(Color::TRANSPARENT.a, 0);
    }

    #[test]
    fn test_edge_insets() {
        let insets = EdgeInsets::all(10.0);
        assert_eq!(insets.top, 10.0);
        assert_eq!(insets.right, 10.0);
        assert_eq!(insets.bottom, 10.0);
        assert_eq!(insets.left, 10.0);

        let sym = EdgeInsets::symmetric(5.0, 10.0);
        assert_eq!(sym.left, 5.0);
        assert_eq!(sym.right, 5.0);
        assert_eq!(sym.top, 10.0);
        assert_eq!(sym.bottom, 10.0);
    }
}
