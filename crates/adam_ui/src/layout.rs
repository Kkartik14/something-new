//! Flexbox-style layout engine for Adam UI.
//!
//! Two-pass layout algorithm:
//! 1. **Measure pass (bottom-up):** each node reports its desired size
//! 2. **Layout pass (top-down):** parent assigns final position and size

use crate::vtree::*;

// ---------------------------------------------------------------------------
// Layout result types
// ---------------------------------------------------------------------------

/// The computed layout for a single node.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct LayoutRect {
    pub x: f32,
    pub y: f32,
    pub width: f32,
    pub height: f32,
}

impl LayoutRect {
    pub const ZERO: LayoutRect = LayoutRect {
        x: 0.0, y: 0.0, width: 0.0, height: 0.0,
    };

    pub fn contains_point(&self, px: f32, py: f32) -> bool {
        px >= self.x && px < self.x + self.width
            && py >= self.y && py < self.y + self.height
    }
}

impl Default for LayoutRect {
    fn default() -> Self {
        Self::ZERO
    }
}

/// Constraints passed from parent to child during layout.
#[derive(Debug, Clone, Copy)]
pub struct Constraints {
    pub min_width: f32,
    pub max_width: f32,
    pub min_height: f32,
    pub max_height: f32,
}

impl Constraints {
    pub fn tight(width: f32, height: f32) -> Self {
        Self {
            min_width: width,
            max_width: width,
            min_height: height,
            max_height: height,
        }
    }

    pub fn loose(max_width: f32, max_height: f32) -> Self {
        Self {
            min_width: 0.0,
            max_width,
            min_height: 0.0,
            max_height,
        }
    }

    pub fn unbounded() -> Self {
        Self {
            min_width: 0.0,
            max_width: f32::INFINITY,
            min_height: 0.0,
            max_height: f32::INFINITY,
        }
    }

    pub fn constrain_width(&self, width: f32) -> f32 {
        width.max(self.min_width).min(self.max_width)
    }

    pub fn constrain_height(&self, height: f32) -> f32 {
        height.max(self.min_height).min(self.max_height)
    }
}

/// A laid-out tree: parallel structure to the ViewNode tree.
#[derive(Debug, Clone)]
pub struct LayoutNode {
    pub rect: LayoutRect,
    pub children: Vec<LayoutNode>,
}

impl LayoutNode {
    pub fn leaf(rect: LayoutRect) -> Self {
        Self { rect, children: Vec::new() }
    }

    pub fn with_children(rect: LayoutRect, children: Vec<LayoutNode>) -> Self {
        Self { rect, children }
    }
}

// ---------------------------------------------------------------------------
// Text measurement callback
// ---------------------------------------------------------------------------

/// Callback for measuring text size. The layout engine doesn't know about
/// fonts — the rendering backend provides this.
pub trait TextMeasure {
    fn measure_text(&self, text: &str, font: &Font, max_width: f32) -> (f32, f32);
}

/// Default text measure: estimates based on character count.
/// Used for testing without a real font renderer.
pub struct EstimatedTextMeasure {
    pub char_width: f32,
    pub line_height: f32,
}

impl Default for EstimatedTextMeasure {
    fn default() -> Self {
        Self {
            char_width: 8.0,
            line_height: 20.0,
        }
    }
}

impl TextMeasure for EstimatedTextMeasure {
    fn measure_text(&self, text: &str, font: &Font, max_width: f32) -> (f32, f32) {
        let scale = font.size / 16.0;
        let cw = self.char_width * scale;
        let lh = self.line_height * scale;

        let total_width = text.len() as f32 * cw;
        if total_width <= max_width || max_width == f32::INFINITY {
            (total_width, lh)
        } else {
            // Wrap text
            let chars_per_line = (max_width / cw).floor().max(1.0) as usize;
            let lines = (text.len() + chars_per_line - 1) / chars_per_line;
            (max_width, lines as f32 * lh)
        }
    }
}

// ---------------------------------------------------------------------------
// Layout engine
// ---------------------------------------------------------------------------

/// Perform layout on a view tree within given constraints.
pub fn layout(node: &ViewNode, constraints: Constraints, text_measure: &dyn TextMeasure) -> LayoutNode {
    let (width, height, children) = measure_and_layout(node, constraints, text_measure);
    LayoutNode {
        rect: LayoutRect { x: 0.0, y: 0.0, width, height },
        children,
    }
}

fn measure_and_layout(
    node: &ViewNode,
    constraints: Constraints,
    tm: &dyn TextMeasure,
) -> (f32, f32, Vec<LayoutNode>) {
    match node {
        ViewNode::Text { content, modifiers } => {
            let pad = modifiers.padding.unwrap_or(EdgeInsets::ZERO);
            let font = modifiers.font.clone().unwrap_or_default();
            let inner_max_w = (constraints.max_width - pad.left - pad.right).max(0.0);
            let (tw, th) = tm.measure_text(content, &font, inner_max_w);
            let w = constraints.constrain_width(tw + pad.left + pad.right);
            let h = constraints.constrain_height(th + pad.top + pad.bottom);
            apply_size_override(w, h, modifiers, constraints)
        }

        ViewNode::Button { label, modifiers, .. } => {
            let pad = modifiers.padding.unwrap_or(EdgeInsets::symmetric(16.0, 8.0));
            let font = modifiers.font.clone().unwrap_or_default();
            let inner_max_w = (constraints.max_width - pad.left - pad.right).max(0.0);
            let (tw, th) = tm.measure_text(label, &font, inner_max_w);
            let w = constraints.constrain_width(tw + pad.left + pad.right);
            let h = constraints.constrain_height(th + pad.top + pad.bottom);
            apply_size_override(w, h, modifiers, constraints)
        }

        ViewNode::TextInput { placeholder, modifiers, .. } => {
            let pad = modifiers.padding.unwrap_or(EdgeInsets::symmetric(12.0, 8.0));
            let font = modifiers.font.clone().unwrap_or_default();
            let (tw, th) = tm.measure_text(placeholder, &font, constraints.max_width);
            let w = constraints.constrain_width((tw + pad.left + pad.right).max(200.0));
            let h = constraints.constrain_height(th + pad.top + pad.bottom);
            apply_size_override(w, h, modifiers, constraints)
        }

        ViewNode::Toggle { modifiers, .. } => {
            let w = constraints.constrain_width(51.0); // standard toggle width
            let h = constraints.constrain_height(31.0);
            apply_size_override(w, h, modifiers, constraints)
        }

        ViewNode::Slider { modifiers, .. } => {
            let w = constraints.constrain_width(200.0);
            let h = constraints.constrain_height(30.0);
            apply_size_override(w, h, modifiers, constraints)
        }

        ViewNode::Image { modifiers, .. } => {
            let w = constraints.constrain_width(100.0);
            let h = constraints.constrain_height(100.0);
            apply_size_override(w, h, modifiers, constraints)
        }

        ViewNode::Progress { modifiers, .. } => {
            let w = constraints.constrain_width(200.0);
            let h = constraints.constrain_height(4.0);
            apply_size_override(w, h, modifiers, constraints)
        }

        ViewNode::Spacer { modifiers } => {
            let flex = modifiers.flex.unwrap_or(1.0);
            let w = if constraints.max_width.is_finite() {
                constraints.max_width * flex
            } else {
                0.0
            };
            let h = if constraints.max_height.is_finite() {
                constraints.max_height * flex
            } else {
                0.0
            };
            apply_size_override(w, h, modifiers, constraints)
        }

        ViewNode::Container { kind, children, modifiers } => {
            layout_container(*kind, children, modifiers, constraints, tm)
        }

        ViewNode::List { items, modifiers } => {
            // List is like a Column with scrolling
            layout_container(ContainerKind::Column, items, modifiers, constraints, tm)
        }

        ViewNode::Conditional { condition, child } => {
            if *condition {
                measure_and_layout(child, constraints, tm)
            } else {
                (0.0, 0.0, Vec::new())
            }
        }

        ViewNode::Custom { modifiers, .. } => {
            let w = constraints.constrain_width(0.0);
            let h = constraints.constrain_height(0.0);
            apply_size_override(w, h, modifiers, constraints)
        }

        ViewNode::Empty => (0.0, 0.0, Vec::new()),
    }
}

fn apply_size_override(
    default_w: f32,
    default_h: f32,
    modifiers: &Modifiers,
    constraints: Constraints,
) -> (f32, f32, Vec<LayoutNode>) {
    let (w, h) = if let Some(size) = &modifiers.size {
        let w = match size.width {
            Dimension::Fixed(v) => constraints.constrain_width(v),
            Dimension::Fill => constraints.max_width.min(f32::MAX),
            Dimension::Wrap => default_w,
            Dimension::Flex(_) => default_w,
        };
        let h = match size.height {
            Dimension::Fixed(v) => constraints.constrain_height(v),
            Dimension::Fill => constraints.max_height.min(f32::MAX),
            Dimension::Wrap => default_h,
            Dimension::Flex(_) => default_h,
        };
        (w, h)
    } else {
        (default_w, default_h)
    };

    // Apply margin
    let margin = modifiers.margin.unwrap_or(EdgeInsets::ZERO);
    (w + margin.left + margin.right, h + margin.top + margin.bottom, Vec::new())
}

fn layout_container(
    kind: ContainerKind,
    children: &[ViewNode],
    modifiers: &Modifiers,
    constraints: Constraints,
    tm: &dyn TextMeasure,
) -> (f32, f32, Vec<LayoutNode>) {
    let pad = modifiers.padding.unwrap_or(EdgeInsets::ZERO);
    let spacing = modifiers.spacing.unwrap_or(0.0);
    let margin = modifiers.margin.unwrap_or(EdgeInsets::ZERO);

    let inner_max_w = (constraints.max_width - pad.left - pad.right - margin.left - margin.right).max(0.0);
    let inner_max_h = (constraints.max_height - pad.top - pad.bottom - margin.top - margin.bottom).max(0.0);

    match kind {
        ContainerKind::Column => {
            layout_column(children, inner_max_w, inner_max_h, spacing, pad, margin, constraints, tm)
        }
        ContainerKind::Row => {
            layout_row(children, inner_max_w, inner_max_h, spacing, pad, margin, constraints, tm)
        }
        ContainerKind::Stack => {
            layout_stack(children, inner_max_w, inner_max_h, pad, margin, constraints, tm)
        }
        ContainerKind::Scroll => {
            // Scroll: measure children without height constraint
            layout_column(children, inner_max_w, f32::INFINITY, spacing, pad, margin, constraints, tm)
        }
    }
}

fn layout_column(
    children: &[ViewNode],
    max_w: f32,
    max_h: f32,
    spacing: f32,
    pad: EdgeInsets,
    margin: EdgeInsets,
    constraints: Constraints,
    tm: &dyn TextMeasure,
) -> (f32, f32, Vec<LayoutNode>) {
    // First pass: measure all non-flex children to determine remaining space
    let child_constraints = Constraints::loose(max_w, f32::INFINITY);

    let mut measured: Vec<(f32, f32, Vec<LayoutNode>)> = Vec::new();
    let mut total_fixed_height = 0.0f32;
    let mut total_flex = 0.0f32;
    let mut max_child_width = 0.0f32;

    for child in children {
        let flex = child_flex(child);
        if flex > 0.0 {
            measured.push((0.0, 0.0, Vec::new()));
            total_flex += flex;
        } else {
            let (w, h, sub) = measure_and_layout(child, child_constraints, tm);
            total_fixed_height += h;
            max_child_width = max_child_width.max(w);
            measured.push((w, h, sub));
        }
    }

    let total_spacing = if children.len() > 1 {
        spacing * (children.len() - 1) as f32
    } else {
        0.0
    };

    let remaining_height = (max_h - total_fixed_height - total_spacing).max(0.0);

    // Second pass: assign flex sizes and positions
    let mut layout_children = Vec::new();
    let mut y_offset = pad.top + margin.top;

    for (i, child) in children.iter().enumerate() {
        let flex = child_flex(child);
        let (w, h, sub_children) = if flex > 0.0 {
            let flex_h = if total_flex > 0.0 {
                remaining_height * (flex / total_flex)
            } else {
                0.0
            };
            let flex_constraints = Constraints {
                min_width: 0.0,
                max_width: max_w,
                min_height: flex_h,
                max_height: flex_h,
            };
            measure_and_layout(child, flex_constraints, tm)
        } else {
            measured[i].clone()
        };

        let x_offset = pad.left + margin.left;
        // Apply alignment within the column
        let child_x = match child_alignment(child).unwrap_or(Alignment::Start) {
            Alignment::Start => x_offset,
            Alignment::Center => x_offset + (max_w - w).max(0.0) / 2.0,
            Alignment::End => x_offset + (max_w - w).max(0.0),
            Alignment::Stretch => x_offset,
        };

        let child_w = if matches!(child_alignment(child), Some(Alignment::Stretch)) {
            max_w
        } else {
            w
        };

        layout_children.push(LayoutNode {
            rect: LayoutRect {
                x: child_x,
                y: y_offset,
                width: child_w,
                height: h,
            },
            children: sub_children,
        });

        y_offset += h + spacing;
        max_child_width = max_child_width.max(child_w);
    }

    let total_h = y_offset - spacing + pad.bottom + margin.bottom;
    let total_w = max_child_width + pad.left + pad.right + margin.left + margin.right;

    let final_w = if let Some(size) = &modifiers_of_children_parent(children).and_then(|m| m.size.as_ref()) {
        match size.width {
            Dimension::Fill => constraints.max_width.min(f32::MAX),
            Dimension::Fixed(v) => v,
            _ => constraints.constrain_width(total_w),
        }
    } else {
        constraints.constrain_width(total_w)
    };

    (final_w, constraints.constrain_height(total_h.max(0.0)), layout_children)
}

fn layout_row(
    children: &[ViewNode],
    max_w: f32,
    max_h: f32,
    spacing: f32,
    pad: EdgeInsets,
    margin: EdgeInsets,
    constraints: Constraints,
    tm: &dyn TextMeasure,
) -> (f32, f32, Vec<LayoutNode>) {
    let child_constraints = Constraints::loose(f32::INFINITY, max_h);

    let mut measured: Vec<(f32, f32, Vec<LayoutNode>)> = Vec::new();
    let mut total_fixed_width = 0.0f32;
    let mut total_flex = 0.0f32;
    let mut max_child_height = 0.0f32;

    for child in children {
        let flex = child_flex(child);
        if flex > 0.0 {
            measured.push((0.0, 0.0, Vec::new()));
            total_flex += flex;
        } else {
            let (w, h, sub) = measure_and_layout(child, child_constraints, tm);
            total_fixed_width += w;
            max_child_height = max_child_height.max(h);
            measured.push((w, h, sub));
        }
    }

    let total_spacing = if children.len() > 1 {
        spacing * (children.len() - 1) as f32
    } else {
        0.0
    };

    let remaining_width = (max_w - total_fixed_width - total_spacing).max(0.0);

    let mut layout_children = Vec::new();
    let mut x_offset = pad.left + margin.left;

    for (i, child) in children.iter().enumerate() {
        let flex = child_flex(child);
        let (w, h, sub_children) = if flex > 0.0 {
            let flex_w = if total_flex > 0.0 {
                remaining_width * (flex / total_flex)
            } else {
                0.0
            };
            let flex_constraints = Constraints {
                min_width: flex_w,
                max_width: flex_w,
                min_height: 0.0,
                max_height: max_h,
            };
            measure_and_layout(child, flex_constraints, tm)
        } else {
            measured[i].clone()
        };

        let y_offset = pad.top + margin.top;
        let child_y = match child_alignment(child).unwrap_or(Alignment::Start) {
            Alignment::Start => y_offset,
            Alignment::Center => y_offset + (max_h - h).max(0.0) / 2.0,
            Alignment::End => y_offset + (max_h - h).max(0.0),
            Alignment::Stretch => y_offset,
        };

        let child_h = if matches!(child_alignment(child), Some(Alignment::Stretch)) {
            max_h
        } else {
            h
        };

        layout_children.push(LayoutNode {
            rect: LayoutRect {
                x: x_offset,
                y: child_y,
                width: w,
                height: child_h,
            },
            children: sub_children,
        });

        x_offset += w + spacing;
        max_child_height = max_child_height.max(child_h);
    }

    let total_w = x_offset - spacing + pad.right + margin.right;
    let total_h = max_child_height + pad.top + pad.bottom + margin.top + margin.bottom;

    (constraints.constrain_width(total_w.max(0.0)), constraints.constrain_height(total_h), layout_children)
}

fn layout_stack(
    children: &[ViewNode],
    max_w: f32,
    max_h: f32,
    pad: EdgeInsets,
    margin: EdgeInsets,
    constraints: Constraints,
    tm: &dyn TextMeasure,
) -> (f32, f32, Vec<LayoutNode>) {
    let child_constraints = Constraints::loose(max_w, max_h);

    let mut layout_children = Vec::new();
    let mut max_child_w = 0.0f32;
    let mut max_child_h = 0.0f32;

    for child in children {
        let (w, h, sub) = measure_and_layout(child, child_constraints, tm);
        max_child_w = max_child_w.max(w);
        max_child_h = max_child_h.max(h);

        let x = pad.left + margin.left;
        let y = pad.top + margin.top;

        layout_children.push(LayoutNode {
            rect: LayoutRect { x, y, width: w, height: h },
            children: sub,
        });
    }

    let total_w = max_child_w + pad.left + pad.right + margin.left + margin.right;
    let total_h = max_child_h + pad.top + pad.bottom + margin.top + margin.bottom;

    (constraints.constrain_width(total_w), constraints.constrain_height(total_h), layout_children)
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn child_flex(node: &ViewNode) -> f32 {
    match node {
        ViewNode::Spacer { modifiers } => modifiers.flex.unwrap_or(1.0),
        ViewNode::Text { modifiers, .. }
        | ViewNode::Image { modifiers, .. }
        | ViewNode::Container { modifiers, .. }
        | ViewNode::Button { modifiers, .. }
        | ViewNode::TextInput { modifiers, .. }
        | ViewNode::Toggle { modifiers, .. }
        | ViewNode::Slider { modifiers, .. }
        | ViewNode::List { modifiers, .. }
        | ViewNode::Custom { modifiers, .. }
        | ViewNode::Progress { modifiers, .. } => {
            if let Some(size) = &modifiers.size {
                match size.width {
                    Dimension::Flex(f) => f,
                    _ => match size.height {
                        Dimension::Flex(f) => f,
                        _ => 0.0,
                    },
                }
            } else {
                0.0
            }
        }
        _ => 0.0,
    }
}

fn child_alignment(node: &ViewNode) -> Option<Alignment> {
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
        | ViewNode::Spacer { modifiers } => modifiers.alignment,
        _ => None,
    }
}

fn modifiers_of_children_parent(_children: &[ViewNode]) -> Option<&Modifiers> {
    // This is a placeholder — in practice the container's own modifiers
    // are passed separately. Returns None.
    None
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn tm() -> EstimatedTextMeasure {
        EstimatedTextMeasure::default()
    }

    fn text(s: &str) -> ViewNode {
        ViewNode::Text {
            content: s.to_string(),
            modifiers: Default::default(),
        }
    }

    fn text_padded(s: &str, padding: f32) -> ViewNode {
        ViewNode::Text {
            content: s.to_string(),
            modifiers: Modifiers {
                padding: Some(EdgeInsets::all(padding)),
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

    fn column_spaced(children: Vec<ViewNode>, spacing: f32) -> ViewNode {
        ViewNode::Container {
            kind: ContainerKind::Column,
            children,
            modifiers: Modifiers {
                spacing: Some(spacing),
                ..Default::default()
            },
        }
    }

    fn row(children: Vec<ViewNode>) -> ViewNode {
        ViewNode::Container {
            kind: ContainerKind::Row,
            children,
            modifiers: Default::default(),
        }
    }

    fn stack(children: Vec<ViewNode>) -> ViewNode {
        ViewNode::Container {
            kind: ContainerKind::Stack,
            children,
            modifiers: Default::default(),
        }
    }

    #[test]
    fn test_layout_text() {
        let node = text("hello");
        let result = layout(&node, Constraints::loose(400.0, 800.0), &tm());
        // "hello" = 5 chars * 8px = 40px wide, 20px tall
        assert_eq!(result.rect.x, 0.0);
        assert_eq!(result.rect.y, 0.0);
        assert!((result.rect.width - 40.0).abs() < 0.1);
        assert!((result.rect.height - 20.0).abs() < 0.1);
    }

    #[test]
    fn test_layout_text_with_padding() {
        let node = text_padded("hi", 10.0);
        let result = layout(&node, Constraints::loose(400.0, 800.0), &tm());
        // "hi" = 2*8 = 16px + 20 padding = 36px wide
        // 20px tall + 20 padding = 40px
        assert!((result.rect.width - 36.0).abs() < 0.1);
        assert!((result.rect.height - 40.0).abs() < 0.1);
    }

    #[test]
    fn test_layout_column() {
        let node = column(vec![text("aaa"), text("bb")]);
        let result = layout(&node, Constraints::loose(400.0, 800.0), &tm());

        assert_eq!(result.children.len(), 2);

        // First child at y=0
        assert!((result.children[0].rect.y).abs() < 0.1);
        assert!((result.children[0].rect.height - 20.0).abs() < 0.1);

        // Second child below first
        assert!((result.children[1].rect.y - 20.0).abs() < 0.1);
    }

    #[test]
    fn test_layout_column_with_spacing() {
        let node = column_spaced(vec![text("a"), text("b"), text("c")], 10.0);
        let result = layout(&node, Constraints::loose(400.0, 800.0), &tm());

        assert_eq!(result.children.len(), 3);
        // child 0: y=0, h=20
        // child 1: y=30 (20 + 10 spacing)
        // child 2: y=60 (30 + 20 + 10 spacing)
        assert!((result.children[0].rect.y).abs() < 0.1);
        assert!((result.children[1].rect.y - 30.0).abs() < 0.1);
        assert!((result.children[2].rect.y - 60.0).abs() < 0.1);
    }

    #[test]
    fn test_layout_row() {
        let node = row(vec![text("aaa"), text("bb")]);
        let result = layout(&node, Constraints::loose(400.0, 800.0), &tm());

        assert_eq!(result.children.len(), 2);

        // First child at x=0
        assert!((result.children[0].rect.x).abs() < 0.1);
        // "aaa" = 24px wide
        assert!((result.children[0].rect.width - 24.0).abs() < 0.1);

        // Second child after first
        assert!((result.children[1].rect.x - 24.0).abs() < 0.1);
    }

    #[test]
    fn test_layout_stack() {
        let node = stack(vec![text("back"), text("front")]);
        let result = layout(&node, Constraints::loose(400.0, 800.0), &tm());

        assert_eq!(result.children.len(), 2);
        // Both at same position
        assert_eq!(result.children[0].rect.x, result.children[1].rect.x);
        assert_eq!(result.children[0].rect.y, result.children[1].rect.y);
    }

    #[test]
    fn test_layout_nested() {
        // Column with a Row inside
        let node = column(vec![
            text("header"),
            row(vec![text("a"), text("b")]),
            text("footer"),
        ]);
        let result = layout(&node, Constraints::loose(400.0, 800.0), &tm());

        assert_eq!(result.children.len(), 3);
        // Header at y=0
        assert!((result.children[0].rect.y).abs() < 0.1);
        // Row below header
        assert!((result.children[1].rect.y - 20.0).abs() < 0.1);
        // Row should have 2 children
        assert_eq!(result.children[1].children.len(), 2);
    }

    #[test]
    fn test_layout_empty() {
        let result = layout(&ViewNode::Empty, Constraints::loose(400.0, 800.0), &tm());
        assert_eq!(result.rect.width, 0.0);
        assert_eq!(result.rect.height, 0.0);
    }

    #[test]
    fn test_layout_conditional_visible() {
        let node = ViewNode::Conditional {
            condition: true,
            child: Box::new(text("visible")),
        };
        let result = layout(&node, Constraints::loose(400.0, 800.0), &tm());
        assert!(result.rect.width > 0.0);
        assert!(result.rect.height > 0.0);
    }

    #[test]
    fn test_layout_conditional_hidden() {
        let node = ViewNode::Conditional {
            condition: false,
            child: Box::new(text("hidden")),
        };
        let result = layout(&node, Constraints::loose(400.0, 800.0), &tm());
        assert_eq!(result.rect.width, 0.0);
        assert_eq!(result.rect.height, 0.0);
    }

    #[test]
    fn test_layout_button() {
        let node = ViewNode::Button {
            label: "Click".to_string(),
            action: ActionId(1),
            modifiers: Default::default(),
        };
        let result = layout(&node, Constraints::loose(400.0, 800.0), &tm());
        assert!(result.rect.width > 0.0);
        assert!(result.rect.height > 0.0);
    }

    #[test]
    fn test_layout_toggle() {
        let node = ViewNode::Toggle {
            is_on: true,
            binding: BindingId(1),
            modifiers: Default::default(),
        };
        let result = layout(&node, Constraints::loose(400.0, 800.0), &tm());
        assert!((result.rect.width - 51.0).abs() < 0.1);
        assert!((result.rect.height - 31.0).abs() < 0.1);
    }

    #[test]
    fn test_layout_spacer() {
        let node = column(vec![
            text("top"),
            ViewNode::Spacer { modifiers: Default::default() },
            text("bottom"),
        ]);
        let result = layout(&node, Constraints::tight(400.0, 800.0), &tm());
        assert_eq!(result.children.len(), 3);
        // Spacer should fill remaining space
        let spacer_h = result.children[1].rect.height;
        let text_h = result.children[0].rect.height + result.children[2].rect.height;
        assert!(spacer_h > 0.0, "spacer should have height");
        assert!((spacer_h + text_h - 800.0).abs() < 1.0, "total should fill 800px");
    }

    #[test]
    fn test_layout_1000_nodes_performance() {
        let items: Vec<ViewNode> = (0..1000)
            .map(|i| text(&format!("item {}", i)))
            .collect();
        let tree = column(items);

        let start = std::time::Instant::now();
        let result = layout(&tree, Constraints::loose(400.0, 10000.0), &tm());
        let elapsed = start.elapsed();

        assert_eq!(result.children.len(), 1000);
        assert!(elapsed.as_millis() < 50, "layout of 1000 nodes took {:?}", elapsed);
    }

    #[test]
    fn test_layout_rect_contains_point() {
        let rect = LayoutRect { x: 10.0, y: 20.0, width: 100.0, height: 50.0 };
        assert!(rect.contains_point(10.0, 20.0));
        assert!(rect.contains_point(50.0, 40.0));
        assert!(!rect.contains_point(9.0, 20.0));
        assert!(!rect.contains_point(10.0, 70.0));
        assert!(!rect.contains_point(110.0, 20.0));
    }

    #[test]
    fn test_constraints_tight() {
        let c = Constraints::tight(100.0, 200.0);
        assert_eq!(c.constrain_width(50.0), 100.0);
        assert_eq!(c.constrain_width(150.0), 100.0);
        assert_eq!(c.constrain_height(300.0), 200.0);
    }

    #[test]
    fn test_constraints_loose() {
        let c = Constraints::loose(100.0, 200.0);
        assert_eq!(c.constrain_width(50.0), 50.0);
        assert_eq!(c.constrain_width(150.0), 100.0);
    }

    #[test]
    fn test_text_wrapping() {
        // Long text that should wrap
        let node = text("a]bcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxyz");
        let result = layout(&node, Constraints::loose(100.0, 800.0), &tm());
        // Should wrap because text is wider than 100px
        assert!(result.rect.height > 20.0, "wrapped text should be taller than one line");
    }

    #[test]
    fn test_scroll_layout_unbounded_height() {
        let items: Vec<ViewNode> = (0..100)
            .map(|i| text(&format!("item {}", i)))
            .collect();
        let node = ViewNode::Container {
            kind: ContainerKind::Scroll,
            children: items,
            modifiers: Default::default(),
        };

        let result = layout(&node, Constraints::loose(400.0, 200.0), &tm());
        // Scroll container: children laid out without height constraint
        // Content height = 100 * 20 = 2000, but container constrained to 200
        assert!(result.rect.height <= 200.0, "scroll container should be constrained");
        assert_eq!(result.children.len(), 100);
    }
}
