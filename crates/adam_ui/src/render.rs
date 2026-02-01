//! Rendering abstraction for Adam UI.
//!
//! Defines the `RenderBackend` trait that rendering backends (Skia, test, etc.)
//! implement. The UI framework uses this trait to draw view nodes.

use crate::layout::{LayoutNode, LayoutRect};
use crate::vtree::*;

// ---------------------------------------------------------------------------
// Render commands
// ---------------------------------------------------------------------------

/// A render command produced by walking the view + layout trees.
#[derive(Debug, Clone, PartialEq)]
pub enum RenderCommand {
    /// Draw a filled rectangle.
    FillRect {
        rect: LayoutRect,
        color: Color,
        corner_radius: f32,
    },
    /// Draw a rectangle border.
    StrokeRect {
        rect: LayoutRect,
        color: Color,
        width: f32,
        corner_radius: f32,
    },
    /// Draw text.
    DrawText {
        text: String,
        x: f32,
        y: f32,
        font: Font,
        color: Color,
    },
    /// Draw an image.
    DrawImage {
        source: ImageSource,
        rect: LayoutRect,
    },
    /// Push a clip rectangle (for scroll views, corner radius).
    PushClip {
        rect: LayoutRect,
        corner_radius: f32,
    },
    /// Pop the clip rectangle.
    PopClip,
    /// Set opacity for subsequent draws.
    PushOpacity(f32),
    /// Restore opacity.
    PopOpacity,
    /// Draw a shadow.
    DrawShadow {
        rect: LayoutRect,
        shadow: Shadow,
        corner_radius: f32,
    },
    /// Draw a progress bar.
    DrawProgress {
        rect: LayoutRect,
        value: Option<f64>,
        color: Color,
        track_color: Color,
    },
    /// Draw a toggle switch.
    DrawToggle {
        rect: LayoutRect,
        is_on: bool,
        track_color: Color,
        thumb_color: Color,
    },
    /// Draw a slider.
    DrawSlider {
        rect: LayoutRect,
        value: f64,
        min: f64,
        max: f64,
        track_color: Color,
        thumb_color: Color,
    },
}

// ---------------------------------------------------------------------------
// RenderBackend trait
// ---------------------------------------------------------------------------

/// Trait that rendering backends implement.
pub trait RenderBackend {
    /// Begin a new frame.
    fn begin_frame(&mut self, width: f32, height: f32);

    /// Execute a render command.
    fn execute(&mut self, command: &RenderCommand);

    /// End the frame and present to screen.
    fn end_frame(&mut self);
}

// ---------------------------------------------------------------------------
// Command generation — walk view + layout trees to produce commands
// ---------------------------------------------------------------------------

/// Generate render commands from a view tree and its layout.
pub fn generate_render_commands(
    view: &ViewNode,
    layout_node: &LayoutNode,
) -> Vec<RenderCommand> {
    let mut commands = Vec::new();
    render_node(view, layout_node, &mut commands);
    commands
}

fn render_node(
    view: &ViewNode,
    layout_node: &LayoutNode,
    commands: &mut Vec<RenderCommand>,
) {
    let rect = layout_node.rect;

    match view {
        ViewNode::Text { content, modifiers } => {
            render_background(rect, modifiers, commands);
            let pad = modifiers.padding.unwrap_or(EdgeInsets::ZERO);
            let color = modifiers.foreground.unwrap_or(Color::BLACK);
            let font = modifiers.font.clone().unwrap_or_default();
            commands.push(RenderCommand::DrawText {
                text: content.clone(),
                x: rect.x + pad.left,
                y: rect.y + pad.top,
                font,
                color,
            });
        }

        ViewNode::Button { label, modifiers, .. } => {
            let bg = modifiers.background.unwrap_or(Color::rgb(0, 122, 255));
            let cr = modifiers.corner_radius.unwrap_or(8.0);
            render_shadow(rect, modifiers, cr, commands);
            commands.push(RenderCommand::FillRect {
                rect,
                color: bg,
                corner_radius: cr,
            });
            if let Some(border) = &modifiers.border {
                commands.push(RenderCommand::StrokeRect {
                    rect,
                    color: border.color,
                    width: border.width,
                    corner_radius: cr,
                });
            }
            let pad = modifiers.padding.unwrap_or(EdgeInsets::symmetric(16.0, 8.0));
            let fg = modifiers.foreground.unwrap_or(Color::WHITE);
            let font = modifiers.font.clone().unwrap_or_default();
            commands.push(RenderCommand::DrawText {
                text: label.clone(),
                x: rect.x + pad.left,
                y: rect.y + pad.top,
                font,
                color: fg,
            });
        }

        ViewNode::Image { source, modifiers } => {
            render_background(rect, modifiers, commands);
            let cr = modifiers.corner_radius.unwrap_or(0.0);
            if cr > 0.0 {
                commands.push(RenderCommand::PushClip { rect, corner_radius: cr });
            }
            commands.push(RenderCommand::DrawImage {
                source: source.clone(),
                rect,
            });
            if cr > 0.0 {
                commands.push(RenderCommand::PopClip);
            }
        }

        ViewNode::Container { kind, children, modifiers } => {
            let cr = modifiers.corner_radius.unwrap_or(0.0);
            render_shadow(rect, modifiers, cr, commands);
            render_background(rect, modifiers, commands);

            if let Some(opacity) = modifiers.opacity {
                commands.push(RenderCommand::PushOpacity(opacity));
            }

            let needs_clip = matches!(kind, ContainerKind::Scroll) || cr > 0.0;
            if needs_clip {
                commands.push(RenderCommand::PushClip { rect, corner_radius: cr });
            }

            if let Some(border) = &modifiers.border {
                commands.push(RenderCommand::StrokeRect {
                    rect,
                    color: border.color,
                    width: border.width,
                    corner_radius: cr,
                });
            }

            for (child_view, child_layout) in children.iter().zip(layout_node.children.iter()) {
                render_node(child_view, child_layout, commands);
            }

            if needs_clip {
                commands.push(RenderCommand::PopClip);
            }
            if modifiers.opacity.is_some() {
                commands.push(RenderCommand::PopOpacity);
            }
        }

        ViewNode::List { items, modifiers } => {
            render_background(rect, modifiers, commands);
            commands.push(RenderCommand::PushClip {
                rect,
                corner_radius: modifiers.corner_radius.unwrap_or(0.0),
            });
            for (item_view, item_layout) in items.iter().zip(layout_node.children.iter()) {
                render_node(item_view, item_layout, commands);
            }
            commands.push(RenderCommand::PopClip);
        }

        ViewNode::TextInput { value, placeholder, modifiers, .. } => {
            let bg = modifiers.background.unwrap_or(Color::rgb(245, 245, 245));
            let cr = modifiers.corner_radius.unwrap_or(6.0);
            commands.push(RenderCommand::FillRect { rect, color: bg, corner_radius: cr });
            if let Some(border) = &modifiers.border {
                commands.push(RenderCommand::StrokeRect {
                    rect,
                    color: border.color,
                    width: border.width,
                    corner_radius: cr,
                });
            }
            let pad = modifiers.padding.unwrap_or(EdgeInsets::symmetric(12.0, 8.0));
            let display_text = if value.is_empty() { placeholder } else { value };
            let color = if value.is_empty() {
                Color::rgb(180, 180, 180)
            } else {
                modifiers.foreground.unwrap_or(Color::BLACK)
            };
            commands.push(RenderCommand::DrawText {
                text: display_text.clone(),
                x: rect.x + pad.left,
                y: rect.y + pad.top,
                font: modifiers.font.clone().unwrap_or_default(),
                color,
            });
        }

        ViewNode::Toggle { is_on, modifiers, .. } => {
            render_background(rect, modifiers, commands);
            let track_color = if *is_on {
                Color::rgb(52, 199, 89)
            } else {
                Color::rgb(200, 200, 200)
            };
            commands.push(RenderCommand::DrawToggle {
                rect,
                is_on: *is_on,
                track_color,
                thumb_color: Color::WHITE,
            });
        }

        ViewNode::Slider { value, min, max, modifiers, .. } => {
            render_background(rect, modifiers, commands);
            commands.push(RenderCommand::DrawSlider {
                rect,
                value: *value,
                min: *min,
                max: *max,
                track_color: Color::rgb(200, 200, 200),
                thumb_color: Color::rgb(0, 122, 255),
            });
        }

        ViewNode::Progress { value, modifiers } => {
            render_background(rect, modifiers, commands);
            commands.push(RenderCommand::DrawProgress {
                rect,
                value: *value,
                color: Color::rgb(0, 122, 255),
                track_color: Color::rgb(230, 230, 230),
            });
        }

        ViewNode::Conditional { condition, child } => {
            if *condition && !layout_node.children.is_empty() {
                render_node(child, &layout_node.children[0], commands);
            } else if *condition {
                // Fallback: use the current layout_node
                render_node(child, layout_node, commands);
            }
        }

        ViewNode::Custom { modifiers, .. } => {
            render_background(rect, modifiers, commands);
        }

        ViewNode::Spacer { .. } => {
            // Spacers are invisible
        }

        ViewNode::Empty => {}
    }
}

fn render_background(rect: LayoutRect, modifiers: &Modifiers, commands: &mut Vec<RenderCommand>) {
    if let Some(bg) = modifiers.background {
        let cr = modifiers.corner_radius.unwrap_or(0.0);
        commands.push(RenderCommand::FillRect {
            rect,
            color: bg,
            corner_radius: cr,
        });
    }
}

fn render_shadow(
    rect: LayoutRect,
    modifiers: &Modifiers,
    corner_radius: f32,
    commands: &mut Vec<RenderCommand>,
) {
    if let Some(shadow) = &modifiers.shadow {
        commands.push(RenderCommand::DrawShadow {
            rect,
            shadow: *shadow,
            corner_radius,
        });
    }
}

// ---------------------------------------------------------------------------
// Test render backend — records commands for verification
// ---------------------------------------------------------------------------

/// A render backend that records all commands for testing.
pub struct TestRenderBackend {
    pub frames: Vec<Vec<RenderCommand>>,
    current_frame: Vec<RenderCommand>,
}

impl TestRenderBackend {
    pub fn new() -> Self {
        Self {
            frames: Vec::new(),
            current_frame: Vec::new(),
        }
    }

    /// Get all commands from the last frame.
    pub fn last_frame(&self) -> Option<&Vec<RenderCommand>> {
        self.frames.last()
    }

    /// Get all commands from all frames.
    pub fn all_commands(&self) -> Vec<&RenderCommand> {
        self.frames.iter().flat_map(|f| f.iter()).collect()
    }

    /// Count commands of a specific type in the last frame.
    pub fn count_in_last_frame<F: Fn(&RenderCommand) -> bool>(&self, pred: F) -> usize {
        self.frames.last()
            .map(|f| f.iter().filter(|c| pred(c)).count())
            .unwrap_or(0)
    }
}

impl Default for TestRenderBackend {
    fn default() -> Self {
        Self::new()
    }
}

impl RenderBackend for TestRenderBackend {
    fn begin_frame(&mut self, _width: f32, _height: f32) {
        self.current_frame.clear();
    }

    fn execute(&mut self, command: &RenderCommand) {
        self.current_frame.push(command.clone());
    }

    fn end_frame(&mut self) {
        self.frames.push(std::mem::take(&mut self.current_frame));
    }
}

// ---------------------------------------------------------------------------
// Full render pipeline helper
// ---------------------------------------------------------------------------

/// Render a view tree through a backend.
pub fn render_to_backend(
    view: &ViewNode,
    layout_node: &LayoutNode,
    backend: &mut dyn RenderBackend,
    width: f32,
    height: f32,
) {
    let commands = generate_render_commands(view, layout_node);
    backend.begin_frame(width, height);
    for cmd in &commands {
        backend.execute(cmd);
    }
    backend.end_frame();
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::layout::{self, Constraints, EstimatedTextMeasure};

    fn text(s: &str) -> ViewNode {
        ViewNode::Text {
            content: s.to_string(),
            modifiers: Default::default(),
        }
    }

    fn do_layout(view: &ViewNode) -> LayoutNode {
        let tm = EstimatedTextMeasure::default();
        layout::layout(view, Constraints::loose(400.0, 800.0), &tm)
    }

    #[test]
    fn test_render_text() {
        let view = text("Hello");
        let layout_node = do_layout(&view);
        let commands = generate_render_commands(&view, &layout_node);

        let text_cmds: Vec<_> = commands.iter().filter(|c| matches!(c, RenderCommand::DrawText { .. })).collect();
        assert_eq!(text_cmds.len(), 1);
        match text_cmds[0] {
            RenderCommand::DrawText { text: t, .. } => assert_eq!(t, "Hello"),
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_render_button() {
        let view = ViewNode::Button {
            label: "Click".to_string(),
            action: ActionId(1),
            modifiers: Default::default(),
        };
        let layout_node = do_layout(&view);
        let commands = generate_render_commands(&view, &layout_node);

        // Button should have a fill rect and text
        let fills: Vec<_> = commands.iter().filter(|c| matches!(c, RenderCommand::FillRect { .. })).collect();
        let texts: Vec<_> = commands.iter().filter(|c| matches!(c, RenderCommand::DrawText { .. })).collect();
        assert!(!fills.is_empty(), "button should have fill rect");
        assert!(!texts.is_empty(), "button should have text");
    }

    #[test]
    fn test_render_container_with_background() {
        let view = ViewNode::Container {
            kind: ContainerKind::Column,
            children: vec![text("child")],
            modifiers: Modifiers {
                background: Some(Color::RED),
                ..Default::default()
            },
        };
        let layout_node = do_layout(&view);
        let commands = generate_render_commands(&view, &layout_node);

        let fills: Vec<_> = commands.iter().filter(|c| matches!(c, RenderCommand::FillRect { .. })).collect();
        assert!(!fills.is_empty(), "container with background should have fill");
    }

    #[test]
    fn test_render_toggle() {
        let view = ViewNode::Toggle {
            is_on: true,
            binding: BindingId(1),
            modifiers: Default::default(),
        };
        let layout_node = do_layout(&view);
        let commands = generate_render_commands(&view, &layout_node);

        let toggles: Vec<_> = commands.iter().filter(|c| matches!(c, RenderCommand::DrawToggle { .. })).collect();
        assert_eq!(toggles.len(), 1);
    }

    #[test]
    fn test_render_progress() {
        let view = ViewNode::Progress {
            value: Some(0.5),
            modifiers: Default::default(),
        };
        let layout_node = do_layout(&view);
        let commands = generate_render_commands(&view, &layout_node);

        let progress: Vec<_> = commands.iter().filter(|c| matches!(c, RenderCommand::DrawProgress { .. })).collect();
        assert_eq!(progress.len(), 1);
    }

    #[test]
    fn test_render_empty() {
        let commands = generate_render_commands(&ViewNode::Empty, &LayoutNode::leaf(LayoutRect::ZERO));
        assert!(commands.is_empty());
    }

    #[test]
    fn test_test_backend() {
        let view = text("test");
        let layout_node = do_layout(&view);
        let mut backend = TestRenderBackend::new();

        render_to_backend(&view, &layout_node, &mut backend, 400.0, 800.0);

        assert_eq!(backend.frames.len(), 1);
        let text_count = backend.count_in_last_frame(|c| matches!(c, RenderCommand::DrawText { .. }));
        assert_eq!(text_count, 1);
    }

    #[test]
    fn test_render_scroll_view_clips() {
        let items: Vec<ViewNode> = (0..10)
            .map(|i| text(&format!("item {}", i)))
            .collect();
        let view = ViewNode::Container {
            kind: ContainerKind::Scroll,
            children: items,
            modifiers: Default::default(),
        };
        let layout_node = do_layout(&view);
        let commands = generate_render_commands(&view, &layout_node);

        // Should have PushClip and PopClip
        let clips: Vec<_> = commands.iter().filter(|c| matches!(c, RenderCommand::PushClip { .. })).collect();
        let pops: Vec<_> = commands.iter().filter(|c| matches!(c, RenderCommand::PopClip)).collect();
        assert_eq!(clips.len(), 1, "scroll view should push clip");
        assert_eq!(pops.len(), 1, "scroll view should pop clip");
    }

    #[test]
    fn test_render_text_input_placeholder() {
        let view = ViewNode::TextInput {
            value: String::new(),
            binding: BindingId(1),
            placeholder: "Enter text...".to_string(),
            modifiers: Default::default(),
        };
        let layout_node = do_layout(&view);
        let commands = generate_render_commands(&view, &layout_node);

        let texts: Vec<_> = commands.iter().filter_map(|c| {
            if let RenderCommand::DrawText { text, color, .. } = c {
                Some((text.as_str(), *color))
            } else {
                None
            }
        }).collect();

        assert_eq!(texts.len(), 1);
        assert_eq!(texts[0].0, "Enter text...");
        // Placeholder should be gray
        assert_eq!(texts[0].1, Color::rgb(180, 180, 180));
    }

    #[test]
    fn test_render_text_input_with_value() {
        let view = ViewNode::TextInput {
            value: "hello".to_string(),
            binding: BindingId(1),
            placeholder: "Enter text...".to_string(),
            modifiers: Default::default(),
        };
        let layout_node = do_layout(&view);
        let commands = generate_render_commands(&view, &layout_node);

        let texts: Vec<_> = commands.iter().filter_map(|c| {
            if let RenderCommand::DrawText { text, .. } = c {
                Some(text.as_str())
            } else {
                None
            }
        }).collect();

        assert_eq!(texts.len(), 1);
        assert_eq!(texts[0], "hello");
    }

    #[test]
    fn test_render_with_opacity() {
        let view = ViewNode::Container {
            kind: ContainerKind::Column,
            children: vec![text("semi-transparent")],
            modifiers: Modifiers {
                opacity: Some(0.5),
                ..Default::default()
            },
        };
        let layout_node = do_layout(&view);
        let commands = generate_render_commands(&view, &layout_node);

        let opacities: Vec<_> = commands.iter()
            .filter(|c| matches!(c, RenderCommand::PushOpacity(_) | RenderCommand::PopOpacity))
            .collect();
        assert_eq!(opacities.len(), 2, "should push and pop opacity");
    }

    #[test]
    fn test_render_with_shadow() {
        let view = ViewNode::Button {
            label: "Shadow".to_string(),
            action: ActionId(1),
            modifiers: Modifiers {
                shadow: Some(Shadow {
                    color: Color::rgba(0, 0, 0, 128),
                    offset_x: 2.0,
                    offset_y: 2.0,
                    blur_radius: 4.0,
                }),
                ..Default::default()
            },
        };
        let layout_node = do_layout(&view);
        let commands = generate_render_commands(&view, &layout_node);

        let shadows: Vec<_> = commands.iter()
            .filter(|c| matches!(c, RenderCommand::DrawShadow { .. }))
            .collect();
        assert_eq!(shadows.len(), 1);
    }
}
