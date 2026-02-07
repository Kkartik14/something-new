//! Adam UI — declarative, reactive UI framework with Skia rendering.
//!
//! # Architecture
//!
//! ```text
//! ┌─────────────┐     ┌─────────────┐     ┌──────────┐     ┌─────────┐
//! │ View (body) │────►│ Virtual Tree│────►│  Layout  │────►│ Render  │
//! │ + @state    │     │ + Diffing   │     │  Engine  │     │ Backend │
//! └─────────────┘     └─────────────┘     └──────────┘     └─────────┘
//!       ▲                                                        │
//!       │                ┌─────────────┐                         │
//!       └────────────────│   Input /   │◄────────────────────────┘
//!                        │   Gesture   │
//!                        └─────────────┘
//! ```
//!
//! - **state** — Reactive cells with dependency tracking
//! - **view** — View trait, ViewContext for lifecycle management
//! - **vtree** — Virtual view tree nodes, diffing algorithm
//! - **layout** — Flexbox-style two-pass layout engine
//! - **render** — RenderBackend trait, test backend, command generation
//! - **input** — Input events, hit testing, event processing
//! - **animation** — Spring, tween, keyframe animations
//! - **components** — Built-in UI components (Text, Button, List, etc.)

pub mod state;
pub mod vtree;
pub mod view;
pub mod layout;
pub mod render;
pub mod input;
pub mod animation;
pub mod components;
pub mod skia_backend;

// Re-exports for convenience
pub use state::{ReactiveCell, ViewId, CellId, Binding, SubscriptionRegistry};
pub use view::{View, ViewContext};
pub use vtree::{ViewNode, ContainerKind, Modifiers, Color, EdgeInsets, Key, ActionId, BindingId};
pub use layout::{LayoutRect, LayoutNode, Constraints, TextMeasure, EstimatedTextMeasure};
pub use render::{RenderBackend, RenderCommand, TestRenderBackend};
pub use skia_backend::SkiaRenderBackend;
pub use input::{InputEvent, UIEvent, EventProcessor, HitTestResult};
pub use animation::{Animation, AnimationManager, Easing, SpringParams};
pub use components::ViewModifiers;

// ---------------------------------------------------------------------------
// Integration tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod integration_tests {
    use crate::state::*;
    use crate::view::*;
    use crate::vtree::*;
    use crate::layout;
    use crate::layout::*;
    use crate::render::*;
    use crate::input::*;
    use crate::animation::*;
    use crate::components::*;

    // -- Counter view for tests --

    struct CounterView {
        id: ViewId,
        count: ReactiveCell<i32>,
    }

    impl CounterView {
        fn new(initial: i32) -> Self {
            Self {
                id: ViewId::next(),
                count: ReactiveCell::new(initial),
            }
        }
    }

    impl View for CounterView {
        fn body(&self) -> ViewNode {
            let count = self.count.get();
            column_spaced(12.0, vec![
                styled_text(
                    &format!("Count: {}", count),
                    24.0,
                    Color::BLACK,
                ),
                row_spaced(8.0, vec![
                    button("-", ActionId(1)),
                    button("+", ActionId(2)),
                ]),
            ])
        }

        fn view_id(&self) -> ViewId {
            self.id
        }
    }

    // -- Test: state change triggers body re-evaluation --

    #[test]
    fn test_view_state_change() {
        let mut ctx = ViewContext::new();
        let counter = CounterView::new(0);

        let tree1 = ctx.evaluate_body(&counter);
        match &tree1 {
            ViewNode::Container { children, .. } => {
                match &children[0] {
                    ViewNode::Text { content, .. } => assert_eq!(content, "Count: 0"),
                    _ => panic!("expected text"),
                }
            }
            _ => panic!("expected column"),
        }

        counter.count.set(5);
        ctx.registry.notify_change(counter.count.id());
        let dirty = ctx.take_dirty_views();
        assert_eq!(dirty.len(), 1);

        let tree2 = ctx.evaluate_body(&counter);
        match &tree2 {
            ViewNode::Container { children, .. } => {
                match &children[0] {
                    ViewNode::Text { content, .. } => assert_eq!(content, "Count: 5"),
                    _ => panic!("expected text"),
                }
            }
            _ => panic!("expected column"),
        }
    }

    // -- Test: diff detects text change --

    #[test]
    fn test_diff_update_text() {
        let old = text("hello");
        let new = text("world");
        let mutations = diff(&old, &new);
        assert_eq!(mutations.len(), 1);
        assert!(matches!(&mutations[0], Mutation::UpdateText { new_content, .. } if new_content == "world"));
    }

    // -- Test: diff add child --

    #[test]
    fn test_diff_add_child() {
        let old = column(vec![text("a")]);
        let new = column(vec![text("a"), text("b")]);
        let mutations = diff(&old, &new);
        assert_eq!(mutations.len(), 1);
        assert!(matches!(&mutations[0], Mutation::Insert { index: 1, .. }));
    }

    // -- Test: diff remove child --

    #[test]
    fn test_diff_remove_child() {
        let old = column(vec![text("a"), text("b")]);
        let new = column(vec![text("a")]);
        let mutations = diff(&old, &new);
        assert_eq!(mutations.len(), 1);
        assert!(matches!(&mutations[0], Mutation::Remove { index: 1, .. }));
    }

    // -- Test: layout column --

    #[test]
    fn test_layout_column() {
        let tree = column(vec![text("first"), text("second")]);
        let tm = EstimatedTextMeasure::default();
        let result = layout::layout(&tree, Constraints::loose(400.0, 800.0), &tm);

        assert_eq!(result.children.len(), 2);
        assert!(result.children[0].rect.y < result.children[1].rect.y);
    }

    // -- Test: layout row --

    #[test]
    fn test_layout_row() {
        let tree = row(vec![text("left"), text("right")]);
        let tm = EstimatedTextMeasure::default();
        let result = layout::layout(&tree, Constraints::loose(400.0, 800.0), &tm);

        assert_eq!(result.children.len(), 2);
        assert!(result.children[0].rect.x < result.children[1].rect.x);
    }

    // -- Test: layout padding --

    #[test]
    fn test_layout_padding() {
        let tree = text("padded")
            .padding(EdgeInsets::all(20.0));
        let tm = EstimatedTextMeasure::default();
        let result = layout::layout(&tree, Constraints::loose(400.0, 800.0), &tm);

        // "padded" = 6*8 = 48px + 40 padding = 88px
        assert!((result.rect.width - 88.0).abs() < 1.0);
    }

    // -- Test: layout nested --

    #[test]
    fn test_layout_nested() {
        let tree = column(vec![
            row(vec![text("a"), text("b")]),
            text("c"),
        ]);
        let tm = EstimatedTextMeasure::default();
        let result = layout::layout(&tree, Constraints::loose(400.0, 800.0), &tm);

        assert_eq!(result.children.len(), 2);
        assert_eq!(result.children[0].children.len(), 2);
    }

    // -- Test: render text produces DrawText command --

    #[test]
    fn test_render_text() {
        let tree = text("Hello");
        let tm = EstimatedTextMeasure::default();
        let layout_node = layout::layout(&tree, Constraints::loose(400.0, 800.0), &tm);
        let commands = generate_render_commands(&tree, &layout_node);

        let text_cmds: Vec<_> = commands.iter()
            .filter(|c| matches!(c, RenderCommand::DrawText { .. }))
            .collect();
        assert_eq!(text_cmds.len(), 1);
    }

    // -- Test: render button has fill and text --

    #[test]
    fn test_render_button() {
        let tree = button("Click", ActionId(1));
        let tm = EstimatedTextMeasure::default();
        let layout_node = layout::layout(&tree, Constraints::loose(400.0, 800.0), &tm);
        let commands = generate_render_commands(&tree, &layout_node);

        let fills = commands.iter().filter(|c| matches!(c, RenderCommand::FillRect { .. })).count();
        let texts = commands.iter().filter(|c| matches!(c, RenderCommand::DrawText { .. })).count();
        assert!(fills > 0);
        assert!(texts > 0);
    }

    // -- Test: hit test button --

    #[test]
    fn test_hit_test_button() {
        let tree = button("Click", ActionId(42));
        let tm = EstimatedTextMeasure::default();
        let layout_node = layout::layout(&tree, Constraints::loose(400.0, 800.0), &tm);

        let hit = hit_test(&tree, &layout_node, 5.0, 5.0);
        assert!(hit.is_some());
        assert_eq!(hit.unwrap().action, Some(ActionId(42)));
    }

    // -- Test: hit test miss --

    #[test]
    fn test_hit_test_miss() {
        let tree = button("Click", ActionId(1));
        let tm = EstimatedTextMeasure::default();
        let layout_node = layout::layout(&tree, Constraints::loose(400.0, 800.0), &tm);

        let hit = hit_test(&tree, &layout_node, 999.0, 999.0);
        assert!(hit.is_none());
    }

    // -- Test: scroll view --

    #[test]
    fn test_scroll_view() {
        let items: Vec<ViewNode> = (0..50)
            .map(|i| text(&format!("item {}", i)))
            .collect();
        let tree = scroll(items);
        let tm = EstimatedTextMeasure::default();
        let result = layout::layout(&tree, Constraints::loose(400.0, 200.0), &tm);

        assert_eq!(result.children.len(), 50);
        assert!(result.rect.height <= 200.0);
    }

    // -- Test: spring animation --

    #[test]
    fn test_animation_spring() {
        let mut anim = Animation::new_spring(0.0, 100.0, SpringParams::default());
        let dt = 1.0 / 60.0;
        let mut values = Vec::new();
        for _ in 0..120 {
            values.push(anim.tick(dt));
        }
        // Should approach 100
        let last = *values.last().unwrap();
        assert!((last - 100.0).abs() < 5.0);
    }

    // -- Test: list rendering --

    #[test]
    fn test_list_rendering() {
        let items: Vec<ViewNode> = (0..100)
            .map(|i| text(&format!("item {}", i)))
            .collect();
        let tree = list(items);
        let tm = EstimatedTextMeasure::default();
        let layout_node = layout::layout(&tree, Constraints::loose(400.0, 10000.0), &tm);
        let commands = generate_render_commands(&tree, &layout_node);

        let text_count = commands.iter()
            .filter(|c| matches!(c, RenderCommand::DrawText { .. }))
            .count();
        assert_eq!(text_count, 100);
    }

    // -- Test: counter app end-to-end --

    #[test]
    fn test_counter_app() {
        let mut ctx = ViewContext::new();
        let counter = CounterView::new(0);

        // Render initial state
        let tree = ctx.evaluate_body(&counter);
        let tm = EstimatedTextMeasure::default();
        let layout_node = layout::layout(&tree, Constraints::loose(400.0, 800.0), &tm);
        let mut backend = TestRenderBackend::new();
        render_to_backend(&tree, &layout_node, &mut backend, 400.0, 800.0);

        assert_eq!(backend.frames.len(), 1);
        let text_count = backend.count_in_last_frame(|c| matches!(c, RenderCommand::DrawText { .. }));
        assert!(text_count >= 3, "counter app should have at least 3 text draws");

        // Simulate increment
        counter.count.set(1);
        ctx.registry.notify_change(counter.count.id());
        let dirty = ctx.take_dirty_views();
        assert_eq!(dirty.len(), 1);

        let tree2 = ctx.evaluate_body(&counter);
        let layout_node2 = layout::layout(&tree2, Constraints::loose(400.0, 800.0), &tm);
        render_to_backend(&tree2, &layout_node2, &mut backend, 400.0, 800.0);

        assert_eq!(backend.frames.len(), 2);
    }

    // -- Test: todo app end-to-end --

    #[test]
    fn test_todo_app() {
        let todos = vec!["Buy groceries", "Walk the dog", "Write code"];
        let items: Vec<ViewNode> = todos.iter().enumerate().map(|(i, todo)| {
            row_spaced(8.0, vec![
                toggle(false, BindingId(i as u64)),
                text(*todo),
                spacer(),
                button("X", ActionId(100 + i as u64)),
            ])
        }).collect();

        let tree = column_spaced(8.0, vec![
            styled_text("Todo List", 28.0, Color::BLACK),
            text_input("", "Add a todo...", BindingId(99)),
            list(items),
        ]);

        let tm = EstimatedTextMeasure::default();
        let layout_node = layout::layout(&tree, Constraints::loose(400.0, 800.0), &tm);
        let commands = generate_render_commands(&tree, &layout_node);

        // Should have multiple text draws and button draws
        let text_count = commands.iter().filter(|c| matches!(c, RenderCommand::DrawText { .. })).count();
        assert!(text_count >= 5, "todo app should render text nodes: {}", text_count);
    }

    // -- Test: event processor tap fires action --

    #[test]
    fn test_tap_fires_action() {
        let tree = button("Click", ActionId(42));
        let tm = EstimatedTextMeasure::default();
        let layout_node = layout::layout(&tree, Constraints::loose(400.0, 800.0), &tm);
        let mut processor = EventProcessor::new();

        processor.process(
            &InputEvent::PointerDown { x: 5.0, y: 5.0, pointer_id: 0 },
            &tree, &layout_node, 0,
        );
        let events = processor.process(
            &InputEvent::PointerUp { x: 5.0, y: 5.0, pointer_id: 0 },
            &tree, &layout_node, 100,
        );

        assert_eq!(events.len(), 1);
        match &events[0] {
            UIEvent::Tap { action, .. } => assert_eq!(*action, ActionId(42)),
            _ => panic!("expected tap"),
        }
    }

    // -- Test: multiple animations --

    #[test]
    fn test_multiple_animations() {
        let mut mgr = AnimationManager::new();
        mgr.add(Animation::new_tween(0.0, 100.0, 1.0, Easing::Linear));
        mgr.add(Animation::new_spring(0.0, 50.0, SpringParams::default()));

        assert_eq!(mgr.active_count(), 2);
        mgr.tick_all(0.5);
        assert!(mgr.active_count() >= 1);
    }

    // -- Test: view modifier chain --

    #[test]
    fn test_modifier_chain_renders() {
        let tree = text("Styled")
            .padding(EdgeInsets::all(16.0))
            .background(Color::rgb(240, 240, 240))
            .corner_radius(8.0)
            .border(1.0, Color::rgb(200, 200, 200));

        let tm = EstimatedTextMeasure::default();
        let layout_node = layout::layout(&tree, Constraints::loose(400.0, 800.0), &tm);
        let commands = generate_render_commands(&tree, &layout_node);

        // Should have background fill
        let fills = commands.iter().filter(|c| matches!(c, RenderCommand::FillRect { .. })).count();
        assert!(fills > 0, "styled text should have fill rect");
    }

    // -- Test: conditional view rendering --

    #[test]
    fn test_conditional_view() {
        let tree = column(vec![
            text("always"),
            show_if(false, text("hidden")),
            show_if(true, text("visible")),
        ]);

        let tm = EstimatedTextMeasure::default();
        let layout_node = layout::layout(&tree, Constraints::loose(400.0, 800.0), &tm);

        // The hidden conditional should have zero size
        assert_eq!(layout_node.children[1].rect.width, 0.0);
        assert_eq!(layout_node.children[1].rect.height, 0.0);
        // The visible conditional should have nonzero size
        assert!(layout_node.children[2].rect.width > 0.0);
    }

    // -- Test: keyed list diffing --

    #[test]
    fn test_keyed_list_diff() {
        let old = keyed_list(vec![
            (Key::Int(1), text("a")),
            (Key::Int(2), text("b")),
        ]);
        let new = keyed_list(vec![
            (Key::Int(2), text("b")),
            (Key::Int(1), text("a")),
        ]);
        let mutations = diff(&old, &new);
        // Should have move mutations
        assert!(!mutations.is_empty());
    }

    // -- Test: subscription cleanup on view removal --

    #[test]
    fn test_view_cleanup() {
        let mut ctx = ViewContext::new();
        let counter = CounterView::new(0);

        // Register subscriptions
        let _tree = ctx.evaluate_body(&counter);
        assert!(!ctx.registry.view_to_cells.is_empty());

        // Remove view
        ctx.remove_view(counter.view_id());
        assert!(ctx.registry.view_to_cells.is_empty());

        // State changes should not produce dirty views
        counter.count.set(99);
        ctx.registry.notify_change(counter.count.id());
        assert!(ctx.take_dirty_views().is_empty());
    }

    // -- Test: 1000-node diff performance --

    #[test]
    fn test_1000_node_diff_performance() {
        let old_items: Vec<ViewNode> = (0..1000).map(|i| text(&format!("item {}", i))).collect();
        let new_items: Vec<ViewNode> = (0..1000).map(|i| {
            if i == 500 { text("changed") } else { text(&format!("item {}", i)) }
        }).collect();

        let old = column(old_items);
        let new = column(new_items);

        let start = std::time::Instant::now();
        let mutations = diff(&old, &new);
        let elapsed = start.elapsed();

        assert_eq!(mutations.len(), 1);
        assert!(elapsed.as_millis() < 10);
    }

    // -- Test: 1000-node layout performance --

    #[test]
    fn test_1000_node_layout_performance() {
        let items: Vec<ViewNode> = (0..1000).map(|i| text(&format!("item {}", i))).collect();
        let tree = column(items);
        let tm = EstimatedTextMeasure::default();

        let start = std::time::Instant::now();
        let result = layout::layout(&tree, Constraints::loose(400.0, 50000.0), &tm);
        let elapsed = start.elapsed();

        assert_eq!(result.children.len(), 1000);
        assert!(elapsed.as_millis() < 50);
    }

    // -- Test: full render pipeline --

    #[test]
    fn test_full_render_pipeline() {
        let tree = column_spaced(8.0, vec![
            text("Title").background(Color::rgb(240, 240, 240)),
            button("Action", ActionId(1)),
            progress(0.5),
            toggle(true, BindingId(1)),
        ]);

        let tm = EstimatedTextMeasure::default();
        let layout_node = layout::layout(&tree, Constraints::loose(400.0, 800.0), &tm);
        let mut backend = TestRenderBackend::new();
        render_to_backend(&tree, &layout_node, &mut backend, 400.0, 800.0);

        assert_eq!(backend.frames.len(), 1);
        let frame = backend.last_frame().unwrap();
        assert!(!frame.is_empty(), "should have render commands");
    }

    // -- Test: animation cancellation --

    #[test]
    fn test_animation_cancellation() {
        let mut mgr = AnimationManager::new();
        let anim = Animation::new_tween(0.0, 100.0, 10.0, Easing::Linear);
        let id = mgr.add(anim);

        mgr.tick_all(1.0);
        let val = mgr.value(id);
        assert!(val.is_some());

        mgr.cancel(id);
        let val = mgr.value(id);
        assert!(val.is_none());
    }
}
