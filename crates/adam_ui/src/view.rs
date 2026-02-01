//! View trait and view rendering context.
//!
//! The `View` trait is the core protocol for Adam UI. Every view declaration
//! in Adam source code compiles to a struct that implements `View`.

use crate::state::{ViewId, SubscriptionRegistry, begin_tracking, end_tracking};
use crate::vtree::ViewNode;

// ---------------------------------------------------------------------------
// View trait
// ---------------------------------------------------------------------------

/// The core view protocol. Every Adam view implements this trait.
///
/// The compiler transforms:
/// ```text
/// view Counter {
///     @state count: i32 = 0
///     body { Text("Count: {count}") }
/// }
/// ```
/// Into a struct with a `body()` method that returns a `ViewNode` tree.
pub trait View {
    /// Build the view tree for this view. Called when the view needs rendering.
    fn body(&self) -> ViewNode;

    /// Get the view's unique ID. Each view instance has a stable identity.
    fn view_id(&self) -> ViewId;
}

// ---------------------------------------------------------------------------
// ViewContext — manages the lifecycle of a view tree
// ---------------------------------------------------------------------------

/// Manages a tree of views, their subscriptions, and re-rendering.
pub struct ViewContext {
    pub registry: SubscriptionRegistry,
    /// Root view node tree (result of last body() call)
    pub root_tree: Option<ViewNode>,
}

impl ViewContext {
    pub fn new() -> Self {
        Self {
            registry: SubscriptionRegistry::new(),
            root_tree: None,
        }
    }

    /// Evaluate a view's body with dependency tracking.
    /// Returns the new ViewNode tree and updates subscriptions.
    pub fn evaluate_body(&mut self, view: &dyn View) -> ViewNode {
        let view_id = view.view_id();

        // Start tracking reads
        begin_tracking(view_id);

        // Call body — any ReactiveCell.get() calls are recorded
        let tree = view.body();

        // End tracking and update subscriptions
        if let Some(scope) = end_tracking() {
            self.registry.update_subscriptions(scope);
        }

        tree
    }

    /// Set the root tree.
    pub fn set_root(&mut self, tree: ViewNode) {
        self.root_tree = Some(tree);
    }

    /// Get dirty views and clear the dirty list.
    pub fn take_dirty_views(&mut self) -> Vec<ViewId> {
        self.registry.take_dirty_views()
    }

    /// Remove a view from tracking.
    pub fn remove_view(&mut self, view_id: ViewId) {
        self.registry.remove_view(view_id);
    }
}

impl Default for ViewContext {
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
    use crate::state::ReactiveCell;

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
            ViewNode::Text {
                content: format!("Count: {}", count),
                modifiers: Default::default(),
            }
        }

        fn view_id(&self) -> ViewId {
            self.id
        }
    }

    #[test]
    fn test_view_body_returns_tree() {
        let counter = CounterView::new(42);
        let tree = counter.body();
        match &tree {
            ViewNode::Text { content, .. } => assert_eq!(content, "Count: 42"),
            _ => panic!("expected Text node"),
        }
    }

    #[test]
    fn test_view_context_tracks_deps() {
        let mut ctx = ViewContext::new();
        let counter = CounterView::new(0);

        let _tree = ctx.evaluate_body(&counter);

        // The counter's @state cell should be tracked
        let deps = ctx.registry.view_to_cells.get(&counter.id);
        assert!(deps.is_some());
        assert_eq!(deps.unwrap().len(), 1);
        assert_eq!(deps.unwrap()[0], counter.count.id());
    }

    #[test]
    fn test_state_change_marks_dirty() {
        let mut ctx = ViewContext::new();
        let counter = CounterView::new(0);

        // Evaluate body to register subscriptions
        let _tree = ctx.evaluate_body(&counter);

        // Change state
        counter.count.set(1);

        // Notify the registry
        ctx.registry.notify_change(counter.count.id());

        let dirty = ctx.take_dirty_views();
        assert_eq!(dirty.len(), 1);
        assert_eq!(dirty[0], counter.id);
    }

    #[test]
    fn test_re_evaluate_updates_tree() {
        let mut ctx = ViewContext::new();
        let counter = CounterView::new(0);

        let tree1 = ctx.evaluate_body(&counter);
        match &tree1 {
            ViewNode::Text { content, .. } => assert_eq!(content, "Count: 0"),
            _ => panic!("expected Text"),
        }

        counter.count.set(5);
        let tree2 = ctx.evaluate_body(&counter);
        match &tree2 {
            ViewNode::Text { content, .. } => assert_eq!(content, "Count: 5"),
            _ => panic!("expected Text"),
        }
    }

    #[test]
    fn test_remove_view_clears_tracking() {
        let mut ctx = ViewContext::new();
        let counter = CounterView::new(0);

        let _tree = ctx.evaluate_body(&counter);
        ctx.remove_view(counter.id);

        // After removal, state changes should not mark anything dirty
        counter.count.set(99);
        ctx.registry.notify_change(counter.count.id());
        assert!(ctx.take_dirty_views().is_empty());
    }
}
