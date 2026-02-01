//! Reactive state tracking for Adam UI views.
//!
//! `ReactiveCell<T>` wraps a value and tracks reads/writes. When a cell is read
//! during a view's `body()` evaluation, that view is registered as a subscriber.
//! When the cell is written, all subscribers are notified to re-render.

use std::cell::{Cell, RefCell};
use std::sync::atomic::{AtomicU64, Ordering};

// ---------------------------------------------------------------------------
// Unique IDs
// ---------------------------------------------------------------------------

/// Globally unique identifier for a reactive cell.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CellId(pub u64);

static NEXT_CELL_ID: AtomicU64 = AtomicU64::new(1);

impl CellId {
    pub fn next() -> Self {
        CellId(NEXT_CELL_ID.fetch_add(1, Ordering::Relaxed))
    }
}

/// Globally unique identifier for a view instance.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ViewId(pub u64);

static NEXT_VIEW_ID: AtomicU64 = AtomicU64::new(1);

impl ViewId {
    pub fn next() -> Self {
        ViewId(NEXT_VIEW_ID.fetch_add(1, Ordering::Relaxed))
    }
}

// ---------------------------------------------------------------------------
// Dependency tracker (thread-local)
// ---------------------------------------------------------------------------

// During a view's `body()` call we record which cells are read.
// This is stored in a thread-local so nested body() calls form a stack.
thread_local! {
    static TRACKING_STACK: RefCell<Vec<TrackingScope>> = RefCell::new(Vec::new());
}

/// A tracking scope records all cell reads during a single body() evaluation.
#[derive(Debug, Clone)]
pub struct TrackingScope {
    pub view_id: ViewId,
    pub dependencies: Vec<CellId>,
}

/// Begin tracking reads for a view. Call before `body()`.
pub fn begin_tracking(view_id: ViewId) {
    TRACKING_STACK.with(|stack| {
        stack.borrow_mut().push(TrackingScope {
            view_id,
            dependencies: Vec::new(),
        });
    });
}

/// End tracking and return the collected dependencies.
pub fn end_tracking() -> Option<TrackingScope> {
    TRACKING_STACK.with(|stack| stack.borrow_mut().pop())
}

/// Record that a cell was read during the current tracking scope.
pub fn record_read(cell_id: CellId) {
    TRACKING_STACK.with(|stack| {
        let mut stack = stack.borrow_mut();
        if let Some(scope) = stack.last_mut() {
            if !scope.dependencies.contains(&cell_id) {
                scope.dependencies.push(cell_id);
            }
        }
    });
}

// ---------------------------------------------------------------------------
// Subscription registry (which views depend on which cells)
// ---------------------------------------------------------------------------

use std::collections::{HashMap, HashSet};

/// The subscription registry maps cell IDs → set of view IDs that depend on them.
/// When a cell changes, all subscribed views need to re-render.
pub struct SubscriptionRegistry {
    /// cell → views that read it
    pub cell_to_views: HashMap<CellId, HashSet<ViewId>>,
    /// view → cells it reads (for cleanup when view re-evaluates)
    pub view_to_cells: HashMap<ViewId, Vec<CellId>>,
    /// Views that need re-rendering
    pub dirty_views: Vec<ViewId>,
}

impl SubscriptionRegistry {
    pub fn new() -> Self {
        Self {
            cell_to_views: HashMap::new(),
            view_to_cells: HashMap::new(),
            dirty_views: Vec::new(),
        }
    }

    /// After a view's body() is evaluated, register its dependencies.
    pub fn update_subscriptions(&mut self, scope: TrackingScope) {
        let view_id = scope.view_id;

        // Remove old subscriptions for this view
        if let Some(old_cells) = self.view_to_cells.remove(&view_id) {
            for cell_id in &old_cells {
                if let Some(views) = self.cell_to_views.get_mut(cell_id) {
                    views.remove(&view_id);
                }
            }
        }

        // Add new subscriptions
        for &cell_id in &scope.dependencies {
            self.cell_to_views
                .entry(cell_id)
                .or_default()
                .insert(view_id);
        }
        self.view_to_cells.insert(view_id, scope.dependencies);
    }

    /// Called when a cell value changes. Marks dependent views as dirty.
    pub fn notify_change(&mut self, cell_id: CellId) {
        if let Some(views) = self.cell_to_views.get(&cell_id) {
            for &view_id in views {
                if !self.dirty_views.contains(&view_id) {
                    self.dirty_views.push(view_id);
                }
            }
        }
    }

    /// Drain dirty views that need re-rendering.
    pub fn take_dirty_views(&mut self) -> Vec<ViewId> {
        std::mem::take(&mut self.dirty_views)
    }

    /// Remove all subscriptions for a view (when it's destroyed).
    pub fn remove_view(&mut self, view_id: ViewId) {
        if let Some(cells) = self.view_to_cells.remove(&view_id) {
            for cell_id in &cells {
                if let Some(views) = self.cell_to_views.get_mut(cell_id) {
                    views.remove(&view_id);
                }
            }
        }
        self.dirty_views.retain(|&id| id != view_id);
    }
}

impl Default for SubscriptionRegistry {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// ReactiveCell<T>
// ---------------------------------------------------------------------------

/// A reactive cell holds a value and notifies subscribers when it changes.
///
/// In the Adam runtime, `@state` fields are backed by reactive cells.
/// When the cell's value is read during a view's `body()`, the dependency
/// is automatically tracked. When the value is written, subscribed views
/// are marked dirty for re-rendering.
pub struct ReactiveCell<T> {
    id: CellId,
    value: RefCell<T>,
    version: Cell<u64>,
}

impl<T: Clone + PartialEq> ReactiveCell<T> {
    /// Create a new reactive cell with an initial value.
    pub fn new(value: T) -> Self {
        Self {
            id: CellId::next(),
            value: RefCell::new(value),
            version: Cell::new(0),
        }
    }

    /// Get the cell's unique ID.
    pub fn id(&self) -> CellId {
        self.id
    }

    /// Get the current value, recording a read dependency.
    pub fn get(&self) -> T {
        record_read(self.id);
        self.value.borrow().clone()
    }

    /// Get the current value without tracking (for internal use).
    pub fn get_untracked(&self) -> T {
        self.value.borrow().clone()
    }

    /// Set a new value. Returns true if the value actually changed.
    pub fn set(&self, new_value: T) -> bool {
        let changed = {
            let current = self.value.borrow();
            *current != new_value
        };
        if changed {
            *self.value.borrow_mut() = new_value;
            self.version.set(self.version.get() + 1);
        }
        changed
    }

    /// Get the current version (incremented on each change).
    pub fn version(&self) -> u64 {
        self.version.get()
    }
}

impl<T: Clone + PartialEq + Default> Default for ReactiveCell<T> {
    fn default() -> Self {
        Self::new(T::default())
    }
}

impl<T: Clone + PartialEq + std::fmt::Debug> std::fmt::Debug for ReactiveCell<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ReactiveCell")
            .field("id", &self.id)
            .field("value", &*self.value.borrow())
            .field("version", &self.version.get())
            .finish()
    }
}

// ---------------------------------------------------------------------------
// Binding<T> — two-way reference to a parent's @state
// ---------------------------------------------------------------------------

/// A binding provides two-way access to a parent view's state cell.
/// In Adam, `@binding` fields receive a Binding from the parent.
pub struct Binding<T> {
    cell_id: CellId,
    getter: Box<dyn Fn() -> T>,
    setter: Box<dyn Fn(T)>,
}

impl<T> Binding<T> {
    /// Create a binding from a reactive cell.
    pub fn from_cell(cell: &ReactiveCell<T>) -> Self
    where
        T: Clone + PartialEq + 'static,
    {
        let cell_id = cell.id();
        // We need raw pointer trickery because Binding needs to reference
        // the cell. In the real runtime, the cell pointer is stable because
        // views are heap-allocated. For now, use a simple approach.
        let ptr = cell as *const ReactiveCell<T>;
        Self {
            cell_id,
            getter: Box::new(move || unsafe { &*ptr }.get()),
            setter: Box::new(move |v| {
                unsafe { &*ptr }.set(v);
            }),
        }
    }

    /// Get the current value (tracks dependency).
    pub fn get(&self) -> T {
        (self.getter)()
    }

    /// Set a new value (notifies subscribers).
    pub fn set(&self, value: T) {
        (self.setter)(value);
    }

    /// Get the underlying cell ID.
    pub fn cell_id(&self) -> CellId {
        self.cell_id
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_reactive_cell_basic() {
        let cell = ReactiveCell::new(42i32);
        assert_eq!(cell.get_untracked(), 42);
        assert_eq!(cell.version(), 0);

        assert!(cell.set(100));
        assert_eq!(cell.get_untracked(), 100);
        assert_eq!(cell.version(), 1);

        // Setting same value doesn't bump version
        assert!(!cell.set(100));
        assert_eq!(cell.version(), 1);
    }

    #[test]
    fn test_reactive_cell_tracking() {
        let cell_a = ReactiveCell::new(1i32);
        let cell_b = ReactiveCell::new(2i32);

        let view_id = ViewId::next();
        begin_tracking(view_id);

        // Reading cells during tracking records dependencies
        let _a = cell_a.get();
        let _b = cell_b.get();
        let _a2 = cell_a.get(); // duplicate read, should not duplicate

        let scope = end_tracking().unwrap();
        assert_eq!(scope.view_id, view_id);
        assert_eq!(scope.dependencies.len(), 2);
        assert!(scope.dependencies.contains(&cell_a.id()));
        assert!(scope.dependencies.contains(&cell_b.id()));
    }

    #[test]
    fn test_subscription_registry() {
        let mut registry = SubscriptionRegistry::new();

        let cell_a = CellId::next();
        let cell_b = CellId::next();
        let view_1 = ViewId::next();
        let view_2 = ViewId::next();

        // View 1 depends on cell_a and cell_b
        registry.update_subscriptions(TrackingScope {
            view_id: view_1,
            dependencies: vec![cell_a, cell_b],
        });

        // View 2 depends only on cell_a
        registry.update_subscriptions(TrackingScope {
            view_id: view_2,
            dependencies: vec![cell_a],
        });

        // Changing cell_a marks both views dirty
        registry.notify_change(cell_a);
        let dirty = registry.take_dirty_views();
        assert_eq!(dirty.len(), 2);

        // Changing cell_b marks only view_1 dirty
        registry.notify_change(cell_b);
        let dirty = registry.take_dirty_views();
        assert_eq!(dirty.len(), 1);
        assert_eq!(dirty[0], view_1);
    }

    #[test]
    fn test_subscription_update_removes_old() {
        let mut registry = SubscriptionRegistry::new();

        let cell_a = CellId::next();
        let cell_b = CellId::next();
        let view_1 = ViewId::next();

        // View 1 initially depends on cell_a
        registry.update_subscriptions(TrackingScope {
            view_id: view_1,
            dependencies: vec![cell_a],
        });

        // After re-evaluation, view 1 now depends on cell_b only
        registry.update_subscriptions(TrackingScope {
            view_id: view_1,
            dependencies: vec![cell_b],
        });

        // Changing cell_a should NOT mark view_1 dirty
        registry.notify_change(cell_a);
        assert!(registry.take_dirty_views().is_empty());

        // Changing cell_b should mark view_1 dirty
        registry.notify_change(cell_b);
        let dirty = registry.take_dirty_views();
        assert_eq!(dirty.len(), 1);
        assert_eq!(dirty[0], view_1);
    }

    #[test]
    fn test_remove_view() {
        let mut registry = SubscriptionRegistry::new();

        let cell_a = CellId::next();
        let view_1 = ViewId::next();

        registry.update_subscriptions(TrackingScope {
            view_id: view_1,
            dependencies: vec![cell_a],
        });

        registry.remove_view(view_1);

        // Changing cell_a should not produce dirty views
        registry.notify_change(cell_a);
        assert!(registry.take_dirty_views().is_empty());
    }

    #[test]
    fn test_nested_tracking() {
        let cell_a = ReactiveCell::new(1i32);
        let cell_b = ReactiveCell::new(2i32);

        let parent_id = ViewId::next();
        let child_id = ViewId::next();

        // Parent starts tracking
        begin_tracking(parent_id);
        let _a = cell_a.get();

        // Child starts tracking (nested)
        begin_tracking(child_id);
        let _b = cell_b.get();

        // Child ends
        let child_scope = end_tracking().unwrap();
        assert_eq!(child_scope.view_id, child_id);
        assert_eq!(child_scope.dependencies.len(), 1);
        assert!(child_scope.dependencies.contains(&cell_b.id()));

        // Parent ends
        let parent_scope = end_tracking().unwrap();
        assert_eq!(parent_scope.view_id, parent_id);
        assert_eq!(parent_scope.dependencies.len(), 1);
        assert!(parent_scope.dependencies.contains(&cell_a.id()));
    }

    #[test]
    fn test_cell_ids_unique() {
        let c1 = ReactiveCell::new(1);
        let c2 = ReactiveCell::new(2);
        let c3 = ReactiveCell::new(3);
        assert_ne!(c1.id(), c2.id());
        assert_ne!(c2.id(), c3.id());
    }

    #[test]
    fn test_view_ids_unique() {
        let v1 = ViewId::next();
        let v2 = ViewId::next();
        let v3 = ViewId::next();
        assert_ne!(v1, v2);
        assert_ne!(v2, v3);
    }

    #[test]
    fn test_reactive_cell_string() {
        let cell = ReactiveCell::new(String::from("hello"));
        assert_eq!(cell.get_untracked(), "hello");
        assert!(cell.set(String::from("world")));
        assert_eq!(cell.get_untracked(), "world");
        assert_eq!(cell.version(), 1);
    }

    #[test]
    fn test_reactive_cell_default() {
        let cell: ReactiveCell<i32> = ReactiveCell::default();
        assert_eq!(cell.get_untracked(), 0);
    }

    #[test]
    fn test_reactive_cell_debug() {
        let cell = ReactiveCell::new(42);
        let debug = format!("{:?}", cell);
        assert!(debug.contains("ReactiveCell"));
        assert!(debug.contains("42"));
    }
}
