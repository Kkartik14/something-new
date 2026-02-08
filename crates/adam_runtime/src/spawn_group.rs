//! Spawn Group — structured concurrency with fork-join semantics.
//!
//! A spawn group lets you launch multiple concurrent tasks and wait for all
//! of them to complete, collecting their results in spawn order.
//!
//! ```adam
//! group := spawn_group()
//! for url in urls {
//!     group.spawn { fetch(url) }
//! }
//! results := group.wait()   // blocks until all complete
//! ```

use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

use crate::scheduler;
use crate::thread::ThreadId;

// ================================================================
// SpawnGroup
// ================================================================

/// A group of concurrently spawned tasks that can be awaited together.
///
/// Results are collected in spawn order (not completion order).
/// Panics in individual tasks are caught and represented as errors.
pub struct SpawnGroup<T: Send + 'static> {
    /// Shared state between the group owner and spawned tasks.
    shared: Arc<SpawnGroupShared<T>>,
}

/// Shared state for a spawn group, accessed by all spawned tasks.
struct SpawnGroupShared<T: Send + 'static> {
    /// Results stored by index. Each slot starts as None and is filled
    /// when the corresponding task completes.
    results: Mutex<Vec<SpawnGroupResult<T>>>,
    /// Number of tasks still running.
    remaining: AtomicU64,
    /// ThreadId of the thread blocked on wait(), so we can wake it.
    waiter: Mutex<Option<ThreadId>>,
}

/// Result of a single task in a spawn group.
enum SpawnGroupResult<T> {
    /// Not yet completed.
    Pending,
    /// Completed successfully with a value.
    Ok(T),
    /// Task panicked.
    Panicked(String),
}

impl<T: Send + 'static> SpawnGroup<T> {
    /// Create a new empty spawn group.
    pub fn new() -> Self {
        SpawnGroup {
            shared: Arc::new(SpawnGroupShared {
                results: Mutex::new(Vec::new()),
                remaining: AtomicU64::new(0),
                waiter: Mutex::new(None),
            }),
        }
    }

    /// Spawn a new task in this group.
    ///
    /// The task will run concurrently on the scheduler. Its result
    /// will be collected at the position corresponding to the order
    /// of this spawn call.
    pub fn spawn(&self, f: impl FnOnce() -> T + Send + 'static) {
        let index = {
            let mut results = self.shared.results.lock().unwrap();
            let idx = results.len();
            results.push(SpawnGroupResult::Pending);
            idx
        };
        self.shared.remaining.fetch_add(1, Ordering::SeqCst);

        let shared = self.shared.clone();
        let sched = scheduler::get_scheduler();
        sched.spawn(move || {
            // Use a completion guard to decrement remaining on drop.
            // This ensures wait() doesn't deadlock even if the task panics,
            // because the guard's destructor runs during unwinding.
            let guard = CompletionGuard {
                shared: shared.clone(),
                index,
                completed: AtomicBool::new(false),
            };

            // Run the task. Panics are caught by the trampoline's catch_unwind
            // (in context_switch.rs), but first the guard's Drop will run
            // during stack unwinding.
            let value = f();

            // Store success result and mark as completed.
            {
                let mut results = shared.results.lock().unwrap();
                results[index] = SpawnGroupResult::Ok(value);
            }
            guard.completed.store(true, Ordering::SeqCst);

            // Guard drops here and decrements remaining.
        });
    }

    /// Wait for all spawned tasks to complete and collect their results.
    ///
    /// Returns results in spawn order. Panicked tasks are represented
    /// as `Err(message)`.
    ///
    /// Consumes the spawn group — it cannot be reused after waiting.
    pub fn wait(self) -> Vec<Result<T, String>> {
        // Fast path: all tasks already done (or no tasks spawned).
        if self.shared.remaining.load(Ordering::SeqCst) == 0 {
            return self.extract_results();
        }

        // Register as waiter and block until all tasks complete.
        let thread_id = scheduler::current_thread_id();
        {
            let mut waiter = self.shared.waiter.lock().unwrap();
            *waiter = Some(thread_id);
        }

        // Check again after registering (avoid race).
        while self.shared.remaining.load(Ordering::SeqCst) > 0 {
            scheduler::block_current();
        }

        self.extract_results()
    }

    /// Extract results from the shared state.
    fn extract_results(&self) -> Vec<Result<T, String>> {
        let mut results = self.shared.results.lock().unwrap();
        results
            .drain(..)
            .map(|r| match r {
                SpawnGroupResult::Ok(v) => Ok(v),
                SpawnGroupResult::Panicked(msg) => Err(msg),
                SpawnGroupResult::Pending => Err("task did not complete".to_string()),
            })
            .collect()
    }

    /// Number of tasks still running.
    pub fn remaining(&self) -> u64 {
        self.shared.remaining.load(Ordering::SeqCst)
    }
}

/// Guard that runs on Drop to ensure remaining count is decremented.
///
/// If the task panics, the guard's destructor marks it as panicked
/// and decrements remaining. This prevents wait() from deadlocking
/// when a task fails.
struct CompletionGuard<T: Send + 'static> {
    shared: Arc<SpawnGroupShared<T>>,
    index: usize,
    /// Set to true when the task completes normally.
    /// If false on drop, the task panicked.
    completed: AtomicBool,
}

impl<T: Send + 'static> Drop for CompletionGuard<T> {
    fn drop(&mut self) {
        // If not completed, the task panicked — mark it.
        if !self.completed.load(Ordering::SeqCst) {
            if let Ok(mut results) = self.shared.results.lock() {
                results[self.index] = SpawnGroupResult::Panicked("task panicked".to_string());
            }
            // If the mutex is poisoned, we can't store the result.
            // The result stays as Pending, but remaining is still decremented.
        }

        // Decrement remaining.
        let prev = self.shared.remaining.fetch_sub(1, Ordering::SeqCst);
        if prev == 1 {
            // Last task — wake the waiter.
            if let Ok(waiter) = self.shared.waiter.lock() {
                if let Some(thread_id) = *waiter {
                    scheduler::get_scheduler().wake(thread_id);
                }
            }
        }
    }
}

impl<T: Send + 'static> Default for SpawnGroup<T> {
    fn default() -> Self {
        Self::new()
    }
}

// ================================================================
// C FFI — called by compiled Adam code
// ================================================================

/// Create a new spawn group. Returns an opaque pointer.
#[no_mangle]
pub extern "C" fn __adam_spawn_group_create() -> *mut () {
    let group = Box::new(SpawnGroup::<Vec<u8>>::new());
    Box::into_raw(group) as *mut ()
}

/// Spawn a task in a spawn group.
/// `fn_ptr` is a function pointer, `arg` is passed as its argument.
#[no_mangle]
pub unsafe extern "C" fn __adam_spawn_group_spawn(
    group: *mut (),
    fn_ptr: extern "C" fn(usize) -> *mut u8,
    arg: usize,
    result_size: usize,
) {
    let group = &*(group as *const SpawnGroup<Vec<u8>>);
    group.spawn(move || {
        let result_ptr = fn_ptr(arg);
        if result_ptr.is_null() || result_size == 0 {
            return Vec::new();
        }
        let bytes = std::slice::from_raw_parts(result_ptr, result_size).to_vec();
        // Free the result buffer allocated by the task.
        std::alloc::dealloc(
            result_ptr,
            std::alloc::Layout::from_size_align(result_size, 8).unwrap(),
        );
        bytes
    });
}

/// Wait for all tasks in the group to complete.
/// Returns a pointer to the results array (caller must free).
#[no_mangle]
pub unsafe extern "C" fn __adam_spawn_group_wait(group: *mut ()) -> usize {
    let group = *Box::from_raw(group as *mut SpawnGroup<Vec<u8>>);
    let results = group.wait();
    results.len() // Return count; actual result retrieval is TODO.
}

/// Drop a spawn group without waiting.
#[no_mangle]
pub unsafe extern "C" fn __adam_spawn_group_drop(group: *mut ()) {
    let _ = Box::from_raw(group as *mut SpawnGroup<Vec<u8>>);
}

// ================================================================
// Tests
// ================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicI32, Ordering};
    use std::sync::Arc as StdArc;

    /// Helper: run green threads on a scheduler and wait for completion.
    fn run_with_scheduler<F: FnOnce(StdArc<crate::scheduler::Scheduler>) + Send + 'static>(
        workers: usize,
        f: F,
    ) {
        let sched = StdArc::new(crate::scheduler::Scheduler::with_workers(workers));
        let sched2 = sched.clone();
        sched.spawn(move || {
            f(sched2);
        });
        sched.start();

        let start = std::time::Instant::now();
        while !sched.is_shutdown() && start.elapsed() < std::time::Duration::from_secs(10) {
            std::thread::sleep(std::time::Duration::from_millis(10));
        }
        sched.shutdown();
    }

    #[test]
    fn test_empty_group() {
        // Empty group: wait returns immediately with empty vec.
        let result = StdArc::new(AtomicI32::new(0));
        let result2 = result.clone();

        run_with_scheduler(2, move |_sched| {
            let group: SpawnGroup<i32> = SpawnGroup::new();
            let results = group.wait();
            assert!(results.is_empty());
            result2.store(1, Ordering::SeqCst);
        });

        assert_eq!(result.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn test_single_task() {
        let result = StdArc::new(AtomicI32::new(0));
        let result2 = result.clone();

        run_with_scheduler(2, move |_sched| {
            let group: SpawnGroup<i32> = SpawnGroup::new();
            group.spawn(|| 42);
            let results = group.wait();
            assert_eq!(results.len(), 1);
            assert_eq!(results[0], Ok(42));
            result2.store(1, Ordering::SeqCst);
        });

        assert_eq!(result.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn test_multiple_tasks_ordered_results() {
        // Results should be in spawn order, not completion order.
        let result = StdArc::new(AtomicI32::new(0));
        let result2 = result.clone();

        run_with_scheduler(4, move |_sched| {
            let group: SpawnGroup<i32> = SpawnGroup::new();
            for i in 0..5 {
                group.spawn(move || i * 10);
            }
            let results = group.wait();
            assert_eq!(results.len(), 5);
            for (i, r) in results.iter().enumerate() {
                assert_eq!(*r, Ok(i as i32 * 10));
            }
            result2.store(1, Ordering::SeqCst);
        });

        assert_eq!(result.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn test_panic_in_task() {
        // Panic in one task: other tasks still complete, panicked task returns Err.
        let result = StdArc::new(AtomicI32::new(0));
        let result2 = result.clone();

        run_with_scheduler(4, move |_sched| {
            let group: SpawnGroup<i32> = SpawnGroup::new();

            group.spawn(|| 1);
            group.spawn(|| panic!("oops"));
            group.spawn(|| 3);

            let results = group.wait();
            assert_eq!(results.len(), 3);
            assert_eq!(results[0], Ok(1));
            assert!(
                results[1].is_err(),
                "panicked task should be Err, got {:?}",
                results[1]
            );
            assert_eq!(results[2], Ok(3));
            result2.store(1, Ordering::SeqCst);
        });

        assert_eq!(result.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn test_many_tasks() {
        // Stress test: 100 concurrent tasks.
        let result = StdArc::new(AtomicI32::new(0));
        let result2 = result.clone();

        run_with_scheduler(4, move |_sched| {
            let group: SpawnGroup<i32> = SpawnGroup::new();
            for i in 0..100 {
                group.spawn(move || i);
            }
            let results = group.wait();
            assert_eq!(results.len(), 100);
            let sum: i32 = results.iter().map(|r| r.as_ref().unwrap()).sum();
            assert_eq!(sum, 4950); // 0+1+...+99
            result2.store(1, Ordering::SeqCst);
        });

        assert_eq!(result.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn test_tasks_with_channels() {
        // Tasks communicate via channels.
        let result = StdArc::new(AtomicI32::new(0));
        let result2 = result.clone();

        run_with_scheduler(4, move |_sched| {
            use crate::channel::TypedChannel;

            let ch: TypedChannel<i32> = TypedChannel::new(10);
            let group: SpawnGroup<()> = SpawnGroup::new();

            // 5 producers send their index.
            for i in 0..5 {
                let ch_clone = ch.clone();
                group.spawn(move || {
                    ch_clone.send(i);
                });
            }

            // Collect all values.
            let mut total = 0;
            for _ in 0..5 {
                total += ch.recv().unwrap();
            }
            assert_eq!(total, 10); // 0+1+2+3+4

            let results = group.wait();
            assert_eq!(results.len(), 5);
            assert!(results.iter().all(|r| r.is_ok()));
            result2.store(1, Ordering::SeqCst);
        });

        assert_eq!(result.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn test_remaining_count() {
        let result = StdArc::new(AtomicI32::new(0));
        let result2 = result.clone();

        run_with_scheduler(2, move |_sched| {
            let group: SpawnGroup<i32> = SpawnGroup::new();
            assert_eq!(group.remaining(), 0);

            group.spawn(|| 1);
            group.spawn(|| 2);
            // remaining may already be 0 if tasks completed, but it was >= 2 when spawned.
            // Just verify it doesn't crash.
            let _r = group.remaining();

            let results = group.wait();
            assert_eq!(results.len(), 2);
            result2.store(1, Ordering::SeqCst);
        });

        assert_eq!(result.load(Ordering::SeqCst), 1);
    }
}
