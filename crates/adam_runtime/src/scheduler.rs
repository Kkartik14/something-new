//! Work-stealing scheduler — distributes green threads across OS threads.
//!
//! Architecture:
//! - One Worker per CPU core (OS thread)
//! - Each Worker has a local run queue (VecDeque)
//! - Global run queue for overflow and newly spawned threads from outside workers
//! - When a worker's local queue is empty, it steals from other workers
//! - When all queues are empty, workers park (sleep) until new work arrives

use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Condvar, Mutex, OnceLock};
use std::thread::{self, JoinHandle};

use crate::context_switch;
use crate::thread::{CpuContext, GreenThread, ThreadId, ThreadState};

// ================================================================
// Global scheduler singleton
// ================================================================

static SCHEDULER: OnceLock<Arc<Scheduler>> = OnceLock::new();

/// Get or initialize the global scheduler.
pub fn global_scheduler() -> Arc<Scheduler> {
    SCHEDULER.get_or_init(|| Arc::new(Scheduler::new())).clone()
}

// ================================================================
// Scheduler
// ================================================================

pub struct Scheduler {
    /// Shared state protected by mutex.
    inner: Mutex<SchedulerInner>,
    /// Condition variable to wake parked workers.
    notify: Condvar,
    /// Number of active OS worker threads.
    worker_count: usize,
    /// Flag to signal shutdown.
    shutdown: AtomicBool,
    /// Number of workers currently parked (waiting for work).
    parked_workers: AtomicUsize,
}

struct SchedulerInner {
    /// Global run queue — threads that aren't assigned to any worker.
    global_queue: VecDeque<GreenThread>,
    /// Per-worker local run queues (indexed by worker_id).
    local_queues: Vec<VecDeque<GreenThread>>,
    /// Worker handles (only used for shutdown join).
    worker_handles: Vec<Option<JoinHandle<()>>>,
    /// Total number of live threads (for shutdown detection).
    live_thread_count: usize,
    /// Threads currently blocked on channel/select operations.
    /// Keyed by ThreadId so they can be woken by channel operations.
    blocked_threads: HashMap<ThreadId, GreenThread>,
    /// Thread IDs that were woken before they were put in blocked_threads.
    /// Handles the race between blocking and waking.
    pending_wakes: HashSet<ThreadId>,
}

impl Scheduler {
    /// Create a new scheduler with one worker per CPU core.
    pub fn new() -> Self {
        let cores = thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(1);
        Self::with_workers(cores)
    }

    /// Create a scheduler with a specific number of workers.
    pub fn with_workers(worker_count: usize) -> Self {
        let worker_count = worker_count.max(1);
        let local_queues = (0..worker_count).map(|_| VecDeque::new()).collect();
        let worker_handles = (0..worker_count).map(|_| None).collect();

        Scheduler {
            inner: Mutex::new(SchedulerInner {
                global_queue: VecDeque::new(),
                local_queues,
                worker_handles,
                live_thread_count: 0,
                blocked_threads: HashMap::new(),
                pending_wakes: HashSet::new(),
            }),
            notify: Condvar::new(),
            worker_count,
            shutdown: AtomicBool::new(false),
            parked_workers: AtomicUsize::new(0),
        }
    }

    /// Start all worker OS threads.
    pub fn start(self: &Arc<Self>) {
        let mut inner = self.inner.lock().unwrap();
        for worker_id in 0..self.worker_count {
            let sched = Arc::clone(self);
            let handle = thread::Builder::new()
                .name(format!("adam-worker-{}", worker_id))
                .spawn(move || {
                    run_worker(sched, worker_id);
                })
                .expect("failed to spawn worker thread");
            inner.worker_handles[worker_id] = Some(handle);
        }
    }

    /// Spawn a new green thread.
    pub fn spawn(&self, f: impl FnOnce() + Send + 'static) {
        let mut thread = GreenThread::new(f);
        context_switch::init_thread_context(&mut thread);

        let mut inner = self.inner.lock().unwrap();
        inner.live_thread_count += 1;
        inner.global_queue.push_back(thread);
        drop(inner);

        // Wake a parked worker.
        self.notify.notify_one();
    }

    /// Spawn a green thread onto a specific worker's local queue.
    pub fn spawn_local(&self, worker_id: usize, f: impl FnOnce() + Send + 'static) {
        let mut thread = GreenThread::new(f);
        context_switch::init_thread_context(&mut thread);

        let mut inner = self.inner.lock().unwrap();
        inner.live_thread_count += 1;
        if worker_id < inner.local_queues.len() {
            inner.local_queues[worker_id].push_back(thread);
        } else {
            inner.global_queue.push_back(thread);
        }
        drop(inner);

        self.notify.notify_one();
    }

    /// Signal all workers to shut down and wait for them to finish.
    pub fn shutdown(&self) {
        self.shutdown.store(true, Ordering::SeqCst);
        self.notify.notify_all();

        // Collect handles first, then join outside the lock.
        let handles: Vec<JoinHandle<()>> = {
            let mut inner = self.inner.lock().unwrap();
            inner
                .worker_handles
                .iter_mut()
                .filter_map(|h| h.take())
                .collect()
        };
        for h in handles {
            let _ = h.join();
        }
    }

    /// Check if the scheduler is shutting down.
    pub fn is_shutdown(&self) -> bool {
        self.shutdown.load(Ordering::SeqCst)
    }

    /// Number of worker threads.
    pub fn worker_count(&self) -> usize {
        self.worker_count
    }

    /// Number of threads currently in queues.
    pub fn pending_count(&self) -> usize {
        let inner = self.inner.lock().unwrap();
        let local: usize = inner.local_queues.iter().map(|q| q.len()).sum();
        inner.global_queue.len() + local
    }

    /// Total number of live (non-completed) threads.
    pub fn live_count(&self) -> usize {
        let inner = self.inner.lock().unwrap();
        inner.live_thread_count
    }

    /// Wake a blocked green thread, moving it back to the global run queue.
    ///
    /// Called by channel operations when a blocked thread should be unblocked.
    /// Handles the race where wake() is called before the thread has been
    /// placed in blocked_threads by the worker loop.
    pub fn wake(&self, thread_id: ThreadId) {
        let mut inner = self.inner.lock().unwrap();
        if let Some(mut thread) = inner.blocked_threads.remove(&thread_id) {
            thread.state = ThreadState::Ready;
            inner.global_queue.push_back(thread);
            drop(inner);
            self.notify.notify_one();
        } else {
            // Thread hasn't been placed in blocked_threads yet.
            // Record the pending wake so the worker handles it.
            inner.pending_wakes.insert(thread_id);
            drop(inner);
            self.notify.notify_one();
        }
    }

    /// Number of threads currently blocked on channel operations.
    pub fn blocked_count(&self) -> usize {
        let inner = self.inner.lock().unwrap();
        inner.blocked_threads.len()
    }
}

// ================================================================
// Worker loop
// ================================================================

// Per-worker thread-local: the current worker's saved CPU context.
// When a green thread yields, its state is saved and this context is restored.
// Public so the trampoline in context_switch.rs can access it.
thread_local! {
    pub static WORKER_CONTEXT: std::cell::UnsafeCell<CpuContext> = std::cell::UnsafeCell::new(CpuContext::new());
    static WORKER_ID: std::cell::Cell<usize> = std::cell::Cell::new(0);
    /// Raw pointer to the currently executing GreenThread on this worker.
    /// Set by the worker before context-switching to a green thread.
    /// Used by yield_current(), block_current(), and current_thread_id().
    pub static CURRENT_THREAD_PTR: std::cell::Cell<usize> = std::cell::Cell::new(0);
    /// Per-worker scheduling tick counter. Every 61st tick, the worker
    /// checks the global queue before the local queue to prevent starvation.
    /// (Borrowed from Go's runtime: schedtick % 61.)
    static SCHED_TICK: std::cell::Cell<u64> = std::cell::Cell::new(0);
    /// Raw pointer to the Arc<Scheduler> that owns this worker.
    /// Used by channel operations to wake threads on the correct scheduler.
    pub static CURRENT_SCHEDULER: std::cell::Cell<usize> = std::cell::Cell::new(0);
}

/// Get the scheduler for the current worker thread.
///
/// Falls back to the global scheduler if not called from a worker.
/// Used by channel operations to wake blocked threads.
pub fn get_scheduler() -> Arc<Scheduler> {
    CURRENT_SCHEDULER.with(|s| {
        let ptr = s.get();
        if ptr == 0 {
            return global_scheduler();
        }
        // Safety: The pointer is valid because the worker function holds
        // an Arc<Scheduler> for the entire duration of the worker thread.
        // We temporarily create an Arc to clone it, then forget the temporary
        // to avoid decrementing the refcount we don't own.
        unsafe {
            let arc = Arc::from_raw(ptr as *const Scheduler);
            let cloned = arc.clone();
            std::mem::forget(arc);
            cloned
        }
    })
}

/// Main loop for a worker OS thread.
fn run_worker(sched: Arc<Scheduler>, worker_id: usize) {
    WORKER_ID.with(|id| id.set(worker_id));
    // Store a non-owning raw pointer to the scheduler in TLS.
    // The `sched` parameter keeps the Arc alive for the worker's lifetime,
    // so the pointer is valid as long as this function is running.
    CURRENT_SCHEDULER.with(|s| s.set(Arc::as_ptr(&sched) as usize));

    loop {
        if sched.is_shutdown() {
            return;
        }

        // Try to get work: local queue first, then global, then steal.
        let thread = find_work(&sched, worker_id);

        match thread {
            Some(mut green_thread) => {
                green_thread.state = ThreadState::Running;
                // For NEW threads (first context switch), update the thread
                // pointer register (x19/r12) to reflect the current address.
                // For RESUMED threads (yielded/blocked), we must NOT modify
                // the saved registers — the compiler may have reused x19/r12
                // for its own purposes after the trampoline ran.
                if green_thread.entry.is_some() {
                    green_thread.update_thread_ptr();
                }

                // Store a pointer to the green thread in TLS so that
                // yield_current() / block_current() can access it.
                let thread_ptr = &mut green_thread as *mut GreenThread as usize;
                CURRENT_THREAD_PTR.with(|p| p.set(thread_ptr));

                // Context switch to the green thread.
                WORKER_CONTEXT.with(|ctx| {
                    let worker_ctx = unsafe { &mut *ctx.get() };
                    unsafe {
                        crate::context_switch::switch_raw(worker_ctx, &mut green_thread);
                    }
                });

                // Clear the current thread pointer.
                CURRENT_THREAD_PTR.with(|p| p.set(0));

                // When we return here, the green thread has yielded or completed.
                match green_thread.state {
                    ThreadState::Completed => {
                        let mut inner = sched.inner.lock().unwrap();
                        inner.live_thread_count = inner.live_thread_count.saturating_sub(1);
                        // Thread is dropped here, freeing its stack.
                    }
                    ThreadState::Ready => {
                        // Thread yielded — put it back in the local queue.
                        let mut inner = sched.inner.lock().unwrap();
                        inner.local_queues[worker_id].push_back(green_thread);
                    }
                    ThreadState::Blocked => {
                        // Thread is blocked on a channel/select.
                        // Check if it was already woken (race with wake()).
                        let thread_id = green_thread.id;
                        let mut inner = sched.inner.lock().unwrap();
                        if inner.pending_wakes.remove(&thread_id) {
                            // Already woken — put back in run queue.
                            green_thread.state = ThreadState::Ready;
                            inner.local_queues[worker_id].push_back(green_thread);
                        } else {
                            // Store in blocked_threads until wake() is called.
                            inner.blocked_threads.insert(thread_id, green_thread);
                        }
                    }
                    ThreadState::Running => {
                        // Shouldn't happen — but handle gracefully.
                        let mut inner = sched.inner.lock().unwrap();
                        inner.local_queues[worker_id].push_back(green_thread);
                    }
                }
            }
            None => {
                // No work available — park this worker.
                let inner = sched.inner.lock().unwrap();
                let runnable = inner
                    .live_thread_count
                    .saturating_sub(inner.blocked_threads.len());
                if inner.live_thread_count == 0 && !sched.is_shutdown() {
                    // All threads completed — signal shutdown.
                    drop(inner);
                    sched.shutdown.store(true, Ordering::SeqCst);
                    sched.notify.notify_all();
                    return;
                }
                let _ = runnable; // used for future deadlock detection

                sched.parked_workers.fetch_add(1, Ordering::SeqCst);
                let _inner = sched
                    .notify
                    .wait_timeout(inner, std::time::Duration::from_millis(10))
                    .unwrap()
                    .0;
                sched.parked_workers.fetch_sub(1, Ordering::SeqCst);
            }
        }
    }
}

/// Find work for a worker: check local queue, global queue, then steal.
///
/// Every 61st tick, the global queue is checked first to prevent starvation
/// of newly spawned threads when local queues are dominated by yield-polling
/// threads (e.g., select or channel spin-wait loops).
fn find_work(sched: &Scheduler, worker_id: usize) -> Option<GreenThread> {
    let tick = SCHED_TICK.with(|t| {
        let v = t.get().wrapping_add(1);
        t.set(v);
        v
    });

    let mut inner = sched.inner.lock().unwrap();

    // Every 61st tick, check global queue first (Go's starvation prevention).
    if tick % 61 == 0 {
        if let Some(t) = inner.global_queue.pop_front() {
            return Some(t);
        }
    }

    // 1. Check local queue.
    if let Some(t) = inner.local_queues[worker_id].pop_front() {
        return Some(t);
    }

    // 2. Check global queue.
    if let Some(t) = inner.global_queue.pop_front() {
        // Also batch-steal from global queue to local.
        let steal_count = inner
            .global_queue
            .len()
            .min(inner.local_queues[worker_id].capacity().max(1) / 2);
        for _ in 0..steal_count {
            if let Some(t2) = inner.global_queue.pop_front() {
                inner.local_queues[worker_id].push_back(t2);
            }
        }
        return Some(t);
    }

    // 3. Work-stealing: try to steal from other workers' local queues.
    let num_workers = inner.local_queues.len();
    for offset in 1..num_workers {
        let victim = (worker_id + offset) % num_workers;
        let victim_len = inner.local_queues[victim].len();
        if victim_len > 1 {
            // Steal half of the victim's queue.
            let steal_count = victim_len / 2;
            for _ in 0..steal_count {
                if let Some(t) = inner.local_queues[victim].pop_back() {
                    inner.local_queues[worker_id].push_back(t);
                }
            }
            // Return the first stolen thread.
            return inner.local_queues[worker_id].pop_front();
        } else if victim_len == 1 {
            return inner.local_queues[victim].pop_front();
        }
    }

    None
}

/// Yield the current green thread back to the scheduler.
///
/// Called from within a running green thread to cooperatively give up the CPU.
/// The thread is marked Ready and will be re-scheduled later.
pub fn yield_current() {
    // Extract values from TLS before context switching.
    // We must NOT hold TLS references across the switch, because the thread
    // may resume on a different OS worker thread with different TLS.
    let thread_ptr = CURRENT_THREAD_PTR.with(|p| p.get());
    if thread_ptr == 0 {
        return; // Not inside a green thread.
    }
    let thread = unsafe { &mut *(thread_ptr as *mut GreenThread) };
    thread.state = ThreadState::Ready;

    let worker_ctx_ptr = WORKER_CONTEXT.with(|ctx| ctx.get());
    unsafe {
        crate::context_switch::adam_context_switch_raw(
            &mut thread.context as *mut CpuContext,
            worker_ctx_ptr as *const CpuContext,
        );
    }
    // After resuming here, we may be on a different worker.
}

/// Block the current green thread.
///
/// Called from channel operations when the thread needs to wait.
/// The thread is marked Blocked and removed from the run queue.
/// It will be re-scheduled when wake() is called with its ThreadId.
pub fn block_current() {
    // Extract values from TLS before context switching.
    // We must NOT hold TLS references across the switch, because the thread
    // may resume on a different OS worker thread with different TLS.
    let thread_ptr = CURRENT_THREAD_PTR.with(|p| p.get());
    if thread_ptr == 0 {
        return; // Not inside a green thread.
    }
    let thread = unsafe { &mut *(thread_ptr as *mut GreenThread) };
    thread.state = ThreadState::Blocked;

    let worker_ctx_ptr = WORKER_CONTEXT.with(|ctx| ctx.get());
    unsafe {
        crate::context_switch::adam_context_switch_raw(
            &mut thread.context as *mut CpuContext,
            worker_ctx_ptr as *const CpuContext,
        );
    }
    // After resuming here, we may be on a different worker.
}

/// Get the ThreadId of the currently executing green thread.
///
/// Returns 0 if not called from within a green thread.
pub fn current_thread_id() -> ThreadId {
    CURRENT_THREAD_PTR.with(|p| {
        let ptr = p.get();
        if ptr == 0 {
            return 0;
        }
        let thread = unsafe { &*(ptr as *const GreenThread) };
        thread.id
    })
}

// ================================================================
// C FFI — called by compiled Adam code
// ================================================================

/// Spawn a new green thread from compiled Adam code.
///
/// `fn_ptr` is the spawn body function, `arg` is a pointer to the
/// captured environment (heap-allocated struct). The spawned function
/// receives `arg` as its parameter.
#[no_mangle]
pub extern "C" fn __adam_spawn(fn_ptr: extern "C" fn(usize), arg: usize) {
    let sched = get_scheduler();
    sched.spawn(move || {
        fn_ptr(arg);
    });
}

/// Yield the current green thread.
#[no_mangle]
pub extern "C" fn __adam_yield() {
    yield_current();
}

// ================================================================
// Tests
// ================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicI32, Ordering};
    use std::sync::Arc;

    #[test]
    fn test_scheduler_creation() {
        let sched = Scheduler::with_workers(4);
        assert_eq!(sched.worker_count(), 4);
        assert_eq!(sched.pending_count(), 0);
        assert_eq!(sched.live_count(), 0);
    }

    #[test]
    fn test_scheduler_spawn_increments_count() {
        let sched = Arc::new(Scheduler::with_workers(1));
        sched.spawn(|| {});
        assert_eq!(sched.live_count(), 1);
        assert_eq!(sched.pending_count(), 1);
    }

    #[test]
    fn test_scheduler_spawn_many() {
        let sched = Arc::new(Scheduler::with_workers(2));
        for _ in 0..100 {
            sched.spawn(|| {});
        }
        assert_eq!(sched.live_count(), 100);
    }

    #[test]
    fn test_scheduler_start_and_shutdown() {
        let sched = Arc::new(Scheduler::with_workers(2));
        sched.start();
        // Give workers time to start.
        std::thread::sleep(std::time::Duration::from_millis(10));
        sched.shutdown();
        assert!(sched.is_shutdown());
    }

    #[test]
    fn test_scheduler_runs_tasks() {
        let counter = Arc::new(AtomicI32::new(0));
        let sched = Arc::new(Scheduler::with_workers(2));

        for _ in 0..10 {
            let c = counter.clone();
            sched.spawn(move || {
                c.fetch_add(1, Ordering::SeqCst);
            });
        }

        sched.start();

        // Wait for all tasks to complete (with timeout).
        let start = std::time::Instant::now();
        while sched.live_count() > 0 && start.elapsed() < std::time::Duration::from_secs(5) {
            std::thread::sleep(std::time::Duration::from_millis(10));
        }

        sched.shutdown();
        assert_eq!(counter.load(Ordering::SeqCst), 10);
    }

    #[test]
    fn test_scheduler_runs_1000_tasks() {
        let counter = Arc::new(AtomicI32::new(0));
        let sched = Arc::new(Scheduler::with_workers(4));

        for _ in 0..1000 {
            let c = counter.clone();
            sched.spawn(move || {
                c.fetch_add(1, Ordering::SeqCst);
            });
        }

        sched.start();

        let start = std::time::Instant::now();
        while sched.live_count() > 0 && start.elapsed() < std::time::Duration::from_secs(10) {
            std::thread::sleep(std::time::Duration::from_millis(10));
        }

        sched.shutdown();
        assert_eq!(counter.load(Ordering::SeqCst), 1000);
    }

    #[test]
    fn test_work_stealing() {
        // Spawn all tasks onto worker 0, verify they still all complete
        // (other workers should steal).
        let counter = Arc::new(AtomicI32::new(0));
        let sched = Arc::new(Scheduler::with_workers(4));

        for _ in 0..100 {
            let c = counter.clone();
            sched.spawn_local(0, move || {
                c.fetch_add(1, Ordering::SeqCst);
            });
        }

        sched.start();

        let start = std::time::Instant::now();
        while sched.live_count() > 0 && start.elapsed() < std::time::Duration::from_secs(5) {
            std::thread::sleep(std::time::Duration::from_millis(10));
        }

        sched.shutdown();
        assert_eq!(counter.load(Ordering::SeqCst), 100);
    }

    #[test]
    fn test_scheduler_runs_10k_tasks() {
        let counter = Arc::new(AtomicI32::new(0));
        let sched = Arc::new(Scheduler::with_workers(4));

        for _ in 0..10_000 {
            let c = counter.clone();
            sched.spawn(move || {
                c.fetch_add(1, Ordering::SeqCst);
            });
        }

        sched.start();

        let start = std::time::Instant::now();
        while sched.live_count() > 0 && start.elapsed() < std::time::Duration::from_secs(10) {
            std::thread::sleep(std::time::Duration::from_millis(10));
        }

        sched.shutdown();
        assert_eq!(counter.load(Ordering::SeqCst), 10_000);
    }

    #[test]
    fn test_scheduler_runs_100k_tasks() {
        let counter = Arc::new(AtomicI32::new(0));
        let sched = Arc::new(Scheduler::with_workers(4));

        for _ in 0..100_000 {
            let c = counter.clone();
            sched.spawn(move || {
                c.fetch_add(1, Ordering::SeqCst);
            });
        }

        sched.start();

        let start = std::time::Instant::now();
        while sched.live_count() > 0 && start.elapsed() < std::time::Duration::from_secs(30) {
            std::thread::sleep(std::time::Duration::from_millis(50));
        }

        sched.shutdown();
        assert_eq!(counter.load(Ordering::SeqCst), 100_000);
    }

    #[test]
    fn test_scheduler_auto_shutdown() {
        // When all threads complete, the scheduler should auto-detect and stop.
        let sched = Arc::new(Scheduler::with_workers(2));
        let done = Arc::new(AtomicBool::new(false));
        let done_clone = done.clone();

        sched.spawn(move || {
            done_clone.store(true, Ordering::SeqCst);
        });

        sched.start();

        let start = std::time::Instant::now();
        while !sched.is_shutdown() && start.elapsed() < std::time::Duration::from_secs(5) {
            std::thread::sleep(std::time::Duration::from_millis(10));
        }

        assert!(done.load(Ordering::SeqCst), "task should have run");
        sched.shutdown(); // ensure cleanup
    }
}
