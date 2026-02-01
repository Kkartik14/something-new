//! Green thread (fiber/goroutine) data structures.
//!
//! Each green thread has its own stack and saved CPU context, allowing
//! cooperative multitasking via context switching.

use std::sync::atomic::{AtomicU64, Ordering};

pub type ThreadId = u64;

static NEXT_THREAD_ID: AtomicU64 = AtomicU64::new(1);

fn next_thread_id() -> ThreadId {
    NEXT_THREAD_ID.fetch_add(1, Ordering::Relaxed)
}

/// Default stack size: 64KB.
/// (Go uses 8KB with growable stacks; we use fixed stacks, so we need more.)
pub const DEFAULT_STACK_SIZE: usize = 64 * 1024;

/// Maximum stack size: 1MB.
pub const MAX_STACK_SIZE: usize = 1024 * 1024;

/// Minimum stack size: 4KB.
pub const MIN_STACK_SIZE: usize = 4 * 1024;

// ================================================================
// Thread State
// ================================================================

/// State of a green thread.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum ThreadState {
    /// Ready to be scheduled onto an OS thread.
    Ready = 0,
    /// Currently executing on an OS thread.
    Running = 1,
    /// Blocked waiting on a channel operation, select, or sleep.
    Blocked = 2,
    /// Finished execution.
    Completed = 3,
}

// ================================================================
// CPU Context (platform-specific saved registers)
// ================================================================

/// Saved CPU register context for context switching.
///
/// On context switch we save callee-saved registers of the current thread
/// and restore the next thread's registers. Caller-saved registers are
/// already saved/restored by the normal calling convention.
#[repr(C)]
#[derive(Clone)]
pub struct CpuContext {
    /// Platform-specific register storage.
    pub regs: RegisterFile,
}

/// aarch64 (ARM64) callee-saved registers.
///
/// ARM64 calling convention (AAPCS64) callee-saved:
/// - x19–x28 (general purpose)
/// - x29 (frame pointer, FP)
/// - x30 (link register, LR — return address)
/// - sp (stack pointer)
/// - d8–d15 (SIMD/FP lower 64 bits)
#[cfg(target_arch = "aarch64")]
#[repr(C)]
#[derive(Clone, Copy, Default)]
pub struct RegisterFile {
    pub sp: u64,
    pub x19: u64,
    pub x20: u64,
    pub x21: u64,
    pub x22: u64,
    pub x23: u64,
    pub x24: u64,
    pub x25: u64,
    pub x26: u64,
    pub x27: u64,
    pub x28: u64,
    pub x29: u64,   // FP
    pub x30: u64,   // LR (return address)
    pub d8: u64,
    pub d9: u64,
    pub d10: u64,
    pub d11: u64,
    pub d12: u64,
    pub d13: u64,
    pub d14: u64,
    pub d15: u64,
}

/// x86_64 callee-saved registers.
///
/// System V AMD64 ABI callee-saved:
/// - rbx, rbp, r12–r15
/// - rsp (stack pointer)
/// - return address on stack
#[cfg(target_arch = "x86_64")]
#[repr(C)]
#[derive(Clone, Copy, Default)]
pub struct RegisterFile {
    pub rsp: u64,
    pub rbx: u64,
    pub rbp: u64,
    pub r12: u64,
    pub r13: u64,
    pub r14: u64,
    pub r15: u64,
}

impl CpuContext {
    pub fn new() -> Self {
        CpuContext {
            regs: RegisterFile::default(),
        }
    }
}

impl Default for CpuContext {
    fn default() -> Self {
        Self::new()
    }
}

// ================================================================
// Stack
// ================================================================

/// A green thread's stack.
///
/// Allocated as a contiguous region with 16-byte alignment.
/// Stacks grow downward on both aarch64 and x86_64 — execution begins
/// at the top (highest address) of the allocated region.
pub struct Stack {
    /// Pointer to the bottom of the allocated region (lowest address).
    ptr: *mut u8,
    /// Current usable size in bytes.
    size: usize,
    /// Total allocated capacity in bytes.
    capacity: usize,
}

// Stack holds a raw pointer but is only accessed by the thread that owns it
// or by the scheduler during context switch (which is synchronized).
unsafe impl Send for Stack {}

impl Stack {
    /// Allocate a new stack with the given size.
    ///
    /// The size is rounded up to the nearest page boundary (4KB).
    pub fn new(size: usize) -> Self {
        let size = align_to_page(size.max(MIN_STACK_SIZE));
        let layout = std::alloc::Layout::from_size_align(size, 16)
            .expect("invalid stack layout");
        let ptr = unsafe { std::alloc::alloc(layout) };
        if ptr.is_null() {
            std::alloc::handle_alloc_error(layout);
        }
        Stack {
            ptr,
            size,
            capacity: size,
        }
    }

    /// Pointer to the bottom (lowest address) of the stack.
    pub fn bottom(&self) -> *mut u8 {
        self.ptr
    }

    /// Pointer to the top (highest address) of the stack.
    /// This is where execution begins on downward-growing stacks.
    /// The pointer is aligned to 16 bytes as required by both aarch64 and x86_64 ABIs.
    pub fn top(&self) -> *mut u8 {
        unsafe { self.ptr.add(self.size) }
    }

    /// The usable size of the stack in bytes.
    pub fn size(&self) -> usize {
        self.size
    }

    /// The total allocated capacity.
    pub fn capacity(&self) -> usize {
        self.capacity
    }
}

impl Drop for Stack {
    fn drop(&mut self) {
        if !self.ptr.is_null() {
            let layout = std::alloc::Layout::from_size_align(self.capacity, 16)
                .expect("invalid stack layout on dealloc");
            unsafe { std::alloc::dealloc(self.ptr, layout) };
            self.ptr = std::ptr::null_mut();
        }
    }
}

/// Round up to the nearest 4KB page boundary.
fn align_to_page(size: usize) -> usize {
    const PAGE_SIZE: usize = 4096;
    (size + PAGE_SIZE - 1) & !(PAGE_SIZE - 1)
}

// ================================================================
// Green Thread
// ================================================================

/// A green thread (fiber / goroutine).
///
/// Created by `spawn { ... }` in Adam code. Each green thread runs on a
/// small stack (default 8KB) and is cooperatively scheduled onto OS threads
/// by the work-stealing scheduler.
pub struct GreenThread {
    /// Unique thread identifier.
    pub id: ThreadId,
    /// Current execution state.
    pub state: ThreadState,
    /// Saved CPU register context for suspension/resumption.
    pub context: CpuContext,
    /// The thread's private stack.
    pub stack: Stack,
    /// Entry function for new (not-yet-started) threads.
    /// `None` after the thread has been started.
    pub entry: Option<Box<dyn FnOnce() + Send>>,
}

// GreenThread can be moved between OS threads by the scheduler.
unsafe impl Send for GreenThread {}

impl GreenThread {
    /// Create a new green thread that will execute the given closure.
    pub fn new(f: impl FnOnce() + Send + 'static) -> Self {
        let id = next_thread_id();
        let stack = Stack::new(DEFAULT_STACK_SIZE);
        GreenThread {
            id,
            state: ThreadState::Ready,
            context: CpuContext::new(),
            stack,
            entry: Some(Box::new(f)),
        }
    }

    /// Create a new green thread with a custom stack size.
    pub fn with_stack_size(f: impl FnOnce() + Send + 'static, stack_size: usize) -> Self {
        let size = stack_size.clamp(MIN_STACK_SIZE, MAX_STACK_SIZE);
        let id = next_thread_id();
        let stack = Stack::new(size);
        GreenThread {
            id,
            state: ThreadState::Ready,
            context: CpuContext::new(),
            stack,
            entry: Some(Box::new(f)),
        }
    }

    /// Update the stored thread pointer in the context to reflect the
    /// current address. Must be called after moving the GreenThread and
    /// before context-switching to it.
    pub fn update_thread_ptr(&mut self) {
        #[cfg(target_arch = "aarch64")]
        {
            self.context.regs.x19 = self as *mut GreenThread as u64;
        }
        #[cfg(target_arch = "x86_64")]
        {
            self.context.regs.r12 = self as *mut GreenThread as u64;
        }
    }

    /// Set up the initial CPU context so that the first context switch
    /// into this thread will begin executing the entry function.
    ///
    /// Must be called before the first context switch to this thread.
    /// The `trampoline` argument is a function pointer that will be called
    /// with a pointer to this GreenThread as its argument.
    pub fn init_context(&mut self, trampoline: unsafe extern "C" fn()) {
        // Stack pointer: top of stack, aligned to 16 bytes.
        let sp = (self.stack.top() as usize) & !0xF;

        #[cfg(target_arch = "aarch64")]
        {
            self.context.regs.sp = sp as u64;
            // LR (x30) = trampoline function. When context_switch restores
            // this context and returns, execution jumps to the trampoline.
            self.context.regs.x30 = trampoline as u64;
            // x19 = pointer to this GreenThread (passed to trampoline as arg).
            // The trampoline will move this into x0 before calling the entry.
            self.context.regs.x19 = self as *mut GreenThread as u64;
        }

        #[cfg(target_arch = "x86_64")]
        {
            // x86_64: we push the return address onto the stack.
            // After context_switch restores rsp and does `ret`, it jumps to trampoline.
            let sp = sp - 8; // make room for return address
            unsafe {
                *(sp as *mut u64) = trampoline as u64;
            }
            self.context.regs.rsp = sp as u64;
            // r12 = pointer to this GreenThread.
            self.context.regs.r12 = self as *mut GreenThread as u64;
        }
    }
}

// ================================================================
// Tests
// ================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_thread_ids_unique() {
        let t1 = GreenThread::new(|| {});
        let t2 = GreenThread::new(|| {});
        let t3 = GreenThread::new(|| {});
        assert_ne!(t1.id, t2.id);
        assert_ne!(t2.id, t3.id);
        assert_ne!(t1.id, t3.id);
    }

    #[test]
    fn test_thread_initial_state() {
        let t = GreenThread::new(|| {});
        assert_eq!(t.state, ThreadState::Ready);
        assert!(t.entry.is_some());
    }

    #[test]
    fn test_default_stack_size() {
        let t = GreenThread::new(|| {});
        assert_eq!(t.stack.size(), DEFAULT_STACK_SIZE);
    }

    #[test]
    fn test_custom_stack_size() {
        let t = GreenThread::with_stack_size(|| {}, 16 * 1024);
        assert_eq!(t.stack.size(), 16 * 1024);
    }

    #[test]
    fn test_stack_size_clamped_min() {
        let t = GreenThread::with_stack_size(|| {}, 100);
        assert_eq!(t.stack.size(), MIN_STACK_SIZE);
    }

    #[test]
    fn test_stack_size_clamped_max() {
        let t = GreenThread::with_stack_size(|| {}, 10 * 1024 * 1024);
        assert_eq!(t.stack.size(), align_to_page(MAX_STACK_SIZE));
    }

    #[test]
    fn test_stack_alignment() {
        let stack = Stack::new(DEFAULT_STACK_SIZE);
        assert_eq!(stack.top() as usize % 16, 0, "stack top must be 16-byte aligned");
        assert_eq!(stack.bottom() as usize % 16, 0, "stack bottom must be 16-byte aligned");
    }

    #[test]
    fn test_stack_top_above_bottom() {
        let stack = Stack::new(DEFAULT_STACK_SIZE);
        assert!(stack.top() as usize > stack.bottom() as usize);
        assert_eq!(
            stack.top() as usize - stack.bottom() as usize,
            stack.size()
        );
    }

    #[test]
    fn test_stack_drop() {
        // Just ensure no crash on drop.
        let _stack = Stack::new(DEFAULT_STACK_SIZE);
    }

    #[test]
    fn test_green_thread_size() {
        let size = std::mem::size_of::<GreenThread>();
        assert!(
            size <= 256,
            "GreenThread struct is {} bytes, should be <= 256",
            size
        );
    }

    #[test]
    fn test_cpu_context_size() {
        let ctx_size = std::mem::size_of::<CpuContext>();
        // aarch64: 21 registers * 8 bytes = 168 bytes
        // x86_64: 7 registers * 8 bytes = 56 bytes
        #[cfg(target_arch = "aarch64")]
        assert_eq!(ctx_size, 168);
        #[cfg(target_arch = "x86_64")]
        assert_eq!(ctx_size, 56);
    }

    #[test]
    fn test_create_many_threads() {
        // Verify we can create 1000 threads without issues.
        let threads: Vec<GreenThread> = (0..1000)
            .map(|i| GreenThread::new(move || { let _ = i; }))
            .collect();
        assert_eq!(threads.len(), 1000);
        // All IDs should be unique.
        let mut ids: Vec<ThreadId> = threads.iter().map(|t| t.id).collect();
        ids.sort();
        ids.dedup();
        assert_eq!(ids.len(), 1000);
    }

    #[test]
    fn test_thread_state_transitions() {
        let mut t = GreenThread::new(|| {});
        assert_eq!(t.state, ThreadState::Ready);
        t.state = ThreadState::Running;
        assert_eq!(t.state, ThreadState::Running);
        t.state = ThreadState::Blocked;
        assert_eq!(t.state, ThreadState::Blocked);
        t.state = ThreadState::Ready;
        assert_eq!(t.state, ThreadState::Ready);
        t.state = ThreadState::Completed;
        assert_eq!(t.state, ThreadState::Completed);
    }

    #[test]
    fn test_align_to_page() {
        assert_eq!(align_to_page(1), 4096);
        assert_eq!(align_to_page(4096), 4096);
        assert_eq!(align_to_page(4097), 8192);
        assert_eq!(align_to_page(8192), 8192);
    }

    #[test]
    fn test_context_default() {
        let ctx = CpuContext::new();
        assert_eq!(ctx.regs.sp, 0);
        #[cfg(target_arch = "aarch64")]
        assert_eq!(ctx.regs.x30, 0);
        #[cfg(target_arch = "x86_64")]
        assert_eq!(ctx.regs.rsp, 0);
    }

    #[test]
    fn test_thread_send() {
        // Verify GreenThread is Send (can be moved between OS threads).
        fn assert_send<T: Send>() {}
        assert_send::<GreenThread>();
    }
}
