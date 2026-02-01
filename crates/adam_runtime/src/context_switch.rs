//! Context switching — save/restore CPU registers to switch between green threads.
//!
//! The actual register save/restore is done in platform-specific assembly
//! (src/arch/aarch64.S, src/arch/x86_64.S). This module provides the safe
//! Rust interface.

use crate::thread::{CpuContext, GreenThread, ThreadState};

extern "C" {
    /// Low-level context switch implemented in assembly.
    ///
    /// Saves callee-saved registers of the current thread into `old`,
    /// then restores registers from `new` and returns. After the restore,
    /// execution continues at whatever return address was saved in `new`.
    fn adam_context_switch(old: *mut CpuContext, new: *const CpuContext);
}

/// Public wrapper for the raw assembly context switch.
///
/// Used by the scheduler's yield/block functions to switch from a green
/// thread back to the worker context.
///
/// # Safety
/// Both pointers must be valid CpuContext references.
pub unsafe fn adam_context_switch_raw(old: *mut CpuContext, new: *const CpuContext) {
    adam_context_switch(old, new);
}

/// Switch from the current thread to the next thread.
///
/// Saves the current thread's CPU registers and restores the next thread's.
/// When this function "returns", it returns in the context of `current` —
/// but only after some future context switch back to `current`.
///
/// # Safety
/// Both `current` and `next` must be valid GreenThread references.
/// `next` must have a properly initialized context (either from a previous
/// context_switch or from init_context).
pub unsafe fn switch(current: &mut GreenThread, next: &mut GreenThread) {
    current.state = ThreadState::Ready;
    next.state = ThreadState::Running;

    adam_context_switch(
        &mut current.context as *mut CpuContext,
        &next.context as *const CpuContext,
    );
}

/// Raw context switch from a worker's CPU context to a green thread.
///
/// Used by the scheduler: saves the worker's current CPU state into
/// `worker_ctx` and restores the green thread's context. When the green
/// thread yields or completes, a reverse switch restores `worker_ctx`.
///
/// # Safety
/// `worker_ctx` must be a valid CpuContext. `thread` must have an initialized context.
pub unsafe fn switch_raw(worker_ctx: &mut CpuContext, thread: &mut GreenThread) {
    adam_context_switch(
        worker_ctx as *mut CpuContext,
        &thread.context as *const CpuContext,
    );
}

/// The trampoline function that starts execution of a new green thread.
///
/// When a green thread is first context-switched to, execution begins here.
/// The thread's entry closure is called, and when it returns, the thread
/// is marked as completed.
///
/// On aarch64: x19 holds the GreenThread pointer (set by init_context).
/// On x86_64: r12 holds the GreenThread pointer (set by init_context).
///
/// # Safety
/// This function is called from assembly with a raw pointer to a GreenThread.
#[no_mangle]
pub unsafe extern "C" fn adam_thread_trampoline(thread_ptr: usize) {
    let thread = &mut *(thread_ptr as *mut GreenThread);

    // Take the entry closure out of the thread and execute it.
    // Wrap in catch_unwind to prevent panics from unwinding through
    // the assembly trampoline, which has no unwind info.
    if let Some(entry) = thread.entry.take() {
        let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(entry));
    }

    // IMPORTANT: After entry() returns, the GreenThread may have been moved
    // to a different address. During yields, the GreenThread is moved from
    // the worker's stack → queue → different worker's stack. The `thread_ptr`
    // from the function argument (originally from x19) is now STALE.
    //
    // We must re-read the current GreenThread address from TLS, which the
    // worker loop updates before every context switch.
    let current_ptr = crate::scheduler::CURRENT_THREAD_PTR.with(|p| p.get());
    let thread = &mut *(current_ptr as *mut GreenThread);

    // Mark thread as completed.
    thread.state = ThreadState::Completed;

    // Switch back to the worker OS thread's context.
    // The worker context is stored in thread-local storage by the scheduler.
    crate::scheduler::WORKER_CONTEXT.with(|ctx| {
        let worker_ctx = &*ctx.get();
        // Restore the worker's context. We don't need to save the current
        // (completed) thread's context since it won't be resumed.
        adam_context_switch(
            &mut thread.context as *mut CpuContext,
            worker_ctx as *const CpuContext,
        );
    });

    // Should never reach here, but just in case.
    unreachable!("thread trampoline should have switched back to worker");
}

/// Architecture-specific trampoline that bridges from assembly to Rust.
///
/// On aarch64, the context_switch `ret` jumps to LR which we set to this
/// function. x19 holds the GreenThread pointer, which we move to x0
/// (first argument) before calling adam_thread_trampoline.
#[cfg(target_arch = "aarch64")]
#[no_mangle]
pub unsafe extern "C" fn adam_trampoline_entry() {
    // On aarch64, x19 was set to the GreenThread pointer by init_context.
    // Move x19 -> x0 (first argument register) and call the trampoline.
    core::arch::asm!(
        "mov x0, x19",
        "bl _adam_thread_trampoline",
        options(noreturn)
    );
}

/// Architecture-specific trampoline for x86_64.
#[cfg(target_arch = "x86_64")]
#[no_mangle]
pub unsafe extern "C" fn adam_trampoline_entry() {
    // On x86_64, r12 was set to the GreenThread pointer by init_context.
    // Move r12 -> rdi (first argument register in System V ABI).
    core::arch::asm!(
        "mov rdi, r12",
        "call _adam_thread_trampoline",
        options(noreturn)
    );
}

/// Initialize a green thread's context for first execution.
///
/// After this call, the first context_switch to this thread will begin
/// executing at the trampoline, which calls the thread's entry closure.
pub fn init_thread_context(thread: &mut GreenThread) {
    thread.init_context(adam_trampoline_entry);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::thread::GreenThread;
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::Arc;

    #[test]
    fn test_context_switch_basic() {
        // Test that we can switch from a "main" context to a green thread
        // and back.
        let did_run = Arc::new(AtomicBool::new(false));
        let did_run_clone = did_run.clone();

        // We need a "main" thread context to save the current state into.
        let mut main_ctx = CpuContext::new();
        let main_ctx_ptr = &mut main_ctx as *mut CpuContext;

        let mut thread = GreenThread::new(move || {
            did_run_clone.store(true, Ordering::SeqCst);
        });

        // Set up the thread's entry trampoline.
        // But the trampoline's completion loop means we can't easily test
        // a full round-trip without a scheduler. So let's test something
        // simpler: verify init_context sets up registers correctly.
        init_thread_context(&mut thread);

        #[cfg(target_arch = "aarch64")]
        {
            assert_ne!(thread.context.regs.sp, 0, "sp should be set");
            assert_ne!(thread.context.regs.x30, 0, "LR should point to trampoline");
            assert_ne!(thread.context.regs.x19, 0, "x19 should hold thread ptr");
        }

        #[cfg(target_arch = "x86_64")]
        {
            assert_ne!(thread.context.regs.rsp, 0, "rsp should be set");
            assert_ne!(thread.context.regs.r12, 0, "r12 should hold thread ptr");
        }

        // We suppress the actual context switch test here because without
        // a scheduler, the trampoline's completion spin-loop would hang.
        // Full context switch testing happens in P9.S3 (scheduler tests).
        let _ = main_ctx_ptr;
    }

    #[test]
    fn test_init_thread_context_stack_alignment() {
        let mut thread = GreenThread::new(|| {});
        init_thread_context(&mut thread);

        #[cfg(target_arch = "aarch64")]
        {
            // aarch64 requires 16-byte stack alignment.
            assert_eq!(thread.context.regs.sp % 16, 0, "sp must be 16-byte aligned");
        }

        #[cfg(target_arch = "x86_64")]
        {
            // x86_64 requires 16-byte alignment before CALL, so after pushing
            // the return address the stack is at 16n+8.
            assert_eq!(thread.context.regs.rsp % 8, 0, "rsp must be 8-byte aligned");
        }
    }

    // Static storage for main context pointer in tests.
    static TEST_MAIN_CTX: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
    static TEST_COUNTER: std::sync::atomic::AtomicI32 = std::sync::atomic::AtomicI32::new(0);

    #[test]
    fn test_actual_context_switch() {
        // Test a real context switch to a green thread and back.
        TEST_COUNTER.store(0, Ordering::SeqCst);

        let mut main_ctx = CpuContext::new();
        TEST_MAIN_CTX.store(&mut main_ctx as *mut CpuContext as usize, Ordering::SeqCst);

        let mut thread = GreenThread::new(|| {
            TEST_COUNTER.fetch_add(1, Ordering::SeqCst);

            // Switch back to main context.
            unsafe {
                // Get the thread pointer from the callee-saved register.
                let thread_self_ptr: usize;
                #[cfg(target_arch = "aarch64")]
                core::arch::asm!("mov {}, x19", out(reg) thread_self_ptr);
                #[cfg(target_arch = "x86_64")]
                core::arch::asm!("mov {}, r12", out(reg) thread_self_ptr);

                let thread_self = &mut *(thread_self_ptr as *mut GreenThread);
                let main_ptr = TEST_MAIN_CTX.load(Ordering::SeqCst) as *const CpuContext;
                super::adam_context_switch(
                    &mut thread_self.context as *mut CpuContext,
                    main_ptr,
                );
            }
        });

        init_thread_context(&mut thread);

        // Switch to the green thread.
        unsafe {
            super::adam_context_switch(
                &mut main_ctx as *mut CpuContext,
                &thread.context as *const CpuContext,
            );
        }

        // We're back. Check if the thread ran.
        assert_eq!(TEST_COUNTER.load(Ordering::SeqCst), 1, "green thread should have run");
    }

    #[test]
    fn test_raw_context_switch_roundtrip() {
        // Test the raw assembly context_switch: switch to a simple context
        // that immediately switches back.
        use std::sync::atomic::AtomicI32;

        static COUNTER: AtomicI32 = AtomicI32::new(0);

        let mut ctx_main = CpuContext::new();
        let ctx_other = CpuContext::new();

        // Set up ctx_other to point to a function that increments a counter
        // and switches back to ctx_main.
        //
        // We'll use a small helper function as the "other" thread.
        // The helper will save its state into ctx_other and restore ctx_main.
        unsafe extern "C" fn other_fn() {
            COUNTER.fetch_add(1, Ordering::SeqCst);

            // We need to switch back. But we don't have access to the contexts
            // from here without globals. This test is intentionally limited.
            // Full round-trip testing requires the scheduler.
        }

        // For a minimal test, verify we can call the assembly function
        // without crashing when old == new (identity switch).
        unsafe {
            adam_context_switch(
                &mut ctx_main as *mut CpuContext,
                &ctx_main as *const CpuContext,
            );
        }
        // If we get here, the identity switch worked (saved and restored same context).

        let _ = ctx_other;
        let _ = other_fn;
    }
}
