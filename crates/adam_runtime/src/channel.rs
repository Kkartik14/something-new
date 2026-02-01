//! Typed channels — primary communication mechanism between green threads.
//!
//! Two variants:
//! - **Unbuffered** (capacity=0): send blocks until a receiver is ready, recv blocks until a sender is ready.
//! - **Buffered** (capacity>0): send blocks only when buffer is full, recv blocks when buffer is empty.
//!
//! Blocking is cooperative: the green thread is set to Blocked state and the
//! scheduler picks another ready thread. When the complementary operation
//! happens, the blocked thread is woken and re-queued.

use std::any::Any;
use std::collections::VecDeque;
use std::marker::PhantomData;
use std::sync::{Arc, Mutex};

use crate::scheduler;
use crate::thread::ThreadId;

// ================================================================
// Waiter types — used for rendezvous between senders and receivers
// ================================================================

/// A sender waiting to deliver a value.
struct SenderWaiter {
    /// The ThreadId of the blocked sender.
    thread_id: ThreadId,
    /// The value to be sent. Taken by the receiver.
    value: Mutex<Option<Box<dyn Any + Send>>>,
}

/// A receiver waiting for a value.
struct ReceiverWaiter {
    /// The ThreadId of the blocked receiver.
    thread_id: ThreadId,
    /// Slot where the sender deposits the value.
    value: Mutex<Option<Box<dyn Any + Send>>>,
}

// ================================================================
// Raw Channel (type-erased)
// ================================================================

/// A type-erased channel that stores values as `Box<dyn Any + Send>`.
///
/// This is the core implementation. `TypedChannel<T>` wraps this
/// with type safety.
pub struct Channel {
    inner: Mutex<ChannelInner>,
}

struct ChannelInner {
    /// Buffer for buffered channels.
    buffer: VecDeque<Box<dyn Any + Send>>,
    /// Maximum buffer capacity. 0 = unbuffered (synchronous).
    capacity: usize,
    /// Senders waiting to deliver a value (blocked).
    senders_waiting: VecDeque<Arc<SenderWaiter>>,
    /// Receivers waiting for a value (blocked).
    receivers_waiting: VecDeque<Arc<ReceiverWaiter>>,
    /// Whether the channel has been closed.
    closed: bool,
    /// Threads blocked in select statements watching this channel.
    /// Woken when channel state changes (new value, space freed, close).
    select_waiters: Vec<ThreadId>,
}

impl Channel {
    /// Create a new channel with the given buffer capacity.
    /// Capacity 0 creates an unbuffered (synchronous) channel.
    pub fn new(capacity: usize) -> Arc<Self> {
        Arc::new(Channel {
            inner: Mutex::new(ChannelInner {
                buffer: VecDeque::with_capacity(capacity),
                capacity,
                senders_waiting: VecDeque::new(),
                receivers_waiting: VecDeque::new(),
                closed: false,
                select_waiters: Vec::new(),
            }),
        })
    }

    /// Send a type-erased value through the channel. Blocks if needed.
    ///
    /// Panics if the channel is closed.
    pub fn send_any(&self, value: Box<dyn Any + Send>) {
        let mut value = Some(value);

        loop {
            let mut inner = self.inner.lock().unwrap();

            if inner.closed {
                panic!("send on closed channel");
            }

            let val = value.take().unwrap();

            // Fast path: is there a receiver waiting?
            if let Some(receiver) = inner.receivers_waiting.pop_front() {
                // Direct handoff — deposit value in receiver's slot and wake receiver.
                let receiver_id = receiver.thread_id;
                *receiver.value.lock().unwrap() = Some(val);
                drop(inner);
                scheduler::get_scheduler().wake(receiver_id);
                return;
            }

            // Buffered channel with space?
            if inner.buffer.len() < inner.capacity {
                inner.buffer.push_back(val);
                // Notify select waiters that a value is now available.
                Channel::wake_select_waiters(&mut inner);
                return;
            }

            // Must block. Register as a waiting sender.
            let thread_id = scheduler::current_thread_id();
            let waiter = Arc::new(SenderWaiter {
                thread_id,
                value: Mutex::new(Some(val)),
            });
            inner.senders_waiting.push_back(waiter.clone());
            drop(inner);

            // Block until a receiver takes our value and wakes us.
            scheduler::block_current();

            // After waking, check if value was taken.
            let taken = waiter.value.lock().unwrap().is_none();
            if taken {
                return;
            }
            // Value not taken (spurious wake or close) — retry with the value.
            value = waiter.value.lock().unwrap().take();
            if value.is_none() {
                return; // Value was taken between our checks.
            }
        }
    }

    /// Receive a type-erased value from the channel. Blocks if needed.
    ///
    /// Returns `None` if the channel is closed and empty.
    pub fn recv_any(&self) -> Option<Box<dyn Any + Send>> {
        loop {
            let mut inner = self.inner.lock().unwrap();

            // Fast path: is there a sender waiting?
            if let Some(sender) = inner.senders_waiting.pop_front() {
                let sender_id = sender.thread_id;
                let value = sender.value.lock().unwrap().take();

                if !inner.buffer.is_empty() {
                    let buffered_value = inner.buffer.pop_front();
                    if let Some(sender_val) = value {
                        inner.buffer.push_back(sender_val);
                    }
                    // Notify select waiters — space may have changed.
                    Channel::wake_select_waiters(&mut inner);
                    drop(inner);
                    scheduler::get_scheduler().wake(sender_id);
                    return buffered_value;
                }

                // Notify select waiters — a sender slot freed up.
                Channel::wake_select_waiters(&mut inner);
                drop(inner);
                scheduler::get_scheduler().wake(sender_id);
                return value;
            }

            // Check buffer.
            if let Some(value) = inner.buffer.pop_front() {
                // Notify select waiters — buffer space freed.
                Channel::wake_select_waiters(&mut inner);
                return Some(value);
            }

            // Channel closed and empty?
            if inner.closed {
                return None;
            }

            // Must block. Register as a waiting receiver.
            let thread_id = scheduler::current_thread_id();
            let waiter = Arc::new(ReceiverWaiter {
                thread_id,
                value: Mutex::new(None),
            });
            inner.receivers_waiting.push_back(waiter.clone());
            drop(inner);

            // Block until a sender deposits a value and wakes us.
            scheduler::block_current();

            // After waking, check if a value was deposited.
            let value = waiter.value.lock().unwrap().take();
            if value.is_some() {
                return value;
            }

            // Spurious wake or close — check close.
            if self.inner.lock().unwrap().closed {
                return None;
            }
            // Otherwise retry.
        }
    }

    /// Try to send without blocking. Returns true if sent successfully.
    pub fn try_send_any(&self, value: Box<dyn Any + Send>) -> bool {
        let mut inner = self.inner.lock().unwrap();

        if inner.closed {
            return false;
        }

        // Direct handoff to a waiting receiver?
        if let Some(receiver) = inner.receivers_waiting.pop_front() {
            let receiver_id = receiver.thread_id;
            *receiver.value.lock().unwrap() = Some(value);
            drop(inner);
            scheduler::get_scheduler().wake(receiver_id);
            return true;
        }

        // Buffer has space?
        if inner.buffer.len() < inner.capacity {
            inner.buffer.push_back(value);
            Channel::wake_select_waiters(&mut inner);
            return true;
        }

        false
    }

    /// Try to receive without blocking. Returns None if no value available.
    pub fn try_recv_any(&self) -> Option<Box<dyn Any + Send>> {
        let mut inner = self.inner.lock().unwrap();

        // Waiting sender?
        if let Some(sender) = inner.senders_waiting.pop_front() {
            let sender_id = sender.thread_id;
            let value = sender.value.lock().unwrap().take();

            if !inner.buffer.is_empty() {
                let buffered_value = inner.buffer.pop_front();
                if let Some(sender_val) = value {
                    inner.buffer.push_back(sender_val);
                }
                Channel::wake_select_waiters(&mut inner);
                drop(inner);
                scheduler::get_scheduler().wake(sender_id);
                return buffered_value;
            }

            Channel::wake_select_waiters(&mut inner);
            drop(inner);
            scheduler::get_scheduler().wake(sender_id);
            return value;
        }

        // Buffer?
        if let Some(value) = inner.buffer.pop_front() {
            Channel::wake_select_waiters(&mut inner);
            return Some(value);
        }

        None
    }

    /// Check if a send would succeed without blocking.
    ///
    /// Used by select to probe channel readiness.
    pub fn can_send(&self) -> bool {
        let inner = self.inner.lock().unwrap();
        if inner.closed {
            return false;
        }
        // Can send if there's a waiting receiver or buffer has space.
        !inner.receivers_waiting.is_empty() || inner.buffer.len() < inner.capacity
    }

    /// Check if a recv would succeed without blocking.
    ///
    /// Used by select to probe channel readiness.
    pub fn can_recv(&self) -> bool {
        let inner = self.inner.lock().unwrap();
        // Can recv if there's a waiting sender, buffer has values, or channel is closed.
        !inner.senders_waiting.is_empty() || !inner.buffer.is_empty() || inner.closed
    }

    /// Try to send without blocking, returning the value on failure.
    ///
    /// Unlike try_send_any which drops the value on failure, this returns
    /// it back to the caller. Used by select to avoid losing values.
    pub fn try_send_reclaim(&self, value: Box<dyn Any + Send>) -> Result<(), Box<dyn Any + Send>> {
        let mut inner = self.inner.lock().unwrap();

        if inner.closed {
            return Err(value);
        }

        // Direct handoff to a waiting receiver?
        if let Some(receiver) = inner.receivers_waiting.pop_front() {
            let receiver_id = receiver.thread_id;
            *receiver.value.lock().unwrap() = Some(value);
            drop(inner);
            scheduler::get_scheduler().wake(receiver_id);
            return Ok(());
        }

        // Buffer has space?
        if inner.buffer.len() < inner.capacity {
            inner.buffer.push_back(value);
            return Ok(());
        }

        Err(value)
    }

    /// Register a thread to be woken when this channel's state changes.
    ///
    /// Used by select to block until any watched channel is ready.
    pub fn register_select_waiter(&self, thread_id: ThreadId) {
        let mut inner = self.inner.lock().unwrap();
        inner.select_waiters.push(thread_id);
    }

    /// Remove a select waiter registration.
    pub fn unregister_select_waiter(&self, thread_id: ThreadId) {
        let mut inner = self.inner.lock().unwrap();
        inner.select_waiters.retain(|&id| id != thread_id);
    }

    /// Wake all select waiters (called internally when channel state changes).
    fn wake_select_waiters(inner: &mut ChannelInner) {
        if !inner.select_waiters.is_empty() {
            let sched = scheduler::get_scheduler();
            for &id in &inner.select_waiters {
                sched.wake(id);
            }
            inner.select_waiters.clear();
        }
    }

    /// Close the channel.
    ///
    /// Waiting receivers will receive None. Waiting senders will panic
    /// when they resume.
    pub fn close(&self) {
        let mut inner = self.inner.lock().unwrap();
        inner.closed = true;

        // Wake select waiters so they observe the close.
        Channel::wake_select_waiters(&mut inner);

        // Collect all waiting thread IDs to wake.
        let sender_ids: Vec<ThreadId> = inner
            .senders_waiting
            .drain(..)
            .map(|w| w.thread_id)
            .collect();
        let receiver_ids: Vec<ThreadId> = inner
            .receivers_waiting
            .drain(..)
            .map(|w| w.thread_id)
            .collect();
        drop(inner);

        // Wake all blocked threads so they can observe the close.
        let sched = scheduler::get_scheduler();
        for id in sender_ids {
            sched.wake(id);
        }
        for id in receiver_ids {
            sched.wake(id);
        }
    }

    /// Check if the channel is closed.
    pub fn is_closed(&self) -> bool {
        self.inner.lock().unwrap().closed
    }

    /// Number of values currently buffered.
    pub fn len(&self) -> usize {
        self.inner.lock().unwrap().buffer.len()
    }

    /// Whether the buffer is empty.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

// ================================================================
// TypedChannel<T> — type-safe wrapper
// ================================================================

/// A typed channel for sending values of type T between green threads.
///
/// Created via `TypedChannel::new(capacity)`. Clone to share between threads.
pub struct TypedChannel<T: Send + 'static> {
    raw: Arc<Channel>,
    _phantom: PhantomData<T>,
}

impl<T: Send + 'static> TypedChannel<T> {
    /// Create a new typed channel with the given buffer capacity.
    /// Capacity 0 creates an unbuffered (synchronous) channel.
    pub fn new(capacity: usize) -> Self {
        TypedChannel {
            raw: Channel::new(capacity),
            _phantom: PhantomData,
        }
    }

    /// Send a value through the channel. Blocks if needed.
    pub fn send(&self, value: T) {
        self.raw.send_any(Box::new(value));
    }

    /// Receive a value from the channel. Blocks if needed.
    /// Returns None if the channel is closed and empty.
    pub fn recv(&self) -> Option<T> {
        self.raw.recv_any().map(|v| *v.downcast::<T>().unwrap())
    }

    /// Try to send without blocking.
    pub fn try_send(&self, value: T) -> bool {
        self.raw.try_send_any(Box::new(value))
    }

    /// Try to receive without blocking.
    pub fn try_recv(&self) -> Option<T> {
        self.raw.try_recv_any().map(|v| *v.downcast::<T>().unwrap())
    }

    /// Close the channel.
    pub fn close(&self) {
        self.raw.close();
    }

    /// Check if the channel is closed.
    pub fn is_closed(&self) -> bool {
        self.raw.is_closed()
    }

    /// Get a reference to the underlying raw channel (for select).
    pub fn raw(&self) -> &Arc<Channel> {
        &self.raw
    }
}

impl<T: Send + 'static> Clone for TypedChannel<T> {
    fn clone(&self) -> Self {
        TypedChannel {
            raw: self.raw.clone(),
            _phantom: PhantomData,
        }
    }
}

// TypedChannel is Send+Sync because the inner Channel is protected by Mutex.
unsafe impl<T: Send + 'static> Send for TypedChannel<T> {}
unsafe impl<T: Send + 'static> Sync for TypedChannel<T> {}

// ================================================================
// C FFI — called by compiled Adam code
// ================================================================

/// Create a new channel with the given capacity.
/// Returns an Arc<Channel> leaked as a raw pointer.
#[no_mangle]
pub extern "C" fn __adam_chan_create(capacity: usize) -> *const Channel {
    let ch = Channel::new(capacity);
    Arc::into_raw(ch)
}

/// Clone a channel reference (increment refcount).
#[no_mangle]
pub unsafe extern "C" fn __adam_chan_clone(ch: *const Channel) -> *const Channel {
    let arc = Arc::from_raw(ch);
    let cloned = arc.clone();
    // Don't drop the original.
    std::mem::forget(arc);
    Arc::into_raw(cloned)
}

/// Send raw bytes through a channel.
/// `val_ptr` points to the value, `val_size` is its size in bytes.
#[no_mangle]
pub unsafe extern "C" fn __adam_chan_send(ch: *const Channel, val_ptr: *const u8, val_size: usize) {
    let ch = &*ch;
    let bytes = std::slice::from_raw_parts(val_ptr, val_size).to_vec();
    ch.send_any(Box::new(bytes));
}

/// Receive raw bytes from a channel.
/// Writes the received bytes to `val_ptr`. Returns true if a value was received.
#[no_mangle]
pub unsafe extern "C" fn __adam_chan_recv(
    ch: *const Channel,
    val_ptr: *mut u8,
    val_size: usize,
) -> bool {
    let ch = &*ch;
    match ch.recv_any() {
        Some(boxed) => {
            let bytes = boxed.downcast::<Vec<u8>>().unwrap();
            let copy_len = bytes.len().min(val_size);
            std::ptr::copy_nonoverlapping(bytes.as_ptr(), val_ptr, copy_len);
            true
        }
        None => false,
    }
}

/// Close a channel.
#[no_mangle]
pub unsafe extern "C" fn __adam_chan_close(ch: *const Channel) {
    let ch = &*ch;
    ch.close();
}

/// Drop a channel reference (decrement refcount).
#[no_mangle]
pub unsafe extern "C" fn __adam_chan_drop(ch: *const Channel) {
    let _ = Arc::from_raw(ch);
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

    // ---- Basic creation tests ----

    #[test]
    fn test_channel_creation_unbuffered() {
        let ch = Channel::new(0);
        assert!(!ch.is_closed());
        assert_eq!(ch.len(), 0);
    }

    #[test]
    fn test_channel_creation_buffered() {
        let ch = Channel::new(10);
        assert!(!ch.is_closed());
        assert_eq!(ch.len(), 0);
    }

    #[test]
    fn test_typed_channel_clone() {
        let ch: TypedChannel<i32> = TypedChannel::new(5);
        let ch2 = ch.clone();
        assert!(!ch.is_closed());
        assert!(!ch2.is_closed());
    }

    // ---- Buffered channel (non-blocking path) ----

    #[test]
    fn test_buffered_try_send_recv() {
        // Buffered channels can send/recv without blocking if buffer isn't full/empty.
        // Use try_send/try_recv which don't require a scheduler.
        let ch = Channel::new(3);
        assert!(ch.try_send_any(Box::new(1i32)));
        assert!(ch.try_send_any(Box::new(2i32)));
        assert!(ch.try_send_any(Box::new(3i32)));
        // Buffer full.
        assert!(!ch.try_send_any(Box::new(4i32)));

        let v1 = ch.try_recv_any().unwrap().downcast::<i32>().unwrap();
        assert_eq!(*v1, 1);
        let v2 = ch.try_recv_any().unwrap().downcast::<i32>().unwrap();
        assert_eq!(*v2, 2);
        let v3 = ch.try_recv_any().unwrap().downcast::<i32>().unwrap();
        assert_eq!(*v3, 3);
        // Buffer empty.
        assert!(ch.try_recv_any().is_none());
    }

    #[test]
    fn test_typed_buffered_try_send_recv() {
        let ch: TypedChannel<String> = TypedChannel::new(2);
        assert!(ch.try_send("hello".to_string()));
        assert!(ch.try_send("world".to_string()));
        assert!(!ch.try_send("overflow".to_string()));

        assert_eq!(ch.try_recv(), Some("hello".to_string()));
        assert_eq!(ch.try_recv(), Some("world".to_string()));
        assert_eq!(ch.try_recv(), None);
    }

    #[test]
    fn test_channel_close() {
        let ch = Channel::new(1);
        ch.try_send_any(Box::new(42i32));
        ch.close();
        assert!(ch.is_closed());
        // Can still receive buffered values.
        let v = ch.try_recv_any().unwrap().downcast::<i32>().unwrap();
        assert_eq!(*v, 42);
        // After buffer drained, recv returns None.
        assert!(ch.try_recv_any().is_none());
    }

    #[test]
    fn test_try_send_on_closed_channel() {
        let ch = Channel::new(1);
        ch.close();
        assert!(!ch.try_send_any(Box::new(1i32)));
    }

    // ---- Blocking tests (require scheduler) ----

    #[test]
    fn test_unbuffered_send_recv() {
        // Unbuffered channel: sender blocks until receiver is ready.
        let result = StdArc::new(AtomicI32::new(0));
        let result2 = result.clone();

        run_with_scheduler(2, move |sched| {
            let ch: TypedChannel<i32> = TypedChannel::new(0);
            let ch2 = ch.clone();

            // Spawn receiver.
            sched.spawn(move || {
                let val = ch2.recv().unwrap();
                result2.store(val, Ordering::SeqCst);
            });

            // Send (will block until receiver is ready).
            ch.send(42);
        });

        assert_eq!(result.load(Ordering::SeqCst), 42);
    }

    #[test]
    fn test_unbuffered_recv_then_send() {
        // Receiver blocks first, then sender delivers.
        let result = StdArc::new(AtomicI32::new(0));
        let result2 = result.clone();

        run_with_scheduler(2, move |sched| {
            let ch: TypedChannel<i32> = TypedChannel::new(0);
            let ch2 = ch.clone();

            // Spawn sender (will arrive after receiver blocks).
            sched.spawn(move || {
                ch2.send(99);
            });

            // Recv (blocks until sender sends).
            let val = ch.recv().unwrap();
            result2.store(val, Ordering::SeqCst);
        });

        assert_eq!(result.load(Ordering::SeqCst), 99);
    }

    #[test]
    fn test_buffered_blocking_send() {
        // Buffer of size 1: first send doesn't block, second blocks.
        let result = StdArc::new(AtomicI32::new(0));
        let result2 = result.clone();

        run_with_scheduler(2, move |sched| {
            let ch: TypedChannel<i32> = TypedChannel::new(1);
            let ch2 = ch.clone();

            // Spawn receiver that takes both values.
            sched.spawn(move || {
                let v1 = ch2.recv().unwrap();
                let v2 = ch2.recv().unwrap();
                result2.store(v1 + v2, Ordering::SeqCst);
            });

            ch.send(10); // Goes into buffer (no block).
            ch.send(20); // Buffer full — blocks until receiver takes.
        });

        assert_eq!(result.load(Ordering::SeqCst), 30);
    }

    #[test]
    fn test_multiple_senders_receivers() {
        let sum = StdArc::new(AtomicI32::new(0));
        let sum2 = sum.clone();

        run_with_scheduler(4, move |sched| {
            let ch: TypedChannel<i32> = TypedChannel::new(0);

            // 5 senders, each sending their index.
            for i in 0..5 {
                let ch_clone = ch.clone();
                sched.spawn(move || {
                    ch_clone.send(i);
                });
            }

            // 1 receiver collecting all values.
            let ch_recv = ch.clone();
            let s = sum2.clone();
            sched.spawn(move || {
                for _ in 0..5 {
                    let val = ch_recv.recv().unwrap();
                    s.fetch_add(val, Ordering::SeqCst);
                }
            });
        });

        // Sum of 0+1+2+3+4 = 10
        assert_eq!(sum.load(Ordering::SeqCst), 10);
    }

    #[test]
    fn test_channel_ping_pong() {
        // Two threads ping-pong a value back and forth.
        let final_val = StdArc::new(AtomicI32::new(0));
        let final_val2 = final_val.clone();

        run_with_scheduler(2, move |sched| {
            let ch1: TypedChannel<i32> = TypedChannel::new(0);
            let ch2: TypedChannel<i32> = TypedChannel::new(0);

            let ch1_send = ch1.clone();
            let ch2_recv = ch2.clone();
            let ch1_recv = ch1.clone();
            let ch2_send = ch2.clone();

            // Thread A: send on ch1, recv from ch2.
            sched.spawn(move || {
                ch1_send.send(1);
                let val = ch2_recv.recv().unwrap();
                final_val2.store(val, Ordering::SeqCst);
            });

            // Thread B: recv from ch1, add 1, send on ch2.
            sched.spawn(move || {
                let val = ch1_recv.recv().unwrap();
                ch2_send.send(val + 1);
            });
        });

        assert_eq!(final_val.load(Ordering::SeqCst), 2);
    }

    #[test]
    fn test_channel_close_wakes_receivers() {
        let got_none = StdArc::new(AtomicI32::new(0));
        let got_none2 = got_none.clone();

        run_with_scheduler(2, move |sched| {
            let ch: TypedChannel<i32> = TypedChannel::new(0);
            let ch2 = ch.clone();

            // Receiver will block, then get None when channel is closed.
            sched.spawn(move || {
                let result = ch2.recv();
                if result.is_none() {
                    got_none2.store(1, Ordering::SeqCst);
                }
            });

            // Give receiver time to block, then close.
            scheduler::yield_current();
            ch.close();
        });

        assert_eq!(got_none.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn test_channel_many_values() {
        // Send 100 values through a buffered channel.
        let sum = StdArc::new(AtomicI32::new(0));
        let sum2 = sum.clone();

        run_with_scheduler(2, move |sched| {
            let ch: TypedChannel<i32> = TypedChannel::new(10);
            let ch2 = ch.clone();

            // Sender.
            sched.spawn(move || {
                for i in 0..100 {
                    ch2.send(i);
                }
            });

            // Receiver.
            let s = sum2;
            for _ in 0..100 {
                let val = ch.recv().unwrap();
                s.fetch_add(val, Ordering::SeqCst);
            }
        });

        // Sum of 0..100 = 4950
        assert_eq!(sum.load(Ordering::SeqCst), 4950);
    }

    #[test]
    fn test_unbuffered_try_operations() {
        // Unbuffered: try_send always fails without a waiting receiver.
        let ch = Channel::new(0);
        assert!(!ch.try_send_any(Box::new(1i32)));
        assert!(ch.try_recv_any().is_none());
    }

    #[test]
    fn test_channel_fifo_order() {
        // Verify values come out in FIFO order for buffered channels.
        let ch: TypedChannel<i32> = TypedChannel::new(10);
        for i in 0..10 {
            ch.try_send(i);
        }
        for i in 0..10 {
            assert_eq!(ch.try_recv(), Some(i));
        }
    }

    #[test]
    fn test_channel_stress() {
        // Stress test: many senders, many receivers, through a buffered channel.
        let total_sent = StdArc::new(AtomicI32::new(0));
        let total_recv = StdArc::new(AtomicI32::new(0));
        let ts = total_sent.clone();
        let tr = total_recv.clone();

        run_with_scheduler(2, move |sched| {
            let ch: TypedChannel<i32> = TypedChannel::new(16);

            // 10 senders, each sending 5 values in a loop.
            for _ in 0..10 {
                let ch_clone = ch.clone();
                let ts = ts.clone();
                sched.spawn(move || {
                    for _ in 0..3 {
                        ch_clone.send(1);
                        ts.fetch_add(1, Ordering::SeqCst);
                    }
                });
            }

            // 10 receivers, each receiving 5 values in a loop.
            for _ in 0..10 {
                let ch_clone = ch.clone();
                let tr = tr.clone();
                sched.spawn(move || {
                    for _ in 0..3 {
                        let val = ch_clone.recv().unwrap();
                        tr.fetch_add(val, Ordering::SeqCst);
                    }
                });
            }
        });

        assert_eq!(total_sent.load(Ordering::SeqCst), 30);
        assert_eq!(total_recv.load(Ordering::SeqCst), 30);
    }
}
