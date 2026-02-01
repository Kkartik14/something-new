//! Select statement — wait on multiple channel operations simultaneously.
//!
//! `select` evaluates multiple channel operations and executes the first
//! one that is ready. If multiple are ready, one is chosen at random for
//! fairness. If none are ready, the green thread blocks until one becomes
//! ready (or a timeout fires).
//!
//! ```adam
//! select {
//!     val := ch1.recv() => process(val)
//!     ch2.send(result) => {}
//!     after 1.seconds => print("timeout")
//! }
//! ```

use std::any::Any;
use std::sync::Arc;
use std::time::{Duration, Instant};

use crate::channel::Channel;
use crate::scheduler;

// ================================================================
// Select types
// ================================================================

/// A single case (arm) in a select statement.
pub enum SelectCase {
    /// Receive from a channel.
    Recv {
        channel: Arc<Channel>,
    },
    /// Send a value to a channel. The value is wrapped in Option so it
    /// can be moved out on success without cloning.
    Send {
        channel: Arc<Channel>,
        value: Option<Box<dyn Any + Send>>,
    },
    /// Default case — fires immediately if no other case is ready.
    Default,
}

/// The result of a select operation.
pub enum SelectResult {
    /// A recv case at the given index completed. Value is None if channel was closed.
    Recv {
        index: usize,
        value: Option<Box<dyn Any + Send>>,
    },
    /// A send case at the given index completed.
    Sent {
        index: usize,
    },
    /// The timeout expired before any case was ready.
    Timeout,
    /// The default case was selected (no other case was ready).
    Default,
}

// ================================================================
// Select implementation
// ================================================================

/// Simple xorshift64 PRNG for randomizing case order.
fn xorshift64(state: &mut u64) -> u64 {
    let mut x = *state;
    x ^= x << 13;
    x ^= x >> 7;
    x ^= x << 17;
    *state = x;
    x
}

/// Execute a select statement over the given cases.
///
/// Tries each case in random order. If one is immediately ready, it
/// is executed and the result returned. If a Default case is present
/// and no other case is ready, Default is returned. Otherwise, the
/// green thread blocks until a case becomes ready or the timeout fires.
pub fn select(cases: &mut [SelectCase], timeout: Option<Duration>) -> SelectResult {
    assert!(!cases.is_empty(), "select requires at least one case");

    let start = Instant::now();

    // Build index arrays for each case type.
    let mut channel_indices: Vec<usize> = Vec::new();
    let mut has_default = false;

    for (i, case) in cases.iter().enumerate() {
        match case {
            SelectCase::Default => {
                has_default = true;
            }
            _ => {
                channel_indices.push(i);
            }
        }
    }

    // PRNG seed from thread ID and timestamp for randomness.
    let mut rng_state = (scheduler::current_thread_id() as u64)
        .wrapping_mul(6364136223846793005)
        .wrapping_add(start.elapsed().as_nanos() as u64)
        .max(1); // ensure non-zero

    loop {
        // Shuffle channel_indices for fairness.
        let len = channel_indices.len();
        for i in (1..len).rev() {
            let j = (xorshift64(&mut rng_state) as usize) % (i + 1);
            channel_indices.swap(i, j);
        }

        // Try each channel case in shuffled order.
        for &idx in &channel_indices {
            match &mut cases[idx] {
                SelectCase::Recv { channel } => {
                    if let Some(value) = channel.try_recv_any() {
                        return SelectResult::Recv {
                            index: idx,
                            value: Some(value),
                        };
                    }
                    if channel.is_closed() {
                        return SelectResult::Recv {
                            index: idx,
                            value: None,
                        };
                    }
                }
                SelectCase::Send { channel, value } => {
                    if let Some(val) = value.take() {
                        match channel.try_send_reclaim(val) {
                            Ok(()) => {
                                return SelectResult::Sent { index: idx };
                            }
                            Err(returned_val) => {
                                // Send failed — put value back for retry.
                                *value = Some(returned_val);
                            }
                        }
                    }
                }
                SelectCase::Default => unreachable!(),
            }
        }

        // No channel case was ready.

        // If there's a default case, take it.
        if has_default {
            return SelectResult::Default;
        }

        // Check timeout.
        if let Some(dur) = timeout {
            if start.elapsed() >= dur {
                return SelectResult::Timeout;
            }
        }

        // Yield to let other threads run, then retry.
        scheduler::yield_current();
    }
}

// ================================================================
// C FFI — called by compiled Adam code
// ================================================================

/// Create a select case for a recv operation.
/// Returns a pointer to a heap-allocated SelectCase.
#[no_mangle]
pub unsafe extern "C" fn __adam_select_recv(ch: *const Channel) -> *mut SelectCase {
    let arc = Arc::from_raw(ch);
    let cloned = arc.clone();
    std::mem::forget(arc); // don't drop the original
    Box::into_raw(Box::new(SelectCase::Recv { channel: cloned }))
}

/// Create a select case for a send operation.
#[no_mangle]
pub unsafe extern "C" fn __adam_select_send(
    ch: *const Channel,
    val_ptr: *const u8,
    val_size: usize,
) -> *mut SelectCase {
    let arc = Arc::from_raw(ch);
    let cloned = arc.clone();
    std::mem::forget(arc);
    let bytes = std::slice::from_raw_parts(val_ptr, val_size).to_vec();
    Box::into_raw(Box::new(SelectCase::Send {
        channel: cloned,
        value: Some(Box::new(bytes)),
    }))
}

/// Create a select case for the default arm.
#[no_mangle]
pub extern "C" fn __adam_select_default() -> *mut SelectCase {
    Box::into_raw(Box::new(SelectCase::Default))
}

/// Drop a select case.
#[no_mangle]
pub unsafe extern "C" fn __adam_select_case_drop(case: *mut SelectCase) {
    let _ = Box::from_raw(case);
}

/// Run a select statement.
///
/// `cases` is a pointer to an array of `*mut SelectCase`.
/// `num_cases` is the number of cases.
/// `timeout_ms` is the timeout in milliseconds (0 = no timeout, u64::MAX = no timeout).
/// `recv_buf` is a pointer to a buffer where the received value bytes will be written.
/// `recv_buf_size` is the size of the recv buffer.
/// Returns the index of the selected case (or num_cases for timeout/default).
#[no_mangle]
pub unsafe extern "C" fn __adam_select(
    cases_ptr: *mut *mut SelectCase,
    num_cases: usize,
    timeout_ms: u64,
    recv_buf: *mut u8,
    recv_buf_size: usize,
) -> usize {
    let case_ptrs = std::slice::from_raw_parts_mut(cases_ptr, num_cases);
    let mut cases: Vec<SelectCase> = case_ptrs
        .iter_mut()
        .map(|ptr| *Box::from_raw(*ptr))
        .collect();

    let timeout = if timeout_ms == 0 || timeout_ms == u64::MAX {
        None
    } else {
        Some(Duration::from_millis(timeout_ms))
    };

    let result = select(&mut cases, timeout);

    // Put cases back (some may have been consumed).
    for (i, case) in cases.into_iter().enumerate() {
        case_ptrs[i] = Box::into_raw(Box::new(case));
    }

    match result {
        SelectResult::Recv { index, value } => {
            // Copy received value bytes to the output buffer.
            if !recv_buf.is_null() && recv_buf_size > 0 {
                if let Some(boxed) = value {
                    if let Ok(bytes) = boxed.downcast::<Vec<u8>>() {
                        let copy_len = bytes.len().min(recv_buf_size);
                        std::ptr::copy_nonoverlapping(bytes.as_ptr(), recv_buf, copy_len);
                    }
                }
            }
            index
        }
        SelectResult::Sent { index } => index,
        SelectResult::Timeout => num_cases,
        SelectResult::Default => num_cases + 1,
    }
}

// ================================================================
// Tests
// ================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::channel::TypedChannel;
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

    // ---- Basic select tests ----

    #[test]
    fn test_select_single_recv_ready() {
        // One recv case, channel has a value. Should return immediately.
        let result = StdArc::new(AtomicI32::new(0));
        let result2 = result.clone();

        run_with_scheduler(2, move |_sched| {
            let ch: TypedChannel<i32> = TypedChannel::new(1);
            ch.send(42);

            let mut cases = vec![SelectCase::Recv {
                channel: ch.raw().clone(),
            }];

            match select(&mut cases, None) {
                SelectResult::Recv { index, value } => {
                    assert_eq!(index, 0);
                    let val = *value.unwrap().downcast::<i32>().unwrap();
                    result2.store(val, Ordering::SeqCst);
                }
                _ => panic!("expected Recv result"),
            }
        });

        assert_eq!(result.load(Ordering::SeqCst), 42);
    }

    #[test]
    fn test_select_single_send_ready() {
        // One send case, channel has buffer space.
        let result = StdArc::new(AtomicI32::new(0));
        let result2 = result.clone();

        run_with_scheduler(2, move |_sched| {
            let ch: TypedChannel<i32> = TypedChannel::new(1);

            let mut cases = vec![SelectCase::Send {
                channel: ch.raw().clone(),
                value: Some(Box::new(99i32)),
            }];

            match select(&mut cases, None) {
                SelectResult::Sent { index } => {
                    assert_eq!(index, 0);
                    let val = ch.recv().unwrap();
                    result2.store(val, Ordering::SeqCst);
                }
                _ => panic!("expected Sent result"),
            }
        });

        assert_eq!(result.load(Ordering::SeqCst), 99);
    }

    #[test]
    fn test_select_default_when_not_ready() {
        // No channels ready, default case should fire.
        let result = StdArc::new(AtomicI32::new(0));
        let result2 = result.clone();

        run_with_scheduler(2, move |_sched| {
            let ch: TypedChannel<i32> = TypedChannel::new(0);

            let mut cases = vec![
                SelectCase::Recv {
                    channel: ch.raw().clone(),
                },
                SelectCase::Default,
            ];

            match select(&mut cases, None) {
                SelectResult::Default => {
                    result2.store(1, Ordering::SeqCst);
                }
                _ => panic!("expected Default result"),
            }
        });

        assert_eq!(result.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn test_select_blocks_until_ready() {
        // Select blocks until a sender sends.
        let result = StdArc::new(AtomicI32::new(0));
        let result2 = result.clone();

        run_with_scheduler(2, move |sched| {
            let ch: TypedChannel<i32> = TypedChannel::new(0);
            let ch2 = ch.clone();

            // Spawn a sender that sends after a brief yield.
            sched.spawn(move || {
                scheduler::yield_current();
                ch2.send(77);
            });

            // Select should block until the sender runs.
            let mut cases = vec![SelectCase::Recv {
                channel: ch.raw().clone(),
            }];

            match select(&mut cases, None) {
                SelectResult::Recv { index, value } => {
                    assert_eq!(index, 0);
                    let val = *value.unwrap().downcast::<i32>().unwrap();
                    result2.store(val, Ordering::SeqCst);
                }
                _ => panic!("expected Recv result"),
            }
        });

        assert_eq!(result.load(Ordering::SeqCst), 77);
    }

    #[test]
    fn test_select_multiple_channels() {
        // Select on two channels, only one has a value.
        let result = StdArc::new(AtomicI32::new(-1));
        let result2 = result.clone();

        run_with_scheduler(2, move |_sched| {
            let ch1: TypedChannel<i32> = TypedChannel::new(1);
            let ch2: TypedChannel<i32> = TypedChannel::new(1);

            ch2.send(55);

            let mut cases = vec![
                SelectCase::Recv {
                    channel: ch1.raw().clone(),
                },
                SelectCase::Recv {
                    channel: ch2.raw().clone(),
                },
            ];

            match select(&mut cases, None) {
                SelectResult::Recv { index, value } => {
                    assert_eq!(index, 1);
                    let val = *value.unwrap().downcast::<i32>().unwrap();
                    result2.store(val, Ordering::SeqCst);
                }
                _ => panic!("expected Recv result"),
            }
        });

        assert_eq!(result.load(Ordering::SeqCst), 55);
    }

    #[test]
    fn test_select_fairness() {
        // Select on two channels, both have values. Over many iterations,
        // both should be selected (not always the same one).
        let count0 = StdArc::new(AtomicI32::new(0));
        let count1 = StdArc::new(AtomicI32::new(0));
        let c0 = count0.clone();
        let c1 = count1.clone();

        run_with_scheduler(2, move |_sched| {
            for _ in 0..100 {
                let ch0: TypedChannel<i32> = TypedChannel::new(1);
                let ch1: TypedChannel<i32> = TypedChannel::new(1);
                ch0.send(0);
                ch1.send(1);

                let mut cases = vec![
                    SelectCase::Recv {
                        channel: ch0.raw().clone(),
                    },
                    SelectCase::Recv {
                        channel: ch1.raw().clone(),
                    },
                ];

                match select(&mut cases, None) {
                    SelectResult::Recv { index, .. } => {
                        if index == 0 {
                            c0.fetch_add(1, Ordering::SeqCst);
                        } else {
                            c1.fetch_add(1, Ordering::SeqCst);
                        }
                    }
                    _ => panic!("expected Recv"),
                }
            }
        });

        let v0 = count0.load(Ordering::SeqCst);
        let v1 = count1.load(Ordering::SeqCst);
        assert_eq!(v0 + v1, 100);
        // Both should have been selected at least some times.
        // With fair randomness, each should get ~50. Allow wide margin.
        assert!(v0 > 5, "channel 0 was selected {} times, expected > 5", v0);
        assert!(v1 > 5, "channel 1 was selected {} times, expected > 5", v1);
    }

    #[test]
    fn test_select_timeout() {
        // No channels ready, timeout fires.
        let result = StdArc::new(AtomicI32::new(0));
        let result2 = result.clone();

        run_with_scheduler(2, move |_sched| {
            let ch: TypedChannel<i32> = TypedChannel::new(0);

            let mut cases = vec![SelectCase::Recv {
                channel: ch.raw().clone(),
            }];

            match select(&mut cases, Some(Duration::from_millis(50))) {
                SelectResult::Timeout => {
                    result2.store(1, Ordering::SeqCst);
                }
                _ => panic!("expected Timeout"),
            }
        });

        assert_eq!(result.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn test_select_closed_channel() {
        // Closed channel: recv arm fires with None.
        let result = StdArc::new(AtomicI32::new(0));
        let result2 = result.clone();

        run_with_scheduler(2, move |_sched| {
            let ch: TypedChannel<i32> = TypedChannel::new(0);
            ch.close();

            let mut cases = vec![SelectCase::Recv {
                channel: ch.raw().clone(),
            }];

            match select(&mut cases, None) {
                SelectResult::Recv { index, value } => {
                    assert_eq!(index, 0);
                    assert!(value.is_none(), "expected None for closed channel");
                    result2.store(1, Ordering::SeqCst);
                }
                _ => panic!("expected Recv result"),
            }
        });

        assert_eq!(result.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn test_select_send_and_recv() {
        // Mix of send and recv cases.
        let result = StdArc::new(AtomicI32::new(0));
        let result2 = result.clone();

        run_with_scheduler(2, move |_sched| {
            let send_ch: TypedChannel<i32> = TypedChannel::new(1);
            let recv_ch: TypedChannel<i32> = TypedChannel::new(1);
            recv_ch.send(10);

            let mut cases = vec![
                SelectCase::Send {
                    channel: send_ch.raw().clone(),
                    value: Some(Box::new(20i32)),
                },
                SelectCase::Recv {
                    channel: recv_ch.raw().clone(),
                },
            ];

            match select(&mut cases, None) {
                SelectResult::Recv { index, value } => {
                    // recv_ch had a value, so it might be selected.
                    let val = *value.unwrap().downcast::<i32>().unwrap();
                    result2.store(val + index as i32 * 100, Ordering::SeqCst);
                }
                SelectResult::Sent { index } => {
                    // send_ch had space, so it might be selected.
                    result2.store(-(index as i32 + 1), Ordering::SeqCst);
                }
                _ => panic!("expected Recv or Sent"),
            }
        });

        // Either case could fire. Just verify we got a result.
        let v = result.load(Ordering::SeqCst);
        assert!(v != 0, "select should have completed");
    }

    #[test]
    fn test_select_nested() {
        // Run select inside a loop, like a server event loop.
        let sum = StdArc::new(AtomicI32::new(0));
        let sum2 = sum.clone();

        run_with_scheduler(2, move |sched| {
            let ch: TypedChannel<i32> = TypedChannel::new(5);
            let ch2 = ch.clone();

            // Sender: push 5 values.
            sched.spawn(move || {
                for i in 1..=5 {
                    ch2.send(i);
                }
                ch2.close();
            });

            // Receiver: use select in a loop until channel closes.
            let mut total = 0;
            loop {
                let mut cases = vec![SelectCase::Recv {
                    channel: ch.raw().clone(),
                }];

                match select(&mut cases, None) {
                    SelectResult::Recv { value: Some(v), .. } => {
                        total += *v.downcast::<i32>().unwrap();
                    }
                    SelectResult::Recv { value: None, .. } => {
                        // Channel closed.
                        break;
                    }
                    _ => panic!("unexpected select result"),
                }
            }
            sum2.store(total, Ordering::SeqCst);
        });

        assert_eq!(sum.load(Ordering::SeqCst), 15); // 1+2+3+4+5
    }

    #[test]
    fn test_select_with_blocking_sender() {
        // Select recv on an unbuffered channel with a sender on another thread.
        let result = StdArc::new(AtomicI32::new(0));
        let result2 = result.clone();

        run_with_scheduler(2, move |sched| {
            let ch1: TypedChannel<i32> = TypedChannel::new(0);
            let ch2: TypedChannel<i32> = TypedChannel::new(0);
            let ch1_clone = ch1.clone();

            // Sender sends on ch1.
            sched.spawn(move || {
                ch1_clone.send(123);
            });

            // Select on ch1 and ch2 — ch1 should eventually be ready.
            let mut cases = vec![
                SelectCase::Recv {
                    channel: ch1.raw().clone(),
                },
                SelectCase::Recv {
                    channel: ch2.raw().clone(),
                },
            ];

            match select(&mut cases, Some(Duration::from_secs(5))) {
                SelectResult::Recv { index, value } => {
                    assert_eq!(index, 0);
                    let val = *value.unwrap().downcast::<i32>().unwrap();
                    result2.store(val, Ordering::SeqCst);
                }
                SelectResult::Timeout => {
                    panic!("select timed out");
                }
                _ => panic!("unexpected result"),
            }
        });

        assert_eq!(result.load(Ordering::SeqCst), 123);
    }
}
