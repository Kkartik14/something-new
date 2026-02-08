//! Adam Runtime — green thread scheduler, channels, memory allocator.

pub mod alloc;
pub mod channel;
pub mod context_switch;
pub mod format;
pub mod io;
pub mod map;
pub mod math;
pub mod print;
pub mod scheduler;
pub mod select;
pub mod set;
pub mod spawn_group;
pub mod string;
pub mod thread;
pub mod vec;

// ================================================================
// Cross-module concurrency integration tests
// ================================================================

#[cfg(test)]
mod concurrency_tests {
    use std::sync::atomic::{AtomicI32, Ordering};
    use std::sync::Arc;

    use crate::channel::TypedChannel;
    use crate::scheduler::Scheduler;
    use crate::select::{select, SelectCase, SelectResult};
    use crate::spawn_group::SpawnGroup;

    /// Helper: run green threads on a scheduler and wait for completion.
    fn run_with_scheduler<F: FnOnce() + Send + 'static>(workers: usize, f: F) {
        let sched = Arc::new(Scheduler::with_workers(workers));
        sched.spawn(f);
        sched.start();

        let start = std::time::Instant::now();
        while !sched.is_shutdown() && start.elapsed() < std::time::Duration::from_secs(10) {
            std::thread::sleep(std::time::Duration::from_millis(10));
        }
        sched.shutdown();
    }

    #[test]
    fn test_spawn_and_channel_integration() {
        // Spawn a producer and consumer communicating via a channel.
        let result = Arc::new(AtomicI32::new(0));
        let result2 = result.clone();

        run_with_scheduler(4, move || {
            let ch: TypedChannel<i32> = TypedChannel::new(10);
            let ch_send = ch.clone();

            let group: SpawnGroup<()> = SpawnGroup::new();

            // Producer: send 10 values.
            group.spawn(move || {
                for i in 0..10 {
                    ch_send.send(i);
                }
            });

            // Consumer: receive 10 values and sum them.
            let mut sum = 0;
            for _ in 0..10 {
                sum += ch.recv().unwrap();
            }
            assert_eq!(sum, 45); // 0+1+...+9

            group.wait();
            result2.store(1, Ordering::SeqCst);
        });

        assert_eq!(result.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn test_multiple_producers_single_consumer() {
        let result = Arc::new(AtomicI32::new(0));
        let result2 = result.clone();

        run_with_scheduler(4, move || {
            let ch: TypedChannel<i32> = TypedChannel::new(100);
            let group: SpawnGroup<()> = SpawnGroup::new();

            // 10 producers, each sending their index 10 times.
            for i in 0..10 {
                let ch_clone = ch.clone();
                group.spawn(move || {
                    for _ in 0..10 {
                        ch_clone.send(i);
                    }
                });
            }

            // Single consumer: receive all 100 values.
            let mut sum = 0;
            for _ in 0..100 {
                sum += ch.recv().unwrap();
            }
            // Sum should be 10 * (0+1+...+9) = 10 * 45 = 450
            assert_eq!(sum, 450);

            group.wait();
            result2.store(1, Ordering::SeqCst);
        });

        assert_eq!(result.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn test_single_producer_multiple_consumers() {
        let result = Arc::new(AtomicI32::new(0));
        let result2 = result.clone();

        run_with_scheduler(4, move || {
            let ch: TypedChannel<i32> = TypedChannel::new(100);
            let ch_send = ch.clone();

            let group: SpawnGroup<i32> = SpawnGroup::new();

            // 5 consumers, each receiving 20 values and summing.
            for _ in 0..5 {
                let ch_clone = ch.clone();
                group.spawn(move || {
                    let mut partial = 0;
                    for _ in 0..20 {
                        partial += ch_clone.recv().unwrap();
                    }
                    partial
                });
            }

            // Single producer: send 100 values.
            for i in 0..100 {
                ch_send.send(i);
            }

            let results = group.wait();
            let total: i32 = results.iter().map(|r| r.as_ref().unwrap()).sum();
            // Total should be 0+1+...+99 = 4950
            assert_eq!(total, 4950);

            result2.store(1, Ordering::SeqCst);
        });

        assert_eq!(result.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn test_channel_close_broadcasts() {
        // Closing a channel should wake all blocked receivers.
        let result = Arc::new(AtomicI32::new(0));
        let result2 = result.clone();

        run_with_scheduler(4, move || {
            let ch: TypedChannel<i32> = TypedChannel::new(0);
            let group: SpawnGroup<bool> = SpawnGroup::new();

            // 5 receivers blocked on an empty channel.
            for _ in 0..5 {
                let ch_clone = ch.clone();
                group.spawn(move || {
                    ch_clone.recv().is_none() // Should return None after close.
                });
            }

            // Give receivers time to block.
            crate::scheduler::yield_current();
            crate::scheduler::yield_current();

            // Close the channel.
            ch.close();

            let results = group.wait();
            // All receivers should have gotten None (close).
            assert!(results.iter().all(|r| *r.as_ref().unwrap()));
            result2.store(1, Ordering::SeqCst);
        });

        assert_eq!(result.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn test_select_with_spawn_group() {
        // Use select inside a spawn group task.
        let result = Arc::new(AtomicI32::new(0));
        let result2 = result.clone();

        run_with_scheduler(4, move || {
            let ch1: TypedChannel<i32> = TypedChannel::new(1);
            let ch2: TypedChannel<i32> = TypedChannel::new(1);

            // Send values to both channels.
            ch1.send(10);
            ch2.send(20);

            // Select should pick one of them.
            let mut cases = vec![
                SelectCase::Recv {
                    channel: ch1.raw().clone(),
                },
                SelectCase::Recv {
                    channel: ch2.raw().clone(),
                },
            ];

            let sel_result = select(&mut cases, None);
            match sel_result {
                SelectResult::Recv { index, .. } => {
                    assert!(index == 0 || index == 1);
                }
                _ => panic!("expected Recv"),
            }

            result2.store(1, Ordering::SeqCst);
        });

        assert_eq!(result.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn test_spawn_group_with_channels() {
        // Spawn group tasks communicate via channels.
        let result = Arc::new(AtomicI32::new(0));
        let result2 = result.clone();

        run_with_scheduler(4, move || {
            let ch: TypedChannel<i32> = TypedChannel::new(20);
            let group: SpawnGroup<()> = SpawnGroup::new();

            // Spawn 20 tasks, each sends its index.
            for i in 0..20 {
                let ch_clone = ch.clone();
                group.spawn(move || {
                    ch_clone.send(i);
                });
            }

            // Collect all values.
            let mut values = Vec::new();
            for _ in 0..20 {
                values.push(ch.recv().unwrap());
            }

            group.wait();

            values.sort();
            assert_eq!(values, (0..20).collect::<Vec<i32>>());
            result2.store(1, Ordering::SeqCst);
        });

        assert_eq!(result.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn test_pipeline_pattern() {
        // Pipeline: stage1 -> stage2 -> stage3.
        let result = Arc::new(AtomicI32::new(0));
        let result2 = result.clone();

        run_with_scheduler(4, move || {
            let ch1: TypedChannel<i32> = TypedChannel::new(10);
            let ch2: TypedChannel<i32> = TypedChannel::new(10);
            let ch3: TypedChannel<i32> = TypedChannel::new(10);

            let group: SpawnGroup<()> = SpawnGroup::new();

            // Stage 1: produce values.
            let ch1_send = ch1.clone();
            group.spawn(move || {
                for i in 0..10 {
                    ch1_send.send(i);
                }
            });

            // Stage 2: double values.
            let ch1_recv = ch1.clone();
            let ch2_send = ch2.clone();
            group.spawn(move || {
                for _ in 0..10 {
                    let v = ch1_recv.recv().unwrap();
                    ch2_send.send(v * 2);
                }
            });

            // Stage 3: add 1 to values.
            let ch2_recv = ch2.clone();
            let ch3_send = ch3.clone();
            group.spawn(move || {
                for _ in 0..10 {
                    let v = ch2_recv.recv().unwrap();
                    ch3_send.send(v + 1);
                }
            });

            // Collect final values.
            let mut total = 0;
            for _ in 0..10 {
                total += ch3.recv().unwrap();
            }
            // Each value i becomes i*2+1. Sum = sum(2i+1) for i=0..9
            // = 2*45 + 10 = 100
            assert_eq!(total, 100);

            group.wait();
            result2.store(1, Ordering::SeqCst);
        });

        assert_eq!(result.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn test_fan_out_fan_in() {
        // Fan-out: one producer, N workers. Fan-in: workers send to result channel.
        let result = Arc::new(AtomicI32::new(0));
        let result2 = result.clone();

        run_with_scheduler(4, move || {
            let work_ch: TypedChannel<i32> = TypedChannel::new(50);
            let result_ch: TypedChannel<i32> = TypedChannel::new(50);
            let group: SpawnGroup<()> = SpawnGroup::new();

            // 4 workers: each squares the received value.
            for _ in 0..4 {
                let wc = work_ch.clone();
                let rc = result_ch.clone();
                group.spawn(move || {
                    loop {
                        match wc.recv() {
                            Some(v) => rc.send(v * v),
                            None => break, // channel closed
                        }
                    }
                });
            }

            // Producer: send 20 values then close.
            for i in 0..20 {
                work_ch.send(i);
            }
            work_ch.close();

            // Collect 20 results.
            let mut results = Vec::new();
            for _ in 0..20 {
                results.push(result_ch.recv().unwrap());
            }
            results.sort();

            let expected: Vec<i32> = (0..20).map(|i| i * i).collect();
            assert_eq!(results, expected);

            group.wait();
            result2.store(1, Ordering::SeqCst);
        });

        assert_eq!(result.load(Ordering::SeqCst), 1);
    }
}

// ================================================================
// Cross-module standard library integration tests
// ================================================================

#[cfg(test)]
mod stdlib_integration_tests {
    use crate::format::{
        __adam_format_bool, __adam_format_float, __adam_format_int, __adam_parse_bool,
        __adam_parse_float, __adam_parse_int,
    };
    use crate::io::{__adam_fs_read, __adam_fs_write};
    use crate::map::{
        AdamMap, __adam_map_contains_key, __adam_map_drop, __adam_map_get, __adam_map_insert,
        __adam_map_len, __adam_map_new, __adam_map_remove,
    };
    use crate::math::{
        __adam_math_cos, __adam_math_e, __adam_math_infinity, __adam_math_nan, __adam_math_pi,
        __adam_math_sin, __adam_math_sqrt,
    };
    use crate::string::AdamString;
    use crate::string::{
        __adam_string_clone, __adam_string_contains, __adam_string_drop, __adam_string_eq,
        __adam_string_find, __adam_string_hash, __adam_string_len, __adam_string_push,
        __adam_string_replace, __adam_string_slice, __adam_string_split, __adam_string_to_upper,
        __adam_string_trim, __str_concat,
    };
    use crate::vec::{
        AdamVec, __adam_vec_clone, __adam_vec_drop, __adam_vec_get, __adam_vec_len, __adam_vec_new,
        __adam_vec_pop, __adam_vec_push, __adam_vec_reverse, __adam_vec_sort,
    };

    use std::alloc::{dealloc, Layout};
    use std::ptr;

    // ---------------------------------------------------------------
    // Helpers
    // ---------------------------------------------------------------

    fn make_string(s: &str) -> AdamString {
        AdamString::from_bytes(s.as_bytes())
    }

    unsafe fn read_string(s: &AdamString) -> String {
        if s.len == 0 || s.ptr.is_null() {
            return String::new();
        }
        let bytes = std::slice::from_raw_parts(s.ptr, s.len as usize);
        String::from_utf8_lossy(bytes).into_owned()
    }

    fn drop_string(s: AdamString) {
        __adam_string_drop(s.ptr, s.len, s.cap);
    }

    fn new_i32_vec() -> AdamVec {
        __adam_vec_new(
            std::mem::size_of::<i32>() as u64,
            std::mem::align_of::<i32>() as u64,
        )
    }

    fn push_i32(v: &mut AdamVec, val: i32) {
        let bytes = val.to_ne_bytes();
        __adam_vec_push(v, bytes.as_ptr());
    }

    #[allow(dead_code)]
    fn pop_i32(v: &mut AdamVec) -> Option<i32> {
        let mut buf = [0u8; 4];
        if __adam_vec_pop(v, buf.as_mut_ptr()) {
            Some(i32::from_ne_bytes(buf))
        } else {
            None
        }
    }

    fn get_i32_vec(v: &AdamVec, idx: u64) -> Option<i32> {
        let mut buf = [0u8; 4];
        if __adam_vec_get(v, idx, buf.as_mut_ptr()) {
            Some(i32::from_ne_bytes(buf))
        } else {
            None
        }
    }

    fn new_i32_map() -> AdamMap {
        __adam_map_new(
            std::mem::size_of::<i32>() as u64,
            std::mem::size_of::<i32>() as u64,
            std::mem::align_of::<i32>() as u64,
            std::mem::align_of::<i32>() as u64,
        )
    }

    fn hash_i32(val: i32) -> u64 {
        val as u64
    }

    fn map_insert_i32(m: &mut AdamMap, key: i32, val: i32) -> bool {
        let mut old_val: i32 = 0;
        __adam_map_insert(
            m,
            key.to_ne_bytes().as_ptr(),
            val.to_ne_bytes().as_ptr(),
            hash_i32(key),
            &mut old_val as *mut i32 as *mut u8,
        )
    }

    fn map_get_i32(m: &AdamMap, key: i32) -> Option<i32> {
        let mut out: i32 = 0;
        let found = __adam_map_get(
            m,
            key.to_ne_bytes().as_ptr(),
            hash_i32(key),
            &mut out as *mut i32 as *mut u8,
        );
        if found {
            Some(out)
        } else {
            None
        }
    }

    fn map_remove_i32(m: &mut AdamMap, key: i32) -> Option<i32> {
        let mut out: i32 = 0;
        let found = __adam_map_remove(
            m,
            key.to_ne_bytes().as_ptr(),
            hash_i32(key),
            &mut out as *mut i32 as *mut u8,
        );
        if found {
            Some(out)
        } else {
            None
        }
    }

    /// Helper to get a temp file path for integration tests.
    fn tmp_path(name: &str) -> std::path::PathBuf {
        let mut p = std::env::temp_dir();
        p.push(format!("adam_stdlib_integ_{}", name));
        p
    }

    fn cleanup_file(p: &std::path::PathBuf) {
        let _ = std::fs::remove_file(p);
    }

    extern "C" fn cmp_i32(a: *const u8, b: *const u8) -> i32 {
        let va = i32::from_ne_bytes(unsafe { *(a as *const [u8; 4]) });
        let vb = i32::from_ne_bytes(unsafe { *(b as *const [u8; 4]) });
        va.cmp(&vb) as i32
    }

    // ---------------------------------------------------------------
    // 1. String + Vec integration: Create vec of strings, push/pop,
    //    verify contents.
    // ---------------------------------------------------------------
    #[test]
    fn test_string_vec_push_pop() {
        // Use a vec of AdamString structs (size = 24, align = 8 on 64-bit).
        let mut v = __adam_vec_new(
            std::mem::size_of::<AdamString>() as u64,
            std::mem::align_of::<AdamString>() as u64,
        );

        let words = ["hello", "world", "adam", "lang"];
        for word in &words {
            let s = make_string(word);
            __adam_vec_push(&mut v, &s as *const AdamString as *const u8);
            // Do NOT drop s -- ownership moved into the vec.
        }
        assert_eq!(__adam_vec_len(&v), 4);

        // Pop the last element.
        let mut popped = AdamString {
            ptr: ptr::null_mut(),
            len: 0,
            cap: 0,
        };
        assert!(__adam_vec_pop(
            &mut v,
            &mut popped as *mut AdamString as *mut u8
        ));
        unsafe {
            assert_eq!(read_string(&popped), "lang");
        }
        drop_string(popped);

        // Verify remaining elements.
        for (i, expected) in ["hello", "world", "adam"].iter().enumerate() {
            let mut elem = AdamString {
                ptr: ptr::null_mut(),
                len: 0,
                cap: 0,
            };
            assert!(__adam_vec_get(
                &v,
                i as u64,
                &mut elem as *mut AdamString as *mut u8
            ));
            unsafe {
                assert_eq!(read_string(&elem), *expected);
            }
            // elem is a copy of bytes; the actual string data is still owned by the vec.
        }

        // Clean up: pop remaining and drop strings.
        while __adam_vec_len(&v) > 0 {
            let mut elem = AdamString {
                ptr: ptr::null_mut(),
                len: 0,
                cap: 0,
            };
            __adam_vec_pop(&mut v, &mut elem as *mut AdamString as *mut u8);
            drop_string(elem);
        }
        __adam_vec_drop(&mut v);
    }

    // ---------------------------------------------------------------
    // 2. String + Map integration: Use string bytes as map keys.
    //    We use the string hash as the hash parameter and store i32
    //    values keyed by i32 representations of string hashes.
    // ---------------------------------------------------------------
    #[test]
    fn test_string_bytes_as_map_keys() {
        // Map with u64 keys (string hashes) and i32 values.
        let mut m = __adam_map_new(
            std::mem::size_of::<u64>() as u64,
            std::mem::size_of::<i32>() as u64,
            std::mem::align_of::<u64>() as u64,
            std::mem::align_of::<i32>() as u64,
        );

        let words = ["apple", "banana", "cherry"];
        for (i, word) in words.iter().enumerate() {
            let s = make_string(word);
            let h = __adam_string_hash(s.ptr, s.len, s.cap);
            let val = (i as i32) * 10;
            let mut old: i32 = 0;
            __adam_map_insert(
                &mut m,
                h.to_ne_bytes().as_ptr(),
                val.to_ne_bytes().as_ptr(),
                h,
                &mut old as *mut i32 as *mut u8,
            );
            drop_string(s);
        }

        assert_eq!(__adam_map_len(&m), 3);

        // Look up each word by its hash.
        for (i, word) in words.iter().enumerate() {
            let s = make_string(word);
            let h = __adam_string_hash(s.ptr, s.len, s.cap);
            let mut out: i32 = 0;
            let found = __adam_map_get(
                &m,
                h.to_ne_bytes().as_ptr(),
                h,
                &mut out as *mut i32 as *mut u8,
            );
            assert!(found, "word '{}' should be in the map", word);
            assert_eq!(out, (i as i32) * 10);
            drop_string(s);
        }

        __adam_map_drop(&mut m);
    }

    // ---------------------------------------------------------------
    // 3. String + Format roundtrip: format_int -> parse_int for
    //    various values.
    // ---------------------------------------------------------------
    #[test]
    fn test_format_int_parse_int_roundtrip() {
        let values: &[i64] = &[0, 1, -1, 42, -42, 1000000, -9999999, i64::MAX, i64::MIN];
        for &v in values {
            let s = __adam_format_int(v);
            let mut parsed: i64 = 0;
            let ok = __adam_parse_int(s.ptr, s.len, &mut parsed);
            assert!(ok, "parse_int should succeed for formatted value {}", v);
            assert_eq!(parsed, v, "roundtrip failed for {}", v);
            drop_string(s);
        }
    }

    // ---------------------------------------------------------------
    // 4. String + Format float roundtrip: format_float -> parse_float.
    // ---------------------------------------------------------------
    #[test]
    fn test_format_float_parse_float_roundtrip() {
        let values: &[f64] = &[0.0, 1.5, -2.75, 3.14159, 1e10, -1e-5, 0.1];
        for &v in values {
            let s = __adam_format_float(v, 10); // high precision to preserve value
            let mut parsed: f64 = 0.0;
            let ok = __adam_parse_float(s.ptr, s.len, &mut parsed);
            assert!(ok, "parse_float should succeed for formatted value {}", v);
            assert!(
                (parsed - v).abs() < 1e-6,
                "roundtrip failed for {}: got {}",
                v,
                parsed
            );
            drop_string(s);
        }
    }

    // ---------------------------------------------------------------
    // 5. String + Math: Convert math results to strings, verify
    //    formatting.
    // ---------------------------------------------------------------
    #[test]
    fn test_math_result_to_string() {
        let pi = __adam_math_pi();
        let s = __adam_format_float(pi, 4);
        unsafe {
            assert_eq!(read_string(&s), "3.1416");
        }
        drop_string(s);

        let sqrt2 = __adam_math_sqrt(2.0);
        let s = __adam_format_float(sqrt2, 5);
        unsafe {
            assert_eq!(read_string(&s), "1.41421");
        }
        drop_string(s);

        let e = __adam_math_e();
        let s = __adam_format_float(e, 3);
        unsafe {
            assert_eq!(read_string(&s), "2.718");
        }
        drop_string(s);
    }

    // ---------------------------------------------------------------
    // 6. Vec + Sort with custom data: Vec of (i32, i32) pairs sorted
    //    by first element.
    // ---------------------------------------------------------------
    #[test]
    fn test_vec_sort_pairs_by_first() {
        // Each element is [i32; 2] = 8 bytes, align 4.
        let mut v = __adam_vec_new(8, 4);

        let pairs: &[(i32, i32)] = &[(5, 50), (1, 10), (3, 30), (2, 20), (4, 40)];
        for &(a, b) in pairs {
            let buf: [i32; 2] = [a, b];
            __adam_vec_push(&mut v, buf.as_ptr() as *const u8);
        }

        extern "C" fn cmp_pair_by_first(a: *const u8, b: *const u8) -> i32 {
            let a0 = i32::from_ne_bytes(unsafe { *(a as *const [u8; 4]) });
            let b0 = i32::from_ne_bytes(unsafe { *(b as *const [u8; 4]) });
            a0.cmp(&b0) as i32
        }

        __adam_vec_sort(&mut v, cmp_pair_by_first);

        // Verify sorted order: (1,10), (2,20), (3,30), (4,40), (5,50).
        for i in 0..5u64 {
            let mut buf = [0i32; 2];
            assert!(__adam_vec_get(&v, i, buf.as_mut_ptr() as *mut u8));
            assert_eq!(buf[0], (i + 1) as i32);
            assert_eq!(buf[1], ((i + 1) * 10) as i32);
        }
        __adam_vec_drop(&mut v);
    }

    // ---------------------------------------------------------------
    // 7. Map + Vec: Store vec lengths as map values keyed by index.
    //    Build multiple vecs, store their lengths in a map.
    // ---------------------------------------------------------------
    #[test]
    fn test_map_storing_vec_metadata() {
        let mut m = new_i32_map();

        // Create 5 vecs with different lengths, store length in map keyed by index.
        for i in 0..5i32 {
            let mut v = new_i32_vec();
            for j in 0..=(i as i32) {
                push_i32(&mut v, j);
            }
            let len = __adam_vec_len(&v) as i32;
            map_insert_i32(&mut m, i, len);
            __adam_vec_drop(&mut v);
        }

        assert_eq!(__adam_map_len(&m), 5);
        for i in 0..5i32 {
            assert_eq!(map_get_i32(&m, i), Some(i + 1));
        }
        __adam_map_drop(&mut m);
    }

    // ---------------------------------------------------------------
    // 8. String + IO roundtrip: Write string to temp file, read back,
    //    compare.
    // ---------------------------------------------------------------
    #[test]
    fn test_string_io_roundtrip() {
        let p = tmp_path("string_io_rt");
        cleanup_file(&p);
        let path_bytes = p.to_str().unwrap().as_bytes();

        let original = make_string("Hello from Adam runtime integration test!");

        let r = __adam_fs_write(
            path_bytes.as_ptr(),
            path_bytes.len() as u64,
            original.ptr,
            original.len,
        );
        assert!(r.success);

        let mut read_back = AdamString::from_bytes(b"");
        let r = __adam_fs_read(path_bytes.as_ptr(), path_bytes.len() as u64, &mut read_back);
        assert!(r.success);

        assert!(__adam_string_eq(
            original.ptr,
            original.len,
            original.cap,
            read_back.ptr,
            read_back.len,
            read_back.cap,
        ));

        drop_string(original);
        drop_string(read_back);
        cleanup_file(&p);
    }

    // ---------------------------------------------------------------
    // 9. Vec + IO: Serialize vec of ints to string, write to file,
    //    read back, parse.
    // ---------------------------------------------------------------
    #[test]
    fn test_vec_io_serialize_deserialize() {
        let p = tmp_path("vec_io_serde");
        cleanup_file(&p);
        let path_bytes = p.to_str().unwrap().as_bytes();

        // Build a vec of ints and serialize to comma-separated string.
        let mut v = new_i32_vec();
        let values = [10, 20, 30, 40, 50];
        for &val in &values {
            push_i32(&mut v, val);
        }

        // Build the CSV string.
        let mut csv = String::new();
        for i in 0..(__adam_vec_len(&v) as usize) {
            if i > 0 {
                csv.push(',');
            }
            let val = get_i32_vec(&v, i as u64).unwrap();
            csv.push_str(&val.to_string());
        }
        let csv_str = make_string(&csv);

        // Write to file.
        let r = __adam_fs_write(
            path_bytes.as_ptr(),
            path_bytes.len() as u64,
            csv_str.ptr,
            csv_str.len,
        );
        assert!(r.success);

        // Read back.
        let mut read_back = AdamString::from_bytes(b"");
        let r = __adam_fs_read(path_bytes.as_ptr(), path_bytes.len() as u64, &mut read_back);
        assert!(r.success);

        // Parse: split by comma, parse each.
        let delim = make_string(",");
        let mut arr: *mut AdamString = ptr::null_mut();
        let mut count: u64 = 0;
        __adam_string_split(
            read_back.ptr,
            read_back.len,
            read_back.cap,
            delim.ptr,
            delim.len,
            delim.cap,
            &mut arr,
            &mut count,
        );
        assert_eq!(count, 5);

        unsafe {
            for i in 0..count as usize {
                let part = &*arr.add(i);
                let mut parsed: i64 = 0;
                let ok = __adam_parse_int(part.ptr, part.len, &mut parsed);
                assert!(ok);
                assert_eq!(parsed, values[i] as i64);
            }
            // Clean up split results.
            for i in 0..count as usize {
                let part = ptr::read(arr.add(i));
                drop_string(part);
            }
            let layout = Layout::from_size_align_unchecked(
                count as usize * std::mem::size_of::<AdamString>(),
                std::mem::align_of::<AdamString>(),
            );
            dealloc(arr as *mut u8, layout);
        }

        __adam_vec_drop(&mut v);
        drop_string(csv_str);
        drop_string(delim);
        drop_string(read_back);
        cleanup_file(&p);
    }

    // ---------------------------------------------------------------
    // 10. Math + Format: Format math constants to strings, verify
    //     precision.
    // ---------------------------------------------------------------
    #[test]
    fn test_math_format_constants() {
        let pi = __adam_math_pi();
        let s = __adam_format_float(pi, 6);
        unsafe {
            let text = read_string(&s);
            assert_eq!(text, "3.141593");
        }
        drop_string(s);

        let e = __adam_math_e();
        let s = __adam_format_float(e, 6);
        unsafe {
            let text = read_string(&s);
            assert_eq!(text, "2.718282");
        }
        drop_string(s);
    }

    // ---------------------------------------------------------------
    // 11. String methods chain: Create string -> to_upper -> replace
    //     -> split -> reconstruct.
    // ---------------------------------------------------------------
    #[test]
    fn test_string_method_chain() {
        let s = make_string("hello world foo");

        // to_upper
        let upper = __adam_string_to_upper(s.ptr, s.len, s.cap);
        unsafe {
            assert_eq!(read_string(&upper), "HELLO WORLD FOO");
        }

        // replace "WORLD" with "ADAM"
        let from = make_string("WORLD");
        let to = make_string("ADAM");
        let replaced = __adam_string_replace(
            upper.ptr, upper.len, upper.cap, from.ptr, from.len, from.cap, to.ptr, to.len, to.cap,
        );
        unsafe {
            assert_eq!(read_string(&replaced), "HELLO ADAM FOO");
        }

        // split by space
        let space = make_string(" ");
        let mut arr: *mut AdamString = ptr::null_mut();
        let mut count: u64 = 0;
        __adam_string_split(
            replaced.ptr,
            replaced.len,
            replaced.cap,
            space.ptr,
            space.len,
            space.cap,
            &mut arr,
            &mut count,
        );
        assert_eq!(count, 3);

        unsafe {
            assert_eq!(read_string(&*arr.add(0)), "HELLO");
            assert_eq!(read_string(&*arr.add(1)), "ADAM");
            assert_eq!(read_string(&*arr.add(2)), "FOO");

            // Reconstruct by concatenating parts with "-".
            let sep = make_string("-");
            let mut result =
                __adam_string_clone((*arr.add(0)).ptr, (*arr.add(0)).len, (*arr.add(0)).cap);
            for i in 1..count as usize {
                // concat result + sep
                let tmp = __str_concat(
                    result.ptr, result.len, result.cap, sep.ptr, sep.len, sep.cap,
                );
                drop_string(result);
                // concat tmp + parts[i]
                let part = &*arr.add(i);
                result = __str_concat(tmp.ptr, tmp.len, tmp.cap, part.ptr, part.len, part.cap);
                drop_string(tmp);
            }
            assert_eq!(read_string(&result), "HELLO-ADAM-FOO");
            drop_string(result);
            drop_string(sep);

            // Clean up split results.
            for i in 0..count as usize {
                let part = ptr::read(arr.add(i));
                drop_string(part);
            }
            let layout = Layout::from_size_align_unchecked(
                count as usize * std::mem::size_of::<AdamString>(),
                std::mem::align_of::<AdamString>(),
            );
            dealloc(arr as *mut u8, layout);
        }

        drop_string(s);
        drop_string(upper);
        drop_string(from);
        drop_string(to);
        drop_string(replaced);
        drop_string(space);
    }

    // ---------------------------------------------------------------
    // 12. Multiple string operations: concat + trim + find + slice
    //     in sequence.
    // ---------------------------------------------------------------
    #[test]
    fn test_multiple_string_operations() {
        let a = make_string("  hello ");
        let b = make_string(" world  ");

        // Concat.
        let concat = __str_concat(a.ptr, a.len, a.cap, b.ptr, b.len, b.cap);
        unsafe {
            assert_eq!(read_string(&concat), "  hello  world  ");
        }

        // Trim.
        let trimmed = __adam_string_trim(concat.ptr, concat.len, concat.cap);
        unsafe {
            assert_eq!(read_string(&trimmed), "hello  world");
        }

        // Find "world".
        let needle = make_string("world");
        let mut idx: u64 = 0;
        let found = __adam_string_find(
            trimmed.ptr,
            trimmed.len,
            trimmed.cap,
            needle.ptr,
            needle.len,
            needle.cap,
            &mut idx,
        );
        assert!(found);
        assert_eq!(idx, 7);

        // Slice from idx to end.
        let sliced = __adam_string_slice(trimmed.ptr, trimmed.len, trimmed.cap, idx, trimmed.len);
        unsafe {
            assert_eq!(read_string(&sliced), "world");
        }

        drop_string(a);
        drop_string(b);
        drop_string(concat);
        drop_string(trimmed);
        drop_string(needle);
        drop_string(sliced);
    }

    // ---------------------------------------------------------------
    // 13. Vec operations chain: new -> push N -> sort -> reverse ->
    //     verify descending.
    // ---------------------------------------------------------------
    #[test]
    fn test_vec_operations_chain() {
        let mut v = new_i32_vec();
        let data = [5, 3, 8, 1, 9, 2, 7, 4, 6, 0];
        for &val in &data {
            push_i32(&mut v, val);
        }
        assert_eq!(__adam_vec_len(&v), 10);

        __adam_vec_sort(&mut v, cmp_i32);
        // Should be ascending: 0..9.
        for i in 0..10 {
            assert_eq!(get_i32_vec(&v, i as u64), Some(i));
        }

        __adam_vec_reverse(&mut v);
        // Should be descending: 9..0.
        for i in 0..10 {
            assert_eq!(get_i32_vec(&v, i as u64), Some(9 - i));
        }
        __adam_vec_drop(&mut v);
    }

    // ---------------------------------------------------------------
    // 14. Map collision stress: Insert 100 entries, verify all present,
    //     remove odd keys, verify even keys remain.
    // ---------------------------------------------------------------
    #[test]
    fn test_map_collision_stress() {
        let mut m = new_i32_map();

        for i in 0..100i32 {
            map_insert_i32(&mut m, i, i * 100);
        }
        assert_eq!(__adam_map_len(&m), 100);

        // Verify all present.
        for i in 0..100i32 {
            assert_eq!(map_get_i32(&m, i), Some(i * 100), "key {} missing", i);
        }

        // Remove odd keys.
        for i in (1..100i32).step_by(2) {
            let removed = map_remove_i32(&mut m, i);
            assert_eq!(removed, Some(i * 100));
        }
        assert_eq!(__adam_map_len(&m), 50);

        // Verify even keys still present, odd keys gone.
        for i in 0..100i32 {
            if i % 2 == 0 {
                assert_eq!(map_get_i32(&m, i), Some(i * 100), "even key {} missing", i);
            } else {
                assert_eq!(map_get_i32(&m, i), None, "odd key {} should be removed", i);
            }
        }
        __adam_map_drop(&mut m);
    }

    // ---------------------------------------------------------------
    // 15. String hash as map key hash: Use __adam_string_hash to
    //     build a map of string->int.
    // ---------------------------------------------------------------
    #[test]
    fn test_string_hash_as_map_key() {
        // Map: u64 key (hash), i32 value
        let mut m = __adam_map_new(
            std::mem::size_of::<u64>() as u64,
            std::mem::size_of::<i32>() as u64,
            std::mem::align_of::<u64>() as u64,
            std::mem::align_of::<i32>() as u64,
        );

        let entries: [(&str, i32); 3] = [("alpha", 1), ("beta", 2), ("gamma", 3)];
        for &(word, val) in &entries {
            let s = make_string(word);
            let h = __adam_string_hash(s.ptr, s.len, s.cap);
            let mut old: i32 = 0;
            __adam_map_insert(
                &mut m,
                h.to_ne_bytes().as_ptr(),
                val.to_ne_bytes().as_ptr(),
                h,
                &mut old as *mut i32 as *mut u8,
            );
            drop_string(s);
        }

        // Verify containment.
        for &(word, _) in &entries {
            let s = make_string(word);
            let h = __adam_string_hash(s.ptr, s.len, s.cap);
            assert!(__adam_map_contains_key(&m, h.to_ne_bytes().as_ptr(), h));
            drop_string(s);
        }

        // Verify non-existent key.
        let s = make_string("delta");
        let h = __adam_string_hash(s.ptr, s.len, s.cap);
        assert!(!__adam_map_contains_key(&m, h.to_ne_bytes().as_ptr(), h));
        drop_string(s);

        __adam_map_drop(&mut m);
    }

    // ---------------------------------------------------------------
    // 16. Format + Parse round-trip for bool: format_bool -> parse_bool.
    // ---------------------------------------------------------------
    #[test]
    fn test_format_parse_bool_roundtrip() {
        // true
        let s = __adam_format_bool(1);
        let mut parsed: i8 = -1;
        let ok = __adam_parse_bool(s.ptr, s.len, &mut parsed);
        assert!(ok);
        assert_eq!(parsed, 1);
        drop_string(s);

        // false
        let s = __adam_format_bool(0);
        let mut parsed: i8 = -1;
        let ok = __adam_parse_bool(s.ptr, s.len, &mut parsed);
        assert!(ok);
        assert_eq!(parsed, 0);
        drop_string(s);
    }

    // ---------------------------------------------------------------
    // 17. Math special values through format: format_float(NaN),
    //     format_float(infinity).
    // ---------------------------------------------------------------
    #[test]
    fn test_format_math_special_values() {
        let nan = __adam_math_nan();
        let s = __adam_format_float(nan, -1);
        unsafe {
            assert_eq!(read_string(&s), "NaN");
        }
        drop_string(s);

        let inf = __adam_math_infinity();
        let s = __adam_format_float(inf, -1);
        unsafe {
            assert_eq!(read_string(&s), "inf");
        }
        drop_string(s);

        let neg_inf = -__adam_math_infinity();
        let s = __adam_format_float(neg_inf, -1);
        unsafe {
            assert_eq!(read_string(&s), "-inf");
        }
        drop_string(s);
    }

    // ---------------------------------------------------------------
    // 18. String clone + modify original: Clone string, push to
    //     original, verify clone unchanged.
    // ---------------------------------------------------------------
    #[test]
    fn test_string_clone_and_modify_original() {
        let mut original = make_string("hello");
        let cloned = __adam_string_clone(original.ptr, original.len, original.cap);

        // Push to original.
        let suffix = make_string(" world");
        __adam_string_push(
            &mut original.ptr,
            &mut original.len,
            &mut original.cap,
            suffix.ptr,
            suffix.len,
            suffix.cap,
        );

        unsafe {
            assert_eq!(read_string(&original), "hello world");
            assert_eq!(read_string(&cloned), "hello");
        }

        // Pointers should differ.
        assert_ne!(original.ptr, cloned.ptr);

        drop_string(original);
        drop_string(cloned);
        drop_string(suffix);
    }

    // ---------------------------------------------------------------
    // 19. Vec clone + modify original: Clone vec, push to original,
    //     verify clone unchanged.
    // ---------------------------------------------------------------
    #[test]
    fn test_vec_clone_and_modify_original() {
        let mut v = new_i32_vec();
        push_i32(&mut v, 10);
        push_i32(&mut v, 20);
        push_i32(&mut v, 30);

        let mut cloned = __adam_vec_clone(&v);

        // Modify original: push more.
        push_i32(&mut v, 40);
        push_i32(&mut v, 50);

        assert_eq!(__adam_vec_len(&v), 5);
        assert_eq!(__adam_vec_len(&cloned), 3);

        // Clone should still have only the original 3 elements.
        assert_eq!(get_i32_vec(&cloned, 0), Some(10));
        assert_eq!(get_i32_vec(&cloned, 1), Some(20));
        assert_eq!(get_i32_vec(&cloned, 2), Some(30));
        assert_eq!(get_i32_vec(&cloned, 3), None); // out of bounds

        __adam_vec_drop(&mut v);
        __adam_vec_drop(&mut cloned);
    }

    // ---------------------------------------------------------------
    // 20. Large data pipeline: Generate 1000 integers -> push to vec
    //     -> sort -> format each to string.
    // ---------------------------------------------------------------
    #[test]
    fn test_large_pipeline() {
        let mut v = new_i32_vec();
        // Push 1000 values in reverse order.
        for i in (0..1000).rev() {
            push_i32(&mut v, i);
        }
        assert_eq!(__adam_vec_len(&v), 1000);

        // Sort ascending.
        __adam_vec_sort(&mut v, cmp_i32);

        // Verify sorted and format a few.
        for i in 0..1000 {
            let val = get_i32_vec(&v, i as u64).unwrap();
            assert_eq!(val, i);
        }

        // Format first and last to string.
        let first = get_i32_vec(&v, 0).unwrap();
        let s = __adam_format_int(first as i64);
        unsafe {
            assert_eq!(read_string(&s), "0");
        }
        drop_string(s);

        let last = get_i32_vec(&v, 999).unwrap();
        let s = __adam_format_int(last as i64);
        unsafe {
            assert_eq!(read_string(&s), "999");
        }
        drop_string(s);

        __adam_vec_drop(&mut v);
    }

    // ---------------------------------------------------------------
    // 21. IO error handling: Read nonexistent file returns
    //     AdamIoResult with success=false.
    // ---------------------------------------------------------------
    #[test]
    fn test_io_error_nonexistent_file() {
        let p = tmp_path("this_file_does_not_exist_integ");
        cleanup_file(&p);
        let path_bytes = p.to_str().unwrap().as_bytes();

        let mut out = AdamString::from_bytes(b"");
        let r = __adam_fs_read(path_bytes.as_ptr(), path_bytes.len() as u64, &mut out);
        assert!(!r.success);
        assert_ne!(r.error_code, 0);
        assert!(r.error_msg.len > 0); // should have a human-readable error
        drop_string(out);
    }

    // ---------------------------------------------------------------
    // 22. String split + rejoin: Split a CSV line, verify count and
    //     values.
    // ---------------------------------------------------------------
    #[test]
    fn test_string_split_csv_rejoin() {
        let csv = make_string("name,age,city,country");
        let delim = make_string(",");
        let mut arr: *mut AdamString = ptr::null_mut();
        let mut count: u64 = 0;

        __adam_string_split(
            csv.ptr, csv.len, csv.cap, delim.ptr, delim.len, delim.cap, &mut arr, &mut count,
        );
        assert_eq!(count, 4);

        unsafe {
            assert_eq!(read_string(&*arr.add(0)), "name");
            assert_eq!(read_string(&*arr.add(1)), "age");
            assert_eq!(read_string(&*arr.add(2)), "city");
            assert_eq!(read_string(&*arr.add(3)), "country");

            // Verify each part contains expected substring.
            let name_part = &*arr.add(0);
            let name_needle = make_string("nam");
            assert!(__adam_string_contains(
                name_part.ptr,
                name_part.len,
                name_part.cap,
                name_needle.ptr,
                name_needle.len,
                name_needle.cap,
            ));
            drop_string(name_needle);

            // Clean up split results.
            for i in 0..count as usize {
                let part = ptr::read(arr.add(i));
                drop_string(part);
            }
            let layout = Layout::from_size_align_unchecked(
                count as usize * std::mem::size_of::<AdamString>(),
                std::mem::align_of::<AdamString>(),
            );
            dealloc(arr as *mut u8, layout);
        }

        drop_string(csv);
        drop_string(delim);
    }

    // ---------------------------------------------------------------
    // 23. Nested collections: Map with i32 keys and i32 values, then
    //     vec of all values.
    // ---------------------------------------------------------------
    #[test]
    fn test_nested_collections() {
        let mut m = new_i32_map();

        // Insert 20 entries.
        for i in 0..20i32 {
            map_insert_i32(&mut m, i, i * i);
        }

        // Extract all values into a vec.
        let mut v = new_i32_vec();
        for i in 0..20i32 {
            let val = map_get_i32(&m, i).unwrap();
            push_i32(&mut v, val);
        }

        assert_eq!(__adam_vec_len(&v), 20);

        // Sort the vec and verify.
        __adam_vec_sort(&mut v, cmp_i32);
        // The values are i^2 for i in 0..20, which is already sorted.
        for i in 0..20 {
            let expected = (i * i) as i32;
            assert_eq!(get_i32_vec(&v, i as u64), Some(expected));
        }

        __adam_vec_drop(&mut v);
        __adam_map_drop(&mut m);
    }

    // ---------------------------------------------------------------
    // 24. Math accuracy: Verify sin^2(x) + cos^2(x) ~= 1 for various
    //     x values.
    // ---------------------------------------------------------------
    #[test]
    fn test_math_sin_cos_identity() {
        let x_values: &[f64] = &[
            0.0,
            0.1,
            0.5,
            1.0,
            1.5,
            2.0,
            2.5,
            3.0,
            std::f64::consts::PI,
            std::f64::consts::FRAC_PI_2,
            std::f64::consts::FRAC_PI_4,
            -1.0,
            -2.5,
            10.0,
            100.0,
        ];
        for &x in x_values {
            let s = __adam_math_sin(x);
            let c = __adam_math_cos(x);
            let sum = s * s + c * c;
            assert!(
                (sum - 1.0).abs() < 1e-10,
                "sin^2({}) + cos^2({}) = {} != 1.0",
                x,
                x,
                sum
            );
        }
    }

    // ---------------------------------------------------------------
    // 25. String interning check: Two separate strings with same
    //     content have same hash but different pointers.
    // ---------------------------------------------------------------
    #[test]
    fn test_string_same_content_different_pointers_same_hash() {
        let s1 = make_string("identical content here");
        let s2 = make_string("identical content here");

        // Different allocations => different pointers.
        assert_ne!(s1.ptr, s2.ptr);

        // Same content => same hash.
        let h1 = __adam_string_hash(s1.ptr, s1.len, s1.cap);
        let h2 = __adam_string_hash(s2.ptr, s2.len, s2.cap);
        assert_eq!(h1, h2);

        // Same content => eq returns true.
        assert!(__adam_string_eq(
            s1.ptr, s1.len, s1.cap, s2.ptr, s2.len, s2.cap,
        ));

        // Same length.
        assert_eq!(
            __adam_string_len(s1.ptr, s1.len, s1.cap),
            __adam_string_len(s2.ptr, s2.len, s2.cap),
        );

        drop_string(s1);
        drop_string(s2);
    }
}

#[cfg(test)]
mod adversarial_tests;

#[cfg(test)]
mod extreme_stress_tests;
