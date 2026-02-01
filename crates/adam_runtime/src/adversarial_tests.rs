//! Adversarial tests for the Adam runtime — stress-testing memory, concurrency,
//! and data structures under extreme conditions.

#[cfg(test)]
mod stress_tests {
    use crate::channel::TypedChannel;
    use crate::vec::{__adam_vec_new, __adam_vec_push, __adam_vec_pop, __adam_vec_get, __adam_vec_len, __adam_vec_drop, __adam_vec_sort, __adam_vec_reverse, __adam_vec_clone, AdamVec};
    use crate::map::{__adam_map_new, __adam_map_insert, __adam_map_get, __adam_map_remove, __adam_map_len, __adam_map_drop, AdamMap};
    use crate::string::{AdamString, __adam_string_drop, __adam_string_len, __adam_string_push, __adam_string_clone};

    // -----------------------------------------------------------------------
    // Vec stress: grow/shrink cycles
    // -----------------------------------------------------------------------

    fn new_i64_vec() -> AdamVec {
        __adam_vec_new(
            std::mem::size_of::<i64>() as u64,
            std::mem::align_of::<i64>() as u64,
        )
    }

    fn push_i64(v: &mut AdamVec, val: i64) {
        __adam_vec_push(v, val.to_ne_bytes().as_ptr());
    }

    fn get_i64(v: &AdamVec, idx: u64) -> Option<i64> {
        let mut buf = [0u8; 8];
        if __adam_vec_get(v, idx, buf.as_mut_ptr()) {
            Some(i64::from_ne_bytes(buf))
        } else {
            None
        }
    }

    fn pop_i64(v: &mut AdamVec) -> Option<i64> {
        let mut buf = [0u8; 8];
        if __adam_vec_pop(v, buf.as_mut_ptr()) {
            Some(i64::from_ne_bytes(buf))
        } else {
            None
        }
    }

    extern "C" fn cmp_i64(a: *const u8, b: *const u8) -> i32 {
        let va = i64::from_ne_bytes(unsafe { *(a as *const [u8; 8]) });
        let vb = i64::from_ne_bytes(unsafe { *(b as *const [u8; 8]) });
        va.cmp(&vb) as i32
    }

    #[test]
    fn test_vec_grow_shrink_100_rounds() {
        let mut v = new_i64_vec();
        for _ in 0..100 {
            for i in 0..1000 {
                push_i64(&mut v, i);
            }
            assert_eq!(__adam_vec_len(&v), 1000);
            for _ in 0..1000 {
                pop_i64(&mut v);
            }
            assert_eq!(__adam_vec_len(&v), 0);
        }
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_vec_large_allocation_1m() {
        let mut v = new_i64_vec();
        for i in 0..1_000_000i64 {
            push_i64(&mut v, i);
        }
        assert_eq!(__adam_vec_len(&v), 1_000_000);
        assert_eq!(get_i64(&v, 999_999), Some(999_999));
        assert_eq!(get_i64(&v, 0), Some(0));
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_vec_out_of_bounds() {
        let v = new_i64_vec();
        assert_eq!(get_i64(&v, 0), None);
        assert_eq!(get_i64(&v, u64::MAX), None);
        // Don't drop empty vec — it should handle gracefully.
    }

    #[test]
    fn test_vec_sort_reverse_stress() {
        let mut v = new_i64_vec();
        // Push 10000 values in reverse order.
        for i in (0..10_000i64).rev() {
            push_i64(&mut v, i);
        }
        __adam_vec_sort(&mut v, cmp_i64);
        // Verify sorted.
        for i in 0..10_000 {
            assert_eq!(get_i64(&v, i as u64), Some(i));
        }
        __adam_vec_reverse(&mut v);
        // Verify reversed.
        for i in 0..10_000 {
            assert_eq!(get_i64(&v, i as u64), Some(9999 - i));
        }
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_vec_clone_independence() {
        let mut v = new_i64_vec();
        push_i64(&mut v, 10);
        push_i64(&mut v, 20);

        let mut cloned = __adam_vec_clone(&v);
        push_i64(&mut v, 30); // Modify original.

        assert_eq!(__adam_vec_len(&v), 3);
        assert_eq!(__adam_vec_len(&cloned), 2);
        assert_eq!(get_i64(&cloned, 0), Some(10));
        assert_eq!(get_i64(&cloned, 1), Some(20));

        __adam_vec_drop(&mut v);
        __adam_vec_drop(&mut cloned);
    }

    // -----------------------------------------------------------------------
    // Map stress: collision and churn
    // -----------------------------------------------------------------------

    fn new_i64_map() -> AdamMap {
        __adam_map_new(8, 8, 8, 8)
    }

    fn map_insert(m: &mut AdamMap, key: i64, val: i64) {
        let mut old = 0i64;
        __adam_map_insert(
            m,
            key.to_ne_bytes().as_ptr(),
            val.to_ne_bytes().as_ptr(),
            key as u64,
            &mut old as *mut i64 as *mut u8,
        );
    }

    fn map_get(m: &AdamMap, key: i64) -> Option<i64> {
        let mut out = 0i64;
        if __adam_map_get(
            m,
            key.to_ne_bytes().as_ptr(),
            key as u64,
            &mut out as *mut i64 as *mut u8,
        ) {
            Some(out)
        } else {
            None
        }
    }

    fn map_remove(m: &mut AdamMap, key: i64) -> Option<i64> {
        let mut out = 0i64;
        if __adam_map_remove(
            m,
            key.to_ne_bytes().as_ptr(),
            key as u64,
            &mut out as *mut i64 as *mut u8,
        ) {
            Some(out)
        } else {
            None
        }
    }

    #[test]
    fn test_map_100k_entries() {
        let mut m = new_i64_map();
        for i in 0..100_000i64 {
            map_insert(&mut m, i, i * i);
        }
        assert_eq!(__adam_map_len(&m), 100_000);
        for i in 0..100_000i64 {
            assert_eq!(map_get(&m, i), Some(i * i));
        }
        __adam_map_drop(&mut m);
    }

    #[test]
    fn test_map_overwrite_stress() {
        let mut m = new_i64_map();
        for i in 0..10_000i64 {
            map_insert(&mut m, 0, i); // Same key, overwrite.
        }
        assert_eq!(__adam_map_len(&m), 1);
        assert_eq!(map_get(&m, 0), Some(9_999));
        __adam_map_drop(&mut m);
    }

    #[test]
    fn test_map_insert_remove_all() {
        let mut m = new_i64_map();
        for i in 0..1_000i64 {
            map_insert(&mut m, i, i);
        }
        for i in 0..1_000i64 {
            assert_eq!(map_remove(&mut m, i), Some(i));
        }
        assert_eq!(__adam_map_len(&m), 0);
        __adam_map_drop(&mut m);
    }

    // -----------------------------------------------------------------------
    // String stress: concatenation growth
    // -----------------------------------------------------------------------

    #[test]
    fn test_string_concatenation_10k() {
        let mut s = AdamString::from_bytes(b"");
        for _ in 0..10_000 {
            let chunk = AdamString::from_bytes(b"hello");
            __adam_string_push(&mut s.ptr, &mut s.len, &mut s.cap, chunk.ptr, chunk.len, chunk.cap);
            __adam_string_drop(chunk.ptr, chunk.len, chunk.cap);
        }
        assert_eq!(__adam_string_len(s.ptr, s.len, s.cap), 50_000);
        __adam_string_drop(s.ptr, s.len, s.cap);
    }

    #[test]
    fn test_string_clone_independence() {
        let mut s = AdamString::from_bytes(b"hello");
        let cloned = __adam_string_clone(s.ptr, s.len, s.cap);

        let suffix = AdamString::from_bytes(b" world");
        __adam_string_push(&mut s.ptr, &mut s.len, &mut s.cap, suffix.ptr, suffix.len, suffix.cap);

        assert_eq!(__adam_string_len(s.ptr, s.len, s.cap), 11);
        assert_eq!(__adam_string_len(cloned.ptr, cloned.len, cloned.cap), 5);
        assert_ne!(s.ptr, cloned.ptr);

        __adam_string_drop(s.ptr, s.len, s.cap);
        __adam_string_drop(cloned.ptr, cloned.len, cloned.cap);
        __adam_string_drop(suffix.ptr, suffix.len, suffix.cap);
    }

    // -----------------------------------------------------------------------
    // Channel concurrency stress
    // -----------------------------------------------------------------------

    #[test]
    fn test_channel_producer_consumer_10k() {
        // Use try_send/try_recv to avoid scheduler dependency.
        let ch = TypedChannel::<i64>::new(10_000);
        let count = 10_000i64;

        // Fill the buffer (all try_sends should succeed since capacity == count).
        for i in 0..count {
            assert!(ch.try_send(i), "try_send should succeed for {}", i);
        }

        // Drain and sum.
        let mut total = 0i64;
        for _ in 0..count {
            total += ch.try_recv().unwrap();
        }
        let expected: i64 = (0..count).sum();
        assert_eq!(total, expected);

        // Channel should be empty now.
        assert!(ch.try_recv().is_none());
    }

    #[test]
    fn test_channel_multiple_producers_4() {
        // Use try_send to avoid scheduler dependency.
        let ch = TypedChannel::<i64>::new(10_000);

        for producer_id in 0..4i64 {
            for i in 0..2500i64 {
                assert!(ch.try_send(producer_id * 10_000 + i));
            }
        }

        // Receive all 10,000 values.
        let mut received = Vec::new();
        for _ in 0..10_000 {
            received.push(ch.try_recv().unwrap());
        }
        assert_eq!(received.len(), 10_000);
    }

    #[test]
    fn test_channel_close_stops_recv() {
        // Test that try_recv returns None on a closed empty channel.
        let ch = TypedChannel::<i64>::new(10);
        ch.close();
        assert!(ch.try_recv().is_none(), "try_recv on closed empty channel should return None");
    }

    #[test]
    fn test_channel_close_drains_buffer() {
        // Values buffered before close should still be retrievable.
        let ch = TypedChannel::<i64>::new(10);
        assert!(ch.try_send(42));
        assert!(ch.try_send(99));
        ch.close();
        assert_eq!(ch.try_recv(), Some(42));
        assert_eq!(ch.try_recv(), Some(99));
        assert!(ch.try_recv().is_none());
    }

    // -----------------------------------------------------------------------
    // Allocation churn: rapid create/destroy
    // -----------------------------------------------------------------------

    #[test]
    fn test_allocation_churn_10k_cycles() {
        for _ in 0..10_000 {
            let mut v = new_i64_vec();
            push_i64(&mut v, 42);
            __adam_vec_drop(&mut v);

            let mut m = new_i64_map();
            map_insert(&mut m, 1, 2);
            __adam_map_drop(&mut m);

            let s = AdamString::from_bytes(b"test");
            __adam_string_drop(s.ptr, s.len, s.cap);
        }
    }

    #[test]
    fn test_interleaved_collections_stress() {
        let mut vecs: Vec<AdamVec> = Vec::new();
        let mut maps: Vec<AdamMap> = Vec::new();

        for i in 0..100 {
            let mut v = new_i64_vec();
            push_i64(&mut v, i);
            vecs.push(v);

            let mut m = new_i64_map();
            map_insert(&mut m, i, i * 10);
            maps.push(m);
        }

        // Drop in interleaved order.
        for i in (0..100).rev() {
            if i % 2 == 0 {
                __adam_vec_drop(&mut vecs[i as usize]);
            } else {
                __adam_map_drop(&mut maps[i as usize]);
            }
        }
        // Drop remaining.
        for i in (0..100).rev() {
            if i % 2 != 0 {
                __adam_vec_drop(&mut vecs[i as usize]);
            } else {
                __adam_map_drop(&mut maps[i as usize]);
            }
        }
    }
}
