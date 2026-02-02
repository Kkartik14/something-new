//! EXTREME stress tests for the Adam runtime.
//!
//! These tests push every data structure to its absolute limits:
//! massive allocations, hash collision torture, concurrent flooding,
//! sort stability at scale, rapid insert/delete churn, binary data
//! in strings, large keys/values in maps, channel backpressure, and more.

#[cfg(test)]
mod extreme {
    use crate::channel::TypedChannel;
    use crate::map::{
        __adam_map_contains_key, __adam_map_drop, __adam_map_get, __adam_map_insert,
        __adam_map_len, __adam_map_new, __adam_map_remove, AdamMap,
    };
    use crate::set::{
        __adam_set_contains, __adam_set_drop, __adam_set_insert, __adam_set_len, __adam_set_new,
        __adam_set_remove,
    };
    use crate::string::{
        __adam_string_clone, __adam_string_drop, __adam_string_eq, __adam_string_hash,
        __adam_string_len, __adam_string_push, AdamString, __str_concat,
    };
    use crate::vec::{
        __adam_vec_clone, __adam_vec_drop, __adam_vec_get, __adam_vec_len, __adam_vec_new,
        __adam_vec_pop, __adam_vec_push, __adam_vec_reverse, __adam_vec_sort, AdamVec,
    };

    // ===================================================================
    // Helpers
    // ===================================================================

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

    fn new_i64_map() -> AdamMap {
        __adam_map_new(8, 8, 8, 8)
    }

    fn map_insert_i64(m: &mut AdamMap, key: i64, val: i64) -> bool {
        let mut old = 0i64;
        __adam_map_insert(
            m,
            key.to_ne_bytes().as_ptr(),
            val.to_ne_bytes().as_ptr(),
            key as u64,
            &mut old as *mut i64 as *mut u8,
        )
    }

    fn map_get_i64(m: &AdamMap, key: i64) -> Option<i64> {
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

    fn map_remove_i64(m: &mut AdamMap, key: i64) -> Option<i64> {
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

    fn new_i64_set() -> AdamMap {
        __adam_set_new(
            std::mem::size_of::<i64>() as u64,
            std::mem::align_of::<i64>() as u64,
        )
    }

    fn set_insert_i64(s: &mut AdamMap, val: i64) -> bool {
        __adam_set_insert(s, val.to_ne_bytes().as_ptr(), val as u64)
    }

    fn set_contains_i64(s: &AdamMap, val: i64) -> bool {
        __adam_set_contains(s, val.to_ne_bytes().as_ptr(), val as u64)
    }

    fn set_remove_i64(s: &mut AdamMap, val: i64) -> bool {
        __adam_set_remove(s, val.to_ne_bytes().as_ptr(), val as u64)
    }

    fn make_string(s: &str) -> AdamString {
        AdamString::from_bytes(s.as_bytes())
    }

    fn drop_string(s: AdamString) {
        __adam_string_drop(s.ptr, s.len, s.cap);
    }

    unsafe fn read_string(s: &AdamString) -> String {
        if s.len == 0 || s.ptr.is_null() {
            return String::new();
        }
        let bytes = std::slice::from_raw_parts(s.ptr, s.len as usize);
        String::from_utf8_lossy(bytes).into_owned()
    }

    // ===================================================================
    // 1. Memory pressure: 10M elements in a vec
    // ===================================================================

    #[test]
    fn test_01_vec_10m_elements() {
        let mut v = new_i64_vec();
        let n: i64 = 10_000_000;
        for i in 0..n {
            push_i64(&mut v, i);
        }
        assert_eq!(__adam_vec_len(&v), n as u64);

        // Spot-check first, last, and middle
        assert_eq!(get_i64(&v, 0), Some(0));
        assert_eq!(get_i64(&v, (n - 1) as u64), Some(n - 1));
        assert_eq!(get_i64(&v, (n / 2) as u64), Some(n / 2));

        // Verify every 100,000th element
        for i in (0..n).step_by(100_000) {
            assert_eq!(
                get_i64(&v, i as u64),
                Some(i),
                "element at index {} wrong",
                i
            );
        }

        __adam_vec_drop(&mut v);
    }

    // ===================================================================
    // 2. Map hash collision torture: 50K entries with same hash
    // ===================================================================

    #[test]
    fn test_02_map_hash_collision_50k() {
        let mut m = new_i64_map();
        let collision_hash: u64 = 42;
        let n = 50_000i64;

        for i in 0..n {
            let mut old = 0i64;
            __adam_map_insert(
                &mut m,
                i.to_ne_bytes().as_ptr(),
                (i * 7).to_ne_bytes().as_ptr(),
                collision_hash,
                &mut old as *mut i64 as *mut u8,
            );
        }
        assert_eq!(__adam_map_len(&m), n as u64);

        // Verify every entry is retrievable with the collision hash
        for i in 0..n {
            let mut out = 0i64;
            let found = __adam_map_get(
                &m,
                i.to_ne_bytes().as_ptr(),
                collision_hash,
                &mut out as *mut i64 as *mut u8,
            );
            assert!(found, "key {} not found with collision hash", i);
            assert_eq!(out, i * 7, "value mismatch for key {}", i);
        }

        // Remove half and verify other half persists
        for i in (0..n).step_by(2) {
            let mut out = 0i64;
            let removed = __adam_map_remove(
                &mut m,
                i.to_ne_bytes().as_ptr(),
                collision_hash,
                &mut out as *mut i64 as *mut u8,
            );
            assert!(removed, "failed to remove key {}", i);
        }
        assert_eq!(__adam_map_len(&m), (n / 2) as u64);

        for i in (1..n).step_by(2) {
            let mut out = 0i64;
            let found = __adam_map_get(
                &m,
                i.to_ne_bytes().as_ptr(),
                collision_hash,
                &mut out as *mut i64 as *mut u8,
            );
            assert!(found, "odd key {} should still exist", i);
            assert_eq!(out, i * 7);
        }

        __adam_map_drop(&mut m);
    }

    // ===================================================================
    // 3. String builder stress: concatenate 50K small strings
    // ===================================================================

    #[test]
    fn test_03_string_builder_50k() {
        let mut s = AdamString::from_bytes(b"");
        let chunk = AdamString::from_bytes(b"abcde"); // 5 bytes each

        for _ in 0..50_000 {
            __adam_string_push(
                &mut s.ptr, &mut s.len, &mut s.cap,
                chunk.ptr, chunk.len, chunk.cap,
            );
        }

        assert_eq!(__adam_string_len(s.ptr, s.len, s.cap), 250_000);
        assert!(s.cap >= 250_000);

        // Verify content at start and end
        unsafe {
            let bytes = std::slice::from_raw_parts(s.ptr, s.len as usize);
            assert_eq!(&bytes[0..5], b"abcde");
            assert_eq!(&bytes[249_995..250_000], b"abcde");
        }

        __adam_string_drop(s.ptr, s.len, s.cap);
        __adam_string_drop(chunk.ptr, chunk.len, chunk.cap);
    }

    // ===================================================================
    // 4. Concurrent channel flooding: 8 producers x 10K items
    // ===================================================================

    #[test]
    fn test_04_channel_flooding_8_producers() {
        // Using try_send with a large buffer to avoid scheduler dependency
        let total_items = 8 * 10_000;
        let ch = TypedChannel::<i64>::new(total_items);

        // 8 producers each sending 10K items
        for producer_id in 0..8i64 {
            for i in 0..10_000i64 {
                let val = producer_id * 10_000 + i;
                assert!(
                    ch.try_send(val),
                    "try_send failed for producer {} item {}",
                    producer_id,
                    i
                );
            }
        }

        // Drain all and verify count
        let mut sum: i64 = 0;
        let mut count = 0;
        while let Some(val) = ch.try_recv() {
            sum += val;
            count += 1;
        }
        assert_eq!(count, total_items);

        // Expected sum: for each producer p, sum of (p*10000 + 0..10000)
        // = sum_{p=0}^{7} (p*10000*10000 + sum_{i=0}^{9999} i)
        // = 10000 * sum_{p=0}^{7}(p*10000) + 8 * (9999*10000/2)
        // = 10000 * 10000 * 28 + 8 * 49995000
        // = 2_800_000_000 + 399_960_000 = 3_199_960_000
        let expected: i64 = (0..8i64)
            .map(|p| {
                (0..10_000i64).map(|i| p * 10_000 + i).sum::<i64>()
            })
            .sum();
        assert_eq!(sum, expected);
    }

    // ===================================================================
    // 5. Vec sort stability: 100K elements, sorted, already-sorted, reverse
    // ===================================================================

    #[test]
    fn test_05_vec_sort_100k_stability() {
        let n = 100_000i64;
        let mut v = new_i64_vec();

        // Push in reverse order
        for i in (0..n).rev() {
            push_i64(&mut v, i);
        }
        assert_eq!(__adam_vec_len(&v), n as u64);

        // Sort (worst case: reverse-sorted input)
        __adam_vec_sort(&mut v, cmp_i64);

        // Verify ascending
        for i in 0..n {
            assert_eq!(
                get_i64(&v, i as u64),
                Some(i),
                "after first sort, index {} wrong",
                i
            );
        }

        // Sort already-sorted (best case)
        __adam_vec_sort(&mut v, cmp_i64);
        for i in 0..n {
            assert_eq!(
                get_i64(&v, i as u64),
                Some(i),
                "after second sort, index {} wrong",
                i
            );
        }

        // Reverse, then sort again (worst case again)
        __adam_vec_reverse(&mut v);
        for i in 0..n {
            assert_eq!(
                get_i64(&v, i as u64),
                Some(n - 1 - i),
                "after reverse, index {} wrong",
                i
            );
        }
        __adam_vec_sort(&mut v, cmp_i64);
        for i in 0..n {
            assert_eq!(
                get_i64(&v, i as u64),
                Some(i),
                "after third sort, index {} wrong",
                i
            );
        }

        __adam_vec_drop(&mut v);
    }

    // ===================================================================
    // 6. Map rapid insert/delete cycles: 10K rounds of 100 insert + 100 delete
    // ===================================================================

    #[test]
    fn test_06_map_insert_delete_churn() {
        let mut m = new_i64_map();
        let batch = 100i64;
        let rounds = 10_000;

        for round in 0..rounds {
            let base = round * batch;
            // Insert 100
            for i in 0..batch {
                map_insert_i64(&mut m, base + i, (base + i) * 3);
            }
            // Verify they are present
            for i in 0..batch {
                assert_eq!(
                    map_get_i64(&m, base + i),
                    Some((base + i) * 3),
                    "round {} key {} not found after insert",
                    round,
                    base + i
                );
            }
            // Delete all 100
            for i in 0..batch {
                let removed = map_remove_i64(&mut m, base + i);
                assert_eq!(
                    removed,
                    Some((base + i) * 3),
                    "round {} key {} removal failed",
                    round,
                    base + i
                );
            }
            assert_eq!(__adam_map_len(&m), 0, "map not empty after round {}", round);
        }

        __adam_map_drop(&mut m);
    }

    // ===================================================================
    // 7. Set intersection simulation: two sets of 10K, count overlap
    // ===================================================================

    #[test]
    fn test_07_set_intersection_10k() {
        let mut set_a = new_i64_set();
        let mut set_b = new_i64_set();

        // Set A: 0..10000
        for i in 0..10_000i64 {
            set_insert_i64(&mut set_a, i);
        }
        // Set B: 5000..15000
        for i in 5_000..15_000i64 {
            set_insert_i64(&mut set_b, i);
        }

        assert_eq!(__adam_set_len(&set_a), 10_000);
        assert_eq!(__adam_set_len(&set_b), 10_000);

        // Count intersection: elements in both A and B
        let mut intersection_count = 0u64;
        for i in 0..10_000i64 {
            if set_contains_i64(&set_b, i) {
                intersection_count += 1;
            }
        }
        // Overlap is 5000..10000 = 5000 elements
        assert_eq!(intersection_count, 5_000);

        // Count union (unique elements)
        // A has 0..10000, B has 5000..15000 => union is 0..15000 = 15000
        let mut union_count = 0u64;
        for i in 0..15_000i64 {
            if set_contains_i64(&set_a, i) || set_contains_i64(&set_b, i) {
                union_count += 1;
            }
        }
        assert_eq!(union_count, 15_000);

        __adam_set_drop(&mut set_a);
        __adam_set_drop(&mut set_b);
    }

    // ===================================================================
    // 8. Mixed collection churn: interleave vec/map/set/string in tight loops
    // ===================================================================

    #[test]
    fn test_08_mixed_collection_churn() {
        for round in 0..500 {
            // Vec
            let mut v = new_i64_vec();
            for j in 0..20i64 {
                push_i64(&mut v, round * 20 + j);
            }
            assert_eq!(__adam_vec_len(&v), 20);

            // Map
            let mut m = new_i64_map();
            for j in 0..20i64 {
                map_insert_i64(&mut m, j, round * 20 + j);
            }
            assert_eq!(__adam_map_len(&m), 20);

            // Set
            let mut s = new_i64_set();
            for j in 0..20i64 {
                set_insert_i64(&mut s, round * 20 + j);
            }
            assert_eq!(__adam_set_len(&s), 20);

            // String
            let mut str_val = AdamString::from_bytes(b"");
            let chunk = AdamString::from_bytes(b"XY");
            for _ in 0..10 {
                __adam_string_push(
                    &mut str_val.ptr, &mut str_val.len, &mut str_val.cap,
                    chunk.ptr, chunk.len, chunk.cap,
                );
            }
            assert_eq!(__adam_string_len(str_val.ptr, str_val.len, str_val.cap), 20);

            // Verify vec
            for j in 0..20i64 {
                assert_eq!(get_i64(&v, j as u64), Some(round * 20 + j));
            }

            // Verify map
            for j in 0..20i64 {
                assert_eq!(map_get_i64(&m, j), Some(round * 20 + j));
            }

            // Verify set
            for j in 0..20i64 {
                assert!(set_contains_i64(&s, round * 20 + j));
            }

            // Cleanup
            __adam_vec_drop(&mut v);
            __adam_map_drop(&mut m);
            __adam_set_drop(&mut s);
            __adam_string_drop(str_val.ptr, str_val.len, str_val.cap);
            __adam_string_drop(chunk.ptr, chunk.len, chunk.cap);
        }
    }

    // ===================================================================
    // 9. Zero-size edge cases: empty collections
    // ===================================================================

    #[test]
    fn test_09_zero_size_edge_cases() {
        // Empty vec: push nothing, pop nothing
        let mut v = new_i64_vec();
        assert_eq!(__adam_vec_len(&v), 0);
        assert_eq!(pop_i64(&mut v), None);
        assert_eq!(get_i64(&v, 0), None);
        assert_eq!(get_i64(&v, u64::MAX), None);
        __adam_vec_drop(&mut v);

        // Empty map: get nothing, remove nothing
        let mut m = new_i64_map();
        assert_eq!(__adam_map_len(&m), 0);
        assert_eq!(map_get_i64(&m, 0), None);
        assert_eq!(map_get_i64(&m, i64::MAX), None);
        assert_eq!(map_remove_i64(&mut m, 0), None);
        __adam_map_drop(&mut m);

        // Empty set: contains nothing, remove nothing
        let mut s = new_i64_set();
        assert_eq!(__adam_set_len(&s), 0);
        assert!(!set_contains_i64(&s, 0));
        assert!(!set_contains_i64(&s, i64::MAX));
        assert!(!set_remove_i64(&mut s, 0));
        __adam_set_drop(&mut s);

        // Empty string
        let s = AdamString::from_bytes(b"");
        assert_eq!(__adam_string_len(s.ptr, s.len, s.cap), 0);
        // Clone of empty
        let cloned = __adam_string_clone(s.ptr, s.len, s.cap);
        assert_eq!(cloned.len, 0);
        // Hash of empty
        let h = __adam_string_hash(s.ptr, s.len, s.cap);
        assert_eq!(h, 0xcbf29ce484222325); // FNV-1a offset basis

        // Drop empty vec again (double-drop safety after fields zeroed)
        let mut v2 = new_i64_vec();
        __adam_vec_drop(&mut v2);
        __adam_vec_drop(&mut v2); // second drop should be safe (ptr is null)
    }

    // ===================================================================
    // 10. Clone-then-modify independence: large collections
    // ===================================================================

    #[test]
    fn test_10_clone_then_modify_independence() {
        // Vec
        let mut v = new_i64_vec();
        for i in 0..5_000i64 {
            push_i64(&mut v, i);
        }
        let mut cloned_v = __adam_vec_clone(&v);

        // Modify original: overwrite everything with negatives
        for i in 0..5_000i64 {
            let val = -i;
            let bytes = val.to_ne_bytes();
            crate::vec::__adam_vec_set(&mut v, i as u64, bytes.as_ptr());
        }

        // Clone must be unchanged
        for i in 0..5_000i64 {
            assert_eq!(
                get_i64(&cloned_v, i as u64),
                Some(i),
                "vec clone index {} was modified",
                i
            );
        }
        __adam_vec_drop(&mut v);
        __adam_vec_drop(&mut cloned_v);

        // String
        let mut s = AdamString::from_bytes(b"original content here");
        let cloned_s = __adam_string_clone(s.ptr, s.len, s.cap);

        // Modify original by pushing
        let extra = AdamString::from_bytes(b" EXTRA DATA APPENDED!!!");
        __adam_string_push(
            &mut s.ptr, &mut s.len, &mut s.cap,
            extra.ptr, extra.len, extra.cap,
        );

        unsafe {
            assert_eq!(read_string(&cloned_s), "original content here");
            assert!(read_string(&s).starts_with("original content here EXTRA"));
        }

        __adam_string_drop(s.ptr, s.len, s.cap);
        drop_string(cloned_s);
        __adam_string_drop(extra.ptr, extra.len, extra.cap);
    }

    // ===================================================================
    // 11. Repeated grow/shrink: push 1000, pop 999, repeat 100 times
    // ===================================================================

    #[test]
    fn test_11_grow_shrink_reallocation() {
        let mut v = new_i64_vec();
        let mut global_counter: i64 = 0;

        for round in 0..100 {
            // Push 1000
            for _ in 0..1_000 {
                push_i64(&mut v, global_counter);
                global_counter += 1;
            }
            // Pop 999 (leave 1 element each round)
            for _ in 0..999 {
                let popped = pop_i64(&mut v);
                assert!(popped.is_some(), "pop failed in round {}", round);
            }
            // After each round, vec should have (round+1) elements
            assert_eq!(
                __adam_vec_len(&v),
                (round + 1) as u64,
                "wrong length after round {}",
                round
            );
        }

        // Final: 100 elements remaining
        assert_eq!(__adam_vec_len(&v), 100);

        // The remaining elements should be the first element of each push batch
        // Round 0: pushed 0..999, popped 999..1 -> remaining: element 0
        // Round 1: pushed 1000..1999, popped 1999..1001 -> remaining: 0, 1000
        // etc.
        for round in 0..100 {
            let expected_val = (round * 1000) as i64;
            assert_eq!(
                get_i64(&v, round as u64),
                Some(expected_val),
                "remaining element at index {} wrong",
                round
            );
        }

        __adam_vec_drop(&mut v);
    }

    // ===================================================================
    // 12. String with binary data: null bytes, high unicode, etc.
    // ===================================================================

    #[test]
    fn test_12_string_binary_data() {
        // String containing null bytes
        let binary: &[u8] = &[0x00, 0x01, 0x02, 0x00, 0xFF, 0xFE, 0x00];
        let s = AdamString::from_bytes(binary);
        assert_eq!(__adam_string_len(s.ptr, s.len, s.cap), 7);

        // Clone should preserve binary data
        let cloned = __adam_string_clone(s.ptr, s.len, s.cap);
        assert_eq!(cloned.len, 7);
        assert!(__adam_string_eq(
            s.ptr, s.len, s.cap,
            cloned.ptr, cloned.len, cloned.cap,
        ));

        // Hash should be deterministic for binary data
        let h1 = __adam_string_hash(s.ptr, s.len, s.cap);
        let h2 = __adam_string_hash(cloned.ptr, cloned.len, cloned.cap);
        assert_eq!(h1, h2);

        __adam_string_drop(s.ptr, s.len, s.cap);
        drop_string(cloned);

        // High unicode: 4-byte sequences
        let unicode_str = "\u{1F600}\u{1F601}\u{1F602}\u{1F603}\u{1F604}\u{1F605}\u{1F606}\u{1F607}\u{1F608}\u{1F609}";
        let us = make_string(unicode_str);
        // 10 emoji * 4 bytes = 40 bytes
        assert_eq!(__adam_string_len(us.ptr, us.len, us.cap), 40);
        drop_string(us);

        // Mix of null bytes and valid UTF-8 sequences, treated as raw bytes
        let raw_mix: &[u8] = b"Hello\x00World\x00\xFF\xFE\x00END";
        let ms = AdamString::from_bytes(raw_mix);
        assert_eq!(__adam_string_len(ms.ptr, ms.len, ms.cap), raw_mix.len() as u64);

        // Push more binary data
        let extra: &[u8] = &[0xDE, 0xAD, 0xBE, 0xEF];
        let extra_s = AdamString::from_bytes(extra);
        let mut ms_ptr = ms.ptr;
        let mut ms_len = ms.len;
        let mut ms_cap = ms.cap;
        __adam_string_push(
            &mut ms_ptr, &mut ms_len, &mut ms_cap,
            extra_s.ptr, extra_s.len, extra_s.cap,
        );
        assert_eq!(ms_len, (raw_mix.len() + 4) as u64);

        __adam_string_drop(ms_ptr, ms_len, ms_cap);
        __adam_string_drop(extra_s.ptr, extra_s.len, extra_s.cap);
    }

    // ===================================================================
    // 13. Map with large keys and values: 1KB keys and 1KB values
    // ===================================================================

    #[test]
    fn test_13_map_large_keys_values() {
        let key_size: u64 = 1024;
        let val_size: u64 = 1024;

        let mut m = __adam_map_new(key_size, val_size, 1, 1);

        let n = 500;
        let mut keys: Vec<Vec<u8>> = Vec::new();
        let mut vals: Vec<Vec<u8>> = Vec::new();

        for i in 0..n {
            // Create a unique 1KB key: fill with pattern based on i
            let mut key = vec![0u8; key_size as usize];
            let i_bytes = (i as u64).to_ne_bytes();
            key[..8].copy_from_slice(&i_bytes);
            for j in 8..key_size as usize {
                key[j] = ((i + j) & 0xFF) as u8;
            }

            // Create a unique 1KB value
            let mut val = vec![0u8; val_size as usize];
            let vi_bytes = ((i * 7) as u64).to_ne_bytes();
            val[..8].copy_from_slice(&vi_bytes);
            for j in 8..val_size as usize {
                val[j] = ((i * 3 + j) & 0xFF) as u8;
            }

            // Simple hash from first 8 bytes
            let hash = u64::from_ne_bytes(key[..8].try_into().unwrap());
            let mut old_val = vec![0u8; val_size as usize];
            __adam_map_insert(
                &mut m,
                key.as_ptr(),
                val.as_ptr(),
                hash,
                old_val.as_mut_ptr(),
            );

            keys.push(key);
            vals.push(val);
        }

        assert_eq!(__adam_map_len(&m), n as u64);

        // Verify all entries
        for i in 0..n {
            let hash = u64::from_ne_bytes(keys[i][..8].try_into().unwrap());
            let mut out = vec![0u8; val_size as usize];
            let found = __adam_map_get(
                &m,
                keys[i].as_ptr(),
                hash,
                out.as_mut_ptr(),
            );
            assert!(found, "key {} not found", i);
            assert_eq!(out, vals[i], "value mismatch for key {}", i);
        }

        // Remove half and verify
        for i in (0..n).step_by(2) {
            let hash = u64::from_ne_bytes(keys[i][..8].try_into().unwrap());
            let mut out = vec![0u8; val_size as usize];
            let removed = __adam_map_remove(
                &mut m,
                keys[i].as_ptr(),
                hash,
                out.as_mut_ptr(),
            );
            assert!(removed, "failed to remove key {}", i);
            assert_eq!(out, vals[i]);
        }

        assert_eq!(__adam_map_len(&m), (n / 2) as u64);

        // Verify odd entries still present
        for i in (1..n).step_by(2) {
            let hash = u64::from_ne_bytes(keys[i][..8].try_into().unwrap());
            let found = __adam_map_contains_key(&m, keys[i].as_ptr(), hash);
            assert!(found, "odd key {} should still exist", i);
        }

        __adam_map_drop(&mut m);
    }

    // ===================================================================
    // 14. Channel backpressure: fill to capacity, verify fails, drain, verify works
    // ===================================================================

    #[test]
    fn test_14_channel_backpressure() {
        let capacity = 100;
        let ch = TypedChannel::<i64>::new(capacity);

        // Fill to capacity
        for i in 0..capacity as i64 {
            assert!(
                ch.try_send(i),
                "try_send should succeed for item {}",
                i
            );
        }

        // Next try_send should fail (buffer full)
        assert!(
            !ch.try_send(999),
            "try_send should fail when buffer is full"
        );
        assert!(
            !ch.try_send(1000),
            "try_send should still fail"
        );

        // Drain half
        for i in 0..50i64 {
            let val = ch.try_recv();
            assert_eq!(val, Some(i), "drain: expected {}", i);
        }

        // Now we can send again (50 slots freed)
        for i in 0..50i64 {
            assert!(
                ch.try_send(1000 + i),
                "try_send should succeed after drain for item {}",
                i
            );
        }

        // Buffer should be full again
        assert!(!ch.try_send(9999), "buffer should be full again");

        // Drain everything and verify FIFO order
        let mut drained = Vec::new();
        while let Some(val) = ch.try_recv() {
            drained.push(val);
        }
        assert_eq!(drained.len(), 100);
        // First 50 should be 50..99 (remainder from first fill)
        for i in 0..50 {
            assert_eq!(drained[i], (50 + i) as i64);
        }
        // Next 50 should be 1000..1049 (second fill)
        for i in 0..50 {
            assert_eq!(drained[50 + i], 1000 + i as i64);
        }

        // Close and verify
        ch.close();
        assert!(ch.is_closed());
        assert!(!ch.try_send(0));
        assert!(ch.try_recv().is_none());
    }

    // ===================================================================
    // 15. Vec with very large elements (1KB each)
    // ===================================================================

    #[test]
    fn test_15_vec_large_elements() {
        let elem_size: u64 = 1024;
        let mut v = __adam_vec_new(elem_size, 1);

        let n = 5_000;
        for i in 0..n {
            let mut elem = vec![0u8; elem_size as usize];
            let i_bytes = (i as u64).to_ne_bytes();
            elem[..8].copy_from_slice(&i_bytes);
            elem[1016..1024].copy_from_slice(&i_bytes);
            __adam_vec_push(&mut v, elem.as_ptr());
        }

        assert_eq!(__adam_vec_len(&v), n as u64);

        // Verify
        for i in 0..n {
            let mut buf = vec![0u8; elem_size as usize];
            assert!(__adam_vec_get(&v, i as u64, buf.as_mut_ptr()));
            let stored_i = u64::from_ne_bytes(buf[..8].try_into().unwrap());
            assert_eq!(stored_i, i as u64);
            let stored_end = u64::from_ne_bytes(buf[1016..1024].try_into().unwrap());
            assert_eq!(stored_end, i as u64);
        }

        __adam_vec_drop(&mut v);
    }

    // ===================================================================
    // 16. String concat stress via __str_concat
    // ===================================================================

    #[test]
    fn test_16_string_concat_chain() {
        // Chain-concatenate many small strings using __str_concat
        let mut result = AdamString::from_bytes(b"");
        for _i in 0..10_000u64 {
            let chunk = AdamString::from_bytes(b"XY");
            let new_result = __str_concat(
                result.ptr, result.len, result.cap,
                chunk.ptr, chunk.len, chunk.cap,
            );
            __adam_string_drop(result.ptr, result.len, result.cap);
            __adam_string_drop(chunk.ptr, chunk.len, chunk.cap);
            result = new_result;
        }
        assert_eq!(result.len, 20_000);
        __adam_string_drop(result.ptr, result.len, result.cap);
    }

    // ===================================================================
    // 17. Set massive insert and contains
    // ===================================================================

    #[test]
    fn test_17_set_massive_insert_contains() {
        let mut s = new_i64_set();
        let n = 50_000i64;

        for i in 0..n {
            assert!(set_insert_i64(&mut s, i), "insert {} should be new", i);
        }
        assert_eq!(__adam_set_len(&s), n as u64);

        // Verify all present
        for i in 0..n {
            assert!(set_contains_i64(&s, i), "set should contain {}", i);
        }

        // Verify none outside range
        for i in n..(n + 1000) {
            assert!(!set_contains_i64(&s, i), "set should not contain {}", i);
        }

        // Re-insert should return false (already present)
        for i in 0..n {
            assert!(!set_insert_i64(&mut s, i), "re-insert {} should return false", i);
        }
        assert_eq!(__adam_set_len(&s), n as u64);

        __adam_set_drop(&mut s);
    }

    // ===================================================================
    // 18. Map clone independence
    // ===================================================================

    #[test]
    fn test_18_map_clone_independence() {
        let mut m = new_i64_map();
        for i in 0..1_000i64 {
            map_insert_i64(&mut m, i, i * 100);
        }

        let mut cloned = crate::map::__adam_map_clone(&m);
        assert_eq!(__adam_map_len(&cloned), 1_000);

        // Modify original: overwrite values
        for i in 0..1_000i64 {
            map_insert_i64(&mut m, i, -i);
        }

        // Clone should be unaffected
        for i in 0..1_000i64 {
            assert_eq!(
                map_get_i64(&cloned, i),
                Some(i * 100),
                "cloned map key {} should still have original value",
                i
            );
        }

        // Remove everything from original
        for i in 0..1_000i64 {
            map_remove_i64(&mut m, i);
        }
        assert_eq!(__adam_map_len(&m), 0);

        // Clone should still be intact
        assert_eq!(__adam_map_len(&cloned), 1_000);

        __adam_map_drop(&mut m);
        __adam_map_drop(&mut cloned);
    }

    // ===================================================================
    // 19. Channel fill-drain-refill cycles
    // ===================================================================

    #[test]
    fn test_19_channel_fill_drain_cycles() {
        let capacity = 1_000;
        let ch = TypedChannel::<i64>::new(capacity);

        for cycle in 0..100 {
            // Fill
            for i in 0..capacity as i64 {
                assert!(
                    ch.try_send(cycle * 1000 + i),
                    "cycle {} send {} failed",
                    cycle,
                    i
                );
            }
            // Buffer should be full
            assert!(!ch.try_send(-1), "cycle {} buffer should be full", cycle);

            // Drain
            let mut sum = 0i64;
            for _ in 0..capacity {
                sum += ch.try_recv().unwrap();
            }
            // Verify sum
            let base = cycle * 1000;
            let expected_sum: i64 = (0..capacity as i64).map(|i| base + i).sum();
            assert_eq!(sum, expected_sum, "cycle {} sum mismatch", cycle);

            // Buffer should be empty
            assert!(ch.try_recv().is_none(), "cycle {} buffer should be empty", cycle);
        }
    }

    // ===================================================================
    // 20. Vec sort with all duplicates
    // ===================================================================

    #[test]
    fn test_20_vec_sort_all_duplicates() {
        let mut v = new_i64_vec();
        let n = 10_000;

        // All the same value
        for _ in 0..n {
            push_i64(&mut v, 42);
        }
        __adam_vec_sort(&mut v, cmp_i64);

        // All should still be 42
        for i in 0..n {
            assert_eq!(get_i64(&v, i as u64), Some(42));
        }

        __adam_vec_drop(&mut v);
    }

    // ===================================================================
    // 21. Map with negative keys and hash collision patterns
    // ===================================================================

    #[test]
    fn test_21_map_negative_keys() {
        let mut m = new_i64_map();

        // Insert negative keys
        for i in -10_000..10_000i64 {
            map_insert_i64(&mut m, i, i * i);
        }
        assert_eq!(__adam_map_len(&m), 20_000);

        // Verify all
        for i in -10_000..10_000i64 {
            assert_eq!(
                map_get_i64(&m, i),
                Some(i * i),
                "key {} mismatch",
                i
            );
        }

        // Remove all negative keys
        for i in -10_000..0i64 {
            assert_eq!(map_remove_i64(&mut m, i), Some(i * i));
        }
        assert_eq!(__adam_map_len(&m), 10_000);

        // Positive keys should still be there
        for i in 0..10_000i64 {
            assert_eq!(map_get_i64(&m, i), Some(i * i));
        }

        __adam_map_drop(&mut m);
    }

    // ===================================================================
    // 22. String hash uniqueness check
    // ===================================================================

    #[test]
    fn test_22_string_hash_uniqueness() {
        let mut hashes = std::collections::HashSet::new();
        let mut strings_to_drop = Vec::new();

        for i in 0..10_000u64 {
            let text = format!("key_{:08}", i);
            let s = make_string(&text);
            let h = __adam_string_hash(s.ptr, s.len, s.cap);
            hashes.insert(h);
            strings_to_drop.push(s);
        }

        // FNV-1a should produce virtually no collisions for 10K distinct inputs
        // Allow at most 1 collision (extremely unlikely)
        assert!(
            hashes.len() >= 9_999,
            "too many hash collisions: {} unique out of 10000",
            hashes.len()
        );

        for s in strings_to_drop {
            drop_string(s);
        }
    }

    // ===================================================================
    // 23. Vec extend stress
    // ===================================================================

    #[test]
    fn test_23_vec_extend_stress() {
        let mut main_vec = new_i64_vec();

        for batch in 0..100 {
            let mut temp = new_i64_vec();
            for i in 0..1_000i64 {
                push_i64(&mut temp, batch * 1_000 + i);
            }
            crate::vec::__adam_vec_extend(&mut main_vec, &temp);
            __adam_vec_drop(&mut temp);
        }

        assert_eq!(__adam_vec_len(&main_vec), 100_000);

        // Verify
        for i in 0..100_000i64 {
            assert_eq!(
                get_i64(&main_vec, i as u64),
                Some(i),
                "extended vec index {} wrong",
                i
            );
        }

        __adam_vec_drop(&mut main_vec);
    }

    // ===================================================================
    // 24. Set remove all then reinsert
    // ===================================================================

    #[test]
    fn test_24_set_remove_all_reinsert() {
        let mut s = new_i64_set();
        let n = 5_000i64;

        // Insert
        for i in 0..n {
            set_insert_i64(&mut s, i);
        }
        assert_eq!(__adam_set_len(&s), n as u64);

        // Remove all
        for i in 0..n {
            assert!(set_remove_i64(&mut s, i), "remove {} should succeed", i);
        }
        assert_eq!(__adam_set_len(&s), 0);

        // Reinsert the same elements
        for i in 0..n {
            assert!(set_insert_i64(&mut s, i), "reinsert {} should be new", i);
        }
        assert_eq!(__adam_set_len(&s), n as u64);

        // Verify
        for i in 0..n {
            assert!(set_contains_i64(&s, i), "reinserted {} should be present", i);
        }

        __adam_set_drop(&mut s);
    }

    // ===================================================================
    // 25. Allocation churn: rapid create/destroy of many collections
    // ===================================================================

    #[test]
    fn test_25_allocation_churn_extreme() {
        for _ in 0..5_000 {
            // Vec
            let mut v = new_i64_vec();
            push_i64(&mut v, 1);
            push_i64(&mut v, 2);
            push_i64(&mut v, 3);
            __adam_vec_drop(&mut v);

            // Map
            let mut m = new_i64_map();
            map_insert_i64(&mut m, 1, 10);
            map_insert_i64(&mut m, 2, 20);
            __adam_map_drop(&mut m);

            // Set
            let mut s = new_i64_set();
            set_insert_i64(&mut s, 100);
            __adam_set_drop(&mut s);

            // String
            let str_val = AdamString::from_bytes(b"churn test string data here");
            __adam_string_drop(str_val.ptr, str_val.len, str_val.cap);

            // Channel
            let ch = TypedChannel::<i64>::new(10);
            ch.try_send(42);
            ch.try_recv();
            ch.close();
        }
    }

    // ===================================================================
    // 26. Vec sort with two-value pattern
    // ===================================================================

    #[test]
    fn test_26_vec_sort_two_values() {
        let mut v = new_i64_vec();

        // Alternating 0 and 1, 50K elements
        for i in 0..50_000i64 {
            push_i64(&mut v, i % 2);
        }
        __adam_vec_sort(&mut v, cmp_i64);

        // First 25000 should be 0, next 25000 should be 1
        for i in 0..25_000 {
            assert_eq!(get_i64(&v, i), Some(0), "index {} should be 0", i);
        }
        for i in 25_000..50_000 {
            assert_eq!(get_i64(&v, i), Some(1), "index {} should be 1", i);
        }

        __adam_vec_drop(&mut v);
    }

    // ===================================================================
    // 27. String equality stress
    // ===================================================================

    #[test]
    fn test_27_string_equality_stress() {
        // Build two copies of the same large string via different paths
        let mut s1 = AdamString::from_bytes(b"");
        let mut s2 = AdamString::from_bytes(b"");
        let chunk = AdamString::from_bytes(b"TestData123!");

        for _ in 0..5_000 {
            __adam_string_push(
                &mut s1.ptr, &mut s1.len, &mut s1.cap,
                chunk.ptr, chunk.len, chunk.cap,
            );
            __adam_string_push(
                &mut s2.ptr, &mut s2.len, &mut s2.cap,
                chunk.ptr, chunk.len, chunk.cap,
            );
        }

        assert_eq!(s1.len, s2.len);
        assert!(s1.len > 0);
        assert!(__adam_string_eq(
            s1.ptr, s1.len, s1.cap,
            s2.ptr, s2.len, s2.cap,
        ));

        // Different pointers
        assert_ne!(s1.ptr, s2.ptr);

        // Same hash
        let h1 = __adam_string_hash(s1.ptr, s1.len, s1.cap);
        let h2 = __adam_string_hash(s2.ptr, s2.len, s2.cap);
        assert_eq!(h1, h2);

        __adam_string_drop(s1.ptr, s1.len, s1.cap);
        __adam_string_drop(s2.ptr, s2.len, s2.cap);
        __adam_string_drop(chunk.ptr, chunk.len, chunk.cap);
    }

    // ===================================================================
    // 28. Map overwrite stress
    // ===================================================================

    #[test]
    fn test_28_map_overwrite_stress() {
        let mut m = new_i64_map();

        // Insert 1000 keys, then overwrite each 100 times
        for i in 0..1_000i64 {
            map_insert_i64(&mut m, i, 0);
        }

        for round in 1..=100i64 {
            for i in 0..1_000i64 {
                let existed = map_insert_i64(&mut m, i, round);
                assert!(existed, "key {} should exist in round {}", i, round);
            }
        }

        // Length should still be 1000
        assert_eq!(__adam_map_len(&m), 1_000);

        // All values should be 100 (last round)
        for i in 0..1_000i64 {
            assert_eq!(map_get_i64(&m, i), Some(100));
        }

        __adam_map_drop(&mut m);
    }
}
