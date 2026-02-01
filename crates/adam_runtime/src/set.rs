//! AdamSet — type-erased hash set for the Adam runtime.
//!
//! Thin wrapper around `AdamMap` with zero-sized values (`val_size = 0`).

use crate::map::{
    __adam_map_clear, __adam_map_clone, __adam_map_contains_key, __adam_map_drop,
    __adam_map_insert, __adam_map_is_empty, __adam_map_len, __adam_map_new, __adam_map_remove,
    AdamMap,
};
use std::ptr;

/// Create a new empty set. Returns an `AdamMap` with `val_size = 0`.
#[no_mangle]
pub extern "C" fn __adam_set_new(elem_size: u64, elem_align: u64) -> AdamMap {
    __adam_map_new(elem_size, 0, elem_align, 1)
}

/// Return the number of elements in the set.
#[no_mangle]
pub extern "C" fn __adam_set_len(set: *const AdamMap) -> u64 {
    __adam_map_len(set)
}

/// Return `true` if the set has no elements.
#[no_mangle]
pub extern "C" fn __adam_set_is_empty(set: *const AdamMap) -> bool {
    __adam_map_is_empty(set)
}

/// Insert an element. Returns `true` if the element is **new** (was not already present).
#[no_mangle]
pub extern "C" fn __adam_set_insert(set: *mut AdamMap, val_ptr: *const u8, hash: u64) -> bool {
    // map_insert returns true if the key already existed.
    // For a set, we invert: return true if the element is new.
    let existed = __adam_map_insert(
        set,
        val_ptr,
        ptr::null(),     // no value bytes (val_size == 0)
        hash,
        ptr::null_mut(), // no old value to retrieve
    );
    !existed
}

/// Remove an element. Returns `true` if the element was present and removed.
#[no_mangle]
pub extern "C" fn __adam_set_remove(set: *mut AdamMap, val_ptr: *const u8, hash: u64) -> bool {
    __adam_map_remove(set, val_ptr, hash, ptr::null_mut())
}

/// Check whether the set contains an element.
#[no_mangle]
pub extern "C" fn __adam_set_contains(set: *const AdamMap, val_ptr: *const u8, hash: u64) -> bool {
    __adam_map_contains_key(set, val_ptr, hash)
}

/// Remove all elements. Does not deallocate.
#[no_mangle]
pub extern "C" fn __adam_set_clear(set: *mut AdamMap) {
    __adam_map_clear(set);
}

/// Free the set. Must not be used after this call.
#[no_mangle]
pub extern "C" fn __adam_set_drop(set: *mut AdamMap) {
    __adam_map_drop(set);
}

/// Deep-copy the set.
#[no_mangle]
pub extern "C" fn __adam_set_clone(set: *const AdamMap) -> AdamMap {
    __adam_map_clone(set)
}

// ===================================================================
// Tests
// ===================================================================

#[cfg(test)]
mod tests {
    use super::*;

    /// Trivial hash for i32 testing.
    fn hash_i32(val: i32) -> u64 {
        val as u64
    }

    fn new_i32_set() -> AdamMap {
        __adam_set_new(
            std::mem::size_of::<i32>() as u64,
            std::mem::align_of::<i32>() as u64,
        )
    }

    fn insert_i32(s: &mut AdamMap, val: i32) -> bool {
        __adam_set_insert(s, val.to_ne_bytes().as_ptr(), hash_i32(val))
    }

    fn contains_i32(s: &AdamMap, val: i32) -> bool {
        __adam_set_contains(s, val.to_ne_bytes().as_ptr(), hash_i32(val))
    }

    fn remove_i32(s: &mut AdamMap, val: i32) -> bool {
        __adam_set_remove(s, val.to_ne_bytes().as_ptr(), hash_i32(val))
    }

    // ---------------------------------------------------------------
    // Basic construction
    // ---------------------------------------------------------------

    #[test]
    fn test_new_set_is_empty() {
        let s = new_i32_set();
        assert_eq!(__adam_set_len(&s), 0);
        assert!(__adam_set_is_empty(&s));
    }

    // ---------------------------------------------------------------
    // Insert / Contains
    // ---------------------------------------------------------------

    #[test]
    fn test_insert_and_contains() {
        let mut s = new_i32_set();
        assert!(insert_i32(&mut s, 42));
        assert!(contains_i32(&s, 42));
        assert!(!contains_i32(&s, 99));
        __adam_set_drop(&mut s);
    }

    #[test]
    fn test_insert_duplicate_returns_false() {
        let mut s = new_i32_set();
        assert!(insert_i32(&mut s, 10)); // new
        assert!(!insert_i32(&mut s, 10)); // duplicate
        assert_eq!(__adam_set_len(&s), 1);
        __adam_set_drop(&mut s);
    }

    #[test]
    fn test_insert_multiple() {
        let mut s = new_i32_set();
        for i in 0..20 {
            assert!(insert_i32(&mut s, i));
        }
        assert_eq!(__adam_set_len(&s), 20);
        for i in 0..20 {
            assert!(contains_i32(&s, i));
        }
        assert!(!contains_i32(&s, 99));
        __adam_set_drop(&mut s);
    }

    // ---------------------------------------------------------------
    // Remove
    // ---------------------------------------------------------------

    #[test]
    fn test_remove() {
        let mut s = new_i32_set();
        insert_i32(&mut s, 1);
        insert_i32(&mut s, 2);
        insert_i32(&mut s, 3);
        assert!(remove_i32(&mut s, 2));
        assert!(!contains_i32(&s, 2));
        assert_eq!(__adam_set_len(&s), 2);
        assert!(contains_i32(&s, 1));
        assert!(contains_i32(&s, 3));
        __adam_set_drop(&mut s);
    }

    #[test]
    fn test_remove_missing() {
        let mut s = new_i32_set();
        insert_i32(&mut s, 1);
        assert!(!remove_i32(&mut s, 99));
        assert_eq!(__adam_set_len(&s), 1);
        __adam_set_drop(&mut s);
    }

    // ---------------------------------------------------------------
    // Clear
    // ---------------------------------------------------------------

    #[test]
    fn test_clear() {
        let mut s = new_i32_set();
        for i in 0..10 {
            insert_i32(&mut s, i);
        }
        __adam_set_clear(&mut s);
        assert_eq!(__adam_set_len(&s), 0);
        assert!(__adam_set_is_empty(&s));
        assert!(!contains_i32(&s, 0));
        __adam_set_drop(&mut s);
    }

    // ---------------------------------------------------------------
    // Clone
    // ---------------------------------------------------------------

    #[test]
    fn test_clone() {
        let mut s = new_i32_set();
        insert_i32(&mut s, 10);
        insert_i32(&mut s, 20);
        insert_i32(&mut s, 30);
        let mut c = __adam_set_clone(&s);
        assert_eq!(__adam_set_len(&c), 3);
        assert!(contains_i32(&c, 10));
        assert!(contains_i32(&c, 20));
        assert!(contains_i32(&c, 30));
        // Mutating original should not affect clone.
        remove_i32(&mut s, 10);
        assert!(contains_i32(&c, 10));
        __adam_set_drop(&mut s);
        __adam_set_drop(&mut c);
    }

    #[test]
    fn test_clone_empty() {
        let s = new_i32_set();
        let mut c = __adam_set_clone(&s);
        assert_eq!(__adam_set_len(&c), 0);
        __adam_set_drop(&mut c);
    }

    // ---------------------------------------------------------------
    // Drop
    // ---------------------------------------------------------------

    #[test]
    fn test_drop() {
        let mut s = new_i32_set();
        for i in 0..50 {
            insert_i32(&mut s, i);
        }
        __adam_set_drop(&mut s);
        assert!(s.buckets.is_null());
        assert_eq!(s.count, 0);
    }

    // ---------------------------------------------------------------
    // Adversarial / exhaustive tests
    // ---------------------------------------------------------------

    #[test]
    fn test_insert_1000_elements_all_contained() {
        let mut s = new_i32_set();
        for i in 0..1_000 {
            assert!(insert_i32(&mut s, i), "insert of {} should return true (new)", i);
        }
        assert_eq!(__adam_set_len(&s), 1_000);
        for i in 0..1_000 {
            assert!(
                contains_i32(&s, i),
                "set should contain {}",
                i
            );
        }
        // Elements outside the range should not be contained.
        assert!(!contains_i32(&s, 1_000));
        assert!(!contains_i32(&s, -1));
        __adam_set_drop(&mut s);
    }

    #[test]
    fn test_insert_duplicate_returns_false_stress() {
        let mut s = new_i32_set();
        for i in 0..50 {
            assert!(insert_i32(&mut s, i));
        }
        // Re-inserting every element should return false (already present).
        for i in 0..50 {
            assert!(
                !insert_i32(&mut s, i),
                "duplicate insert of {} should return false",
                i
            );
        }
        assert_eq!(__adam_set_len(&s), 50);
        __adam_set_drop(&mut s);
    }

    #[test]
    fn test_remove_non_existent_returns_false() {
        let mut s = new_i32_set();
        insert_i32(&mut s, 1);
        insert_i32(&mut s, 2);
        insert_i32(&mut s, 3);
        assert!(!remove_i32(&mut s, 99), "removing non-existent should return false");
        assert!(!remove_i32(&mut s, -1), "removing non-existent should return false");
        assert!(!remove_i32(&mut s, 0), "removing non-existent should return false");
        assert_eq!(__adam_set_len(&s), 3);
        __adam_set_drop(&mut s);
    }

    #[test]
    fn test_insert_after_remove_returns_true() {
        let mut s = new_i32_set();
        assert!(insert_i32(&mut s, 42));
        assert!(remove_i32(&mut s, 42));
        assert!(!contains_i32(&s, 42));
        // Re-inserting after removal should be treated as new.
        assert!(insert_i32(&mut s, 42), "insert after remove should return true (new)");
        assert!(contains_i32(&s, 42));
        assert_eq!(__adam_set_len(&s), 1);
        __adam_set_drop(&mut s);
    }

    #[test]
    fn test_clone_populated_set_modify_original() {
        let mut s = new_i32_set();
        for i in 0..100 {
            insert_i32(&mut s, i);
        }
        let mut c = __adam_set_clone(&s);
        // Remove all elements from original.
        for i in 0..100 {
            remove_i32(&mut s, i);
        }
        assert!(__adam_set_is_empty(&s));
        // Clone should be completely unaffected.
        assert_eq!(__adam_set_len(&c), 100);
        for i in 0..100 {
            assert!(contains_i32(&c, i), "clone should still contain {}", i);
        }
        __adam_set_drop(&mut s);
        __adam_set_drop(&mut c);
    }

    #[test]
    fn test_clear_then_repopulate() {
        let mut s = new_i32_set();
        for i in 0..50 {
            insert_i32(&mut s, i);
        }
        __adam_set_clear(&mut s);
        assert!(__adam_set_is_empty(&s));
        // Re-populate with different elements.
        for i in 100..200 {
            assert!(insert_i32(&mut s, i));
        }
        assert_eq!(__adam_set_len(&s), 100);
        // Old elements should not be present.
        for i in 0..50 {
            assert!(!contains_i32(&s, i));
        }
        // New elements should be present.
        for i in 100..200 {
            assert!(contains_i32(&s, i));
        }
        __adam_set_drop(&mut s);
    }

    #[test]
    fn test_set_with_hash_collisions() {
        // Force all elements to have the same hash.
        let mut s = __adam_set_new(
            std::mem::size_of::<i32>() as u64,
            std::mem::align_of::<i32>() as u64,
        );
        let collision_hash: u64 = 7;
        for i in 0..30i32 {
            let is_new = __adam_set_insert(&mut s, i.to_ne_bytes().as_ptr(), collision_hash);
            assert!(is_new, "element {} should be new", i);
        }
        assert_eq!(__adam_set_len(&s), 30);
        // All should be contained.
        for i in 0..30i32 {
            assert!(
                __adam_set_contains(&s, i.to_ne_bytes().as_ptr(), collision_hash),
                "set should contain {} (collision chain)",
                i
            );
        }
        // Remove from the middle of the chain.
        for i in (0..30i32).step_by(2) {
            assert!(__adam_set_remove(&mut s, i.to_ne_bytes().as_ptr(), collision_hash));
        }
        assert_eq!(__adam_set_len(&s), 15);
        // Odd elements should still be present.
        for i in (1..30i32).step_by(2) {
            assert!(
                __adam_set_contains(&s, i.to_ne_bytes().as_ptr(), collision_hash),
                "odd element {} should still be in set after removing evens",
                i
            );
        }
        __adam_set_drop(&mut s);
    }

    #[test]
    fn test_contains_on_empty_set() {
        let s = new_i32_set();
        assert!(!contains_i32(&s, 0));
        assert!(!contains_i32(&s, 42));
        assert!(!contains_i32(&s, -1));
        assert!(!contains_i32(&s, i32::MAX));
        assert!(!contains_i32(&s, i32::MIN));
    }

    #[test]
    fn test_large_set_remove_half_verify_other_half() {
        let mut s = new_i32_set();
        for i in 0..1_000 {
            insert_i32(&mut s, i);
        }
        // Remove even elements.
        for i in (0..1_000).step_by(2) {
            assert!(remove_i32(&mut s, i), "even element {} should be removable", i);
        }
        assert_eq!(__adam_set_len(&s), 500);
        // Verify odd elements remain.
        for i in (1..1_000).step_by(2) {
            assert!(contains_i32(&s, i), "odd element {} should still be present", i);
        }
        // Verify even elements are gone.
        for i in (0..1_000).step_by(2) {
            assert!(!contains_i32(&s, i), "even element {} should have been removed", i);
        }
        __adam_set_drop(&mut s);
    }

    #[test]
    fn test_set_len_decreases_on_remove() {
        let mut s = new_i32_set();
        for i in 0..10 {
            insert_i32(&mut s, i);
        }
        assert_eq!(__adam_set_len(&s), 10);
        for i in 0..10 {
            remove_i32(&mut s, i);
            assert_eq!(
                __adam_set_len(&s),
                (9 - i) as u64,
                "len should be {} after removing {} elements",
                9 - i,
                i + 1
            );
        }
        assert!(__adam_set_is_empty(&s));
        __adam_set_drop(&mut s);
    }

    #[test]
    fn test_remove_from_empty_set() {
        let mut s = new_i32_set();
        assert!(!remove_i32(&mut s, 0));
        assert!(!remove_i32(&mut s, 42));
        assert_eq!(__adam_set_len(&s), 0);
    }
}
