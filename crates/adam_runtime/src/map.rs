//! AdamMap — type-erased hash map for the Adam runtime.
//!
//! Uses open addressing with linear probing.
//! The compiler passes the pre-computed hash at each call site.
//!
//! Entry layout (per bucket):
//!   [hash: u64 (8 bytes)] [key: key_size bytes] [val: val_size bytes] [occupied: u8 (1 byte)]

use std::alloc::{alloc_zeroed, dealloc, Layout};
use std::ptr::{self, copy_nonoverlapping};

/// Default initial capacity (must be a power of two).
const INITIAL_CAPACITY: u64 = 16;

/// Load factor threshold (75%). When `count * 4 >= capacity * 3` we resize.
const LOAD_FACTOR_NUM: u64 = 3;
const LOAD_FACTOR_DEN: u64 = 4;

/// A type-erased hash map.
#[repr(C)]
pub struct AdamMap {
    pub(crate) buckets: *mut u8,
    pub(crate) count: u64,
    pub(crate) capacity: u64,
    pub(crate) key_size: u64,
    pub(crate) val_size: u64,
    pub(crate) key_align: u64,
    pub(crate) val_align: u64,
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

impl AdamMap {
    /// Size of a single bucket entry in bytes.
    #[inline(always)]
    fn entry_size(&self) -> usize {
        // hash(8) + key_size + val_size + occupied(1)
        8 + self.key_size as usize + self.val_size as usize + 1
    }

    /// Pointer to the start of bucket `i`.
    #[inline(always)]
    unsafe fn bucket_ptr(&self, i: u64) -> *mut u8 {
        self.buckets.add(i as usize * self.entry_size())
    }

    /// Offset of the hash field within a bucket (always 0).
    #[inline(always)]
    fn hash_offset(&self) -> usize {
        0
    }

    /// Offset of the key bytes within a bucket.
    #[inline(always)]
    fn key_offset(&self) -> usize {
        8
    }

    /// Offset of the value bytes within a bucket.
    #[inline(always)]
    fn val_offset(&self) -> usize {
        8 + self.key_size as usize
    }

    /// Offset of the `occupied` flag within a bucket.
    #[inline(always)]
    fn occupied_offset(&self) -> usize {
        8 + self.key_size as usize + self.val_size as usize
    }

    /// Read the `occupied` byte for bucket `i`.
    #[inline(always)]
    unsafe fn is_occupied(&self, i: u64) -> bool {
        *self.bucket_ptr(i).add(self.occupied_offset()) != 0
    }

    /// Read the stored hash for bucket `i`.
    #[inline(always)]
    unsafe fn stored_hash(&self, i: u64) -> u64 {
        let p = self.bucket_ptr(i).add(self.hash_offset());
        (p as *const u64).read_unaligned()
    }

    /// Pointer to the key bytes for bucket `i`.
    #[inline(always)]
    unsafe fn key_ptr(&self, i: u64) -> *mut u8 {
        self.bucket_ptr(i).add(self.key_offset())
    }

    /// Pointer to the value bytes for bucket `i`.
    #[inline(always)]
    unsafe fn val_ptr(&self, i: u64) -> *mut u8 {
        self.bucket_ptr(i).add(self.val_offset())
    }

    /// Allocate (zeroed) bucket array for `cap` entries.
    fn alloc_buckets(entry_size: usize, cap: u64) -> *mut u8 {
        if cap == 0 {
            return ptr::null_mut();
        }
        let total = cap as usize * entry_size;
        if total == 0 {
            return ptr::null_mut();
        }
        let layout = unsafe { Layout::from_size_align_unchecked(total, 8) };
        let ptr = unsafe { alloc_zeroed(layout) };
        if ptr.is_null() {
            std::process::abort();
        }
        ptr
    }

    /// Find the bucket index for `key_ptr` with `hash`.
    /// Returns `(index, found)`. If `found` is true the key is at `index`;
    /// otherwise `index` is the first empty slot.
    unsafe fn probe(&self, key_ptr: *const u8, hash: u64) -> (u64, bool) {
        if self.capacity == 0 {
            return (0, false);
        }
        let mask = self.capacity - 1;
        let mut i = hash & mask;
        loop {
            if !self.is_occupied(i) {
                return (i, false);
            }
            if self.stored_hash(i) == hash && self.keys_equal(i, key_ptr) {
                return (i, true);
            }
            i = (i + 1) & mask;
        }
    }

    /// Byte-for-byte comparison of key at bucket `i` with `key_ptr`.
    #[inline]
    unsafe fn keys_equal(&self, i: u64, key_ptr: *const u8) -> bool {
        let ks = self.key_size as usize;
        if ks == 0 {
            return true;
        }
        let a = std::slice::from_raw_parts(self.key_ptr(i), ks);
        let b = std::slice::from_raw_parts(key_ptr, ks);
        a == b
    }

    /// Write an entry into bucket `i`.
    unsafe fn write_entry(&mut self, i: u64, hash: u64, key_ptr: *const u8, val_ptr: *const u8) {
        let bp = self.bucket_ptr(i);
        // Write hash.
        (bp.add(self.hash_offset()) as *mut u64).write_unaligned(hash);
        // Write key.
        if self.key_size > 0 {
            copy_nonoverlapping(key_ptr, bp.add(self.key_offset()), self.key_size as usize);
        }
        // Write value.
        if self.val_size > 0 {
            copy_nonoverlapping(val_ptr, bp.add(self.val_offset()), self.val_size as usize);
        }
        // Mark occupied.
        *bp.add(self.occupied_offset()) = 1;
    }

    /// Check if resize is needed and perform it.
    fn maybe_resize(&mut self) {
        if self.capacity == 0 || self.count * LOAD_FACTOR_DEN >= self.capacity * LOAD_FACTOR_NUM {
            self.resize(if self.capacity == 0 {
                INITIAL_CAPACITY
            } else {
                self.capacity * 2
            });
        }
    }

    /// Resize to `new_cap` (must be power of two).
    fn resize(&mut self, new_cap: u64) {
        let old_buckets = self.buckets;
        let old_cap = self.capacity;
        let entry_sz = self.entry_size();

        self.buckets = Self::alloc_buckets(entry_sz, new_cap);
        self.capacity = new_cap;
        self.count = 0;

        if !old_buckets.is_null() && old_cap > 0 {
            for i in 0..old_cap {
                unsafe {
                    let bp = old_buckets.add(i as usize * entry_sz);
                    let occ_off = self.occupied_offset();
                    if *bp.add(occ_off) != 0 {
                        let hash = (bp.add(self.hash_offset()) as *const u64).read_unaligned();
                        let kp = bp.add(self.key_offset());
                        let vp = bp.add(self.val_offset());
                        let (idx, _) = self.probe(kp, hash);
                        self.write_entry(idx, hash, kp, vp);
                        self.count += 1;
                    }
                }
            }
            // Dealloc old buckets.
            let total = old_cap as usize * entry_sz;
            if total > 0 {
                unsafe {
                    let layout = Layout::from_size_align_unchecked(total, 8);
                    dealloc(old_buckets, layout);
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// FFI functions
// ---------------------------------------------------------------------------

/// Create a new empty map.
#[no_mangle]
pub extern "C" fn __adam_map_new(
    key_size: u64,
    val_size: u64,
    key_align: u64,
    val_align: u64,
) -> AdamMap {
    AdamMap {
        buckets: ptr::null_mut(),
        count: 0,
        capacity: 0,
        key_size,
        val_size,
        key_align,
        val_align,
    }
}

/// Return the number of entries.
#[no_mangle]
pub extern "C" fn __adam_map_len(map: *const AdamMap) -> u64 {
    unsafe { (*map).count }
}

/// Return `true` if the map has no entries.
#[no_mangle]
pub extern "C" fn __adam_map_is_empty(map: *const AdamMap) -> bool {
    unsafe { (*map).count == 0 }
}

/// Insert a key-value pair. Returns `true` if the key already existed
/// (and writes the old value to `old_val_ptr`).
#[no_mangle]
pub extern "C" fn __adam_map_insert(
    map: *mut AdamMap,
    key_ptr: *const u8,
    val_ptr: *const u8,
    hash: u64,
    old_val_ptr: *mut u8,
) -> bool {
    let m = unsafe { &mut *map };
    m.maybe_resize();

    let (idx, found) = unsafe { m.probe(key_ptr, hash) };

    if found {
        // Copy old value out, then overwrite.
        if m.val_size > 0 && !old_val_ptr.is_null() {
            unsafe {
                copy_nonoverlapping(m.val_ptr(idx), old_val_ptr, m.val_size as usize);
            }
        }
        // Overwrite value.
        if m.val_size > 0 {
            unsafe {
                copy_nonoverlapping(val_ptr, m.val_ptr(idx), m.val_size as usize);
            }
        }
        true
    } else {
        unsafe {
            m.write_entry(idx, hash, key_ptr, val_ptr);
        }
        m.count += 1;
        false
    }
}

/// Look up a key. Returns `true` if found, copying the value to `out_ptr`.
#[no_mangle]
pub extern "C" fn __adam_map_get(
    map: *const AdamMap,
    key_ptr: *const u8,
    hash: u64,
    out_ptr: *mut u8,
) -> bool {
    let m = unsafe { &*map };
    if m.capacity == 0 {
        return false;
    }
    let (idx, found) = unsafe { m.probe(key_ptr, hash) };
    if found && m.val_size > 0 && !out_ptr.is_null() {
        unsafe {
            copy_nonoverlapping(m.val_ptr(idx), out_ptr, m.val_size as usize);
        }
    }
    found
}

/// Remove a key. Returns `true` if found, copying the removed value to `out_ptr`.
///
/// Uses backward-shift deletion to maintain probe sequences.
#[no_mangle]
pub extern "C" fn __adam_map_remove(
    map: *mut AdamMap,
    key_ptr: *const u8,
    hash: u64,
    out_ptr: *mut u8,
) -> bool {
    let m = unsafe { &mut *map };
    if m.capacity == 0 {
        return false;
    }
    let (idx, found) = unsafe { m.probe(key_ptr, hash) };
    if !found {
        return false;
    }

    // Copy value out.
    if m.val_size > 0 && !out_ptr.is_null() {
        unsafe {
            copy_nonoverlapping(m.val_ptr(idx), out_ptr, m.val_size as usize);
        }
    }

    // Mark as empty.
    unsafe {
        *m.bucket_ptr(idx).add(m.occupied_offset()) = 0;
    }
    m.count -= 1;

    // Backward-shift deletion: rehash subsequent entries in the same cluster.
    {
        let mask = m.capacity - 1;
        let mut gap = idx;
        let mut j = (idx + 1) & mask;
        loop {
            unsafe {
                if !m.is_occupied(j) {
                    break;
                }
                let h = m.stored_hash(j);
                let ideal = h & mask;
                // Move entry at `j` to `gap` if `ideal` is NOT in the
                // half-open interval (gap, j] (cyclically). This is the
                // standard backward-shift deletion condition.
                let ideal_in_range = if gap < j {
                    ideal > gap && ideal <= j
                } else {
                    ideal > gap || ideal <= j
                };
                if !ideal_in_range {
                    // Move entry from j to gap.
                    let entry_sz = m.entry_size();
                    copy_nonoverlapping(m.bucket_ptr(j), m.bucket_ptr(gap), entry_sz);
                    *m.bucket_ptr(j).add(m.occupied_offset()) = 0;
                    gap = j;
                }
            }
            j = (j + 1) & mask;
        }
    }

    true
}

/// Check whether a key exists.
#[no_mangle]
pub extern "C" fn __adam_map_contains_key(
    map: *const AdamMap,
    key_ptr: *const u8,
    hash: u64,
) -> bool {
    let m = unsafe { &*map };
    if m.capacity == 0 {
        return false;
    }
    let (_, found) = unsafe { m.probe(key_ptr, hash) };
    found
}

/// Remove all entries. Does not deallocate the bucket array.
#[no_mangle]
pub extern "C" fn __adam_map_clear(map: *mut AdamMap) {
    let m = unsafe { &mut *map };
    if m.capacity > 0 && !m.buckets.is_null() {
        let total = m.capacity as usize * m.entry_size();
        unsafe {
            ptr::write_bytes(m.buckets, 0, total);
        }
    }
    m.count = 0;
}

/// Free the bucket array. The map must not be used after this.
#[no_mangle]
pub extern "C" fn __adam_map_drop(map: *mut AdamMap) {
    let m = unsafe { &mut *map };
    if !m.buckets.is_null() && m.capacity > 0 {
        let total = m.capacity as usize * m.entry_size();
        if total > 0 {
            unsafe {
                let layout = Layout::from_size_align_unchecked(total, 8);
                dealloc(m.buckets, layout);
            }
        }
    }
    m.buckets = ptr::null_mut();
    m.count = 0;
    m.capacity = 0;
}

/// Deep-copy the map.
#[no_mangle]
pub extern "C" fn __adam_map_clone(map: *const AdamMap) -> AdamMap {
    let m = unsafe { &*map };
    if m.capacity == 0 || m.buckets.is_null() {
        return AdamMap {
            buckets: ptr::null_mut(),
            count: 0,
            capacity: 0,
            key_size: m.key_size,
            val_size: m.val_size,
            key_align: m.key_align,
            val_align: m.val_align,
        };
    }
    let entry_sz = m.entry_size();
    let total = m.capacity as usize * entry_sz;
    let new_buckets = AdamMap::alloc_buckets(entry_sz, m.capacity);
    unsafe {
        copy_nonoverlapping(m.buckets, new_buckets, total);
    }
    AdamMap {
        buckets: new_buckets,
        count: m.count,
        capacity: m.capacity,
        key_size: m.key_size,
        val_size: m.val_size,
        key_align: m.key_align,
        val_align: m.val_align,
    }
}

// ===================================================================
// Tests
// ===================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // Helper: i32 key, i32 value map.
    fn new_i32_map() -> AdamMap {
        __adam_map_new(
            std::mem::size_of::<i32>() as u64,
            std::mem::size_of::<i32>() as u64,
            std::mem::align_of::<i32>() as u64,
            std::mem::align_of::<i32>() as u64,
        )
    }

    /// Trivial hash function for testing: just the value cast to u64.
    fn hash_i32(val: i32) -> u64 {
        val as u64
    }

    fn insert_i32(m: &mut AdamMap, key: i32, val: i32) -> bool {
        let mut old_val: i32 = 0;
        let existed = __adam_map_insert(
            m,
            key.to_ne_bytes().as_ptr(),
            val.to_ne_bytes().as_ptr(),
            hash_i32(key),
            &mut old_val as *mut i32 as *mut u8,
        );
        existed
    }

    fn get_i32(m: &AdamMap, key: i32) -> Option<i32> {
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

    fn remove_i32(m: &mut AdamMap, key: i32) -> Option<i32> {
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

    // ---------------------------------------------------------------
    // Basic construction
    // ---------------------------------------------------------------

    #[test]
    fn test_new_map_is_empty() {
        let m = new_i32_map();
        assert_eq!(__adam_map_len(&m), 0);
        assert!(__adam_map_is_empty(&m));
    }

    // ---------------------------------------------------------------
    // Insert / Get
    // ---------------------------------------------------------------

    #[test]
    fn test_insert_and_get() {
        let mut m = new_i32_map();
        assert!(!insert_i32(&mut m, 1, 100));
        assert_eq!(get_i32(&m, 1), Some(100));
        __adam_map_drop(&mut m);
    }

    #[test]
    fn test_insert_overwrite() {
        let mut m = new_i32_map();
        assert!(!insert_i32(&mut m, 1, 100));
        assert!(insert_i32(&mut m, 1, 200));
        assert_eq!(get_i32(&m, 1), Some(200));
        assert_eq!(__adam_map_len(&m), 1);
        __adam_map_drop(&mut m);
    }

    #[test]
    fn test_insert_multiple() {
        let mut m = new_i32_map();
        for i in 0..10 {
            insert_i32(&mut m, i, i * 10);
        }
        assert_eq!(__adam_map_len(&m), 10);
        for i in 0..10 {
            assert_eq!(get_i32(&m, i), Some(i * 10));
        }
        __adam_map_drop(&mut m);
    }

    #[test]
    fn test_get_missing_key() {
        let mut m = new_i32_map();
        insert_i32(&mut m, 1, 100);
        assert_eq!(get_i32(&m, 99), None);
        __adam_map_drop(&mut m);
    }

    #[test]
    fn test_get_from_empty() {
        let m = new_i32_map();
        assert_eq!(get_i32(&m, 1), None);
    }

    // ---------------------------------------------------------------
    // Remove
    // ---------------------------------------------------------------

    #[test]
    fn test_remove() {
        let mut m = new_i32_map();
        insert_i32(&mut m, 1, 100);
        insert_i32(&mut m, 2, 200);
        assert_eq!(remove_i32(&mut m, 1), Some(100));
        assert_eq!(__adam_map_len(&m), 1);
        assert_eq!(get_i32(&m, 1), None);
        assert_eq!(get_i32(&m, 2), Some(200));
        __adam_map_drop(&mut m);
    }

    #[test]
    fn test_remove_missing() {
        let mut m = new_i32_map();
        insert_i32(&mut m, 1, 100);
        assert_eq!(remove_i32(&mut m, 99), None);
        assert_eq!(__adam_map_len(&m), 1);
        __adam_map_drop(&mut m);
    }

    #[test]
    fn test_remove_from_empty() {
        let mut m = new_i32_map();
        assert_eq!(remove_i32(&mut m, 1), None);
    }

    #[test]
    fn test_remove_and_reinsert() {
        let mut m = new_i32_map();
        insert_i32(&mut m, 1, 100);
        remove_i32(&mut m, 1);
        assert_eq!(get_i32(&m, 1), None);
        insert_i32(&mut m, 1, 200);
        assert_eq!(get_i32(&m, 1), Some(200));
        __adam_map_drop(&mut m);
    }

    // ---------------------------------------------------------------
    // Contains key
    // ---------------------------------------------------------------

    #[test]
    fn test_contains_key() {
        let mut m = new_i32_map();
        insert_i32(&mut m, 5, 50);
        assert!(__adam_map_contains_key(
            &m,
            5_i32.to_ne_bytes().as_ptr(),
            hash_i32(5)
        ));
        assert!(!__adam_map_contains_key(
            &m,
            99_i32.to_ne_bytes().as_ptr(),
            hash_i32(99)
        ));
        __adam_map_drop(&mut m);
    }

    // ---------------------------------------------------------------
    // Clear
    // ---------------------------------------------------------------

    #[test]
    fn test_clear() {
        let mut m = new_i32_map();
        for i in 0..10 {
            insert_i32(&mut m, i, i);
        }
        __adam_map_clear(&mut m);
        assert_eq!(__adam_map_len(&m), 0);
        assert!(__adam_map_is_empty(&m));
        assert_eq!(get_i32(&m, 0), None);
        __adam_map_drop(&mut m);
    }

    // ---------------------------------------------------------------
    // Clone
    // ---------------------------------------------------------------

    #[test]
    fn test_clone() {
        let mut m = new_i32_map();
        insert_i32(&mut m, 1, 100);
        insert_i32(&mut m, 2, 200);
        let mut c = __adam_map_clone(&m);
        assert_eq!(__adam_map_len(&c), 2);
        assert_eq!(get_i32(&c, 1), Some(100));
        assert_eq!(get_i32(&c, 2), Some(200));
        // Mutating original should not affect clone.
        insert_i32(&mut m, 1, 999);
        assert_eq!(get_i32(&c, 1), Some(100));
        __adam_map_drop(&mut m);
        __adam_map_drop(&mut c);
    }

    #[test]
    fn test_clone_empty() {
        let m = new_i32_map();
        let mut c = __adam_map_clone(&m);
        assert_eq!(__adam_map_len(&c), 0);
        __adam_map_drop(&mut c);
    }

    // ---------------------------------------------------------------
    // Hash collision handling
    // ---------------------------------------------------------------

    #[test]
    fn test_hash_collisions() {
        // Use the same hash for different keys to force linear probing.
        let mut m = new_i32_map();
        let collision_hash: u64 = 42;
        for i in 0i32..5 {
            let key: i32 = i;
            let val: i32 = i * 10;
            let mut old: i32 = 0;
            __adam_map_insert(
                &mut m,
                key.to_ne_bytes().as_ptr(),
                val.to_ne_bytes().as_ptr(),
                collision_hash,
                &mut old as *mut i32 as *mut u8,
            );
        }
        assert_eq!(__adam_map_len(&m), 5);
        // All keys should be retrievable.
        for i in 0i32..5 {
            let mut out: i32 = 0;
            let found = __adam_map_get(
                &m,
                i.to_ne_bytes().as_ptr(),
                collision_hash,
                &mut out as *mut i32 as *mut u8,
            );
            assert!(found, "key {} should exist", i);
            assert_eq!(out, i * 10);
        }
        __adam_map_drop(&mut m);
    }

    #[test]
    fn test_collision_remove() {
        // Insert keys with same hash, remove one, check others still accessible.
        let mut m = new_i32_map();
        let collision_hash: u64 = 7;
        for i in 0i32..4 {
            let mut old: i32 = 0;
            __adam_map_insert(
                &mut m,
                i.to_ne_bytes().as_ptr(),
                (i * 100).to_ne_bytes().as_ptr(),
                collision_hash,
                &mut old as *mut i32 as *mut u8,
            );
        }
        // Remove key 1.
        let mut out: i32 = 0;
        let found = __adam_map_remove(
            &mut m,
            1_i32.to_ne_bytes().as_ptr(),
            collision_hash,
            &mut out as *mut i32 as *mut u8,
        );
        assert!(found);
        assert_eq!(out, 100);
        // Others should still be found.
        for i in [0i32, 2, 3] {
            let mut v: i32 = 0;
            let f = __adam_map_get(
                &m,
                i.to_ne_bytes().as_ptr(),
                collision_hash,
                &mut v as *mut i32 as *mut u8,
            );
            assert!(f, "key {} should exist after removing 1", i);
            assert_eq!(v, i * 100);
        }
        __adam_map_drop(&mut m);
    }

    // ---------------------------------------------------------------
    // Resize verification
    // ---------------------------------------------------------------

    #[test]
    fn test_resize() {
        let mut m = new_i32_map();
        // Insert enough entries to trigger multiple resizes.
        // Initial cap is 16, load factor 0.75 => resize at 12.
        for i in 0..100 {
            insert_i32(&mut m, i, i * 2);
        }
        assert_eq!(__adam_map_len(&m), 100);
        // All entries should still be accessible.
        for i in 0..100 {
            assert_eq!(
                get_i32(&m, i),
                Some(i * 2),
                "key {} missing after resize",
                i
            );
        }
        __adam_map_drop(&mut m);
    }

    #[test]
    fn test_resize_preserves_data() {
        let mut m = new_i32_map();
        // Fill just past the first resize threshold.
        for i in 0..13 {
            insert_i32(&mut m, i, i + 1000);
        }
        for i in 0..13 {
            assert_eq!(get_i32(&m, i), Some(i + 1000));
        }
        __adam_map_drop(&mut m);
    }

    // ---------------------------------------------------------------
    // Drop
    // ---------------------------------------------------------------

    #[test]
    fn test_drop() {
        let mut m = new_i32_map();
        for i in 0..50 {
            insert_i32(&mut m, i, i);
        }
        __adam_map_drop(&mut m);
        assert!(m.buckets.is_null());
        assert_eq!(m.count, 0);
        assert_eq!(m.capacity, 0);
    }

    // ---------------------------------------------------------------
    // Edge: insert after clear
    // ---------------------------------------------------------------

    #[test]
    fn test_insert_after_clear() {
        let mut m = new_i32_map();
        for i in 0..10 {
            insert_i32(&mut m, i, i);
        }
        __adam_map_clear(&mut m);
        insert_i32(&mut m, 42, 420);
        assert_eq!(__adam_map_len(&m), 1);
        assert_eq!(get_i32(&m, 42), Some(420));
        __adam_map_drop(&mut m);
    }

    // ---------------------------------------------------------------
    // Adversarial / exhaustive tests
    // ---------------------------------------------------------------

    #[test]
    fn test_insert_1000_entries_all_retrievable() {
        let mut m = new_i32_map();
        for i in 0..1_000 {
            assert!(!insert_i32(&mut m, i, i * 3));
        }
        assert_eq!(__adam_map_len(&m), 1_000);
        for i in 0..1_000 {
            assert_eq!(
                get_i32(&m, i),
                Some(i * 3),
                "key {} should map to {}",
                i,
                i * 3
            );
        }
        __adam_map_drop(&mut m);
    }

    #[test]
    fn test_insert_and_remove_same_key_repeatedly() {
        let mut m = new_i32_map();
        for round in 0..100 {
            assert!(!insert_i32(&mut m, 42, round));
            assert_eq!(get_i32(&m, 42), Some(round));
            assert_eq!(remove_i32(&mut m, 42), Some(round));
            assert_eq!(get_i32(&m, 42), None);
            assert_eq!(__adam_map_len(&m), 0);
        }
        __adam_map_drop(&mut m);
    }

    #[test]
    fn test_all_entries_same_hash_worst_case_collision() {
        // Force all keys to hash to the same bucket.
        let mut m = new_i32_map();
        let collision_hash: u64 = 0;
        let n = 50;
        for i in 0..n {
            let key: i32 = i;
            let val: i32 = i * 7;
            let mut old: i32 = 0;
            __adam_map_insert(
                &mut m,
                key.to_ne_bytes().as_ptr(),
                val.to_ne_bytes().as_ptr(),
                collision_hash,
                &mut old as *mut i32 as *mut u8,
            );
        }
        assert_eq!(__adam_map_len(&m), n as u64);
        for i in 0..n {
            let mut out: i32 = 0;
            let found = __adam_map_get(
                &m,
                i.to_ne_bytes().as_ptr(),
                collision_hash,
                &mut out as *mut i32 as *mut u8,
            );
            assert!(found, "key {} should be found with same-hash collision", i);
            assert_eq!(out, i * 7);
        }
        __adam_map_drop(&mut m);
    }

    #[test]
    fn test_remove_all_entries_one_by_one_verify_empty() {
        let mut m = new_i32_map();
        let n = 200;
        for i in 0..n {
            insert_i32(&mut m, i, i);
        }
        for i in 0..n {
            assert_eq!(
                remove_i32(&mut m, i),
                Some(i),
                "remove key {} should return {}",
                i,
                i
            );
        }
        assert_eq!(__adam_map_len(&m), 0);
        assert!(__adam_map_is_empty(&m));
        // Verify no ghosts.
        for i in 0..n {
            assert_eq!(get_i32(&m, i), None);
        }
        __adam_map_drop(&mut m);
    }

    #[test]
    fn test_insert_after_full_clear_fresh_state() {
        let mut m = new_i32_map();
        for i in 0..50 {
            insert_i32(&mut m, i, i);
        }
        __adam_map_clear(&mut m);
        assert_eq!(__adam_map_len(&m), 0);
        // Insert completely new keys.
        for i in 100..150 {
            assert!(!insert_i32(&mut m, i, i * 2));
        }
        assert_eq!(__adam_map_len(&m), 50);
        // Old keys should not be present.
        for i in 0..50 {
            assert_eq!(get_i32(&m, i), None);
        }
        // New keys should be present.
        for i in 100..150 {
            assert_eq!(get_i32(&m, i), Some(i * 2));
        }
        __adam_map_drop(&mut m);
    }

    #[test]
    fn test_overwrite_existing_key_returns_old_value() {
        let mut m = new_i32_map();
        insert_i32(&mut m, 5, 500);
        // Overwrite and capture old value.
        let mut old_val: i32 = 0;
        let existed = __adam_map_insert(
            &mut m,
            5_i32.to_ne_bytes().as_ptr(),
            999_i32.to_ne_bytes().as_ptr(),
            hash_i32(5),
            &mut old_val as *mut i32 as *mut u8,
        );
        assert!(existed);
        assert_eq!(old_val, 500);
        assert_eq!(get_i32(&m, 5), Some(999));
        assert_eq!(__adam_map_len(&m), 1);
        __adam_map_drop(&mut m);
    }

    #[test]
    fn test_many_removes_dont_break_probing_tombstone_handling() {
        // Insert many, remove many, insert more. Probing should still work
        // correctly after backward-shift deletion fills gaps.
        let mut m = new_i32_map();
        for i in 0..100 {
            insert_i32(&mut m, i, i);
        }
        // Remove every other key.
        for i in (0..100).step_by(2) {
            remove_i32(&mut m, i);
        }
        assert_eq!(__adam_map_len(&m), 50);
        // Remaining keys should all be accessible.
        for i in (1..100).step_by(2) {
            assert_eq!(
                get_i32(&m, i),
                Some(i),
                "odd key {} should still be present",
                i
            );
        }
        // Insert new keys that may land on formerly occupied slots.
        for i in 200..300 {
            insert_i32(&mut m, i, i * 10);
        }
        assert_eq!(__adam_map_len(&m), 150);
        // All keys should be retrievable.
        for i in (1..100).step_by(2) {
            assert_eq!(get_i32(&m, i), Some(i));
        }
        for i in 200..300 {
            assert_eq!(get_i32(&m, i), Some(i * 10));
        }
        __adam_map_drop(&mut m);
    }

    #[test]
    fn test_clone_populated_map_modify_original() {
        let mut m = new_i32_map();
        for i in 0..50 {
            insert_i32(&mut m, i, i * 100);
        }
        let mut c = __adam_map_clone(&m);
        // Modify every value in original.
        for i in 0..50 {
            insert_i32(&mut m, i, -(i));
        }
        // Clone should be unaffected.
        for i in 0..50 {
            assert_eq!(
                get_i32(&c, i),
                Some(i * 100),
                "cloned key {} should still have value {}",
                i,
                i * 100
            );
        }
        __adam_map_drop(&mut m);
        __adam_map_drop(&mut c);
    }

    #[test]
    fn test_contains_key_for_every_key_after_mass_insert() {
        let mut m = new_i32_map();
        for i in 0..500 {
            insert_i32(&mut m, i, i);
        }
        for i in 0i32..500 {
            assert!(
                __adam_map_contains_key(&m, i.to_ne_bytes().as_ptr(), hash_i32(i)),
                "contains_key should return true for key {}",
                i
            );
        }
        // Keys not inserted should not be found.
        for i in 500i32..510 {
            assert!(
                !__adam_map_contains_key(&m, i.to_ne_bytes().as_ptr(), hash_i32(i)),
                "contains_key should return false for key {}",
                i
            );
        }
        __adam_map_drop(&mut m);
    }

    #[test]
    fn test_interleaved_insert_remove_get() {
        let mut m = new_i32_map();
        // Insert 0..20
        for i in 0..20 {
            insert_i32(&mut m, i, i);
        }
        // Remove even keys.
        for i in (0..20).step_by(2) {
            remove_i32(&mut m, i);
        }
        // Insert 20..30
        for i in 20..30 {
            insert_i32(&mut m, i, i);
        }
        // Get all odd keys 0..20 and 20..30
        for i in (1..20).step_by(2) {
            assert_eq!(get_i32(&m, i), Some(i));
        }
        for i in 20..30 {
            assert_eq!(get_i32(&m, i), Some(i));
        }
        // Even keys should be gone.
        for i in (0..20).step_by(2) {
            assert_eq!(get_i32(&m, i), None);
        }
        assert_eq!(__adam_map_len(&m), 20); // 10 odd + 10 new
        __adam_map_drop(&mut m);
    }

    #[test]
    fn test_zero_sized_keys() {
        // Edge case: key_size=0. All keys are "equal" because keys_equal
        // with ks==0 returns true. So effectively only one entry can exist.
        let mut m = __adam_map_new(0, 4, 1, 4); // key_size=0, val_size=4
        let val1: i32 = 100;
        let val2: i32 = 200;
        let hash: u64 = 0;
        let mut old: i32 = 0;

        // Insert first "key" (zero-sized).
        let existed = __adam_map_insert(
            &mut m,
            std::ptr::null(),
            val1.to_ne_bytes().as_ptr(),
            hash,
            &mut old as *mut i32 as *mut u8,
        );
        assert!(!existed);
        assert_eq!(__adam_map_len(&m), 1);

        // Insert again with same hash should overwrite (keys_equal returns true for ks=0).
        let existed = __adam_map_insert(
            &mut m,
            std::ptr::null(),
            val2.to_ne_bytes().as_ptr(),
            hash,
            &mut old as *mut i32 as *mut u8,
        );
        assert!(existed);
        assert_eq!(old, 100);
        assert_eq!(__adam_map_len(&m), 1);

        // Get should return the overwritten value.
        let mut out: i32 = 0;
        let found = __adam_map_get(&m, std::ptr::null(), hash, &mut out as *mut i32 as *mut u8);
        assert!(found);
        assert_eq!(out, 200);

        __adam_map_drop(&mut m);
    }

    #[test]
    fn test_single_element_map_operations() {
        let mut m = new_i32_map();
        // Insert.
        assert!(!insert_i32(&mut m, 7, 77));
        assert_eq!(__adam_map_len(&m), 1);
        // Get.
        assert_eq!(get_i32(&m, 7), Some(77));
        // Contains.
        assert!(__adam_map_contains_key(
            &m,
            7_i32.to_ne_bytes().as_ptr(),
            hash_i32(7)
        ));
        // Overwrite.
        assert!(insert_i32(&mut m, 7, 88));
        assert_eq!(get_i32(&m, 7), Some(88));
        assert_eq!(__adam_map_len(&m), 1);
        // Remove.
        assert_eq!(remove_i32(&mut m, 7), Some(88));
        assert!(__adam_map_is_empty(&m));
        // Remove again should fail.
        assert_eq!(remove_i32(&mut m, 7), None);
        __adam_map_drop(&mut m);
    }

    #[test]
    fn test_resize_triggers_at_correct_load_factor() {
        // Initial capacity is 16. maybe_resize is called before each insert
        // with the current count. Resize triggers when count*4 >= capacity*3.
        // With cap=16: 12*4=48 >= 16*3=48, so resize triggers when count=12,
        // i.e., before inserting the 13th element.
        let mut m = new_i32_map();
        // Insert 12 entries - no resize yet (count is checked before insert).
        for i in 0..12 {
            insert_i32(&mut m, i, i);
        }
        assert_eq!(m.capacity, 16, "capacity should be 16 after 12 inserts");

        // Insert the 13th entry. Before this insert, count=12, and
        // 12*4=48 >= 16*3=48, so maybe_resize triggers.
        insert_i32(&mut m, 12, 12);
        assert_eq!(
            m.capacity, 32,
            "capacity should be 32 after 13 inserts (75% load factor trigger)"
        );

        // All entries should still be accessible.
        for i in 0..13 {
            assert_eq!(get_i32(&m, i), Some(i));
        }
        __adam_map_drop(&mut m);
    }

    #[test]
    fn test_insert_same_key_different_values_100_times() {
        let mut m = new_i32_map();
        for i in 0..100 {
            let is_overwrite = insert_i32(&mut m, 42, i);
            if i == 0 {
                assert!(!is_overwrite, "first insert should not be overwrite");
            } else {
                assert!(is_overwrite, "insert {} should be overwrite", i);
            }
        }
        // Only one entry.
        assert_eq!(__adam_map_len(&m), 1);
        // Value should be the last one written.
        assert_eq!(get_i32(&m, 42), Some(99));
        __adam_map_drop(&mut m);
    }

    #[test]
    fn test_get_and_remove_correct_values_for_colliding_keys() {
        // Insert several keys that all hash to the same bucket, then
        // verify get and remove return the correct per-key values.
        let mut m = new_i32_map();
        let collision_hash: u64 = 3;
        let n = 20;
        for i in 0..n {
            let key: i32 = i;
            let val: i32 = i * 11;
            let mut old: i32 = 0;
            __adam_map_insert(
                &mut m,
                key.to_ne_bytes().as_ptr(),
                val.to_ne_bytes().as_ptr(),
                collision_hash,
                &mut old as *mut i32 as *mut u8,
            );
        }
        // Get each key.
        for i in 0..n {
            let mut out: i32 = 0;
            let found = __adam_map_get(
                &m,
                i.to_ne_bytes().as_ptr(),
                collision_hash,
                &mut out as *mut i32 as *mut u8,
            );
            assert!(found);
            assert_eq!(out, i * 11);
        }
        // Remove from the middle of the collision chain.
        for i in (0..n).step_by(3) {
            let mut out: i32 = 0;
            let found = __adam_map_remove(
                &mut m,
                i.to_ne_bytes().as_ptr(),
                collision_hash,
                &mut out as *mut i32 as *mut u8,
            );
            assert!(found);
            assert_eq!(out, i * 11);
        }
        // Remaining keys should still be correct.
        for i in 0..n {
            let mut out: i32 = 0;
            let found = __adam_map_get(
                &m,
                i.to_ne_bytes().as_ptr(),
                collision_hash,
                &mut out as *mut i32 as *mut u8,
            );
            if i % 3 == 0 {
                assert!(!found, "key {} should have been removed", i);
            } else {
                assert!(found, "key {} should still exist", i);
                assert_eq!(out, i * 11);
            }
        }
        __adam_map_drop(&mut m);
    }

    #[test]
    fn test_remove_in_reverse_insertion_order() {
        let mut m = new_i32_map();
        for i in 0..100 {
            insert_i32(&mut m, i, i);
        }
        // Remove in reverse order.
        for i in (0..100).rev() {
            assert_eq!(remove_i32(&mut m, i), Some(i));
        }
        assert!(__adam_map_is_empty(&m));
        __adam_map_drop(&mut m);
    }
}
