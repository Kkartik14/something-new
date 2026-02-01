//! AdamVec — type-erased growable array for the Adam runtime.
//!
//! The compiler emits element size and alignment at each call site.
//! All data is moved via opaque `*const u8` / `*mut u8` pointers.

use std::alloc::{alloc, dealloc, realloc, Layout};
use std::ptr::{self, copy, copy_nonoverlapping};

/// A type-erased growable array.
///
/// Matches the LLVM struct `{ i8*, i64, i64, i64, i64 }`.
#[repr(C)]
pub struct AdamVec {
    ptr: *mut u8,
    len: u64,
    cap: u64,
    elem_size: u64,
    elem_align: u64,
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

impl AdamVec {
    /// Byte offset to the element at `index`.
    #[inline(always)]
    fn offset(&self, index: u64) -> usize {
        (index as usize) * (self.elem_size as usize)
    }

    /// Layout for a buffer of `n` elements.
    #[inline(always)]
    fn buf_layout(&self, n: u64) -> Layout {
        if n == 0 || self.elem_size == 0 {
            // ZST or empty — use alignment 1, size 0 to keep Layout valid.
            unsafe { Layout::from_size_align_unchecked(0, 1) }
        } else {
            let size = (n as usize) * (self.elem_size as usize);
            let align = std::cmp::max(self.elem_align as usize, 1);
            unsafe { Layout::from_size_align_unchecked(size, align) }
        }
    }

    /// Ensure capacity for at least `required` total elements. Uses 2x growth.
    fn grow_to(&mut self, required: u64) {
        if required <= self.cap {
            return;
        }
        let new_cap = if self.cap == 0 {
            std::cmp::max(required, 4)
        } else {
            let doubled = self.cap * 2;
            std::cmp::max(doubled, required)
        };

        if self.elem_size == 0 {
            // ZST — no actual allocation needed.
            self.cap = new_cap;
            return;
        }

        let new_layout = self.buf_layout(new_cap);
        let new_ptr = if self.ptr.is_null() || self.cap == 0 {
            unsafe { alloc(new_layout) }
        } else {
            let old_layout = self.buf_layout(self.cap);
            unsafe { realloc(self.ptr, old_layout, new_layout.size()) }
        };
        if new_ptr.is_null() {
            std::process::abort();
        }
        self.ptr = new_ptr;
        self.cap = new_cap;
    }
}

// ---------------------------------------------------------------------------
// FFI functions
// ---------------------------------------------------------------------------

/// Create a new empty vec.
#[no_mangle]
pub extern "C" fn __adam_vec_new(elem_size: u64, elem_align: u64) -> AdamVec {
    AdamVec {
        ptr: ptr::null_mut(),
        len: 0,
        cap: 0,
        elem_size,
        elem_align,
    }
}

/// Create a new vec with pre-allocated capacity.
#[no_mangle]
pub extern "C" fn __adam_vec_with_capacity(cap: u64, elem_size: u64, elem_align: u64) -> AdamVec {
    let mut v = __adam_vec_new(elem_size, elem_align);
    if cap > 0 {
        v.grow_to(cap);
    }
    v
}

/// Return the number of elements.
#[no_mangle]
pub extern "C" fn __adam_vec_len(vec: *const AdamVec) -> u64 {
    unsafe { (*vec).len }
}

/// Return the buffer capacity (in elements).
#[no_mangle]
pub extern "C" fn __adam_vec_capacity(vec: *const AdamVec) -> u64 {
    unsafe { (*vec).cap }
}

/// Return `true` if the vec contains no elements.
#[no_mangle]
pub extern "C" fn __adam_vec_is_empty(vec: *const AdamVec) -> bool {
    unsafe { (*vec).len == 0 }
}

/// Push an element onto the end of the vec.
///
/// Copies `elem_size` bytes from `val_ptr` into the buffer.
/// Grows (2x) if the buffer is full.
#[no_mangle]
pub extern "C" fn __adam_vec_push(vec: *mut AdamVec, val_ptr: *const u8) {
    let v = unsafe { &mut *vec };
    v.grow_to(v.len + 1);
    if v.elem_size > 0 {
        unsafe {
            let dst = v.ptr.add(v.offset(v.len));
            copy_nonoverlapping(val_ptr, dst, v.elem_size as usize);
        }
    }
    v.len += 1;
}

/// Pop the last element. Copies it to `out_ptr`. Returns `false` if empty.
#[no_mangle]
pub extern "C" fn __adam_vec_pop(vec: *mut AdamVec, out_ptr: *mut u8) -> bool {
    let v = unsafe { &mut *vec };
    if v.len == 0 {
        return false;
    }
    v.len -= 1;
    if v.elem_size > 0 {
        unsafe {
            let src = v.ptr.add(v.offset(v.len));
            copy_nonoverlapping(src, out_ptr, v.elem_size as usize);
        }
    }
    true
}

/// Get element at `idx`. Copies it to `out_ptr`. Returns `false` if out of bounds.
#[no_mangle]
pub extern "C" fn __adam_vec_get(vec: *const AdamVec, idx: u64, out_ptr: *mut u8) -> bool {
    let v = unsafe { &*vec };
    if idx >= v.len {
        return false;
    }
    if v.elem_size > 0 {
        unsafe {
            let src = v.ptr.add(v.offset(idx));
            copy_nonoverlapping(src, out_ptr, v.elem_size as usize);
        }
    }
    true
}

/// Overwrite element at `idx` with `elem_size` bytes from `val_ptr`.
///
/// # Safety
/// Caller must ensure `idx < len`.
#[no_mangle]
pub extern "C" fn __adam_vec_set(vec: *mut AdamVec, idx: u64, val_ptr: *const u8) {
    let v = unsafe { &mut *vec };
    if idx >= v.len {
        return;
    }
    if v.elem_size > 0 {
        unsafe {
            let dst = v.ptr.add(v.offset(idx));
            copy_nonoverlapping(val_ptr, dst, v.elem_size as usize);
        }
    }
}

/// Insert an element at `idx`, shifting subsequent elements right.
///
/// If `idx > len` this is treated as a push (appended at end).
#[no_mangle]
pub extern "C" fn __adam_vec_insert(vec: *mut AdamVec, idx: u64, val_ptr: *const u8) {
    let v = unsafe { &mut *vec };
    let idx = std::cmp::min(idx, v.len);
    v.grow_to(v.len + 1);
    if v.elem_size > 0 {
        unsafe {
            let base = v.ptr.add(v.offset(idx));
            let count = (v.len - idx) as usize * v.elem_size as usize;
            if count > 0 {
                copy(base, base.add(v.elem_size as usize), count);
            }
            copy_nonoverlapping(val_ptr, base, v.elem_size as usize);
        }
    }
    v.len += 1;
}

/// Remove element at `idx`, shift subsequent elements left, copy removed to `out_ptr`.
///
/// # Safety
/// Caller must ensure `idx < len`.
#[no_mangle]
pub extern "C" fn __adam_vec_remove(vec: *mut AdamVec, idx: u64, out_ptr: *mut u8) {
    let v = unsafe { &mut *vec };
    if idx >= v.len {
        return;
    }
    if v.elem_size > 0 {
        unsafe {
            let base = v.ptr.add(v.offset(idx));
            // Copy removed element out.
            copy_nonoverlapping(base, out_ptr, v.elem_size as usize);
            // Shift left.
            let tail = (v.len - idx - 1) as usize * v.elem_size as usize;
            if tail > 0 {
                copy(base.add(v.elem_size as usize), base, tail);
            }
        }
    }
    v.len -= 1;
}

/// Copy the first element to `out_ptr`. Returns `false` if empty.
#[no_mangle]
pub extern "C" fn __adam_vec_first(vec: *const AdamVec, out_ptr: *mut u8) -> bool {
    __adam_vec_get(vec, 0, out_ptr)
}

/// Copy the last element to `out_ptr`. Returns `false` if empty.
#[no_mangle]
pub extern "C" fn __adam_vec_last(vec: *const AdamVec, out_ptr: *mut u8) -> bool {
    let v = unsafe { &*vec };
    if v.len == 0 {
        return false;
    }
    __adam_vec_get(vec, v.len - 1, out_ptr)
}

/// Returns `true` if the vec contains an element equal (byte-for-byte) to `val_ptr`.
#[no_mangle]
pub extern "C" fn __adam_vec_contains(vec: *const AdamVec, val_ptr: *const u8) -> bool {
    let v = unsafe { &*vec };
    let esz = v.elem_size as usize;
    for i in 0..v.len {
        let elem = unsafe { v.ptr.add(v.offset(i)) };
        let equal = if esz == 0 {
            true
        } else {
            unsafe {
                let a = std::slice::from_raw_parts(elem, esz);
                let b = std::slice::from_raw_parts(val_ptr, esz);
                a == b
            }
        };
        if equal {
            return true;
        }
    }
    false
}

/// Reverse the vec in-place.
#[no_mangle]
pub extern "C" fn __adam_vec_reverse(vec: *mut AdamVec) {
    let v = unsafe { &mut *vec };
    if v.len <= 1 || v.elem_size == 0 {
        return;
    }
    let esz = v.elem_size as usize;
    let mut lo: u64 = 0;
    let mut hi: u64 = v.len - 1;
    // Stack buffer for small elements, heap for large.
    let mut stack_buf = [0u8; 128];
    let heap_buf: Vec<u8>;
    let tmp: *mut u8 = if esz <= 128 {
        stack_buf.as_mut_ptr()
    } else {
        heap_buf = vec![0u8; esz];
        // Keep heap_buf alive until end of function via forget-trick.
        heap_buf.as_ptr() as *mut u8
    };
    while lo < hi {
        unsafe {
            let a = v.ptr.add(v.offset(lo));
            let b = v.ptr.add(v.offset(hi));
            copy_nonoverlapping(a, tmp, esz);
            copy_nonoverlapping(b, a, esz);
            copy_nonoverlapping(tmp, b, esz);
        }
        lo += 1;
        hi -= 1;
    }
}

/// Clear the vec (set len to 0). Does not deallocate.
#[no_mangle]
pub extern "C" fn __adam_vec_clear(vec: *mut AdamVec) {
    unsafe {
        (*vec).len = 0;
    }
}

/// Append all elements from `src` to `dst`.
#[no_mangle]
pub extern "C" fn __adam_vec_extend(dst: *mut AdamVec, src: *const AdamVec) {
    let s = unsafe { &*src };
    let d = unsafe { &mut *dst };
    if s.len == 0 {
        return;
    }
    d.grow_to(d.len + s.len);
    if d.elem_size > 0 {
        unsafe {
            let dst_end = d.ptr.add(d.offset(d.len));
            let src_start = s.ptr;
            let bytes = s.len as usize * s.elem_size as usize;
            copy_nonoverlapping(src_start, dst_end, bytes);
        }
    }
    d.len += s.len;
}

/// Sort the vec in-place using the provided comparison function.
///
/// `cmp_fn(a, b)` must return negative if a < b, 0 if equal, positive if a > b.
#[no_mangle]
pub extern "C" fn __adam_vec_sort(
    vec: *mut AdamVec,
    cmp_fn: extern "C" fn(*const u8, *const u8) -> i32,
) {
    let v = unsafe { &mut *vec };
    if v.len <= 1 {
        return;
    }
    let esz = v.elem_size as usize;
    if esz == 0 {
        return;
    }
    // Build a Vec<usize> of indices, sort the indices, then permute in-place.
    // This avoids writing a full generic sort on raw memory.
    // Instead we use a simple approach: copy all elements into a Vec<Vec<u8>>,
    // sort, then copy back.
    let n = v.len as usize;
    let mut elems: Vec<Vec<u8>> = Vec::with_capacity(n);
    for i in 0..n {
        let mut buf = vec![0u8; esz];
        unsafe {
            copy_nonoverlapping(v.ptr.add(i * esz), buf.as_mut_ptr(), esz);
        }
        elems.push(buf);
    }
    elems.sort_by(|a, b| {
        let r = cmp_fn(a.as_ptr(), b.as_ptr());
        if r < 0 {
            std::cmp::Ordering::Less
        } else if r > 0 {
            std::cmp::Ordering::Greater
        } else {
            std::cmp::Ordering::Equal
        }
    });
    // Copy sorted elements back.
    for (i, elem) in elems.iter().enumerate() {
        unsafe {
            copy_nonoverlapping(elem.as_ptr(), v.ptr.add(i * esz), esz);
        }
    }
}

/// Free the buffer. The vec must not be used after this call.
#[no_mangle]
pub extern "C" fn __adam_vec_drop(vec: *mut AdamVec) {
    let v = unsafe { &mut *vec };
    if !v.ptr.is_null() && v.cap > 0 && v.elem_size > 0 {
        let layout = v.buf_layout(v.cap);
        unsafe {
            dealloc(v.ptr, layout);
        }
    }
    v.ptr = ptr::null_mut();
    v.len = 0;
    v.cap = 0;
}

/// Deep-copy the vec.
#[no_mangle]
pub extern "C" fn __adam_vec_clone(vec: *const AdamVec) -> AdamVec {
    let v = unsafe { &*vec };
    if v.len == 0 || v.elem_size == 0 {
        return AdamVec {
            ptr: ptr::null_mut(),
            len: v.len,
            cap: 0,
            elem_size: v.elem_size,
            elem_align: v.elem_align,
        };
    }
    let new_cap = v.len; // Clone uses tight capacity.
    let layout = v.buf_layout(new_cap);
    let new_ptr = unsafe { alloc(layout) };
    if new_ptr.is_null() {
        std::process::abort();
    }
    let bytes = v.len as usize * v.elem_size as usize;
    unsafe {
        copy_nonoverlapping(v.ptr, new_ptr, bytes);
    }
    AdamVec {
        ptr: new_ptr,
        len: v.len,
        cap: new_cap,
        elem_size: v.elem_size,
        elem_align: v.elem_align,
    }
}

/// Get a raw pointer to the element at `idx`.
///
/// Returns null if `idx >= len`.
#[no_mangle]
pub extern "C" fn __adam_vec_get_ptr(vec: *const AdamVec, idx: u64) -> *const u8 {
    let v = unsafe { &*vec };
    if idx >= v.len {
        return ptr::null();
    }
    unsafe { v.ptr.add(v.offset(idx)) }
}

// ===================================================================
// Tests
// ===================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // Helper: create a vec of i32-sized elements.
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

    fn pop_i32(v: &mut AdamVec) -> Option<i32> {
        let mut buf = [0u8; 4];
        if __adam_vec_pop(v, buf.as_mut_ptr()) {
            Some(i32::from_ne_bytes(buf))
        } else {
            None
        }
    }

    fn get_i32(v: &AdamVec, idx: u64) -> Option<i32> {
        let mut buf = [0u8; 4];
        if __adam_vec_get(v, idx, buf.as_mut_ptr()) {
            Some(i32::from_ne_bytes(buf))
        } else {
            None
        }
    }

    // ---------------------------------------------------------------
    // Basic construction
    // ---------------------------------------------------------------

    #[test]
    fn test_new_vec_is_empty() {
        let v = new_i32_vec();
        assert_eq!(__adam_vec_len(&v), 0);
        assert_eq!(__adam_vec_capacity(&v), 0);
        assert!(__adam_vec_is_empty(&v));
    }

    #[test]
    fn test_with_capacity() {
        let mut v = __adam_vec_with_capacity(
            16,
            std::mem::size_of::<i32>() as u64,
            std::mem::align_of::<i32>() as u64,
        );
        assert_eq!(__adam_vec_len(&v), 0);
        assert!(
            __adam_vec_capacity(&v) >= 16,
            "capacity should be at least 16"
        );
        assert!(__adam_vec_is_empty(&v));
        __adam_vec_drop(&mut v);
    }

    // ---------------------------------------------------------------
    // Push / Pop
    // ---------------------------------------------------------------

    #[test]
    fn test_push_single() {
        let mut v = new_i32_vec();
        push_i32(&mut v, 42);
        assert_eq!(__adam_vec_len(&v), 1);
        assert!(!__adam_vec_is_empty(&v));
        assert_eq!(get_i32(&v, 0), Some(42));
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_push_multiple() {
        let mut v = new_i32_vec();
        for i in 0..10 {
            push_i32(&mut v, i);
        }
        assert_eq!(__adam_vec_len(&v), 10);
        for i in 0..10 {
            assert_eq!(get_i32(&v, i as u64), Some(i));
        }
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_pop_single() {
        let mut v = new_i32_vec();
        push_i32(&mut v, 99);
        assert_eq!(pop_i32(&mut v), Some(99));
        assert_eq!(__adam_vec_len(&v), 0);
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_pop_order() {
        let mut v = new_i32_vec();
        push_i32(&mut v, 1);
        push_i32(&mut v, 2);
        push_i32(&mut v, 3);
        assert_eq!(pop_i32(&mut v), Some(3));
        assert_eq!(pop_i32(&mut v), Some(2));
        assert_eq!(pop_i32(&mut v), Some(1));
        assert_eq!(pop_i32(&mut v), None);
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_pop_empty() {
        let mut v = new_i32_vec();
        assert_eq!(pop_i32(&mut v), None);
    }

    // ---------------------------------------------------------------
    // Get / Set
    // ---------------------------------------------------------------

    #[test]
    fn test_get_out_of_bounds() {
        let v = new_i32_vec();
        assert_eq!(get_i32(&v, 0), None);
        assert_eq!(get_i32(&v, 100), None);
    }

    #[test]
    fn test_set() {
        let mut v = new_i32_vec();
        push_i32(&mut v, 10);
        push_i32(&mut v, 20);
        let val: i32 = 99;
        __adam_vec_set(&mut v, 1, val.to_ne_bytes().as_ptr());
        assert_eq!(get_i32(&v, 0), Some(10));
        assert_eq!(get_i32(&v, 1), Some(99));
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_set_out_of_bounds_ignored() {
        let mut v = new_i32_vec();
        push_i32(&mut v, 10);
        let val: i32 = 99;
        // Should be a no-op, not crash.
        __adam_vec_set(&mut v, 5, val.to_ne_bytes().as_ptr());
        assert_eq!(get_i32(&v, 0), Some(10));
        __adam_vec_drop(&mut v);
    }

    // ---------------------------------------------------------------
    // Insert / Remove
    // ---------------------------------------------------------------

    #[test]
    fn test_insert_at_beginning() {
        let mut v = new_i32_vec();
        push_i32(&mut v, 2);
        push_i32(&mut v, 3);
        let val: i32 = 1;
        __adam_vec_insert(&mut v, 0, val.to_ne_bytes().as_ptr());
        assert_eq!(__adam_vec_len(&v), 3);
        assert_eq!(get_i32(&v, 0), Some(1));
        assert_eq!(get_i32(&v, 1), Some(2));
        assert_eq!(get_i32(&v, 2), Some(3));
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_insert_in_middle() {
        let mut v = new_i32_vec();
        push_i32(&mut v, 1);
        push_i32(&mut v, 3);
        let val: i32 = 2;
        __adam_vec_insert(&mut v, 1, val.to_ne_bytes().as_ptr());
        assert_eq!(get_i32(&v, 0), Some(1));
        assert_eq!(get_i32(&v, 1), Some(2));
        assert_eq!(get_i32(&v, 2), Some(3));
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_insert_at_end() {
        let mut v = new_i32_vec();
        push_i32(&mut v, 1);
        push_i32(&mut v, 2);
        let val: i32 = 3;
        __adam_vec_insert(&mut v, 2, val.to_ne_bytes().as_ptr());
        assert_eq!(get_i32(&v, 2), Some(3));
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_remove_first() {
        let mut v = new_i32_vec();
        push_i32(&mut v, 10);
        push_i32(&mut v, 20);
        push_i32(&mut v, 30);
        let mut out = [0u8; 4];
        __adam_vec_remove(&mut v, 0, out.as_mut_ptr());
        assert_eq!(i32::from_ne_bytes(out), 10);
        assert_eq!(__adam_vec_len(&v), 2);
        assert_eq!(get_i32(&v, 0), Some(20));
        assert_eq!(get_i32(&v, 1), Some(30));
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_remove_last() {
        let mut v = new_i32_vec();
        push_i32(&mut v, 10);
        push_i32(&mut v, 20);
        let mut out = [0u8; 4];
        __adam_vec_remove(&mut v, 1, out.as_mut_ptr());
        assert_eq!(i32::from_ne_bytes(out), 20);
        assert_eq!(__adam_vec_len(&v), 1);
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_remove_middle() {
        let mut v = new_i32_vec();
        push_i32(&mut v, 1);
        push_i32(&mut v, 2);
        push_i32(&mut v, 3);
        push_i32(&mut v, 4);
        let mut out = [0u8; 4];
        __adam_vec_remove(&mut v, 1, out.as_mut_ptr());
        assert_eq!(i32::from_ne_bytes(out), 2);
        assert_eq!(get_i32(&v, 0), Some(1));
        assert_eq!(get_i32(&v, 1), Some(3));
        assert_eq!(get_i32(&v, 2), Some(4));
        __adam_vec_drop(&mut v);
    }

    // ---------------------------------------------------------------
    // First / Last
    // ---------------------------------------------------------------

    #[test]
    fn test_first_last() {
        let mut v = new_i32_vec();
        push_i32(&mut v, 10);
        push_i32(&mut v, 20);
        push_i32(&mut v, 30);
        let mut buf = [0u8; 4];
        assert!(__adam_vec_first(&v, buf.as_mut_ptr()));
        assert_eq!(i32::from_ne_bytes(buf), 10);
        assert!(__adam_vec_last(&v, buf.as_mut_ptr()));
        assert_eq!(i32::from_ne_bytes(buf), 30);
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_first_last_empty() {
        let v = new_i32_vec();
        let mut buf = [0u8; 4];
        assert!(!__adam_vec_first(&v, buf.as_mut_ptr()));
        assert!(!__adam_vec_last(&v, buf.as_mut_ptr()));
    }

    #[test]
    fn test_first_last_single() {
        let mut v = new_i32_vec();
        push_i32(&mut v, 42);
        let mut buf = [0u8; 4];
        assert!(__adam_vec_first(&v, buf.as_mut_ptr()));
        assert_eq!(i32::from_ne_bytes(buf), 42);
        assert!(__adam_vec_last(&v, buf.as_mut_ptr()));
        assert_eq!(i32::from_ne_bytes(buf), 42);
        __adam_vec_drop(&mut v);
    }

    // ---------------------------------------------------------------
    // Contains
    // ---------------------------------------------------------------

    #[test]
    fn test_contains() {
        let mut v = new_i32_vec();
        push_i32(&mut v, 1);
        push_i32(&mut v, 2);
        push_i32(&mut v, 3);
        let yes: i32 = 2;
        let no: i32 = 99;
        assert!(__adam_vec_contains(&v, yes.to_ne_bytes().as_ptr()));
        assert!(!__adam_vec_contains(&v, no.to_ne_bytes().as_ptr()));
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_contains_empty() {
        let v = new_i32_vec();
        let val: i32 = 1;
        assert!(!__adam_vec_contains(&v, val.to_ne_bytes().as_ptr()));
    }

    // ---------------------------------------------------------------
    // Reverse
    // ---------------------------------------------------------------

    #[test]
    fn test_reverse() {
        let mut v = new_i32_vec();
        for i in 0..5 {
            push_i32(&mut v, i);
        }
        __adam_vec_reverse(&mut v);
        for i in 0..5 {
            assert_eq!(get_i32(&v, i as u64), Some(4 - i));
        }
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_reverse_single() {
        let mut v = new_i32_vec();
        push_i32(&mut v, 42);
        __adam_vec_reverse(&mut v);
        assert_eq!(get_i32(&v, 0), Some(42));
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_reverse_empty() {
        let mut v = new_i32_vec();
        __adam_vec_reverse(&mut v); // should not crash
        assert_eq!(__adam_vec_len(&v), 0);
    }

    // ---------------------------------------------------------------
    // Clear
    // ---------------------------------------------------------------

    #[test]
    fn test_clear() {
        let mut v = new_i32_vec();
        push_i32(&mut v, 1);
        push_i32(&mut v, 2);
        let cap_before = __adam_vec_capacity(&v);
        __adam_vec_clear(&mut v);
        assert_eq!(__adam_vec_len(&v), 0);
        assert!(__adam_vec_is_empty(&v));
        // Capacity should be unchanged.
        assert_eq!(__adam_vec_capacity(&v), cap_before);
        __adam_vec_drop(&mut v);
    }

    // ---------------------------------------------------------------
    // Extend
    // ---------------------------------------------------------------

    #[test]
    fn test_extend() {
        let mut a = new_i32_vec();
        let mut b = new_i32_vec();
        push_i32(&mut a, 1);
        push_i32(&mut a, 2);
        push_i32(&mut b, 3);
        push_i32(&mut b, 4);
        __adam_vec_extend(&mut a, &b);
        assert_eq!(__adam_vec_len(&a), 4);
        assert_eq!(get_i32(&a, 0), Some(1));
        assert_eq!(get_i32(&a, 1), Some(2));
        assert_eq!(get_i32(&a, 2), Some(3));
        assert_eq!(get_i32(&a, 3), Some(4));
        __adam_vec_drop(&mut a);
        __adam_vec_drop(&mut b);
    }

    #[test]
    fn test_extend_empty_src() {
        let mut a = new_i32_vec();
        let b = new_i32_vec();
        push_i32(&mut a, 1);
        __adam_vec_extend(&mut a, &b);
        assert_eq!(__adam_vec_len(&a), 1);
        __adam_vec_drop(&mut a);
    }

    // ---------------------------------------------------------------
    // Sort
    // ---------------------------------------------------------------

    extern "C" fn cmp_i32(a: *const u8, b: *const u8) -> i32 {
        let va = i32::from_ne_bytes(unsafe { *(a as *const [u8; 4]) });
        let vb = i32::from_ne_bytes(unsafe { *(b as *const [u8; 4]) });
        va.cmp(&vb) as i32
    }

    #[test]
    fn test_sort() {
        let mut v = new_i32_vec();
        push_i32(&mut v, 5);
        push_i32(&mut v, 1);
        push_i32(&mut v, 4);
        push_i32(&mut v, 2);
        push_i32(&mut v, 3);
        __adam_vec_sort(&mut v, cmp_i32);
        for i in 0..5 {
            assert_eq!(get_i32(&v, i as u64), Some(i + 1));
        }
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_sort_already_sorted() {
        let mut v = new_i32_vec();
        for i in 0..5 {
            push_i32(&mut v, i);
        }
        __adam_vec_sort(&mut v, cmp_i32);
        for i in 0..5 {
            assert_eq!(get_i32(&v, i as u64), Some(i));
        }
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_sort_reverse() {
        let mut v = new_i32_vec();
        for i in (0..5).rev() {
            push_i32(&mut v, i);
        }
        __adam_vec_sort(&mut v, cmp_i32);
        for i in 0..5 {
            assert_eq!(get_i32(&v, i as u64), Some(i));
        }
        __adam_vec_drop(&mut v);
    }

    // ---------------------------------------------------------------
    // Clone
    // ---------------------------------------------------------------

    #[test]
    fn test_clone() {
        let mut v = new_i32_vec();
        push_i32(&mut v, 10);
        push_i32(&mut v, 20);
        let mut c = __adam_vec_clone(&v);
        assert_eq!(__adam_vec_len(&c), 2);
        assert_eq!(get_i32(&c, 0), Some(10));
        assert_eq!(get_i32(&c, 1), Some(20));
        // Mutating original should not affect clone.
        let val: i32 = 99;
        __adam_vec_set(&mut v, 0, val.to_ne_bytes().as_ptr());
        assert_eq!(get_i32(&c, 0), Some(10));
        __adam_vec_drop(&mut v);
        __adam_vec_drop(&mut c);
    }

    #[test]
    fn test_clone_empty() {
        let v = new_i32_vec();
        let mut c = __adam_vec_clone(&v);
        assert_eq!(__adam_vec_len(&c), 0);
        assert!(__adam_vec_is_empty(&c));
        __adam_vec_drop(&mut c);
    }

    // ---------------------------------------------------------------
    // Drop
    // ---------------------------------------------------------------

    #[test]
    fn test_drop() {
        let mut v = new_i32_vec();
        for i in 0..100 {
            push_i32(&mut v, i);
        }
        __adam_vec_drop(&mut v);
        // After drop, ptr should be null and len/cap zero.
        assert_eq!(v.len, 0);
        assert_eq!(v.cap, 0);
        assert!(v.ptr.is_null());
    }

    // ---------------------------------------------------------------
    // Capacity growth
    // ---------------------------------------------------------------

    #[test]
    fn test_capacity_growth() {
        let mut v = new_i32_vec();
        assert_eq!(__adam_vec_capacity(&v), 0);
        push_i32(&mut v, 1);
        let cap1 = __adam_vec_capacity(&v);
        assert!(cap1 >= 1);
        // Keep pushing until capacity grows.
        for i in 1..100 {
            push_i32(&mut v, i);
        }
        assert!(
            __adam_vec_capacity(&v) >= 100,
            "capacity should accommodate 100 elements"
        );
        __adam_vec_drop(&mut v);
    }

    // ---------------------------------------------------------------
    // Large vec (1000 elements)
    // ---------------------------------------------------------------

    #[test]
    fn test_large_vec() {
        let mut v = new_i32_vec();
        for i in 0..1000 {
            push_i32(&mut v, i);
        }
        assert_eq!(__adam_vec_len(&v), 1000);
        for i in 0..1000 {
            assert_eq!(get_i32(&v, i as u64), Some(i));
        }
        // Pop all.
        for i in (0..1000).rev() {
            assert_eq!(pop_i32(&mut v), Some(i));
        }
        assert!(__adam_vec_is_empty(&v));
        __adam_vec_drop(&mut v);
    }

    // ---------------------------------------------------------------
    // get_ptr
    // ---------------------------------------------------------------

    #[test]
    fn test_get_ptr() {
        let mut v = new_i32_vec();
        push_i32(&mut v, 42);
        push_i32(&mut v, 99);
        let p = __adam_vec_get_ptr(&v, 1);
        assert!(!p.is_null());
        let val = i32::from_ne_bytes(unsafe { *(p as *const [u8; 4]) });
        assert_eq!(val, 99);
        // Out-of-bounds returns null.
        assert!(__adam_vec_get_ptr(&v, 10).is_null());
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_get_ptr_empty() {
        let v = new_i32_vec();
        assert!(__adam_vec_get_ptr(&v, 0).is_null());
    }

    // ---------------------------------------------------------------
    // Adversarial / exhaustive tests
    // ---------------------------------------------------------------

    #[test]
    fn test_push_10000_elements_all_accessible() {
        let mut v = new_i32_vec();
        for i in 0..10_000 {
            push_i32(&mut v, i);
        }
        assert_eq!(__adam_vec_len(&v), 10_000);
        for i in 0..10_000 {
            assert_eq!(
                get_i32(&v, i as u64),
                Some(i),
                "element at index {} should be {}",
                i,
                i
            );
        }
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_insert_at_index_0_repeatedly() {
        // Worst case: shifts entire array on every insert.
        let mut v = new_i32_vec();
        let n = 500;
        for i in 0..n {
            let val: i32 = i;
            __adam_vec_insert(&mut v, 0, val.to_ne_bytes().as_ptr());
        }
        assert_eq!(__adam_vec_len(&v), n as u64);
        // Elements should be in reverse order (last inserted is at index 0).
        for i in 0..n {
            assert_eq!(
                get_i32(&v, i as u64),
                Some(n - 1 - i),
                "index {} should be {}",
                i,
                n - 1 - i
            );
        }
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_remove_from_index_0_repeatedly() {
        // Worst case: shifts entire array on every remove.
        let mut v = new_i32_vec();
        let n = 500;
        for i in 0..n {
            push_i32(&mut v, i);
        }
        for i in 0..n {
            let mut out = [0u8; 4];
            __adam_vec_remove(&mut v, 0, out.as_mut_ptr());
            assert_eq!(i32::from_ne_bytes(out), i);
            assert_eq!(__adam_vec_len(&v), (n - 1 - i) as u64);
        }
        assert!(__adam_vec_is_empty(&v));
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_sort_with_duplicates() {
        let mut v = new_i32_vec();
        let data = [5, 3, 3, 1, 4, 1, 2, 5, 2, 4];
        for &val in &data {
            push_i32(&mut v, val);
        }
        __adam_vec_sort(&mut v, cmp_i32);
        let expected = [1, 1, 2, 2, 3, 3, 4, 4, 5, 5];
        for (i, &exp) in expected.iter().enumerate() {
            assert_eq!(
                get_i32(&v, i as u64),
                Some(exp),
                "sorted index {} should be {}",
                i,
                exp
            );
        }
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_sort_already_sorted_large() {
        // Best case for many sorting algorithms.
        let mut v = new_i32_vec();
        for i in 0..200 {
            push_i32(&mut v, i);
        }
        __adam_vec_sort(&mut v, cmp_i32);
        for i in 0..200 {
            assert_eq!(get_i32(&v, i as u64), Some(i));
        }
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_sort_reverse_sorted_large() {
        // Worst case for some sorting algorithms.
        let mut v = new_i32_vec();
        for i in (0..200).rev() {
            push_i32(&mut v, i);
        }
        __adam_vec_sort(&mut v, cmp_i32);
        for i in 0..200 {
            assert_eq!(get_i32(&v, i as u64), Some(i));
        }
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_extend_from_self_clone() {
        // Double in size by extending from a clone of self.
        let mut v = new_i32_vec();
        for i in 0..50 {
            push_i32(&mut v, i);
        }
        let c = __adam_vec_clone(&v);
        __adam_vec_extend(&mut v, &c);
        assert_eq!(__adam_vec_len(&v), 100);
        for i in 0..50 {
            assert_eq!(get_i32(&v, i as u64), Some(i));
            assert_eq!(get_i32(&v, (i + 50) as u64), Some(i));
        }
        // Must drop clone to avoid leak.
        let mut c = c;
        __adam_vec_drop(&mut c);
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_contains_large_vec_linear_scan_stress() {
        let mut v = new_i32_vec();
        for i in 0..5_000 {
            push_i32(&mut v, i);
        }
        // Check element at the very end (worst case linear scan).
        let last: i32 = 4_999;
        assert!(__adam_vec_contains(&v, last.to_ne_bytes().as_ptr()));
        // Check element that does not exist.
        let missing: i32 = 5_000;
        assert!(!__adam_vec_contains(&v, missing.to_ne_bytes().as_ptr()));
        // Check element at the very start.
        let first: i32 = 0;
        assert!(__adam_vec_contains(&v, first.to_ne_bytes().as_ptr()));
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_reverse_single_element_noop() {
        let mut v = new_i32_vec();
        push_i32(&mut v, 42);
        __adam_vec_reverse(&mut v);
        assert_eq!(__adam_vec_len(&v), 1);
        assert_eq!(get_i32(&v, 0), Some(42));
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_pop_returns_lifo_order() {
        let mut v = new_i32_vec();
        for i in 0..100 {
            push_i32(&mut v, i);
        }
        for i in (0..100).rev() {
            assert_eq!(pop_i32(&mut v), Some(i), "pop should return {} (LIFO)", i);
        }
        assert_eq!(pop_i32(&mut v), None);
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_clone_large_vec_modify_original_verify_clone_unchanged() {
        let mut v = new_i32_vec();
        for i in 0..1_000 {
            push_i32(&mut v, i);
        }
        let mut c = __adam_vec_clone(&v);
        // Modify every element in the original.
        for i in 0..1_000u64 {
            let val: i32 = -(i as i32);
            __adam_vec_set(&mut v, i, val.to_ne_bytes().as_ptr());
        }
        // Clone should be completely unaffected.
        for i in 0..1_000 {
            assert_eq!(
                get_i32(&c, i as u64),
                Some(i),
                "clone index {} should still be {}",
                i,
                i
            );
        }
        __adam_vec_drop(&mut v);
        __adam_vec_drop(&mut c);
    }

    #[test]
    fn test_mixed_push_pop_sequence() {
        let mut v = new_i32_vec();
        // Push 5, pop 3, push 5, pop 3, ...
        let mut expected_len: u64 = 0;
        let mut counter: i32 = 0;
        for _ in 0..20 {
            for _ in 0..5 {
                push_i32(&mut v, counter);
                counter += 1;
                expected_len += 1;
            }
            for _ in 0..3 {
                let popped = pop_i32(&mut v);
                assert!(popped.is_some());
                expected_len -= 1;
            }
            assert_eq!(__adam_vec_len(&v), expected_len);
        }
        // Final length: 20 * (5-3) = 40
        assert_eq!(__adam_vec_len(&v), 40);
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_capacity_never_decreases_after_push() {
        let mut v = new_i32_vec();
        let mut prev_cap = __adam_vec_capacity(&v);
        for i in 0..200 {
            push_i32(&mut v, i);
            let new_cap = __adam_vec_capacity(&v);
            assert!(
                new_cap >= prev_cap,
                "capacity decreased from {} to {} at push {}",
                prev_cap,
                new_cap,
                i
            );
            prev_cap = new_cap;
        }
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_set_at_every_valid_index() {
        let mut v = new_i32_vec();
        for i in 0..100 {
            push_i32(&mut v, i);
        }
        // Overwrite every element with its negative.
        for i in 0..100u64 {
            let val: i32 = -(i as i32);
            __adam_vec_set(&mut v, i, val.to_ne_bytes().as_ptr());
        }
        for i in 0..100u64 {
            assert_eq!(get_i32(&v, i), Some(-(i as i32)));
        }
        // Set out of bounds should be no-op.
        let val: i32 = 999;
        __adam_vec_set(&mut v, 100, val.to_ne_bytes().as_ptr());
        assert_eq!(__adam_vec_len(&v), 100);
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_get_ptr_returns_correct_address() {
        let mut v = new_i32_vec();
        for i in 0..10 {
            push_i32(&mut v, i * 100);
        }
        // Verify pointer arithmetic: consecutive elements should be elem_size apart.
        let elem_size = std::mem::size_of::<i32>();
        for i in 0..10u64 {
            let p = __adam_vec_get_ptr(&v, i);
            assert!(!p.is_null());
            let val = i32::from_ne_bytes(unsafe { *(p as *const [u8; 4]) });
            assert_eq!(val, (i as i32) * 100);
            // Verify pointer is base + i * elem_size.
            let base = __adam_vec_get_ptr(&v, 0);
            assert_eq!(
                p as usize,
                base as usize + i as usize * elem_size,
                "pointer for index {} is not at expected offset",
                i
            );
        }
        // Out of bounds returns null.
        assert!(__adam_vec_get_ptr(&v, 10).is_null());
        assert!(__adam_vec_get_ptr(&v, u64::MAX).is_null());
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_clear_then_repopulate() {
        let mut v = new_i32_vec();
        for i in 0..50 {
            push_i32(&mut v, i);
        }
        __adam_vec_clear(&mut v);
        assert!(__adam_vec_is_empty(&v));
        // Re-populate with different values.
        for i in 100..150 {
            push_i32(&mut v, i);
        }
        assert_eq!(__adam_vec_len(&v), 50);
        for i in 0..50u64 {
            assert_eq!(get_i32(&v, i), Some(100 + i as i32));
        }
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_insert_and_remove_interleaved() {
        let mut v = new_i32_vec();
        push_i32(&mut v, 10);
        push_i32(&mut v, 20);
        push_i32(&mut v, 30);
        // Insert at 1: [10, 15, 20, 30]
        let val: i32 = 15;
        __adam_vec_insert(&mut v, 1, val.to_ne_bytes().as_ptr());
        assert_eq!(get_i32(&v, 0), Some(10));
        assert_eq!(get_i32(&v, 1), Some(15));
        assert_eq!(get_i32(&v, 2), Some(20));
        assert_eq!(get_i32(&v, 3), Some(30));
        // Remove at 2: [10, 15, 30]
        let mut out = [0u8; 4];
        __adam_vec_remove(&mut v, 2, out.as_mut_ptr());
        assert_eq!(i32::from_ne_bytes(out), 20);
        assert_eq!(__adam_vec_len(&v), 3);
        assert_eq!(get_i32(&v, 0), Some(10));
        assert_eq!(get_i32(&v, 1), Some(15));
        assert_eq!(get_i32(&v, 2), Some(30));
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_reverse_even_count() {
        let mut v = new_i32_vec();
        for i in 0..6 {
            push_i32(&mut v, i);
        }
        __adam_vec_reverse(&mut v);
        for i in 0..6 {
            assert_eq!(get_i32(&v, i as u64), Some(5 - i));
        }
        __adam_vec_drop(&mut v);
    }

    #[test]
    fn test_reverse_odd_count() {
        let mut v = new_i32_vec();
        for i in 0..7 {
            push_i32(&mut v, i);
        }
        __adam_vec_reverse(&mut v);
        for i in 0..7 {
            assert_eq!(get_i32(&v, i as u64), Some(6 - i));
        }
        __adam_vec_drop(&mut v);
    }
}
