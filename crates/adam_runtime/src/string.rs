use std::alloc::{alloc, dealloc, Layout};
use std::ptr;

/// An Adam string as seen by the runtime: { ptr, len, cap }.
/// This matches the LLVM struct `{ i8*, i64, i64 }`.
#[repr(C)]
pub struct AdamString {
    pub ptr: *mut u8,
    pub len: u64,
    pub cap: u64,
}

impl AdamString {
    fn empty() -> Self {
        AdamString {
            ptr: ptr::null_mut(),
            len: 0,
            cap: 0,
        }
    }

    pub(crate) fn from_bytes(bytes: &[u8]) -> Self {
        if bytes.is_empty() {
            return Self::empty();
        }
        let cap = bytes.len() as u64;
        unsafe {
            let layout = Layout::from_size_align_unchecked(cap as usize, 1);
            let ptr = alloc(layout);
            if ptr.is_null() {
                std::process::abort();
            }
            ptr::copy_nonoverlapping(bytes.as_ptr(), ptr, bytes.len());
            AdamString {
                ptr,
                len: bytes.len() as u64,
                cap,
            }
        }
    }
}

/// Concatenate two Adam strings. Returns a new string.
/// Arguments are the raw fields of two string structs.
#[no_mangle]
pub extern "C" fn __str_concat(
    a_ptr: *const u8,
    a_len: u64,
    _a_cap: u64,
    b_ptr: *const u8,
    b_len: u64,
    _b_cap: u64,
) -> AdamString {
    let total_len = a_len + b_len;
    if total_len == 0 {
        return AdamString::empty();
    }
    unsafe {
        let layout = Layout::from_size_align_unchecked(total_len as usize, 1);
        let ptr = alloc(layout);
        if ptr.is_null() {
            std::process::abort();
        }
        if a_len > 0 {
            ptr::copy_nonoverlapping(a_ptr, ptr, a_len as usize);
        }
        if b_len > 0 {
            ptr::copy_nonoverlapping(b_ptr, ptr.add(a_len as usize), b_len as usize);
        }
        AdamString {
            ptr,
            len: total_len,
            cap: total_len,
        }
    }
}

/// Drop (free) an Adam string.
#[no_mangle]
pub extern "C" fn __adam_string_drop(ptr: *mut u8, _len: u64, cap: u64) {
    if ptr.is_null() || cap == 0 {
        return;
    }
    unsafe {
        let layout = Layout::from_size_align_unchecked(cap as usize, 1);
        dealloc(ptr, layout);
    }
}

/// Convert an i64 to a string.
#[no_mangle]
pub extern "C" fn __int_to_string(val: i64) -> AdamString {
    let s = val.to_string();
    AdamString::from_bytes(s.as_bytes())
}

/// Convert an f64 to a string.
#[no_mangle]
pub extern "C" fn __float_to_string(val: f64) -> AdamString {
    let s = val.to_string();
    AdamString::from_bytes(s.as_bytes())
}

/// Convert a bool to a string ("true" or "false").
#[no_mangle]
pub extern "C" fn __bool_to_string(val: i8) -> AdamString {
    let s = if val != 0 { "true" } else { "false" };
    AdamString::from_bytes(s.as_bytes())
}

/// Convert a char (Unicode scalar, i32) to a string.
#[no_mangle]
pub extern "C" fn __char_to_string(val: u32) -> AdamString {
    if let Some(c) = char::from_u32(val) {
        let mut buf = [0u8; 4];
        let s = c.encode_utf8(&mut buf);
        AdamString::from_bytes(s.as_bytes())
    } else {
        AdamString::from_bytes("\u{FFFD}".as_bytes())
    }
}

/// Create a string from a C string pointer (null-terminated).
#[no_mangle]
pub extern "C" fn __adam_string_from_cstr(ptr: *const u8) -> AdamString {
    if ptr.is_null() {
        return AdamString::empty();
    }
    unsafe {
        let len = libc_strlen(ptr);
        if len == 0 {
            return AdamString::empty();
        }
        AdamString::from_bytes(std::slice::from_raw_parts(ptr, len))
    }
}

/// Minimal strlen without libc dependency.
unsafe fn libc_strlen(mut ptr: *const u8) -> usize {
    let start = ptr;
    while *ptr != 0 {
        ptr = ptr.add(1);
    }
    ptr as usize - start as usize
}

// ================================================================
// P10.S2 — String Methods
// ================================================================

/// Helper: reconstruct a &[u8] from raw ptr+len.
#[inline]
unsafe fn slice_from_raw(ptr: *const u8, len: u64) -> &'static [u8] {
    if len == 0 || ptr.is_null() {
        &[]
    } else {
        std::slice::from_raw_parts(ptr, len as usize)
    }
}

/// Helper: reconstruct a &str from raw ptr+len (assumes valid UTF-8).
#[inline]
unsafe fn str_from_raw(ptr: *const u8, len: u64) -> &'static str {
    std::str::from_utf8_unchecked(slice_from_raw(ptr, len))
}

// 1. __adam_string_len
/// Return the byte length of an Adam string.
#[no_mangle]
pub extern "C" fn __adam_string_len(_ptr: *const u8, len: u64, _cap: u64) -> u64 {
    len
}

// 2. __adam_string_is_empty
/// Return true if the string has zero length.
#[no_mangle]
pub extern "C" fn __adam_string_is_empty(_ptr: *const u8, len: u64, _cap: u64) -> bool {
    len == 0
}

// 3. __adam_string_contains
/// Return true if `self` contains the substring `sub`.
#[no_mangle]
pub extern "C" fn __adam_string_contains(
    ptr: *const u8,
    len: u64,
    _cap: u64,
    sub_ptr: *const u8,
    sub_len: u64,
    _sub_cap: u64,
) -> bool {
    unsafe {
        let haystack = str_from_raw(ptr, len);
        let needle = str_from_raw(sub_ptr, sub_len);
        haystack.contains(needle)
    }
}

// 4. __adam_string_starts_with
/// Return true if `self` starts with `prefix`.
#[no_mangle]
pub extern "C" fn __adam_string_starts_with(
    ptr: *const u8,
    len: u64,
    _cap: u64,
    prefix_ptr: *const u8,
    prefix_len: u64,
    _prefix_cap: u64,
) -> bool {
    unsafe {
        let s = str_from_raw(ptr, len);
        let prefix = str_from_raw(prefix_ptr, prefix_len);
        s.starts_with(prefix)
    }
}

// 5. __adam_string_ends_with
/// Return true if `self` ends with `suffix`.
#[no_mangle]
pub extern "C" fn __adam_string_ends_with(
    ptr: *const u8,
    len: u64,
    _cap: u64,
    suffix_ptr: *const u8,
    suffix_len: u64,
    _suffix_cap: u64,
) -> bool {
    unsafe {
        let s = str_from_raw(ptr, len);
        let suffix = str_from_raw(suffix_ptr, suffix_len);
        s.ends_with(suffix)
    }
}

// 6. __adam_string_find
/// Find the first occurrence of `sub` in `self`.
/// Returns true if found, writing the byte index to `*found`.
#[no_mangle]
pub extern "C" fn __adam_string_find(
    ptr: *const u8,
    len: u64,
    _cap: u64,
    sub_ptr: *const u8,
    sub_len: u64,
    _sub_cap: u64,
    found: *mut u64,
) -> bool {
    unsafe {
        let haystack = str_from_raw(ptr, len);
        let needle = str_from_raw(sub_ptr, sub_len);
        match haystack.find(needle) {
            Some(idx) => {
                if !found.is_null() {
                    *found = idx as u64;
                }
                true
            }
            None => false,
        }
    }
}

// 7. __adam_string_rfind
/// Find the last occurrence of `sub` in `self`.
/// Returns true if found, writing the byte index to `*found`.
#[no_mangle]
pub extern "C" fn __adam_string_rfind(
    ptr: *const u8,
    len: u64,
    _cap: u64,
    sub_ptr: *const u8,
    sub_len: u64,
    _sub_cap: u64,
    found: *mut u64,
) -> bool {
    unsafe {
        let haystack = str_from_raw(ptr, len);
        let needle = str_from_raw(sub_ptr, sub_len);
        match haystack.rfind(needle) {
            Some(idx) => {
                if !found.is_null() {
                    *found = idx as u64;
                }
                true
            }
            None => false,
        }
    }
}

// 8. __adam_string_split
/// Split `self` by `delim`. Allocates an array of AdamStrings via std::alloc.
/// Writes the array pointer to `*out_arr` and the count to `*out_count`.
#[no_mangle]
pub extern "C" fn __adam_string_split(
    ptr: *const u8,
    len: u64,
    _cap: u64,
    delim_ptr: *const u8,
    delim_len: u64,
    _delim_cap: u64,
    out_arr: *mut *mut AdamString,
    out_count: *mut u64,
) {
    unsafe {
        let s = str_from_raw(ptr, len);
        let delim = str_from_raw(delim_ptr, delim_len);

        let parts: Vec<&str> = s.split(delim).collect();
        let count = parts.len();

        if count == 0 {
            *out_arr = ptr::null_mut();
            *out_count = 0;
            return;
        }

        // Allocate array of AdamString structs.
        let array_size = count * std::mem::size_of::<AdamString>();
        let layout =
            Layout::from_size_align_unchecked(array_size, std::mem::align_of::<AdamString>());
        let arr_ptr = alloc(layout) as *mut AdamString;
        if arr_ptr.is_null() {
            std::process::abort();
        }

        for (i, part) in parts.iter().enumerate() {
            let adam_str = AdamString::from_bytes(part.as_bytes());
            ptr::write(arr_ptr.add(i), adam_str);
        }

        *out_arr = arr_ptr;
        *out_count = count as u64;
    }
}

// 9. __adam_string_trim
/// Trim leading and trailing whitespace, returning a new string.
#[no_mangle]
pub extern "C" fn __adam_string_trim(ptr: *const u8, len: u64, _cap: u64) -> AdamString {
    unsafe {
        let s = str_from_raw(ptr, len);
        let trimmed = s.trim();
        AdamString::from_bytes(trimmed.as_bytes())
    }
}

// 10. __adam_string_trim_start
/// Trim leading whitespace, returning a new string.
#[no_mangle]
pub extern "C" fn __adam_string_trim_start(ptr: *const u8, len: u64, _cap: u64) -> AdamString {
    unsafe {
        let s = str_from_raw(ptr, len);
        let trimmed = s.trim_start();
        AdamString::from_bytes(trimmed.as_bytes())
    }
}

// 11. __adam_string_trim_end
/// Trim trailing whitespace, returning a new string.
#[no_mangle]
pub extern "C" fn __adam_string_trim_end(ptr: *const u8, len: u64, _cap: u64) -> AdamString {
    unsafe {
        let s = str_from_raw(ptr, len);
        let trimmed = s.trim_end();
        AdamString::from_bytes(trimmed.as_bytes())
    }
}

// 12. __adam_string_to_upper
/// Convert to uppercase, returning a new string.
#[no_mangle]
pub extern "C" fn __adam_string_to_upper(ptr: *const u8, len: u64, _cap: u64) -> AdamString {
    unsafe {
        let s = str_from_raw(ptr, len);
        let upper = s.to_uppercase();
        AdamString::from_bytes(upper.as_bytes())
    }
}

// 13. __adam_string_to_lower
/// Convert to lowercase, returning a new string.
#[no_mangle]
pub extern "C" fn __adam_string_to_lower(ptr: *const u8, len: u64, _cap: u64) -> AdamString {
    unsafe {
        let s = str_from_raw(ptr, len);
        let lower = s.to_lowercase();
        AdamString::from_bytes(lower.as_bytes())
    }
}

// 14. __adam_string_replace
/// Replace all occurrences of `from` with `to`, returning a new string.
#[no_mangle]
pub extern "C" fn __adam_string_replace(
    ptr: *const u8,
    len: u64,
    _cap: u64,
    from_ptr: *const u8,
    from_len: u64,
    _from_cap: u64,
    to_ptr: *const u8,
    to_len: u64,
    _to_cap: u64,
) -> AdamString {
    unsafe {
        let s = str_from_raw(ptr, len);
        let from = str_from_raw(from_ptr, from_len);
        let to = str_from_raw(to_ptr, to_len);
        let result = s.replace(from, to);
        AdamString::from_bytes(result.as_bytes())
    }
}

// 15. __adam_string_repeat
/// Repeat the string `n` times, returning a new string.
#[no_mangle]
pub extern "C" fn __adam_string_repeat(ptr: *const u8, len: u64, _cap: u64, n: u64) -> AdamString {
    unsafe {
        let s = str_from_raw(ptr, len);
        let result = s.repeat(n as usize);
        AdamString::from_bytes(result.as_bytes())
    }
}

// 16. __adam_string_slice
/// Return a new string from byte range [start, end).
/// Panics if start or end is not on a UTF-8 char boundary or out of range.
#[no_mangle]
pub extern "C" fn __adam_string_slice(
    ptr: *const u8,
    len: u64,
    _cap: u64,
    start: u64,
    end: u64,
) -> AdamString {
    unsafe {
        let s = str_from_raw(ptr, len);
        let start = start as usize;
        let end = end as usize;
        if start > s.len() || end > s.len() || start > end {
            panic!(
                "string slice out of range: start={}, end={}, len={}",
                start,
                end,
                s.len()
            );
        }
        if !s.is_char_boundary(start) {
            panic!("string slice start {} is not on a char boundary", start);
        }
        if !s.is_char_boundary(end) {
            panic!("string slice end {} is not on a char boundary", end);
        }
        let sliced = &s[start..end];
        AdamString::from_bytes(sliced.as_bytes())
    }
}

// 17. __adam_string_push
/// Append string `s` to the mutable string pointed to by (ptr, len, cap).
/// May reallocate with 2x growth factor.
#[no_mangle]
pub extern "C" fn __adam_string_push(
    ptr_ptr: *mut *mut u8,
    len_ptr: *mut u64,
    cap_ptr: *mut u64,
    s_ptr: *const u8,
    s_len: u64,
    _s_cap: u64,
) {
    if s_len == 0 {
        return;
    }
    unsafe {
        let cur_ptr = *ptr_ptr;
        let cur_len = *len_ptr;
        let cur_cap = *cap_ptr;
        let new_len = cur_len + s_len;

        if new_len > cur_cap {
            // Need to grow. Use 2x growth factor, minimum new_len.
            let new_cap = std::cmp::max(new_len, cur_cap * 2);
            let new_ptr = if cur_cap == 0 || cur_ptr.is_null() {
                // Fresh allocation.
                let layout = Layout::from_size_align_unchecked(new_cap as usize, 1);
                let p = alloc(layout);
                if p.is_null() {
                    std::process::abort();
                }
                // Copy existing data if any.
                if cur_len > 0 && !cur_ptr.is_null() {
                    ptr::copy_nonoverlapping(cur_ptr, p, cur_len as usize);
                }
                p
            } else {
                // Realloc existing buffer.
                let old_layout = Layout::from_size_align_unchecked(cur_cap as usize, 1);
                let p = std::alloc::realloc(cur_ptr, old_layout, new_cap as usize);
                if p.is_null() {
                    std::process::abort();
                }
                p
            };

            *ptr_ptr = new_ptr;
            *cap_ptr = new_cap;
        }

        // Append the data.
        let dst = (*ptr_ptr).add(cur_len as usize);
        ptr::copy_nonoverlapping(s_ptr, dst, s_len as usize);
        *len_ptr = new_len;
    }
}

// 18. __adam_string_push_char
/// Push a single Unicode char (as u32) onto the mutable string.
/// May reallocate with 2x growth factor.
#[no_mangle]
pub extern "C" fn __adam_string_push_char(
    ptr_ptr: *mut *mut u8,
    len_ptr: *mut u64,
    cap_ptr: *mut u64,
    c: u32,
) {
    let ch = match char::from_u32(c) {
        Some(c) => c,
        None => '\u{FFFD}', // replacement character
    };
    let mut buf = [0u8; 4];
    let encoded = ch.encode_utf8(&mut buf);
    let encoded_bytes = encoded.as_bytes();

    // Reuse __adam_string_push to do the actual append + realloc.
    __adam_string_push(
        ptr_ptr,
        len_ptr,
        cap_ptr,
        encoded_bytes.as_ptr(),
        encoded_bytes.len() as u64,
        0,
    );
}

// 19. __adam_string_eq
/// Byte-by-byte equality of two Adam strings.
#[no_mangle]
pub extern "C" fn __adam_string_eq(
    a_ptr: *const u8,
    a_len: u64,
    _a_cap: u64,
    b_ptr: *const u8,
    b_len: u64,
    _b_cap: u64,
) -> bool {
    if a_len != b_len {
        return false;
    }
    if a_len == 0 {
        return true;
    }
    unsafe {
        let a = slice_from_raw(a_ptr, a_len);
        let b = slice_from_raw(b_ptr, b_len);
        a == b
    }
}

// 20. __adam_string_cmp
/// Lexicographic comparison of two Adam strings.
/// Returns -1, 0, or 1.
#[no_mangle]
pub extern "C" fn __adam_string_cmp(
    a_ptr: *const u8,
    a_len: u64,
    _a_cap: u64,
    b_ptr: *const u8,
    b_len: u64,
    _b_cap: u64,
) -> i8 {
    unsafe {
        let a = slice_from_raw(a_ptr, a_len);
        let b = slice_from_raw(b_ptr, b_len);
        match a.cmp(b) {
            std::cmp::Ordering::Less => -1,
            std::cmp::Ordering::Equal => 0,
            std::cmp::Ordering::Greater => 1,
        }
    }
}

// 21. __adam_string_hash
/// FNV-1a hash of the string bytes. Returns u64.
#[no_mangle]
pub extern "C" fn __adam_string_hash(ptr: *const u8, len: u64, _cap: u64) -> u64 {
    const FNV_OFFSET_BASIS: u64 = 0xcbf29ce484222325;
    const FNV_PRIME: u64 = 0x00000100000001B3;

    unsafe {
        let bytes = slice_from_raw(ptr, len);
        let mut hash = FNV_OFFSET_BASIS;
        for &byte in bytes {
            hash ^= byte as u64;
            hash = hash.wrapping_mul(FNV_PRIME);
        }
        hash
    }
}

// 22. __adam_string_clone
/// Deep copy of an Adam string.
#[no_mangle]
pub extern "C" fn __adam_string_clone(ptr: *const u8, len: u64, _cap: u64) -> AdamString {
    unsafe {
        let bytes = slice_from_raw(ptr, len);
        AdamString::from_bytes(bytes)
    }
}

// 23. __adam_string_char_at
/// Get the Unicode char at the given byte index.
/// Panics if the index is out of bounds or not on a char boundary.
/// Returns the char as a u32.
#[no_mangle]
pub extern "C" fn __adam_string_char_at(ptr: *const u8, len: u64, _cap: u64, idx: u64) -> u32 {
    unsafe {
        let s = str_from_raw(ptr, len);
        let idx = idx as usize;
        if idx >= s.len() {
            panic!(
                "char_at index {} out of bounds for string of length {}",
                idx,
                s.len()
            );
        }
        if !s.is_char_boundary(idx) {
            panic!("char_at index {} is not on a char boundary", idx);
        }
        let ch = s[idx..].chars().next().unwrap();
        ch as u32
    }
}

// ================================================================
// Tests
// ================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use std::ptr;

    /// Helper to create an AdamString from a Rust &str for testing.
    fn make_string(s: &str) -> AdamString {
        AdamString::from_bytes(s.as_bytes())
    }

    /// Helper to read an AdamString back to a Rust String (for assertions).
    unsafe fn read_string(s: &AdamString) -> String {
        if s.len == 0 || s.ptr.is_null() {
            return String::new();
        }
        let bytes = std::slice::from_raw_parts(s.ptr, s.len as usize);
        String::from_utf8_lossy(bytes).into_owned()
    }

    /// Helper to drop an AdamString after a test.
    fn drop_string(s: AdamString) {
        __adam_string_drop(s.ptr, s.len, s.cap);
    }

    // ---- len / is_empty ----

    #[test]
    fn test_len_ascii() {
        let s = make_string("hello");
        assert_eq!(__adam_string_len(s.ptr, s.len, s.cap), 5);
        drop_string(s);
    }

    #[test]
    fn test_len_empty() {
        let s = AdamString::empty();
        assert_eq!(__adam_string_len(s.ptr, s.len, s.cap), 0);
    }

    #[test]
    fn test_len_unicode() {
        // "hi" + emoji (U+1F600 = 4 bytes)
        let s = make_string("hi\u{1F600}");
        // 'h' 'i' = 2 bytes, emoji = 4 bytes => 6 bytes
        assert_eq!(__adam_string_len(s.ptr, s.len, s.cap), 6);
        drop_string(s);
    }

    #[test]
    fn test_is_empty_true() {
        let s = AdamString::empty();
        assert!(__adam_string_is_empty(s.ptr, s.len, s.cap));
    }

    #[test]
    fn test_is_empty_false() {
        let s = make_string("x");
        assert!(!__adam_string_is_empty(s.ptr, s.len, s.cap));
        drop_string(s);
    }

    // ---- contains / starts_with / ends_with ----

    #[test]
    fn test_contains_found() {
        let s = make_string("hello world");
        let sub = make_string("lo wo");
        assert!(__adam_string_contains(
            s.ptr, s.len, s.cap, sub.ptr, sub.len, sub.cap,
        ));
        drop_string(s);
        drop_string(sub);
    }

    #[test]
    fn test_contains_not_found() {
        let s = make_string("hello world");
        let sub = make_string("xyz");
        assert!(!__adam_string_contains(
            s.ptr, s.len, s.cap, sub.ptr, sub.len, sub.cap,
        ));
        drop_string(s);
        drop_string(sub);
    }

    #[test]
    fn test_contains_empty_needle() {
        let s = make_string("hello");
        let sub = AdamString::empty();
        // Empty needle is always contained.
        assert!(__adam_string_contains(
            s.ptr, s.len, s.cap, sub.ptr, sub.len, sub.cap,
        ));
        drop_string(s);
    }

    #[test]
    fn test_contains_empty_haystack_empty_needle() {
        let s = AdamString::empty();
        let sub = AdamString::empty();
        assert!(__adam_string_contains(
            s.ptr, s.len, s.cap, sub.ptr, sub.len, sub.cap,
        ));
    }

    #[test]
    fn test_starts_with() {
        let s = make_string("hello world");
        let prefix = make_string("hello");
        assert!(__adam_string_starts_with(
            s.ptr, s.len, s.cap, prefix.ptr, prefix.len, prefix.cap,
        ));
        drop_string(s);
        drop_string(prefix);
    }

    #[test]
    fn test_starts_with_empty_prefix() {
        let s = make_string("hello");
        let prefix = AdamString::empty();
        assert!(__adam_string_starts_with(
            s.ptr, s.len, s.cap, prefix.ptr, prefix.len, prefix.cap,
        ));
        drop_string(s);
    }

    #[test]
    fn test_ends_with() {
        let s = make_string("hello world");
        let suffix = make_string("world");
        assert!(__adam_string_ends_with(
            s.ptr, s.len, s.cap, suffix.ptr, suffix.len, suffix.cap,
        ));
        drop_string(s);
        drop_string(suffix);
    }

    #[test]
    fn test_ends_with_empty_suffix() {
        let s = make_string("hello");
        let suffix = AdamString::empty();
        assert!(__adam_string_ends_with(
            s.ptr, s.len, s.cap, suffix.ptr, suffix.len, suffix.cap,
        ));
        drop_string(s);
    }

    // ---- find / rfind ----

    #[test]
    fn test_find_found() {
        let s = make_string("abcabc");
        let sub = make_string("bc");
        let mut idx: u64 = 0;
        assert!(__adam_string_find(
            s.ptr, s.len, s.cap, sub.ptr, sub.len, sub.cap, &mut idx,
        ));
        assert_eq!(idx, 1); // first occurrence at byte index 1
        drop_string(s);
        drop_string(sub);
    }

    #[test]
    fn test_find_not_found() {
        let s = make_string("abc");
        let sub = make_string("xyz");
        let mut idx: u64 = 999;
        assert!(!__adam_string_find(
            s.ptr, s.len, s.cap, sub.ptr, sub.len, sub.cap, &mut idx,
        ));
        // idx should be unchanged.
        assert_eq!(idx, 999);
        drop_string(s);
        drop_string(sub);
    }

    #[test]
    fn test_rfind_found() {
        let s = make_string("abcabc");
        let sub = make_string("bc");
        let mut idx: u64 = 0;
        assert!(__adam_string_rfind(
            s.ptr, s.len, s.cap, sub.ptr, sub.len, sub.cap, &mut idx,
        ));
        assert_eq!(idx, 4); // last occurrence at byte index 4
        drop_string(s);
        drop_string(sub);
    }

    #[test]
    fn test_rfind_not_found() {
        let s = make_string("abc");
        let sub = make_string("xyz");
        let mut idx: u64 = 999;
        assert!(!__adam_string_rfind(
            s.ptr, s.len, s.cap, sub.ptr, sub.len, sub.cap, &mut idx,
        ));
        assert_eq!(idx, 999);
        drop_string(s);
        drop_string(sub);
    }

    // ---- split ----

    #[test]
    fn test_split_basic() {
        let s = make_string("a,b,c");
        let delim = make_string(",");
        let mut arr: *mut AdamString = ptr::null_mut();
        let mut count: u64 = 0;

        __adam_string_split(
            s.ptr, s.len, s.cap, delim.ptr, delim.len, delim.cap, &mut arr, &mut count,
        );

        assert_eq!(count, 3);
        unsafe {
            assert_eq!(read_string(&*arr.add(0)), "a");
            assert_eq!(read_string(&*arr.add(1)), "b");
            assert_eq!(read_string(&*arr.add(2)), "c");
            // Clean up each part.
            for i in 0..count as usize {
                let part = ptr::read(arr.add(i));
                drop_string(part);
            }
            // Free the array itself.
            let layout = Layout::from_size_align_unchecked(
                count as usize * std::mem::size_of::<AdamString>(),
                std::mem::align_of::<AdamString>(),
            );
            dealloc(arr as *mut u8, layout);
        }
        drop_string(s);
        drop_string(delim);
    }

    #[test]
    fn test_split_no_delimiter() {
        let s = make_string("hello");
        let delim = make_string(",");
        let mut arr: *mut AdamString = ptr::null_mut();
        let mut count: u64 = 0;

        __adam_string_split(
            s.ptr, s.len, s.cap, delim.ptr, delim.len, delim.cap, &mut arr, &mut count,
        );

        assert_eq!(count, 1);
        unsafe {
            assert_eq!(read_string(&*arr.add(0)), "hello");
            let part = ptr::read(arr.add(0));
            drop_string(part);
            let layout = Layout::from_size_align_unchecked(
                std::mem::size_of::<AdamString>(),
                std::mem::align_of::<AdamString>(),
            );
            dealloc(arr as *mut u8, layout);
        }
        drop_string(s);
        drop_string(delim);
    }

    #[test]
    fn test_split_empty_string() {
        let s = AdamString::empty();
        let delim = make_string(",");
        let mut arr: *mut AdamString = ptr::null_mut();
        let mut count: u64 = 0;

        __adam_string_split(
            s.ptr, s.len, s.cap, delim.ptr, delim.len, delim.cap, &mut arr, &mut count,
        );

        // Splitting "" by "," yields [""]
        assert_eq!(count, 1);
        unsafe {
            assert_eq!(read_string(&*arr.add(0)), "");
            let part = ptr::read(arr.add(0));
            drop_string(part);
            let layout = Layout::from_size_align_unchecked(
                std::mem::size_of::<AdamString>(),
                std::mem::align_of::<AdamString>(),
            );
            dealloc(arr as *mut u8, layout);
        }
        drop_string(delim);
    }

    #[test]
    fn test_split_multi_char_delim() {
        let s = make_string("one::two::three");
        let delim = make_string("::");
        let mut arr: *mut AdamString = ptr::null_mut();
        let mut count: u64 = 0;

        __adam_string_split(
            s.ptr, s.len, s.cap, delim.ptr, delim.len, delim.cap, &mut arr, &mut count,
        );

        assert_eq!(count, 3);
        unsafe {
            assert_eq!(read_string(&*arr.add(0)), "one");
            assert_eq!(read_string(&*arr.add(1)), "two");
            assert_eq!(read_string(&*arr.add(2)), "three");
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
        drop_string(delim);
    }

    // ---- trim / trim_start / trim_end ----

    #[test]
    fn test_trim() {
        let s = make_string("  hello world  ");
        let result = __adam_string_trim(s.ptr, s.len, s.cap);
        unsafe {
            assert_eq!(read_string(&result), "hello world");
        }
        drop_string(s);
        drop_string(result);
    }

    #[test]
    fn test_trim_mixed_whitespace() {
        let s = make_string("\t\n  hello \r\n ");
        let result = __adam_string_trim(s.ptr, s.len, s.cap);
        unsafe {
            assert_eq!(read_string(&result), "hello");
        }
        drop_string(s);
        drop_string(result);
    }

    #[test]
    fn test_trim_start() {
        let s = make_string("  hello  ");
        let result = __adam_string_trim_start(s.ptr, s.len, s.cap);
        unsafe {
            assert_eq!(read_string(&result), "hello  ");
        }
        drop_string(s);
        drop_string(result);
    }

    #[test]
    fn test_trim_end() {
        let s = make_string("  hello  ");
        let result = __adam_string_trim_end(s.ptr, s.len, s.cap);
        unsafe {
            assert_eq!(read_string(&result), "  hello");
        }
        drop_string(s);
        drop_string(result);
    }

    #[test]
    fn test_trim_empty() {
        let s = AdamString::empty();
        let result = __adam_string_trim(s.ptr, s.len, s.cap);
        assert_eq!(result.len, 0);
    }

    // ---- to_upper / to_lower ----

    #[test]
    fn test_to_upper() {
        let s = make_string("hello World 123");
        let result = __adam_string_to_upper(s.ptr, s.len, s.cap);
        unsafe {
            assert_eq!(read_string(&result), "HELLO WORLD 123");
        }
        drop_string(s);
        drop_string(result);
    }

    #[test]
    fn test_to_lower() {
        let s = make_string("HELLO World 123");
        let result = __adam_string_to_lower(s.ptr, s.len, s.cap);
        unsafe {
            assert_eq!(read_string(&result), "hello world 123");
        }
        drop_string(s);
        drop_string(result);
    }

    #[test]
    fn test_to_upper_unicode() {
        // German sharp s uppercases to "SS"
        let s = make_string("stra\u{00DF}e");
        let result = __adam_string_to_upper(s.ptr, s.len, s.cap);
        unsafe {
            assert_eq!(read_string(&result), "STRASSE");
        }
        drop_string(s);
        drop_string(result);
    }

    // ---- replace ----

    #[test]
    fn test_replace() {
        let s = make_string("hello world");
        let from = make_string("world");
        let to = make_string("rust");
        let result = __adam_string_replace(
            s.ptr, s.len, s.cap, from.ptr, from.len, from.cap, to.ptr, to.len, to.cap,
        );
        unsafe {
            assert_eq!(read_string(&result), "hello rust");
        }
        drop_string(s);
        drop_string(from);
        drop_string(to);
        drop_string(result);
    }

    #[test]
    fn test_replace_multiple() {
        let s = make_string("aabaa");
        let from = make_string("a");
        let to = make_string("x");
        let result = __adam_string_replace(
            s.ptr, s.len, s.cap, from.ptr, from.len, from.cap, to.ptr, to.len, to.cap,
        );
        unsafe {
            assert_eq!(read_string(&result), "xxbxx");
        }
        drop_string(s);
        drop_string(from);
        drop_string(to);
        drop_string(result);
    }

    // ---- repeat ----

    #[test]
    fn test_repeat() {
        let s = make_string("ab");
        let result = __adam_string_repeat(s.ptr, s.len, s.cap, 3);
        unsafe {
            assert_eq!(read_string(&result), "ababab");
        }
        drop_string(s);
        drop_string(result);
    }

    #[test]
    fn test_repeat_zero() {
        let s = make_string("hello");
        let result = __adam_string_repeat(s.ptr, s.len, s.cap, 0);
        assert_eq!(result.len, 0);
        drop_string(s);
    }

    // ---- slice ----

    #[test]
    fn test_slice_basic() {
        let s = make_string("hello world");
        let result = __adam_string_slice(s.ptr, s.len, s.cap, 6, 11);
        unsafe {
            assert_eq!(read_string(&result), "world");
        }
        drop_string(s);
        drop_string(result);
    }

    #[test]
    fn test_slice_unicode() {
        // "a\u{00e9}b" = a(1 byte) + e-acute(2 bytes) + b(1 byte) = 4 bytes
        let s = make_string("a\u{00e9}b");
        let result = __adam_string_slice(s.ptr, s.len, s.cap, 1, 3);
        unsafe {
            assert_eq!(read_string(&result), "\u{00e9}");
        }
        drop_string(s);
        drop_string(result);
    }

    // NOTE: slice_invalid_boundary and slice_out_of_range tests removed
    // because panics in extern "C" functions abort rather than unwind,
    // making #[should_panic] tests impossible.

    // ---- push / push_char ----

    #[test]
    fn test_push_basic() {
        let mut s = make_string("hello");
        let suffix = make_string(" world");
        __adam_string_push(
            &mut s.ptr, &mut s.len, &mut s.cap, suffix.ptr, suffix.len, suffix.cap,
        );
        unsafe {
            assert_eq!(read_string(&s), "hello world");
        }
        drop_string(s);
        drop_string(suffix);
    }

    #[test]
    fn test_push_reallocation() {
        // Start with an empty string and push many times to force reallocation.
        let mut s = AdamString::empty();

        for i in 0..100u64 {
            let ch = make_string("x");
            __adam_string_push(&mut s.ptr, &mut s.len, &mut s.cap, ch.ptr, ch.len, ch.cap);
            drop_string(ch);
            assert_eq!(s.len, i + 1);
        }

        assert_eq!(s.len, 100);
        assert!(s.cap >= 100);
        unsafe {
            let result = read_string(&s);
            assert_eq!(result, "x".repeat(100));
        }
        drop_string(s);
    }

    #[test]
    fn test_push_char_ascii() {
        let mut s = make_string("hello");
        __adam_string_push_char(&mut s.ptr, &mut s.len, &mut s.cap, '!' as u32);
        unsafe {
            assert_eq!(read_string(&s), "hello!");
        }
        drop_string(s);
    }

    #[test]
    fn test_push_char_unicode() {
        let mut s = make_string("hi");
        // Push emoji U+1F600
        __adam_string_push_char(&mut s.ptr, &mut s.len, &mut s.cap, 0x1F600);
        unsafe {
            assert_eq!(read_string(&s), "hi\u{1F600}");
        }
        drop_string(s);
    }

    #[test]
    fn test_push_empty_suffix() {
        let mut s = make_string("hello");
        let empty = AdamString::empty();
        __adam_string_push(
            &mut s.ptr, &mut s.len, &mut s.cap, empty.ptr, empty.len, empty.cap,
        );
        unsafe {
            assert_eq!(read_string(&s), "hello");
        }
        drop_string(s);
    }

    // ---- eq / cmp ----

    #[test]
    fn test_eq_same() {
        let a = make_string("hello");
        let b = make_string("hello");
        assert!(__adam_string_eq(a.ptr, a.len, a.cap, b.ptr, b.len, b.cap,));
        drop_string(a);
        drop_string(b);
    }

    #[test]
    fn test_eq_different() {
        let a = make_string("hello");
        let b = make_string("world");
        assert!(!__adam_string_eq(a.ptr, a.len, a.cap, b.ptr, b.len, b.cap,));
        drop_string(a);
        drop_string(b);
    }

    #[test]
    fn test_eq_empty() {
        let a = AdamString::empty();
        let b = AdamString::empty();
        assert!(__adam_string_eq(a.ptr, a.len, a.cap, b.ptr, b.len, b.cap,));
    }

    #[test]
    fn test_eq_different_lengths() {
        let a = make_string("hi");
        let b = make_string("hello");
        assert!(!__adam_string_eq(a.ptr, a.len, a.cap, b.ptr, b.len, b.cap,));
        drop_string(a);
        drop_string(b);
    }

    #[test]
    fn test_cmp_equal() {
        let a = make_string("hello");
        let b = make_string("hello");
        assert_eq!(
            __adam_string_cmp(a.ptr, a.len, a.cap, b.ptr, b.len, b.cap,),
            0,
        );
        drop_string(a);
        drop_string(b);
    }

    #[test]
    fn test_cmp_less() {
        let a = make_string("abc");
        let b = make_string("abd");
        assert_eq!(
            __adam_string_cmp(a.ptr, a.len, a.cap, b.ptr, b.len, b.cap,),
            -1,
        );
        drop_string(a);
        drop_string(b);
    }

    #[test]
    fn test_cmp_greater() {
        let a = make_string("abd");
        let b = make_string("abc");
        assert_eq!(
            __adam_string_cmp(a.ptr, a.len, a.cap, b.ptr, b.len, b.cap,),
            1,
        );
        drop_string(a);
        drop_string(b);
    }

    #[test]
    fn test_cmp_prefix() {
        // "ab" < "abc" because it is a prefix.
        let a = make_string("ab");
        let b = make_string("abc");
        assert_eq!(
            __adam_string_cmp(a.ptr, a.len, a.cap, b.ptr, b.len, b.cap,),
            -1,
        );
        drop_string(a);
        drop_string(b);
    }

    // ---- hash ----

    #[test]
    fn test_hash_consistency() {
        let a = make_string("hello");
        let b = make_string("hello");
        let h1 = __adam_string_hash(a.ptr, a.len, a.cap);
        let h2 = __adam_string_hash(b.ptr, b.len, b.cap);
        assert_eq!(h1, h2);
        drop_string(a);
        drop_string(b);
    }

    #[test]
    fn test_hash_different_strings() {
        let a = make_string("hello");
        let b = make_string("world");
        let h1 = __adam_string_hash(a.ptr, a.len, a.cap);
        let h2 = __adam_string_hash(b.ptr, b.len, b.cap);
        assert_ne!(h1, h2);
        drop_string(a);
        drop_string(b);
    }

    #[test]
    fn test_hash_empty() {
        let s = AdamString::empty();
        // FNV-1a of empty input is the offset basis.
        let h = __adam_string_hash(s.ptr, s.len, s.cap);
        assert_eq!(h, 0xcbf29ce484222325);
    }

    // ---- clone ----

    #[test]
    fn test_clone() {
        let s = make_string("hello");
        let cloned = __adam_string_clone(s.ptr, s.len, s.cap);
        unsafe {
            assert_eq!(read_string(&cloned), "hello");
        }
        // Ensure it is a deep copy (different pointer).
        assert_ne!(s.ptr, cloned.ptr);
        drop_string(s);
        drop_string(cloned);
    }

    #[test]
    fn test_clone_empty() {
        let s = AdamString::empty();
        let cloned = __adam_string_clone(s.ptr, s.len, s.cap);
        assert_eq!(cloned.len, 0);
        assert!(cloned.ptr.is_null());
    }

    // ---- char_at ----

    #[test]
    fn test_char_at_ascii() {
        let s = make_string("hello");
        assert_eq!(__adam_string_char_at(s.ptr, s.len, s.cap, 0), 'h' as u32);
        assert_eq!(__adam_string_char_at(s.ptr, s.len, s.cap, 4), 'o' as u32);
        drop_string(s);
    }

    #[test]
    fn test_char_at_unicode() {
        // "a\u{00e9}" = a(1 byte) + e-acute(2 bytes). char_at(1) = e-acute.
        let s = make_string("a\u{00e9}");
        assert_eq!(__adam_string_char_at(s.ptr, s.len, s.cap, 0), 'a' as u32);
        assert_eq!(
            __adam_string_char_at(s.ptr, s.len, s.cap, 1),
            '\u{00e9}' as u32,
        );
        drop_string(s);
    }

    #[test]
    fn test_char_at_emoji() {
        let s = make_string("\u{1F600}");
        assert_eq!(__adam_string_char_at(s.ptr, s.len, s.cap, 0), 0x1F600);
        drop_string(s);
    }

    // NOTE: char_at_out_of_bounds and char_at_not_char_boundary tests removed
    // because panics in extern "C" functions abort rather than unwind,
    // making #[should_panic] tests impossible.

    // ---- CJK / additional unicode tests ----

    #[test]
    fn test_contains_cjk() {
        // "hello\u{4e16}\u{754c}" = "hello世界"
        let s = make_string("hello\u{4e16}\u{754c}");
        let sub = make_string("\u{4e16}\u{754c}");
        assert!(__adam_string_contains(
            s.ptr, s.len, s.cap, sub.ptr, sub.len, sub.cap,
        ));
        drop_string(s);
        drop_string(sub);
    }

    #[test]
    fn test_to_upper_cjk_unchanged() {
        // CJK characters have no uppercase; they should pass through unchanged.
        let s = make_string("\u{4e16}\u{754c}");
        let result = __adam_string_to_upper(s.ptr, s.len, s.cap);
        unsafe {
            assert_eq!(read_string(&result), "\u{4e16}\u{754c}");
        }
        drop_string(s);
        drop_string(result);
    }

    #[test]
    fn test_find_unicode() {
        // "a\u{1F600}b\u{1F600}c"
        let s = make_string("a\u{1F600}b\u{1F600}c");
        let sub = make_string("\u{1F600}c");
        let mut idx: u64 = 0;
        assert!(__adam_string_find(
            s.ptr, s.len, s.cap, sub.ptr, sub.len, sub.cap, &mut idx,
        ));
        // 'a'=1, first emoji=4, 'b'=1 => second emoji starts at byte 6
        assert_eq!(idx, 6);
        drop_string(s);
        drop_string(sub);
    }

    // ---- adversarial / edge-case tests ----

    // Helper to free a split result array.
    unsafe fn free_split_result(arr: *mut AdamString, count: u64) {
        if arr.is_null() || count == 0 {
            return;
        }
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

    // ==== 1. Unicode stress tests ====

    #[test]
    fn test_unicode_emoji_only_string() {
        // A string consisting entirely of 4-byte emoji codepoints.
        let s = make_string("\u{1F600}\u{1F601}\u{1F602}\u{1F603}\u{1F604}");
        // 5 emoji * 4 bytes each = 20 bytes
        assert_eq!(__adam_string_len(s.ptr, s.len, s.cap), 20);
        assert!(!__adam_string_is_empty(s.ptr, s.len, s.cap));
        // char_at byte 0 = first emoji
        assert_eq!(__adam_string_char_at(s.ptr, s.len, s.cap, 0), 0x1F600);
        // char_at byte 4 = second emoji
        assert_eq!(__adam_string_char_at(s.ptr, s.len, s.cap, 4), 0x1F601);
        // char_at byte 16 = fifth emoji
        assert_eq!(__adam_string_char_at(s.ptr, s.len, s.cap, 16), 0x1F604);
        drop_string(s);
    }

    #[test]
    fn test_unicode_mixed_scripts() {
        // Latin + CJK + Arabic + Emoji combined in one string.
        let s =
            make_string("Hello\u{4e16}\u{754c}\u{0645}\u{0631}\u{062D}\u{0628}\u{0627}\u{1F30D}");
        // Verify contains for each script segment.
        let latin = make_string("Hello");
        let cjk = make_string("\u{4e16}\u{754c}");
        let arabic = make_string("\u{0645}\u{0631}\u{062D}\u{0628}\u{0627}");
        let emoji = make_string("\u{1F30D}");
        assert!(__adam_string_contains(
            s.ptr, s.len, s.cap, latin.ptr, latin.len, latin.cap
        ));
        assert!(__adam_string_contains(
            s.ptr, s.len, s.cap, cjk.ptr, cjk.len, cjk.cap
        ));
        assert!(__adam_string_contains(
            s.ptr, s.len, s.cap, arabic.ptr, arabic.len, arabic.cap
        ));
        assert!(__adam_string_contains(
            s.ptr, s.len, s.cap, emoji.ptr, emoji.len, emoji.cap
        ));
        drop_string(s);
        drop_string(latin);
        drop_string(cjk);
        drop_string(arabic);
        drop_string(emoji);
    }

    #[test]
    fn test_unicode_zero_width_and_combining() {
        // Zero-width joiner (U+200D) and combining acute accent (U+0301).
        // "e" + combining acute = looks like e-acute but is two codepoints.
        let s = make_string("e\u{0301}");
        // 'e' is 1 byte, U+0301 is 2 bytes => 3 bytes total
        assert_eq!(__adam_string_len(s.ptr, s.len, s.cap), 3);
        assert_eq!(__adam_string_char_at(s.ptr, s.len, s.cap, 0), 'e' as u32);
        assert_eq!(__adam_string_char_at(s.ptr, s.len, s.cap, 1), 0x0301);
        // Zero-width joiner between two emoji
        let zwj = make_string("\u{1F468}\u{200D}\u{1F469}");
        // U+1F468 = 4 bytes, U+200D = 3 bytes, U+1F469 = 4 bytes => 11 bytes
        assert_eq!(__adam_string_len(zwj.ptr, zwj.len, zwj.cap), 11);
        drop_string(s);
        drop_string(zwj);
    }

    #[test]
    fn test_unicode_bom() {
        // UTF-8 BOM is U+FEFF (3 bytes: 0xEF 0xBB 0xBF).
        let s = make_string("\u{FEFF}Hello");
        // BOM is 3 bytes + "Hello" is 5 bytes = 8 bytes
        assert_eq!(__adam_string_len(s.ptr, s.len, s.cap), 8);
        // trim should NOT strip BOM (it is not whitespace)
        let trimmed = __adam_string_trim(s.ptr, s.len, s.cap);
        unsafe {
            assert_eq!(read_string(&trimmed), "\u{FEFF}Hello");
        }
        // starts_with BOM
        let bom = make_string("\u{FEFF}");
        assert!(__adam_string_starts_with(
            s.ptr, s.len, s.cap, bom.ptr, bom.len, bom.cap
        ));
        drop_string(s);
        drop_string(trimmed);
        drop_string(bom);
    }

    // ==== 2. Empty string edge cases ====

    #[test]
    fn test_contains_empty_empty() {
        // contains("", "") should be true.
        let s = AdamString::empty();
        let sub = AdamString::empty();
        assert!(__adam_string_contains(
            s.ptr, s.len, s.cap, sub.ptr, sub.len, sub.cap
        ));
    }

    #[test]
    fn test_find_on_empty_string() {
        let s = AdamString::empty();
        let sub = make_string("x");
        let mut idx: u64 = 999;
        assert!(!__adam_string_find(
            s.ptr, s.len, s.cap, sub.ptr, sub.len, sub.cap, &mut idx,
        ));
        assert_eq!(idx, 999); // unchanged
        drop_string(sub);
    }

    #[test]
    fn test_find_empty_needle_in_empty_string() {
        // Finding "" in "" should succeed at index 0.
        let s = AdamString::empty();
        let sub = AdamString::empty();
        let mut idx: u64 = 999;
        assert!(__adam_string_find(
            s.ptr, s.len, s.cap, sub.ptr, sub.len, sub.cap, &mut idx,
        ));
        assert_eq!(idx, 0);
    }

    #[test]
    fn test_split_empty_delimiter() {
        // Splitting by "" produces individual characters plus surrounding empties.
        let s = make_string("ab");
        let delim = AdamString::empty();
        let mut arr: *mut AdamString = ptr::null_mut();
        let mut count: u64 = 0;
        __adam_string_split(
            s.ptr, s.len, s.cap, delim.ptr, delim.len, delim.cap, &mut arr, &mut count,
        );
        // Rust's "ab".split("") yields ["", "a", "b", ""]
        assert_eq!(count, 4);
        unsafe {
            assert_eq!(read_string(&*arr.add(0)), "");
            assert_eq!(read_string(&*arr.add(1)), "a");
            assert_eq!(read_string(&*arr.add(2)), "b");
            assert_eq!(read_string(&*arr.add(3)), "");
            free_split_result(arr, count);
        }
        drop_string(s);
    }

    #[test]
    fn test_replace_empty_from() {
        // Replacing "" inserts `to` between every character and at both ends.
        let s = make_string("ab");
        let from = AdamString::empty();
        let to = make_string("-");
        let result = __adam_string_replace(
            s.ptr, s.len, s.cap, from.ptr, from.len, from.cap, to.ptr, to.len, to.cap,
        );
        unsafe {
            // Rust: "ab".replace("", "-") => "-a-b-"
            assert_eq!(read_string(&result), "-a-b-");
        }
        drop_string(s);
        drop_string(to);
        drop_string(result);
    }

    #[test]
    fn test_trim_whitespace_only() {
        let s = make_string("   \t\n\r  ");
        let result = __adam_string_trim(s.ptr, s.len, s.cap);
        unsafe {
            assert_eq!(read_string(&result), "");
        }
        assert_eq!(result.len, 0);
        drop_string(s);
    }

    // ==== 3. Large string stress ====

    #[test]
    fn test_large_string_contains_find_replace() {
        // Build a ~100KB string.
        let base = "abcdefghij".repeat(10_000); // 100,000 bytes
        let s = make_string(&base);
        assert_eq!(__adam_string_len(s.ptr, s.len, s.cap), 100_000);

        // contains at the very end
        let needle = make_string("ghij");
        assert!(__adam_string_contains(
            s.ptr, s.len, s.cap, needle.ptr, needle.len, needle.cap
        ));

        // find should locate first occurrence at byte 6
        let mut idx: u64 = 0;
        assert!(__adam_string_find(
            s.ptr, s.len, s.cap, needle.ptr, needle.len, needle.cap, &mut idx,
        ));
        assert_eq!(idx, 6);

        // replace a single character across the whole thing
        let from_ch = make_string("a");
        let to_ch = make_string("A");
        let replaced = __adam_string_replace(
            s.ptr,
            s.len,
            s.cap,
            from_ch.ptr,
            from_ch.len,
            from_ch.cap,
            to_ch.ptr,
            to_ch.len,
            to_ch.cap,
        );
        assert_eq!(replaced.len, 100_000);
        // The first byte should now be 'A'
        assert_eq!(
            __adam_string_char_at(replaced.ptr, replaced.len, replaced.cap, 0),
            'A' as u32
        );

        drop_string(s);
        drop_string(needle);
        drop_string(from_ch);
        drop_string(to_ch);
        drop_string(replaced);
    }

    #[test]
    fn test_push_10000_chars() {
        // Push 10,000 single characters one at a time to stress reallocation.
        let mut s = AdamString::empty();
        for _ in 0..10_000 {
            __adam_string_push_char(&mut s.ptr, &mut s.len, &mut s.cap, 'z' as u32);
        }
        assert_eq!(s.len, 10_000);
        assert!(s.cap >= 10_000);
        unsafe {
            let content = read_string(&s);
            assert_eq!(content.len(), 10_000);
            assert!(content.chars().all(|c| c == 'z'));
        }
        drop_string(s);
    }

    #[test]
    fn test_split_1000_delimiters() {
        // String with 1000 commas => 1001 parts.
        let base = "x,".repeat(1000); // "x,x,x,...x," (last part is empty)
        let s = make_string(&base);
        let delim = make_string(",");
        let mut arr: *mut AdamString = ptr::null_mut();
        let mut count: u64 = 0;
        __adam_string_split(
            s.ptr, s.len, s.cap, delim.ptr, delim.len, delim.cap, &mut arr, &mut count,
        );
        assert_eq!(count, 1001);
        unsafe {
            // First part should be "x"
            assert_eq!(read_string(&*arr.add(0)), "x");
            // Last part should be "" (trailing delimiter)
            assert_eq!(read_string(&*arr.add(1000)), "");
            free_split_result(arr, count);
        }
        drop_string(s);
        drop_string(delim);
    }

    // ==== 4. Boundary conditions ====

    #[test]
    fn test_slice_empty_range() {
        // slice(0, 0) should yield an empty string.
        let s = make_string("hello");
        let result = __adam_string_slice(s.ptr, s.len, s.cap, 0, 0);
        assert_eq!(result.len, 0);
        drop_string(s);
    }

    #[test]
    fn test_slice_full_range() {
        // slice(0, len) should yield the entire string.
        let s = make_string("hello");
        let result = __adam_string_slice(s.ptr, s.len, s.cap, 0, s.len);
        unsafe {
            assert_eq!(read_string(&result), "hello");
        }
        drop_string(s);
        drop_string(result);
    }

    #[test]
    fn test_find_at_very_start() {
        let s = make_string("hello");
        let sub = make_string("hel");
        let mut idx: u64 = 999;
        assert!(__adam_string_find(
            s.ptr, s.len, s.cap, sub.ptr, sub.len, sub.cap, &mut idx,
        ));
        assert_eq!(idx, 0);
        drop_string(s);
        drop_string(sub);
    }

    #[test]
    fn test_find_at_very_end() {
        let s = make_string("hello");
        let sub = make_string("llo");
        let mut idx: u64 = 0;
        assert!(__adam_string_find(
            s.ptr, s.len, s.cap, sub.ptr, sub.len, sub.cap, &mut idx,
        ));
        assert_eq!(idx, 2);
        drop_string(s);
        drop_string(sub);
    }

    #[test]
    fn test_rfind_vs_find_multiple_occurrences() {
        let s = make_string("abXcdXefXgh");
        let sub = make_string("X");
        let mut find_idx: u64 = 0;
        let mut rfind_idx: u64 = 0;
        assert!(__adam_string_find(
            s.ptr,
            s.len,
            s.cap,
            sub.ptr,
            sub.len,
            sub.cap,
            &mut find_idx,
        ));
        assert!(__adam_string_rfind(
            s.ptr,
            s.len,
            s.cap,
            sub.ptr,
            sub.len,
            sub.cap,
            &mut rfind_idx,
        ));
        assert_eq!(find_idx, 2); // first X
        assert_eq!(rfind_idx, 8); // last X
        assert!(rfind_idx > find_idx);
        drop_string(s);
        drop_string(sub);
    }

    #[test]
    fn test_contains_needle_equals_haystack() {
        let s = make_string("hello");
        let sub = make_string("hello");
        assert!(__adam_string_contains(
            s.ptr, s.len, s.cap, sub.ptr, sub.len, sub.cap
        ));
        drop_string(s);
        drop_string(sub);
    }

    #[test]
    fn test_contains_needle_longer_than_haystack() {
        let s = make_string("hi");
        let sub = make_string("hello world");
        assert!(!__adam_string_contains(
            s.ptr, s.len, s.cap, sub.ptr, sub.len, sub.cap
        ));
        drop_string(s);
        drop_string(sub);
    }

    // ==== 5. Hash edge cases ====

    #[test]
    fn test_hash_different_single_chars() {
        // Hash of "a" and "b" should differ.
        let a = make_string("a");
        let b = make_string("b");
        let ha = __adam_string_hash(a.ptr, a.len, a.cap);
        let hb = __adam_string_hash(b.ptr, b.len, b.cap);
        assert_ne!(ha, hb);
        drop_string(a);
        drop_string(b);
    }

    #[test]
    fn test_hash_stability_across_calls() {
        // Calling hash multiple times on the same string should always return the same value.
        let s = make_string("stability");
        let h1 = __adam_string_hash(s.ptr, s.len, s.cap);
        let h2 = __adam_string_hash(s.ptr, s.len, s.cap);
        let h3 = __adam_string_hash(s.ptr, s.len, s.cap);
        assert_eq!(h1, h2);
        assert_eq!(h2, h3);
        drop_string(s);
    }

    #[test]
    fn test_hash_many_different_strings() {
        // Generate many short strings and verify hashes are mostly unique.
        let mut hashes = std::collections::HashSet::new();
        let mut strings = Vec::new();
        for i in 0..100u32 {
            let text = format!("str_{}", i);
            let s = make_string(&text);
            let h = __adam_string_hash(s.ptr, s.len, s.cap);
            hashes.insert(h);
            strings.push(s);
        }
        // With FNV-1a and 100 distinct inputs, collisions are extremely unlikely.
        assert_eq!(hashes.len(), 100);
        for s in strings {
            drop_string(s);
        }
    }

    #[test]
    fn test_hash_similar_strings_differ() {
        // Strings that differ by one byte should have different hashes.
        let a = make_string("abc");
        let b = make_string("abd");
        let ha = __adam_string_hash(a.ptr, a.len, a.cap);
        let hb = __adam_string_hash(b.ptr, b.len, b.cap);
        assert_ne!(ha, hb);
        drop_string(a);
        drop_string(b);
    }

    // ==== 6. Comparison edge cases ====

    #[test]
    fn test_cmp_empty_vs_nonempty() {
        let empty = AdamString::empty();
        let nonempty = make_string("a");
        // "" < "a"
        assert_eq!(
            __adam_string_cmp(
                empty.ptr,
                empty.len,
                empty.cap,
                nonempty.ptr,
                nonempty.len,
                nonempty.cap,
            ),
            -1,
        );
        // "a" > ""
        assert_eq!(
            __adam_string_cmp(
                nonempty.ptr,
                nonempty.len,
                nonempty.cap,
                empty.ptr,
                empty.len,
                empty.cap,
            ),
            1,
        );
        drop_string(nonempty);
    }

    #[test]
    fn test_cmp_differ_only_in_last_byte() {
        let a = make_string("hellx");
        let b = make_string("helly");
        assert_eq!(
            __adam_string_cmp(a.ptr, a.len, a.cap, b.ptr, b.len, b.cap,),
            -1,
        );
        assert_eq!(
            __adam_string_cmp(b.ptr, b.len, b.cap, a.ptr, a.len, a.cap,),
            1,
        );
        drop_string(a);
        drop_string(b);
    }

    #[test]
    fn test_eq_same_content_different_cap() {
        // Build two strings with the same logical content but different capacities.
        // String a: push to grow capacity beyond len.
        let mut a = make_string("hel");
        let suffix = make_string("lo");
        __adam_string_push(
            &mut a.ptr, &mut a.len, &mut a.cap, suffix.ptr, suffix.len, suffix.cap,
        );
        // String b: exact allocation.
        let b = make_string("hello");
        // a.cap might be > 5 due to growth, b.cap == 5
        assert!(__adam_string_eq(a.ptr, a.len, a.cap, b.ptr, b.len, b.cap));
        drop_string(a);
        drop_string(b);
        drop_string(suffix);
    }

    #[test]
    fn test_cmp_empty_vs_empty() {
        let a = AdamString::empty();
        let b = AdamString::empty();
        assert_eq!(
            __adam_string_cmp(a.ptr, a.len, a.cap, b.ptr, b.len, b.cap),
            0,
        );
    }

    // ==== 7. Clone independence ====

    #[test]
    fn test_clone_independence_after_push() {
        let original = make_string("hello");
        let cloned = __adam_string_clone(original.ptr, original.len, original.cap);

        // Now mutate the original by pushing more data.
        let mut orig_ptr = original.ptr;
        let mut orig_len = original.len;
        let mut orig_cap = original.cap;
        let extra = make_string(" world!!!");
        __adam_string_push(
            &mut orig_ptr,
            &mut orig_len,
            &mut orig_cap,
            extra.ptr,
            extra.len,
            extra.cap,
        );

        unsafe {
            // Clone should still be "hello", unaffected.
            assert_eq!(read_string(&cloned), "hello");
            // Original (now via raw fields) should be "hello world!!!".
            let orig_adam = AdamString {
                ptr: orig_ptr,
                len: orig_len,
                cap: orig_cap,
            };
            assert_eq!(read_string(&orig_adam), "hello world!!!");
            __adam_string_drop(orig_adam.ptr, orig_adam.len, orig_adam.cap);
        }
        drop_string(cloned);
        drop_string(extra);
    }

    #[test]
    fn test_clone_independence_after_push_char() {
        let original = make_string("abc");
        let cloned = __adam_string_clone(original.ptr, original.len, original.cap);

        let mut orig_ptr = original.ptr;
        let mut orig_len = original.len;
        let mut orig_cap = original.cap;
        __adam_string_push_char(&mut orig_ptr, &mut orig_len, &mut orig_cap, 'Z' as u32);

        unsafe {
            assert_eq!(read_string(&cloned), "abc");
            let orig_adam = AdamString {
                ptr: orig_ptr,
                len: orig_len,
                cap: orig_cap,
            };
            assert_eq!(read_string(&orig_adam), "abcZ");
            __adam_string_drop(orig_adam.ptr, orig_adam.len, orig_adam.cap);
        }
        drop_string(cloned);
    }

    // ==== Additional adversarial tests ====

    #[test]
    fn test_replace_no_match() {
        let s = make_string("hello");
        let from = make_string("xyz");
        let to = make_string("!");
        let result = __adam_string_replace(
            s.ptr, s.len, s.cap, from.ptr, from.len, from.cap, to.ptr, to.len, to.cap,
        );
        unsafe {
            assert_eq!(read_string(&result), "hello");
        }
        drop_string(s);
        drop_string(from);
        drop_string(to);
        drop_string(result);
    }

    #[test]
    fn test_replace_to_empty() {
        // Replace with empty string effectively deletes occurrences.
        let s = make_string("aXbXc");
        let from = make_string("X");
        let to = AdamString::empty();
        let result = __adam_string_replace(
            s.ptr, s.len, s.cap, from.ptr, from.len, from.cap, to.ptr, to.len, to.cap,
        );
        unsafe {
            assert_eq!(read_string(&result), "abc");
        }
        drop_string(s);
        drop_string(from);
        drop_string(result);
    }

    #[test]
    fn test_repeat_one() {
        let s = make_string("hello");
        let result = __adam_string_repeat(s.ptr, s.len, s.cap, 1);
        unsafe {
            assert_eq!(read_string(&result), "hello");
        }
        drop_string(s);
        drop_string(result);
    }

    #[test]
    fn test_concat_two_empty() {
        let a = AdamString::empty();
        let b = AdamString::empty();
        let result = __str_concat(a.ptr, a.len, a.cap, b.ptr, b.len, b.cap);
        assert_eq!(result.len, 0);
        assert!(result.ptr.is_null());
    }

    #[test]
    fn test_concat_empty_plus_nonempty() {
        let a = AdamString::empty();
        let b = make_string("world");
        let result = __str_concat(a.ptr, a.len, a.cap, b.ptr, b.len, b.cap);
        unsafe {
            assert_eq!(read_string(&result), "world");
        }
        drop_string(b);
        drop_string(result);
    }

    #[test]
    fn test_to_lower_unicode_turkish_i() {
        // Turkish capital I-with-dot-above (U+0130) lowercases to "i\u{0307}" in non-Turkish locale,
        // but we just verify it doesn't crash and produces a lowercase result.
        let s = make_string("\u{0130}");
        let result = __adam_string_to_lower(s.ptr, s.len, s.cap);
        // Result should not be empty and should differ from input.
        assert!(result.len > 0);
        drop_string(s);
        drop_string(result);
    }

    #[test]
    fn test_push_multibyte_chars_reallocation() {
        // Push 500 4-byte emoji characters to stress multibyte reallocation.
        let mut s = AdamString::empty();
        for _ in 0..500 {
            __adam_string_push_char(&mut s.ptr, &mut s.len, &mut s.cap, 0x1F600);
        }
        // 500 * 4 bytes = 2000 bytes
        assert_eq!(s.len, 2000);
        assert!(s.cap >= 2000);
        // Verify first and last character
        assert_eq!(__adam_string_char_at(s.ptr, s.len, s.cap, 0), 0x1F600);
        assert_eq!(__adam_string_char_at(s.ptr, s.len, s.cap, 1996), 0x1F600);
        drop_string(s);
    }

    #[test]
    fn test_rfind_single_occurrence() {
        // When there is only one occurrence, find and rfind should agree.
        let s = make_string("abcdef");
        let sub = make_string("cd");
        let mut find_idx: u64 = 0;
        let mut rfind_idx: u64 = 0;
        assert!(__adam_string_find(
            s.ptr,
            s.len,
            s.cap,
            sub.ptr,
            sub.len,
            sub.cap,
            &mut find_idx,
        ));
        assert!(__adam_string_rfind(
            s.ptr,
            s.len,
            s.cap,
            sub.ptr,
            sub.len,
            sub.cap,
            &mut rfind_idx,
        ));
        assert_eq!(find_idx, rfind_idx);
        assert_eq!(find_idx, 2);
        drop_string(s);
        drop_string(sub);
    }

    #[test]
    fn test_slice_middle_of_unicode() {
        // Slice that cuts exactly around a multibyte character.
        // "a\u{00e9}b" = [0x61, 0xC3, 0xA9, 0x62]
        // Slice [0,1) = "a", slice [1,3) = "\u{00e9}", slice [3,4) = "b"
        let s = make_string("a\u{00e9}b");
        let r1 = __adam_string_slice(s.ptr, s.len, s.cap, 0, 1);
        let r2 = __adam_string_slice(s.ptr, s.len, s.cap, 1, 3);
        let r3 = __adam_string_slice(s.ptr, s.len, s.cap, 3, 4);
        unsafe {
            assert_eq!(read_string(&r1), "a");
            assert_eq!(read_string(&r2), "\u{00e9}");
            assert_eq!(read_string(&r3), "b");
        }
        drop_string(s);
        drop_string(r1);
        drop_string(r2);
        drop_string(r3);
    }

    #[test]
    fn test_int_to_string_negative() {
        let s = __int_to_string(-42);
        unsafe {
            assert_eq!(read_string(&s), "-42");
        }
        drop_string(s);
    }

    #[test]
    fn test_int_to_string_zero() {
        let s = __int_to_string(0);
        unsafe {
            assert_eq!(read_string(&s), "0");
        }
        drop_string(s);
    }

    #[test]
    fn test_float_to_string_nan_inf() {
        let nan_s = __float_to_string(f64::NAN);
        let inf_s = __float_to_string(f64::INFINITY);
        let neg_inf_s = __float_to_string(f64::NEG_INFINITY);
        unsafe {
            assert_eq!(read_string(&nan_s), "NaN");
            assert_eq!(read_string(&inf_s), "inf");
            assert_eq!(read_string(&neg_inf_s), "-inf");
        }
        drop_string(nan_s);
        drop_string(inf_s);
        drop_string(neg_inf_s);
    }

    #[test]
    fn test_char_to_string_replacement_for_invalid() {
        // An invalid Unicode scalar value should produce the replacement character.
        let s = __char_to_string(0xD800); // surrogate, invalid
        unsafe {
            assert_eq!(read_string(&s), "\u{FFFD}");
        }
        drop_string(s);
    }

    #[test]
    fn test_from_cstr_null() {
        let s = __adam_string_from_cstr(ptr::null());
        assert_eq!(s.len, 0);
        assert!(s.ptr.is_null());
    }
}
