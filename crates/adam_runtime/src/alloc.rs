use std::alloc::{alloc, dealloc, Layout};

/// Allocate `size` bytes with the given alignment. Returns a pointer.
#[no_mangle]
pub extern "C" fn __adam_alloc(size: u64, align: u64) -> *mut u8 {
    if size == 0 {
        return align as *mut u8; // Non-null dangling pointer for ZSTs
    }
    unsafe {
        let layout = Layout::from_size_align_unchecked(size as usize, align as usize);
        let ptr = alloc(layout);
        if ptr.is_null() {
            std::process::abort();
        }
        ptr
    }
}

/// Deallocate a pointer previously allocated with `__adam_alloc`.
#[no_mangle]
pub extern "C" fn __adam_dealloc(ptr: *mut u8, size: u64, align: u64) {
    if size == 0 || ptr.is_null() {
        return;
    }
    unsafe {
        let layout = Layout::from_size_align_unchecked(size as usize, align as usize);
        dealloc(ptr, layout);
    }
}

/// Reallocate — grow or shrink an allocation.
#[no_mangle]
pub extern "C" fn __adam_realloc(
    ptr: *mut u8,
    old_size: u64,
    align: u64,
    new_size: u64,
) -> *mut u8 {
    if old_size == 0 {
        return __adam_alloc(new_size, align);
    }
    if new_size == 0 {
        __adam_dealloc(ptr, old_size, align);
        return align as *mut u8;
    }
    unsafe {
        let layout = Layout::from_size_align_unchecked(old_size as usize, align as usize);
        let new_ptr = std::alloc::realloc(ptr, layout, new_size as usize);
        if new_ptr.is_null() {
            std::process::abort();
        }
        new_ptr
    }
}
