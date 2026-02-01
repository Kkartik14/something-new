/// Print a string (ptr + len) to stdout, no newline.
#[no_mangle]
pub extern "C" fn __adam_print_str(ptr: *const u8, len: u64) {
    if ptr.is_null() || len == 0 {
        return;
    }
    unsafe {
        let bytes = std::slice::from_raw_parts(ptr, len as usize);
        // Use write() syscall directly for no-buffering guarantees.
        #[cfg(unix)]
        {
            libc_write(1, bytes.as_ptr(), bytes.len());
        }
        #[cfg(not(unix))]
        {
            use std::io::Write;
            let _ = std::io::stdout().write_all(bytes);
        }
    }
}

/// Print a null-terminated C string to stdout, no newline.
#[no_mangle]
pub extern "C" fn __adam_print_cstr(ptr: *const u8) {
    if ptr.is_null() {
        return;
    }
    unsafe {
        let mut p = ptr;
        let start = p;
        while *p != 0 {
            p = p.add(1);
        }
        let len = p as usize - start as usize;
        if len > 0 {
            __adam_print_str(start, len as u64);
        }
    }
}

/// Print a string followed by a newline.
#[no_mangle]
pub extern "C" fn __adam_println_str(ptr: *const u8, len: u64) {
    __adam_print_str(ptr, len);
    __adam_print_str(b"\n".as_ptr(), 1);
}

/// Print a null-terminated C string followed by a newline.
#[no_mangle]
pub extern "C" fn __adam_println_cstr(ptr: *const u8) {
    __adam_print_cstr(ptr);
    __adam_print_str(b"\n".as_ptr(), 1);
}

/// Print an i64.
#[no_mangle]
pub extern "C" fn __adam_print_int(val: i64) {
    let s = val.to_string();
    __adam_print_str(s.as_ptr(), s.len() as u64);
}

/// Print an f64.
#[no_mangle]
pub extern "C" fn __adam_print_float(val: f64) {
    let s = val.to_string();
    __adam_print_str(s.as_ptr(), s.len() as u64);
}

/// Print a bool.
#[no_mangle]
pub extern "C" fn __adam_print_bool(val: i8) {
    let s = if val != 0 { "true" } else { "false" };
    __adam_print_str(s.as_ptr(), s.len() as u64);
}

/// The Adam `print()` function — accepts a C string pointer.
/// This is the simplest variant used for string constant arguments.
#[no_mangle]
pub extern "C" fn print(ptr: *const u8) {
    __adam_println_cstr(ptr);
}

/// Unix write syscall wrapper.
#[cfg(unix)]
unsafe fn libc_write(fd: i32, buf: *const u8, count: usize) -> isize {
    extern "C" {
        fn write(fd: i32, buf: *const u8, count: usize) -> isize;
    }
    write(fd, buf, count)
}
