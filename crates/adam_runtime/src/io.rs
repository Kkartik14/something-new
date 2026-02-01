//! I/O runtime functions: stdio, file-system helpers, and file-handle operations.

use crate::string::AdamString;
use std::alloc::Layout;
use std::fs;
use std::io::{self, BufRead, BufReader, Read, Write};
use std::os::raw::c_void;
use std::ptr;
use std::slice;

// ================================================================
// Result / error types
// ================================================================

/// Result struct returned by I/O functions that can fail.
#[repr(C)]
pub struct AdamIoResult {
    /// `true` = success, `false` = error (or EOF where documented).
    pub success: bool,
    /// OS error code (0 when success, 0 for EOF).
    pub error_code: i32,
    /// Human-readable error message (empty when success).
    pub error_msg: AdamString,
}

/// Opaque file handle exposed to Adam code.
#[repr(C)]
pub struct AdamFile {
    /// Points to a `Box<BufReader<fs::File>>` (read) **or** `Box<fs::File>` (write).
    /// The tag is encoded in the low bit: 0 = BufReader, 1 = raw File.
    handle: *mut c_void,
}

// We tag the pointer so we know whether to treat the handle as a
// BufReader<File> (opened for reading) or a raw File (opened for writing).
const TAG_RAW_FILE: usize = 1;

fn tag_bufreader(br: Box<BufReader<fs::File>>) -> *mut c_void {
    Box::into_raw(br) as *mut c_void // low bit is 0 (aligned)
}

fn tag_raw_file(f: Box<fs::File>) -> *mut c_void {
    let ptr = Box::into_raw(f) as usize;
    (ptr | TAG_RAW_FILE) as *mut c_void
}

fn is_raw_file(handle: *mut c_void) -> bool {
    (handle as usize) & TAG_RAW_FILE != 0
}

unsafe fn as_bufreader<'a>(handle: *mut c_void) -> &'a mut BufReader<fs::File> {
    &mut *(handle as *mut BufReader<fs::File>)
}

unsafe fn as_raw_file<'a>(handle: *mut c_void) -> &'a mut fs::File {
    let ptr = ((handle as usize) & !TAG_RAW_FILE) as *mut fs::File;
    &mut *ptr
}

// ================================================================
// Helpers
// ================================================================

fn io_ok() -> AdamIoResult {
    AdamIoResult {
        success: true,
        error_code: 0,
        error_msg: AdamString::from_bytes(b""),
    }
}

fn io_err(e: io::Error) -> AdamIoResult {
    let code = e.raw_os_error().unwrap_or(-1);
    let msg = e.to_string();
    AdamIoResult {
        success: false,
        error_code: code,
        error_msg: AdamString::from_bytes(msg.as_bytes()),
    }
}

/// Reconstruct a `&str` from the raw (ptr, len) pair that Adam passes in.
unsafe fn path_str<'a>(ptr: *const u8, len: u64) -> &'a str {
    std::str::from_utf8_unchecked(slice::from_raw_parts(ptr, len as usize))
}

// ================================================================
// Stdio functions
// ================================================================

/// Print bytes to **stderr**, no trailing newline.
#[no_mangle]
pub extern "C" fn __adam_eprint_str(ptr: *const u8, len: u64) {
    if ptr.is_null() || len == 0 {
        return;
    }
    unsafe {
        let bytes = slice::from_raw_parts(ptr, len as usize);
        let _ = io::stderr().write_all(bytes);
    }
}

/// Print bytes to **stderr** followed by a newline.
#[no_mangle]
pub extern "C" fn __adam_eprintln_str(ptr: *const u8, len: u64) {
    __adam_eprint_str(ptr, len);
    let _ = io::stderr().write_all(b"\n");
}

/// Read one line from **stdin** (including the trailing `\n`, if any).
/// The line is stored in `*out`.
#[no_mangle]
pub extern "C" fn __adam_read_line(out: *mut AdamString) -> AdamIoResult {
    if out.is_null() {
        return io_err(io::Error::new(io::ErrorKind::InvalidInput, "null output pointer"));
    }
    let mut buf = String::new();
    match io::stdin().read_line(&mut buf) {
        Ok(_) => {
            unsafe {
                ptr::write(out, AdamString::from_bytes(buf.as_bytes()));
            }
            io_ok()
        }
        Err(e) => {
            unsafe {
                ptr::write(out, AdamString::from_bytes(b""));
            }
            io_err(e)
        }
    }
}

// ================================================================
// File-system convenience functions
// ================================================================

/// Read the entire contents of a file into an `AdamString`.
#[no_mangle]
pub extern "C" fn __adam_fs_read(
    path_ptr: *const u8,
    path_len: u64,
    out: *mut AdamString,
) -> AdamIoResult {
    unsafe {
        let path = path_str(path_ptr, path_len);
        match fs::read_to_string(path) {
            Ok(contents) => {
                ptr::write(out, AdamString::from_bytes(contents.as_bytes()));
                io_ok()
            }
            Err(e) => {
                ptr::write(out, AdamString::from_bytes(b""));
                io_err(e)
            }
        }
    }
}

/// Read the entire contents of a file as raw bytes.
///
/// The caller receives a pointer (`*out_ptr`) and length (`*out_len`).
/// The memory is allocated with `std::alloc::alloc` (alignment 1) and the
/// caller is responsible for freeing it.
#[no_mangle]
pub extern "C" fn __adam_fs_read_bytes(
    path_ptr: *const u8,
    path_len: u64,
    out_ptr: *mut *mut u8,
    out_len: *mut u64,
) -> AdamIoResult {
    unsafe {
        let path = path_str(path_ptr, path_len);
        match fs::read(path) {
            Ok(bytes) => {
                if bytes.is_empty() {
                    *out_ptr = ptr::null_mut();
                    *out_len = 0;
                } else {
                    let layout = Layout::from_size_align_unchecked(bytes.len(), 1);
                    let buf = std::alloc::alloc(layout);
                    if buf.is_null() {
                        std::process::abort();
                    }
                    ptr::copy_nonoverlapping(bytes.as_ptr(), buf, bytes.len());
                    *out_ptr = buf;
                    *out_len = bytes.len() as u64;
                }
                io_ok()
            }
            Err(e) => {
                *out_ptr = ptr::null_mut();
                *out_len = 0;
                io_err(e)
            }
        }
    }
}

/// Write a string to a file (create or truncate).
#[no_mangle]
pub extern "C" fn __adam_fs_write(
    path_ptr: *const u8,
    path_len: u64,
    content_ptr: *const u8,
    content_len: u64,
) -> AdamIoResult {
    unsafe {
        let path = path_str(path_ptr, path_len);
        let data = slice::from_raw_parts(content_ptr, content_len as usize);
        match fs::write(path, data) {
            Ok(()) => io_ok(),
            Err(e) => io_err(e),
        }
    }
}

/// Write raw bytes to a file (create or truncate).
#[no_mangle]
pub extern "C" fn __adam_fs_write_bytes(
    path_ptr: *const u8,
    path_len: u64,
    content_ptr: *const u8,
    content_len: u64,
) -> AdamIoResult {
    // Identical to __adam_fs_write — both write raw bytes.
    __adam_fs_write(path_ptr, path_len, content_ptr, content_len)
}

/// Append data to a file.
#[no_mangle]
pub extern "C" fn __adam_fs_append(
    path_ptr: *const u8,
    path_len: u64,
    content_ptr: *const u8,
    content_len: u64,
) -> AdamIoResult {
    unsafe {
        let path = path_str(path_ptr, path_len);
        let data = slice::from_raw_parts(content_ptr, content_len as usize);
        let file = fs::OpenOptions::new().create(true).append(true).open(path);
        match file {
            Ok(mut f) => match f.write_all(data) {
                Ok(()) => io_ok(),
                Err(e) => io_err(e),
            },
            Err(e) => io_err(e),
        }
    }
}

/// Check whether a path (file or directory) exists.
#[no_mangle]
pub extern "C" fn __adam_fs_exists(path_ptr: *const u8, path_len: u64) -> bool {
    unsafe {
        let path = path_str(path_ptr, path_len);
        std::path::Path::new(path).exists()
    }
}

/// Remove a file.
#[no_mangle]
pub extern "C" fn __adam_fs_remove(path_ptr: *const u8, path_len: u64) -> AdamIoResult {
    unsafe {
        let path = path_str(path_ptr, path_len);
        match fs::remove_file(path) {
            Ok(()) => io_ok(),
            Err(e) => io_err(e),
        }
    }
}

/// Create a directory and all missing parent directories.
#[no_mangle]
pub extern "C" fn __adam_fs_mkdir(path_ptr: *const u8, path_len: u64) -> AdamIoResult {
    unsafe {
        let path = path_str(path_ptr, path_len);
        match fs::create_dir_all(path) {
            Ok(()) => io_ok(),
            Err(e) => io_err(e),
        }
    }
}

/// List the entries of a directory.
///
/// On success the callee allocates an array of `AdamString` via
/// `std::alloc::alloc` and writes the pointer into `*out_arr` and the
/// count into `*out_count`.  The caller is responsible for freeing both
/// the individual strings **and** the array itself.
#[no_mangle]
pub extern "C" fn __adam_fs_list_dir(
    path_ptr: *const u8,
    path_len: u64,
    out_arr: *mut *mut AdamString,
    out_count: *mut u64,
) -> AdamIoResult {
    unsafe {
        let path = path_str(path_ptr, path_len);
        let entries: Result<Vec<_>, _> = fs::read_dir(path)
            .map(|rd| rd.filter_map(|e| e.ok()).collect());
        match entries {
            Ok(entries) => {
                let count = entries.len();
                if count == 0 {
                    *out_arr = ptr::null_mut();
                    *out_count = 0;
                    return io_ok();
                }
                let layout = Layout::from_size_align_unchecked(
                    count * std::mem::size_of::<AdamString>(),
                    std::mem::align_of::<AdamString>(),
                );
                let arr = std::alloc::alloc(layout) as *mut AdamString;
                if arr.is_null() {
                    std::process::abort();
                }
                for (i, entry) in entries.iter().enumerate() {
                    let name = entry.file_name();
                    let name_bytes = name.to_string_lossy();
                    ptr::write(arr.add(i), AdamString::from_bytes(name_bytes.as_bytes()));
                }
                *out_arr = arr;
                *out_count = count as u64;
                io_ok()
            }
            Err(e) => {
                *out_arr = ptr::null_mut();
                *out_count = 0;
                io_err(e)
            }
        }
    }
}

/// Return `true` if the path is a regular file.
#[no_mangle]
pub extern "C" fn __adam_fs_is_file(path_ptr: *const u8, path_len: u64) -> bool {
    unsafe {
        let path = path_str(path_ptr, path_len);
        std::path::Path::new(path).is_file()
    }
}

/// Return `true` if the path is a directory.
#[no_mangle]
pub extern "C" fn __adam_fs_is_dir(path_ptr: *const u8, path_len: u64) -> bool {
    unsafe {
        let path = path_str(path_ptr, path_len);
        std::path::Path::new(path).is_dir()
    }
}

/// Get the size of a file in bytes.
#[no_mangle]
pub extern "C" fn __adam_fs_file_size(
    path_ptr: *const u8,
    path_len: u64,
    out: *mut u64,
) -> AdamIoResult {
    unsafe {
        let path = path_str(path_ptr, path_len);
        match fs::metadata(path) {
            Ok(meta) => {
                *out = meta.len();
                io_ok()
            }
            Err(e) => {
                *out = 0;
                io_err(e)
            }
        }
    }
}

// ================================================================
// File-handle functions
// ================================================================

/// Open an existing file for **reading**.  The handle wraps a
/// `BufReader<File>` so that `file_read_line` is efficient.
#[no_mangle]
pub extern "C" fn __adam_file_open(
    path_ptr: *const u8,
    path_len: u64,
    file_out: *mut AdamFile,
) -> AdamIoResult {
    unsafe {
        let path = path_str(path_ptr, path_len);
        match fs::File::open(path) {
            Ok(f) => {
                let br = Box::new(BufReader::new(f));
                ptr::write(
                    file_out,
                    AdamFile {
                        handle: tag_bufreader(br),
                    },
                );
                io_ok()
            }
            Err(e) => {
                ptr::write(
                    file_out,
                    AdamFile {
                        handle: ptr::null_mut(),
                    },
                );
                io_err(e)
            }
        }
    }
}

/// Create (or truncate) a file for **writing**.
#[no_mangle]
pub extern "C" fn __adam_file_create(
    path_ptr: *const u8,
    path_len: u64,
    file_out: *mut AdamFile,
) -> AdamIoResult {
    unsafe {
        let path = path_str(path_ptr, path_len);
        match fs::File::create(path) {
            Ok(f) => {
                let boxed = Box::new(f);
                ptr::write(
                    file_out,
                    AdamFile {
                        handle: tag_raw_file(boxed),
                    },
                );
                io_ok()
            }
            Err(e) => {
                ptr::write(
                    file_out,
                    AdamFile {
                        handle: ptr::null_mut(),
                    },
                );
                io_err(e)
            }
        }
    }
}

/// Read the **entire** remaining contents of a file handle into an `AdamString`.
#[no_mangle]
pub extern "C" fn __adam_file_read_all(
    file: *mut AdamFile,
    out: *mut AdamString,
) -> AdamIoResult {
    unsafe {
        if file.is_null() || (*file).handle.is_null() {
            ptr::write(out, AdamString::from_bytes(b""));
            return io_err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "invalid file handle",
            ));
        }
        let mut buf = String::new();
        let result = if is_raw_file((*file).handle) {
            as_raw_file((*file).handle).read_to_string(&mut buf)
        } else {
            as_bufreader((*file).handle).read_to_string(&mut buf)
        };
        match result {
            Ok(_) => {
                ptr::write(out, AdamString::from_bytes(buf.as_bytes()));
                io_ok()
            }
            Err(e) => {
                ptr::write(out, AdamString::from_bytes(b""));
                io_err(e)
            }
        }
    }
}

/// Read the next line from a file handle.
///
/// Returns `success = false, error_code = 0` at **EOF** (not a real error).
/// Returns `success = false, error_code != 0` on actual I/O errors.
#[no_mangle]
pub extern "C" fn __adam_file_read_line(
    file: *mut AdamFile,
    out: *mut AdamString,
) -> AdamIoResult {
    unsafe {
        if file.is_null() || (*file).handle.is_null() {
            ptr::write(out, AdamString::from_bytes(b""));
            return io_err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "invalid file handle",
            ));
        }
        if is_raw_file((*file).handle) {
            // Cannot efficiently read lines from a non-buffered handle.
            ptr::write(out, AdamString::from_bytes(b""));
            return io_err(io::Error::new(
                io::ErrorKind::Unsupported,
                "file was opened for writing, not reading",
            ));
        }
        let reader = as_bufreader((*file).handle);
        let mut line = String::new();
        match reader.read_line(&mut line) {
            Ok(0) => {
                // EOF
                ptr::write(out, AdamString::from_bytes(b""));
                AdamIoResult {
                    success: false,
                    error_code: 0,
                    error_msg: AdamString::from_bytes(b""),
                }
            }
            Ok(_) => {
                ptr::write(out, AdamString::from_bytes(line.as_bytes()));
                io_ok()
            }
            Err(e) => {
                ptr::write(out, AdamString::from_bytes(b""));
                io_err(e)
            }
        }
    }
}

/// Write data through a file handle.
#[no_mangle]
pub extern "C" fn __adam_file_write(
    file: *mut AdamFile,
    data_ptr: *const u8,
    data_len: u64,
) -> AdamIoResult {
    unsafe {
        if file.is_null() || (*file).handle.is_null() {
            return io_err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "invalid file handle",
            ));
        }
        let data = slice::from_raw_parts(data_ptr, data_len as usize);
        let result = if is_raw_file((*file).handle) {
            as_raw_file((*file).handle).write_all(data)
        } else {
            // BufReader wraps a File — get mutable access to the inner writer.
            as_bufreader((*file).handle).get_mut().write_all(data)
        };
        match result {
            Ok(()) => io_ok(),
            Err(e) => io_err(e),
        }
    }
}

/// Close a file handle and free the underlying memory.
#[no_mangle]
pub extern "C" fn __adam_file_close(file: *mut AdamFile) {
    unsafe {
        if file.is_null() || (*file).handle.is_null() {
            return;
        }
        let handle = (*file).handle;
        if is_raw_file(handle) {
            let ptr = ((handle as usize) & !TAG_RAW_FILE) as *mut fs::File;
            drop(Box::from_raw(ptr));
        } else {
            let ptr = handle as *mut BufReader<fs::File>;
            drop(Box::from_raw(ptr));
        }
        (*file).handle = ptr::null_mut();
    }
}

/// Flush any buffered data in a file handle.
#[no_mangle]
pub extern "C" fn __adam_file_flush(file: *mut AdamFile) -> AdamIoResult {
    unsafe {
        if file.is_null() || (*file).handle.is_null() {
            return io_err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "invalid file handle",
            ));
        }
        let result = if is_raw_file((*file).handle) {
            as_raw_file((*file).handle).flush()
        } else {
            as_bufreader((*file).handle).get_mut().flush()
        };
        match result {
            Ok(()) => io_ok(),
            Err(e) => io_err(e),
        }
    }
}

// ================================================================
// Tests
// ================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::path::PathBuf;

    /// Helper: create a unique temporary file path under `std::env::temp_dir()`.
    fn tmp_path(name: &str) -> PathBuf {
        let mut p = std::env::temp_dir();
        p.push(format!("adam_io_test_{}", name));
        p
    }

    /// Helper: create a unique temporary directory path.
    fn tmp_dir(name: &str) -> PathBuf {
        let mut p = std::env::temp_dir();
        p.push(format!("adam_io_test_dir_{}", name));
        p
    }

    /// Helper: clean up a file if it exists.
    fn cleanup_file(p: &PathBuf) {
        let _ = fs::remove_file(p);
    }

    /// Helper: clean up a directory if it exists.
    fn cleanup_dir(p: &PathBuf) {
        let _ = fs::remove_dir_all(p);
    }

    // ------------------------------------------------------------------
    // 1. eprint does not crash
    // ------------------------------------------------------------------
    #[test]
    fn test_eprint_str_does_not_crash() {
        let msg = b"hello stderr";
        __adam_eprint_str(msg.as_ptr(), msg.len() as u64);
    }

    // ------------------------------------------------------------------
    // 2. eprintln does not crash
    // ------------------------------------------------------------------
    #[test]
    fn test_eprintln_str_does_not_crash() {
        let msg = b"hello stderr with newline";
        __adam_eprintln_str(msg.as_ptr(), msg.len() as u64);
    }

    // ------------------------------------------------------------------
    // 3. eprint with null ptr does not crash
    // ------------------------------------------------------------------
    #[test]
    fn test_eprint_null_ptr_does_not_crash() {
        __adam_eprint_str(std::ptr::null(), 0);
    }

    // ------------------------------------------------------------------
    // 4. fs_write then fs_read roundtrip
    // ------------------------------------------------------------------
    #[test]
    fn test_fs_write_then_read_roundtrip() {
        let p = tmp_path("write_read");
        cleanup_file(&p);
        let path_bytes = p.to_str().unwrap().as_bytes();
        let content = b"Hello, Adam!";

        let r = __adam_fs_write(
            path_bytes.as_ptr(),
            path_bytes.len() as u64,
            content.as_ptr(),
            content.len() as u64,
        );
        assert!(r.success);

        let mut out = AdamString::from_bytes(b"");
        let r = __adam_fs_read(path_bytes.as_ptr(), path_bytes.len() as u64, &mut out);
        assert!(r.success);

        unsafe {
            let read_back = slice::from_raw_parts(out.ptr, out.len as usize);
            assert_eq!(read_back, content);
        }

        cleanup_file(&p);
    }

    // ------------------------------------------------------------------
    // 5. fs_write_bytes then fs_read_bytes roundtrip
    // ------------------------------------------------------------------
    #[test]
    fn test_fs_write_bytes_then_read_bytes_roundtrip() {
        let p = tmp_path("write_read_bytes");
        cleanup_file(&p);
        let path_bytes = p.to_str().unwrap().as_bytes();
        let content: &[u8] = &[0xDE, 0xAD, 0xBE, 0xEF, 0x00, 0x42];

        let r = __adam_fs_write_bytes(
            path_bytes.as_ptr(),
            path_bytes.len() as u64,
            content.as_ptr(),
            content.len() as u64,
        );
        assert!(r.success);

        let mut out_ptr: *mut u8 = ptr::null_mut();
        let mut out_len: u64 = 0;
        let r = __adam_fs_read_bytes(
            path_bytes.as_ptr(),
            path_bytes.len() as u64,
            &mut out_ptr,
            &mut out_len,
        );
        assert!(r.success);
        assert_eq!(out_len, content.len() as u64);

        unsafe {
            let read_back = slice::from_raw_parts(out_ptr, out_len as usize);
            assert_eq!(read_back, content);
            // Free the allocated buffer.
            let layout = Layout::from_size_align_unchecked(out_len as usize, 1);
            std::alloc::dealloc(out_ptr, layout);
        }

        cleanup_file(&p);
    }

    // ------------------------------------------------------------------
    // 6. fs_exists returns false for nonexistent path
    // ------------------------------------------------------------------
    #[test]
    fn test_fs_exists_nonexistent() {
        let p = tmp_path("does_not_exist_12345");
        cleanup_file(&p);
        let path_bytes = p.to_str().unwrap().as_bytes();
        assert!(!__adam_fs_exists(path_bytes.as_ptr(), path_bytes.len() as u64));
    }

    // ------------------------------------------------------------------
    // 7. fs_exists returns true after write
    // ------------------------------------------------------------------
    #[test]
    fn test_fs_exists_after_write() {
        let p = tmp_path("exists_after_write");
        cleanup_file(&p);
        let path_bytes = p.to_str().unwrap().as_bytes();

        let content = b"data";
        __adam_fs_write(
            path_bytes.as_ptr(),
            path_bytes.len() as u64,
            content.as_ptr(),
            content.len() as u64,
        );
        assert!(__adam_fs_exists(path_bytes.as_ptr(), path_bytes.len() as u64));

        cleanup_file(&p);
    }

    // ------------------------------------------------------------------
    // 8. fs_remove removes a file
    // ------------------------------------------------------------------
    #[test]
    fn test_fs_remove() {
        let p = tmp_path("to_remove");
        cleanup_file(&p);
        let path_bytes = p.to_str().unwrap().as_bytes();

        fs::write(&p, b"delete me").unwrap();
        assert!(p.exists());

        let r = __adam_fs_remove(path_bytes.as_ptr(), path_bytes.len() as u64);
        assert!(r.success);
        assert!(!p.exists());
    }

    // ------------------------------------------------------------------
    // 9. fs_mkdir creates directory (and parents)
    // ------------------------------------------------------------------
    #[test]
    fn test_fs_mkdir() {
        let p = tmp_dir("mkdir_test/nested/deep");
        cleanup_dir(&tmp_dir("mkdir_test"));
        let path_bytes = p.to_str().unwrap().as_bytes();

        let r = __adam_fs_mkdir(path_bytes.as_ptr(), path_bytes.len() as u64);
        assert!(r.success);
        assert!(p.is_dir());

        cleanup_dir(&tmp_dir("mkdir_test"));
    }

    // ------------------------------------------------------------------
    // 10. fs_list_dir lists correct entries
    // ------------------------------------------------------------------
    #[test]
    fn test_fs_list_dir() {
        let dir = tmp_dir("list_dir_test");
        cleanup_dir(&dir);
        fs::create_dir_all(&dir).unwrap();

        // Create a few files inside.
        fs::write(dir.join("alpha.txt"), b"a").unwrap();
        fs::write(dir.join("beta.txt"), b"b").unwrap();
        fs::write(dir.join("gamma.txt"), b"c").unwrap();

        let path_bytes = dir.to_str().unwrap().as_bytes();
        let mut arr: *mut AdamString = ptr::null_mut();
        let mut count: u64 = 0;
        let r = __adam_fs_list_dir(
            path_bytes.as_ptr(),
            path_bytes.len() as u64,
            &mut arr,
            &mut count,
        );
        assert!(r.success);
        assert_eq!(count, 3);

        // Collect names and sort them for deterministic comparison.
        let mut names: Vec<String> = Vec::new();
        unsafe {
            for i in 0..count as usize {
                let s = &*arr.add(i);
                let bytes = slice::from_raw_parts(s.ptr, s.len as usize);
                names.push(String::from_utf8_lossy(bytes).to_string());
            }
            // Free the array and strings.
            for i in 0..count as usize {
                let s = &*arr.add(i);
                if !s.ptr.is_null() && s.cap > 0 {
                    let layout = Layout::from_size_align_unchecked(s.cap as usize, 1);
                    std::alloc::dealloc(s.ptr, layout);
                }
            }
            let layout = Layout::from_size_align_unchecked(
                (count as usize) * std::mem::size_of::<AdamString>(),
                std::mem::align_of::<AdamString>(),
            );
            std::alloc::dealloc(arr as *mut u8, layout);
        }

        names.sort();
        assert_eq!(names, vec!["alpha.txt", "beta.txt", "gamma.txt"]);

        cleanup_dir(&dir);
    }

    // ------------------------------------------------------------------
    // 11. fs_is_file
    // ------------------------------------------------------------------
    #[test]
    fn test_fs_is_file() {
        let p = tmp_path("is_file_test");
        cleanup_file(&p);
        fs::write(&p, b"data").unwrap();

        let path_bytes = p.to_str().unwrap().as_bytes();
        assert!(__adam_fs_is_file(path_bytes.as_ptr(), path_bytes.len() as u64));
        assert!(!__adam_fs_is_dir(path_bytes.as_ptr(), path_bytes.len() as u64));

        cleanup_file(&p);
    }

    // ------------------------------------------------------------------
    // 12. fs_is_dir
    // ------------------------------------------------------------------
    #[test]
    fn test_fs_is_dir() {
        let p = tmp_dir("is_dir_test");
        cleanup_dir(&p);
        fs::create_dir_all(&p).unwrap();

        let path_bytes = p.to_str().unwrap().as_bytes();
        assert!(__adam_fs_is_dir(path_bytes.as_ptr(), path_bytes.len() as u64));
        assert!(!__adam_fs_is_file(path_bytes.as_ptr(), path_bytes.len() as u64));

        cleanup_dir(&p);
    }

    // ------------------------------------------------------------------
    // 13. fs_file_size
    // ------------------------------------------------------------------
    #[test]
    fn test_fs_file_size() {
        let p = tmp_path("file_size_test");
        cleanup_file(&p);
        let content = b"twelve chars";
        fs::write(&p, content).unwrap();

        let path_bytes = p.to_str().unwrap().as_bytes();
        let mut size: u64 = 0;
        let r = __adam_fs_file_size(path_bytes.as_ptr(), path_bytes.len() as u64, &mut size);
        assert!(r.success);
        assert_eq!(size, content.len() as u64);

        cleanup_file(&p);
    }

    // ------------------------------------------------------------------
    // 14. fs_append appends correctly
    // ------------------------------------------------------------------
    #[test]
    fn test_fs_append() {
        let p = tmp_path("append_test");
        cleanup_file(&p);
        let path_bytes = p.to_str().unwrap().as_bytes();

        let part1 = b"Hello, ";
        let part2 = b"World!";

        __adam_fs_write(
            path_bytes.as_ptr(),
            path_bytes.len() as u64,
            part1.as_ptr(),
            part1.len() as u64,
        );
        __adam_fs_append(
            path_bytes.as_ptr(),
            path_bytes.len() as u64,
            part2.as_ptr(),
            part2.len() as u64,
        );

        let mut out = AdamString::from_bytes(b"");
        let r = __adam_fs_read(path_bytes.as_ptr(), path_bytes.len() as u64, &mut out);
        assert!(r.success);

        unsafe {
            let read_back = slice::from_raw_parts(out.ptr, out.len as usize);
            assert_eq!(read_back, b"Hello, World!");
        }

        cleanup_file(&p);
    }

    // ------------------------------------------------------------------
    // 15. file_open, file_read_all, file_close roundtrip
    // ------------------------------------------------------------------
    #[test]
    fn test_file_open_read_all_close() {
        let p = tmp_path("file_open_read");
        cleanup_file(&p);
        let content = b"Read me via handle!";
        fs::write(&p, content).unwrap();

        let path_bytes = p.to_str().unwrap().as_bytes();
        let mut fh = AdamFile {
            handle: ptr::null_mut(),
        };
        let r = __adam_file_open(path_bytes.as_ptr(), path_bytes.len() as u64, &mut fh);
        assert!(r.success);
        assert!(!fh.handle.is_null());

        let mut out = AdamString::from_bytes(b"");
        let r = __adam_file_read_all(&mut fh, &mut out);
        assert!(r.success);

        unsafe {
            let read_back = slice::from_raw_parts(out.ptr, out.len as usize);
            assert_eq!(read_back, content);
        }

        __adam_file_close(&mut fh);
        assert!(fh.handle.is_null());

        cleanup_file(&p);
    }

    // ------------------------------------------------------------------
    // 16. file_create, file_write, file_close, then read back
    // ------------------------------------------------------------------
    #[test]
    fn test_file_create_write_close_read_back() {
        let p = tmp_path("file_create_write");
        cleanup_file(&p);

        let path_bytes = p.to_str().unwrap().as_bytes();
        let mut fh = AdamFile {
            handle: ptr::null_mut(),
        };
        let r = __adam_file_create(path_bytes.as_ptr(), path_bytes.len() as u64, &mut fh);
        assert!(r.success);

        let data = b"Written via handle!";
        let r = __adam_file_write(&mut fh, data.as_ptr(), data.len() as u64);
        assert!(r.success);

        __adam_file_close(&mut fh);

        // Read back with fs_read.
        let mut out = AdamString::from_bytes(b"");
        let r = __adam_fs_read(path_bytes.as_ptr(), path_bytes.len() as u64, &mut out);
        assert!(r.success);

        unsafe {
            let read_back = slice::from_raw_parts(out.ptr, out.len as usize);
            assert_eq!(read_back, data.as_slice());
        }

        cleanup_file(&p);
    }

    // ------------------------------------------------------------------
    // 17. file_read_line reads lines one by one
    // ------------------------------------------------------------------
    #[test]
    fn test_file_read_line() {
        let p = tmp_path("file_read_line");
        cleanup_file(&p);
        fs::write(&p, "line1\nline2\nline3\n").unwrap();

        let path_bytes = p.to_str().unwrap().as_bytes();
        let mut fh = AdamFile {
            handle: ptr::null_mut(),
        };
        let r = __adam_file_open(path_bytes.as_ptr(), path_bytes.len() as u64, &mut fh);
        assert!(r.success);

        // Read three lines.
        for expected in &["line1\n", "line2\n", "line3\n"] {
            let mut out = AdamString::from_bytes(b"");
            let r = __adam_file_read_line(&mut fh, &mut out);
            assert!(r.success);
            unsafe {
                let bytes = slice::from_raw_parts(out.ptr, out.len as usize);
                assert_eq!(bytes, expected.as_bytes());
            }
        }

        // Next read should signal EOF: success=false, error_code=0.
        let mut out = AdamString::from_bytes(b"");
        let r = __adam_file_read_line(&mut fh, &mut out);
        assert!(!r.success);
        assert_eq!(r.error_code, 0);

        __adam_file_close(&mut fh);
        cleanup_file(&p);
    }

    // ------------------------------------------------------------------
    // 18. error: open nonexistent file
    // ------------------------------------------------------------------
    #[test]
    fn test_error_open_nonexistent() {
        let p = tmp_path("does_not_exist_open");
        cleanup_file(&p);
        let path_bytes = p.to_str().unwrap().as_bytes();

        let mut fh = AdamFile {
            handle: ptr::null_mut(),
        };
        let r = __adam_file_open(path_bytes.as_ptr(), path_bytes.len() as u64, &mut fh);
        assert!(!r.success);
        assert_ne!(r.error_code, 0);
        assert!(fh.handle.is_null());
    }

    // ------------------------------------------------------------------
    // 19. error: read from null/closed file handle
    // ------------------------------------------------------------------
    #[test]
    fn test_error_read_from_null_handle() {
        let mut fh = AdamFile {
            handle: ptr::null_mut(),
        };
        let mut out = AdamString::from_bytes(b"");
        let r = __adam_file_read_all(&mut fh, &mut out);
        assert!(!r.success);
    }

    // ------------------------------------------------------------------
    // 20. error: write to null file handle
    // ------------------------------------------------------------------
    #[test]
    fn test_error_write_to_null_handle() {
        let mut fh = AdamFile {
            handle: ptr::null_mut(),
        };
        let data = b"data";
        let r = __adam_file_write(&mut fh, data.as_ptr(), data.len() as u64);
        assert!(!r.success);
    }

    // ------------------------------------------------------------------
    // 21. error: flush null handle
    // ------------------------------------------------------------------
    #[test]
    fn test_error_flush_null_handle() {
        let mut fh = AdamFile {
            handle: ptr::null_mut(),
        };
        let r = __adam_file_flush(&mut fh);
        assert!(!r.success);
    }

    // ------------------------------------------------------------------
    // 22. error: fs_read nonexistent file
    // ------------------------------------------------------------------
    #[test]
    fn test_error_fs_read_nonexistent() {
        let p = tmp_path("no_such_file_for_read");
        cleanup_file(&p);
        let path_bytes = p.to_str().unwrap().as_bytes();

        let mut out = AdamString::from_bytes(b"");
        let r = __adam_fs_read(path_bytes.as_ptr(), path_bytes.len() as u64, &mut out);
        assert!(!r.success);
        assert_ne!(r.error_code, 0);
    }

    // ------------------------------------------------------------------
    // 23. error: fs_remove nonexistent file
    // ------------------------------------------------------------------
    #[test]
    fn test_error_fs_remove_nonexistent() {
        let p = tmp_path("no_such_file_for_remove");
        cleanup_file(&p);
        let path_bytes = p.to_str().unwrap().as_bytes();

        let r = __adam_fs_remove(path_bytes.as_ptr(), path_bytes.len() as u64);
        assert!(!r.success);
    }

    // ------------------------------------------------------------------
    // 24. close is idempotent (double close does not crash)
    // ------------------------------------------------------------------
    #[test]
    fn test_file_close_idempotent() {
        let p = tmp_path("close_idempotent");
        cleanup_file(&p);
        fs::write(&p, b"x").unwrap();

        let path_bytes = p.to_str().unwrap().as_bytes();
        let mut fh = AdamFile {
            handle: ptr::null_mut(),
        };
        let r = __adam_file_open(path_bytes.as_ptr(), path_bytes.len() as u64, &mut fh);
        assert!(r.success);

        __adam_file_close(&mut fh);
        assert!(fh.handle.is_null());
        // Second close should be a no-op.
        __adam_file_close(&mut fh);
        assert!(fh.handle.is_null());

        cleanup_file(&p);
    }

    // ------------------------------------------------------------------
    // 25. file_flush on a valid writable handle
    // ------------------------------------------------------------------
    #[test]
    fn test_file_flush_valid_handle() {
        let p = tmp_path("flush_test");
        cleanup_file(&p);
        let path_bytes = p.to_str().unwrap().as_bytes();

        let mut fh = AdamFile {
            handle: ptr::null_mut(),
        };
        let r = __adam_file_create(path_bytes.as_ptr(), path_bytes.len() as u64, &mut fh);
        assert!(r.success);

        let data = b"flush me";
        __adam_file_write(&mut fh, data.as_ptr(), data.len() as u64);

        let r = __adam_file_flush(&mut fh);
        assert!(r.success);

        __adam_file_close(&mut fh);
        cleanup_file(&p);
    }

    // ------------------------------------------------------------------
    // 26. fs_read_bytes for an empty file
    // ------------------------------------------------------------------
    #[test]
    fn test_fs_read_bytes_empty_file() {
        let p = tmp_path("empty_file_bytes");
        cleanup_file(&p);
        fs::write(&p, b"").unwrap();

        let path_bytes = p.to_str().unwrap().as_bytes();
        let mut out_ptr: *mut u8 = ptr::null_mut();
        let mut out_len: u64 = 0;
        let r = __adam_fs_read_bytes(
            path_bytes.as_ptr(),
            path_bytes.len() as u64,
            &mut out_ptr,
            &mut out_len,
        );
        assert!(r.success);
        assert_eq!(out_len, 0);
        assert!(out_ptr.is_null());

        cleanup_file(&p);
    }

    // ------------------------------------------------------------------
    // 27. fs_is_file and fs_is_dir on nonexistent path
    // ------------------------------------------------------------------
    #[test]
    fn test_is_file_is_dir_nonexistent() {
        let p = tmp_path("nonexistent_type_check");
        cleanup_file(&p);
        let path_bytes = p.to_str().unwrap().as_bytes();

        assert!(!__adam_fs_is_file(path_bytes.as_ptr(), path_bytes.len() as u64));
        assert!(!__adam_fs_is_dir(path_bytes.as_ptr(), path_bytes.len() as u64));
    }

    // ------------------------------------------------------------------
    // 28. fs_file_size on nonexistent path returns error
    // ------------------------------------------------------------------
    #[test]
    fn test_fs_file_size_nonexistent() {
        let p = tmp_path("nonexistent_size");
        cleanup_file(&p);
        let path_bytes = p.to_str().unwrap().as_bytes();

        let mut size: u64 = 999;
        let r = __adam_fs_file_size(path_bytes.as_ptr(), path_bytes.len() as u64, &mut size);
        assert!(!r.success);
        assert_eq!(size, 0);
    }

    // ------------------------------------------------------------------
    // 29. fs_list_dir on nonexistent directory returns error
    // ------------------------------------------------------------------
    #[test]
    fn test_fs_list_dir_nonexistent() {
        let p = tmp_dir("nonexistent_list_dir");
        cleanup_dir(&p);
        let path_bytes = p.to_str().unwrap().as_bytes();

        let mut arr: *mut AdamString = ptr::null_mut();
        let mut count: u64 = 0;
        let r = __adam_fs_list_dir(
            path_bytes.as_ptr(),
            path_bytes.len() as u64,
            &mut arr,
            &mut count,
        );
        assert!(!r.success);
        assert_eq!(count, 0);
        assert!(arr.is_null());
    }

    // ------------------------------------------------------------------
    // 30. fs_list_dir on empty directory
    // ------------------------------------------------------------------
    #[test]
    fn test_fs_list_dir_empty() {
        let dir = tmp_dir("empty_list_dir");
        cleanup_dir(&dir);
        fs::create_dir_all(&dir).unwrap();

        let path_bytes = dir.to_str().unwrap().as_bytes();
        let mut arr: *mut AdamString = ptr::null_mut();
        let mut count: u64 = 0;
        let r = __adam_fs_list_dir(
            path_bytes.as_ptr(),
            path_bytes.len() as u64,
            &mut arr,
            &mut count,
        );
        assert!(r.success);
        assert_eq!(count, 0);
        assert!(arr.is_null());

        cleanup_dir(&dir);
    }

    // ------------------------------------------------------------------
    // 31. file_read_line on a write-only handle returns error
    // ------------------------------------------------------------------
    #[test]
    fn test_file_read_line_on_write_handle() {
        let p = tmp_path("read_line_write_handle");
        cleanup_file(&p);
        let path_bytes = p.to_str().unwrap().as_bytes();

        let mut fh = AdamFile {
            handle: ptr::null_mut(),
        };
        let r = __adam_file_create(path_bytes.as_ptr(), path_bytes.len() as u64, &mut fh);
        assert!(r.success);

        let mut out = AdamString::from_bytes(b"");
        let r = __adam_file_read_line(&mut fh, &mut out);
        assert!(!r.success); // Cannot read lines from a write handle.

        __adam_file_close(&mut fh);
        cleanup_file(&p);
    }

    // ------------------------------------------------------------------
    // 32. large file roundtrip
    // ------------------------------------------------------------------
    #[test]
    fn test_large_file_roundtrip() {
        let p = tmp_path("large_file");
        cleanup_file(&p);
        let path_bytes = p.to_str().unwrap().as_bytes();

        // 64 KB of data.
        let content: Vec<u8> = (0..65536u32).map(|i| (i % 256) as u8).collect();
        let r = __adam_fs_write_bytes(
            path_bytes.as_ptr(),
            path_bytes.len() as u64,
            content.as_ptr(),
            content.len() as u64,
        );
        assert!(r.success);

        let mut out_ptr: *mut u8 = ptr::null_mut();
        let mut out_len: u64 = 0;
        let r = __adam_fs_read_bytes(
            path_bytes.as_ptr(),
            path_bytes.len() as u64,
            &mut out_ptr,
            &mut out_len,
        );
        assert!(r.success);
        assert_eq!(out_len, content.len() as u64);

        unsafe {
            let read_back = slice::from_raw_parts(out_ptr, out_len as usize);
            assert_eq!(read_back, content.as_slice());
            let layout = Layout::from_size_align_unchecked(out_len as usize, 1);
            std::alloc::dealloc(out_ptr, layout);
        }

        cleanup_file(&p);
    }

    // ------------------------------------------------------------------
    // 33. error_msg is non-empty for failed operations
    // ------------------------------------------------------------------
    #[test]
    fn test_error_msg_populated() {
        let p = tmp_path("nonexistent_for_error_msg");
        cleanup_file(&p);
        let path_bytes = p.to_str().unwrap().as_bytes();

        let mut out = AdamString::from_bytes(b"");
        let r = __adam_fs_read(path_bytes.as_ptr(), path_bytes.len() as u64, &mut out);
        assert!(!r.success);
        // The error message should contain something useful.
        assert!(r.error_msg.len > 0);
    }

    // ==================================================================
    // Adversarial / exhaustive tests (34–55)
    // ==================================================================

    // ------------------------------------------------------------------
    // 34. Unicode filename test: create/read file with Unicode path
    // ------------------------------------------------------------------
    #[test]
    fn test_unicode_filename() {
        let p = tmp_path("unicode_\u{00E9}\u{00E8}\u{00EA}_\u{4F60}\u{597D}_\u{1F600}");
        cleanup_file(&p);
        let path_bytes = p.to_str().unwrap().as_bytes();
        let content = b"unicode path content";

        let r = __adam_fs_write(
            path_bytes.as_ptr(),
            path_bytes.len() as u64,
            content.as_ptr(),
            content.len() as u64,
        );
        assert!(r.success, "write to unicode path should succeed");

        let mut out = AdamString::from_bytes(b"");
        let r = __adam_fs_read(path_bytes.as_ptr(), path_bytes.len() as u64, &mut out);
        assert!(r.success, "read from unicode path should succeed");
        unsafe {
            let read_back = slice::from_raw_parts(out.ptr, out.len as usize);
            assert_eq!(read_back, content);
        }

        cleanup_file(&p);
    }

    // ------------------------------------------------------------------
    // 35. Binary file with all 256 byte values (0x00-0xFF)
    // ------------------------------------------------------------------
    #[test]
    fn test_binary_all_256_byte_values() {
        let p = tmp_path("all_256_bytes");
        cleanup_file(&p);
        let path_bytes = p.to_str().unwrap().as_bytes();

        let content: Vec<u8> = (0u16..=255u16).map(|b| b as u8).collect();
        assert_eq!(content.len(), 256);

        let r = __adam_fs_write_bytes(
            path_bytes.as_ptr(),
            path_bytes.len() as u64,
            content.as_ptr(),
            content.len() as u64,
        );
        assert!(r.success);

        let mut out_ptr: *mut u8 = ptr::null_mut();
        let mut out_len: u64 = 0;
        let r = __adam_fs_read_bytes(
            path_bytes.as_ptr(),
            path_bytes.len() as u64,
            &mut out_ptr,
            &mut out_len,
        );
        assert!(r.success);
        assert_eq!(out_len, 256);

        unsafe {
            let read_back = slice::from_raw_parts(out_ptr, out_len as usize);
            assert_eq!(read_back, content.as_slice());
            for i in 0..256usize {
                assert_eq!(
                    read_back[i], i as u8,
                    "byte at index {} should be {} but got {}",
                    i, i as u8, read_back[i]
                );
            }
            let layout = Layout::from_size_align_unchecked(out_len as usize, 1);
            std::alloc::dealloc(out_ptr, layout);
        }

        cleanup_file(&p);
    }

    // ------------------------------------------------------------------
    // 36. Overwrite file: write to same path twice, second content wins
    // ------------------------------------------------------------------
    #[test]
    fn test_overwrite_file() {
        let p = tmp_path("overwrite_test");
        cleanup_file(&p);
        let path_bytes = p.to_str().unwrap().as_bytes();

        let first = b"first content";
        let second = b"second content wins!";

        let r = __adam_fs_write(
            path_bytes.as_ptr(),
            path_bytes.len() as u64,
            first.as_ptr(),
            first.len() as u64,
        );
        assert!(r.success);

        let r = __adam_fs_write(
            path_bytes.as_ptr(),
            path_bytes.len() as u64,
            second.as_ptr(),
            second.len() as u64,
        );
        assert!(r.success);

        let mut out = AdamString::from_bytes(b"");
        let r = __adam_fs_read(path_bytes.as_ptr(), path_bytes.len() as u64, &mut out);
        assert!(r.success);
        unsafe {
            let read_back = slice::from_raw_parts(out.ptr, out.len as usize);
            assert_eq!(read_back, second, "second write should overwrite the first");
        }

        cleanup_file(&p);
    }

    // ------------------------------------------------------------------
    // 37. Append to nonexistent file: should create the file
    // ------------------------------------------------------------------
    #[test]
    fn test_append_to_nonexistent_creates_file() {
        let p = tmp_path("append_create_new");
        cleanup_file(&p);
        assert!(!p.exists(), "file should not exist before test");
        let path_bytes = p.to_str().unwrap().as_bytes();

        let content = b"appended from nothing";
        let r = __adam_fs_append(
            path_bytes.as_ptr(),
            path_bytes.len() as u64,
            content.as_ptr(),
            content.len() as u64,
        );
        assert!(r.success, "append to nonexistent file should succeed");
        assert!(p.exists(), "file should exist after append");

        let mut out = AdamString::from_bytes(b"");
        let r = __adam_fs_read(path_bytes.as_ptr(), path_bytes.len() as u64, &mut out);
        assert!(r.success);
        unsafe {
            let read_back = slice::from_raw_parts(out.ptr, out.len as usize);
            assert_eq!(read_back, content);
        }

        cleanup_file(&p);
    }

    // ------------------------------------------------------------------
    // 38. Read empty file to string: should succeed with empty string
    // ------------------------------------------------------------------
    #[test]
    fn test_read_empty_file_to_string() {
        let p = tmp_path("empty_file_str");
        cleanup_file(&p);
        fs::write(&p, b"").unwrap();

        let path_bytes = p.to_str().unwrap().as_bytes();
        let mut out = AdamString::from_bytes(b"placeholder");
        let r = __adam_fs_read(path_bytes.as_ptr(), path_bytes.len() as u64, &mut out);
        assert!(r.success);
        assert_eq!(out.len, 0, "reading empty file should produce empty string");
        assert!(out.ptr.is_null(), "empty AdamString should have null ptr");

        cleanup_file(&p);
    }

    // ------------------------------------------------------------------
    // 39. Write empty content: file exists with size 0
    // ------------------------------------------------------------------
    #[test]
    fn test_write_empty_content() {
        let p = tmp_path("empty_content_write");
        cleanup_file(&p);
        let path_bytes = p.to_str().unwrap().as_bytes();

        let empty: &[u8] = b"";
        let r = __adam_fs_write(
            path_bytes.as_ptr(),
            path_bytes.len() as u64,
            empty.as_ptr(),
            0,
        );
        assert!(r.success);
        assert!(p.exists(), "file should exist after writing empty content");

        let mut size: u64 = 999;
        let r = __adam_fs_file_size(path_bytes.as_ptr(), path_bytes.len() as u64, &mut size);
        assert!(r.success);
        assert_eq!(size, 0, "file size should be 0 after writing empty content");

        cleanup_file(&p);
    }

    // ------------------------------------------------------------------
    // 40. Multiple sequential reads: read_all twice, second returns empty
    // ------------------------------------------------------------------
    #[test]
    fn test_multiple_sequential_read_all() {
        let p = tmp_path("multi_read_all");
        cleanup_file(&p);
        fs::write(&p, b"some data").unwrap();

        let path_bytes = p.to_str().unwrap().as_bytes();
        let mut fh = AdamFile { handle: ptr::null_mut() };
        let r = __adam_file_open(path_bytes.as_ptr(), path_bytes.len() as u64, &mut fh);
        assert!(r.success);

        // First read should get all content.
        let mut out1 = AdamString::from_bytes(b"");
        let r = __adam_file_read_all(&mut fh, &mut out1);
        assert!(r.success);
        unsafe {
            let bytes = slice::from_raw_parts(out1.ptr, out1.len as usize);
            assert_eq!(bytes, b"some data");
        }

        // Second read should return empty (cursor is at end).
        let mut out2 = AdamString::from_bytes(b"");
        let r = __adam_file_read_all(&mut fh, &mut out2);
        assert!(r.success, "second read_all should succeed (not error)");
        assert_eq!(out2.len, 0, "second read_all should return empty string");

        __adam_file_close(&mut fh);
        cleanup_file(&p);
    }

    // ------------------------------------------------------------------
    // 41. File handle read_line on file without trailing newline
    // ------------------------------------------------------------------
    #[test]
    fn test_read_line_no_trailing_newline() {
        let p = tmp_path("no_trailing_newline");
        cleanup_file(&p);
        // File has two lines; second line has no trailing newline.
        fs::write(&p, "first\nsecond_no_newline").unwrap();

        let path_bytes = p.to_str().unwrap().as_bytes();
        let mut fh = AdamFile { handle: ptr::null_mut() };
        let r = __adam_file_open(path_bytes.as_ptr(), path_bytes.len() as u64, &mut fh);
        assert!(r.success);

        // First line includes newline.
        let mut out = AdamString::from_bytes(b"");
        let r = __adam_file_read_line(&mut fh, &mut out);
        assert!(r.success);
        unsafe {
            let bytes = slice::from_raw_parts(out.ptr, out.len as usize);
            assert_eq!(bytes, b"first\n");
        }

        // Second line should be returned WITHOUT trailing newline.
        let mut out = AdamString::from_bytes(b"");
        let r = __adam_file_read_line(&mut fh, &mut out);
        assert!(r.success, "last line without newline should still succeed");
        unsafe {
            let bytes = slice::from_raw_parts(out.ptr, out.len as usize);
            assert_eq!(bytes, b"second_no_newline");
        }

        // Next read should be EOF.
        let mut out = AdamString::from_bytes(b"");
        let r = __adam_file_read_line(&mut fh, &mut out);
        assert!(!r.success, "EOF should return success=false");
        assert_eq!(r.error_code, 0, "EOF should have error_code=0");

        __adam_file_close(&mut fh);
        cleanup_file(&p);
    }

    // ------------------------------------------------------------------
    // 42. File handle write multiple times then read back full content
    // ------------------------------------------------------------------
    #[test]
    fn test_file_handle_write_multiple_chunks() {
        let p = tmp_path("multi_chunk_write");
        cleanup_file(&p);
        let path_bytes = p.to_str().unwrap().as_bytes();

        let mut fh = AdamFile { handle: ptr::null_mut() };
        let r = __adam_file_create(path_bytes.as_ptr(), path_bytes.len() as u64, &mut fh);
        assert!(r.success);

        let chunks: &[&[u8]] = &[b"chunk1_", b"chunk2_", b"chunk3_", b"end"];
        for chunk in chunks {
            let r = __adam_file_write(&mut fh, chunk.as_ptr(), chunk.len() as u64);
            assert!(r.success, "writing chunk should succeed");
        }

        __adam_file_close(&mut fh);

        // Read back entire contents via fs_read.
        let mut out = AdamString::from_bytes(b"");
        let r = __adam_fs_read(path_bytes.as_ptr(), path_bytes.len() as u64, &mut out);
        assert!(r.success);
        unsafe {
            let read_back = slice::from_raw_parts(out.ptr, out.len as usize);
            assert_eq!(read_back, b"chunk1_chunk2_chunk3_end");
        }

        cleanup_file(&p);
    }

    // ------------------------------------------------------------------
    // 43. Large directory listing: create 50 files, list them all
    // ------------------------------------------------------------------
    #[test]
    fn test_large_directory_listing_50_files() {
        let dir = tmp_dir("large_listing_50");
        cleanup_dir(&dir);
        fs::create_dir_all(&dir).unwrap();

        let expected_count = 50u64;
        for i in 0..expected_count {
            fs::write(dir.join(format!("file_{:03}.txt", i)), format!("data_{}", i)).unwrap();
        }

        let path_bytes = dir.to_str().unwrap().as_bytes();
        let mut arr: *mut AdamString = ptr::null_mut();
        let mut count: u64 = 0;
        let r = __adam_fs_list_dir(
            path_bytes.as_ptr(),
            path_bytes.len() as u64,
            &mut arr,
            &mut count,
        );
        assert!(r.success);
        assert_eq!(count, expected_count, "should list exactly 50 entries");

        // Collect and verify all names are present.
        let mut names: Vec<String> = Vec::new();
        unsafe {
            for i in 0..count as usize {
                let s = &*arr.add(i);
                let bytes = slice::from_raw_parts(s.ptr, s.len as usize);
                names.push(String::from_utf8_lossy(bytes).to_string());
            }
            // Free strings and array.
            for i in 0..count as usize {
                let s = &*arr.add(i);
                if !s.ptr.is_null() && s.cap > 0 {
                    let layout = Layout::from_size_align_unchecked(s.cap as usize, 1);
                    std::alloc::dealloc(s.ptr, layout);
                }
            }
            let layout = Layout::from_size_align_unchecked(
                (count as usize) * std::mem::size_of::<AdamString>(),
                std::mem::align_of::<AdamString>(),
            );
            std::alloc::dealloc(arr as *mut u8, layout);
        }

        names.sort();
        for i in 0..50u64 {
            let expected_name = format!("file_{:03}.txt", i);
            assert!(
                names.contains(&expected_name),
                "directory listing should contain {}",
                expected_name
            );
        }

        cleanup_dir(&dir);
    }

    // ------------------------------------------------------------------
    // 44. Nested directory creation: mkdir with deep path (a/b/c/d/e)
    // ------------------------------------------------------------------
    #[test]
    fn test_nested_directory_creation_deep() {
        let base = tmp_dir("deep_nest");
        cleanup_dir(&base);
        let deep = base.join("a").join("b").join("c").join("d").join("e");
        let path_bytes = deep.to_str().unwrap().as_bytes();

        let r = __adam_fs_mkdir(path_bytes.as_ptr(), path_bytes.len() as u64);
        assert!(r.success, "creating deeply nested directory should succeed");
        assert!(deep.is_dir(), "deeply nested path should be a directory");

        // Verify each intermediate also exists.
        assert!(base.join("a").is_dir());
        assert!(base.join("a").join("b").is_dir());
        assert!(base.join("a").join("b").join("c").is_dir());
        assert!(base.join("a").join("b").join("c").join("d").is_dir());

        cleanup_dir(&base);
    }

    // ------------------------------------------------------------------
    // 45. Remove directory fails for file (fs_remove is for files only)
    // ------------------------------------------------------------------
    #[test]
    fn test_fs_remove_fails_on_directory() {
        let dir = tmp_dir("remove_dir_fail");
        cleanup_dir(&dir);
        fs::create_dir_all(&dir).unwrap();
        assert!(dir.is_dir());

        let path_bytes = dir.to_str().unwrap().as_bytes();
        let r = __adam_fs_remove(path_bytes.as_ptr(), path_bytes.len() as u64);
        assert!(
            !r.success,
            "fs_remove should fail when given a directory path"
        );
        assert!(dir.is_dir(), "directory should still exist after failed remove");

        cleanup_dir(&dir);
    }

    // ------------------------------------------------------------------
    // 46. fs_exists on directory returns true
    // ------------------------------------------------------------------
    #[test]
    fn test_fs_exists_on_directory() {
        let dir = tmp_dir("exists_dir_check");
        cleanup_dir(&dir);
        fs::create_dir_all(&dir).unwrap();

        let path_bytes = dir.to_str().unwrap().as_bytes();
        assert!(
            __adam_fs_exists(path_bytes.as_ptr(), path_bytes.len() as u64),
            "fs_exists should return true for directories"
        );

        // Also verify for a file in the same test for contrast.
        let file = dir.join("somefile.txt");
        fs::write(&file, b"exists").unwrap();
        let file_bytes = file.to_str().unwrap().as_bytes();
        assert!(
            __adam_fs_exists(file_bytes.as_ptr(), file_bytes.len() as u64),
            "fs_exists should return true for files"
        );

        cleanup_dir(&dir);
    }

    // ------------------------------------------------------------------
    // 47. fs_write then fs_read UTF-8 roundtrip with complex chars
    // ------------------------------------------------------------------
    #[test]
    fn test_utf8_roundtrip_complex() {
        let p = tmp_path("utf8_complex_roundtrip");
        cleanup_file(&p);
        let path_bytes = p.to_str().unwrap().as_bytes();

        // Complex UTF-8 string: emoji, CJK, diacritics, RTL, math symbols
        let content = "Hello \u{1F600}\u{1F4A9} \u{4E16}\u{754C} caf\u{00E9} \u{0645}\u{0631}\u{062D}\u{0628}\u{0627} \u{221E}\u{2202}\u{222B} \u{00DF}\u{00FC}\u{00E4}\u{00F6}";
        let content_bytes = content.as_bytes();

        let r = __adam_fs_write(
            path_bytes.as_ptr(),
            path_bytes.len() as u64,
            content_bytes.as_ptr(),
            content_bytes.len() as u64,
        );
        assert!(r.success);

        let mut out = AdamString::from_bytes(b"");
        let r = __adam_fs_read(path_bytes.as_ptr(), path_bytes.len() as u64, &mut out);
        assert!(r.success);
        unsafe {
            let read_back = slice::from_raw_parts(out.ptr, out.len as usize);
            assert_eq!(
                read_back, content_bytes,
                "UTF-8 roundtrip should preserve exact bytes"
            );
            // Also verify it parses as valid UTF-8.
            let read_str = std::str::from_utf8(read_back).expect("read back should be valid UTF-8");
            assert_eq!(read_str, content);
        }

        cleanup_file(&p);
    }

    // ------------------------------------------------------------------
    // 48. file_size after append: verify size grows
    // ------------------------------------------------------------------
    #[test]
    fn test_file_size_after_append() {
        let p = tmp_path("size_after_append");
        cleanup_file(&p);
        let path_bytes = p.to_str().unwrap().as_bytes();

        let initial = b"initial";
        let r = __adam_fs_write(
            path_bytes.as_ptr(),
            path_bytes.len() as u64,
            initial.as_ptr(),
            initial.len() as u64,
        );
        assert!(r.success);

        let mut size_before: u64 = 0;
        let r = __adam_fs_file_size(path_bytes.as_ptr(), path_bytes.len() as u64, &mut size_before);
        assert!(r.success);
        assert_eq!(size_before, initial.len() as u64);

        let appended = b"_appended_data";
        let r = __adam_fs_append(
            path_bytes.as_ptr(),
            path_bytes.len() as u64,
            appended.as_ptr(),
            appended.len() as u64,
        );
        assert!(r.success);

        let mut size_after: u64 = 0;
        let r = __adam_fs_file_size(path_bytes.as_ptr(), path_bytes.len() as u64, &mut size_after);
        assert!(r.success);
        assert_eq!(
            size_after,
            (initial.len() + appended.len()) as u64,
            "file size should grow after append"
        );
        assert!(
            size_after > size_before,
            "size after append must be larger than before"
        );

        cleanup_file(&p);
    }

    // ------------------------------------------------------------------
    // 49. fs_read_bytes on binary file with embedded nulls
    // ------------------------------------------------------------------
    #[test]
    fn test_read_bytes_embedded_nulls() {
        let p = tmp_path("embedded_nulls");
        cleanup_file(&p);
        let path_bytes = p.to_str().unwrap().as_bytes();

        // Data with many embedded null bytes.
        let content: Vec<u8> = vec![
            0x00, 0x01, 0x00, 0x00, 0xFF, 0x00, 0x42, 0x00, 0x00, 0x00, 0xAB, 0x00,
        ];

        let r = __adam_fs_write_bytes(
            path_bytes.as_ptr(),
            path_bytes.len() as u64,
            content.as_ptr(),
            content.len() as u64,
        );
        assert!(r.success);

        let mut out_ptr: *mut u8 = ptr::null_mut();
        let mut out_len: u64 = 0;
        let r = __adam_fs_read_bytes(
            path_bytes.as_ptr(),
            path_bytes.len() as u64,
            &mut out_ptr,
            &mut out_len,
        );
        assert!(r.success);
        assert_eq!(out_len, content.len() as u64);

        unsafe {
            let read_back = slice::from_raw_parts(out_ptr, out_len as usize);
            assert_eq!(
                read_back, content.as_slice(),
                "embedded null bytes must be preserved exactly"
            );
            // Verify each null individually.
            assert_eq!(read_back[0], 0x00);
            assert_eq!(read_back[2], 0x00);
            assert_eq!(read_back[3], 0x00);
            assert_eq!(read_back[5], 0x00);
            assert_eq!(read_back[7], 0x00);
            assert_eq!(read_back[8], 0x00);
            assert_eq!(read_back[9], 0x00);
            assert_eq!(read_back[11], 0x00);

            let layout = Layout::from_size_align_unchecked(out_len as usize, 1);
            std::alloc::dealloc(out_ptr, layout);
        }

        cleanup_file(&p);
    }

    // ------------------------------------------------------------------
    // 50. Concurrent-safe unique paths: verify no cross-test interference
    // ------------------------------------------------------------------
    #[test]
    fn test_unique_paths_no_interference() {
        let p1 = tmp_path("interference_a");
        let p2 = tmp_path("interference_b");
        cleanup_file(&p1);
        cleanup_file(&p2);

        let p1_bytes = p1.to_str().unwrap().as_bytes();
        let p2_bytes = p2.to_str().unwrap().as_bytes();

        let content_a = b"content_for_a";
        let content_b = b"content_for_b";

        let r = __adam_fs_write(
            p1_bytes.as_ptr(), p1_bytes.len() as u64,
            content_a.as_ptr(), content_a.len() as u64,
        );
        assert!(r.success);

        let r = __adam_fs_write(
            p2_bytes.as_ptr(), p2_bytes.len() as u64,
            content_b.as_ptr(), content_b.len() as u64,
        );
        assert!(r.success);

        // Read back both and ensure they did not interfere.
        let mut out_a = AdamString::from_bytes(b"");
        let r = __adam_fs_read(p1_bytes.as_ptr(), p1_bytes.len() as u64, &mut out_a);
        assert!(r.success);

        let mut out_b = AdamString::from_bytes(b"");
        let r = __adam_fs_read(p2_bytes.as_ptr(), p2_bytes.len() as u64, &mut out_b);
        assert!(r.success);

        unsafe {
            let a = slice::from_raw_parts(out_a.ptr, out_a.len as usize);
            let b = slice::from_raw_parts(out_b.ptr, out_b.len as usize);
            assert_eq!(a, content_a, "file a should have its own content");
            assert_eq!(b, content_b, "file b should have its own content");
            assert_ne!(a, b, "two different files should not have same content");
        }

        cleanup_file(&p1);
        cleanup_file(&p2);
    }

    // ------------------------------------------------------------------
    // 51. Error message contains useful info when opening nonexistent file
    // ------------------------------------------------------------------
    #[test]
    fn test_error_message_contains_useful_info() {
        let p = tmp_path("nonexistent_for_error_detail");
        cleanup_file(&p);
        let path_bytes = p.to_str().unwrap().as_bytes();

        let mut fh = AdamFile { handle: ptr::null_mut() };
        let r = __adam_file_open(path_bytes.as_ptr(), path_bytes.len() as u64, &mut fh);
        assert!(!r.success);
        assert!(r.error_msg.len > 0, "error message should not be empty");

        unsafe {
            let msg_bytes = slice::from_raw_parts(r.error_msg.ptr, r.error_msg.len as usize);
            let msg = std::str::from_utf8(msg_bytes).expect("error message should be valid UTF-8");
            // The OS error message should contain something meaningful (e.g. "No such file").
            let msg_lower = msg.to_lowercase();
            assert!(
                msg_lower.contains("no such file")
                    || msg_lower.contains("not found")
                    || msg_lower.contains("does not exist")
                    || msg_lower.contains("cannot find"),
                "error message '{}' should describe a missing-file error",
                msg
            );
        }
    }

    // ------------------------------------------------------------------
    // 52. fs_mkdir on existing directory: should succeed (idempotent)
    // ------------------------------------------------------------------
    #[test]
    fn test_fs_mkdir_idempotent() {
        let dir = tmp_dir("mkdir_idempotent");
        cleanup_dir(&dir);

        let path_bytes = dir.to_str().unwrap().as_bytes();

        // First creation.
        let r = __adam_fs_mkdir(path_bytes.as_ptr(), path_bytes.len() as u64);
        assert!(r.success, "first mkdir should succeed");
        assert!(dir.is_dir());

        // Second creation on same path should also succeed (create_dir_all is idempotent).
        let r = __adam_fs_mkdir(path_bytes.as_ptr(), path_bytes.len() as u64);
        assert!(
            r.success,
            "mkdir on existing directory should succeed (idempotent)"
        );
        assert!(dir.is_dir());

        cleanup_dir(&dir);
    }

    // ------------------------------------------------------------------
    // 53. File handle flush followed by write: flush doesn't break writes
    // ------------------------------------------------------------------
    #[test]
    fn test_flush_then_write() {
        let p = tmp_path("flush_then_write");
        cleanup_file(&p);
        let path_bytes = p.to_str().unwrap().as_bytes();

        let mut fh = AdamFile { handle: ptr::null_mut() };
        let r = __adam_file_create(path_bytes.as_ptr(), path_bytes.len() as u64, &mut fh);
        assert!(r.success);

        // Write, flush, then write more.
        let chunk1 = b"before_flush_";
        let r = __adam_file_write(&mut fh, chunk1.as_ptr(), chunk1.len() as u64);
        assert!(r.success);

        let r = __adam_file_flush(&mut fh);
        assert!(r.success, "flush should succeed");

        let chunk2 = b"after_flush";
        let r = __adam_file_write(&mut fh, chunk2.as_ptr(), chunk2.len() as u64);
        assert!(r.success, "write after flush should succeed");

        // Flush again for good measure.
        let r = __adam_file_flush(&mut fh);
        assert!(r.success);

        __adam_file_close(&mut fh);

        // Read back and verify both chunks are present.
        let mut out = AdamString::from_bytes(b"");
        let r = __adam_fs_read(path_bytes.as_ptr(), path_bytes.len() as u64, &mut out);
        assert!(r.success);
        unsafe {
            let read_back = slice::from_raw_parts(out.ptr, out.len as usize);
            assert_eq!(read_back, b"before_flush_after_flush");
        }

        cleanup_file(&p);
    }

    // ------------------------------------------------------------------
    // 54. Overwrite with shorter content: old data does not linger
    // ------------------------------------------------------------------
    #[test]
    fn test_overwrite_shorter_content() {
        let p = tmp_path("overwrite_shorter");
        cleanup_file(&p);
        let path_bytes = p.to_str().unwrap().as_bytes();

        let long_content = b"this is a very long piece of content that should be fully replaced";
        let short_content = b"short";

        let r = __adam_fs_write(
            path_bytes.as_ptr(), path_bytes.len() as u64,
            long_content.as_ptr(), long_content.len() as u64,
        );
        assert!(r.success);

        let r = __adam_fs_write(
            path_bytes.as_ptr(), path_bytes.len() as u64,
            short_content.as_ptr(), short_content.len() as u64,
        );
        assert!(r.success);

        // File size should be the short content length, not the long one.
        let mut size: u64 = 0;
        let r = __adam_fs_file_size(path_bytes.as_ptr(), path_bytes.len() as u64, &mut size);
        assert!(r.success);
        assert_eq!(size, short_content.len() as u64, "old data should not linger");

        let mut out = AdamString::from_bytes(b"");
        let r = __adam_fs_read(path_bytes.as_ptr(), path_bytes.len() as u64, &mut out);
        assert!(r.success);
        unsafe {
            let read_back = slice::from_raw_parts(out.ptr, out.len as usize);
            assert_eq!(read_back, short_content);
        }

        cleanup_file(&p);
    }

    // ------------------------------------------------------------------
    // 55. file_create truncates existing file
    // ------------------------------------------------------------------
    #[test]
    fn test_file_create_truncates_existing() {
        let p = tmp_path("create_truncates");
        cleanup_file(&p);
        let path_bytes = p.to_str().unwrap().as_bytes();

        // Write some initial content.
        fs::write(&p, b"original data that is long").unwrap();

        // Open with file_create (should truncate).
        let mut fh = AdamFile { handle: ptr::null_mut() };
        let r = __adam_file_create(path_bytes.as_ptr(), path_bytes.len() as u64, &mut fh);
        assert!(r.success);

        let new_data = b"new";
        let r = __adam_file_write(&mut fh, new_data.as_ptr(), new_data.len() as u64);
        assert!(r.success);
        __adam_file_close(&mut fh);

        // Verify only the new data is in the file.
        let mut out = AdamString::from_bytes(b"");
        let r = __adam_fs_read(path_bytes.as_ptr(), path_bytes.len() as u64, &mut out);
        assert!(r.success);
        unsafe {
            let read_back = slice::from_raw_parts(out.ptr, out.len as usize);
            assert_eq!(
                read_back, new_data,
                "file_create should truncate existing content"
            );
        }

        cleanup_file(&p);
    }

    // ------------------------------------------------------------------
    // 56. Multiple appends accumulate correctly
    // ------------------------------------------------------------------
    #[test]
    fn test_multiple_appends_accumulate() {
        let p = tmp_path("multi_append");
        cleanup_file(&p);
        let path_bytes = p.to_str().unwrap().as_bytes();

        let parts: &[&[u8]] = &[b"one_", b"two_", b"three_", b"four_", b"five"];
        for part in parts {
            let r = __adam_fs_append(
                path_bytes.as_ptr(), path_bytes.len() as u64,
                part.as_ptr(), part.len() as u64,
            );
            assert!(r.success);
        }

        let mut out = AdamString::from_bytes(b"");
        let r = __adam_fs_read(path_bytes.as_ptr(), path_bytes.len() as u64, &mut out);
        assert!(r.success);
        unsafe {
            let read_back = slice::from_raw_parts(out.ptr, out.len as usize);
            assert_eq!(read_back, b"one_two_three_four_five");
        }

        cleanup_file(&p);
    }

    // ------------------------------------------------------------------
    // 57. io_ok and io_err helper sanity
    // ------------------------------------------------------------------
    #[test]
    fn test_io_ok_result_fields() {
        let r = io_ok();
        assert!(r.success);
        assert_eq!(r.error_code, 0);
        assert_eq!(r.error_msg.len, 0);
    }

    #[test]
    fn test_io_err_result_fields() {
        let e = io::Error::new(io::ErrorKind::NotFound, "test error");
        let r = io_err(e);
        assert!(!r.success);
        assert!(r.error_msg.len > 0);
        unsafe {
            let msg = slice::from_raw_parts(r.error_msg.ptr, r.error_msg.len as usize);
            assert_eq!(msg, b"test error");
        }
    }

    // ------------------------------------------------------------------
    // 58. Read line on empty file returns EOF immediately
    // ------------------------------------------------------------------
    #[test]
    fn test_read_line_empty_file_eof() {
        let p = tmp_path("read_line_empty");
        cleanup_file(&p);
        fs::write(&p, b"").unwrap();

        let path_bytes = p.to_str().unwrap().as_bytes();
        let mut fh = AdamFile { handle: ptr::null_mut() };
        let r = __adam_file_open(path_bytes.as_ptr(), path_bytes.len() as u64, &mut fh);
        assert!(r.success);

        let mut out = AdamString::from_bytes(b"");
        let r = __adam_file_read_line(&mut fh, &mut out);
        assert!(!r.success, "reading line from empty file should signal EOF");
        assert_eq!(r.error_code, 0, "EOF should have error_code 0");
        assert_eq!(out.len, 0);

        __adam_file_close(&mut fh);
        cleanup_file(&p);
    }

    // ------------------------------------------------------------------
    // 59. fs_read on binary (non-UTF8) file returns error
    // ------------------------------------------------------------------
    #[test]
    fn test_fs_read_non_utf8_fails() {
        let p = tmp_path("non_utf8_read_fail");
        cleanup_file(&p);

        // Write invalid UTF-8 bytes.
        let invalid_utf8: &[u8] = &[0xFF, 0xFE, 0x80, 0x81, 0xC0, 0xC1];
        fs::write(&p, invalid_utf8).unwrap();

        let path_bytes = p.to_str().unwrap().as_bytes();
        let mut out = AdamString::from_bytes(b"");
        let r = __adam_fs_read(path_bytes.as_ptr(), path_bytes.len() as u64, &mut out);
        // fs::read_to_string will fail on invalid UTF-8.
        assert!(!r.success, "fs_read should fail on non-UTF-8 content");

        cleanup_file(&p);
    }

    // ------------------------------------------------------------------
    // 60. Close null pointer (not just null handle) does not crash
    // ------------------------------------------------------------------
    #[test]
    fn test_file_close_with_null_ptr() {
        // Passing a raw null pointer to close should be safe.
        __adam_file_close(ptr::null_mut());
    }

    // ------------------------------------------------------------------
    // 61. fs_list_dir does not include . and ..
    // ------------------------------------------------------------------
    #[test]
    fn test_list_dir_no_dot_entries() {
        let dir = tmp_dir("no_dot_entries");
        cleanup_dir(&dir);
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("real_file.txt"), b"x").unwrap();

        let path_bytes = dir.to_str().unwrap().as_bytes();
        let mut arr: *mut AdamString = ptr::null_mut();
        let mut count: u64 = 0;
        let r = __adam_fs_list_dir(
            path_bytes.as_ptr(),
            path_bytes.len() as u64,
            &mut arr,
            &mut count,
        );
        assert!(r.success);
        assert_eq!(count, 1, "should only have one real entry");

        unsafe {
            let s = &*arr;
            let bytes = slice::from_raw_parts(s.ptr, s.len as usize);
            let name = std::str::from_utf8(bytes).unwrap();
            assert_ne!(name, ".", "listing should not contain '.'");
            assert_ne!(name, "..", "listing should not contain '..'");
            assert_eq!(name, "real_file.txt");

            // Free.
            if !s.ptr.is_null() && s.cap > 0 {
                let layout = Layout::from_size_align_unchecked(s.cap as usize, 1);
                std::alloc::dealloc(s.ptr, layout);
            }
            let layout = Layout::from_size_align_unchecked(
                std::mem::size_of::<AdamString>(),
                std::mem::align_of::<AdamString>(),
            );
            std::alloc::dealloc(arr as *mut u8, layout);
        }

        cleanup_dir(&dir);
    }
}
