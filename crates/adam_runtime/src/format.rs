//! Adam Runtime — Formatting and Conversion functions (P10.S6).
//!
//! Provides FFI-safe parsing and formatting functions for the Adam language.

use crate::string::AdamString;

// ================================================================
// Parsing functions
// ================================================================

/// Parse a UTF-8 string (given as ptr+len) into an i64.
/// Returns true on success, false on error. Result written to `out`.
#[no_mangle]
pub extern "C" fn __adam_parse_int(ptr: *const u8, len: u64, out: *mut i64) -> bool {
    if ptr.is_null() || out.is_null() {
        return false;
    }
    let slice = unsafe { std::slice::from_raw_parts(ptr, len as usize) };
    let s = match std::str::from_utf8(slice) {
        Ok(s) => s.trim(),
        Err(_) => return false,
    };
    match s.parse::<i64>() {
        Ok(v) => {
            unsafe { *out = v };
            true
        }
        Err(_) => false,
    }
}

/// Parse a UTF-8 string (given as ptr+len) into an f64.
/// Returns true on success, false on error. Result written to `out`.
#[no_mangle]
pub extern "C" fn __adam_parse_float(ptr: *const u8, len: u64, out: *mut f64) -> bool {
    if ptr.is_null() || out.is_null() {
        return false;
    }
    let slice = unsafe { std::slice::from_raw_parts(ptr, len as usize) };
    let s = match std::str::from_utf8(slice) {
        Ok(s) => s.trim(),
        Err(_) => return false,
    };
    match s.parse::<f64>() {
        Ok(v) => {
            unsafe { *out = v };
            true
        }
        Err(_) => false,
    }
}

/// Parse a UTF-8 string (given as ptr+len) into a bool (i8).
/// Accepts "true" -> 1, "false" -> 0. Returns false on anything else.
#[no_mangle]
pub extern "C" fn __adam_parse_bool(ptr: *const u8, len: u64, out: *mut i8) -> bool {
    if ptr.is_null() || out.is_null() {
        return false;
    }
    let slice = unsafe { std::slice::from_raw_parts(ptr, len as usize) };
    let s = match std::str::from_utf8(slice) {
        Ok(s) => s.trim(),
        Err(_) => return false,
    };
    match s {
        "true" => {
            unsafe { *out = 1 };
            true
        }
        "false" => {
            unsafe { *out = 0 };
            true
        }
        _ => false,
    }
}

// ================================================================
// Integer formatting
// ================================================================

/// Format an i64 as a decimal string.
#[no_mangle]
pub extern "C" fn __adam_format_int(val: i64) -> AdamString {
    let s = val.to_string();
    AdamString::from_bytes(s.as_bytes())
}

/// Format an i64 as a hexadecimal string with "0x" prefix.
#[no_mangle]
pub extern "C" fn __adam_format_int_hex(val: i64) -> AdamString {
    let s = if val < 0 {
        format!("-0x{:x}", (val as i128).unsigned_abs())
    } else {
        format!("0x{:x}", val)
    };
    AdamString::from_bytes(s.as_bytes())
}

/// Format an i64 as an octal string with "0o" prefix.
#[no_mangle]
pub extern "C" fn __adam_format_int_octal(val: i64) -> AdamString {
    let s = if val < 0 {
        format!("-0o{:o}", (val as i128).unsigned_abs())
    } else {
        format!("0o{:o}", val)
    };
    AdamString::from_bytes(s.as_bytes())
}

/// Format an i64 as a binary string with "0b" prefix.
#[no_mangle]
pub extern "C" fn __adam_format_int_binary(val: i64) -> AdamString {
    let s = if val < 0 {
        format!("-0b{:b}", (val as i128).unsigned_abs())
    } else {
        format!("0b{:b}", val)
    };
    AdamString::from_bytes(s.as_bytes())
}

// ================================================================
// Float formatting
// ================================================================

/// Format an f64 with a given precision. If precision < 0, use default formatting.
#[no_mangle]
pub extern "C" fn __adam_format_float(val: f64, precision: i32) -> AdamString {
    let s = if precision < 0 {
        format!("{}", val)
    } else {
        format!("{:.prec$}", val, prec = precision as usize)
    };
    AdamString::from_bytes(s.as_bytes())
}

/// Format an f64 in scientific notation. If precision < 0, use default.
#[no_mangle]
pub extern "C" fn __adam_format_float_scientific(val: f64, precision: i32) -> AdamString {
    let s = if precision < 0 {
        format!("{:e}", val)
    } else {
        format!("{:.prec$e}", val, prec = precision as usize)
    };
    AdamString::from_bytes(s.as_bytes())
}

// ================================================================
// Bool formatting
// ================================================================

/// Format a bool (i8) as "true" or "false".
#[no_mangle]
pub extern "C" fn __adam_format_bool(val: i8) -> AdamString {
    let s = if val != 0 { "true" } else { "false" };
    AdamString::from_bytes(s.as_bytes())
}

// ================================================================
// Char formatting
// ================================================================

/// Format a Unicode scalar value (u32) as a string containing that character.
#[no_mangle]
pub extern "C" fn __adam_format_char(val: u32) -> AdamString {
    if let Some(c) = char::from_u32(val) {
        let mut buf = [0u8; 4];
        let s = c.encode_utf8(&mut buf);
        AdamString::from_bytes(s.as_bytes())
    } else {
        AdamString::from_bytes("\u{FFFD}".as_bytes())
    }
}

// ================================================================
// Debug formatting
// ================================================================

/// Debug format for i64 — same as format_int.
#[no_mangle]
pub extern "C" fn __adam_format_debug_int(val: i64) -> AdamString {
    let s = val.to_string();
    AdamString::from_bytes(s.as_bytes())
}

/// Debug format for f64 — uses enough precision to be unambiguous.
#[no_mangle]
pub extern "C" fn __adam_format_debug_float(val: f64) -> AdamString {
    let s = format!("{:?}", val);
    AdamString::from_bytes(s.as_bytes())
}

/// Debug format for bool — same as format_bool.
#[no_mangle]
pub extern "C" fn __adam_format_debug_bool(val: i8) -> AdamString {
    let s = if val != 0 { "true" } else { "false" };
    AdamString::from_bytes(s.as_bytes())
}

/// Debug format for string — wraps in double quotes, escapes special chars.
#[no_mangle]
pub extern "C" fn __adam_format_debug_string(ptr: *const u8, len: u64) -> AdamString {
    if ptr.is_null() || len == 0 {
        return AdamString::from_bytes(b"\"\"");
    }
    let slice = unsafe { std::slice::from_raw_parts(ptr, len as usize) };
    let s = match std::str::from_utf8(slice) {
        Ok(s) => s,
        Err(_) => {
            // Non-UTF-8: show as raw byte hex
            let mut out = String::with_capacity(len as usize * 4 + 2);
            out.push('"');
            for &b in slice {
                if b.is_ascii_graphic() || b == b' ' {
                    out.push(b as char);
                } else {
                    out.push_str(&format!("\\x{:02x}", b));
                }
            }
            out.push('"');
            return AdamString::from_bytes(out.as_bytes());
        }
    };

    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for c in s.chars() {
        match c {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\0' => out.push_str("\\0"),
            c if c.is_control() => {
                // Escape other control characters as \u{XXXX}
                out.push_str(&format!("\\u{{{:04x}}}", c as u32));
            }
            c => out.push(c),
        }
    }
    out.push('"');
    AdamString::from_bytes(out.as_bytes())
}

/// Debug format for char — wraps in single quotes.
#[no_mangle]
pub extern "C" fn __adam_format_debug_char(val: u32) -> AdamString {
    if let Some(c) = char::from_u32(val) {
        let mut out = String::with_capacity(6);
        out.push('\'');
        match c {
            '\\' => out.push_str("\\\\"),
            '\'' => out.push_str("\\'"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\0' => out.push_str("\\0"),
            c if c.is_control() => {
                out.push_str(&format!("\\u{{{:04x}}}", c as u32));
            }
            c => out.push(c),
        }
        out.push('\'');
        AdamString::from_bytes(out.as_bytes())
    } else {
        AdamString::from_bytes("'\\u{fffd}'".as_bytes())
    }
}

// ================================================================
// Tests
// ================================================================

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper to extract a Rust String from an AdamString (for test assertions).
    fn adam_to_string(s: &AdamString) -> String {
        if s.ptr.is_null() || s.len == 0 {
            return String::new();
        }
        let slice = unsafe { std::slice::from_raw_parts(s.ptr, s.len as usize) };
        String::from_utf8_lossy(slice).into_owned()
    }

    // --- parse_int ---

    #[test]
    fn test_parse_int_valid() {
        let input = b"42";
        let mut out: i64 = 0;
        assert!(__adam_parse_int(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
        assert_eq!(out, 42);
    }

    #[test]
    fn test_parse_int_negative() {
        let input = b"-100";
        let mut out: i64 = 0;
        assert!(__adam_parse_int(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
        assert_eq!(out, -100);
    }

    #[test]
    fn test_parse_int_invalid() {
        let input = b"hello";
        let mut out: i64 = 0;
        assert!(!__adam_parse_int(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
    }

    #[test]
    fn test_parse_int_empty() {
        let input = b"";
        let mut out: i64 = 0;
        assert!(!__adam_parse_int(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
    }

    #[test]
    fn test_parse_int_with_whitespace() {
        let input = b"  123  ";
        let mut out: i64 = 0;
        assert!(__adam_parse_int(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
        assert_eq!(out, 123);
    }

    #[test]
    fn test_parse_int_float_string() {
        let input = b"3.14";
        let mut out: i64 = 0;
        assert!(!__adam_parse_int(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
    }

    #[test]
    fn test_parse_int_null_ptr() {
        let mut out: i64 = 0;
        assert!(!__adam_parse_int(std::ptr::null(), 5, &mut out));
    }

    // --- parse_float ---

    #[test]
    fn test_parse_float_valid() {
        let input = b"3.14";
        let mut out: f64 = 0.0;
        assert!(__adam_parse_float(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
        assert!((out - 3.14).abs() < 1e-10);
    }

    #[test]
    fn test_parse_float_integer() {
        let input = b"42";
        let mut out: f64 = 0.0;
        assert!(__adam_parse_float(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
        assert!((out - 42.0).abs() < 1e-10);
    }

    #[test]
    fn test_parse_float_negative() {
        let input = b"-2.5";
        let mut out: f64 = 0.0;
        assert!(__adam_parse_float(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
        assert!((out - (-2.5)).abs() < 1e-10);
    }

    #[test]
    fn test_parse_float_scientific() {
        let input = b"1.5e3";
        let mut out: f64 = 0.0;
        assert!(__adam_parse_float(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
        assert!((out - 1500.0).abs() < 1e-10);
    }

    #[test]
    fn test_parse_float_invalid() {
        let input = b"abc";
        let mut out: f64 = 0.0;
        assert!(!__adam_parse_float(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
    }

    // --- parse_bool ---

    #[test]
    fn test_parse_bool_true() {
        let input = b"true";
        let mut out: i8 = 0;
        assert!(__adam_parse_bool(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
        assert_eq!(out, 1);
    }

    #[test]
    fn test_parse_bool_false() {
        let input = b"false";
        let mut out: i8 = 0;
        assert!(__adam_parse_bool(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
        assert_eq!(out, 0);
    }

    #[test]
    fn test_parse_bool_yes_fails() {
        let input = b"yes";
        let mut out: i8 = 0;
        assert!(!__adam_parse_bool(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
    }

    #[test]
    fn test_parse_bool_empty_fails() {
        let input = b"";
        let mut out: i8 = 0;
        assert!(!__adam_parse_bool(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
    }

    #[test]
    fn test_parse_bool_capitalized_fails() {
        let input = b"True";
        let mut out: i8 = 0;
        assert!(!__adam_parse_bool(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
    }

    // --- format_int ---

    #[test]
    fn test_format_int_positive() {
        let s = __adam_format_int(42);
        assert_eq!(adam_to_string(&s), "42");
    }

    #[test]
    fn test_format_int_negative() {
        let s = __adam_format_int(-99);
        assert_eq!(adam_to_string(&s), "-99");
    }

    #[test]
    fn test_format_int_zero() {
        let s = __adam_format_int(0);
        assert_eq!(adam_to_string(&s), "0");
    }

    // --- format_int_hex ---

    #[test]
    fn test_format_int_hex() {
        let s = __adam_format_int_hex(255);
        assert_eq!(adam_to_string(&s), "0xff");
    }

    #[test]
    fn test_format_int_hex_negative() {
        let s = __adam_format_int_hex(-1);
        assert_eq!(adam_to_string(&s), "-0x1");
    }

    #[test]
    fn test_format_int_hex_zero() {
        let s = __adam_format_int_hex(0);
        assert_eq!(adam_to_string(&s), "0x0");
    }

    // --- format_int_octal ---

    #[test]
    fn test_format_int_octal() {
        let s = __adam_format_int_octal(8);
        assert_eq!(adam_to_string(&s), "0o10");
    }

    #[test]
    fn test_format_int_octal_negative() {
        let s = __adam_format_int_octal(-8);
        assert_eq!(adam_to_string(&s), "-0o10");
    }

    // --- format_int_binary ---

    #[test]
    fn test_format_int_binary() {
        let s = __adam_format_int_binary(10);
        assert_eq!(adam_to_string(&s), "0b1010");
    }

    #[test]
    fn test_format_int_binary_negative() {
        let s = __adam_format_int_binary(-5);
        assert_eq!(adam_to_string(&s), "-0b101");
    }

    // --- format_float ---

    #[test]
    fn test_format_float_default() {
        let s = __adam_format_float(3.14, -1);
        assert_eq!(adam_to_string(&s), "3.14");
    }

    #[test]
    fn test_format_float_precision_2() {
        let s = __adam_format_float(3.14159, 2);
        assert_eq!(adam_to_string(&s), "3.14");
    }

    #[test]
    fn test_format_float_precision_0() {
        let s = __adam_format_float(3.7, 0);
        assert_eq!(adam_to_string(&s), "4");
    }

    #[test]
    fn test_format_float_precision_5() {
        let s = __adam_format_float(1.0, 5);
        assert_eq!(adam_to_string(&s), "1.00000");
    }

    // --- format_float_scientific ---

    #[test]
    fn test_format_float_scientific_default() {
        let s = __adam_format_float_scientific(1500.0, -1);
        assert_eq!(adam_to_string(&s), "1.5e3");
    }

    #[test]
    fn test_format_float_scientific_precision_3() {
        let s = __adam_format_float_scientific(1500.0, 3);
        assert_eq!(adam_to_string(&s), "1.500e3");
    }

    // --- format_bool ---

    #[test]
    fn test_format_bool_true() {
        let s = __adam_format_bool(1);
        assert_eq!(adam_to_string(&s), "true");
    }

    #[test]
    fn test_format_bool_false() {
        let s = __adam_format_bool(0);
        assert_eq!(adam_to_string(&s), "false");
    }

    #[test]
    fn test_format_bool_nonzero() {
        let s = __adam_format_bool(42);
        assert_eq!(adam_to_string(&s), "true");
    }

    // --- format_char ---

    #[test]
    fn test_format_char_ascii() {
        let s = __adam_format_char(b'A' as u32);
        assert_eq!(adam_to_string(&s), "A");
    }

    #[test]
    fn test_format_char_unicode() {
        let s = __adam_format_char(0x1F600); // grinning face emoji
        assert_eq!(adam_to_string(&s), "\u{1F600}");
    }

    #[test]
    fn test_format_char_invalid() {
        let s = __adam_format_char(0xD800); // surrogate, invalid
        assert_eq!(adam_to_string(&s), "\u{FFFD}");
    }

    // --- debug formatting ---

    #[test]
    fn test_debug_int() {
        let s = __adam_format_debug_int(42);
        assert_eq!(adam_to_string(&s), "42");
    }

    #[test]
    fn test_debug_float() {
        let s = __adam_format_debug_float(1.0);
        // Rust Debug for 1.0_f64 is "1.0"
        assert_eq!(adam_to_string(&s), "1.0");
    }

    #[test]
    fn test_debug_bool() {
        let s = __adam_format_debug_bool(1);
        assert_eq!(adam_to_string(&s), "true");
    }

    #[test]
    fn test_debug_string_simple() {
        let input = b"hello";
        let s = __adam_format_debug_string(input.as_ptr(), input.len() as u64);
        assert_eq!(adam_to_string(&s), "\"hello\"");
    }

    #[test]
    fn test_debug_string_with_escapes() {
        let input = b"line1\nline2";
        let s = __adam_format_debug_string(input.as_ptr(), input.len() as u64);
        assert_eq!(adam_to_string(&s), "\"line1\\nline2\"");
    }

    #[test]
    fn test_debug_string_with_quotes() {
        let input = b"say \"hi\"";
        let s = __adam_format_debug_string(input.as_ptr(), input.len() as u64);
        assert_eq!(adam_to_string(&s), "\"say \\\"hi\\\"\"");
    }

    #[test]
    fn test_debug_string_with_backslash() {
        let input = b"a\\b";
        let s = __adam_format_debug_string(input.as_ptr(), input.len() as u64);
        assert_eq!(adam_to_string(&s), "\"a\\\\b\"");
    }

    #[test]
    fn test_debug_string_with_tab() {
        let input = b"col1\tcol2";
        let s = __adam_format_debug_string(input.as_ptr(), input.len() as u64);
        assert_eq!(adam_to_string(&s), "\"col1\\tcol2\"");
    }

    #[test]
    fn test_debug_string_empty() {
        let s = __adam_format_debug_string(std::ptr::null(), 0);
        assert_eq!(adam_to_string(&s), "\"\"");
    }

    #[test]
    fn test_debug_char_ascii() {
        let s = __adam_format_debug_char(b'A' as u32);
        assert_eq!(adam_to_string(&s), "'A'");
    }

    #[test]
    fn test_debug_char_newline() {
        let s = __adam_format_debug_char(b'\n' as u32);
        assert_eq!(adam_to_string(&s), "'\\n'");
    }

    #[test]
    fn test_debug_char_single_quote() {
        let s = __adam_format_debug_char(b'\'' as u32);
        assert_eq!(adam_to_string(&s), "'\\''");
    }

    // ================================================================
    // Adversarial / exhaustive tests
    // ================================================================

    // --- parse_int edge cases ---

    #[test]
    fn test_parse_int_i64_max() {
        let input = b"9223372036854775807";
        let mut out: i64 = 0;
        assert!(__adam_parse_int(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
        assert_eq!(out, i64::MAX);
    }

    #[test]
    fn test_parse_int_i64_min() {
        let input = b"-9223372036854775808";
        let mut out: i64 = 0;
        assert!(__adam_parse_int(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
        assert_eq!(out, i64::MIN);
    }

    #[test]
    fn test_parse_int_overflow() {
        let input = b"9999999999999999999999";
        let mut out: i64 = 0;
        assert!(!__adam_parse_int(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
    }

    #[test]
    fn test_parse_int_leading_zeros() {
        let input = b"007";
        let mut out: i64 = 0;
        assert!(__adam_parse_int(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
        assert_eq!(out, 7);
    }

    #[test]
    fn test_parse_int_just_plus_sign() {
        let input = b"+";
        let mut out: i64 = 0;
        assert!(!__adam_parse_int(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
    }

    #[test]
    fn test_parse_int_just_minus_sign() {
        let input = b"-";
        let mut out: i64 = 0;
        assert!(!__adam_parse_int(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
    }

    #[test]
    fn test_parse_int_null_out() {
        let input = b"42";
        assert!(!__adam_parse_int(
            input.as_ptr(),
            input.len() as u64,
            std::ptr::null_mut()
        ));
    }

    // --- parse_float edge cases ---

    #[test]
    fn test_parse_float_inf() {
        let input = b"inf";
        let mut out: f64 = 0.0;
        assert!(__adam_parse_float(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
        assert_eq!(out, f64::INFINITY);
    }

    #[test]
    fn test_parse_float_neg_inf() {
        let input = b"-inf";
        let mut out: f64 = 0.0;
        assert!(__adam_parse_float(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
        assert_eq!(out, f64::NEG_INFINITY);
    }

    #[test]
    fn test_parse_float_nan() {
        let input = b"NaN";
        let mut out: f64 = 0.0;
        assert!(__adam_parse_float(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
        assert!(out.is_nan());
    }

    #[test]
    fn test_parse_float_leading_zeros() {
        let input = b"00.5";
        let mut out: f64 = 0.0;
        assert!(__adam_parse_float(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
        assert!((out - 0.5).abs() < 1e-10);
    }

    #[test]
    fn test_parse_float_leading_dot() {
        // ".5" should parse to 0.5
        let input = b".5";
        let mut out: f64 = 0.0;
        assert!(__adam_parse_float(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
        assert!((out - 0.5).abs() < 1e-10);
    }

    #[test]
    fn test_parse_float_trailing_dot() {
        // "1." should parse to 1.0
        let input = b"1.";
        let mut out: f64 = 0.0;
        assert!(__adam_parse_float(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
        assert!((out - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_parse_float_null_out() {
        let input = b"3.14";
        assert!(!__adam_parse_float(
            input.as_ptr(),
            input.len() as u64,
            std::ptr::null_mut()
        ));
    }

    #[test]
    fn test_parse_float_empty() {
        let input = b"";
        let mut out: f64 = 0.0;
        assert!(!__adam_parse_float(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
    }

    // --- parse_bool edge cases ---

    #[test]
    fn test_parse_bool_with_whitespace() {
        // The implementation trims, so " true " should work
        let input = b" true ";
        let mut out: i8 = 0;
        assert!(__adam_parse_bool(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
        assert_eq!(out, 1);
    }

    #[test]
    fn test_parse_bool_false_with_whitespace() {
        let input = b"  false  ";
        let mut out: i8 = 0;
        assert!(__adam_parse_bool(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
        assert_eq!(out, 0);
    }

    #[test]
    fn test_parse_bool_uppercase_true_fails() {
        let input = b"TRUE";
        let mut out: i8 = 0;
        assert!(!__adam_parse_bool(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
    }

    #[test]
    fn test_parse_bool_mixed_case_fails() {
        let input = b"True";
        let mut out: i8 = -1;
        assert!(!__adam_parse_bool(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
    }

    #[test]
    fn test_parse_bool_1_fails() {
        let input = b"1";
        let mut out: i8 = 0;
        assert!(!__adam_parse_bool(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
    }

    #[test]
    fn test_parse_bool_null_ptr() {
        let mut out: i8 = 0;
        assert!(!__adam_parse_bool(std::ptr::null(), 5, &mut out));
    }

    #[test]
    fn test_parse_bool_null_out() {
        let input = b"true";
        assert!(!__adam_parse_bool(
            input.as_ptr(),
            input.len() as u64,
            std::ptr::null_mut()
        ));
    }

    // --- format_int extremes ---

    #[test]
    fn test_format_int_i64_min() {
        let s = __adam_format_int(i64::MIN);
        assert_eq!(adam_to_string(&s), "-9223372036854775808");
    }

    #[test]
    fn test_format_int_i64_max() {
        let s = __adam_format_int(i64::MAX);
        assert_eq!(adam_to_string(&s), "9223372036854775807");
    }

    // --- Hex/octal/binary extremes ---

    #[test]
    fn test_format_int_hex_i64_max() {
        let s = __adam_format_int_hex(i64::MAX);
        assert_eq!(adam_to_string(&s), "0x7fffffffffffffff");
    }

    #[test]
    fn test_format_int_hex_i64_min() {
        let s = __adam_format_int_hex(i64::MIN);
        assert_eq!(adam_to_string(&s), "-0x8000000000000000");
    }

    #[test]
    fn test_format_int_binary_i64_min() {
        let s = __adam_format_int_binary(i64::MIN);
        let result = adam_to_string(&s);
        assert!(result.starts_with("-0b1"));
        assert_eq!(
            result,
            format!("-0b{:b}", (i64::MIN as i128).unsigned_abs())
        );
    }

    #[test]
    fn test_format_int_binary_zero() {
        let s = __adam_format_int_binary(0);
        assert_eq!(adam_to_string(&s), "0b0");
    }

    #[test]
    fn test_format_int_octal_zero() {
        let s = __adam_format_int_octal(0);
        assert_eq!(adam_to_string(&s), "0o0");
    }

    #[test]
    fn test_format_int_hex_zero_adversarial() {
        let s = __adam_format_int_hex(0);
        assert_eq!(adam_to_string(&s), "0x0");
    }

    // --- Float formatting edge cases ---

    #[test]
    fn test_format_float_zero_precision_0() {
        let s = __adam_format_float(0.0, 0);
        assert_eq!(adam_to_string(&s), "0");
    }

    #[test]
    fn test_format_float_infinity_default() {
        let s = __adam_format_float(f64::INFINITY, -1);
        assert_eq!(adam_to_string(&s), "inf");
    }

    #[test]
    fn test_format_float_neg_infinity_default() {
        let s = __adam_format_float(f64::NEG_INFINITY, -1);
        assert_eq!(adam_to_string(&s), "-inf");
    }

    #[test]
    fn test_format_float_nan_precision_2() {
        let s = __adam_format_float(f64::NAN, 2);
        assert_eq!(adam_to_string(&s), "NaN");
    }

    #[test]
    fn test_format_float_nan_default() {
        let s = __adam_format_float(f64::NAN, -1);
        assert_eq!(adam_to_string(&s), "NaN");
    }

    #[test]
    fn test_format_float_precision_10_pi() {
        let s = __adam_format_float(std::f64::consts::PI, 10);
        let result = adam_to_string(&s);
        // Should have exactly 10 decimal places
        let dot_pos = result.find('.').unwrap();
        let decimals = &result[dot_pos + 1..];
        assert_eq!(
            decimals.len(),
            10,
            "Expected 10 decimal places, got {}",
            decimals.len()
        );
        assert!(result.starts_with("3.1415926536"));
    }

    // --- Scientific notation edge cases ---

    #[test]
    fn test_format_float_scientific_zero() {
        let s = __adam_format_float_scientific(0.0, -1);
        assert_eq!(adam_to_string(&s), "0e0");
    }

    #[test]
    fn test_format_float_scientific_nan() {
        let s = __adam_format_float_scientific(f64::NAN, -1);
        assert_eq!(adam_to_string(&s), "NaN");
    }

    #[test]
    fn test_format_float_scientific_inf() {
        let s = __adam_format_float_scientific(f64::INFINITY, -1);
        assert_eq!(adam_to_string(&s), "inf");
    }

    #[test]
    fn test_format_float_scientific_neg_inf() {
        let s = __adam_format_float_scientific(f64::NEG_INFINITY, 2);
        assert_eq!(adam_to_string(&s), "-inf");
    }

    // --- Debug string with all escape types ---

    #[test]
    fn test_debug_string_all_escapes() {
        // String containing \n, \r, \t, \0, \\, " all at once
        let input = b"a\nb\rc\td\0e\\f\"g";
        let s = __adam_format_debug_string(input.as_ptr(), input.len() as u64);
        assert_eq!(adam_to_string(&s), "\"a\\nb\\rc\\td\\0e\\\\f\\\"g\"");
    }

    #[test]
    fn test_debug_string_only_special_chars() {
        let input = b"\n\r\t\0\\\"";
        let s = __adam_format_debug_string(input.as_ptr(), input.len() as u64);
        assert_eq!(adam_to_string(&s), "\"\\n\\r\\t\\0\\\\\\\"\"");
    }

    // --- Debug char edge cases ---

    #[test]
    fn test_debug_char_null() {
        let s = __adam_format_debug_char(0);
        assert_eq!(adam_to_string(&s), "'\\0'");
    }

    #[test]
    fn test_debug_char_control_bell() {
        // BEL character (0x07) is a control character
        let s = __adam_format_debug_char(0x07);
        assert_eq!(adam_to_string(&s), "'\\u{0007}'");
    }

    #[test]
    fn test_debug_char_tab() {
        let s = __adam_format_debug_char(b'\t' as u32);
        assert_eq!(adam_to_string(&s), "'\\t'");
    }

    #[test]
    fn test_debug_char_carriage_return() {
        let s = __adam_format_debug_char(b'\r' as u32);
        assert_eq!(adam_to_string(&s), "'\\r'");
    }

    #[test]
    fn test_debug_char_backslash() {
        let s = __adam_format_debug_char(b'\\' as u32);
        assert_eq!(adam_to_string(&s), "'\\\\'");
    }

    #[test]
    fn test_debug_char_invalid_surrogate() {
        // 0xD800 is a surrogate, not a valid char
        let s = __adam_format_debug_char(0xD800);
        assert_eq!(adam_to_string(&s), "'\\u{fffd}'");
    }

    // --- Round-trip tests ---

    #[test]
    fn test_round_trip_format_parse_int_positive() {
        let original: i64 = 123456789;
        let formatted = __adam_format_int(original);
        let formatted_str = adam_to_string(&formatted);
        let input = formatted_str.as_bytes();
        let mut out: i64 = 0;
        assert!(__adam_parse_int(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
        assert_eq!(out, original);
    }

    #[test]
    fn test_round_trip_format_parse_int_negative() {
        let original: i64 = -987654321;
        let formatted = __adam_format_int(original);
        let formatted_str = adam_to_string(&formatted);
        let input = formatted_str.as_bytes();
        let mut out: i64 = 0;
        assert!(__adam_parse_int(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
        assert_eq!(out, original);
    }

    #[test]
    fn test_round_trip_format_parse_int_i64_max() {
        let original = i64::MAX;
        let formatted = __adam_format_int(original);
        let formatted_str = adam_to_string(&formatted);
        let input = formatted_str.as_bytes();
        let mut out: i64 = 0;
        assert!(__adam_parse_int(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
        assert_eq!(out, original);
    }

    #[test]
    fn test_round_trip_format_parse_int_i64_min() {
        let original = i64::MIN;
        let formatted = __adam_format_int(original);
        let formatted_str = adam_to_string(&formatted);
        let input = formatted_str.as_bytes();
        let mut out: i64 = 0;
        assert!(__adam_parse_int(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
        assert_eq!(out, original);
    }

    // --- Format bool edge cases ---

    #[test]
    fn test_format_bool_neg1() {
        let s = __adam_format_bool(-1);
        assert_eq!(adam_to_string(&s), "true");
    }

    #[test]
    fn test_format_bool_127() {
        let s = __adam_format_bool(127);
        assert_eq!(adam_to_string(&s), "true");
    }

    #[test]
    fn test_format_bool_neg128() {
        let s = __adam_format_bool(-128);
        assert_eq!(adam_to_string(&s), "true");
    }

    // --- Format char edge cases ---

    #[test]
    fn test_format_char_null() {
        let s = __adam_format_char(0);
        // Null char is valid Unicode, should produce a string with U+0000
        let result = adam_to_string(&s);
        assert_eq!(result.len(), 1);
        assert_eq!(result.as_bytes()[0], 0);
    }

    #[test]
    fn test_format_char_max_valid() {
        // U+10FFFF is the max valid Unicode scalar value
        let s = __adam_format_char(0x10FFFF);
        let result = adam_to_string(&s);
        assert_eq!(result.chars().next().unwrap() as u32, 0x10FFFF);
    }

    // --- Debug float edge cases ---

    #[test]
    fn test_debug_float_nan() {
        let s = __adam_format_debug_float(f64::NAN);
        assert_eq!(adam_to_string(&s), "NaN");
    }

    #[test]
    fn test_debug_float_inf() {
        let s = __adam_format_debug_float(f64::INFINITY);
        assert_eq!(adam_to_string(&s), "inf");
    }

    #[test]
    fn test_debug_float_neg_inf() {
        let s = __adam_format_debug_float(f64::NEG_INFINITY);
        assert_eq!(adam_to_string(&s), "-inf");
    }

    // --- Debug string with non-UTF-8 ---

    #[test]
    fn test_debug_string_non_utf8() {
        let input: &[u8] = &[0xFF, 0xFE, b'A'];
        let s = __adam_format_debug_string(input.as_ptr(), input.len() as u64);
        let result = adam_to_string(&s);
        // Non-UTF-8 path: should hex-escape non-printable bytes
        assert!(result.starts_with('"'));
        assert!(result.ends_with('"'));
        assert!(result.contains("\\xff"));
        assert!(result.contains("\\xfe"));
        assert!(result.contains("A"));
    }

    // --- Debug string with unicode ---

    #[test]
    fn test_debug_string_unicode() {
        let input = "hello world".as_bytes();
        let s = __adam_format_debug_string(input.as_ptr(), input.len() as u64);
        let result = adam_to_string(&s);
        assert_eq!(result, "\"hello world\"");
    }

    // --- parse_float with whitespace ---

    #[test]
    fn test_parse_float_with_whitespace() {
        let input = b"  3.14  ";
        let mut out: f64 = 0.0;
        assert!(__adam_parse_float(
            input.as_ptr(),
            input.len() as u64,
            &mut out
        ));
        assert!((out - 3.14).abs() < 1e-10);
    }
}
