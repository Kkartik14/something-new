//! Adam Runtime — Math functions (P10.S4).
//!
//! Provides FFI-safe math constants and functions for the Adam language.

// ================================================================
// Constants
// ================================================================

#[no_mangle]
pub extern "C" fn __adam_math_pi() -> f64 {
    std::f64::consts::PI
}

#[no_mangle]
pub extern "C" fn __adam_math_e() -> f64 {
    std::f64::consts::E
}

#[no_mangle]
pub extern "C" fn __adam_math_tau() -> f64 {
    std::f64::consts::TAU
}

#[no_mangle]
pub extern "C" fn __adam_math_infinity() -> f64 {
    f64::INFINITY
}

#[no_mangle]
pub extern "C" fn __adam_math_neg_infinity() -> f64 {
    f64::NEG_INFINITY
}

#[no_mangle]
pub extern "C" fn __adam_math_nan() -> f64 {
    f64::NAN
}

// ================================================================
// Basic math functions
// ================================================================

#[no_mangle]
pub extern "C" fn __adam_math_abs(x: f64) -> f64 {
    x.abs()
}

#[no_mangle]
pub extern "C" fn __adam_math_abs_int(x: i64) -> i64 {
    x.wrapping_abs()
}

#[no_mangle]
pub extern "C" fn __adam_math_floor(x: f64) -> f64 {
    x.floor()
}

#[no_mangle]
pub extern "C" fn __adam_math_ceil(x: f64) -> f64 {
    x.ceil()
}

#[no_mangle]
pub extern "C" fn __adam_math_round(x: f64) -> f64 {
    x.round()
}

#[no_mangle]
pub extern "C" fn __adam_math_min(a: f64, b: f64) -> f64 {
    a.min(b)
}

#[no_mangle]
pub extern "C" fn __adam_math_max(a: f64, b: f64) -> f64 {
    a.max(b)
}

#[no_mangle]
pub extern "C" fn __adam_math_min_int(a: i64, b: i64) -> i64 {
    a.min(b)
}

#[no_mangle]
pub extern "C" fn __adam_math_max_int(a: i64, b: i64) -> i64 {
    a.max(b)
}

#[no_mangle]
pub extern "C" fn __adam_math_clamp(val: f64, lo: f64, hi: f64) -> f64 {
    val.clamp(lo, hi)
}

// ================================================================
// Power / Root / Logarithm
// ================================================================

#[no_mangle]
pub extern "C" fn __adam_math_sqrt(x: f64) -> f64 {
    x.sqrt()
}

#[no_mangle]
pub extern "C" fn __adam_math_pow(base: f64, exp: f64) -> f64 {
    base.powf(exp)
}

#[no_mangle]
pub extern "C" fn __adam_math_log(x: f64) -> f64 {
    x.ln()
}

#[no_mangle]
pub extern "C" fn __adam_math_log2(x: f64) -> f64 {
    x.log2()
}

#[no_mangle]
pub extern "C" fn __adam_math_log10(x: f64) -> f64 {
    x.log10()
}

// ================================================================
// Trigonometric functions
// ================================================================

#[no_mangle]
pub extern "C" fn __adam_math_sin(x: f64) -> f64 {
    x.sin()
}

#[no_mangle]
pub extern "C" fn __adam_math_cos(x: f64) -> f64 {
    x.cos()
}

#[no_mangle]
pub extern "C" fn __adam_math_tan(x: f64) -> f64 {
    x.tan()
}

#[no_mangle]
pub extern "C" fn __adam_math_asin(x: f64) -> f64 {
    x.asin()
}

#[no_mangle]
pub extern "C" fn __adam_math_acos(x: f64) -> f64 {
    x.acos()
}

#[no_mangle]
pub extern "C" fn __adam_math_atan(x: f64) -> f64 {
    x.atan()
}

#[no_mangle]
pub extern "C" fn __adam_math_atan2(y: f64, x: f64) -> f64 {
    y.atan2(x)
}

// ================================================================
// Classification functions
// ================================================================

#[no_mangle]
pub extern "C" fn __adam_math_is_nan(x: f64) -> bool {
    x.is_nan()
}

#[no_mangle]
pub extern "C" fn __adam_math_is_infinite(x: f64) -> bool {
    x.is_infinite()
}

#[no_mangle]
pub extern "C" fn __adam_math_is_finite(x: f64) -> bool {
    x.is_finite()
}

// ================================================================
// Exponential and other functions
// ================================================================

#[no_mangle]
pub extern "C" fn __adam_math_exp(x: f64) -> f64 {
    x.exp()
}

#[no_mangle]
pub extern "C" fn __adam_math_hypot(x: f64, y: f64) -> f64 {
    x.hypot(y)
}

// ================================================================
// Tests
// ================================================================

#[cfg(test)]
mod tests {
    use super::*;

    const EPSILON: f64 = 1e-10;

    fn approx_eq(a: f64, b: f64) -> bool {
        (a - b).abs() < EPSILON
    }

    // --- Constants ---

    #[test]
    fn test_pi() {
        assert!(approx_eq(__adam_math_pi(), std::f64::consts::PI));
    }

    #[test]
    fn test_e() {
        assert!(approx_eq(__adam_math_e(), std::f64::consts::E));
    }

    #[test]
    fn test_tau() {
        assert!(approx_eq(__adam_math_tau(), 2.0 * std::f64::consts::PI));
    }

    #[test]
    fn test_infinity() {
        assert!(__adam_math_infinity().is_infinite());
        assert!(__adam_math_infinity() > 0.0);
    }

    #[test]
    fn test_neg_infinity() {
        assert!(__adam_math_neg_infinity().is_infinite());
        assert!(__adam_math_neg_infinity() < 0.0);
    }

    #[test]
    fn test_nan() {
        assert!(__adam_math_nan().is_nan());
    }

    // --- Absolute value ---

    #[test]
    fn test_abs_positive() {
        assert!(approx_eq(__adam_math_abs(3.14), 3.14));
    }

    #[test]
    fn test_abs_negative() {
        assert!(approx_eq(__adam_math_abs(-2.71), 2.71));
    }

    #[test]
    fn test_abs_zero() {
        assert!(approx_eq(__adam_math_abs(0.0), 0.0));
    }

    #[test]
    fn test_abs_int_positive() {
        assert_eq!(__adam_math_abs_int(42), 42);
    }

    #[test]
    fn test_abs_int_negative() {
        assert_eq!(__adam_math_abs_int(-99), 99);
    }

    #[test]
    fn test_abs_int_zero() {
        assert_eq!(__adam_math_abs_int(0), 0);
    }

    // --- Floor / Ceil / Round ---

    #[test]
    fn test_floor_positive() {
        assert!(approx_eq(__adam_math_floor(3.7), 3.0));
    }

    #[test]
    fn test_floor_negative() {
        assert!(approx_eq(__adam_math_floor(-3.2), -4.0));
    }

    #[test]
    fn test_ceil_positive() {
        assert!(approx_eq(__adam_math_ceil(3.2), 4.0));
    }

    #[test]
    fn test_ceil_negative() {
        assert!(approx_eq(__adam_math_ceil(-3.7), -3.0));
    }

    #[test]
    fn test_round_half_up() {
        assert!(approx_eq(__adam_math_round(2.5), 3.0));
    }

    #[test]
    fn test_round_down() {
        assert!(approx_eq(__adam_math_round(2.4), 2.0));
    }

    #[test]
    fn test_round_negative() {
        assert!(approx_eq(__adam_math_round(-2.5), -3.0));
    }

    // --- Min / Max / Clamp ---

    #[test]
    fn test_min() {
        assert!(approx_eq(__adam_math_min(1.0, 2.0), 1.0));
        assert!(approx_eq(__adam_math_min(-5.0, 3.0), -5.0));
    }

    #[test]
    fn test_max() {
        assert!(approx_eq(__adam_math_max(1.0, 2.0), 2.0));
        assert!(approx_eq(__adam_math_max(-5.0, 3.0), 3.0));
    }

    #[test]
    fn test_min_int() {
        assert_eq!(__adam_math_min_int(10, 20), 10);
        assert_eq!(__adam_math_min_int(-3, -7), -7);
    }

    #[test]
    fn test_max_int() {
        assert_eq!(__adam_math_max_int(10, 20), 20);
        assert_eq!(__adam_math_max_int(-3, -7), -3);
    }

    #[test]
    fn test_clamp_within_range() {
        assert!(approx_eq(__adam_math_clamp(5.0, 0.0, 10.0), 5.0));
    }

    #[test]
    fn test_clamp_below() {
        assert!(approx_eq(__adam_math_clamp(-5.0, 0.0, 10.0), 0.0));
    }

    #[test]
    fn test_clamp_above() {
        assert!(approx_eq(__adam_math_clamp(15.0, 0.0, 10.0), 10.0));
    }

    // --- Sqrt / Pow ---

    #[test]
    fn test_sqrt_4() {
        assert!(approx_eq(__adam_math_sqrt(4.0), 2.0));
    }

    #[test]
    fn test_sqrt_negative_is_nan() {
        assert!(__adam_math_sqrt(-1.0).is_nan());
    }

    #[test]
    fn test_pow_2_10() {
        assert!(approx_eq(__adam_math_pow(2.0, 10.0), 1024.0));
    }

    #[test]
    fn test_pow_fractional() {
        assert!(approx_eq(__adam_math_pow(9.0, 0.5), 3.0));
    }

    // --- Logarithms ---

    #[test]
    fn test_log_1_is_0() {
        assert!(approx_eq(__adam_math_log(1.0), 0.0));
    }

    #[test]
    fn test_log_e_is_1() {
        assert!(approx_eq(__adam_math_log(std::f64::consts::E), 1.0));
    }

    #[test]
    fn test_log2_8() {
        assert!(approx_eq(__adam_math_log2(8.0), 3.0));
    }

    #[test]
    fn test_log10_1000() {
        assert!(approx_eq(__adam_math_log10(1000.0), 3.0));
    }

    // --- Trigonometry ---

    #[test]
    fn test_sin_0() {
        assert!(approx_eq(__adam_math_sin(0.0), 0.0));
    }

    #[test]
    fn test_sin_pi_over_2() {
        assert!(approx_eq(__adam_math_sin(std::f64::consts::FRAC_PI_2), 1.0));
    }

    #[test]
    fn test_cos_0() {
        assert!(approx_eq(__adam_math_cos(0.0), 1.0));
    }

    #[test]
    fn test_cos_pi() {
        assert!(approx_eq(__adam_math_cos(std::f64::consts::PI), -1.0));
    }

    #[test]
    fn test_tan_0() {
        assert!(approx_eq(__adam_math_tan(0.0), 0.0));
    }

    #[test]
    fn test_asin_1() {
        assert!(approx_eq(
            __adam_math_asin(1.0),
            std::f64::consts::FRAC_PI_2
        ));
    }

    #[test]
    fn test_acos_1() {
        assert!(approx_eq(__adam_math_acos(1.0), 0.0));
    }

    #[test]
    fn test_atan_0() {
        assert!(approx_eq(__adam_math_atan(0.0), 0.0));
    }

    #[test]
    fn test_atan2_quadrant() {
        // atan2(1, 1) = PI/4
        assert!(approx_eq(
            __adam_math_atan2(1.0, 1.0),
            std::f64::consts::FRAC_PI_4
        ));
    }

    // --- Classification ---

    #[test]
    fn test_is_nan_true() {
        assert!(__adam_math_is_nan(f64::NAN));
    }

    #[test]
    fn test_is_nan_false() {
        assert!(!__adam_math_is_nan(1.0));
    }

    #[test]
    fn test_is_infinite_true() {
        assert!(__adam_math_is_infinite(f64::INFINITY));
        assert!(__adam_math_is_infinite(f64::NEG_INFINITY));
    }

    #[test]
    fn test_is_infinite_false() {
        assert!(!__adam_math_is_infinite(42.0));
    }

    #[test]
    fn test_is_finite_true() {
        assert!(__adam_math_is_finite(42.0));
        assert!(__adam_math_is_finite(-0.0));
    }

    #[test]
    fn test_is_finite_false() {
        assert!(!__adam_math_is_finite(f64::INFINITY));
        assert!(!__adam_math_is_finite(f64::NAN));
    }

    // --- Exp / Hypot ---

    #[test]
    fn test_exp_0() {
        assert!(approx_eq(__adam_math_exp(0.0), 1.0));
    }

    #[test]
    fn test_exp_1() {
        assert!(approx_eq(__adam_math_exp(1.0), std::f64::consts::E));
    }

    #[test]
    fn test_hypot_3_4() {
        assert!(approx_eq(__adam_math_hypot(3.0, 4.0), 5.0));
    }

    #[test]
    fn test_hypot_zero() {
        assert!(approx_eq(__adam_math_hypot(0.0, 0.0), 0.0));
    }

    // --- Special value interactions ---

    #[test]
    fn test_min_with_nan() {
        // f64::min propagates NaN per IEEE when using .min()
        // Rust's f64::min returns the other value if one is NaN
        let result = __adam_math_min(f64::NAN, 1.0);
        assert!(approx_eq(result, 1.0));
    }

    #[test]
    fn test_max_with_nan() {
        let result = __adam_math_max(f64::NAN, 1.0);
        assert!(approx_eq(result, 1.0));
    }

    #[test]
    fn test_abs_negative_zero() {
        // -0.0.abs() should be 0.0
        let result = __adam_math_abs(-0.0);
        assert!(result.is_sign_positive());
    }

    // ================================================================
    // Adversarial / exhaustive tests
    // ================================================================

    // --- NaN propagation ---

    #[test]
    fn test_abs_nan() {
        assert!(__adam_math_abs(f64::NAN).is_nan());
    }

    #[test]
    fn test_floor_nan() {
        assert!(__adam_math_floor(f64::NAN).is_nan());
    }

    #[test]
    fn test_ceil_nan() {
        assert!(__adam_math_ceil(f64::NAN).is_nan());
    }

    #[test]
    fn test_round_nan() {
        assert!(__adam_math_round(f64::NAN).is_nan());
    }

    #[test]
    fn test_sqrt_nan() {
        assert!(__adam_math_sqrt(f64::NAN).is_nan());
    }

    // --- Infinity handling ---

    #[test]
    fn test_floor_inf() {
        assert_eq!(__adam_math_floor(f64::INFINITY), f64::INFINITY);
    }

    #[test]
    fn test_ceil_neg_inf() {
        assert_eq!(__adam_math_ceil(f64::NEG_INFINITY), f64::NEG_INFINITY);
    }

    #[test]
    fn test_abs_neg_inf() {
        assert_eq!(__adam_math_abs(f64::NEG_INFINITY), f64::INFINITY);
    }

    #[test]
    fn test_round_inf() {
        assert_eq!(__adam_math_round(f64::INFINITY), f64::INFINITY);
    }

    // --- Trig edge values ---

    #[test]
    fn test_sin_2pi_approx_zero() {
        let result = __adam_math_sin(2.0 * std::f64::consts::PI);
        assert!(
            result.abs() < 1e-10,
            "sin(2*PI) should be ~0, got {}",
            result
        );
    }

    #[test]
    fn test_cos_2pi_approx_one() {
        let result = __adam_math_cos(2.0 * std::f64::consts::PI);
        assert!(
            approx_eq(result, 1.0),
            "cos(2*PI) should be ~1, got {}",
            result
        );
    }

    #[test]
    fn test_tan_pi_over_4_approx_one() {
        let result = __adam_math_tan(std::f64::consts::FRAC_PI_4);
        assert!(
            approx_eq(result, 1.0),
            "tan(PI/4) should be ~1, got {}",
            result
        );
    }

    // --- Pow edge cases ---

    #[test]
    fn test_pow_0_0() {
        // IEEE 754: 0^0 = 1
        assert!(approx_eq(__adam_math_pow(0.0, 0.0), 1.0));
    }

    #[test]
    fn test_pow_0_1() {
        assert!(approx_eq(__adam_math_pow(0.0, 1.0), 0.0));
    }

    #[test]
    fn test_pow_1_huge() {
        assert!(approx_eq(__adam_math_pow(1.0, 1e18), 1.0));
    }

    #[test]
    fn test_pow_2_neg1() {
        assert!(approx_eq(__adam_math_pow(2.0, -1.0), 0.5));
    }

    #[test]
    fn test_pow_neg1_nan() {
        // (-1)^NAN = NAN
        assert!(__adam_math_pow(-1.0, f64::NAN).is_nan());
    }

    // --- Log edge cases ---

    #[test]
    fn test_log_zero_neg_inf() {
        assert_eq!(__adam_math_log(0.0), f64::NEG_INFINITY);
    }

    #[test]
    fn test_log_neg1_nan() {
        assert!(__adam_math_log(-1.0).is_nan());
    }

    #[test]
    fn test_log2_1_is_0() {
        assert!(approx_eq(__adam_math_log2(1.0), 0.0));
    }

    #[test]
    fn test_log10_0_1_approx_neg1() {
        assert!(approx_eq(__adam_math_log10(0.1), -1.0));
    }

    // --- Inverse trig boundary ---

    #[test]
    fn test_asin_out_of_domain() {
        assert!(__adam_math_asin(2.0).is_nan());
    }

    #[test]
    fn test_acos_out_of_domain() {
        assert!(__adam_math_acos(2.0).is_nan());
    }

    #[test]
    fn test_asin_neg_out_of_domain() {
        assert!(__adam_math_asin(-1.5).is_nan());
    }

    #[test]
    fn test_acos_neg_out_of_domain() {
        assert!(__adam_math_acos(-1.5).is_nan());
    }

    // --- Clamp edge cases ---

    #[test]
    fn test_clamp_nan_val() {
        // clamp(NaN, 0, 1) -- Rust's f64::clamp with NaN produces NaN
        let result = __adam_math_clamp(f64::NAN, 0.0, 1.0);
        assert!(result.is_nan());
    }

    #[test]
    fn test_clamp_at_lo_boundary() {
        assert!(approx_eq(__adam_math_clamp(0.0, 0.0, 1.0), 0.0));
    }

    #[test]
    fn test_clamp_at_hi_boundary() {
        assert!(approx_eq(__adam_math_clamp(1.0, 0.0, 1.0), 1.0));
    }

    // --- Min/max with NaN ---

    #[test]
    fn test_min_nan_nan() {
        // Both NaN: Rust's f64::min returns NaN
        assert!(__adam_math_min(f64::NAN, f64::NAN).is_nan());
    }

    #[test]
    fn test_max_nan_nan() {
        assert!(__adam_math_max(f64::NAN, f64::NAN).is_nan());
    }

    #[test]
    fn test_min_second_nan() {
        // f64::min where second arg is NaN: Rust returns the NaN
        let result = __adam_math_min(1.0, f64::NAN);
        assert!(approx_eq(result, 1.0));
    }

    #[test]
    fn test_max_second_nan() {
        let result = __adam_math_max(1.0, f64::NAN);
        assert!(approx_eq(result, 1.0));
    }

    // --- Hypot edge cases ---

    #[test]
    fn test_hypot_inf_nan() {
        // IEEE 754: hypot(inf, NaN) = inf
        assert_eq!(__adam_math_hypot(f64::INFINITY, f64::NAN), f64::INFINITY);
    }

    #[test]
    fn test_hypot_zero_inf() {
        assert_eq!(__adam_math_hypot(0.0, f64::INFINITY), f64::INFINITY);
    }

    #[test]
    fn test_hypot_neg_inf() {
        assert_eq!(__adam_math_hypot(f64::NEG_INFINITY, 0.0), f64::INFINITY);
    }

    // --- Exp edge cases ---

    #[test]
    fn test_exp_overflow() {
        // exp(710) overflows to infinity
        assert_eq!(__adam_math_exp(710.0), f64::INFINITY);
    }

    #[test]
    fn test_exp_underflow() {
        // exp(-750) underflows to 0
        assert_eq!(__adam_math_exp(-750.0), 0.0);
    }

    #[test]
    fn test_exp_nan() {
        assert!(__adam_math_exp(f64::NAN).is_nan());
    }

    #[test]
    fn test_exp_neg_inf() {
        assert_eq!(__adam_math_exp(f64::NEG_INFINITY), 0.0);
    }

    #[test]
    fn test_exp_pos_inf() {
        assert_eq!(__adam_math_exp(f64::INFINITY), f64::INFINITY);
    }

    // --- Negative zero ---

    #[test]
    fn test_floor_neg_zero() {
        let result = __adam_math_floor(-0.0);
        assert_eq!(result, 0.0);
        assert!(
            result.is_sign_negative(),
            "floor(-0.0) should preserve sign"
        );
    }

    #[test]
    fn test_ceil_neg_zero() {
        let result = __adam_math_ceil(-0.0);
        assert_eq!(result, 0.0);
        assert!(result.is_sign_negative(), "ceil(-0.0) should preserve sign");
    }

    // --- Large angles ---

    #[test]
    fn test_sin_large_angle_finite() {
        let result = __adam_math_sin(1e10);
        assert!(result.is_finite(), "sin(1e10) should be finite");
        assert!(result >= -1.0 && result <= 1.0, "sin must be in [-1, 1]");
    }

    #[test]
    fn test_cos_large_angle_finite() {
        let result = __adam_math_cos(1e10);
        assert!(result.is_finite(), "cos(1e10) should be finite");
        assert!(result >= -1.0 && result <= 1.0, "cos must be in [-1, 1]");
    }

    // --- abs_int edge cases ---

    #[test]
    fn test_abs_int_min_wrapping() {
        // i64::MIN.wrapping_abs() == i64::MIN due to two's complement overflow
        assert_eq!(__adam_math_abs_int(i64::MIN), i64::MIN);
    }

    #[test]
    fn test_abs_int_max() {
        assert_eq!(__adam_math_abs_int(i64::MAX), i64::MAX);
    }

    #[test]
    fn test_abs_int_neg1() {
        assert_eq!(__adam_math_abs_int(-1), 1);
    }

    // --- min_int/max_int edge cases ---

    #[test]
    fn test_min_int_min_max() {
        assert_eq!(__adam_math_min_int(i64::MIN, i64::MAX), i64::MIN);
    }

    #[test]
    fn test_max_int_min_max() {
        assert_eq!(__adam_math_max_int(i64::MIN, i64::MAX), i64::MAX);
    }

    #[test]
    fn test_max_int_equal() {
        assert_eq!(__adam_math_max_int(0, 0), 0);
    }

    #[test]
    fn test_min_int_equal() {
        assert_eq!(__adam_math_min_int(7, 7), 7);
    }

    // --- Classification with special values ---

    #[test]
    fn test_is_nan_neg_zero() {
        assert!(!__adam_math_is_nan(-0.0));
    }

    #[test]
    fn test_is_infinite_neg_zero() {
        assert!(!__adam_math_is_infinite(-0.0));
    }

    #[test]
    fn test_is_finite_neg_zero() {
        assert!(__adam_math_is_finite(-0.0));
    }

    // --- Atan2 edge cases ---

    #[test]
    fn test_atan2_zero_zero() {
        assert!(approx_eq(__adam_math_atan2(0.0, 0.0), 0.0));
    }

    #[test]
    fn test_atan2_neg_zero_neg_zero() {
        let result = __adam_math_atan2(-0.0, -0.0);
        // atan2(-0, -0) = -PI per IEEE
        assert!(approx_eq(result, -std::f64::consts::PI));
    }

    // --- Sqrt edge cases ---

    #[test]
    fn test_sqrt_inf() {
        assert_eq!(__adam_math_sqrt(f64::INFINITY), f64::INFINITY);
    }

    #[test]
    fn test_sqrt_zero() {
        assert_eq!(__adam_math_sqrt(0.0), 0.0);
    }

    #[test]
    fn test_sqrt_neg_zero() {
        let result = __adam_math_sqrt(-0.0);
        assert_eq!(result, 0.0);
    }
}
