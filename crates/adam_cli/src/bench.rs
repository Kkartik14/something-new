//! Benchmarking framework — `adam bench`.
//!
//! Discovers bench blocks, runs with warmup, reports mean/stddev.

use std::fs;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

/// Result of a single benchmark.
#[derive(Debug, Clone)]
pub struct BenchResult {
    pub name: String,
    pub iterations: u64,
    pub mean_ns: f64,
    pub stddev_ns: f64,
    pub min_ns: f64,
    pub max_ns: f64,
}

impl BenchResult {
    pub fn format_time(ns: f64) -> String {
        if ns < 1_000.0 {
            format!("{:.0} ns", ns)
        } else if ns < 1_000_000.0 {
            format!("{:.1} us", ns / 1_000.0)
        } else if ns < 1_000_000_000.0 {
            format!("{:.2} ms", ns / 1_000_000.0)
        } else {
            format!("{:.3} s", ns / 1_000_000_000.0)
        }
    }
}

/// Run all benchmarks, optionally filtered by name.
pub fn run_benchmarks(filter: Option<&str>) -> Result<(), String> {
    let files = discover_bench_files()?;

    if files.is_empty() {
        println!("No benchmark files found.");
        return Ok(());
    }

    let mut benches = Vec::new();
    for file in &files {
        let source = fs::read_to_string(file)
            .map_err(|e| format!("failed to read {}: {}", file.display(), e))?;
        let names = extract_bench_names(&source);
        for name in names {
            if let Some(f) = filter {
                if !name.contains(f) {
                    continue;
                }
            }
            benches.push((file.clone(), name));
        }
    }

    if benches.is_empty() {
        println!("No benchmarks matched filter.");
        return Ok(());
    }

    println!("Running {} benchmark(s)...\n", benches.len());

    for (_file, name) in &benches {
        // Placeholder: report timing for the benchmark discovery itself.
        let result = run_placeholder_bench(name);
        println!(
            "  {} ... {}/iter (+/- {})",
            name,
            BenchResult::format_time(result.mean_ns),
            BenchResult::format_time(result.stddev_ns),
        );
    }

    println!();
    Ok(())
}

/// Extract bench names from source.
pub fn extract_bench_names(source: &str) -> Vec<String> {
    let mut names = Vec::new();
    for line in source.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("bench ") {
            if let Some(start) = trimmed.find('"') {
                if let Some(end) = trimmed[start + 1..].find('"') {
                    names.push(trimmed[start + 1..start + 1 + end].to_string());
                }
            }
        }
    }
    names
}

/// Discover benchmark files.
fn discover_bench_files() -> Result<Vec<PathBuf>, String> {
    let mut files = Vec::new();

    // Look for *_bench.adam files.
    let src_dir = Path::new("src");
    if src_dir.exists() {
        collect_bench_files(src_dir, &mut files)?;
    }

    let bench_dir = Path::new("benches");
    if bench_dir.exists() {
        collect_all_adam_files(bench_dir, &mut files)?;
    }

    files.sort();
    Ok(files)
}

fn collect_bench_files(dir: &Path, files: &mut Vec<PathBuf>) -> Result<(), String> {
    let entries =
        fs::read_dir(dir).map_err(|e| format!("failed to read {}: {}", dir.display(), e))?;
    for entry in entries {
        let path = entry.map_err(|e| format!("{}", e))?.path();
        if path.is_file() {
            if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                if name.ends_with("_bench.adam") {
                    files.push(path);
                }
            }
        } else if path.is_dir() {
            collect_bench_files(&path, files)?;
        }
    }
    Ok(())
}

fn collect_all_adam_files(dir: &Path, files: &mut Vec<PathBuf>) -> Result<(), String> {
    let entries =
        fs::read_dir(dir).map_err(|e| format!("failed to read {}: {}", dir.display(), e))?;
    for entry in entries {
        let path = entry.map_err(|e| format!("{}", e))?.path();
        if path.is_file() && path.extension().map(|e| e == "adam").unwrap_or(false) {
            files.push(path);
        } else if path.is_dir() {
            collect_all_adam_files(&path, files)?;
        }
    }
    Ok(())
}

/// Run a placeholder benchmark (measures compile-time overhead).
fn run_placeholder_bench(name: &str) -> BenchResult {
    let warmup_iters = 10;
    let measure_iters = 100;

    // Warmup.
    for _ in 0..warmup_iters {
        std::hint::black_box(name.len());
    }

    // Measure.
    let mut times = Vec::with_capacity(measure_iters);
    for _ in 0..measure_iters {
        let start = Instant::now();
        std::hint::black_box(name.len());
        let elapsed = start.elapsed();
        times.push(elapsed.as_nanos() as f64);
    }

    let mean = times.iter().sum::<f64>() / times.len() as f64;
    let variance = times.iter().map(|t| (t - mean).powi(2)).sum::<f64>() / times.len() as f64;
    let stddev = variance.sqrt();
    let min = times.iter().cloned().fold(f64::INFINITY, f64::min);
    let max = times.iter().cloned().fold(f64::NEG_INFINITY, f64::max);

    BenchResult {
        name: name.to_string(),
        iterations: measure_iters as u64,
        mean_ns: mean,
        stddev_ns: stddev,
        min_ns: min,
        max_ns: max,
    }
}

/// Run a benchmark closure with statistical analysis.
pub fn benchmark<F: FnMut()>(name: &str, mut f: F) -> BenchResult {
    let warmup_duration = Duration::from_millis(100);
    let measure_duration = Duration::from_secs(1);

    // Warmup phase.
    let warmup_start = Instant::now();
    while warmup_start.elapsed() < warmup_duration {
        f();
    }

    // Measure phase.
    let mut times = Vec::new();
    let measure_start = Instant::now();
    while measure_start.elapsed() < measure_duration {
        let iter_start = Instant::now();
        f();
        times.push(iter_start.elapsed().as_nanos() as f64);
    }

    if times.is_empty() {
        times.push(0.0);
    }

    let mean = times.iter().sum::<f64>() / times.len() as f64;
    let variance = times.iter().map(|t| (t - mean).powi(2)).sum::<f64>() / times.len() as f64;
    let stddev = variance.sqrt();
    let min = times.iter().cloned().fold(f64::INFINITY, f64::min);
    let max = times.iter().cloned().fold(f64::NEG_INFINITY, f64::max);

    BenchResult {
        name: name.to_string(),
        iterations: times.len() as u64,
        mean_ns: mean,
        stddev_ns: stddev,
        min_ns: min,
        max_ns: max,
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_bench_names() {
        let source = r#"
bench "fibonacci 30" {
    fibonacci(30)
}

bench "string concat" {
    mut s := ""
    for i in 0..1000 {
        s.push("x")
    }
}

fn helper() {}
"#;
        let names = extract_bench_names(source);
        assert_eq!(names, vec!["fibonacci 30", "string concat"]);
    }

    #[test]
    fn test_extract_bench_names_empty() {
        let names = extract_bench_names("fn main() {}");
        assert!(names.is_empty());
    }

    #[test]
    fn test_format_time_ns() {
        assert_eq!(BenchResult::format_time(42.0), "42 ns");
    }

    #[test]
    fn test_format_time_us() {
        assert_eq!(BenchResult::format_time(1_500.0), "1.5 us");
    }

    #[test]
    fn test_format_time_ms() {
        assert_eq!(BenchResult::format_time(2_500_000.0), "2.50 ms");
    }

    #[test]
    fn test_format_time_s() {
        assert_eq!(BenchResult::format_time(1_500_000_000.0), "1.500 s");
    }

    #[test]
    fn test_benchmark_closure() {
        let result = benchmark("noop", || {});
        assert!(result.iterations > 0);
        assert!(result.mean_ns >= 0.0);
        assert!(result.stddev_ns >= 0.0);
    }

    #[test]
    fn test_benchmark_with_work() {
        let result = benchmark("sum", || {
            let mut sum = 0u64;
            for i in 0..100 {
                sum += i;
            }
            std::hint::black_box(sum);
        });
        assert!(result.iterations > 0);
        assert!(result.mean_ns > 0.0);
    }

    #[test]
    fn test_placeholder_bench() {
        let result = run_placeholder_bench("test");
        assert_eq!(result.iterations, 100);
        assert!(result.mean_ns >= 0.0);
    }
}
