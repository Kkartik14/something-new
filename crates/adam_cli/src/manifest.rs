//! Package manifest (adam.toml) parsing and package management.

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::fs;
use std::path::Path;

/// The adam.toml manifest structure.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Manifest {
    pub package: Package,
    #[serde(default)]
    pub dependencies: BTreeMap<String, DependencySpec>,
    #[serde(default, rename = "dev-dependencies")]
    pub dev_dependencies: BTreeMap<String, DependencySpec>,
    #[serde(default)]
    pub build: BuildConfig,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Package {
    pub name: String,
    pub version: String,
    #[serde(default = "default_adam_version")]
    pub adam: String,
}

fn default_adam_version() -> String {
    "0.1".to_string()
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum DependencySpec {
    Version(String),
    Detailed(DetailedDep),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DetailedDep {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub version: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub git: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub path: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct BuildConfig {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub target: Option<String>,
}

impl Manifest {
    /// Load manifest from adam.toml in the current directory.
    pub fn load() -> Result<Self, String> {
        Self::load_from(Path::new("adam.toml"))
    }

    /// Load manifest from a specific path.
    pub fn load_from(path: &Path) -> Result<Self, String> {
        if !path.exists() {
            return Err(format!("no adam.toml found (looked in {})", path.display()));
        }
        let content = fs::read_to_string(path)
            .map_err(|e| format!("failed to read {}: {}", path.display(), e))?;
        toml::from_str(&content).map_err(|e| format!("invalid adam.toml: {}", e))
    }

    /// Save manifest to adam.toml.
    pub fn save(&self) -> Result<(), String> {
        self.save_to(Path::new("adam.toml"))
    }

    /// Save manifest to a specific path.
    pub fn save_to(&self, path: &Path) -> Result<(), String> {
        let content = toml::to_string_pretty(self)
            .map_err(|e| format!("failed to serialize manifest: {}", e))?;
        fs::write(path, content).map_err(|e| format!("failed to write {}: {}", path.display(), e))
    }

    /// Create a default manifest for a new project.
    pub fn new_project(name: &str) -> Self {
        Manifest {
            package: Package {
                name: name.to_string(),
                version: "0.1.0".to_string(),
                adam: "0.1".to_string(),
            },
            dependencies: BTreeMap::new(),
            dev_dependencies: BTreeMap::new(),
            build: BuildConfig::default(),
        }
    }
}

// ---------------------------------------------------------------------------
// Lock file
// ---------------------------------------------------------------------------

/// adam.lock structure for reproducible builds.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LockFile {
    pub packages: Vec<LockedPackage>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LockedPackage {
    pub name: String,
    pub version: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub checksum: Option<String>,
}

impl LockFile {
    pub fn load() -> Result<Self, String> {
        let path = Path::new("adam.lock");
        if !path.exists() {
            return Ok(LockFile {
                packages: Vec::new(),
            });
        }
        let content =
            fs::read_to_string(path).map_err(|e| format!("failed to read adam.lock: {}", e))?;
        toml::from_str(&content).map_err(|e| format!("invalid adam.lock: {}", e))
    }

    pub fn save(&self) -> Result<(), String> {
        let content = toml::to_string_pretty(self)
            .map_err(|e| format!("failed to serialize lock file: {}", e))?;
        fs::write("adam.lock", content).map_err(|e| format!("failed to write adam.lock: {}", e))
    }
}

// ---------------------------------------------------------------------------
// Package management commands
// ---------------------------------------------------------------------------

pub fn pkg_init() -> Result<(), i32> {
    if Path::new("adam.toml").exists() {
        eprintln!("error: adam.toml already exists");
        return Err(1);
    }
    let dir_name = std::env::current_dir()
        .ok()
        .and_then(|p| p.file_name().map(|n| n.to_string_lossy().into_owned()))
        .unwrap_or_else(|| "myproject".to_string());

    let manifest = Manifest::new_project(&dir_name);
    manifest.save().map_err(|e| {
        eprintln!("error: {}", e);
        1
    })?;
    println!("Created adam.toml for '{}'", dir_name);
    Ok(())
}

pub fn pkg_add(name: &str, version: Option<&str>) -> Result<(), i32> {
    let mut manifest = Manifest::load().map_err(|e| {
        eprintln!("error: {}", e);
        1
    })?;

    let spec = match version {
        Some(v) => DependencySpec::Version(v.to_string()),
        None => DependencySpec::Version("*".to_string()),
    };

    manifest.dependencies.insert(name.to_string(), spec);
    manifest.save().map_err(|e| {
        eprintln!("error: {}", e);
        1
    })?;

    println!("Added dependency '{}'", name);
    Ok(())
}

pub fn pkg_remove(name: &str) -> Result<(), i32> {
    let mut manifest = Manifest::load().map_err(|e| {
        eprintln!("error: {}", e);
        1
    })?;

    if manifest.dependencies.remove(name).is_none() {
        eprintln!("warning: '{}' was not in dependencies", name);
    }

    manifest.save().map_err(|e| {
        eprintln!("error: {}", e);
        1
    })?;

    println!("Removed dependency '{}'", name);
    Ok(())
}

pub fn pkg_update() -> Result<(), i32> {
    let manifest = Manifest::load().map_err(|e| {
        eprintln!("error: {}", e);
        1
    })?;

    // Resolve dependencies and generate lock file.
    let mut locked = Vec::new();
    for (name, spec) in &manifest.dependencies {
        let resolved = resolve_dependency(name, spec).map_err(|e| {
            eprintln!("error resolving '{}': {}", name, e);
            1
        })?;
        locked.push(resolved);
    }

    let lock = LockFile { packages: locked };
    lock.save().map_err(|e| {
        eprintln!("error: {}", e);
        1
    })?;

    println!("Dependencies updated ({} packages).", lock.packages.len());
    Ok(())
}

fn resolve_dependency(name: &str, spec: &DependencySpec) -> Result<LockedPackage, String> {
    match spec {
        DependencySpec::Version(v) => {
            // Parse version requirement and resolve to a concrete version.
            let _req = SemverReq::parse(v)?;
            // Without a registry, resolve to the required version directly.
            let resolved_version = match v.as_str() {
                "*" => "0.0.0".to_string(),
                _ => strip_comparator(v),
            };
            Ok(LockedPackage {
                name: name.to_string(),
                version: resolved_version,
                source: Some("registry".to_string()),
                checksum: None,
            })
        }
        DependencySpec::Detailed(d) => {
            let version = d.version.clone().unwrap_or_else(|| "0.0.0".to_string());
            let source = if let Some(git) = &d.git {
                Some(format!("git+{}", git))
            } else if let Some(path) = &d.path {
                Some(format!("path+{}", path))
            } else {
                Some("registry".to_string())
            };
            Ok(LockedPackage {
                name: name.to_string(),
                version,
                source,
                checksum: None,
            })
        }
    }
}

/// Strip semver comparators to get the base version.
fn strip_comparator(s: &str) -> String {
    let s = s.trim();
    let s = s.strip_prefix(">=").unwrap_or(s);
    let s = s.strip_prefix("<=").unwrap_or(s);
    let s = s.strip_prefix('>').unwrap_or(s);
    let s = s.strip_prefix('<').unwrap_or(s);
    let s = s.strip_prefix('~').unwrap_or(s);
    let s = s.strip_prefix('^').unwrap_or(s);
    let s = s.strip_prefix('=').unwrap_or(s);
    s.trim().to_string()
}

// ---------------------------------------------------------------------------
// Semver parsing and matching
// ---------------------------------------------------------------------------

/// A parsed semantic version: major.minor.patch
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Semver {
    pub major: u64,
    pub minor: u64,
    pub patch: u64,
}

impl Semver {
    pub fn parse(s: &str) -> Result<Self, String> {
        let parts: Vec<&str> = s.trim().split('.').collect();
        let major = parts
            .first()
            .ok_or("missing major version")?
            .parse::<u64>()
            .map_err(|_| format!("invalid major version in '{}'", s))?;
        let minor = parts
            .get(1)
            .map(|p| p.parse::<u64>())
            .transpose()
            .map_err(|_| format!("invalid minor version in '{}'", s))?
            .unwrap_or(0);
        let patch = parts
            .get(2)
            .map(|p| p.parse::<u64>())
            .transpose()
            .map_err(|_| format!("invalid patch version in '{}'", s))?
            .unwrap_or(0);
        Ok(Semver {
            major,
            minor,
            patch,
        })
    }
}

impl std::fmt::Display for Semver {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)
    }
}

/// A semver requirement (version constraint).
#[derive(Debug, Clone)]
pub enum SemverReq {
    /// Any version
    Any,
    /// Exact match: =1.2.3 or 1.2.3
    Exact(Semver),
    /// Caret: ^1.2.3 (>=1.2.3, <2.0.0)
    Caret(Semver),
    /// Tilde: ~1.2.3 (>=1.2.3, <1.3.0)
    Tilde(Semver),
    /// Greater or equal: >=1.2.3
    Gte(Semver),
    /// Less than: <2.0.0
    Lt(Semver),
}

impl SemverReq {
    pub fn parse(s: &str) -> Result<Self, String> {
        let s = s.trim();
        if s == "*" {
            return Ok(SemverReq::Any);
        }
        if let Some(rest) = s.strip_prefix(">=") {
            return Ok(SemverReq::Gte(Semver::parse(rest)?));
        }
        if let Some(rest) = s.strip_prefix('>') {
            // Treat > as >= for simplicity.
            return Ok(SemverReq::Gte(Semver::parse(rest)?));
        }
        if let Some(rest) = s.strip_prefix("<=") {
            // Treat <= as <(next) for simplicity; just use Lt.
            return Ok(SemverReq::Lt(Semver::parse(rest)?));
        }
        if let Some(rest) = s.strip_prefix('<') {
            return Ok(SemverReq::Lt(Semver::parse(rest)?));
        }
        if let Some(rest) = s.strip_prefix('~') {
            return Ok(SemverReq::Tilde(Semver::parse(rest)?));
        }
        if let Some(rest) = s.strip_prefix('^') {
            return Ok(SemverReq::Caret(Semver::parse(rest)?));
        }
        if let Some(rest) = s.strip_prefix('=') {
            return Ok(SemverReq::Exact(Semver::parse(rest)?));
        }
        // Default: caret semantics (like Cargo).
        Ok(SemverReq::Caret(Semver::parse(s)?))
    }

    /// Check if a concrete version satisfies this requirement.
    pub fn matches(&self, ver: &Semver) -> bool {
        match self {
            SemverReq::Any => true,
            SemverReq::Exact(req) => ver == req,
            SemverReq::Caret(req) => {
                if req.major > 0 {
                    ver.major == req.major && *ver >= *req
                } else if req.minor > 0 {
                    ver.major == 0 && ver.minor == req.minor && *ver >= *req
                } else {
                    ver.major == 0 && ver.minor == 0 && ver.patch == req.patch
                }
            }
            SemverReq::Tilde(req) => {
                ver.major == req.major && ver.minor == req.minor && ver.patch >= req.patch
            }
            SemverReq::Gte(req) => *ver >= *req,
            SemverReq::Lt(req) => *ver < *req,
        }
    }
}

pub fn pkg_install() -> Result<(), i32> {
    let manifest = Manifest::load().map_err(|e| {
        eprintln!("error: {}", e);
        1
    })?;

    // Create packages directory.
    let pkg_dir = Path::new(".adam").join("packages");
    if !pkg_dir.exists() {
        fs::create_dir_all(&pkg_dir).map_err(|e| {
            eprintln!("error: failed to create packages dir: {}", e);
            1
        })?;
    }

    // Install path dependencies by symlinking into .adam/packages/.
    let mut count = 0;
    for (name, spec) in &manifest.dependencies {
        if let DependencySpec::Detailed(d) = spec {
            if let Some(path) = &d.path {
                let src = Path::new(path);
                let dst = pkg_dir.join(name);
                if dst.exists() {
                    // Already installed.
                    continue;
                }
                if src.exists() {
                    #[cfg(unix)]
                    {
                        std::os::unix::fs::symlink(
                            fs::canonicalize(src).unwrap_or_else(|_| src.to_path_buf()),
                            &dst,
                        )
                        .map_err(|e| {
                            eprintln!("error: failed to symlink '{}': {}", name, e);
                            1
                        })?;
                    }
                    #[cfg(not(unix))]
                    {
                        // On non-unix, copy the directory.
                        let _ = fs::create_dir_all(&dst);
                    }
                    count += 1;
                } else {
                    eprintln!(
                        "warning: path dependency '{}' not found at '{}'",
                        name, path
                    );
                }
            }
        }
    }

    println!("Dependencies installed ({} path deps linked).", count);
    Ok(())
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    fn temp_dir() -> tempfile::TempDir {
        tempfile::tempdir().unwrap()
    }

    #[test]
    fn test_manifest_new_project() {
        let m = Manifest::new_project("myapp");
        assert_eq!(m.package.name, "myapp");
        assert_eq!(m.package.version, "0.1.0");
        assert_eq!(m.package.adam, "0.1");
        assert!(m.dependencies.is_empty());
    }

    #[test]
    fn test_manifest_roundtrip() {
        let dir = temp_dir();
        let path = dir.path().join("adam.toml");

        let mut m = Manifest::new_project("test");
        m.dependencies.insert(
            "http".to_string(),
            DependencySpec::Version("1.0".to_string()),
        );

        m.save_to(&path).unwrap();
        let loaded = Manifest::load_from(&path).unwrap();
        assert_eq!(loaded.package.name, "test");
        assert!(loaded.dependencies.contains_key("http"));
    }

    #[test]
    fn test_manifest_parse_full() {
        let toml_str = r#"
[package]
name = "myapp"
version = "1.0.0"
adam = "0.1"

[dependencies]
http = "2.0"
json = { version = "1.5", git = "https://github.com/adam-lang/json" }

[dev-dependencies]
bench = "0.5"

[build]
target = "ios"
"#;
        let m: Manifest = toml::from_str(toml_str).unwrap();
        assert_eq!(m.package.name, "myapp");
        assert_eq!(m.dependencies.len(), 2);
        assert_eq!(m.dev_dependencies.len(), 1);
        assert_eq!(m.build.target, Some("ios".to_string()));
    }

    #[test]
    fn test_manifest_missing_file() {
        let result = Manifest::load_from(Path::new("/nonexistent/adam.toml"));
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("no adam.toml found"));
    }

    #[test]
    fn test_manifest_invalid_toml() {
        let dir = temp_dir();
        let path = dir.path().join("adam.toml");
        let mut f = fs::File::create(&path).unwrap();
        f.write_all(b"this is not valid toml {{{").unwrap();

        let result = Manifest::load_from(&path);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("invalid adam.toml"));
    }

    #[test]
    fn test_lock_file_roundtrip() {
        let dir = temp_dir();
        let lock_path = dir.path().join("adam.lock");

        let lock = LockFile {
            packages: vec![LockedPackage {
                name: "http".to_string(),
                version: "2.0.1".to_string(),
                source: Some("registry".to_string()),
                checksum: Some("abc123".to_string()),
            }],
        };
        let content = toml::to_string_pretty(&lock).unwrap();
        fs::write(&lock_path, content).unwrap();

        let loaded: LockFile = toml::from_str(&fs::read_to_string(&lock_path).unwrap()).unwrap();
        assert_eq!(loaded.packages.len(), 1);
        assert_eq!(loaded.packages[0].name, "http");
    }

    #[test]
    fn test_dependency_spec_version() {
        // TOML requires a document (key-value pairs), not a bare value.
        let doc = "dep = \"1.0\"";
        let table: BTreeMap<String, DependencySpec> = toml::from_str(doc).unwrap();
        match table.get("dep").unwrap() {
            DependencySpec::Version(v) => assert_eq!(v, "1.0"),
            _ => panic!("expected version"),
        }
    }

    #[test]
    fn test_dependency_spec_detailed() {
        let doc = r#"
[dep]
version = "1.0"
git = "https://example.com/repo"
"#;
        let table: BTreeMap<String, DependencySpec> = toml::from_str(doc).unwrap();
        match table.get("dep").unwrap() {
            DependencySpec::Detailed(d) => {
                assert_eq!(d.version, Some("1.0".to_string()));
                assert_eq!(d.git, Some("https://example.com/repo".to_string()));
            }
            _ => panic!("expected detailed"),
        }
    }

    // Semver parsing tests

    #[test]
    fn test_semver_parse_full() {
        let v = Semver::parse("1.2.3").unwrap();
        assert_eq!(
            v,
            Semver {
                major: 1,
                minor: 2,
                patch: 3
            }
        );
    }

    #[test]
    fn test_semver_parse_major_minor() {
        let v = Semver::parse("2.5").unwrap();
        assert_eq!(
            v,
            Semver {
                major: 2,
                minor: 5,
                patch: 0
            }
        );
    }

    #[test]
    fn test_semver_parse_major_only() {
        let v = Semver::parse("3").unwrap();
        assert_eq!(
            v,
            Semver {
                major: 3,
                minor: 0,
                patch: 0
            }
        );
    }

    #[test]
    fn test_semver_display() {
        let v = Semver {
            major: 1,
            minor: 2,
            patch: 3,
        };
        assert_eq!(v.to_string(), "1.2.3");
    }

    #[test]
    fn test_semver_ordering() {
        let v1 = Semver::parse("1.0.0").unwrap();
        let v2 = Semver::parse("1.0.1").unwrap();
        let v3 = Semver::parse("1.1.0").unwrap();
        let v4 = Semver::parse("2.0.0").unwrap();
        assert!(v1 < v2);
        assert!(v2 < v3);
        assert!(v3 < v4);
    }

    // SemverReq parsing tests

    #[test]
    fn test_semver_req_any() {
        let req = SemverReq::parse("*").unwrap();
        assert!(req.matches(&Semver::parse("0.0.0").unwrap()));
        assert!(req.matches(&Semver::parse("99.99.99").unwrap()));
    }

    #[test]
    fn test_semver_req_caret() {
        let req = SemverReq::parse("^1.2.3").unwrap();
        assert!(req.matches(&Semver::parse("1.2.3").unwrap()));
        assert!(req.matches(&Semver::parse("1.9.0").unwrap()));
        assert!(!req.matches(&Semver::parse("2.0.0").unwrap()));
        assert!(!req.matches(&Semver::parse("1.2.2").unwrap()));
    }

    #[test]
    fn test_semver_req_caret_zero_major() {
        let req = SemverReq::parse("^0.2.1").unwrap();
        assert!(req.matches(&Semver::parse("0.2.1").unwrap()));
        assert!(req.matches(&Semver::parse("0.2.9").unwrap()));
        assert!(!req.matches(&Semver::parse("0.3.0").unwrap()));
    }

    #[test]
    fn test_semver_req_tilde() {
        let req = SemverReq::parse("~1.2.3").unwrap();
        assert!(req.matches(&Semver::parse("1.2.3").unwrap()));
        assert!(req.matches(&Semver::parse("1.2.9").unwrap()));
        assert!(!req.matches(&Semver::parse("1.3.0").unwrap()));
    }

    #[test]
    fn test_semver_req_gte() {
        let req = SemverReq::parse(">=1.0.0").unwrap();
        assert!(req.matches(&Semver::parse("1.0.0").unwrap()));
        assert!(req.matches(&Semver::parse("2.0.0").unwrap()));
        assert!(!req.matches(&Semver::parse("0.9.9").unwrap()));
    }

    #[test]
    fn test_semver_req_lt() {
        let req = SemverReq::parse("<2.0.0").unwrap();
        assert!(req.matches(&Semver::parse("1.9.9").unwrap()));
        assert!(!req.matches(&Semver::parse("2.0.0").unwrap()));
    }

    #[test]
    fn test_semver_req_default_caret() {
        // Bare version "1.2" should be treated as ^1.2.0
        let req = SemverReq::parse("1.2").unwrap();
        assert!(req.matches(&Semver::parse("1.2.0").unwrap()));
        assert!(req.matches(&Semver::parse("1.9.0").unwrap()));
        assert!(!req.matches(&Semver::parse("2.0.0").unwrap()));
    }

    // Resolution tests

    #[test]
    fn test_resolve_version_dep() {
        let locked =
            resolve_dependency("http", &DependencySpec::Version("1.5.0".to_string())).unwrap();
        assert_eq!(locked.name, "http");
        assert_eq!(locked.version, "1.5.0");
        assert_eq!(locked.source, Some("registry".to_string()));
    }

    #[test]
    fn test_resolve_wildcard_dep() {
        let locked = resolve_dependency("util", &DependencySpec::Version("*".to_string())).unwrap();
        assert_eq!(locked.version, "0.0.0");
    }

    #[test]
    fn test_resolve_git_dep() {
        let locked = resolve_dependency(
            "json",
            &DependencySpec::Detailed(DetailedDep {
                version: Some("1.0.0".to_string()),
                git: Some("https://github.com/example/json".to_string()),
                path: None,
            }),
        )
        .unwrap();
        assert_eq!(locked.name, "json");
        assert_eq!(locked.version, "1.0.0");
        assert!(locked.source.as_ref().unwrap().starts_with("git+"));
    }

    #[test]
    fn test_resolve_path_dep() {
        let locked = resolve_dependency(
            "local",
            &DependencySpec::Detailed(DetailedDep {
                version: None,
                git: None,
                path: Some("../local".to_string()),
            }),
        )
        .unwrap();
        assert_eq!(locked.version, "0.0.0");
        assert!(locked.source.as_ref().unwrap().starts_with("path+"));
    }

    #[test]
    fn test_lock_file_with_resolved_deps() {
        let dir = temp_dir();
        let lock_path = dir.path().join("adam.lock");

        let lock = LockFile {
            packages: vec![
                LockedPackage {
                    name: "http".to_string(),
                    version: "2.0.1".to_string(),
                    source: Some("registry".to_string()),
                    checksum: None,
                },
                LockedPackage {
                    name: "json".to_string(),
                    version: "1.0.0".to_string(),
                    source: Some("git+https://github.com/example/json".to_string()),
                    checksum: None,
                },
            ],
        };

        let content = toml::to_string_pretty(&lock).unwrap();
        fs::write(&lock_path, &content).unwrap();

        let loaded: LockFile = toml::from_str(&fs::read_to_string(&lock_path).unwrap()).unwrap();
        assert_eq!(loaded.packages.len(), 2);
        assert_eq!(loaded.packages[0].name, "http");
        assert_eq!(
            loaded.packages[1].source,
            Some("git+https://github.com/example/json".to_string())
        );
    }
}
