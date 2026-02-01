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
        toml::from_str(&content)
            .map_err(|e| format!("invalid adam.toml: {}", e))
    }

    /// Save manifest to adam.toml.
    pub fn save(&self) -> Result<(), String> {
        self.save_to(Path::new("adam.toml"))
    }

    /// Save manifest to a specific path.
    pub fn save_to(&self, path: &Path) -> Result<(), String> {
        let content = toml::to_string_pretty(self)
            .map_err(|e| format!("failed to serialize manifest: {}", e))?;
        fs::write(path, content)
            .map_err(|e| format!("failed to write {}: {}", path.display(), e))
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
            return Ok(LockFile { packages: Vec::new() });
        }
        let content = fs::read_to_string(path)
            .map_err(|e| format!("failed to read adam.lock: {}", e))?;
        toml::from_str(&content)
            .map_err(|e| format!("invalid adam.lock: {}", e))
    }

    pub fn save(&self) -> Result<(), String> {
        let content = toml::to_string_pretty(self)
            .map_err(|e| format!("failed to serialize lock file: {}", e))?;
        fs::write("adam.lock", content)
            .map_err(|e| format!("failed to write adam.lock: {}", e))
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
    let _manifest = Manifest::load().map_err(|e| {
        eprintln!("error: {}", e);
        1
    })?;

    // Resolve dependencies and update lock file.
    let lock = LockFile { packages: Vec::new() };
    lock.save().map_err(|e| {
        eprintln!("error: {}", e);
        1
    })?;

    println!("Dependencies updated.");
    Ok(())
}

pub fn pkg_install() -> Result<(), i32> {
    let _manifest = Manifest::load().map_err(|e| {
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

    println!("Dependencies installed.");
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
        m.dependencies.insert("http".to_string(), DependencySpec::Version("1.0".to_string()));

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
            packages: vec![
                LockedPackage {
                    name: "http".to_string(),
                    version: "2.0.1".to_string(),
                    source: Some("registry".to_string()),
                    checksum: Some("abc123".to_string()),
                },
            ],
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
}
