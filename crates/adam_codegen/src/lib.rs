//! Adam Codegen — lowers Adam IR to LLVM IR for native compilation.

pub mod concurrency;
pub mod context;
pub mod function;
pub mod link;
pub mod memory;
pub mod targets;
pub mod types;

pub use context::CodeGen;
pub use link::{find_runtime_library, link_object, link_with_target_config};
pub use targets::{Arch, OutputFormat, Platform, TargetConfig};

#[cfg(test)]
mod pipeline_stress_tests;
#[cfg(test)]
mod tests;
