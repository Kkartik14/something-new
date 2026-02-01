//! Adam Codegen — lowers Adam IR to LLVM IR for native compilation.

pub mod context;
pub mod types;
pub mod function;
pub mod memory;
pub mod concurrency;
pub mod link;
pub mod targets;

pub use context::CodeGen;
pub use link::{link_object, find_runtime_library};
pub use targets::{Platform, Arch, TargetConfig, OutputFormat};

#[cfg(test)]
mod tests;
