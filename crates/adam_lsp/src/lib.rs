//! Adam LSP — Language Server Protocol implementation for IDE support.
//!
//! Provides:
//! - **Diagnostics** — real-time error reporting from lexer, parser, resolver, type checker
//! - **Go-to-definition** — jump to variable/function/type declarations
//! - **Hover** — type and documentation information on hover
//! - **Completions** — context-aware identifier completions
//! - **Document symbols** — outline of functions, structs, enums, traits
//! - **Protocol** — JSON-RPC 2.0 over stdin/stdout

pub mod analysis;
pub mod completion;
pub mod document;
pub mod goto;
pub mod hover;
pub mod symbols;
pub mod protocol;

pub use analysis::{AnalysisResult, Diagnostic, DiagnosticSeverity, Position, Range};
pub use completion::{CompletionItem, CompletionKind, complete_at};
pub use document::{DocumentStore, Document};
pub use goto::goto_definition;
pub use hover::hover_at;
pub use symbols::{DocumentSymbol, SymbolKind, document_symbols};

#[cfg(test)]
mod tests;
