//! Adam Parser — transforms token stream into AST.
//!
//! Handwritten recursive descent parser with Pratt parsing
//! for expressions (correct operator precedence).

pub mod parser;
mod types;
mod pattern;
mod expr;
mod stmt;
mod item;

#[cfg(test)]
mod tests;

pub use parser::{Parser, ParseError, ParseResult};
