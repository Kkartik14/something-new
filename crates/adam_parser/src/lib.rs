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
#[cfg(test)]
mod adversarial_tests;

pub use parser::{Parser, ParseError, ParseResult};
