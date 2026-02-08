//! Adam Parser — transforms token stream into AST.
//!
//! Handwritten recursive descent parser with Pratt parsing
//! for expressions (correct operator precedence).

mod expr;
mod item;
pub mod parser;
mod pattern;
mod stmt;
mod types;

#[cfg(test)]
mod adversarial_tests;
#[cfg(test)]
mod tests;

pub use parser::{ParseError, ParseResult, Parser};
