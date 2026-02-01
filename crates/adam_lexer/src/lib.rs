//! Adam Lexer — tokenizes Adam source code.
//!
//! Converts source text into a stream of tokens, handling:
//! - Keywords, identifiers, literals
//! - Operators and delimiters
//! - String interpolation
//! - Significant newlines (Go-style)
//! - Comments (line and nested block)

pub mod token;
pub mod lexer;

#[cfg(test)]
mod tests;
#[cfg(test)]
mod adversarial_tests;

pub use token::*;
pub use lexer::Lexer;
