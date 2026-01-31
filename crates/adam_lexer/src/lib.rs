//! Adam Lexer — tokenizes Adam source code.
//!
//! Converts source text into a stream of tokens, handling:
//! - Keywords, identifiers, literals
//! - Operators and delimiters
//! - String interpolation
//! - Significant newlines (Go-style)
//! - Comments (line and nested block)
