//! Parser infrastructure — token navigation, error handling, synchronization.

use adam_lexer::{Token, TokenKind, Span as LexSpan};
use adam_ast::common::{Span, Ident};

/// Convert lexer span to AST span.
pub(crate) fn span(lex: LexSpan) -> Span {
    Span::new(lex.start, lex.end)
}

/// Parse error.
#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

impl ParseError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}..{}] {}", self.span.start, self.span.end, self.message)
    }
}

/// Result of parsing a source file.
#[derive(Debug)]
pub struct ParseResult {
    pub ast: adam_ast::item::SourceFile,
    pub errors: Vec<ParseError>,
}

/// The parser.
pub struct Parser {
    pub(crate) tokens: Vec<Token>,
    pub(crate) pos: usize,
    pub(crate) errors: Vec<ParseError>,
    /// When true, don't parse struct literals (avoids ambiguity in if/while conditions).
    pub(crate) no_struct_literal: bool,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            pos: 0,
            errors: vec![],
            no_struct_literal: false,
        }
    }

    pub fn parse(mut self) -> ParseResult {
        let ast = self.parse_source_file();
        ParseResult {
            ast,
            errors: self.errors,
        }
    }

    // ---- Token navigation ----

    pub(crate) fn current(&self) -> &Token {
        self.tokens
            .get(self.pos)
            .unwrap_or_else(|| self.tokens.last().unwrap())
    }

    pub(crate) fn peek(&self) -> &Token {
        self.tokens
            .get(self.pos + 1)
            .unwrap_or_else(|| self.tokens.last().unwrap())
    }

    #[allow(dead_code)]
    pub(crate) fn peek_nth(&self, n: usize) -> &Token {
        self.tokens
            .get(self.pos + n)
            .unwrap_or_else(|| self.tokens.last().unwrap())
    }

    pub(crate) fn advance(&mut self) -> Token {
        let tok = self.current().clone();
        if !matches!(tok.kind, TokenKind::Eof) {
            self.pos += 1;
        }
        tok
    }

    pub(crate) fn prev_span(&self) -> Span {
        if self.pos > 0 {
            span(self.tokens[self.pos - 1].span)
        } else {
            Span::new(0, 0)
        }
    }

    pub(crate) fn current_span(&self) -> Span {
        span(self.current().span)
    }

    // ---- Matching helpers ----

    pub(crate) fn at(&self, kind: TokenKind) -> bool {
        self.current().kind == kind
    }

    pub(crate) fn at_ident(&self) -> bool {
        matches!(self.current().kind, TokenKind::Identifier(_))
    }

    pub(crate) fn at_string(&self) -> bool {
        matches!(self.current().kind, TokenKind::StringLiteral(_))
    }

    pub(crate) fn eat(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    pub(crate) fn eat_ident(&mut self) -> Option<Ident> {
        if let TokenKind::Identifier(name) = &self.current().kind {
            let name = name.clone();
            let s = self.current_span();
            self.advance();
            Some(Ident::new(name, s))
        } else {
            None
        }
    }

    pub(crate) fn expect_ident(&mut self) -> Result<Ident, ParseError> {
        if let Some(ident) = self.eat_ident() {
            Ok(ident)
        } else {
            Err(self.error_at_current(&format!(
                "expected identifier, found {}",
                self.current().kind
            )))
        }
    }

    pub(crate) fn expect(&mut self, kind: TokenKind) -> Result<Token, ParseError> {
        if self.current().kind == kind {
            Ok(self.advance())
        } else {
            Err(self.error_at_current(&format!(
                "expected {}, found {}",
                kind,
                self.current().kind
            )))
        }
    }

    // ---- Newline handling ----

    pub(crate) fn skip_newlines(&mut self) {
        while self.at(TokenKind::Newline) {
            self.advance();
        }
    }

    pub(crate) fn expect_terminator(&mut self) -> Result<(), ParseError> {
        if self.at(TokenKind::Newline) {
            self.advance();
            Ok(())
        } else if self.at(TokenKind::Eof)
            || self.at(TokenKind::RBrace)
            || self.at(TokenKind::Semicolon)
        {
            if self.at(TokenKind::Semicolon) {
                self.advance();
            }
            Ok(())
        } else {
            Err(self.error_at_current(&format!(
                "expected newline or `}}`, found {}",
                self.current().kind
            )))
        }
    }

    // ---- Error handling ----

    pub(crate) fn error_at_current(&self, message: &str) -> ParseError {
        ParseError::new(message, self.current_span())
    }

    pub(crate) fn record_error(&mut self, err: ParseError) {
        self.errors.push(err);
    }

    #[allow(dead_code)]
    pub(crate) fn error(&mut self, message: &str) {
        let err = self.error_at_current(message);
        self.record_error(err);
    }

    // ---- Error recovery ----

    pub(crate) fn synchronize(&mut self) {
        loop {
            match &self.current().kind {
                TokenKind::Fn
                | TokenKind::Struct
                | TokenKind::Enum
                | TokenKind::Trait
                | TokenKind::Impl
                | TokenKind::View
                | TokenKind::Use
                | TokenKind::Mod
                | TokenKind::Pub
                | TokenKind::Eof => break,
                TokenKind::Newline => {
                    self.advance();
                    break;
                }
                _ => {
                    self.advance();
                }
            }
        }
    }

    #[allow(dead_code)]
    pub(crate) fn synchronize_to_brace(&mut self) {
        let mut depth = 0i32;
        loop {
            match &self.current().kind {
                TokenKind::LBrace => {
                    depth += 1;
                    self.advance();
                }
                TokenKind::RBrace if depth > 0 => {
                    depth -= 1;
                    self.advance();
                }
                TokenKind::RBrace | TokenKind::Eof => break,
                _ => {
                    self.advance();
                }
            }
        }
    }
}
