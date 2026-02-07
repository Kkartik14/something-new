//! Core lexer implementation.
//!
//! Scans Adam source text character by character, producing a stream of tokens.
//! Handles: identifiers, keywords, numbers (decimal/hex/octal/binary),
//! strings with interpolation, operators, significant newlines, and comments.

use crate::token::*;

/// The Adam lexer.
pub struct Lexer<'src> {
    source: &'src [u8],
    pos: usize,
    line: u32,
    column: u32,
    errors: Vec<LexError>,
    /// Stack tracking interpolation nesting depth.
    /// Each entry is the brace depth for that interpolation level.
    interpolation_stack: Vec<u32>,
    /// Pending tokens to emit on next calls (for string interpolation).
    pending: Vec<Token>,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            source: source.as_bytes(),
            pos: 0,
            line: 1,
            column: 1,
            errors: Vec::new(),
            interpolation_stack: Vec::new(),
            pending: Vec::new(),
        }
    }

    /// Tokenize the entire source, returning tokens and errors.
    pub fn tokenize(mut self) -> LexResult {
        let mut tokens = Vec::new();

        loop {
            self.skip_whitespace();
            self.skip_comments();
            // After skipping comments there may be more whitespace
            if self.skip_whitespace() || self.skip_comments() {
                continue;
            }

            if self.is_at_end() {
                // Insert final newline if last token needs it
                if let Some(last) = tokens.last() {
                    let last: &Token = last;
                    if last.kind.ends_statement() && last.kind != TokenKind::Newline {
                        tokens.push(Token::new(
                            TokenKind::Newline,
                            Span::new(self.pos as u32, self.pos as u32),
                            self.line,
                            self.column,
                        ));
                    }
                }
                tokens.push(Token::new(
                    TokenKind::Eof,
                    Span::new(self.pos as u32, self.pos as u32),
                    self.line,
                    self.column,
                ));
                break;
            }

            let token = self.lex_token();

            // Handle significant newlines (Go-style)
            if token.kind == TokenKind::Newline {
                if let Some(last) = tokens.last() {
                    if last.kind.ends_statement() {
                        tokens.push(token);
                    }
                    // else: discard non-significant newline
                }
                // No previous token: discard leading newline
            } else {
                tokens.push(token);
            }
        }

        LexResult {
            tokens,
            errors: self.errors,
        }
    }

    // === Character navigation ===

    fn is_at_end(&self) -> bool {
        self.pos >= self.source.len()
    }

    fn peek(&self) -> u8 {
        if self.is_at_end() {
            0
        } else {
            self.source[self.pos]
        }
    }

    fn peek_next(&self) -> u8 {
        if self.pos + 1 >= self.source.len() {
            0
        } else {
            self.source[self.pos + 1]
        }
    }

    fn advance(&mut self) -> u8 {
        let ch = self.source[self.pos];
        self.pos += 1;
        if ch == b'\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        ch
    }

    fn eat(&mut self, expected: u8) -> bool {
        if !self.is_at_end() && self.source[self.pos] == expected {
            self.advance();
            true
        } else {
            false
        }
    }

    // === Whitespace and comments ===

    /// Skip spaces and tabs (not newlines). Returns true if anything was skipped.
    fn skip_whitespace(&mut self) -> bool {
        let start = self.pos;
        while !self.is_at_end() {
            match self.peek() {
                b' ' | b'\t' | b'\r' => {
                    self.advance();
                }
                _ => break,
            }
        }
        self.pos > start
    }

    /// Skip comments. Returns true if a comment was skipped.
    fn skip_comments(&mut self) -> bool {
        if self.is_at_end() || self.peek() != b'/' {
            return false;
        }

        match self.peek_next() {
            b'/' => {
                // Line comment: skip to end of line
                while !self.is_at_end() && self.peek() != b'\n' {
                    self.advance();
                }
                true
            }
            b'*' => {
                // Block comment: /* ... */ with nesting
                let start_line = self.line;
                let start_col = self.column;
                let start_pos = self.pos;
                self.advance(); // /
                self.advance(); // *
                let mut depth: u32 = 1;

                while !self.is_at_end() && depth > 0 {
                    if self.peek() == b'/' && self.peek_next() == b'*' {
                        self.advance();
                        self.advance();
                        depth += 1;
                    } else if self.peek() == b'*' && self.peek_next() == b'/' {
                        self.advance();
                        self.advance();
                        depth -= 1;
                    } else {
                        self.advance();
                    }
                }

                if depth > 0 {
                    self.errors.push(LexError {
                        message: "unterminated block comment".to_string(),
                        line: start_line,
                        column: start_col,
                        span: Span::new(start_pos as u32, self.pos as u32),
                    });
                }
                true
            }
            _ => false,
        }
    }

    // === Main token dispatch ===

    fn lex_token(&mut self) -> Token {
        // Return pending token if one was queued (from string interpolation).
        if !self.pending.is_empty() {
            return self.pending.remove(0);
        }

        let start_pos = self.pos;
        let start_line = self.line;
        let start_col = self.column;

        let ch = self.peek();

        // Newline
        if ch == b'\n' {
            self.advance();
            return Token::new(
                TokenKind::Newline,
                Span::new(start_pos as u32, self.pos as u32),
                start_line,
                start_col,
            );
        }

        // Closing brace inside string interpolation
        if ch == b'}' && !self.interpolation_stack.is_empty() {
            let depth = self.interpolation_stack.last_mut().unwrap();
            if *depth == 0 {
                // End of interpolation — emit StringInterpolEnd, then resume
                // string lexing for the remainder after the }.
                self.advance(); // consume }
                self.interpolation_stack.pop();

                let end_token = Token::new(
                    TokenKind::StringInterpolEnd,
                    Span::new(start_pos as u32, self.pos as u32),
                    start_line,
                    start_col,
                );

                // Continue lexing the rest of the string.
                // The next char is either more string content, another {, or ".
                if !self.is_at_end() && self.peek() != b'"' {
                    // There's more string content — lex it and queue it.
                    let resume_pos = self.pos;
                    let resume_line = self.line;
                    let resume_col = self.column;
                    let resume_token = self.lex_string_continuation(resume_pos, resume_line, resume_col);
                    // Insert at front so it comes before any StringInterpolStart
                    // that lex_string_continuation may have queued.
                    self.pending.insert(0, resume_token);
                } else if !self.is_at_end() && self.peek() == b'"' {
                    // Closing quote — emit an empty string literal to close out.
                    self.advance(); // consume "
                    // No need to emit empty string — just consume the quote.
                }

                return end_token;
            } else {
                *depth -= 1;
            }
        }

        // Opening brace inside interpolation: track depth
        if ch == b'{' && !self.interpolation_stack.is_empty() {
            if let Some(depth) = self.interpolation_stack.last_mut() {
                *depth += 1;
            }
        }

        // String literal
        if ch == b'"' {
            return self.lex_string(start_pos, start_line, start_col);
        }

        // Char literal
        if ch == b'\'' {
            return self.lex_char(start_pos, start_line, start_col);
        }

        // Number literal
        if ch.is_ascii_digit() {
            return self.lex_number(start_pos, start_line, start_col);
        }

        // Identifier or keyword
        if is_ident_start(ch) {
            return self.lex_identifier(start_pos, start_line, start_col);
        }

        // Operators and delimiters
        self.lex_operator(start_pos, start_line, start_col)
    }

    // === Identifiers and keywords ===

    fn lex_identifier(&mut self, start_pos: usize, start_line: u32, start_col: u32) -> Token {
        while !self.is_at_end() && is_ident_continue(self.peek()) {
            self.advance();
        }

        let text = std::str::from_utf8(&self.source[start_pos..self.pos]).unwrap();
        let kind = match text {
            "fn" => TokenKind::Fn,
            "struct" => TokenKind::Struct,
            "enum" => TokenKind::Enum,
            "trait" => TokenKind::Trait,
            "impl" => TokenKind::Impl,
            "view" => TokenKind::View,
            "type" => TokenKind::TypeKw,
            "let" => TokenKind::Let,
            "mut" => TokenKind::Mut,
            "own" => TokenKind::Own,
            "pub" => TokenKind::Pub,
            "use" => TokenKind::Use,
            "mod" => TokenKind::Mod,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "match" => TokenKind::Match,
            "for" => TokenKind::For,
            "while" => TokenKind::While,
            "loop" => TokenKind::Loop,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            "return" => TokenKind::Return,
            "spawn" => TokenKind::Spawn,
            "select" => TokenKind::Select,
            "after" => TokenKind::After,
            "chan" => TokenKind::Chan,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "nil" => TokenKind::Nil,
            "self" => TokenKind::SelfValue,
            "Self" => TokenKind::SelfType,
            _ => TokenKind::Identifier(text.to_string()),
        };

        Token::new(
            kind,
            Span::new(start_pos as u32, self.pos as u32),
            start_line,
            start_col,
        )
    }

    // === Number literals ===

    fn lex_number(&mut self, start_pos: usize, start_line: u32, start_col: u32) -> Token {
        // Check for base prefix
        if self.peek() == b'0' && self.pos + 1 < self.source.len() {
            match self.source[self.pos + 1] {
                b'x' | b'X' => return self.lex_hex(start_pos, start_line, start_col),
                b'o' | b'O' => return self.lex_octal(start_pos, start_line, start_col),
                b'b' | b'B' => return self.lex_binary(start_pos, start_line, start_col),
                _ => {}
            }
        }

        // Decimal integer or float
        self.eat_digits();

        // Check for float
        if self.peek() == b'.' && self.peek_next().is_ascii_digit() {
            self.advance(); // .
            self.eat_digits();

            // Scientific notation
            if self.peek() == b'e' || self.peek() == b'E' {
                self.advance();
                if self.peek() == b'+' || self.peek() == b'-' {
                    self.advance();
                }
                self.eat_digits();
            }

            let text = self.number_text(start_pos);
            match text.parse::<f64>() {
                Ok(val) => Token::new(
                    TokenKind::FloatLiteral(val),
                    Span::new(start_pos as u32, self.pos as u32),
                    start_line,
                    start_col,
                ),
                Err(_) => self.error_token("invalid float literal", start_pos, start_line, start_col),
            }
        } else if self.peek() == b'e' || self.peek() == b'E' {
            // Scientific notation without decimal point: 1e10
            self.advance();
            if self.peek() == b'+' || self.peek() == b'-' {
                self.advance();
            }
            self.eat_digits();

            let text = self.number_text(start_pos);
            match text.parse::<f64>() {
                Ok(val) => Token::new(
                    TokenKind::FloatLiteral(val),
                    Span::new(start_pos as u32, self.pos as u32),
                    start_line,
                    start_col,
                ),
                Err(_) => self.error_token("invalid float literal", start_pos, start_line, start_col),
            }
        } else {
            let text = self.number_text(start_pos);
            match text.parse::<i64>() {
                Ok(val) => Token::new(
                    TokenKind::IntLiteral(val),
                    Span::new(start_pos as u32, self.pos as u32),
                    start_line,
                    start_col,
                ),
                Err(_) => self.error_token("integer literal overflow", start_pos, start_line, start_col),
            }
        }
    }

    fn lex_hex(&mut self, start_pos: usize, start_line: u32, start_col: u32) -> Token {
        self.advance(); // 0
        self.advance(); // x

        if !self.peek().is_ascii_hexdigit() {
            return self.error_token("expected hex digits after `0x`", start_pos, start_line, start_col);
        }

        while !self.is_at_end() && (self.peek().is_ascii_hexdigit() || self.peek() == b'_') {
            self.advance();
        }

        let text = self.number_text_from(start_pos + 2);
        match i64::from_str_radix(&text, 16) {
            Ok(val) => Token::new(
                TokenKind::IntLiteral(val),
                Span::new(start_pos as u32, self.pos as u32),
                start_line,
                start_col,
            ),
            Err(_) => self.error_token("hex literal overflow", start_pos, start_line, start_col),
        }
    }

    fn lex_octal(&mut self, start_pos: usize, start_line: u32, start_col: u32) -> Token {
        self.advance(); // 0
        self.advance(); // o

        if self.is_at_end() || !(self.peek() >= b'0' && self.peek() <= b'7') {
            return self.error_token("expected octal digits after `0o`", start_pos, start_line, start_col);
        }

        while !self.is_at_end() && ((self.peek() >= b'0' && self.peek() <= b'7') || self.peek() == b'_') {
            self.advance();
        }

        let text = self.number_text_from(start_pos + 2);
        match i64::from_str_radix(&text, 8) {
            Ok(val) => Token::new(
                TokenKind::IntLiteral(val),
                Span::new(start_pos as u32, self.pos as u32),
                start_line,
                start_col,
            ),
            Err(_) => self.error_token("octal literal overflow", start_pos, start_line, start_col),
        }
    }

    fn lex_binary(&mut self, start_pos: usize, start_line: u32, start_col: u32) -> Token {
        self.advance(); // 0
        self.advance(); // b

        if self.is_at_end() || (self.peek() != b'0' && self.peek() != b'1') {
            return self.error_token("expected binary digits after `0b`", start_pos, start_line, start_col);
        }

        while !self.is_at_end() && (self.peek() == b'0' || self.peek() == b'1' || self.peek() == b'_') {
            self.advance();
        }

        let text = self.number_text_from(start_pos + 2);
        match i64::from_str_radix(&text, 2) {
            Ok(val) => Token::new(
                TokenKind::IntLiteral(val),
                Span::new(start_pos as u32, self.pos as u32),
                start_line,
                start_col,
            ),
            Err(_) => self.error_token("binary literal overflow", start_pos, start_line, start_col),
        }
    }

    /// Eat consecutive digits and underscores.
    fn eat_digits(&mut self) {
        while !self.is_at_end() && (self.peek().is_ascii_digit() || self.peek() == b'_') {
            self.advance();
        }
    }

    /// Get number text from start_pos, stripping underscores.
    fn number_text(&self, start_pos: usize) -> String {
        self.number_text_from(start_pos)
    }

    fn number_text_from(&self, start_pos: usize) -> String {
        let raw = std::str::from_utf8(&self.source[start_pos..self.pos]).unwrap();
        raw.replace('_', "")
    }

    // === String literals with interpolation ===

    fn lex_string(&mut self, start_pos: usize, start_line: u32, start_col: u32) -> Token {
        self.advance(); // opening "

        let mut value = String::new();

        loop {
            if self.is_at_end() {
                self.errors.push(LexError {
                    message: "unterminated string literal".to_string(),
                    line: start_line,
                    column: start_col,
                    span: Span::new(start_pos as u32, self.pos as u32),
                });
                return Token::new(
                    TokenKind::Error("unterminated string".to_string()),
                    Span::new(start_pos as u32, self.pos as u32),
                    start_line,
                    start_col,
                );
            }

            match self.peek() {
                b'"' => {
                    self.advance(); // closing "
                    break;
                }
                b'{' => {
                    // String interpolation start.
                    let brace_pos = self.pos;
                    self.advance(); // {
                    self.interpolation_stack.push(0);

                    let interp_token = Token::new(
                        TokenKind::StringInterpolStart,
                        Span::new(brace_pos as u32, self.pos as u32),
                        self.line,
                        self.column - 1,
                    );

                    if !value.is_empty() {
                        // Queue the interpolation start for next call.
                        self.pending.push(interp_token);
                        return Token::new(
                            TokenKind::StringLiteral(value),
                            Span::new(start_pos as u32, brace_pos as u32),
                            start_line,
                            start_col,
                        );
                    }

                    // No accumulated text — emit interpolation start directly.
                    return interp_token;
                }
                b'\\' => {
                    self.advance(); // backslash
                    if self.is_at_end() {
                        self.errors.push(LexError {
                            message: "unterminated escape sequence".to_string(),
                            line: self.line,
                            column: self.column,
                            span: Span::new((self.pos - 1) as u32, self.pos as u32),
                        });
                        break;
                    }
                    match self.advance() {
                        b'n' => value.push('\n'),
                        b't' => value.push('\t'),
                        b'r' => value.push('\r'),
                        b'\\' => value.push('\\'),
                        b'"' => value.push('"'),
                        b'0' => value.push('\0'),
                        b'{' => value.push('{'),
                        b'u' => {
                            if let Some(c) = self.lex_unicode_escape() {
                                value.push(c);
                            }
                        }
                        ch => {
                            self.errors.push(LexError {
                                message: format!("invalid escape sequence: \\{}", ch as char),
                                line: self.line,
                                column: self.column - 1,
                                span: Span::new((self.pos - 2) as u32, self.pos as u32),
                            });
                            value.push(ch as char);
                        }
                    }
                }
                _ => {
                    // Regular character — handle UTF-8
                    let ch = self.advance();
                    if ch < 0x80 {
                        value.push(ch as char);
                    } else {
                        // Multi-byte UTF-8: rewind and decode
                        self.pos -= 1;
                        self.column -= 1;
                        if let Some(c) = self.advance_utf8() {
                            value.push(c);
                        }
                    }
                }
            }
        }

        Token::new(
            TokenKind::StringLiteral(value),
            Span::new(start_pos as u32, self.pos as u32),
            start_line,
            start_col,
        )
    }

    /// Continue lexing a string after an interpolation ends.
    /// Like `lex_string` but without consuming the opening `"`.
    fn lex_string_continuation(&mut self, start_pos: usize, start_line: u32, start_col: u32) -> Token {
        let mut value = String::new();

        loop {
            if self.is_at_end() {
                self.errors.push(LexError {
                    message: "unterminated string literal".to_string(),
                    line: start_line,
                    column: start_col,
                    span: Span::new(start_pos as u32, self.pos as u32),
                });
                return Token::new(
                    TokenKind::Error("unterminated string".to_string()),
                    Span::new(start_pos as u32, self.pos as u32),
                    start_line,
                    start_col,
                );
            }

            match self.peek() {
                b'"' => {
                    self.advance();
                    break;
                }
                b'{' => {
                    let brace_pos = self.pos;
                    self.advance();
                    self.interpolation_stack.push(0);

                    let interp_token = Token::new(
                        TokenKind::StringInterpolStart,
                        Span::new(brace_pos as u32, self.pos as u32),
                        self.line,
                        self.column - 1,
                    );

                    if !value.is_empty() {
                        self.pending.push(interp_token);
                        return Token::new(
                            TokenKind::StringLiteral(value),
                            Span::new(start_pos as u32, brace_pos as u32),
                            start_line,
                            start_col,
                        );
                    }

                    return interp_token;
                }
                b'\\' => {
                    self.advance();
                    if self.is_at_end() {
                        self.errors.push(LexError {
                            message: "unterminated escape sequence".to_string(),
                            line: self.line,
                            column: self.column,
                            span: Span::new((self.pos - 1) as u32, self.pos as u32),
                        });
                        break;
                    }
                    match self.advance() {
                        b'n' => value.push('\n'),
                        b't' => value.push('\t'),
                        b'r' => value.push('\r'),
                        b'\\' => value.push('\\'),
                        b'"' => value.push('"'),
                        b'0' => value.push('\0'),
                        b'{' => value.push('{'),
                        b'u' => {
                            if let Some(c) = self.lex_unicode_escape() {
                                value.push(c);
                            }
                        }
                        ch => {
                            self.errors.push(LexError {
                                message: format!("invalid escape sequence: \\{}", ch as char),
                                line: self.line,
                                column: self.column - 1,
                                span: Span::new((self.pos - 2) as u32, self.pos as u32),
                            });
                            value.push(ch as char);
                        }
                    }
                }
                _ => {
                    let ch = self.advance();
                    if ch < 0x80 {
                        value.push(ch as char);
                    } else {
                        self.pos -= 1;
                        self.column -= 1;
                        if let Some(c) = self.advance_utf8() {
                            value.push(c);
                        }
                    }
                }
            }
        }

        Token::new(
            TokenKind::StringLiteral(value),
            Span::new(start_pos as u32, self.pos as u32),
            start_line,
            start_col,
        )
    }

    /// Lex \u{XXXX} unicode escape.
    fn lex_unicode_escape(&mut self) -> Option<char> {
        if !self.eat(b'{') {
            self.errors.push(LexError {
                message: "expected `{` after `\\u`".to_string(),
                line: self.line,
                column: self.column,
                span: Span::new((self.pos - 1) as u32, self.pos as u32),
            });
            return None;
        }

        let start = self.pos;
        while !self.is_at_end() && self.peek() != b'}' && (self.pos - start) < 8 {
            if !self.peek().is_ascii_hexdigit() {
                self.errors.push(LexError {
                    message: "invalid hex digit in unicode escape".to_string(),
                    line: self.line,
                    column: self.column,
                    span: Span::new(self.pos as u32, (self.pos + 1) as u32),
                });
                return None;
            }
            self.advance();
        }

        if !self.eat(b'}') {
            self.errors.push(LexError {
                message: "expected `}` to close unicode escape".to_string(),
                line: self.line,
                column: self.column,
                span: Span::new(start as u32, self.pos as u32),
            });
            return None;
        }

        let hex = std::str::from_utf8(&self.source[start..self.pos - 1]).unwrap();
        match u32::from_str_radix(hex, 16) {
            Ok(code) => char::from_u32(code).or_else(|| {
                self.errors.push(LexError {
                    message: format!("invalid unicode code point: U+{:04X}", code),
                    line: self.line,
                    column: self.column,
                    span: Span::new(start as u32, self.pos as u32),
                });
                None
            }),
            Err(_) => {
                self.errors.push(LexError {
                    message: "invalid unicode escape".to_string(),
                    line: self.line,
                    column: self.column,
                    span: Span::new(start as u32, self.pos as u32),
                });
                None
            }
        }
    }

    /// Advance past one UTF-8 character and return it.
    fn advance_utf8(&mut self) -> Option<char> {
        let remaining = &self.source[self.pos..];
        match std::str::from_utf8(remaining) {
            Ok(s) => {
                if let Some(c) = s.chars().next() {
                    let len = c.len_utf8();
                    self.pos += len;
                    self.column += 1;
                    Some(c)
                } else {
                    None
                }
            }
            Err(e) => {
                // Try to decode at least the valid prefix
                let valid_len = e.valid_up_to();
                if valid_len > 0 {
                    let s = std::str::from_utf8(&remaining[..valid_len]).unwrap();
                    if let Some(c) = s.chars().next() {
                        let len = c.len_utf8();
                        self.pos += len;
                        self.column += 1;
                        return Some(c);
                    }
                }
                // Skip the invalid byte
                self.pos += 1;
                self.column += 1;
                self.errors.push(LexError {
                    message: "invalid UTF-8 byte".to_string(),
                    line: self.line,
                    column: self.column - 1,
                    span: Span::new((self.pos - 1) as u32, self.pos as u32),
                });
                None
            }
        }
    }

    // === Char literals ===

    fn lex_char(&mut self, start_pos: usize, start_line: u32, start_col: u32) -> Token {
        self.advance(); // opening '

        if self.is_at_end() || self.peek() == b'\n' {
            return self.error_token("unterminated character literal", start_pos, start_line, start_col);
        }

        let ch = if self.peek() == b'\\' {
            self.advance(); // backslash
            if self.is_at_end() {
                return self.error_token("unterminated escape in character literal", start_pos, start_line, start_col);
            }
            match self.advance() {
                b'n' => '\n',
                b't' => '\t',
                b'r' => '\r',
                b'\\' => '\\',
                b'\'' => '\'',
                b'0' => '\0',
                b'u' => {
                    match self.lex_unicode_escape() {
                        Some(c) => c,
                        None => {
                            // Skip to closing '
                            while !self.is_at_end() && self.peek() != b'\'' && self.peek() != b'\n' {
                                self.advance();
                            }
                            if !self.is_at_end() && self.peek() == b'\'' {
                                self.advance();
                            }
                            return self.error_token("invalid unicode escape in char literal", start_pos, start_line, start_col);
                        }
                    }
                }
                c => {
                    self.errors.push(LexError {
                        message: format!("invalid escape sequence in char: \\{}", c as char),
                        line: start_line,
                        column: start_col,
                        span: Span::new(start_pos as u32, self.pos as u32),
                    });
                    c as char
                }
            }
        } else if self.peek() < 0x80 {
            self.advance() as char
        } else {
            match self.advance_utf8() {
                Some(c) => c,
                None => return self.error_token("invalid character in char literal", start_pos, start_line, start_col),
            }
        };

        if !self.eat(b'\'') {
            return self.error_token("unterminated character literal, expected `'`", start_pos, start_line, start_col);
        }

        Token::new(
            TokenKind::CharLiteral(ch),
            Span::new(start_pos as u32, self.pos as u32),
            start_line,
            start_col,
        )
    }

    // === Operators and delimiters ===

    fn lex_operator(&mut self, start_pos: usize, start_line: u32, start_col: u32) -> Token {
        let ch = self.advance();
        let kind = match ch {
            b'+' => {
                if self.eat(b'=') { TokenKind::PlusAssign }
                else { TokenKind::Plus }
            }
            b'-' => {
                if self.eat(b'>') { TokenKind::Arrow }
                else if self.eat(b'=') { TokenKind::MinusAssign }
                else { TokenKind::Minus }
            }
            b'*' => {
                if self.eat(b'=') { TokenKind::StarAssign }
                else { TokenKind::Star }
            }
            b'/' => {
                if self.eat(b'=') { TokenKind::SlashAssign }
                else { TokenKind::Slash }
            }
            b'%' => {
                if self.eat(b'=') { TokenKind::PercentAssign }
                else { TokenKind::Percent }
            }
            b'=' => {
                if self.eat(b'=') { TokenKind::Eq }
                else if self.eat(b'>') { TokenKind::FatArrow }
                else { TokenKind::Assign }
            }
            b'!' => {
                if self.eat(b'=') { TokenKind::NotEq }
                else { TokenKind::Not }
            }
            b'<' => {
                if self.eat(b'=') { TokenKind::LtEq }
                else { TokenKind::Lt }
            }
            b'>' => {
                if self.eat(b'=') { TokenKind::GtEq }
                else { TokenKind::Gt }
            }
            b'&' => {
                if self.eat(b'&') { TokenKind::And }
                else { TokenKind::Ampersand }
            }
            b'|' => {
                if self.eat(b'|') { TokenKind::Or }
                else { TokenKind::Pipe }
            }
            b'.' => {
                if self.eat(b'.') { TokenKind::DotDot }
                else { TokenKind::Dot }
            }
            b':' => {
                if self.eat(b'=') { TokenKind::ColonAssign }
                else { TokenKind::Colon }
            }
            b'?' => TokenKind::Question,
            b'@' => TokenKind::At,
            b'(' => TokenKind::LParen,
            b')' => TokenKind::RParen,
            b'{' => TokenKind::LBrace,
            b'}' => TokenKind::RBrace,
            b'[' => TokenKind::LBracket,
            b']' => TokenKind::RBracket,
            b',' => TokenKind::Comma,
            b';' => TokenKind::Semicolon,
            _ => {
                self.errors.push(LexError {
                    message: format!("unexpected character: `{}`", ch as char),
                    line: start_line,
                    column: start_col,
                    span: Span::new(start_pos as u32, self.pos as u32),
                });
                TokenKind::Error(format!("unexpected `{}`", ch as char))
            }
        };

        Token::new(
            kind,
            Span::new(start_pos as u32, self.pos as u32),
            start_line,
            start_col,
        )
    }

    // === Helpers ===

    fn error_token(&mut self, message: &str, start_pos: usize, start_line: u32, start_col: u32) -> Token {
        self.errors.push(LexError {
            message: message.to_string(),
            line: start_line,
            column: start_col,
            span: Span::new(start_pos as u32, self.pos as u32),
        });
        Token::new(
            TokenKind::Error(message.to_string()),
            Span::new(start_pos as u32, self.pos as u32),
            start_line,
            start_col,
        )
    }
}

// === Character classification ===

fn is_ident_start(ch: u8) -> bool {
    ch.is_ascii_alphabetic() || ch == b'_'
}

fn is_ident_continue(ch: u8) -> bool {
    ch.is_ascii_alphanumeric() || ch == b'_'
}
