//! Pattern parsing — match arms, let bindings, for loops.

use adam_ast::common::*;
use adam_ast::expr::Expr;
use adam_ast::pattern::*;
use adam_lexer::TokenKind;

use crate::parser::{span, ParseError, Parser};

impl Parser {
    /// Parse a pattern.
    pub(crate) fn parse_pattern(&mut self) -> Result<Spanned<Pattern>, ParseError> {
        let pat = self.parse_single_pattern()?;

        // Check for or-pattern: P1 | P2 | P3
        if self.at(TokenKind::Pipe) {
            let start = pat.span;
            let mut patterns = vec![pat];
            while self.eat(TokenKind::Pipe) {
                self.skip_newlines();
                patterns.push(self.parse_single_pattern()?);
            }
            let end = patterns.last().unwrap().span;
            Ok(Spanned::new(Pattern::Or(patterns), start.merge(end)))
        } else {
            Ok(pat)
        }
    }

    /// Parse a single pattern (without or).
    fn parse_single_pattern(&mut self) -> Result<Spanned<Pattern>, ParseError> {
        let start = self.current_span();

        match &self.current().kind {
            // Wildcard: _
            TokenKind::Identifier(name) if name == "_" => {
                self.advance();
                Ok(Spanned::new(Pattern::Wildcard, start))
            }

            // Rest: ..
            TokenKind::DotDot => {
                self.advance();
                Ok(Spanned::new(Pattern::Rest, start))
            }

            // Bool literal pattern
            TokenKind::True => {
                self.advance();
                Ok(Spanned::new(
                    Pattern::Literal(Spanned::new(Expr::BoolLiteral(true), start)),
                    start,
                ))
            }
            TokenKind::False => {
                self.advance();
                Ok(Spanned::new(
                    Pattern::Literal(Spanned::new(Expr::BoolLiteral(false), start)),
                    start,
                ))
            }

            // Nil literal pattern
            TokenKind::Nil => {
                self.advance();
                Ok(Spanned::new(
                    Pattern::Literal(Spanned::new(Expr::NilLiteral, start)),
                    start,
                ))
            }

            // Int literal pattern (possibly negative)
            TokenKind::Minus => {
                self.advance();
                if let TokenKind::IntLiteral(n) = &self.current().kind {
                    let n = -*n;
                    let end = self.current_span();
                    self.advance();
                    let s = start.merge(end);
                    Ok(Spanned::new(
                        Pattern::Literal(Spanned::new(Expr::IntLiteral(n), s)),
                        s,
                    ))
                } else if let TokenKind::FloatLiteral(n) = &self.current().kind {
                    let n = -*n;
                    let end = self.current_span();
                    self.advance();
                    let s = start.merge(end);
                    Ok(Spanned::new(
                        Pattern::Literal(Spanned::new(Expr::FloatLiteral(n), s)),
                        s,
                    ))
                } else {
                    Err(self.error_at_current("expected number after `-` in pattern"))
                }
            }

            TokenKind::IntLiteral(_) => {
                let (n, s) = self.eat_int_literal();
                Ok(Spanned::new(
                    Pattern::Literal(Spanned::new(Expr::IntLiteral(n), s)),
                    s,
                ))
            }
            TokenKind::FloatLiteral(_) => {
                let (n, s) = self.eat_float_literal();
                Ok(Spanned::new(
                    Pattern::Literal(Spanned::new(Expr::FloatLiteral(n), s)),
                    s,
                ))
            }

            // String literal pattern
            TokenKind::StringLiteral(_) => {
                let (val, s) = self.eat_string_literal();
                Ok(Spanned::new(
                    Pattern::Literal(Spanned::new(Expr::StringLiteral(val), s)),
                    s,
                ))
            }

            // Char literal pattern
            TokenKind::CharLiteral(_) => {
                let (val, s) = self.eat_char_literal();
                Ok(Spanned::new(
                    Pattern::Literal(Spanned::new(Expr::CharLiteral(val), s)),
                    s,
                ))
            }

            // Tuple pattern: (a, b, c)
            TokenKind::LParen => {
                self.advance();
                self.skip_newlines();
                let mut patterns = vec![];
                if !self.at(TokenKind::RParen) {
                    patterns.push(self.parse_pattern()?);
                    while self.eat(TokenKind::Comma) {
                        self.skip_newlines();
                        if self.at(TokenKind::RParen) {
                            break;
                        }
                        patterns.push(self.parse_pattern()?);
                    }
                }
                self.skip_newlines();
                let end_tok = self.expect(TokenKind::RParen)?;
                let s = start.merge(span(end_tok.span));
                Ok(Spanned::new(Pattern::Tuple(patterns), s))
            }

            // Identifier: binding, variant, or struct pattern
            TokenKind::Identifier(_) => {
                let name = self.expect_ident()?;

                // Check for variant pattern: Name(fields)
                if self.at(TokenKind::LParen) {
                    self.advance();
                    self.skip_newlines();
                    let mut fields = vec![];
                    if !self.at(TokenKind::RParen) {
                        fields.push(self.parse_pattern()?);
                        while self.eat(TokenKind::Comma) {
                            self.skip_newlines();
                            if self.at(TokenKind::RParen) {
                                break;
                            }
                            fields.push(self.parse_pattern()?);
                        }
                    }
                    self.skip_newlines();
                    let end_tok = self.expect(TokenKind::RParen)?;
                    let s = start.merge(span(end_tok.span));
                    Ok(Spanned::new(
                        Pattern::Variant(VariantPattern { name, fields }),
                        s,
                    ))
                }
                // Check for struct pattern: Name { fields }
                else if self.at(TokenKind::LBrace) {
                    self.advance();
                    self.skip_newlines();
                    let mut fields = vec![];
                    let mut has_rest = false;

                    while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
                        if self.at(TokenKind::DotDot) {
                            self.advance();
                            has_rest = true;
                            self.skip_newlines();
                            break;
                        }

                        let field_name = self.expect_ident()?;

                        // Check for `field: pattern` or just `field` (shorthand binding)
                        let pattern = if self.eat(TokenKind::Colon) {
                            self.skip_newlines();
                            self.parse_pattern()?
                        } else {
                            Spanned::new(Pattern::Binding(field_name.clone()), field_name.span)
                        };

                        fields.push((field_name, pattern));

                        if !self.eat(TokenKind::Comma) {
                            self.skip_newlines();
                            break;
                        }
                        self.skip_newlines();
                    }

                    self.skip_newlines();
                    let end_tok = self.expect(TokenKind::RBrace)?;
                    let s = start.merge(span(end_tok.span));
                    Ok(Spanned::new(
                        Pattern::Struct(StructPattern {
                            name,
                            fields,
                            has_rest,
                        }),
                        s,
                    ))
                }
                // Check for dot path: Enum.Variant(...)
                else if self.at(TokenKind::Dot) {
                    self.advance();
                    let variant_name = self.expect_ident()?;

                    if self.at(TokenKind::LParen) {
                        self.advance();
                        self.skip_newlines();
                        let mut fields = vec![];
                        if !self.at(TokenKind::RParen) {
                            fields.push(self.parse_pattern()?);
                            while self.eat(TokenKind::Comma) {
                                self.skip_newlines();
                                if self.at(TokenKind::RParen) {
                                    break;
                                }
                                fields.push(self.parse_pattern()?);
                            }
                        }
                        self.skip_newlines();
                        let end_tok = self.expect(TokenKind::RParen)?;
                        let s = start.merge(span(end_tok.span));
                        // Use the variant name, with the enum name as context
                        // For now, encode as Variant with a qualified name
                        let qualified = Ident::new(
                            format!("{}.{}", name.name, variant_name.name),
                            name.span.merge(variant_name.span),
                        );
                        Ok(Spanned::new(
                            Pattern::Variant(VariantPattern {
                                name: qualified,
                                fields,
                            }),
                            s,
                        ))
                    } else {
                        // Enum.Variant without parens (e.g., Option.None)
                        let s = start.merge(variant_name.span);
                        let qualified = Ident::new(
                            format!("{}.{}", name.name, variant_name.name),
                            name.span.merge(variant_name.span),
                        );
                        Ok(Spanned::new(
                            Pattern::Variant(VariantPattern {
                                name: qualified,
                                fields: vec![],
                            }),
                            s,
                        ))
                    }
                }
                // Simple binding
                else {
                    let s = name.span;
                    Ok(Spanned::new(Pattern::Binding(name), s))
                }
            }

            _ => {
                Err(self
                    .error_at_current(&format!("expected pattern, found {}", self.current().kind)))
            }
        }
    }

    // ---- Literal extraction helpers ----

    pub(crate) fn eat_int_literal(&mut self) -> (i64, Span) {
        if let TokenKind::IntLiteral(n) = &self.current().kind {
            let n = *n;
            let s = self.current_span();
            self.advance();
            (n, s)
        } else {
            panic!("expected int literal");
        }
    }

    pub(crate) fn eat_float_literal(&mut self) -> (f64, Span) {
        if let TokenKind::FloatLiteral(n) = &self.current().kind {
            let n = *n;
            let s = self.current_span();
            self.advance();
            (n, s)
        } else {
            panic!("expected float literal");
        }
    }

    pub(crate) fn eat_string_literal(&mut self) -> (String, Span) {
        if let TokenKind::StringLiteral(val) = &self.current().kind {
            let val = val.clone();
            let s = self.current_span();
            self.advance();
            (val, s)
        } else {
            panic!("expected string literal");
        }
    }

    pub(crate) fn eat_char_literal(&mut self) -> (char, Span) {
        if let TokenKind::CharLiteral(c) = &self.current().kind {
            let c = *c;
            let s = self.current_span();
            self.advance();
            (c, s)
        } else {
            panic!("expected char literal");
        }
    }
}
