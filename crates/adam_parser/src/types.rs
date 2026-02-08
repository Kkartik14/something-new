//! Type parsing — all type annotation forms.

use adam_ast::common::*;
use adam_ast::types::*;
use adam_lexer::TokenKind;

use crate::parser::{span, ParseError, Parser};

impl Parser {
    /// Parse a type annotation. Handles the `T ! E` result operator at the top level.
    pub(crate) fn parse_type(&mut self) -> Result<Spanned<Type>, ParseError> {
        let ty = self.parse_type_inner()?;

        // Check for result type: T ! E  (! has lowest precedence in types)
        if self.at(TokenKind::Not) {
            let start = ty.span;
            self.advance();
            let err_type = self.parse_type_inner()?;
            let s = start.merge(err_type.span);
            Ok(Spanned::new(
                Type::Result(Box::new(ResultType {
                    ok: ty,
                    err: err_type,
                })),
                s,
            ))
        } else {
            Ok(ty)
        }
    }

    /// Parse a type without the `!` result operator.
    fn parse_type_inner(&mut self) -> Result<Spanned<Type>, ParseError> {
        let start = self.current_span();

        match &self.current().kind {
            // Optional: ?T
            TokenKind::Question => {
                self.advance();
                let inner = self.parse_type_inner()?;
                let s = start.merge(inner.span);
                Ok(Spanned::new(Type::Optional(Box::new(inner)), s))
            }

            // Reference: &T or &mut T
            TokenKind::Ampersand => {
                self.advance();
                if self.at(TokenKind::Mut) {
                    self.advance();
                    let inner = self.parse_type_inner()?;
                    let s = start.merge(inner.span);
                    Ok(Spanned::new(Type::MutReference(Box::new(inner)), s))
                } else {
                    let inner = self.parse_type_inner()?;
                    let s = start.merge(inner.span);
                    Ok(Spanned::new(Type::Reference(Box::new(inner)), s))
                }
            }

            // Array: [T] or [T; N]
            TokenKind::LBracket => {
                self.advance();
                self.skip_newlines();
                let element = self.parse_type()?;
                self.skip_newlines();
                let size = if self.eat(TokenKind::Semicolon) {
                    self.skip_newlines();
                    Some(self.parse_expr()?)
                } else {
                    None
                };
                self.skip_newlines();
                let end_tok = self.expect(TokenKind::RBracket)?;
                let s = start.merge(span(end_tok.span));
                Ok(Spanned::new(
                    Type::Array(Box::new(ArrayType { element, size })),
                    s,
                ))
            }

            // Tuple: (T, U, V) or grouped type: (T)
            TokenKind::LParen => {
                self.advance();
                self.skip_newlines();
                let mut types = vec![];
                let mut had_comma = false;

                if !self.at(TokenKind::RParen) {
                    types.push(self.parse_type()?);
                    while self.eat(TokenKind::Comma) {
                        had_comma = true;
                        self.skip_newlines();
                        if self.at(TokenKind::RParen) {
                            break;
                        }
                        types.push(self.parse_type()?);
                    }
                }

                self.skip_newlines();
                let end_tok = self.expect(TokenKind::RParen)?;
                let s = start.merge(span(end_tok.span));

                if types.len() == 1 && !had_comma {
                    // Grouped type: (T) → T
                    Ok(types.into_iter().next().unwrap())
                } else {
                    Ok(Spanned::new(Type::Tuple(types), s))
                }
            }

            // Function type: fn(T, U) -> V
            TokenKind::Fn => {
                self.advance();
                self.expect(TokenKind::LParen)?;
                self.skip_newlines();
                let mut params = vec![];
                if !self.at(TokenKind::RParen) {
                    params.push(self.parse_type()?);
                    while self.eat(TokenKind::Comma) {
                        self.skip_newlines();
                        if self.at(TokenKind::RParen) {
                            break;
                        }
                        params.push(self.parse_type()?);
                    }
                }
                self.skip_newlines();
                self.expect(TokenKind::RParen)?;
                self.expect(TokenKind::Arrow)?;
                let return_type = self.parse_type()?;
                let s = start.merge(return_type.span);
                Ok(Spanned::new(
                    Type::Function(Box::new(FnType {
                        params,
                        return_type,
                    })),
                    s,
                ))
            }

            // Channel: chan[T]
            TokenKind::Chan => {
                self.advance();
                self.expect(TokenKind::LBracket)?;
                let inner = self.parse_type()?;
                let end_tok = self.expect(TokenKind::RBracket)?;
                let s = start.merge(span(end_tok.span));
                Ok(Spanned::new(Type::Channel(Box::new(inner)), s))
            }

            // Named type: Name, Name[T, U]
            TokenKind::Identifier(_) => {
                let name = self.expect_ident()?;
                let generic_args = self.parse_generic_args()?;

                let end = if generic_args.is_empty() {
                    name.span
                } else {
                    self.prev_span()
                };
                let s = start.merge(end);

                Ok(Spanned::new(
                    Type::Named(TypePath { name, generic_args }),
                    s,
                ))
            }

            // Self type
            TokenKind::SelfType => {
                let tok = self.advance();
                let s = span(tok.span);
                Ok(Spanned::new(
                    Type::Named(TypePath {
                        name: Ident::new("Self", s),
                        generic_args: vec![],
                    }),
                    s,
                ))
            }

            _ => {
                Err(self.error_at_current(&format!("expected type, found {}", self.current().kind)))
            }
        }
    }

    /// Parse generic arguments: [T, U, V]
    pub(crate) fn parse_generic_args(&mut self) -> Result<Vec<Spanned<Type>>, ParseError> {
        if !self.at(TokenKind::LBracket) {
            return Ok(vec![]);
        }
        self.advance();
        self.skip_newlines();
        let mut args = vec![];

        if !self.at(TokenKind::RBracket) {
            args.push(self.parse_type()?);
            while self.eat(TokenKind::Comma) {
                self.skip_newlines();
                if self.at(TokenKind::RBracket) {
                    break;
                }
                args.push(self.parse_type()?);
            }
        }

        self.skip_newlines();
        self.expect(TokenKind::RBracket)?;
        Ok(args)
    }

    /// Parse generic parameter definitions: [T, U: Bound, V: A + B]
    pub(crate) fn parse_generic_params(&mut self) -> Result<Vec<GenericParam>, ParseError> {
        if !self.at(TokenKind::LBracket) {
            return Ok(vec![]);
        }
        self.advance();
        self.skip_newlines();
        let mut params = vec![];

        if !self.at(TokenKind::RBracket) {
            params.push(self.parse_generic_param()?);
            while self.eat(TokenKind::Comma) {
                self.skip_newlines();
                if self.at(TokenKind::RBracket) {
                    break;
                }
                params.push(self.parse_generic_param()?);
            }
        }

        self.skip_newlines();
        self.expect(TokenKind::RBracket)?;
        Ok(params)
    }

    fn parse_generic_param(&mut self) -> Result<GenericParam, ParseError> {
        let name = self.expect_ident()?;
        let mut bounds = vec![];

        if self.eat(TokenKind::Colon) {
            bounds.push(self.expect_ident()?);
            while self.eat(TokenKind::Plus) {
                bounds.push(self.expect_ident()?);
            }
        }

        Ok(GenericParam { name, bounds })
    }
}
