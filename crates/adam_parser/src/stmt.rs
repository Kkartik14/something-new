//! Statement and block parsing.

use adam_ast::common::*;
use adam_ast::stmt::*;
use adam_lexer::TokenKind;

use crate::parser::{ParseError, Parser};

impl Parser {
    /// Parse a block: { stmts }
    pub(crate) fn parse_block(&mut self) -> Result<Block, ParseError> {
        self.expect(TokenKind::LBrace)?;
        self.skip_newlines();

        let mut stmts = vec![];
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            match self.parse_stmt() {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => {
                    self.record_error(err);
                    self.synchronize();
                }
            }
            self.skip_newlines();
        }

        self.expect(TokenKind::RBrace)?;
        Ok(Block { stmts })
    }

    /// Parse a statement.
    pub(crate) fn parse_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        self.skip_newlines();
        let start = self.current_span();

        match &self.current().kind {
            // Items that can appear in blocks
            TokenKind::Fn
            | TokenKind::Struct
            | TokenKind::Enum
            | TokenKind::Trait
            | TokenKind::Impl
            | TokenKind::View
            | TokenKind::Use
            | TokenKind::Mod
            | TokenKind::Pub => {
                let item = self.parse_item()?;
                let s = item.span;
                Ok(Spanned::new(Stmt::Item(item), s))
            }

            // For loop
            TokenKind::For => self.parse_for_stmt(),

            // While loop
            TokenKind::While => self.parse_while_stmt(),

            // Loop
            TokenKind::Loop => {
                self.advance();
                let block = self.parse_block()?;
                let s = start.merge(self.prev_span());
                Ok(Spanned::new(Stmt::Loop(block), s))
            }

            // Optional let keyword
            TokenKind::Let => {
                self.advance(); // consume 'let'
                self.parse_let_stmt(start)
            }

            // Mutable binding: mut name := ... or mut name: Type = ...
            TokenKind::Mut if matches!(self.peek().kind, TokenKind::Identifier(_)) => {
                self.advance(); // consume 'mut'
                self.parse_let_stmt_with_mutability(start, Mutability::Mutable)
            }

            // Identifier at start — could be let binding (x := ..., x: T = ...) or expression
            TokenKind::Identifier(_) => {
                // Check for := (colon-assign let binding)
                if self.peek().kind == TokenKind::ColonAssign {
                    self.parse_let_stmt(start)
                }
                // Check for : Type = (typed let binding)
                else if self.peek().kind == TokenKind::Colon {
                    // Peek further: is this name: Type = expr?
                    // We need to distinguish from name: (some other use of colon).
                    // In statement context, identifier : always starts a typed let.
                    self.parse_let_stmt(start)
                }
                // Otherwise, expression statement
                else {
                    self.parse_expr_stmt(start)
                }
            }

            // Everything else is an expression statement
            _ => self.parse_expr_stmt(start),
        }
    }

    /// Parse a let binding. Handles both := and typed forms.
    fn parse_let_stmt(&mut self, start: Span) -> Result<Spanned<Stmt>, ParseError> {
        // Check for 'mut' after optional 'let'
        let mutability = if self.at(TokenKind::Mut) {
            self.advance();
            Mutability::Mutable
        } else {
            Mutability::Immutable
        };

        self.parse_let_stmt_with_mutability(start, mutability)
    }

    fn parse_let_stmt_with_mutability(
        &mut self,
        start: Span,
        mutability: Mutability,
    ) -> Result<Spanned<Stmt>, ParseError> {
        let name = self.expect_ident()?;

        if self.eat(TokenKind::ColonAssign) {
            // x := expr
            let value = self.parse_expr()?;
            let s = start.merge(value.span);
            self.eat_optional_terminator();
            Ok(Spanned::new(
                Stmt::Let(LetStmt {
                    mutability,
                    name,
                    ty: None,
                    value,
                    is_colon_assign: true,
                }),
                s,
            ))
        } else if self.eat(TokenKind::Colon) {
            // x: Type = expr
            let ty = self.parse_type()?;
            self.expect(TokenKind::Assign)?;
            let value = self.parse_expr()?;
            let s = start.merge(value.span);
            self.eat_optional_terminator();
            Ok(Spanned::new(
                Stmt::Let(LetStmt {
                    mutability,
                    name,
                    ty: Some(ty),
                    value,
                    is_colon_assign: false,
                }),
                s,
            ))
        } else {
            Err(self.error_at_current(&format!(
                "expected `:=` or `:` after variable name, found {}",
                self.current().kind
            )))
        }
    }

    /// Parse for-in loop: for binding in iterable { body }
    fn parse_for_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.current_span();
        self.advance(); // consume 'for'

        let binding = self.expect_ident()?;

        // Expect 'in' keyword (it's an identifier, not a keyword token)
        if let TokenKind::Identifier(name) = &self.current().kind {
            if name == "in" {
                self.advance();
            } else {
                return Err(self.error_at_current(&format!("expected `in`, found `{}`", name)));
            }
        } else {
            return Err(
                self.error_at_current(&format!("expected `in`, found {}", self.current().kind))
            );
        }

        let iterable = self.parse_expr_no_struct()?;
        let body = self.parse_block()?;

        let s = start.merge(self.prev_span());
        Ok(Spanned::new(
            Stmt::For(ForStmt {
                binding,
                iterable,
                body,
            }),
            s,
        ))
    }

    /// Parse while loop: while condition { body }
    fn parse_while_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.current_span();
        self.advance(); // consume 'while'

        let condition = self.parse_expr_no_struct()?;
        let body = self.parse_block()?;

        let s = start.merge(self.prev_span());
        Ok(Spanned::new(Stmt::While(WhileStmt { condition, body }), s))
    }

    /// Parse expression statement.
    fn parse_expr_stmt(&mut self, start: Span) -> Result<Spanned<Stmt>, ParseError> {
        let expr = self.parse_expr()?;
        let s = start.merge(expr.span);
        self.eat_optional_terminator();
        Ok(Spanned::new(Stmt::Expr(expr), s))
    }

    /// Consume a newline or semicolon if present (optional terminator after statements).
    fn eat_optional_terminator(&mut self) {
        if self.at(TokenKind::Newline) || self.at(TokenKind::Semicolon) {
            self.advance();
        }
    }
}
