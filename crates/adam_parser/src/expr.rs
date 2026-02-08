//! Expression parsing — Pratt parser with correct operator precedence.

use adam_ast::common::*;
use adam_ast::expr::*;
use adam_ast::stmt::Block;
use adam_lexer::TokenKind;

use crate::parser::{span, ParseError, Parser};

/// Binding power for infix operators: (left_bp, right_bp).
/// Higher = tighter binding. Left-assoc: right_bp = left_bp + 1.
/// Right-assoc: right_bp = left_bp.
fn infix_bp(kind: &TokenKind) -> Option<(u8, u8)> {
    match kind {
        // Assignment (right-associative)
        TokenKind::Assign
        | TokenKind::PlusAssign
        | TokenKind::MinusAssign
        | TokenKind::StarAssign
        | TokenKind::SlashAssign
        | TokenKind::PercentAssign => Some((2, 1)),

        TokenKind::Or => Some((3, 4)),
        TokenKind::And => Some((5, 6)),
        TokenKind::Eq | TokenKind::NotEq => Some((7, 8)),
        TokenKind::Lt | TokenKind::Gt | TokenKind::LtEq | TokenKind::GtEq => Some((9, 10)),
        TokenKind::DotDot => Some((11, 12)),
        TokenKind::Plus | TokenKind::Minus => Some((13, 14)),
        TokenKind::Star | TokenKind::Slash | TokenKind::Percent => Some((15, 16)),
        _ => None,
    }
}

/// Binding power for postfix operators.
fn postfix_bp(kind: &TokenKind, no_struct_literal: bool) -> Option<u8> {
    match kind {
        TokenKind::Question => Some(19),
        TokenKind::Dot => Some(21),
        TokenKind::LParen => Some(21),
        TokenKind::LBracket => Some(21),
        TokenKind::LBrace if !no_struct_literal => Some(21),
        _ => None,
    }
}

impl Parser {
    /// Parse an expression.
    pub(crate) fn parse_expr(&mut self) -> Result<Spanned<Expr>, ParseError> {
        self.parse_expr_bp(0)
    }

    /// Parse an expression disallowing struct literals (for if/while conditions).
    pub(crate) fn parse_expr_no_struct(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let old = self.no_struct_literal;
        self.no_struct_literal = true;
        let result = self.parse_expr_bp(0);
        self.no_struct_literal = old;
        result
    }

    /// Pratt parser core.
    fn parse_expr_bp(&mut self, min_bp: u8) -> Result<Spanned<Expr>, ParseError> {
        let mut lhs = self.parse_prefix()?;

        loop {
            // Skip newlines inside expressions only at specific points
            // (handled by individual postfix parsers)

            // Check for postfix
            if let Some(lbp) = postfix_bp(&self.current().kind, self.no_struct_literal) {
                if lbp < min_bp {
                    break;
                }
                lhs = self.parse_postfix(lhs)?;
                continue;
            }

            // Check for infix
            if let Some((lbp, rbp)) = infix_bp(&self.current().kind) {
                if lbp < min_bp {
                    break;
                }
                lhs = self.parse_infix(lhs, rbp)?;
                continue;
            }

            break;
        }

        Ok(lhs)
    }

    /// Parse prefix expressions and primary expressions.
    fn parse_prefix(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let start = self.current_span();

        match &self.current().kind {
            // ---- Literals ----
            TokenKind::IntLiteral(_) => {
                let (n, s) = self.eat_int_literal();
                Ok(Spanned::new(Expr::IntLiteral(n), s))
            }
            TokenKind::FloatLiteral(_) => {
                let (n, s) = self.eat_float_literal();
                Ok(Spanned::new(Expr::FloatLiteral(n), s))
            }
            TokenKind::StringLiteral(_) => self.parse_string_or_interpolation(),
            TokenKind::CharLiteral(_) => {
                let (c, s) = self.eat_char_literal();
                Ok(Spanned::new(Expr::CharLiteral(c), s))
            }
            TokenKind::True => {
                self.advance();
                Ok(Spanned::new(Expr::BoolLiteral(true), start))
            }
            TokenKind::False => {
                self.advance();
                Ok(Spanned::new(Expr::BoolLiteral(false), start))
            }
            TokenKind::Nil => {
                self.advance();
                Ok(Spanned::new(Expr::NilLiteral, start))
            }

            // ---- Identifier ----
            TokenKind::Identifier(_) | TokenKind::SelfValue => {
                let ident = if self.at(TokenKind::SelfValue) {
                    let tok = self.advance();
                    Ident::new("self", span(tok.span))
                } else {
                    self.expect_ident()?
                };
                Ok(Spanned::new(Expr::Identifier(ident.clone()), ident.span))
            }

            // ---- Unary operators ----
            TokenKind::Minus => {
                self.advance();
                let operand = self.parse_expr_bp(17)?; // prefix binding power
                let s = start.merge(operand.span);
                Ok(Spanned::new(
                    Expr::Unary(Box::new(UnaryExpr {
                        op: UnaryOp::Neg,
                        operand,
                    })),
                    s,
                ))
            }
            TokenKind::Not => {
                self.advance();
                let operand = self.parse_expr_bp(17)?;
                let s = start.merge(operand.span);
                Ok(Spanned::new(
                    Expr::Unary(Box::new(UnaryExpr {
                        op: UnaryOp::Not,
                        operand,
                    })),
                    s,
                ))
            }
            TokenKind::Ampersand => {
                self.advance();
                let operand = self.parse_expr_bp(17)?;
                let s = start.merge(operand.span);
                Ok(Spanned::new(
                    Expr::Unary(Box::new(UnaryExpr {
                        op: UnaryOp::Ref,
                        operand,
                    })),
                    s,
                ))
            }

            // ---- Grouping / Tuple: (expr, ...) ----
            TokenKind::LParen => self.parse_grouped_or_tuple(),

            // ---- Array literal: [expr, ...] ----
            TokenKind::LBracket => self.parse_array_literal(),

            // ---- Block: { stmts } ----
            TokenKind::LBrace => {
                let block = self.parse_block()?;
                let s = start.merge(self.prev_span());
                Ok(Spanned::new(Expr::Block(block), s))
            }

            // ---- If expression ----
            TokenKind::If => self.parse_if_expr(),

            // ---- Match expression ----
            TokenKind::Match => self.parse_match_expr(),

            // ---- Closure: |params| body ----
            TokenKind::Pipe => self.parse_closure(),

            // ---- Spawn: spawn { block } ----
            TokenKind::Spawn => {
                self.advance();
                let block = self.parse_block()?;
                let s = start.merge(self.prev_span());
                Ok(Spanned::new(Expr::Spawn(block), s))
            }

            // ---- Channel create: chan[T]() or chan[T](cap) ----
            TokenKind::Chan => self.parse_chan_create(),

            // ---- Select expression ----
            TokenKind::Select => self.parse_select_expr(),

            // ---- Return ----
            TokenKind::Return => {
                self.advance();
                let value = if self.at(TokenKind::Newline)
                    || self.at(TokenKind::Eof)
                    || self.at(TokenKind::RBrace)
                {
                    None
                } else {
                    Some(Box::new(self.parse_expr()?))
                };
                let s = start.merge(self.prev_span());
                Ok(Spanned::new(Expr::Return(value), s))
            }

            // ---- Break ----
            TokenKind::Break => {
                self.advance();
                let value = if self.at(TokenKind::Newline)
                    || self.at(TokenKind::Eof)
                    || self.at(TokenKind::RBrace)
                {
                    None
                } else {
                    Some(Box::new(self.parse_expr()?))
                };
                let s = start.merge(self.prev_span());
                Ok(Spanned::new(Expr::Break(value), s))
            }

            // ---- Continue ----
            TokenKind::Continue => {
                self.advance();
                Ok(Spanned::new(Expr::Continue, start))
            }

            _ => Err(self.error_at_current(&format!(
                "expected expression, found {}",
                self.current().kind
            ))),
        }
    }

    /// Parse infix expression.
    fn parse_infix(&mut self, lhs: Spanned<Expr>, rbp: u8) -> Result<Spanned<Expr>, ParseError> {
        let op_tok = self.advance();
        let start = lhs.span;

        match &op_tok.kind {
            // Assignment: =
            TokenKind::Assign => {
                let value = self.parse_expr_bp(rbp)?;
                let s = start.merge(value.span);
                Ok(Spanned::new(
                    Expr::Assign(Box::new(AssignExpr { target: lhs, value })),
                    s,
                ))
            }

            // Compound assignment: +=, -=, *=, /=, %=  (desugar to x = x op val)
            TokenKind::PlusAssign
            | TokenKind::MinusAssign
            | TokenKind::StarAssign
            | TokenKind::SlashAssign
            | TokenKind::PercentAssign => {
                let op = match &op_tok.kind {
                    TokenKind::PlusAssign => BinaryOp::Add,
                    TokenKind::MinusAssign => BinaryOp::Sub,
                    TokenKind::StarAssign => BinaryOp::Mul,
                    TokenKind::SlashAssign => BinaryOp::Div,
                    TokenKind::PercentAssign => BinaryOp::Mod,
                    _ => unreachable!(),
                };
                let rhs = self.parse_expr_bp(rbp)?;
                let rhs_span = rhs.span;
                let binary = Spanned::new(
                    Expr::Binary(Box::new(BinaryExpr {
                        left: lhs.clone(),
                        op,
                        right: rhs,
                    })),
                    start.merge(rhs_span),
                );
                let s = start.merge(rhs_span);
                Ok(Spanned::new(
                    Expr::Assign(Box::new(AssignExpr {
                        target: lhs,
                        value: binary,
                    })),
                    s,
                ))
            }

            // Range: ..
            TokenKind::DotDot => {
                let end_expr = self.parse_expr_bp(rbp)?;
                let s = start.merge(end_expr.span);
                Ok(Spanned::new(
                    Expr::Range(Box::new(RangeExpr {
                        start: lhs,
                        end: end_expr,
                        inclusive: false,
                    })),
                    s,
                ))
            }

            // Binary operators
            _ => {
                let op = match &op_tok.kind {
                    TokenKind::Plus => BinaryOp::Add,
                    TokenKind::Minus => BinaryOp::Sub,
                    TokenKind::Star => BinaryOp::Mul,
                    TokenKind::Slash => BinaryOp::Div,
                    TokenKind::Percent => BinaryOp::Mod,
                    TokenKind::Eq => BinaryOp::Eq,
                    TokenKind::NotEq => BinaryOp::NotEq,
                    TokenKind::Lt => BinaryOp::Lt,
                    TokenKind::Gt => BinaryOp::Gt,
                    TokenKind::LtEq => BinaryOp::LtEq,
                    TokenKind::GtEq => BinaryOp::GtEq,
                    TokenKind::And => BinaryOp::And,
                    TokenKind::Or => BinaryOp::Or,
                    _ => {
                        return Err(ParseError::new(
                            format!("unexpected infix operator: {}", op_tok.kind),
                            span(op_tok.span),
                        ));
                    }
                };
                let rhs = self.parse_expr_bp(rbp)?;
                let s = start.merge(rhs.span);
                Ok(Spanned::new(
                    Expr::Binary(Box::new(BinaryExpr {
                        left: lhs,
                        op,
                        right: rhs,
                    })),
                    s,
                ))
            }
        }
    }

    /// Parse postfix expression.
    fn parse_postfix(&mut self, lhs: Spanned<Expr>) -> Result<Spanned<Expr>, ParseError> {
        let start = lhs.span;

        match &self.current().kind {
            // Try: expr?
            TokenKind::Question => {
                self.advance();
                let s = start.merge(self.prev_span());
                Ok(Spanned::new(Expr::Try(Box::new(lhs)), s))
            }

            // Field access or method call: expr.name or expr.name(args)
            TokenKind::Dot => {
                self.advance();
                let name = self.expect_ident()?;

                // Method call: expr.name(args)
                if self.at(TokenKind::LParen) {
                    let args = self.parse_arg_list()?;
                    let s = start.merge(self.prev_span());
                    Ok(Spanned::new(
                        Expr::MethodCall(Box::new(MethodCallExpr {
                            receiver: lhs,
                            method: name,
                            generic_args: vec![],
                            args,
                        })),
                        s,
                    ))
                } else {
                    let s = start.merge(name.span);
                    Ok(Spanned::new(
                        Expr::FieldAccess(Box::new(FieldAccessExpr {
                            object: lhs,
                            field: name,
                        })),
                        s,
                    ))
                }
            }

            // Function call: expr(args)
            TokenKind::LParen => {
                let args = self.parse_arg_list()?;
                let s = start.merge(self.prev_span());
                Ok(Spanned::new(
                    Expr::Call(Box::new(CallExpr {
                        callee: lhs,
                        generic_args: vec![],
                        args,
                    })),
                    s,
                ))
            }

            // Index: expr[index]
            TokenKind::LBracket => {
                self.advance();
                self.skip_newlines();
                let index = self.parse_expr()?;
                self.skip_newlines();
                self.expect(TokenKind::RBracket)?;
                let s = start.merge(self.prev_span());
                Ok(Spanned::new(
                    Expr::Index(Box::new(IndexExpr { object: lhs, index })),
                    s,
                ))
            }

            // Struct literal: Name { field: value, ... }
            TokenKind::LBrace => {
                // Only valid when lhs is an identifier
                if let Expr::Identifier(ident) = &lhs.node {
                    self.parse_struct_literal_body(ident.clone())
                } else {
                    // Not a struct literal — stop the postfix loop
                    Err(ParseError::new("unexpected `{`", self.current_span()))
                }
            }

            _ => Err(self.error_at_current("unexpected postfix token")),
        }
    }

    // ---- Specific expression parsers ----

    /// Parse argument list: (expr, expr, ...)
    fn parse_arg_list(&mut self) -> Result<Vec<Spanned<Expr>>, ParseError> {
        self.expect(TokenKind::LParen)?;
        self.skip_newlines();
        let mut args = vec![];

        while !self.at(TokenKind::RParen) && !self.at(TokenKind::Eof) {
            // Support named arguments: name: expr
            if self.at_ident() && self.peek().kind == TokenKind::Colon {
                // For now, parse as regular expression (name is just context)
                // We could store the name for named args later
                let _name = self.eat_ident();
                self.advance(); // consume :
                self.skip_newlines();
            }

            args.push(self.parse_expr()?);
            self.skip_newlines();
            if !self.eat(TokenKind::Comma) {
                self.skip_newlines();
                break;
            }
            self.skip_newlines();
        }

        self.expect(TokenKind::RParen)?;
        Ok(args)
    }

    /// Parse string literal or string interpolation.
    fn parse_string_or_interpolation(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let start = self.current_span();
        let (first_str, _) = self.eat_string_literal();

        // Check for interpolation
        if !self.at(TokenKind::StringInterpolStart) {
            let s = start;
            return Ok(Spanned::new(Expr::StringLiteral(first_str), s));
        }

        // Build interpolation parts
        let mut parts = vec![];
        if !first_str.is_empty() {
            parts.push(StringPart::Literal(first_str));
        }

        while self.at(TokenKind::StringInterpolStart) {
            self.advance(); // consume StringInterpolStart
            let expr = self.parse_expr()?;
            parts.push(StringPart::Interpolation(expr));
            self.expect(TokenKind::StringInterpolEnd)?;

            // After interpolation end, there should be a string continuation
            if self.at_string() {
                let (s, _) = self.eat_string_literal();
                if !s.is_empty() {
                    parts.push(StringPart::Literal(s));
                }
            }
        }

        let s = start.merge(self.prev_span());
        Ok(Spanned::new(Expr::StringInterpolation(parts), s))
    }

    /// Parse grouped expression or tuple: (expr) or (expr, expr, ...)
    fn parse_grouped_or_tuple(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let start = self.current_span();
        self.advance(); // consume (

        let old_no_struct = self.no_struct_literal;
        self.no_struct_literal = false;

        self.skip_newlines();

        // Empty tuple: ()
        if self.at(TokenKind::RParen) {
            let end_tok = self.advance();
            self.no_struct_literal = old_no_struct;
            let s = start.merge(span(end_tok.span));
            return Ok(Spanned::new(Expr::TupleLiteral(vec![]), s));
        }

        let first = self.parse_expr()?;
        self.skip_newlines();

        // Single expression: (expr)
        if self.at(TokenKind::RParen) {
            let end_tok = self.advance();
            self.no_struct_literal = old_no_struct;
            // Just unwrap the grouping — return the inner expression with merged span
            let s = start.merge(span(end_tok.span));
            Ok(Spanned::new(first.node, s))
        }
        // Tuple: (expr, expr, ...)
        else if self.eat(TokenKind::Comma) {
            self.skip_newlines();
            let mut elements = vec![first];
            while !self.at(TokenKind::RParen) && !self.at(TokenKind::Eof) {
                elements.push(self.parse_expr()?);
                self.skip_newlines();
                if !self.eat(TokenKind::Comma) {
                    break;
                }
                self.skip_newlines();
            }
            let end_tok = self.expect(TokenKind::RParen)?;
            self.no_struct_literal = old_no_struct;
            let s = start.merge(span(end_tok.span));
            Ok(Spanned::new(Expr::TupleLiteral(elements), s))
        } else {
            self.no_struct_literal = old_no_struct;
            Err(self.error_at_current(&format!(
                "expected `)` or `,`, found {}",
                self.current().kind
            )))
        }
    }

    /// Parse array literal: [expr, expr, ...]
    fn parse_array_literal(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let start = self.current_span();
        self.advance(); // consume [
        self.skip_newlines();

        let mut elements = vec![];
        while !self.at(TokenKind::RBracket) && !self.at(TokenKind::Eof) {
            elements.push(self.parse_expr()?);
            self.skip_newlines();
            if !self.eat(TokenKind::Comma) {
                self.skip_newlines();
                break;
            }
            self.skip_newlines();
        }

        let end_tok = self.expect(TokenKind::RBracket)?;
        let s = start.merge(span(end_tok.span));
        Ok(Spanned::new(Expr::ArrayLiteral(elements), s))
    }

    /// Parse struct literal body: { field: value, ... }
    fn parse_struct_literal_body(&mut self, name: Ident) -> Result<Spanned<Expr>, ParseError> {
        let start = name.span;
        self.advance(); // consume {
        self.skip_newlines();

        let mut fields = vec![];
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            let field_name = self.expect_ident()?;

            let value = if self.eat(TokenKind::Colon) {
                self.skip_newlines();
                self.parse_expr()?
            } else {
                // Shorthand: Name { x, y } is Name { x: x, y: y }
                Spanned::new(Expr::Identifier(field_name.clone()), field_name.span)
            };

            fields.push((field_name, value));

            self.skip_newlines();
            if !self.eat(TokenKind::Comma) {
                self.skip_newlines();
                break;
            }
            self.skip_newlines();
        }

        let end_tok = self.expect(TokenKind::RBrace)?;
        let s = start.merge(span(end_tok.span));
        Ok(Spanned::new(
            Expr::StructLiteral(Box::new(StructLiteralExpr { name, fields })),
            s,
        ))
    }

    /// Parse if expression.
    fn parse_if_expr(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let start = self.current_span();
        self.advance(); // consume 'if'

        let condition = self.parse_expr_no_struct()?;
        let then_block = self.parse_block()?;

        let else_block = if self.eat(TokenKind::Else) {
            if self.at(TokenKind::If) {
                // else if
                let else_if = self.parse_if_expr()?;
                let if_span = else_if.span;
                if let Expr::If(if_expr) = else_if.node {
                    Some(ElseBlock::ElseIf(Box::new(Spanned::new(*if_expr, if_span))))
                } else {
                    unreachable!()
                }
            } else {
                // else block
                let block = self.parse_block()?;
                Some(ElseBlock::Else(block))
            }
        } else {
            None
        };

        let s = start.merge(self.prev_span());
        Ok(Spanned::new(
            Expr::If(Box::new(IfExpr {
                condition,
                then_block,
                else_block,
            })),
            s,
        ))
    }

    /// Parse match expression.
    fn parse_match_expr(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let start = self.current_span();
        self.advance(); // consume 'match'

        let scrutinee = self.parse_expr_no_struct()?;
        self.expect(TokenKind::LBrace)?;
        self.skip_newlines();

        let mut arms = vec![];
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            arms.push(self.parse_match_arm()?);
            self.skip_newlines();
        }

        self.expect(TokenKind::RBrace)?;
        let s = start.merge(self.prev_span());
        Ok(Spanned::new(
            Expr::Match(Box::new(MatchExpr { scrutinee, arms })),
            s,
        ))
    }

    /// Parse a single match arm: pattern [if guard] => expr
    fn parse_match_arm(&mut self) -> Result<MatchArm, ParseError> {
        let pattern = self.parse_pattern()?;

        let guard = if self.eat(TokenKind::If) {
            Some(self.parse_expr_no_struct()?)
        } else {
            None
        };

        self.expect(TokenKind::FatArrow)?;
        let body = self.parse_expr()?;

        // Consume optional terminator after arm body
        if self.at(TokenKind::Newline) {
            self.advance();
        }

        Ok(MatchArm {
            pattern,
            guard,
            body,
        })
    }

    /// Parse closure: |params| body
    fn parse_closure(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let start = self.current_span();
        self.advance(); // consume first |

        let mut params = vec![];
        if !self.at(TokenKind::Pipe) {
            params.push(self.parse_closure_param()?);
            while self.eat(TokenKind::Comma) {
                if self.at(TokenKind::Pipe) {
                    break;
                }
                params.push(self.parse_closure_param()?);
            }
        }
        self.expect(TokenKind::Pipe)?;

        let body = self.parse_expr()?;
        let s = start.merge(body.span);
        Ok(Spanned::new(
            Expr::Closure(Box::new(ClosureExpr { params, body })),
            s,
        ))
    }

    fn parse_closure_param(&mut self) -> Result<ClosureParam, ParseError> {
        let name = self.expect_ident()?;
        let ty = if self.at_ident()
            || self.at(TokenKind::Ampersand)
            || self.at(TokenKind::Question)
            || self.at(TokenKind::LBracket)
            || self.at(TokenKind::LParen)
            || self.at(TokenKind::Fn)
            || self.at(TokenKind::Chan)
        {
            Some(self.parse_type()?)
        } else {
            None
        };
        Ok(ClosureParam { name, ty })
    }

    /// Parse channel create: chan[T]() or chan[T](capacity)
    fn parse_chan_create(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let start = self.current_span();
        self.advance(); // consume 'chan'

        self.expect(TokenKind::LBracket)?;
        let element_type = self.parse_type()?;
        self.expect(TokenKind::RBracket)?;

        self.expect(TokenKind::LParen)?;
        let capacity = if !self.at(TokenKind::RParen) {
            Some(self.parse_expr()?)
        } else {
            None
        };
        self.expect(TokenKind::RParen)?;

        let s = start.merge(self.prev_span());
        Ok(Spanned::new(
            Expr::ChanCreate(Box::new(ChanCreateExpr {
                element_type,
                capacity,
            })),
            s,
        ))
    }

    /// Parse select expression.
    fn parse_select_expr(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let start = self.current_span();
        self.advance(); // consume 'select'
        self.expect(TokenKind::LBrace)?;
        self.skip_newlines();

        let mut arms = vec![];
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            arms.push(self.parse_select_arm()?);
            self.skip_newlines();
        }

        self.expect(TokenKind::RBrace)?;
        let s = start.merge(self.prev_span());
        Ok(Spanned::new(Expr::Select(Box::new(SelectExpr { arms })), s))
    }

    /// Parse a single select arm.
    fn parse_select_arm(&mut self) -> Result<SelectArm, ParseError> {
        let kind = if self.at(TokenKind::After) {
            // after expr => body
            self.advance();
            let timeout = self.parse_expr_no_struct()?;
            SelectArmKind::After(timeout)
        } else if self.at_ident() && self.peek().kind == TokenKind::ColonAssign {
            // binding := channel.recv() => body
            let binding = self.expect_ident()?;
            self.expect(TokenKind::ColonAssign)?;
            let channel = self.parse_expr_no_struct()?;
            SelectArmKind::Recv { binding, channel }
        } else {
            // channel.send(value) => body
            let channel = self.parse_expr_no_struct()?;
            SelectArmKind::Send {
                channel: channel.clone(),
                value: channel, // The send expression includes the value
            }
        };

        self.expect(TokenKind::FatArrow)?;

        // Parse body as block
        let body = if self.at(TokenKind::LBrace) {
            self.parse_block()?
        } else {
            // Single expression — wrap in a block
            let expr = self.parse_expr()?;
            let expr_span = expr.span;
            Block {
                stmts: vec![Spanned::new(adam_ast::stmt::Stmt::Expr(expr), expr_span)],
            }
        };

        // Consume optional terminator
        if self.at(TokenKind::Newline) {
            self.advance();
        }

        Ok(SelectArm { kind, body })
    }
}
