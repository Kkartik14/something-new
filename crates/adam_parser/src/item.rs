//! Top-level item parsing — fn, struct, enum, trait, impl, view, use, mod.

use adam_ast::common::*;
use adam_ast::item::*;
use adam_ast::types::Type;
use adam_lexer::TokenKind;

use crate::parser::{span, ParseError, Parser};

impl Parser {
    /// Parse a full source file.
    pub(crate) fn parse_source_file(&mut self) -> SourceFile {
        self.skip_newlines();
        let mut items = vec![];

        while !self.at(TokenKind::Eof) {
            match self.parse_item() {
                Ok(item) => items.push(item),
                Err(err) => {
                    self.record_error(err);
                    self.synchronize();
                }
            }
            self.skip_newlines();
        }

        SourceFile { items }
    }

    /// Parse a top-level item.
    pub(crate) fn parse_item(&mut self) -> Result<Spanned<Item>, ParseError> {
        let start = self.current_span();
        let visibility = self.parse_visibility();

        match &self.current().kind {
            TokenKind::Fn => {
                let def = self.parse_fn_def(visibility)?;
                let s = start.merge(self.prev_span());
                Ok(Spanned::new(Item::Function(def), s))
            }
            TokenKind::Struct => {
                let def = self.parse_struct_def(visibility)?;
                let s = start.merge(self.prev_span());
                Ok(Spanned::new(Item::Struct(def), s))
            }
            TokenKind::Enum => {
                let def = self.parse_enum_def(visibility)?;
                let s = start.merge(self.prev_span());
                Ok(Spanned::new(Item::Enum(def), s))
            }
            TokenKind::Trait => {
                let def = self.parse_trait_def(visibility)?;
                let s = start.merge(self.prev_span());
                Ok(Spanned::new(Item::Trait(def), s))
            }
            TokenKind::Impl => {
                let def = self.parse_impl_block()?;
                let s = start.merge(self.prev_span());
                Ok(Spanned::new(Item::Impl(def), s))
            }
            TokenKind::View => {
                let def = self.parse_view_def(visibility)?;
                let s = start.merge(self.prev_span());
                Ok(Spanned::new(Item::View(def), s))
            }
            TokenKind::Use => {
                let decl = self.parse_use_decl()?;
                let s = start.merge(self.prev_span());
                Ok(Spanned::new(Item::Use(decl), s))
            }
            TokenKind::Mod => {
                let decl = self.parse_mod_decl(visibility)?;
                let s = start.merge(self.prev_span());
                Ok(Spanned::new(Item::Mod(decl), s))
            }
            _ => Err(self.error_at_current(&format!(
                "expected item (fn, struct, enum, trait, impl, view, use, mod), found {}",
                self.current().kind
            ))),
        }
    }

    /// Parse optional `pub` visibility.
    fn parse_visibility(&mut self) -> Visibility {
        if self.eat(TokenKind::Pub) {
            Visibility::Public
        } else {
            Visibility::Private
        }
    }

    /// Parse function definition.
    fn parse_fn_def(&mut self, visibility: Visibility) -> Result<FnDef, ParseError> {
        self.advance(); // consume 'fn'
        let name = self.expect_ident()?;
        let generic_params = self.parse_generic_params()?;
        let params = self.parse_param_list()?;

        let return_type = if self.eat(TokenKind::Arrow) {
            Some(self.parse_type()?)
        } else {
            None
        };

        let body = if self.at(TokenKind::LBrace) {
            Some(self.parse_block()?)
        } else {
            // Trait method declaration without body
            self.eat_optional_item_terminator();
            None
        };

        Ok(FnDef {
            visibility,
            name,
            generic_params,
            params,
            return_type,
            body,
        })
    }

    /// Parse parameter list: (param, param, ...)
    fn parse_param_list(&mut self) -> Result<Vec<Param>, ParseError> {
        self.expect(TokenKind::LParen)?;
        self.skip_newlines();
        let mut params = vec![];

        while !self.at(TokenKind::RParen) && !self.at(TokenKind::Eof) {
            params.push(self.parse_param()?);
            self.skip_newlines();
            if !self.eat(TokenKind::Comma) {
                self.skip_newlines();
                break;
            }
            self.skip_newlines();
        }

        self.expect(TokenKind::RParen)?;
        Ok(params)
    }

    /// Parse a single parameter.
    fn parse_param(&mut self) -> Result<Param, ParseError> {
        // Check for self parameter
        if self.at(TokenKind::SelfValue) {
            let tok = self.advance();
            let name = Ident::new("self", span(tok.span));
            let ty = Spanned::new(
                Type::Named(adam_ast::types::TypePath {
                    name: Ident::new("Self", name.span),
                    generic_args: vec![],
                }),
                name.span,
            );
            return Ok(Param {
                ownership: ParamOwnership::SelfBorrow,
                name,
                ty,
            });
        }

        // Check for `mut self`
        if self.at(TokenKind::Mut) && self.peek().kind == TokenKind::SelfValue {
            self.advance(); // consume 'mut'
            let tok = self.advance(); // consume 'self'
            let name = Ident::new("self", span(tok.span));
            let ty = Spanned::new(
                Type::Named(adam_ast::types::TypePath {
                    name: Ident::new("Self", name.span),
                    generic_args: vec![],
                }),
                name.span,
            );
            return Ok(Param {
                ownership: ParamOwnership::SelfMutBorrow,
                name,
                ty,
            });
        }

        // Check for `own self`
        if self.at(TokenKind::Own) && self.peek().kind == TokenKind::SelfValue {
            self.advance(); // consume 'own'
            let tok = self.advance(); // consume 'self'
            let name = Ident::new("self", span(tok.span));
            let ty = Spanned::new(
                Type::Named(adam_ast::types::TypePath {
                    name: Ident::new("Self", name.span),
                    generic_args: vec![],
                }),
                name.span,
            );
            return Ok(Param {
                ownership: ParamOwnership::SelfOwn,
                name,
                ty,
            });
        }

        // Regular parameter: [own|mut] name Type
        let ownership = if self.eat(TokenKind::Own) {
            ParamOwnership::Own
        } else if self.eat(TokenKind::Mut) {
            ParamOwnership::MutBorrow
        } else {
            ParamOwnership::Borrow
        };

        let name = self.expect_ident()?;
        let ty = self.parse_type()?;

        Ok(Param {
            ownership,
            name,
            ty,
        })
    }

    /// Parse struct definition.
    fn parse_struct_def(&mut self, visibility: Visibility) -> Result<StructDef, ParseError> {
        self.advance(); // consume 'struct'
        let name = self.expect_ident()?;
        let generic_params = self.parse_generic_params()?;

        self.expect(TokenKind::LBrace)?;
        self.skip_newlines();

        let mut fields = vec![];
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            let field_vis = self.parse_visibility();
            let field_name = self.expect_ident()?;
            let ty = self.parse_type()?;
            fields.push(FieldDef {
                visibility: field_vis,
                name: field_name,
                ty,
            });
            self.skip_newlines();
        }

        self.expect(TokenKind::RBrace)?;

        Ok(StructDef {
            visibility,
            name,
            generic_params,
            fields,
        })
    }

    /// Parse enum definition.
    fn parse_enum_def(&mut self, visibility: Visibility) -> Result<EnumDef, ParseError> {
        self.advance(); // consume 'enum'
        let name = self.expect_ident()?;
        let generic_params = self.parse_generic_params()?;

        self.expect(TokenKind::LBrace)?;
        self.skip_newlines();

        let mut variants = vec![];
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            let variant_name = self.expect_ident()?;

            let mut variant_fields = vec![];
            if self.at(TokenKind::LParen) {
                self.advance();
                self.skip_newlines();
                if !self.at(TokenKind::RParen) {
                    variant_fields.push(self.parse_type()?);
                    while self.eat(TokenKind::Comma) {
                        self.skip_newlines();
                        if self.at(TokenKind::RParen) {
                            break;
                        }
                        variant_fields.push(self.parse_type()?);
                    }
                }
                self.skip_newlines();
                self.expect(TokenKind::RParen)?;
            }

            variants.push(EnumVariant {
                name: variant_name,
                fields: variant_fields,
            });
            self.skip_newlines();
        }

        self.expect(TokenKind::RBrace)?;

        Ok(EnumDef {
            visibility,
            name,
            generic_params,
            variants,
        })
    }

    /// Parse trait definition.
    fn parse_trait_def(&mut self, visibility: Visibility) -> Result<TraitDef, ParseError> {
        self.advance(); // consume 'trait'
        let name = self.expect_ident()?;
        let generic_params = self.parse_generic_params()?;

        self.expect(TokenKind::LBrace)?;
        self.skip_newlines();

        let mut methods = vec![];
        let mut associated_types = vec![];
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            let method_start = self.current_span();
            let method_vis = self.parse_visibility();
            if self.at(TokenKind::Fn) {
                let def = self.parse_fn_def(method_vis)?;
                let s = method_start.merge(self.prev_span());
                methods.push(Spanned::new(def, s));
            } else if self.at(TokenKind::TypeKw) {
                self.advance(); // consume 'type'
                let type_name = self.expect_ident()?;
                associated_types.push(AssociatedTypeDef { name: type_name });
            } else {
                return Err(self.error_at_current(&format!(
                    "expected method definition in trait, found {}",
                    self.current().kind
                )));
            }
            self.skip_newlines();
        }

        self.expect(TokenKind::RBrace)?;

        Ok(TraitDef {
            visibility,
            name,
            generic_params,
            methods,
            associated_types,
        })
    }

    /// Parse impl block: impl Type { methods } or impl Trait for Type { methods }
    fn parse_impl_block(&mut self) -> Result<ImplBlock, ParseError> {
        self.advance(); // consume 'impl'
        let generic_params = self.parse_generic_params()?;

        let first_type = self.parse_type()?;

        // Check for 'for' → trait impl
        let (trait_name, target_type) = if self.at(TokenKind::For) {
            self.advance();
            let target = self.parse_type()?;
            // first_type should be a named type (the trait name)
            let trait_ident = if let Type::Named(path) = &first_type.node {
                path.name.clone()
            } else {
                return Err(ParseError::new("expected trait name", first_type.span));
            };
            (Some(trait_ident), target)
        } else {
            (None, first_type)
        };

        self.expect(TokenKind::LBrace)?;
        self.skip_newlines();

        let mut methods = vec![];
        let mut associated_type_bindings = vec![];
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            let method_start = self.current_span();
            let method_vis = self.parse_visibility();
            if self.at(TokenKind::Fn) {
                let def = self.parse_fn_def(method_vis)?;
                let s = method_start.merge(self.prev_span());
                methods.push(Spanned::new(def, s));
            } else if self.at(TokenKind::TypeKw) {
                self.advance(); // consume 'type'
                let type_name = self.expect_ident()?;
                self.expect(TokenKind::Assign)?;
                let concrete_type = self.parse_type()?;
                associated_type_bindings.push((type_name, concrete_type));
            } else {
                return Err(self.error_at_current(&format!(
                    "expected method definition in impl, found {}",
                    self.current().kind
                )));
            }
            self.skip_newlines();
        }

        self.expect(TokenKind::RBrace)?;

        Ok(ImplBlock {
            generic_params,
            trait_name,
            target_type,
            methods,
            associated_type_bindings,
        })
    }

    /// Parse view definition.
    fn parse_view_def(&mut self, visibility: Visibility) -> Result<ViewDef, ParseError> {
        self.advance(); // consume 'view'
        let name = self.expect_ident()?;

        self.expect(TokenKind::LBrace)?;
        self.skip_newlines();

        let mut fields = vec![];

        // Parse view fields (@state, @prop, @binding) before body
        while self.at(TokenKind::At) {
            fields.push(self.parse_view_field()?);
            self.skip_newlines();
        }

        // Parse body block
        let body = if self.at_ident() {
            // Check for 'body' keyword (it's an identifier, not a keyword)
            if let TokenKind::Identifier(name) = &self.current().kind {
                if name == "body" {
                    self.advance();
                    self.parse_block()?
                } else {
                    return Err(self.error_at_current(&format!(
                        "expected `body` block in view, found `{}`",
                        name
                    )));
                }
            } else {
                unreachable!()
            }
        } else {
            return Err(self.error_at_current(&format!(
                "expected `body` block in view, found {}",
                self.current().kind
            )));
        };

        self.skip_newlines();
        self.expect(TokenKind::RBrace)?;

        Ok(ViewDef {
            visibility,
            name,
            fields,
            body,
        })
    }

    /// Parse a view field: @state name: Type [= default]
    fn parse_view_field(&mut self) -> Result<ViewField, ParseError> {
        self.expect(TokenKind::At)?;

        let attr_name = self.expect_ident()?;
        let attribute = match attr_name.name.as_str() {
            "state" => ViewFieldAttr::State,
            "prop" => ViewFieldAttr::Prop,
            "binding" => ViewFieldAttr::Binding,
            _ => {
                return Err(ParseError::new(
                    format!(
                        "expected `state`, `prop`, or `binding` after `@`, found `{}`",
                        attr_name.name
                    ),
                    attr_name.span,
                ));
            }
        };

        let name = self.expect_ident()?;
        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type()?;

        let default = if self.eat(TokenKind::Assign) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        Ok(ViewField {
            attribute,
            name,
            ty,
            default,
        })
    }

    /// Parse use declaration: use path.to.thing
    fn parse_use_decl(&mut self) -> Result<UseDecl, ParseError> {
        self.advance(); // consume 'use'

        let mut path = vec![self.expect_ident()?];

        while self.eat(TokenKind::Dot) {
            // Check for wildcard: use foo.bar.*
            if self.at(TokenKind::Star) {
                self.advance();
                self.eat_optional_item_terminator();
                return Ok(UseDecl {
                    path,
                    items: UseItems::All,
                });
            }

            // Check for multiple: use foo.{bar, baz}
            if self.at(TokenKind::LBrace) {
                self.advance();
                self.skip_newlines();
                let mut names = vec![];
                if !self.at(TokenKind::RBrace) {
                    names.push(self.expect_ident()?);
                    while self.eat(TokenKind::Comma) {
                        self.skip_newlines();
                        if self.at(TokenKind::RBrace) {
                            break;
                        }
                        names.push(self.expect_ident()?);
                    }
                }
                self.skip_newlines();
                self.expect(TokenKind::RBrace)?;
                self.eat_optional_item_terminator();
                return Ok(UseDecl {
                    path,
                    items: UseItems::Multiple(names),
                });
            }

            // Regular path segment
            path.push(self.expect_ident()?);
        }

        self.eat_optional_item_terminator();
        Ok(UseDecl {
            path,
            items: UseItems::Single,
        })
    }

    /// Parse module declaration: mod name
    fn parse_mod_decl(&mut self, visibility: Visibility) -> Result<ModDecl, ParseError> {
        self.advance(); // consume 'mod'
        let name = self.expect_ident()?;
        self.eat_optional_item_terminator();
        Ok(ModDecl { visibility, name })
    }

    /// Consume optional newline/semicolon after an item.
    fn eat_optional_item_terminator(&mut self) {
        if self.at(TokenKind::Newline) || self.at(TokenKind::Semicolon) {
            self.advance();
        }
    }
}
