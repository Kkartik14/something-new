//! Top-level item AST nodes.

use crate::common::*;
use crate::expr::Expr;
use crate::stmt::Block;
use crate::types::{GenericParam, Type};

/// A complete source file.
#[derive(Debug, Clone, PartialEq)]
pub struct SourceFile {
    pub items: Vec<Spanned<Item>>,
}

/// Top-level item.
#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Function(FnDef),
    Struct(StructDef),
    Enum(EnumDef),
    Trait(TraitDef),
    Impl(ImplBlock),
    View(ViewDef),
    Use(UseDecl),
    Mod(ModDecl),
}

/// Function definition.
#[derive(Debug, Clone, PartialEq)]
pub struct FnDef {
    pub visibility: Visibility,
    pub name: Ident,
    pub generic_params: Vec<GenericParam>,
    pub params: Vec<Param>,
    pub return_type: Option<Spanned<Type>>,
    pub body: Option<Block>,
}

/// Function parameter.
#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub ownership: ParamOwnership,
    pub name: Ident,
    pub ty: Spanned<Type>,
}

/// How a parameter receives its value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParamOwnership {
    /// Default — shared, immutable borrow.
    Borrow,
    /// `own` keyword — takes ownership.
    Own,
    /// `mut` keyword — mutable borrow.
    MutBorrow,
    /// `self` — borrows self.
    SelfBorrow,
    /// `mut self` — mutable borrow of self.
    SelfMutBorrow,
    /// `own self` — takes ownership of self.
    SelfOwn,
}

/// Struct definition.
#[derive(Debug, Clone, PartialEq)]
pub struct StructDef {
    pub visibility: Visibility,
    pub name: Ident,
    pub generic_params: Vec<GenericParam>,
    pub fields: Vec<FieldDef>,
}

/// Struct field.
#[derive(Debug, Clone, PartialEq)]
pub struct FieldDef {
    pub visibility: Visibility,
    pub name: Ident,
    pub ty: Spanned<Type>,
}

/// Enum definition.
#[derive(Debug, Clone, PartialEq)]
pub struct EnumDef {
    pub visibility: Visibility,
    pub name: Ident,
    pub generic_params: Vec<GenericParam>,
    pub variants: Vec<EnumVariant>,
}

/// Enum variant.
#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub name: Ident,
    pub fields: Vec<Spanned<Type>>,
}

/// Trait definition.
#[derive(Debug, Clone, PartialEq)]
pub struct TraitDef {
    pub visibility: Visibility,
    pub name: Ident,
    pub generic_params: Vec<GenericParam>,
    pub methods: Vec<Spanned<FnDef>>,
}

/// Impl block.
#[derive(Debug, Clone, PartialEq)]
pub struct ImplBlock {
    pub generic_params: Vec<GenericParam>,
    pub trait_name: Option<Ident>,
    pub target_type: Spanned<Type>,
    pub methods: Vec<Spanned<FnDef>>,
}

/// View definition.
#[derive(Debug, Clone, PartialEq)]
pub struct ViewDef {
    pub visibility: Visibility,
    pub name: Ident,
    pub fields: Vec<ViewField>,
    pub body: Block,
}

/// View field (@state, @prop, @binding).
#[derive(Debug, Clone, PartialEq)]
pub struct ViewField {
    pub attribute: ViewFieldAttr,
    pub name: Ident,
    pub ty: Spanned<Type>,
    pub default: Option<Spanned<Expr>>,
}

/// View field attribute.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ViewFieldAttr {
    State,
    Prop,
    Binding,
}

/// Use declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct UseDecl {
    pub path: Vec<Ident>,
    pub items: UseItems,
}

/// What a use declaration imports.
#[derive(Debug, Clone, PartialEq)]
pub enum UseItems {
    /// `use foo.bar.*`
    All,
    /// `use foo.bar.Baz` — last path element is the imported name.
    Single,
    /// `use foo.{bar, baz}`
    Multiple(Vec<Ident>),
}

/// Module declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct ModDecl {
    pub visibility: Visibility,
    pub name: Ident,
}
