//! Token types for the Adam lexer.

use std::fmt;

/// Source location — byte offset range in the source text.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn new(start: u32, end: u32) -> Self {
        Self { start, end }
    }

    pub fn merge(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

/// A single token produced by the lexer.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub line: u32,
    pub column: u32,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span, line: u32, column: u32) -> Self {
        Self {
            kind,
            span,
            line,
            column,
        }
    }
}

/// Every possible token kind in Adam.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // === Literals ===
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    CharLiteral(char),
    BoolLiteral(bool),

    // === Identifier ===
    Identifier(String),

    // === Keywords ===
    Fn,
    Struct,
    Enum,
    Trait,
    Impl,
    View,
    TypeKw,
    Let,
    Mut,
    Own,
    Pub,
    Use,
    Mod,
    If,
    Else,
    Match,
    For,
    While,
    Loop,
    Break,
    Continue,
    Return,
    Spawn,
    Select,
    After,
    Chan,
    True,
    False,
    Nil,
    SelfValue, // self
    SelfType,  // Self

    // === Operators ===
    Plus,          // +
    Minus,         // -
    Star,          // *
    Slash,         // /
    Percent,       // %
    Eq,            // ==
    NotEq,         // !=
    Lt,            // <
    Gt,            // >
    LtEq,          // <=
    GtEq,          // >=
    And,           // &&
    Or,            // ||
    Not,           // !
    Ampersand,     // &
    Assign,        // =
    ColonAssign,   // :=
    Arrow,         // ->
    FatArrow,      // =>
    Dot,           // .
    DotDot,        // ..
    Question,      // ?
    PlusAssign,    // +=
    MinusAssign,   // -=
    StarAssign,    // *=
    SlashAssign,   // /=
    PercentAssign, // %=

    // === Delimiters ===
    LParen,    // (
    RParen,    // )
    LBrace,    // {
    RBrace,    // }
    LBracket,  // [
    RBracket,  // ]
    Comma,     // ,
    Colon,     // :
    Semicolon, // ;

    // === Special ===
    Newline,             // significant newline (statement terminator)
    At,                  // @ (for @state, @prop, @binding)
    StringInterpolStart, // start of {expr} inside a string
    StringInterpolEnd,   // end of {expr} inside a string
    Pipe,                // | (for closures and or-patterns)

    // === Meta ===
    Eof,
    Error(String),
}

impl TokenKind {
    /// Returns true if this token kind can end a statement (triggers significant newline).
    pub fn ends_statement(&self) -> bool {
        matches!(
            self,
            TokenKind::Identifier(_)
                | TokenKind::IntLiteral(_)
                | TokenKind::FloatLiteral(_)
                | TokenKind::StringLiteral(_)
                | TokenKind::CharLiteral(_)
                | TokenKind::BoolLiteral(_)
                | TokenKind::True
                | TokenKind::False
                | TokenKind::Nil
                | TokenKind::SelfValue
                | TokenKind::SelfType
                | TokenKind::Break
                | TokenKind::Continue
                | TokenKind::Return
                | TokenKind::RParen
                | TokenKind::RBrace
                | TokenKind::RBracket
                | TokenKind::Question
                | TokenKind::StringInterpolEnd
        )
    }

    /// Returns true if this is a keyword.
    pub fn is_keyword(&self) -> bool {
        matches!(
            self,
            TokenKind::Fn
                | TokenKind::Struct
                | TokenKind::Enum
                | TokenKind::Trait
                | TokenKind::Impl
                | TokenKind::View
                | TokenKind::Let
                | TokenKind::Mut
                | TokenKind::Own
                | TokenKind::Pub
                | TokenKind::Use
                | TokenKind::Mod
                | TokenKind::If
                | TokenKind::Else
                | TokenKind::Match
                | TokenKind::For
                | TokenKind::While
                | TokenKind::Loop
                | TokenKind::Break
                | TokenKind::Continue
                | TokenKind::Return
                | TokenKind::Spawn
                | TokenKind::Select
                | TokenKind::After
                | TokenKind::Chan
                | TokenKind::True
                | TokenKind::False
                | TokenKind::Nil
                | TokenKind::SelfValue
                | TokenKind::SelfType
                | TokenKind::TypeKw
        )
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::IntLiteral(n) => write!(f, "integer `{n}`"),
            TokenKind::FloatLiteral(n) => write!(f, "float `{n}`"),
            TokenKind::StringLiteral(s) => write!(f, "string `\"{s}\"`"),
            TokenKind::CharLiteral(c) => write!(f, "char `'{c}'`"),
            TokenKind::BoolLiteral(b) => write!(f, "`{b}`"),
            TokenKind::Identifier(s) => write!(f, "identifier `{s}`"),
            TokenKind::Fn => write!(f, "`fn`"),
            TokenKind::Struct => write!(f, "`struct`"),
            TokenKind::Enum => write!(f, "`enum`"),
            TokenKind::Trait => write!(f, "`trait`"),
            TokenKind::Impl => write!(f, "`impl`"),
            TokenKind::View => write!(f, "`view`"),
            TokenKind::TypeKw => write!(f, "`type`"),
            TokenKind::Let => write!(f, "`let`"),
            TokenKind::Mut => write!(f, "`mut`"),
            TokenKind::Own => write!(f, "`own`"),
            TokenKind::Pub => write!(f, "`pub`"),
            TokenKind::Use => write!(f, "`use`"),
            TokenKind::Mod => write!(f, "`mod`"),
            TokenKind::If => write!(f, "`if`"),
            TokenKind::Else => write!(f, "`else`"),
            TokenKind::Match => write!(f, "`match`"),
            TokenKind::For => write!(f, "`for`"),
            TokenKind::While => write!(f, "`while`"),
            TokenKind::Loop => write!(f, "`loop`"),
            TokenKind::Break => write!(f, "`break`"),
            TokenKind::Continue => write!(f, "`continue`"),
            TokenKind::Return => write!(f, "`return`"),
            TokenKind::Spawn => write!(f, "`spawn`"),
            TokenKind::Select => write!(f, "`select`"),
            TokenKind::After => write!(f, "`after`"),
            TokenKind::Chan => write!(f, "`chan`"),
            TokenKind::True => write!(f, "`true`"),
            TokenKind::False => write!(f, "`false`"),
            TokenKind::Nil => write!(f, "`nil`"),
            TokenKind::SelfValue => write!(f, "`self`"),
            TokenKind::SelfType => write!(f, "`Self`"),
            TokenKind::Plus => write!(f, "`+`"),
            TokenKind::Minus => write!(f, "`-`"),
            TokenKind::Star => write!(f, "`*`"),
            TokenKind::Slash => write!(f, "`/`"),
            TokenKind::Percent => write!(f, "`%`"),
            TokenKind::Eq => write!(f, "`==`"),
            TokenKind::NotEq => write!(f, "`!=`"),
            TokenKind::Lt => write!(f, "`<`"),
            TokenKind::Gt => write!(f, "`>`"),
            TokenKind::LtEq => write!(f, "`<=`"),
            TokenKind::GtEq => write!(f, "`>=`"),
            TokenKind::And => write!(f, "`&&`"),
            TokenKind::Or => write!(f, "`||`"),
            TokenKind::Not => write!(f, "`!`"),
            TokenKind::Ampersand => write!(f, "`&`"),
            TokenKind::Assign => write!(f, "`=`"),
            TokenKind::ColonAssign => write!(f, "`:=`"),
            TokenKind::Arrow => write!(f, "`->`"),
            TokenKind::FatArrow => write!(f, "`=>`"),
            TokenKind::Dot => write!(f, "`.`"),
            TokenKind::DotDot => write!(f, "`..`"),
            TokenKind::Question => write!(f, "`?`"),
            TokenKind::PlusAssign => write!(f, "`+=`"),
            TokenKind::MinusAssign => write!(f, "`-=`"),
            TokenKind::StarAssign => write!(f, "`*=`"),
            TokenKind::SlashAssign => write!(f, "`/=`"),
            TokenKind::PercentAssign => write!(f, "`%=`"),
            TokenKind::LParen => write!(f, "`(`"),
            TokenKind::RParen => write!(f, "`)`"),
            TokenKind::LBrace => write!(f, "`{{`"),
            TokenKind::RBrace => write!(f, "`}}`"),
            TokenKind::LBracket => write!(f, "`[`"),
            TokenKind::RBracket => write!(f, "`]`"),
            TokenKind::Comma => write!(f, "`,`"),
            TokenKind::Colon => write!(f, "`:`"),
            TokenKind::Semicolon => write!(f, "`;`"),
            TokenKind::Newline => write!(f, "newline"),
            TokenKind::At => write!(f, "`@`"),
            TokenKind::StringInterpolStart => write!(f, "interpolation start"),
            TokenKind::StringInterpolEnd => write!(f, "interpolation end"),
            TokenKind::Pipe => write!(f, "`|`"),
            TokenKind::Eof => write!(f, "end of file"),
            TokenKind::Error(msg) => write!(f, "error: {msg}"),
        }
    }
}

/// Lexer error with location.
#[derive(Debug, Clone, PartialEq)]
pub struct LexError {
    pub message: String,
    pub line: u32,
    pub column: u32,
    pub span: Span,
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}: {}", self.line, self.column, self.message)
    }
}

/// Result of lexing a source file.
#[derive(Debug)]
pub struct LexResult {
    pub tokens: Vec<Token>,
    pub errors: Vec<LexError>,
}
