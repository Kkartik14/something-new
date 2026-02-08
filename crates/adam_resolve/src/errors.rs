//! Name resolution errors.

use adam_ast::common::Span;

/// A name resolution error.
#[derive(Debug, Clone)]
pub struct ResolveError {
    pub kind: ResolveErrorKind,
    pub span: Span,
}

impl ResolveError {
    pub fn new(kind: ResolveErrorKind, span: Span) -> Self {
        Self { kind, span }
    }
}

impl std::fmt::Display for ResolveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ResolveErrorKind::UndefinedName { name, suggestion } => {
                write!(
                    f,
                    "[{}..{}] undefined name `{}`",
                    self.span.start, self.span.end, name
                )?;
                if let Some(sug) = suggestion {
                    write!(f, ". Did you mean `{}`?", sug)?;
                }
                Ok(())
            }
            ResolveErrorKind::DuplicateDefinition {
                name,
                previous_span,
            } => {
                write!(
                    f,
                    "[{}..{}] duplicate definition of `{}`, previously defined at [{}..{}]",
                    self.span.start, self.span.end, name, previous_span.start, previous_span.end,
                )
            }
            ResolveErrorKind::UndefinedType { name, suggestion } => {
                write!(
                    f,
                    "[{}..{}] cannot find type `{}`",
                    self.span.start, self.span.end, name
                )?;
                if let Some(sug) = suggestion {
                    write!(f, ". Did you mean `{}`?", sug)?;
                }
                Ok(())
            }
            ResolveErrorKind::UndefinedTrait { name } => {
                write!(
                    f,
                    "[{}..{}] cannot find trait `{}`",
                    self.span.start, self.span.end, name
                )
            }
            ResolveErrorKind::UndefinedVariant { variant, enum_name } => {
                write!(
                    f,
                    "[{}..{}] enum variant `{}` not found in enum `{}`",
                    self.span.start, self.span.end, variant, enum_name
                )
            }
            ResolveErrorKind::ImportNotFound { path } => {
                write!(
                    f,
                    "[{}..{}] cannot find import `{}`",
                    self.span.start, self.span.end, path
                )
            }
            ResolveErrorKind::CircularImport { path } => {
                write!(
                    f,
                    "[{}..{}] circular import detected: {}",
                    self.span.start, self.span.end, path
                )
            }
        }
    }
}

/// Specific kind of resolve error.
#[derive(Debug, Clone)]
pub enum ResolveErrorKind {
    /// Name used but never declared.
    UndefinedName {
        name: String,
        suggestion: Option<String>,
    },

    /// Same name declared twice in the same scope.
    DuplicateDefinition { name: String, previous_span: Span },

    /// Type name not found.
    UndefinedType {
        name: String,
        suggestion: Option<String>,
    },

    /// Trait name not found.
    UndefinedTrait { name: String },

    /// Enum variant not found.
    UndefinedVariant { variant: String, enum_name: String },

    /// Import path not found.
    ImportNotFound { path: String },

    /// Circular import detected.
    CircularImport { path: String },
}

/// Compute Levenshtein distance between two strings.
pub fn levenshtein(a: &str, b: &str) -> usize {
    let a_len = a.len();
    let b_len = b.len();

    if a_len == 0 {
        return b_len;
    }
    if b_len == 0 {
        return a_len;
    }

    let mut prev: Vec<usize> = (0..=b_len).collect();
    let mut curr = vec![0; b_len + 1];

    for (i, ca) in a.chars().enumerate() {
        curr[0] = i + 1;
        for (j, cb) in b.chars().enumerate() {
            let cost = if ca == cb { 0 } else { 1 };
            curr[j + 1] = (prev[j] + cost).min(prev[j + 1] + 1).min(curr[j] + 1);
        }
        std::mem::swap(&mut prev, &mut curr);
    }

    prev[b_len]
}

/// Find the best "did you mean" suggestion from a list of candidates.
/// Returns `None` if no candidate is within distance 2.
pub fn suggest(name: &str, candidates: &[&str]) -> Option<String> {
    let mut best: Option<(usize, &str)> = None;

    for &candidate in candidates {
        let dist = levenshtein(name, candidate);
        if dist <= 2 && dist > 0 {
            if best.is_none() || dist < best.unwrap().0 {
                best = Some((dist, candidate));
            }
        }
    }

    best.map(|(_, s)| s.to_string())
}
