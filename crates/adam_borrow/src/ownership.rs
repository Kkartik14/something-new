//! Ownership and borrow state tracking for variables.
//!
//! Each variable in scope has a `VarState` indicating whether it is
//! currently owned, moved, borrowed, or mutably borrowed. The
//! `OwnershipTracker` maintains a map of variable names to their
//! current states and provides methods for state transitions.

use std::collections::HashMap;

use adam_ast::common::Span;

/// The current ownership / borrow state of a variable.
#[derive(Debug, Clone, PartialEq)]
pub enum VarState {
    /// The variable owns its value — it can be used, moved, or borrowed.
    Owned,

    /// The variable's value has been moved out.
    Moved { moved_at: Span },

    /// The variable is currently shared-borrowed `count` times.
    Borrowed { count: u32 },

    /// The variable is currently mutably borrowed.
    MutBorrowed { span: Span },

    /// Some fields of this variable have been moved out but others remain.
    PartiallyMoved,
}

/// Tracks ownership and borrow state for every variable in scope.
#[derive(Debug, Clone)]
pub struct OwnershipTracker {
    /// Variable name -> current state.
    vars: HashMap<String, VarState>,
    /// Variable name -> whether it is declared `mut`.
    mutability: HashMap<String, bool>,
}

impl OwnershipTracker {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            mutability: HashMap::new(),
        }
    }

    /// Mark a variable as owned (freshly declared or reassigned).
    pub fn mark_owned(&mut self, name: &str, mutable: bool) {
        self.vars.insert(name.to_string(), VarState::Owned);
        self.mutability.insert(name.to_string(), mutable);
    }

    /// Mark a variable as moved at the given span.
    pub fn mark_moved(&mut self, name: &str, span: Span) {
        self.vars
            .insert(name.to_string(), VarState::Moved { moved_at: span });
    }

    /// Add a shared borrow to the variable.
    pub fn mark_borrowed(&mut self, name: &str) {
        let entry = self.vars.entry(name.to_string()).or_insert(VarState::Owned);
        match entry {
            VarState::Borrowed { count } => *count += 1,
            VarState::Owned => {
                *entry = VarState::Borrowed { count: 1 };
            }
            _ => {
                // If moved or mut-borrowed, we leave the state — the checker
                // will detect the conflict and emit an error.
            }
        }
    }

    /// Release one shared borrow.
    pub fn release_borrow(&mut self, name: &str) {
        if let Some(state) = self.vars.get_mut(name) {
            if let VarState::Borrowed { count } = state {
                if *count <= 1 {
                    *state = VarState::Owned;
                } else {
                    *count -= 1;
                }
            }
        }
    }

    /// Mutably borrow the variable.
    pub fn mark_mut_borrowed(&mut self, name: &str, span: Span) {
        self.vars
            .insert(name.to_string(), VarState::MutBorrowed { span });
    }

    /// Release a mutable borrow.
    pub fn release_mut_borrow(&mut self, name: &str) {
        if let Some(state) = self.vars.get_mut(name) {
            if matches!(state, VarState::MutBorrowed { .. }) {
                *state = VarState::Owned;
            }
        }
    }

    /// Mark a variable as partially moved.
    pub fn mark_partially_moved(&mut self, name: &str) {
        self.vars.insert(name.to_string(), VarState::PartiallyMoved);
    }

    /// Check whether a variable can be used (read). Returns `None` if ok,
    /// or `Some(state)` with the problematic state if not.
    pub fn check_usable(&self, name: &str) -> Option<&VarState> {
        match self.vars.get(name) {
            Some(state @ VarState::Moved { .. }) => Some(state),
            Some(state @ VarState::PartiallyMoved) => Some(state),
            _ => None,
        }
    }

    /// Check whether a variable can be mutably borrowed. Returns `None` if ok,
    /// or `Some(state)` with the problematic state if not.
    pub fn check_mut_borrowable(&self, name: &str) -> Option<&VarState> {
        match self.vars.get(name) {
            Some(state @ VarState::Moved { .. }) => Some(state),
            Some(state @ VarState::Borrowed { .. }) => Some(state),
            Some(state @ VarState::MutBorrowed { .. }) => Some(state),
            Some(state @ VarState::PartiallyMoved) => Some(state),
            _ => None,
        }
    }

    /// Check whether a variable can be shared-borrowed. Returns `None` if ok,
    /// or `Some(state)` with the problematic state if not.
    pub fn check_borrowable(&self, name: &str) -> Option<&VarState> {
        match self.vars.get(name) {
            Some(state @ VarState::Moved { .. }) => Some(state),
            Some(state @ VarState::MutBorrowed { .. }) => Some(state),
            Some(state @ VarState::PartiallyMoved) => Some(state),
            _ => None,
        }
    }

    /// Get the current state of a variable, if tracked.
    pub fn get_state(&self, name: &str) -> Option<&VarState> {
        self.vars.get(name)
    }

    /// Check if a variable is declared as mutable.
    pub fn is_mutable(&self, name: &str) -> bool {
        self.mutability.get(name).copied().unwrap_or(false)
    }

    /// Check if a type name represents a Copy type (primitives that are
    /// implicitly copied on use and never moved).
    pub fn is_copy_type(type_name: &str) -> bool {
        matches!(
            type_name,
            "i8" | "i16"
                | "i32"
                | "i64"
                | "u8"
                | "u16"
                | "u32"
                | "u64"
                | "f32"
                | "f64"
                | "bool"
                | "char"
        )
    }

    /// Snapshot the current state so we can merge branches later.
    pub fn snapshot(&self) -> HashMap<String, VarState> {
        self.vars.clone()
    }

    /// Restore from a snapshot.
    pub fn restore(&mut self, snapshot: HashMap<String, VarState>) {
        self.vars = snapshot;
    }

    /// Merge two branch snapshots conservatively: if a variable is moved in
    /// *either* branch, it is considered moved after the merge.
    pub fn merge_branches(
        &mut self,
        before: &HashMap<String, VarState>,
        branch_a: &HashMap<String, VarState>,
        branch_b: &HashMap<String, VarState>,
    ) {
        for (name, state_before) in before {
            let state_a = branch_a.get(name).unwrap_or(state_before);
            let state_b = branch_b.get(name).unwrap_or(state_before);

            let merged = match (state_a, state_b) {
                // Both branches agree — keep that state.
                (a, b) if a == b => a.clone(),
                // If either branch moved it, it is considered moved.
                (VarState::Moved { moved_at }, _) => VarState::Moved {
                    moved_at: *moved_at,
                },
                (_, VarState::Moved { moved_at }) => VarState::Moved {
                    moved_at: *moved_at,
                },
                // Partially moved in either branch.
                (VarState::PartiallyMoved, _) | (_, VarState::PartiallyMoved) => {
                    VarState::PartiallyMoved
                }
                // Otherwise keep the more restrictive state from branch A.
                (a, _) => a.clone(),
            };

            self.vars.insert(name.clone(), merged);
        }
    }

    /// Return all tracked variable names.
    pub fn tracked_names(&self) -> Vec<String> {
        self.vars.keys().cloned().collect()
    }
}

impl Default for OwnershipTracker {
    fn default() -> Self {
        Self::new()
    }
}

/// Tracks the origin of a borrow — what a reference points to.
#[derive(Debug, Clone, PartialEq)]
pub enum BorrowOrigin {
    /// Points to a local variable declared at a given scope depth.
    Local { name: String, scope_depth: usize },
    /// Points to a function parameter (always valid for the function's lifetime).
    Param { name: String },
    /// Unknown origin (e.g. from a function call).
    Unknown,
}
