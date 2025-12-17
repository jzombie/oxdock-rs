use crate::{GuardedPath, UnguardedPath};

/// Defines the guard policy for an operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GuardPolicy {
    /// The operation is guarded and restricted to the workspace.
    Guarded,
    /// The operation is unguarded and may access the host system.
    Unguarded,
}

/// A path that carries its guard policy.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PolicyPath {
    /// A path that is guarded.
    Guarded(GuardedPath),
    /// A path that is unguarded.
    Unguarded(UnguardedPath),
}

impl PolicyPath {
    /// Returns the guard policy of this path.
    pub fn policy(&self) -> GuardPolicy {
        match self {
            Self::Guarded(_) => GuardPolicy::Guarded,
            Self::Unguarded(_) => GuardPolicy::Unguarded,
        }
    }

    /// Returns the inner guarded path if this is a guarded path.
    pub fn as_guarded(&self) -> Option<&GuardedPath> {
        match self {
            Self::Guarded(p) => Some(p),
            _ => None,
        }
    }

    /// Returns the inner unguarded path if this is an unguarded path.
    pub fn as_unguarded(&self) -> Option<&UnguardedPath> {
        match self {
            Self::Unguarded(p) => Some(p),
            _ => None,
        }
    }

    pub fn to_path_buf(&self) -> std::path::PathBuf {
        match self {
            Self::Guarded(p) => p.as_path().to_path_buf(),
            Self::Unguarded(p) => p.as_path().to_path_buf(),
        }
    }
}

impl From<GuardedPath> for PolicyPath {
    fn from(path: GuardedPath) -> Self {
        Self::Guarded(path)
    }
}

impl From<UnguardedPath> for PolicyPath {
    fn from(path: UnguardedPath) -> Self {
        Self::Unguarded(path)
    }
}
