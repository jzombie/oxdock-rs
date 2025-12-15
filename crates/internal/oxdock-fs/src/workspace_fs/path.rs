#![allow(clippy::disallowed_types, clippy::disallowed_methods)]

use anyhow::Result;
use std::path::{Path, PathBuf};
use super::{AccessMode, guard_path};


/// Path guaranteed to stay within a guard root. The root is stored alongside the
/// resolved absolute path so consumers cannot escape without constructing a new
/// guard explicitly.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct GuardedPath {
    root: PathBuf,
    path: PathBuf,
}

impl GuardedPath {
    pub fn new(root: &Path, candidate: &Path) -> Result<Self> {
        let guarded = guard_path(root, candidate, AccessMode::Passthru)?;
        Ok(Self {
            root: root.to_path_buf(),
            path: guarded,
        })
    }

    /// Build a guard from string paths without requiring callers to reference `std::path` types.
    pub fn new_from_str(root: &str, candidate: &str) -> Result<Self> {
        Self::new(Path::new(root), Path::new(candidate))
    }

    /// Create a guard where the root is the path itself.
    pub fn new_root(root: &Path) -> Result<Self> {
        let guarded = guard_path(root, root, AccessMode::Passthru)?;
        Ok(Self {
            root: guarded.clone(),
            path: guarded,
        })
    }

    /// Build a root guard from a string path without exposing `std::path` types to callers.
    pub fn new_root_from_str(root: &str) -> Result<Self> {
        Self::new_root(Path::new(root))
    }

    pub fn as_path(&self) -> &Path {
        &self.path
    }

    pub fn root(&self) -> &Path {
        &self.root
    }

    pub fn to_path_buf(&self) -> PathBuf {
        self.path.clone()
    }

    pub fn display(&self) -> impl std::fmt::Display + '_ {
        self.path.display()
    }

    pub fn join(&self, rel: &str) -> Result<Self> {
        GuardedPath::new(&self.root, &self.path.join(rel))
    }

    /// Return the parent directory as a guarded path, if it exists within the same root.
    pub fn parent(&self) -> Option<Self> {
        self.path
            .parent()
            .and_then(|p| GuardedPath::new(&self.root, p).ok())
    }

    pub(crate) fn from_guarded_parts(root: PathBuf, path: PathBuf) -> Self {
        Self { root, path }
    }
}

/// Trait implemented by both `GuardedPath` and `UnguardedPath` so callers can
/// rely on a consistent set of path helper methods. This includes a small set
/// of constructors and a `root` accessor so code that needs to treat either
/// type homogenously can do so without ad-hoc helper functions.
pub trait PathLike: Sized + std::fmt::Display {
    fn as_path(&self) -> &Path;
    fn root(&self) -> &Path;
    fn to_path_buf(&self) -> PathBuf;
    fn join(&self, rel: &str) -> Result<Self>;
    fn parent(&self) -> Option<Self>;

    // Constructors analogous to those on `GuardedPath`.
    fn new_from_str(root: &str, candidate: &str) -> Result<Self>;
    fn new_root(root: &Path) -> Result<Self>;
    fn new_root_from_str(root: &str) -> Result<Self> {
        Self::new_root(Path::new(root))
    }
}

impl std::fmt::Display for GuardedPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.path.display().fmt(f)
    }
}

// Replace old impl with PathLike implementation
impl PathLike for GuardedPath {
    fn as_path(&self) -> &Path {
        self.as_path()
    }

    fn root(&self) -> &Path {
        self.root()
    }

    fn to_path_buf(&self) -> PathBuf {
        self.to_path_buf()
    }

    fn join(&self, rel: &str) -> Result<Self> {
        self.join(rel)
    }

    fn parent(&self) -> Option<Self> {
        self.parent()
    }

    fn new_from_str(root: &str, candidate: &str) -> Result<Self> {
        GuardedPath::new(Path::new(root), Path::new(candidate))
    }

    fn new_root(root: &Path) -> Result<Self> {
        GuardedPath::new_root(root)
    }
}

/// Path wrapper that intentionally skips guard checks. Use only for paths that
/// originate outside the guarded workspace (e.g., external file handles).
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct UnguardedPath {
    path: PathBuf,
}

impl UnguardedPath {
    pub fn new(path: impl Into<PathBuf>) -> Self {
        Self { path: path.into() }
    }

    pub fn as_path(&self) -> &Path {
        &self.path
    }

    pub fn to_path_buf(&self) -> PathBuf {
        self.path.clone()
    }
}

impl std::fmt::Display for UnguardedPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.path.display().fmt(f)
    }
}

impl PathLike for UnguardedPath {
    fn as_path(&self) -> &Path {
        self.as_path()
    }

    fn root(&self) -> &Path {
        // For UnguardedPath the "root" is the path itself; callers expecting a
        // root should handle this semantic difference.
        self.as_path()
    }

    fn to_path_buf(&self) -> PathBuf {
        self.to_path_buf()
    }

    fn join(&self, rel: &str) -> Result<Self> {
        Ok(UnguardedPath::new(self.path.join(rel)))
    }

    fn parent(&self) -> Option<Self> {
        self.path.parent().map(|p| UnguardedPath::new(p.to_path_buf()))
    }

    fn new_from_str(_root: &str, candidate: &str) -> Result<Self> {
        Ok(UnguardedPath::new(Path::new(candidate).to_path_buf()))
    }

    fn new_root(root: &Path) -> Result<Self> {
        Ok(UnguardedPath::new(root.to_path_buf()))
    }
}
