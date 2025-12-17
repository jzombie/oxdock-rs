use super::AccessMode;
use super::PathResolver;
use super::guard_path;
use crate::PathLike;
use anyhow::Result;
use std::borrow::Cow;
#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
use std::path::{Path, PathBuf};
#[cfg(miri)]
use std::sync::atomic::{AtomicUsize, Ordering};
#[allow(clippy::disallowed_types)]
use tempfile::{Builder, TempDir};

#[cfg(miri)]
static TEMP_COUNTER: AtomicUsize = AtomicUsize::new(0);

/// Path guaranteed to stay within a guard root. The root is stored alongside the
/// resolved absolute path so consumers cannot escape without constructing a new
/// guard explicitly.
#[derive(Clone, Debug, Eq, PartialEq)]
#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
pub struct GuardedPath {
    root: PathBuf,
    path: PathBuf,
}

impl GuardedPath {
    #[allow(clippy::disallowed_types, clippy::disallowed_methods)]
    pub fn new(root: &Path, candidate: &Path) -> Result<Self> {
        let guarded = guard_path(root, candidate, AccessMode::Passthru)?;
        Ok(Self {
            root: root.to_path_buf(),
            path: guarded,
        })
    }

    /// Build a guard from string paths without requiring callers to reference `std::path` types.
    #[allow(clippy::disallowed_types, clippy::disallowed_methods)]
    pub fn new_from_str(root: &str, candidate: &str) -> Result<Self> {
        Self::new(Path::new(root), Path::new(candidate))
    }

    /// Create a guard where the root is the path itself.
    #[allow(clippy::disallowed_types, clippy::disallowed_methods)]
    pub fn new_root(root: &Path) -> Result<Self> {
        let guarded = guard_path(root, root, AccessMode::Passthru)?;
        Ok(Self {
            root: guarded.clone(),
            path: guarded,
        })
    }

    /// Build a root guard from a string path without exposing `std::path` types to callers.
    #[allow(clippy::disallowed_types, clippy::disallowed_methods)]
    pub fn new_root_from_str(root: &str) -> Result<Self> {
        Self::new_root(Path::new(root))
    }

    /// Create a guarded temporary directory using `tempfile::Builder`.
    #[cfg(not(miri))]
    pub fn tempdir() -> Result<GuardedTempDir> {
        Self::tempdir_with(|_| {})
    }

    /// Create a guarded temporary directory with a custom `tempfile::Builder`
    /// configuration (e.g., prefixes). The temporary directory is deleted when
    /// the returned `GuardedTempDir` is dropped unless it is persisted.
    #[cfg(not(miri))]
    #[allow(clippy::disallowed_types, clippy::disallowed_methods)]
    pub fn tempdir_with<F>(configure: F) -> Result<GuardedTempDir>
    where
        F: FnOnce(&mut Builder),
    {
        let mut builder = Builder::new();
        configure(&mut builder);
        let tempdir = builder.tempdir()?;
        let guard = GuardedPath::new_root(tempdir.path())?;
        Ok(GuardedTempDir::new(guard, Some(tempdir)))
    }

    /// Create a synthetic temporary directory when running under Miri.
    /// No host filesystem operations are performed to keep isolation intact.
    #[cfg(miri)]
    pub fn tempdir() -> Result<GuardedTempDir> {
        Self::tempdir_with(|_| {})
    }

    /// Miri-safe variant of `tempdir_with` that allocates a synthetic root path
    /// without touching the host filesystem.
    #[cfg(miri)]
    #[allow(clippy::disallowed_types)]
    pub fn tempdir_with<F>(_configure: F) -> Result<GuardedTempDir>
    where
        F: FnOnce(&mut Builder),
    {
        let id = TEMP_COUNTER.fetch_add(1, Ordering::Relaxed);
        let path = PathBuf::from(format!("/miri/tempdir-{id}"));
        let guard = GuardedPath {
            root: path.clone(),
            path,
        };
        Ok(GuardedTempDir::new(guard, None))
    }

    #[allow(clippy::disallowed_types)]
    pub fn as_path(&self) -> &Path {
        &self.path
    }

    /// Backend-aware existence check that avoids host `stat` when running under Miri.
    pub fn exists(&self) -> bool {
        PathResolver::new(self.root(), self.root())
            .map(|resolver| resolver.exists(self))
            .unwrap_or(false)
    }

    #[allow(clippy::disallowed_types)]
    pub fn root(&self) -> &Path {
        &self.root
    }

    #[allow(clippy::disallowed_types)]
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
        let parent = self.path.parent()?.to_path_buf();
        GuardedPath::new(&self.root, &parent).ok()
    }

    #[allow(clippy::disallowed_types, clippy::disallowed_methods)]
    pub(crate) fn from_guarded_parts(root: PathBuf, path: PathBuf) -> Self {
        Self { root, path }
    }
}

#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
impl std::fmt::Display for GuardedPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.path.display().fmt(f)
    }
}

#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
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

    #[allow(clippy::disallowed_types, clippy::disallowed_methods)]
    fn new_root(root: &Path) -> Result<Self> {
        GuardedPath::new_root(root)
    }
}

/// Temporary directory that cleans itself up on drop while exposing the guarded path.
#[allow(clippy::disallowed_types)]
pub struct GuardedTempDir {
    guard: Option<GuardedPath>,
    tempdir: Option<TempDir>,
}

impl GuardedTempDir {
    #[allow(clippy::disallowed_types)]
    fn new(guard: GuardedPath, tempdir: Option<TempDir>) -> Self {
        Self {
            guard: Some(guard),
            tempdir,
        }
    }

    pub fn as_guarded_path(&self) -> &GuardedPath {
        self.guard.as_ref().unwrap()
    }

    /// Prevent automatic cleanup and return the guarded path rooted at the
    /// temporary directory.
    #[allow(deprecated)]
    pub fn persist(mut self) -> GuardedPath {
        if let Some(tempdir) = self.tempdir.take() {
            let _ = tempdir.into_path();
        }
        self.guard.take().unwrap()
    }

    #[allow(clippy::disallowed_types)]
    pub fn into_parts(mut self) -> (Option<TempDir>, GuardedPath) {
        (self.tempdir.take(), self.guard.take().unwrap())
    }
}

impl std::ops::Deref for GuardedTempDir {
    type Target = GuardedPath;

    fn deref(&self) -> &Self::Target {
        self.guard.as_ref().unwrap()
    }
}

impl Drop for GuardedTempDir {
    fn drop(&mut self) {
        #[cfg(miri)]
        {
            if let Some(guard) = &self.guard {
                super::backend::cleanup_synthetic_root(guard.root());
            }
        }
    }
}

#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
impl std::fmt::Display for GuardedTempDir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.guard.as_ref().unwrap().fmt(f)
    }
}

/// Path wrapper that intentionally skips guard checks. Use only for paths that
/// originate outside the guarded workspace (e.g., external file handles).
#[derive(Clone, Debug, Eq, PartialEq)]
#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
pub struct UnguardedPath {
    path: PathBuf,
}

#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
impl UnguardedPath {
    #[allow(clippy::disallowed_types, clippy::disallowed_methods)]
    pub fn new(path: impl Into<PathBuf>) -> Self {
        Self { path: path.into() }
    }

    #[allow(clippy::disallowed_types)]
    pub fn as_path(&self) -> &Path {
        &self.path
    }

    #[allow(clippy::disallowed_types)]
    pub fn to_path_buf(&self) -> PathBuf {
        self.path.clone()
    }
}

#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
impl std::fmt::Display for UnguardedPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.path.display().fmt(f)
    }
}

#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
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
        self.path
            .parent()
            .map(|p| UnguardedPath::new(p.to_path_buf()))
    }

    fn new_from_str(_root: &str, candidate: &str) -> Result<Self> {
        Ok(UnguardedPath::new(Path::new(candidate).to_path_buf()))
    }

    #[allow(clippy::disallowed_types, clippy::disallowed_methods)]
    fn new_root(root: &Path) -> Result<Self> {
        Ok(UnguardedPath::new(root.to_path_buf()))
    }
}

/// Convert a guarded path into a platform-normalized `Path` suitable for
/// passing to system `Command` APIs. This lives in `oxdock-fs` so that
/// usage of `std::path::Path` stays inside the crate that is allowed to touch
/// host path types and syscalls.
#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
pub fn command_path(path: &GuardedPath) -> Cow<'_, std::path::Path> {
    use std::path::{Component, Prefix};

    let mut components = path.as_path().components();
    if let Some(Component::Prefix(prefix)) = components.next() {
        match prefix.kind() {
            Prefix::VerbatimDisk(drive) => {
                let mut buf = PathBuf::from(format!("{}:", char::from(drive)));
                buf.extend(components);
                return Cow::Owned(buf);
            }
            Prefix::VerbatimUNC(server, share) => {
                let mut buf = PathBuf::from(r"\\");
                buf.push(server);
                buf.push(share);
                buf.extend(components);
                return Cow::Owned(buf);
            }
            Prefix::Verbatim(_) => {
                let mut buf = PathBuf::new();
                buf.push(prefix.as_os_str());
                buf.extend(components);
                return Cow::Owned(buf);
            }
            _ => {}
        }
    }

    Cow::Borrowed(path.as_path())
}

/// Convert a guarded path into a string with forward slashes, suitable for
/// use with `rust-embed` or other tools that require normalized paths.
/// This strips Windows verbatim prefixes and ensures separators are `/`.
#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
pub fn embed_path(path: &GuardedPath) -> String {
    let cmd = command_path(path);
    let s = cmd.to_string_lossy();
    if std::path::MAIN_SEPARATOR == '\\' {
        s.replace('\\', "/")
    } else {
        s.into_owned()
    }
}
