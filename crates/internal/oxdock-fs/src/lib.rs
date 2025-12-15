use anyhow::Result;
use std::path::{Path, PathBuf};

pub mod workspace_fs;
pub use workspace_fs::PathResolver;

use workspace_fs::access::guard_path;
use workspace_fs::AccessMode;

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

    /// Create a guard where the root is the path itself.
    pub fn new_root(root: &Path) -> Result<Self> {
        let guarded = guard_path(root, root, AccessMode::Passthru)?;
        Ok(Self {
            root: guarded.clone(),
            path: guarded,
        })
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

    pub(crate) fn from_guarded_parts(root: PathBuf, path: PathBuf) -> Self {
        Self { root, path }
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

/// Trait representing the workspace-scoped filesystem operations provided by
/// this crate. `PathResolver` implements this trait and existing behavior is
/// preserved; the trait exists to allow generic consumers or test doubles to
/// depend on the abstraction rather than the concrete type.
pub trait WorkspaceFs {
    fn canonicalize_abs(&self, path: &GuardedPath) -> Result<GuardedPath>;
    fn metadata_abs(&self, path: &GuardedPath) -> Result<std::fs::Metadata>;
    fn metadata_external(&self, path: &UnguardedPath) -> Result<std::fs::Metadata>;
    fn root(&self) -> &GuardedPath;
    fn build_context(&self) -> &GuardedPath;
    fn set_root(&mut self, root: GuardedPath);

    fn read_file(&self, path: &GuardedPath) -> Result<Vec<u8>>;
    fn read_to_string(&self, path: &GuardedPath) -> Result<String>;
    fn read_dir_entries(&self, path: &GuardedPath) -> Result<Vec<std::fs::DirEntry>>;

    fn write_file(&self, path: &GuardedPath, contents: &[u8]) -> Result<()>;
    fn create_dir_all_abs(&self, path: &GuardedPath) -> Result<()>;
    fn remove_file_abs(&self, path: &GuardedPath) -> Result<()>;
    fn remove_dir_all_abs(&self, path: &GuardedPath) -> Result<()>;

    fn copy_file(&self, src: &GuardedPath, dst: &GuardedPath) -> Result<u64>;
    fn copy_dir_recursive(&self, src: &GuardedPath, dst: &GuardedPath) -> Result<()>;
    fn copy_dir_from_external(&self, src: &UnguardedPath, dst: &GuardedPath) -> Result<()>;
    fn copy_file_from_external(&self, src: &UnguardedPath, dst: &GuardedPath) -> Result<u64>;

    fn symlink(&self, src: &GuardedPath, dst: &GuardedPath) -> Result<()>;

    fn open_external_file(&self, path: &UnguardedPath) -> Result<std::fs::File>;
    fn set_permissions_mode_unix(&self, path: &GuardedPath, mode: u32) -> Result<()>;

    fn resolve_workdir(&self, current: &GuardedPath, new_dir: &str) -> Result<GuardedPath>;
    fn resolve_read(&self, cwd: &GuardedPath, rel: &str) -> Result<GuardedPath>;
    fn resolve_write(&self, cwd: &GuardedPath, rel: &str) -> Result<GuardedPath>;
    fn resolve_copy_source(&self, from: &str) -> Result<GuardedPath>;

    fn copy_from_git(&self, rev: &str, from: &str, to: &GuardedPath) -> Result<()>;
}

impl WorkspaceFs for PathResolver {
    fn canonicalize_abs(&self, path: &GuardedPath) -> Result<GuardedPath> {
        PathResolver::canonicalize_abs(self, path)
    }

    fn metadata_abs(&self, path: &GuardedPath) -> Result<std::fs::Metadata> {
        PathResolver::metadata_abs(self, path)
    }

    fn metadata_external(&self, path: &UnguardedPath) -> Result<std::fs::Metadata> {
        PathResolver::metadata_external(self, path)
    }

    fn root(&self) -> &GuardedPath {
        PathResolver::root(self)
    }

    fn build_context(&self) -> &GuardedPath {
        PathResolver::build_context(self)
    }

    fn set_root(&mut self, root: GuardedPath) {
        PathResolver::set_root(self, root)
    }

    fn read_file(&self, path: &GuardedPath) -> Result<Vec<u8>> {
        PathResolver::read_file(self, path)
    }

    fn read_to_string(&self, path: &GuardedPath) -> Result<String> {
        PathResolver::read_to_string(self, path)
    }

    fn read_dir_entries(&self, path: &GuardedPath) -> Result<Vec<std::fs::DirEntry>> {
        PathResolver::read_dir_entries(self, path)
    }

    fn write_file(&self, path: &GuardedPath, contents: &[u8]) -> Result<()> {
        PathResolver::write_file(self, path, contents)
    }

    fn create_dir_all_abs(&self, path: &GuardedPath) -> Result<()> {
        PathResolver::create_dir_all_abs(self, path)
    }

    fn remove_file_abs(&self, path: &GuardedPath) -> Result<()> {
        PathResolver::remove_file_abs(self, path)
    }

    fn remove_dir_all_abs(&self, path: &GuardedPath) -> Result<()> {
        PathResolver::remove_dir_all_abs(self, path)
    }

    fn copy_file(&self, src: &GuardedPath, dst: &GuardedPath) -> Result<u64> {
        PathResolver::copy_file(self, src, dst)
    }

    fn copy_dir_recursive(&self, src: &GuardedPath, dst: &GuardedPath) -> Result<()> {
        PathResolver::copy_dir_recursive(self, src, dst)
    }

    fn copy_dir_from_external(&self, src: &UnguardedPath, dst: &GuardedPath) -> Result<()> {
        PathResolver::copy_dir_from_external(self, src, dst)
    }

    fn copy_file_from_external(&self, src: &UnguardedPath, dst: &GuardedPath) -> Result<u64> {
        PathResolver::copy_file_from_external(self, src, dst)
    }

    fn symlink(&self, src: &GuardedPath, dst: &GuardedPath) -> Result<()> {
        PathResolver::symlink(self, src, dst)
    }

    fn open_external_file(&self, path: &UnguardedPath) -> Result<std::fs::File> {
        PathResolver::open_external_file(self, path)
    }

    fn set_permissions_mode_unix(&self, path: &GuardedPath, mode: u32) -> Result<()> {
        PathResolver::set_permissions_mode_unix(self, path, mode)
    }

    fn resolve_workdir(&self, current: &GuardedPath, new_dir: &str) -> Result<GuardedPath> {
        PathResolver::resolve_workdir(self, current, new_dir)
    }

    fn resolve_read(&self, cwd: &GuardedPath, rel: &str) -> Result<GuardedPath> {
        PathResolver::resolve_read(self, cwd, rel)
    }

    fn resolve_write(&self, cwd: &GuardedPath, rel: &str) -> Result<GuardedPath> {
        PathResolver::resolve_write(self, cwd, rel)
    }

    fn resolve_copy_source(&self, from: &str) -> Result<GuardedPath> {
        PathResolver::resolve_copy_source(self, from)
    }

    fn copy_from_git(&self, rev: &str, from: &str, to: &GuardedPath) -> Result<()> {
        PathResolver::copy_from_git(self, rev, from, to)
    }
}
