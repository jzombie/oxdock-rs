use anyhow::Result;

/// Returns true if the filesystem is running in an isolated environment (e.g. Miri)
/// where access to the host filesystem is restricted.
pub fn is_isolated() -> bool {
    cfg!(miri)
}

pub mod workspace_fs;
pub use workspace_fs::command_path;
pub use workspace_fs::{DirEntry, EntryKind, GuardedPath, GuardedTempDir, PathResolver};

#[allow(clippy::disallowed_types)]
pub use workspace_fs::UnguardedPath;

#[cfg(feature = "mock-fs")]
pub use workspace_fs::mock::MockFs;

/// Trait implemented by both `GuardedPath` and `UnguardedPath` so callers can
/// rely on a consistent set of path helper methods. This includes a small set
/// of constructors and a `root` accessor so code that needs to treat either
/// type homogenously can do so without ad-hoc helper functions.
#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
pub trait PathLike: Sized + std::fmt::Display {
    fn as_path(&self) -> &std::path::Path;
    fn root(&self) -> &std::path::Path;
    fn to_path_buf(&self) -> std::path::PathBuf;
    fn join(&self, rel: &str) -> Result<Self>;
    fn parent(&self) -> Option<Self>;
    // Constructors analogous to those on `GuardedPath`.
    fn new_from_str(root: &str, candidate: &str) -> Result<Self>;
    fn new_root(root: &std::path::Path) -> Result<Self>;
    fn new_root_from_str(root: &str) -> Result<Self> {
        Self::new_root(std::path::Path::new(root))
    }
}

/// Trait representing the workspace-scoped filesystem operations provided by
/// this crate. `PathResolver` implements this trait and existing behavior is
/// preserved; the trait exists to allow generic consumers or test doubles to
/// depend on the abstraction rather than the concrete type.
pub trait WorkspaceFs {
    fn canonicalize_abs(&self, path: &GuardedPath) -> Result<GuardedPath>;
    fn metadata_abs(&self, path: &GuardedPath) -> Result<std::fs::Metadata>;
    #[allow(clippy::disallowed_types)]
    fn metadata_external(&self, path: &UnguardedPath) -> Result<std::fs::Metadata>;
    fn root(&self) -> &GuardedPath;
    fn build_context(&self) -> &GuardedPath;
    fn set_root(&mut self, root: GuardedPath);

    fn read_file(&self, path: &GuardedPath) -> Result<Vec<u8>>;
    fn read_to_string(&self, path: &GuardedPath) -> Result<String>;
    fn read_dir_entries(&self, path: &GuardedPath) -> Result<Vec<DirEntry>>;

    fn write_file(&self, path: &GuardedPath, contents: &[u8]) -> Result<()>;
    fn create_dir_all_abs(&self, path: &GuardedPath) -> Result<()>;
    fn remove_file_abs(&self, path: &GuardedPath) -> Result<()>;
    fn remove_dir_all_abs(&self, path: &GuardedPath) -> Result<()>;

    fn copy_file(&self, src: &GuardedPath, dst: &GuardedPath) -> Result<u64>;
    fn copy_dir_recursive(&self, src: &GuardedPath, dst: &GuardedPath) -> Result<()>;
    #[allow(clippy::disallowed_types)]
    fn copy_dir_from_external(&self, src: &UnguardedPath, dst: &GuardedPath) -> Result<()>;
    #[allow(clippy::disallowed_types)]
    fn copy_file_from_external(&self, src: &UnguardedPath, dst: &GuardedPath) -> Result<u64>;

    fn symlink(&self, src: &GuardedPath, dst: &GuardedPath) -> Result<()>;

    #[allow(clippy::disallowed_types)]
    fn open_external_file(&self, path: &UnguardedPath) -> Result<std::fs::File>;
    fn set_permissions_mode_unix(&self, path: &GuardedPath, mode: u32) -> Result<()>;

    fn resolve_workdir(&self, current: &GuardedPath, new_dir: &str) -> Result<GuardedPath>;
    fn resolve_read(&self, cwd: &GuardedPath, rel: &str) -> Result<GuardedPath>;
    fn resolve_write(&self, cwd: &GuardedPath, rel: &str) -> Result<GuardedPath>;
    fn resolve_copy_source(&self, from: &str) -> Result<GuardedPath>;

    fn entry_kind(&self, path: &GuardedPath) -> Result<EntryKind>;

    fn copy_from_git(&self, rev: &str, from: &str, to: &GuardedPath) -> Result<()>;
}

impl WorkspaceFs for PathResolver {
    fn canonicalize_abs(&self, path: &GuardedPath) -> Result<GuardedPath> {
        PathResolver::canonicalize_abs(self, path)
    }

    fn metadata_abs(&self, path: &GuardedPath) -> Result<std::fs::Metadata> {
        PathResolver::metadata_abs(self, path)
    }

    #[allow(clippy::disallowed_types)]
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

    fn read_dir_entries(&self, path: &GuardedPath) -> Result<Vec<DirEntry>> {
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

    #[allow(clippy::disallowed_types)]
    fn copy_dir_from_external(&self, src: &UnguardedPath, dst: &GuardedPath) -> Result<()> {
        PathResolver::copy_dir_from_external(self, src, dst)
    }

    #[allow(clippy::disallowed_types)]
    fn copy_file_from_external(&self, src: &UnguardedPath, dst: &GuardedPath) -> Result<u64> {
        PathResolver::copy_file_from_external(self, src, dst)
    }

    fn symlink(&self, src: &GuardedPath, dst: &GuardedPath) -> Result<()> {
        PathResolver::symlink(self, src, dst)
    }

    #[allow(clippy::disallowed_types)]
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

    fn entry_kind(&self, path: &GuardedPath) -> Result<EntryKind> {
        PathResolver::entry_kind(self, path)
    }

    fn copy_from_git(&self, rev: &str, from: &str, to: &GuardedPath) -> Result<()> {
        PathResolver::copy_from_git(self, rev, from, to)
    }
}
