pub mod workspace_fs;
pub use workspace_fs::PathResolver;

// Re-export std path types so dependent crates can avoid importing `std::path` directly.
pub mod path {
    pub use std::path::{Path, PathBuf};
}
use anyhow::Result;
use std::path::{Path, PathBuf};

/// Trait representing the workspace-scoped filesystem operations provided by
/// this crate. `PathResolver` implements this trait and existing behavior is
/// preserved; the trait exists to allow generic consumers or test doubles to
/// depend on the abstraction rather than the concrete type.
pub trait WorkspaceFs {
    fn canonicalize_abs(&self, path: &Path) -> Result<PathBuf>;
    fn metadata_abs(&self, path: &Path) -> Result<std::fs::Metadata>;
    fn metadata_external(&self, path: &Path) -> Result<std::fs::Metadata>;
    fn root(&self) -> &Path;
    fn build_context(&self) -> &Path;
    fn set_root(&mut self, root: &Path);

    fn read_file(&self, path: &Path) -> Result<Vec<u8>>;
    fn read_to_string(&self, path: &Path) -> Result<String>;
    fn read_dir_entries(&self, path: &Path) -> Result<Vec<std::fs::DirEntry>>;

    fn write_file(&self, path: &Path, contents: &[u8]) -> Result<()>;
    fn create_dir_all_abs(&self, path: &Path) -> Result<()>;
    fn remove_file_abs(&self, path: &Path) -> Result<()>;
    fn remove_dir_all_abs(&self, path: &Path) -> Result<()>;

    fn copy_file(&self, src: &Path, dst: &Path) -> Result<u64>;
    fn copy_dir_recursive(&self, src: &Path, dst: &Path) -> Result<()>;
    fn copy_dir_from_external(&self, src: &Path, dst: &Path) -> Result<()>;
    fn copy_file_from_external(&self, src: &Path, dst: &Path) -> Result<u64>;

    fn symlink(&self, src: &Path, dst: &Path) -> Result<()>;

    fn open_external_file(&self, path: &Path) -> Result<std::fs::File>;
    fn set_permissions_mode_unix(&self, path: &Path, mode: u32) -> Result<()>;

    fn resolve_workdir(&self, current: &Path, new_dir: &str) -> Result<PathBuf>;
    fn resolve_read(&self, cwd: &Path, rel: &str) -> Result<PathBuf>;
    fn resolve_write(&self, cwd: &Path, rel: &str) -> Result<PathBuf>;
    fn resolve_copy_source(&self, from: &str) -> Result<PathBuf>;

    fn copy_from_git(&self, rev: &str, from: &str, to: &Path) -> Result<()>;
}

impl WorkspaceFs for PathResolver {
    fn canonicalize_abs(&self, path: &Path) -> Result<PathBuf> {
        PathResolver::canonicalize_abs(self, path)
    }

    fn metadata_abs(&self, path: &Path) -> Result<std::fs::Metadata> {
        PathResolver::metadata_abs(self, path)
    }

    fn metadata_external(&self, path: &Path) -> Result<std::fs::Metadata> {
        PathResolver::metadata_external(self, path)
    }

    fn read_file(&self, path: &Path) -> Result<Vec<u8>> {
        PathResolver::read_file(self, path)
    }

    fn read_to_string(&self, path: &Path) -> Result<String> {
        PathResolver::read_to_string(self, path)
    }

    fn read_dir_entries(&self, path: &Path) -> Result<Vec<std::fs::DirEntry>> {
        PathResolver::read_dir_entries(self, path)
    }

    fn write_file(&self, path: &Path, contents: &[u8]) -> Result<()> {
        PathResolver::write_file(self, path, contents)
    }

    fn create_dir_all_abs(&self, path: &Path) -> Result<()> {
        PathResolver::create_dir_all_abs(self, path)
    }

    fn remove_file_abs(&self, path: &Path) -> Result<()> {
        PathResolver::remove_file_abs(self, path)
    }

    fn remove_dir_all_abs(&self, path: &Path) -> Result<()> {
        PathResolver::remove_dir_all_abs(self, path)
    }

    fn copy_file(&self, src: &Path, dst: &Path) -> Result<u64> {
        PathResolver::copy_file(self, src, dst)
    }

    fn copy_dir_recursive(&self, src: &Path, dst: &Path) -> Result<()> {
        PathResolver::copy_dir_recursive(self, src, dst)
    }

    fn copy_dir_from_external(&self, src: &Path, dst: &Path) -> Result<()> {
        PathResolver::copy_dir_from_external(self, src, dst)
    }

    fn copy_file_from_external(&self, src: &Path, dst: &Path) -> Result<u64> {
        PathResolver::copy_file_from_external(self, src, dst)
    }

    fn symlink(&self, src: &Path, dst: &Path) -> Result<()> {
        PathResolver::symlink(self, src, dst)
    }

    fn open_external_file(&self, path: &Path) -> Result<std::fs::File> {
        PathResolver::open_external_file(self, path)
    }

    fn set_permissions_mode_unix(&self, path: &Path, mode: u32) -> Result<()> {
        PathResolver::set_permissions_mode_unix(self, path, mode)
    }

    fn resolve_workdir(&self, current: &Path, new_dir: &str) -> Result<PathBuf> {
        PathResolver::resolve_workdir(self, current, new_dir)
    }

    fn resolve_read(&self, cwd: &Path, rel: &str) -> Result<PathBuf> {
        PathResolver::resolve_read(self, cwd, rel)
    }

    fn resolve_write(&self, cwd: &Path, rel: &str) -> Result<PathBuf> {
        PathResolver::resolve_write(self, cwd, rel)
    }

    fn resolve_copy_source(&self, from: &str) -> Result<PathBuf> {
        PathResolver::resolve_copy_source(self, from)
    }

    fn copy_from_git(&self, rev: &str, from: &str, to: &Path) -> Result<()> {
        PathResolver::copy_from_git(self, rev, from, to)
    }

    fn root(&self) -> &Path {
        PathResolver::root(self)
    }

    fn build_context(&self) -> &Path {
        PathResolver::build_context(self)
    }

    fn set_root(&mut self, root: &Path) {
        PathResolver::set_root(self, root)
    }
}
