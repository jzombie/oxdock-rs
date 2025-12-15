pub mod resolver;
pub use resolver::PathResolver;

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
		self.canonicalize_abs(path)
	}

	fn metadata_abs(&self, path: &Path) -> Result<std::fs::Metadata> {
		self.metadata_abs(path)
	}

	fn metadata_external(&self, path: &Path) -> Result<std::fs::Metadata> {
		self.metadata_external(path)
	}

	fn read_file(&self, path: &Path) -> Result<Vec<u8>> {
		self.read_file(path)
	}

	fn read_to_string(&self, path: &Path) -> Result<String> {
		self.read_to_string(path)
	}

	fn read_dir_entries(&self, path: &Path) -> Result<Vec<std::fs::DirEntry>> {
		self.read_dir_entries(path)
	}

	fn write_file(&self, path: &Path, contents: &[u8]) -> Result<()> {
		self.write_file(path, contents)
	}

	fn create_dir_all_abs(&self, path: &Path) -> Result<()> {
		self.create_dir_all_abs(path)
	}

	fn remove_file_abs(&self, path: &Path) -> Result<()> {
		self.remove_file_abs(path)
	}

	fn remove_dir_all_abs(&self, path: &Path) -> Result<()> {
		self.remove_dir_all_abs(path)
	}

	fn copy_file(&self, src: &Path, dst: &Path) -> Result<u64> {
		self.copy_file(src, dst)
	}

	fn copy_dir_recursive(&self, src: &Path, dst: &Path) -> Result<()> {
		self.copy_dir_recursive(src, dst)
	}

	fn copy_dir_from_external(&self, src: &Path, dst: &Path) -> Result<()> {
		self.copy_dir_from_external(src, dst)
	}

	fn copy_file_from_external(&self, src: &Path, dst: &Path) -> Result<u64> {
		self.copy_file_from_external(src, dst)
	}

	fn open_external_file(&self, path: &Path) -> Result<std::fs::File> {
		self.open_external_file(path)
	}

	fn set_permissions_mode_unix(&self, path: &Path, mode: u32) -> Result<()> {
		self.set_permissions_mode_unix(path, mode)
	}

	fn resolve_workdir(&self, current: &Path, new_dir: &str) -> Result<PathBuf> {
		self.resolve_workdir(current, new_dir)
	}

	fn resolve_read(&self, cwd: &Path, rel: &str) -> Result<PathBuf> {
		self.resolve_read(cwd, rel)
	}

	fn resolve_write(&self, cwd: &Path, rel: &str) -> Result<PathBuf> {
		self.resolve_write(cwd, rel)
	}

	fn resolve_copy_source(&self, from: &str) -> Result<PathBuf> {
		self.resolve_copy_source(from)
	}

	fn copy_from_git(&self, rev: &str, from: &str, to: &Path) -> Result<()> {
		self.copy_from_git(rev, from, to)
	}

	fn root(&self) -> &Path {
		self.root()
	}

	fn build_context(&self) -> &Path {
		self.build_context()
	}

	fn set_root(&mut self, root: &Path) {
		self.set_root(root)
	}
}
