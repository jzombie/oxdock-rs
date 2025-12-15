// Workspace-scoped path resolver with guarded file operations.
// Methods are split across submodules by concern (access checks, IO, copy, git, resolve helpers).

use anyhow::{Context, Result};

#[allow(clippy::disallowed_types)]
use std::path::Path;

pub mod path;
pub use path::{GuardedPath, UnguardedPath};

#[derive(Clone, Copy, Debug)]
pub(crate) enum AccessMode {
    Read,
    Write,
    Passthru,
}

impl AccessMode {
    fn name(&self) -> &'static str {
        match self {
            AccessMode::Read => "READ",
            AccessMode::Write => "WRITE",
            AccessMode::Passthru => "PASSTHRU",
        }
    }
}

/// Resolves and validates filesystem paths within a confined workspace and build context.
pub struct PathResolver {
    root: GuardedPath,
    build_context: GuardedPath,
}

impl PathResolver {
    /// Build a resolver rooted at `CARGO_MANIFEST_DIR`, using that same
    /// directory as the build context. This centralizes env lookup and path
    /// creation so callers avoid ad-hoc path construction.
    #[allow(clippy::disallowed_types, clippy::disallowed_methods)]
    pub fn from_manifest_env() -> Result<Self> {
        let manifest_dir =
            std::env::var("CARGO_MANIFEST_DIR").context("CARGO_MANIFEST_DIR missing")?;
        let path = Path::new(&manifest_dir);
        Self::new(path, path)
    }

    #[allow(clippy::disallowed_types)]
    pub fn new(root: &Path, build_context: &Path) -> Result<Self> {
        Ok(Self {
            root: GuardedPath::new_root(root)?,
            build_context: GuardedPath::new_root(build_context)?,
        })
    }

    pub fn root(&self) -> &GuardedPath {
        &self.root
    }

    pub fn build_context(&self) -> &GuardedPath {
        &self.build_context
    }

    pub fn set_root(&mut self, root: GuardedPath) {
        self.root = root;
    }
}

pub(crate) mod access;
mod copy;
mod git;
mod io;
mod resolve;
use access::guard_path;

// PathResolver is exported at crate root via `lib.rs` re-export.
