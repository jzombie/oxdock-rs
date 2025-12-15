// Workspace-scoped path resolver with guarded file operations.
// Methods are split across submodules by concern (access checks, IO, copy, git, resolve helpers).
use std::path::{Path, PathBuf};

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
    root: PathBuf,
    build_context: PathBuf,
}

impl PathResolver {
    pub fn new(root: &Path, build_context: &Path) -> Self {
        Self {
            root: root.to_path_buf(),
            build_context: build_context.to_path_buf(),
        }
    }

    pub fn root(&self) -> &Path {
        &self.root
    }

    pub fn build_context(&self) -> &Path {
        &self.build_context
    }

    pub fn set_root(&mut self, root: &Path) {
        self.root = root.to_path_buf();
    }
}

mod access;
mod copy;
mod git;
mod io;
mod resolve;

// PathResolver is exported at crate root via `lib.rs` re-export.
