use anyhow::{Context, Result, bail};
#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
use std::path::Path;

use super::{AccessMode, PathResolver};
use crate::GuardedPath;

// Path resolution helpers (WORKDIR, READ/WRITE, COPY sources).
#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
impl PathResolver {
    pub fn resolve_workdir(&self, current: &GuardedPath, new_dir: &str) -> Result<GuardedPath> {
        if new_dir == "/" {
            // Reset to the resolver root when WORKDIR is set to '/'.
            return Ok(self.root().clone());
        }
        let candidate = if Path::new(new_dir).is_absolute() {
            Path::new(new_dir).to_path_buf()
        } else {
            current.as_path().join(new_dir)
        };

        let resolved = self
            .check_access(&candidate, AccessMode::Passthru)
            .with_context(|| format!("WORKDIR {} escapes root", candidate.display()))?;
        self.backend.resolve_workdir(resolved)
    }

    pub fn resolve_read(&self, cwd: &GuardedPath, rel: &str) -> Result<GuardedPath> {
        self.resolve(cwd, rel, AccessMode::Read)
    }

    pub fn resolve_write(&self, cwd: &GuardedPath, rel: &str) -> Result<GuardedPath> {
        self.resolve(cwd, rel, AccessMode::Write)
    }

    pub fn resolve_copy_source(&self, from: &str) -> Result<GuardedPath> {
        if Path::new(from).is_absolute() {
            bail!("COPY source must be relative to build context");
        }
        let candidate = self.build_context.as_path().join(from);

        match self
            .check_access_with_root(&self.build_context, &candidate, AccessMode::Read)
            .with_context(|| format!("failed to resolve COPY source {}", candidate.display()))
        {
            Ok(guarded) => self.backend.resolve_copy_source(guarded),
            Err(primary) => {
                if let Some(workspace_root) = &self.workspace_root {
                    let workspace_candidate = workspace_root.as_path().join(from);
                    if let Ok(workspace_guarded) = self.check_access_with_root(
                        workspace_root,
                        &workspace_candidate,
                        AccessMode::Read,
                    ) {
                        return self.backend.resolve_copy_source(workspace_guarded);
                    }
                }
                Err(primary)
            }
        }
    }

    fn resolve(&self, cwd: &GuardedPath, rel: &str, mode: AccessMode) -> Result<GuardedPath> {
        let candidate = if Path::new(rel).is_absolute() {
            Path::new(rel).to_path_buf()
        } else {
            cwd.as_path().join(rel)
        };

        self.check_access(&candidate, mode)
    }
}
