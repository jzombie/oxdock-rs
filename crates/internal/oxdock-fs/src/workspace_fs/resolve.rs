#![allow(clippy::disallowed_types, clippy::disallowed_methods)]

use anyhow::{Context, Result, bail};
use std::fs;
use std::path::Path;

use super::{AccessMode, PathResolver};
#[cfg(miri)]
use crate::EntryKind;
use crate::GuardedPath;

// Path resolution helpers (WORKDIR, READ/WRITE, COPY sources).
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

        #[cfg(not(miri))]
        {
            if let Ok(meta) = fs::metadata(resolved.as_path()) {
                if meta.is_dir() {
                    let canon = fs::canonicalize(resolved.as_path())
                        .unwrap_or_else(|_| resolved.to_path_buf());
                    return GuardedPath::new(resolved.root(), &canon);
                }
                bail!("WORKDIR path is not a directory: {}", resolved.display());
            }

            fs::create_dir_all(resolved.as_path())
                .with_context(|| format!("failed to create WORKDIR {}", resolved.display()))?;
            let final_abs =
                fs::canonicalize(resolved.as_path()).unwrap_or_else(|_| resolved.to_path_buf());
            GuardedPath::new(resolved.root(), &final_abs)
        }

        #[cfg(miri)]
        {
            let rel = Self::normalize_rel(&resolved);
            let state = self.state_for_guard(&resolved);

            if resolved.as_path().exists() {
                let meta = fs::metadata(resolved.as_path())
                    .with_context(|| format!("failed to stat {}", resolved.display()))?;
                if !meta.is_dir() {
                    bail!("WORKDIR path is not a directory: {}", resolved.display());
                }
                {
                    let mut state_mut = state.borrow_mut();
                    if state_mut.entry_kind(&rel).is_none() {
                        state_mut.ensure_dir(&rel);
                    }
                }
            } else {
                {
                    let mut state_mut = state.borrow_mut();
                    if state_mut.entry_kind(&rel).is_none() {
                        state_mut.ensure_dir(&rel);
                    }
                }
                if let Some(parent) = resolved.as_path().parent() {
                    fs::create_dir_all(parent)
                        .with_context(|| format!("creating dir {}", parent.display()))?;
                }
                fs::create_dir_all(resolved.as_path())
                    .with_context(|| format!("creating WORKDIR {}", resolved.display()))?;
            }

            let final_abs =
                fs::canonicalize(resolved.as_path()).unwrap_or_else(|_| resolved.to_path_buf());
            GuardedPath::new(resolved.root(), &final_abs)
        }
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
        let guarded = self
            .check_access_with_root(&self.build_context, &candidate, AccessMode::Read)
            .with_context(|| format!("failed to resolve COPY source {}", candidate.display()))?;

        #[cfg(not(miri))]
        {
            if !guarded.as_path().exists() {
                bail!(
                    "COPY source missing in build context: {}",
                    guarded.display()
                );
            }
            Ok(guarded)
        }

        #[cfg(miri)]
        {
            let rel = Self::normalize_rel(&guarded);
            let kind = self
                .state_for_guard(&guarded)
                .borrow()
                .entry_kind(&rel)
                .ok_or_else(|| {
                    anyhow::anyhow!(
                        "COPY source missing in build context: {}",
                        guarded.display()
                    )
                })?;
            if matches!(kind, EntryKind::Dir | EntryKind::File) {
                Ok(guarded)
            } else {
                bail!("COPY source unsupported kind: {}", guarded.display());
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
