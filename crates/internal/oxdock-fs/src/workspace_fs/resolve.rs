use anyhow::{Context, Result, bail};
use std::fs;
use std::path::{Path, PathBuf};

use super::{AccessMode, PathResolver};

// Path resolution helpers (WORKDIR, READ/WRITE, COPY sources).
impl PathResolver {
    #[allow(clippy::disallowed_methods)]
    pub fn resolve_workdir(&self, current: &Path, new_dir: &str) -> Result<PathBuf> {
        if new_dir == "/" {
            return fs::canonicalize(self.root()).or_else(|_| Ok(self.root.clone()));
        }
        let candidate = if Path::new(new_dir).is_absolute() {
            PathBuf::from(new_dir)
        } else {
            current.join(new_dir)
        };

        let resolved = self
            .check_access(&candidate, AccessMode::Passthru)
            .with_context(|| format!("WORKDIR {} escapes root", candidate.display()))?;

        if let Ok(meta) = fs::metadata(&resolved) {
            if meta.is_dir() {
                return fs::canonicalize(&resolved).or(Ok(resolved));
            }
            bail!("WORKDIR path is not a directory: {}", resolved.display());
        }

        fs::create_dir_all(&resolved)
            .with_context(|| format!("failed to create WORKDIR {}", resolved.display()))?;
        let final_abs = fs::canonicalize(&resolved).with_context(|| {
            format!("failed to canonicalize created WORKDIR {}", resolved.display())
        })?;
        Ok(final_abs)
    }

    pub fn resolve_read(&self, cwd: &Path, rel: &str) -> Result<PathBuf> {
        self.resolve(cwd, rel, AccessMode::Read)
    }

    pub fn resolve_write(&self, cwd: &Path, rel: &str) -> Result<PathBuf> {
        self.resolve(cwd, rel, AccessMode::Write)
    }

    #[allow(clippy::disallowed_methods)]
    pub fn resolve_copy_source(&self, from: &str) -> Result<PathBuf> {
        if Path::new(from).is_absolute() {
            bail!("COPY source must be relative to build context");
        }
        let candidate = self.build_context.join(from);
        let guarded = self
            .check_access_with_root(&self.build_context, &candidate, AccessMode::Read)
            .with_context(|| format!("failed to resolve COPY source {}", candidate.display()))?;

        if !guarded.exists() {
            bail!(
                "COPY source missing in build context: {}",
                guarded.display()
            );
        }

        let cand_abs = fs::canonicalize(&guarded)
            .with_context(|| format!("failed to canonicalize COPY source {}", guarded.display()))?;
        Ok(cand_abs)
    }

    fn resolve(&self, cwd: &Path, rel: &str, mode: AccessMode) -> Result<PathBuf> {
        let candidate = if Path::new(rel).is_absolute() {
            PathBuf::from(rel)
        } else {
            cwd.join(rel)
        };

        self.check_access(&candidate, mode)
    }
}
