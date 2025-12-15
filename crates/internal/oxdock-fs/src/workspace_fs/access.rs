use anyhow::{Context, Result, bail};
use std::fs;
use std::path::Path;

use super::{AccessMode, PathResolver};
use crate::GuardedPath;

/// Ensure `candidate` stays within `root`, even if parts of the path do not yet exist.
pub(crate) fn guard_path(root: &Path, candidate: &Path, mode: AccessMode) -> Result<std::path::PathBuf> {
    if !root.exists() {
        fs::create_dir_all(root)
            .with_context(|| format!("failed to create root {}", root.display()))?;
    }
    let root_abs = fs::canonicalize(root)
        .with_context(|| format!("failed to canonicalize root {}", root.display()))?;

    if let Ok(cand_abs) = fs::canonicalize(candidate) {
        if !cand_abs.starts_with(&root_abs) {
            bail!(
                "{} access to {} escapes allowed root {}",
                mode.name(),
                cand_abs.display(),
                root_abs.display()
            );
        }
        return Ok(cand_abs);
    }

    let mut ancestor = candidate;
    while !ancestor.exists() {
        if let Some(parent) = ancestor.parent() {
            ancestor = parent;
        } else {
            ancestor = root;
            break;
        }
    }

    let ancestor_abs = fs::canonicalize(ancestor)
        .with_context(|| format!("failed to canonicalize ancestor {}", ancestor.display()))?;

    let mut rem_components: Vec<std::ffi::OsString> = Vec::new();
    {
        let mut skip = ancestor.components();
        let mut full = candidate.components();
        loop {
            match (skip.next(), full.next()) {
                (Some(s), Some(f)) if s == f => continue,
                (_opt_s, opt_f) => {
                    if let Some(f) = opt_f {
                        rem_components.push(f.as_os_str().to_os_string());
                        for comp in full {
                            rem_components.push(comp.as_os_str().to_os_string());
                        }
                    }
                    break;
                }
            }
        }
    }

    let mut cand_abs = ancestor_abs.clone();
    for c in rem_components.iter() {
        let s = std::ffi::OsStr::new(&c);
        if s == "." {
            continue;
        }
        if s == ".." {
            cand_abs = cand_abs
                .parent()
                .map(|p| p.to_path_buf())
                .unwrap_or(cand_abs);
            continue;
        }
        cand_abs.push(s);
    }

    if !cand_abs.starts_with(&root_abs) {
        bail!(
            "{} access to {} escapes allowed root {}",
            mode.name(),
            cand_abs.display(),
            root_abs.display()
        );
    }

    Ok(cand_abs)
}

// Guard and canonicalize paths under the configured roots.
impl PathResolver {
    #[allow(clippy::disallowed_methods)]
    pub(crate) fn check_access_with_root(
        &self,
        root: &GuardedPath,
        candidate: &Path,
        mode: AccessMode,
    ) -> Result<GuardedPath> {
        let guarded = guard_path(root.as_path(), candidate, mode)?;
        Ok(GuardedPath::from_guarded_parts(
            root.to_path_buf(),
            guarded,
        ))
    }

    pub(crate) fn check_access(
        &self,
        candidate: &Path,
        mode: AccessMode,
    ) -> Result<GuardedPath> {
        self.check_access_with_root(&self.root, candidate, mode)
    }
}
