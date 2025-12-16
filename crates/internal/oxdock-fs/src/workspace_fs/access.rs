#[cfg(not(miri))]
use anyhow::Context;
use anyhow::{Result, bail};
#[cfg_attr(miri, allow(clippy::disallowed_types, clippy::disallowed_methods))]
use std::path::Path;
#[cfg(miri)]
#[cfg_attr(miri, allow(clippy::disallowed_types, clippy::disallowed_methods))]
use std::path::PathBuf;

use super::{AccessMode, GuardedPath, PathResolver};

/// Ensure `candidate` stays within `root`, even if parts of the path do not yet exist.
#[cfg_attr(miri, allow(clippy::disallowed_types, clippy::disallowed_methods))]
pub(crate) fn guard_path(
    root: &Path,
    candidate: &Path,
    mode: AccessMode,
) -> Result<std::path::PathBuf> {
    #[cfg(miri)]
    {
        guard_path_miri(root, candidate, mode)
    }

    #[cfg(not(miri))]
    {
        if !root.exists() {
            std::fs::create_dir_all(root)
                .with_context(|| format!("failed to create root {}", root.display()))?;
        }
        let root_abs = std::fs::canonicalize(root)
            .with_context(|| format!("failed to canonicalize root {}", root.display()))?;

        if let Ok(cand_abs) = std::fs::canonicalize(candidate) {
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

        let ancestor_abs = std::fs::canonicalize(ancestor)
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
}

#[cfg(miri)]
#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
fn guard_path_miri(root: &Path, candidate: &Path, mode: AccessMode) -> Result<PathBuf> {
    // Avoid host filesystem syscalls under Miri by normalizing paths purely in memory.
    let root_abs = if root.is_absolute() {
        normalize_no_fs(root)
    } else {
        normalize_no_fs(&Path::new("/miri").join(root))
    };

    let candidate_abs = if candidate.is_absolute() {
        normalize_no_fs(candidate)
    } else {
        normalize_no_fs(&root_abs.join(candidate))
    };

    if !candidate_abs.starts_with(&root_abs) {
        bail!(
            "{} access to {} escapes allowed root {}",
            mode.name(),
            candidate_abs.display(),
            root_abs.display()
        );
    }

    Ok(candidate_abs)
}

#[cfg(miri)]
#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
fn normalize_no_fs(path: &Path) -> PathBuf {
    let mut parts: Vec<PathBuf> = Vec::new();
    let is_abs = path.is_absolute();

    for comp in path.components() {
        match comp {
            std::path::Component::RootDir => parts.clear(),
            std::path::Component::CurDir => {}
            std::path::Component::ParentDir => {
                if let Some(last) = parts.last() && !last.as_os_str().is_empty() {
                    parts.pop();
                }
            }
            std::path::Component::Normal(seg) => parts.push(seg.into()),
            std::path::Component::Prefix(p) => parts.push(PathBuf::from(p.as_os_str())),
        }
    }

    let mut out = if is_abs {
        PathBuf::from("/")
    } else {
        PathBuf::new()
    };
    for seg in parts {
        out.push(seg);
    }
    out
}

// Guard and canonicalize paths under the configured roots.
impl PathResolver {
    #[cfg_attr(miri, allow(clippy::disallowed_types, clippy::disallowed_methods))]
    pub(crate) fn check_access_with_root(
        &self,
        root: &GuardedPath,
        candidate: &Path,
        mode: AccessMode,
    ) -> Result<GuardedPath> {
        let guarded = guard_path(root.as_path(), candidate, mode)?;
        #[cfg(miri)]
        {
            Ok(GuardedPath::from_guarded_parts(
                root.root().to_path_buf(),
                guarded,
            ))
        }

        #[cfg(not(miri))]
        Ok(GuardedPath::from_guarded_parts(root.to_path_buf(), guarded))
    }

    #[cfg_attr(miri, allow(clippy::disallowed_types, clippy::disallowed_methods))]
    pub(crate) fn check_access(&self, candidate: &Path, mode: AccessMode) -> Result<GuardedPath> {
        self.check_access_with_root(&self.root, candidate, mode)
    }
}
