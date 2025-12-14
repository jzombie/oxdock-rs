use anyhow::{Context, Result, bail};
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Clone, Copy, Debug)]
enum AccessMode {
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

    pub fn set_root(&mut self, root: &Path) {
        self.root = root.to_path_buf();
    }

    pub fn resolve_workdir(&self, current: &Path, new_dir: &str) -> Result<PathBuf> {
        if new_dir == "/" {
            return fs::canonicalize(self.root()).or_else(|_| Ok(self.root.clone()));
        }
        let candidate = if Path::new(new_dir).is_absolute() {
            PathBuf::from(new_dir)
        } else {
            current.join(new_dir)
        };

        let resolved = check_access(&self.root, &candidate, AccessMode::Passthru)
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
            format!(
                "failed to canonicalize created WORKDIR {}",
                resolved.display()
            )
        })?;
        Ok(final_abs)
    }

    pub fn resolve_read(&self, cwd: &Path, rel: &str) -> Result<PathBuf> {
        self.resolve(cwd, rel, AccessMode::Read)
    }

    pub fn resolve_write(&self, cwd: &Path, rel: &str) -> Result<PathBuf> {
        self.resolve(cwd, rel, AccessMode::Write)
    }

    pub fn resolve_copy_source(&self, from: &str) -> Result<PathBuf> {
        if Path::new(from).is_absolute() {
            bail!("COPY source must be relative to build context");
        }
        let candidate = self.build_context.join(from);
        let guarded = check_access(&self.build_context, &candidate, AccessMode::Read)
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

        check_access(&self.root, &candidate, mode)
    }
}

fn check_access(root: &Path, candidate: &Path, mode: AccessMode) -> Result<PathBuf> {
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
