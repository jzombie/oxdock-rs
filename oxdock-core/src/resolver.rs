use anyhow::{Context, Result, bail};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

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

    /// Copy from a git revision/path into the provided destination path.
    /// This method extracts blobs/trees using `git` plumbing (`git show`)
    /// and copies the material into `to`. `to` should be an absolute path already
    /// validated by the caller.
    pub fn copy_from_git(&self, rev: &str, from: &str, to: &Path) -> Result<()> {
        if Path::new(from).is_absolute() {
            bail!("COPY_GIT source must be relative to build context");
        }

        // Ensure destination is within allowed root (don't trust caller)
        let _ = check_access(&self.root, to, AccessMode::Write)
            .with_context(|| format!("destination {} escapes allowed root", to.display()))?;

        // Ensure build_context is allowed under root
        let _ =
            check_access(&self.root, &self.build_context, AccessMode::Read).with_context(|| {
                format!(
                    "build context {} not under root",
                    self.build_context.display()
                )
            })?;

        // First check the object type at `rev:from` to avoid treating trees as blobs.
        let cat_type = Command::new("git")
            .arg("-C")
            .arg(&self.build_context)
            .arg("cat-file")
            .arg("-t")
            .arg(format!("{}:{}", rev, from))
            .output();

        if let Ok(tout) = cat_type
            && tout.status.success()
        {
            let typ = String::from_utf8_lossy(&tout.stdout).trim().to_string();
            if typ == "blob" {
                // It's a file/blob; use git show to extract contents and write file
                let show = Command::new("git")
                    .arg("-C")
                    .arg(&self.build_context)
                    .arg("show")
                    .arg(format!("{}:{}", rev, from))
                    .output()
                    .with_context(|| format!("failed to run git show for {}:{}", rev, from))?;

                if show.status.success() {
                    if let Some(parent) = to.parent() {
                        fs::create_dir_all(parent)
                            .with_context(|| format!("creating parent {}", parent.display()))?;
                    }
                    fs::write(to, &show.stdout)
                        .with_context(|| format!("writing git blob to {}", to.display()))?;
                    return Ok(());
                }
            }
            // if typ is tree/commit/other, fallthrough to tree listing
        }

        // Fallback: list tree entries and stream blobs via git show (no `tar` dependency).
        let ls = Command::new("git")
            .arg("-C")
            .arg(&self.build_context)
            .arg("ls-tree")
            .arg("-r")
            .arg("-z")
            .arg(rev)
            .arg("--")
            .arg(from)
            .output()
            .with_context(|| format!("failed to run git ls-tree for {}:{}", rev, from))?;

        if !ls.status.success() {
            bail!("git ls-tree failed for {}:{}", rev, from);
        }

        let out = ls.stdout;
        if out.is_empty() {
            bail!("path {} not found in rev {}", from, rev);
        }

        // Each entry is NUL-separated and has the form: "<mode> <type> <hash>\t<path>\0"
        for chunk in out.split(|b| *b == 0) {
            if chunk.is_empty() {
                continue;
            }
            if let Some(tab_pos) = chunk.iter().position(|b| *b == b'\t') {
                let header = &chunk[..tab_pos];
                let path = &chunk[tab_pos + 1..];
                let header_str = String::from_utf8_lossy(header);
                let mut parts = header_str.split_whitespace();
                let mode = parts.next().unwrap_or("");
                let typ = parts.next().unwrap_or("");
                let _hash = parts.next().unwrap_or("");

                let path_str = String::from_utf8_lossy(path).into_owned();
                let rel = if path_str.starts_with(&format!("{}/", from)) {
                    path_str[from.len() + 1..].to_string()
                } else if path_str == from {
                    String::new()
                } else {
                    path_str.clone()
                };

                let dest = if rel.is_empty() {
                    to.to_path_buf()
                } else {
                    to.join(&rel)
                };

                // Ensure parent directory exists
                if let Some(parent) = dest.parent() {
                    fs::create_dir_all(parent)
                        .with_context(|| format!("creating parent {}", parent.display()))?;
                }

                match typ {
                    "blob" => {
                        let blob = Command::new("git")
                            .arg("-C")
                            .arg(&self.build_context)
                            .arg("show")
                            .arg(format!("{}:{}", rev, path_str))
                            .output()
                            .with_context(|| {
                                format!("failed to run git show for {}:{}", rev, path_str)
                            })?;
                        if !blob.status.success() {
                            bail!("git show failed for {}:{}", rev, path_str);
                        }

                        if mode == "120000" {
                            let target = String::from_utf8_lossy(&blob.stdout).into_owned();
                            #[cfg(unix)]
                            {
                                use std::os::unix::fs::symlink;
                                symlink(target.trim_end_matches('\n'), &dest).with_context(
                                    || format!("creating symlink {} -> {}", dest.display(), target),
                                )?;
                            }
                            #[cfg(windows)]
                            {
                                fs::write(&dest, &blob.stdout).with_context(|| {
                                    format!("writing symlink placeholder {}", dest.display())
                                })?;
                            }
                        } else {
                            fs::write(&dest, &blob.stdout)
                                .with_context(|| format!("writing blob to {}", dest.display()))?;
                        }
                    }
                    "commit" => {
                        bail!("submodule entries are not supported: {}", path_str);
                    }
                    _ => {
                        // ignore other types
                    }
                }
            }
        }

        Ok(())
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
