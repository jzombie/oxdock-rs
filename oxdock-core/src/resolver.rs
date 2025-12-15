use anyhow::{Context, Result, bail};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use tempfile::tempdir;

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
    /// This method extracts blobs/trees using `git` plumbing (`git show` or `git archive`)
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
        let _ = check_access(&self.root, &self.build_context, AccessMode::Read)
            .with_context(|| format!("build context {} not under root", self.build_context.display()))?;

        // Create a tempdir to hold extracted content if needed
        let tmp = tempdir().with_context(|| "failed to create tempdir for git extraction")?;
        let tmp_path = tmp.path().to_path_buf();

        // First try to treat `from` as a file via `git show`.
        let show = Command::new("git")
            .arg("-C")
            .arg(&self.build_context)
            .arg("show")
            .arg(format!("{}:{}", rev, from))
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .output();

        match show {
            Ok(out) if out.status.success() => {
                // It's a file/blob; write stdout bytes to destination file (create parent dirs)
                if let Some(parent) = to.parent() {
                    fs::create_dir_all(parent)
                        .with_context(|| format!("creating parent {}", parent.display()))?;
                }
                fs::write(to, &out.stdout)
                    .with_context(|| format!("writing git blob to {}", to.display()))?;
                return Ok(());
            }
            _ => {
                // Not a blob; fall through to try archive for tree
            }
        }

        // Use `git archive` to extract the tree at rev:from into tmp
        // git -C <build_context> archive --format=tar <rev> <from>
        let mut git = Command::new("git")
            .arg("-C")
            .arg(&self.build_context)
            .arg("archive")
            .arg("--format=tar")
            .arg(rev)
            .arg(from)
            .stdout(Stdio::piped())
            .spawn()
            .with_context(|| format!("failed to spawn git archive for {}:{}", rev, from))?;

        // Extract with tar -x -C tmp
        let mut tar = Command::new("tar")
            .arg("-x")
            .arg("-C")
            .arg(&tmp_path)
            .stdin(git.stdout.take().unwrap())
            .spawn()
            .with_context(|| "failed to spawn tar to extract git archive")?;

        let git_status = git.wait().with_context(|| "git archive failed")?;
        if !git_status.success() {
            bail!("git archive failed for {}:{}", rev, from);
        }
        let tar_status = tar.wait().with_context(|| "tar extraction failed")?;
        if !tar_status.success() {
            bail!("tar extraction failed for {}:{}", rev, from);
        }

        // tmp/<from> should now contain the extracted tree (from may be nested)
        let extracted = tmp_path.join(from);
        if !extracted.exists() {
            bail!("path {} not found in git archive for rev {}", from, rev);
        }

        // Copy extracted into `to`. If extracted is a dir, copy its contents into `to`.
        if extracted.is_dir() {
            // create parent directories for `to`
            if let Some(parent) = to.parent() {
                fs::create_dir_all(parent)
                    .with_context(|| format!("creating parent {}", parent.display()))?;
            }
            copy_dir_recursive(&extracted, to)?;
        } else if extracted.is_file() {
            if let Some(parent) = to.parent() {
                fs::create_dir_all(parent)
                    .with_context(|| format!("creating parent {}", parent.display()))?;
            }
            fs::copy(&extracted, to)
                .with_context(|| format!("copying {} to {}", extracted.display(), to.display()))?;
        } else {
            bail!("unsupported file type in git archive: {}", extracted.display());
        }

        // tmp will be dropped and cleaned up here
        Ok(())
    }

}

fn copy_dir_recursive(src: &Path, dst: &Path) -> Result<()> {
    fs::create_dir_all(dst).with_context(|| format!("creating dir {}", dst.display()))?;
    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let file_type = entry.file_type()?;
        let src_path = entry.path();
        let dst_path = dst.join(entry.file_name());
        if file_type.is_dir() {
            copy_dir_recursive(&src_path, &dst_path)?;
        } else if file_type.is_file() {
            fs::copy(&src_path, &dst_path).with_context(|| {
                format!("copying {} to {}", src_path.display(), dst_path.display())
            })?;
        } else {
            bail!("unsupported file type: {}", src_path.display());
        }
    }
    Ok(())
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
