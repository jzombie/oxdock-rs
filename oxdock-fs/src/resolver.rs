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

/// Resolves and validates filesystem paths within a confined workspace.
///
/// `PathResolver` centralizes logic for resolving paths used by the system
/// while ensuring they do not escape configured roots. It provides safe
/// helpers for reading, writing, copying, and canonicalizing paths that are
/// constrained to live under the configured `root` (and, when appropriate,
/// the `build_context`). The resolver also offers helpers that allow looking
/// up sources from the build context (for example, `COPY`/`COPY_GIT`).
pub struct PathResolver {
    /// The allowed filesystem root for resolved paths. Most operations are
    /// validated against this path to prevent directory traversal or
    /// accidental access outside the workspace.
    root: PathBuf,

    /// The build context directory (typically the project or repository
    /// directory) used as an alternate source root for operations such as
    /// `COPY` and `COPY_GIT`. Some operations validate paths against the
    /// build context instead of the general `root`.
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

    /// Copy from a git revision/path into the provided destination path.
    /// This method extracts blobs/trees using `git` plumbing (`git show`)
    /// and copies the material into `to`. `to` should be an absolute path already
    /// validated by the caller.
    #[allow(clippy::disallowed_methods)]
    pub fn copy_from_git(&self, rev: &str, from: &str, to: &Path) -> Result<()> {
        if Path::new(from).is_absolute() {
            bail!("COPY_GIT source must be relative to build context");
        }

        // Ensure destination is within allowed root (don't trust caller)
        let _ = self
            .check_access_with_root(&self.root, to, AccessMode::Write)
            .with_context(|| format!("destination {} escapes allowed root", to.display()))?;

        // Ensure build_context is allowed under root
        let _ = self
            .check_access(&self.build_context, AccessMode::Read)
            .with_context(|| {
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

        // Fallback: When the requested `rev:from` is not a blob (for example,
        // when it's a tree/directory) we can't extract it with a single
        // `git show` write. In that case we enumerate the tree entries with
        // `git ls-tree -r -z <rev> -- <from>` and stream each blob individually
        // using `git show <rev>:<path>`.
        //
        // Triggers:
        // - `git cat-file -t <rev>:<from>` returns `tree` (directory) or other
        //   non-`blob` types.
        //
        // Behavior & format:
        // - `git ls-tree -r -z` returns NUL-separated entries of the form
        //   "<mode> <type> <hash>\t<path>\0". We split on NUL and parse the
        //   header to obtain the mode and type for each entry.
        // - For entries with type `blob` we call `git show` to retrieve the
        //   raw contents and write them to the destination path (creating
        //   parent directories as needed).
        // - For entries with mode `120000` (git's symlink blob) we create a
        //   platform-appropriate symlink on Unix and a placeholder file on
        //   Windows.
        // - `commit` entries (submodules) are rejected.
        //
        // Rationale: this approach avoids requiring external tools like
        // `tar`/`unpack` on the host — the implementation relies only on
        // standard `git` plumbing commands and filesystem operations.
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

    #[allow(clippy::disallowed_methods)]
    pub fn create_dir_all_abs(&self, path: &Path) -> Result<()> {
        let guarded = self
            .check_access(path, AccessMode::Write)
            .with_context(|| format!("create_dir_all denied for {}", path.display()))?;
        fs::create_dir_all(&guarded)
            .with_context(|| format!("creating dir {}", guarded.display()))?;
        Ok(())
    }

    #[allow(clippy::disallowed_methods)]
    pub fn read_dir_entries(&self, path: &Path) -> Result<Vec<std::fs::DirEntry>> {
        let guarded = self
            .check_access(path, AccessMode::Read)
            .with_context(|| format!("read_dir denied for {}", path.display()))?;
        let entries = fs::read_dir(&guarded)
            .with_context(|| format!("failed to read dir {}", guarded.display()))?;
        let vec: Vec<std::fs::DirEntry> = entries.collect::<Result<_, _>>()?;
        Ok(vec)
    }

    #[allow(clippy::disallowed_methods)]
    pub fn read_file(&self, path: &Path) -> Result<Vec<u8>> {
        let guarded = self
            .check_access(path, AccessMode::Read)
            .with_context(|| format!("read denied for {}", path.display()))?;
        let data =
            fs::read(&guarded).with_context(|| format!("failed to read {}", guarded.display()))?;
        Ok(data)
    }

    #[allow(clippy::disallowed_methods)]
    pub fn read_to_string(&self, path: &Path) -> Result<String> {
        let guarded = self
            .check_access(path, AccessMode::Read)
            .with_context(|| format!("read denied for {}", path.display()))?;
        let s = fs::read_to_string(&guarded)
            .with_context(|| format!("failed to read {}", guarded.display()))?;
        Ok(s)
    }

    #[allow(clippy::disallowed_methods)]
    pub fn write_file(&self, path: &Path, contents: &[u8]) -> Result<()> {
        let guarded = self
            .check_access(path, AccessMode::Write)
            .with_context(|| format!("write denied for {}", path.display()))?;
        if let Some(parent) = guarded.parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("creating dir {}", parent.display()))?;
        }
        fs::write(&guarded, contents).with_context(|| format!("writing {}", guarded.display()))?;
        Ok(())
    }

    #[allow(clippy::disallowed_methods)]
    pub fn copy_file(&self, src: &Path, dst: &Path) -> Result<u64> {
        // Ensure destination is allowed for write
        let guarded_dst = self
            .check_access(dst, AccessMode::Write)
            .with_context(|| format!("copy destination denied for {}", dst.display()))?;
        // Source may be under root or under build_context; allow either for read
        let guarded_src = self
            .check_access(src, AccessMode::Read)
            .or_else(|_| self.check_access_with_root(&self.build_context, src, AccessMode::Read))
            .with_context(|| format!("copy source denied for {}", src.display()))?;
        if let Some(parent) = guarded_dst.parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("creating dir {}", parent.display()))?;
        }
        let n = fs::copy(&guarded_src, &guarded_dst).with_context(|| {
            format!(
                "copying {} to {}",
                guarded_src.display(),
                guarded_dst.display()
            )
        })?;
        Ok(n)
    }

    #[allow(clippy::disallowed_methods)]
    pub fn canonicalize_abs(&self, path: &Path) -> Result<PathBuf> {
        // Use check_access to validate and return an absolute path that is within allowed roots
        let cand = self
            .check_access(path, AccessMode::Passthru)
            .or_else(|_| {
                self.check_access_with_root(&self.build_context, path, AccessMode::Passthru)
            })
            .with_context(|| format!("canonicalize denied for {}", path.display()))?;
        Ok(cand)
    }

    #[allow(clippy::disallowed_methods)]
    pub fn metadata_abs(&self, path: &Path) -> Result<std::fs::Metadata> {
        let guarded = self
            .check_access(path, AccessMode::Read)
            .or_else(|_| self.check_access_with_root(&self.build_context, path, AccessMode::Read))
            .with_context(|| format!("metadata denied for {}", path.display()))?;
        let m = fs::metadata(&guarded)
            .with_context(|| format!("failed to stat {}", guarded.display()))?;
        Ok(m)
    }

    #[allow(clippy::disallowed_methods)]
    #[allow(clippy::only_used_in_recursion)]
    pub fn copy_dir_recursive(&self, src: &Path, dst: &Path) -> Result<()> {
        // Validate the destination root for write access, then create it.
        let guarded_dst_root = self
            .check_access(dst, AccessMode::Write)
            .with_context(|| format!("copy destination denied for {}", dst.display()))?;
        fs::create_dir_all(&guarded_dst_root)
            .with_context(|| format!("creating dir {}", guarded_dst_root.display()))?;

        for entry in fs::read_dir(src)? {
            let entry = entry?;
            let file_type = entry.file_type()?;
            let src_path = entry.path();
            let dst_path = guarded_dst_root.join(entry.file_name());

            // Guard source: allow it under root OR build_context for reads.
            let guarded_src = self
                .check_access(&src_path, AccessMode::Read)
                .or_else(|_| {
                    self.check_access_with_root(&self.build_context, &src_path, AccessMode::Read)
                })
                .with_context(|| format!("copy source denied for {}", src_path.display()))?;

            // Guard destination path (each file/dir) for writes.
            let guarded_dst = self
                .check_access(&dst_path, AccessMode::Write)
                .with_context(|| format!("copy destination denied for {}", dst_path.display()))?;

            if file_type.is_dir() {
                // Create destination dir then recurse.
                fs::create_dir_all(&guarded_dst)
                    .with_context(|| format!("creating dir {}", guarded_dst.display()))?;
                self.copy_dir_recursive(&guarded_src, &guarded_dst)?;
            } else if file_type.is_file() {
                if let Some(parent) = guarded_dst.parent() {
                    fs::create_dir_all(parent)
                        .with_context(|| format!("creating dir {}", parent.display()))?;
                }
                fs::copy(&guarded_src, &guarded_dst).with_context(|| {
                    format!(
                        "copying {} to {}",
                        guarded_src.display(),
                        guarded_dst.display()
                    )
                })?;
            } else {
                bail!("unsupported file type: {}", src_path.display());
            }
        }
        Ok(())
    }

    /// Remove a file after validating it is within allowed roots.
    #[allow(clippy::disallowed_methods)]
    pub fn remove_file_abs(&self, path: &Path) -> Result<()> {
        let guarded = self
            .check_access(path, AccessMode::Write)
            .with_context(|| format!("remove_file denied for {}", path.display()))?;
        match fs::remove_file(&guarded) {
            Ok(_) => {}
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
            Err(e) => {
                return Err(e).with_context(|| format!("failed to remove file {}", guarded.display()))
            }
        }
        Ok(())
    }

    /// Remove a directory and its contents after validating it is within allowed roots.
    #[allow(clippy::disallowed_methods)]
    pub fn remove_dir_all_abs(&self, path: &Path) -> Result<()> {
        let guarded = self
            .check_access(path, AccessMode::Write)
            .with_context(|| format!("remove_dir_all denied for {}", path.display()))?;
        fs::remove_dir_all(&guarded)
            .with_context(|| format!("failed to remove dir {}", guarded.display()))?;
        Ok(())
    }

    /// Copy a directory tree from an external source (not necessarily under
    /// the resolver roots) into a destination that must be under the
    /// resolver's allowed `root`. This is used to move material from a
    /// temporary build directory into the guarded out_dir.
    #[allow(clippy::disallowed_methods)]
    pub fn copy_dir_from_external(&self, src: &Path, dst: &Path) -> Result<()> {
        // Ensure destination root is allowed
        let guarded_dst_root = self
            .check_access(dst, AccessMode::Write)
            .with_context(|| format!("copy destination denied for {}", dst.display()))?;
        fs::create_dir_all(&guarded_dst_root)
            .with_context(|| format!("creating dir {}", guarded_dst_root.display()))?;

        for entry in fs::read_dir(src)? {
            let entry = entry?;
            let file_type = entry.file_type()?;
            let src_path = entry.path();
            let dst_path = guarded_dst_root.join(entry.file_name());

            if file_type.is_dir() {
                fs::create_dir_all(&dst_path)
                    .with_context(|| format!("creating dir {}", dst_path.display()))?;
                // Recurse from external source
                self.copy_dir_from_external(&src_path, &dst_path)?;
            } else if file_type.is_file() {
                if let Some(parent) = dst_path.parent() {
                    fs::create_dir_all(parent)
                        .with_context(|| format!("creating dir {}", parent.display()))?;
                }
                fs::copy(&src_path, &dst_path).with_context(|| {
                    format!("copying {} to {}", src_path.display(), dst_path.display())
                })?;
            } else {
                bail!("unsupported file type: {}", src_path.display());
            }
        }
        Ok(())
    }

    /// Copy a single file from an external source into a destination guarded
    /// by this resolver. The `src` path is not validated against the resolver
    /// roots (it may live outside them); the `dst` is validated to ensure it
    /// resides under the allowed `root`.
    #[allow(clippy::disallowed_methods)]
    pub fn copy_file_from_external(&self, src: &Path, dst: &Path) -> Result<u64> {
        let guarded_dst = self
            .check_access(dst, AccessMode::Write)
            .with_context(|| format!("copy destination denied for {}", dst.display()))?;
        if let Some(parent) = guarded_dst.parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("creating dir {}", parent.display()))?;
        }
        let n = fs::copy(src, &guarded_dst)
            .with_context(|| format!("copying {} to {}", src.display(), guarded_dst.display()))?;
        Ok(n)
    }

    /// Open an external file (potentially outside resolver roots) and return a `File`.
    ///
    /// This helper centralizes raw `File::open` usage so other crates do not call
    /// `std::fs` directly. The resolver does not validate the `path` against its
    /// configured roots because this operation is explicitly intended for
    /// external device files or temporary locations.
    #[allow(clippy::disallowed_methods)]
    pub fn open_external_file(&self, path: &Path) -> Result<std::fs::File> {
        let f = fs::File::open(path)
            .with_context(|| format!("failed to open {}", path.display()))?;
        Ok(f)
    }
}

impl PathResolver {
    #[allow(clippy::disallowed_methods)]
    /// Validate that `candidate` resolves to a path located under `root`.
    ///
    /// This method performs the same semantics as the previous free
    /// function `check_access` but as a private helper on `PathResolver`.
    /// It returns an absolute, canonicalized path to the candidate (if
    /// possible) that is guaranteed to start with the canonicalized
    /// `root`. If the candidate does not yet exist, the function
    /// canonicalizes the nearest existing ancestor and then reconstructs
    /// the remainder of the candidate path, while ensuring the resulting
    /// path does not escape `root` (via `..` components, symlinks, or
    /// absolute paths).
    ///
    /// Parameters:
    /// - `root`: the allowed root directory under which `candidate` must
    ///   reside.
    /// - `candidate`: the path to validate (may be absolute or relative,
    ///   and may point to a not-yet-existing location).
    /// - `mode`: an `AccessMode` value used only for error messages.
    ///
    /// Returns: a `Result<PathBuf>` containing the absolute path that is
    /// safe to use (and lives under `root`), or an error if the candidate
    /// would escape `root` or some canonicalization step fails.
    #[allow(clippy::disallowed_methods)]
    fn check_access_with_root(
        &self,
        root: &Path,
        candidate: &Path,
        mode: AccessMode,
    ) -> Result<PathBuf> {
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

    /// Convenience wrapper that validates `candidate` against the resolver's
    /// configured `root`.
    ///
    /// Equivalent to calling `check_access_with_root(&self.root, ...)`.
    /// Kept private to ensure callers go through `PathResolver` and cannot
    /// accidentally validate against the wrong root.
    fn check_access(&self, candidate: &Path, mode: AccessMode) -> Result<PathBuf> {
        self.check_access_with_root(&self.root, candidate, mode)
    }
}
