use anyhow::{Context, Result, bail};
use std::fs;

use super::{AccessMode, EntryKind, PathResolver};
use crate::GuardedPath;

#[allow(clippy::disallowed_types)]
use crate::UnguardedPath;

// Guarded filesystem IO helpers (read/write/metadata etc.).
impl PathResolver {
    #[cfg(not(miri))]
    #[allow(clippy::disallowed_methods)]
    pub fn create_dir_all_abs(&self, path: &GuardedPath) -> Result<()> {
        if !self.root.as_path().exists() {
            fs::create_dir_all(self.root.as_path())
                .with_context(|| format!("creating resolver root {}", self.root.display()))?;
        }

        let guarded = self
            .check_access(path.as_path(), AccessMode::Write)
            .with_context(|| format!("create_dir_all denied for {}", path.display()))?;
        fs::create_dir_all(guarded.as_path())
            .with_context(|| format!("creating dir {}", guarded.display()))?;
        Ok(())
    }

    #[cfg(miri)]
    pub fn create_dir_all_abs(&self, path: &GuardedPath) -> Result<()> {
        let guarded = self
            .check_access(path.as_path(), AccessMode::Write)
            .with_context(|| format!("create_dir_all denied for {}", path.display()))?;
        let state = self.state_for_guard(&guarded);
        let rel = Self::normalize_rel(&guarded);
        state.borrow_mut().ensure_dir(&rel);
        // Mirror to the host filesystem so std::fs queries (e.g., Path::exists) succeed under Miri.
        if let Some(parent) = guarded.as_path().parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("creating dir {}", parent.display()))?;
        }
        fs::create_dir_all(guarded.as_path())
            .with_context(|| format!("creating dir {}", guarded.display()))?;
        Ok(())
    }

    #[cfg(not(miri))]
    #[allow(clippy::disallowed_methods)]
    pub fn read_dir_entries(&self, path: &GuardedPath) -> Result<Vec<super::DirEntry>> {
        let guarded = self
            .check_access(path.as_path(), AccessMode::Read)
            .with_context(|| format!("read_dir denied for {}", path.display()))?;
        let entries = fs::read_dir(guarded.as_path())
            .with_context(|| format!("failed to read dir {}", guarded.display()))?;
        let vec: Vec<std::fs::DirEntry> = entries.collect::<Result<_, _>>()?;
        Ok(vec)
    }

    #[cfg(miri)]
    pub fn read_dir_entries(&self, path: &GuardedPath) -> Result<Vec<super::DirEntry>> {
        let guarded = self
            .check_access(path.as_path(), AccessMode::Read)
            .with_context(|| format!("read_dir denied for {}", path.display()))?;
        // Prefer real host FS entries if they exist (we mirror writes there),
        // otherwise fall back to synthetic in-memory entries.
        if guarded.as_path().exists() {
            let entries = fs::read_dir(guarded.as_path())
                .with_context(|| format!("failed to read dir {}", guarded.display()))?;
            let mut out: Vec<super::DirEntry> = Vec::new();
            for entry in entries {
                let entry = entry?;
                let ft = entry.file_type()?;
                let kind = if ft.is_dir() {
                    EntryKind::Dir
                } else {
                    EntryKind::File
                };
                out.push(super::DirEntry::new(entry.path(), kind));
            }
            return Ok(out);
        }

        let rel = Self::normalize_rel(&guarded);
        let state_ref = self.state_for_guard(&guarded);
        let state = state_ref.borrow();
        if !state.dir_exists(&rel) {
            bail!("directory does not exist: {}", guarded.display());
        }
        let children = state.list_children(&rel);
        Ok(children
            .into_iter()
            .map(|(name, kind)| super::DirEntry::new(guarded.as_path().join(name), kind))
            .collect())
    }

    #[cfg(not(miri))]
    #[allow(clippy::disallowed_methods)]
    pub fn read_file(&self, path: &GuardedPath) -> Result<Vec<u8>> {
        let guarded = self
            .check_access(path.as_path(), AccessMode::Read)
            .with_context(|| format!("read denied for {}", path.display()))?;
        let data = fs::read(guarded.as_path())
            .with_context(|| format!("failed to read {}", guarded.display()))?;
        Ok(data)
    }

    #[cfg(miri)]
    pub fn read_file(&self, path: &GuardedPath) -> Result<Vec<u8>> {
        let guarded = self
            .check_access(path.as_path(), AccessMode::Read)
            .with_context(|| format!("read denied for {}", path.display()))?;
        // Prefer host file if present (we mirror writes to host), else fall back to synthetic state.
        if guarded.as_path().exists() {
            let data = fs::read(guarded.as_path())
                .with_context(|| format!("failed to read {}", guarded.display()))?;
            return Ok(data);
        }
        let rel = Self::normalize_rel(&guarded);
        let data = self.state_for_guard(&guarded).borrow().read_file(&rel)?;
        Ok(data)
    }

    #[cfg(not(miri))]
    #[allow(clippy::disallowed_methods)]
    pub fn read_to_string(&self, path: &GuardedPath) -> Result<String> {
        let guarded = self
            .check_access(path.as_path(), AccessMode::Read)
            .with_context(|| format!("read denied for {}", path.display()))?;
        let s = fs::read_to_string(guarded.as_path())
            .with_context(|| format!("failed to read {}", guarded.display()))?;
        Ok(s)
    }

    #[cfg(miri)]
    pub fn read_to_string(&self, path: &GuardedPath) -> Result<String> {
        let bytes = self.read_file(path)?;
        let s = String::from_utf8(bytes).with_context(|| format!("{} is not UTF-8", path))?;
        Ok(s)
    }

    #[cfg(not(miri))]
    #[allow(clippy::disallowed_methods)]
    pub fn write_file(&self, path: &GuardedPath, contents: &[u8]) -> Result<()> {
        let guarded = self
            .check_access(path.as_path(), AccessMode::Write)
            .with_context(|| format!("write denied for {}", path.display()))?;
        if let Some(parent) = guarded.as_path().parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("creating dir {}", parent.display()))?;
        }
        fs::write(guarded.as_path(), contents)
            .with_context(|| format!("writing {}", guarded.display()))?;
        Ok(())
    }

    #[cfg(miri)]
    pub fn write_file(&self, path: &GuardedPath, contents: &[u8]) -> Result<()> {
        let guarded = self
            .check_access(path.as_path(), AccessMode::Write)
            .with_context(|| format!("write denied for {}", path.display()))?;
        let rel = Self::normalize_rel(&guarded);
        let binding = self.state_for_guard(&guarded);
        let mut state = binding.borrow_mut();
        state.write_file(&rel, contents);
        if let Some(parent) = guarded.as_path().parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("creating dir {}", parent.display()))?;
        }
        fs::write(guarded.as_path(), contents)
            .with_context(|| format!("writing {}", guarded.display()))?;
        Ok(())
    }

    #[cfg(not(miri))]
    #[allow(clippy::disallowed_methods)]
    pub fn canonicalize_abs(&self, path: &GuardedPath) -> Result<GuardedPath> {
        let cand = self
            .check_access(path.as_path(), AccessMode::Passthru)
            .or_else(|_| {
                self.check_access_with_root(
                    &self.build_context,
                    path.as_path(),
                    AccessMode::Passthru,
                )
            })
            .with_context(|| format!("canonicalize denied for {}", path.display()))?;
        Ok(cand)
    }

    #[cfg(miri)]
    pub fn canonicalize_abs(&self, path: &GuardedPath) -> Result<GuardedPath> {
        let cand = self
            .check_access(path.as_path(), AccessMode::Passthru)
            .or_else(|_| {
                self.check_access_with_root(
                    &self.build_context,
                    path.as_path(),
                    AccessMode::Passthru,
                )
            })
            .with_context(|| format!("canonicalize denied for {}", path.display()))?;
        let canon = fs::canonicalize(cand.as_path()).unwrap_or_else(|_| cand.to_path_buf());
        GuardedPath::new(cand.root(), &canon)
    }

    #[cfg(not(miri))]
    #[allow(clippy::disallowed_methods)]
    pub fn metadata_abs(&self, path: &GuardedPath) -> Result<std::fs::Metadata> {
        let guarded = self
            .check_access(path.as_path(), AccessMode::Read)
            .or_else(|_| {
                self.check_access_with_root(&self.build_context, path.as_path(), AccessMode::Read)
            })
            .with_context(|| format!("metadata denied for {}", path.display()))?;
        let m = fs::metadata(guarded.as_path())
            .with_context(|| format!("failed to stat {}", guarded.display()))?;
        Ok(m)
    }

    #[cfg(miri)]
    pub fn metadata_abs(&self, path: &GuardedPath) -> Result<std::fs::Metadata> {
        let guarded = self
            .check_access(path.as_path(), AccessMode::Read)
            .or_else(|_| {
                self.check_access_with_root(&self.build_context, path.as_path(), AccessMode::Read)
            })
            .with_context(|| format!("metadata denied for {}", path.display()))?;
        let m = fs::metadata(guarded.as_path())
            .with_context(|| format!("failed to stat {}", guarded.display()))?;
        Ok(m)
    }

    #[cfg(not(miri))]
    pub fn entry_kind(&self, path: &GuardedPath) -> Result<EntryKind> {
        let meta = self.metadata_abs(path)?;
        if meta.is_dir() {
            Ok(EntryKind::Dir)
        } else if meta.is_file() {
            Ok(EntryKind::File)
        } else {
            bail!("unsupported file type: {}", path.display());
        }
    }

    #[cfg(miri)]
    pub fn entry_kind(&self, path: &GuardedPath) -> Result<EntryKind> {
        let meta = self.metadata_abs(path)?;
        if meta.is_dir() {
            Ok(EntryKind::Dir)
        } else if meta.is_file() {
            Ok(EntryKind::File)
        } else {
            bail!("unsupported file type: {}", path.display());
        }
    }

    #[allow(clippy::disallowed_methods, clippy::disallowed_types)]
    pub fn metadata_external(&self, path: &UnguardedPath) -> Result<std::fs::Metadata> {
        let m = fs::metadata(path.as_path()).with_context(|| {
            format!("failed to stat external path {}", path.as_path().display())
        })?;
        Ok(m)
    }

    #[cfg(not(miri))]
    #[allow(clippy::disallowed_methods)]
    pub fn set_permissions_mode_unix(&self, path: &GuardedPath, mode: u32) -> Result<()> {
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let guarded = self
                .check_access(path.as_path(), AccessMode::Write)
                .with_context(|| format!("set_permissions denied for {}", path.display()))?;
            fs::set_permissions(guarded.as_path(), fs::Permissions::from_mode(mode))
                .with_context(|| format!("failed to set permissions on {}", guarded.display()))?;
        }
        #[cfg(not(unix))]
        {
            let _ = path;
            let _ = mode;
        }
        Ok(())
    }

    #[cfg(miri)]
    pub fn set_permissions_mode_unix(&self, _path: &GuardedPath, _mode: u32) -> Result<()> {
        Ok(())
    }

    #[allow(clippy::disallowed_methods, clippy::disallowed_types)]
    pub fn open_external_file(&self, path: &UnguardedPath) -> Result<std::fs::File> {
        let f = fs::File::open(path.as_path())
            .with_context(|| format!("failed to open {}", path.as_path().display()))?;
        Ok(f)
    }

    /// Remove a file after validating it is within allowed roots.
    #[cfg(not(miri))]
    #[allow(clippy::disallowed_methods)]
    pub fn remove_file_abs(&self, path: &GuardedPath) -> Result<()> {
        let guarded = self
            .check_access(path.as_path(), AccessMode::Write)
            .with_context(|| format!("remove_file denied for {}", path.display()))?;
        match std::fs::remove_file(guarded.as_path()) {
            Ok(_) => {}
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
            Err(e) => {
                return Err(e)
                    .with_context(|| format!("failed to remove file {}", guarded.display()));
            }
        }
        Ok(())
    }

    #[cfg(miri)]
    pub fn remove_file_abs(&self, path: &GuardedPath) -> Result<()> {
        let guarded = self
            .check_access(path.as_path(), AccessMode::Write)
            .with_context(|| format!("remove_file denied for {}", path.display()))?;
        let rel = Self::normalize_rel(&guarded);
        self.state_for_guard(&guarded)
            .borrow_mut()
            .remove_file(&rel);
        match std::fs::remove_file(guarded.as_path()) {
            Ok(_) => {}
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
            Err(e) => {
                return Err(e)
                    .with_context(|| format!("failed to remove file {}", guarded.display()));
            }
        }
        Ok(())
    }

    /// Remove a directory and its contents after validating it is within allowed roots.
    #[cfg(not(miri))]
    #[allow(clippy::disallowed_methods)]
    pub fn remove_dir_all_abs(&self, path: &GuardedPath) -> Result<()> {
        let guarded = self
            .check_access(path.as_path(), AccessMode::Write)
            .with_context(|| format!("remove_dir_all denied for {}", path.display()))?;
        std::fs::remove_dir_all(guarded.as_path())
            .with_context(|| format!("failed to remove dir {}", guarded.display()))?;
        Ok(())
    }

    #[cfg(miri)]
    pub fn remove_dir_all_abs(&self, path: &GuardedPath) -> Result<()> {
        let guarded = self
            .check_access(path.as_path(), AccessMode::Write)
            .with_context(|| format!("remove_dir_all denied for {}", path.display()))?;
        let rel = Self::normalize_rel(&guarded);
        self.state_for_guard(&guarded)
            .borrow_mut()
            .remove_dir_all(&rel);
        let _ = std::fs::remove_dir_all(guarded.as_path());
        Ok(())
    }

    #[cfg(not(miri))]
    #[allow(clippy::disallowed_methods)]
    pub fn symlink(&self, src: &GuardedPath, dst: &GuardedPath) -> Result<()> {
        let guarded_src = self
            .check_access(src.as_path(), AccessMode::Read)
            .or_else(|_| {
                self.check_access_with_root(&self.build_context, src.as_path(), AccessMode::Read)
            })
            .with_context(|| format!("symlink source denied for {}", src.display()))?;
        let guarded_dst = self
            .check_access(dst.as_path(), AccessMode::Write)
            .with_context(|| format!("symlink destination denied for {}", dst.display()))?;

        if guarded_dst.as_path().exists() {
            bail!(
                "SYMLINK destination already exists: {}",
                guarded_dst.display()
            );
        }
        if guarded_src.as_path() == guarded_dst.as_path() {
            bail!(
                "SYMLINK source resolves to the destination itself: {}",
                guarded_src.display()
            );
        }

        #[cfg(unix)]
        {
            std::os::unix::fs::symlink(guarded_src.as_path(), guarded_dst.as_path()).with_context(
                || {
                    format!(
                        "failed to symlink {} -> {}",
                        guarded_src.display(),
                        guarded_dst.display()
                    )
                },
            )?;
            Ok(())
        }

        #[cfg(all(windows, not(unix)))]
        {
            use std::os::windows::fs::{symlink_dir, symlink_file};
            let meta = fs::metadata(guarded_src.as_path()).with_context(|| {
                format!("failed to stat symlink source {}", guarded_src.display())
            })?;

            let try_link = if meta.is_dir() {
                symlink_dir(guarded_src.as_path(), guarded_dst.as_path())
            } else {
                symlink_file(guarded_src.as_path(), guarded_dst.as_path())
            };

            if let Err(e) = try_link {
                eprintln!(
                    "warning: failed to create symlink {} -> {}: {}; falling back to copy",
                    guarded_dst.display(),
                    guarded_src.display(),
                    e
                );

                if meta.is_dir() {
                    self.copy_dir_recursive(&guarded_src, &guarded_dst)
                        .with_context(|| {
                            format!(
                                "failed to copy dir {} -> {}",
                                guarded_src.display(),
                                guarded_dst.display()
                            )
                        })?;
                } else {
                    self.copy_file(&guarded_src, &guarded_dst)
                        .with_context(|| {
                            format!(
                                "failed to copy file {} -> {}",
                                guarded_src.display(),
                                guarded_dst.display()
                            )
                        })?;
                }
            }

            Ok(())
        }

        #[cfg(not(any(unix, windows)))]
        {
            let meta = fs::metadata(&guarded_src).with_context(|| {
                format!("failed to stat symlink source {}", guarded_src.display())
            })?;
            // No native symlinks; fall back to copying to preserve behavior.
            if meta.is_dir() {
                self.copy_dir_recursive(&guarded_src, &guarded_dst)
                    .with_context(|| {
                        format!(
                            "failed to copy dir {} -> {}",
                            guarded_src.display(),
                            guarded_dst.display()
                        )
                    })?;
            } else {
                self.copy_file(&guarded_src, &guarded_dst)
                    .with_context(|| {
                        format!(
                            "failed to copy file {} -> {}",
                            guarded_src.display(),
                            guarded_dst.display()
                        )
                    })?;
            }
            Ok(())
        }
    }

    #[cfg(miri)]
    pub fn symlink(&self, _src: &GuardedPath, _dst: &GuardedPath) -> Result<()> {
        bail!("symlinks are not supported under miri synthetic filesystem");
    }
}
