#[cfg(not(miri))]
use anyhow::bail;
use anyhow::{Context, Result};
use std::fs;

#[cfg(miri)]
use super::EntryKind;
use super::{AccessMode, PathResolver};
use crate::GuardedPath;

#[allow(clippy::disallowed_types)]
use crate::UnguardedPath;

// Guarded filesystem IO helpers (read/write/metadata etc.).
impl PathResolver {
    #[allow(clippy::disallowed_methods)]
    pub fn create_dir_all_abs(&self, path: &GuardedPath) -> Result<()> {
        let guarded = self
            .check_access(path.as_path(), AccessMode::Write)
            .with_context(|| format!("create_dir_all denied for {}", path.display()))?;
        self.backend.create_dir_all_abs(&self.root, &guarded)
    }

    #[allow(clippy::disallowed_methods)]
    pub fn read_dir_entries(&self, path: &GuardedPath) -> Result<Vec<super::DirEntry>> {
        let guarded = self
            .check_access(path.as_path(), AccessMode::Read)
            .or_else(|_| {
                self.check_access_with_root(&self.build_context, path.as_path(), AccessMode::Read)
            })
            .with_context(|| format!("read_dir denied for {}", path.display()))?;
        self.backend.read_dir_entries(&guarded)
    }

    #[allow(clippy::disallowed_methods)]
    pub fn read_file(&self, path: &GuardedPath) -> Result<Vec<u8>> {
        let guarded = self
            .check_access(path.as_path(), AccessMode::Read)
            .or_else(|_| {
                self.check_access_with_root(&self.build_context, path.as_path(), AccessMode::Read)
            })
            .with_context(|| format!("read denied for {}", path.display()))?;
        self.backend.read_file(&guarded)
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

    #[allow(clippy::disallowed_methods)]
    pub fn write_file(&self, path: &GuardedPath, contents: &[u8]) -> Result<()> {
        let guarded = self
            .check_access(path.as_path(), AccessMode::Write)
            .with_context(|| format!("write denied for {}", path.display()))?;
        self.backend.write_file(&guarded, contents)
    }

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
        self.backend.canonicalize_abs(cand)
    }

    #[allow(clippy::disallowed_methods)]
    pub fn metadata_abs(&self, path: &GuardedPath) -> Result<std::fs::Metadata> {
        let guarded = self
            .check_access(path.as_path(), AccessMode::Read)
            .or_else(|_| {
                self.check_access_with_root(&self.build_context, path.as_path(), AccessMode::Read)
            })
            .with_context(|| format!("metadata denied for {}", path.display()))?;
        self.backend.metadata_abs(&guarded)
    }

    pub fn entry_kind(&self, path: &GuardedPath) -> Result<super::EntryKind> {
        self.backend.entry_kind(path)
    }

    /// Lightweight existence check that avoids host `stat` calls under Miri.
    pub fn exists(&self, path: &GuardedPath) -> bool {
        self.backend.entry_kind(path).is_ok()
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
    #[allow(clippy::disallowed_methods)]
    pub fn remove_file_abs(&self, path: &GuardedPath) -> Result<()> {
        let guarded = self
            .check_access(path.as_path(), AccessMode::Write)
            .with_context(|| format!("remove_file denied for {}", path.display()))?;
        self.backend.remove_file_abs(&guarded)
    }

    /// Remove a directory and its contents after validating it is within allowed roots.
    #[allow(clippy::disallowed_methods)]
    pub fn remove_dir_all_abs(&self, path: &GuardedPath) -> Result<()> {
        let guarded = self
            .check_access(path.as_path(), AccessMode::Write)
            .with_context(|| format!("remove_dir_all denied for {}", path.display()))?;
        self.backend.remove_dir_all_abs(&guarded)
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
    pub fn symlink(&self, src: &GuardedPath, dst: &GuardedPath) -> Result<()> {
        let guarded_src = self
            .check_access_with_root(&self.root, src.as_path(), AccessMode::Read)
            .with_context(|| format!("symlink source denied for {}", src.display()))?;
        let guarded_dst = self
            .check_access_with_root(&self.root, dst.as_path(), AccessMode::Write)
            .with_context(|| format!("symlink destination denied for {}", dst.display()))?;

        let kind = self.entry_kind(&guarded_src)?;

        if kind == EntryKind::Dir {
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
