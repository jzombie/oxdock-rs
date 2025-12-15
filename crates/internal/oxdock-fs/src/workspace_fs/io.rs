use anyhow::{Context, Result};
use std::fs;
use std::path::{Path, PathBuf};

use super::{AccessMode, PathResolver};

// Guarded filesystem IO helpers (read/write/metadata etc.).
impl PathResolver {
    #[allow(clippy::disallowed_methods)]
    pub fn create_dir_all_abs(&self, path: &Path) -> Result<()> {
        if !self.root.exists() {
            fs::create_dir_all(&self.root)
                .with_context(|| format!("creating resolver root {}", self.root.display()))?;
        }

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
    pub fn canonicalize_abs(&self, path: &Path) -> Result<PathBuf> {
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
    pub fn metadata_external(&self, path: &Path) -> Result<std::fs::Metadata> {
        let m = fs::metadata(path)
            .with_context(|| format!("failed to stat external path {}", path.display()))?;
        Ok(m)
    }

    #[allow(clippy::disallowed_methods)]
    pub fn set_permissions_mode_unix(&self, path: &Path, mode: u32) -> Result<()> {
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let guarded = self
                .check_access(path, AccessMode::Write)
                .with_context(|| format!("set_permissions denied for {}", path.display()))?;
            fs::set_permissions(&guarded, fs::Permissions::from_mode(mode))
                .with_context(|| format!("failed to set permissions on {}", guarded.display()))?;
        }
        #[cfg(not(unix))]
        {
            let _ = path;
            let _ = mode;
        }
        Ok(())
    }

    #[allow(clippy::disallowed_methods)]
    pub fn open_external_file(&self, path: &Path) -> Result<std::fs::File> {
        let f =
            fs::File::open(path).with_context(|| format!("failed to open {}", path.display()))?;
        Ok(f)
    }

    /// Remove a file after validating it is within allowed roots.
    #[allow(clippy::disallowed_methods)]
    pub fn remove_file_abs(&self, path: &Path) -> Result<()> {
        let guarded = self
            .check_access(path, AccessMode::Write)
            .with_context(|| format!("remove_file denied for {}", path.display()))?;
        match std::fs::remove_file(&guarded) {
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
    #[allow(clippy::disallowed_methods)]
    pub fn remove_dir_all_abs(&self, path: &Path) -> Result<()> {
        let guarded = self
            .check_access(path, AccessMode::Write)
            .with_context(|| format!("remove_dir_all denied for {}", path.display()))?;
        std::fs::remove_dir_all(&guarded)
            .with_context(|| format!("failed to remove dir {}", guarded.display()))?;
        Ok(())
    }
}
