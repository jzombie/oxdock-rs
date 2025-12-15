use anyhow::{Context, Result, bail};
use std::fs;
use std::path::Path;

use super::{AccessMode, PathResolver};

// Copy helpers for guarded and external sources.
impl PathResolver {
    #[allow(clippy::disallowed_methods)]
    pub fn copy_file(&self, src: &Path, dst: &Path) -> Result<u64> {
        let guarded_dst = self
            .check_access(dst, AccessMode::Write)
            .with_context(|| format!("copy destination denied for {}", dst.display()))?;
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
    #[allow(clippy::only_used_in_recursion)]
    pub fn copy_dir_recursive(&self, src: &Path, dst: &Path) -> Result<()> {
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

            let guarded_src = self
                .check_access(&src_path, AccessMode::Read)
                .or_else(|_| {
                    self.check_access_with_root(&self.build_context, &src_path, AccessMode::Read)
                })
                .with_context(|| format!("copy source denied for {}", src_path.display()))?;

            let guarded_dst = self
                .check_access(&dst_path, AccessMode::Write)
                .with_context(|| format!("copy destination denied for {}", dst_path.display()))?;

            if file_type.is_dir() {
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

    #[allow(clippy::disallowed_methods)]
    pub fn copy_dir_from_external(&self, src: &Path, dst: &Path) -> Result<()> {
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
}
