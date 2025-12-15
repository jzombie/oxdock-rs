use anyhow::{Context, Result, bail};
use std::fs;

use super::{AccessMode, PathResolver};
use crate::{GuardedPath, UnguardedPath};

// Copy helpers for guarded and external sources.
impl PathResolver {
    #[allow(clippy::disallowed_methods)]
    pub fn copy_file(&self, src: &GuardedPath, dst: &GuardedPath) -> Result<u64> {
        let guarded_dst = self
            .check_access(dst.as_path(), AccessMode::Write)
            .with_context(|| format!("copy destination denied for {}", dst.display()))?;
        let guarded_src = self
            .check_access(src.as_path(), AccessMode::Read)
            .or_else(|_| {
                self.check_access_with_root(&self.build_context, src.as_path(), AccessMode::Read)
            })
            .with_context(|| format!("copy source denied for {}", src.display()))?;
        if let Some(parent) = guarded_dst.as_path().parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("creating dir {}", parent.display()))?;
        }
        let n = fs::copy(guarded_src.as_path(), guarded_dst.as_path()).with_context(|| {
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
    pub fn copy_dir_recursive(&self, src: &GuardedPath, dst: &GuardedPath) -> Result<()> {
        let guarded_dst_root = self
            .check_access(dst.as_path(), AccessMode::Write)
            .with_context(|| format!("copy destination denied for {}", dst.display()))?;
        fs::create_dir_all(guarded_dst_root.as_path())
            .with_context(|| format!("creating dir {}", guarded_dst_root.display()))?;

        for entry in fs::read_dir(src.as_path())? {
            let entry = entry?;
            let file_type = entry.file_type()?;
            let src_path = entry.path();
            let dst_path = guarded_dst_root.as_path().join(entry.file_name());

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
                fs::create_dir_all(guarded_dst.as_path())
                    .with_context(|| format!("creating dir {}", guarded_dst.display()))?;
                self.copy_dir_recursive(&guarded_src, &guarded_dst)?;
            } else if file_type.is_file() {
                if let Some(parent) = guarded_dst.as_path().parent() {
                    fs::create_dir_all(parent)
                        .with_context(|| format!("creating dir {}", parent.display()))?;
                }
                fs::copy(guarded_src.as_path(), guarded_dst.as_path()).with_context(|| {
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
    pub fn copy_dir_from_external(&self, src: &UnguardedPath, dst: &GuardedPath) -> Result<()> {
        let guarded_dst_root = self
            .check_access(dst.as_path(), AccessMode::Write)
            .with_context(|| format!("copy destination denied for {}", dst.display()))?;
        fs::create_dir_all(guarded_dst_root.as_path())
            .with_context(|| format!("creating dir {}", guarded_dst_root.display()))?;

        for entry in fs::read_dir(src.as_path())? {
            let entry = entry?;
            let file_type = entry.file_type()?;
            let src_path = entry.path();
            let dst_path = guarded_dst_root.as_path().join(entry.file_name());

            if file_type.is_dir() {
                fs::create_dir_all(&dst_path)
                    .with_context(|| format!("creating dir {}", dst_path.display()))?;
                self.copy_dir_from_external(
                    &UnguardedPath::new(src_path),
                    &GuardedPath::new(guarded_dst_root.root(), &dst_path)?,
                )?;
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
    pub fn copy_file_from_external(&self, src: &UnguardedPath, dst: &GuardedPath) -> Result<u64> {
        let guarded_dst = self
            .check_access(dst.as_path(), AccessMode::Write)
            .with_context(|| format!("copy destination denied for {}", dst.display()))?;
        if let Some(parent) = guarded_dst.as_path().parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("creating dir {}", parent.display()))?;
        }
        let n = fs::copy(src.as_path(), guarded_dst.as_path()).with_context(|| {
            format!(
                "copying {} to {}",
                src.as_path().display(),
                guarded_dst.display()
            )
        })?;
        Ok(n)
    }
}
