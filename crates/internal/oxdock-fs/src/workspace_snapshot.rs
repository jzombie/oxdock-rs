#[cfg(windows)]
use crate::command_path;
use crate::{GuardedPath, GuardedTempDir, PathResolver};
use anyhow::{Context, Result, bail};
#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
use std::process::Command;

/// Copy the workspace rooted at `source` into `dest`, excluding VCS metadata
/// such as `.git`. Both paths must be guarded to ensure callers cannot escape
/// their allowed roots.
pub fn copy_workspace_to(source: &GuardedPath, dest: &GuardedPath) -> Result<()> {
    let source_resolver = PathResolver::new(source.as_path(), source.as_path())?;
    let dest_resolver = PathResolver::new(dest.as_path(), dest.as_path())?;
    copy_dir_filtered(&source_resolver, &dest_resolver, source, dest)
}

/// Convenience wrapper that materializes a workspace snapshot inside a guarded
/// temporary directory. The snapshot directory is removed automatically when
/// the returned struct is dropped unless persisted.
pub struct WorkspaceSnapshot {
    tempdir: GuardedTempDir,
}

impl WorkspaceSnapshot {
    pub fn new(source: &GuardedPath) -> Result<Self> {
        let tempdir = GuardedPath::tempdir()?;
        let dest = tempdir.as_guarded_path().clone();
        let tar_path = dest.join("snapshot.tar")?;
        run_git_archive(source, &tar_path)?;
        extract_tar(&tar_path, &dest)?;
        let resolver = PathResolver::new(dest.as_path(), dest.as_path())?;
        resolver
            .remove_file(&tar_path)
            .with_context(|| format!("failed to remove {}", tar_path.display()))?;
        Ok(Self { tempdir })
    }

    pub fn root(&self) -> &GuardedPath {
        self.tempdir.as_guarded_path()
    }

    #[allow(dead_code)]
    pub fn into_tempdir(self) -> GuardedTempDir {
        self.tempdir
    }
}

fn run_git_archive(source: &GuardedPath, tar_path: &GuardedPath) -> Result<()> {
    #[cfg(windows)]
    let tar_arg = command_path(tar_path);
    #[cfg(windows)]
    let tar_dest = tar_arg.as_ref();
    #[cfg(not(windows))]
    let tar_dest = tar_path.as_path();

    #[allow(clippy::disallowed_methods, clippy::disallowed_types)]
    let status = Command::new("git")
        .current_dir(source.as_path())
        .args(["archive", "--format=tar", "--output"])
        .arg(tar_dest)
        .arg("HEAD")
        .status()?;
    if !status.success() {
        bail!("git archive failed with status {}", status);
    }
    Ok(())
}

fn extract_tar(tar_path: &GuardedPath, dest: &GuardedPath) -> Result<()> {
    #[cfg(windows)]
    let tar_arg = command_path(tar_path);
    #[cfg(windows)]
    let tar_src = tar_arg.as_ref();
    #[cfg(not(windows))]
    let tar_src = tar_path.as_path();

    #[cfg(windows)]
    let dest_arg = command_path(dest);
    #[cfg(windows)]
    let dest_path = dest_arg.as_ref();
    #[cfg(not(windows))]
    let dest_path = dest.as_path();

    #[allow(clippy::disallowed_types, clippy::disallowed_methods)]
    let mut cmd = Command::new("tar");
    #[cfg(unix)]
    {
        cmd.arg("--warning=no-timestamp");
    }
    let status = cmd
        .arg("-xf")
        .arg(tar_src)
        .arg("-C")
        .arg(dest_path)
        .status()?;
    if !status.success() {
        bail!("tar failed with status {}", status);
    }
    Ok(())
}

fn copy_dir_filtered(
    source_resolver: &PathResolver,
    dest_resolver: &PathResolver,
    src: &GuardedPath,
    dst: &GuardedPath,
) -> Result<()> {
    dest_resolver
        .create_dir_all(dst)
        .with_context(|| format!("creating {}", dst.display()))?;

    for entry in source_resolver.read_dir_entries(src)? {
        let name = entry.file_name().to_string_lossy().to_string();
        if name == ".git" || name == "target" {
            continue;
        }

        let entry_path = entry.path();
        let src_child = GuardedPath::new(src.root(), &entry_path).with_context(|| {
            format!(
                "guarding source entry {} within {}",
                entry_path.display(),
                src.display()
            )
        })?;
        let dst_child = dst.join(&name).with_context(|| {
            format!("constructing destination entry {}/{}", dst.display(), name)
        })?;

        let ft = entry.file_type().with_context(|| {
            format!("failed to determine file type for {}", entry_path.display())
        })?;
        if ft.is_dir() {
            copy_dir_filtered(source_resolver, dest_resolver, &src_child, &dst_child)?;
        } else if ft.is_file() {
            let data = source_resolver.read_file(&src_child)?;
            dest_resolver.write_file(&dst_child, &data)?;
        } else if ft.is_symlink() {
            bail!(
                "symlinks are not supported in workspace snapshots (encountered {})",
                entry_path.display()
            );
        } else {
            bail!(
                "unsupported file type in workspace snapshot: {}",
                entry_path.display()
            );
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn snapshot_excludes_git_dir() -> Result<()> {
        let source = GuardedPath::tempdir()?;
        let source_root = source.as_guarded_path().clone();
        let resolver = PathResolver::new(source_root.as_path(), source_root.as_path())?;
        let dot_git = source_root.join(".git")?;
        resolver.create_dir_all(&dot_git)?;
        let file = source_root.join("README.md")?;
        resolver.write_file(&file, b"hello")?;

        let dest = GuardedPath::tempdir()?;
        let dest_root = dest.as_guarded_path().clone();
        copy_workspace_to(&source_root, &dest_root)?;

        let dest_resolver = PathResolver::new(dest_root.as_path(), dest_root.as_path())?;
        let copied = dest_root.join("README.md")?;
        assert_eq!(dest_resolver.read_to_string(&copied)?, "hello");
        assert!(
            !dest_root.join(".git")?.exists(),
            ".git directory should not be copied"
        );
        Ok(())
    }
}
