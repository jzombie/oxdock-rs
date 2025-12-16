use anyhow::{Context, Result};
#[cfg(not(miri))]
use std::ffi::OsStr;
#[cfg(not(miri))]
use std::process::{Command, Output};

#[cfg(not(miri))]
use super::AccessMode;
use super::PathResolver;
use crate::GuardedPath;
#[cfg(not(miri))]
use anyhow::bail;
#[cfg(not(miri))]
use std::path::PathBuf;

impl PathResolver {
    #[cfg(not(miri))]
    fn git_command(&self) -> GitCommand {
        GitCommand::new(self.build_context.as_path())
    }

    /// Detect whether a .git directory exists at or above the resolver root.
    pub fn has_git_dir(&self) -> Result<bool> {
        let mut cur = Some(self.root.clone());
        while let Some(p) = cur {
            if let Ok(dot_git) = p.join(".git")
                && dot_git.as_path().exists()
            {
                return Ok(true);
            }
            cur = p.parent();
        }
        Ok(false)
    }

    pub fn copy_from_git(&self, rev: &str, from: &str, to: &GuardedPath) -> Result<()> {
        #[cfg(miri)]
        {
            let _ = rev;
            let source = self.resolve_copy_source(from)?;
            if let Some(parent) = to.as_path().parent() {
                let parent_guard = GuardedPath::new(to.root(), parent)?;
                self.create_dir_all_abs(&parent_guard)
                    .with_context(|| format!("creating parent {}", parent.display()))?;
            }
            match self.entry_kind(&source)? {
                super::EntryKind::Dir => self.copy_dir_recursive(&source, to)?,
                super::EntryKind::File => {
                    self.copy_file(&source, to)?;
                }
            }
            return Ok(());
        }

        #[cfg(not(miri))]
        {
            if PathBuf::from(from).is_absolute() {
                bail!("COPY_GIT source must be relative to build context");
            }

            let _ = self
                .check_access_with_root(&self.root, to.as_path(), AccessMode::Write)
                .with_context(|| format!("destination {} escapes allowed root", to.display()))?;

            let _ = self
                .check_access(self.build_context.as_path(), AccessMode::Read)
                .with_context(|| {
                    format!(
                        "build context {} not under root",
                        self.build_context.display()
                    )
                })?;

            let cat_type = {
                let mut cmd = self.git_command();
                cmd.arg("cat-file")
                    .arg("-t")
                    .arg(format!("{}:{}", rev, from));
                cmd.output()
            };

            if let Ok(tout) = cat_type
                && tout.status.success()
            {
                let typ = String::from_utf8_lossy(&tout.stdout).trim().to_string();
                if typ == "blob" {
                    let mut show_cmd = self.git_command();
                    show_cmd.arg("show").arg(format!("{}:{}", rev, from));
                    let show = show_cmd
                        .output()
                        .with_context(|| format!("failed to run git show for {}:{}", rev, from))?;

                    if show.status.success() {
                        if let Some(parent) = to.as_path().parent() {
                            let parent_guard = GuardedPath::new(to.root(), parent)?;
                            self.create_dir_all_abs(&parent_guard)
                                .with_context(|| format!("creating parent {}", parent.display()))?;
                        }
                        self.write_file(to, &show.stdout)
                            .with_context(|| format!("writing git blob to {}", to.display()))?;
                        return Ok(());
                    }
                }
            }

            let mut ls_cmd = self.git_command();
            ls_cmd
                .arg("ls-tree")
                .arg("-r")
                .arg("-z")
                .arg(rev)
                .arg("--")
                .arg(from);
            let ls = ls_cmd
                .output()
                .with_context(|| format!("failed to run git ls-tree for {}:{}", rev, from))?;

            if !ls.status.success() {
                bail!("git ls-tree failed for {}:{}", rev, from);
            }

            let out = ls.stdout;
            if out.is_empty() {
                bail!("path {} not found in rev {}", from, rev);
            }

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
                        to.clone()
                    } else {
                        to.join(&rel)?
                    };

                    if let Some(parent) = dest.as_path().parent() {
                        let parent_guard = GuardedPath::new(dest.root(), parent)?;
                        self.create_dir_all_abs(&parent_guard)
                            .with_context(|| format!("creating parent {}", parent.display()))?;
                    }

                    match typ {
                        "blob" => {
                            let mut blob_cmd = self.git_command();
                            blob_cmd.arg("show").arg(format!("{}:{}", rev, path_str));
                            let blob = blob_cmd.output().with_context(|| {
                                format!("failed to run git show for {}:{}", rev, path_str)
                            })?;
                            if !blob.status.success() {
                                bail!("git show failed for {}:{}", rev, path_str);
                            }

                            // Git uses filemode `120000` to represent symbolic links.
                            // Handle symlink entries by using the blob contents as the
                            // link target: on Unix create an actual symlink from the
                            // target text; on Windows write a textual placeholder file
                            // containing the link target (trimmed). The placeholder is
                            // not an OS-level symlink and may lossy-convert non-UTF8
                            // bytes because `from_utf8_lossy` is used above.
                            if mode == "120000" {
                                let target = String::from_utf8_lossy(&blob.stdout).into_owned();
                                #[cfg(unix)]
                                {
                                    use std::os::unix::fs::symlink;
                                    symlink(target.trim_end_matches('\n'), dest.as_path())
                                        .with_context(|| {
                                            format!(
                                                "creating symlink {} -> {}",
                                                dest.display(),
                                                target
                                            )
                                        })?;
                                }
                                #[cfg(windows)]
                                {
                                    // On Windows create a textual placeholder file containing the
                                    // symlink target (trimmed). This does NOT create an OS-level
                                    // symlink and may lose or alter non-UTF8 bytes (from_utf8_lossy
                                    // is used above).
                                    self.write_file(
                                        &dest,
                                        target.trim_end_matches('\n').as_bytes(),
                                    )
                                    .with_context(|| {
                                        format!(
                                            "writing symlink placeholder {} -> {}",
                                            dest.display(),
                                            target
                                        )
                                    })?;
                                }
                            } else {
                                self.write_file(&dest, &blob.stdout).with_context(|| {
                                    format!("writing blob to {}", dest.display())
                                })?;
                            }
                        }
                        "commit" => {
                            bail!("submodule entries are not supported: {}", path_str);
                        }
                        _ => {}
                    }
                }
            }
            return Ok(());
        }

        #[allow(unreachable_code)]
        Ok(())
    }
}

#[cfg(not(miri))]
#[allow(clippy::disallowed_methods, clippy::disallowed_types)]
struct GitCommand {
    inner: Command,
}

#[cfg(not(miri))]
#[allow(clippy::disallowed_methods)]
impl GitCommand {
    fn new(cwd: &std::path::Path) -> Self {
        let mut cmd = Command::new("git");
        cmd.arg("-C").arg(cwd);
        Self { inner: cmd }
    }

    fn arg(&mut self, arg: impl AsRef<OsStr>) -> &mut Self {
        self.inner.arg(arg);
        self
    }

    fn output(&mut self) -> Result<Output> {
        let desc = format!("{:?}", self.inner);
        let out = self
            .inner
            .output()
            .with_context(|| format!("failed to run {desc}"))?;
        Ok(out)
    }
}
