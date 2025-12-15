use anyhow::{Context, Result, bail};
use std::process::Command;
use std::fs;
use std::path::Path;

use super::{AccessMode, PathResolver};

// COPY_GIT support using git plumbing commands.
impl PathResolver {
    #[allow(clippy::disallowed_methods)]
    pub fn copy_from_git(&self, rev: &str, from: &str, to: &Path) -> Result<()> {
        if Path::new(from).is_absolute() {
            bail!("COPY_GIT source must be relative to build context");
        }

        let _ = self
            .check_access_with_root(&self.root, to, AccessMode::Write)
            .with_context(|| format!("destination {} escapes allowed root", to.display()))?;

        let _ = self
            .check_access(&self.build_context, AccessMode::Read)
            .with_context(|| {
                format!(
                    "build context {} not under root",
                    self.build_context.display()
                )
            })?;

        let cat_type = Command::new("git")
            .arg("-C")
            .arg(&self.build_context)
            .arg("cat-file")
            .arg("-t")
            .arg(format!("{}:{}", rev, from))
            .output();

        if let Ok(tout) = cat_type && tout.status.success() {
            let typ = String::from_utf8_lossy(&tout.stdout).trim().to_string();
            if typ == "blob" {
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
        }

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
                    _ => {}
                }
            }
        }

        Ok(())
    }
}
