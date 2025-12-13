use anyhow::{Context, Result, bail};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Debug, Clone)]
pub enum Step {
    Workdir(String),
    Run(String),
    Copy { from: String, to: String },
    Symlink { link: String, target: String },
    Mkdir(String),
    Ls(Option<String>),
    Write { path: String, contents: String },
    Shell,
}

pub fn parse_script(input: &str) -> Result<Vec<Step>> {
    let mut steps = Vec::new();
    for (idx, line) in input.lines().enumerate() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        let mut parts = line.splitn(2, ' ');
        let op = parts.next().unwrap();
        let rest = parts.next().map(str::trim).unwrap_or("");
        match op {
            "WORKDIR" => {
                if rest.is_empty() {
                    bail!("line {}: WORKDIR requires a path", idx + 1);
                }
                steps.push(Step::Workdir(rest.to_string()));
            }
            "RUN" => {
                if rest.is_empty() {
                    bail!("line {}: RUN requires a command", idx + 1);
                }
                steps.push(Step::Run(rest.to_string()));
            }
            "COPY" => {
                let mut p = rest.split_whitespace();
                let from = p.next().ok_or_else(|| {
                    anyhow::anyhow!("line {}: COPY requires <from> <to>", idx + 1)
                })?;
                let to = p.next().ok_or_else(|| {
                    anyhow::anyhow!("line {}: COPY requires <from> <to>", idx + 1)
                })?;
                steps.push(Step::Copy {
                    from: from.to_string(),
                    to: to.to_string(),
                });
            }
            "SYMLINK" => {
                let mut p = rest.split_whitespace();
                let link = p.next().ok_or_else(|| {
                    anyhow::anyhow!("line {}: SYMLINK requires <link> <target>", idx + 1)
                })?;
                let target = p.next().ok_or_else(|| {
                    anyhow::anyhow!("line {}: SYMLINK requires <link> <target>", idx + 1)
                })?;
                steps.push(Step::Symlink {
                    link: link.to_string(),
                    target: target.to_string(),
                });
            }
            "MKDIR" => {
                if rest.is_empty() {
                    bail!("line {}: MKDIR requires a path", idx + 1);
                }
                steps.push(Step::Mkdir(rest.to_string()));
            }
            "LS" => {
                let path = rest
                    .split_whitespace()
                    .next()
                    .filter(|s| !s.is_empty())
                    .map(|s| s.to_string());
                steps.push(Step::Ls(path));
            }
            "WRITE" => {
                let mut p = rest.splitn(2, ' ');
                let path = p.next().filter(|s| !s.is_empty()).ok_or_else(|| {
                    anyhow::anyhow!("line {}: WRITE requires <path> <contents>", idx + 1)
                })?;
                let contents = p.next().filter(|s| !s.is_empty()).ok_or_else(|| {
                    anyhow::anyhow!("line {}: WRITE requires <path> <contents>", idx + 1)
                })?;
                steps.push(Step::Write {
                    path: path.to_string(),
                    contents: contents.to_string(),
                });
            }
            "SHELL" => {
                steps.push(Step::Shell);
            }
            other => bail!("line {}: unknown instruction '{}'", idx + 1, other),
        }
    }
    Ok(steps)
}

pub fn run_steps(fs_root: &Path, steps: &[Step]) -> Result<()> {
    run_steps_with_context(fs_root, fs_root, steps)
}

pub fn run_steps_with_context(fs_root: &Path, build_context: &Path, steps: &[Step]) -> Result<()> {
    let mut cwd = fs_root.to_path_buf();

    for step in steps {
        match step {
            Step::Workdir(path) => {
                cwd = resolve_workdir(fs_root, &cwd, path)?;
            }
            Step::Run(cmd) => {
                let mut command = shell_cmd(cmd);
                command.current_dir(&cwd);
                run_cmd(&mut command)?;
            }
            Step::Copy { from, to } => {
                let from_abs = resolve_copy_source(build_context, from)?;
                let to_abs = resolve_dest(&cwd, to);
                copy_entry(&from_abs, &to_abs)?;
            }
            Step::Symlink { link, target } => {
                let link_abs = resolve_dest(&cwd, link);
                if link_abs.exists() {
                    fs::remove_file(&link_abs)?;
                }
                #[cfg(unix)]
                std::os::unix::fs::symlink(target, &link_abs)?;
                #[cfg(all(windows, not(unix)))]
                std::os::windows::fs::symlink_dir(target, &link_abs)?;
                #[cfg(not(any(unix, windows)))]
                copy_dir(&resolve_dest(&cwd, target), &link_abs)?;
            }
            Step::Mkdir(path) => {
                let target = resolve_dest(&cwd, path);
                fs::create_dir_all(&target)
                    .with_context(|| format!("failed to create dir {}", target.display()))?;
            }
            Step::Ls(path_opt) => {
                let dir = path_opt
                    .as_deref()
                    .map(|p| resolve_dest(&cwd, p))
                    .unwrap_or_else(|| cwd.clone());
                let mut entries: Vec<_> = fs::read_dir(&dir)
                    .with_context(|| format!("failed to read dir {}", dir.display()))?
                    .collect::<Result<_, _>>()?;
                entries.sort_by(|a, b| a.file_name().cmp(&b.file_name()));
                println!("{}:", dir.display());
                for entry in entries {
                    println!("{}", entry.file_name().to_string_lossy());
                }
            }
            Step::Write { path, contents } => {
                let target = resolve_dest(&cwd, path);
                if let Some(parent) = target.parent() {
                    fs::create_dir_all(parent)
                        .with_context(|| format!("failed to create parent {}", parent.display()))?;
                }
                fs::write(&target, contents)
                    .with_context(|| format!("failed to write {}", target.display()))?;
            }
            Step::Shell => {
                run_shell(&cwd)?;
            }
        }
    }

    Ok(())
}

fn run_cmd(cmd: &mut Command) -> Result<()> {
    let status = cmd
        .status()
        .with_context(|| format!("failed to run {:?}", cmd))?;
    if !status.success() {
        bail!("command {:?} failed with status {}", cmd, status);
    }
    Ok(())
}

fn copy_entry(src: &Path, dst: &Path) -> Result<()> {
    if !src.exists() {
        bail!("source missing: {}", src.display());
    }
    let meta = src.metadata()?;
    if meta.is_dir() {
        copy_dir(src, dst)?;
    } else if meta.is_file() {
        if let Some(parent) = dst.parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("creating dir {}", parent.display()))?;
        }
        fs::copy(src, dst)
            .with_context(|| format!("copying {} to {}", src.display(), dst.display()))?;
    } else {
        bail!("unsupported file type: {}", src.display());
    }
    Ok(())
}

fn copy_dir(src: &Path, dst: &Path) -> Result<()> {
    fs::create_dir_all(dst).with_context(|| format!("creating dir {}", dst.display()))?;
    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let file_type = entry.file_type()?;
        let src_path = entry.path();
        let dst_path = dst.join(entry.file_name());
        if file_type.is_dir() {
            copy_dir(&src_path, &dst_path)?;
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

fn resolve_workdir(root: &Path, current: &Path, new_dir: &str) -> Result<PathBuf> {
    if new_dir == "/" {
        return Ok(root.to_path_buf());
    }
    let candidate = if Path::new(new_dir).is_absolute() {
        PathBuf::from(new_dir)
    } else {
        current.join(new_dir)
    };
    if !candidate.exists() {
        bail!("WORKDIR does not exist: {}", candidate.display());
    }
    Ok(candidate)
}

fn resolve_dest(cwd: &Path, rel: &str) -> PathBuf {
    if Path::new(rel).is_absolute() {
        PathBuf::from(rel)
    } else {
        cwd.join(rel)
    }
}

fn resolve_copy_source(build_context: &Path, from: &str) -> Result<PathBuf> {
    if Path::new(from).is_absolute() {
        bail!("COPY source must be relative to build context");
    }
    let candidate = build_context.join(from);
    if candidate.exists() {
        Ok(candidate)
    } else {
        bail!(
            "COPY source missing in build context: {}",
            candidate.display()
        );
    }
}

fn shell_cmd(cmd: &str) -> Command {
    let mut c = Command::new("sh");
    c.arg("-c").arg(cmd);
    c
}

fn run_shell(cwd: &Path) -> Result<()> {
    #[cfg(unix)]
    {
        let mut cmd = Command::new("sh");
        cmd.current_dir(cwd);

        // Reattach stdin to the controlling TTY so a piped-in script can still open an interactive shell.
        if let Ok(tty) = fs::File::open("/dev/tty") {
            cmd.stdin(tty);
        }

        let status = cmd.status()?;
        if !status.success() {
            bail!("shell exited with status {}", status);
        }
        return Ok(());
    }

    #[cfg(windows)]
    {
        use std::os::windows::process::CommandExt;

        let mut cmd = Command::new("cmd");
        cmd.current_dir(cwd).arg("/K");

        // Reattach stdin to the console if available; CONIN$ is the Windows console input device.
        if let Ok(con) = fs::File::open("CONIN$") {
            cmd.stdin(con);
        }

        // CREATE_NEW_CONSOLE (0x00000010) to ensure we get an interactive console if none is attached.
        const CREATE_NEW_CONSOLE: u32 = 0x00000010;
        cmd.creation_flags(CREATE_NEW_CONSOLE);

        let status = cmd.status()?;
        if !status.success() {
            bail!("shell exited with status {}", status);
        }
        return Ok(());
    }

    #[cfg(not(any(unix, windows)))]
    {
        let _ = cwd;
        bail!("run_shell unsupported on this platform");
    }
}
