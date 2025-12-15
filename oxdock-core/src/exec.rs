use anyhow::{Context, Result, bail};
use std::collections::HashMap;
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, Command as ProcessCommand, ExitStatus};

use crate::ast::{Step, StepKind, WorkspaceTarget};
use crate::resolver::PathResolver;

pub fn run_steps(fs_root: &Path, steps: &[Step]) -> Result<()> {
    run_steps_with_context(fs_root, fs_root, steps)
}

pub fn run_steps_with_context(fs_root: &Path, build_context: &Path, steps: &[Step]) -> Result<()> {
    run_steps_with_context_result(fs_root, build_context, steps).map(|_| ())
}

/// Execute the DSL and return the final working directory after all steps.
pub fn run_steps_with_context_result(
    fs_root: &Path,
    build_context: &Path,
    steps: &[Step],
) -> Result<PathBuf> {
    match run_steps_inner(fs_root, build_context, steps) {
        Ok(final_cwd) => Ok(final_cwd),
        Err(err) => {
            // Compose a single error message with the top cause plus a compact fs snapshot.
            let chain = err.chain().map(|e| e.to_string()).collect::<Vec<_>>();
            let primary = chain
                .first()
                .cloned()
                .unwrap_or_else(|| "unknown error".into());
            let rest = if chain.len() > 1 {
                let causes = chain
                    .iter()
                    .skip(1)
                    .map(|s| s.as_str())
                    .collect::<Vec<_>>()
                    .join("\n  ");
                format!("\ncauses:\n  {}", causes)
            } else {
                String::new()
            };
            let tree = describe_dir(fs_root, 2, 24);
            let snapshot = format!(
                "filesystem snapshot (root {}):\n{}",
                fs_root.display(),
                tree
            );
            let msg = format!("{}{}\n{}", primary, rest, snapshot);
            Err(anyhow::anyhow!(msg))
        }
    }
}

fn run_steps_inner(fs_root: &Path, build_context: &Path, steps: &[Step]) -> Result<PathBuf> {
    let cargo_target_dir = fs_root.join(".cargo-target");
    let mut resolver = PathResolver::new(fs_root, build_context);
    let mut cwd = resolver.root().to_path_buf();
    let mut envs: HashMap<String, String> = HashMap::new();
    let mut bg_children: Vec<Child> = Vec::new();

    let check_bg = |bg: &mut Vec<Child>| -> Result<Option<ExitStatus>> {
        let mut finished: Option<ExitStatus> = None;
        for child in bg.iter_mut() {
            if let Some(status) = child.try_wait()? {
                finished = Some(status);
                break;
            }
        }
        if let Some(status) = finished {
            // Tear down remaining background children.
            for child in bg.iter_mut() {
                if child.try_wait()?.is_none() {
                    let _ = child.kill();
                    let _ = child.wait();
                }
            }
            bg.clear();
            return Ok(Some(status));
        }
        Ok(None)
    };

    for (idx, step) in steps.iter().enumerate() {
        if !crate::ast::guards_allow_any(&step.guards, &envs) {
            continue;
        }
        match &step.kind {
            StepKind::Workdir(path) => {
                cwd = resolver
                    .resolve_workdir(&cwd, path)
                    .with_context(|| format!("step {}: WORKDIR {}", idx + 1, path))?;
            }
            StepKind::Workspace(target) => match target {
                WorkspaceTarget::Snapshot => {
                    resolver.set_root(fs_root);
                    cwd = resolver.root().to_path_buf();
                }
                WorkspaceTarget::Local => {
                    resolver.set_root(build_context);
                    cwd = resolver.root().to_path_buf();
                }
            },
            StepKind::Env { key, value } => {
                envs.insert(key.clone(), value.clone());
            }
            StepKind::Run(cmd) => {
                let mut command = shell_cmd(cmd);
                command.current_dir(&cwd);
                command.envs(envs.iter());
                // Prevent contention with the outer Cargo build by isolating nested cargo targets.
                command.env("CARGO_TARGET_DIR", &cargo_target_dir);
                run_cmd(&mut command).with_context(|| format!("step {}: RUN {}", idx + 1, cmd))?;
            }
            StepKind::Echo(msg) => {
                let out = interpolate(msg, &envs);
                println!("{}", out);
            }
            StepKind::RunBg(cmd) => {
                let mut command = shell_cmd(cmd);
                command.current_dir(&cwd);
                command.envs(envs.iter());
                command.env("CARGO_TARGET_DIR", &cargo_target_dir);
                let child = command
                    .spawn()
                    .with_context(|| format!("step {}: RUN_BG {}", idx + 1, cmd))?;
                bg_children.push(child);
            }
            StepKind::Copy { from, to } => {
                let from_abs = resolver
                    .resolve_copy_source(from)
                    .with_context(|| format!("step {}: COPY {} {}", idx + 1, from, to))?;
                let to_abs = resolver
                    .resolve_write(&cwd, to)
                    .with_context(|| format!("step {}: COPY {} {}", idx + 1, from, to))?;
                copy_entry(&from_abs, &to_abs)
                    .with_context(|| format!("step {}: COPY {} {}", idx + 1, from, to))?;
            }
            StepKind::Symlink { from, to } => {
                let to_abs = resolver
                    .resolve_write(&cwd, to)
                    .with_context(|| format!("step {}: SYMLINK {} {}", idx + 1, from, to))?;
                if to_abs.exists() {
                    bail!("SYMLINK destination already exists: {}", to_abs.display());
                }
                let from_abs = resolver
                    .resolve_copy_source(from)
                    .with_context(|| format!("step {}: SYMLINK {} {}", idx + 1, from, to))?;
                if from_abs == to_abs {
                    bail!(
                        "SYMLINK source resolves to the destination itself: {}",
                        from_abs.display()
                    );
                }
                #[cfg(unix)]
                std::os::unix::fs::symlink(&from_abs, &to_abs)
                    .with_context(|| format!("step {}: SYMLINK {} {}", idx + 1, from, to))?;
                #[cfg(all(windows, not(unix)))]
                std::os::windows::fs::symlink_dir(&from_abs, &to_abs)
                    .with_context(|| format!("step {}: SYMLINK {} {}", idx + 1, from, to))?;
                #[cfg(not(any(unix, windows)))]
                copy_dir(&from_abs, &to_abs)?;
            }
            StepKind::Mkdir(path) => {
                let target = resolver
                    .resolve_write(&cwd, path)
                    .with_context(|| format!("step {}: MKDIR {}", idx + 1, path))?;
                fs::create_dir_all(&target)
                    .with_context(|| format!("failed to create dir {}", target.display()))?;
            }
            StepKind::Ls(path_opt) => {
                let dir = if let Some(p) = path_opt.as_deref() {
                    resolver
                        .resolve_read(&cwd, p)
                        .with_context(|| format!("step {}: LS {}", idx + 1, p))?
                } else {
                    cwd.clone()
                };
                let mut entries: Vec<_> = fs::read_dir(&dir)
                    .with_context(|| format!("failed to read dir {}", dir.display()))?
                    .collect::<Result<_, _>>()?;
                entries.sort_by_key(|a| a.file_name());
                println!("{}:", dir.display());
                for entry in entries {
                    println!("{}", entry.file_name().to_string_lossy());
                }
            }
            StepKind::Cat(path) => {
                let target = resolver
                    .resolve_read(&cwd, path)
                    .with_context(|| format!("step {}: CAT {}", idx + 1, path))?;
                let data = fs::read(&target)
                    .with_context(|| format!("failed to read {}", target.display()))?;
                let mut out = io::stdout();
                out.write_all(&data)
                    .with_context(|| format!("failed to write {} to stdout", target.display()))?;
                out.flush().ok();
            }
            StepKind::Write { path, contents } => {
                let target = resolver
                    .resolve_write(&cwd, path)
                    .with_context(|| format!("step {}: WRITE {}", idx + 1, path))?;
                if let Some(parent) = target.parent() {
                    fs::create_dir_all(parent)
                        .with_context(|| format!("failed to create parent {}", parent.display()))?;
                }
                fs::write(&target, contents)
                    .with_context(|| format!("failed to write {}", target.display()))?;
            }
            StepKind::Exit(code) => {
                for child in bg_children.iter_mut() {
                    if child.try_wait()?.is_none() {
                        let _ = child.kill();
                        let _ = child.wait();
                    }
                }
                bg_children.clear();
                bail!("EXIT requested with code {}", code);
            }
        }

        if let Some(status) = check_bg(&mut bg_children)? {
            if status.success() {
                return Ok(cwd);
            } else {
                bail!("RUN_BG exited with status {}", status);
            }
        }
    }

    if !bg_children.is_empty() {
        let mut first = bg_children.remove(0);
        let status = first.wait()?;
        for child in bg_children.iter_mut() {
            if child.try_wait()?.is_none() {
                let _ = child.kill();
                let _ = child.wait();
            }
        }
        bg_children.clear();
        if status.success() {
            return Ok(cwd);
        } else {
            bail!("RUN_BG exited with status {}", status);
        }
    }

    Ok(cwd)
}

fn run_cmd(cmd: &mut ProcessCommand) -> Result<()> {
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

fn describe_dir(root: &Path, max_depth: usize, max_entries: usize) -> String {
    fn helper(path: &Path, depth: usize, max_depth: usize, left: &mut usize, out: &mut String) {
        if *left == 0 {
            return;
        }
        let indent = "  ".repeat(depth);
        if depth > 0 {
            out.push_str(&format!(
                "{}{}\n",
                indent,
                path.file_name().unwrap_or_default().to_string_lossy()
            ));
        }
        if depth >= max_depth {
            return;
        }
        let entries = match fs::read_dir(path) {
            Ok(e) => e,
            Err(_) => return,
        };
        let mut names: Vec<_> = entries.filter_map(|e| e.ok()).collect();
        names.sort_by_key(|a| a.file_name());
        for entry in names {
            if *left == 0 {
                return;
            }
            *left -= 1;
            let p = entry.path();
            if p.is_dir() {
                helper(&p, depth + 1, max_depth, left, out);
            } else {
                out.push_str(&format!(
                    "{}  {}\n",
                    indent,
                    entry.file_name().to_string_lossy()
                ));
            }
        }
    }

    let mut out = String::new();
    let mut left = max_entries;
    helper(root, 0, max_depth, &mut left, &mut out);
    out
}

pub fn shell_program() -> String {
    #[cfg(windows)]
    {
        std::env::var("COMSPEC").unwrap_or_else(|_| "cmd".to_string())
    }

    #[cfg(not(windows))]
    {
        std::env::var("SHELL").unwrap_or_else(|_| "sh".to_string())
    }
}

fn shell_cmd(cmd: &str) -> ProcessCommand {
    let program = shell_program();
    let mut c = ProcessCommand::new(program);
    if cfg!(windows) {
        c.arg("/C").arg(cmd);
    } else {
        c.arg("-c").arg(cmd);
    }
    c
}

#[allow(clippy::while_let_on_iterator)]
fn interpolate(template: &str, script_envs: &HashMap<String, String>) -> String {
    let mut out = String::with_capacity(template.len());
    let mut chars = template.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '$' {
            if let Some(&'{') = chars.peek() {
                chars.next();
                let mut name = String::new();
                while let Some(&ch) = chars.peek() {
                    chars.next();
                    if ch == '}' {
                        break;
                    }
                    name.push(ch);
                }
                if !name.is_empty() {
                    let val = script_envs
                        .get(&name)
                        .cloned()
                        .or_else(|| std::env::var(&name).ok())
                        .unwrap_or_default();
                    out.push_str(&val);
                }
            } else {
                let mut name = String::new();
                while let Some(&ch) = chars.peek() {
                    if ch.is_ascii_alphanumeric() || ch == '_' {
                        name.push(ch);
                        chars.next();
                    } else {
                        break;
                    }
                }
                if !name.is_empty() {
                    let val = script_envs
                        .get(&name)
                        .cloned()
                        .or_else(|| std::env::var(&name).ok())
                        .unwrap_or_default();
                    out.push_str(&val);
                } else {
                    out.push('$');
                }
            }
        } else if c == '{' {
            let mut name = String::new();
            while let Some(ch) = chars.next() {
                if ch == '}' {
                    break;
                }
                name.push(ch);
            }
            if !name.is_empty() {
                let val = script_envs
                    .get(&name)
                    .cloned()
                    .or_else(|| std::env::var(&name).ok())
                    .unwrap_or_default();
                out.push_str(&val);
            }
        } else {
            out.push(c);
        }
    }
    out
}
