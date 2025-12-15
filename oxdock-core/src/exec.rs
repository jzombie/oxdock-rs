use anyhow::{Context, Result, bail};
use std::collections::HashMap;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, Command as ProcessCommand, ExitStatus, Stdio};

use crate::ast::{self, Step, StepKind, WorkspaceTarget};
use oxdock_fs::resolver::PathResolver;

struct ExecState {
    resolver: PathResolver,
    cargo_target_dir: PathBuf,
    cwd: PathBuf,
    envs: HashMap<String, String>,
    bg_children: Vec<Child>,
}

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
            let resolver = PathResolver::new(fs_root, build_context);
            let tree = describe_dir(&resolver, fs_root, 2, 24);
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
    let mut state = ExecState {
        resolver: PathResolver::new(fs_root, build_context),
        cargo_target_dir: fs_root.join(".cargo-target"),
        cwd: PathResolver::new(fs_root, build_context)
            .root()
            .to_path_buf(),
        envs: HashMap::new(),
        bg_children: Vec::new(),
    };

    let mut stdout = io::stdout();
    execute_steps(&mut state, steps, false, &mut stdout)?;

    Ok(state.cwd)
}

fn execute_steps(
    state: &mut ExecState,
    steps: &[Step],
    capture_output: bool,
    out: &mut dyn Write,
) -> Result<()> {
    let fs_root = state.resolver.root().to_path_buf();
    let build_context = state.resolver.build_context().to_path_buf();

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
        if !crate::ast::guards_allow_any(&step.guards, &state.envs) {
            continue;
        }
        match &step.kind {
            StepKind::Workdir(path) => {
                state.cwd = state
                    .resolver
                    .resolve_workdir(&state.cwd, path)
                    .with_context(|| format!("step {}: WORKDIR {}", idx + 1, path))?;
            }
            StepKind::Workspace(target) => match target {
                WorkspaceTarget::Snapshot => {
                    state.resolver.set_root(&fs_root);
                    state.cwd = state.resolver.root().to_path_buf();
                }
                WorkspaceTarget::Local => {
                    state.resolver.set_root(&build_context);
                    state.cwd = state.resolver.root().to_path_buf();
                }
            },
            StepKind::Env { key, value } => {
                state.envs.insert(key.clone(), value.clone());
            }
            StepKind::Run(cmd) => {
                let mut command = shell_cmd(cmd);
                command.current_dir(&state.cwd);
                command.envs(state.envs.iter());
                // Prevent contention with the outer Cargo build by isolating nested cargo targets.
                command.env("CARGO_TARGET_DIR", state.cargo_target_dir.as_path());
                if capture_output {
                    command.stdout(Stdio::piped());
                    let output = command
                        .output()
                        .with_context(|| format!("step {}: RUN {}", idx + 1, cmd))?;
                    if !output.status.success() {
                        bail!("command {:?} failed with status {}", command, output.status);
                    }
                    out.write_all(&output.stdout)?;
                } else {
                    run_cmd(&mut command)
                        .with_context(|| format!("step {}: RUN {}", idx + 1, cmd))?;
                }
            }
            StepKind::Echo(msg) => {
                let rendered = interpolate(msg, &state.envs);
                writeln!(out, "{}", rendered)?;
            }
            StepKind::RunBg(cmd) => {
                if capture_output {
                    bail!("RUN_BG is not supported inside CAPTURE");
                }
                let mut command = shell_cmd(cmd);
                command.current_dir(&state.cwd);
                command.envs(state.envs.iter());
                command.env("CARGO_TARGET_DIR", state.cargo_target_dir.as_path());
                let child = command
                    .spawn()
                    .with_context(|| format!("step {}: RUN_BG {}", idx + 1, cmd))?;
                state.bg_children.push(child);
            }
            StepKind::Copy { from, to } => {
                let from_abs = state
                    .resolver
                    .resolve_copy_source(from)
                    .with_context(|| format!("step {}: COPY {} {}", idx + 1, from, to))?;
                let to_abs = state
                    .resolver
                    .resolve_write(&state.cwd, to)
                    .with_context(|| format!("step {}: COPY {} {}", idx + 1, from, to))?;
                copy_entry(&state.resolver, &from_abs, &to_abs)
                    .with_context(|| format!("step {}: COPY {} {}", idx + 1, from, to))?;
            }
            StepKind::CopyGit { rev, from, to } => {
                let to_abs = state
                    .resolver
                    .resolve_write(&state.cwd, to)
                    .with_context(|| {
                        format!("step {}: COPY_GIT {} {} {}", idx + 1, rev, from, to)
                    })?;
                state
                    .resolver
                    .copy_from_git(rev, from, &to_abs)
                    .with_context(|| {
                        format!("step {}: COPY_GIT {} {} {}", idx + 1, rev, from, to)
                    })?;
            }
            StepKind::Symlink { from, to } => {
                let to_abs = state
                    .resolver
                    .resolve_write(&state.cwd, to)
                    .with_context(|| format!("step {}: SYMLINK {} {}", idx + 1, from, to))?;
                if to_abs.exists() {
                    bail!("SYMLINK destination already exists: {}", to_abs.display());
                }
                let from_abs = state
                    .resolver
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
                let target = state
                    .resolver
                    .resolve_write(&state.cwd, path)
                    .with_context(|| format!("step {}: MKDIR {}", idx + 1, path))?;
                state
                    .resolver
                    .create_dir_all_abs(&target)
                    .with_context(|| format!("failed to create dir {}", target.display()))?;
            }
            StepKind::Ls(path_opt) => {
                let dir = if let Some(p) = path_opt.as_deref() {
                    state
                        .resolver
                        .resolve_read(&state.cwd, p)
                        .with_context(|| format!("step {}: LS {}", idx + 1, p))?
                } else {
                    state.cwd.clone()
                };
                let mut entries = state
                    .resolver
                    .read_dir_entries(&dir)
                    .with_context(|| format!("failed to read dir {}", dir.display()))?;
                entries.sort_by_key(|a| a.file_name());
                writeln!(out, "{}:", dir.display())?;
                for entry in entries {
                    writeln!(out, "{}", entry.file_name().to_string_lossy())?;
                }
            }
            StepKind::Cwd => {
                // Print the canonical (physical) current working directory to stdout.
                let real = canonical_cwd(&state.resolver, &state.cwd).with_context(|| {
                    format!(
                        "step {}: CWD failed to canonicalize {}",
                        idx + 1,
                        state.cwd.display()
                    )
                })?;
                writeln!(out, "{}", real)?;
            }
            StepKind::Cat(path) => {
                let target = state
                    .resolver
                    .resolve_read(&state.cwd, path)
                    .with_context(|| format!("step {}: CAT {}", idx + 1, path))?;
                let data = state
                    .resolver
                    .read_file(&target)
                    .with_context(|| format!("failed to read {}", target.display()))?;
                out.write_all(&data)
                    .with_context(|| format!("failed to write {} to stdout", target.display()))?;
            }
            StepKind::Write { path, contents } => {
                let target = state
                    .resolver
                    .resolve_write(&state.cwd, path)
                    .with_context(|| format!("step {}: WRITE {}", idx + 1, path))?;
                if let Some(parent) = target.parent() {
                    state
                        .resolver
                        .create_dir_all_abs(parent)
                        .with_context(|| format!("failed to create parent {}", parent.display()))?;
                }
                state
                    .resolver
                    .write_file(&target, contents.as_bytes())
                    .with_context(|| format!("failed to write {}", target.display()))?;
            }
            StepKind::Capture { path, cmd } => {
                let target = state
                    .resolver
                    .resolve_write(&state.cwd, path)
                    .with_context(|| format!("step {}: CAPTURE {}", idx + 1, path))?;
                if let Some(parent) = target.parent() {
                    state
                        .resolver
                        .create_dir_all_abs(parent)
                        .with_context(|| format!("failed to create parent {}", parent.display()))?;
                }
                let steps = ast::parse_script(cmd)
                    .with_context(|| format!("step {}: CAPTURE parse failed", idx + 1))?;
                if steps.len() != 1 {
                    bail!("CAPTURE expects exactly one instruction");
                }
                let mut sub_state = ExecState {
                    resolver: PathResolver::new(
                        state.resolver.root(),
                        state.resolver.build_context(),
                    ),
                    cargo_target_dir: state.cargo_target_dir.clone(),
                    cwd: state.cwd.clone(),
                    envs: state.envs.clone(),
                    bg_children: Vec::new(),
                };
                let mut buf: Vec<u8> = Vec::new();
                execute_steps(&mut sub_state, &steps, true, &mut buf)?;
                state
                    .resolver
                    .write_file(&target, &buf)
                    .with_context(|| format!("failed to write {}", target.display()))?;
            }
            StepKind::Exit(code) => {
                for child in state.bg_children.iter_mut() {
                    if child.try_wait()?.is_none() {
                        let _ = child.kill();
                        let _ = child.wait();
                    }
                }
                state.bg_children.clear();
                bail!("EXIT requested with code {}", code);
            }
        }

        if let Some(status) = check_bg(&mut state.bg_children)? {
            if status.success() {
                return Ok(());
            } else {
                bail!("RUN_BG exited with status {}", status);
            }
        }
    }

    if !state.bg_children.is_empty() {
        let mut first = state.bg_children.remove(0);
        let status = first.wait()?;
        for child in state.bg_children.iter_mut() {
            if child.try_wait()?.is_none() {
                let _ = child.kill();
                let _ = child.wait();
            }
        }
        state.bg_children.clear();
        if status.success() {
            return Ok(());
        } else {
            bail!("RUN_BG exited with status {}", status);
        }
    }

    Ok(())
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

fn copy_entry(resolver: &PathResolver, src: &Path, dst: &Path) -> Result<()> {
    if !src.exists() {
        bail!("source missing: {}", src.display());
    }
    let meta = resolver.metadata_abs(src)?;
    if meta.is_dir() {
        resolver.copy_dir_recursive(src, dst)?;
    } else if meta.is_file() {
        if let Some(parent) = dst.parent() {
            resolver.create_dir_all_abs(parent)?;
        }
        resolver.copy_file(src, dst)?;
    } else {
        bail!("unsupported file type: {}", src.display());
    }
    Ok(())
}

// helper removed: resolver handles recursive copying now

fn canonical_cwd(resolver: &PathResolver, cwd: &Path) -> Result<String> {
    Ok(resolver.canonicalize_abs(cwd)?.display().to_string())
}

fn describe_dir(
    resolver: &PathResolver,
    root: &Path,
    max_depth: usize,
    max_entries: usize,
) -> String {
    fn helper(
        resolver: &PathResolver,
        path: &Path,
        depth: usize,
        max_depth: usize,
        left: &mut usize,
        out: &mut String,
    ) {
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
        let entries = match resolver.read_dir_entries(path) {
            Ok(e) => e,
            Err(_) => return,
        };
        let mut names: Vec<_> = entries.into_iter().collect();
        names.sort_by_key(|a| a.file_name());
        for entry in names {
            if *left == 0 {
                return;
            }
            *left -= 1;
            let p = entry.path();
            if p.is_dir() {
                helper(resolver, &p, depth + 1, max_depth, left, out);
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
    helper(resolver, root, 0, max_depth, &mut left, &mut out);
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
