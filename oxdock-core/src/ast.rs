use anyhow::{Context, Result, bail};
use std::collections::HashMap;
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, Command as ProcessCommand, ExitStatus};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Command {
    Workdir,
    Workspace,
    Env,
    Echo,
    Run,
    RunBg,
    Copy,
    Symlink,
    Mkdir,
    Ls,
    Cat,
    Write,
    Exit,
}

pub const COMMANDS: &[Command] = &[
    Command::Workdir,
    Command::Workspace,
    Command::Env,
    Command::Echo,
    Command::Run,
    Command::RunBg,
    Command::Copy,
    Command::Symlink,
    Command::Mkdir,
    Command::Ls,
    Command::Cat,
    Command::Write,
    Command::Exit,
];

fn platform_matches(target: PlatformGuard) -> bool {
    match target {
        PlatformGuard::Unix => cfg!(unix),
        PlatformGuard::Windows => cfg!(windows),
        PlatformGuard::Macos => cfg!(target_os = "macos"),
        PlatformGuard::Linux => cfg!(target_os = "linux"),
    }
}

fn guard_allows(guard: &Guard, script_envs: &std::collections::HashMap<String, String>) -> bool {
    match guard {
        Guard::Platform { target, invert } => {
            let res = platform_matches(*target);
            if *invert { !res } else { res }
        }
        Guard::EnvExists { key, invert } => {
            let res = script_envs
                .get(key)
                .cloned()
                .or_else(|| std::env::var(key).ok())
                .map(|v| !v.is_empty())
                .unwrap_or(false);
            if *invert { !res } else { res }
        }
        Guard::EnvEquals { key, value, invert } => {
            let res = script_envs
                .get(key)
                .cloned()
                .or_else(|| std::env::var(key).ok())
                .map(|v| v == *value)
                .unwrap_or(false);
            if *invert { !res } else { res }
        }
    }
}

fn guard_group_allows(
    group: &[Guard],
    script_envs: &std::collections::HashMap<String, String>,
) -> bool {
    group.iter().all(|g| guard_allows(g, script_envs))
}

fn guards_allow_any(
    groups: &[Vec<Guard>],
    script_envs: &std::collections::HashMap<String, String>,
) -> bool {
    // No guards means allow.
    if groups.is_empty() {
        return true;
    }
    groups.iter().any(|g| guard_group_allows(g, script_envs))
}

fn parse_guard(raw: &str, line_no: usize) -> Result<Guard> {
    let mut text = raw.trim();
    let mut invert_prefix = false;
    if let Some(rest) = text.strip_prefix('!') {
        invert_prefix = true;
        text = rest.trim();
    }

    if let Some(after) = text.strip_prefix("platform") {
        let after = after.trim_start();
        if let Some(rest) = after.strip_prefix(':').or_else(|| after.strip_prefix('=')) {
            let tag = rest.trim().to_ascii_lowercase();
            let target = match tag.as_str() {
                "unix" => PlatformGuard::Unix,
                "windows" => PlatformGuard::Windows,
                "mac" | "macos" => PlatformGuard::Macos,
                "linux" => PlatformGuard::Linux,
                _ => bail!("line {}: unknown platform '{}'", line_no, rest),
            };
            return Ok(Guard::Platform {
                target,
                invert: invert_prefix,
            });
        }
    }

    if let Some(rest) = text.strip_prefix("env:") {
        let rest = rest.trim();
        if let Some(pos) = rest.find("!=") {
            let key = rest[..pos].trim();
            let value = rest[pos + 2..].trim();
            if key.is_empty() || value.is_empty() {
                bail!("line {}: guard env: requires key and value", line_no);
            }
            return Ok(Guard::EnvEquals {
                key: key.to_string(),
                value: value.to_string(),
                invert: true,
            });
        }
        if let Some(pos) = rest.find('=') {
            let key = rest[..pos].trim();
            let value = rest[pos + 1..].trim();
            if key.is_empty() || value.is_empty() {
                bail!("line {}: guard env: requires key and value", line_no);
            }
            return Ok(Guard::EnvEquals {
                key: key.to_string(),
                value: value.to_string(),
                invert: invert_prefix,
            });
        }
        if rest.is_empty() {
            bail!("line {}: guard env: requires a variable name", line_no);
        }
        return Ok(Guard::EnvExists {
            key: rest.to_string(),
            invert: invert_prefix,
        });
    }

    let tag = text.to_ascii_lowercase();
    let target = match tag.as_str() {
        "unix" => PlatformGuard::Unix,
        "windows" => PlatformGuard::Windows,
        "mac" | "macos" => PlatformGuard::Macos,
        "linux" => PlatformGuard::Linux,
        _ => bail!("line {}: unknown guard '{}'", line_no, raw),
    };
    Ok(Guard::Platform {
        target,
        invert: invert_prefix,
    })
}

impl Command {
    pub const fn as_str(self) -> &'static str {
        match self {
            Command::Workdir => "WORKDIR",
            Command::Workspace => "WORKSPACE",
            Command::Env => "ENV",
            Command::Echo => "ECHO",
            Command::Run => "RUN",
            Command::RunBg => "RUN_BG",
            Command::Copy => "COPY",
            Command::Symlink => "SYMLINK",
            Command::Mkdir => "MKDIR",
            Command::Ls => "LS",
            Command::Cat => "CAT",
            Command::Write => "WRITE",
            Command::Exit => "EXIT",
        }
    }

    pub fn parse(op: &str) -> Option<Self> {
        COMMANDS.iter().copied().find(|c| c.as_str() == op)
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum PlatformGuard {
    Unix,
    Windows,
    Macos,
    Linux,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Guard {
    Platform {
        target: PlatformGuard,
        invert: bool,
    },
    EnvExists {
        key: String,
        invert: bool,
    },
    EnvEquals {
        key: String,
        value: String,
        invert: bool,
    },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum StepKind {
    Workdir(String),
    Workspace(WorkspaceTarget),
    Env { key: String, value: String },
    Run(String),
    Echo(String),
    RunBg(String),
    Copy { from: String, to: String },
    Symlink { from: String, to: String },
    Mkdir(String),
    Ls(Option<String>),
    Cat(String),
    Write { path: String, contents: String },
    Exit(i32),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Step {
    pub guards: Vec<Vec<Guard>>,
    pub kind: StepKind,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum WorkspaceTarget {
    Snapshot,
    Local,
}

fn split_dsl_commands(line: &str) -> Vec<String> {
    let mut commands = Vec::new();
    let mut start = 0;
    let mut i = 0;
    let bytes = line.as_bytes();

    while i < bytes.len() {
        if bytes[i] == b';' {
            let mut j = i + 1;
            while j < bytes.len() && bytes[j].is_ascii_whitespace() {
                j += 1;
            }
            let rest = &line[j..];
            if starts_with_command(rest) {
                let seg = line[start..i].trim();
                if !seg.is_empty() {
                    commands.push(seg.to_string());
                }
                start = j;
                i = j;
                continue;
            }
        }
        i += 1;
    }

    let tail = line[start..].trim();
    if !tail.is_empty() {
        commands.push(tail.to_string());
    }

    commands
}

fn starts_with_command(text: &str) -> bool {
    let Some(first) = text.split_whitespace().next() else {
        return false;
    };
    COMMANDS.iter().any(|c| c.as_str() == first)
}

pub fn parse_script(input: &str) -> Result<Vec<Step>> {
    let mut steps = Vec::new();
    let mut pending_guards: Vec<Vec<Guard>> = Vec::new();
    for (idx, line) in input.lines().enumerate() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }

        // If a line begins with a guard block, parse it. If the guard block
        // is followed by a command on the same line, apply guards (plus any
        // previously-pending guards) to that command. If the guard block is
        // on its own line (no command after `]`), stash the guards to apply
        // to the next non-empty command line.
        let (groups, remainder_opt) = if let Some(rest) = line.strip_prefix('[') {
            let end = rest
                .find(']')
                .ok_or_else(|| anyhow::anyhow!("line {}: guard must close with ]", idx + 1))?;
            let guards_raw = &rest[..end];
            let after = rest[end + 1..].trim();
            // Parse alternatives separated by `|`, each alternative is a comma-separated AND group.
            let mut parsed_groups: Vec<Vec<Guard>> = Vec::new();
            for alt in guards_raw.split('|') {
                let mut group: Vec<Guard> = Vec::new();
                for g in alt.split(',') {
                    let parsed = parse_guard(g, idx + 1)?;
                    group.push(parsed);
                }
                parsed_groups.push(group);
            }
            if after.is_empty() {
                // Guard-only line: combine with pending_guards by ANDing groups (cartesian product).
                if pending_guards.is_empty() {
                    pending_guards = parsed_groups;
                } else {
                    let mut new_pending: Vec<Vec<Guard>> = Vec::new();
                    for p in pending_guards.iter() {
                        for q in parsed_groups.iter() {
                            let mut merged = p.clone();
                            merged.extend(q.clone());
                            new_pending.push(merged);
                        }
                    }
                    pending_guards = new_pending;
                }
                continue;
            }
            (parsed_groups, Some(after))
        } else {
            (Vec::new(), Some(line))
        };

        // Combine any pending guard groups (from previous guard-only lines)
        // with any groups parsed on this line. Combination is an AND between
        // groups (cartesian product), producing OR-of-AND groups for the
        // resulting step. If there are no inline groups, use pending groups.
        let mut all_groups: Vec<Vec<Guard>> = Vec::new();
        if groups.is_empty() {
            all_groups = pending_guards.clone();
        } else if pending_guards.is_empty() {
            all_groups = groups.clone();
        } else {
            for p in pending_guards.iter() {
                for q in groups.iter() {
                    let mut merged = p.clone();
                    merged.extend(q.clone());
                    all_groups.push(merged);
                }
            }
        }
        pending_guards.clear();
        let remainder = remainder_opt.unwrap();

        // Allow multiple commands on one line separated by ';'. Each segment
        // gets the same guard set derived above. We only split on semicolons
        // that precede another DSL instruction (e.g., `; WORKDIR ...`). This
        // prevents breaking shell commands like `RUN echo a; echo b`, where the
        // semicolon is part of the shell command itself and should be kept.
        for cmd_text in split_dsl_commands(remainder).into_iter() {
            let mut parts = cmd_text.splitn(2, ' ');
            let op = parts.next().unwrap();
            let rest = parts.next().map(str::trim).unwrap_or("");
            let cmd = Command::parse(op)
                .ok_or_else(|| anyhow::anyhow!("line {}: unknown instruction '{}'", idx + 1, op))?;

            let kind = match cmd {
                Command::Workdir => {
                    if rest.is_empty() {
                        bail!("line {}: WORKDIR requires a path", idx + 1);
                    }
                    StepKind::Workdir(rest.to_string())
                }
                Command::Workspace => {
                    let target = match rest {
                        "SNAPSHOT" | "snapshot" => WorkspaceTarget::Snapshot,
                        "LOCAL" | "local" => WorkspaceTarget::Local,
                        _ => bail!("line {}: WORKSPACE requires LOCAL or SNAPSHOT", idx + 1),
                    };
                    StepKind::Workspace(target)
                }
                Command::Env => {
                    let mut parts = rest.splitn(2, '=');
                    let key = parts
                        .next()
                        .map(str::trim)
                        .filter(|s| !s.is_empty())
                        .ok_or_else(|| {
                            anyhow::anyhow!("line {}: ENV requires KEY=VALUE", idx + 1)
                        })?;
                    let value = parts.next().map(str::to_string).ok_or_else(|| {
                        anyhow::anyhow!("line {}: ENV requires KEY=VALUE", idx + 1)
                    })?;
                    StepKind::Env {
                        key: key.to_string(),
                        value,
                    }
                }
                Command::Echo => {
                    if rest.is_empty() {
                        bail!("line {}: ECHO requires a message", idx + 1);
                    }
                    StepKind::Echo(rest.to_string())
                }
                Command::Run => {
                    if rest.is_empty() {
                        bail!("line {}: RUN requires a command", idx + 1);
                    }
                    StepKind::Run(rest.to_string())
                }
                Command::RunBg => {
                    if rest.is_empty() {
                        bail!("line {}: RUN_BG requires a command", idx + 1);
                    }
                    StepKind::RunBg(rest.to_string())
                }
                Command::Copy => {
                    let mut p = rest.split_whitespace();
                    let from = p.next().ok_or_else(|| {
                        anyhow::anyhow!("line {}: COPY requires <from> <to>", idx + 1)
                    })?;
                    let to = p.next().ok_or_else(|| {
                        anyhow::anyhow!("line {}: COPY requires <from> <to>", idx + 1)
                    })?;
                    StepKind::Copy {
                        from: from.to_string(),
                        to: to.to_string(),
                    }
                }
                Command::Symlink => {
                    let mut p = rest.split_whitespace();
                    let from = p.next().ok_or_else(|| {
                        anyhow::anyhow!("line {}: SYMLINK requires <from> <to>", idx + 1)
                    })?;
                    let to = p.next().ok_or_else(|| {
                        anyhow::anyhow!("line {}: SYMLINK requires <from> <to>", idx + 1)
                    })?;
                    StepKind::Symlink {
                        from: from.to_string(),
                        to: to.to_string(),
                    }
                }
                Command::Mkdir => {
                    if rest.is_empty() {
                        bail!("line {}: MKDIR requires a path", idx + 1);
                    }
                    StepKind::Mkdir(rest.to_string())
                }
                Command::Ls => {
                    let path = rest
                        .split_whitespace()
                        .next()
                        .filter(|s| !s.is_empty())
                        .map(|s| s.to_string());
                    StepKind::Ls(path)
                }
                Command::Cat => {
                    if rest.is_empty() {
                        bail!("line {}: CAT requires a path", idx + 1);
                    }
                    StepKind::Cat(rest.to_string())
                }
                Command::Write => {
                    let mut p = rest.splitn(2, ' ');
                    let path = p.next().filter(|s| !s.is_empty()).ok_or_else(|| {
                        anyhow::anyhow!("line {}: WRITE requires <path> <contents>", idx + 1)
                    })?;
                    let contents = p.next().filter(|s| !s.is_empty()).ok_or_else(|| {
                        anyhow::anyhow!("line {}: WRITE requires <path> <contents>", idx + 1)
                    })?;
                    StepKind::Write {
                        path: path.to_string(),
                        contents: contents.to_string(),
                    }
                }
                Command::Exit => {
                    if rest.is_empty() {
                        bail!("line {}: EXIT requires a code", idx + 1);
                    }
                    let code: i32 = rest.parse().map_err(|_| {
                        anyhow::anyhow!("line {}: EXIT code must be an integer", idx + 1)
                    })?;
                    StepKind::Exit(code)
                }
            };

            steps.push(Step {
                guards: all_groups.clone(),
                kind,
            });
        }
    }
    Ok(steps)
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
    let mut root = fs_root.to_path_buf();
    let mut cwd = root.clone();
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
        if !guards_allow_any(&step.guards, &envs) {
            continue;
        }
        match &step.kind {
            StepKind::Workdir(path) => {
                cwd = resolve_workdir(&root, &cwd, path)
                    .with_context(|| format!("step {}: WORKDIR {}", idx + 1, path))?;
            }
            StepKind::Workspace(target) => match target {
                WorkspaceTarget::Snapshot => {
                    root = fs_root.to_path_buf();
                    cwd = root.clone();
                }
                WorkspaceTarget::Local => {
                    root = build_context.to_path_buf();
                    cwd = root.clone();
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
                // ECHO is a DSL primitive that writes a message to stdout in a
                // cross-platform way. Support simple variable interpolation so
                // `ECHO $FOO` or `ECHO {FOO}` prints values from script ENV or
                // the process environment.
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
                let from_abs = resolve_copy_source(build_context, from)
                    .with_context(|| format!("step {}: COPY {} {}", idx + 1, from, to))?;
                let to_abs = resolve_dest(&root, &cwd, to)
                    .with_context(|| format!("step {}: COPY {} {}", idx + 1, from, to))?;
                copy_entry(&from_abs, &to_abs)
                    .with_context(|| format!("step {}: COPY {} {}", idx + 1, from, to))?;
            }
            StepKind::Symlink { from, to } => {
                let to_abs = resolve_dest(&root, &cwd, to)
                    .with_context(|| format!("step {}: SYMLINK {} {}", idx + 1, from, to))?;
                if to_abs.exists() {
                    bail!("SYMLINK destination already exists: {}", to_abs.display());
                }
                // Resolve source from build context and ensure it is within build_context.
                let from_abs = resolve_copy_source(build_context, from)
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
                let target = resolve_dest(&root, &cwd, path)
                    .with_context(|| format!("step {}: MKDIR {}", idx + 1, path))?;
                fs::create_dir_all(&target)
                    .with_context(|| format!("failed to create dir {}", target.display()))?;
            }
            StepKind::Ls(path_opt) => {
                let dir = if let Some(p) = path_opt.as_deref() {
                    resolve_dest(&root, &cwd, p)
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
                let target = resolve_dest(&root, &cwd, path)
                    .with_context(|| format!("step {}: CAT {}", idx + 1, path))?;
                let data = fs::read(&target)
                    .with_context(|| format!("failed to read {}", target.display()))?;
                let mut out = io::stdout();
                out.write_all(&data)
                    .with_context(|| format!("failed to write {} to stdout", target.display()))?;
                out.flush().ok();
            }
            StepKind::Write { path, contents } => {
                let target = resolve_dest(&root, &cwd, path)
                    .with_context(|| format!("step {}: WRITE {}", idx + 1, path))?;
                if let Some(parent) = target.parent() {
                    fs::create_dir_all(parent)
                        .with_context(|| format!("failed to create parent {}", parent.display()))?;
                }
                fs::write(&target, contents)
                    .with_context(|| format!("failed to write {}", target.display()))?;
            }
            StepKind::Exit(code) => {
                // Tear down backgrounds before exiting.
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
        // Wait for the first background process to exit, then propagate its status and tear down the rest.
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

fn resolve_workdir(root: &Path, current: &Path, new_dir: &str) -> Result<PathBuf> {
    if new_dir == "/" {
        return Ok(root.to_path_buf());
    }
    let candidate = if Path::new(new_dir).is_absolute() {
        PathBuf::from(new_dir)
    } else {
        current.join(new_dir)
    };
    // Ensure the workdir is contained under `root` and create it if missing.
    // We canonicalize `root` and use a hybrid approach to resolve the candidate
    // even when it does not yet exist: find the nearest existing ancestor,
    // canonicalize that, apply remaining path components (resolving `.`/`..`),
    // then ensure the final absolute path is within `root` before creating it.
    let root_abs = fs::canonicalize(root)
        .with_context(|| format!("failed to canonicalize root {}", root.display()))?;

    // If the candidate exists, canonicalize and validate directly.
    if let Ok(cand_abs) = fs::canonicalize(&candidate) {
        if !cand_abs.starts_with(&root_abs) {
            bail!(
                "WORKDIR {} escapes allowed root {}",
                cand_abs.display(),
                root_abs.display()
            );
        }
        let meta = fs::metadata(&cand_abs)
            .with_context(|| format!("failed to stat resolved WORKDIR {}", cand_abs.display()))?;
        if meta.is_dir() {
            return Ok(cand_abs);
        }
        bail!("WORKDIR path is not a directory: {}", cand_abs.display());
    }

    // Candidate does not exist yet. Find the nearest existing ancestor.
    let mut ancestor = candidate.as_path();
    while !ancestor.exists() {
        if let Some(parent) = ancestor.parent() {
            ancestor = parent;
        } else {
            // No existing ancestor found; use root as the ancestor base.
            ancestor = root;
            break;
        }
    }

    let ancestor_abs = fs::canonicalize(ancestor)
        .with_context(|| format!("failed to canonicalize ancestor {}", ancestor.display()))?;

    // Compute the remaining components from ancestor to candidate.
    let mut rem_components: Vec<std::ffi::OsString> = Vec::new();
    {
        let mut skip = ancestor.components();
        let mut full = candidate.components();
        // Skip shared prefix components equal to ancestor
        loop {
            match (skip.next(), full.next()) {
                (Some(s), Some(f)) if s == f => continue,
                (_opt_s, opt_f) => {
                    if let Some(f) = opt_f {
                        rem_components.push(f.as_os_str().to_os_string());
                        for comp in full {
                            rem_components.push(comp.as_os_str().to_os_string());
                        }
                    }
                    break;
                }
            }
        }
    }

    // Apply remaining components resolving '.' and '..' without following symlinks.
    let mut cand_abs = ancestor_abs.clone();
    for c in rem_components.iter() {
        let s = std::ffi::OsStr::new(&c);
        if s == "." {
            continue;
        }
        if s == ".." {
            cand_abs = cand_abs
                .parent()
                .map(|p| p.to_path_buf())
                .unwrap_or(cand_abs);
            continue;
        }
        cand_abs.push(s);
    }

    // Ensure the resolved candidate would remain inside root.
    if !cand_abs.starts_with(&root_abs) {
        bail!(
            "WORKDIR {} escapes allowed root {}",
            cand_abs.display(),
            root_abs.display()
        );
    }

    // Create the directory and canonicalize the created path.
    fs::create_dir_all(&cand_abs)
        .with_context(|| format!("failed to create WORKDIR {}", cand_abs.display()))?;
    let final_abs = fs::canonicalize(&cand_abs).with_context(|| {
        format!(
            "failed to canonicalize created WORKDIR {}",
            cand_abs.display()
        )
    })?;
    Ok(final_abs)
}
fn resolve_dest(root: &Path, cwd: &Path, rel: &str) -> Result<PathBuf> {
    // Resolve a destination path relative to `cwd` and ensure it stays inside
    // `root`. If the path (or its ancestors) do not exist yet, we normalize
    // the path components and validate containment before returning the
    // non-canonical candidate (the caller may create it).
    let candidate = if Path::new(rel).is_absolute() {
        PathBuf::from(rel)
    } else {
        cwd.join(rel)
    };

    let root_abs = fs::canonicalize(root)
        .with_context(|| format!("failed to canonicalize root {}", root.display()))?;

    // If candidate exists, canonicalize and validate containment.
    if let Ok(cand_abs) = fs::canonicalize(&candidate) {
        if !cand_abs.starts_with(&root_abs) {
            bail!(
                "destination {} escapes allowed root {}",
                cand_abs.display(),
                root_abs.display()
            );
        }
        return Ok(cand_abs);
    }

    // Candidate missing: find nearest existing ancestor and apply remaining
    // components similar to `resolve_workdir`.
    let mut ancestor = candidate.as_path();
    while !ancestor.exists() {
        if let Some(parent) = ancestor.parent() {
            ancestor = parent;
        } else {
            ancestor = root;
            break;
        }
    }
    let ancestor_abs = fs::canonicalize(ancestor)
        .with_context(|| format!("failed to canonicalize ancestor {}", ancestor.display()))?;

    // Build remaining components
    let mut rem_components: Vec<std::ffi::OsString> = Vec::new();
    {
        let mut skip = ancestor.components();
        let mut full = candidate.components();
        loop {
            match (skip.next(), full.next()) {
                (Some(s), Some(f)) if s == f => continue,
                (_opt_s, opt_f) => {
                    if let Some(f) = opt_f {
                        rem_components.push(f.as_os_str().to_os_string());
                        for comp in full {
                            rem_components.push(comp.as_os_str().to_os_string());
                        }
                    }
                    break;
                }
            }
        }
    }

    let mut cand_abs = ancestor_abs.clone();
    for c in rem_components.iter() {
        let s = std::ffi::OsStr::new(&c);
        if s == "." {
            continue;
        }
        if s == ".." {
            cand_abs = cand_abs
                .parent()
                .map(|p| p.to_path_buf())
                .unwrap_or(cand_abs);
            continue;
        }
        cand_abs.push(s);
    }

    if !cand_abs.starts_with(&root_abs) {
        bail!(
            "destination {} escapes allowed root {}",
            cand_abs.display(),
            root_abs.display()
        );
    }

    Ok(cand_abs)
}

fn resolve_copy_source(build_context: &Path, from: &str) -> Result<PathBuf> {
    if Path::new(from).is_absolute() {
        bail!("COPY source must be relative to build context");
    }
    let candidate = build_context.join(from);
    if !candidate.exists() {
        bail!(
            "COPY source missing in build context: {}",
            candidate.display()
        );
    }

    // Canonicalize candidate and ensure it is contained within build_context.
    let build_abs = fs::canonicalize(build_context).with_context(|| {
        format!(
            "failed to canonicalize build context {}",
            build_context.display()
        )
    })?;
    let cand_abs = fs::canonicalize(&candidate)
        .with_context(|| format!("failed to canonicalize COPY source {}", candidate.display()))?;
    if !cand_abs.starts_with(&build_abs) {
        bail!(
            "COPY source {} is outside build context {}",
            cand_abs.display(),
            build_abs.display()
        );
    }
    Ok(cand_abs)
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

// No resolve_symlink_source: symlinks use the literal target string, which resolves at access time
// relative to the link's directory. On unsupported platforms, we fall back to copying.

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
            // Support $VAR and ${VAR}
            if let Some(&'{') = chars.peek() {
                // consume '{'
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
                // parse identifier
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
                    // literal '$' if no name
                    out.push('$');
                }
            }
        } else if c == '{' {
            // Support {VAR} as shorthand
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
