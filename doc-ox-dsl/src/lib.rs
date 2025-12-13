use anyhow::{Context, Result, bail};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command as ProcessCommand;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Command {
    Workdir,
    Run,
    Copy,
    Symlink,
    Mkdir,
    Ls,
    Write,
    Shell,
}

pub const COMMANDS: &[Command] = &[
    Command::Workdir,
    Command::Run,
    Command::Copy,
    Command::Symlink,
    Command::Mkdir,
    Command::Ls,
    Command::Write,
    Command::Shell,
];

fn platform_matches(target: PlatformGuard) -> bool {
    match target {
        PlatformGuard::Unix => cfg!(unix),
        PlatformGuard::Windows => cfg!(windows),
        PlatformGuard::Macos => cfg!(target_os = "macos"),
        PlatformGuard::Linux => cfg!(target_os = "linux"),
    }
}

fn guard_allows(guard: &Guard) -> bool {
    let allowed = match guard {
        Guard::Platform { target, invert } => {
            let res = platform_matches(*target);
            if *invert { !res } else { res }
        }
        Guard::EnvExists { key, invert } => {
            let res = std::env::var(key).map(|v| !v.is_empty()).unwrap_or(false);
            if *invert { !res } else { res }
        }
        Guard::EnvEquals { key, value, invert } => {
            let res = std::env::var(key).map(|v| v == *value).unwrap_or(false);
            if *invert { !res } else { res }
        }
    };
    allowed
}

fn guards_allow_all(guards: &[Guard]) -> bool {
    guards.iter().all(guard_allows)
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
            Command::Run => "RUN",
            Command::Copy => "COPY",
            Command::Symlink => "SYMLINK",
            Command::Mkdir => "MKDIR",
            Command::Ls => "LS",
            Command::Write => "WRITE",
            Command::Shell => "SHELL",
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
    Run(String),
    Copy { from: String, to: String },
    Symlink { link: String, target: String },
    Mkdir(String),
    Ls(Option<String>),
    Write { path: String, contents: String },
    Shell,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Step {
    pub guards: Vec<Guard>,
    pub kind: StepKind,
}

pub fn parse_script(input: &str) -> Result<Vec<Step>> {
    let mut steps = Vec::new();
    for (idx, line) in input.lines().enumerate() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }

        let (guards, remainder) = if let Some(rest) = line.strip_prefix('[') {
            let end = rest
                .find(']')
                .ok_or_else(|| anyhow::anyhow!("line {}: guard must close with ]", idx + 1))?;
            let guards_raw = &rest[..end];
            let after = rest[end + 1..].trim();
            if after.is_empty() {
                bail!("line {}: guard must precede a command", idx + 1);
            }
            let guards = guards_raw
                .split(',')
                .map(|g| parse_guard(g, idx + 1))
                .collect::<Result<Vec<_>>>()?;
            (guards, after)
        } else {
            (Vec::new(), line)
        };

        let mut parts = remainder.splitn(2, ' ');
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
            Command::Run => {
                if rest.is_empty() {
                    bail!("line {}: RUN requires a command", idx + 1);
                }
                StepKind::Run(rest.to_string())
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
                let link = p.next().ok_or_else(|| {
                    anyhow::anyhow!("line {}: SYMLINK requires <link> <target>", idx + 1)
                })?;
                let target = p.next().ok_or_else(|| {
                    anyhow::anyhow!("line {}: SYMLINK requires <link> <target>", idx + 1)
                })?;
                StepKind::Symlink {
                    link: link.to_string(),
                    target: target.to_string(),
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
            Command::Shell => StepKind::Shell,
        };

        steps.push(Step { guards, kind });
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
    let mut cwd = fs_root.to_path_buf();

    for (idx, step) in steps.iter().enumerate() {
        if !guards_allow_all(&step.guards) {
            continue;
        }
        match &step.kind {
            StepKind::Workdir(path) => {
                cwd = resolve_workdir(fs_root, &cwd, path)
                    .with_context(|| format!("step {}: WORKDIR {}", idx + 1, path))?;
            }
            StepKind::Run(cmd) => {
                let mut command = shell_cmd(cmd);
                command.current_dir(&cwd);
                // Prevent contention with the outer Cargo build by isolating nested cargo targets.
                command.env("CARGO_TARGET_DIR", &cargo_target_dir);
                run_cmd(&mut command).with_context(|| format!("step {}: RUN {}", idx + 1, cmd))?;
            }
            StepKind::Copy { from, to } => {
                let from_abs = resolve_copy_source(build_context, from)?;
                let to_abs = resolve_dest(&cwd, to);
                copy_entry(&from_abs, &to_abs)
                    .with_context(|| format!("step {}: COPY {} {}", idx + 1, from, to))?;
            }
            StepKind::Symlink { link, target } => {
                let link_abs = resolve_dest(&cwd, link);
                if link_abs.exists() {
                    bail!("SYMLINK link already exists: {}", link_abs.display());
                }
                let target_abs = if Path::new(target).is_absolute() {
                    PathBuf::from(target)
                } else {
                    let from_build = build_context.join(target);
                    if from_build.exists() {
                        from_build
                    } else {
                        build_context
                            .parent()
                            .map(|p| p.join(target))
                            .unwrap_or(from_build)
                    }
                };
                if target_abs == link_abs {
                    bail!(
                        "SYMLINK target resolves to the link itself: {}",
                        target_abs.display()
                    );
                }
                if !target_abs.exists() {
                    bail!("SYMLINK target missing: {}", target_abs.display());
                }
                #[cfg(unix)]
                std::os::unix::fs::symlink(&target_abs, &link_abs)
                    .with_context(|| format!("step {}: SYMLINK {} {}", idx + 1, link, target))?;
                #[cfg(all(windows, not(unix)))]
                std::os::windows::fs::symlink_dir(&target_abs, &link_abs)
                    .with_context(|| format!("step {}: SYMLINK {} {}", idx + 1, link, target))?;
                #[cfg(not(any(unix, windows)))]
                copy_dir(&resolve_dest(&cwd, target), &link_abs)?;
            }
            StepKind::Mkdir(path) => {
                let target = resolve_dest(&cwd, path);
                fs::create_dir_all(&target)
                    .with_context(|| format!("failed to create dir {}", target.display()))?;
            }
            StepKind::Ls(path_opt) => {
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
            StepKind::Write { path, contents } => {
                let target = resolve_dest(&cwd, path);
                if let Some(parent) = target.parent() {
                    fs::create_dir_all(parent)
                        .with_context(|| format!("failed to create parent {}", parent.display()))?;
                }
                fs::write(&target, contents)
                    .with_context(|| format!("failed to write {}", target.display()))?;
            }
            StepKind::Shell => {
                run_shell(&cwd)?;
            }
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
    if let Ok(meta) = fs::symlink_metadata(&candidate) {
        if meta.is_dir() {
            return Ok(candidate);
        }
        if meta.file_type().is_symlink() {
            let target = fs::read_link(&candidate)
                .with_context(|| format!("reading symlink target for {}", candidate.display()))?;
            let target_abs = if target.is_absolute() {
                target
            } else {
                candidate
                    .parent()
                    .map(|p| p.join(&target))
                    .unwrap_or(target)
            };
            if !target_abs.exists() {
                fs::create_dir_all(&target_abs)
                    .with_context(|| format!("creating symlink target {}", target_abs.display()))?;
            }
            return Ok(candidate);
        }
        bail!(
            "WORKDIR path is not a directory or symlink: {}",
            candidate.display()
        );
    }
    bail!("WORKDIR does not exist: {}", candidate.display());
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
        names.sort_by(|a, b| a.file_name().cmp(&b.file_name()));
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

fn shell_program() -> String {
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

fn run_shell(cwd: &Path) -> Result<()> {
    #[cfg(unix)]
    {
        let mut cmd = ProcessCommand::new(shell_program());
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

        let mut cmd = ProcessCommand::new(shell_program());
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

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    fn run_sets_cargo_target_dir_to_fs_root() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path();

        let steps = vec![Step {
            guards: Vec::new(),
            kind: StepKind::Run("printf %s \"$CARGO_TARGET_DIR\" > seen.txt".to_string()),
        }];

        run_steps(root, &steps).unwrap();

        let seen = std::fs::read_to_string(root.join("seen.txt")).unwrap();
        let expected = root.join(".cargo-target");
        assert_eq!(
            seen,
            expected.to_string_lossy(),
            "CARGO_TARGET_DIR should be scoped"
        );
    }

    #[test]
    fn guard_skips_when_env_missing() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path();

        let guard_var = "DOC_OX_GUARD_TEST_TOKEN_UNSET";
        let script = format!(
            indoc!(
                r#"
                [env:{guard}] WRITE skipped.txt hi
                WRITE kept.txt ok
                "#
            ),
            guard = guard_var
        );
        let steps = parse_script(&script).unwrap();

        run_steps(root, &steps).unwrap();

        assert!(
            !root.join("skipped.txt").exists(),
            "guarded WRITE should be skipped"
        );
        assert!(root.join("kept.txt").exists(), "unguarded WRITE should run");
    }

    #[test]
    fn guard_respects_platform_negation() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path();

        let script = indoc!(
            r#"
            [!unix] WRITE platform.txt hi
            WRITE always.txt ok
            "#
        );
        let steps = parse_script(script).unwrap();

        run_steps(root, &steps).unwrap();

        let expect_skipped = cfg!(unix);
        assert_eq!(
            root.join("platform.txt").exists(),
            !expect_skipped,
            "platform guard should skip on unix and run elsewhere"
        );
        assert!(
            root.join("always.txt").exists(),
            "unguarded WRITE should run"
        );
    }

    #[test]
    fn guard_matches_profile_env() {
        // Cargo sets PROFILE during builds/tests; verify guards see it.
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path();

        let profile = std::env::var("PROFILE").unwrap_or_else(|_| "debug".to_string());
        unsafe {
            std::env::set_var("PROFILE", &profile);
        }
        let script = format!(
            indoc!(
                r#"
                [env:PROFILE={0}] WRITE hit.txt yes
                [env:PROFILE!={0}] WRITE miss.txt no
                "#
            ),
            profile
        );

        let steps = parse_script(&script).unwrap();
        run_steps(root, &steps).unwrap();

        assert!(
            root.join("hit.txt").exists(),
            "PROFILE-matching guard should run"
        );
        assert!(
            !root.join("miss.txt").exists(),
            "PROFILE inequality guard should skip for current profile"
        );
    }

    #[test]
    fn multiple_guards_all_must_pass() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path();

        let key = "DOC_OX_MULTI_GUARD_TEST_PASS";
        unsafe {
            std::env::set_var(key, "ok");
        }

        let script = format!(
            indoc!(
                r#"
                [env:{k},env:{k}=ok] WRITE hit.txt yes
                WRITE always.txt ok
                "#
            ),
            k = key
        );
        let steps = parse_script(&script).unwrap();
        run_steps(root, &steps).unwrap();

        assert!(
            root.join("hit.txt").exists(),
            "guarded step should run when all guards pass"
        );
        assert!(
            root.join("always.txt").exists(),
            "unguarded step should run"
        );
    }

    #[test]
    fn multiple_guards_skip_when_one_fails() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path();

        let key = "DOC_OX_MULTI_GUARD_TEST_FAIL";
        unsafe {
            std::env::set_var(key, "ok");
        }

        let script = format!(
            indoc!(
                r#"
                [env:{k},env:{k}!=ok] WRITE miss.txt yes
                WRITE always.txt ok
                "#
            ),
            k = key
        );
        let steps = parse_script(&script).unwrap();
        run_steps(root, &steps).unwrap();

        assert!(
            !root.join("miss.txt").exists(),
            "guarded step should skip when any guard fails"
        );
        assert!(
            root.join("always.txt").exists(),
            "unguarded step should run"
        );
    }
}
