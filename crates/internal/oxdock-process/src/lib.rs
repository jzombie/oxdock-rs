#[cfg(feature = "mock-process")]
mod mock;
mod shell;

use anyhow::{Context, Result, bail};
use oxdock_fs::{GuardedPath, PolicyPath};
use shell::shell_cmd;
pub use shell::{ShellLauncher, shell_program};
use std::collections::HashMap;
#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
use std::fs::File;
#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
use std::path::{Path, PathBuf};
#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
use std::process::{Child, Command as ProcessCommand, ExitStatus, Output as StdOutput, Stdio};
use std::{
    ffi::{OsStr, OsString},
    iter::IntoIterator,
};

#[cfg(miri)]
use oxdock_fs::PathResolver;

#[cfg(feature = "mock-process")]
pub use mock::{MockHandle, MockProcessManager, MockRunCall, MockSpawnCall};

/// Context passed to process managers describing the current execution
/// environment. Clones are cheap and explicit so background handles can own
/// their working roots without juggling lifetimes.
#[derive(Clone, Debug)]
pub struct CommandContext {
    cwd: PolicyPath,
    envs: HashMap<String, String>,
    cargo_target_dir: GuardedPath,
    workspace_root: GuardedPath,
    build_context: GuardedPath,
}

impl CommandContext {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        cwd: &PolicyPath,
        envs: &HashMap<String, String>,
        cargo_target_dir: &GuardedPath,
        workspace_root: &GuardedPath,
        build_context: &GuardedPath,
    ) -> Self {
        Self {
            cwd: cwd.clone(),
            envs: envs.clone(),
            cargo_target_dir: cargo_target_dir.clone(),
            workspace_root: workspace_root.clone(),
            build_context: build_context.clone(),
        }
    }

    pub fn cwd(&self) -> &PolicyPath {
        &self.cwd
    }

    pub fn envs(&self) -> &HashMap<String, String> {
        &self.envs
    }

    pub fn cargo_target_dir(&self) -> &GuardedPath {
        &self.cargo_target_dir
    }

    pub fn workspace_root(&self) -> &GuardedPath {
        &self.workspace_root
    }

    pub fn build_context(&self) -> &GuardedPath {
        &self.build_context
    }
}

/// Handle for background processes spawned by a [`ProcessManager`].
pub trait BackgroundHandle {
    fn try_wait(&mut self) -> Result<Option<ExitStatus>>;
    fn kill(&mut self) -> Result<()>;
    fn wait(&mut self) -> Result<ExitStatus>;
}

/// Abstraction for running shell commands both in the foreground and
/// background. `oxdock-core` relies on this trait to decouple the executor
/// from `std::process::Command`, which in turn enables Miri-friendly test
/// doubles.
pub trait ProcessManager: Clone {
    type Handle: BackgroundHandle;

    fn run(&mut self, ctx: &CommandContext, script: &str) -> Result<()>;
    fn run_capture(&mut self, ctx: &CommandContext, script: &str) -> Result<Vec<u8>>;
    fn spawn_bg(&mut self, ctx: &CommandContext, script: &str) -> Result<Self::Handle>;
}

/// Default process manager that shells out using the system shell.
#[derive(Clone, Default)]
#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
pub struct ShellProcessManager;

impl ProcessManager for ShellProcessManager {
    type Handle = ChildHandle;

    #[allow(clippy::disallowed_types, clippy::disallowed_methods)]
    fn run(&mut self, ctx: &CommandContext, script: &str) -> Result<()> {
        let mut command = shell_cmd(script);
        apply_ctx(&mut command, ctx);
        run_cmd(&mut command)
    }

    #[allow(clippy::disallowed_types, clippy::disallowed_methods)]
    fn run_capture(&mut self, ctx: &CommandContext, script: &str) -> Result<Vec<u8>> {
        let mut command = shell_cmd(script);
        apply_ctx(&mut command, ctx);
        command.stdout(Stdio::piped());
        let output = command
            .output()
            .with_context(|| format!("failed to run {:?}", command))?;
        if !output.status.success() {
            bail!("command {:?} failed with status {}", command, output.status);
        }
        Ok(output.stdout)
    }

    #[allow(clippy::disallowed_types, clippy::disallowed_methods)]
    fn spawn_bg(&mut self, ctx: &CommandContext, script: &str) -> Result<Self::Handle> {
        let mut command = shell_cmd(script);
        apply_ctx(&mut command, ctx);
        let child = command
            .spawn()
            .with_context(|| format!("failed to spawn {:?}", command))?;
        Ok(ChildHandle { child })
    }
}

#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
fn apply_ctx(command: &mut ProcessCommand, ctx: &CommandContext) {
    // Use command_path to strip Windows verbatim prefixes (\\?\) before passing to Command.
    // While Rust's std::process::Command handles verbatim paths in current_dir correctly,
    // environment variables are passed as-is. If we pass a verbatim path in CARGO_TARGET_DIR,
    // tools that don't understand it (or shell scripts echoing it) might misbehave or produce
    // unexpected output. Normalizing here ensures consistency.
    let cwd_path: std::borrow::Cow<std::path::Path> = match ctx.cwd() {
        PolicyPath::Guarded(p) => oxdock_fs::command_path(p),
        PolicyPath::Unguarded(p) => std::borrow::Cow::Borrowed(p.as_path()),
    };
    command.current_dir(cwd_path);
    command.envs(ctx.envs());
    command.env(
        "CARGO_TARGET_DIR",
        oxdock_fs::command_path(ctx.cargo_target_dir()).into_owned(),
    );
}

#[derive(Debug)]
#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
pub struct ChildHandle {
    child: Child,
}

impl BackgroundHandle for ChildHandle {
    fn try_wait(&mut self) -> Result<Option<ExitStatus>> {
        Ok(self.child.try_wait()?)
    }

    fn kill(&mut self) -> Result<()> {
        if self.child.try_wait()?.is_none() {
            let _ = self.child.kill();
        }
        Ok(())
    }

    fn wait(&mut self) -> Result<ExitStatus> {
        Ok(self.child.wait()?)
    }
}

// Synthetic process manager for Miri. Commands are interpreted with a tiny
// shell that supports the patterns exercised in tests: sleep, printf/echo with
// env interpolation, redirection, and exit codes. IO is routed through the
// workspace filesystem so we never touch the host.
#[cfg(miri)]
#[derive(Clone, Default)]
pub struct SyntheticProcessManager;

#[cfg(miri)]
#[derive(Clone)]
pub struct SyntheticBgHandle {
    ctx: CommandContext,
    actions: Vec<Action>,
    remaining: std::time::Duration,
    last_polled: std::time::Instant,
    status: ExitStatus,
    applied: bool,
    killed: bool,
}

#[cfg(miri)]
#[derive(Clone)]
enum Action {
    WriteFile { target: GuardedPath, data: Vec<u8> },
}

#[cfg(miri)]
impl BackgroundHandle for SyntheticBgHandle {
    fn try_wait(&mut self) -> Result<Option<ExitStatus>> {
        if self.killed {
            self.applied = true;
            return Ok(Some(self.status));
        }
        if self.applied {
            return Ok(Some(self.status));
        }
        let now = std::time::Instant::now();
        let elapsed = now.saturating_duration_since(self.last_polled);
        const MAX_ADVANCE: std::time::Duration = std::time::Duration::from_millis(15);
        let advance = elapsed.min(MAX_ADVANCE).min(self.remaining);
        self.remaining = self.remaining.saturating_sub(advance);
        self.last_polled = now;

        if self.remaining.is_zero() {
            apply_actions(&self.ctx, &self.actions)?;
            self.applied = true;
            Ok(Some(self.status))
        } else {
            Ok(None)
        }
    }

    fn kill(&mut self) -> Result<()> {
        self.killed = true;
        Ok(())
    }

    fn wait(&mut self) -> Result<ExitStatus> {
        if self.killed {
            self.applied = true;
            return Ok(self.status);
        }
        if !self.applied {
            if !self.remaining.is_zero() {
                std::thread::sleep(self.remaining);
            }
            apply_actions(&self.ctx, &self.actions)?;
            self.applied = true;
        }
        Ok(self.status)
    }
}

#[cfg(miri)]
impl ProcessManager for SyntheticProcessManager {
    type Handle = SyntheticBgHandle;

    fn run(&mut self, ctx: &CommandContext, script: &str) -> Result<()> {
        let (_out, status) = execute_sync(ctx, script, false)?;
        if !status.success() {
            bail!("command {:?} failed with status {}", script, status);
        }
        Ok(())
    }

    fn run_capture(&mut self, ctx: &CommandContext, script: &str) -> Result<Vec<u8>> {
        let (out, status) = execute_sync(ctx, script, true)?;
        if !status.success() {
            bail!("command {:?} failed with status {}", script, status);
        }
        Ok(out)
    }

    fn spawn_bg(&mut self, ctx: &CommandContext, script: &str) -> Result<Self::Handle> {
        let plan = plan_background(ctx, script)?;
        Ok(plan)
    }
}

#[cfg(miri)]
fn execute_sync(
    ctx: &CommandContext,
    script: &str,
    capture: bool,
) -> Result<(Vec<u8>, ExitStatus)> {
    let mut stdout = Vec::new();
    let mut status = exit_status_from_code(0);
    let resolver = PathResolver::new(
        ctx.workspace_root().as_path(),
        ctx.build_context().as_path(),
    )?;

    let script = normalize_shell(script);
    for raw in script.split(';') {
        let cmd = raw.trim();
        if cmd.is_empty() {
            continue;
        }
        let (action, sleep_dur, exit_code) = parse_command(cmd, ctx, &resolver, capture)?;
        if sleep_dur > std::time::Duration::ZERO {
            std::thread::sleep(sleep_dur);
        }
        if let Some(action) = action {
            match action {
                CommandAction::Write { target, data } => {
                    if let Some(parent) = target.as_path().parent() {
                        let parent_guard = GuardedPath::new(target.root(), parent)?;
                        resolver.create_dir_all(&parent_guard)?;
                    }
                    resolver.write_file(&target, &data)?;
                }
                CommandAction::Stdout { data } => {
                    stdout.extend_from_slice(&data);
                }
            }
        }
        if let Some(code) = exit_code {
            status = exit_status_from_code(code);
            break;
        }
    }

    Ok((stdout, status))
}

#[cfg(miri)]
fn plan_background(ctx: &CommandContext, script: &str) -> Result<SyntheticBgHandle> {
    let resolver = PathResolver::new(
        ctx.workspace_root().as_path(),
        ctx.build_context().as_path(),
    )?;
    let mut actions: Vec<Action> = Vec::new();
    let mut ready = std::time::Duration::ZERO;
    let mut status = exit_status_from_code(0);

    let script = normalize_shell(script);
    for raw in script.split(';') {
        let cmd = raw.trim();
        if cmd.is_empty() {
            continue;
        }
        let (action, sleep_dur, exit_code) = parse_command(cmd, ctx, &resolver, false)?;
        ready += sleep_dur;
        if let Some(CommandAction::Write { target, data }) = action {
            actions.push(Action::WriteFile { target, data });
        }
        if let Some(code) = exit_code {
            status = exit_status_from_code(code);
            break;
        }
    }

    let min_ready = std::time::Duration::from_millis(50);
    ready = ready.max(min_ready);

    let handle = SyntheticBgHandle {
        ctx: ctx.clone(),
        actions,
        remaining: ready,
        last_polled: std::time::Instant::now(),
        status,
        applied: false,
        killed: false,
    };
    Ok(handle)
}

#[cfg(miri)]
enum CommandAction {
    Write { target: GuardedPath, data: Vec<u8> },
    Stdout { data: Vec<u8> },
}

#[cfg(miri)]
fn parse_command(
    cmd: &str,
    ctx: &CommandContext,
    resolver: &PathResolver,
    capture: bool,
) -> Result<(Option<CommandAction>, std::time::Duration, Option<i32>)> {
    let (core, redirect) = split_redirect(cmd);
    let tokens: Vec<&str> = core.split_whitespace().collect();
    if tokens.is_empty() {
        return Ok((None, std::time::Duration::ZERO, None));
    }

    match tokens[0] {
        "sleep" => {
            let dur = tokens
                .get(1)
                .and_then(|s| s.parse::<f64>().ok())
                .unwrap_or(0.0);
            let duration = std::time::Duration::from_secs_f64(dur);
            Ok((None, duration, None))
        }
        "exit" => {
            let code = tokens
                .get(1)
                .and_then(|s| s.parse::<i32>().ok())
                .unwrap_or(0);
            Ok((None, std::time::Duration::ZERO, Some(code)))
        }
        "printf" => {
            let body = extract_body(&core, "printf %s");
            let expanded = expand_env(&body, ctx);
            let data = expanded.into_bytes();
            if let Some(path_str) = redirect {
                let target = resolve_write(resolver, ctx, &path_str)?;
                Ok((
                    Some(CommandAction::Write { target, data }),
                    std::time::Duration::ZERO,
                    None,
                ))
            } else if capture {
                Ok((
                    Some(CommandAction::Stdout { data }),
                    std::time::Duration::ZERO,
                    None,
                ))
            } else {
                Ok((None, std::time::Duration::ZERO, None))
            }
        }
        "echo" => {
            let body = core.strip_prefix("echo").unwrap_or("").trim();
            let expanded = expand_env(body, ctx);
            let mut data = expanded.into_bytes();
            data.push(b'\n');
            if let Some(path_str) = redirect {
                let target = resolve_write(resolver, ctx, &path_str)?;
                Ok((
                    Some(CommandAction::Write { target, data }),
                    std::time::Duration::ZERO,
                    None,
                ))
            } else if capture {
                Ok((
                    Some(CommandAction::Stdout { data }),
                    std::time::Duration::ZERO,
                    None,
                ))
            } else {
                Ok((None, std::time::Duration::ZERO, None))
            }
        }
        _ => {
            // Fallback: treat as no-op success so Miri tests can proceed.
            Ok((None, std::time::Duration::ZERO, None))
        }
    }
}

#[cfg(miri)]
fn resolve_write(resolver: &PathResolver, ctx: &CommandContext, path: &str) -> Result<GuardedPath> {
    match ctx.cwd() {
        PolicyPath::Guarded(p) => resolver.resolve_write(p, path),
        PolicyPath::Unguarded(_) => bail!("unguarded writes not supported in Miri"),
    }
}

#[cfg(miri)]
fn split_redirect(cmd: &str) -> (String, Option<String>) {
    if let Some(idx) = cmd.find('>') {
        let (left, right) = cmd.split_at(idx);
        let path = right.trim_start_matches('>').trim();
        (left.trim().to_string(), Some(path.to_string()))
    } else {
        (cmd.trim().to_string(), None)
    }
}

#[cfg(miri)]
fn extract_body(cmd: &str, prefix: &str) -> String {
    cmd.strip_prefix(prefix)
        .unwrap_or(cmd)
        .trim()
        .trim_matches('"')
        .to_string()
}

#[cfg(miri)]
fn expand_env(input: &str, ctx: &CommandContext) -> String {
    let mut out = String::new();
    let mut chars = input.chars().peekable();
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
                out.push_str(&env_lookup(&name, ctx));
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
                if name.is_empty() {
                    out.push('$');
                } else {
                    out.push_str(&env_lookup(&name, ctx));
                }
            }
        } else if c == '%' {
            // Windows-style %VAR%
            let mut name = String::new();
            while let Some(&ch) = chars.peek() {
                chars.next();
                if ch == '%' {
                    break;
                }
                name.push(ch);
            }
            if name.is_empty() {
                out.push('%');
            } else {
                out.push_str(&env_lookup(&name, ctx));
            }
        } else {
            out.push(c);
        }
    }
    out
}

#[cfg(miri)]
fn env_lookup(name: &str, ctx: &CommandContext) -> String {
    if name == "CARGO_TARGET_DIR" {
        return ctx.cargo_target_dir().display().to_string();
    }
    ctx.envs()
        .get(name)
        .cloned()
        .or_else(|| std::env::var(name).ok())
        .unwrap_or_default()
}

#[cfg(miri)]
fn normalize_shell(script: &str) -> String {
    let trimmed = script.trim();
    if let Some(rest) = trimmed.strip_prefix("sh -c ") {
        return rest.trim_matches(&['"', '\''] as &[_]).to_string();
    }
    if let Some(rest) = trimmed.strip_prefix("cmd /C ") {
        return rest.trim_matches(&['"', '\''] as &[_]).to_string();
    }
    trimmed.to_string()
}

#[cfg(miri)]
fn apply_actions(ctx: &CommandContext, actions: &[Action]) -> Result<()> {
    let resolver = PathResolver::new(
        ctx.workspace_root().as_path(),
        ctx.build_context().as_path(),
    )?;
    for action in actions {
        match action {
            Action::WriteFile { target, data } => {
                if let Some(parent) = target.as_path().parent() {
                    let parent_guard = GuardedPath::new(target.root(), parent)?;
                    resolver.create_dir_all(&parent_guard)?;
                }
                resolver.write_file(target, data)?;
            }
        }
    }
    Ok(())
}

#[cfg(miri)]
fn exit_status_from_code(code: i32) -> ExitStatus {
    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        ExitStatusExt::from_raw(code << 8)
    }
    #[cfg(windows)]
    {
        use std::os::windows::process::ExitStatusExt;
        ExitStatusExt::from_raw(code as u32)
    }
}

#[cfg(not(miri))]
pub type DefaultProcessManager = ShellProcessManager;

#[cfg(miri)]
pub type DefaultProcessManager = SyntheticProcessManager;

pub fn default_process_manager() -> DefaultProcessManager {
    DefaultProcessManager::default()
}

#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
fn run_cmd(cmd: &mut ProcessCommand) -> Result<()> {
    let status = cmd
        .status()
        .with_context(|| format!("failed to run {:?}", cmd))?;
    if !status.success() {
        bail!("command {:?} failed with status {}", cmd, status);
    }
    Ok(())
}

/// Builder wrapper that centralizes direct usages of `std::process::Command`.
#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
pub struct CommandBuilder {
    inner: ProcessCommand,
    program: OsString,
    args: Vec<OsString>,
    cwd: Option<PathBuf>,
}

#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
impl CommandBuilder {
    pub fn new(program: impl AsRef<OsStr>) -> Self {
        let prog = program.as_ref().to_os_string();
        Self {
            inner: ProcessCommand::new(&prog),
            program: prog,
            args: Vec::new(),
            cwd: None,
        }
    }

    pub fn arg(&mut self, arg: impl AsRef<OsStr>) -> &mut Self {
        let val = arg.as_ref().to_os_string();
        self.inner.arg(&val);
        self.args.push(val);
        self
    }

    pub fn args<S, I>(&mut self, args: I) -> &mut Self
    where
        S: AsRef<OsStr>,
        I: IntoIterator<Item = S>,
    {
        for arg in args {
            self.arg(arg);
        }
        self
    }

    pub fn env(&mut self, key: impl AsRef<OsStr>, value: impl AsRef<OsStr>) -> &mut Self {
        self.inner.env(key, value);
        self
    }

    pub fn stdin_file(&mut self, file: File) -> &mut Self {
        self.inner.stdin(Stdio::from(file));
        self
    }

    pub fn current_dir(&mut self, dir: impl AsRef<Path>) -> &mut Self {
        let path = dir.as_ref();
        self.inner.current_dir(path);
        self.cwd = Some(path.to_path_buf());
        self
    }

    pub fn status(&mut self) -> Result<ExitStatus> {
        #[cfg(miri)]
        {
            let snap = self.snapshot();
            synthetic_status(&snap)
        }

        #[cfg(not(miri))]
        {
            let desc = format!("{:?}", self.inner);
            let status = self
                .inner
                .status()
                .with_context(|| format!("failed to run {desc}"))?;
            Ok(status)
        }
    }

    pub fn output(&mut self) -> Result<CommandOutput> {
        #[cfg(miri)]
        {
            let snap = self.snapshot();
            synthetic_output(&snap)
        }

        #[cfg(not(miri))]
        {
            let desc = format!("{:?}", self.inner);
            let out = self
                .inner
                .output()
                .with_context(|| format!("failed to run {desc}"))?;
            Ok(CommandOutput::from(out))
        }
    }

    pub fn spawn(&mut self) -> Result<ChildHandle> {
        #[cfg(miri)]
        {
            bail!("spawn is not supported under miri synthetic process backend")
        }

        #[cfg(not(miri))]
        {
            let desc = format!("{:?}", self.inner);
            let child = self
                .inner
                .spawn()
                .with_context(|| format!("failed to spawn {desc}"))?;
            Ok(ChildHandle { child })
        }
    }

    /// Return a lightweight snapshot of the command configuration for testing.
    pub fn snapshot(&self) -> CommandSnapshot {
        CommandSnapshot {
            program: self.program.clone(),
            args: self.args.clone(),
            cwd: self.cwd.clone(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
pub struct CommandSnapshot {
    pub program: OsString,
    pub args: Vec<OsString>,
    pub cwd: Option<PathBuf>,
}

pub struct CommandOutput {
    pub status: ExitStatus,
    pub stdout: Vec<u8>,
    pub stderr: Vec<u8>,
}

impl CommandOutput {
    pub fn success(&self) -> bool {
        self.status.success()
    }
}

#[allow(clippy::disallowed_types)]
impl From<StdOutput> for CommandOutput {
    fn from(value: StdOutput) -> Self {
        Self {
            status: value.status,
            stdout: value.stdout,
            stderr: value.stderr,
        }
    }
}

#[cfg(miri)]
fn synthetic_status(snapshot: &CommandSnapshot) -> Result<ExitStatus> {
    Ok(synthetic_output(snapshot)?.status)
}

#[cfg(miri)]
fn synthetic_output(snapshot: &CommandSnapshot) -> Result<CommandOutput> {
    let program = snapshot.program.to_string_lossy().to_string();
    let args: Vec<String> = snapshot
        .args
        .iter()
        .map(|a| a.to_string_lossy().to_string())
        .collect();

    if program == "git" {
        return simulate_git(&args);
    }
    if program == "cargo" {
        return simulate_cargo(&args);
    }

    Ok(CommandOutput {
        status: exit_status_from_code(0),
        stdout: Vec::new(),
        stderr: Vec::new(),
    })
}

#[cfg(miri)]
fn simulate_git(args: &[String]) -> Result<CommandOutput> {
    let mut iter = args.iter();
    if matches!(iter.next(), Some(arg) if arg == "-C") {
        let _ = iter.next();
    }
    let remaining: Vec<String> = iter.map(|s| s.to_string()).collect();

    if remaining.len() >= 2 && remaining[0] == "rev-parse" && remaining[1] == "HEAD" {
        return Ok(CommandOutput {
            status: exit_status_from_code(0),
            stdout: b"HEAD\n".to_vec(),
            stderr: Vec::new(),
        });
    }

    // Default success for init/add/commit and other read-only queries.
    Ok(CommandOutput {
        status: exit_status_from_code(0),
        stdout: Vec::new(),
        stderr: Vec::new(),
    })
}

#[cfg(miri)]
fn simulate_cargo(args: &[String]) -> Result<CommandOutput> {
    // Heuristic: manifests containing "build_exit_fail" should fail to mimic fixture.
    let mut status = exit_status_from_code(0);
    if args.iter().any(|a| a.contains("build_exit_fail")) {
        status = exit_status_from_code(1);
    }
    Ok(CommandOutput {
        status,
        stdout: Vec::new(),
        stderr: Vec::new(),
    })
}
