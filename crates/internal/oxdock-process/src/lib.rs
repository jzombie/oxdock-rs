#![allow(clippy::disallowed_types, clippy::disallowed_methods)]

mod shell;
#[cfg(feature = "mock-process")]
mod mock;

use anyhow::{Context, Result, bail};
use shell::shell_cmd;
pub use shell::{ShellLauncher, shell_program};
use std::collections::HashMap;
use std::fs::File;
use std::path::Path;
use std::process::{Child, Command as ProcessCommand, ExitStatus, Output as StdOutput, Stdio};
use std::{ffi::OsStr, iter::IntoIterator};

#[cfg(feature = "mock-process")]
pub use mock::{MockHandle, MockProcessManager, MockRunCall, MockSpawnCall};

/// Context passed to process managers describing the current execution
/// environment.
pub struct CommandContext<'a> {
    cwd: &'a Path,
    envs: &'a HashMap<String, String>,
    cargo_target_dir: &'a Path,
}

impl<'a> CommandContext<'a> {
    pub fn new(
        cwd: &'a Path,
        envs: &'a HashMap<String, String>,
        cargo_target_dir: &'a Path,
    ) -> Self {
        Self {
            cwd,
            envs,
            cargo_target_dir,
        }
    }

    pub fn cwd(&self) -> &'a Path {
        self.cwd
    }

    pub fn envs(&self) -> &'a HashMap<String, String> {
        self.envs
    }

    pub fn cargo_target_dir(&self) -> &'a Path {
        self.cargo_target_dir
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

    fn run(&mut self, ctx: &CommandContext<'_>, script: &str) -> Result<()>;
    fn run_capture(&mut self, ctx: &CommandContext<'_>, script: &str) -> Result<Vec<u8>>;
    fn spawn_bg(&mut self, ctx: &CommandContext<'_>, script: &str) -> Result<Self::Handle>;
}

/// Default process manager that shells out using the system shell.
#[derive(Clone, Default)]
pub struct ShellProcessManager;

impl ProcessManager for ShellProcessManager {
    type Handle = ChildHandle;

    fn run(&mut self, ctx: &CommandContext<'_>, script: &str) -> Result<()> {
        let mut command = shell_cmd(script);
        apply_ctx(&mut command, ctx);
        run_cmd(&mut command)
    }

    fn run_capture(&mut self, ctx: &CommandContext<'_>, script: &str) -> Result<Vec<u8>> {
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

    fn spawn_bg(&mut self, ctx: &CommandContext<'_>, script: &str) -> Result<Self::Handle> {
        let mut command = shell_cmd(script);
        apply_ctx(&mut command, ctx);
        let child = command
            .spawn()
            .with_context(|| format!("failed to spawn {:?}", command))?;
        Ok(ChildHandle { child })
    }
}

fn apply_ctx(command: &mut ProcessCommand, ctx: &CommandContext<'_>) {
    command.current_dir(ctx.cwd());
    command.envs(ctx.envs());
    command.env("CARGO_TARGET_DIR", ctx.cargo_target_dir());
}

#[derive(Debug)]
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
pub struct CommandBuilder {
    inner: ProcessCommand,
}

impl CommandBuilder {
    pub fn new(program: impl AsRef<OsStr>) -> Self {
        Self {
            inner: ProcessCommand::new(program),
        }
    }

    pub fn arg(&mut self, arg: impl AsRef<OsStr>) -> &mut Self {
        self.inner.arg(arg);
        self
    }

    pub fn args<S, I>(&mut self, args: I) -> &mut Self
    where
        S: AsRef<OsStr>,
        I: IntoIterator<Item = S>,
    {
        self.inner.args(args);
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
        self.inner.current_dir(dir);
        self
    }

    pub fn status(&mut self) -> Result<ExitStatus> {
        let desc = format!("{:?}", self.inner);
        let status = self
            .inner
            .status()
            .with_context(|| format!("failed to run {desc}"))?;
        Ok(status)
    }

    pub fn output(&mut self) -> Result<CommandOutput> {
        let desc = format!("{:?}", self.inner);
        let out = self
            .inner
            .output()
            .with_context(|| format!("failed to run {desc}"))?;
        Ok(CommandOutput::from(out))
    }

    pub fn spawn(&mut self) -> Result<ChildHandle> {
        let desc = format!("{:?}", self.inner);
        let child = self
            .inner
            .spawn()
            .with_context(|| format!("failed to spawn {desc}"))?;
        Ok(ChildHandle { child })
    }

    /// Return a debug-friendly representation of the command and its args.
    pub fn render(&self) -> String {
        format!("{:?}", self.inner)
    }
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

impl From<StdOutput> for CommandOutput {
    fn from(value: StdOutput) -> Self {
        Self {
            status: value.status,
            stdout: value.stdout,
            stderr: value.stderr,
        }
    }
}
