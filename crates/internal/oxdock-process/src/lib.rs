use anyhow::{Context, Result, bail};
use std::collections::HashMap;
use std::path::Path;
use std::process::{Child, Command as ProcessCommand, ExitStatus, Stdio};

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

/// Return the system shell program.
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
