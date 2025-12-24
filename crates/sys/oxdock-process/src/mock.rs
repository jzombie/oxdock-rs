use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
#[allow(clippy::disallowed_types)]
use std::path::PathBuf;
use std::rc::Rc;

use anyhow::{anyhow, bail, Result};

use crate::{
    BackgroundHandle, CommandContext, CommandMode, CommandOptions, CommandResult, CommandStdout,
    ProcessManager, SharedInput,
};

/// Captured invocation for a foreground run.
#[derive(Clone, Debug, PartialEq, Eq)]
#[allow(clippy::disallowed_types)]
pub struct MockRunCall {
    pub script: String,
    pub cwd: PathBuf,
    pub envs: HashMap<String, String>,
    pub cargo_target_dir: PathBuf,
    pub stdin_provided: bool,
    pub stdin: Option<Vec<u8>>,
}

/// Captured invocation for a background spawn.
#[derive(Clone, Debug, PartialEq, Eq)]
#[allow(clippy::disallowed_types)]
pub struct MockSpawnCall {
    pub script: String,
    pub cwd: PathBuf,
    pub envs: HashMap<String, String>,
    pub cargo_target_dir: PathBuf,
    pub stdin_provided: bool,
    pub stdin: Option<Vec<u8>>,
}

#[derive(Clone, Default)]
pub struct MockProcessManager {
    runs: Rc<RefCell<Vec<MockRunCall>>>,
    spawns: Rc<RefCell<Vec<MockSpawnCall>>>,
    killed: Rc<RefCell<Vec<String>>>,
    plans: Rc<RefCell<VecDeque<BgPlan>>>,
}

impl MockProcessManager {
    pub fn recorded_runs(&self) -> Vec<MockRunCall> {
        self.runs.borrow().clone()
    }

    pub fn spawn_log(&self) -> Vec<MockSpawnCall> {
        self.spawns.borrow().clone()
    }

    pub fn killed(&self) -> Vec<String> {
        self.killed.borrow().clone()
    }

    pub fn push_bg_plan(&self, ready_after: usize, status: std::process::ExitStatus) {
        self.plans.borrow_mut().push_back(BgPlan {
            ready_after,
            status,
        });
    }
}

impl ProcessManager for MockProcessManager {
    type Handle = MockHandle;

    fn run_command(
        &mut self,
        ctx: &CommandContext,
        script: &str,
        options: CommandOptions,
    ) -> Result<CommandResult<Self::Handle>> {
        let CommandOptions {
            mode,
            stdin,
            stdout,
            stderr,
        } = options;
        let _ = stderr;
        let stdin_provided = stdin.is_some();
        let captured_stdin = capture_stdin(stdin)?;

        match mode {
            CommandMode::Foreground => {
                self.runs.borrow_mut().push(MockRunCall {
                    script: script.to_string(),
                    cwd: ctx.cwd().to_path_buf(),
                    envs: ctx.envs().clone(),
                    cargo_target_dir: ctx.cargo_target_dir().to_path_buf(),
                    stdin_provided,
                    stdin: captured_stdin.clone(),
                });
                match stdout {
                    CommandStdout::Capture => Ok(CommandResult::Captured(Vec::new())),
                    CommandStdout::Stream(_) | CommandStdout::Inherit => Ok(CommandResult::Completed),
                }
            }
            CommandMode::Background => {
                if matches!(stdout, CommandStdout::Capture) {
                    bail!("cannot capture stdout for background command");
                }
                self.spawns.borrow_mut().push(MockSpawnCall {
                    script: script.to_string(),
                    cwd: ctx.cwd().to_path_buf(),
                    envs: ctx.envs().clone(),
                    cargo_target_dir: ctx.cargo_target_dir().to_path_buf(),
                    stdin_provided,
                    stdin: captured_stdin.clone(),
                });
                let plan = self
                    .plans
                    .borrow_mut()
                    .pop_front()
                    .unwrap_or_else(BgPlan::success);
                Ok(CommandResult::Background(MockHandle {
                    script: script.to_string(),
                    remaining: plan.ready_after,
                    status: plan.status,
                    killed: self.killed.clone(),
                }))
            }
        }
    }
}

struct BgPlan {
    ready_after: usize,
    status: std::process::ExitStatus,
}

impl BgPlan {
    fn success() -> Self {
        Self {
            ready_after: 0,
            status: exit_status_from_code(0),
        }
    }
}

#[derive(Clone)]
pub struct MockHandle {
    script: String,
    remaining: usize,
    status: std::process::ExitStatus,
    killed: Rc<RefCell<Vec<String>>>,
}

impl BackgroundHandle for MockHandle {
    fn try_wait(&mut self) -> Result<Option<std::process::ExitStatus>> {
        if self.remaining == 0 {
            Ok(Some(self.status))
        } else {
            self.remaining -= 1;
            Ok(None)
        }
    }

    fn kill(&mut self) -> Result<()> {
        self.killed.borrow_mut().push(self.script.clone());
        Ok(())
    }

    fn wait(&mut self) -> Result<std::process::ExitStatus> {
        Ok(self.status)
    }
}

fn capture_stdin(stdin: Option<SharedInput>) -> Result<Option<Vec<u8>>> {
    if let Some(reader) = stdin {
        let mut guard = reader.lock().map_err(|_| anyhow!("failed to lock stdin"))?;
        let mut buf = Vec::new();
        std::io::copy(&mut *guard, &mut buf)?;
        Ok(Some(buf))
    } else {
        Ok(None)
    }
}

#[cfg(unix)]
fn exit_status_from_code(code: i32) -> std::process::ExitStatus {
    use std::os::unix::process::ExitStatusExt;
    ExitStatusExt::from_raw(code << 8)
}

#[cfg(windows)]
fn exit_status_from_code(code: i32) -> std::process::ExitStatus {
    use std::os::windows::process::ExitStatusExt;
    ExitStatusExt::from_raw(code as u32)
}
