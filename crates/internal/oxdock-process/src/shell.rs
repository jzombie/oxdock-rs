use anyhow::{Context, Result, bail};
use std::ffi::OsStr;
use std::fs::File;
#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
use std::process::{Command, ExitStatus, Stdio};

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

#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
pub(crate) fn shell_cmd(cmd: &str) -> Command {
    let program = shell_program();
    let mut c = Command::new(program);
    if cfg!(windows) {
        c.arg("/C").arg(cmd);
    } else {
        c.arg("-c").arg(cmd);
    }
    c
}

#[derive(Default)]
pub struct ShellLauncher;

#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
impl ShellLauncher {
    pub fn run(&self, cmd: &mut Command) -> Result<()> {
        let status = cmd
            .status()
            .with_context(|| format!("failed to run {:?}", cmd))?;
        if !status.success() {
            bail!("command {:?} failed with status {}", cmd, status);
        }
        Ok(())
    }

    pub fn run_with_output(&self, cmd: &mut Command) -> Result<(ExitStatus, Vec<u8>, Vec<u8>)> {
        cmd.stdout(Stdio::piped()).stderr(Stdio::piped());
        let output = cmd
            .output()
            .with_context(|| format!("failed to run {:?}", cmd))?;
        Ok((output.status, output.stdout, output.stderr))
    }

    pub fn spawn(&self, cmd: &mut Command) -> Result<()> {
        cmd.spawn()
            .with_context(|| format!("failed to spawn {:?}", cmd))?;
        Ok(())
    }

    pub fn with_stdins<'a>(&self, cmd: &'a mut Command, stdin: Option<File>) -> &'a mut Command {
        if let Some(file) = stdin {
            cmd.stdin(file);
        }
        cmd
    }

    pub fn program_arg(&self, program: impl AsRef<OsStr>) -> Command {
        Command::new(program)
    }
}
