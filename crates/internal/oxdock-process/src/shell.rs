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
    #[allow(clippy::disallowed_macros)]
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

#[cfg(test)]
mod tests {
    use super::{ShellLauncher, shell_cmd, shell_program};
    use std::env;
    use std::ffi::OsStr;
    use std::sync::Mutex;

    // For Windows, fixing COMSPEC override test race condition
    static ENV_LOCK: Mutex<()> = Mutex::new(());

    struct EnvGuard {
        key: &'static str,
        value: Option<String>,
    }

    impl EnvGuard {
        fn set(key: &'static str, value: &str) -> Self {
            let prev = env::var(key).ok();
            unsafe {
                env::set_var(key, value);
            }
            Self { key, value: prev }
        }
    }

    impl Drop for EnvGuard {
        fn drop(&mut self) {
            match &self.value {
                Some(value) => unsafe {
                    env::set_var(self.key, value);
                },
                None => unsafe {
                    env::remove_var(self.key);
                },
            }
        }
    }

    #[test]
    fn shell_program_prefers_env_override() {
        let _lock = ENV_LOCK.lock().expect("env lock");
        #[cfg(windows)]
        let _guard = EnvGuard::set("COMSPEC", "custom-cmd");
        #[cfg(not(windows))]
        let _guard = EnvGuard::set("SHELL", "custom-sh");
        let program = shell_program();
        #[cfg(windows)]
        assert_eq!(program, "custom-cmd");
        #[cfg(not(windows))]
        assert_eq!(program, "custom-sh");
    }

    #[cfg_attr(
        miri,
        ignore = "spawns shell command; Miri does not support process execution"
    )]
    #[test]
    fn shell_launcher_run_with_output_captures_stdout() {
        let _lock = ENV_LOCK.lock().expect("env lock");
        let launcher = ShellLauncher;
        let mut cmd = shell_cmd("echo hello");
        let (status, stdout, _stderr) = launcher.run_with_output(&mut cmd).expect("run output");
        assert!(status.success());
        let out = String::from_utf8_lossy(&stdout);
        assert!(out.contains("hello"));
    }

    #[test]
    fn shell_launcher_program_arg_tracks_program() {
        let launcher = ShellLauncher;
        let cmd = launcher.program_arg("echo");
        assert_eq!(cmd.get_program(), OsStr::new("echo"));
    }
}
