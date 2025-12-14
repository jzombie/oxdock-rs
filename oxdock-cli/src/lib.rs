use anyhow::{Context, Result, bail};
use std::fs;
use std::io::{self, IsTerminal, Read};
use std::path::{Path, PathBuf};
use std::process::Command;

pub use oxdock_core::{Guard, Step, StepKind, parse_script, run_steps, run_steps_with_context};

pub fn run() -> Result<()> {
    let mut args = std::env::args().skip(1);
    let opts = Options::parse(&mut args)?;
    execute(opts)
}

#[derive(Debug, Clone)]
pub enum ScriptSource {
    Path(PathBuf),
    Stdin,
}

#[derive(Debug, Clone)]
pub struct Options {
    pub script: ScriptSource,
    pub shell: bool,
}

impl Options {
    pub fn parse(args: &mut impl Iterator<Item = String>) -> Result<Self> {
        let mut script: Option<ScriptSource> = None;
        let mut shell = false;
        while let Some(arg) = args.next() {
            if arg.is_empty() {
                continue;
            }
            match arg.as_str() {
                "--script" => {
                    let p = args
                        .next()
                        .ok_or_else(|| anyhow::anyhow!("--script requires a path"))?;
                    if p == "-" {
                        script = Some(ScriptSource::Stdin);
                    } else {
                        script = Some(ScriptSource::Path(PathBuf::from(p)));
                    }
                }
                "--shell" => {
                    shell = true;
                }
                other => bail!("unexpected flag: {}", other),
            }
        }

        let script = script.unwrap_or(ScriptSource::Stdin);

        Ok(Self { script, shell })
    }
}

pub fn execute(opts: Options) -> Result<()> {
    // Prefer the runtime env var when present (e.g., under `cargo run`), but fall back to the
    // compile-time value so the binary can be invoked directly without CARGO_MANIFEST_DIR set.
    let workspace_root = discover_workspace_root()?;

    let temp = tempfile::tempdir().context("failed to create temp dir")?;
    let temp_path = temp.path().to_path_buf();

    // Materialize source tree without .git
    archive_head(&workspace_root, &temp_path)?;

    // Drop into an interactive shell if requested.
    if opts.shell {
        let stdin = io::stdin();
        if !stdin.is_terminal() {
            bail!("--shell requires a tty (stdin is piped)");
        }
        return run_shell(&temp_path);
    }

    // Interpret a tiny Dockerfile-ish script
    let script = match &opts.script {
        ScriptSource::Path(path) => fs::read_to_string(path)
            .with_context(|| format!("failed to read script at {}", path.display()))?,
        ScriptSource::Stdin => {
            let stdin = io::stdin();
            if stdin.is_terminal() {
                bail!(
                    "no stdin detected; pass --script <file> or pipe a script into stdin (use --script - if explicit)"
                );
            }
            let mut buf = String::new();
            stdin
                .lock()
                .read_to_string(&mut buf)
                .context("failed to read script from stdin")?;
            buf
        }
    };
    let steps = parse_script(&script)?;
    // Use the caller's workspace as the build context so WORKSPACE LOCAL can hop back and so COPY
    // can source from the original tree if needed.
    run_steps_with_context(&temp_path, &workspace_root, &steps)?;

    Ok(())
}

fn discover_workspace_root() -> Result<PathBuf> {
    if let Ok(root) = std::env::var("OXDOCK_WORKSPACE_ROOT") {
        return Ok(PathBuf::from(root));
    }

    if let Ok(manifest_dir) = std::env::var("CARGO_MANIFEST_DIR")
        && let Some(parent) = PathBuf::from(manifest_dir).parent()
    {
        return Ok(parent.to_path_buf());
    }

    // Prefer the git repository root of the current working directory.
    if let Ok(output) = Command::new("git")
        .arg("rev-parse")
        .arg("--show-toplevel")
        .output()
        && output.status.success()
    {
        let path = String::from_utf8_lossy(&output.stdout).trim().to_string();
        if !path.is_empty() {
            return Ok(PathBuf::from(path));
        }
    }

    std::env::current_dir().context("failed to determine current directory for workspace root")
}
pub fn run_script(workspace_root: &Path, steps: &[Step]) -> Result<()> {
    run_steps_with_context(workspace_root, workspace_root, steps)
}

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

fn run_shell(cwd: &Path) -> Result<()> {
    #[cfg(unix)]
    {
        let mut cmd = Command::new(shell_program());
        cmd.current_dir(cwd);

        // Reattach stdin to the controlling TTY so a piped-in script can still open an interactive shell.
        if let Ok(tty) = fs::File::open("/dev/tty") {
            cmd.stdin(tty);
        }

        let status = cmd.status()?;
        if !status.success() {
            bail!("shell exited with status {}", status);
        }
        Ok(())
    }

    #[cfg(windows)]
    {
        use std::os::windows::process::CommandExt;

        let mut cmd = Command::new(shell_program());
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
        Ok(())
    }

    #[cfg(not(any(unix, windows)))]
    {
        let _ = cwd;
        bail!("interactive shell unsupported on this platform");
    }
}
fn run_cmd(cmd: &mut Command) -> Result<()> {
    let status = cmd
        .status()
        .with_context(|| format!("failed to run {:?}", cmd))?;
    if !status.success() {
        bail!("command {:?} failed with status {}", cmd, status);
    }
    Ok(())
}

fn archive_head(workspace_root: &Path, temp_root: &Path) -> Result<()> {
    let archive_path = temp_root.join("src.tar");
    let archive_str = archive_path.to_string_lossy().to_string();
    run_cmd(Command::new("git").current_dir(workspace_root).args([
        "archive",
        "--format=tar",
        "--output",
        &archive_str,
        "HEAD",
    ]))?;

    run_cmd(
        Command::new("tar")
            .arg("-xf")
            .arg(&archive_str)
            .arg("-C")
            .arg(temp_root),
    )?;

    // Drop the intermediate archive to keep the temp workspace clean.
    fs::remove_file(&archive_path)
        .with_context(|| format!("failed to remove {}", archive_path.display()))?;
    Ok(())
}
