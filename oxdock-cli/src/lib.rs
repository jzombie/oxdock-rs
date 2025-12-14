use anyhow::{Context, Result, bail};
use std::env;
use std::fs;
use std::io::{self, IsTerminal, Read};
use std::path::{Path, PathBuf};
use std::process::Command;

pub use oxdock_core::{Guard, Step, StepKind, parse_script, run_steps, run_steps_with_context, shell_program};

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
    #[cfg(windows)]
    maybe_reexec_shell_to_temp(&opts)?;

    // Prefer the runtime env var when present (e.g., under `cargo run`), but fall back to the
    // compile-time value so the binary can be invoked directly without CARGO_MANIFEST_DIR set.
    let workspace_root = discover_workspace_root()?;

    let temp = tempfile::tempdir().context("failed to create temp dir")?;
    // Keep the workspace alive for interactive shells; otherwise the tempdir cleans up on drop.
    let temp_path = if opts.shell {
        temp.keep()
    } else {
        temp.path().to_path_buf()
    };

    // Materialize source tree without .git
    archive_head(&workspace_root, &temp_path)?;

    // Interpret a tiny Dockerfile-ish script
    let script = match &opts.script {
        ScriptSource::Path(path) => fs::read_to_string(path)
            .with_context(|| format!("failed to read script at {}", path.display()))?,
        ScriptSource::Stdin => {
            let stdin = io::stdin();
            if stdin.is_terminal() {
                // No piped script provided. If the caller requested `--shell`,
                // allow running with an empty script and drop into the interactive
                // shell afterwards. Otherwise, require a script on stdin.
                if opts.shell {
                    String::new()
                } else {
                    bail!(
                        "no stdin detected; pass --script <file> or pipe a script into stdin (use --script - if explicit)"
                    );
                }
            } else {
                let mut buf = String::new();
                stdin
                    .lock()
                    .read_to_string(&mut buf)
                    .context("failed to read script from stdin")?;
                buf
            }
        }
    };
    // Parse and run steps if we have a non-empty script. Empty scripts are
    // valid when `--shell` is requested and the caller didn't pipe a script.
    if !script.trim().is_empty() {
        let steps = parse_script(&script)?;
        // Use the caller's workspace as the build context so WORKSPACE LOCAL can hop back and so COPY
        // can source from the original tree if needed.
        run_steps_with_context(&temp_path, &workspace_root, &steps)?;
    }

    // If requested, drop into an interactive shell after running the script.
    if opts.shell {
        if !has_controlling_tty() {
            bail!("--shell requires a tty (no controlling tty available)");
        }
        return run_shell(&temp_path, &workspace_root);
    }

    Ok(())
}

fn has_controlling_tty() -> bool {
    #[cfg(unix)]
    {
        std::fs::File::open("/dev/tty").is_ok()
    }

    #[cfg(windows)]
    {
        std::fs::File::open("CONIN$").is_ok()
    }

    #[cfg(not(any(unix, windows)))]
    {
        false
    }
}

#[cfg(windows)]
fn maybe_reexec_shell_to_temp(opts: &Options) -> Result<()> {
    // Only used for interactive shells. Copy the binary to a temp path and run it there so the
    // original target exe is free for rebuilding while the shell stays open.
    if !opts.shell {
        return Ok(());
    }
    if std::env::var("OXDOCK_SHELL_REEXEC").ok().as_deref() == Some("1") {
        return Ok(());
    }

    let self_path = std::env::current_exe().context("determine current executable")?;
    let mut temp_path = std::env::temp_dir();
    let ts = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis();
    temp_path.push(format!("oxdock-shell-{ts}-{}.exe", std::process::id()));

    fs::copy(&self_path, &temp_path)
        .with_context(|| format!("failed to copy shell runner to {}", temp_path.display()))?;

    let mut cmd = Command::new(&temp_path);
    cmd.args(std::env::args_os().skip(1));
    cmd.env("OXDOCK_SHELL_REEXEC", "1");

    cmd.spawn()
        .with_context(|| format!("failed to spawn shell from {}", temp_path.display()))?;

    // Exit immediately so the original binary can be rebuilt while the shell child stays running.
    std::process::exit(0);
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

fn shell_banner(cwd: &Path, workspace_root: &Path) -> String {
    let pkg = env::var("CARGO_PKG_NAME").unwrap_or_else(|_| "oxdock".to_string());
    format!(
        "{} shell workspace: {} (materialized from git HEAD at {})",
        pkg,
        cwd.display(),
        workspace_root.display()
    )
}

#[cfg(windows)]
fn escape_for_cmd(s: &str) -> String {
    // Escape characters that would otherwise be interpreted by cmd when echoed.
    s.replace('^', "^^")
        .replace('&', "^&")
        .replace('|', "^|")
        .replace('>', "^>")
        .replace('<', "^<")
}

fn run_shell(cwd: &Path, workspace_root: &Path) -> Result<()> {
    let banner = shell_banner(cwd, workspace_root);

    #[cfg(unix)]
    {
        let mut cmd = Command::new(shell_program());
        cmd.current_dir(cwd);

        // Print a single banner inside the subshell, then exec the user's shell to stay interactive.
        let script = format!("printf '%s\\n' \"{}\"; exec {}", banner, shell_program());
        cmd.arg("-c").arg(script);

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
        // Launch via `start` so Windows opens a real interactive console window rooted at the temp
        // workspace. Using `start` avoids stdin/handle inheritance issues that can make the child
        // non-interactive when CREATE_NEW_CONSOLE is set.
        let mut cmd = Command::new("cmd");
        cmd.arg("/C")
            .arg("start")
            .arg("oxdock shell")
            .arg("/D")
            .arg(cwd)
            .arg("cmd")
            .arg("/K")
            .arg(format!("echo {} && cd /d .", escape_for_cmd(&banner)));

        // Fire-and-forget so the parent console regains control immediately; the child window is
        // fully interactive. If the launch fails, surface the error right away.
        cmd.spawn()
            .context("failed to start interactive shell window")?;
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
