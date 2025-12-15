use anyhow::{Context, Result, bail};
use oxdock_fs::{GuardedPath, PathResolver};
use std::env;
use std::io::{self, IsTerminal, Read};
use std::process::{Command, Stdio};

pub use oxdock_core::{
    Guard, Step, StepKind, parse_script, run_steps, run_steps_with_context, shell_program,
};

pub fn run() -> Result<()> {
    let workspace_root = GuardedPath::new_root_from_str(&discover_workspace_root()?)
        .context("guard workspace root")?;

    let mut args = std::env::args().skip(1);
    let opts = Options::parse(&mut args, &workspace_root)?;
    execute(opts, workspace_root)
}

#[derive(Debug, Clone)]
pub enum ScriptSource {
    Path(GuardedPath),
    Stdin,
}

#[derive(Debug, Clone)]
pub struct Options {
    pub script: ScriptSource,
    pub shell: bool,
    pub inline: bool,
}

impl Options {
    pub fn parse(
        args: &mut impl Iterator<Item = String>,
        workspace_root: &GuardedPath,
    ) -> Result<Self> {
        let mut script: Option<ScriptSource> = None;
        let mut shell = false;
        let mut inline = false;
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
                        script = Some(ScriptSource::Path(
                            workspace_root
                                .join(&p)
                                .with_context(|| format!("guard script path {p}"))?,
                        ));
                    }
                }
                "--shell" => {
                    shell = true;
                }
                "--inline" => {
                    // Read script interactively from the terminal (type the script,
                    // finish with a single '.' on a line or Ctrl-D).
                    inline = true;
                }
                other => bail!("unexpected flag: {}", other),
            }
        }

        let script = script.unwrap_or(ScriptSource::Stdin);

        Ok(Self {
            script,
            shell,
            inline,
        })
    }
}

pub fn execute(opts: Options, workspace_root: GuardedPath) -> Result<()> {
    #[cfg(windows)]
    maybe_reexec_shell_to_temp(&opts)?;

    let tempdir = GuardedPath::tempdir().context("failed to create temp dir")?;
    let temp_root = tempdir.as_guarded_path().clone();

    // Materialize source tree without .git
    archive_head(&workspace_root, &temp_root)?;

    // Interpret a tiny Dockerfile-ish script
    let script = match &opts.script {
        ScriptSource::Path(path) => {
            // Read script path via PathResolver rooted at the workspace so
            // script files are validated to live under the workspace.
            let resolver = PathResolver::new(workspace_root.as_path(), workspace_root.as_path())?;
            resolver
                .read_to_string(path)
                .with_context(|| format!("failed to read script at {}", path.display()))?
        }
        ScriptSource::Stdin => {
            let stdin = io::stdin();
            if stdin.is_terminal() {
                // No piped script provided. If the caller requested `--shell` or
                // `--inline`, allow running with an initially-empty script so we
                // can either drop into the interactive shell or open the editor
                // later. Otherwise, require a script on stdin.
                if opts.shell || opts.inline {
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

    // If the caller requested inline editing, open the user's default editor
    // on a temporary file and read the contents as the script. The editor is
    // chosen from `$VISUAL`, then `$EDITOR`. On Unix fall back to `nano` or
    // `vi`; on Windows fall back to `notepad`.
    let mut script = script;
    if opts.inline {
        script = open_editor_and_read().context("failed to read inline script from editor")?;
    }
    // Parse and run steps if we have a non-empty script. Empty scripts are
    // valid when `--shell` is requested and the caller didn't pipe a script.
    if !script.trim().is_empty() {
        let steps = parse_script(&script)?;
        // Use the caller's workspace as the build context so WORKSPACE LOCAL can hop back and so COPY
        // can source from the original tree if needed.
        run_steps_with_context(&temp_root, &workspace_root, &steps)?;
    }

    // If requested, drop into an interactive shell after running the script.
    if opts.shell {
        if !has_controlling_tty() {
            bail!("--shell requires a tty (no controlling tty available)");
        }
        return run_shell(&temp_root, &workspace_root);
    }

    Ok(())
}

fn has_controlling_tty() -> bool {
    // Prefer checking whether stdin or stderr is a terminal. This avoids
    // directly opening device files via `std::fs` while still detecting
    // whether an interactive tty is available in the common cases.
    #[cfg(unix)]
    {
        io::stdin().is_terminal() || io::stderr().is_terminal()
    }

    #[cfg(windows)]
    {
        io::stdin().is_terminal() || io::stderr().is_terminal()
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
    let base_temp =
        GuardedPath::new_root(std::env::temp_dir().as_path()).context("guard system temp dir")?;
    let ts = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis();
    let temp_file = base_temp
        .join(&format!("oxdock-shell-{ts}-{}.exe", std::process::id()))
        .context("construct temp shell path")?;

    // Copy the current executable into the temporary location via a
    // resolver whose root is the temp directory. The source may live
    // outside the temp dir, so use `copy_file_from_external`.
    let temp_root_guard = temp_file
        .parent()
        .ok_or_else(|| anyhow::anyhow!("temp path unexpectedly missing parent"))?;
    let resolver_temp = PathResolver::new(temp_root_guard.as_path(), temp_root_guard.as_path())?;
    let dest = temp_file;
    #[allow(clippy::disallowed_types)]
    let source = oxdock_fs::UnguardedPath::new(self_path);
    resolver_temp
        .copy_file_from_external(&source, &dest)
        .with_context(|| format!("failed to copy shell runner to {}", dest.display()))?;

    let mut cmd = Command::new(dest.as_path());
    cmd.args(std::env::args_os().skip(1));
    cmd.env("OXDOCK_SHELL_REEXEC", "1");

    cmd.spawn()
        .with_context(|| format!("failed to spawn shell from {}", dest.display()))?;

    // Exit immediately so the original binary can be rebuilt while the shell child stays running.
    std::process::exit(0);
}

fn discover_workspace_root() -> Result<String> {
    if let Ok(root) = std::env::var("OXDOCK_WORKSPACE_ROOT") {
        return Ok(root);
    }

    if let Ok(resolver) = PathResolver::from_manifest_env()
        && let Some(parent) = resolver.root().as_path().parent()
    {
        return Ok(parent.to_string_lossy().to_string());
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
            return Ok(path);
        }
    }

    Ok(std::env::current_dir()
        .context("failed to determine current directory for workspace root")?
        .to_string_lossy()
        .to_string())
}
pub fn run_script(workspace_root: &GuardedPath, steps: &[Step]) -> Result<()> {
    run_steps_with_context(workspace_root, workspace_root, steps)
}

// TODO: Include mention that this is still using the user's filesystem, and can result in destructive actions outside of the workspace
fn shell_banner(cwd: &GuardedPath, workspace_root: &GuardedPath) -> String {
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

fn run_shell(cwd: &GuardedPath, workspace_root: &GuardedPath) -> Result<()> {
    let banner = shell_banner(cwd, workspace_root);

    #[cfg(unix)]
    {
        let mut cmd = Command::new(shell_program());
        cmd.current_dir(cwd.as_path());

        // Print a single banner inside the subshell, then exec the user's shell to stay interactive.
        let script = format!("printf '%s\\n' \"{}\"; exec {}", banner, shell_program());
        cmd.arg("-c").arg(script);

        // Reattach stdin to the controlling TTY so a piped-in script can still open an interactive shell.
        // Use `PathResolver::open_external_file` to centralize raw `File::open` usage.
        #[allow(clippy::disallowed_types)]
        let tty_path = oxdock_fs::UnguardedPath::new("/dev/tty");
        if let Ok(resolver) = PathResolver::new(workspace_root.as_path(), workspace_root.as_path())
            && let Ok(tty) = resolver.open_external_file(&tty_path)
        {
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
            .arg(cwd.as_path())
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

// TODO: Don't hardcode internal config
fn open_editor_and_read() -> Result<String> {
    // Create a named temp file for editing.
    let tmp = tempfile::NamedTempFile::new().context("create temp file for editor")?;
    let tmp_dir = tmp
        .path()
        .parent()
        .ok_or_else(|| anyhow::anyhow!("temp file unexpectedly missing parent"))?;
    let tmp_root = GuardedPath::new_root(tmp_dir)?;
    let tmp_file = GuardedPath::new(tmp_root.root(), tmp.path())?;

    // Determine a per-user cache path to store the last inline script.
    fn last_inline_path() -> Option<GuardedPath> {
        if let Ok(x) = std::env::var("XDG_CONFIG_HOME")
            && let Ok(root) = GuardedPath::new_root_from_str(&x)
            && let Ok(path) = root.join("oxdock").and_then(|p| p.join("last_inline"))
        {
            return Some(path);
        }

        if cfg!(windows)
            && let Ok(app) = std::env::var("APPDATA")
            && let Ok(root) = GuardedPath::new_root_from_str(&app)
            && let Ok(path) = root.join("oxdock").and_then(|p| p.join("last_inline"))
        {
            return Some(path);
        }

        if let Ok(home) = std::env::var("HOME")
            && let Ok(root) = GuardedPath::new_root_from_str(&home)
            && let Ok(path) = root
                .join(".config")
                .and_then(|p| p.join("oxdock"))
                .and_then(|p| p.join("last_inline"))
        {
            return Some(path);
        }
        None
    }

    // If a cached inline script exists, preload it into the temp file so the
    // editor opens with the previous content.
    if let Some(cache) = last_inline_path()
        && let Some(cache_parent) = cache.parent()
        && let Ok(cache_fs) = PathResolver::new(cache_parent.as_path(), cache_parent.as_path())
        && cache_fs.metadata_abs(&cache).is_ok()
        && let Ok(prev) = cache_fs.read_to_string(&cache)
    {
        let tmp_fs = PathResolver::new(tmp_root.as_path(), tmp_root.as_path())?;
        let _ = tmp_fs.write_file(&tmp_file, prev.as_bytes());
    }

    // Choose editor: VISUAL, EDITOR, then sensible defaults.
    let editor = std::env::var("VISUAL")
        .or_else(|_| std::env::var("EDITOR"))
        .unwrap_or_else(|_| {
            if cfg!(windows) {
                "notepad".to_string()
            } else {
                // prefer nano if available, otherwise vi
                "nano".to_string()
            }
        });

    // Spawn editor attached to the current terminal so users can interact.
    let mut cmd = Command::new(editor);
    cmd.arg(tmp_file.as_path())
        .stdin(Stdio::inherit())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit());

    let status = cmd.status().context("failed to launch editor")?;
    if !status.success() {
        bail!("editor exited with status {}", status);
    }

    // Read the file contents back (tmp stays until drop)
    let tmp_fs = PathResolver::new(tmp_root.as_path(), tmp_root.as_path())?;
    let contents = tmp_fs
        .read_to_string(&tmp_file)
        .with_context(|| format!("failed to read edited file {}", tmp_file.display()))?;

    // Save the edited script back to the cache for next time. Ignore errors
    // so the editor flow remains best-effort.
    if let Some(cache) = last_inline_path()
        && let Some(parent) = cache.parent()
        && let Ok(cache_fs) = PathResolver::new(parent.as_path(), parent.as_path())
    {
        let _ = cache_fs.create_dir_all_abs(&parent);
        let _ = cache_fs.write_file(&cache, contents.as_bytes());
    }

    Ok(contents)
}

fn archive_head(workspace_root: &GuardedPath, temp_root: &GuardedPath) -> Result<()> {
    let archive_guard = temp_root
        .join("src.tar")
        .context("failed to create archive path under temp root")?;
    let archive_str = archive_guard.display().to_string();
    run_cmd(
        Command::new("git")
            .current_dir(workspace_root.as_path())
            .args(["archive", "--format=tar", "--output", &archive_str, "HEAD"]),
    )?;

    run_cmd(
        Command::new("tar")
            .arg("-xf")
            .arg(&archive_str)
            .arg("-C")
            .arg(temp_root.as_path()),
    )?;

    // Drop the intermediate archive to keep the temp workspace clean.
    // Remove intermediate archive via a resolver rooted at the temp root.
    let resolver_temp = PathResolver::new(temp_root.as_path(), temp_root.as_path())?;
    resolver_temp
        .remove_file_abs(&archive_guard)
        .with_context(|| format!("failed to remove {}", archive_guard.display()))?;
    Ok(())
}
