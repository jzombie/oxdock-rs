use anyhow::{Context, Result, bail};
use oxdock_fs::{GuardedPath, PathResolver};
use oxdock_process::CommandBuilder;
#[cfg(windows)]
use std::borrow::Cow;
use std::env;
use std::io::{self, IsTerminal, Read};

pub use oxdock_core::{Guard, Step, StepKind, parse_script, run_steps, run_steps_with_context};
pub use oxdock_process::shell_program;

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
}

impl Options {
    pub fn parse(
        args: &mut impl Iterator<Item = String>,
        workspace_root: &GuardedPath,
    ) -> Result<Self> {
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
                other => bail!("unexpected flag: {}", other),
            }
        }

        let script = script.unwrap_or(ScriptSource::Stdin);

        Ok(Self { script, shell })
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
                // No piped script provided. If the caller requested `--shell`
                // allow running with an initially-empty script so we can either
                // drop into the interactive shell or open the editor later.
                // Otherwise, require a script on stdin.
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

    let mut cmd = CommandBuilder::new(dest.as_path());
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
    if let Ok(output) = CommandBuilder::new("git")
        .arg("rev-parse")
        .arg("--show-toplevel")
        .output()
        && output.success()
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

fn shell_banner(cwd: &GuardedPath, workspace_root: &GuardedPath) -> String {
    #[cfg(windows)]
    let cwd_disp = command_path(cwd).as_ref().display().to_string();
    #[cfg(windows)]
    let workspace_disp = command_path(workspace_root).as_ref().display().to_string();

    #[cfg(not(windows))]
    let cwd_disp = cwd.display().to_string();
    #[cfg(not(windows))]
    let workspace_disp = workspace_root.display().to_string();

    let pkg = env::var("CARGO_PKG_NAME").unwrap_or_else(|_| "oxdock".to_string());
    indoc::formatdoc! {"
        {pkg} shell workspace
          cwd: {cwd_disp}
          source: git HEAD at {workspace_disp}
          lifetime: temporary directory created for this shell session; it disappears when you exit
          creation: {pkg} archived the repo at HEAD into this temp workspace before launching the shell

          WARNING: This shell still runs on your host filesystem and is **not** isolated!
    "}
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

#[cfg(windows)]
fn windows_banner_command(banner: &str, cwd: &std::path::Path) -> String {
    let mut parts: Vec<String> = banner
        .lines()
        .map(|line| format!("echo {}", escape_for_cmd(line)))
        .collect();
    parts.push(format!(
        "cd /d {}",
        escape_for_cmd(&cwd.display().to_string())
    ));
    parts.join(" && ")
}

fn run_shell(cwd: &GuardedPath, workspace_root: &GuardedPath) -> Result<()> {
    let banner = shell_banner(cwd, workspace_root);

    #[cfg(unix)]
    {
        let mut cmd = CommandBuilder::new(shell_program());
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
            cmd.stdin_file(tty);
        }

        let status = cmd.status()?;
        if !status.success() {
            bail!("shell exited with status {}", status);
        }
        Ok(())
    }

    #[cfg(windows)]
    {
        // Launch via `start` so Windows opens a real interactive console window. Normalize the path
        // and also set the parent process working directory to the temp workspace; this avoids
        // start's `/D` parsing quirks on paths with spaces or verbatim prefixes.
        let cwd_path = command_path(cwd);
        let banner_cmd = windows_banner_command(&banner, cwd_path.as_ref());
        let mut cmd = CommandBuilder::new("cmd");
        cmd.current_dir(cwd_path.as_ref())
            .arg("/C")
            .arg("start")
            .arg("oxdock shell")
            .arg("cmd")
            .arg("/K")
            .arg(banner_cmd);

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
fn run_cmd(mut cmd: CommandBuilder) -> Result<()> {
    let status = cmd.status()?;
    if !status.success() {
        bail!("command returned status {}", status);
    }
    Ok(())
}

#[cfg(windows)]
fn command_path(path: &GuardedPath) -> Cow<'_, std::path::Path> {
    #[cfg(windows)]
    {
        use std::path::{Component, PathBuf, Prefix};

        let mut components = path.as_path().components();
        if let Some(Component::Prefix(prefix)) = components.next() {
            match prefix.kind() {
                Prefix::VerbatimDisk(drive) => {
                    let mut buf = PathBuf::from(format!("{}:", char::from(drive)));
                    buf.extend(components);
                    return Cow::Owned(buf);
                }
                Prefix::VerbatimUNC(server, share) => {
                    let mut buf = PathBuf::from(r"\\");
                    buf.push(server);
                    buf.push(share);
                    buf.extend(components);
                    return Cow::Owned(buf);
                }
                Prefix::Verbatim(_) => {
                    let mut buf = PathBuf::new();
                    buf.push(prefix.as_os_str());
                    buf.extend(components);
                    return Cow::Owned(buf);
                }
                _ => {}
            }
        }
    }

    Cow::Borrowed(path.as_path())
}

fn archive_head(workspace_root: &GuardedPath, temp_root: &GuardedPath) -> Result<()> {
    let archive_guard = temp_root
        .join("src.tar")
        .context("failed to create archive path under temp root")?;
    #[cfg(windows)]
    let archive_path_buf = command_path(&archive_guard);
    #[cfg(windows)]
    let archive_path = archive_path_buf.as_ref();
    #[cfg(not(windows))]
    let archive_path = archive_guard.as_path();

    #[cfg(windows)]
    let temp_root_path_buf = command_path(temp_root);
    #[cfg(windows)]
    let temp_root_path = temp_root_path_buf.as_ref();
    #[cfg(not(windows))]
    let temp_root_path = temp_root.as_path();
    let mut archive_cmd = CommandBuilder::new("git");
    archive_cmd
        .current_dir(workspace_root.as_path())
        .args(["archive", "--format=tar", "--output"]);
    archive_cmd.arg(archive_path).arg("HEAD");
    run_cmd(archive_cmd)?;

    let mut tar_cmd = CommandBuilder::new("tar");
    #[cfg(unix)]
    {
        // The temp workspace often inherits mtimes newer than the host clock (e.g. containerized builds),
        // which makes GNU tar print noisy "timestamp is in the future" diagnostics. Silence just that warning.
        tar_cmd.arg("--warning=no-timestamp");
    }
    tar_cmd
        .arg("-xf")
        .arg(archive_path)
        .arg("-C")
        .arg(temp_root_path);
    run_cmd(tar_cmd)?;

    // Drop the intermediate archive to keep the temp workspace clean.
    // Remove intermediate archive via a resolver rooted at the temp root.
    let resolver_temp = PathResolver::new(temp_root.as_path(), temp_root.as_path())?;
    resolver_temp
        .remove_file_abs(&archive_guard)
        .with_context(|| format!("failed to remove {}", archive_guard.display()))?;
    Ok(())
}

#[cfg(test)]
mod tests {
    #[cfg(windows)]
    use super::*;
    #[cfg(windows)]
    use oxdock_fs::PathResolver;

    #[cfg(windows)]
    fn git(
        workspace_root: &GuardedPath,
        args: impl IntoIterator<Item = &'static str>,
    ) -> Result<()> {
        let mut cmd = CommandBuilder::new("git");
        cmd.current_dir(workspace_root.as_path()).args(args);
        run_cmd(cmd)
    }

    #[cfg_attr(
        miri,
        ignore = "relies on tempdirs, git, and tar binaries which are unavailable under Miri"
    )]
    #[cfg(windows)]
    #[test]
    fn archive_head_handles_windows_temp_paths() -> Result<()> {
        // Use a temp workspace with spaces to mirror common Windows user dirs.
        let workspace = GuardedPath::tempdir_with(|builder| {
            builder.prefix("oxdock shell test ");
        })?;
        let workspace_root = workspace.as_guarded_path().clone();
        let resolver = PathResolver::new(workspace_root.as_path(), workspace_root.as_path())?;
        let readme = workspace_root.join("README.md")?;
        resolver.write_file(&readme, b"shell workspace")?;

        git(&workspace_root, ["init"])?;
        git(
            &workspace_root,
            ["config", "user.email", "test@example.com"],
        )?;
        git(&workspace_root, ["config", "user.name", "Test User"])?;
        git(&workspace_root, ["add", "."])?;
        git(&workspace_root, ["commit", "-m", "init"])?;

        let archive_temp = GuardedPath::tempdir_with(|builder| {
            builder.prefix("oxdock shell dst ");
        })?;
        let archive_root = archive_temp.as_guarded_path().clone();
        archive_head(&workspace_root, &archive_root)?;

        let extracted_readme = archive_root.join("README.md")?;
        let archive_resolver = PathResolver::new(archive_root.as_path(), archive_root.as_path())?;
        let contents = archive_resolver.read_to_string(&extracted_readme)?;
        assert_eq!(contents, "shell workspace");

        Ok(())
    }

    #[cfg(windows)]
    #[test]
    fn command_path_strips_verbatim_prefix() -> Result<()> {
        let temp = GuardedPath::tempdir()?;
        let converted = command_path(temp.as_guarded_path());
        let as_str = converted.as_ref().display().to_string();
        assert!(
            !as_str.starts_with(r"\\?\"),
            "expected non-verbatim path, got {as_str}"
        );
        Ok(())
    }

    #[cfg(windows)]
    #[test]
    fn windows_banner_command_emits_all_lines() {
        let banner = "line1\nline2\nline3";
        let cwd = std::path::Path::new(r"C:\temp\workspace");
        let cmd = windows_banner_command(banner, cwd);
        assert!(cmd.contains("line1"));
        assert!(cmd.contains("line2"));
        assert!(cmd.contains("line3"));
        assert!(cmd.contains("cd /d C:\\temp\\workspace"));
    }
}
