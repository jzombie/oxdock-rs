use anyhow::{Context, Result, bail};
#[cfg(windows)]
use oxdock_fs::command_path;
use oxdock_fs::{GuardedPath, PathResolver, copy_workspace_to};
use oxdock_process::CommandBuilder;
#[cfg(test)]
use oxdock_process::CommandSnapshot;
use std::env;
use std::io::{self, IsTerminal, Read};
#[cfg(test)]
use std::sync::Mutex;

pub use oxdock_core::{
    Guard, Step, StepKind, parse_script, run_steps, run_steps_with_context,
    run_steps_with_context_result,
};
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
    execute_with_shell_runner(opts, workspace_root, run_shell, true, true)
}

fn execute_with_shell_runner<F>(
    opts: Options,
    workspace_root: GuardedPath,
    shell_runner: F,
    require_tty: bool,
    prepare_snapshot: bool,
) -> Result<()>
where
    F: FnOnce(&GuardedPath, &GuardedPath) -> Result<()>,
{
    #[cfg(windows)]
    maybe_reexec_shell_to_temp(&opts)?;

    let tempdir = GuardedPath::tempdir().context("failed to create temp dir")?;
    let temp_root = tempdir.as_guarded_path().clone();

    // Materialize source tree without .git
    if prepare_snapshot {
        copy_workspace_to(&workspace_root, &temp_root).context("failed to snapshot workspace")?;
    }

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
    let mut final_cwd = temp_root.clone();
    if !script.trim().is_empty() {
        let steps = parse_script(&script)?;
        // Use the caller's workspace as the build context so WORKSPACE LOCAL can hop back and so COPY
        // can source from the original tree if needed. Capture the final working directory so shells
        // inherit whatever WORKDIR the script ended on.
        final_cwd = run_steps_with_context_result(&temp_root, &workspace_root, &steps)?;
    }

    // If requested, drop into an interactive shell after running the script.
    if opts.shell {
        if require_tty && !has_controlling_tty() {
            bail!("--shell requires a tty (no controlling tty available)");
        }
        return shell_runner(&final_cwd, &workspace_root);
    }

    Ok(())
}

#[cfg(test)]
fn execute_for_test<F>(opts: Options, workspace_root: GuardedPath, shell_runner: F) -> Result<()>
where
    F: FnOnce(&GuardedPath, &GuardedPath) -> Result<()>,
{
    execute_with_shell_runner(opts, workspace_root, shell_runner, false, false)
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
        .copy_file_from_unguarded(&source, &dest)
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
    let cwd_disp = oxdock_fs::command_path(cwd).as_ref().display().to_string();
    #[cfg(windows)]
    let workspace_disp = oxdock_fs::command_path(workspace_root)
        .as_ref()
        .display()
        .to_string();

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
fn windows_banner_command(banner: &str, cwd: &GuardedPath) -> String {
    let mut parts: Vec<String> = banner
        .lines()
        .map(|line| format!("echo {}", escape_for_cmd(line)))
        .collect();
    let cwd_path = oxdock_fs::command_path(cwd);
    parts.push(format!(
        "cd /d {}",
        escape_for_cmd(&cwd_path.as_ref().display().to_string())
    ));
    parts.join(" && ")
}

// TODO: Migrate to oxdock-process crate so that Miri flags don't need to be handled here.
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
        #[cfg(not(miri))]
        {
            #[allow(clippy::disallowed_types)]
            let tty_path = oxdock_fs::UnguardedPath::new("/dev/tty");
            if let Ok(resolver) =
                PathResolver::new(workspace_root.as_path(), workspace_root.as_path())
                && let Ok(tty) = resolver.open_file_unguarded(&tty_path)
            {
                cmd.stdin_file(tty);
            }
        }

        if try_shell_command_hook(&mut cmd)? {
            return Ok(());
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
        let cwd_path = oxdock_fs::command_path(cwd);
        let banner_cmd = windows_banner_command(&banner, cwd);
        let mut cmd = CommandBuilder::new("cmd");
        cmd.current_dir(cwd_path.as_ref())
            .arg("/C")
            .arg("start")
            .arg("oxdock shell")
            .arg("cmd")
            .arg("/K")
            .arg(banner_cmd);

        if try_shell_command_hook(&mut cmd)? {
            return Ok(());
        }

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
#[cfg(test)]
type ShellCmdHook = dyn FnMut(&CommandSnapshot) -> Result<()> + Send;

#[cfg(test)]
static SHELL_CMD_HOOK: Mutex<Option<Box<ShellCmdHook>>> = Mutex::new(None);

#[cfg(test)]
fn set_shell_command_hook<F>(hook: F)
where
    F: FnMut(&CommandSnapshot) -> Result<()> + Send + 'static,
{
    *SHELL_CMD_HOOK.lock().unwrap() = Some(Box::new(hook));
}

#[cfg(test)]
fn clear_shell_command_hook() {
    *SHELL_CMD_HOOK.lock().unwrap() = None;
}

#[cfg(test)]
fn try_shell_command_hook(cmd: &mut CommandBuilder) -> Result<bool> {
    if let Some(hook) = SHELL_CMD_HOOK.lock().unwrap().as_mut() {
        let snap = cmd.snapshot();
        hook(&snap)?;
        return Ok(true);
    }
    Ok(false)
}

#[cfg(not(test))]
fn try_shell_command_hook(_cmd: &mut CommandBuilder) -> Result<bool> {
    Ok(false)
}

// `command_path` now lives in `oxdock-fs` to centralize Path usage.

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use oxdock_fs::PathResolver;
    #[cfg(not(miri))]
    use serial_test::serial;
    use std::cell::Cell;

    #[cfg_attr(
        miri,
        ignore = "GuardedPath::tempdir relies on OS tempdirs; blocked under Miri isolation"
    )]
    #[test]
    fn shell_runner_receives_final_workdir() -> Result<()> {
        let workspace = GuardedPath::tempdir()?;
        let workspace_root = workspace.as_guarded_path().clone();
        let script_path = workspace_root.join("script.ox")?;
        let resolver = PathResolver::new(workspace_root.as_path(), workspace_root.as_path())?;
        let script = indoc! {"
            WRITE temp.txt 123
            WORKDIR sub
        "};
        resolver.write_file(&script_path, script.as_bytes())?;

        let opts = Options {
            script: ScriptSource::Path(script_path),
            shell: true,
        };

        let observed = Cell::new(false);
        execute_for_test(opts, workspace_root.clone(), |cwd, _| {
            assert!(
                cwd.as_path().ends_with("sub"),
                "final cwd should end in WORKDIR target, got {}",
                cwd.display()
            );

            let temp_root = GuardedPath::new_root(cwd.root())
                .context("construct guard for temp workspace root")?;
            let sub_dir = temp_root.join("sub")?;
            assert_eq!(
                cwd.as_path(),
                sub_dir.as_path(),
                "shell runner cwd should match guarded sub dir"
            );
            let temp_file = temp_root.join("temp.txt")?;
            let temp_resolver = PathResolver::new(temp_root.as_path(), temp_root.as_path())?;
            let contents = temp_resolver.read_to_string(&temp_file)?;
            assert!(
                contents.contains("123"),
                "expected WRITE command to materialize temp file"
            );
            observed.set(true);
            Ok(())
        })?;

        assert!(
            observed.into_inner(),
            "shell runner closure should have been invoked"
        );
        Ok(())
    }

    #[cfg(any(unix, windows))]
    #[test]
    // [serial] is required because this test interacts with global state (filesystem/env)
    // which causes race conditions on high-core-count machines. CI runners often have
    // fewer cores, masking this issue not apparent there.
    #[cfg_attr(not(miri), serial)]
    fn run_shell_builds_command_for_platform() -> Result<()> {
        let workspace = GuardedPath::tempdir()?;
        let workspace_root = workspace.as_guarded_path().clone();
        let cwd = workspace_root.join("subdir")?;
        #[cfg(not(miri))]
        {
            let resolver = PathResolver::new(workspace_root.as_path(), workspace_root.as_path())?;
            resolver.create_dir_all(&cwd)?;
        }

        let captured = std::sync::Arc::new(std::sync::Mutex::new(None::<CommandSnapshot>));
        let guard = captured.clone();
        set_shell_command_hook(move |cmd| {
            *guard.lock().unwrap() = Some(cmd.clone());
            Ok(())
        });
        run_shell(&cwd, &workspace_root)?;
        clear_shell_command_hook();

        let snap = captured
            .lock()
            .unwrap()
            .clone()
            .expect("hook should capture snapshot");
        let cwd_path = snap.cwd.expect("cwd should be set");
        assert!(
            cwd_path.ends_with("subdir"),
            "expected cwd to include subdir, got {}",
            cwd_path.display()
        );

        #[cfg(unix)]
        {
            let program = snap.program.to_string_lossy();
            assert_eq!(program, shell_program(), "expected shell program name");
            let args: Vec<_> = snap
                .args
                .iter()
                .map(|s| s.to_string_lossy().to_string())
                .collect();
            assert_eq!(
                args.len(),
                2,
                "expected two args (-c script), got {:?}",
                args
            );
            assert_eq!(args[0], "-c");
            assert!(
                args[1].contains("exec"),
                "expected script to exec the shell, got {:?}",
                args[1]
            );
        }

        #[cfg(windows)]
        {
            let program = snap.program.to_string_lossy().to_string();
            assert_eq!(program, "cmd", "expected cmd.exe launcher");
            let args: Vec<_> = snap
                .args
                .iter()
                .map(|s| s.to_string_lossy().to_string())
                .collect();
            let banner_cmd = windows_banner_command(&shell_banner(&cwd, &workspace_root), &cwd);
            let expected = vec![
                "/C".to_string(),
                "start".to_string(),
                "oxdock shell".to_string(),
                "cmd".to_string(),
                "/K".to_string(),
                banner_cmd,
            ];
            assert_eq!(args, expected, "expected exact windows shell argv");
        }

        Ok(())
    }
}

#[cfg(all(test, windows))]
mod windows_shell_tests {
    use super::*;
    use oxdock_fs::PathResolver;
    use serial_test::serial;

    #[test]
    fn command_path_strips_verbatim_prefix() -> Result<()> {
        let temp = GuardedPath::tempdir()?;
        let converted = oxdock_fs::command_path(temp.as_guarded_path());
        let as_str = converted.as_ref().display().to_string();
        assert!(
            !as_str.starts_with(r"\\?\"),
            "expected non-verbatim path, got {as_str}"
        );
        Ok(())
    }

    #[test]
    fn windows_banner_command_emits_all_lines() {
        let banner = "line1\nline2\nline3";
        let workspace = GuardedPath::tempdir().expect("tempdir");
        let cwd = workspace.as_guarded_path().clone();
        let cmd = windows_banner_command(banner, &cwd);
        assert!(cmd.contains("line1"));
        assert!(cmd.contains("line2"));
        assert!(cmd.contains("line3"));
        assert!(cmd.contains("cd /d "));
    }

    #[test]
    // [serial] is required because this test interacts with global state (filesystem/env)
    // which causes race conditions on high-core-count machines. CI runners often have
    // fewer cores, masking this issue not apparent there.
    #[cfg_attr(not(miri), serial)]
    fn run_shell_builds_windows_command() -> Result<()> {
        let workspace = GuardedPath::tempdir_with(|builder| {
            builder.prefix("oxdock shell win ");
        })?;
        let workspace_root = workspace.as_guarded_path().clone();
        let cwd = workspace_root.join("subdir")?;
        let resolver = PathResolver::new(workspace_root.as_path(), workspace_root.as_path())?;
        resolver.create_dir_all(&cwd)?;

        let captured = std::sync::Arc::new(std::sync::Mutex::new(None::<CommandSnapshot>));
        let guard = captured.clone();
        set_shell_command_hook(move |cmd| {
            *guard.lock().unwrap() = Some(cmd.clone());
            Ok(())
        });
        run_shell(&cwd, &workspace_root)?;
        clear_shell_command_hook();

        let snap = captured
            .lock()
            .unwrap()
            .clone()
            .expect("hook should capture snapshot");
        let program = snap.program.to_string_lossy().to_string();
        assert_eq!(program, "cmd", "expected cmd.exe launcher");
        let args: Vec<_> = snap
            .args
            .iter()
            .map(|s| s.to_string_lossy().to_string())
            .collect();
        let banner_cmd = windows_banner_command(&shell_banner(&cwd, &workspace_root), &cwd);
        let expected = vec![
            "/C".to_string(),
            "start".to_string(),
            "oxdock shell".to_string(),
            "cmd".to_string(),
            "/K".to_string(),
            banner_cmd,
        ];
        assert_eq!(args, expected, "expected exact windows shell argv");
        let cwd_path = snap.cwd.expect("cwd should be set");
        assert!(
            cwd_path.ends_with("subdir"),
            "expected cwd to include subdir, got {}",
            cwd_path.display()
        );
        Ok(())
    }
}
