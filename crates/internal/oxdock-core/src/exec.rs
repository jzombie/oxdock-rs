use anyhow::{Context, Result, anyhow, bail};
use std::collections::HashMap;
use std::io::{self, Write};
use std::process::ExitStatus;

use oxdock_fs::{EntryKind, GuardedPath, PathResolver, WorkspaceFs, to_forward_slashes};
use oxdock_parser::{Step, StepKind, WorkspaceTarget};
use oxdock_process::{
    BackgroundHandle, BuiltinEnv, CommandContext, ProcessManager, SharedInput, default_process_manager,
    expand_command_env,
};
use sha2::{Digest, Sha256};
use std::sync::{Arc, Mutex};

struct ExecState<P: ProcessManager> {
    fs: Box<dyn WorkspaceFs>,
    cargo_target_dir: GuardedPath,
    cwd: GuardedPath,
    envs: HashMap<String, String>,
    bg_children: Vec<P::Handle>,
    scope_stack: Vec<ScopeSnapshot>,
}

struct ScopeSnapshot {
    cwd: GuardedPath,
    root: GuardedPath,
    envs: HashMap<String, String>,
}

impl<P: ProcessManager> ExecState<P> {
    fn command_ctx(&self) -> CommandContext {
        CommandContext::new(
            &self.cwd.clone().into(),
            &self.envs,
            &self.cargo_target_dir,
            self.fs.root(),
            self.fs.build_context(),
        )
    }
}

pub fn run_steps(fs_root: &GuardedPath, steps: &[Step]) -> Result<()> {
    run_steps_with_context(fs_root, fs_root, steps)
}

pub fn run_steps_with_context(
    fs_root: &GuardedPath,
    build_context: &GuardedPath,
    steps: &[Step],
) -> Result<()> {
    run_steps_with_context_result(fs_root, build_context, steps, None, None).map(|_| ())
}

/// Execute the DSL and return the final working directory after all steps.
pub fn run_steps_with_context_result(
    fs_root: &GuardedPath,
    build_context: &GuardedPath,
    steps: &[Step],
    stdin: Option<SharedInput>,
    stdout: Option<&mut dyn Write>,
) -> Result<GuardedPath> {
    match run_steps_inner(fs_root, build_context, steps, stdin, stdout) {
        Ok(final_cwd) => Ok(final_cwd),
        Err(err) => {
            // Compose a single error message with the top cause plus a compact fs snapshot.
            let chain = err.chain().map(|e| e.to_string()).collect::<Vec<_>>();
            let mut primary = chain
                .first()
                .cloned()
                .unwrap_or_else(|| "unknown error".into());
            let rest = if chain.len() > 1 {
                let first_cause = chain[1].clone();
                primary = format!("{primary} ({first_cause})");
                if chain.len() > 2 {
                    let causes = chain
                        .iter()
                        .skip(2)
                        .map(|s| s.as_str())
                        .collect::<Vec<_>>()
                        .join("\n  ");
                    format!("\ncauses:\n  {}", causes)
                } else {
                    String::new()
                }
            } else {
                String::new()
            };
            let fs = PathResolver::new(fs_root.as_path(), build_context.as_path())?;
            let tree = describe_dir(&fs, fs_root, 2, 24);
            let snapshot = format!(
                "filesystem snapshot (root {}):\n{}",
                fs_root.display(),
                tree
            );
            let msg = format!("{}{}\n{}", primary, rest, snapshot);
            Err(anyhow::anyhow!(msg))
        }
    }
}

fn run_steps_inner(
    fs_root: &GuardedPath,
    build_context: &GuardedPath,
    steps: &[Step],
    stdin: Option<SharedInput>,
    stdout: Option<&mut dyn Write>,
) -> Result<GuardedPath> {
    let mut resolver = PathResolver::new_guarded(fs_root.clone(), build_context.clone())?;
    resolver.set_workspace_root(build_context.clone());
    run_steps_with_fs(Box::new(resolver), steps, stdin, stdout)
}

pub fn run_steps_with_fs(
    fs: Box<dyn WorkspaceFs>,
    steps: &[Step],
    stdin: Option<SharedInput>,
    stdout: Option<&mut dyn Write>,
) -> Result<GuardedPath> {
    run_steps_with_manager(fs, steps, default_process_manager(), stdin, stdout)
}

fn run_steps_with_manager<P: ProcessManager>(
    fs: Box<dyn WorkspaceFs>,
    steps: &[Step],
    process: P,
    stdin: Option<SharedInput>,
    stdout: Option<&mut dyn Write>,
) -> Result<GuardedPath> {
    let fs_root = fs.root().clone();
    let cwd = fs.root().clone();
    let build_context = fs.build_context().clone();
    let envs = BuiltinEnv::collect(&build_context).into_envs();
    let mut state = ExecState {
        fs,
        cargo_target_dir: fs_root.join(".cargo-target")?,
        cwd,
        envs,
        bg_children: Vec::new(),
        scope_stack: Vec::new(),
    };

    let mut default_stdout = io::stdout();
    let (out, capture_output) = if let Some(s) = stdout {
        (s, true)
    } else {
        (&mut default_stdout as &mut dyn Write, false)
    };
    let mut proc_mgr = process;
    execute_steps(
        &mut state,
        &mut proc_mgr,
        steps,
        capture_output,
        stdin,
        false, // expose_stdin defaults to false
        out,
    )?;

    Ok(state.cwd)
}

fn execute_steps<P: ProcessManager>(
    state: &mut ExecState<P>,
    process: &mut P,
    steps: &[Step],
    capture_output: bool,
    stdin: Option<SharedInput>,
    expose_stdin: bool,
    out: &mut dyn Write,
) -> Result<()> {
    let fs_root = state.fs.root().clone();
    let build_context = state.fs.build_context().clone();

    // Inject stdin into envs if available and utf8
    // Note: We cannot peek at the stream without consuming it.
    // So we can't inject "stdin" env var if we are streaming.
    // The user wants streaming, so we skip env injection if we have a stream.
    // Or we only inject if it's not streaming?
    // The previous implementation took &[u8], so it was always buffered.
    // Now we have a stream. We can't buffer it just to put it in env.
    
    // let old_stdin = state.envs.get("stdin").cloned();
    // if let Some(data) = stdin {
    //     if let Ok(s) = std::str::from_utf8(data) {
    //         state.envs.insert("stdin".to_string(), s.to_string());
    //     }
    // }

    let check_bg = |bg: &mut Vec<P::Handle>| -> Result<Option<ExitStatus>> {
        let mut finished: Option<ExitStatus> = None;
        for child in bg.iter_mut() {
            if let Some(status) = child.try_wait()? {
                finished = Some(status);
                break;
            }
        }
        if let Some(status) = finished {
            // Tear down remaining background children.
            for child in bg.iter_mut() {
                if child.try_wait()?.is_none() {
                    let _ = child.kill();
                    let _ = child.wait();
                }
            }
            bg.clear();
            return Ok(Some(status));
        }
        Ok(None)
    };

    for (idx, step) in steps.iter().enumerate() {
        if step.scope_enter > 0 {
            for _ in 0..step.scope_enter {
                state.scope_stack.push(ScopeSnapshot {
                    cwd: state.cwd.clone(),
                    root: state.fs.root().clone(),
                    envs: state.envs.clone(),
                });
            }
        }

        let should_run = oxdock_parser::guards_allow_any(&step.guards, &state.envs);
        let step_result: Result<()> = if !should_run {
            Ok(())
        } else {
            match &step.kind {
                StepKind::Workdir(path) => {
                    state.cwd = state
                        .fs
                        .resolve_workdir(&state.cwd, path)
                        .with_context(|| format!("step {}: WORKDIR {}", idx + 1, path))?;
                    Ok(())
                }
                StepKind::Workspace(target) => {
                    match target {
                        WorkspaceTarget::Snapshot => {
                            state.fs.set_root(fs_root.clone());
                            state.cwd = state.fs.root().clone();
                        }
                        WorkspaceTarget::Local => {
                            state.fs.set_root(build_context.clone());
                            state.cwd = state.fs.root().clone();
                        }
                    }
                    Ok(())
                }
                StepKind::Env { key, value } => {
                    let ctx = state.command_ctx();
                    let rendered = expand_command_env(value, &ctx);
                    state.envs.insert(key.clone(), rendered);
                    Ok(())
                }
                StepKind::Run(cmd) => {
                    let ctx = state.command_ctx();
                    let rendered = expand_command_env(cmd, &ctx);
                    if capture_output {
                        // If capturing, we do NOT pass stdin by default unless it was explicitly passed down
                        // via WITH_IO (which would have set `expose_stdin` to true).
                        let step_stdin = if expose_stdin { stdin.clone() } else { None };
                        let output = process
                            .run_capture(&ctx, &rendered, step_stdin)
                            .with_context(|| {
                                format!("step {}: RUN {}", idx + 1, rendered)
                            })?;
                        out.write_all(&output)?;
                    } else {
                        let step_stdin = if expose_stdin { stdin.clone() } else { None };
                        process
                            .run(&ctx, &rendered, step_stdin, None)
                            .with_context(|| format!("step {}: RUN {}", idx + 1, rendered))?;
                    }
                    Ok(())
                }
                StepKind::Echo(msg) => {
                    let ctx = state.command_ctx();
                    let rendered = expand_command_env(msg, &ctx);
                    writeln!(out, "{}", rendered)?;
                    Ok(())
                }
                StepKind::RunBg(cmd) => {
                    if capture_output {
                        bail!("RUN_BG is not supported inside CAPTURE_TO_FILE");
                    }
                    let ctx = state.command_ctx();
                    let rendered = expand_command_env(cmd, &ctx);
                    let step_stdin = if expose_stdin { stdin.clone() } else { None };
                    let child = process
                        .spawn_bg(&ctx, &rendered, step_stdin, None)
                        .with_context(|| format!("step {}: RUN_BG {}", idx + 1, rendered))?;
                    state.bg_children.push(child);
                    Ok(())
                }
                StepKind::Copy { from, to } => {
                    let from_abs = state
                        .fs
                        .resolve_copy_source(from)
                        .with_context(|| format!("step {}: COPY {} {}", idx + 1, from, to))?;
                    let to_abs = state
                        .fs
                        .resolve_write(&state.cwd, to)
                        .with_context(|| format!("step {}: COPY {} {}", idx + 1, from, to))?;
                    copy_entry(state.fs.as_ref(), &from_abs, &to_abs)
                        .with_context(|| format!("step {}: COPY {} {}", idx + 1, from, to))?;
                    Ok(())
                }
                StepKind::CopyGit {
                    rev,
                    from,
                    to,
                    include_dirty,
                } => {
                    let to_abs = state.fs.resolve_write(&state.cwd, to).with_context(|| {
                        format!("step {}: COPY_GIT {} {} {}", idx + 1, rev, from, to)
                    })?;
                    state
                        .fs
                        .copy_from_git(rev, from, &to_abs, *include_dirty)
                        .with_context(|| {
                            format!("step {}: COPY_GIT {} {} {}", idx + 1, rev, from, to)
                        })?;
                    Ok(())
                }
                StepKind::HashSha256 { path } => {
                    let target = state
                        .fs
                        .resolve_read(&state.cwd, path)
                        .with_context(|| format!("step {}: HASH_SHA256 {}", idx + 1, path))?;
                    let mut hasher = Sha256::new();
                    hash_path(state.fs.as_ref(), &target, "", &mut hasher)?;
                    let digest = hasher.finalize();
                    writeln!(out, "{:x}", digest)?;
                    Ok(())
                }

                StepKind::Symlink { from, to } => {
                    let to_abs = state.fs.resolve_write(&state.cwd, to).with_context(|| {
                        format!("step {}: SYMLINK {} {}", idx + 1, from, to)
                    })?;
                    let from_abs = state.fs.resolve_copy_source(from).with_context(|| {
                        format!("step {}: SYMLINK {} {}", idx + 1, from, to)
                    })?;
                    state.fs.symlink(&from_abs, &to_abs).with_context(|| {
                        format!("step {}: SYMLINK {} {}", idx + 1, from, to)
                    })?;
                    Ok(())
                }
                StepKind::Mkdir(path) => {
                    let target = state
                        .fs
                        .resolve_write(&state.cwd, path)
                        .with_context(|| format!("step {}: MKDIR {}", idx + 1, path))?;
                    state.fs.create_dir_all(&target).with_context(|| {
                        format!("failed to create dir {}", target.display())
                    })?;
                    Ok(())
                }
                StepKind::Ls(path_opt) => {
                    let dir = if let Some(p) = path_opt.as_deref() {
                        state
                            .fs
                            .resolve_read(&state.cwd, p)
                            .with_context(|| format!("step {}: LS {}", idx + 1, p))?
                    } else {
                        state.cwd.clone()
                    };
                    let mut entries = state
                        .fs
                        .read_dir_entries(&dir)
                        .with_context(|| format!("failed to read dir {}", dir.display()))?;
                    entries.sort_by_key(|a| a.file_name());
                    writeln!(out, "{}:", dir.display())?;
                    for entry in entries {
                        writeln!(out, "{}", entry.file_name().to_string_lossy())?;
                    }
                    Ok(())
                }
                StepKind::Cwd => {
                    // Print the canonical (physical) current working directory to stdout.
                    let real =
                        canonical_cwd(state.fs.as_ref(), &state.cwd).with_context(|| {
                            format!(
                                "step {}: CWD failed to canonicalize {}",
                                idx + 1,
                                state.cwd.display()
                            )
                        })?;
                    writeln!(out, "{}", real)?;
                    Ok(())
                }
                StepKind::Cat(path) => {
                    let target = state
                        .fs
                        .resolve_read(&state.cwd, path)
                        .with_context(|| format!("step {}: CAT {}", idx + 1, path))?;
                    let data = state
                        .fs
                        .read_file(&target)
                        .with_context(|| format!("failed to read {}", target.display()))?;
                    out.write_all(&data).with_context(|| {
                        format!("failed to write {} to stdout", target.display())
                    })?;
                    Ok(())
                }
                StepKind::Write { path, contents } => {
                    let target = state
                        .fs
                        .resolve_write(&state.cwd, path)
                        .with_context(|| format!("step {}: WRITE {}", idx + 1, path))?;
                    state.fs.ensure_parent_dir(&target).with_context(|| {
                        format!("failed to create parent for {}", target.display())
                    })?;
                    let ctx = state.command_ctx();
                    let rendered = expand_command_env(contents, &ctx);
                    state
                        .fs
                        .write_file(&target, rendered.as_bytes())
                        .with_context(|| format!("failed to write {}", target.display()))?;
                    Ok(())
                }
                StepKind::WithIo { streams, cmd } => {
                    let ctx = state.command_ctx();
                    let rendered_cmd = expand_command_env(cmd, &ctx);
                    let steps = oxdock_parser::parse_script(&rendered_cmd).with_context(|| {
                        format!("step {}: WITH_IO parse failed", idx + 1)
                    })?;

                                        // Determine IO configuration for the inner steps
                    let mut step_stdin = None;
                    let mut step_stdout: &mut dyn Write = out;
                    let mut real_stdout = std::io::stdout();
                    let mut next_expose_stdin = false;

                    if streams.contains(&"stdin".to_string()) {
                        step_stdin = stdin.clone();
                        next_expose_stdin = true;
                    }

                    if streams.contains(&"stdout".to_string()) {
                        // If stdout is requested, we bypass capture and write to real stdout
                        // UNLESS we are already writing to a custom buffer (e.g. from oxbook-cli)
                        // Wait, "stdout" means "outer process stdout".
                        // If `out` is already pointing to `real_stdout` (default), then we are good.
                        // If `out` is a buffer (capture), then "stdout" means we should bypass it?
                        // The user said: "WITH_IO [stdout] RUN ... then the RUN command's stdout should go to the outer process stdout (bypassing capture)."
                        
                        // However, `out` passed to `execute_steps` IS the target output.
                        // If `capture_output` is true, `out` is likely a buffer.
                        // If `capture_output` is false, `out` is likely stdout.
                        
                        // If we want to bypass capture, we should use `real_stdout`.
                        step_stdout = &mut real_stdout;
                    }

                    // We need to handle `capture_output` flag for recursive call.
                    // If we are bypassing capture (writing to real stdout), then capture_output should be false?
                    // Or rather, `execute_steps` uses `capture_output` to decide whether to use `out` or not?
                    // Actually `execute_steps` passes `out` to `process.run`.
                    // `process.run` writes to `out` if it's not capturing?
                    // No, `process.run` writes to `out` if `capture_output` is false?
                    // Let's check `execute_steps` logic for `Run`.
                    
                    // If `streams` contains "stdout", we want the output to go to `step_stdout`.
                    // And we want to ensure it's NOT captured into a buffer if we are inside CAPTURE_TO_FILE.
                    
                    // Let's look at `Run` implementation below.
                    
                    execute_steps(
                        state,
                        process,
                        &steps,
                        capture_output, // Pass through? Or force?
                        step_stdin,
                        next_expose_stdin,
                        step_stdout,
                    )?;
                    Ok(())
                }

                StepKind::CaptureToFile { path, cmd } => {
                    let target =
                        state.fs.resolve_write(&state.cwd, path).with_context(|| {
                            format!("step {}: CAPTURE_TO_FILE {}", idx + 1, path)
                        })?;
                    state.fs.ensure_parent_dir(&target).with_context(|| {
                        format!("failed to create parent for {}", target.display())
                    })?;
                    let steps = oxdock_parser::parse_script(cmd).with_context(|| {
                        format!("step {}: CAPTURE_TO_FILE parse failed", idx + 1)
                    })?;
                    if steps.len() != 1 {
                        bail!("CAPTURE_TO_FILE expects exactly one instruction");
                    }
                    let mut sub_state = ExecState {
                        fs: {
                            let mut resolver = PathResolver::new(
                                state.fs.root().as_path(),
                                state.fs.build_context().as_path(),
                            )?;
                            resolver.set_workspace_root(state.fs.build_context().clone());
                            Box::new(resolver)
                        },
                        cargo_target_dir: state.cargo_target_dir.clone(),
                        cwd: state.cwd.clone(),
                        envs: state.envs.clone(),
                        bg_children: Vec::new(),
                        scope_stack: Vec::new(),
                    };
                    let mut sub_process = process.clone();
                    let mut buf: Vec<u8> = Vec::new();
                    // CAPTURE_TO_FILE should NOT pass stdin by default unless explicitly requested?
                    // But here we are capturing output. Stdin is orthogonal.
                    // If the user wants to pipe stdin into a captured command, they can use WITH_IO [stdin] inside?
                    // Or should we pass it through?
                    // The user said "STDIN SHOULD'T EVEN BE AVAILABLE TO IT OTHERWISE".
                    // So we pass None.
                    execute_steps(
                        &mut sub_state,
                        &mut sub_process,
                        &steps,
                        true,
                        None,
                        false,
                        &mut buf,
                    )?;
                    state
                        .fs
                        .write_file(&target, &buf)
                        .with_context(|| format!("failed to write {}", target.display()))?;
                    Ok(())
                }
                StepKind::Exit(code) => {
                    for child in state.bg_children.iter_mut() {
                        if child.try_wait()?.is_none() {
                            let _ = child.kill();
                            let _ = child.wait();
                        }
                    }
                    state.bg_children.clear();
                    bail!("EXIT requested with code {}", code);
                }
            }
        };

        let restore_result = restore_scopes(state, step.scope_exit);
        step_result?;
        restore_result?;
        if let Some(status) = check_bg(&mut state.bg_children)? {
            if status.success() {
                return Ok(());
            } else {
                bail!("RUN_BG exited with status {}", status);
            }
        }
    }

    if !state.bg_children.is_empty() {
        let mut first = state.bg_children.remove(0);
        let status = first.wait()?;
        for child in state.bg_children.iter_mut() {
            if child.try_wait()?.is_none() {
                let _ = child.kill();
                let _ = child.wait();
            }
        }
        state.bg_children.clear();
        if status.success() {
            return Ok(());
        } else {
            bail!("RUN_BG exited with status {}", status);
        }
    }

    Ok(())
}

fn restore_scopes<P: ProcessManager>(state: &mut ExecState<P>, count: usize) -> Result<()> {
    for _ in 0..count {
        let snapshot = state
            .scope_stack
            .pop()
            .ok_or_else(|| anyhow!("scope stack underflow during pop"))?;
        state.fs.set_root(snapshot.root.clone());
        state.cwd = snapshot.cwd;
        state.envs = snapshot.envs;
    }
    Ok(())
}

fn copy_entry(fs: &dyn WorkspaceFs, src: &GuardedPath, dst: &GuardedPath) -> Result<()> {
    match fs.entry_kind(src)? {
        EntryKind::Dir => {
            fs.copy_dir_recursive(src, dst)?;
        }
        EntryKind::File => {
            fs.ensure_parent_dir(dst)?;
            fs.copy_file(src, dst)?;
        }
    }
    Ok(())
}

fn canonical_cwd(fs: &dyn WorkspaceFs, cwd: &GuardedPath) -> Result<String> {
    Ok(fs.canonicalize(cwd)?.display().to_string())
}

fn describe_dir(
    fs: &dyn WorkspaceFs,
    root: &GuardedPath,
    max_depth: usize,
    max_entries: usize,
) -> String {
    fn helper(
        fs: &dyn WorkspaceFs,
        guard_root: &GuardedPath,
        path: &GuardedPath,
        depth: usize,
        max_depth: usize,
        left: &mut usize,
        out: &mut String,
    ) {
        if *left == 0 {
            return;
        }
        let indent = "  ".repeat(depth);
        if depth > 0 {
            out.push_str(&format!(
                "{}{}\n",
                indent,
                path.as_path()
                    .file_name()
                    .unwrap_or_default()
                    .to_string_lossy()
            ));
        }
        if depth >= max_depth {
            return;
        }
        let entries = match fs.read_dir_entries(path) {
            Ok(e) => e,
            Err(_) => return,
        };
        let mut names: Vec<_> = entries.into_iter().collect();
        names.sort_by_key(|a| a.file_name());
        for entry in names {
            if *left == 0 {
                return;
            }
            *left -= 1;
            let file_type = match entry.file_type() {
                Ok(ft) => ft,
                Err(_) => continue,
            };
            let p = entry.path();
            let guarded_child = match GuardedPath::new(guard_root.root(), &p) {
                Ok(child) => child,
                Err(_) => continue,
            };
            if file_type.is_dir() {
                helper(
                    fs,
                    guard_root,
                    &guarded_child,
                    depth + 1,
                    max_depth,
                    left,
                    out,
                );
            } else {
                out.push_str(&format!(
                    "{}  {}\n",
                    indent,
                    entry.file_name().to_string_lossy()
                ));
            }
        }
    }

    let mut out = String::new();
    let mut left = max_entries;
    helper(fs, root, root, 0, max_depth, &mut left, &mut out);
    out
}

fn hash_path(
    fs: &dyn WorkspaceFs,
    path: &GuardedPath,
    rel: &str,
    hasher: &mut Sha256,
) -> Result<()> {
    match fs.entry_kind(path)? {
        EntryKind::Dir => {
            hasher.update(b"D\0");
            hasher.update(rel.as_bytes());
            hasher.update(b"\0");
            let mut entries = fs.read_dir_entries(path)?;
            entries.sort_by_key(|entry| entry.file_name());
            for entry in entries {
                let name = entry.file_name().to_string_lossy().to_string();
                let child = path.join(&name)?;
                let child_rel = if rel.is_empty() {
                    name
                } else {
                    format!("{}/{}", rel, name)
                };
                let child_rel = to_forward_slashes(&child_rel);
                hash_path(fs, &child, &child_rel, hasher)?;
            }
        }
        EntryKind::File => {
            let data = fs.read_file(path)?;
            if rel.is_empty() {
                hasher.update(&data);
            } else {
                hasher.update(b"F\0");
                hasher.update(rel.as_bytes());
                hasher.update(b"\0");
                hasher.update(&data);
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxdock_fs::{GuardedPath, MockFs};
    use oxdock_parser::Guard;
    use oxdock_process::{MockProcessManager, MockRunCall};
    use std::collections::HashMap;

    #[test]
    fn run_records_env_and_cwd() {
        let root = GuardedPath::new_root_from_str(".").unwrap();
        let steps = vec![
            Step {
                guards: Vec::new(),
                kind: StepKind::Env {
                    key: "FOO".into(),
                    value: "bar".into(),
                },
                scope_enter: 0,
                scope_exit: 0,
            },
            Step {
                guards: Vec::new(),
                kind: StepKind::Run("echo hi".into()),
                scope_enter: 0,
                scope_exit: 0,
            },
        ];
        let mock = MockProcessManager::default();
        let fs = Box::new(PathResolver::new_guarded(root.clone(), root.clone()).unwrap());
        run_steps_with_manager(fs, &steps, mock.clone(), None).unwrap();
        let runs = mock.recorded_runs();
        assert_eq!(runs.len(), 1);
        let MockRunCall {
            script,
            cwd,
            envs,
            cargo_target_dir,
            ..
        } = &runs[0];
        assert_eq!(script, "echo hi");
        assert_eq!(cwd, root.as_path());
        assert_eq!(
            cargo_target_dir,
            &root.join(".cargo-target").unwrap().to_path_buf()
        );
        assert_eq!(envs.get("FOO"), Some(&"bar".into()));
    }

    #[test]
    fn run_expands_env_values() {
        let root = GuardedPath::new_root_from_str(".").unwrap();
        let steps = vec![
            Step {
                guards: Vec::new(),
                kind: StepKind::Env {
                    key: "FOO".into(),
                    value: "bar".into(),
                },
                scope_enter: 0,
                scope_exit: 0,
            },
            Step {
                guards: Vec::new(),
                kind: StepKind::Run("echo ${FOO}".into()),
                scope_enter: 0,
                scope_exit: 0,
            },
        ];
        let mock = MockProcessManager::default();
        let fs = Box::new(PathResolver::new_guarded(root.clone(), root.clone()).unwrap());
        run_steps_with_manager(fs, &steps, mock.clone(), None).unwrap();
        let runs = mock.recorded_runs();
        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].script, "echo bar");
    }

    #[test]
    fn run_bg_completion_short_circuits_pipeline() {
        let root = GuardedPath::new_root_from_str(".").unwrap();
        let steps = vec![
            Step {
                guards: Vec::new(),
                kind: StepKind::RunBg("sleep".into()),
                scope_enter: 0,
                scope_exit: 0,
            },
            Step {
                guards: Vec::new(),
                kind: StepKind::Run("echo after".into()),
                scope_enter: 0,
                scope_exit: 0,
            },
        ];
        let mock = MockProcessManager::default();
        mock.push_bg_plan(0, success_status());
        let fs = Box::new(PathResolver::new_guarded(root.clone(), root.clone()).unwrap());
        run_steps_with_manager(fs, &steps, mock.clone(), None).unwrap();
        assert!(
            mock.recorded_runs().is_empty(),
            "foreground run should not execute when RUN_BG completes early"
        );
        let spawns = mock.spawn_log();
        let spawned: Vec<_> = spawns.iter().map(|c| c.script.as_str()).collect();
        assert_eq!(spawned, vec!["sleep"]);
    }

    #[test]
    fn exit_kills_background_processes() {
        let root = GuardedPath::new_root_from_str(".").unwrap();
        let steps = vec![
            Step {
                guards: Vec::new(),
                kind: StepKind::RunBg("bg-task".into()),
                scope_enter: 0,
                scope_exit: 0,
            },
            Step {
                guards: Vec::new(),
                kind: StepKind::Exit(5),
                scope_enter: 0,
                scope_exit: 0,
            },
        ];
        let mock = MockProcessManager::default();
        mock.push_bg_plan(usize::MAX, success_status());
        let fs = Box::new(PathResolver::new_guarded(root.clone(), root.clone()).unwrap());
        let err = run_steps_with_manager(fs, &steps, mock.clone(), None).unwrap_err();
        assert!(
            err.to_string().contains("EXIT requested with code 5"),
            "unexpected error: {err}"
        );
        assert_eq!(mock.killed(), vec!["bg-task"]);
    }

    #[test]
    fn symlink_errors_report_underlying_cause() {
        let temp = GuardedPath::tempdir().unwrap();
        let root = temp.as_guarded_path().clone();
        let steps = vec![
            Step {
                guards: Vec::new(),
                kind: StepKind::Mkdir("client".into()),
                scope_enter: 0,
                scope_exit: 0,
            },
            Step {
                guards: Vec::new(),
                kind: StepKind::Symlink {
                    from: "client".into(),
                    to: "client".into(),
                },
                scope_enter: 0,
                scope_exit: 0,
            },
        ];
        let err = run_steps(&root, &steps).unwrap_err();
        let msg = err.to_string();
        assert!(
            msg.contains("step 2: SYMLINK client client"),
            "error should include step context: {msg}"
        );
        assert!(
            msg.contains("SYMLINK destination already exists"),
            "error should surface underlying cause: {msg}"
        );
    }

    #[test]
    fn guarded_run_waits_for_env_to_be_set() {
        let root = GuardedPath::new_root_from_str(".").unwrap();
        let guard = Guard::EnvEquals {
            key: "READY".into(),
            value: "1".into(),
            invert: false,
        };
        let steps = vec![
            Step {
                guards: vec![vec![guard.clone()]],
                kind: StepKind::Run("echo first".into()),
                scope_enter: 0,
                scope_exit: 0,
            },
            Step {
                guards: Vec::new(),
                kind: StepKind::Env {
                    key: "READY".into(),
                    value: "1".into(),
                },
                scope_enter: 0,
                scope_exit: 0,
            },
            Step {
                guards: vec![vec![guard]],
                kind: StepKind::Run("echo second".into()),
                scope_enter: 0,
                scope_exit: 0,
            },
        ];
        let mock = MockProcessManager::default();
        let fs = Box::new(PathResolver::new_guarded(root.clone(), root.clone()).unwrap());
        run_steps_with_manager(fs, &steps, mock.clone(), None).unwrap();
        let runs = mock.recorded_runs();
        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].script, "echo second");
    }

    #[test]
    fn guard_groups_allow_any_matching_branch() {
        let root = GuardedPath::new_root_from_str(".").unwrap();
        let guard_alpha = Guard::EnvEquals {
            key: "MODE".into(),
            value: "alpha".into(),
            invert: false,
        };
        let guard_beta = Guard::EnvEquals {
            key: "MODE".into(),
            value: "beta".into(),
            invert: false,
        };
        let steps = vec![
            Step {
                guards: Vec::new(),
                kind: StepKind::Env {
                    key: "MODE".into(),
                    value: "beta".into(),
                },
                scope_enter: 0,
                scope_exit: 0,
            },
            Step {
                guards: vec![vec![guard_alpha], vec![guard_beta]],
                kind: StepKind::Run("echo guarded".into()),
                scope_enter: 0,
                scope_exit: 0,
            },
        ];
        let mock = MockProcessManager::default();
        let fs = Box::new(PathResolver::new_guarded(root.clone(), root.clone()).unwrap());
        run_steps_with_manager(fs, &steps, mock.clone(), None).unwrap();
        let runs = mock.recorded_runs();
        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].script, "echo guarded");
    }

    #[test]
    fn capture_to_file_rejects_multiple_instructions() {
        let root = GuardedPath::new_root_from_str(".").unwrap();
        let capture = Step {
            guards: Vec::new(),
            kind: StepKind::CaptureToFile {
                path: "out.txt".into(),
                cmd: "WRITE one 1; WRITE two 2".into(),
            },
            scope_enter: 0,
            scope_exit: 0,
        };
        let mock = MockProcessManager::default();
        let fs = Box::new(PathResolver::new_guarded(root.clone(), root.clone()).unwrap());
        let err = run_steps_with_manager(fs, &[capture], mock, None).unwrap_err();
        assert!(
            err.to_string()
                .contains("CAPTURE_TO_FILE expects exactly one instruction"),
            "unexpected error: {err}"
        );
    }

    fn success_status() -> ExitStatus {
        exit_status_from_code(0)
    }

    #[cfg(unix)]
    fn exit_status_from_code(code: i32) -> ExitStatus {
        use std::os::unix::process::ExitStatusExt;
        ExitStatusExt::from_raw(code << 8)
    }

    #[cfg(windows)]
    fn exit_status_from_code(code: i32) -> ExitStatus {
        use std::os::windows::process::ExitStatusExt;
        ExitStatusExt::from_raw(code as u32)
    }

    fn create_exec_state(fs: MockFs) -> ExecState<MockProcessManager> {
        let cargo = fs.root().join(".cargo-target").unwrap();
        ExecState {
            fs: Box::new(fs.clone()),
            cargo_target_dir: cargo,
            cwd: fs.root().clone(),
            envs: HashMap::new(),
            bg_children: Vec::new(),
            scope_stack: Vec::new(),
        }
    }

    fn run_with_mock_fs(steps: &[Step]) -> (GuardedPath, HashMap<String, Vec<u8>>) {
        let fs = MockFs::new();
        let mut state = create_exec_state(fs.clone());
        let mut proc = MockProcessManager::default();
        let mut sink = Vec::new();
        execute_steps(&mut state, &mut proc, steps, false, None, &mut sink).unwrap();
        (state.cwd, fs.snapshot())
    }

    #[test]
    fn mock_fs_handles_workdir_and_write() {
        let steps = vec![
            Step {
                guards: Vec::new(),
                kind: StepKind::Mkdir("app".into()),
                scope_enter: 0,
                scope_exit: 0,
            },
            Step {
                guards: Vec::new(),
                kind: StepKind::Workdir("app".into()),
                scope_enter: 0,
                scope_exit: 0,
            },
            Step {
                guards: Vec::new(),
                kind: StepKind::Write {
                    path: "out.txt".into(),
                    contents: "hi".into(),
                },
                scope_enter: 0,
                scope_exit: 0,
            },
            Step {
                guards: Vec::new(),
                kind: StepKind::Cat("out.txt".into()),
                scope_enter: 0,
                scope_exit: 0,
            },
        ];
        let (_cwd, files) = run_with_mock_fs(&steps);
        let written = files
            .iter()
            .find(|(k, _)| k.ends_with("app/out.txt"))
            .map(|(_, v)| String::from_utf8_lossy(v).to_string());
        assert_eq!(written, Some("hi".into()));
    }

    #[test]
    fn write_interpolates_env_values() {
        let steps = vec![
            Step {
                guards: Vec::new(),
                kind: StepKind::Env {
                    key: "FOO".into(),
                    value: "bar".into(),
                },
                scope_enter: 0,
                scope_exit: 0,
            },
            Step {
                guards: Vec::new(),
                kind: StepKind::Env {
                    key: "BAZ".into(),
                    value: "${FOO}-baz".into(),
                },
                scope_enter: 0,
                scope_exit: 0,
            },
            Step {
                guards: Vec::new(),
                kind: StepKind::Write {
                    path: "out.txt".into(),
                    contents: "val ${BAZ}".into(),
                },
                scope_enter: 0,
                scope_exit: 0,
            },
        ];
        let (_cwd, files) = run_with_mock_fs(&steps);
        let written = files
            .iter()
            .find(|(k, _)| k.ends_with("out.txt"))
            .map(|(_, v)| String::from_utf8_lossy(v).to_string());
        assert_eq!(written, Some("val bar-baz".into()));
    }

    #[test]
    fn final_cwd_tracks_last_workdir() {
        let steps = vec![
            Step {
                guards: Vec::new(),
                kind: StepKind::Write {
                    path: "temp.txt".into(),
                    contents: "123".into(),
                },
                scope_enter: 0,
                scope_exit: 0,
            },
            Step {
                guards: Vec::new(),
                kind: StepKind::Workdir("sub".into()),
                scope_enter: 0,
                scope_exit: 0,
            },
        ];
        let (cwd, snapshot) = run_with_mock_fs(&steps);
        assert!(
            cwd.as_path().ends_with("sub"),
            "expected final cwd to match last WORKDIR, got {}",
            cwd.display()
        );
        let keys: Vec<_> = snapshot.keys().cloned().collect();
        assert!(
            keys.iter().any(|path| path.ends_with("temp.txt")),
            "WRITE should produce temp file, snapshot: {:?}",
            keys
        );
    }

    #[test]
    fn mock_fs_normalizes_backslash_workdir() {
        let steps = vec![
            Step {
                guards: Vec::new(),
                kind: StepKind::Mkdir("win\\nested".into()),
                scope_enter: 0,
                scope_exit: 0,
            },
            Step {
                guards: Vec::new(),
                kind: StepKind::Workdir("win\\nested".into()),
                scope_enter: 0,
                scope_exit: 0,
            },
            Step {
                guards: Vec::new(),
                kind: StepKind::Write {
                    path: "inner.txt".into(),
                    contents: "ok".into(),
                },
                scope_enter: 0,
                scope_exit: 0,
            },
        ];
        let (cwd, snapshot) = run_with_mock_fs(&steps);
        let cwd_display = cwd.display().to_string();
        assert!(
            cwd_display.ends_with("win\\nested") || cwd_display.ends_with("win/nested"),
            "expected cwd to normalize backslashes, got {cwd_display}"
        );
        assert!(
            snapshot
                .keys()
                .any(|path| path.ends_with("win/nested/inner.txt")),
            "expected file under normalized path, snapshot: {:?}",
            snapshot.keys()
        );
    }

    #[cfg(windows)]
    #[test]
    fn mock_fs_rejects_absolute_windows_paths() {
        let steps = vec![Step {
            guards: Vec::new(),
            kind: StepKind::Workdir(r"C:\outside".into()),
            scope_enter: 0,
            scope_exit: 0,
        }];
        let fs = MockFs::new();
        let mut state = create_exec_state(fs);
        let mut proc = MockProcessManager::default();
        let mut sink = Vec::new();
        let err = execute_steps(&mut state, &mut proc, &steps, false, None, &mut sink).unwrap_err();
        let msg = format!("{err:#}");
        assert!(
            msg.contains("escapes allowed root"),
            "unexpected error for absolute Windows path: {msg}"
        );
    }

    #[test]
    fn with_stdin_passes_content_to_run() {
        let steps = vec![Step {
            guards: Vec::new(),
            kind: StepKind::WithStdin {
                content: "hello world".into(),
                cmd: "RUN cat".into(),
            },
            scope_enter: 0,
            scope_exit: 0,
        }];
        let fs = MockFs::new();
        let mut state = create_exec_state(fs);
        let mut proc = MockProcessManager::default();
        let mut sink = Vec::new();
        execute_steps(&mut state, &mut proc, &steps, false, None, &mut sink).unwrap();

        let runs = proc.recorded_runs();
        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].script, "cat");
        assert!(runs[0].stdin_provided, "stdin should be provided");
    }

    #[test]
    fn with_stdout_bypasses_capture() {
        let steps = vec![Step {
            guards: Vec::new(),
            kind: StepKind::CaptureToFile {
                path: "captured.txt".into(),
                cmd: "WITH_STDOUT \"RUN echo 'should not be captured'\"".into(),
            },
            scope_enter: 0,
            scope_exit: 0,
        }];
        let fs = MockFs::new();
        let mut state = create_exec_state(fs);
        let mut proc = MockProcessManager::default();
        let mut sink = Vec::new();
        execute_steps(&mut state, &mut proc, &steps, false, None, &mut sink).unwrap();

        // Verify captured.txt is empty
        let captured = state.fs.read_file(&state.fs.resolve_read(&state.cwd, "captured.txt").unwrap()).unwrap();
        assert!(captured.is_empty(), "expected captured.txt to be empty, got {:?}", String::from_utf8_lossy(&captured));
        
        // Verify the command was run
        let runs = proc.recorded_runs();
        assert_eq!(runs.len(), 1);
        assert_eq!(runs[0].script, "echo 'should not be captured'");
    }
}
