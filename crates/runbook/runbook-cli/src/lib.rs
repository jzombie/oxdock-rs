use anyhow::{Context, Result, bail};
use notify::{Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher};
use os_pipe::pipe;
use oxdock_core::{
    ExecIo, run_steps_with_context_result_with_io,
    run_steps_with_context_result_with_io_and_process,
};
use oxdock_fs::{
    GuardedPath, GuardedTempDir, PathResolver, discover_workspace_root, to_forward_slashes,
};
use oxdock_parser::parse_script;
use oxdock_process::{
    CancellationToken, CommandCancelled, CommandOutput, InterruptibleProcessManager, SharedInput,
    SharedOutput,
};

pub mod session;

use once_cell::sync::Lazy;
use sha2::{Digest, Sha256};
use std::collections::{HashMap, HashSet};
use std::io::{self, BufRead, Cursor, IsTerminal, Stdout, Write};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, mpsc};
use std::time::Duration;

const STABLE_READ_RETRIES: usize = 5;
const STABLE_READ_DELAY: Duration = Duration::from_millis(30);
const MAX_FENCE_PREFIX_WS: usize = 3;
const OUTPUT_BEGIN: &str = "<!-- runbook-output:begin -->";
const OUTPUT_END: &str = "<!-- runbook-output:end -->";
const OUTPUT_META_PREFIX: &str = "<!-- runbook-output:meta";
const WATCH_DEBOUNCE_WINDOW: Duration = Duration::from_millis(120);
const COMMAND_POLL_INTERVAL: Duration = Duration::from_millis(75);
const SHORT_HASH_LEN: usize = 32;
// If set to "0" disables terminal streaming; otherwise auto-on when stdout is a TTY.
const STREAM_STDOUT_ENV: &str = "RUNBOOK_STREAM_STDOUT";
const PIPE_SETUP: &str = "setup";
const PIPE_SNIPPET: &str = "snippet";
pub const BLOCK_EVENT_ENV: &str = "RUNBOOK_EMIT_BLOCK_EVENTS";
pub const WORKER_EVENT_PREFIX: &str = "@@WORKER:";

#[derive(Debug)]
struct FenceBlock {
    fence: char,
    fence_len: usize,
    info: String,
    start_line: usize,
    end_line: usize,
    content: String,
}

struct OutputBlock {
    start_index: usize,
    end_index: usize,
    code_hash: Option<String>,
    stdout_hash: Option<String>,
    stderr_hash: Option<String>,
    combined_hash: Option<String>,
    stdout: String,
    stderr: String,
}
const STDERR_MARKER: &str = "<!-- runbook-output:stderr -->";

struct FenceInfo {
    language: Option<String>,
    params: HashMap<String, String>,
}

struct RunnerSpec {
    language: String,
    command: Vec<String>,
    oxfile: Option<GuardedPath>,
    env_hash: Option<String>,
}

#[allow(dead_code)]
struct RunnerEnv {
    root: GuardedPath,
    cwd: GuardedPath,
    env_hash: Option<String>,
    _tempdir: Option<GuardedTempDir>,
}

#[derive(Default)]
pub(crate) struct RunnerCache {
    envs: HashMap<String, RunnerEnv>,
}

#[derive(Default)]
pub(crate) struct RunControl {
    token: CancellationToken,
    active: AtomicBool,
}

impl RunControl {
    fn new() -> Self {
        Self {
            token: CancellationToken::new(),
            active: AtomicBool::new(false),
        }
    }

    pub(crate) fn begin(&self) -> RunGuard<'_> {
        self.token.reset();
        self.active.store(true, Ordering::SeqCst);
        RunGuard { control: self }
    }

    fn finish(&self) {
        self.active.store(false, Ordering::SeqCst);
    }

    pub(crate) fn request_cancel(&self) -> bool {
        if self.active.load(Ordering::SeqCst) {
            self.token.cancel();
            true
        } else {
            false
        }
    }

    pub(crate) fn token(&self) -> &CancellationToken {
        &self.token
    }
}

pub(crate) struct RunGuard<'a> {
    control: &'a RunControl,
}

impl Drop for RunGuard<'_> {
    fn drop(&mut self) {
        self.control.finish();
    }
}

pub trait ExecutionOutputObserver: Send + Sync {
    fn on_stdout(&self, _chunk: &[u8]) {}
    fn on_stderr(&self, _chunk: &[u8]) {}
}

pub fn run() -> Result<()> {
    let target = parse_target_path()?;
    let workspace_root = discover_workspace_root().context("resolve workspace root")?;
    let resolver = PathResolver::new_guarded(workspace_root.clone(), workspace_root.clone())
        .context("create workspace path resolver")?;

    // Auto-register runners discovered in the workspace (files named
    // `temp.runner.<lang>.oxfile`). This allows users to add runner oxfiles to
    // the repository without recompiling or env vars.
    scan_and_register_runners(&workspace_root);

    // Prefer resolving the provided path relative to the process current working
    // directory (the terminal cwd) when that directory can be represented as a
    // `GuardedPath` under the workspace root. Fall back to resolving relative
    // to the workspace root if the current directory lies outside the root.
    let process_cwd = std::env::current_dir().context("determine current directory")?;
    let cwd_base = match GuardedPath::new(workspace_root.root(), &process_cwd) {
        Ok(g) => g,
        Err(_) => workspace_root.clone(),
    };
    let watched = resolver
        .resolve_read(&cwd_base, &target)
        .with_context(|| format!("resolve markdown path {}", target))?;

    let cwd = watched.parent().unwrap_or_else(|| workspace_root.clone());
    let mut cache = RunnerCache::default();
    let emit_block_events = Arc::new(AtomicBool::new(emit_block_events_enabled()));
    let run_control = Arc::new(RunControl::new());

    let mut command_rx = None;
    if !std::io::stdin().is_terminal() {
        let (tx, rx) = mpsc::channel();
        spawn_command_listener(tx, emit_block_events.clone(), run_control.clone());
        command_rx = Some(rx);
    }

    let initial_contents = read_stable_contents(&resolver, &watched)?;
    let rendered = {
        let _guard = run_control.begin();
        render_shell_outputs(
            &initial_contents,
            &resolver,
            &workspace_root,
            &cwd,
            &mut cache,
            true,
            None,
            None,
            emit_block_events.load(Ordering::Relaxed),
            Some(run_control.token()),
            None,
        )?
    };
    if rendered != initial_contents {
        resolver
            .write_file(&watched, rendered.as_bytes())
            .with_context(|| format!("write {}", watched.display()))?;
    }
    let mut last_contents = rendered;
    eprintln!("Watching {}", watched.display());
    run_watch_loop(
        &resolver,
        &workspace_root,
        &watched,
        &cwd,
        &mut cache,
        &mut last_contents,
        command_rx,
        emit_block_events,
        run_control,
    )?;
    Ok(())
}

pub fn run_block(path: &str, line: usize) -> Result<()> {
    let target_line = line.max(1);
    let workspace_root = discover_workspace_root().context("resolve workspace root")?;
    let resolver = PathResolver::new_guarded(workspace_root.clone(), workspace_root.clone())
        .context("create workspace path resolver")?;

    scan_and_register_runners(&workspace_root);

    let process_cwd = std::env::current_dir().context("determine current directory")?;
    let cwd_base = match GuardedPath::new(workspace_root.root(), &process_cwd) {
        Ok(g) => g,
        Err(_) => workspace_root.clone(),
    };

    let watched = resolver
        .resolve_read(&cwd_base, path)
        .with_context(|| format!("resolve markdown path {path}"))?;

    let source_dir = watched.parent().unwrap_or_else(|| workspace_root.clone());
    let mut cache = RunnerCache::default();
    let stream_stdout = match std::env::var_os(STREAM_STDOUT_ENV) {
        Some(val) => val != "0",
        None => std::io::stdout().is_terminal(),
    };

    let emit_block_events = Arc::new(AtomicBool::new(false));

    let mut last_contents = String::new();
    let execution = run_block_in_place(
        &resolver,
        &workspace_root,
        &watched,
        &source_dir,
        &mut cache,
        &mut last_contents,
        target_line,
        emit_block_events,
        None,
        None,
    )?;

    match execution {
        Some(result) => {
            if !stream_stdout && !result.stdout.is_empty() {
                print!("{}", result.stdout);
                io::stdout().flush().ok();
            }
            if !result.stderr.is_empty() {
                eprint!("{}", result.stderr);
                io::stderr().flush().ok();
            }
            if !result.success {
                println!("Block at line {} finished with errors", target_line);
            }
        }
        None => {
            println!("No code block covering line {}", target_line);
        }
    }

    Ok(())
}

pub fn run_block_worker(path: &str) -> Result<()> {
    const EVENT_PREFIX: &str = "@@WORKER:";
    let workspace_root = discover_workspace_root().context("resolve workspace root")?;
    let resolver = PathResolver::new_guarded(workspace_root.clone(), workspace_root.clone())
        .context("create workspace path resolver")?;

    scan_and_register_runners(&workspace_root);

    let process_cwd = std::env::current_dir().context("determine current directory")?;
    let cwd_base = match GuardedPath::new(workspace_root.root(), &process_cwd) {
        Ok(g) => g,
        Err(_) => workspace_root.clone(),
    };

    let watched = resolver
        .resolve_read(&cwd_base, path)
        .with_context(|| format!("resolve markdown path {path}"))?;

    let source_dir = watched.parent().unwrap_or_else(|| workspace_root.clone());
    let mut cache = RunnerCache::default();
    let stream_stdout = match std::env::var_os(STREAM_STDOUT_ENV) {
        Some(val) => val != "0",
        None => std::io::stdout().is_terminal(),
    };

    let stdin = io::stdin();
    let mut input = stdin.lock();
    let mut command = String::new();
    let mut stdout = io::stdout();

    loop {
        command.clear();
        if input.read_line(&mut command)? == 0 {
            break;
        }
        let trimmed = command.trim();
        if trimmed.is_empty() {
            continue;
        }
        if let Some(rest) = trimmed.strip_prefix("RUN") {
            let line_value = rest.trim();
            let Ok(target_line) = line_value.parse::<usize>() else {
                eprintln!("invalid RUN command: {trimmed}");
                continue;
            };
            writeln!(stdout, "{EVENT_PREFIX} START {target_line}")?;
            stdout.flush()?;
            let outcome = run_block_once(
                &resolver,
                &watched,
                &workspace_root,
                &source_dir,
                &mut cache,
                target_line,
                stream_stdout,
            );
            match outcome {
                Ok(true) => {
                    writeln!(stdout, "{EVENT_PREFIX} DONE {target_line} OK")?;
                }
                Ok(false) => {
                    writeln!(stdout, "{EVENT_PREFIX} DONE {target_line} ERR")?;
                }
                Err(err) => {
                    eprintln!("run-block error at line {target_line}: {err}");
                    writeln!(stdout, "{EVENT_PREFIX} DONE {target_line} ERR")?;
                }
            }
            stdout.flush()?;
        }
    }

    Ok(())
}

fn run_block_once(
    resolver: &PathResolver,
    watched: &GuardedPath,
    workspace_root: &GuardedPath,
    source_dir: &GuardedPath,
    cache: &mut RunnerCache,
    line: usize,
    stream_stdout: bool,
) -> Result<bool> {
    let target_line = line.max(1);
    let contents = read_stable_contents(resolver, watched)?;
    let fences = parse_fences(&contents);
    let fence = match fences
        .into_iter()
        .find(|block| block.start_line <= target_line && block.end_line >= target_line)
    {
        Some(fence) => fence,
        None => {
            println!("No code block covering line {}", target_line);
            return Ok(false);
        }
    };

    let spec = match runner_spec(&fence.info, resolver, workspace_root, source_dir, cache)? {
        Some(spec) => spec,
        None => {
            println!(
                "No runner configured for fence at line {} (info: {})",
                fence.start_line,
                if fence.info.is_empty() {
                    String::from("plain")
                } else {
                    fence.info.clone()
                }
            );
            return Ok(false);
        }
    };

    let output = run_runner(
        resolver,
        workspace_root,
        source_dir,
        cache,
        &spec,
        &fence.content,
        None,
        None,
        None,
        None,
        None,
        None,
    )?;

    if !stream_stdout && !output.stdout.is_empty() {
        print!("{}", output.stdout);
        io::stdout().flush().ok();
    }
    if !output.stderr.is_empty() {
        eprint!("{}", output.stderr);
        io::stderr().flush().ok();
    }

    Ok(true)
}

fn parse_target_path() -> Result<String> {
    let mut args = std::env::args();
    let _ = args.next();
    match (args.next(), args.next()) {
        (Some(path), None) => Ok(path),
        (None, None) => {
            eprintln!("No path provided, defaulting to README.md");
            Ok(String::from("README.md"))
        }
        _ => bail!("Usage: runbook-cli <path-to-markdown>"),
    }
}

fn read_contents(resolver: &PathResolver, path: &GuardedPath) -> Result<String> {
    resolver
        .read_to_string(path)
        .with_context(|| format!("read {}", path.display()))
}

fn emit_block_events_enabled() -> bool {
    match std::env::var_os(BLOCK_EVENT_ENV) {
        Some(val) => val != "0",
        None => false,
    }
}

pub(crate) fn read_stable_contents(resolver: &PathResolver, path: &GuardedPath) -> Result<String> {
    let mut last = read_contents(resolver, path)?;
    for _ in 0..STABLE_READ_RETRIES {
        std::thread::sleep(STABLE_READ_DELAY);
        let next = read_contents(resolver, path)?;
        if next == last {
            return Ok(next);
        }
        last = next;
    }
    Ok(last)
}

fn run_watch_loop(
    resolver: &PathResolver,
    workspace_root: &GuardedPath,
    watched: &GuardedPath,
    source_dir: &GuardedPath,
    cache: &mut RunnerCache,
    last_contents: &mut String,
    command_rx: Option<mpsc::Receiver<ServerCommand>>,
    emit_block_events: Arc<AtomicBool>,
    run_control: Arc<RunControl>,
) -> Result<()> {
    let (tx, rx) = mpsc::channel();
    let mut watcher = RecommendedWatcher::new(
        move |result| {
            let _ = tx.send(result);
        },
        notify::Config::default(),
    )
    .context("initialize file watcher")?;

    let watch_root = watched.parent().unwrap_or_else(|| watched.clone());
    watcher
        .watch(watch_root.as_path(), RecursiveMode::NonRecursive)
        .with_context(|| format!("watch {}", watch_root.display()))?;

    loop {
        drain_server_commands(
            command_rx.as_ref(),
            resolver,
            workspace_root,
            watched,
            source_dir,
            cache,
            last_contents,
            &emit_block_events,
            &run_control,
        )?;

        let mut should_process = false;
        match rx.recv_timeout(COMMAND_POLL_INTERVAL) {
            Ok(Ok(event)) => {
                if event_maybe_affects(&event, watched) && is_relevant_kind(&event) {
                    should_process = true;
                }
            }
            Ok(Err(err)) => {
                eprintln!("watch error: {err}");
                continue;
            }
            Err(mpsc::RecvTimeoutError::Timeout) => continue,
            Err(mpsc::RecvTimeoutError::Disconnected) => {
                return Err(anyhow::anyhow!("watcher channel closed"));
            }
        }

        if !should_process {
            continue;
        }

        loop {
            drain_server_commands(
                command_rx.as_ref(),
                resolver,
                workspace_root,
                watched,
                source_dir,
                cache,
                last_contents,
                &emit_block_events,
                &run_control,
            )?;
            match rx.recv_timeout(WATCH_DEBOUNCE_WINDOW) {
                Ok(Ok(event)) => {
                    if event_maybe_affects(&event, watched) && is_relevant_kind(&event) {
                        continue;
                    }
                }
                Ok(Err(err)) => {
                    eprintln!("watch error: {err}");
                }
                Err(mpsc::RecvTimeoutError::Timeout) => break,
                Err(mpsc::RecvTimeoutError::Disconnected) => {
                    return Err(anyhow::anyhow!("watcher channel closed"));
                }
            }
        }

        if let Ok(new_contents) = read_stable_contents(resolver, watched)
            && new_contents != *last_contents
        {
            report_fence_changes(watched, last_contents, &new_contents);
            let rendered = {
                let _guard = run_control.begin();
                render_shell_outputs(
                    &new_contents,
                    resolver,
                    workspace_root,
                    source_dir,
                    cache,
                    false,
                    None,
                    None,
                    emit_block_events.load(Ordering::Relaxed),
                    Some(run_control.token()),
                    None,
                )?
            };
            if rendered != new_contents {
                resolver
                    .write_file(watched, rendered.as_bytes())
                    .with_context(|| format!("write {}", watched.display()))?;
            }
            *last_contents = rendered;
        }
    }
}

fn drain_server_commands(
    command_rx: Option<&mpsc::Receiver<ServerCommand>>,
    resolver: &PathResolver,
    workspace_root: &GuardedPath,
    watched: &GuardedPath,
    source_dir: &GuardedPath,
    cache: &mut RunnerCache,
    last_contents: &mut String,
    emit_block_events: &Arc<AtomicBool>,
    run_control: &Arc<RunControl>,
) -> Result<()> {
    if let Some(rx) = command_rx {
        while let Ok(cmd) = rx.try_recv() {
            handle_server_command(
                cmd,
                resolver,
                workspace_root,
                watched,
                source_dir,
                cache,
                last_contents,
                emit_block_events,
                run_control,
            )?;
        }
    }
    Ok(())
}

fn handle_server_command(
    command: ServerCommand,
    resolver: &PathResolver,
    workspace_root: &GuardedPath,
    watched: &GuardedPath,
    source_dir: &GuardedPath,
    cache: &mut RunnerCache,
    last_contents: &mut String,
    emit_block_events: &Arc<AtomicBool>,
    run_control: &Arc<RunControl>,
) -> Result<()> {
    match command {
        ServerCommand::RunBlock { line } => {
            let _guard = run_control.begin();
            let target_line = line.max(1);
            println!("{WORKER_EVENT_PREFIX} START {target_line}");
            io::stdout().flush().ok();

            let result = run_block_in_place(
                resolver,
                workspace_root,
                watched,
                source_dir,
                cache,
                last_contents,
                target_line,
                emit_block_events.clone(),
                Some(run_control.token()),
                None,
            )?;

            if result.is_none() {
                println!("No code block covering line {target_line}");
            }

            let success = result.as_ref().map(|exec| exec.success).unwrap_or(false);
            println!(
                "{WORKER_EVENT_PREFIX} DONE {target_line} {}",
                if success { "OK" } else { "ERR" }
            );
            io::stdout().flush().ok();
        }
    }
    Ok(())
}

pub(crate) fn run_block_in_place(
    resolver: &PathResolver,
    workspace_root: &GuardedPath,
    watched: &GuardedPath,
    source_dir: &GuardedPath,
    cache: &mut RunnerCache,
    last_contents: &mut String,
    target_line: usize,
    emit_block_events: Arc<AtomicBool>,
    cancel_token: Option<&CancellationToken>,
    output_observer: Option<Arc<dyn ExecutionOutputObserver>>,
) -> Result<Option<BlockExecution>> {
    let contents = read_stable_contents(resolver, watched)?;
    let fences = parse_fences(&contents);
    let block = match fences
        .into_iter()
        .find(|f| f.start_line <= target_line && f.end_line >= target_line)
    {
        Some(block) => block,
        None => return Ok(None),
    };

    let mut forced_lines = HashSet::new();
    for line in block.start_line..=block.end_line {
        forced_lines.insert(line);
    }

    let mut matched: Option<BlockExecution> = None;
    {
        let mut callback = |exec: BlockExecution| {
            if exec.start_line <= target_line && exec.end_line >= target_line {
                matched = Some(exec.clone());
            }
        };

        let rendered = render_shell_outputs(
            &contents,
            resolver,
            workspace_root,
            source_dir,
            cache,
            true,
            Some(&forced_lines),
            Some(&mut callback),
            emit_block_events.load(Ordering::Relaxed),
            cancel_token,
            output_observer.clone(),
        )?;

        if rendered != contents {
            resolver
                .write_file(watched, rendered.as_bytes())
                .with_context(|| format!("write {}", watched.display()))?;
            *last_contents = rendered;
        } else {
            *last_contents = contents;
        }
    }

    if matched.is_none() {
        matched = Some(BlockExecution {
            start_line: block.start_line,
            end_line: block.end_line,
            success: false,
            stdout: String::new(),
            stderr: String::from("block execution did not produce a result"),
        });
    }

    Ok(matched)
}

fn spawn_command_listener(
    tx: mpsc::Sender<ServerCommand>,
    emit_block_events: Arc<AtomicBool>,
    run_control: Arc<RunControl>,
) {
    std::thread::spawn(move || {
        let stdin = io::stdin();
        let mut reader = io::BufReader::new(stdin.lock());
        let mut line = String::new();
        loop {
            line.clear();
            match reader.read_line(&mut line) {
                Ok(0) => break,
                Ok(_) => {
                    let trimmed = line.trim();
                    if trimmed.is_empty() {
                        continue;
                    }
                    if let Some(rest) = trimmed.strip_prefix("RUN") {
                        let value = rest.trim();
                        match value.parse::<usize>() {
                            Ok(line_no) => {
                                let _ = tx.send(ServerCommand::RunBlock { line: line_no });
                            }
                            Err(_) => eprintln!("invalid RUN command: {trimmed}"),
                        }
                    } else if trimmed.eq_ignore_ascii_case("EVENTS ON") {
                        emit_block_events.store(true, Ordering::Relaxed);
                    } else if trimmed.eq_ignore_ascii_case("EVENTS OFF") {
                        emit_block_events.store(false, Ordering::Relaxed);
                    } else if trimmed.eq_ignore_ascii_case("STOP") {
                        if run_control.request_cancel() {
                            println!("Stop requested");
                        } else {
                            println!("Stop requested but no block running");
                        }
                    } else {
                        eprintln!("unknown command: {trimmed}");
                    }
                }
                Err(_) => break,
            }
        }
    });
}

fn is_relevant_kind(event: &Event) -> bool {
    matches!(
        event.kind,
        EventKind::Modify(_) | EventKind::Create(_) | EventKind::Remove(_)
    )
}

#[allow(clippy::disallowed_types)]
fn event_maybe_affects(event: &Event, watched: &GuardedPath) -> bool {
    if event.paths.is_empty() {
        return true;
    }
    let watched_name = watched.as_path().file_name();
    event.paths.iter().any(|path| {
        path == watched.as_path()
            || watched_name
                .map(|name| path.file_name() == Some(name))
                .unwrap_or(false)
    })
}

fn report_fence_changes(path: &GuardedPath, before: &str, after: &str) {
    let before_fences = parse_fences(before);
    let after_fences = parse_fences(after);
    if before_fences.is_empty() && after_fences.is_empty() {
        return;
    }

    let mut changed = Vec::new();
    let max_len = before_fences.len().max(after_fences.len());
    for idx in 0..max_len {
        match (before_fences.get(idx), after_fences.get(idx)) {
            (Some(before), Some(after)) => {
                if before.info != after.info || before.content != after.content {
                    changed.push((idx + 1, Some(before), Some(after)));
                }
            }
            (Some(before), None) => changed.push((idx + 1, Some(before), None)),
            (None, Some(after)) => changed.push((idx + 1, None, Some(after))),
            (None, None) => {}
        }
    }

    if changed.is_empty() {
        return;
    }

    println!("Fence changes for {}", path.display());
    for (idx, before, after) in changed {
        let label = fence_label(before, after);
        match (before, after) {
            (Some(before), Some(after)) => {
                println!(
                    "  #{idx} {label}: lines {}-{} -> {}-{}",
                    before.start_line, before.end_line, after.start_line, after.end_line
                );
                if before.info != after.info {
                    println!(
                        "       info: {} -> {}",
                        display_info(&before.info),
                        display_info(&after.info)
                    );
                }
            }
            (Some(before), None) => {
                println!(
                    "  #{idx} {label}: removed (lines {}-{})",
                    before.start_line, before.end_line
                );
            }
            (None, Some(after)) => {
                println!(
                    "  #{idx} {label}: added (lines {}-{})",
                    after.start_line, after.end_line
                );
            }
            (None, None) => {}
        }
    }
}

fn fence_label(before: Option<&FenceBlock>, after: Option<&FenceBlock>) -> String {
    let info = before
        .and_then(|block| {
            if block.info.is_empty() {
                None
            } else {
                Some(block.info.as_str())
            }
        })
        .or_else(|| {
            after.and_then(|block| {
                if block.info.is_empty() {
                    None
                } else {
                    Some(block.info.as_str())
                }
            })
        })
        .unwrap_or("plain");
    format!("({info})")
}

fn display_info(info: &str) -> String {
    if info.is_empty() {
        String::from("plain")
    } else {
        info.to_string()
    }
}

fn parse_fences(input: &str) -> Vec<FenceBlock> {
    let mut fences = Vec::new();
    let mut current: Option<FenceBlock> = None;

    for (line_idx, line) in input.lines().enumerate() {
        let line_no = line_idx + 1;
        let (ws, rest) = split_leading_ws(line);
        if ws > MAX_FENCE_PREFIX_WS {
            if let Some(block) = current.as_mut() {
                block.content.push_str(line);
                block.content.push('\n');
            }
            continue;
        }

        if let Some((fence_char, fence_len, info)) = parse_fence_start(rest) {
            match current.take() {
                Some(mut open) => {
                    if fence_char == open.fence && fence_len >= open.fence_len {
                        open.end_line = line_no;
                        fences.push(open);
                    } else {
                        open.content.push_str(line);
                        open.content.push('\n');
                        current = Some(open);
                    }
                }
                None => {
                    let block = FenceBlock {
                        fence: fence_char,
                        fence_len,
                        info,
                        start_line: line_no,
                        end_line: line_no,
                        content: String::new(),
                    };
                    current = Some(block);
                }
            }
            continue;
        }

        if let Some(block) = current.as_mut() {
            block.content.push_str(line);
            block.content.push('\n');
        }
    }

    if let Some(mut open) = current.take() {
        open.end_line = input.lines().count().max(open.start_line);
        fences.push(open);
    }

    fences
}

fn parse_fence_start(line: &str) -> Option<(char, usize, String)> {
    let trimmed = line.trim_end();
    let mut chars = trimmed.chars();
    let fence_char = chars.next()?;
    if fence_char != '`' && fence_char != '~' {
        return None;
    }

    let mut count = 1;
    for ch in chars.by_ref() {
        if ch == fence_char {
            count += 1;
        } else {
            break;
        }
    }
    if count < 3 {
        return None;
    }

    let rest = &trimmed[count..];
    let info = rest.trim().to_string();
    Some((fence_char, count, info))
}

fn split_leading_ws(line: &str) -> (usize, &str) {
    let mut count = 0;
    for ch in line.chars() {
        if ch == ' ' || ch == '\t' {
            count += 1;
        } else {
            break;
        }
    }
    (count, &line[count..])
}

fn render_shell_outputs(
    contents: &str,
    resolver: &PathResolver,
    workspace_root: &GuardedPath,
    source_dir: &GuardedPath,
    cache: &mut RunnerCache,
    only_missing_outputs: bool,
    forced_lines: Option<&HashSet<usize>>,
    on_block_run: Option<&mut dyn FnMut(BlockExecution)>,
    emit_block_events: bool,
    cancel_token: Option<&CancellationToken>,
    output_observer: Option<Arc<dyn ExecutionOutputObserver>>,
) -> Result<String> {
    let lines: Vec<&str> = contents.lines().collect();
    use comrak::{Arena, nodes::NodeValue, parse_document};

    let arena = Arena::new();
    let mut options = comrak::Options::default();
    options.render.sourcepos = true;
    let root = parse_document(&arena, contents, &options);

    // Collect code block nodes with their source line ranges.
    let mut blocks: Vec<(usize, usize, String, String)> = Vec::new(); // (start_line, end_line, info, script)
    for node in root.descendants() {
        let n = node.data.borrow();
        if let NodeValue::CodeBlock(cb) = &n.value {
            // comrak sets source positions when `options.parse.sourcepos` is true.
            let sp = &n.sourcepos;
            // comrak sourcepos lines are 1-based inclusive
            let start = sp.start.line.saturating_sub(1);
            let end = sp.end.line.saturating_sub(1);
            let info = cb.info.clone();
            let script = cb.literal.clone();
            blocks.push((start, end, info.trim().to_string(), script));
        }
    }

    // Sort by start line
    blocks.sort_by_key(|b| b.0);

    let mut out_lines: Vec<String> = Vec::new();
    let mut prev_reader: Option<SharedInput> = None;
    let mut cursor: usize = 0;
    let mut on_block_run = on_block_run;

    for (start, end, info, script) in blocks {
        // If this block ends before the current cursor, it's already been
        // consumed by a previous step (or overlaps); skip it to avoid
        // attaching outputs to the wrong fence.
        if end < cursor {
            continue;
        }
        if start >= lines.len() {
            break;
        }

        // Clamp start to cursor so we don't re-emit lines we've already
        // written when sourcepos spans slightly earlier than expected.
        let start_clamped = std::cmp::max(start, cursor);

        // Copy lines up to the fence
        for ln in cursor..start_clamped {
            out_lines.push(lines[ln].to_string());
        }

        // Copy the original fence lines (clamped)
        let fence_end = end.min(lines.len().saturating_sub(1));
        if fence_end >= start_clamped {
            for ln in start_clamped..=fence_end {
                out_lines.push(lines[ln].to_string());
            }
        }

        // Determine existing output block after the fence
        let next_index = fence_end + 1;
        let existing = if next_index <= lines.len() {
            parse_output_block(&lines, next_index)
        } else {
            None
        };

        let block_start_line = start + 1;
        let block_end_line = end + 1;
        let forced = forced_lines.map_or(false, |set| {
            (block_start_line..=block_end_line).any(|ln| set.contains(&ln))
        });

        if let Some(spec) = runner_spec(&info, resolver, workspace_root, source_dir, cache)? {
            let code_hash = code_hash(&script, &spec);
            let mut should_run = match &existing {
                Some(block) => {
                    let code_hash_short = short_hash(&code_hash);
                    let matches_code = block.code_hash.as_deref() == Some(&code_hash)
                        || block.code_hash.as_deref() == Some(&code_hash_short);
                    if only_missing_outputs {
                        !matches_code
                    } else {
                        let stdout_norm = normalize_output(&block.stdout);
                        let stderr_norm = normalize_output(&block.stderr);
                        let expected_combined_hash =
                            combined_output_hash(&stdout_norm, &stderr_norm);
                        let expected_combined_short = short_hash(&expected_combined_hash);
                        let matches_output = if let Some(meta_hash) = block.combined_hash.as_deref()
                        {
                            meta_hash == expected_combined_hash
                                || meta_hash == expected_combined_short
                        } else {
                            let expected_stdout_hash = sha256_hex(&stdout_norm);
                            let expected_stderr_hash = sha256_hex(&stderr_norm);
                            let matches_stdout =
                                block.stdout_hash.as_deref() == Some(&expected_stdout_hash);
                            let matches_stderr =
                                block.stderr_hash.as_deref() == Some(&expected_stderr_hash);
                            matches_stdout && matches_stderr
                        };
                        !(matches_code && matches_output)
                    }
                }
                None => true,
            };

            if forced {
                should_run = true;
            }

            if should_run {
                if cancel_token.map_or(false, |token| token.is_cancelled()) {
                    let output_block = format_output_block(&code_hash, "", "execution cancelled");
                    out_lines.extend(output_block);
                    cursor = existing.map(|b| b.end_index).unwrap_or(fence_end + 1);
                    if let Some(cb) = on_block_run.as_mut() {
                        cb(BlockExecution {
                            start_line: block_start_line,
                            end_line: block_end_line,
                            success: false,
                            stdout: String::new(),
                            stderr: String::from("execution cancelled"),
                        });
                    }
                    break;
                }
                if emit_block_events {
                    println!("{WORKER_EVENT_PREFIX} START {block_start_line}");
                    io::stdout().flush().ok();
                }

                // Prepare stdin from previous fence
                let stdin_stream = prev_reader.take();

                let (reader, writer) =
                    pipe().with_context(|| "create pipe for piping stdout to next fence")?;
                let reader_shared: SharedInput = Arc::new(Mutex::new(reader));
                let writer_shared: Arc<Mutex<dyn std::io::Write + Send>> =
                    Arc::new(Mutex::new(writer));

                let capture_stdout: Arc<Mutex<Vec<u8>>> = Arc::new(Mutex::new(Vec::new()));
                let capture_stderr: Arc<Mutex<Vec<u8>>> = Arc::new(Mutex::new(Vec::new()));

                struct TeeWriter {
                    cap: Arc<Mutex<Vec<u8>>>,
                    pipe: Arc<Mutex<dyn std::io::Write + Send>>,
                    observer: Option<Arc<dyn ExecutionOutputObserver>>,
                }
                impl std::io::Write for TeeWriter {
                    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
                        if let Some(obs) = &self.observer {
                            obs.on_stdout(buf);
                        }
                        if let Ok(mut g) = self.cap.lock() {
                            let _ = std::io::Write::write_all(&mut *g, buf);
                        }
                        if let Ok(mut p) = self.pipe.lock() {
                            let _ = std::io::Write::write_all(&mut *p, buf);
                        }
                        Ok(buf.len())
                    }
                    fn flush(&mut self) -> std::io::Result<()> {
                        if let Ok(mut g) = self.cap.lock() {
                            let _ = std::io::Write::flush(&mut *g);
                        }
                        if let Ok(mut p) = self.pipe.lock() {
                            let _ = std::io::Write::flush(&mut *p);
                        }
                        Ok(())
                    }
                }

                struct CaptureWriter {
                    cap: Arc<Mutex<Vec<u8>>>,
                    observer: Option<Arc<dyn ExecutionOutputObserver>>,
                }
                impl std::io::Write for CaptureWriter {
                    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
                        if let Some(obs) = &self.observer {
                            obs.on_stderr(buf);
                        }
                        if let Ok(mut g) = self.cap.lock() {
                            let _ = std::io::Write::write_all(&mut *g, buf);
                        }
                        Ok(buf.len())
                    }
                    fn flush(&mut self) -> std::io::Result<()> {
                        if let Ok(mut g) = self.cap.lock() {
                            let _ = std::io::Write::flush(&mut *g);
                        }
                        Ok(())
                    }
                }

                let tee = TeeWriter {
                    cap: capture_stdout.clone(),
                    pipe: writer_shared.clone(),
                    observer: output_observer.clone(),
                };
                let stdout_writer: SharedOutput = Arc::new(Mutex::new(tee));
                let stderr_writer: SharedOutput = Arc::new(Mutex::new(CaptureWriter {
                    cap: capture_stderr.clone(),
                    observer: output_observer.clone(),
                }));

                let run_res = run_runner(
                    resolver,
                    workspace_root,
                    source_dir,
                    cache,
                    &spec,
                    &script,
                    stdin_stream,
                    Some(stdout_writer.clone()),
                    Some(stderr_writer.clone()),
                    Some(capture_stdout.clone()),
                    Some(capture_stderr.clone()),
                    cancel_token,
                );

                let mut block_success = false;
                match run_res {
                    Ok(run_output) => {
                        prev_reader = Some(reader_shared);
                        let output_block =
                            format_output_block(&code_hash, &run_output.stdout, &run_output.stderr);
                        out_lines.extend(output_block);
                        // advance cursor past any existing output if present
                        cursor = existing.map(|b| b.end_index).unwrap_or(fence_end + 1);
                        block_success = true;
                        if let Some(cb) = on_block_run.as_mut() {
                            cb(BlockExecution {
                                start_line: block_start_line,
                                end_line: block_end_line,
                                success: true,
                                stdout: run_output.stdout.clone(),
                                stderr: run_output.stderr.clone(),
                            });
                        }
                    }
                    Err(err) => {
                        let cancelled_block = err.downcast_ref::<CommandCancelled>().is_some();
                        let stdout_output = {
                            let data = capture_stdout.lock().unwrap();
                            String::from_utf8_lossy(&data).to_string()
                        };
                        let mut stderr_output = {
                            let data = capture_stderr.lock().unwrap();
                            String::from_utf8_lossy(&data).to_string()
                        };
                        if cancelled_block {
                            if !stderr_output.trim().is_empty() && !stderr_output.ends_with('\n') {
                                stderr_output.push('\n');
                            }
                            if stderr_output.trim().is_empty() {
                                stderr_output = String::from("execution cancelled");
                            } else {
                                stderr_output.push_str("execution cancelled");
                            }
                        } else {
                            let mut chain = Vec::new();
                            for (idx, cause) in err.chain().enumerate() {
                                if idx == 0 {
                                    chain.push(format!("error: {}", cause));
                                } else {
                                    chain.push(format!("caused by: {}", cause));
                                }
                            }
                            let err_msg = chain.join("\n");
                            if !stderr_output.is_empty() && !stderr_output.ends_with('\n') {
                                stderr_output.push('\n');
                            }
                            if !stderr_output.is_empty() {
                                stderr_output.push_str(&err_msg);
                            } else {
                                stderr_output = err_msg.clone();
                            }
                        }
                        let output_block =
                            format_output_block(&code_hash, &stdout_output, &stderr_output);
                        out_lines.extend(output_block);
                        cursor = existing.map(|b| b.end_index).unwrap_or(fence_end + 1);
                        if let Some(cb) = on_block_run.as_mut() {
                            cb(BlockExecution {
                                start_line: block_start_line,
                                end_line: block_end_line,
                                success: false,
                                stdout: stdout_output.clone(),
                                stderr: stderr_output.clone(),
                            });
                        }
                        if cancelled_block {
                            break;
                        }
                    }
                }

                if emit_block_events {
                    println!(
                        "{WORKER_EVENT_PREFIX} DONE {block_start_line} {}",
                        if block_success { "OK" } else { "ERR" }
                    );
                    io::stdout().flush().ok();
                }
            } else if let Some(block) = existing {
                out_lines.extend(
                    lines[block.start_index..block.end_index]
                        .iter()
                        .map(|l| (*l).to_string()),
                );
                let prev_buf = block.stdout.clone();
                let cursor_reader = Cursor::new(prev_buf.into_bytes());
                prev_reader = Some(Arc::new(Mutex::new(cursor_reader)));
                cursor = block.end_index;
            } else {
                // No existing block and not running: leave cursor after fence
                cursor = fence_end + 1;
            }
        } else {
            // No runner spec found. If the fence declared a language,
            // write an informative output block so users see that the
            // fence was not executed.
            let parsed = parse_fence_info(&info);
            if let Some(lang) = parsed.language {
                let code_hash = sha256_hex(&format!("{}\n{}", script, lang));
                let output = format!("error: no runner registered for language '{}'", lang);
                let output_block = format_output_block(&code_hash, "", &output);
                out_lines.extend(output_block);
                if forced {
                    if let Some(cb) = on_block_run.as_mut() {
                        cb(BlockExecution {
                            start_line: block_start_line,
                            end_line: block_end_line,
                            success: false,
                            stdout: String::new(),
                            stderr: output.clone(),
                        });
                    }
                }
            } else if forced {
                if let Some(cb) = on_block_run.as_mut() {
                    cb(BlockExecution {
                        start_line: block_start_line,
                        end_line: block_end_line,
                        success: false,
                        stdout: String::new(),
                        stderr: String::from("no runner configured for anonymous fence"),
                    });
                }
            }
            cursor = existing.map(|b| b.end_index).unwrap_or(fence_end + 1);
        }
    }

    // Append remaining lines
    for ln in cursor..lines.len() {
        out_lines.push(lines[ln].to_string());
    }

    let mut rendered = out_lines.join("\n");
    if contents.ends_with('\n') {
        rendered.push('\n');
    }
    Ok(rendered)
}

fn is_fence_close(line: &str, fence_char: char, fence_len: usize) -> bool {
    let trimmed = line.trim_end();
    let mut count = 0;
    for ch in trimmed.chars() {
        if ch == fence_char {
            count += 1;
        } else {
            break;
        }
    }
    if count < fence_len {
        return false;
    }
    trimmed[count..].trim().is_empty()
}

#[allow(unused_assignments)]
fn parse_output_block(lines: &[&str], start: usize) -> Option<OutputBlock> {
    let mut idx = start;
    while idx < lines.len() && lines[idx].trim().is_empty() {
        idx += 1;
    }
    if idx >= lines.len() {
        return None;
    }

    let mut has_begin_marker = false;
    let mut start_index = idx;
    if lines[idx].trim_end() == OUTPUT_BEGIN {
        has_begin_marker = true;
        start_index = idx;
        idx += 1;
        while idx < lines.len() && lines[idx].trim().is_empty() {
            idx += 1;
        }
    }

    let mut code_hash = None;
    let mut stdout_hash = None;
    let mut stderr_hash = None;
    let mut combined_hash = None;
    if idx < lines.len() && lines[idx].trim_start().starts_with(OUTPUT_META_PREFIX) {
        parse_output_meta(
            lines[idx].trim(),
            &mut code_hash,
            &mut stdout_hash,
            &mut stderr_hash,
            &mut combined_hash,
        );
        idx += 1;
    }
    while idx < lines.len() && lines[idx].trim().is_empty() {
        idx += 1;
    }

    if idx >= lines.len() || !lines[idx].trim_start().starts_with("```") {
        return None;
    }
    parse_inline_meta(lines[idx], &mut code_hash, &mut combined_hash);

    if !has_begin_marker
        && code_hash.is_none()
        && combined_hash.is_none()
        && stdout_hash.is_none()
        && stderr_hash.is_none()
    {
        return None;
    }

    let (stdout, mut idx) = parse_fenced_block(lines, idx)?;
    let mut stderr = String::new();
    let mut end_index = idx;

    // Look for an optional stderr block. In the legacy format this was
    // prefixed with a comment marker; the compact format omits the marker.
    let mut peek_idx = idx;
    while peek_idx < lines.len() && lines[peek_idx].trim().is_empty() {
        peek_idx += 1;
    }

    if peek_idx < lines.len() && lines[peek_idx].trim_start() == STDERR_MARKER {
        let mut stderr_start = peek_idx + 1;
        while stderr_start < lines.len() && lines[stderr_start].trim().is_empty() {
            stderr_start += 1;
        }
        if let Some((parsed_stderr, next_idx)) = parse_fenced_block(lines, stderr_start) {
            stderr = parsed_stderr;
            end_index = next_idx;
            idx = next_idx;
        }
    } else if peek_idx < lines.len()
        && lines[peek_idx].trim_start().starts_with("```")
        && let Some((parsed_stderr, next_idx)) = parse_fenced_block(lines, peek_idx)
    {
        stderr = parsed_stderr;
        end_index = next_idx;
        idx = next_idx;
    }

    if has_begin_marker {
        let mut end_seek = idx;
        while end_seek < lines.len() && lines[end_seek].trim().is_empty() {
            end_seek += 1;
        }
        if end_seek >= lines.len() || lines[end_seek].trim_end() != OUTPUT_END {
            return None;
        }
        end_index = end_seek + 1;
    }

    Some(OutputBlock {
        start_index,
        end_index,
        code_hash,
        stdout_hash,
        stderr_hash,
        combined_hash,
        stdout,
        stderr,
    })
}

fn parse_fenced_block(lines: &[&str], fence_idx: usize) -> Option<(String, usize)> {
    if fence_idx >= lines.len() || !lines[fence_idx].trim_start().starts_with("```") {
        return None;
    }

    let mut idx = fence_idx + 1;
    let mut body: Vec<&str> = Vec::new();
    while idx < lines.len() {
        if lines[idx].trim_end() == "```" {
            return Some((body.join("\n"), idx + 1));
        }
        body.push(lines[idx]);
        idx += 1;
    }
    None
}

fn parse_output_meta(
    line: &str,
    code_hash: &mut Option<String>,
    stdout_hash: &mut Option<String>,
    stderr_hash: &mut Option<String>,
    combined_hash: &mut Option<String>,
) {
    let trimmed = line
        .trim_start_matches("<!--")
        .trim_end_matches("-->")
        .trim();
    let tokens: Vec<&str> = trimmed.split_whitespace().collect();
    for token in tokens {
        if let Some(value) = token.strip_prefix("code=") {
            *code_hash = Some(value.to_string());
        } else if let Some(value) = token.strip_prefix("stdout=") {
            *stdout_hash = Some(value.to_string());
        } else if let Some(value) = token.strip_prefix("stderr=") {
            *stderr_hash = Some(value.to_string());
        } else if let Some(value) = token.strip_prefix("hash=") {
            *combined_hash = Some(value.to_string());
        } else if let Some(value) = token.strip_prefix("output=") {
            // Backwards-compatible: single output hash stored as stdout and combined.
            *stdout_hash = Some(value.to_string());
            combined_hash.get_or_insert_with(|| value.to_string());
        }
    }
}

fn parse_inline_meta(
    line: &str,
    code_hash: &mut Option<String>,
    combined_hash: &mut Option<String>,
) -> bool {
    let trimmed = line.trim_start();
    if !trimmed.starts_with("```") {
        return false;
    }
    let mut tokens = trimmed.trim_start_matches('`').split_whitespace();
    // Skip the fence language token if present.
    let _ = tokens.next();
    let mut saw_runbook = false;
    for token in tokens {
        if token == "runbook" {
            saw_runbook = true;
            continue;
        }
        if let Some(value) = token.strip_prefix("code=") {
            code_hash.get_or_insert_with(|| value.to_string());
        } else if let Some(value) = token.strip_prefix("hash=") {
            combined_hash.get_or_insert_with(|| value.to_string());
        }
    }
    saw_runbook
}

fn format_output_block(code_hash: &str, stdout: &str, stderr: &str) -> Vec<String> {
    let mut lines = Vec::new();
    let stdout_norm = normalize_output(stdout);
    let stderr_norm = normalize_output(stderr);
    let combined_hash = combined_output_hash(&stdout_norm, &stderr_norm);
    let code_hash_short = short_hash(code_hash);
    let combined_hash_short = short_hash(&combined_hash);
    // First fenced block: stdout (may be empty). Keep it minimal for humans.
    lines.push(format!(
        "```text runbook code={code_hash_short} hash={combined_hash_short}"
    ));
    if !stdout_norm.is_empty() {
        for line in stdout_norm.lines() {
            lines.push(line.to_string());
        }
    }
    lines.push("```".to_string());

    // If stderr is present, emit a second fenced block that is visibly marked
    // as an error for humans reading the Markdown in a terminal.
    if !stderr_norm.is_empty() {
        lines.push(String::new());
        lines.push(format!(
            "```text runbook code={code_hash_short} hash={combined_hash_short}"
        ));
        let mut s_lines = stderr_norm.lines();
        if let Some(first) = s_lines.next() {
            lines.push(format!("ERROR: {first}"));
            for line in s_lines {
                lines.push(line.to_string());
            }
        }
        lines.push("```".to_string());
    }
    lines
}

fn combined_output_hash(stdout_norm: &str, stderr_norm: &str) -> String {
    // Hash stdout and stderr together so we can track changes with a single token.
    let mut hasher = Sha256::new();
    hasher.update(stdout_norm.as_bytes());
    hasher.update(b"\n--stderr--\n");
    hasher.update(stderr_norm.as_bytes());
    let digest = hasher.finalize();
    format!("{digest:x}")
}

fn sha256_hex(input: &str) -> String {
    let mut hasher = Sha256::new();
    hasher.update(input.as_bytes());
    let digest = hasher.finalize();
    format!("{digest:x}")
}

fn short_hash(full_hex: &str) -> String {
    full_hex.chars().take(SHORT_HASH_LEN).collect()
}

fn normalize_output(output: &str) -> String {
    output.trim_end_matches('\n').to_string()
}

fn parse_fence_info(info: &str) -> FenceInfo {
    let mut language = None;
    let mut params = HashMap::new();
    for token in info.split_whitespace() {
        if let Some((key, value)) = token.split_once('=') {
            params.insert(key.to_string(), value.trim_matches('"').to_string());
        } else if language.is_none() {
            language = Some(token.to_string());
        }
    }
    if let Some(lang) = params.get("lang") {
        language = Some(lang.clone());
    }
    FenceInfo { language, params }
}

fn runner_spec(
    info: &str,
    resolver: &PathResolver,
    workspace_root: &GuardedPath,
    source_dir: &GuardedPath,
    cache: &mut RunnerCache,
) -> Result<Option<RunnerSpec>> {
    let parsed = parse_fence_info(info);
    let language = match parsed.language {
        Some(lang) => lang,
        None => return Ok(None),
    };
    let oxfile = resolve_oxfile_path(
        resolver,
        workspace_root,
        source_dir,
        parsed.params.get("oxfile"),
        &language,
    )?;
    let env_hash = match &oxfile {
        Some(path) => env_hash_for_oxfile(resolver, path, cache)?,
        None => None,
    };
    let command = match parsed.params.get("cmd") {
        Some(s) if !s.is_empty() => parse_command_parts(Some(s), &language),
        _ => Vec::new(),
    };
    if command.is_empty() && oxfile.is_none() {
        return Ok(None);
    }
    Ok(Some(RunnerSpec {
        language,
        command,
        oxfile,
        env_hash,
    }))
}

fn parse_command_parts(cmd: Option<&String>, _language: &str) -> Vec<String> {
    match cmd {
        Some(value) => value
            .split(',')
            .map(|part| part.trim().to_string())
            .filter(|part| !part.is_empty())
            .collect(),
        None => Vec::new(),
    }
}

fn resolve_oxfile_path(
    resolver: &PathResolver,
    workspace_root: &GuardedPath,
    source_dir: &GuardedPath,
    explicit: Option<&String>,
    language: &str,
) -> Result<Option<GuardedPath>> {
    if let Some(path) = explicit {
        let guarded = resolver
            .resolve_read(source_dir, path)
            .with_context(|| format!("resolve oxfile {path}"))?;
        return Ok(Some(guarded));
    }

    let filename = format!("runbook.{language}.oxfile");
    let local = source_dir.join(&filename)?;
    if resolver.entry_kind(&local).is_ok() {
        return Ok(Some(local));
    }

    let workspace = workspace_root.join(&filename)?;
    if resolver.entry_kind(&workspace).is_ok() {
        return Ok(Some(workspace));
    }

    // Fallback: also look for the temp.runner naming convention to support
    // in-repo runners without requiring the runbook.<lang> alias.
    let temp_name_runner = format!("temp.runner.{language}.oxfile");
    let temp_local_runner = source_dir.join(&temp_name_runner)?;
    if resolver.entry_kind(&temp_local_runner).is_ok() {
        return Ok(Some(temp_local_runner));
    }
    let temp_workspace_runner = workspace_root.join(&temp_name_runner)?;
    if resolver.entry_kind(&temp_workspace_runner).is_ok() {
        return Ok(Some(temp_workspace_runner));
    }

    // Check registry for a language-specific oxfile override.
    if let Some(registered) = get_registered_oxfile(language) {
        if let Ok(guarded) = resolver.resolve_read(workspace_root, &registered) {
            return Ok(Some(guarded));
        }
        if let Ok(guarded) = resolver.resolve_read(source_dir, &registered) {
            return Ok(Some(guarded));
        }
    }

    Ok(None)
}

// Simple global registry allowing external code to register a language -> oxfile
// mapping. The registered path is resolved relative to the workspace or the
// source directory when used.
static LANGUAGE_OXFILE_REGISTRY: Lazy<Mutex<HashMap<String, String>>> =
    Lazy::new(|| Mutex::new(HashMap::new()));

pub fn register_language_oxfile(language: &str, path: &str) {
    let mut reg = LANGUAGE_OXFILE_REGISTRY.lock().unwrap();
    reg.insert(language.to_string(), path.to_string());
}

fn get_registered_oxfile(language: &str) -> Option<String> {
    let reg = LANGUAGE_OXFILE_REGISTRY.lock().unwrap();
    reg.get(language).cloned()
}

// Scan the workspace for `temp.runner.<lang>.oxfile` files and register them.
#[allow(
    clippy::disallowed_types,
    clippy::disallowed_methods,
    clippy::collapsible_if
)]
pub(crate) fn scan_and_register_runners(workspace_root: &GuardedPath) {
    let root = workspace_root.root();
    let mut stack = vec![root.to_path_buf()];
    while let Some(dir) = stack.pop() {
        let rd = match std::fs::read_dir(&dir) {
            Ok(r) => r,
            Err(_) => continue,
        };
        for entry in rd.flatten() {
            let path = entry.path();
            if path.is_dir() {
                stack.push(path);
                continue;
            }
            let name = match path.file_name().and_then(|s| s.to_str()) {
                Some(n) => n,
                None => continue,
            };
            const PREFIX_RUNNER: &str = "temp.runner.";
            const SUFFIX: &str = ".oxfile";
            if name.starts_with(PREFIX_RUNNER) && name.ends_with(SUFFIX) {
                let pfx_len = PREFIX_RUNNER.len();
                let sfx_len = SUFFIX.len();
                let lang = &name[pfx_len..name.len() - sfx_len];
                if lang.is_empty() {
                    continue;
                }
                if let Ok(rel) = path.strip_prefix(root) {
                    if let Some(rel_str) = rel.to_str() {
                        let rel_str = to_forward_slashes(rel_str);
                        register_language_oxfile(lang, &rel_str);
                    }
                }
            }
        }
    }
}

struct EnvTee {
    buf: Arc<Mutex<Vec<u8>>>,
    term: Option<Stdout>,
}

impl std::io::Write for EnvTee {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        if let Ok(mut b) = self.buf.lock() {
            let _ = std::io::Write::write_all(&mut *b, buf);
        }
        if let Some(term) = &mut self.term {
            let _ = std::io::Write::write_all(term, buf);
        }
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        if let Ok(mut b) = self.buf.lock() {
            let _ = std::io::Write::flush(&mut *b);
        }
        if let Some(term) = &mut self.term {
            let _ = std::io::Write::flush(term);
        }
        Ok(())
    }
}

fn env_hash_for_oxfile(
    resolver: &PathResolver,
    path: &GuardedPath,
    cache: &mut RunnerCache,
) -> Result<Option<String>> {
    let key = path.display();
    let script = resolver
        .read_to_string(path)
        .with_context(|| format!("read {}", path.display()))?;
    let hash = sha256_hex(&script);
    let rebuild = match cache.envs.get(&key) {
        Some(env) => env.env_hash.as_deref() != Some(&hash),
        None => true,
    };
    if rebuild {
        let env = build_env_from_oxfile(resolver, path, &script, hash.clone())?;
        cache.envs.insert(key, env);
    }
    Ok(Some(hash))
}

fn build_env_from_oxfile(
    resolver: &PathResolver,
    path: &GuardedPath,
    script: &str,
    hash: String,
) -> Result<RunnerEnv> {
    // Expose runner directory relative to the workspace root so
    // temp oxfiles can resolve it inside the copied temp workspace without
    // host-absolute paths.
    let runner_dir = path.parent().unwrap_or_else(|| resolver.root().clone());
    let runner_rel = runner_dir
        .as_path()
        .strip_prefix(resolver.root().as_path())
        .map(|p| to_forward_slashes(&p.to_string_lossy()))
        .unwrap_or_else(|_| runner_dir.display().to_string());
    let steps = parse_script(script).with_context(|| format!("parse {}", path.display()))?;
    let tempdir =
        GuardedPath::tempdir().with_context(|| format!("tempdir for {}", path.display()))?;
    let temp_root = tempdir.as_guarded_path().clone();
    let build_context = resolver.root().clone();
    let output_buf = Arc::new(Mutex::new(Vec::new()));

    // Stream build/compile chatter to the user's terminal when available, while still
    // capturing for diagnostics. Allow opt-out via RUNBOOK_STREAM_STDOUT=0.
    let stream_build_to_terminal = match std::env::var_os(STREAM_STDOUT_ENV) {
        Some(val) => val != "0",
        None => std::io::stdout().is_terminal(),
    };

    let build_stdout: SharedOutput = if stream_build_to_terminal {
        Arc::new(Mutex::new(EnvTee {
            buf: output_buf.clone(),
            term: Some(std::io::stdout()),
        }))
    } else {
        output_buf.clone()
    };

    let mut io_cfg = ExecIo::new();
    io_cfg.insert_inherit_env("RUNBOOK_RUNNER_DIR", runner_rel.clone());
    io_cfg.set_stdout(Some(build_stdout.clone()));
    io_cfg.set_stderr(Some(build_stdout.clone()));
    io_cfg.insert_output_pipe_stdout_inherit(PIPE_SETUP);
    io_cfg.insert_output_pipe_stderr_inherit(PIPE_SETUP);
    io_cfg.insert_output_pipe(PIPE_SNIPPET, build_stdout.clone());
    let final_cwd =
        run_steps_with_context_result_with_io(&temp_root, &build_context, &steps, io_cfg)
            .with_context(|| format!("run {}", path.display()))?;

    Ok(RunnerEnv {
        root: temp_root,
        cwd: final_cwd,
        env_hash: Some(hash),
        _tempdir: Some(tempdir),
    })
}

struct RunnerOutput {
    stdout: String,
    stderr: String,
}

#[derive(Clone)]
struct BlockExecution {
    start_line: usize,
    end_line: usize,
    success: bool,
    stdout: String,
    stderr: String,
}

enum ServerCommand {
    RunBlock { line: usize },
}

#[allow(clippy::too_many_arguments)]
fn run_runner(
    resolver: &PathResolver,
    workspace_root: &GuardedPath,
    source_dir: &GuardedPath,
    cache: &mut RunnerCache,
    spec: &RunnerSpec,
    script: &str,
    stdin: Option<SharedInput>,
    stdout: Option<SharedOutput>,
    stderr: Option<SharedOutput>,
    stdout_capture: Option<Arc<Mutex<Vec<u8>>>>,
    stderr_capture: Option<Arc<Mutex<Vec<u8>>>>,
    cancel_token: Option<&CancellationToken>,
) -> Result<RunnerOutput> {
    let env = match &spec.oxfile {
        Some(path) => {
            let key = path.display();
            cache
                .envs
                .get(&key)
                .with_context(|| format!("missing env for {}", path.display()))?
        }
        None => {
            return run_in_default_env(
                resolver,
                workspace_root,
                source_dir,
                spec,
                script,
                stdin,
                stdout,
                stderr,
                stdout_capture,
                stderr_capture,
                cancel_token,
            );
        }
    };
    run_in_env(
        resolver,
        env,
        spec,
        script,
        stdin,
        stdout,
        stderr,
        stdout_capture,
        stderr_capture,
        cancel_token,
    )
}

#[allow(clippy::too_many_arguments)]
fn run_in_default_env(
    resolver: &PathResolver,
    workspace_root: &GuardedPath,
    source_dir: &GuardedPath,
    spec: &RunnerSpec,
    script: &str,
    stdin: Option<SharedInput>,
    stdout: Option<SharedOutput>,
    stderr: Option<SharedOutput>,
    stdout_capture: Option<Arc<Mutex<Vec<u8>>>>,
    stderr_capture: Option<Arc<Mutex<Vec<u8>>>>,
    cancel_token: Option<&CancellationToken>,
) -> Result<RunnerOutput> {
    let env = RunnerEnv {
        root: workspace_root.clone(),
        cwd: source_dir.clone(),
        env_hash: None,
        _tempdir: None,
    };
    run_in_env_with_resolver(
        resolver,
        &env,
        spec,
        script,
        stdin,
        stdout,
        stderr,
        stdout_capture,
        stderr_capture,
        cancel_token,
    )
}

#[allow(clippy::too_many_arguments)]
fn run_in_env(
    workspace_resolver: &PathResolver,
    env: &RunnerEnv,
    spec: &RunnerSpec,
    script: &str,
    stdin: Option<SharedInput>,
    stdout: Option<SharedOutput>,
    stderr: Option<SharedOutput>,
    stdout_capture: Option<Arc<Mutex<Vec<u8>>>>,
    stderr_capture: Option<Arc<Mutex<Vec<u8>>>>,
    cancel_token: Option<&CancellationToken>,
) -> Result<RunnerOutput> {
    run_in_env_with_resolver(
        workspace_resolver,
        env,
        spec,
        script,
        stdin,
        stdout,
        stderr,
        stdout_capture,
        stderr_capture,
        cancel_token,
    )
}

#[allow(clippy::too_many_arguments)]
fn run_in_env_with_resolver(
    workspace_resolver: &PathResolver,
    env: &RunnerEnv,
    spec: &RunnerSpec,
    script: &str,
    stdin: Option<SharedInput>,
    stdout: Option<SharedOutput>,
    stderr: Option<SharedOutput>,
    stdout_capture: Option<Arc<Mutex<Vec<u8>>>>,
    stderr_capture: Option<Arc<Mutex<Vec<u8>>>>,
    cancel_token: Option<&CancellationToken>,
) -> Result<RunnerOutput> {
    if let Some(oxfile_path) = &spec.oxfile {
        let oxfile_content = workspace_resolver
            .read_to_string(oxfile_path)
            .with_context(|| format!("read {}", oxfile_path.display()))?;
        let steps = parse_script(&oxfile_content)
            .with_context(|| format!("parse {}", oxfile_path.display()))?;
        // Make runner location available to oxfiles so they can be path-agnostic.
        let runner_dir = oxfile_path.parent().unwrap_or_else(|| env.root.clone());
        let runner_dir_value = runner_dir.display().to_string();

        // Persist the snippet so runners can execute the file while
        // receiving the previous fence's stdout via stdin.
        let lang_safe: String = spec
            .language
            .chars()
            .map(|c| {
                if c.is_ascii_alphanumeric() || c == '-' || c == '_' {
                    c
                } else {
                    '_'
                }
            })
            .collect();
        let snippet_name = format!("runbook-snippet.{lang_safe}");
        let snippets_dir = workspace_resolver
            .root()
            .join("target")?
            .join("runbook")?
            .join("snippets")?;
        workspace_resolver
            .create_dir_all(&snippets_dir)
            .with_context(|| format!("create snippets dir at {}", snippets_dir.display()))?;
        let snippet_path = snippets_dir
            .join(&snippet_name)
            .with_context(|| format!("snippet path for {}", spec.language))?;
        workspace_resolver
            .write_file(&snippet_path, script.as_bytes())
            .with_context(|| format!("write {}", snippet_path.display()))?;

        // Use the previous fence's stdout (if any) as stdin; the snippet
        // itself is provided via RUNBOOK_SNIPPET_PATH.
        let input_stream = stdin;

        // Prepare stdout: if provided, use it; otherwise create a capture
        // buffer and use that for stdout so we can return captured output.
        let (use_stdout, internal_stdout): (SharedOutput, Option<Arc<Mutex<Vec<u8>>>>) =
            match stdout {
                Some(s) => (s, None),
                None => {
                    let buf = Arc::new(Mutex::new(Vec::new()));

                    // Check if we should stream to terminal
                    let stream_to_terminal = match std::env::var_os(STREAM_STDOUT_ENV) {
                        Some(val) => val != "0",
                        None => std::io::stdout().is_terminal(),
                    };

                    let out: SharedOutput = if stream_to_terminal {
                        Arc::new(Mutex::new(EnvTee {
                            buf: buf.clone(),
                            term: Some(std::io::stdout()),
                        }))
                    } else {
                        buf.clone()
                    };
                    (out, Some(buf))
                }
            };

        let (use_stderr, internal_stderr): (SharedOutput, Option<Arc<Mutex<Vec<u8>>>>) =
            match stderr {
                Some(s) => (s, None),
                None => {
                    let buf = Arc::new(Mutex::new(Vec::new()));
                    (buf.clone(), Some(buf))
                }
            };

        let mut io_cfg = ExecIo::new();
        io_cfg.insert_inherit_env("RUNBOOK_SNIPPET_PATH", snippet_path.display().to_string());
        io_cfg.insert_inherit_env("RUNBOOK_SNIPPET_DIR", snippets_dir.display().to_string());
        io_cfg.insert_inherit_env("RUNBOOK_RUNNER_DIR", runner_dir_value.clone());
        io_cfg.set_stdin(input_stream);
        io_cfg.set_stdout(Some(use_stdout.clone()));
        io_cfg.set_stderr(Some(use_stderr.clone()));

        // Pipe setup/build output directly to the user's terminal using inherited stdio
        // so tools like Cargo keep their colorized output.
        io_cfg.insert_output_pipe_stdout_inherit(PIPE_SETUP);
        io_cfg.insert_output_pipe_stderr_inherit(PIPE_SETUP);
        io_cfg.insert_output_pipe_stdout(PIPE_SNIPPET, use_stdout.clone());
        io_cfg.insert_output_pipe_stderr(PIPE_SNIPPET, use_stderr.clone());

        if let Some(token) = cancel_token {
            let process = InterruptibleProcessManager::new(token.clone());
            run_steps_with_context_result_with_io_and_process(
                &env.root,
                workspace_resolver.root(),
                &steps,
                io_cfg,
                process,
            )
        } else {
            run_steps_with_context_result_with_io(
                &env.root,
                workspace_resolver.root(),
                &steps,
                io_cfg,
            )
        }
        .with_context(|| format!("run {}", oxfile_path.display()))?;

        // Determine which capture buffer to read from: prefer explicit
        // capture buffers passed by the caller; otherwise use the internal
        // captures when stdout/stderr writers were synthesized above.
        let stdout_bytes = if let Some(cb) = stdout_capture {
            cb.lock().unwrap().clone()
        } else if let Some(cb) = internal_stdout {
            cb.lock().unwrap().clone()
        } else {
            Vec::new()
        };

        let stderr_bytes = if let Some(cb) = stderr_capture {
            cb.lock().unwrap().clone()
        } else if let Some(cb) = internal_stderr {
            cb.lock().unwrap().clone()
        } else {
            Vec::new()
        };

        return Ok(RunnerOutput {
            stdout: String::from_utf8_lossy(&stdout_bytes).to_string(),
            stderr: String::from_utf8_lossy(&stderr_bytes).to_string(),
        });
    }

    // At this point we require an oxfile to drive execution. The old behavior
    // that wrote a temporary script file and executed it directly has been
    // removed — languages must provide an `oxfile` (either local, workspace
    // level, or registered via `register_language_oxfile`).
    anyhow::bail!("no oxfile configured for language {}", spec.language)
}

// script file execution removed: languages must delegate to an oxfile.

#[allow(dead_code)]
fn command_output_to_string(output: &CommandOutput) -> String {
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let mut combined = String::new();
    if !stdout.is_empty() {
        combined.push_str(&stdout);
    }
    if !stderr.is_empty() {
        if !combined.is_empty() && !combined.ends_with('\n') {
            combined.push('\n');
        }
        combined.push_str(&stderr);
    }
    if output.success() {
        combined
    } else if combined.is_empty() {
        "error: command failed".to_string()
    } else {
        combined
    }
}

fn code_hash(script: &str, spec: &RunnerSpec) -> String {
    let mut combined = String::new();
    combined.push_str(script);
    combined.push('\n');
    combined.push_str(&spec.language);
    combined.push('\n');
    combined.push_str(&spec.command.join("\u{1f}"));
    if let Some(hash) = &spec.env_hash {
        combined.push('\n');
        combined.push_str(hash);
    }
    sha256_hex(&combined)
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::{Context, Result};
    use indoc::indoc;

    fn run_probe_runner(script: &str) -> Result<(GuardedPath, Option<String>, Option<String>)> {
        let temp = GuardedPath::tempdir().context("probe workspace tempdir")?;
        let workspace = temp.as_guarded_path().clone();
        let resolver = PathResolver::new_guarded(workspace.clone(), workspace.clone())
            .context("create workspace resolver")?;
        let runner_path = workspace.join("temp.runner.probe.oxfile")?;
        resolver
            .write_file(&runner_path, script.as_bytes())
            .context("write probe runner")?;

        let mut cache = RunnerCache::default();
        let hash = env_hash_for_oxfile(&resolver, &runner_path, &mut cache)
            .context("build runner env")?
            .expect("runner env hash");
        let key = runner_path.display().to_string();
        let spec = RunnerSpec {
            language: "probe".to_string(),
            command: Vec::new(),
            oxfile: Some(runner_path.clone()),
            env_hash: Some(hash),
        };

        run_runner(
            &resolver, &workspace, &workspace, &mut cache, &spec, "", None, None, None, None, None,
        )
        .context("execute probe runner")?;

        let env = cache.envs.get(&key).expect("runner env should be cached");
        let leak_path_file = env.root.join("leak-path.txt")?;
        let leak_dir_file = env.root.join("leak-dir.txt")?;

        Ok((
            workspace,
            read_if_exists(&leak_path_file)?,
            read_if_exists(&leak_dir_file)?,
        ))
    }

    fn read_if_exists(path: &GuardedPath) -> Result<Option<String>> {
        let resolver =
            PathResolver::new(path.root(), path.root()).context("resolver for leak file")?;
        if resolver.entry_kind(path).is_err() {
            return Ok(None);
        }
        let contents = resolver.read_to_string(path).context("read leak file")?;
        Ok(Some(contents.trim().to_string()))
    }

    #[test]
    fn snippet_env_hidden_without_inherit() -> Result<()> {
        let script = indoc! {
            r#"
            [env:RUNBOOK_SNIPPET_PATH]
            WRITE leak-path.txt "path={{ env:RUNBOOK_SNIPPET_PATH }}"
            [env:RUNBOOK_SNIPPET_DIR]
            WRITE leak-dir.txt "dir={{ env:RUNBOOK_SNIPPET_DIR }}"
            "#
        };

        let (_workspace, path_value, dir_value) = run_probe_runner(script)?;
        assert!(
            path_value.is_none() && dir_value.is_none(),
            "snippet env should not be visible without INHERIT_ENV"
        );
        Ok(())
    }

    #[test]
    fn snippet_env_available_with_inherit() -> Result<()> {
        let script = indoc! {
            r#"
            INHERIT_ENV [RUNBOOK_SNIPPET_PATH, RUNBOOK_SNIPPET_DIR]
            [env:RUNBOOK_SNIPPET_PATH]
            WRITE leak-path.txt "path={{ env:RUNBOOK_SNIPPET_PATH }}"
            [env:RUNBOOK_SNIPPET_DIR]
            WRITE leak-dir.txt "dir={{ env:RUNBOOK_SNIPPET_DIR }}"
            "#
        };

        let (workspace, path_value, dir_value) = run_probe_runner(script)?;
        let path_value = path_value.expect("snippet path should be inherited");
        let dir_value = dir_value.expect("snippet dir should be inherited");

        // Parse env-provided paths using the workspace resolver so platform
        // specific quirks (backslashes, file://) are handled consistently.
        let resolver = PathResolver::new_guarded(workspace.clone(), workspace.clone())
            .context("create resolver for snippet env parse")?;
        let dir_raw = dir_value.trim_start_matches("dir=");
        let parsed_dir = resolver.parse_env_path(resolver.root(), dir_raw)?;
        let expected_dir = resolver
            .root()
            .join("target")?
            .join("runbook")?
            .join("snippets")?;
        assert_eq!(
            parsed_dir.as_path(),
            expected_dir.as_path(),
            "snippet dir should resolve to snippets staging directory"
        );

        let path_raw = path_value.trim_start_matches("path=");
        let parsed_path = resolver.parse_env_path(resolver.root(), path_raw)?;
        let expected_path = expected_dir.join("runbook-snippet.probe")?;
        assert_eq!(
            parsed_path.as_path(),
            expected_path.as_path(),
            "snippet path should resolve to generated snippet file"
        );
        Ok(())
    }
}
