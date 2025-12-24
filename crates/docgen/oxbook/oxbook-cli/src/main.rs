use anyhow::{Context, Result, bail};
use notify::{Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher};
use os_pipe::pipe;
use oxdock_core::run_steps_with_context_result;
use oxdock_fs::{GuardedPath, GuardedTempDir, PathResolver, discover_workspace_root, to_forward_slashes};
use oxdock_parser::{Step, StepKind, parse_script};
use oxdock_process::{CommandOutput, SharedInput, SharedOutput};

use once_cell::sync::Lazy;
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::io::{Cursor, IsTerminal, Stdout};
use std::sync::{Arc, Mutex};
use std::time::Duration;

const STABLE_READ_RETRIES: usize = 5;
const STABLE_READ_DELAY: Duration = Duration::from_millis(30);
const MAX_FENCE_PREFIX_WS: usize = 3;
const OUTPUT_BEGIN: &str = "<!-- oxbook-output:begin -->";
const OUTPUT_END: &str = "<!-- oxbook-output:end -->";
const OUTPUT_META_PREFIX: &str = "<!-- oxbook-output:meta";
const WATCH_DEBOUNCE_WINDOW: Duration = Duration::from_millis(120);
const SHORT_HASH_LEN: usize = 32;
// If set to "0" disables terminal streaming; otherwise auto-on when stdout is a TTY.
const STREAM_STDOUT_ENV: &str = "OXBOOK_STREAM_STDOUT";

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
const STDERR_MARKER: &str = "<!-- oxbook-output:stderr -->";

struct FenceInfo {
    language: Option<String>,
    params: HashMap<String, String>,
}

struct InterpreterSpec {
    language: String,
    command: Vec<String>,
    oxfile: Option<GuardedPath>,
    env_hash: Option<String>,
}

#[allow(dead_code)]
struct InterpreterEnv {
    root: GuardedPath,
    cwd: GuardedPath,
    env_hash: Option<String>,
    _tempdir: Option<GuardedTempDir>,
}

#[derive(Default)]
struct InterpreterCache {
    envs: HashMap<String, InterpreterEnv>,
}

fn main() -> Result<()> {
    let target = parse_target_path()?;
    let workspace_root = discover_workspace_root().context("resolve workspace root")?;
    let resolver = PathResolver::new_guarded(workspace_root.clone(), workspace_root.clone())
        .context("create workspace path resolver")?;

    // Auto-register interpreters discovered in the workspace (files named
    // `temp.interpreter.<lang>.oxfile`). This allows users to add interpreter
    // oxfiles to the repository without recompiling or env vars.
    scan_and_register_interpreters(&workspace_root);

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
    let mut cache = InterpreterCache::default();

    let initial_contents = read_stable_contents(&resolver, &watched)?;
    let rendered = render_shell_outputs(
        &initial_contents,
        &resolver,
        &workspace_root,
        &cwd,
        &mut cache,
        true,
    )?;
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
    )?;
    Ok(())
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
        _ => bail!("Usage: oxbook-cli <path-to-markdown>"),
    }
}

fn read_contents(resolver: &PathResolver, path: &GuardedPath) -> Result<String> {
    resolver
        .read_to_string(path)
        .with_context(|| format!("read {}", path.display()))
}

fn read_stable_contents(resolver: &PathResolver, path: &GuardedPath) -> Result<String> {
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
    cache: &mut InterpreterCache,
    last_contents: &mut String,
) -> Result<()> {
    let (tx, rx) = std::sync::mpsc::channel();
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
        let mut should_process = false;
        match rx.recv() {
            Ok(Ok(event)) => {
                if event_maybe_affects(&event, watched) && is_relevant_kind(&event) {
                    should_process = true;
                }
            }
            Ok(Err(err)) => {
                eprintln!("watch error: {err}");
                continue;
            }
            Err(err) => return Err(err).context("watcher channel closed"),
        }

        if !should_process {
            continue;
        }

        loop {
            match rx.recv_timeout(WATCH_DEBOUNCE_WINDOW) {
                Ok(Ok(event)) => {
                    if event_maybe_affects(&event, watched) && is_relevant_kind(&event) {
                        continue;
                    }
                }
                Ok(Err(err)) => {
                    eprintln!("watch error: {err}");
                }
                Err(std::sync::mpsc::RecvTimeoutError::Timeout) => break,
                Err(std::sync::mpsc::RecvTimeoutError::Disconnected) => {
                    return Err(anyhow::anyhow!("watcher channel closed"));
                }
            }
        }

        if let Ok(new_contents) = read_stable_contents(resolver, watched)
            && new_contents != *last_contents
        {
            report_fence_changes(watched, last_contents, &new_contents);
            let rendered = render_shell_outputs(
                &new_contents,
                resolver,
                workspace_root,
                source_dir,
                cache,
                false,
            )?;
            if rendered != new_contents {
                resolver
                    .write_file(watched, rendered.as_bytes())
                    .with_context(|| format!("write {}", watched.display()))?;
            }
            *last_contents = rendered;
        }
    }
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
    cache: &mut InterpreterCache,
    only_missing_outputs: bool,
) -> Result<String> {
    let lines: Vec<&str> = contents.lines().collect();
    let mut out_lines: Vec<String> = Vec::new();
    let mut prev_reader: Option<SharedInput> = None;
    let mut i = 0;
    while i < lines.len() {
        let line = lines[i];
        let (ws, rest) = split_leading_ws(line);
        if ws <= MAX_FENCE_PREFIX_WS
            && let Some((fence_char, fence_len, info)) = parse_fence_start(rest)
        {
            out_lines.push(line.to_string());
            i += 1;
            let mut body_lines: Vec<&str> = Vec::new();
            let mut closed = false;
            while i < lines.len() {
                let inner = lines[i];
                let (inner_ws, inner_rest) = split_leading_ws(inner);
                if inner_ws <= MAX_FENCE_PREFIX_WS
                    && is_fence_close(inner_rest, fence_char, fence_len)
                {
                    out_lines.push(inner.to_string());
                    i += 1;
                    closed = true;
                    break;
                }
                body_lines.push(inner);
                out_lines.push(inner.to_string());
                i += 1;
            }
            if !closed {
                continue;
            }
            let script = body_lines.join("\n");
            // Parse fence info params (currently used for language/oxfile/cmd only).
            let _fence_info = parse_fence_info(&info);
            let existing = parse_output_block(&lines, i);
            if let Some(spec) =
                interpreter_spec(&info, resolver, workspace_root, source_dir, cache)?
            {
                let code_hash = code_hash(&script, &spec);
                let should_run = match &existing {
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
                            let matches_output =
                                if let Some(meta_hash) = block.combined_hash.as_deref() {
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
                if should_run {
                    // Always stream the previous fence's stdout into this fence's stdin.
                    let stdin_stream = prev_reader.take();

                    // Create a cross-platform anonymous pipe (reader, writer)
                    // to stream this fence's stdout to the next fence while
                    // also capturing it for embedding.
                    let (reader, writer) =
                        pipe().with_context(|| "create pipe for piping stdout to next fence")?;
                    let reader_shared: SharedInput = Arc::new(Mutex::new(reader));
                    let writer_shared: Arc<Mutex<dyn std::io::Write + Send>> =
                        Arc::new(Mutex::new(writer));

                    // Capture buffer to embed output in the markdown.
                    let capture_buf: Arc<Mutex<Vec<u8>>> = Arc::new(Mutex::new(Vec::new()));

                    // Tee writer writes to both the capture buffer and the
                    // pipe writer so we both embed and stream the output.
                    struct TeeWriter {
                        cap: Arc<Mutex<Vec<u8>>>,
                        pipe: Arc<Mutex<dyn std::io::Write + Send>>,
                    }
                    impl std::io::Write for TeeWriter {
                        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
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

                    let tee = TeeWriter {
                        cap: capture_buf.clone(),
                        pipe: writer_shared.clone(),
                    };
                    let shared_out: SharedOutput = Arc::new(Mutex::new(tee));

                    // Run interpreter with stdin_stream and the tee writer
                    // as stdout. Provide the capture buffer so the caller
                    // can inspect the captured bytes after execution.
                    let run_res = run_interpreter(
                        resolver,
                        workspace_root,
                        source_dir,
                        cache,
                        &spec,
                        &script,
                        stdin_stream,
                        Some(shared_out.clone()),
                        Some(capture_buf.clone()),
                    );
                    match run_res {
                        Ok(_) => {
                            let data = capture_buf.lock().unwrap();
                            let output = String::from_utf8_lossy(&data).to_string();
                            // Make the reader available to the next fence.
                            prev_reader = Some(reader_shared);
                            let output_block = format_output_block(&code_hash, &output, "");
                            if let Some(block) = existing {
                                i = block.end_index;
                            }
                            out_lines.extend(output_block);
                        }
                        Err(err) => {
                            let mut chain = Vec::new();
                            for (idx, cause) in err.chain().enumerate() {
                                if idx == 0 {
                                    chain.push(format!("error: {}", cause));
                                } else {
                                    chain.push(format!("caused by: {}", cause));
                                }
                            }
                            let err_msg = chain.join("\n");
                            if let Some(block) = existing {
                                i = block.end_index;
                            }
                            let output_block = format_output_block(&code_hash, "", &err_msg);
                            out_lines.extend(output_block);
                        }
                    }
                } else if let Some(block) = existing {
                    out_lines.extend(
                        lines[block.start_index..block.end_index]
                            .iter()
                            .map(|line| (*line).to_string()),
                    );
                    // When reusing an existing block, restore prev_reader from
                    // the saved stdout so subsequent fences can consume it.
                    let prev_buf = block.stdout.clone();
                    let cursor = Cursor::new(prev_buf.into_bytes());
                    prev_reader = Some(Arc::new(Mutex::new(cursor)));
                    i = block.end_index;
                }
            } else {
                // No interpreter spec found. If the fence declared a language,
                // write an informative output block so users see that the
                // fence was not executed.
                let parsed = parse_fence_info(&info);
                if let Some(lang) = parsed.language {
                    let code_hash = sha256_hex(&format!("{}\n{}", script, lang));
                    let output =
                        format!("error: no interpreter registered for language '{}'", lang);
                    if let Some(block) = existing {
                        i = block.end_index;
                    }
                    let output_block = format_output_block(&code_hash, "", &output);
                    out_lines.extend(output_block);
                }
            }
            continue;
        }
        out_lines.push(line.to_string());
        i += 1;
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
) {
    let trimmed = line.trim_start();
    if !trimmed.starts_with("```") {
        return;
    }
    let mut tokens = trimmed.trim_start_matches('`').split_whitespace();
    // Skip the fence language token if present.
    let _ = tokens.next();
    for token in tokens {
        if token == "oxbook" {
            continue;
        }
        if let Some(value) = token.strip_prefix("code=") {
            code_hash.get_or_insert_with(|| value.to_string());
        } else if let Some(value) = token.strip_prefix("hash=") {
            combined_hash.get_or_insert_with(|| value.to_string());
        }
    }
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
        "```text oxbook code={code_hash_short} hash={combined_hash_short}"
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
            "```text oxbook code={code_hash_short} hash={combined_hash_short}"
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

fn interpreter_spec(
    info: &str,
    resolver: &PathResolver,
    workspace_root: &GuardedPath,
    source_dir: &GuardedPath,
    cache: &mut InterpreterCache,
) -> Result<Option<InterpreterSpec>> {
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
    Ok(Some(InterpreterSpec {
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

    let filename = format!("oxbook.{language}.oxfile");
    let local = source_dir.join(&filename)?;
    if resolver.entry_kind(&local).is_ok() {
        return Ok(Some(local));
    }

    let workspace = workspace_root.join(&filename)?;
    if resolver.entry_kind(&workspace).is_ok() {
        return Ok(Some(workspace));
    }

    // Fallback: also look for the temp interpreter naming convention to
    // support in-repo interpreters without requiring the oxbook.<lang> alias.
    let temp_name = format!("temp.interpreter.{language}.oxfile");
    let temp_local = source_dir.join(&temp_name)?;
    if resolver.entry_kind(&temp_local).is_ok() {
        return Ok(Some(temp_local));
    }
    let temp_workspace = workspace_root.join(&temp_name)?;
    if resolver.entry_kind(&temp_workspace).is_ok() {
        return Ok(Some(temp_workspace));
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

#[allow(
    clippy::disallowed_types,
    clippy::disallowed_methods,
    clippy::collapsible_if
)]
fn scan_and_register_interpreters(workspace_root: &GuardedPath) {
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
            const PREFIX: &str = "temp.interpreter.";
            const SUFFIX: &str = ".oxfile";
            if name.starts_with(PREFIX) && name.ends_with(SUFFIX) {
                let lang = &name[PREFIX.len()..name.len() - SUFFIX.len()];
                if lang.is_empty() {
                    continue;
                }
                if let Ok(rel) = path.strip_prefix(root) {
                    if let Some(rel_str) = rel.to_str() {
                        // Normalize to forward slashes for resolution.
                        let rel_str = rel_str.replace('\\', "/");
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
    cache: &mut InterpreterCache,
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
) -> Result<InterpreterEnv> {
    let mut steps = parse_script(script).with_context(|| format!("parse {}", path.display()))?;

    // Expose interpreter directory relative to the workspace root so oxfiles can
    // resolve it inside the copied temp workspace without host-absolute paths.
    let interpreter_dir = path.parent().unwrap_or_else(|| resolver.root().clone());
    let interpreter_rel = interpreter_dir
        .as_path()
        .strip_prefix(resolver.root().as_path())
        .map(|p| to_forward_slashes(&p.to_string_lossy()))
        .unwrap_or_else(|_| interpreter_dir.display().to_string());
    let interpreter_env = Step {
        guards: Vec::new(),
        kind: StepKind::Env {
            key: "OXBOOK_INTERPRETER_DIR".to_string(),
            value: interpreter_rel.into(),
        },
        scope_enter: 0,
        scope_exit: 0,
    };
    steps.insert(0, interpreter_env);
    let tempdir =
        GuardedPath::tempdir().with_context(|| format!("tempdir for {}", path.display()))?;
    let temp_root = tempdir.as_guarded_path().clone();
    let build_context = resolver.root().clone();
    let output_buf = Arc::new(Mutex::new(Vec::new()));

    // Stream build/compile chatter to the user's terminal when available, while still
    // capturing for diagnostics. Allow opt-out via OXBOOK_STREAM_STDOUT=0.
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

    let final_cwd =
        run_steps_with_context_result(&temp_root, &build_context, &steps, None, Some(build_stdout))
            .with_context(|| format!("run {}", path.display()))?;

    Ok(InterpreterEnv {
        root: temp_root,
        cwd: final_cwd,
        env_hash: Some(hash),
        _tempdir: Some(tempdir),
    })
}

#[allow(clippy::too_many_arguments)]
fn run_interpreter(
    resolver: &PathResolver,
    workspace_root: &GuardedPath,
    source_dir: &GuardedPath,
    cache: &mut InterpreterCache,
    spec: &InterpreterSpec,
    script: &str,
    stdin: Option<SharedInput>,
    stdout: Option<SharedOutput>,
    capture_buf: Option<Arc<Mutex<Vec<u8>>>>,
) -> Result<String> {
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
                capture_buf,
            );
        }
    };
    run_in_env(resolver, env, spec, script, stdin, stdout, capture_buf)
}

#[allow(clippy::too_many_arguments)]
fn run_in_default_env(
    resolver: &PathResolver,
    workspace_root: &GuardedPath,
    source_dir: &GuardedPath,
    spec: &InterpreterSpec,
    script: &str,
    stdin: Option<SharedInput>,
    stdout: Option<SharedOutput>,
    capture_buf: Option<Arc<Mutex<Vec<u8>>>>,
) -> Result<String> {
    let env = InterpreterEnv {
        root: workspace_root.clone(),
        cwd: source_dir.clone(),
        env_hash: None,
        _tempdir: None,
    };
    run_in_env_with_resolver(
        resolver,
        resolver,
        &env,
        spec,
        script,
        stdin,
        stdout,
        capture_buf,
    )
}

fn run_in_env(
    workspace_resolver: &PathResolver,
    env: &InterpreterEnv,
    spec: &InterpreterSpec,
    script: &str,
    stdin: Option<SharedInput>,
    stdout: Option<SharedOutput>,
    capture_buf: Option<Arc<Mutex<Vec<u8>>>>,
) -> Result<String> {
    let resolver = PathResolver::new_guarded(env.root.clone(), env.root.clone())?;
    run_in_env_with_resolver(
        workspace_resolver,
        &resolver,
        env,
        spec,
        script,
        stdin,
        stdout,
        capture_buf,
    )
}

#[allow(clippy::too_many_arguments)]
fn run_in_env_with_resolver(
    workspace_resolver: &PathResolver,
    resolver: &PathResolver,
    env: &InterpreterEnv,
    spec: &InterpreterSpec,
    script: &str,
    stdin: Option<SharedInput>,
    stdout: Option<SharedOutput>,
    capture_buf: Option<Arc<Mutex<Vec<u8>>>>,
) -> Result<String> {
    if let Some(oxfile_path) = &spec.oxfile {
        let oxfile_content = workspace_resolver
            .read_to_string(oxfile_path)
            .with_context(|| format!("read {}", oxfile_path.display()))?;
        let mut steps = parse_script(&oxfile_content)
            .with_context(|| format!("parse {}", oxfile_path.display()))?;

        // Make interpreter location available to oxfiles so they can be path-agnostic.
        let interpreter_dir = oxfile_path
            .parent()
            .unwrap_or_else(|| env.root.clone());
        let interpreter_env = Step {
            guards: Vec::new(),
            kind: StepKind::Env {
                key: "OXBOOK_INTERPRETER_DIR".to_string(),
                value: interpreter_dir.display().to_string().into(),
            },
            scope_enter: 0,
            scope_exit: 0,
        };

        // Persist the snippet so interpreters can execute the file while
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
        let snippet_name = format!("oxbook-snippet.{lang_safe}");
        let snippet_path = env
            .root
            .join(&snippet_name)
            .with_context(|| format!("snippet path for {}", spec.language))?;
        resolver
            .write_file(&snippet_path, script.as_bytes())
            .with_context(|| format!("write {}", snippet_path.display()))?;

        let snippet_dir = snippet_path.parent().unwrap_or_else(|| env.root.clone());

        let snippet_env = Step {
            guards: Vec::new(),
            kind: StepKind::Env {
                key: "OXBOOK_SNIPPET_PATH".to_string(),
                value: snippet_path.display().to_string().into(),
            },
            scope_enter: 0,
            scope_exit: 0,
        };
        let snippet_dir_env = Step {
            guards: Vec::new(),
            kind: StepKind::Env {
                key: "OXBOOK_SNIPPET_DIR".to_string(),
                value: snippet_dir.display().to_string().into(),
            },
            scope_enter: 0,
            scope_exit: 0,
        };
        steps.insert(0, snippet_env);
        steps.insert(0, snippet_dir_env);
        steps.insert(0, interpreter_env);

        // Use the previous fence's stdout (if any) as stdin; the snippet
        // itself is provided via OXBOOK_SNIPPET_PATH.
        let input_stream = stdin;

        // Prepare stdout: if provided, use it; otherwise create a capture
        // buffer and use that for stdout so we can return captured output.
        let (use_stdout, internal_capture): (SharedOutput, Option<Arc<Mutex<Vec<u8>>>>) =
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

        run_steps_with_context_result(
            &env.root,
            workspace_resolver.root(),
            &steps,
            input_stream,
            Some(use_stdout),
        )
        .with_context(|| format!("run {}", oxfile_path.display()))?;

        // Determine which capture buffer to read from: prefer explicit
        // capture_buf passed by caller, otherwise use internal_capture.
        if let Some(cb) = capture_buf {
            let data = cb.lock().unwrap();
            return Ok(String::from_utf8_lossy(&data).to_string());
        }
        if let Some(cb) = internal_capture {
            let data = cb.lock().unwrap();
            return Ok(String::from_utf8_lossy(&data).to_string());
        }

        // If stdout was provided and no capture buffer passed, we cannot
        // reliably return the captured text here — return empty string.
        return Ok(String::new());
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

fn code_hash(script: &str, spec: &InterpreterSpec) -> String {
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
