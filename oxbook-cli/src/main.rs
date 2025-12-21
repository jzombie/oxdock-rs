use anyhow::{Context, Result, bail};
use notify::{Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher};
use oxdock_fs::{GuardedPath, PathResolver, PolicyPath, discover_workspace_root};
use oxdock_process::{CommandContext, ProcessManager, default_process_manager};
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::time::Duration;

const STABLE_READ_RETRIES: usize = 5;
const STABLE_READ_DELAY: Duration = Duration::from_millis(30);
const MAX_FENCE_PREFIX_WS: usize = 3;
const OUTPUT_BEGIN: &str = "<!-- oxbook-output:begin -->";
const OUTPUT_END: &str = "<!-- oxbook-output:end -->";
const OUTPUT_META_PREFIX: &str = "<!-- oxbook-output:meta";
const WATCH_DEBOUNCE_WINDOW: Duration = Duration::from_millis(120);

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
    output_hash: Option<String>,
    output: String,
}

fn main() -> Result<()> {
    let target = parse_target_path()?;
    let workspace_root = discover_workspace_root().context("resolve workspace root")?;
    let resolver = PathResolver::new_guarded(workspace_root.clone(), workspace_root.clone())
        .context("create workspace path resolver")?;
    let watched = resolver
        .resolve_read(&workspace_root, &target)
        .with_context(|| format!("resolve markdown path {}", target))?;

    let cwd = watched.parent().unwrap_or_else(|| workspace_root.clone());
    let ctx = build_command_context(&workspace_root, &cwd)?;
    let mut manager = default_process_manager();

    let initial_contents = read_stable_contents(&resolver, &watched)?;
    let rendered = render_shell_outputs(&initial_contents, &ctx, &mut manager)?;
    if rendered != initial_contents {
        resolver
            .write_file(&watched, rendered.as_bytes())
            .with_context(|| format!("write {}", watched.display()))?;
    }
    let mut last_contents = rendered;
    eprintln!("Watching {}", watched.display());
    run_watch_loop(&resolver, &watched, &ctx, &mut manager, &mut last_contents)?;
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
    watched: &GuardedPath,
    ctx: &CommandContext,
    manager: &mut impl ProcessManager,
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

        if let Ok(new_contents) = read_stable_contents(resolver, watched) {
            if new_contents != *last_contents {
                report_fence_changes(watched, last_contents, &new_contents);
                let rendered = render_shell_outputs(&new_contents, ctx, manager)?;
                if rendered != new_contents {
                    resolver
                        .write_file(watched, rendered.as_bytes())
                        .with_context(|| format!("write {}", watched.display()))?;
                }
                *last_contents = rendered;
            }
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
            || watched_name.map(|name| path.file_name() == Some(name)).unwrap_or(false)
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

fn build_command_context(
    workspace_root: &GuardedPath,
    cwd: &GuardedPath,
) -> Result<CommandContext> {
    let cargo_target_dir = workspace_root.join("target")?;
    let envs = HashMap::new();
    let policy = PolicyPath::from(cwd.clone());
    Ok(CommandContext::new(
        &policy,
        &envs,
        &cargo_target_dir,
        workspace_root,
        workspace_root,
    ))
}

fn render_shell_outputs(
    contents: &str,
    ctx: &CommandContext,
    manager: &mut impl ProcessManager,
) -> Result<String> {
    let lines: Vec<&str> = contents.lines().collect();
    let mut out_lines: Vec<String> = Vec::new();
    let mut i = 0;
    while i < lines.len() {
        let line = lines[i];
        let (ws, rest) = split_leading_ws(line);
        if ws <= MAX_FENCE_PREFIX_WS {
            if let Some((fence_char, fence_len, info)) = parse_fence_start(rest) {
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
                if matches!(fence_language(&info), Some("sh") | Some("bash")) {
                    let script = body_lines.join("\n");
                    let code_hash = sha256_hex(&script);
                    let existing = parse_output_block(&lines, i);
                    let should_run = match &existing {
                        Some(block) => {
                            let expected_output_hash = sha256_hex(&block.output);
                            let matches_code = block.code_hash.as_deref() == Some(&code_hash);
                            let matches_output =
                                block.output_hash.as_deref() == Some(&expected_output_hash);
                            !(matches_code && matches_output)
                        }
                        None => true,
                    };
                    if should_run {
                        let output = match manager.run_capture(ctx, &script) {
                            Ok(bytes) => String::from_utf8_lossy(&bytes).to_string(),
                            Err(err) => format!("error: {err}"),
                        };
                        let output_block = format_output_block(&code_hash, &output);
                        if let Some(block) = existing {
                            i = block.end_index;
                        }
                        out_lines.extend(output_block);
                    } else if let Some(block) = existing {
                        out_lines.extend(
                            lines[block.start_index..block.end_index]
                                .iter()
                                .map(|line| (*line).to_string()),
                        );
                        i = block.end_index;
                    }
                }
                continue;
            }
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

fn fence_language(info: &str) -> Option<&str> {
    info.split_whitespace()
        .next()
        .and_then(|token| token.split(',').next())
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

fn parse_output_block(lines: &[&str], start: usize) -> Option<OutputBlock> {
    let mut idx = start;
    if idx < lines.len() && lines[idx].trim().is_empty() {
        idx += 1;
    }
    if idx >= lines.len() || lines[idx].trim_end() != OUTPUT_BEGIN {
        return None;
    }
    let start_index = idx;
    idx += 1;

    let mut code_hash = None;
    let mut output_hash = None;
    if idx < lines.len() && lines[idx].trim_start().starts_with(OUTPUT_META_PREFIX) {
        let meta_line = lines[idx].trim();
        parse_output_meta(meta_line, &mut code_hash, &mut output_hash);
        idx += 1;
    }

    if idx >= lines.len() || lines[idx].trim_end() != "```text" {
        return None;
    }
    idx += 1;

    let mut output_lines = Vec::new();
    while idx < lines.len() {
        if lines[idx].trim_end() == "```" {
            idx += 1;
            break;
        }
        output_lines.push(lines[idx]);
        idx += 1;
    }
    if idx >= lines.len() || lines[idx].trim_end() != OUTPUT_END {
        return None;
    }
    let end_index = idx + 1;
    let output = output_lines.join("\n");
    Some(OutputBlock {
        start_index,
        end_index,
        code_hash,
        output_hash,
        output,
    })
}

fn parse_output_meta(line: &str, code_hash: &mut Option<String>, output_hash: &mut Option<String>) {
    let trimmed = line.trim_start_matches("<!--").trim_end_matches("-->").trim();
    let tokens: Vec<&str> = trimmed.split_whitespace().collect();
    for token in tokens {
        if let Some(value) = token.strip_prefix("code=") {
            *code_hash = Some(value.to_string());
        } else if let Some(value) = token.strip_prefix("output=") {
            *output_hash = Some(value.to_string());
        }
    }
}

fn format_output_block(code_hash: &str, output: &str) -> Vec<String> {
    let mut lines = Vec::new();
    lines.push(OUTPUT_BEGIN.to_string());
    let output_hash = sha256_hex(&normalize_output(output));
    lines.push(format!(
        "{OUTPUT_META_PREFIX} code={code_hash} output={output_hash} -->"
    ));
    lines.push("```text".to_string());
    for line in normalize_output(output).lines() {
        lines.push(line.to_string());
    }
    lines.push("```".to_string());
    lines.push(OUTPUT_END.to_string());
    lines
}

fn sha256_hex(input: &str) -> String {
    let mut hasher = Sha256::new();
    hasher.update(input.as_bytes());
    let digest = hasher.finalize();
    format!("{digest:x}")
}

fn normalize_output(output: &str) -> String {
    output.trim_end_matches('\n').to_string()
}
