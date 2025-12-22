use anyhow::{Context, Result, bail};
use notify::{Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher};
use oxdock_core::run_steps_with_context_result;
use oxdock_fs::{GuardedPath, GuardedTempDir, PathResolver, command_path, discover_workspace_root};
use oxdock_parser::parse_script;
use oxdock_process::{CommandBuilder, CommandOutput, SharedInput};
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::io::Cursor;
use std::sync::{Arc, Mutex};
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
    let watched = resolver
        .resolve_read(&workspace_root, &target)
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

        if let Ok(new_contents) = read_stable_contents(resolver, watched) {
            if new_contents != *last_contents {
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
                if let Some(spec) =
                    interpreter_spec(&info, resolver, workspace_root, source_dir, cache)?
                {
                    let script = body_lines.join("\n");
                    let code_hash = code_hash(&script, &spec);
                    let existing = parse_output_block(&lines, i);
                    let should_run = match &existing {
                        Some(block) => {
                            let matches_code = block.code_hash.as_deref() == Some(&code_hash);
                            if only_missing_outputs {
                                !matches_code
                            } else {
                                let expected_output_hash = sha256_hex(&block.output);
                                let matches_output =
                                    block.output_hash.as_deref() == Some(&expected_output_hash);
                                !(matches_code && matches_output)
                            }
                        }
                        None => true,
                    };
                    if should_run {
                        let output = run_interpreter(
                            resolver,
                            workspace_root,
                            source_dir,
                            cache,
                            &spec,
                            &script,
                        )?;
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
        _ => {
            if oxfile.is_some() {
                Vec::new()
            } else {
                vec![language.to_string()]
            }
        }
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

fn parse_command_parts(cmd: Option<&String>, language: &str) -> Vec<String> {
    match cmd {
        Some(value) => value
            .split(',')
            .map(|part| part.trim().to_string())
            .filter(|part| !part.is_empty())
            .collect(),
        None => vec![language.to_string()],
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

    Ok(None)
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
    let steps = parse_script(script).with_context(|| format!("parse {}", path.display()))?;
    let tempdir = GuardedPath::tempdir().with_context(|| format!("tempdir for {}", path.display()))?;
    let temp_root = tempdir.as_guarded_path().clone();
    let build_context = resolver.root().clone();
    let mut output_buf = Vec::new();
    let final_cwd = run_steps_with_context_result(
        &temp_root,
        &build_context,
        &steps,
        None,
        Some(&mut output_buf),
    )
    .with_context(|| format!("run {}", path.display()))?;

    Ok(InterpreterEnv {
        root: temp_root,
        cwd: final_cwd,
        env_hash: Some(hash),
        _tempdir: Some(tempdir),
    })
}

fn run_interpreter(
    resolver: &PathResolver,
    workspace_root: &GuardedPath,
    source_dir: &GuardedPath,
    cache: &mut InterpreterCache,
    spec: &InterpreterSpec,
    script: &str,
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
            );
        }
    };
    run_in_env(resolver, env, spec, script)
}

fn run_in_default_env(
    resolver: &PathResolver,
    workspace_root: &GuardedPath,
    source_dir: &GuardedPath,
    spec: &InterpreterSpec,
    script: &str,
) -> Result<String> {
    let env = InterpreterEnv {
        root: workspace_root.clone(),
        cwd: source_dir.clone(),
        env_hash: None,
        _tempdir: None,
    };
    run_in_env_with_resolver(resolver, resolver, &env, spec, script)
}

fn run_in_env(
    workspace_resolver: &PathResolver,
    env: &InterpreterEnv,
    spec: &InterpreterSpec,
    script: &str,
) -> Result<String> {
    let resolver = PathResolver::new_guarded(env.root.clone(), env.root.clone())?;
    run_in_env_with_resolver(workspace_resolver, &resolver, env, spec, script)
}

fn run_in_env_with_resolver(
    workspace_resolver: &PathResolver,
    resolver: &PathResolver,
    env: &InterpreterEnv,
    spec: &InterpreterSpec,
    script: &str,
) -> Result<String> {
    if let Some(oxfile_path) = &spec.oxfile {
        let oxfile_content = workspace_resolver
            .read_to_string(oxfile_path)
            .with_context(|| format!("read {}", oxfile_path.display()))?;
        let steps = parse_script(&oxfile_content)
            .with_context(|| format!("parse {}", oxfile_path.display()))?;

        let input = Arc::new(Mutex::new(Cursor::new(script.to_string())));
        let mut output_buf = Vec::new();

        run_steps_with_context_result(
            &env.root,
            workspace_resolver.root(),
            &steps,
            Some(input),
            Some(&mut output_buf),
        )
        .with_context(|| format!("run {}", oxfile_path.display()))?;

        return Ok(String::from_utf8_lossy(&output_buf).to_string());
    }

    let script_path = write_script_file(resolver, &env.cwd, &spec.language, script)?;
    let mut cmd_parts = build_command_with_script(&spec.command, &script_path);
    if cmd_parts.is_empty() {
        return Ok(String::from("error: missing command"));
    }
    let program = cmd_parts.remove(0);
    let mut cmd = CommandBuilder::new(program);
    if !cmd_parts.is_empty() {
        cmd.args(cmd_parts);
    }
    cmd.current_dir(command_path(&env.cwd));
    let cargo_target_dir = env.root.join("target")?;
    cmd.env(
        "CARGO_TARGET_DIR",
        command_path(&cargo_target_dir).into_owned(),
    );
    let output = cmd.output().with_context(|| format!("run {}", spec.language))?;
    Ok(command_output_to_string(&output))
}

fn write_script_file(
    resolver: &PathResolver,
    cwd: &GuardedPath,
    language: &str,
    script: &str,
) -> Result<GuardedPath> {
    let hash = sha256_hex(script);
    let ext = script_extension(language);
    let filename = format!("oxbook-cache/{language}-{hash}.{ext}");
    let target = resolver.resolve_write(cwd, &filename)?;
    if let Some(parent) = target.parent() {
        resolver.create_dir_all(&parent)?;
    }
    resolver.write_file(&target, script.as_bytes())?;
    Ok(target)
}

fn script_extension(language: &str) -> &'static str {
    match language {
        "rust" => "rs",
        "python" => "py",
        "bash" | "sh" => "sh",
        "node" | "js" | "javascript" => "js",
        _ => "txt",
    }
}

fn build_command_with_script(command: &[String], script_path: &GuardedPath) -> Vec<String> {
    let file_str = command_path(script_path).display().to_string();
    let mut parts = Vec::new();
    let mut has_file = false;
    let mut has_stdin = false;
    for part in command {
        if part.contains("{file}") {
            has_file = true;
            parts.push(part.replace("{file}", &file_str));
        } else if part.contains("{stdin}") {
            has_stdin = true;
            parts.push(part.replace("{stdin}", &file_str));
        } else {
            parts.push(part.clone());
        }
    }
    if !has_file && !has_stdin {
        parts.push(file_str);
    }
    parts
}

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
        format!("error: command failed")
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

