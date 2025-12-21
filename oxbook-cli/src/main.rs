use anyhow::{Context, Result, bail};
use notify::{Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher};
use oxdock_fs::{GuardedPath, PathResolver, discover_workspace_root};
use similar::TextDiff;
use std::time::Duration;

const STABLE_READ_RETRIES: usize = 5;
const STABLE_READ_DELAY: Duration = Duration::from_millis(30);
const MAX_FENCE_PREFIX_WS: usize = 3;

#[derive(Debug)]
struct FenceBlock {
    fence: char,
    fence_len: usize,
    info: String,
    start_line: usize,
    end_line: usize,
    content: String,
}

fn main() -> Result<()> {
    let target = parse_target_path()?;
    let workspace_root = discover_workspace_root().context("resolve workspace root")?;
    let resolver = PathResolver::new_guarded(workspace_root.clone(), workspace_root.clone())
        .context("create workspace path resolver")?;
    let watched = resolver
        .resolve_read(&workspace_root, &target)
        .with_context(|| format!("resolve markdown path {}", target))?;

    let mut last_contents = read_stable_contents(&resolver, &watched)?;
    eprintln!("Watching {}", watched.display());
    run_watch_loop(&resolver, &watched, &mut last_contents)?;
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
        match rx.recv() {
            Ok(Ok(event)) => {
                if event_maybe_affects(&event, watched) && is_relevant_kind(&event) {
                    if let Ok(new_contents) = read_stable_contents(resolver, watched) {
                        if new_contents != *last_contents {
                            print_change(watched, last_contents, &new_contents);
                            report_fence_changes(watched, last_contents, &new_contents);
                            *last_contents = new_contents;
                        }
                    }
                }
            }
            Ok(Err(err)) => eprintln!("watch error: {err}"),
            Err(err) => return Err(err).context("watcher channel closed"),
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

fn print_change(path: &GuardedPath, before: &str, after: &str) {
    let before_label = format!("{} (before)", path.display());
    let after_label = format!("{} (after)", path.display());
    let diff = TextDiff::from_lines(before, after);
    print!("{}", diff.unified_diff().header(&before_label, &after_label));
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
