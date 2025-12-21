use anyhow::{Context, Result, bail};
use notify::{Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher};
use oxdock_fs::{GuardedPath, PathResolver, discover_workspace_root};
use similar::TextDiff;
use std::time::Duration;

const STABLE_READ_RETRIES: usize = 5;
const STABLE_READ_DELAY: Duration = Duration::from_millis(30);

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
