use anyhow::{Result, bail};
use std::collections::HashMap;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Command {
    Workdir,
    Workspace,
    Env,
    Echo,
    Run,
    RunBg,
    Copy,
    CopyGit,
    Symlink,
    Mkdir,
    Ls,
    Cat,
    Write,
    Exit,
}

pub const COMMANDS: &[Command] = &[
    Command::Workdir,
    Command::Workspace,
    Command::Env,
    Command::Echo,
    Command::Run,
    Command::RunBg,
    Command::Copy,
    Command::CopyGit,
    Command::Symlink,
    Command::Mkdir,
    Command::Ls,
    Command::Cat,
    Command::Write,
    Command::Exit,
];

fn platform_matches(target: PlatformGuard) -> bool {
    match target {
        PlatformGuard::Unix => cfg!(unix),
        PlatformGuard::Windows => cfg!(windows),
        PlatformGuard::Macos => cfg!(target_os = "macos"),
        PlatformGuard::Linux => cfg!(target_os = "linux"),
    }
}

fn guard_allows(guard: &Guard, script_envs: &HashMap<String, String>) -> bool {
    match guard {
        Guard::Platform { target, invert } => {
            let res = platform_matches(*target);
            if *invert { !res } else { res }
        }
        Guard::EnvExists { key, invert } => {
            let res = script_envs
                .get(key)
                .cloned()
                .or_else(|| std::env::var(key).ok())
                .map(|v| !v.is_empty())
                .unwrap_or(false);
            if *invert { !res } else { res }
        }
        Guard::EnvEquals { key, value, invert } => {
            let res = script_envs
                .get(key)
                .cloned()
                .or_else(|| std::env::var(key).ok())
                .map(|v| v == *value)
                .unwrap_or(false);
            if *invert { !res } else { res }
        }
    }
}

fn guard_group_allows(group: &[Guard], script_envs: &HashMap<String, String>) -> bool {
    group.iter().all(|g| guard_allows(g, script_envs))
}

pub fn guards_allow_any(groups: &[Vec<Guard>], script_envs: &HashMap<String, String>) -> bool {
    // No guards means allow.
    if groups.is_empty() {
        return true;
    }
    groups.iter().any(|g| guard_group_allows(g, script_envs))
}

fn parse_guard(raw: &str, line_no: usize) -> Result<Guard> {
    let mut text = raw.trim();
    let mut invert_prefix = false;
    if let Some(rest) = text.strip_prefix('!') {
        invert_prefix = true;
        text = rest.trim();
    }

    if let Some(after) = text.strip_prefix("platform") {
        let after = after.trim_start();
        if let Some(rest) = after.strip_prefix(':').or_else(|| after.strip_prefix('=')) {
            let tag = rest.trim().to_ascii_lowercase();
            let target = match tag.as_str() {
                "unix" => PlatformGuard::Unix,
                "windows" => PlatformGuard::Windows,
                "mac" | "macos" => PlatformGuard::Macos,
                "linux" => PlatformGuard::Linux,
                _ => bail!("line {}: unknown platform '{}'", line_no, rest),
            };
            return Ok(Guard::Platform {
                target,
                invert: invert_prefix,
            });
        }
    }

    if let Some(rest) = text.strip_prefix("env:") {
        let rest = rest.trim();
        if let Some(pos) = rest.find("!=") {
            let key = rest[..pos].trim();
            let value = rest[pos + 2..].trim();
            if key.is_empty() || value.is_empty() {
                bail!("line {}: guard env: requires key and value", line_no);
            }
            return Ok(Guard::EnvEquals {
                key: key.to_string(),
                value: value.to_string(),
                invert: true,
            });
        }
        if let Some(pos) = rest.find('=') {
            let key = rest[..pos].trim();
            let value = rest[pos + 1..].trim();
            if key.is_empty() || value.is_empty() {
                bail!("line {}: guard env: requires key and value", line_no);
            }
            return Ok(Guard::EnvEquals {
                key: key.to_string(),
                value: value.to_string(),
                invert: invert_prefix,
            });
        }
        if rest.is_empty() {
            bail!("line {}: guard env: requires a variable name", line_no);
        }
        return Ok(Guard::EnvExists {
            key: rest.to_string(),
            invert: invert_prefix,
        });
    }

    let tag = text.to_ascii_lowercase();
    let target = match tag.as_str() {
        "unix" => PlatformGuard::Unix,
        "windows" => PlatformGuard::Windows,
        "mac" | "macos" => PlatformGuard::Macos,
        "linux" => PlatformGuard::Linux,
        _ => bail!("line {}: unknown guard '{}'", line_no, raw),
    };
    Ok(Guard::Platform {
        target,
        invert: invert_prefix,
    })
}

impl Command {
    pub const fn as_str(self) -> &'static str {
        match self {
            Command::Workdir => "WORKDIR",
            Command::Workspace => "WORKSPACE",
            Command::Env => "ENV",
            Command::Echo => "ECHO",
            Command::Run => "RUN",
            Command::RunBg => "RUN_BG",
            Command::Copy => "COPY",
            Command::CopyGit => "COPY_GIT",
            Command::Symlink => "SYMLINK",
            Command::Mkdir => "MKDIR",
            Command::Ls => "LS",
            Command::Cat => "CAT",
            Command::Write => "WRITE",
            Command::Exit => "EXIT",
        }
    }

    pub fn parse(op: &str) -> Option<Self> {
        COMMANDS.iter().copied().find(|c| c.as_str() == op)
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum PlatformGuard {
    Unix,
    Windows,
    Macos,
    Linux,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Guard {
    Platform {
        target: PlatformGuard,
        invert: bool,
    },
    EnvExists {
        key: String,
        invert: bool,
    },
    EnvEquals {
        key: String,
        value: String,
        invert: bool,
    },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum StepKind {
    Workdir(String),
    Workspace(WorkspaceTarget),
    Env { key: String, value: String },
    Run(String),
    Echo(String),
    RunBg(String),
    Copy { from: String, to: String },
    Symlink { from: String, to: String },
    Mkdir(String),
    Ls(Option<String>),
    Cat(String),
    Write { path: String, contents: String },
    CopyGit { rev: String, from: String, to: String },
    Exit(i32),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Step {
    pub guards: Vec<Vec<Guard>>,
    pub kind: StepKind,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum WorkspaceTarget {
    Snapshot,
    Local,
}

pub fn parse_script(input: &str) -> Result<Vec<Step>> {
    use std::collections::VecDeque;
    let mut steps = Vec::new();
    let mut pending_guards: Vec<Vec<Guard>> = Vec::new();

    // Use a queue so we can push back semicolon tail segments to be parsed
    // as separate commands while preserving their original logical order.
    let mut queue: VecDeque<(usize, String)> = VecDeque::new();
    for (i, raw) in input.lines().enumerate() {
        let raw = raw.trim();
        if raw.is_empty() || raw.starts_with('#') {
            continue;
        }
        queue.push_back((i + 1, raw.to_string()));
    }

    while let Some((line_no, rawline)) = queue.pop_front() {
        let line = rawline.as_str();

        // If a line begins with a guard block, parse it. If the guard block
        // is followed by a command on the same line, apply guards (plus any
        // previously-pending guards) to that command. If the guard block is
        // on its own line (no command after `]`), stash the guards to apply
        // to the next non-empty command line.
        let (groups, remainder_opt) = if let Some(rest) = line.strip_prefix('[') {
            let end = rest
                .find(']')
                .ok_or_else(|| anyhow::anyhow!("line {}: guard must close with ]", line_no))?;
            let guards_raw = &rest[..end];
            let after = rest[end + 1..].trim();
            // Parse alternatives separated by `|`, each alternative is a comma-separated AND group.
            let mut parsed_groups: Vec<Vec<Guard>> = Vec::new();
            for alt in guards_raw.split('|') {
                let mut group: Vec<Guard> = Vec::new();
                for g in alt.split(',') {
                    let parsed = parse_guard(g, line_no)?;
                    group.push(parsed);
                }
                parsed_groups.push(group);
            }
            if after.is_empty() {
                // Guard-only line: combine with pending_guards by ANDing groups (cartesian product).
                if pending_guards.is_empty() {
                    pending_guards = parsed_groups;
                } else {
                    let mut new_pending: Vec<Vec<Guard>> = Vec::new();
                    for p in pending_guards.iter() {
                        for q in parsed_groups.iter() {
                            let mut merged = p.clone();
                            merged.extend(q.clone());
                            new_pending.push(merged);
                        }
                    }
                    pending_guards = new_pending;
                }
                continue;
            }
            (parsed_groups, Some(after.to_string()))
        } else {
            (Vec::new(), Some(line.to_string()))
        };

        // Combine any pending guard groups (from previous guard-only lines)
        // with any groups parsed on this line. Combination is an AND between
        // groups (cartesian product), producing OR-of-AND groups for the
        // resulting step. If there are no inline groups, use pending groups.
        let mut all_groups: Vec<Vec<Guard>> = Vec::new();
        if groups.is_empty() {
            // No inline groups: attach pending groups (if any) to this step.
            all_groups = pending_guards.clone();
        } else if pending_guards.is_empty() {
            all_groups = groups.clone();
        } else {
            for p in pending_guards.iter() {
                for q in groups.iter() {
                    let mut merged = p.clone();
                    merged.extend(q.clone());
                    all_groups.push(merged);
                }
            }
        }
        pending_guards.clear();
        let mut remainder = remainder_opt.unwrap();

        // Parse op and rest from the current remainder.  Create owned
        // strings to avoid borrowing `remainder` while we may need to
        // reassign it below.
        let mut parts = remainder.splitn(2, ' ');
        let op_owned = parts.next().unwrap().trim().to_string();
        let rest_owned = parts
            .next()
            .map(|s| s.trim().to_string())
            .unwrap_or_default();
        let cmd = Command::parse(op_owned.as_str()).ok_or_else(|| {
            anyhow::anyhow!("line {}: unknown instruction '{}'", line_no, op_owned)
        })?;

        // If command is not RUN or RUN_BG and the `rest` contains a ';',
        // split it so we only consume up to the first semicolon for this
        // command. Push the tail back onto the queue to be parsed next.
        if cmd != Command::Run && cmd != Command::RunBg {
            if let Some(idx_sc) = rest_owned.find(';') {
                let first = rest_owned[..idx_sc].trim().to_string();
                let tail = rest_owned[idx_sc + 1..].trim();
                // replace remainder with the first segment for current handling
                remainder = first;
                // push the tail as a new logical line onto the front of the queue
                if !tail.is_empty() {
                    queue.push_front((line_no, tail.to_string()));
                }
            } else {
                remainder = rest_owned.to_string();
            }
        } else {
            // RUN/RUN_BG keep the entire rest verbatim
            remainder = rest_owned.to_string();
        }

        let kind = match cmd {
            Command::Workdir => {
                if remainder.is_empty() {
                    bail!("line {}: WORKDIR requires a path", line_no);
                }
                StepKind::Workdir(remainder.to_string())
            }
            Command::Workspace => {
                let target = match remainder.as_str() {
                    "SNAPSHOT" | "snapshot" => WorkspaceTarget::Snapshot,
                    "LOCAL" | "local" => WorkspaceTarget::Local,
                    _ => bail!("line {}: WORKSPACE requires LOCAL or SNAPSHOT", line_no),
                };
                StepKind::Workspace(target)
            }
            Command::Env => {
                let mut parts = remainder.splitn(2, '=');
                let key = parts
                    .next()
                    .map(str::trim)
                    .filter(|s| !s.is_empty())
                    .ok_or_else(|| anyhow::anyhow!("line {}: ENV requires KEY=VALUE", line_no))?;
                let value = parts
                    .next()
                    .map(str::to_string)
                    .ok_or_else(|| anyhow::anyhow!("line {}: ENV requires KEY=VALUE", line_no))?;
                StepKind::Env {
                    key: key.to_string(),
                    value,
                }
            }
            Command::Echo => {
                if remainder.is_empty() {
                    bail!("line {}: ECHO requires a message", line_no);
                }
                StepKind::Echo(remainder.to_string())
            }
            Command::Run => {
                if remainder.is_empty() {
                    bail!("line {}: RUN requires a command", line_no);
                }
                StepKind::Run(remainder.to_string())
            }
            Command::RunBg => {
                if remainder.is_empty() {
                    bail!("line {}: RUN_BG requires a command", line_no);
                }
                StepKind::RunBg(remainder.to_string())
            }
            Command::Copy => {
                let mut p = remainder.split_whitespace();
                let from = p.next().ok_or_else(|| {
                    anyhow::anyhow!("line {}: COPY requires <from> <to>", line_no)
                })?;
                let to = p.next().ok_or_else(|| {
                    anyhow::anyhow!("line {}: COPY requires <from> <to>", line_no)
                })?;
                StepKind::Copy {
                    from: from.to_string(),
                    to: to.to_string(),
                }
            }
            Command::CopyGit => {
                let mut p = remainder.split_whitespace();
                let rev = p.next().ok_or_else(|| {
                    anyhow::anyhow!("line {}: COPY_GIT requires <rev> <from> <to>", line_no)
                })?;
                let from = p.next().ok_or_else(|| {
                    anyhow::anyhow!("line {}: COPY_GIT requires <rev> <from> <to>", line_no)
                })?;
                let to = p.next().ok_or_else(|| {
                    anyhow::anyhow!("line {}: COPY_GIT requires <rev> <from> <to>", line_no)
                })?;
                StepKind::CopyGit {
                    rev: rev.to_string(),
                    from: from.to_string(),
                    to: to.to_string(),
                }
            }
            Command::Symlink => {
                let mut p = remainder.split_whitespace();
                let from = p.next().ok_or_else(|| {
                    anyhow::anyhow!("line {}: SYMLINK requires <link> <target>", line_no)
                })?;
                let to = p.next().ok_or_else(|| {
                    anyhow::anyhow!("line {}: SYMLINK requires <link> <target>", line_no)
                })?;
                StepKind::Symlink {
                    from: from.to_string(),
                    to: to.to_string(),
                }
            }
            Command::Mkdir => {
                if remainder.is_empty() {
                    bail!("line {}: MKDIR requires a path", line_no);
                }
                StepKind::Mkdir(remainder.to_string())
            }
            Command::Ls => {
                let path = remainder
                    .split_whitespace()
                    .next()
                    .filter(|s| !s.is_empty())
                    .map(|s| s.to_string());
                StepKind::Ls(path)
            }
            Command::Write => {
                let mut p = remainder.splitn(2, ' ');
                let path = p.next().filter(|s| !s.is_empty()).ok_or_else(|| {
                    anyhow::anyhow!("line {}: WRITE requires <path> <contents>", line_no)
                })?;
                let contents = p.next().filter(|s| !s.is_empty()).ok_or_else(|| {
                    anyhow::anyhow!("line {}: WRITE requires <path> <contents>", line_no)
                })?;
                StepKind::Write {
                    path: path.to_string(),
                    contents: contents.to_string(),
                }
            }
            Command::Cat => {
                let path = remainder
                    .split_whitespace()
                    .next()
                    .filter(|s| !s.is_empty())
                    .ok_or_else(|| anyhow::anyhow!("line {}: CAT requires <path>", line_no))?;
                StepKind::Cat(path.to_string())
            }
            Command::Exit => {
                if remainder.is_empty() {
                    bail!("line {}: EXIT requires a code", line_no);
                }
                let code: i32 = remainder.parse().map_err(|_| {
                    anyhow::anyhow!("line {}: EXIT code must be an integer", line_no)
                })?;
                StepKind::Exit(code)
            }
        };

        steps.push(Step {
            guards: all_groups,
            kind,
        });
    }
    Ok(steps)
}
