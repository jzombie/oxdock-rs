use anyhow::{Result, anyhow, bail};
use std::collections::{HashMap, VecDeque};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Command {
    Workdir,
    Workspace,
    Env,
    Echo,
    Run,
    RunBg,
    Copy,
    Capture,
    CopyGit,
    Symlink,
    Mkdir,
    Ls,
    Cwd,
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
    Command::Capture,
    Command::CopyGit,
    Command::Symlink,
    Command::Mkdir,
    Command::Ls,
    Command::Cwd,
    Command::Cat,
    Command::Write,
    Command::Exit,
];

fn platform_matches(target: PlatformGuard) -> bool {
    #[allow(clippy::disallowed_macros)]
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
    if groups.is_empty() {
        return true;
    }
    groups.iter().any(|g| guard_group_allows(g, script_envs))
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
    Env {
        key: String,
        value: String,
    },
    Run(String),
    Echo(String),
    RunBg(String),
    Copy {
        from: String,
        to: String,
    },
    Symlink {
        from: String,
        to: String,
    },
    Mkdir(String),
    Ls(Option<String>),
    Cwd,
    Cat(String),
    Write {
        path: String,
        contents: String,
    },
    Capture {
        path: String,
        cmd: String,
    },
    CopyGit {
        rev: String,
        from: String,
        to: String,
    },
    Exit(i32),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Step {
    pub guards: Vec<Vec<Guard>>,
    pub kind: StepKind,
    pub scope_enter: usize,
    pub scope_exit: usize,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum WorkspaceTarget {
    Snapshot,
    Local,
}

#[derive(Clone)]
struct ScriptLine {
    line_no: usize,
    text: String,
}

fn strip_comments(input: &str) -> Result<String> {
    let mut output = String::with_capacity(input.len());
    let mut chars = input.chars().peekable();
    let mut in_line_comment = false;
    let mut block_depth = 0usize;
    let mut in_double_quote = false;
    let mut in_single_quote = false;
    let mut double_escape = false;
    let mut single_escape = false;

    while let Some(ch) = chars.next() {
        if in_line_comment {
            if ch == '\n' {
                in_line_comment = false;
                output.push(ch);
            }
            continue;
        }

        if block_depth > 0 {
            if ch == '/' && matches!(chars.peek(), Some('*')) {
                chars.next();
                block_depth += 1;
                continue;
            }
            if ch == '*' && matches!(chars.peek(), Some('/')) {
                chars.next();
                block_depth -= 1;
                continue;
            }
            if ch == '\n' {
                output.push('\n');
            }
            continue;
        }

        if !in_double_quote && !in_single_quote && ch == '/' {
            match chars.peek() {
                Some('/') => {
                    chars.next();
                    in_line_comment = true;
                    continue;
                }
                Some('*') => {
                    chars.next();
                    block_depth = 1;
                    continue;
                }
                _ => {}
            }
        }

        output.push(ch);

        if in_double_quote {
            if double_escape {
                double_escape = false;
            } else if ch == '\\' {
                double_escape = true;
            } else if ch == '"' {
                in_double_quote = false;
            }
            continue;
        }

        if in_single_quote {
            if single_escape {
                single_escape = false;
            } else if ch == '\\' {
                single_escape = true;
            } else if ch == '\'' {
                in_single_quote = false;
            }
            continue;
        }

        if ch == '"' {
            in_double_quote = true;
            double_escape = false;
        } else if ch == '\'' {
            in_single_quote = true;
            single_escape = false;
        }
    }

    if block_depth > 0 {
        bail!("unclosed block comment in DSL script");
    }

    Ok(output)
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
                _ => bail!("line {}: unknown platform '{}'", line_no, rest.trim()),
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

fn parse_guard_groups(block: &str, line_no: usize) -> Result<Vec<Vec<Guard>>> {
    let mut groups: Vec<Vec<Guard>> = Vec::new();
    for alt in block.split('|') {
        let mut group: Vec<Guard> = Vec::new();
        for entry in alt.split(',') {
            let trimmed = entry.trim();
            if trimmed.is_empty() {
                continue;
            }
            group.push(parse_guard(trimmed, line_no)?);
        }
        if !group.is_empty() {
            groups.push(group);
        }
    }
    if groups.is_empty() {
        bail!(
            "line {}: guard block must contain at least one guard",
            line_no
        );
    }
    Ok(groups)
}

fn combine_guard_groups(a: &[Vec<Guard>], b: &[Vec<Guard>]) -> Vec<Vec<Guard>> {
    if a.is_empty() {
        return b.to_vec();
    }
    if b.is_empty() {
        return a.to_vec();
    }
    let mut combined = Vec::new();
    for left in a {
        for right in b {
            let mut merged = left.clone();
            merged.extend(right.clone());
            combined.push(merged);
        }
    }
    combined
}

#[derive(Clone)]
struct ScopeFrame {
    line_no: usize,
    had_command: bool,
}

struct ScriptParser {
    lines: VecDeque<ScriptLine>,
    steps: Vec<Step>,
    guard_stack: Vec<Vec<Vec<Guard>>>,
    pending_guards: Option<Vec<Vec<Guard>>>,
    pending_can_open_block: bool,
    pending_scope_enters: usize,
    scope_stack: Vec<ScopeFrame>,
}

impl ScriptParser {
    fn new(input: &str) -> Result<Self> {
        let stripped = strip_comments(input)?;
        let lines = stripped
            .lines()
            .enumerate()
            .map(|(idx, raw)| ScriptLine {
                line_no: idx + 1,
                text: raw.to_string(),
            })
            .collect::<VecDeque<_>>();
        Ok(Self {
            lines,
            steps: Vec::new(),
            guard_stack: vec![Vec::new()],
            pending_guards: None,
            pending_can_open_block: false,
            pending_scope_enters: 0,
            scope_stack: Vec::new(),
        })
    }

    fn parse(mut self) -> Result<Vec<Step>> {
        while let Some(line) = self.next_line() {
            let trimmed = line.text.trim();
            if trimmed.is_empty() || trimmed.starts_with('#') {
                continue;
            }
            if trimmed == "{" {
                self.start_block_from_pending(line.line_no)?;
                continue;
            }
            if trimmed == "}" {
                self.end_block(line.line_no)?;
                continue;
            }
            if trimmed.starts_with('[') {
                self.handle_guard_line(line)?;
                continue;
            }
            self.handle_command(line.line_no, line.text, None)?;
        }

        if self.guard_stack.len() != 1 {
            bail!("unclosed guard block at end of script");
        }
        if let Some(pending) = &self.pending_guards
            && !pending.is_empty()
        {
            bail!("guard declared on final lines without a following command");
        }

        Ok(self.steps)
    }

    fn next_line(&mut self) -> Option<ScriptLine> {
        while let Some(line) = self.lines.pop_front() {
            if line.text.trim().is_empty() {
                continue;
            }
            if line.text.trim_start().starts_with('#') {
                continue;
            }
            return Some(line);
        }
        None
    }

    fn push_front(&mut self, line_no: usize, text: String) {
        self.lines.push_front(ScriptLine { line_no, text });
    }

    fn handle_guard_line(&mut self, first_line: ScriptLine) -> Result<()> {
        let mut buf = String::new();
        let remainder: String;
        let mut current = first_line.text.trim_start().to_string();
        let mut closing_line = first_line.line_no;
        if !current.starts_with('[') {
            bail!("line {}: guard must start with '['", first_line.line_no);
        }
        current.remove(0);

        loop {
            if let Some(idx) = current.find(']') {
                buf.push_str(&current[..idx]);
                remainder = current[idx + 1..].to_string();
                break;
            } else {
                buf.push_str(&current);
                buf.push('\n');
                let next = self
                    .lines
                    .pop_front()
                    .ok_or_else(|| anyhow!("line {}: guard must close with ']'", closing_line))?;
                closing_line = next.line_no;
                current = next.text.trim().to_string();
            }
        }

        let groups = parse_guard_groups(&buf, first_line.line_no)?;
        let remainder_trimmed = {
            let trimmed = remainder.trim();
            if trimmed.starts_with('#') {
                ""
            } else {
                trimmed
            }
        };

        if let Some(after_brace) = remainder_trimmed.strip_prefix('{') {
            let after = after_brace.trim_start();
            if !after.is_empty() {
                self.push_front(closing_line, after.to_string());
            }
            self.start_block(groups, first_line.line_no)?;
            return Ok(());
        }

        if remainder_trimmed.is_empty() {
            self.stash_pending_guard(groups);
            self.pending_can_open_block = true;
            return Ok(());
        }

        self.pending_can_open_block = false;
        self.handle_command(closing_line, remainder_trimmed.to_string(), Some(groups))
    }

    fn stash_pending_guard(&mut self, groups: Vec<Vec<Guard>>) {
        self.pending_guards = Some(if let Some(existing) = self.pending_guards.take() {
            combine_guard_groups(&existing, &groups)
        } else {
            groups
        });
    }

    fn start_block_from_pending(&mut self, line_no: usize) -> Result<()> {
        let guards = self
            .pending_guards
            .take()
            .ok_or_else(|| anyhow!("line {}: '{{' without a pending guard", line_no))?;
        if !self.pending_can_open_block {
            bail!("line {}: '{{' must directly follow a guard", line_no);
        }
        self.pending_can_open_block = false;
        self.start_block(guards, line_no)
    }

    fn start_block(&mut self, guards: Vec<Vec<Guard>>, line_no: usize) -> Result<()> {
        let with_pending = if let Some(pending) = self.pending_guards.take() {
            combine_guard_groups(&pending, &guards)
        } else {
            guards
        };
        let parent = self.guard_stack.last().cloned().unwrap_or_default();
        let next = if parent.is_empty() {
            with_pending
        } else if with_pending.is_empty() {
            parent
        } else {
            combine_guard_groups(&parent, &with_pending)
        };
        self.guard_stack.push(next);
        self.scope_stack.push(ScopeFrame {
            line_no,
            had_command: false,
        });
        self.pending_scope_enters += 1;
        Ok(())
    }

    fn end_block(&mut self, line_no: usize) -> Result<()> {
        if self.guard_stack.len() == 1 {
            bail!("line {}: unexpected '}}'", line_no);
        }
        if self.pending_guards.is_some() {
            bail!(
                "line {}: guard declared immediately before '}}' without a command",
                line_no
            );
        }
        let frame = self
            .scope_stack
            .last()
            .cloned()
            .ok_or_else(|| anyhow!("line {}: scope stack underflow", line_no))?;
        if !frame.had_command {
            bail!(
                "line {}: guard block starting on line {} must contain at least one command",
                line_no,
                frame.line_no
            );
        }
        let step = self
            .steps
            .last_mut()
            .ok_or_else(|| anyhow!("line {}: guard block closed without any commands", line_no))?;
        step.scope_exit += 1;
        self.scope_stack.pop();
        self.guard_stack.pop();
        Ok(())
    }

    fn guard_context(&mut self, inline: Option<Vec<Vec<Guard>>>) -> Vec<Vec<Guard>> {
        let mut context = if let Some(top) = self.guard_stack.last() {
            top.clone()
        } else {
            Vec::new()
        };
        if let Some(pending) = self.pending_guards.take() {
            context = if context.is_empty() {
                pending
            } else {
                combine_guard_groups(&context, &pending)
            };
            self.pending_can_open_block = false;
        }
        if let Some(inline_groups) = inline {
            context = if context.is_empty() {
                inline_groups
            } else {
                combine_guard_groups(&context, &inline_groups)
            };
        }
        context
    }

    fn handle_command(
        &mut self,
        line_no: usize,
        text: String,
        inline_guards: Option<Vec<Vec<Guard>>>,
    ) -> Result<()> {
        let trimmed = text.trim();
        if trimmed.is_empty() {
            return Ok(());
        }
        let (op_str, rest_str) = split_op_and_rest(trimmed);
        let cmd = Command::parse(op_str)
            .ok_or_else(|| anyhow!("line {}: unknown instruction '{}'", line_no, op_str))?;
        let mut remainder = rest_str.to_string();
        if cmd != Command::Run
            && cmd != Command::RunBg
            && let Some(idx) = remainder.find(';')
        {
            let first = remainder[..idx].trim().to_string();
            let tail = remainder[idx + 1..].trim();
            if !tail.is_empty() {
                self.push_front(line_no, tail.to_string());
            }
            remainder = first;
        }

        let guards = self.guard_context(inline_guards);
        let kind = build_step_kind(cmd, &remainder, line_no)?;
        let scope_enter = self.pending_scope_enters;
        self.pending_scope_enters = 0;
        for frame in self.scope_stack.iter_mut() {
            frame.had_command = true;
        }
        self.steps.push(Step {
            guards,
            kind,
            scope_enter,
            scope_exit: 0,
        });
        Ok(())
    }
}

fn split_op_and_rest(input: &str) -> (&str, &str) {
    if let Some((idx, _)) = input.char_indices().find(|(_, ch)| ch.is_whitespace()) {
        let op = &input[..idx];
        let rest = input[idx..].trim();
        (op, rest)
    } else {
        (input, "")
    }
}

fn build_step_kind(cmd: Command, remainder: &str, line_no: usize) -> Result<StepKind> {
    let kind = match cmd {
        Command::Workdir => {
            if remainder.is_empty() {
                bail!("line {}: WORKDIR requires a path", line_no);
            }
            StepKind::Workdir(remainder.to_string())
        }
        Command::Workspace => {
            let target = match remainder {
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
                .ok_or_else(|| anyhow!("line {}: ENV requires KEY=VALUE", line_no))?;
            let value = parts
                .next()
                .map(str::to_string)
                .ok_or_else(|| anyhow!("line {}: ENV requires KEY=VALUE", line_no))?;
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
            let from = p
                .next()
                .ok_or_else(|| anyhow!("line {}: COPY requires <from> <to>", line_no))?;
            let to = p
                .next()
                .ok_or_else(|| anyhow!("line {}: COPY requires <from> <to>", line_no))?;
            StepKind::Copy {
                from: from.to_string(),
                to: to.to_string(),
            }
        }
        Command::Capture => {
            let mut p = remainder.splitn(2, ' ');
            let path = p
                .next()
                .map(str::trim)
                .filter(|s| !s.is_empty())
                .ok_or_else(|| anyhow!("line {}: CAPTURE requires <path> <command>", line_no))?;
            let cmd = p
                .next()
                .map(str::to_string)
                .ok_or_else(|| anyhow!("line {}: CAPTURE requires <path> <command>", line_no))?;
            StepKind::Capture {
                path: path.to_string(),
                cmd,
            }
        }
        Command::CopyGit => {
            let mut p = remainder.split_whitespace();
            let rev = p
                .next()
                .ok_or_else(|| anyhow!("line {}: COPY_GIT requires <rev> <from> <to>", line_no))?;
            let from = p
                .next()
                .ok_or_else(|| anyhow!("line {}: COPY_GIT requires <rev> <from> <to>", line_no))?;
            let to = p
                .next()
                .ok_or_else(|| anyhow!("line {}: COPY_GIT requires <rev> <from> <to>", line_no))?;
            StepKind::CopyGit {
                rev: rev.to_string(),
                from: from.to_string(),
                to: to.to_string(),
            }
        }
        Command::Symlink => {
            let mut p = remainder.split_whitespace();
            let from = p
                .next()
                .ok_or_else(|| anyhow!("line {}: SYMLINK requires <link> <target>", line_no))?;
            let to = p
                .next()
                .ok_or_else(|| anyhow!("line {}: SYMLINK requires <link> <target>", line_no))?;
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
        Command::Cwd => StepKind::Cwd,
        Command::Write => {
            let mut p = remainder.splitn(2, ' ');
            let path = p
                .next()
                .filter(|s| !s.is_empty())
                .ok_or_else(|| anyhow!("line {}: WRITE requires <path> <contents>", line_no))?;
            let contents = p
                .next()
                .filter(|s| !s.is_empty())
                .ok_or_else(|| anyhow!("line {}: WRITE requires <path> <contents>", line_no))?;
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
                .ok_or_else(|| anyhow!("line {}: CAT requires <path>", line_no))?;
            StepKind::Cat(path.to_string())
        }
        Command::Exit => {
            if remainder.is_empty() {
                bail!("line {}: EXIT requires a code", line_no);
            }
            let code: i32 = remainder
                .parse()
                .map_err(|_| anyhow!("line {}: EXIT code must be an integer", line_no))?;
            StepKind::Exit(code)
        }
    };
    Ok(kind)
}

pub fn parse_script(input: &str) -> Result<Vec<Step>> {
    ScriptParser::new(input)?.parse()
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
            Command::Capture => "CAPTURE",
            Command::CopyGit => "COPY_GIT",
            Command::Symlink => "SYMLINK",
            Command::Mkdir => "MKDIR",
            Command::Ls => "LS",
            Command::Cwd => "CWD",
            Command::Cat => "CAT",
            Command::Write => "WRITE",
            Command::Exit => "EXIT",
        }
    }

    pub fn parse(op: &str) -> Option<Self> {
        COMMANDS.iter().copied().find(|c| c.as_str() == op)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    fn commands_are_case_sensitive() {
        for bad in ["run echo hi", "Run echo hi", "rUn echo hi", "write foo bar"] {
            let err = parse_script(bad).expect_err("mixed/lowercase commands must fail");
            assert!(
                err.to_string().contains("unknown instruction"),
                "unexpected error for '{bad}': {err}"
            );
        }
    }

    #[test]
    fn string_dsl_supports_rust_style_comments() {
        let script = indoc! {r#"
            // leading comment line
            WORKDIR /tmp // inline comment
            RUN echo "keep // literal"
            /* block comment
               WORKDIR ignored
               /* nested inner */
               RUN ignored as well
            */
            RUN echo final
            RUN echo 'literal /* stay */ value'
        "#};
        let steps = parse_script(script).expect("parse ok");
        assert_eq!(steps.len(), 4, "expected 4 executable steps");
        match &steps[0].kind {
            StepKind::Workdir(path) => assert_eq!(path, "/tmp"),
            other => panic!("expected WORKDIR, saw {:?}", other),
        }
        match &steps[1].kind {
            StepKind::Run(cmd) => assert_eq!(cmd, "echo \"keep // literal\""),
            other => panic!("expected RUN, saw {:?}", other),
        }
        match &steps[2].kind {
            StepKind::Run(cmd) => assert_eq!(cmd, "echo final"),
            other => panic!("expected RUN, saw {:?}", other),
        }
        match &steps[3].kind {
            StepKind::Run(cmd) => assert_eq!(cmd, "echo 'literal /* stay */ value'"),
            other => panic!("expected RUN, saw {:?}", other),
        }
    }

    #[test]
    fn string_dsl_errors_on_unclosed_block_comment() {
        let err =
            parse_script("RUN echo hi /*").expect_err("unclosed block comment should error");
        assert!(
            err.to_string().contains("unclosed block comment"),
            "unexpected error message: {err}"
        );
    }

    #[test]
    fn env_equals_guard_respects_inversion() {
        let mut envs = HashMap::new();
        envs.insert("FOO".to_string(), "bar".to_string());
        let guard = Guard::EnvEquals {
            key: "FOO".into(),
            value: "bar".into(),
            invert: false,
        };
        assert!(guard_allows(&guard, &envs));

        let inverted = Guard::EnvEquals {
            key: "FOO".into(),
            value: "bar".into(),
            invert: true,
        };
        assert!(!guard_allows(&inverted, &envs));
    }

    #[test]
    fn guards_allow_any_act_as_or_of_ands() {
        let mut envs = HashMap::new();
        envs.insert("MODE".to_string(), "beta".to_string());
        let groups = vec![
            vec![Guard::EnvEquals {
                key: "MODE".into(),
                value: "alpha".into(),
                invert: false,
            }],
            vec![Guard::EnvEquals {
                key: "MODE".into(),
                value: "beta".into(),
                invert: false,
            }],
        ];
        assert!(guards_allow_any(&groups, &envs));
    }

    #[test]
    fn guards_allow_any_falls_back_to_false_when_all_fail() {
        let envs = HashMap::new();
        let groups = vec![vec![Guard::EnvExists {
            key: "MISSING".into(),
            invert: false,
        }]];
        assert!(!guards_allow_any(&groups, &envs));
    }

    #[test]
    fn multi_line_guard_blocks_apply_to_next_command() {
        let script = indoc! {r#"
            [ env:FOO=bar,
              linux
            ]
            RUN echo guarded
        "#};
        let steps = parse_script(script).expect("parse ok");
        assert_eq!(steps.len(), 1);
        assert_eq!(steps[0].guards.len(), 1);
        assert!(matches!(steps[0].kind, StepKind::Run(ref cmd) if cmd == "echo guarded"));
    }

    #[test]
    fn guarded_brace_blocks_apply_to_all_inner_steps() {
        let script = indoc! {r#"
            [env:APP=demo] {
                WRITE one.txt 1
                WRITE two.txt 2
            }
            WRITE three.txt 3
        "#};
        let steps = parse_script(script).expect("parse ok");
        assert_eq!(steps.len(), 3);
        assert_eq!(steps[0].guards.len(), 1);
        assert_eq!(steps[1].guards.len(), 1);
        assert!(steps[2].guards.is_empty());
    }

    #[test]
    fn nested_guard_blocks_stack() {
        let script = indoc! {r#"
            [env:OUTER] {
                [env:INNER] {
                    WRITE nested.txt yes
                }
            }
        "#};
        let steps = parse_script(script).expect("parse ok");
        assert_eq!(steps.len(), 1);
        assert_eq!(steps[0].guards.len(), 1);
        assert_eq!(steps[0].guards[0].len(), 2);
    }

    #[test]
    fn brace_blocks_require_guard() {
        let script = indoc! {r#"
            {
                WRITE nope.txt hi
            }
        "#};
        let err = parse_script(script).expect_err("block without guard must fail");
        assert!(err.to_string().contains("'{'"), "unexpected error: {err}");
    }

    #[test]
    fn guard_lines_chain_before_block() {
        let script = indoc! {r#"
            [env:FOO]
            [linux]
            {
                WRITE ok.txt hi
            }
        "#};
        let steps = parse_script(script).expect("parse ok");
        assert_eq!(steps.len(), 1);
        assert_eq!(steps[0].guards[0].len(), 2);
    }

    #[test]
    fn guard_block_emits_scope_markers() {
        let script = indoc! {r#"
            ENV RUN=1
            [env:RUN] {
                WRITE one.txt 1
                WRITE two.txt 2
            }
            WRITE three.txt 3
        "#};
        let steps = parse_script(script).expect("parse ok");
        assert_eq!(steps.len(), 4);
        assert_eq!(steps[1].scope_enter, 1);
        assert_eq!(steps[1].scope_exit, 0);
        assert_eq!(steps[2].scope_enter, 0);
        assert_eq!(steps[2].scope_exit, 1);
        assert_eq!(steps[3].scope_enter, 0);
        assert_eq!(steps[3].scope_exit, 0);
    }

    #[test]
    fn nested_guard_block_scopes_stack_counts() {
        let script = indoc! {r#"
            [env:OUTER] {
                [env:INNER] {
                    WRITE deep.txt ok
                }
            }
        "#};
        let steps = parse_script(script).expect("parse ok");
        assert_eq!(steps.len(), 1);
        assert_eq!(steps[0].scope_enter, 2);
        assert_eq!(steps[0].scope_exit, 2);
    }

    #[test]
    fn guard_block_must_contain_command() {
        let script = indoc! {r#"
            [env:FOO]
            {
            }
        "#};
        let err = parse_script(script).expect_err("empty block must fail");
        assert!(
            err.to_string()
                .contains("must contain at least one command"),
            "unexpected error: {err}"
        );
    }
}
