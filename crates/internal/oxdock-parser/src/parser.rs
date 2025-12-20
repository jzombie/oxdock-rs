use crate::ast::{Guard, PlatformGuard, Step, StepKind, WorkspaceTarget};
use crate::lexer::{self, RawToken, Rule};
use anyhow::{Result, anyhow, bail};
use pest::iterators::Pair;
use std::collections::VecDeque;

#[derive(Clone)]
struct ScopeFrame {
    line_no: usize,
    had_command: bool,
}

pub struct ScriptParser<'a> {
    tokens: VecDeque<RawToken<'a>>,
    steps: Vec<Step>,
    guard_stack: Vec<Vec<Vec<Guard>>>,
    pending_guards: Option<Vec<Vec<Guard>>>,
    pending_inline_guards: Option<Vec<Vec<Guard>>>,
    pending_can_open_block: bool,
    pending_scope_enters: usize,
    scope_stack: Vec<ScopeFrame>,
}

impl<'a> ScriptParser<'a> {
    pub fn new(input: &'a str) -> Result<Self> {
        let tokens = VecDeque::from(lexer::tokenize(input)?);
        Ok(Self {
            tokens,
            steps: Vec::new(),
            guard_stack: vec![Vec::new()],
            pending_guards: None,
            pending_inline_guards: None,
            pending_can_open_block: false,
            pending_scope_enters: 0,
            scope_stack: Vec::new(),
        })
    }

    pub fn parse(mut self) -> Result<Vec<Step>> {
        while let Some(token) = self.tokens.pop_front() {
            match token {
                RawToken::Guard { pair, line_end } => {
                    let groups = parse_guard_line(pair)?;
                    self.handle_guard_token(line_end, groups)?
                }
                RawToken::BlockStart { line_no } => self.start_block_from_pending(line_no)?,
                RawToken::BlockEnd { line_no } => self.end_block(line_no)?,
                RawToken::Command { pair, line_no } => {
                    let kind = parse_command(pair)?;
                    self.handle_command_token(line_no, kind)?
                }
            }
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

    fn handle_guard_token(&mut self, line_end: usize, groups: Vec<Vec<Guard>>) -> Result<()> {
        if let Some(RawToken::Command { line_no, .. }) = self.tokens.front()
            && *line_no == line_end
        {
            self.pending_inline_guards = Some(groups);
            self.pending_can_open_block = false;
            return Ok(());
        }
        self.stash_pending_guard(groups);
        self.pending_can_open_block = true;
        Ok(())
    }

    fn handle_command_token(&mut self, line_no: usize, kind: StepKind) -> Result<()> {
        let inline = self.pending_inline_guards.take();
        self.handle_command(line_no, kind, inline)
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
        _line_no: usize,
        kind: StepKind,
        inline_guards: Option<Vec<Vec<Guard>>>,
    ) -> Result<()> {
        let guards = self.guard_context(inline_guards);
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

pub fn parse_script(input: &str) -> Result<Vec<Step>> {
    ScriptParser::new(input)?.parse()
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

fn parse_command(pair: Pair<Rule>) -> Result<StepKind> {
    let kind = match pair.as_rule() {
        Rule::workdir_command => {
            let arg = parse_single_arg(pair)?;
            StepKind::Workdir(arg)
        }
        Rule::workspace_command => {
            let target = parse_workspace_target(pair)?;
            StepKind::Workspace(target)
        }
        Rule::env_command => {
            let (key, value) = parse_env_pair(pair)?;
            StepKind::Env { key, value }
        }
        Rule::echo_command => {
            let msg = parse_message(pair)?;
            StepKind::Echo(msg)
        }
        Rule::run_command => {
            let cmd = parse_run_args(pair)?;
            StepKind::Run(cmd)
        }
        Rule::run_bg_command => {
            let cmd = parse_run_args(pair)?;
            StepKind::RunBg(cmd)
        }
        Rule::copy_command => {
            let mut args = parse_args(pair)?;
            StepKind::Copy {
                from: args.remove(0),
                to: args.remove(0),
            }
        }
        Rule::capture_command => {
            let path = parse_single_arg_from_pair(pair.clone())?;
            let cmd = parse_run_args_from_pair(pair)?;
            StepKind::Capture { path, cmd }
        }
        Rule::copy_git_command => {
            let mut args = Vec::new();
            let mut include_dirty = false;
            for inner in pair.into_inner() {
                match inner.as_rule() {
                    Rule::include_dirty_flag => include_dirty = true,
                    Rule::argument => args.push(parse_argument(inner)?),
                    _ => {}
                }
            }
            if args.len() != 3 {
                bail!("COPY_GIT expects 3 arguments (rev, from, to)");
            }
            StepKind::CopyGit {
                rev: args.remove(0),
                from: args.remove(0),
                to: args.remove(0),
                include_dirty,
            }
        }
        Rule::hash_sha256_command => {
            let arg = parse_single_arg(pair)?;
            StepKind::HashSha256 { path: arg }
        }
        Rule::symlink_command => {
            let mut args = parse_args(pair)?;
            StepKind::Symlink {
                from: args.remove(0),
                to: args.remove(0),
            }
        }
        Rule::mkdir_command => {
            let arg = parse_single_arg(pair)?;
            StepKind::Mkdir(arg)
        }
        Rule::ls_command => {
            let args = parse_args(pair)?;
            StepKind::Ls(args.into_iter().next())
        }
        Rule::cwd_command => StepKind::Cwd,
        Rule::cat_command => {
            let arg = parse_single_arg(pair)?;
            StepKind::Cat(arg)
        }
        Rule::write_command => {
            let path = parse_single_arg_from_pair(pair.clone())?;
            let contents = parse_message(pair)?;
            StepKind::Write { path, contents }
        }
        Rule::exit_command => {
            let code = parse_exit_code(pair)?;
            StepKind::Exit(code)
        }
        _ => bail!("unknown command rule: {:?}", pair.as_rule()),
    };
    Ok(kind)
}

fn parse_single_arg(pair: Pair<Rule>) -> Result<String> {
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::argument {
            return parse_argument(inner);
        }
    }
    bail!("missing argument")
}

fn parse_single_arg_from_pair(pair: Pair<Rule>) -> Result<String> {
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::argument {
            return parse_argument(inner);
        }
    }
    bail!("missing argument")
}

fn parse_args(pair: Pair<Rule>) -> Result<Vec<String>> {
    let mut args = Vec::new();
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::argument {
            args.push(parse_argument(inner)?);
        }
    }
    Ok(args)
}

fn parse_argument(pair: Pair<Rule>) -> Result<String> {
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::quoted_string => parse_quoted_string(inner),
        Rule::unquoted_arg => Ok(inner.as_str().to_string()),
        _ => unreachable!(),
    }
}

fn parse_quoted_string(pair: Pair<Rule>) -> Result<String> {
    let s = pair.as_str();
    let _quote = s.chars().next().unwrap();
    let content = &s[1..s.len() - 1];

    let mut out = String::with_capacity(content.len());
    let mut escape = false;
    for ch in content.chars() {
        if escape {
            out.push(ch);
            escape = false;
        } else if ch == '\\' {
            escape = true;
        } else {
            out.push(ch);
        }
    }
    Ok(out)
}

fn parse_workspace_target(pair: Pair<Rule>) -> Result<WorkspaceTarget> {
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::workspace_target {
            return match inner.as_str().to_ascii_lowercase().as_str() {
                "snapshot" => Ok(WorkspaceTarget::Snapshot),
                "local" => Ok(WorkspaceTarget::Local),
                _ => bail!("unknown workspace target"),
            };
        }
    }
    bail!("missing workspace target")
}

fn parse_env_pair(pair: Pair<Rule>) -> Result<(String, String)> {
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::env_pair {
            let mut parts = inner.into_inner();
            let key = parts.next().unwrap().as_str().to_string();
            let value_pair = parts.next().unwrap();
            let value = match value_pair.as_rule() {
                Rule::env_value_part => {
                    let inner_val = value_pair.into_inner().next().unwrap();
                    match inner_val.as_rule() {
                        Rule::quoted_string => parse_quoted_string(inner_val)?,
                        Rule::unquoted_env_value => inner_val.as_str().to_string(),
                        _ => unreachable!(
                            "unexpected rule in env_value_part: {:?}",
                            inner_val.as_rule()
                        ),
                    }
                }
                _ => unreachable!("expected env_value_part"),
            };
            return Ok((key, value));
        }
    }
    bail!("missing env pair")
}

fn parse_message(pair: Pair<Rule>) -> Result<String> {
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::message {
            return parse_concatenated_string(inner);
        }
    }
    bail!("missing message")
}

fn parse_run_args(pair: Pair<Rule>) -> Result<String> {
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::run_args {
            return parse_raw_concatenated_string(inner);
        }
    }
    bail!("missing run args")
}

fn parse_run_args_from_pair(pair: Pair<Rule>) -> Result<String> {
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::run_args {
            return parse_raw_concatenated_string(inner);
        }
    }
    bail!("missing run args")
}

fn parse_concatenated_string(pair: Pair<Rule>) -> Result<String> {
    let mut body = String::new();
    let mut last_end = None;
    for part in pair.into_inner() {
        let span = part.as_span();
        if let Some(end) = last_end
            && span.start() > end
        {
            body.push(' ');
        }
        match part.as_rule() {
            Rule::quoted_string => body.push_str(&parse_quoted_string(part)?),
            Rule::unquoted_msg_content | Rule::unquoted_run_content => body.push_str(part.as_str()),
            _ => {}
        }
        last_end = Some(span.end());
    }
    Ok(body)
}

fn parse_raw_concatenated_string(pair: Pair<Rule>) -> Result<String> {
    let parts: Vec<_> = pair.into_inner().collect();
    if parts.len() == 1 && parts[0].as_rule() == Rule::quoted_string {
        let raw = parts[0].as_str();
        let unquoted = parse_quoted_string(parts[0].clone())?;
        let needs_quotes = unquoted.is_empty()
            || unquoted.chars().any(|c| c == ';' || c == '\n' || c == '\r')
            || unquoted.contains("//")
            || unquoted.contains("/*");
        if needs_quotes {
            return Ok(raw.to_string());
        }
        return Ok(unquoted);
    }
    let mut body = String::new();
    let mut last_end = None;
    for part in parts {
        let span = part.as_span();
        if let Some(end) = last_end
            && span.start() > end
        {
            body.push(' ');
        }
        match part.as_rule() {
            Rule::quoted_string => {
                let raw = part.as_str();
                let unquoted = parse_quoted_string(part.clone())?;
                // Preserve quotes if the content needs them to be parsed correctly
                // or to preserve argument grouping (spaces).
                let needs_quotes = unquoted.is_empty()
                    || unquoted
                        .chars()
                        .any(|c| c.is_whitespace() || c == ';' || c == '\n' || c == '\r')
                    || unquoted.contains("//")
                    || unquoted.contains("/*");

                if needs_quotes {
                    body.push_str(raw);
                } else {
                    body.push_str(&unquoted);
                }
            }
            Rule::unquoted_msg_content | Rule::unquoted_run_content => body.push_str(part.as_str()),
            _ => {}
        }
        last_end = Some(span.end());
    }
    Ok(body)
}

fn parse_exit_code(pair: Pair<Rule>) -> Result<i32> {
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::exit_code {
            return inner
                .as_str()
                .parse()
                .map_err(|_| anyhow!("invalid exit code"));
        }
    }
    bail!("missing exit code")
}

fn parse_guard_line(pair: Pair<Rule>) -> Result<Vec<Vec<Guard>>> {
    let mut groups = Vec::new();
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::guard_groups {
            groups = parse_guard_groups(inner)?;
        }
    }
    Ok(groups)
}

fn parse_guard_groups(pair: Pair<Rule>) -> Result<Vec<Vec<Guard>>> {
    let mut groups = Vec::new();
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::guard_conjunction {
            groups.push(parse_guard_conjunction(inner)?);
        }
    }
    Ok(groups)
}

fn parse_guard_conjunction(pair: Pair<Rule>) -> Result<Vec<Guard>> {
    let mut group = Vec::new();
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::guard_term {
            group.push(parse_guard_term(inner)?);
        }
    }
    Ok(group)
}

fn parse_guard_term(pair: Pair<Rule>) -> Result<Guard> {
    let mut invert = false;
    let mut guard = None;

    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::invert => invert = true,
            Rule::env_guard => guard = Some(parse_env_guard(inner, invert)?),
            Rule::platform_guard => guard = Some(parse_platform_guard(inner, invert)?),
            Rule::bare_platform => guard = Some(parse_bare_platform(inner, invert)?),
            _ => {}
        }
    }
    guard.ok_or_else(|| anyhow!("missing guard predicate"))
}

fn parse_env_guard(pair: Pair<Rule>, invert: bool) -> Result<Guard> {
    let mut key = String::new();
    let mut value = None;
    let mut is_not_equals = false;

    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::env_key => key = inner.as_str().trim().to_string(),
            Rule::env_comparison => {
                let inner_comp = inner.into_inner().next().unwrap();
                match inner_comp.as_rule() {
                    Rule::equals => {
                        value = Some(
                            inner_comp
                                .into_inner()
                                .next()
                                .unwrap()
                                .as_str()
                                .trim()
                                .to_string(),
                        );
                    }
                    Rule::not_equals => {
                        is_not_equals = true;
                        value = Some(
                            inner_comp
                                .into_inner()
                                .next()
                                .unwrap()
                                .as_str()
                                .trim()
                                .to_string(),
                        );
                    }
                    _ => {}
                }
            }
            _ => {}
        }
    }

    if let Some(val) = value {
        Ok(Guard::EnvEquals {
            key,
            value: val,
            invert: invert ^ is_not_equals,
        })
    } else {
        Ok(Guard::EnvExists { key, invert })
    }
}

fn parse_platform_guard(pair: Pair<Rule>, invert: bool) -> Result<Guard> {
    let mut tag = "";
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::platform_tag {
            tag = inner.as_str();
        }
    }
    parse_platform_tag(tag, invert)
}

fn parse_bare_platform(pair: Pair<Rule>, invert: bool) -> Result<Guard> {
    let tag = pair.into_inner().next().unwrap().as_str();
    parse_platform_tag(tag, invert)
}

fn parse_platform_tag(tag: &str, invert: bool) -> Result<Guard> {
    let target = match tag.to_ascii_lowercase().as_str() {
        "unix" => PlatformGuard::Unix,
        "windows" => PlatformGuard::Windows,
        "mac" | "macos" => PlatformGuard::Macos,
        "linux" => PlatformGuard::Linux,
        _ => bail!("unknown platform '{}'", tag),
    };
    Ok(Guard::Platform { target, invert })
}
