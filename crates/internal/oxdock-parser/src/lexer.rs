use crate::ast::{Guard, PlatformGuard, StepKind, WorkspaceTarget};
use anyhow::{Result, anyhow, bail};
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "dsl.pest"]
struct LanguageParser;

pub const LANGUAGE_SPEC: &str = include_str!("dsl.pest");

#[derive(Debug, Clone)]
pub(crate) struct GuardToken {
    pub line_end: usize,
    pub groups: Vec<Vec<Guard>>,
}

#[derive(Debug, Clone)]
pub(crate) struct LexedCommand {
    pub line_no: usize,
    pub kind: StepKind,
}

#[derive(Debug, Clone)]
pub(crate) enum Token {
    Guard(GuardToken),
    BlockStart { line_no: usize },
    BlockEnd { line_no: usize },
    Command(LexedCommand),
}

pub(crate) fn lex_script(input: &str) -> Result<Vec<Token>> {
    let mut tokens = Vec::new();
    let mut pairs = LanguageParser::parse(Rule::script, input)
        .map_err(|err| anyhow!(err.to_string()))?;
    let Some(root) = pairs.next() else {
        return Ok(tokens);
    };
    for pair in root.into_inner() {
        match pair.as_rule() {
            Rule::blank => {}
            Rule::hash_comment => {}
            Rule::guard_line => tokens.push(Token::Guard(parse_guard_line(pair)?)),
            Rule::block_start => tokens.push(Token::BlockStart {
                line_no: pair.as_span().start_pos().line_col().0,
            }),
            Rule::block_end => tokens.push(Token::BlockEnd {
                line_no: pair.as_span().start_pos().line_col().0,
            }),
            Rule::workdir_command
            | Rule::workspace_command
            | Rule::env_command
            | Rule::echo_command
            | Rule::run_command
            | Rule::run_bg_command
            | Rule::copy_command
            | Rule::capture_command
            | Rule::copy_git_command
            | Rule::symlink_command
            | Rule::mkdir_command
            | Rule::ls_command
            | Rule::cwd_command
            | Rule::cat_command
            | Rule::write_command
            | Rule::exit_command => tokens.push(Token::Command(parse_command(pair)?)),
            Rule::semicolon => {}
            Rule::EOI => {}
            Rule::COMMENT => {}
            other => bail!("unexpected parser rule {:?}", other),
        }
    }
    Ok(tokens)
}

fn parse_command(pair: pest::iterators::Pair<Rule>) -> Result<LexedCommand> {
    let line_no = pair.as_span().start_pos().line_col().0;
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
            let mut args = parse_args(pair)?;
            StepKind::CopyGit {
                rev: args.remove(0),
                from: args.remove(0),
                to: args.remove(0),
            }
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
    Ok(LexedCommand { line_no, kind })
}

fn parse_single_arg(pair: pest::iterators::Pair<Rule>) -> Result<String> {
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::argument {
            return parse_argument(inner);
        }
    }
    bail!("missing argument")
}

fn parse_single_arg_from_pair(pair: pest::iterators::Pair<Rule>) -> Result<String> {
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::argument {
            return parse_argument(inner);
        }
    }
    bail!("missing argument")
}

fn parse_args(pair: pest::iterators::Pair<Rule>) -> Result<Vec<String>> {
    let mut args = Vec::new();
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::argument {
            args.push(parse_argument(inner)?);
        }
    }
    Ok(args)
}

fn parse_argument(pair: pest::iterators::Pair<Rule>) -> Result<String> {
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::quoted_string => parse_quoted_string(inner),
        Rule::unquoted_arg => Ok(inner.as_str().to_string()),
        _ => unreachable!(),
    }
}

fn parse_quoted_string(pair: pest::iterators::Pair<Rule>) -> Result<String> {
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

fn parse_workspace_target(pair: pest::iterators::Pair<Rule>) -> Result<WorkspaceTarget> {
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

fn parse_env_pair(pair: pest::iterators::Pair<Rule>) -> Result<(String, String)> {
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::env_pair {
            let mut parts = inner.into_inner();
            let key = parts.next().unwrap().as_str().to_string();
            let value = parts.next().unwrap().as_str().to_string();
            return Ok((key, value));
        }
    }
    bail!("missing env pair")
}

fn parse_message(pair: pest::iterators::Pair<Rule>) -> Result<String> {
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::message {
            return parse_concatenated_string(inner);
        }
    }
    bail!("missing message")
}

fn parse_run_args(pair: pest::iterators::Pair<Rule>) -> Result<String> {
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::run_args {
            return parse_raw_concatenated_string(inner);
        }
    }
    bail!("missing run args")
}

fn parse_run_args_from_pair(pair: pest::iterators::Pair<Rule>) -> Result<String> {
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::run_args {
            return parse_raw_concatenated_string(inner);
        }
    }
    bail!("missing run args")
}

fn parse_concatenated_string(pair: pest::iterators::Pair<Rule>) -> Result<String> {
    let mut body = String::new();
    let mut last_end = None;
    for part in pair.into_inner() {
        let span = part.as_span();
        if let Some(end) = last_end {
            if span.start() > end {
                body.push(' ');
            }
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

fn parse_raw_concatenated_string(pair: pest::iterators::Pair<Rule>) -> Result<String> {
    let mut body = String::new();
    let mut last_end = None;
    for part in pair.into_inner() {
        let span = part.as_span();
        if let Some(end) = last_end {
            if span.start() > end {
                body.push(' ');
            }
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

fn parse_exit_code(pair: pest::iterators::Pair<Rule>) -> Result<i32> {
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::exit_code {
            return inner.as_str().parse().map_err(|_| anyhow!("invalid exit code"));
        }
    }
    bail!("missing exit code")
}

fn parse_guard_line(pair: pest::iterators::Pair<Rule>) -> Result<GuardToken> {
    let span = pair.as_span();
    let (line_end, _) = span.end_pos().line_col();

    let mut groups = Vec::new();
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::guard_groups {
            groups = parse_guard_groups(inner)?;
        }
    }

    Ok(GuardToken { line_end, groups })
}

fn parse_guard_groups(pair: pest::iterators::Pair<Rule>) -> Result<Vec<Vec<Guard>>> {
    let mut groups = Vec::new();
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::guard_conjunction {
            groups.push(parse_guard_conjunction(inner)?);
        }
    }
    Ok(groups)
}

fn parse_guard_conjunction(pair: pest::iterators::Pair<Rule>) -> Result<Vec<Guard>> {
    let mut group = Vec::new();
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::guard_term {
            group.push(parse_guard_term(inner)?);
        }
    }
    Ok(group)
}

fn parse_guard_term(pair: pest::iterators::Pair<Rule>) -> Result<Guard> {
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

fn parse_env_guard(pair: pest::iterators::Pair<Rule>, invert: bool) -> Result<Guard> {
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
                        value = Some(inner_comp.into_inner().next().unwrap().as_str().trim().to_string());
                    }
                    Rule::not_equals => {
                        is_not_equals = true;
                        value = Some(inner_comp.into_inner().next().unwrap().as_str().trim().to_string());
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

fn parse_platform_guard(pair: pest::iterators::Pair<Rule>, invert: bool) -> Result<Guard> {
    let mut tag = "";
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::platform_tag {
            tag = inner.as_str();
        }
    }
    parse_platform_tag(tag, invert)
}

fn parse_bare_platform(pair: pest::iterators::Pair<Rule>, invert: bool) -> Result<Guard> {
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
