use crate::{Guard, PlatformGuard};
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
pub(crate) struct CommandToken {
    pub line_no: usize,
    pub text: String,
}

#[derive(Debug, Clone)]
pub(crate) enum Token {
    Guard(GuardToken),
    BlockStart { line_no: usize },
    BlockEnd { line_no: usize },
    Command(CommandToken),
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
            Rule::run_command | Rule::run_bg_command | Rule::simple_command => {
                tokens.push(Token::Command(parse_command(pair)))
            }
            Rule::semicolon => {}
            Rule::EOI => {}
            Rule::COMMENT => {}
            other => bail!("unexpected parser rule {:?}", other),
        }
    }
    Ok(tokens)
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
            Rule::env_key => key = inner.as_str().to_string(),
            Rule::env_comparison => {
                let inner_comp = inner.into_inner().next().unwrap();
                match inner_comp.as_rule() {
                    Rule::equals => {
                        value = Some(inner_comp.into_inner().next().unwrap().as_str().to_string());
                    }
                    Rule::not_equals => {
                        is_not_equals = true;
                        value = Some(inner_comp.into_inner().next().unwrap().as_str().to_string());
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

fn parse_command(pair: pest::iterators::Pair<Rule>) -> CommandToken {
    let span = pair.as_span();
    let (line_no, _) = span.start_pos().line_col();

    let prefix = match pair.as_rule() {
        Rule::run_command => "RUN ",
        Rule::run_bg_command => "RUN_BG ",
        _ => "",
    };

    let mut body = String::new();
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::run_args | Rule::simple_args => {
                if !body.is_empty() {
                    body.push(' ');
                }
                let mut last_end = None;
                for arg_part in inner.into_inner() {
                    let span = arg_part.as_span();
                    if let Some(end) = last_end {
                        if span.start() > end {
                            body.push(' ');
                        }
                    }
                    body.push_str(arg_part.as_str());
                    last_end = Some(span.end());
                }
            }
            Rule::command_name => {
                body.push_str(inner.as_str());
            }
            _ => {}
        }
    }

    CommandToken {
        line_no,
        text: format!("{}{}", prefix, body).trim().to_string(),
    }
}