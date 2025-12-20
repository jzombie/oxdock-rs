use anyhow::{Result, anyhow, bail};
use pest::{Parser, iterators::Pair};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "dsl.pest"]
pub struct LanguageParser;

pub const LANGUAGE_SPEC: &str = include_str!("dsl.pest");

#[derive(Debug, Clone)]
pub enum RawToken<'a> {
    Guard {
        pair: Pair<'a, Rule>,
        line_end: usize,
    },
    BlockStart {
        line_no: usize,
    },
    BlockEnd {
        line_no: usize,
    },
    Command {
        pair: Pair<'a, Rule>,
        line_no: usize,
    },
}

pub fn tokenize(input: &str) -> Result<Vec<RawToken<'_>>> {
    let mut tokens = Vec::new();
    let mut pairs =
        LanguageParser::parse(Rule::script, input).map_err(|err| anyhow!(err.to_string()))?;
    let Some(root) = pairs.next() else {
        return Ok(tokens);
    };

    for pair in root.into_inner() {
        let line_no = pair.as_span().start_pos().line_col().0;
        match pair.as_rule() {
            Rule::blank | Rule::hash_comment | Rule::semicolon | Rule::EOI | Rule::COMMENT => {}
            Rule::guard_line => {
                let (line_end, _) = pair.as_span().end_pos().line_col();
                tokens.push(RawToken::Guard { pair, line_end });
            }
            Rule::block_start => tokens.push(RawToken::BlockStart { line_no }),
            Rule::block_end => tokens.push(RawToken::BlockEnd { line_no }),
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
            | Rule::exit_command => tokens.push(RawToken::Command { pair, line_no }),
            other => bail!("unexpected parser rule {:?}", other),
        }
    }
    Ok(tokens)
}
