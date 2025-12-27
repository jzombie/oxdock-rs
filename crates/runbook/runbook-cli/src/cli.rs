use anyhow::{Context, Result};

use crate::{run, run_block};
use crate::tui;

#[derive(Debug, Clone)]
pub enum LaunchMode {
    Default,
    Tui,
    RunBlock { path: String, line: usize },
}

impl LaunchMode {
    fn from_args<I, S>(args: I) -> Result<(Self, Vec<String>)>
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        let mut mode = LaunchMode::Default;
        let mut cli_args = Vec::new();
        let mut iter = args.into_iter().map(|s| s.into()).peekable();
        while let Some(arg) = iter.next() {
            match arg.as_str() {
                "--tui" | "-t" => mode = LaunchMode::Tui,
                "tui" if matches!(mode, LaunchMode::Default) && cli_args.is_empty() => {
                    mode = LaunchMode::Tui;
                }
                "--run-block" | "run-block" => {
                    let path = iter
                        .next()
                        .context("expected path after --run-block")?;
                    let line = iter
                        .next()
                        .context("expected line number after --run-block")?
                        .parse::<usize>()
                        .context("invalid line number for --run-block")?;
                    mode = LaunchMode::RunBlock { path, line };
                }
                _ => cli_args.push(arg),
            }
        }
        Ok((mode, cli_args))
    }
}

pub fn run_from_env() -> Result<()> {
    let args = std::env::args().skip(1);
    run_with_args(args)
}

pub fn run_with_args<I, S>(args: I) -> Result<()>
where
    I: IntoIterator<Item = S>,
    S: Into<String>,
{
    let (mode, cli_args) = LaunchMode::from_args(args)?;
    match mode {
        LaunchMode::Tui => tui::run_tui(cli_args),
        LaunchMode::RunBlock { path, line } => run_block(&path, line),
        LaunchMode::Default => run(),
    }
}
