use anyhow::{Context, Result};

use crate::tui;
use crate::{run, run_block};

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
                    let path = iter.next().context("expected path after --run-block")?;
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

#[cfg(test)]
mod tests {
    use super::LaunchMode;

    #[test]
    fn parse_default_args() {
        let (mode, args) = LaunchMode::from_args(std::iter::empty::<String>())
            .expect("parse default");
        assert!(matches!(mode, LaunchMode::Default));
        assert!(args.is_empty());
    }

    #[test]
    fn parse_tui_flag() {
        let (mode, args) = LaunchMode::from_args(["--tui", "notes.md"])
            .expect("parse tui");
        assert!(matches!(mode, LaunchMode::Tui));
        assert_eq!(args, vec!["notes.md"]);
    }

    #[test]
    fn parse_tui_subcommand() {
        let (mode, args) = LaunchMode::from_args(["tui", "notes.md"])
            .expect("parse tui");
        assert!(matches!(mode, LaunchMode::Tui));
        assert_eq!(args, vec!["notes.md"]);
    }

    #[test]
    fn parse_tui_as_positional_after_args() {
        let (mode, args) = LaunchMode::from_args(["notes.md", "tui"])
            .expect("parse args");
        assert!(matches!(mode, LaunchMode::Default));
        assert_eq!(args, vec!["notes.md", "tui"]);
    }

    #[test]
    fn parse_run_block() {
        let (mode, args) = LaunchMode::from_args(["--run-block", "notes.md", "42"])
            .expect("parse run-block");
        assert!(args.is_empty());
        match mode {
            LaunchMode::RunBlock { path, line } => {
                assert_eq!(path, "notes.md");
                assert_eq!(line, 42);
            }
            _ => panic!("expected RunBlock"),
        }
    }

    #[test]
    fn parse_run_block_rejects_invalid_line() {
        let err = LaunchMode::from_args(["--run-block", "notes.md", "nope"])
            .expect_err("invalid line");
        let message = err.to_string();
        assert!(
            message.contains("invalid line number"),
            "unexpected error: {message}"
        );
    }
}
