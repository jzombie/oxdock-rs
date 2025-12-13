use anyhow::{Context, Result, bail};
use std::fs;
use std::io::{self, IsTerminal, Read};
use std::path::{Path, PathBuf};
use std::process::Command;

pub use doc_ox_dsl::{Guard, Step, StepKind, parse_script, run_steps};

pub fn run() -> Result<()> {
    let mut args = std::env::args().skip(1);
    let opts = Options::parse(&mut args)?;
    execute(opts)
}

#[derive(Debug, Clone)]
pub enum ScriptSource {
    Path(PathBuf),
    Stdin,
}

#[derive(Debug, Clone)]
pub struct Options {
    pub script: ScriptSource,
}

impl Options {
    pub fn parse(args: &mut impl Iterator<Item = String>) -> Result<Self> {
        let mut script: Option<ScriptSource> = None;
        while let Some(arg) = args.next() {
            if arg.is_empty() {
                continue;
            }
            match arg.as_str() {
                "--script" => {
                    let p = args
                        .next()
                        .ok_or_else(|| anyhow::anyhow!("--script requires a path"))?;
                    if p == "-" {
                        script = Some(ScriptSource::Stdin);
                    } else {
                        script = Some(ScriptSource::Path(PathBuf::from(p)));
                    }
                }
                other => bail!("unexpected flag: {}", other),
            }
        }

        let script = script.unwrap_or(ScriptSource::Stdin);

        Ok(Self { script })
    }
}

pub fn execute(opts: Options) -> Result<()> {
    let manifest_dir = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR")?.as_str());
    let workspace_root = manifest_dir
        .parent()
        .context(format!(
            "{} manifest dir has no parent",
            env!("CARGO_PKG_NAME")
        ))?
        .to_path_buf();

    let temp = tempfile::tempdir().context("failed to create temp dir")?;
    let temp_path = temp.path().to_path_buf();

    // Materialize source tree without .git
    archive_head(&workspace_root, &temp_path)?;

    // Interpret a tiny Dockerfile-ish script
    let script = match &opts.script {
        ScriptSource::Path(path) => fs::read_to_string(path)
            .with_context(|| format!("failed to read script at {}", path.display()))?,
        ScriptSource::Stdin => {
            let stdin = io::stdin();
            if stdin.is_terminal() {
                bail!(
                    "no stdin detected; pass --script <file> or pipe a script into stdin (use --script - if explicit)"
                );
            }
            let mut buf = String::new();
            stdin
                .lock()
                .read_to_string(&mut buf)
                .context("failed to read script from stdin")?;
            buf
        }
    };
    let steps = parse_script(&script)?;
    run_script(&temp_path, &steps)?;

    Ok(())
}

pub fn run_script(workspace_root: &Path, steps: &[Step]) -> Result<()> {
    run_steps(workspace_root, steps)
}
fn run_cmd(cmd: &mut Command) -> Result<()> {
    let status = cmd
        .status()
        .with_context(|| format!("failed to run {:?}", cmd))?;
    if !status.success() {
        bail!("command {:?} failed with status {}", cmd, status);
    }
    Ok(())
}

fn archive_head(workspace_root: &Path, temp_root: &Path) -> Result<()> {
    let archive_path = temp_root.join("src.tar");
    let archive_str = archive_path.to_string_lossy().to_string();
    run_cmd(Command::new("git").current_dir(workspace_root).args([
        "archive",
        "--format=tar",
        "--output",
        &archive_str,
        "HEAD",
    ]))?;

    run_cmd(
        Command::new("tar")
            .arg("-xf")
            .arg(&archive_str)
            .arg("-C")
            .arg(temp_root),
    )?;

    // Drop the intermediate archive to keep the temp workspace clean.
    fs::remove_file(&archive_path)
        .with_context(|| format!("failed to remove {}", archive_path.display()))?;
    Ok(())
}
