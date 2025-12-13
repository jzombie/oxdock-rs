use anyhow::{bail, Context, Result};
use std::env;
use std::fs;
use std::io::{self, IsTerminal, Read};
use std::path::{Path, PathBuf};
use std::process::Command;

pub use embed_recipe_dsl::{parse_script, run_steps, Step};

pub fn run() -> Result<()> {
    let mut args = std::env::args().skip(1);
    let first = args.next().unwrap_or_default();
    if first == "embed" {
        let opts = EmbedOptions::parse(&mut args)?;
        embed(opts)
    } else {
        let mut rest = std::iter::once(first).chain(args);
        let opts = Options::parse(&mut rest)?;
        execute(opts)
    }
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

#[derive(Debug, Clone)]
pub struct EmbedOptions {
    pub folder: PathBuf,
    pub out: PathBuf,
    pub module: String,
}

impl EmbedOptions {
    pub fn parse(args: &mut impl Iterator<Item = String>) -> Result<Self> {
        let mut folder: Option<PathBuf> = None;
        let mut out: Option<PathBuf> = None;
        let mut module: Option<String> = None;

        while let Some(arg) = args.next() {
            match arg.as_str() {
                "--folder" => {
                    let p = args
                        .next()
                        .ok_or_else(|| anyhow::anyhow!("--folder requires a path"))?;
                    folder = Some(PathBuf::from(p));
                }
                "--out" => {
                    let p = args
                        .next()
                        .ok_or_else(|| anyhow::anyhow!("--out requires a path"))?;
                    out = Some(PathBuf::from(p));
                }
                "--module" => {
                    let m = args
                        .next()
                        .ok_or_else(|| anyhow::anyhow!("--module requires a name"))?;
                    module = Some(m);
                }
                other => bail!("unexpected flag: {}", other),
            }
        }

        let folder = folder.ok_or_else(|| anyhow::anyhow!("--folder is required"))?;
        let out = out.ok_or_else(|| anyhow::anyhow!("--out is required"))?;
        let module = module.unwrap_or_else(|| "EmbeddedAssets".to_string());
        Ok(Self {
            folder,
            out,
            module,
        })
    }
}

pub fn execute(opts: Options) -> Result<()> {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let workspace_root = manifest_dir
        .parent()
        .context("embed-recipe manifest dir has no parent")?
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
                bail!("no stdin detected; pass --script <file> or pipe a script into stdin (use --script - if explicit)");
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

/// Generate a rust-embed ready module that points at the provided folder.
pub fn embed(opts: EmbedOptions) -> Result<()> {
    if !opts.folder.exists() {
        bail!("folder does not exist: {}", opts.folder.display());
    }
    if !opts.folder.is_dir() {
        bail!("folder is not a directory: {}", opts.folder.display());
    }

    if let Some(parent) = opts.out.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create parent dir {}", parent.display()))?;
    }

    let folder_str = opts
        .folder
        .to_str()
        .ok_or_else(|| anyhow::anyhow!("folder path is not valid UTF-8"))?;

    let contents = format!(
        "// Auto-generated by embed-recipe embed\nuse rust_embed::RustEmbed;\n\n#[derive(RustEmbed)]\n#[folder = \"{folder}\"]\npub struct {module};\n",
        folder = folder_str,
        module = opts.module,
    );

    fs::write(&opts.out, contents)
        .with_context(|| format!("failed to write embed module to {}", opts.out.display()))?;

    println!("wrote embed module to {}", opts.out.display());
    println!("module `{}` targets folder `{}`", opts.module, folder_str);
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
    Ok(())
}
