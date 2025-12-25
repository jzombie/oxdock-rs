use anyhow::{Context, Result, anyhow};
use cargo_metadata::{MetadataCommand, PackageId, TargetKind};
use std::collections::HashSet;
use std::env;
use std::fs;
#[allow(clippy::disallowed_types)]
use std::path::{Path, PathBuf};

fn main() -> Result<()> {
    let args = Args::parse(env::args().skip(1).collect())?;
    generate(&args.workspace, &args.out)?;
    Ok(())
}

#[allow(clippy::disallowed_types)]
struct Args {
    workspace: PathBuf,
    out: PathBuf,
}

#[allow(clippy::disallowed_types)]
impl Args {
    fn parse(raw: Vec<String>) -> Result<Self> {
        let mut workspace = None;
        let mut out = None;
        let mut iter = raw.into_iter();
        while let Some(arg) = iter.next() {
            match arg.as_str() {
                "--workspace" => workspace = iter.next().map(PathBuf::from),
                "--out" => out = iter.next().map(PathBuf::from),
                _ => return Err(anyhow!("unknown arg {arg}")),
            }
        }
        let workspace = workspace.ok_or_else(|| anyhow!("--workspace required"))?;
        let out = out.ok_or_else(|| anyhow!("--out required"))?;
        Ok(Self { workspace, out })
    }
}

#[allow(clippy::disallowed_types)]
fn generate(workspace_root: &Path, out_path: &Path) -> Result<PathBuf> {
    let manifest = workspace_root.join("Cargo.toml");
    let meta = MetadataCommand::new()
        .manifest_path(&manifest)
        .no_deps()
        .exec()
        .context("run cargo metadata")?;

    let workspace_ids: HashSet<PackageId> = meta.workspace_members.into_iter().collect();
    let mut deps: Vec<(String, PathBuf)> = meta
        .packages
        .into_iter()
        .filter(|pkg| workspace_ids.contains(&pkg.id))
        .filter(|pkg| {
            pkg.targets
                .iter()
                .any(|t| t.kind.iter().any(|k| matches!(k, TargetKind::Lib)))
        })
        .filter_map(|pkg| {
            let name = pkg.name.trim();
            if name.is_empty() {
                return None;
            }
            let path = pkg
                .manifest_path
                .parent()? // manifest_path is a file path; we want the directory
                .to_path_buf()
                .into_std_path_buf();
            Some((name.to_string(), path))
        })
        .collect();

    deps.sort_by(|a, b| a.0.cmp(&b.0));

    let mut cargo_toml = String::new();
    cargo_toml.push_str("[package]\n");
    cargo_toml.push_str("name = \"oxbook-snippet\"\n");
    cargo_toml.push_str("version = \"0.0.0\"\n");
    cargo_toml.push_str("edition = \"2021\"\n\n");
    cargo_toml.push_str("[workspace]\n\n");
    cargo_toml.push_str("[dependencies]\n");
    for (name, path) in deps {
        let path_str = path_to_toml(&path);
        cargo_toml.push_str(&format!("{name} = {{ path = \"{path_str}\" }}\n"));
    }
    cargo_toml.push_str("\n[[bin]]\nname = \"oxbook-snippet\"\npath = \"src/main.rs\"\n");

    #[allow(clippy::disallowed_methods)]
    {
        if let Some(parent) = out_path.parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("create dir {}", parent.display()))?;
        }
        fs::write(out_path, cargo_toml).with_context(|| format!("write {}", out_path.display()))?;
    }
    Ok(out_path.to_path_buf())
}

#[allow(clippy::disallowed_types)]
fn path_to_toml(path: &Path) -> String {
    // Note: This is an expection to the rule for adhoc string replacements
    // because this crate is currently isolated from the rest of the workspace.
    path.to_string_lossy().replace('\\', "/")
}
