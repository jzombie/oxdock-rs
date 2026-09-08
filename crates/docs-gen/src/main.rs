use anyhow::{Context, Result};
use docs_gen::{
    DocsGenConfig, TargetSpec,
    discovery::discover_targets,
    guard::validate_rel_path,
    providers::{
        CargoMetadataProvider, CommandRefProvider, DataProvider, parse_workspace_members,
        parse_workspace_version,
    },
    template_doc,
};
use oxdock_core::ExecIo;
use oxdock_fs::{GuardedPath, PathResolver};
#[allow(clippy::disallowed_types)]
use std::path::{Path, PathBuf};

fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().collect();
    let config_arg = config_arg(&args);
    #[allow(clippy::disallowed_types)]
    let root_arg: Option<PathBuf> = root_arg(&args);

    let repo_root = match root_arg {
        Some(root) => root,
        None => find_repo_root()?,
    };
    let config_path = match config_arg {
        Some(path) => path,
        None => default_config_path(&repo_root).context(
            "no docs-gen config found (looked for docs-gen.json, .oxdock/docs-gen.json); pass --config",
        )?,
    };
    let config = DocsGenConfig::load(&config_path)
        .with_context(|| format!("load docs-gen config {}", config_path.display()))?;

    let workspace_toml = repo_root.join("Cargo.toml");
    let members = parse_workspace_members(&workspace_toml).unwrap_or_default();

    // Plugin data first: providers materialize generated files (e.g. the
    // `command-ref` index/body sections) before any target renders, so
    // templates can glob them like any other fragment.
    if config.providers.iter().any(|p| p == "command-ref") {
        write_plugin_fragments(&repo_root, &CommandRefProvider, &config)?;
    }

    // Targets: explicit config entries plus algorithmically discovered
    // `target.json` directories (one system — no parallel legacy tree).
    let mut targets: Vec<TargetSpec> = config.targets.clone();
    for discovered in discover_targets(&repo_root, &config, &members)? {
        eprintln!(
            "Discovered target `{}` in {}",
            discovered.spec.name, discovered.dir
        );
        targets.push(discovered.spec);
    }
    targets.sort_by(|a, b| a.name.cmp(&b.name));

    let cargo_enabled = config.providers.iter().any(|p| p == "cargo-metadata");
    for target in &targets {
        render_target(&repo_root, &workspace_toml, target, &config, cargo_enabled)
            .with_context(|| format!("render target `{}`", target.name))?;
    }
    eprintln!("docs-gen rendered {} target(s)", targets.len());
    Ok(())
}

/// Map provider fragments onto configured output paths (config data —
/// fragment name → workspace-relative file). The only writer of generated
/// files; templates consume them via ordinary globs.
#[allow(clippy::disallowed_types)]
fn write_plugin_fragments(
    repo_root: &Path,
    provider: &CommandRefProvider,
    config: &DocsGenConfig,
) -> Result<()> {
    if config.generated_files.is_empty() {
        return Ok(());
    }
    let root = GuardedPath::new_root(repo_root)?;
    let resolver = PathResolver::new(root.as_path(), root.as_path())?;
    for fragment in provider.fragments() {
        let Some(rel) = config.generated_files.get(&fragment.name) else {
            continue;
        };
        let rel = validate_rel_path(repo_root, rel)?;
        let dest = root.join(&rel)?;
        if let Some(parent) = dest.as_path().parent() {
            let parent_guard = GuardedPath::new(repo_root, parent)?;
            resolver.create_dir_all(&parent_guard)?;
        }
        resolver.write_file(&dest, fragment.contents.as_bytes())?;
        eprintln!(
            "{} written via `{}` plugin",
            dest.as_path().display(),
            provider.name()
        );
    }
    Ok(())
}

#[allow(clippy::disallowed_types)]
fn render_target(
    repo_root: &Path,
    workspace_toml: &Path,
    target: &TargetSpec,
    config: &DocsGenConfig,
    cargo_enabled: bool,
) -> Result<()> {
    // Per-target provider values: the owning workspace member's cargo
    // metadata (name/description/version flow into `$docs_ctx`). Wiring
    // only — no hardcoded paths or keys.
    let mut env = ExecIo::new();
    let mut provider_values = serde_json::Value::Null;
    if cargo_enabled {
        if let Some(member) = target.member.as_deref() {
            let member_toml = repo_root.join(member).join("Cargo.toml");
            if member_toml.exists() {
                let provider = CargoMetadataProvider::load(&member_toml, workspace_toml)?;
                let meta = provider.metadata();
                env.insert_inherit_env("CRATE_NAME", &meta.name);
                env.insert_inherit_env("CRATE_DESCRIPTION", &meta.description);
                env.insert_inherit_env("CRATE_VERSION", &meta.version);
                provider_values = provider.values();
            }
        } else {
            let version = parse_workspace_version(workspace_toml);
            env.insert_inherit_env("CRATE_VERSION", &version);
            provider_values = serde_json::json!({ "version": version });
        }
    }
    template_doc::render_target(
        repo_root,
        target,
        config.global_values.as_deref(),
        Some(&provider_values),
        env,
    )?;
    eprintln!("Target `{}` rendered to {}", target.name, target.out);
    Ok(())
}

#[allow(clippy::disallowed_types)]
fn config_arg(args: &[String]) -> Option<PathBuf> {
    args.windows(2)
        .find(|w| w[0] == "--config")
        .map(|w| PathBuf::from(&w[1]))
}

#[allow(clippy::disallowed_types)]
fn root_arg(args: &[String]) -> Option<PathBuf> {
    args.windows(2)
        .find(|w| w[0] == "--root")
        .map(|w| PathBuf::from(&w[1]))
}

/// Default config discovery: `./docs-gen.json`, then
/// `./.oxdock/docs-gen.json`.
#[allow(clippy::disallowed_types)]
fn default_config_path(repo_root: &Path) -> Option<PathBuf> {
    for candidate in ["docs-gen.json", ".oxdock/docs-gen.json"] {
        let path = repo_root.join(candidate);
        if path.exists() {
            return Some(path);
        }
    }
    None
}

#[allow(clippy::disallowed_methods, clippy::disallowed_types)]
fn find_repo_root() -> Result<PathBuf> {
    // `--root` is handled by the caller; this only resolves the default.
    if let Ok(root) = std::env::var("OXDOCK_REPO_ROOT") {
        let root = PathBuf::from(root);
        if root.join("Cargo.toml").exists() {
            return Ok(root);
        }
    }
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("."));

    let mut current = manifest_dir.clone();
    // `canonicalize` is host introspection for startup path discovery (not
    // a guarded workspace read); fall back to the raw dir on failure.
    if let Ok(canonical) = current.canonicalize() {
        current = canonical;
    }
    loop {
        // A docs-gen root is any directory holding its config — no
        // Cargo.toml required, so non-Cargo projects work unchanged.
        if current.join("docs-gen.json").exists() || current.join(".oxdock/docs-gen.json").exists()
        {
            return Ok(current);
        }
        if !current.pop() {
            break;
        }
    }

    Ok(PathBuf::from("."))
}
