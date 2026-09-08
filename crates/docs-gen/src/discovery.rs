use anyhow::{Context, Result};
use oxdock_fs::{GuardedPath, PathResolver};
#[allow(clippy::disallowed_types)]
use std::path::Path;

use crate::config::{StageSpec, TargetSpec};
use crate::guard::validate_rel_path;

/// A `target.json` found by scanning, with its template directory
/// (workspace-root-relative, forward-slashed). `spec.member` is filled in
/// from the workspace member that contains the directory, when any.
#[derive(Clone, Debug)]
pub struct DiscoveredTarget {
    pub dir: String,
    pub spec: crate::config::TargetSpec,
}

/// Discover algorithmic targets: every directory holding a `target.json`
/// under a workspace member's `.oxdock/template` tree or under one of the
/// configured extra roots. No paths are hardcoded — members come from
/// `workspace.members`, roots from config; a directory without
/// `target.json` (e.g. `_global/`, pilot-only dirs) is simply not a target.
///
/// A `target.json` may be sparse — just `name`/`out` (plus optional
/// `values`/`member`). Empty `stages` are synthesized from the target
/// directory's own layout (see `conventional_stages`), so the common case
/// is a two-line file with nothing to get wrong. Targets needing bespoke
/// composition (root README, pilots) declare full `stages` and discovery
/// leaves them untouched.
#[allow(clippy::disallowed_types)]
pub fn discover_targets(
    repo_root: &Path,
    config: &crate::config::DocsGenConfig,
    members: &[String],
) -> Result<Vec<DiscoveredTarget>> {
    let root = GuardedPath::new_root(repo_root)?;
    let resolver = PathResolver::new(root.as_path(), root.as_path())?;

    // Scan set: one `.oxdock/template` tree per member plus the configured
    // extra roots (which cover the repo-root tree). Deduplicated.
    let mut scan_rels: Vec<String> = Vec::new();
    for member in members {
        let member = member.trim_matches('/').to_string();
        if member.is_empty() {
            continue;
        }
        let rel = format!("{member}/.oxdock/template");
        if !scan_rels.contains(&rel) {
            scan_rels.push(rel);
        }
    }
    for extra in &config.template_roots {
        let rel = validate_rel_path(repo_root, extra)?;
        if !scan_rels.contains(&rel) {
            scan_rels.push(rel);
        }
    }

    let mut out = Vec::new();
    for rel in &scan_rels {
        let Ok(dir) = root.join(rel) else {
            continue;
        };
        if !dir.exists() {
            continue;
        }
        find_target_files(&resolver, &root, &dir, 0, &mut out)?;
    }

    // Attribute each target to its containing member for provider values.
    let norm_members: Vec<String> = {
        let mut seen = Vec::new();
        for member in members {
            let member = member.trim_matches('/').to_string();
            if !member.is_empty() && !seen.contains(&member) {
                seen.push(member);
            }
        }
        seen
    };
    for target in &mut out {
        target.spec.member = member_for_dir(&norm_members, &target.dir);
        if target.spec.stages.is_empty() {
            target.spec.stages = conventional_stages(&root, &target.dir);
        }
        target
            .spec
            .validate()
            .with_context(|| format!("invalid target {}", target.spec.name))?;
    }
    out.sort_by(|a, b| a.spec.name.cmp(&b.spec.name));
    Ok(out)
}

/// Compose stages from the target directory's own layout — no
/// document-specific vocabulary (the engine only knows files in a
/// directory):
///
/// - `<dir>/header.tmpl` present → prepend an EXPAND stage for it;
/// - `<dir>/fragments/` present → a verbatim `*.md` glob plus an expanded
///   `*.tmpl` glob (empty globs are no-ops; `.tmpl`-means-expand is the
///   engine's format-neutral convention — `module.rs.tmpl` expands,
///   `snippet.rs` stays verbatim);
/// - `<dir>/footer.tmpl` present → append an EXPAND stage for it.
///
/// Verbatim collection defaults to `*.md`; targets generating other
/// formats declare explicit `stages` (as the pilots do). Each crate owns
/// its wrapper copies (fed by global *values*, so shared strings stay
/// single-sourced); explicit `stages` in `target.json` bypass this
/// entirely.
fn conventional_stages(root: &GuardedPath, dir: &str) -> Vec<StageSpec> {
    let exists = |rel: &str| {
        root.join(rel)
            .map(|guarded| guarded.exists())
            .unwrap_or(false)
    };
    let mut stages = Vec::new();
    if exists(&format!("{dir}/header.tmpl")) {
        stages.push(StageSpec {
            kind: "template".to_string(),
            path: Some(format!("{dir}/header.tmpl")),
            pattern: None,
            text: None,
            expand: false,
        });
    }
    if exists(&format!("{dir}/fragments")) {
        stages.push(StageSpec {
            kind: "glob".to_string(),
            path: None,
            pattern: Some(format!("{dir}/fragments/*.md")),
            text: None,
            expand: false,
        });
        stages.push(StageSpec {
            kind: "glob".to_string(),
            path: None,
            pattern: Some(format!("{dir}/fragments/*.tmpl")),
            text: None,
            expand: true,
        });
    }
    if exists(&format!("{dir}/footer.tmpl")) {
        stages.push(StageSpec {
            kind: "template".to_string(),
            path: Some(format!("{dir}/footer.tmpl")),
            pattern: None,
            text: None,
            expand: false,
        });
    }
    stages
}

/// Longest workspace-member prefix containing `dir` (`dir == member` or
/// `dir` under `member/`), or `None` for repo-root-level targets.
pub fn member_for_dir(members: &[String], dir: &str) -> Option<String> {
    let mut best: Option<&String> = None;
    for member in members {
        if (dir == member || dir.starts_with(&format!("{member}/")))
            && best.is_none_or(|b: &String| member.len() > b.len())
        {
            best = Some(member);
        }
    }
    best.cloned()
}

fn dir_rel(root: &GuardedPath, path: &GuardedPath) -> Result<String> {
    Ok(path
        .as_path()
        .strip_prefix(root.as_path())
        .map(|p| p.to_string_lossy().replace('\\', "/"))
        .unwrap_or_default())
}

/// Recursively collect `target.json` files. Skips hidden directories
/// (`.git`, build residue) and `target/` build trees; depth-capped.
#[allow(clippy::disallowed_types)]
fn find_target_files(
    resolver: &PathResolver,
    root: &GuardedPath,
    dir: &GuardedPath,
    depth: usize,
    out: &mut Vec<DiscoveredTarget>,
) -> Result<()> {
    if depth > 6 {
        return Ok(());
    }
    let entries = resolver
        .read_dir_entries(dir)
        .with_context(|| format!("read template dir {}", dir.as_path().display()))?;
    let mut subdirs = Vec::new();
    for entry in entries {
        let name = entry.file_name().to_string_lossy().to_string();
        if name == "target.json" {
            let file = dir.join(&name)?;
            let text = resolver.read_to_string(&file)?;
            let spec: TargetSpec = serde_json::from_str(&text)
                .with_context(|| format!("parse {}", file.as_path().display()))?;
            // Validated by the caller after sparse stages are synthesized.
            out.push(DiscoveredTarget {
                dir: dir_rel(root, dir)?,
                spec,
            });
        } else if entry.file_type().is_ok_and(|t| t.is_dir())
            && !name.starts_with('.')
            && name != "target"
        {
            subdirs.push(name);
        }
    }
    subdirs.sort();
    for name in subdirs {
        let child = dir.join(&name)?;
        find_target_files(resolver, root, &child, depth + 1, out)?;
    }
    Ok(())
}
