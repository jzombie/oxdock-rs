use anyhow::{Context, Result};
use oxdock_core::ExecIo;
use oxdock_fs::{GuardedPath, PathResolver};
#[allow(clippy::disallowed_types)]
use std::path::Path;

use crate::config::TargetSpec;
use crate::guard::validate_rel_path;
use crate::runner;

/// Pure-DSL stage executor.
///
/// Every dynamic value enters as a runtime `$var` (via `env`, visible in
/// DSL as `$name`); the script text itself is static. No `#var` proc-macro
/// interpolation for runtime data, no manual `StepKind` construction — the
/// script is parsed with the production dispatcher (`runner::run_script`).
///
/// Stage kinds:
/// - `template`: `EXPAND` the file (strict; `{{ $docs_ctx.key }}` and
///   `{{ $docs_global.key }}` keypaths resolve against the loaded values
///   maps, `{{ env:KEY }}` against the environment), append the bytes.
/// - `read`: `READ` the file byte-verbatim, append the bytes.
/// - `glob`: `GLOB` the pattern (sorted, sandbox-relative), then per file
///   either `EXPAND` (when `expand` is true) or `READ` verbatim (so
///   documented `{{ env:EXAMPLE }}` snippets survive), append the bytes.
/// - anything else: append the inline `text` string.
///
/// Formatting comes solely from file bytes. There is deliberately NO
/// `APPEND ... "\n"`: templates own their newlines.
const DRIVER: &str = r#"WRITE $docs_out ""
LET $docs_ctx = LOAD_JSON($docs_ctx_path)
LET $docs_global = LOAD_JSON($docs_global_path)
LET $docs_stages = LOAD_JSON($docs_stages_path)
FOR $docs_idx, $docs_node IN $docs_stages {
    ECHO "[{{ $docs_idx }}] {{ $docs_node.kind }}"
    IF $docs_node.kind == "template" {
        WITH_IO [stdout=pipe:docs_tmpl] EXPAND $docs_node.path
        WITH_IO [stdin=pipe:docs_tmpl] APPEND $docs_out
    } ELSE IF $docs_node.kind == "read" {
        WITH_IO [stdout=pipe:docs_sec] READ $docs_node.path
        WITH_IO [stdin=pipe:docs_sec] APPEND $docs_out
    } ELSE IF $docs_node.kind == "glob" {
        LET $docs_files = GLOB($docs_node.pattern)
        FOR $docs_f IN $docs_files {
            IF $docs_node.expand {
                WITH_IO [stdout=pipe:docs_frag] EXPAND $docs_f
                WITH_IO [stdin=pipe:docs_frag] APPEND $docs_out
            } ELSE {
                WITH_IO [stdout=pipe:docs_frag] READ $docs_f
                WITH_IO [stdin=pipe:docs_frag] APPEND $docs_out
            }
        }
    } ELSE {
        APPEND $docs_out $docs_node.text
    }
}
"#;

/// Staging helper: write ephemeral JSON under `.oxdock-staging` via the
/// guarded filesystem abstraction. Returns the root-relative path.
#[allow(clippy::disallowed_types)]
fn stage_text(
    resolver: &PathResolver,
    root: &GuardedPath,
    filename: &str,
    contents: &str,
) -> Result<String> {
    let staging = root.join(".oxdock-staging")?;
    resolver.create_dir_all(&staging)?;
    let file = staging.join(filename)?;
    resolver.write_file(&file, contents.as_bytes())?;
    let rel = file
        .as_path()
        .strip_prefix(root.as_path())
        .map(|p| p.to_string_lossy().replace('\\', "/"))
        .unwrap_or_else(|_| format!(".oxdock-staging/{filename}"));
    Ok(rel)
}

/// Execute pre-validated, root-relative paths with the pure-DSL driver.
#[allow(clippy::disallowed_types)]
fn execute(
    repo_root: &Path,
    out_rel: &str,
    ctx_rel: &str,
    global_rel: &str,
    stages_rel: &str,
    env: ExecIo,
) -> Result<()> {
    let mut env = env;
    env.insert_inherit_env("docs_out", out_rel);
    env.insert_inherit_env("docs_ctx_path", ctx_rel);
    env.insert_inherit_env("docs_global_path", global_rel);
    env.insert_inherit_env("docs_stages_path", stages_rel);
    runner::run_script(repo_root, DRIVER, env)
}

/// Manifest entry point: render an explicit JSON stage array through the
/// pure-DSL driver (used by tests and external callers).
///
/// `manifest_json` is an array of `{kind, path?, pattern?, expand?, text?}`
/// nodes. Kept generic: no hardcoded paths, keys, or newlines.
#[allow(clippy::disallowed_types)]
pub fn compile(
    repo_root: &Path,
    manifest_json: &str,
    output_path: &Path,
    env: ExecIo,
) -> Result<()> {
    let out_candidate = output_path.to_string_lossy().replace('\\', "/");
    // Accept both absolute (repo-root-joined) and root-relative outputs.
    let out_rel = canonicalize_out(repo_root, output_path, &out_candidate)?;
    // Normalize raw manifests: glob stages default to verbatim (`expand:
    // false`) so the driver can branch on `$docs_node.expand` without
    // key-not-found errors. Pure data normalization, no DSL involved.
    let manifest_json = normalize_manifest(manifest_json)?;
    let root = GuardedPath::new_root(repo_root)?;
    let resolver = PathResolver::new(root.as_path(), root.as_path())?;
    let stages_rel = stage_text(&resolver, &root, "docs_manifest.json", &manifest_json)?;
    let ctx_rel = stage_text(&resolver, &root, "docs_ctx.json", "{}")?;
    let global_rel = stage_text(&resolver, &root, "docs_global.json", "{}")?;
    let staged = [stages_rel.clone(), ctx_rel.clone(), global_rel.clone()];
    let res = execute(repo_root, &out_rel, &ctx_rel, &global_rel, &stages_rel, env);
    cleanup_staged(&resolver, &root, &staged);
    res
}

/// Best-effort removal of ephemeral staging files (kept out of the
/// render path so failures still clean up).
fn cleanup_staged(resolver: &PathResolver, root: &GuardedPath, rels: &[String]) {
    for rel in rels {
        if let Ok(path) = root.join(rel) {
            let _ = resolver.remove_file(&path);
        }
    }
}

/// Render one config-driven target.
///
/// Stages (from `target.stages`, in order) execute through the same pure-DSL
/// driver as `compile`. Context layering:
///
/// - `$docs_ctx`: the *effective* context, staged as a merged JSON file
///   (target `values` > `provider_values` > global file; shallow
///   top-level overlay, unconditional so `IF $docs_ctx.k == ""` fallbacks
///   keep working). Merging is plain file IO — no `StepKind` construction;
///   a future OxDock merge operator will move it into the DSL (plan §5).
/// - `$docs_global`: the raw global file, for templates that explicitly
///   reference shared defaults or implement `IF`/lookup-order fallbacks.
///
/// All paths are `GuardedPath`-validated before execution; output format is
/// irrelevant (any text the stages contain).
#[allow(clippy::disallowed_types)]
pub fn render_target(
    repo_root: &Path,
    target: &TargetSpec,
    global_values: Option<&str>,
    provider_values: Option<&serde_json::Value>,
    env: ExecIo,
) -> Result<()> {
    target.validate()?;
    let out_rel = validate_rel_path(repo_root, &target.out)?;
    let root = GuardedPath::new_root(repo_root)?;
    let resolver = PathResolver::new(root.as_path(), root.as_path())?;

    // Validate every referenced input up front (sandbox containment).
    for stage in &target.stages {
        stage.validate()?;
        match stage.kind.as_str() {
            "template" | "read" => {
                let path = stage.path.as_deref().unwrap_or("");
                validate_rel_path(repo_root, path)?;
            }
            "glob" => {
                reject_traversal_pattern(&stage.pattern.clone().unwrap_or_default())?;
            }
            _ => {}
        }
    }

    // Materialize the stage manifest + values context for the DSL.
    let stages_json = serde_json::to_string(&target.stages)?;
    let stages_rel = stage_text(&resolver, &root, "docs_stages.json", &stages_json)?;

    let global_map = match global_values {
        Some(path) => {
            let rel = validate_rel_path(repo_root, path)?;
            let abs = root.join(&rel)?;
            let text = resolver
                .read_to_string(&abs)
                .with_context(|| format!("read global values {rel}"))?;
            let parsed: serde_json::Value =
                serde_json::from_str(&text).with_context(|| format!("parse {rel}"))?;
            if !parsed.is_object() && !parsed.is_null() {
                anyhow::bail!("global values {rel} must be a JSON object");
            }
            parsed
        }
        None => serde_json::Value::Null,
    };

    let local_map = match &target.values {
        Some(path) => {
            let rel = validate_rel_path(repo_root, path)?;
            let abs = root.join(&rel)?;
            let text = resolver
                .read_to_string(&abs)
                .with_context(|| format!("read target values {rel}"))?;
            let parsed: serde_json::Value =
                serde_json::from_str(&text).with_context(|| format!("parse {rel}"))?;
            if !parsed.is_object() && !parsed.is_null() {
                anyhow::bail!("target values {rel} must be a JSON object");
            }
            parsed
        }
        None => serde_json::Value::Null,
    };

    // Effective context: global < provider < target (target wins).
    let merged = merge_values(
        &global_map,
        provider_values.unwrap_or(&serde_json::Value::Null),
        &local_map,
    );
    let ctx_rel = stage_text(
        &resolver,
        &root,
        "docs_ctx.json",
        &serde_json::to_string(&merged)?,
    )?;

    // Raw global file for explicit shared-default references. When no
    // global file is configured, stage an empty object so the DSL always
    // has a loadable path (staged files are cleaned up; real inputs are
    // never deleted).
    let mut staged = vec![stages_rel.clone(), ctx_rel.clone()];
    let global_rel = match global_values {
        Some(path) => validate_rel_path(repo_root, path)?,
        None => {
            let rel = stage_text(&resolver, &root, "docs_global_empty.json", "{}")?;
            staged.push(rel.clone());
            rel
        }
    };

    let res = execute(repo_root, &out_rel, &ctx_rel, &global_rel, &stages_rel, env);
    cleanup_staged(&resolver, &root, &staged);
    res
}

/// Shallow top-level overlay: `global` < `provider` < `local`.
///
/// Values files are flat key→scalar maps; nested objects are replaced
/// wholesale (not deep-merged). `Null` layers contribute nothing.
pub(crate) fn merge_values(
    global: &serde_json::Value,
    provider: &serde_json::Value,
    local: &serde_json::Value,
) -> serde_json::Value {
    let mut out = serde_json::Map::new();
    for layer in [global, provider, local] {
        if let Some(map) = layer.as_object() {
            for (key, value) in map {
                out.insert(key.clone(), value.clone());
            }
        }
    }
    serde_json::Value::Object(out)
}

/// Normalize a raw manifest JSON array: every `glob` node gains an explicit
/// boolean `expand` (default `false` = verbatim `READ`).
fn normalize_manifest(manifest_json: &str) -> Result<String> {
    let mut parsed: serde_json::Value =
        serde_json::from_str(manifest_json).context("parse docs-gen manifest JSON")?;
    let nodes = parsed
        .as_array_mut()
        .context("docs-gen manifest must be a JSON array")?;
    for node in nodes.iter_mut() {
        let is_glob = node
            .get("kind")
            .and_then(|k| k.as_str())
            .is_some_and(|k| k == "glob");
        if is_glob
            && node.get("expand").is_none()
            && let Some(map) = node.as_object_mut()
        {
            map.insert("expand".to_string(), serde_json::Value::Bool(false));
        }
    }
    serde_json::to_string(&parsed).context("re-serialize docs-gen manifest")
}
/// Resolve an output path that may be absolute (repo-joined, the legacy
/// `main.rs` style) or root-relative (the config style) into a validated
/// root-relative string.
#[allow(clippy::disallowed_types)]
fn canonicalize_out(repo_root: &Path, original: &Path, candidate: &str) -> Result<String> {
    // Legacy callers pass `repo_root.join(...)` absolutes: relativize them.
    if let Ok(rel) = original.strip_prefix(repo_root) {
        let rel_str = rel.to_string_lossy().replace('\\', "/");
        if !rel_str.is_empty() {
            return validate_rel_path(repo_root, &rel_str);
        }
    }
    validate_rel_path(repo_root, candidate)
}

/// Glob patterns are sandbox-root-relative by engine contract (`..`
/// yields empty, never traversal), but reject `..` at the config boundary
/// anyway so typos fail loudly instead of silently producing no output.
fn reject_traversal_pattern(pattern: &str) -> Result<()> {
    if pattern.replace('\\', "/").split('/').any(|seg| seg == "..") {
        anyhow::bail!("fragment pattern must not contain `..`: {pattern}");
    }
    Ok(())
}
