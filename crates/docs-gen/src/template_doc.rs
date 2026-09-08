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

/// A master template (e.g. `output.md.tmpl`) declares document order the way
/// the requester asked: by the location of `{{> }}` directives in a
/// document, not by a sidecar JSON list.
///
/// A whole-line directive `{{> some/file.md }}` includes that file at its
/// position (verbatim, unless it ends in `.tmpl`, which `EXPAND`s with the
/// values context in scope). Every other line is literal document content
/// and expands with the values context in scope. No JSON to hand-manage;
/// no engine changes: docs-gen scans the directives out in Rust and feeds
/// its existing pure-DSL driver. A future OxDock `{{> }}` include operator
/// would move this scan into the engine (plan §5).
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum TemplateNode {
    Literal(String),
    Include(String),
}

/// Split master template text into literal runs and include directives.
/// Pure string processing; every error names its line number.
pub(crate) fn parse_template_includes(text: &str) -> Result<Vec<TemplateNode>> {
    let mut nodes = Vec::new();
    let mut literal = String::new();
    for (idx, line) in text.split_inclusive('\n').enumerate() {
        let lineno = idx + 1;
        // Match whole-line directives; anything else (including mid-line
        // `{{> }}`) stays literal document content.
        let stripped = line.trim();
        let is_empty = stripped.is_empty();
        if let Some(path) = parse_include_directive(stripped, lineno)? {
            if !literal.is_empty() {
                nodes.push(TemplateNode::Literal(std::mem::take(&mut literal)));
            }
            nodes.push(TemplateNode::Include(path));
        } else if !is_empty || !literal.is_empty() {
            // Preserve blank lines inside the document, but never start
            // the node list with one (leading blanks belong to no one).
            literal.push_str(line);
        }
    }
    if !literal.is_empty() {
        nodes.push(TemplateNode::Literal(literal));
    }
    Ok(nodes)
}

/// Parse one trimmed line as a whole-line `{{> path }}` directive.
/// Returns `Ok(None)` for non-directive lines.
fn parse_include_directive(stripped: &str, lineno: usize) -> Result<Option<String>> {
    if !(stripped.starts_with("{{>") && stripped.ends_with("}}")) {
        return Ok(None);
    }
    let inner = stripped["{{>".len()..stripped.len() - "}}".len()].trim();
    if inner.is_empty() {
        anyhow::bail!("target template line {lineno}: empty `{{> }}` include");
    }
    if inner.split_whitespace().count() != 1 {
        anyhow::bail!("target template line {lineno}: include path must be a single token");
    }
    if inner.replace('\\', "/").split('/').any(|seg| seg == "..") {
        anyhow::bail!("target template line {lineno}: include path must not contain `..`");
    }
    Ok(Some(inner.to_string()))
}
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

    // Three target forms, one driver: explicit `stages` win, else a
    // `template` master document derives them from `{{> }}` positions,
    // else discovery already synthesized the local-convention stages.
    // Literal master runs are staged as expandable templates so the
    // values context applies to document prose too.
    let mut staged_lits: Vec<String> = Vec::new();
    let stages: Vec<crate::config::StageSpec> = match (&target.template, target.stages.is_empty()) {
        (Some(_), false) => {
            anyhow::bail!(
                "target `{}` declares both `template` and `stages`; use one",
                target.name
            );
        }
        (Some(tmpl), true) => {
            let rel = validate_rel_path(repo_root, tmpl)?;
            let abs = root.join(&rel)?;
            let text = resolver
                .read_to_string(&abs)
                .with_context(|| format!("read target template {rel}"))?;
            let nodes = parse_template_includes(&text)?;
            let mut built = Vec::new();
            for (idx, node) in nodes.iter().enumerate() {
                match node {
                    TemplateNode::Literal(body) => {
                        let lit_rel = stage_text(
                            &resolver,
                            &root,
                            &format!("docs_literal_{idx}.tmpl"),
                            body,
                        )?;
                        staged_lits.push(lit_rel.clone());
                        built.push(crate::config::StageSpec {
                            kind: "template".to_string(),
                            path: Some(lit_rel),
                            pattern: None,
                            text: None,
                            expand: false,
                        });
                    }
                    TemplateNode::Include(path) => {
                        let path = validate_rel_path(repo_root, path)?;
                        let expand = path.ends_with(".tmpl");
                        built.push(crate::config::StageSpec {
                            kind: if expand { "template" } else { "read" }.to_string(),
                            path: Some(path),
                            pattern: None,
                            text: None,
                            expand,
                        });
                    }
                }
            }
            built
        }
        (None, _) => target.stages.clone(),
    };

    // Validate every referenced input up front (sandbox containment).
    for stage in &stages {
        stage.validate()?;
        match stage.kind.as_str() {
            "template" | "read" => {
                let path = stage.path.as_deref().unwrap_or("");
                // Staged literals live under `.oxdock-staging` and are
                // already guarded; anything else goes through the guard.
                if !path.starts_with(".oxdock-staging/") {
                    validate_rel_path(repo_root, path)?;
                }
            }
            "glob" => {
                reject_traversal_pattern(&stage.pattern.clone().unwrap_or_default())?;
            }
            _ => {}
        }
    }

    // Materialize the stage manifest + values context for the DSL.
    let stages_json = serde_json::to_string(&stages)?;
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
    staged.extend(staged_lits);
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

#[cfg(test)]
mod tests {
    use super::*;

    fn includes(text: &str) -> Vec<TemplateNode> {
        parse_template_includes(text).expect("parse")
    }

    #[test]
    fn directives_only_in_order() {
        assert_eq!(
            includes("{{> a.md }}\n{{>b.tmpl}}\n"),
            vec![
                TemplateNode::Include("a.md".to_string()),
                TemplateNode::Include("b.tmpl".to_string()),
            ]
        );
    }

    #[test]
    fn literal_runs_preserved_byte_exact() {
        assert_eq!(
            includes("# Title\n\n{{> body.md }}\nTrailers {{ $docs_ctx.x }} stay.\n"),
            vec![
                TemplateNode::Literal("# Title\n\n".to_string()),
                TemplateNode::Include("body.md".to_string()),
                TemplateNode::Literal("Trailers {{ $docs_ctx.x }} stay.\n".to_string()),
            ]
        );
    }

    #[test]
    fn mid_line_directives_stay_literal() {
        // Only whole-line directives include; anything else is document
        // prose (and would fail strict expansion as an unknown key, which
        // is the honest signal for a misplaced directive).
        assert_eq!(
            includes("see {{> a.md }} here\n"),
            vec![TemplateNode::Literal("see {{> a.md }} here\n".to_string())]
        );
    }

    #[test]
    fn directive_errors_name_the_line() {
        for (bad, what) in [
            ("{{> }}\n", "empty"),
            ("{{> a.md b.md }}\n", "single token"),
            ("{{> ../escape.md }}\n", "`..`"),
        ] {
            let err = parse_template_includes(bad).expect_err("must fail");
            assert!(
                err.to_string().contains("line 1") && err.to_string().contains(what),
                "unexpected error for {bad:?}: {err:#}"
            );
        }
    }

    #[test]
    fn leading_blanks_ignored_trailing_literal_kept() {
        assert_eq!(
            includes("\n\n{{> a.md }}\n"),
            vec![TemplateNode::Include("a.md".to_string())]
        );
    }
}
