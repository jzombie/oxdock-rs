use anyhow::{Context, Result, bail};
#[allow(clippy::disallowed_types)]
use std::path::Path;

use oxdock_fs::GuardedPath;

/// Validate a workspace-root-relative candidate path.
///
/// Rejects absolute paths and any `..` segment up front, then enforces
/// sandbox containment through `GuardedPath::new`. Returns the normalized
/// relative string for use as a `$var` binding in pure-DSL scripts.
///
/// Every configured `out`, `template`, `values`, and fragment `pattern`
/// base must pass through here before any `WRITE`/`READ` step executes.
#[allow(clippy::disallowed_types)]
pub fn validate_rel_path(repo_root: &Path, candidate: &str) -> Result<String> {
    if candidate.is_empty() {
        bail!("empty path is not allowed");
    }
    // Absolute paths (posix, windows drive, verbatim) never resolve inside
    // the workspace; reject before touching the guard.
    if candidate.starts_with('/')
        || candidate.starts_with('\\')
        || (candidate.len() >= 3
            && candidate.as_bytes()[1] == b':'
            && (candidate.as_bytes()[2] == b'/' || candidate.as_bytes()[2] == b'\\'))
        || candidate.starts_with("\\\\")
    {
        bail!("absolute path not allowed: {candidate}");
    }
    let forward = candidate.replace('\\', "/");
    if forward.split('/').any(|seg| seg == "..") {
        bail!("path traversal not allowed: {candidate}");
    }
    // Anchor relative candidates at the repo root before guarding:
    // `GuardedPath::new` resolves relative paths against the process
    // working directory, which is not necessarily the workspace root
    // (e.g. `cargo test` runs with the crate dir as CWD).
    #[allow(clippy::disallowed_types)]
    let candidate_path = std::path::PathBuf::from(repo_root).join(&forward);
    let guarded = GuardedPath::new(repo_root, &candidate_path)
        .with_context(|| format!("path escapes workspace root: {candidate}"))?;
    // Re-derive the root-relative form so `$var` bindings carry a clean,
    // forward-slashed path regardless of platform.
    let rel = guarded
        .as_path()
        .strip_prefix(repo_root)
        .map(|p| p.to_string_lossy().replace('\\', "/"))
        .unwrap_or_else(|_| forward.clone());
    Ok(rel)
}
