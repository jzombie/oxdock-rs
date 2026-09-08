use anyhow::{Context, Result};
use oxdock_fs::{GuardedPath, PathResolver};

/// Read text through the guarded resolver.
pub fn read_text(resolver: &PathResolver, root: &GuardedPath, rel: &str) -> Result<String> {
    let path = root.join(rel)?;
    resolver
        .read_to_string(&path)
        .with_context(|| format!("read {rel}"))
}

/// Write text through the guarded resolver, creating parents. Skips the
/// write when bytes are unchanged so syncs stay no-op clean.
pub fn write_text(
    resolver: &PathResolver,
    root: &GuardedPath,
    rel: &str,
    text: &str,
) -> Result<()> {
    let path = root.join(rel)?;
    if let Ok(existing) = resolver.read_to_string(&path)
        && existing == text
    {
        return Ok(());
    }
    if let Some(parent) = path.as_path().parent() {
        let parent_guard = GuardedPath::new(root.as_path(), parent)?;
        resolver.create_dir_all(&parent_guard)?;
    }
    resolver
        .write_file(&path, text.as_bytes())
        .with_context(|| format!("write {rel}"))
}
