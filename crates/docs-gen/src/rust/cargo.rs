use anyhow::{Context, Result};
use oxdock_fs::{GuardedPath, PathResolver};

use crate::io::{read_text, write_text};

/// Workspace member list from the root manifest.
#[allow(clippy::disallowed_methods, clippy::disallowed_types)]
pub fn workspace_members(root: &GuardedPath, resolver: &PathResolver) -> Result<Vec<String>> {
    let text = read_text(resolver, root, "Cargo.toml")?;
    let doc: toml_edit::DocumentMut = text.parse().context("parse Cargo.toml")?;
    let members = doc
        .get("workspace")
        .and_then(|w| w.get("members"))
        .and_then(|m| m.as_array())
        .context("workspace.members not found or not an array")?;
    Ok(members
        .iter()
        .filter_map(|v| v.as_str().map(String::from))
        .collect())
}

/// Package name/description from a member manifest. Returns `None` when
/// there is no manifest or no `[package]` name, in which case the
/// committed values.json is static data and left untouched.
#[allow(clippy::disallowed_methods, clippy::disallowed_types)]
pub fn cargo_package(
    root: &GuardedPath,
    resolver: &PathResolver,
    member: &str,
) -> Result<Option<(String, String)>> {
    let rel = format!("{member}/Cargo.toml");
    let path = root.join(&rel)?;
    if resolver.entry_kind(&path).is_err() {
        return Ok(None);
    }
    let text = read_text(resolver, root, &rel)?;
    let doc: toml_edit::DocumentMut = text.parse().context("parse Cargo.toml")?;
    let Some(name) = doc
        .get("package")
        .and_then(|p| p.get("name"))
        .and_then(|v| v.as_str())
    else {
        return Ok(None);
    };
    let description = doc
        .get("package")
        .and_then(|p| p.get("description"))
        .and_then(|v| v.as_str())
        .unwrap_or("No description provided.");
    Ok(Some((name.to_string(), description.to_string())))
}

/// Sync one member's values.json from its manifest, writing back only on
/// change. The manifest is the source of truth for every key EXCEPT
/// `name`, which the committed file may override (display names such as
/// `OxDock` vs the package name `oxdock`). Fixed key order keeps output
/// stable, so manifest edits (descriptions included) always flow through
/// on the next run instead of being shadowed by stale copies.
pub fn sync_package_values(
    root: &GuardedPath,
    resolver: &PathResolver,
    member: &str,
) -> Result<()> {
    let Some((name, description)) = cargo_package(root, resolver, member)? else {
        return Ok(());
    };
    let rel = format!("{member}/.oxdock/template/values.json");
    let path = root.join(&rel)?;
    let mut merged: Vec<(String, String)> = vec![
        ("name".to_string(), name),
        ("description".to_string(), description),
    ];
    if resolver.entry_kind(&path).is_ok() {
        let text = read_text(resolver, root, &rel)?;
        let parsed: serde_json::Value =
            serde_json::from_str(&text).with_context(|| format!("parse {rel}"))?;
        if let Some(map) = parsed.as_object()
            && let Some(override_name) = map.get("name").and_then(|v| v.as_str())
            && let Some(slot) = merged.iter_mut().find(|(k, _)| k == "name")
        {
            slot.1 = override_name.to_string();
        }
    }
    let mut out = String::from("{");
    for (idx, (key, value)) in merged.iter().enumerate() {
        if idx > 0 {
            out.push_str(", ");
        }
        out.push_str(&serde_json::to_string(key)?);
        out.push_str(": ");
        out.push_str(&serde_json::to_string(value)?);
    }
    out.push_str("}\n");
    write_text(resolver, root, &rel, &out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[allow(clippy::disallowed_methods, clippy::disallowed_types)]
    fn fixture_root() -> (oxdock_fs::GuardedTempDir, GuardedPath, PathResolver) {
        let temp = GuardedPath::tempdir().expect("tempdir");
        let root = temp.as_guarded_path().clone();
        let resolver = PathResolver::new(root.as_path(), root.as_path()).expect("resolver");
        (temp, root, resolver)
    }

    #[allow(clippy::disallowed_methods, clippy::disallowed_types)]
    fn write_manifest(
        resolver: &PathResolver,
        root: &GuardedPath,
        member: &str,
        description: &str,
    ) {
        let dir = root.join(member).expect("join");
        resolver.create_dir_all(&dir).expect("mkdir");
        let manifest = dir.join("Cargo.toml").expect("join");
        resolver
            .write_file(
                &manifest,
                format!("[package]\nname = \"{member}-pkg\"\ndescription = \"{description}\"\n")
                    .as_bytes(),
            )
            .expect("write manifest");
        let template_dir = root
            .join(&format!("{member}/.oxdock/template"))
            .expect("join");
        resolver.create_dir_all(&template_dir).expect("mkdir");
    }

    #[test]
    #[cfg_attr(
        miri,
        ignore = "fixture needs host tempdir and file IO, blocked by Miri isolation"
    )]
    fn manifest_description_edits_flow_through_on_resync() {
        // Regression test: a description change in the member manifest
        // must reach values.json on the next sync (previously the
        // committed copy shadowed it forever), while a committed `name`
        // override keeps winning.
        let (_temp, root, resolver) = fixture_root();
        write_manifest(&resolver, &root, "demo", "first description");
        let values = root
            .join("demo/.oxdock/template/values.json")
            .expect("join");
        resolver
            .write_file(
                &values,
                b"{\"name\": \"Display\", \"description\": \"stale copy\"}",
            )
            .expect("write values");

        sync_package_values(&root, &resolver, "demo").expect("sync");
        let after = resolver.read_to_string(&values).expect("read");
        assert!(
            after.contains("\"description\": \"first description\""),
            "manifest description must flow through, got: {after}"
        );
        assert!(
            after.contains("\"name\": \"Display\""),
            "committed name override must win, got: {after}"
        );

        write_manifest(&resolver, &root, "demo", "second description");
        sync_package_values(&root, &resolver, "demo").expect("resync");
        let resynced = resolver.read_to_string(&values).expect("read");
        assert!(
            resynced.contains("\"description\": \"second description\""),
            "edited manifest description must flow through, got: {resynced}"
        );
        assert!(
            resynced.contains("\"name\": \"Display\""),
            "committed name override must survive resync, got: {resynced}"
        );
    }

    #[test]
    #[cfg_attr(
        miri,
        ignore = "fixture needs host tempdir and file IO, blocked by Miri isolation"
    )]
    fn missing_manifest_leaves_values_untouched() {
        let (_temp, root, resolver) = fixture_root();
        sync_package_values(&root, &resolver, "ghost").expect("sync");
        let values = root
            .join("ghost/.oxdock/template/values.json")
            .expect("join");
        assert!(
            resolver.entry_kind(&values).is_err(),
            "no manifest must mean no values file is created"
        );
    }
}
