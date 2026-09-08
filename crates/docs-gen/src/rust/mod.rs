pub mod cargo;

use anyhow::{Context, Result};
use oxdock_fs::{GuardedPath, PathResolver};

use crate::io::read_text;

/// Resolve a version string: explicit process env wins, otherwise fall
/// back to the workspace manifest. The source is always logged so the
/// choice is visible, never magic.
pub fn crate_version(root: &GuardedPath, resolver: &PathResolver) -> Result<String> {
    if let Ok(value) = std::env::var("CRATE_VERSION")
        && !value.is_empty()
    {
        eprintln!("using CRATE_VERSION={value} from the environment");
        return Ok(value);
    }
    let text = read_text(resolver, root, "Cargo.toml")?;
    let doc: toml_edit::DocumentMut = text.parse().context("parse Cargo.toml")?;
    let version = doc
        .get("workspace")
        .and_then(|w| w.get("package"))
        .and_then(|p| p.get("version"))
        .and_then(|v| v.as_str())
        .context("workspace.package.version not found in Cargo.toml")?;
    eprintln!("CRATE_VERSION unset; using workspace version {version}");
    Ok(version.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Save/remove/restore one process variable around a test. Only this
    /// module's test touches `CRATE_VERSION`, so no cross-test
    /// serialization is needed.
    struct EnvGuard {
        key: &'static str,
        previous: Option<String>,
    }

    impl EnvGuard {
        fn remove(key: &'static str) -> Self {
            let previous = std::env::var(key).ok();
            unsafe {
                std::env::remove_var(key);
            }
            Self { key, previous }
        }

        fn set(key: &'static str, value: &str) -> Self {
            let previous = std::env::var(key).ok();
            unsafe {
                std::env::set_var(key, value);
            }
            Self { key, previous }
        }
    }

    impl Drop for EnvGuard {
        fn drop(&mut self) {
            unsafe {
                match &self.previous {
                    Some(value) => std::env::set_var(self.key, value),
                    None => std::env::remove_var(self.key),
                }
            }
        }
    }

    #[allow(clippy::disallowed_methods, clippy::disallowed_types)]
    fn fixture_root(manifest: &str) -> (oxdock_fs::GuardedTempDir, GuardedPath, PathResolver) {
        let temp = GuardedPath::tempdir().expect("tempdir");
        let root = temp.as_guarded_path().clone();
        let resolver = PathResolver::new(root.as_path(), root.as_path()).expect("resolver");
        let manifest_path = root.join("Cargo.toml").expect("join");
        resolver
            .write_file(&manifest_path, manifest.as_bytes())
            .expect("write manifest");
        (temp, root, resolver)
    }

    #[test]
    #[cfg_attr(
        miri,
        ignore = "fixture needs host tempdir and file IO, blocked by Miri isolation"
    )]
    fn env_value_wins_then_falls_back_to_manifest() {
        // Single test (not two) so no parallel thread can race it on the
        // shared process variable; the guard restores ambient state after.
        let (_temp, root, resolver) =
            fixture_root("[workspace.package]\nversion = \"0.10.0-alpha\"\n");
        {
            let _guard = EnvGuard::set("CRATE_VERSION", "9.9.9-test");
            assert_eq!(
                crate_version(&root, &resolver).expect("version"),
                "9.9.9-test"
            );
        }
        {
            let _guard = EnvGuard::remove("CRATE_VERSION");
            assert_eq!(
                crate_version(&root, &resolver).expect("version"),
                "0.10.0-alpha"
            );
        }
    }
}
