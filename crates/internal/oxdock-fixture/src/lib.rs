use anyhow::{Context, Result, ensure};
#[allow(clippy::disallowed_types)]
use oxdock_fs::UnguardedPath;
use oxdock_fs::{GuardedPath, GuardedTempDir, PathResolver, WorkspaceSnapshot, command_path};
use oxdock_process::CommandBuilder;
use std::collections::BTreeMap;
#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
use std::path::{Path, PathBuf};
use toml_edit::{DocumentMut, InlineTable, Item, Table, Value};

/// Builder that materializes a Cargo project template inside a temporary,
/// auto-cleaned directory for integration tests.
#[allow(clippy::disallowed_types)]
pub struct FixtureBuilder {
    template: UnguardedPath,
    replacements: BTreeMap<String, DependencyReplacement>,
    workspace_snapshot: Option<WorkspaceSnapshot>,
    workspace_root_env: Option<PathBuf>,
}

#[allow(clippy::disallowed_types)]
enum DependencyReplacement {
    Path(PathBuf),
    Version(String),
}

/// Handle to a copied fixture workspace.
#[allow(clippy::disallowed_types)]
pub struct FixtureInstance {
    #[allow(dead_code)]
    tempdir: GuardedTempDir,
    root: GuardedPath,
    #[allow(dead_code)]
    workspace_snapshot: Option<WorkspaceSnapshot>,
    workspace_root_env: Option<PathBuf>,
}

impl FixtureBuilder {
    /// Create a builder from an on-disk template directory.
    #[allow(clippy::disallowed_types, clippy::disallowed_methods)]
    pub fn new(template: impl AsRef<Path>) -> Result<Self> {
        let path = template.as_ref();
        ensure!(
            path.exists(),
            "fixture template {} does not exist",
            path.display()
        );
        Ok(Self {
            template: UnguardedPath::new(path),
            replacements: BTreeMap::new(),
            workspace_snapshot: None,
            workspace_root_env: None,
        })
    }

    /// Force the named dependency to use an absolute path inside the instantiated project.
    #[allow(clippy::disallowed_types, clippy::disallowed_methods)]
    pub fn with_path_dependency(mut self, name: impl Into<String>, path: impl AsRef<Path>) -> Self {
        self.replacements.insert(
            name.into(),
            DependencyReplacement::Path(path.as_ref().to_path_buf()),
        );
        self
    }

    /// Force the named dependency to use a literal version string.
    pub fn with_version_dependency(
        mut self,
        name: impl Into<String>,
        version: impl Into<String>,
    ) -> Self {
        self.replacements
            .insert(name.into(), DependencyReplacement::Version(version.into()));
        self
    }

    /// Override the workspace root that should be exposed to commands spawned via [`FixtureInstance`].
    /// When present, the spawned process receives `OXDOCK_WORKSPACE_ROOT`, enabling DSL scripts
    /// to open a clean snapshot of the original repository while working against the copied fixture.
    #[allow(clippy::disallowed_types, clippy::disallowed_methods)]
    pub fn with_workspace_root(mut self, root: impl AsRef<Path>) -> Self {
        self.workspace_root_env = Some(root.as_ref().to_path_buf());
        self
    }

    /// Snapshot the provided workspace root into a temporary directory and expose it to fixture
    /// commands as their `OXDOCK_WORKSPACE_ROOT`.
    #[allow(clippy::disallowed_types, clippy::disallowed_methods)]
    pub fn with_workspace_snapshot_from(mut self, root: impl AsRef<Path>) -> Result<Self> {
        let guard = GuardedPath::new_root(root.as_ref())?;
        let snapshot = WorkspaceSnapshot::new(&guard)?;
        self.workspace_root_env = Some(snapshot.root().as_path().to_path_buf());
        self.workspace_snapshot = Some(snapshot);
        Ok(self)
    }

    /// Snapshot the workspace at a specific commit and expose it via `OXDOCK_WORKSPACE_ROOT`.
    #[allow(clippy::disallowed_types, clippy::disallowed_methods)]
    pub fn with_workspace_snapshot_at_commit(
        mut self,
        root: impl AsRef<Path>,
        commit: impl AsRef<str>,
    ) -> Result<Self> {
        let guard = GuardedPath::new_root(root.as_ref())?;
        let snapshot = WorkspaceSnapshot::at_commit(&guard, commit)?;
        self.workspace_root_env = Some(snapshot.root().as_path().to_path_buf());
        self.workspace_snapshot = Some(snapshot);
        Ok(self)
    }

    /// Copy the template into a guarded temporary directory, returning a handle
    /// that cleans up automatically on drop.
    pub fn instantiate(self) -> Result<FixtureInstance> {
        let tempdir = GuardedPath::tempdir()?;
        let root = tempdir.as_guarded_path().clone();
        let resolver = PathResolver::new_guarded(root.clone(), root.clone())?;

        resolver.copy_dir_from_unguarded(&self.template, &root)?;

        if !self.replacements.is_empty() {
            patch_manifest(&resolver, &root, &self.replacements)?;
        }

        let workspace_root_env = self
            .workspace_root_env
            .or_else(|| detect_workspace_root(self.template.as_path()));

        Ok(FixtureInstance {
            tempdir,
            root,
            workspace_snapshot: self.workspace_snapshot,
            workspace_root_env,
        })
    }
}

impl FixtureInstance {
    /// Guarded path to the fixture root directory.
    pub fn root(&self) -> &GuardedPath {
        &self.root
    }

    /// Guarded path to `Cargo.toml` inside the copied fixture.
    pub fn manifest_path(&self) -> Result<GuardedPath> {
        self.root.join("Cargo.toml")
    }

    /// Build a `CommandBuilder` scoped to the fixture root.
    pub fn cargo(&self) -> CommandBuilder {
        let mut builder = CommandBuilder::new("cargo");
        let cwd = command_path(self.root()).into_owned();
        builder.current_dir(cwd);
        if let Some(root) = &self.workspace_root_env {
            builder.env("OXDOCK_WORKSPACE_ROOT", root);
        }
        builder
    }
}

fn patch_manifest(
    resolver: &PathResolver,
    root: &GuardedPath,
    replacements: &BTreeMap<String, DependencyReplacement>,
) -> Result<()> {
    let manifest_path = root.join("Cargo.toml")?;
    let contents = resolver
        .read_to_string(&manifest_path)
        .context("failed to read fixture manifest")?;
    let mut doc = contents
        .parse::<DocumentMut>()
        .context("failed to parse fixture manifest")?;

    for section in ["dependencies", "dev-dependencies", "build-dependencies"] {
        let Some(item) = doc.get_mut(section) else {
            continue;
        };
        if let Item::Table(table) = item {
            patch_dependency_table(table, replacements);
        }
    }

    resolver.write_file(&manifest_path, doc.to_string().as_bytes())?;
    Ok(())
}

fn patch_dependency_table(
    table: &mut Table,
    replacements: &BTreeMap<String, DependencyReplacement>,
) {
    for (name, replacement) in replacements {
        let Some(item) = table.get_mut(name) else {
            continue;
        };
        match replacement {
            DependencyReplacement::Path(path) => apply_path_replacement(item, path),
            DependencyReplacement::Version(version) => apply_version_replacement(item, version),
        }
    }
}

#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
fn apply_path_replacement(item: &mut Item, path: &Path) {
    let path_value = Value::from(path_string(path));
    match item {
        Item::Value(Value::InlineTable(table)) => {
            table.insert("path", path_value.clone());
            table.remove("workspace");
        }
        Item::Table(table) => {
            insert_or_replace(table, "path", path_value.clone());
            table.remove("workspace");
        }
        Item::Value(existing) => {
            let mut inline = InlineTable::new();
            if let Value::String(s) = existing {
                inline.insert("version", Value::from(s.value().to_string()));
            }
            inline.insert("path", path_value);
            *item = Item::Value(Value::InlineTable(inline));
        }
        _ => {}
    }
}

fn apply_version_replacement(item: &mut Item, version: &str) {
    let version_value = Value::from(version.to_string());
    match item {
        Item::Value(Value::InlineTable(table)) => {
            table.insert("version", version_value.clone());
            table.remove("workspace");
        }
        Item::Table(table) => {
            insert_or_replace(table, "version", version_value.clone());
            table.remove("workspace");
        }
        Item::Value(_) => {
            *item = Item::Value(version_value);
        }
        _ => {}
    }
}

fn insert_or_replace(table: &mut Table, key: &str, val: Value) {
    let item = Item::Value(val);
    if let Some(existing) = table.get_mut(key) {
        *existing = item;
    } else {
        table.insert(key, item);
    }
}

#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
fn path_string(path: &Path) -> String {
    path.to_string_lossy().to_string()
}

#[allow(clippy::disallowed_types, clippy::disallowed_methods)]
fn detect_workspace_root(template: &Path) -> Option<PathBuf> {
    let mut cur = template;
    while let Some(parent) = cur.parent() {
        #[allow(clippy::disallowed_methods, clippy::disallowed_types)]
        {
            if parent.join(".git").exists() {
                return Some(parent.to_path_buf());
            }
        }
        cur = parent;
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use std::collections::BTreeMap;
    use toml_edit::DocumentMut;

    #[test]
    fn patch_manifest_applies_replacements_to_all_sections() -> Result<()> {
        let tempdir = GuardedPath::tempdir()?;
        let root = tempdir.as_guarded_path().clone();
        let resolver = PathResolver::new_guarded(root.clone(), root.clone())?;

        let manifest = indoc! {r#"
            [package]
            name = "fixture"
            version = "0.1.0"

            [dependencies]
            foo = { version = "0.1.0", workspace = true }

            [dev-dependencies]
            bar = "0.2.0"

            [build-dependencies]
            baz = { version = "0.3.0", workspace = true }
        "#};

        let manifest_path = root.join("Cargo.toml")?;
        resolver.write_file(&manifest_path, manifest.as_bytes())?;

        let mut replacements = BTreeMap::new();
        let foo_path = root.join("local_dep")?;
        replacements.insert(
            "foo".to_string(),
            DependencyReplacement::Path(foo_path.as_path().to_path_buf()),
        );
        replacements.insert(
            "bar".to_string(),
            DependencyReplacement::Version("1.2.3".to_string()),
        );
        replacements.insert(
            "baz".to_string(),
            DependencyReplacement::Version("4.5.6".to_string()),
        );

        patch_manifest(&resolver, &root, &replacements)?;

        let rewritten = resolver.read_to_string(&manifest_path)?;
        let doc = rewritten.parse::<DocumentMut>()?;

        let foo_path_string = path_string(foo_path.as_path());
        assert_eq!(
            doc["dependencies"]["foo"]["path"].as_str(),
            Some(foo_path_string.as_str())
        );
        assert!(doc["dependencies"]["foo"].get("workspace").is_none());
        assert_eq!(doc["dev-dependencies"]["bar"].as_str(), Some("1.2.3"));
        assert_eq!(
            doc["build-dependencies"]["baz"]["version"].as_str(),
            Some("4.5.6")
        );

        Ok(())
    }

    #[test]
    fn detect_workspace_root_finds_enclosing_git_directory() -> Result<()> {
        let tempdir = GuardedPath::tempdir()?;
        let root = tempdir.as_guarded_path().clone();
        let resolver = PathResolver::new_guarded(root.clone(), root.clone())?;
        let git_dir = root.join(".git")?;
        resolver.create_dir_all(&git_dir)?;

        let nested = root.join("nested/project")?;
        resolver.create_dir_all(&nested)?;

        let detected = detect_workspace_root(nested.as_path());
        assert_eq!(detected.as_deref(), Some(root.as_path()));

        Ok(())
    }
}
