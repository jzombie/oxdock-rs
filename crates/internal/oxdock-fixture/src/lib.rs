use anyhow::{Context, Result, ensure};
use oxdock_fs::{GuardedPath, GuardedTempDir, PathResolver, UnguardedPath, command_path};
use oxdock_process::CommandBuilder;
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use toml_edit::{DocumentMut, InlineTable, Item, Table, Value};

/// Builder that materializes a Cargo project template inside a temporary,
/// auto-cleaned directory for integration tests.
pub struct FixtureBuilder {
    template: UnguardedPath,
    replacements: BTreeMap<String, DependencyReplacement>,
}

enum DependencyReplacement {
    Path(PathBuf),
    Version(String),
}

/// Handle to a copied fixture workspace.
pub struct FixtureInstance {
    #[allow(dead_code)]
    tempdir: GuardedTempDir,
    root: GuardedPath,
}

impl FixtureBuilder {
    /// Create a builder from an on-disk template directory.
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
        })
    }

    /// Force the named dependency to use an absolute path inside the instantiated project.
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

        Ok(FixtureInstance { tempdir, root })
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

fn path_string(path: &Path) -> String {
    path.to_string_lossy().to_string()
}
