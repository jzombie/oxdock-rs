use anyhow::{Context, Result};
#[allow(clippy::disallowed_types)]
use std::path::Path;

/// One named text fragment a provider contributes.
///
/// Fragments are format-agnostic strings; the caller maps them into
/// templates (e.g. `command_index` / `command_body`) instead of the provider
/// writing to hardcoded output paths.
#[derive(Clone, Debug)]
pub struct Fragment {
    pub name: String,
    pub contents: String,
}

/// Plugin interface wiring data sources into the generic engine.
///
/// The core never imports a concrete provider; `main.rs` selects providers
/// from config. New sources (TOML files, git metadata, …) implement this
/// trait without touching the renderer.
pub trait DataProvider {
    fn name(&self) -> &str;
    /// Values exposed to templates (merged target > global > provider in
    /// DSL via load order; providers only materialize values files and/or
    /// `ExecIo` env entries — no manual `StepKind` construction).
    fn values(&self) -> serde_json::Value {
        serde_json::Value::Null
    }
    fn fragments(&self) -> Vec<Fragment> {
        Vec::new()
    }
}

#[derive(Clone, Debug)]
pub struct CargoMetadata {
    pub name: String,
    pub description: String,
    pub version: String,
}

pub struct CargoMetadataProvider {
    meta: CargoMetadata,
}

impl CargoMetadataProvider {
    #[allow(clippy::disallowed_types)]
    pub fn load(cargo_toml: &Path, workspace_toml: &Path) -> Result<Self> {
        Ok(Self {
            meta: parse_cargo_metadata(cargo_toml, workspace_toml)?,
        })
    }

    pub fn metadata(&self) -> &CargoMetadata {
        &self.meta
    }
}

impl DataProvider for CargoMetadataProvider {
    fn name(&self) -> &str {
        "cargo-metadata"
    }

    fn values(&self) -> serde_json::Value {
        serde_json::json!({
            "name": self.meta.name,
            "description": self.meta.description,
            "version": self.meta.version,
        })
    }
}

/// Command-reference data source (moved out of `main.rs` hardcoding).
///
/// Renders the same `oxdock-parser` metadata registry as before, but returns
/// the results as named fragments instead of writing to fixed output paths
/// — the caller maps fragments to outputs via config.
pub struct CommandRefProvider;

impl DataProvider for CommandRefProvider {
    fn name(&self) -> &str {
        "command-ref"
    }

    fn fragments(&self) -> Vec<Fragment> {
        vec![
            Fragment {
                name: "command_index".to_string(),
                contents: crate::command_ref::render_index(),
            },
            Fragment {
                name: "command_body".to_string(),
                contents: crate::command_ref::generate_body(),
            },
        ]
    }
}

/// Workspace member list (optional enumeration source; no longer the render
/// driver — discovery is config/template-dir driven).
#[allow(clippy::disallowed_methods, clippy::disallowed_types)]
pub fn parse_workspace_members(workspace_toml: &Path) -> Result<Vec<String>> {
    let contents = std::fs::read_to_string(workspace_toml)?;
    let doc: toml_edit::DocumentMut = contents.parse()?;

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

#[allow(clippy::disallowed_methods, clippy::disallowed_types)]
pub fn parse_workspace_version(workspace_toml: &Path) -> String {
    let contents = std::fs::read_to_string(workspace_toml).unwrap_or_default();
    let doc: Result<toml_edit::DocumentMut, _> = contents.parse();
    let Ok(doc) = doc else {
        return "unknown".to_string();
    };
    doc.get("workspace")
        .and_then(|w| w.get("package"))
        .and_then(|p| p.get("version"))
        .and_then(|v| v.as_str())
        .unwrap_or("unknown")
        .to_string()
}

#[allow(clippy::disallowed_methods, clippy::disallowed_types)]
pub fn parse_cargo_metadata(cargo_toml: &Path, workspace_toml: &Path) -> Result<CargoMetadata> {
    let contents = std::fs::read_to_string(cargo_toml)?;
    let doc: toml_edit::DocumentMut = contents.parse()?;

    let name = doc
        .get("package")
        .and_then(|p| p.get("name"))
        .and_then(|v| v.as_str())
        .unwrap_or("unknown")
        .to_string();

    let description = doc
        .get("package")
        .and_then(|p| p.get("description"))
        .and_then(|v| v.as_str())
        .unwrap_or("No description provided.")
        .to_string();

    // Member crates use `version.workspace = true`, so a literal
    // `package.version` string is only present for standalone version pins.
    // Fall back to `workspace.package.version` otherwise so generated docs
    // never need a hardcoded copy of the version.
    let version = doc
        .get("package")
        .and_then(|p| p.get("version"))
        .and_then(|v| v.as_str())
        .map(String::from)
        .unwrap_or_else(|| parse_workspace_version(workspace_toml));

    Ok(CargoMetadata {
        name,
        description,
        version,
    })
}
