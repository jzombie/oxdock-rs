use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};

/// One ordered render stage for a target.
///
/// - `template`: `EXPAND` the file at `path` (strict; `{{ $docs_ctx.key }}`
///   / `{{ $docs_global.key }}` keypaths resolve against values maps).
/// - `read`: `READ` the file at `path` byte-verbatim (documented
///   `{{ env:EXAMPLE }}` snippets survive untouched).
/// - `glob`: collect `pattern` via `GLOB` (sorted, sandbox-relative), then
///   per file `EXPAND` when `expand` is true else `READ` verbatim.
/// - `text`: append the inline `text` string.
///
/// This mirrors — but does not hardcode — the historical `.tmpl`-vs-`.md`
/// split: `expand` is per-stage data, and no key names appear here (key /
/// section discovery is an open design point, see plan §5).
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct StageSpec {
    pub kind: String,
    #[serde(default)]
    pub path: Option<String>,
    #[serde(default)]
    pub pattern: Option<String>,
    #[serde(default)]
    pub text: Option<String>,
    #[serde(default)]
    pub expand: bool,
}

impl StageSpec {
    pub fn validate(&self) -> Result<()> {
        match self.kind.as_str() {
            "template" | "read" => {
                if self.path.as_deref().unwrap_or("").is_empty() {
                    anyhow::bail!("{} stage requires a `path`", self.kind);
                }
            }
            "glob" => {
                if self.pattern.as_deref().unwrap_or("").is_empty() {
                    anyhow::bail!("glob stage requires a `pattern`");
                }
            }
            "text" => {
                if self.text.is_none() {
                    anyhow::bail!("text stage requires `text`");
                }
            }
            other => anyhow::bail!("unknown stage kind: {other}"),
        }
        Ok(())
    }
}

/// One output file declaration.
///
/// All paths are workspace-root-relative strings. `stages` is the ordered
/// composition of the file (the structural definition lives in the
/// referenced template/fragment files, which may be any text format —
/// Markdown, TOML, source code). `values` optionally scopes per-target
/// overrides over the global defaults.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct TargetSpec {
    pub name: String,
    pub out: String,
    #[serde(default)]
    pub values: Option<String>,
    #[serde(default)]
    pub stages: Vec<StageSpec>,
    /// Workspace member owning this target (e.g. `crates/oxdock-core`).
    /// Discovery fills it from the target directory; explicit config
    /// targets may set it directly. Drives per-target provider values
    /// (cargo metadata) and `CRATE_*` env entries.
    #[serde(default)]
    pub member: Option<String>,
    /// Master template document: order comes from `{{> path }}`
    /// positions in the file itself, so no stage list is managed.
    /// Mutually exclusive with `stages`.
    #[serde(default)]
    pub template: Option<String>,
}

impl TargetSpec {
    pub fn validate(&self) -> Result<()> {
        if self.name.is_empty() {
            anyhow::bail!("target requires a `name`");
        }
        if self.out.is_empty() {
            anyhow::bail!("target `{}` requires an `out` path", self.name);
        }
        if self.template.is_some() && !self.stages.is_empty() {
            anyhow::bail!(
                "target `{}` declares both `template` and `stages`; use one",
                self.name
            );
        }
        if self.stages.is_empty() && self.template.is_none() {
            anyhow::bail!("target `{}` requires stages or a template", self.name);
        }
        for stage in &self.stages {
            stage.validate()?;
        }
        Ok(())
    }
}

/// Top-level configuration.
///
/// `template_roots` are extra directory roots scanned (recursively) for
/// `target.json` files, in addition to every workspace member's
/// `.oxdock/template` tree. They are *default values, not constants* —
/// callers may point at any layout. There is exactly one document system:
/// discovered `target.json` files plus explicit `targets` entries.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct DocsGenConfig {
    #[serde(default = "default_template_roots")]
    pub template_roots: Vec<String>,
    #[serde(default)]
    pub global_values: Option<String>,
    #[serde(default)]
    pub targets: Vec<TargetSpec>,
    #[serde(default)]
    pub providers: Vec<String>,
    /// Plugin fragment outputs: fragment name → workspace-relative path.
    /// E.g. the `command-ref` provider maps `command_index`/`command_body`
    /// to their generated section files. Config data, not code.
    #[serde(default)]
    pub generated_files: std::collections::HashMap<String, String>,
}

fn default_template_roots() -> Vec<String> {
    vec![".oxdock/template".to_string()]
}

impl Default for DocsGenConfig {
    fn default() -> Self {
        Self {
            template_roots: default_template_roots(),
            global_values: None,
            targets: Vec::new(),
            providers: Vec::new(),
            generated_files: std::collections::HashMap::new(),
        }
    }
}

impl DocsGenConfig {
    pub fn from_json(text: &str) -> Result<Self> {
        let config: Self = serde_json::from_str(text).context("parse docs-gen config JSON")?;
        for target in &config.targets {
            target.validate()?;
        }
        Ok(config)
    }

    #[allow(clippy::disallowed_methods, clippy::disallowed_types)]
    pub fn load(candidate: &std::path::Path) -> Result<Self> {
        let text = std::fs::read_to_string(candidate)
            .with_context(|| format!("read config {}", candidate.display()))?;
        Self::from_json(&text)
    }
}
